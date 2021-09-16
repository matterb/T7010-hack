//  ===============================================
//  CamelForth for the Intel 80x86
//  Copyright (c) 1996 Bradford J. Rodriguez
// 
//  This program is free software//  you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation//  either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY//  without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

//  Commercial inquiries should be directed to the author at 
//  115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
//  or via email to bj@camelforth.com
// 
//  ===============================================
//    Source code is for GNU Assembler.
//    Forth words are documented as follows:
// x  NAME  ( stack -- stack )  \ description
//    where x=C for ANS Forth Core words,
//           =X for ANS Extensions, 
//           =Z for internal or private words.
//  Guide to stack diagrams:  
//      R: = return stack,
//       c = 8-bit character, 
//    flag = boolean (0 or -1), 
//       n = signed 32-bit, 
//       u = unsigned 32-bit,
//       d = signed 64-bit, 
//      ud = unsigned 64-bit,
//      +n = unsigned 32-bit, 
//       x = any cell value, 
//  *x j*x = any number of cell values,
//  a-addr = aligned adrs, 
//  c-addr = character adrs
//  p-addr = I/O port adrs, 
//     sys = system-specific.
//
// Direct- Threaded Forth model for Intel 80x86
// 32 bit cell, 8 bit char, 8 bit (byte) addrs unit, align 32bit
// Dictionary layout:
// LINK(4bytes):FLAG(1byte):NAME_FIELD(1byte len,name):Code_Field(4byte):PFA
// Register Usage:
//  %esi = IP   Forth Interpreter pointer
//  %edi = UP   User pointer
//  %esp = PSP  Parameter Stack Pointer
//  %ebp = RSP  Return Stack Pointer
//  %eax = W    work register, scratch
//  %ebx = TOS  Top of Parameter Stack
//  %ecx = loop index
//  %edx = scratch
//
//  data stack depth is 128 cells
//  return stack depth is 128 cells
//  user area and stacks fits on a 4k page
//  ===============================================
//  REVISION HISTORY
//  v2013.001
//  ===============================================
//  CAMEL86_32.S: Code Primitives

#ifdef LINUX
   #include <asm/unistd.h>
#endif

.list
// define some macrs and stuff
.set PFA,0x04   
.set link,0
 
// MACROS ========================================
//  mode : 1=immediate word
.macro head  label,length,name,action,mode
    .section .rodata
    .align 4
    .global name_\label
    .int  link
    .ifc \mode,1
// immediate word
        .byte 1
    .else
// normal word
        .byte 0
    .endif
name_\label:
    .set link, name_\label
    .byte \length
    .ascii "\name"
    .global \label
\label: 
    .ifnc \action, docode
        .int \action
    .else
        .global action_\label
        .int action_\label
        .text
action_\label:
    .endif
.endm

// build-time noname words

.macro nohead  label,action,mode
    .section .rodata
    .align 4
    .global name_\label
    .int 0 
    .ifc \mode,1
// immediate word
        .byte 1
    .else
// normal word
        .byte 0
    .endif
name_\label:
    .byte 0
    .global \label
\label: 
    .ifnc \action, docode
        .int \action
    .else
        .global action_\label
        .int action_\label
        .text
action_\label:
    .endif
.endm

#ifdef COREBOOT
// interrupt service vectors
.macro isr_noerr num
    .global vec\num
vec\num:
    cli
    pushl $0
    pushl $\num
    ljmp $0x10,$isr_common
.endm

.macro isr_err num
    .global vec\num
vec\num:
    cli
    pushl $\num
    ljmp $0x10,$isr_common
    .word 0x9090
.endm

.macro irq_map irq_num isr_num
    .global irq\irq_num
irq\irq_num:
    cli
    pushl $\irq_num
    pushl $\isr_num
    ljmp $0x10,$irq_common
.endm

#endif

// INTERPRETER LOGIC =============================

.macro  next
        lodsl
        jmp *(%eax)
.endm

.section .text
#ifdef LINUX
    .global main,_reset

#endif

#ifdef COREBOOT
.global main,_startup,isr_common,irq_common, irq_exit
.extern putchar,getchar,havekey,autoload
#endif

.global docon,do2con,douser
.global dodoes,dodefer
.global docolon, doenter

// PROGRAM ENTRY/INITIALIZATION ==================
main:
#ifdef LINUX
    pusha
    cld                 // direction=increment
    movl %esp, saved_esp
    call set_up_data_segment
    call save_term
    call echo_off
#endif

#ifdef COREBOOT
    cli
    lgdt    %cs:gdtaddr
    ljmp    $0x10, $1f
1:  movl    $0x18, %eax
    movl    %eax, %ds
    movl    %eax, %es
    movl    %eax, %ss
    movl    %eax, %fs
    movl    %eax, %gs
    cld                 // direction=increment

	leal	_stack, %edi
	movl	$_estack, %ecx
	subl	%edi, %ecx
    negl    %ecx
	shrl	$2, %ecx   /* it is 32 bit aligned, right? */
	movl	$0xDEADBEEF, %eax
	rep
	stosl

    movl $_estack, %esp
    movl %esp, saved_esp


/* Initialize the Interrupt Descriptor table */
    leal    _idt, %edi
    leal    vec0, %ebx
    movl    $(0x10 << 16), %eax /* cs selector */

1:  movw    %bx, %ax
    movl    %ebx, %edx
    movw    $0x8E00, %dx        /* Interrupt gate - dpl=0, present */
    movl    %eax, 0(%edi)
    movl    %edx, 4(%edi)
    addl    $0x0c, %ebx
    addl    $8, %edi
    cmpl    $_idt_end, %edi
    jne 1b

/* remap PIC */

    movb $0x11, %al
    movl $0x20, %edx
    outb %al, %dx

    movb $0x11, %al
    movl $0xa0, %edx
    outb %al, %dx

    movb $0x20, %al
    movl $0x21, %edx
    outb %al, %dx

    movb $0x28, %al
    movl $0xa1, %edx
    outb %al, %dx

    movb $0x04, %al
    movl $0x21, %edx
    outb %al, %dx

    movb $0x02, %al
    movl $0xa1, %edx
    outb %al, %dx

    movb $0x01, %al
    movl $0x21, %edx
    outb %al, %dx

    movb $0x01, %al
    movl $0xa1, %edx
    outb %al, %dx

    movb $0x00, %al
    movl $0x21, %edx
    outb %al, %dx

    movb $0x00, %al
    movl $0xa1, %edx
    outb %al, %dx


/* Load the Interrupt descriptor table */
    lidt    idtarg
   
// get space at top of memory for autoload buffer
// use offset from top of ram
// read reg b8 to get gxbase
// read 0ffset 0x8000 from gxbase to get tom
// see gx1cpu.fth for details

    movl  $0x400000, %eax
    movl %eax, mybuff
    movl %eax, mybuff_ptr
    call autoload              
#endif

    movl $User0, %edi       // initial UP
    movl %edi, %eax
    leal 0x600(%eax),%ebp   // user return stack
    leal 0x800(%eax),%esp   //  user parameter stack

    movl $COLD, %eax        // start Forth
    jmp *(%eax)

#ifdef COREBOOT
    .set pstack_size,0x300   // parameter stack size for interrupts
    .set stack_size, 0x400   // total stack size for interrupts

// Interrupt Vector Table. dword aligned
    .align 4
isr_noerr 0
isr_noerr 1
isr_noerr 2
isr_noerr 3
isr_noerr 4
isr_noerr 5
isr_noerr 6
isr_noerr 7
isr_err   8
isr_noerr 9
isr_err 10
isr_err 11
isr_err 12
isr_err 13
isr_err 14
isr_noerr 15
isr_noerr 16
isr_noerr 17
isr_noerr 18
isr_noerr 19
isr_noerr 20
isr_noerr 21
isr_noerr 22
isr_noerr 23
isr_noerr 24
isr_noerr 25
isr_noerr 26
isr_noerr 27
isr_noerr 28
isr_noerr 29
isr_noerr 30
isr_noerr 31
irq_map   0 32
irq_map   1 33
irq_map   2 34
irq_map   3 35
irq_map   4 36
irq_map   5 37
irq_map   6 38
irq_map   7 39
irq_map   8 40 
irq_map   9 41 
irq_map  10 42
irq_map  11 43
irq_map  12 44
irq_map  13 45
irq_map  14 46
irq_map  15 47


isr_common:
/* At this point on the stack there is:
 *  0(%esp) vector
 *  4(%esp) error code
 *  8(%esp) eip
 * 12(%esp) cs
 * 16(%esp) eflags
*/
    pushl %ebx
    pushl %ebp
    movl  %esp, %ebp
// switch to interrupt stack saving registers
    movl istack_ptr, %ebx
    addl $stack_size, %ebx
    movl %ebx, istack_ptr
    movl %ebx, %esp
    pushl %ebp
    pushl %eax
    pushl %ecx
    pushl %edx
    pushl %edi
    pushl %esi

    movl 8(%ebp), %ebx   // vector #
    shll $2, %ebx

    movl $isr_vectors, %eax    // exception handler
    addl %ebx,%eax
    movl %eax, %esi            // IP
    movl $User0, %edi            // UP

    movl 24(%ebp), %ebx       // eflags

    pushl %ebx
    popfl

    movl (istack_ptr), %ebp
    subl $pstack_size, %ebp     // RSP
    next

// restore forth stack        
isr_exit:
    popl %esi
    popl %edi
    popl %edx
    popl %ecx
    popl %eax
    popl %ebp
    movl (istack_ptr), %ebx
    subl $stack_size,%ebx
    movl %ebx, istack_ptr

    movl %ebp, %esp
    popl %ebp
    popl %ebx
    addl $8, %esp
    iret

irq_common:
/* At this point on the stack there is:
 *  0(%esp) vector
 *  4(%esp) error code
 *  8(%esp) eip
 * 12(%esp) cs
 * 16(%esp) eflags
*/
    pushl %ebx
    pushl %ebp
    movl  %esp, %ebp

    movl istack_ptr, %ebx
    addl $stack_size, %ebx
    movl %ebx, istack_ptr
    movl %ebx, %esp
    pushl %ebp
    pushl %eax
    pushl %ecx
    pushl %edx
    pushl %edi
    pushl %esi

    movl 8(%ebp), %ebx   // vector #
    subl  $0x20, %ebx
    shll $2, %ebx

    movl $irq_count, %eax   // interrupt counters
    addl %ebx,%eax
    incl (%eax)

    movl $irq_vectors, %eax    // interrupt handler
    addl %ebx,%eax
    movl %eax, %esi            // IP
   
    leal 28(%ebp), %ebx    // original stack ptr
 
    movl $User0, %edi            // UP
    movl (istack_ptr), %ebp
    subl $pstack_size, %ebp     // RSP
//     sti                         // allow other interrupts
    next

irq_exit:
// TODO: check whether to reset either master or slave
    cli
    movb $0x20, %al          // reset PIC
    movl $0xa0, %edx
    outb %al, %dx
    movl $0x20, %edx
    outb %al, %dx
// restore forth stack
    popl %esi
    popl %edi
    popl %edx
    popl %ecx
    popl %eax
    popl %ebp
    movl (istack_ptr), %ebx
    subl $stack_size,%ebx
    movl %ebx, istack_ptr

    movl %ebp, %esp
    popl %ebp
    popl %ebx
    addl $8, %esp
    sti                         // allow other interrupts
    iret

//  descriptor tables lifted from coreboot
.globl gdt, gdt_end, idtarg

gdtaddr:
    .word   gdt_end - gdt - 1
    .long   gdt     /* we know the offset */

gdt:
/* selgdt 0, unused */
    .word   0x0000, 0x0000      /* dummy */
    .byte   0x00, 0x00, 0x00, 0x00

/* selgdt 8, unused */
    .word   0x0000, 0x0000      /* dummy */
    .byte   0x00, 0x00, 0x00, 0x00

/* selgdt 0x10, flat code segment */
    .word   0xffff, 0x0000
    .byte   0x00, 0x9b, 0xcf, 0x00 /* G=1 and 0x0f, So we get 4Gbytes limit */

/* selgdt 0x18, flat data segment */
    .word   0xffff, 0x0000
    .byte   0x00, 0x93, 0xcf, 0x00

/* selgdt 0x20, unused */
    .word   0x0000, 0x0000      /* dummy */
    .byte   0x00, 0x00, 0x00, 0x00

/* The next two entries are used for executing VGA option ROMs */

/* selgdt 0x28 16 bit 64k code at 0x00000000 */
    .word   0xffff, 0x0000
    .byte   0, 0x9a, 0, 0

/* selgdt 0x30 16 bit 64k data at 0x00000000 */
    .word   0xffff, 0x0000
    .byte   0, 0x92, 0, 0

/* The next two entries are used for ACPI S3 RESUME */

/* selgdt 0x38, flat data segment 16 bit */
    .word   0x0000, 0x0000      /* dummy */
    .byte   0x00, 0x93, 0x8f, 0x00 /* G=1 and 0x0f, So we get 4Gbytes limit */

/* selgdt 0x40, flat code segment 16 bit */
    .word   0xffff, 0x0000
    .byte   0x00, 0x9b, 0x8f, 0x00 /* G=1 and 0x0f, So we get 4Gbytes limit */
gdt_end:

idtarg:
    .word   _idt_end - _idt - 1 /* limit */
    .long   _idt
    .word   0
_idt:
    .fill   48, 8, 0    # idt is uninitialized
_idt_end:

#endif
#ifdef COREBOOT
// 386 cpu register access words ==========
// Descriptor table registers and task register
// Z  idtr@  ( -- base limit )  \ get interrupt discriptor
    head IDTRFETCH,5,"idtr@",docode
        pushl %ebx
        movl $idt_temp,%eax
        sidt 0(%eax)
        movl 2(%eax),%ebx
        pushl %ebx
        movzwl (%eax),%ebx
        next

// Z  idtr!  ( base limit -- )  \ set interrupt decriptor ( does nothing!)
// does nothing at present
    head IDTRSTORE,5,"idtr!",docode
        popl    %ebx
        popl    %ebx
        next

// Z  gdtr@  ( -- base limit )    
    head GDTRFETCH,5,"gdtr@",docode
        pushl %ebx
        movl $gdt_temp,%eax
        sgdt 0(%eax)
        movl 2(%eax),%ebx
        pushl %ebx
        movzwl (%eax),%ebx
        next
.data
gdt_temp:
        .int 0x00,0x00 
idt_temp:
        .int 0x00,0x00

// Z  gdtr!  ( base limit -- )  \ set global descriptor
    head GDTRSTORE,5,"gdtr!",docode
        movl $gdt_temp,%eax
        movw %bx, (%eax)
        popl %ebx
        movl %ebx,2(%eax) 
        lgdt gdt_temp
        popl %ebx
        next

// Z  tr@  ( -- tr ) \ get test register contents  
    head TRFETCH,3,"tr@",docode
        pushl %ebx
        str   %bx
        next

// Z  tr!  ( tr -- )    ax pop    ax ltr   c;
    head TRSTORE,3,"tr!",docode
        ltr 0(%ebx)
        popl % ebx
        next

// Z  ldtr@  ( -- ldt )  \ get local descriptor
    head LDTRFETCH,5,"ldtr@",docode
        pushl %ebx
        sldt %bx
        next

    head LTDRSTORE,5,"ldtr!",docode
// Z  ldtr!  ( ldt -- )  \ set local descriptor 
//  
    lldt 0(%ebx)
    popl %ebx
    next

//  Control registers
// Z  cr0@  ( -- n )  \ get control register 0
//  fetch control register 0
    head CR0FRETCH,4,"cr0@",docode
        pushl %ebx
        movl %cr0, %ebx
        next

// Z  cr2@  ( -- n )  \ fetch control register 2
    head CR2FRETCH,4,"cr2@",docode
        pushl %ebx
        movl %cr2, %ebx
        next

// Z  cr3@  ( -- n )  \ fetch control register 3
    head CR3FRETCH,4,"cr3@",docode
        pushl %ebx
        movl %cr3, %ebx
        next

// Z  cr4@  ( -- u )  \ fetch control register 4
    head CR4FRETCH,4,"cr4@",docode
        pushl %ebx
        movl %cr4, %ebx
        next

// Z  cr0!  ( u -- )  \ store control register 0
    head CR0STORE,4,"cr0!",docode
        movl %ebx, %cr0
        popl %ebx
        next

// Z  cr2!  ( u -- )  \ store control register 0
    head CR2STORE,4,"cr2!",docode
        movl %ebx, %cr2
        popl %ebx
        next

// Z  cr3!  ( u -- )  \ store control register 0
    head CR3STORE,4,"cr3!",docode
        movl %ebx, %cr3
        popl %ebx
        next

// Z  cr4!  ( u --  )  \ store control register 0
    head CR4STORE,4,"cr4!",docode
        movl %ebx, %cr4
        popl %ebx
        next

// Debug registers
// Z  dr0@   ( -- n )  \ fetch debug register 0 contents
    head DBR0FETCH,4,"dr0@",docode
        pushl %ebx
        movl %dr0, %ebx
        next

// Z  dr1@  ( -- n )  \ fetch debug register 1 contents
    head DBR1FETCH,4,"dr1@",docode
        pushl %ebx
        movl %dr1, %ebx
        next

// Z  dr2@  ( -- n )  \ fetch debug register 2 contents
    head DBR2FETCH,4,"dr2@",docode
        pushl %ebx
        movl %dr2, %ebx
        next

// Z  dr3@  ( -- n )  \ fetch debug register 3 contents
//  fetch debug register 3
    head DBR3FETCH,4,"dr3@",docode
        pushl %ebx
        movl %dr3, %ebx
        next

// Z  dr6@  ( -- u )  \ fetch debug register 6 contents
//  fetch debug register 6
    head DBR6FETCH,4,"dr6@",docode
        pushl %ebx
        movl %dr6, %ebx
        next

// Z  dr7@  ( -- u )  \ fetch debug register 7 contents
//  fetch debug register 7
    head DBR7FETCH,4,"dr7@",docode
        pushl %ebx
        movl %dr7, %ebx
        next

// Z  dr0!  ( u -- )  \ set debug register 0 contents
//  store debug register 0
    head DBR0STORE,4,"dr0!",docode
        movl %ebx, %dr0
        popl %ebx
        next

// Z  dr1!  ( u -- )  \ set debug register 1 contents
//  store control register 0
    head DBR1STORE,4,"dr1!",docode
        movl %ebx, %dr1
        popl %ebx
        next

// Z  dr2!  ( u -- )  \ set debug register 2 contents
//  store debug register 0
    head DBR2STORE,4,"dr2!",docode
        movl %ebx, %dr2
        popl %ebx
        next

// Z dr3!  ( u -- )  \ set debug register 3 contents
//  store debug register 3
    head DBR4STORE,4,"dr3!",docode
        movl %ebx, %dr3
        popl %ebx
        next

// Z dr6!  ( u -- )  \ set debug register 6 contents
//  store debug register 6
    head DBR6STORE,4,"dr6!",docode
        movl %ebx, %dr6
        popl %ebx
        next

// Z dr7!  ( u -- )  \ set debug register 7 contents
//  store debug register 7
    head DBR7STORE,4,"dr7!",docode
        movl %ebx, %dr7
        popl %ebx
        next

//  Test registers
//
// Z tr3@  ( -- n )  \ fetch test register 3 contents
    head TR3FRETCH,4,"tr3@",docode
        pushl %ebx
        movl %tr3, %ebx
        next

// Z tr4@  ( -- n )  \ fetch test register 4 contents
//  fetch test register 4
    head TR4FRETCH,4,"tr4@",docode
        pushl %ebx
        movl %tr4, %ebx
        next

// Z tr5@  ( -- n )  \ fetch test register 5 contents
//  fetch test register 5
    head TR5FRETCH,4,"tr5@",docode
        pushl %ebx
        movl %tr5, %ebx
        next

// Z tr6@  ( -- n )  \ fetch test register 6 contents
//  fetch test register 6
    head TR6FRETCH,4,"tr6@",docode
        pushl %ebx
        movl %tr6, %ebx
        next

// Z tr7@  ( -- n )  \ fetch test register 7 contents
//  fetch test register 7
    head TR7FRETCH,4,"tr7@",docode
        pushl %ebx
        movl %tr7, %ebx
        next

// Z tr3!  ( u -- )  \ set test register 3 contents 
    head TR3STORE,4,"tr3!",docode
        movl %ebx, %tr4
        popl %ebx
        next

// Z tr4!  ( u -- )  \ set test register 4 contents 
    head TR4STORE,4,"tr4!",docode
        movl %ebx, %tr4
        popl %ebx
        next

// Z tr5!  ( u -- )  \ set test register 5 contents 
    head TR5STORE,4,"tr5!",docode
        movl %ebx, %tr5
        popl %ebx
        next

// Z tr6  ( u -- )  \ set test register 6 contents 
    head TR6STORE,4,"tr6!",docode
        movl %ebx, %tr6
        popl %ebx
        next

// Z tr7!  ( u -- )  \ set test register 7 contents 
    head TR7STORE,4,"tr7!",docode
        movl %ebx, %tr7
        popl %ebx
        next

// Z  cs@  ( -- sel )  \ fetch code segment selector
    head CSFETCH,3,"cs@",docode
    pushl %ebx
    movl %cs, %ebx
    next

// Z  ds@  ( -- sel )  \ fetch data segment selector
    head DSFETCH,3,"ds@",docode
    pushl %ebx
    movl %ds, %ebx
    next

// Z  es@  ( -- sel )  \ fetch extra segment selector
    head ESFETCH,3,"es@",docode
    pushl %ebx
    movl %es, %ebx
    next

// Z  fs@  ( -- sel )  \ fetch f segment selector
    head FSFETCH,3,"fs@",docode
    pushl %ebx
    movl %fs, %ebx
    next

// Z  gs@  ( -- sel )  \ fetch g segment selector
    head GSFETCH,3,"gs@",docode
    pushl %ebx
    movl %gs, %ebx
    next

// Z  ss@  ( -- sel )  \ fetch stack segment selector
    head SSFETCH,3,"ss@",docode
    pushl %ebx
    movl %ss, %ebx
    next

// Z  sti  ( --  )  \ enable interrupts
    head STI,3,"sti",docode
        sti
        next

// Z  cli  (  --  )  \ disable interrupts
    head CLI,3,"cli",docode
        cli
        next

// Z  cli?  ( -- flag )  \ query interrupts flag
    head CLIQUERY,4,"cli?",docode
        pushl %ebx
        xorl %ebx,%ebx
        pushfl
        popl %eax
        testl $0x200, %eax
        je 1f
        decl %ebx
1:      next

// Z  lock[  ( -- R: -- flg )  \ turn off interrupts till ]unlock
    head LOCKSQ,5,"lock[",docode
        xchgl %esp, %ebp
        pushf
        xchgl %esp, %ebp
        cli
        next

// Z  ]unlock  ( -- R: flag -- ) \ restore interrupt flag state before lock[
    head SQUNLOCK,7,"]unlock",docode
        xchgl %esp, %ebp
        popf
        xchgl %esp, %ebp
        next

#endif

// TERMINAL I/O ===========================

// C (emit  ( c --  )  \ output character to  output device no pause
    head BREMIT,5,"(emit",docolon
        .int EMITV,FETCH,EXECUTE,EXIT

// C  (key  ( -- c  )  \ read character from input device no pause
// keyv @ execute ;
    head BRKEY,4,"(key",docolon
        .int KEYV,FETCH,EXECUTE,EXIT

// C  (key?  ( -- f )  \ return true if character available no pause
// key?v @ execute ;
    head BRKEYQ,5,"(key?",docolon
        .int KEYQV,FETCH,EXECUTE,EXIT

// C  emit  ( c -- )  \ output character to  output device
//  (emit pause ;
    head EMIT,4,"emit",docolon
        .int BREMIT,PAUSE,EXIT

// C  key?  ( -- flag)  \ return true if character available
//  pause (key? ;
    head KEYQ,4,"key?",docolon
        .int PAUSE,BRKEYQ,EXIT

// C  key  ( -- c )  \ read character from input device
// begin key? until (key ;
    head KEY,3,"key",docolon
KEY1:   .int KEYQ,QBRANCH,KEY1
        .int BRKEY,EXIT

// Z  ioinit  ( --  )  \ initialize vectored IO
    head IOINIT,6,"ioinit",docolon
        .int PAUSE
        .int IOINITV,FETCH,EXECUTE
        .int EXIT

#ifdef LINUX
// X  bye   ( i*x -- )  \ return to SYSTEM
    head BYE,3,"bye",docolon
        .int RESTORETTY
        .int LIT,__NR_exit,SYSCALL0
        .int EXIT

#endif

#ifdef COREBOOT
    
// Z  emit_sio  ( c -- )  \ output character to  SIO_PORT
    head EMITSIO_PORT,8,"emit-sio",docode
// 
        .set SIO_PORT, 0x3f8

        movl $(SIO_PORT+0x05), %edx
1:      inb %dx, %al
        test $0x20, %al
        je 1b
        mov $SIO_PORT, %edx
        mov %bl, %al
        outb %al, %dx 
        pop %ebx
        next

// Z  keysio   ( -- c  )  \ get character from SIO_PORT
    head KEYSIO_PORT,7,"key-sio",docode
        pushl %ebx
        xorl %ebx, %ebx
        movl $(SIO_PORT+0x05), %edx
1:      inb %dx, %al
        test $0x01, %al
        je 1b
        movl $SIO_PORT, %edx
        inb %dx, %al
        movb %al, %bl
        next

// Z  key?sio  ( -- f )  \ return true if char waiting
    head KEYQSIO_PORT,8,"key-sio?",docode
        pushl %ebx
        xorl %ebx, %ebx
        movl $(SIO_PORT+0x05), %edx
        inb %dx, %al
        test $0x01, %al
        je 1f
        decl %ebx
1:      next

// Z  emitcon  ( c -- )  \ output character to console
    head EMITCON,8,"emit-con",docode
        push %ebx
        cmpl $0x0d,%ebx
        jne 1f
        pushl $0x0a
        call putchar
        pop %eax
1:      call putchar
        pop %eax
        pop %ebx
        next

// Z  keycon  ( -- c )  \ input character from console
    head KEYCON,7,"key-con",docode
        push %ebx
1:      call getchar
        cmpl $0x0, %eax
        je 1b
        movl %eax, %ebx 
        next

// Z  key?con  ( -- f )  \ return true if char waiting
    head KEYQCON,8,"key-con?",docode
        push %ebx
        xorl %ebx, %ebx
        call havekey
        cmpl $0x0, %eax 
        je 1f
        decl %ebx
1:      next

// X  bye  ( i*x --  ) \ return to Coreboot 
    head BYE,3,"bye",docode
        movl saved_esp, %esp
        xorl %eax, %eax
        leave
1:      jmp 1b
        ret
        hlt
        
#endif

// Multitasking support ========================

// C  pause  ( --  )
    head PAUSE,5,"pause",docolon
        .int PAUSEV,FETCH,EXECUTE
        .int EXIT
    
// C  (null)  ( -- )  \ does nothing
    head PNULL,6,"(null)",docode
        next

.set USER_PSP,0xcc    // user 'sp defined in high camel86_32h.S

// Z  (pause)  ( -- )   \ low level switch task
    head BRPAUSE,7,"(pause)",docode
        pushl %ecx              // loop cntr
        pushl %ebx              // TOS
        pushl %esi              // IP
        pushl %ebp              // RP
        movl %esp, USER_PSP(%edi)   // save sp to 'sp
        movl %edi, %ebx         // next task user area
        movl (%ebx), %edi       // new task UP
        movl USER_PSP(%edi),%esp    // new task SP
        popl %ebp               // new task RP
        popl %esi               // new task IP
        popl %ebx               // TOS
        popl %ecx               // loop cntr
        next

// INTERPRETER LOGIC =============================

// C  exit  ( -- )    \ exit a colon definition
    head EXIT,4,"exit",docode
        xchgl %esp, %ebp
        popl %esi          // pop old IP from RSP
        xchgl %esp, %ebp
        next

// Z  lit  ( -- x )  \ fetch inline literal to stack
// This is the primtive compiled by LITERAL.
    head LIT,3,"lit",docode
        pushl %ebx         // push old TOS
        lodsl              // fetch cell at IP to TOS
        movl %eax, %ebx
        next

// C  execute  ( i*x xt -- j*x ) \ execute Forth word at xt
    head EXECUTE,7,"execute",docode
        movl %ebx, %eax       // address of word ->  W
        popl %ebx          // pop new TOS
        jmp *(%eax)          // go do Forth word

// DEFINING WORDS ================================

// C  variable  ( -- )    \ define a Forth VARIABLE
//   CREATE CELL ALLOT ; 
// Action of ROMable variable is that of CONSTANT// 
// the constant holds the RAM address.
    head VARIABLE,8,"variable",docolon
        .int CREATE,CELL,ALLOT,EXIT
//DOCREATE
// entered by JMP DOVAR with AX=Code Field Adrs
    .text
dovar:  // -- a-addr    exec action of var
docreate: // -- a-addr  exec action of CREATE
        pushl %ebx      // push old TOS
        leal PFA(%eax), %ebx     // param.field address
        next

// C  constant   ( -- )      \  define a Forth constant
//   create -4 allot docon ,cf , ;
    head CONSTANT,8,"constant",docolon
        .int CREATE,LIT,-4,ALLOT
        .int LIT,docon,COMMACF
        .int COMMA,EXIT
    
// DOCON, code action of CONSTANT,
// entered by JMP DOCON with AX=Code Field Adrs
    .text
docon:  // -- x
        pushl %ebx      // push old TOS
        leal PFA(%eax), %ebx     // param.field address
        movl (%ebx), %ebx       // fetch contents of parameter field
        next

do2con:  // -- x1 x2
        pushl %ebx      // push old TOS
        leal PFA(%eax), %ebx     // param.field address
        pushl 4(%ebx)
        movl (%ebx), %ebx       // fetch contents of parameter field
        next

// Z  user  ( n --  )   \ define user variable "n"
//   CONSTANT DOES> (machine code fragment)
    head USER,4,"user",docolon
//        .int CONSTANT,XDOES
        .int CREATE,LIT,-4,ALLOT
        .int LIT,douser,COMMACF
        .int COMMA,EXIT

// DOUSER, code action of USER,
// entered by CALL DOUSER
    .text
douser:  // -- a-addr
        pushl %ebx
        leal PFA(%eax), %ebx       // param.field address
        movl (%ebx), %ebx  // fetch contents of parameter field
        addl %edi, %ebx       // add UP
        next

// DODOES, code action of DOES> clause
// entered by  AX-> JMP fragment
//                  parameter field
//                       ...
//        fragment: CALL DODOES
//                  high-level thread
// Enters high-level thread with address of
// parameter field on top of stack.
// (internal code fragment, not a Forth word)
dodoes:                       // -- a-addr
        xchgl %esp, %ebp      // push old IP on RStk
        pushl %esi
        xchgl %esp,%ebp
        popl %esi          // adrs of thread -> IP
        pushl %ebx         // push old TOS
        leal PFA(%eax), %ebx        // adrs of param.field to TOS
        next

// ENTER, a.k.a. DOCOLON, entered by JMP ENTER
// to enter a new high-level thread (colon def'n.)
// (internal code fragment, not a Forth word)
// N.B.: DOCOLON must be defined before any
// appearance of 'docolon' in a 'word' macro!
    .text
docolon:                // (alternate name)
doenter: xchgl %esp,%ebp     // push old IP on ret stack
        pushl %esi
        xchgl %esp,%ebp
        lea PFA(%eax), %esi // param field adrs -> IP
        next

//  DODEFER, entered by JMP dodefer
//           executes a vectored word
//           IP points to word to addr
dodefer:
        movl 4(%eax),%eax
        jmp *(%eax)
        next

// STACK OPERATIONS ==============================

// C  dup  ( x1 -- x1 x1 )    \ duplicate cell
    head DUPE,3,"dup",docode
pushtos: pushl %ebx
        next

// C  ?dup  ( x1 -- 0 | x1 x1 )  \ dup if nonzero
    head QDUP,4,"?dup",docode
        orl %ebx,%ebx
        jnz pushtos
        next

// C  drop  ( x -- )   \ drop top of stack
    head DROP,4,"drop",docode
poptos: popl %ebx
        next

// C  swap  ( x1 x2 -- x2 x1 )  \ swap top two items
    head SWAP,4,"swap",docode
        popl %edx
        pushl %ebx
        movl %edx,%ebx
        next

// C  over  ( x1 x2 -- x1 x2 x1 )  \ per stack diagram
    head OVER,4,"over",docode
        pushl %ebx
        movl 4(%esp),%ebx
        next

// C  rot  ( x1 x2 x3 -- x2 x3 x1 ) \ per stack diagram
    head ROT,3,"rot",docode
        popl %eax
        popl %edx
        pushl %eax
        pushl %ebx
        movl %edx,%ebx
        next

// C  -rot  ( x1 x2 x3 -- x3 x1 x2 ) \ per stack diagram
    head MINUSROT,4,"-rot",docode
        popl %eax
        popl %edx
        pushl %ebx
        pushl %edx
        movl %eax,%ebx
        next

// X  nip  ( x1 x2 -- x2  )      \ per stack diagram
    head NIP,3,"nip",docode
        popl %eax
        next

// X  pick  ( xn xn-1.. x0 n --- xn..x0 x1 )  \ per stack diagram 
    head PICK,4,"pick",docode
        movl (%esp,%ebx,0x04), %ebx
        next 

// C  >r  ( x --   R: -- x )   \ push to R-Stack
    head TOR,2,">r",docode
        xchgl %esp,%ebp
        pushl %ebx
        xchgl %esp,%ebp
        popl %ebx
        next

// C  r>  ( -- x    R: x -- )    \  pop from R-Stack
    head RFROM,2,"r>",docode
        pushl %ebx
        xchgl %esp,%ebp
        popl %ebx
        xchgl %esp,%ebp
        next
// C  r@   ( -- x     R: x -- x )  \ fetch from R-Stack
    head RFETCH,2,"r@",docode
        pushl %ebx
        movl (%ebp),%ebx
        next

// Z  sp@  ( -- a-addr )     \  get P-Stack pointer
    head SPFETCH,3,"sp@",docode
        pushl %ebx
        movl %esp,%ebx
        next

// Z  sp!  ( a-addr --  )    \ set P-Stack pointer
    head SPSTORE,3,"sp!",docode
        movl %ebx,%esp
        popl %ebx
        next

// Z  rp@  ( -- a-addr )    \ get R-Stack pointer
    head RPFETCH,3,"rp@",docode
        pushl %ebx
        movl %ebp,%ebx
        next

// Z  rp!  ( a-addr -- )    \ set R-Stack pointer
    head RPSTORE,3,"rp!",docode
        movl %ebx,%ebp
        popl %ebx
        next

// Z  up@  ( -- addr )      \ get User Area pointer
    head UPFETCH,3,"up@",docode
        pushl %ebx
        movl %edi,%ebx
        next

// Z  up!  ( addr -- )      \ set User Area pointer
    head UPSTORE,3,"up!",docode
        movl %ebx,%edi
        popl %ebx
        next

// X  tuck  ( x1 x2 -- x2 x1 x2 )    \ per stack diagram
    head TUCK,4,"tuck",docode
//        .int SWAP,OVER,EXIT
        popl %edx
        pushl %ebx 
        pushl %edx
        next 

//  DOUBLE OPERATORS ==============================

// C  2@   ( a-addr -- x1 x2 )    \ fetch 2 cells
//    dup cell+ @ swap @ ;
//    the lower address will appear on top of stack
//    head TWOFETCH,2,"2@",docolon
//        .int DUPE,CELLPLUS,FETCH,SWAP,FETCH,EXIT
    head TWOFETCH,2,"2@",docode
        movl 4(%ebx),%eax
        movl (%ebx),%ebx
        pushl %eax
        next


// C  2!   ( x1 x2 a-addr --  )    \  store 2 cells
//    swap over ! cell+ ! ; 
//    the top of stack is stored at the lower adrs
//    head TWOSTORE,2,"2!",docolon
//        .int SWAP,OVER,STORE,CELLPLUS,STORE,EXIT
    head TWOSTORE,2,"2!",docode
        popl %eax
        movl %eax,(%ebx)
        popl %eax
        movl %eax,4(%ebx)
        popl %ebx
        next

// C  2drop  ( x1 x2 --   )      \   drop 2 cells
//    DROP DROP ;
//    head TWODROP,5,"2drop",docolon
//        .int DROP,DROP,EXIT
    head TWODROP,5,"2drop",docode
        popl %ebx
        popl %ebx
        next

// C  2dup  ( x1 x2 -- x1 x2 x1 x2 )    \ dup top 2 cells
//    over over ; 
//    head TWODUP,4,"2dup",docolon
//        .int OVER,OVER,EXIT
    head TWODUP,4,"2dup",docode
        movl (%esp),%eax
        pushl %ebx
        pushl %eax
        next

// C  2r@   ( -- x1 x2   R: x1 x2 -- x1 x2 )    \ fetch 2 cells from R-stack
    head TWORFETCH,3,"2r@",docode
        pushl %ebx
        movl 4(%ebp),%eax
        movl (%ebp),%ebx
        pushl %eax
        next

// C  2r>   ( -- x1 x2    R: x1 x2 -- )    \ pop 2 cells from R-Stack
    head TWORFROM,3,"2r>",docode
        pushl %ebx
        xchgl %esp,%ebp
        popl %ebx
        popl %edx
        xchgl %esp,%ebp
        pushl %edx
        next

// C  2>r  ( x1 x2 --   R: -- x1 x2 )  \ push 2 cells onto R-Stack
    head TWOTOR,3,"2>r",docode
        popl %edx
        xchgl %esp,%ebp
        pushl %edx
        pushl %ebx
        xchgl %esp,%ebp
        popl %ebx
        next

// =====MEMORY AND I/O OPERATIONS ===============
// =======DATA SPACE OPERATORS===================

// C  !  ( x a-addr --  )    \ store cell in memory
    head STORE,1,"!",docode
        popl %eax
        movl %eax,(%ebx)
        popl %ebx
        next

// Z  w!  (  word c-addr -- )    \ store word in memory
    head WSTORE,2,"w!",docode
        popl %eax
        movw %ax,(%ebx)
        popl %ebx
        next

// C  c!  ( char c-addr --  )   \  store char in memory
    head CSTORE,2,"c!",docode
        popl %eax
        movb %al,(%ebx)
        popl %ebx
        next

// Z  !+  ( addr u -- addr+1 )   \ address operator store and increment
    head STOREPLUS,2,"!+",docode
        popl %eax
        movl %ebx, (%eax)
        leal 4(%eax), %ebx
        next

// Z  w!+  ( w-addr n -- w-addr+1 )   \ address operator store and increment
    head WSTOREPLUS,3,"w!+",docode
        popl %eax
        movw %bx, (%eax)
        incl %eax
        incl %eax
        movl %eax, %ebx
        next

// Z  c!+   ( c-addr n -- c-addr+1 )    \ address operator store and increment
    head CSTOREPLUS,3,"c!+",docode
        popl %eax
        movb %bl, (%eax)
        incl %eax
        movl %eax, %ebx
        next

// C  @   ( a-addr -- x )       \ fetch cell from memory
    head FETCH,1,"@",docode
        movl (%ebx),%ebx
        next

// C  w@  ( a-addr -- x )       \ fetch cell from memory
    head WFETCH,2,"w@",docode
        movw (%ebx),%ax
        xorl %ebx,%ebx
        movw %ax,%bx
        next

// C  c@  ( c-addr -- char )    \ fetch char from memory
    head CFETCH,2,"c@",docode
        movb (%ebx),%al
        xorl %ebx,%ebx
        movb %al, %bl
        next
       
// Z  @+  ( addr -- addr+1 u )  \ address operator fetch and increment
    head FETCHPLUS,2,"@+",docode
        leal 4(%ebx), %eax
        pushl %eax
        movl (%ebx), %ebx
        next

// Z  w@+  ( w-addr -- w-addr+1 w )  \ word address operator fetch and increment
    head WFETCHPLUS,3,"w@+",docode
        leal 2(%ebx), %eax
        pushl %eax
        movw (%ebx), %ax
        xorl %ebx, %ebx
        movw %ax, %bx
        next

// Z  c@+  ( c-addr -- c-addr+1 c )  \ char address operator fetch and increment
    head CFETCHPLUS,3,"c@+",docode
        movl %ebx, %eax
        incl %ebx
        push %ebx
        xorl %ebx, %ebx
        movb (%eax), %bl
        next

// I/O OPERATIONS

// Z  p!  ( ud c-addr -- )   \  output DWORD to port
    head PSTORE,2,"p!",docode
        movl %ebx,%edx
        popl %eax
        outl %eax,%dx
        popl %ebx
        next

// Z  pw!  ( word c-addr -- )       \ output WORD to port
    head PWSTORE,3,"pw!",docode
        movl %ebx,%edx
        popl %eax
        outw %ax,%dx
        popl %ebx
        next

// Z  pc!  ( char c-addr --  )    \ output char to port
    head PCSTORE,3,"pc!",docode
        movl %ebx,%edx
        popl %eax
        outb %al,%dx
        popl %ebx
        next

// Z  p@   ( c-addr -- ud )     \ input DWORD from port
    head PFETCH,2,"p@",docode
        movl %ebx,%edx
        inl %dx,%eax
        movl %eax,%ebx
        next

// Z  pw@  ( c-addr -- word )    \ input WORD from port
    head PWFETCH,3,"pw@",docode
        movl %ebx,%edx
        inw %dx,%ax
        xorl %ebx,%ebx
        movw %ax,%bx
        next

// Z  pc@  ( c-addr -- char )    \ input char from port
    head PCFETCH,3,"pc@",docode
        movl %ebx,%edx
        inb %dx,%al
        xorl %ebx, %ebx
        movb %al,%bl
        next

// =====ARITHMETIC AND LOGICAL OPERATIONS =============

// C  +  ( x1 x2 -- x3 )    \  add; x1+x2
    head PLUS,1,"+",docode
        popl %eax
        addl %eax,%ebx
        next

// D  m+   ( xd1 x -- xd2 )    \ add single to  double
    head MPLUS,2,"m+",docode
        popl %edx
        popl %eax
        addl %ebx,%eax
        adcl $0, %edx
        push %eax
        movl %edx,%ebx
        next

// X  d+  ( xd1 xd2 -- xd3 )      \ add two doubles
    head DPLUS,2,"d+",docode
        xchgl 8(%esp), %ebx
        popl %eax
        addl %ebx, %eax
        popl %edx
        popl %ebx
        adcl %ebx, %edx
        push %eax
        movl %edx,%ebx
        next

// C  -  ( x1 x2 -- x3 )     \ subtract; x1-x2
    head MINUS,1,"-",docode
        popl %eax
        subl %ebx,%eax
        movl %eax,%ebx
        next

// C  and  ( x1 x2 -- x3 )    \ logical and
    head ANDD,3,"and",docode
        popl %eax
        andl %eax,%ebx
        next

// C  or  ( x1 x2 -- x3 )    \ logical or
    head ORR,2,"or",docode
        popl %eax
        orl %eax,%ebx
        next

// C  xor  ( x1 x2 -- x3  )   \ logical xor
    head XORR,3,"xor",docode
        popl %eax
        xorl %eax,%ebx
        next

// C  invert  ( x1 -- x2 )     \ bitwise inversion
    head INVERT,6,"invert",docode
        notl %ebx
        next

// C  negate  ( x1 -- x2 )     \ two's complement
    head NEGATE,6,"negate",docode
        negl %ebx
        next

// C  1+   ( x1 -- x2 )     \ add 1 to TOS
    head ONEPLUS,2,"1+",docode
        incl %ebx
        next

// C  1-  ( x1 -- x2 )   \ subtract 1 from TOS
    head ONEMINUS,2,"1-",docode
        decl %ebx
        next

// C  2*   ( x1 -- x2 )    \ arithmetic left shift
    head TWOSTAR,2,"2*",docode
        sall $1,%ebx
        next

// C  2/   ( x1 -- x2 )    \ arithmetic right shift
    head TWOSLASH,2,"2/",docode
        sarl $1,%ebx
        next

// C  lshift  ( x1 u -- x2 )  \ logical L shift u places
    head LSHIFT,6,"lshift",docode
        xchgl %ecx,%ebx      // count in CX, save old
        popl %eax
        shl  %cl,%eax
        movl %ebx,%ecx       // restore old CX
        movl %eax,%ebx       // shifted value
        next

// C  rshift  ( x1 u -- x2 )   \ logical right shift u places
    head RSHIFT,6,"rshift",docode
        xchgl %ecx,%ebx      // count to CX, saving old CX
        popl %eax
        shrl %cl,%eax
        movl %ebx,%ecx       // restore CX
        movl %eax,%ebx       // shifted value
        next

// C  arshift ( x1 u -- x2 )    \ arithmetic right shift u places
    head ARSHIFT,7,"arshift",docode
        xchgl %ecx,%ebx      // count to CX, saving old CX
        popl %eax
        sarl %cl,%eax
        movl %ebx,%ecx       // restore CX
        movl %eax,%ebx       // shifted value
        next

// C  +!  ( x a-addr -- )     \ increment memory by dword
    head PLUSSTORE,2,"+!",docode
        popl %eax
        addl %eax,(%ebx)
        popl %ebx
        next

// C  w+!  ( w a-addr -- )     \ increment memory by word
    head WPLUSSTORE,3,"w+!",docode
        popl %eax
        add  %ax,(%ebx)
        popl %ebx
        next
        
// C  c+!  ( c a-addr -- )     \ increment memory by byte
    head CPLUSSTORE,3,"c+!",docode
        popl %eax
        add  %al,(%ebx)
        popl %ebx
        next

// =============COMPARISON OPERATIONS ===================
// C  0=   ( x -- flag )    \ return true if TOS=0
    head ZEROEQUAL,2,"0=",docode
        subl $1,%ebx        // set cy if BX was 0
        sbbl %ebx,%ebx       // BX=-1 if cy set
        next

// C  0<>   ( x  -- flag )    \ return true if TOS !=0
    head ZERONEQUAL,3,"0<>",docode
        subl $1,%ebx       // set cy if BX was 0
        cmc        // complement cy
        sbbl %ebx,%ebx
        next

// C  0<   ( x -- flag  )   \ true if TOS negative
    head ZEROLESS,2,"0<",docode
        shll $1,%ebx        // set cy if BX negative
        sbbl %ebx,%ebx       // BX=-1 if cy set
        next

// C  0>   ( x -- flag  )    \ true if TOS negative
    head ZEROGREATER,2,"0>",docode
        subl $1,%ebx
        shll $1,%ebx        // set cy if BX negative
        cmc
        sbbl %ebx,%ebx       // BX=-1 if cy set
        next

// C  =   ( x1 x2 -- flag )   \ true if x1=x2
    head EQUAL,1,"=",docode
        popl %eax
        subl %ebx,%eax       // x1-x2 in AX, flags set
        jz tostrue
tosfalse: xorl %ebx,%ebx
        next

// X  <>  ( x1 x2 -- flag )  \ not equal  (not ANSI)
    head NOTEQUAL,2,"<>",docolon
        .int EQUAL,ZEROEQUAL,EXIT

// C  <   ( n1 n2 -- flag  )   \  signed less than 
    head LESS,1,"<",docode
        popl %eax
        subl %ebx,%eax       // n1-n2 in EAX, flags set
        jge tosfalse
tostrue: movl $0xffffffff,%ebx
        next

// C  >   ( n1 n2 -- flag )   \ signed  greater than
    head GREATER,1,">",docolon
        .int SWAP,LESS,EXIT

// C  u<  ( u1 u2 -- flag )   \  unsigned  less than
    head ULESS,2,"u<",docode
        popl %eax
        subl %ebx,%eax       // u1-u2 in AX, flags set
        jb tostrue
        jmp tosfalse

// X  u>   ( u1 u2 -- flag  )   \  unsigned  grater than (not ANSI)
    head UGREATER,2,"u>",docolon
        .int SWAP,ULESS,EXIT

// LOOP AND BRANCH OPERATIONS ====================

// Z  branch  ( --  )  \ branch always
    head BRANCH,6,"branch",docode
dobranch: movl (%esi),%esi
        next

// Z  ?branch  ( x -- )  \ branch if TOS zero
    head QBRANCH,7,"?branch",docode
        orl %ebx,%ebx
        popl %ebx
        jz dobranch
        leal 4(%esi), %esi 
        next

// H (do)    n1|u1 n2|u2 --  R: -- sys1 sys2
// H run-time code for DO
// '83 and ANSI standard loops terminate when the
// boundary of limit-1 and limit is crossed, in
// either direction.  This can be conveniently
// implemented by making the limit 80000000h, so that
// arithmetic overflow logic can detect crossing.
// I learned this trick from Laxen & Perry F83.
// fudge factor = 80000000h-limit, to be added to
// the start value.
// be wary of o0 0 do .... loop
//    head XDO,4,"(do)",docode
    nohead XDO,docode
        popl %eax          // EBX=index, EAX=limit
        xchgl %esp,%ebp      // push old index on R.Stk
        pushl %ecx
        movl $0x80000000,%ecx    // compute 80000000h-limit
        subl %eax,%ecx       //  "fudge factor"
        pushl %ecx         // push on return stack
        addl %ebx,%ecx       // loop ctr = index+fudge
        xchgl %esp,%ebp
        popl %ebx          // pop new TOS
        next

// H (loop)   R: sys1 sys2 --  | sys1 sys2
// H run-time code for LOOP
// Add 1 to the loop index.  If loop terminates,
// clean up the return stack and skip the branch.
// Else take the inline branch.  Note that LOOP
// terminates when index=80000000h.
//    head XLOOP,6,"(loop)",docode
    nohead XLOOP,docode
        addl $1,%ecx
        jno dobranch    // no overflow = loop
        leal 4(%esi), %esi          // overflow = loop done
        movl 4(%ebp),%ecx   // restore old index 
        addl $8,%ebp        // clean up
        next

// H (+loop)   n --   R: sys1 sys2 --  | sys1 sys2
// H                        run-time code for +LOOP
// Add n to the loop index.  If loop terminates,
// clean up the return stack and skip the branch.
// Else take the inline branch.
//    head XPLUSLOOP,7,"(+loop)",docode
    nohead XPLUSLOOP,docode
        addl %ebx,%ecx
        popl %ebx
        jno dobranch    // no overflow = loop
        leal 4(%esi),%esi          // overflow = loop done
        movl 4(%ebp),%ecx   // restore old index
        addl $8,%ebp        // clean up
        next

// C  i  ( -- n   R: sys1 sys2 -- sys1 sys2 )  \ get the innermost loop index
    head II,1,"i",docode
        pushl %ebx         // push old TOS
        movl %ecx,%ebx       // index=loop ctr-fudge
        subl (%ebp),%ebx
        next

// C  j  ( -- n   R: 4*sys -- 4*sys )  \ get the second loop index
    head JJ,1,"j",docode
        pushl %ebx         // push old TOS
        movl 4(%ebp),%ebx   // outer loop ctr
        subl 8(%ebp),%ebx   //   - outer fudge
        next

// C  unloop  ( --   R: sys1 sys2 -- )   \ drop loop parms
    head UNLOOP,6,"unloop",docode
        movl 4(%ebp),%ecx   // restore old index
        addl $8,%ebp
        next

// =====MULTIPLY AND DIVIDE==================

// C  um*   ( u1 u2 -- ud )    \ unsigned 32x32->64 mult.
    head UMSTAR,3,"um*",docode
        popl %eax
        mull %ebx   // FIXME
        pushl %eax
        movl %edx,%ebx
        next

// C  um/mod   ( ud u1 -- u2 u3 )   \ unsigned 64/32->32
    head UMSLASHMOD,6,"um/mod",docode
        popl %edx
        popl %eax
        divl %ebx
        pushl %edx         // remainder
        movl %eax,%ebx       // quotient
        next

// C  sm/rem  ( d u1 -- u2 u3  )   \ signed 64/32->32
    head SMSLASHREM,6,"sm/rem",docode
        popl %edx
        popl %eax
        idivl %ebx
        pushl %edx         // remainder
        movl %eax,%ebx       // quotient
        next

// BLOCK AND STRING OPERATIONS ===================

// C  fill   ( c-addr u char -- )    \  u >0 fill memory with char
    head FILL,4,"fill",docode
        movl (%esp), %eax
        testl %eax, %eax
        je  nofill
        js  nofill
        xchgl %esp,%ebp      // save regs
        pushl %edi
        pushl %ecx
        xchgl %esp,%ebp
        movl %ebx,%eax       // character  FIXME
        popl %ecx          // count
        popl %edi          // address
        jcxz filldone
        rep stosb
filldone:
    xchgl %esp,%ebp      // restore regs
        popl %ecx
        popl %edi
        xchgl %esp,%ebp
        popl %ebx          // new TOS
        next
nofill: addl $0x8, %esp      // less than 1 char to fill
        popl %ebx
        next
// X  cmove   ( c-addr1 c-addr2 u -- )     \ move from bottom
// as defined in the ANSI optional String word set
// On byte machines, CMOVE and CMOVE> are logical
// factors of MOVE.  They are easy to implement on
// CPUs which have a block-move instruction.
    head CMOVE,5,"cmove",docode
segmove: xchgl %esp,%ebp     // save regs
        pushl %edi
        pushl %esi
        pushl %ecx
        xchgl %esp,%ebp
        movl %ebx,%ecx       // count
        popl %edi          // dest adr
        popl %esi          // source adr
        jcxz cmovedone
        rep movsb
cmovedone:
    xchgl %esp,%ebp      // restore regs
        popl %ecx
        popl %esi
        popl %edi
        xchgl %esp,%ebp
        popl %ebx          // new TOS
        next

// X  cmove>  ( c-addr1 c-addr2 u --  )   \ move from top
// as defined in the ANSI optional String word set
    head CMOVEUP,6,"cmove>",docode
        xchgl %esp,%ebp      // save regs
        pushl %edi
        pushl %esi
        pushl %ecx
        xchgl %esp,%ebp
        movl %ebx,%ecx       // count
        popl %edi          // dest adr
        popl %esi          // source adr
        jcxz cmovudone
        decl %ebx          // count-1
        addl %ebx,%edi       // last byte in dest
        addl %ebx,%esi       // last byte in source
        std             // work backwards
        rep movsb
        cld
cmovudone:
    xchgl %esp,%ebp      // restore regs
        popl %ecx
        popl %esi
        popl %edi
        xchgl %esp,%ebp
        popl %ebx          // new TOS
        next

// Z  skip   ( c-addr u c -- c-addr' u' )   \  skip matching chars
// Although SKIP, SCAN, and S= are perhaps not the
// ideal factors of WORD and FIND, they closely
// follow the string operations available on many
// CPUs, and so are easy to implement and fast.
    head SKIP,4,"skip",docode
    xchgl %esp,%ebp     // save regs
        pushl %edi
        pushl %ecx
        xchgl %esp,%ebp
        movl %ebx,%eax       // char
        popl %ecx          // count
        popl %edi          // adr
        jcxz skipdone
        repe scasb
        jz skipdone     // CX=0, DI ok.
        incl %ecx          // mismatch found,
        decl %edi          //  adjust CX,DI
skipdone:
        pushl %edi         // updated adr
        movl %ecx,%ebx       // updated count
    xchgl %esp,%ebp      // restore regs
        popl %ecx
        popl %edi
        xchgl %esp,%ebp
        next

// Z  scan   ( c-addr u c -- c-addr' u' )   \ find matching char
    head SCAN,4,"scan",docode
    xchgl %esp,%ebp     // save regs
        pushl %edi
        pushl %ecx
        xchgl %esp,%ebp
        movl %ebx,%eax       // char
        popl %ecx          // count
        popl %edi          // adr
        jcxz scandone
        repne scasb
        jnz scandone    // CX=0, DI ok.
        incl %ecx          // match found,
        decl %edi          //  adjust CX,DI
scandone:
        pushl %edi         // updated adr
        movl %ecx,%ebx       // updated count
    xchgl %esp,%ebp      // restore regs
        popl %ecx
        popl %edi
        xchgl %esp,%ebp
        next

// Z  s=   ( c-addr1 c-addr2 u -- -n|0|+n ) \ string compare
//             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2a
    head SEQUAL,2,"s=",docode
segcomp: xchg %esp,%ebp     // save regs
        pushl %edi
        pushl %esi
        pushl %ecx
        xchgl %esp,%ebp
        movl %ebx,%ecx       // count
        popl %edi          // dest adr
        popl %esi          // source adr
        xorl %ebx,%ebx       // preclear BX
        jcxz sequdone
        repe cmpsb
        jz sequdone
        // not equal strings: CY set if DI<SI
        // if CY set, return +1, else return -1
        roll $1,%ebx
        roll $1,%ebx
        decl %ebx
sequdone:
        xchgl %esp,%ebp      // restore regs
        popl %ecx
        popl %esi
        popl %edi
        xchgl %esp,%ebp
        next

#include "camel86_32d_1.0.S"   // CPU Dependencies
#include "camel86_32h_1.0.S"   // High Level words
#include "camel86_32o_1.0.S"   // OS specific words

.set ilatest, link
.section .data
.global saved_esp, root_wid
saved_esp:
    .int 0
sys_term:
    .int 0
sys_pausev:
    .int PNULL
sys_runv:
    .int BRRUN
sys_handler:
    .int 0
sys_ntask:
    .int 1
root_wid:
    .int 0
lastcfa:
    .int ilatest
sys_scr:
    .int 0
usr_stringbuf:
    .space 0xff
usr_string0buf:
    .space 0xff
sys_filebuf:
    .space 0xff
#ifdef LINUX
topofmemory:
    .int 0
#endif
#ifdef COREBOOT

.global mybuff,auto_start,mybuff_ptr
.global irq_count,irq_vectors,isr_vectors
mybuff:
    .int 0
mybuff_ptr:
    .int mybuff
auto_start:
    .int 0
keyv_save:
    .int 0
keyqv_save:
    .int 0
istack_ptr:            // FIXME coreoot stack @ 0x11b000 
    .int _estack       // 
irq_count:
    .fill 0x10,4,0x00
irq_vectors:
    .int irq_default,irq_default,irq_default,irq_default
    .int irq_default,irq_default,irq_default,irq_default
    .int irq_default,irq_default,irq_default,irq_default
    .int irq_default,irq_default,irq_default,irq_default

isr_vectors:
    .int isr_default,isr_default,isr_default,isr_default
    .int isr_default,isr_default,isr_default,isr_default
    .int isr_default,isr_default,isr_default,isr_default
    .int isr_default,isr_default,isr_default,isr_default
    .int isr_default,isr_default,isr_default,isr_default
    .int isr_default,isr_default,isr_default,isr_default
    .int isr_default,isr_default,isr_default,isr_default
    .int isr_default,isr_default,isr_default,isr_default

irq_default:
    .int irq_exit
isr_default:
    .int docolon,LIT,-10,THROW,isr_exit
#endif
//   PAD area
    .space 0x80
    .global User0
User0:
    .space 0xffff

.global ramdict
.section .bss
ramdict:
    .int 0
#ifdef LINUX
    .space 0x4000000  // dictionary starts here !
#endif

