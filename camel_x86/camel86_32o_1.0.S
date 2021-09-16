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
//  CAMEL86_32O.S: 
//    Source code is for GNU Assembler.
//    Forth words are documented as follows:
// x   NAME     stack -- stack    description
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
//  ===============================================
//  REVISION HISTORY
//  v2011.001
//  ===============================================
#ifdef LINUX
// lifted from jonesforth
    .set DATA_SEG_SIZE, 0x1000000
    .section .text
#include <asm/unistd.h>
    
set_up_data_segment:
        xorl %ebx,%ebx      
        movl $__NR_brk,%eax
        int $0x80
        movl %eax,ramdict             // init here aka dp
        addl $DATA_SEG_SIZE,%eax
        movl %eax,%ebx      
        movl $__NR_brk,%eax          // request space on heap
        int $0x80
        movl %eax, topofmemory
        ret

// Z  l-emit  ( c  --  )  \ output char to stdout
    head LEMIT,6,"l-emit",docode
        pushl %edx
        pushl %ecx
        pushl %ebp
        movl %esp, %eax
        movl (saved_esp), %esp
        movl %eax, saved_esp
        pushl %ebx
        call C_emit
        popl %ebx              // clean up stack frame
        movl %esp, %eax
        movl (saved_esp), %esp
        movl %eax,saved_esp
        popl %ebp             // restore save registers
        popl %ecx
        popl %edx
        popl %ebx            // set TOS
        next

// Z  l-key  ( -- c )  \  get keyboard character
    head LKEY,5,"l-key",docolon
LKEY1:  .int PAUSE,LKEYQ,QBRANCH,LKEY1
        .int BRLKEY,EXIT

// Z  (l-key)  ( -- c  )  \ get character from keyboard
    nohead BRLKEY,docode
        pushl %ebx      // save TOS
        pushl %edx
        pushl %ecx
        pushl %ebp

/*      movl %esp, %eax
        movl (saved_esp), %esp
        movl %eax, saved_esp 
*/

        movl $saved_esp ,%eax
        xchgl %esp, (%eax)
        call C_key

/*      movl %esp, %ebx
        movl (saved_esp), %esp
        movl %ebx,saved_esp */

        movzbl %al, %ebx 
        movl $saved_esp ,%eax
        xchgl %esp, (%eax)
        popl %ebp
        popl %ecx
        popl %edx
        next

// Z  l-key?  ( -- flg )  \ check for keyboard char
    head LKEYQ,6,"l-key?",docode
        push %ebx
        pushl %edx
        pushl %ecx
        pushl %ebp

        movl $saved_esp ,%eax
        xchgl %esp, (%eax)
        call C_keyquery
        movl %eax, %ebx 
        movl $saved_esp ,%eax
        xchgl %esp, (%eax)

        popl %ebp
        popl %ecx
        popl %edx
        next

// Z  restore-tty  (  --  )  \ restore terminal settings
    head RESTORETTY,11,"restore-tty",docode
        call restore_term
        next
    .extern free,malloc,realloc

// M  allocate  ( u -- u )  \  allocate memory from heap
    head MALLOCATE,8,"allocate",docode
        pushl %edx
        pushl %ecx
        pushl %ebp

        movl $saved_esp ,%eax
        xchgl %esp, (%eax)

        push %ebx
        call malloc
        movl %eax, %ebx 
        movl $saved_esp ,%eax
        xchgl %esp, (%eax)

        popl %ebp
        popl %ecx
        popl %edx
        pushl %ebx
        xorl %ebx,%ebx
        next

// M  free  ( u --  )  \ free malloc heap memory
    head MFREE,4,"free",docode
        pushl %edx
        pushl %ecx
        pushl %ebp

        movl $saved_esp ,%eax
        xchgl %esp, (%eax)

        push %ebx
        call free
        movl %eax, %ebx 
        movl $saved_esp ,%eax
        xchgl %esp, (%eax)

        popl %ebp
        popl %ecx
        popl %edx
        xorl %ebx, %ebx
        next

// M  resize  ( x u -- )  \  reallocate memory from heap
    head MRESIZE,6,"resize",docode
        pushl %edx
        pushl %ecx
        pushl %ebp
        movl 12(%esp),%ecx
        movl $saved_esp ,%eax
        xchgl %esp, (%eax)
        
        pushl %ebx
        pushl %ecx        
        call realloc
        addl $8,%esp
        movl %eax, %ebx 
        movl $saved_esp ,%eax
        xchgl %esp, (%eax)

        popl %ebp
        popl %ecx
        popl %edx
        popl %eax
        pushl %ebx
        xorl %ebx,%ebx
        next

.macro BEGIN_syscall
        movl    $saved_esp ,%eax       // save forth stack
        xchgl   %esp, (%eax)           // switch to system stack
        pushl   %ebp
        pushl   %edi
        pushl   %esi
        pushl   %edx
        pushl   %ecx
        movl    $saved_esp ,%eax
        xchgl   %esp, (%eax)
        movl    %ebx,%eax   /* sys_call number */
.endm

// Z  syscall6  (   x6 ... x1 n --  flg )  \ LINUX syscall 6
//
    head SYSCALL6,8,"syscall6",docode
        BEGIN_syscall
        popl    %ebx            /* first argument */
        popl    %ecx            /* second argument */
        popl    %edx            /* third argument */
        popl    %esi            /* fourth argument */
        popl    %edi            /* fifth argument */
        popl    %ebp            /* sixth argument */
        jmp __syscall

// Z  syscall5  ( x5 ... x1 n -- flg )  \ LINUX syscall 5
//
    head SYSCALL5,8,"syscall5",docode
        BEGIN_syscall
        popl    %ebx            /* first argument */
        popl    %ecx            /* second argument */
        popl    %edx            /* third argument */
        popl    %esi            /* fourth argument */
        popl    %edi            /* fifth argument */
        jmp __syscall

// Z  syscall4  ( x4 ... x1 n --  flg )  \ LINUX syscall 
//
    head SYSCALL4,8,"syscall4",docode
        BEGIN_syscall
        popl    %ebx            /* first argument */
        popl    %ecx            /* second argument */
        popl    %edx            /* third argument */
        popl    %esi            /* fourth argument */
        jmp __syscall

// Z  syscall3  ( x3 x2 x1 n -- flg )  \ LINUX syscall  
//
    head SYSCALL3,8,"syscall3",docode
        BEGIN_syscall
        popl    %ebx            /* first argument */
        popl    %ecx            /* second argument */
        popl    %edx            /* third argument */
        jmp __syscall

// Z  syscall2  ( x2 x1 n --  flg )  \ LINUX syscall 
//
    head SYSCALL2,8,"syscall2",docode
        BEGIN_syscall
        popl    %ebx            /* first argument */
        popl    %ecx            /* second argument */
        jmp __syscall

// Z  syscall1 ( x1 n --  flg )
    head SYSCALL1,8,"syscall1",docode
        BEGIN_syscall
        popl    %ebx            /* first argument */
        jmp __syscall


// Z  syscall0  ( n -- flg )
//
    head SYSCALL0,8,"syscall0",docode
        BEGIN_syscall
    __syscall:
        int $0x80           /* call Linux */
        cld
        movl    %eax,%ebx
        movl    $saved_esp ,%eax
        xchgl   %esp, (%eax)
        popl    %ecx
        popl    %edx
        popl    %esi
        popl    %edi
        popl    %ebp
        movl    $saved_esp ,%eax
        xchgl   %esp, (%eax)
        next

// Z  length  ( addr0 -- u )  \ length of aciiz string
// 0 begin
//   2dup + c@ 0 <> while
//   dup h# 100 < while
//        1+
//   repeat then
// nip ;
    head ZLENGTH,6,"length",docolon
        .int LIT,0X0
ZLEN1:  .int TWODUP,PLUS,CFETCH,LIT,0X0
        .int NOTEQUAL,QBRANCH,ZLEN2
        .int DUPE,LIT,0x100,LESS,QBRANCH,ZLEN2
        .int ONEPLUS
        .int BRANCH,ZLEN1
ZLEN2:  .int NIP,EXIT

// Z  c>zstr  ( c-addr u 0-addr -- )  \ counted to ascii zero string
//  swap 2dup + >r move 0 r> c! ;
    head CTOZSTR,6,"c>zstr",docolon
        .int SWAP,TWODUP,PLUS,TOR,MOVE,LIT,0X0
        .int RFROM,CSTORE,EXIT

// Z  z>cstr  ( 0-addr addr --  )  \ ascii zero to counted string
//  over length  \  a c u
//  h# 0ff min
//  tuck over c! \  a u c
//  1+ swap move ;
    head ZTOCSTR,6,"z>cstr",docolon
        .int OVER,ZLENGTH,LIT,0XFF,MIN
        .int TUCK,OVER,CSTORE
        .int ONEPLUS,SWAP,MOVE,EXIT

// seek_set 0   from start
// seek_cur 1   from current  
// seek_end 2   from end
// seek_data 3

// Z  tom  ( -- u )   \ top of memory
    head TOPMEM,3,"tom",docolon
        .int LIT,topofmemory,FETCH,EXIT

// H  (ior)   n -- n ior
// ior == 0 means success
// dup dup 0< if
//      negate
// else
//    drop 0
// then ;
    nohead BRIOR,docolon
        .int DUPE,DUPE,ZEROLESS,QBRANCH
        .int BRIOR1,NEGATE,EXIT
BRIOR1: .int DROP,LIT,0,EXIT

// H  (createfile)   c-addr u fam  -- fid flg
//  >r c>nstr 5 syscall3 (brior)
    nohead BRCREATEFILE,docolon
        .int TOR,HERE,CTOZSTR,HERE,LIT,420,RFROM
        .int ROT,LIT,5,SYSCALL3
        .int BRIOR,EXIT

// H  (deletefile)   c-addr u --ior
//   c>nstr 10 syscall1 ;
    nohead BRDELETEFILE,docolon
        .int HERE,CTOZSTR,HERE,LIT,10,SYSCALL1
        .int BRIOR,NIP,EXIT


// H  (openfile)   c-addr u fam -- fid flg
//  rot rot c>nstr 5 syscall2 (brior)
    nohead BROPENFILE,docolon
        .int ROT,ROT,HERE,CTOZSTR,HERE
        .int LIT,5,SYSCALL2
        .int BRIOR,EXIT

// H  (closefile) fid -- ior
//
    nohead BRCLOSEFILE,docolon
        .int LIT,6,SYSCALL1
        .int BRIOR,NIP,EXIT

// H  (writefile) u c-addr fid -- ior
//
    nohead BRWRITEFILE,docolon
        .int LIT,4,SYSCALL3
        .int BRIOR,NIP,EXIT

// H  (readfile)  u c-addr fid -- u ior
//
    nohead BRREADFILE,docolon
        .int LIT,3,SYSCALL3
        .int BRIOR,EXIT

// H  (filesize) fid -- ud ior
// dup r> 
// seek_cur 0 r@ 19 syscall3 dup 0< \ u1 ior1
// seek_end 0 r@ 19 syscall3 dup 0< \ u1 ior1 u2 ior2
// rot or rot                 \ u2 ior3 u1
// seek_set swap r> 19 syscall3  \ u2 ior3 ior
// 0< or swap s>d rot ;          \ ud ior
    nohead BRFILESIZE,docolon
        .int TOR
        .int LIT,1,LIT,0,RFETCH,LIT,19,SYSCALL3
        .int DUPE,ZEROLESS
        .int LIT,2,LIT,0,RFETCH,LIT,19,SYSCALL3
        .int DUPE,ZEROLESS,ROT,ORR,ROT
        .int LIT,0,SWAP,RFROM,LIT,19,SYSCALL3
        .int ZEROLESS,ORR,SWAP,STOD,ROT,EXIT

// H  (fileposition)  fileid -- ud ior
//  >r seek_cur 0 r> 19 syscall3
//  (ior) >r s>d r> ;
    nohead BRFILEPOSITION,docolon
        .int TOR,LIT,1,LIT,0,RFROM
        .int LIT,19,SYSCALL3,BRIOR
        .int TOR,LIT,0,RFROM,EXIT

// H  (repositionfile)      ud fileid -- ior
// >r drop seek_set swap r> 19 syscall3
    nohead BRREPOSITIONFILE,docolon
        .int TOR,DROP,LIT,0,SWAP,RFROM
        .int LIT,19,SYSCALL3,BRIOR,NIP,EXIT

// H  (renamefile) c-addr u c-addr u -- ior     \  new-name old-name
// c>nstr rot rot c>nstr 38 syscall2
    nohead BRRENAMEFILE,docolon
        .int HERE,DUPE,TOR,CTOZSTR
        .int HERE,LIT,0x100,PLUS,DUPE,TOR,CTOZSTR
        .int RFROM,RFROM
        .int LIT,38,SYSCALL2
        .int BRIOR,EXIT

// H  (lseek) fid u type -- ior
// returns -1
//    nohead BRLSEEKFILE,docolon
//  .int LIT,19,SYSCALL3
//  .int BRIOR,EXIT

#endif
// LINUX 



#ifdef COREBOOT
// H  keym ( -- ch read from memory buffer )
// mybuff# @ dup 1a = if
//    drop key_save @ keyv !
//    keyqv_save @ keyqv ! key
// else
//    1 mybuff# +!
// then
    nohead KEYM,docolon
        .int LIT,mybuff_ptr,FETCH,CFETCH,DUPE,LIT,0x1a,EQUAL
        .int QBRANCH,KEYM1
KEYM2:  .int XSQUOTE
        .byte 0x15
        .ascii "End of Auto-Start.."
        .byte 0x0d,0x0a
        .int TYPE,DROP,IOSTORE,KEY,EXIT
//        .int TYPE,DROP,LIT,keyv_save,FETCH,KEYV,STORE
//        .int LIT,keyqv_save,FETCH,KEYQV,STORE
//        .int KEY,EXIT
KEYM1:  .int LIT,1,LIT,mybuff_ptr,PLUSSTORE,EXIT

// H  keyqm
    nohead KEYQM,docolon
        .int TRUE,EXIT

// H  (cold)  --     startup  routine
//  auto-start @ if
//     clear auto-start
//      redirect keyv keyqv and emitv
//  then

    nohead BRCOLD,docolon
        .int LIT,auto_start,FETCH,QBRANCH,BCOLD1
        .int XSQUOTE
        .byte 0x14
        .ascii "Loading Runtime..."
        .byte 0x0d,0x0a
        .int TYPE,LIT,0,LIT,auto_start,STORE
        .int LIT,mybuff,FETCH,LIT,mybuff_ptr,STORE
        .int KEYV,FETCH,LIT,keyv_save,STORE
        .int LIT,KEYM,KEYV,STORE
        .int KEYQV,FETCH,LIT,keyqv_save,STORE
        .int LIT,KEYQM,KEYQV,STORE
BCOLD1: .int EXIT

    head RESTART,7,"restart",docode
        movl $0, auto_start
        movl saved_esp,%esp
        jmp _start
    
//  *******************************************
// high level interrupts
//  usage :  isr: <name> high-level forth words  isr;
//           irq: <name> high-level forth words irq;


// Z  irq:  ( "name " --  )  \ define interrupt service
//   : ;  \ alias for colon
    head IRQDEFINE,4,"irq:",docolon
        .int COLON,EXIT

// Z  irq;  ( --  )  \  end irq definition
//  reveal here 1 cells + , lit irq-exit ,  [ ;
//  dump address of next dword followed by address of irq-exit!
    head IRQENDDEF,4,"irq;",docolon,1 
        .int REVEAL,HERE,LIT,1,CELLS,PLUS,COMMA
        .int LIT,irq_exit,COMMA
        .int LEFTBRACKET,EXIT

// Z  isr:  ( "name " -- )  \ define exception service
//   : ;  \ alias for colon
    head ISRDEFINE,4,"isr:",docolon
        .int COLON,EXIT

// Z  isr;  ( --  )  \ end isr definition
//  reveal here 1 cells + , lit isr-exit ,  [ ;
//  dump address of next dword followed by address of irq-exit!
    head ISRENDDEF,4,"isr;",docolon,1 
        .int REVEAL,HERE,LIT,1,CELLS,PLUS,COMMA
        .int LIT,isr_exit,COMMA
        .int LEFTBRACKET,EXIT

// Z  irq-count  ( n -- u )  \ return count of irq n activations
//   cells 'irq-count + @ ;
    head IRQCOUNT,9,"irq-count",docolon
        .int CELLS,TICKIRQCOUNT,PLUS,FETCH,EXIT

// Z  irq-set  ( xt n -- )  \  set interrupt n  handler
//   cells 'irq-vectors + ! ;
    head IRQSET,7,"irq-set",docolon
        .int CELLS,TICKIRQVECS,PLUS,STORE,EXIT

// Z  irq-get  ( n -- xt )  \  get interrupt n  handler
//   cells 'irq-vectors + @ ;
    head IRQGET,7,"irq-get",docolon
        .int CELLS,TICKIRQVECS,PLUS,FETCH,EXIT

// Z  isr-set  ( xt n --  )   \ set exception n handler
//   cells 'isr-vectors + ! ;
    head ISRSET,7,"isr-set",docolon
        .int CELLS,LIT,isr_vectors,PLUS,STORE,EXIT

// Z  isr-get  ( n -- xt )  \  get exception n handler
//   cells 'isr-vectors + @ ;
    head ISRGET,7,"isr-get",docolon
        .int CELLS,LIT,isr_vectors,PLUS,FETCH,EXIT

// H (ior) -- ior     io result codes
//  dup dup 0< 
//  if invert else drop 0 then
//  ;        
    nohead BRIOR,docolon
        .int DUPE,DUPE,ZEROLESS,QBRANCH
        .int BRIOR1,NEGATE,EXIT
BRIOR1: .int DROP,LIT,0,EXIT

// H  (createfile)   c-addr u fam  -- fid flg
//  rot rot c>nstr 5 syscall3 (brior)  ;
    nohead BRCREATEFILE,docolon
        .int TWODROP,EXIT

// H  (openfile)   c-addr u fam -- fid flg
//  rot rot c>nstr 5 syscall1 (brior)
    nohead BROPENFILE,docolon
        .int TWODROP,EXIT

// H  (closefile) fid -- ior
    nohead BRCLOSEFILE,docolon
        .int EXIT

// H  (writefile) u c-addr fid -- ior
    nohead BRWRITEFILE,docolon
        .int TWODROP,EXIT

// H  (readfile)  u c-addr fid -- u ior
    nohead BRREADFILE,docolon
        .int TWODROP,EXIT

// H  (filesize) fid -- ud ior
// dup r> 
// seek_cur 0 r@ 19 syscall3 dup 0< \ u1 ior1
// seek_end 0 r@ 19 syscall3 dup 0< \ u1 ior1 u2 ior2
// rot or rot                 \ u2 ior3 u1
// seek_set swap r> 19 syscall3  \ u2 ior3 ior
// 0< or swap s>d rot ;          \ ud ior
    nohead BRFILESIZE,docolon
        .int DUPE,EXIT

// H  (lseek) fid u type -- ior
//
//    nohead BRLSEEKFILE,docolon
//  .int TWODROP,EXIT
    nohead BRDELETEFILE,docolon
        .int BRIOR,EXIT

    nohead BRFILEPOSITION,docolon
        .int BRIOR,EXIT

    nohead BRREPOSITIONFILE,docolon
        .int BRIOR,EXIT

    nohead BRRENAMEFILE,docolon
        .int BRIOR,EXIT
#endif
// COREBOOT
