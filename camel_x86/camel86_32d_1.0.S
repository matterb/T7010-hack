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
//  CAMEL86_32d.S: Code Primitives
//    Source code is for GNU Assembler.
//    Forth words are documented as follows:
// x  NAME  (stack -- stack )  \ description
//    where x=C for ANS Forth Core words,
//           =X for ANS Extensions, 
//           =Z for internal or private words.
//  Guide to stack diagrams:  
//	    R: = return stack,
//  	 c = 8-bit character, 
//	  flag = boolean (0 or -1), 
//       n = signed 32-bit, 
//	     u = unsigned 32-bit,
//  	 d = signed 64-bit, 
//	    ud = unsigned 64-bit,
//      +n = unsigned 32-bit, 
//	     x = any cell value, 
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
//	%esi = IP	Forth Interpreter pointer
//	%edi = UP	User pointer
//	%esp = PSP	Parameter Stack Pointer
//	%ebp = RSP	Return Stack Pointer
//	%eax = W	work register, scratch
//	%ebx = TOS	Top of Parameter Stack
//	%ecx = loop index
//	%edx = scratch
//
//  ===============================================
//  REVISION HISTORY
//  v2011.001
//  ===============================================
// =============ALIGNMENT AND PORTABILITY OPERATORS
//  Many of these are synonyms for other words,
//  and so are defined as CODE words.
// C  align  ( -- )  \ align HERE
    head ALIGNN,5,"align",docode
    .global noop
noop:   next

// C  aligned  ( addr -- a-addr )  \ align given addr
    head ALIGNED,7,"aligned",docode
        movl $noop, %eax
        jmp *%eax

// Z  (aligned)  ( addr n -- a-addr )  \ align address on n byte boundary
    head TALIGNED,9,"(aligned)",docode
        decl %ebx
        popl %eax
        addl %ebx, %eax
        notl %ebx
        andl %eax, %ebx
        next

// Z  aligned-w  ( addr -- a-addr )  \ align on word address boundary
    head  WALIGNED,9,"aligned-w",docolon
        .int LIT,2,TALIGNED,EXIT

// Z  aligned-dw  ( addr -- a-addr )  \ align on dword address boundary
    head  DWALIGNED,10,"aligned-dw",docolon
        .int LIT,4,TALIGNED,EXIT

// Z  aligned-qw  ( addr -- a-addr )  \ align on qword address boundary
    head  QWALIGNED,10,"aligned-qw",docolon
        .int LIT,16,TALIGNED,EXIT

// Z  aligned-pg  ( addr -- a-addr )  \ align on page boundary ( 4KB )
    head  PGALIGNED,10,"aligned-pg",docolon
        .int LIT,4096,TALIGNED,EXIT

// Z  cell  ( -- n )  \ size of one cell in bytes ( addressable units)
    head CELL,4,"cell",docon
        .int 0x04

// C  cell+  ( a-addr1 -- a-addr2 )  \  add cell size
//    4 + ; 
    head CELLPLUS,5,"cell+",docode
        addl $4, %ebx
        next

// C  cells  ( n1 -- n2 )  \ cells->address units
//    4 * i ;
    head CELLS,5,"cells",docode
        sall $2, %ebx
    next
        
// C  char+  ( c-addr1 -- c-addr2 )  \   add char size
    head CHARPLUS,5,"char+",docode
        movl $ONEPLUS, %eax
        jmp *(%eax)

// C  chars  ( n1 -- n2 )  \ chars->address units
    head CHARS,5,"chars",docode
        movl $noop, %eax
        jmp *%eax

// C  >body  ( xt -- a-addr  )  \ address of PARAMETER FIELD
//    4 + ;                    
    head TOBODY,5,">body",docolon
        .int LIT,4,PLUS,EXIT    

// X  compile,  ( xt -- )  \ append execution token
//  I called this word ,XT before I discovered that
//  it is defined in the ANSI standard as COMPILE,.
//  On a DTC Forth this simply appends xt (like , )
//  but on an STC Forth this must append 'CALL xt'.
    head COMMAXT,8,"compile,",docode
        movl $COMMA, %eax
        jmp *(%eax)

// H  !cf  ( adrs cfa -- )  \ set code action of a word
//    ! ;
//   head STORECF,3,"!cf",docode
   nohead STORECF,docode
    movl $STORE, %eax
    jmp *(%eax)

// H  ,cf  ( adrs -- )  \ append a code field
//     , ;
//   head COMMACF,3,",cf",docode
   nohead COMMACF,docode
    movl $COMMA, %eax
    jmp *(%eax)

// H  ,call  ( adrs -- )  \ append a subroutine CALL
//    HERE 0xe8 OVER  c!
//    TUCK 5 + - SWAP 1+ !
//    5 ALLOT ;
//    head COMMACALL,5,",call",docolon
    nohead COMMACALL,docolon
        .int HERE
        .int LIT,0xE8,OVER,CSTORE
        .int TUCK,LIT,5,PLUS,MINUS
        .int SWAP,ONEPLUS,STORE
        .int LIT,5,ALLOT,EXIT

// H  !colon   --      change code field to docolon
//    -4 ALLOT docolon-adrs ,CF ;
//  This should be used immediately after CREATE.
//  This is made a distinct word, because on an STC
//  Forth, colon definitions have no code field.
//    head STORCOLON,6,"!colon",docolon
    nohead STORCOLON,docolon
        .int LIT,-4,ALLOT,HERE,LIT,lastcfa,STORE
        .int LIT,docolon,COMMACF,EXIT

// H  ,exit    --      append hi-level EXIT action
//    ['] EXIT ,XT ; 
//  This is made a distinct word, because on an STC
//  Forth, it appends a RET instruction, not an xt.
//    head CEXIT,5,",exit",docolon
    nohead CEXIT,docolon
        .int LIT,EXIT,COMMAXT,EXIT

// ==============CONTROL STRUCTURES ====================
//  These words allow Forth control structure words
//  to be defined portably.

// H  ,branch   xt --    append a branch instruction
//  xt is the branch operator to use, e.g. qbranch
//  or (loop).  It does NOT append the destination
//  address.  On the 80x86 this is equivalent to ,XT
//    head COMMABRANCH,7,",branch",docode
    nohead COMMABRANCH,docode
        movl $COMMAXT, %eax
        jmp *(%eax)

// H  ,dest   dest --        append a branch address
//  This appends the given destination address to
//  the branch instruction. On the 80x86 this is ','
//  ...other CPUs may use relative addressing.
//    head COMMADEST,5,",dest",docode
    nohead COMMADEST,docode
        movl $COMMA, %eax
        jmp *(%eax)

// H  !dest   dest adrs --    change a branch dest'n
//  Changes the destination address found at 'adrs'
//  to the given 'dest'.  On the 80x86 this is '!'
//  ...other CPUs may need relative addressing.
//    head STOREDEST,5,"!dest",docode
    nohead STOREDEST,docode
        movl $STORE, %eax
        jmp *(%eax)

//  HEADER STRUCTURE ==============================
//  The structure of the Forth dictionary headers
//  (name, link, immediate flag, and "smudge" bit)
//  does not necessarily differ across CPUs.  This
//  structure is not easily factored into distinct
//  "portable" words//  instead, it is implicit in
//  the definitions of FIND and CREATE, and also in
//  NFA>LFA, NFA>CFA, IMMED?, IMMEDIATE, HIDE, and
//  REVEAL.  These words must be (substantially)
//  rewritten if either the header structure or its
//  inherent assumptions are changed.

//  ===========================================
// high resolution timer
// Z  rdtsc  ( -- d )  \ cpu ins'n counter
//  
    head RDTSC,5,"rdtsc",docode
        push %ebx
        rdtsc
        push %eax
        movl %edx, %ebx
        next


