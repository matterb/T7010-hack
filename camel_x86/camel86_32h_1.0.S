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
// X   NAME     stack -- stack    description
//    where x=C for ANS Forth Core words,
//           =B for BLOCK
//           =E for EXCEPTION
//           =F for FILE
//           =M for MEMORY
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
//  v2013.001
//  ===============================================
//  CAMEL86_32h.S: Code Primitives
// ========= SYSTEM VARIABLES & CONSTANTS =========
// system variables are defined as constants in the data segment
//
// C  bl  (  -- char )  \ an ASCII space
    head BLANK,2,"bl",docon
        .int 0x20

// X  true  ( -- -1 )  \ true flag
    head TRUE,4,"true",docon
        .int -1

// X  false  ( -- 0  ) \  false flag
    head FALSE,5,"false",docon
        .int 0

// Z  #task-size  (  -- 3072  )  size of task user area
//    1023 bytes available between 2049 - 3072
    head NTASKSIZE,10,"#task-size",docon
        .int 3072

// Z  term  ( -- addr )  \ console task user area
    head TERM,4,"term",docon
        .int User0

// Z  pausev  ( -- addr )  \ system task  pause vector
    head PAUSEV,6,"pausev",docon
        .int sys_pausev

// Z  runv  ( -- addr )  \ system  task run vector
    head RUNV,4,"runv",docon
        .int sys_runv

// Z  #task  ( -- u )  \  count of defined tasks
    head NUMTASK,5,"#task",docon
        .int sys_ntask

// Z  handler  ( -- addr )  \ catch-throw exception handler vector
    head HANDLER,7,"handler",docon
        .int sys_handler

// Z  tibsize  ( -- n )  \ size of TIB
    head TIBSIZE,7,"tibsize",docon
        .int 0x7f         

// W  forth-wordlist  ( -- wid  )  \ FORTH wordlist id
    head FORTHWORDLIST,14,"forth-wordlist",docon
        .int root_wid
#ifdef COREBOOT
// Z  'irq-count  ( -- a-addr )  \ array of interrupt counters
    head TICKIRQCOUNT,10,"'irq-count",docon
        .int irq_count

// Z  'irq-vectors  ( -- a-addr )  \ array of interrupt handlers
    head TICKIRQVECS,12,"'irq-vectors",docon
        .int irq_vectors
#endif
// X  tib  ( -- a-addr )  \ Terminal Input Buffer. above user area
//   HEX -80 USER tib  
    head TIB,3,"tib",douser
        .int -0x80

// Z  u0  ( -- a-addr )  \ current user area adrs
//   0 USER u0
    head U0,2,"u0",douser
        .int 0x00

// C  >in  ( -- a-addr )  \ holds offset into TIB
//   4 USER >in
    head TOIN,3,">in",douser
        .int 0x04

// C  base  ( -- a-addr )  \ holds conversion radix
//   8 USER base
    head BASE,4,"base",douser
        .int 0x08

// C  state  ( -- a-addr )  \ holds compiler state
//   12 USER state
    head STATE,5,"state",douser
        .int 0x0c

// Z  dp  ( -- a-addr )  \ holds dictionary ptr
//   16 USER dp
    head _DP,2,"dp",douser
        .int 0x10

// Z  'source  ( -- a-addr )  \ two cells: len, adrs
//  20 USER 'source
    head TICKSOURCE,7,"'source",douser
        .int 0x14

// Z  latest  ( -- a-addr )  \  last word in dictionary
//  28 USER latest
    head LATEST,6,"latest",douser
        .int 0x1c

// Z  hp  ( -- a-addr  )  \ HOLD pointer
//  32 USER hp
    head HP,2,"hp",douser
        .int 0x20

// Z  lp  ( -- a-addr )  \ Leave-stack pointer
//  36 USER lp
    head LP,2,"lp",douser
        .int 0x24

// vectored IO and PAUSE here
// Z  emitv  ( --- a-addr )  \ variable holding EMIT operator
//  40 USER emitv
    head EMITV,5,"emitv",douser
        .int 0x28

// Z  keyv  ( -- a-addr )  \ variable holding KEY vector
//  44 USER keyv
    head KEYV,4,"keyv",douser
        .int 0x2c

// Z  key?v  ( -- a-addr )  \ user variable for KEY? vector
//  48 USER key?v
    head KEYQV,5,"key?v",douser
        .int 0x30

// Z  ioinitv  ( -- a-addr )  \ user variable for IO initialzation vector
//  52 USER ioinitv
    head IOINITV,7,"ioinitv",douser
        .int 0x34

// Z  pausev  ( -- a-addr )  \ user variable for pause vector
// 56 user pausev
    head UPAUSEV,7,"upausev",douser
        .int 0x38

// Z  'source-id  ( -- a-addr )  \ user variable for ??
// 60 user 'source-id
    head TICKSOURCEID,10,"'source-id",douser
        .int 0x3c

// B  blk  ( -- a-addr )  \ user variable for current block number
// 64 user blk
    head BLK,3,"blk",douser
        .int 0x40

// B  scr  ( -- a-addr )  \ user variable for current screen
// 68 user scr
    head SCR,3,"scr",douser
        .int 0x44

// X  forth-wordlist  ( -- wid )  \ FORTH wordlist id
// 72 user forth-wordlist
    head ROOTWORDLIST,13,"root-wordlist",douser
        .int 0x48

// Z  sorder  ( -- a-addr )  \ search-order specification 9 cells
// 76 usr sorder
    head SORDER,6,"sorder",douser
        .int 0x4c

// Z  nextlist  ( -- a-addr )  \ next available wid 9 cells
// 112 usr nextlist
    head NEXTLIST,8,"nextlist",douser
        .int 0x70

// Z  current  ( -- a-addr )  \ current wid
//   148 user current
    head CURRENT,7,"current",douser
    .int 0x94

// Z  #task  ( -- n )  \ number of running task
//  152 user #task
//    head NUMTASK,5,"#task",douser
//        .int 0x98

// Z  handler ( -- a-addr )  \ exception handler vector
//  156 user handler
//    head HANDLER,7,"handler",douser
//        .int 0x9c

// Z  'buffer  ( -- addr )  \  array of block buffers 
//  160 user 'buffer
    head TICKBUFFER,7,"'buffer",douser
        .int 0xa0

// H  blockid  -- a-addr  4 cells
//  file id for mapped block ranges
//  164 user block-ids
    nohead BLOCKID,douser
        .int 0xa4

// Z  #bufs  ( -- u )  \ number of defined block buffers
// 180 user #bufs
    head NUMBUFS,5,"#bufs",douser
        .int 0xb4

// H  inputfbuf           vector to filebuffer
// 184 user inputfbuf
    nohead BRINPUTFBUF,douser
        .int 0xb8

// H  inputflen          vector to file buffer len
// 188 user inputflen
    nohead BRINPUTFLEN,douser
        .int 0xbc

// H  mapping       file mapping status
//  192 user mapping
    nohead MAPPING,douser
        .int 0xc0

//  Z  block fetch vector
//  196 user (block@)
    head BLKFETCHV,8,"(block@)",douser
        .int 0xc4

//  Z  (block!) ( -- )  \  disk block fetch vector
//  200 user (block!)
    head BLKSTOREV,8,"(block!)",douser
        .int 0xc8

// Z  'sp  ( -- addr )  \ task saved stack point area
//   USER_PSP is defined in camel86_32h_x_xx.S file
// 204  user 'sp
    head TICKSP,3,"'sp",douser
        .int USER_PSP

// Z  urunv  ( -- addr )  \ variable holding task run action vector
// 208  user runv
    head URUNV,5,"urunv",douser
        .int 0xd0

// 212   next user location
//      .int (0xd4)   
//
// 266  (0x10a)  start of hold area
// X  pad  ( -- a-addr )  \  user PAD buffer 84 bytes 
//                    = end of hold area! 34 bytes
// 300 user pad
     head PAD,3,"pad",douser
        .int 0x12c

// Z  l0  ( -- a-addr )  \ bottom of Leave stack:grows up
// 428 user lo
    head L0,2,"l0",douser
        .int 0x1ac

// 1024  end of return stack 0x400
// Z  r0  ( -- a-addr )  \ top of Return stack   128 cells:grows down
//                      large vaue for interrupt handling
// 1536 usr r0
    head R0,2,"r0",douser
        .int 0x600  

// Z  s0  ( -- a-addr )  \ top of Parameter stack 128 cells:grows down
// 2048  usr s0
    head S0,2,"s0",douser
        .int 0x800 

// Z  uinit  ( -- addr )  \ initial values for user area
    head UINIT,5,"uinit",dovar
        .int 0,0,10,0   /*  reserved,>in,base,state */
#ifdef LINUX
        .int ramdict    /*  dp */
#else
        .int _end
#endif
        .int 0,0    /*  'source init'd elsewhere */
        .int ilatest    /*  latest  */
        .int 0,0    /*  hp,lp init'd elsewhere */
        .int 0,0,0,0,0  /* emitv,keyv,key?v,ioinitv,pausev */
        .int 0,0,0    /* 'source-id,blk,scr */
        .int ilatest    /* forth-wordlist */
        .int 1,0,0,0,0  /* sorder 9 cells init'd elsewhere */
        .int 0,0,0,0    
        .int 0,0,0,0,0  /* nextlist 9 cells init'd elsewhere */
        .int 0,0,0,0    
        .int root_wid   /* current wordlist */
        .int 1,0    /* #task,handler init'd elsewhere */
        .int 0,0,0,0,0  /* 'buffer  blockid 4 cells */
        .int 0      /* #bufs */
        .int sys_filebuf,0xff   /* inputfbuf flen */
        .int 0      /* file block mappings bitmap*/ 
                    /*  block fetch and store init elswhere */

// Z  #init  ( -- n )  \ #bytes of user area init data
    head NINIT,5,"#init",docon
        .int 196

// ==================ARITHMETIC OPERATORS=================
// C  s>d  ( n -- d )  \ convert single to a double
//    DUP 0< ; 
    head STOD,3,"s>d",docolon
        .int DUPE,ZEROLESS,EXIT

// X  d>s  ( d -- n )  \ convert double to single
//    drop ; 
    head DTOS,3,"d>s",docolon
        .int DROP,EXIT

// X  u>d  ( u -- d )  \ convert unsigned to a double
// 0 ;
   head UTOD,3,"u>d",docolon
        .int LIT,0,EXIT

// Z  ?negate  ( n1 n2 -- n3 )  \  negate n1 if n2 negative
//    0< if negate then ;         ...a common factor
    head QNEGATE,7,"?negate",docolon
        .int ZEROLESS,QBRANCH,QNEG1,NEGATE
QNEG1:  .int EXIT

// C  abs  ( n1 -- +n2 )  \  absolute value
//    dup ?negate ;
    head ABBS,3,"abs",docolon
        .int DUPE,QNEGATE,EXIT

// D  dnegate  ( d1 -- d2 )  \ negate double precision
//    swap invert swap invert 1 m+ ;
    head DNEGATE,7,"dnegate",docolon
        .int SWAP,INVERT,SWAP,INVERT,LIT,1,MPLUS
        .int EXIT

// D  ?dnegate  ( d1 n -- d2 )  \ negate d1 if n negative
//    0< if dnegate then ;        ...a common factor
    head QDNEGATE,8,"?dnegate",docolon
        .int ZEROLESS,QBRANCH,DNEG1,DNEGATE
DNEG1:  .int EXIT

// D  dabs  ( d1 -- +d2 )  \ absolute value dbl.prec.
//    dup ?dnegate ; 
    head DABS,4,"dabs",docolon
        .int DUPE,QDNEGATE,EXIT

// D  d-  ( d1 d2 -- d1-d2 )  \  double subtraction
//    dnegate d+ ;
    head DMINUS,2,"d-",docolon
        .int DNEGATE,DPLUS,EXIT

// D  d0<  ( d1 -- flg )  \ double less than zero
//  nip 0< ;
    head DZEROLESS,3,"d0<",docolon
        .int NIP,ZEROLESS,EXIT

// D  d<  ( d1 d2 -- flg )  \ double less than  
//   2 pick over = if
//     drop nip u< 
//   else nip < nip then
    head DLESS,2,"d<",docolon
        .int LIT,0x02,PICK,OVER,EQUAL
        .int QBRANCH,DLSS1,DROP,NIP,ULESS,EXIT
DLSS1:  .int NIP,LESS,NIP,EXIT

// D  dmax  ( xd1 xd2 -- )  \ double maximum 
//    2over 2over d<
//    if 2swap  then
//   2drop
    head DMAX,4,"dmax",docolon
        .int TWOOVER,TWOOVER,DLESS,QBRANCH,DMX1
        .int TWOSWAP,TWODROP,EXIT
DMX1:   .int TWODROP,EXIT

// D  dmin  ( xd1 xd2 -- )  \ double minimum 
//  2over 2over d<  0=
//  if  2swap  then
//  2drop
    head DMIN,4,"dmin",docolon
        .int TWOOVER,TWOOVER,DLESS,ZEROEQUAL
        .int QBRANCH,DMN1,TWOSWAP,TWODROP,EXIT
DMN1:   .int TWODROP,EXIT

// D  d0=  ( d1 -- flg )  \ double zero equal
//  or 0=;
    head DZEROEQUAL,3,"d0=",docolon
        .int ORR,ZEROEQUAL,EXIT

// D  d=  ( d1 d2 -- flg )  \  double test for equality  
//    d- d0= ;
    head DEQUAL,2,"d=",docolon
        .int DMINUS,ORR,ZEROEQUAL,EXIT

// D  d<>  ( d1 d2  -- flg )  \ double test for not-equal
//  d= 0= ;
    head DNEQUAL,3,"d<>",docolon
        .int DEQUAL,ZEROEQUAL,EXIT

// D  du<  ( ud1 ud2  -- flg )  \ unsigned double less than 
//  rot swap 2dup <> if
//    2swap then
// 2drop u< ;
    head DULESS,3,"du<",docolon
        .int ROT,SWAP,TWODUP,NOTEQUAL
        .int QBRANCH,DULSS1,TWOSWAP
DULSS1: .int TWODROP,ULESS,EXIT

// D  d2*  ( d1 -- d2 )  \ double arithemtical shift left
// 2* over 0< if
//   1+ then
// swap 2* swap ;
    head DTWOSTAR,3,"d2*",docolon
        .int TWOSTAR,OVER,ZEROLESS
        .int QBRANCH,D2STR1,ONEPLUS
D2STR1: .int SWAP,TWOSTAR,SWAP,EXIT

// D  d2/  ( d1 -- d2 )  \ double arithemtical shift left
// tuck 31 lshift swap 2/ or
// swap 2/ ;
    head DTWOSLASH,3,"d2/",docolon
        .int TUCK,LIT,031,LSHIFT,SWAP
        .int TWOSLASH,ORR,SWAP,TWOSLASH
        .int EXIT

// D  m*  ( n1 n2 -- d )  \ signed 32*32->64 multiply
//    2dup xor >r        carries sign of the result
//    swap abs swap abs um*
//    r> ?dnegate ; 
    head MSTAR,2,"m*",docolon
        .int TWODUP,XORR,TOR
        .int SWAP,ABBS,SWAP,ABBS,UMSTAR
        .int RFROM,QDNEGATE,EXIT

// C fm/mod   d1 n1 -- n2 n3   floored signed div'n
//   DUP >R              divisor
//   2DUP XOR >R         sign of quotient
//   >R                  divisor
//   DABS R@ ABS UM/MOD
//   SWAP R> ?NEGATE SWAP  apply sign to remainder
//   R> 0< IF              if quotient negative,
//       NEGATE
//       OVER IF             if remainder nonzero,
//         R@ ROT -  SWAP 1-     adjust rem,quot
//       THEN
//   THEN  R> DROP ;
//  Ref. dpANS-6 section 3.2.2.1.
    head FMSLASHMOD,6,"fm/mod",docolon
        .int DUPE,TOR,TWODUP,XORR,TOR,TOR,DABS,RFETCH,ABBS
        .int UMSLASHMOD,SWAP,RFROM,QNEGATE,SWAP,RFROM,ZEROLESS,QBRANCH,FMMOD1
        .int NEGATE,OVER,QBRANCH,FMMOD1
        .int RFETCH,ROT,MINUS,SWAP,ONEMINUS
FMMOD1: .int RFROM,DROP,EXIT

// C  fm/mod  ( d1 n1 -- rem quot )  \ floored signed division  2-cell by 1-cell
// my version
// >r tuck r@ sm/rem \ S r q   R: d
// swap rot r@ xor 0< \ q r flg
// over 0<> and if 
//    r> + swap  \ r' q
//    1- exit  \ r' q'  R:d
// then
// swap r> drop
// ;
//  Ref. 200x / 10.2 section 3.2.2.1.
// If r is not zero, and D and d have the same sign,
// then q is the largest integer less than the true quotient.
// If D an d have different signs,
// then q is rounded toward negative infinity
// i.e q is rounded to its arithmetic floor
// and r carries the sign of d or is zero
//    head FMSLASHMOD,6,"fm/mod",docolon
//        .int TOR,TUCK,RFETCH,SMSLASHREM,SWAP,ROT
//        .int RFETCH,XORR,ZEROLESS,OVER,ZERONEQUAL,ANDD,QBRANCH,FMMD1
//        .int RFROM,PLUS,SWAP,ONEMINUS,EXIT
// FMMD1:  .int SWAP,RFROM,DROP,EXIT

// C  *  ( n1 n2 -- n3 )  \ signed multiply
//    m* drop ;
    head STAR,1,"*",docolon
        .int MSTAR,DROP,EXIT

// C  /mod  ( n1 n2 -- n3 n4 )  \ signed divide with remainder
//    >r s>d r> fm/mod ; 
    head SLASHMOD,4,"/mod",docolon
        .int TOR,STOD,RFROM,FMSLASHMOD,EXIT

// C  /  ( n1 n2 -- n3 )  \  signed divide
//    /mod nip  ;
  head SLASH,1,"/",docolon
        .int SLASHMOD,NIP,EXIT

// C  mod  ( n1 n2 -- n3 )  \ signed remainder
//    /mod drop ; 
    head MODD,3,"mod",docolon
        .int SLASHMOD,DROP,EXIT

// C  */mod  ( n1 n2 n3 -- n4 n5 )  \  n1*n2/n3, rem quot
//    >r m* r> fm/mod ; 
    head SSMOD,5,"*/mod",docolon
        .int TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

// C  */  ( n1 n2 n3 -- n4 )  \ n1*n2/n3
//    */mod nip ;
    head STARSLASH,2,"*/",docolon
        .int SSMOD,NIP,EXIT

// C  max  ( n1 n2 -- n3 )  \ signed maximum
//    2dup < if swap then drop ; 
    head MAX,3,"max",docolon
        .int TWODUP,LESS,QBRANCH,MAX1,SWAP
MAX1:   .int DROP,EXIT

// C  min  ( n1 n2 -- n3 )  \ signed minimum
//    2dup > if swap then drop ; 
    head MIN,3,"min",docolon
        .int TWODUP,GREATER,QBRANCH,MIN1,SWAP
MIN1:   .int DROP,EXIT

// =================DOUBLE OPERATORS=======================

// C  2swap  ( x1 x2 x3 x4 -- x3 x4 x1 x2 )  \ as per stack diagram
//    rot >r rot r> ; 
    head TWOSWAP,5,"2swap",docolon
        .int ROT,TOR,ROT,RFROM,EXIT

// C  2over  ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )  \ as per stack diagram
//    2>r 2dup 2r> 2swap ; 
    head TWOOVER,5,"2over",docolon
        .int TWOTOR,TWODUP,TWORFROM
        .int TWOSWAP,EXIT

// D  2rot  ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )  \ as per stack diagram
//    2>r 2swap 2r>> 2swap ; 
    head TWOROT,4,"2rot",docolon
        .int TWOTOR,TWOSWAP,TWORFROM
        .int TWOSWAP,EXIT

// D  2variable  ( "<spaces>name"-- )  \ define a 2-cell variable
//   create 2 cells allot ;
    head TWOVARIABLE,9,"2variable",docolon
        .int CREATE,LIT,2,CELLS,ALLOT,EXIT

// D  2constant  ( x1 x2 "<spaces>name"-- )  \ define a 2-cell constant
//   create -4 allot docon ,cf , , ;
    head TWOCONSTANT,9,"2constant",docolon
        .int CREATE,LIT,-4,ALLOT
        .int LIT,do2con,COMMACF
        .int COMMA,COMMA,EXIT

// D  2value  ( x1 x2 "<spaces>name"-- )  \ create 2-cell value
//  create , ,
//  does> 2@
//  ;
    head TWOVALUE,6,"2value",docolon
        .int CREATE,COMMA,COMMA
        .int XDOES
        call dodoes
        .int TWOFETCH,EXIT

// D  2literal  ( x x -- )  \ append 2-cell numeric literal
// 
//    state @ postpone literal postpone literal then ;  immediate
//  This tests STATE so that it can also be used
//  interpretively.  (ANSI doesn't require this.)
    head TWOLITERAL,8,"2literal",docolon,1
        .int STATE,FETCH,QBRANCH,DLITR1
        .int SWAP,LITERAL,LITERAL
DLITR1: .int EXIT

// Z  2to  ( x x "<spaces>name" -- )  \ set a 2-cell value
//   ' >body state @ if  postpone 2literal postpone 2!
//                   else  2!  then // immediate
    head TWOTO,3,"2to",docolon,1
        .int TICK,TOBODY,STATE,FETCH,QBRANCH,T2O1
//        .int LIT,LIT,COMMAXT
        .int LIT,TWOLITERAL,COMMAXT
        .int LIT,LIT,COMMAXT,COMMAXT,LIT,TWOSTORE,COMMAXT,BRANCH,T2O2
T2O1:   .int TWOSTORE
T2O2:   .int EXIT

// ===================INPUT/OUTPUT=========================
// parse a number
// (##) bl word ?number if
//     postpone literal
// else drop -13 throw then
    nohead BRATWOHASH,docolon
        .int BLANK,WORDD,QNUMBER,QBRANCH,BRHSH1
        .int LITERAL,EXIT
BRHSH1: .int DROP,LIT,-13,THROW,EXIT

// Z  h#  ( <space>cccc  -- x )  \ parse next word as a hexadecimal number
//    base @ >r hex (##) r> base ! ; immediate
    head HHASH,2,"h#",docolon,1
        .int BASE,FETCH,TOR,HEX
        .int BRATWOHASH,RFROM,BASE,STORE,EXIT
 
// Z  d#  ( <space>cccc  -- x )  \ parse next word as a decimal number
    head DHASH,2,"d#",docolon,1
        .int BASE,FETCH,TOR,DECIMAL
        .int BRATWOHASH,RFROM,BASE,STORE,EXIT
 
// Z  o#  ( <space>cccc  -- x )  \ parse next word a an octal number
    head OHASH,2,"o#",docolon,1
        .int BASE,FETCH,TOR,OCTAL
        .int BRATWOHASH,RFROM,BASE,STORE,EXIT

 // Z  b#  ( <space>cccc  -- x )  \ parse next word as a binary number
    head BHASH,2,"b#",docolon,1
        .int BASE,FETCH,TOR,BINARY
        .int BRATWOHASH,RFROM,BASE,STORE,EXIT

// C  count  ( c-addr1 -- c-addr2 u ) \  address and length of counted string
//    dup char+ swap c@ ;
    head COUNT,5,"count",docolon
        .int DUPE,CHARPLUS,SWAP,CFETCH,EXIT

// C  cr  ( -- )  \ output a newline
//    0A emit ;
    head CR,2,"cr",docolon
#ifdef COREBOOT
        .int LIT, 0x0D,EMIT
#endif
        .int LIT,0x0A,EMIT
        .int EXIT

// C  space  ( -- )  \ output a space
//    bl emit ;
    head SPACE,5,"space",docolon
        .int BLANK,EMIT,EXIT

// C  spaces  ( n -- )  \ output n spaces
//    begin dup while space 1- repeat drop ;
    head SPACES,6,"spaces",docolon
SPCS1:  .int DUPE,QBRANCH,SPCS2
        .int SPACE,ONEMINUS,BRANCH,SPCS1
SPCS2:  .int DROP,EXIT

// Z  umin  ( u1 u2 -- u )  \  unsigned minimum of u1 an u2
//    2dup u> if swap then drop ; 
    head UMIN,4,"umin",docolon
        .int TWODUP,UGREATER,QBRANCH,UMIN1,SWAP
UMIN1:  .int DROP,EXIT

// Z  umax  ( u1 u2 -- u )  \ unsigned maximum of u1 and u2
//    2dup u< if swap then drop ; 
    head UMAX,4,"umax",docolon
        .int TWODUP,ULESS,QBRANCH,UMAX1,SWAP
UMAX1:  .int DROP,EXIT

// C  accept  ( c-addr +n -- +n' )  \ get line from terminal max of n char
//    over + 1- over      -- sa ea a
//    begin key           -- sa ea a c
//    dup 0a <> while
//        dup emit        -- sa ea a c
//        dup 8 = if  drop 1-    >r over r> umax
//              else  over c! 1+ over umin
//        then            -- sa ea a
//    repeat              -- sa ea a c
//    drop nip swap - ; 
    head ACCEPT,6,"accept",docolon
        .int OVER,PLUS,ONEMINUS,OVER
ACC1:   .int KEY,DUPE,LIT,0x0A,NOTEQUAL,OVER,LIT,0x0D,NOTEQUAL
        .int ANDD,QBRANCH,ACC5
        .int DUPE,EMIT,DUPE,LIT,0x08,EQUAL,QBRANCH,ACC3
        .int DROP,ONEMINUS,TOR,OVER,RFROM,UMAX
        .int BRANCH,ACC4
ACC3:   .int OVER,CSTORE,ONEPLUS,OVER,UMIN
ACC4:   .int BRANCH,ACC1
// ACC5:   .int DROP,NIP,SWAP,MINUS,EXIT
ACC5:   .int EMIT,NIP,SWAP,MINUS,EXIT

// C  type  ( c-addr +n -- )  \ type line to terminal
//    ?dup if
//      over + swap do i c@ (emit loop
//    else drop then ; 
    head TYPE,4,"type",docolon
        .int QDUP,QBRANCH,TYP4
        .int OVER,PLUS,SWAP,XDO
TYP3:   .int II,CFETCH,BREMIT,XLOOP,TYP3
        .int BRANCH,TYP5
TYP4:   .int DROP
TYP5:   .int EXIT

// Z  ctype  ( z-addr -- )  \ type zer0 terminated string to term'l
//    begin
//      dup c@ 0<> while
//      dup emit 1+
//    repeat
//   drop ;
    head ZTYPE,5,"ztype",docolon
ZTYP1:  .int DUPE,CFETCH,DUPE,QBRANCH,ZTYP2
        .int EMIT,ONEPLUS,BRANCH,ZTYP1
ZTYP2:  .int TWODROP,EXIT

// H  (s")  ( -- c-addr u )  \ run-time code for S"
//    r> count 2dup + aligned >r ;
//  e.g. as used by ."
    nohead XSQUOTE,docolon
        .int RFROM,COUNT,TWODUP,PLUS,ALIGNED,TOR
        .int EXIT

// C  s"  ( -- addr u)  \ get in-line string. uses a single buffer 
//   state @
//   if  postpone (s") 
//       [char] " parse dup c,
//       here swap dup allot cmove
//   else [char] " parse
//         usr_stringscr 2dup c!
//         char+ swap 2dup 2>r
//         cmove 2r>
//   then ; immediate
    head SQUOTE,2,"s\"",docolon,1
        .int STATE,FETCH
        .int QBRANCH,SQUOT1
        .int LIT,XSQUOTE,COMMAXT
        .int LIT,0x22,PARSE,DUPE,CCOMMA
        .int HERE,SWAP,DUPE,ALLOT,CMOVE
        .int EXIT
SQUOT1: .int LIT,0x22,PARSE
        .int LIT,usr_stringbuf,TWODUP,CSTORE
        .int CHARPLUS,SWAP,TWODUP,TWOTOR
        .int CMOVE,TWORFROM
SQUOT2: .int EXIT

// H  (c")     -- c-addr u   run-time code for c"
//    r> count over + aligned >r 1- ;
//  e.g. as used by ."
    nohead XCQUOTE,docolon
        .int RFROM,COUNT,OVER,PLUS,ALIGNED,TOR
        .int ONEMINUS,EXIT

// C  c"  (  -- addr u )  \ get in-line string 
//   state @
//   if  postpone (c") 
//       [char] " parse dup c,
//       here swap dup allot cmove
//   else [char] " parse
//         usr_stringscr 2dup c!
//         char+ swap 2dup 2>r
//         cmove 2r> drop 1-
//   then ; immediate
    head CQUOTE,2,"c\"",docolon,1
        .int STATE,FETCH
        .int QBRANCH,CQUOT1
        .int LIT,XCQUOTE,COMMAXT
        .int LIT,0x22,PARSE,DUPE,CCOMMA
        .int HERE,SWAP,DUPE,ALLOT,CMOVE
        .int EXIT
CQUOT1: .int LIT,0x22,PARSE
        .int LIT,usr_stringbuf,TWODUP,CSTORE
        .int CHARPLUS,SWAP,TWODUP,TWOTOR
        .int CMOVE,TWORFROM,DROP,ONEMINUS
        .int EXIT

// C  ."  ( -- )  \ compile string to print
//    postpone s"  postpone type ;  immediate
    head DOTQUOTE,2,".\"",docolon,1
        .int SQUOTE
        .int LIT,TYPE,COMMAXT
        .int EXIT

// X  .(  ( -- )  \  display comments upto enclosing )
//    [ hex ] 29 parse  type ;  immediate
    head DOTPAREN,2,".(",docolon,1
        .int LIT,0x29,PARSE,TYPE,EXIT

// ===================NUMERIC OUTPUT=====================
//  Numeric conversion is done l.s.digit first, so
//  the output buffer is built backwards in memory.
//
//  Some double-precision arithmetic operators are
//  needed to implement ANSI numeric conversion.
//
// Z  ud/mod  ( ud1 u2 -- u3 ud4 )  \ unsigned 2-cell by 1-cell divide. 2-cell quotient , 1-cell remainder
//    >r 0 r@ um/mod  rot rot r> um/mod rot ; 
    head UDSLASHMOD,6,"ud/mod",docolon
        .int TOR,LIT,0,RFETCH,UMSLASHMOD,ROT,ROT
        .int RFROM,UMSLASHMOD,ROT,EXIT

// Z  ud*  ( ud1 u2 -- ud3 )  \  unsigned 2-cell by 1-cell to 2-cell multiply. 
//    dup >r um* drop  swap r> um* rot + ;
    head UDSTAR,3,"ud*",docolon
        .int DUPE,TOR,UMSTAR,DROP
        .int SWAP,RFROM,UMSTAR,ROT,PLUS,EXIT

// C  hold  ( char -- )  \ add char to HOLD buffer output string
//    -1 hp +!  hp @ c! ;
    head HOLD,4,"hold",docolon
        .int LIT,-1,HP,PLUSSTORE
        .int HP,FETCH,CSTORE,EXIT

// C  holds  ( addr u -- )  \ add string to HOLD buffer output string
// begin 
//  dup   while
// 1- 2dup + c@ hold 
// repeat
// 2drop ;
    head HOLDS,5,"holds",docolon
HLDS1:  .int DUPE,QBRANCH,HLDS2,ONEMINUS
        .int TWODUP,PLUS,CFETCH,HOLD,BRANCH,HLDS1
HLDS2:  .int TWODROP,EXIT

// C  <#  ( -- )  \ begin numeric conversion
//    pad hp ! ;           (initialize Hold Pointer)
    head LESSNUM,2,"<#",docolon
        .int PAD,HP,STORE,EXIT

// Z  >digit  ( n -- c )  \ convert to 0..9a..z
//    [ hex ] dup 9 > 27 and + 30 + ; 
    head TODIGIT,6,">digit",docolon
        .int DUPE,LIT,9,GREATER,LIT,0x27,ANDD,PLUS
        .int LIT,0x30,PLUS,EXIT

// C  #  ( ud1 -- ud2 )  \ convert 1 digit of output
//    base @ ud/mod rot >digit hold ; 
    head NUM,1,"#",docolon
        .int BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
        .int HOLD,EXIT

// C  #s  ( ud1 -- ud2 )  \ convert remaining digits
//    begin # 2dup or 0= until ; 
    head NUMS,2,"#s",docolon
NUMS1:  .int NUM,TWODUP,ORR,ZEROEQUAL,QBRANCH,NUMS1
        .int EXIT

// C  #>  ( ud1 -- c-addr u )  \ end conv., get string
//    2drop hp @ pad over - ; 
    head NUMGREATER,2,"#>",docolon
        .int TWODROP,HP,FETCH,PAD,OVER,MINUS,EXIT

// C  sign  ( n -- )  \ add minus sign to HOLD buffer if n<0
//    0< if 2d hold then ;
    head SIGN,4,"sign",docolon
        .int ZEROLESS,QBRANCH,SIGN1,LIT,0x2D,HOLD
SIGN1:  .int EXIT

// H (u.)  u -- c-addr n
//    s>d <# #s #> ;
    nohead BRUDOT,docolon
        .int LIT,0,LESSNUM,NUMS,NUMGREATER,EXIT

// H (ud.)  ud -- c-addr n
//    <# #s #> ;
    nohead BRUDDOT,docolon
        .int LESSNUM,NUMS,NUMGREATER,EXIT

// H (.)  n -- c-addr n
//    <#  dup s>d dabs #s rot sign #> ;
    nohead BRDOT,docolon
        .int LESSNUM,DUPE,STOD,DABS,NUMS
        .int ROT,SIGN,NUMGREATER,EXIT

// H (d.)  d --c-addr n
//    tuck dabs <# #s rot sign #> ;
    nohead BRDDOT,docolon
        .int TUCK,DABS,LESSNUM,NUMS
        .int ROT,SIGN,NUMGREATER,EXIT

// C  u.  ( u -- )  \  display u unsigned
//    (u.) type space ; 
    head UDOT,2,"u.",docolon
        .int BRUDOT,TYPE,SPACE,EXIT

// Z  ud.  ( ud -- )  \ display unsigned double
//    (ud.) type space ; 
    head UDDOT,3,"ud.",docolon
        .int BRUDDOT,TYPE,SPACE,EXIT

// C  .  ( n --  )  \ display n signed
//    (.) type space ;
    head DOT,1,".",docolon
        .int BRDOT,TYPE,SPACE,EXIT

// C  d.  ( d -- )  \ display n signed
//    (d.) type space ;
    head DDOT,2,"d.",docolon
        .int BRDDOT,TYPE,SPACE,EXIT

// H (.r) n1 addr u --         \ n1 field width
//   rot over - 0 max spaces type ;
    nohead BRDOTR,docolon
        .int ROT,OVER,MINUS,LIT,0,MAX,SPACES,TYPE,EXIT

// X  u.r  ( u n -- )  \ print u in field n long
//  swap (u.) (.r) ;
    head  UDOTR,3,"u.r",docolon
        .int SWAP,BRUDOT,BRDOTR,EXIT

// X  ud.r  ( ud n -- )  \ print u in field n long
//  rot rot (ud.) (.r) ;
    head  UDDOTR,4,"ud.r",docolon
        .int ROT,ROT,BRUDDOT,BRDOTR,EXIT

// X  .r  ( n1 n2 -- )  \ print n1 right aligned in a field n2 long
//  swap (.) (.r) ;
    head DOTR,2,".r",docolon
        .int SWAP,BRDOT,BRDOTR,EXIT

// X  d.r  ( d n -- )  \ print d in field n long
//  rot rot (d.) (.r) ;
    head  DDOTR,3,"d.r",docolon
        .int ROT,ROT,BRDDOT,BRDOTR,EXIT

// C  binary  ( -- )  \ set number base to binary
//    2 base ! ; 
    head BINARY,6,"binary",docolon
        .int LIT,2,BASE,STORE,EXIT

// C  octal  ( -- )  \ set number base to octal
//    8 base ! ; 
    head OCTAL,5,"octal",docolon
        .int LIT,5,BASE,STORE,EXIT

// C  decimal  ( -- )  \ set number base to decimal
//    10 base ! ; 
    head DECIMAL,7,"decimal",docolon
        .int LIT,10,BASE,STORE,EXIT

// X  hex  ( -- )  \ set number base to hex
//    16 base ! ; 
    head HEX,3,"hex",docolon
        .int LIT,16,BASE,STORE,EXIT

// ================DICTIONARY MANAGEMENT================

// C  here  ( -- addr )  \ returns dictionary ptr
//    dp @ ; 
    head HERE,4,"here",docolon
        .int _DP,FETCH,EXIT

// C  allot  ( n -- )  \ allocate n bytes in dict
//    dp +! ; 
    head ALLOT,5,"allot",docolon
        .int _DP,PLUSSTORE,EXIT

// C  ,  ( x -- )  \ append cell to dict
//    here ! 1 cells allot ; 
    head COMMA,1,",",docolon
        .int HERE,STORE,LIT,1,CELLS,ALLOT,EXIT

// C  w,  ( word -- )  \ append word to dict
//    dup ff and here c!
//    8 rshift ff and here c, 2 chars allot ; 
    head WCOMMA,2,"w,",docolon
        .int HERE,WSTORE,LIT,2,CHARS,ALLOT,EXIT

// C  c,  ( char -- )  \ append char to dict
//    here c! 1 chars allot ; 
    head CCOMMA,2,"c,",docolon
        .int HERE,CSTORE,LIT,1,CHARS,ALLOT,EXIT

// ==============WORDLIST=======================

// W  get-current  ( -- wid )  \ get compilation wordlist
//   current @ ;
    head GETCURRENT,11,"get-current",docolon
        .int CURRENT,FETCH,EXIT

// W  set-current  ( wid -- )  \ set compilation wordlist
//   current ! ;
    head SETCURRENT,11,"set-current",docolon
        .int CURRENT,STORE,EXIT

// W  wordlist  ( -- wid )  \ get new wordlist
//  here 0 , ;
    head WORDLIST,8,"wordlist",docolon
        .int HERE,LIT,0,COMMA,EXIT

// W  definitions  ( -- )  \ set current to first in search order
//    sorder @ 0= 0= if  sorder cell+ @ set-current  then ;
    head DEFINITIONS,11,"definitions",docolon
        .int SORDER,FETCH,ZERONEQUAL
        .int QBRANCH,defs1
        .int SORDER,CELLPLUS,FETCH,SETCURRENT
defs1:  .int EXIT

// W  get-order  ( -- widn...wid1 n )  \ get search order
//    sorder @ dup 0<> if
//      1 swap do  sorder i cells + @  -1 +loop
//      sorder @
//    then ;
    head GETORDER,9,"get-order",docolon
        .int SORDER,FETCH,DUPE,ZERONEQUAL
        .int QBRANCH,GETOR2
        .int LIT,0x1,SWAP,XDO
GETOR1: .int SORDER,II,CELLS,PLUS,FETCH
        .int LIT,-1,XPLUSLOOP,GETOR1
        .int SORDER,FETCH
GETOR2: .int EXIT

// W  set-order  ( widn...wid1 n -- )  \ set search order
//    case  -1 of  forth-wordlist 1 recurse  endof
//           0 of  0 sorder !  endof
//    dup >r
//    8 u> if  -49 throw  then  ( remove hardcoded #vocs )
//    r@ sorder ! sorder
//    r@  0 do  cell+ swap over !  loop  drop
//    r>
//    endcase ;
    head SETORDER,9,"set-order",docolon
        .int LIT,-1,QBRANCH,SETOR1
        .int LIT,-1,OVER,EQUAL,QBRANCH,SETOR2,DROP
        .int FORTHWORDLIST,LIT,0x1,SETORDER
SETOR1: .int EXIT
SETOR2: .int LIT,0X0,OVER,EQUAL,QBRANCH,SETOR4,DROP
        .int LIT,0X0,SORDER,STORE
SETOR3: .int EXIT
SETOR4: .int DUPE,TOR
        .int LIT,8,UGREATER,QBRANCH,SETOR5
        .int LIT,-49,THROW         // ,ABORT
SETOR5: .int RFETCH,SORDER,STORE,SORDER
        .int RFETCH,LIT,0X0, XDO
SETOR6: .int CELLPLUS,SWAP,OVER,STORE,XLOOP,SETOR6
        .int DROP,RFROM
        .int DROP
SETOR7: .int EXIT

// W  only  ( -- )  \ set minimum search order
//    -1 set-order // 
    head ONLY,4,"only",docolon
        .int LIT,-1,SETORDER,EXIT

// W  also  ( --  )  \ duplicate first list in search order
//    get-order over swap 1+ set-order ;
    head ALSO,4,"also",docolon
        .int GETORDER,OVER,SWAP
        .int ONEPLUS,SETORDER,EXIT

// W  previous ( -- )  \ remove first in search order list
//    get-order ?dup if  nip 1- set-order  then ;
    head PREVIOUS,8,"previous",docolon
        .int GETORDER,QDUP,QBRANCH,PREV1
        .int NIP,ONEMINUS,SETORDER
PREV1:  .int EXIT

// Z  wid>name ( wid -- addr len)
//      dup forth-wordlist = if 
//          drop s" forth" exit 
//      then
//      cell minus dup 'dovoc = if
//          cfa>nfa count
//      else s" unnamed" then
//      ;
            
    head WIDTONAME,8,"wid>name",docolon
        .int DUPE,FORTHWORDLIST,EQUAL,QBRANCH,WID2N1
        .int DROP,XSQUOTE
        .byte 5
        .ascii "forth"
        .int EXIT
WID2N1: .int CELL,MINUS,DUPE,FETCH,LIT,dovoc,EQUAL,QBRANCH,WID2N2
        .int CFATONFA,COUNT,EXIT
WID2N2: .int DROP,XSQUOTE
        .byte 7
        .ascii "unnamed"
        .int EXIT

// W  order ( -- )  \ display search order and compilation wid
//    cr get-order ?dup  if
//         0 do wid>name type space loop  then
//    cr get-current wid>name type cr ;
    head ORDER,5,"order",docolon
        .int CR,GETORDER,QDUP,QBRANCH,ORDER2
        .int LIT,0,XDO
ORDER1: .int WIDTONAME,TYPE,SPACE,XLOOP,ORDER1
ORDER2: .int CR,GETCURRENT,WIDTONAME,TYPE,CR,EXIT

// W  search-wordlist  ( caddr u wid -- 0 | xt 1 | xt -1 ) \ search for definition in wordlist
//    begin  @ ?dup while
//        2dup c@ =          \ -- caddr u nfa flag
//           if  dup 1+ 2over s= 0= \ -- caddr u nfa flag
//               if  nip nip dup nfa>cfa    \ -- nfa xt
//                   swap immed? 0= 1 or  \ -- xt 1 | xt -1
//                   exit
//               then
//           then
//           nfa>lfa            \ -- caddr u lfa
//    repeat  2drop 0 ;
    head SEARCHWORDLIST,15,"search-wordlist",docolon
SERWR1: .int FETCH,QDUP,QBRANCH,SERWR3
        .int TWODUP,CFETCH,EQUAL,QBRANCH,SERWR2
        .int DUPE,ONEPLUS,TWOOVER,SEQUAL,ZEROEQUAL
        .int QBRANCH,SERWR2
        .int NIP,NIP,DUPE,NFATOCFA
        .int SWAP,IMMEDQ,ZEROEQUAL,LIT,1,ORR
        .int EXIT
SERWR2: .int NFATOLFA
        .int BRANCH,SERWR1
SERWR3: .int TWODROP,LIT,0X00,EXIT

// Z  vocabulary  ( "<spaces>name" -- )  \ create new vocabulary
//  unlimited vocabularies :
//    create wordlist 
//    does> >r
//          get-order nip r> swap set-order ;
    head VOCABULARY,10,"vocabulary",docolon
        .int CREATE,WORDLIST,DROP
        .int XDOES
        .global dovoc 
dovoc:  call dodoes
        .int TOR,GETORDER,NIP,RFROM,SWAP,SETORDER
        .int EXIT

// W  forth  ( --  )  \ make forth-wordlist the first in search order
//    get-order nip forth-wordlist swap set-order
    head FORTH,5,"forth",docolon
        .int GETORDER,NIP,FORTHWORDLIST,SWAP,SETORDER
        .int EXIT

// ==================MEMORY===========================

// C  erase  ( addr u -- )  \ starting at addr fill u memory location with 0
// 0 fill
    head ERASE,5,"erase",docolon
        .int LIT,0,FILL,EXIT

// S  blank  ( addr u -- )  \
// bl fill
    head BLANKS,5,"blank",docolon
        .int BLANK,FILL,EXIT

// C  buffer:  ( -- a-addr )  \ allocate buffer
//   create allot;
    head BUFCOLON,7,"buffer:",docolon
        .int CREATE,ALLOT,EXIT

// M  allocate  ( u -- a-addr ior )  \  assign buffer of size u  from heap
// M  free  ( a-addr -- ior )  \ deassign buffer
// M  resize  ( a-addr1 u -- a-addr2 ior )  \ resize buffer
// H  bitsetup  n addr -- addr val n
//  over 3 rshift + tuck c@ 0x80 rot 7 and rshift ;
    nohead BITSETUP,docolon
        .int OVER,LIT,0x03,RSHIFT,PLUS,TUCK,CFETCH
        .int LIT,0X80,ROT,LIT,0X07,ANDD,RSHIFT
        .int EXIT

// Z  bitset  ( n addr  -- )  \ set bit n in bit array at addr
//   bitsetup or swap c! ;
    head BITSET,6,"bitset",docolon
        .int BITSETUP,ORR,SWAP,CSTORE,EXIT

// Z  bitclear  ( n addr -- )  \ clear bit n in bit array at addr
//   bitsetup invert and swap c! ;
    head BITCLEAR,8,"bitclear",docolon
        .int BITSETUP,INVERT,ANDD,SWAP,CSTORE,EXIT

// Z  bittest  ( n add -- flag )  \ status of bit n in bit array at addr
//   bitsetup and 0<> ;
    head BITTEST,7,"bittest",docolon
        .int BITSETUP,ANDD,ZERONEQUAL,NIP,EXIT

// ================EXCEPTION==========================
// E  catch  ( i*x xt -- j*x 0 | i*x n )  \ try to execute xt 
//   sp@ >r  lp @ >r  handler @ >r
//   rp@ handler !
//   execute
//   r> handler !  2r>  2drop
//   0 ;   
    head CATCH,5,"catch",docolon
        .int SPFETCH,TOR,LP,FETCH,TOR
        .int HANDLER,FETCH,TOR,RPFETCH
        .int HANDLER,STORE,EXECUTE
        .int RFROM,HANDLER,STORE
        .int TWORFROM,TWODROP,LIT,0,EXIT

// E  throw  ( k*x n -- k*x | i*x n )  \  raise an exception
//   ?dup if  handler @ rp!  r> handler !
//            r> lp !
//            r> swap >r
//            sp! drop r>
//        then ;
    head THROW,5,"throw",docolon
        .int QDUP,QBRANCH,THRW1
        .int HANDLER,FETCH,RPSTORE,RFROM
        .int HANDLER,STORE,RFROM,LP
        .int STORE,RFROM,SWAP,TOR
        .int SPSTORE,DROP,RFROM
THRW1:  .int EXIT

//  =================INTERPRETER=====================
//  Note that nfa>lfa, nfa>cfa, immed?, and find
//  are dependent on the structure of the Forth
//  header.  This may be common across many CPUs,
//  or it may be different.

// C  source  (  -- addr n )  \ current input buffer
//    'source 2@ ;         length is at lower adrs
    head SOURCE,6,"source",docolon
        .int TICKSOURCE,TWOFETCH,EXIT

// X  source-id  ( -- addr )  \ input source
    head SOURCEID,9,"source-id",docolon
        .int TICKSOURCEID,FETCH,EXIT

// ================STRING============================
// S  /string  ( addr u n -- addr+n u-n )  \ trim string
//    ROT OVER + ROT ROT - ; 
    head SLASHSTRING,7,"/string",docolon
        .int ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

// H  same? c-addr1 u1 c-addr2 u -- -1|0|1
// ?dup if
//  0 do
//     over c@ over c@ xor if
//         c@ swap c@ > 2* 1+ unloop exit
//     then
//     char+ swap char+ swap
//  loop
//then
//2drop 0 ;
    nohead SAMEQ,docolon
        .int QDUP,QBRANCH,SAMQ3
        .int LIT,0,XDO
SAMQ1:  .int OVER,CFETCH,OVER,CFETCH,XORR
        .int QBRANCH,SAMQ2,CFETCH,SWAP,CFETCH
        .int GREATER,TWOSTAR,ONEPLUS,UNLOOP,EXIT
SAMQ2:  .int CHARPLUS,SWAP,CHARPLUS,SWAP
        .int XLOOP,SAMQ1
SAMQ3:  .int TWODROP,LIT,0,EXIT

// S  compare  ( c-addr u1 c-addr u2  -- -1|0|1 )
// rot 2dup swap - >r  ( ca1 ca2 u2 u1 R: u1-u2 )
// min same?  ?dup if
//   r> drop exit then
// r> dup if 0< 2* 1+ then
// ;
    head COMPARE,7,"compare",docolon
        .int ROT,TWODUP,SWAP,MINUS,TOR
        .int MIN,SAMEQ,QDUP,QBRANCH,CMPR1
        .int RFROM,DROP,EXIT
CMPR1:  .int RFROM,DUPE,QBRANCH,CMPR2
        .int ZEROLESS,TWOSTAR,ONEPLUS
CMPR2:  .int EXIT

// S  search  ( c-addr1 u1 c-addr2 u2 -- c-addr u flg )  \ locate s2 in s1
// 
// dup 0= if 2drop true exit then
// 2over nip over - >r 
// 0 begin
//   0 begin         ( ca1 u1 ca2 u2 i j) 
// \     c-a1[i j +] c@ c-a2 [ j] c@ = while
//       2>r 2over drop 2r@ + +     ( c-a1 u1 c-a2 u2 a1 R:i j)   
//       2over nip 2r>     ( ..a1 a' i j)
//       rot over +   ( a1 i j a2 )
//       -rot 2swap 
//       c@ swap c@ =  while  
//         1+       ( c-a1 u1 c-a2 u2 i j) 
//       repeat
// \ j u2 <> while    ( c-a1 u1 c-a2 u2 i j -- ) 
//       rot 2dup <> >r -rot r> while  ( c-a1 u1 c-a2 u2 i j)  
// \ i u1 u2 - < while     ( c-a1 u1 c-a2 u2 i j)   
//       over r@ < while 
//             drop 1+ 
// repeat then
// r> drop 
// 2swap nip = if ( c-a1 u1 c-a2 u2 i j -- c-a1 u1 i)
//    tuck - -rot + swap true  ( c-a1 u1 i -- c-a u t) 
// else drop false then    ( c-a1 u1 i -- c-a1 u1 t)
// ;

    head SEARCH,6,"search",docolon
        .int DUPE,ZEROEQUAL,QBRANCH,SRCH1,TWODROP,TRUE,EXIT
SRCH1:  .int TWOOVER,NIP,OVER,MINUS,TOR,LIT,0x0
SRCH2:  .int LIT,0x0
SRCH3:  .int TWOTOR,TWOOVER,DROP,TWORFETCH,PLUS,PLUS
        .int TWOOVER,NIP,TWORFROM,ROT,OVER,PLUS
        .int MINUSROT,TWOSWAP,CFETCH,SWAP,CFETCH
        .int EQUAL,QBRANCH,SRCH4,ONEPLUS
        .int BRANCH,SRCH3
SRCH4:  .int ROT,TWODUP,NOTEQUAL
        .int TOR,MINUSROT,RFROM,QBRANCH,SRCH5
        .int OVER,RFETCH,LESS,QBRANCH,SRCH5
        .int DROP,ONEPLUS,BRANCH,SRCH2
SRCH5:  .int RFROM,DROP,TWOSWAP,NIP,EQUAL
        .int QBRANCH,SRCH6,TUCK,MINUS,MINUSROT
        .int PLUS,SWAP,TRUE,EXIT
SRCH6:  .int DROP,FALSE,EXIT

// S  sliteral  ( csys: caddr u -- ; rsys: -- caddr u )  \ compile string
//   postpone (s")
//   dup c,
//   >r here r@ cmove r> allot ; immediate
    head SLITERAL,8,"sliteral",docolon,1
        .int LIT,XSQUOTE,COMMAXT
        .int DUPE,CCOMMA
        .int TOR,HERE,RFETCH,CMOVE,RFROM,ALLOT,EXIT

// S  -trailing  ( caddr u1 -- caddr u2 )  \ remove trailing blanks
//   begin  
//      2dup 1- chars + c@ bl =
//      over 0<> and
//   while  1-  repeat ;
    head MINUSTRAILING,9,"-trailing",docolon
TRAIL1: .int TWODUP,ONEMINUS,PLUS,CFETCH,BLANK,EQUAL
        .int OVER,ZERONEQUAL,ANDD
        .int QBRANCH,TRAIL2
        .int ONEMINUS
        .int BRANCH,TRAIL1
TRAIL2: .int EXIT

// Z  >counted  ( src n dst --  )  \  copy to counted str
//    2dup c! char+ swap cmove ; 
    head TOCOUNTED,8,">counted",docolon
        .int TWODUP,CSTORE,CHARPLUS,SWAP,CMOVE,EXIT

// C  word  ( char -- c-addr  )  \ word delimited by char in input stream
//    dup  source >in @ /string   -- c c adr n
//    dup >r   rot skip           -- c adr' n'
//    over >r  rot scan           -- adr" n"
//    dup if char- then        skip trailing delim.
//    r> r> rot -   >in +!        update >in offset
//    tuck -                      -- adr' n
//    here >counted               --
//    here                        -- a
//    bl over count + c! ;     append trailing blank
    head WORDD,4,"word",docolon
        .int DUPE,SOURCE,TOIN,FETCH,SLASHSTRING
        .int DUPE,TOR,ROT,SKIP
        .int OVER,TOR,ROT,SCAN
        .int DUPE,QBRANCH,WORD1,ONEMINUS  
WORD1:  .int RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
        .int TUCK,MINUS
        .int HERE,TOCOUNTED,HERE
        .int BLANK,OVER,COUNT,PLUS,CSTORE,EXIT

// C  parse  ( char -- c-addr n )  \  parse delimited by char
//    source >in @ /string     -- c adr n
//    2dup >r >r rot scan        -- adr' n'
//    dup if char- then        skip trailing delim.
//    r> r> rot -   >in +!        update >in offset
//    tuck -                      -- adr' n

    head PARSE,5,"parse",docolon
        .int SOURCE,TOIN,FETCH,SLASHSTRING
        .int TWODUP,TOR,TOR,ROT,SCAN
        .int DUPE,QBRANCH,PARSE1,ONEMINUS  
PARSE1:  .int RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
        .int TUCK,MINUS,EXIT

// H  isspace?  ( c -- flg )  \  true if char is a delimiter
//  bl 1+ u< ;
    nohead ISSPACEQ,docolon
        .int BLANK,ONEPLUS,ULESS,EXIT

// H  isnotspace?  ( c -- flg )  \ false if char s not a delimiter
//  isspace? 0= ;
    nohead ISNSPACEQ,docolon
        .int ISSPACEQ,ZEROEQUAL,EXIT

// H  xt-skip  ( addr1 n1 xt -- addr2 n2 )  \ skip all characters satisfying xt ( c -- f )
// >r begin 
//  dup while
//  over c@ r@ execute while
//     1 /string
// repeat  then
// r> drop ;
    nohead XTSKIP,docolon
        .int TOR
XTSKP1: .int DUPE,QBRANCH,XTSKP2
        .int OVER,CFETCH,RFETCH,EXECUTE
        .int QBRANCH,XTSKP2
        .int LIT,1,SLASHSTRING,BRANCH,XTSKP1
XTSKP2: .int RFROM,DROP,EXIT

// C  parse-name  ( " ccccc " -- c-addr u )  \ parse name delim'd by space
// source >in @ /string
// [’] isspace? xt-skip over >r
// [’] isnotspace? xt-skip ( end-word restlen r:start-word )
// 2dup 1 min + source drop - >in !
// drop r> tuck - ;
    head PARSENAME,10,"parse-name",docolon
        .int SOURCE,TOIN,FETCH,SLASHSTRING
        .int LIT,ISSPACEQ,XTSKIP,OVER,TOR
        .int LIT,ISNSPACEQ,XTSKIP
        .int TWODUP,LIT,1,MIN,PLUS,SOURCE
        .int DROP,MINUS,TOIN,STORE
        .int DROP,RFROM,TUCK,MINUS,EXIT

// Z  nfa>lfa  ( nfa -- lfa )  \ name adr -> link field
//    5 - ; 
    head NFATOLFA,7,"nfa>lfa",docolon
        .int LIT,5,MINUS,EXIT

// Z  nfa>cfa  ( nfa -- cfa )  \ name field to code field
//    count 7f and + @ ;    mask off 'smudge' bit
//  
    head NFATOCFA,7,"nfa>cfa",docolon
        .int COUNT
        .int LIT,0x07F,ANDD,PLUS,EXIT

// Z  cfa>nfa  ( cfa  -- nfa )  \ code field to name field
//   
//  dup 0 begin
//    >r 1- 2dup dup c@ 7f and 1+
//    + = r> or >r
//    2dup - 160 >
//  r> swap over or until
//  and swap drop;
   head CFATONFA,7,"cfa>nfa",docolon
        .int DUPE,LIT,0
CFTNF1: .int TOR,ONEMINUS,TWODUP,DUPE
        .int CFETCH,LIT,0x7F,ANDD,ONEPLUS
        .int PLUS,EQUAL,RFROM,ORR
        .int TOR,TWODUP,MINUS,LIT,160,GREATER
        .int RFROM,SWAP,OVER,ORR,QBRANCH,CFTNF1
        .int ANDD,SWAP,DROP,EXIT

// Z  immed?  ( nfa -- f  )  \ fetch immediate flag
//    1- c@ ;                      nonzero if immed
    head IMMEDQ,6,"immed?",docolon
        .int ONEMINUS,CFETCH,EXIT

// C  find  ( c-addr -- c-addr 0 )  \ if not found
// C                  xt  1      if immediate
// C                  xt -1      if "normal"
//    0 sorder @ ?dup 0<>        \  -- c-addr 0 flag
//    if 0 do over count         \  -- c-addr 0 c-addr+1 u
//        sorder i 1+ cells + @  \  -- c-addr 0 c-addr+1 u wid 
//        search-wordlist        \  -- c-addr 0; 0 | xt 1 | xt -1
//        ?dup 0<> if 
//           2swap 2drop unloop
//           exit then           \ --  xt 1 | xt -1
//        loop
//    then ;                     \ --  c-addr 0
    head FIND,4,"find",docolon
        .int LIT,0,SORDER,FETCH,QDUP,ZERONEQUAL
        .int QBRANCH,FIND3,LIT,0,XDO
FIND1:  .int OVER,COUNT,SORDER,II,ONEPLUS
        .int CELLS,PLUS,FETCH,SEARCHWORDLIST
        .int QDUP,ZERONEQUAL,QBRANCH,FIND2
        .int TWOSWAP,TWODROP,UNLOOP,EXIT
FIND2:  .int XLOOP,FIND1
FIND3:  .int EXIT

// C  literal  ( x --  )  \ append numeric literal
//    state @ if ['] lit ,xt , then ;  immediate
//  This tests STATE so that it can also be used
//  interpretively.  (ANSI doesn't require this.)
    head LITERAL,7,"literal",docolon,1
        .int STATE,FETCH,QBRANCH,LITER1
        .int LIT,LIT,COMMAXT,COMMA
LITER1: .int EXIT

// Z  digit?  ( c -- n -1 )  \ if c is a valid digit
// Z            -- x  0   otherwise
//    [ hex ] dup 5a > 20 and -     silly looking
//    dup 40 > c9 and +
//    dup 3a < d0 and +  100  -     but it works!
//    dup base @ u< ;
    head DIGITQ,6,"digit?",docolon
        .int DUPE,LIT,0x5a,GREATER,LIT,0x20,ANDD,MINUS
        .int DUPE,LIT,0x40,GREATER,LIT,0xc9,ANDD,PLUS
        .int DUPE,LIT,0x3a,LESS,LIT,0xd0,ANDD,PLUS
        .int LIT,0x100,MINUS
        .int DUPE,BASE,FETCH,ULESS,EXIT

// Z  ?sign  ( adr n -- adr' n' f )  \  get optional sign
//   advance adr/n if sign;  return NZ if negative
//    over c@                 -- adr n c
//    2c - dup abs 1 = and    -- +=-1, -=+1, else 0
//    dup if 1+               -- +=0, -=+2
//        >r 1 /string r>     -- adr' n' f
//    then ; 
    head QSIGN,5,"?sign",docolon
        .int OVER,CFETCH,LIT,0x2C,MINUS,DUPE,ABBS
        .int LIT,1,EQUAL,ANDD,DUPE,QBRANCH,QSIGN1
        .int ONEPLUS,TOR,LIT,1,SLASHSTRING,RFROM
QSIGN1: .int EXIT

// C  >number  ( ud adr u -- ud' adr' u' )  \ convert string to number
//    begin  dup while
//        over c@ digit?
//        0= if drop exit then
//        >r 2swap base @ ud*
//        r> m+ 2swap
//        1 /string
//    repeat ; 
    head TONUMBER,7,">number",docolon
TONUM1: .int DUPE,QBRANCH,TONUM3
        .int OVER,CFETCH,DIGITQ
        .int ZEROEQUAL,QBRANCH,TONUM2,DROP,EXIT
TONUM2: .int TOR,TWOSWAP,BASE,FETCH,UDSTAR
        .int RFROM,MPLUS,TWOSWAP
        .int LIT,1,SLASHSTRING,BRANCH,TONUM1
TONUM3: .int EXIT

// Z  ?number  ( c-addr -- n t | c-addr f )  \ string->number; false on error
// Z                 -- c-addr 0  if convert error
//    dup  0 0 rot count      -- ca ud adr n
//    ?sign >r  >number       -- ca ud adr' n'
//    if   r> 2drop 2drop 0   -- ca 0   (error)
//    else 2drop nip r> if negate then
//        -1  -- n -1   (ok)
//    then ; 
    head QNUMBER,7,"?number",docolon
        .int DUPE,LIT,0,DUPE,ROT,COUNT
        .int QSIGN,TOR,TONUMBER,QBRANCH,QNUM1
        .int RFROM,TWODROP,TWODROP,LIT,0
        .int BRANCH,QNUM3
QNUM1:  .int TWODROP,NIP,RFROM,QBRANCH,QNUM2,NEGATE
QNUM2:  .int LIT,-1
QNUM3:  .int EXIT

// Z  interpret  ( i*x -- j*x )  \ interpret given buffer
// this is a common factor of evaluate and quit.
// ref. dpans-6, 3.4 the forth text interpreter
//   0 >in !
//   begin bl word dup c@ while    \ -- textadr
//       find                      \ -- a 0/1/-1
//       ?dup if                   \ -- xt 1/-1
//           1+ state @ 0= or      \ immed or interp?
//           if execute else ,xt then
//       else                      \ -- textadr
//           ?number
//           if postpone literal   \ converted ok
//           else -13 throw        \ undefined word exception
//           then
//       then
//   repeat drop ;

   head INTERPRET,9,"interpret",docolon
        .int LIT,0,TOIN,STORE
INTPR1: .int BLANK,WORDD,DUPE,CFETCH,QBRANCH,INTPR9
        .int FIND,QDUP,QBRANCH,INTPR3
        .int ONEPLUS,STATE,FETCH,ZEROEQUAL,ORR
        .int QBRANCH,INTPR2,EXECUTE,BRANCH,INTPR8
INTPR2: .int COMMAXT,BRANCH,INTPR8
INTPR3: .int QNUMBER,QBRANCH,INTPR7
        .int LITERAL,BRANCH,INTPR8
INTPR7: .int TICKSOURCE,TWOFETCH,TYPE,CR,LIT,-13,THROW
INTPR8: .int BRANCH,INTPR1
INTPR9: .int DROP,EXIT


// H  interpretk ( -- )  \    interpret from keyboard
//   cr 0 blk !  0 'source-id !
//  refill drop          \ always true with keyboard source
//  interpret ;
    nohead INTERPRETK,docolon
        .int CR,LIT,0,BLK,STORE
        .int LIT,0,TICKSOURCEID,STORE
        .int REFILL,DROP,INTERPRET,EXIT

// H  interpretm  ( i*x -- j*x )  \ interpret from ??
//   begin  refill  while  interpret  repeat ;
    nohead INTERPRETM,docolon
IPRTM1: .int REFILL
        .int QBRANCH,IPRTM2
        .int INTERPRET
        .int BRANCH,IPRTM1
IPRTM2: .int EXIT

// F  include-file  ( i*x fid -- j*x )   \ interpret from file
//   >r ['] interpretm inputfbuf inputflen r>
//   0 nest-input ;
    head INCLUDEFILE,12,"include-file",docolon
        .int TOR,LIT,INTERPRETM,INPUTFBUF,INPUTFLEN,RFROM
        .int LIT,0,NESTINPUT,EXIT

// F  included  ( i*x caddr u -- j*x  )  \ interpret from file
//   r/o open-file throw
//   include-file ;
    head INCLUDED,8,"included",docolon
        .int RSLASHO,OPENFILE,THROW
        .int INCLUDEFILE,EXIT

// F  include  ( i*x " ccccc " -- j*x )  \ interpret from file 
//   parse-name included ;
    head INCLUDE,7,"include",docolon
        .int PARSENAME,INCLUDED,EXIT

// x  save-input  ( -- x1...xn n )  \ put input stream context on P-Stack
//   >in @  source                      \ always need >in and source
//   blk @ 0= if                        \ ignore source-id if blk<>0
//      source-id case
//          0 of  0  endof              \ nothing extra for tib
//         -1 of  0  endof              \ or evaluate strings
//          2dup >r swap file-position throw \ u1 a u2 ud get linestart
//          rot 1+ s>d d- 2 r>      \ in source,fpos,2
//      endcase
//   else  0  then                      \ nothing extra for block input
//   source-id blk @                    \ always need source-id & blk
//   rot 5 + ;                          \ min 5 items
    head SAVEINPUT,10,"save-input",docolon
        .int TOIN,FETCH,SOURCE
        .int BLK,FETCH,ZEROEQUAL
        .int QBRANCH,SVIP6
        .int SOURCEID,LIT,-1,QBRANCH,SVIP1
        .int LIT,0,OVER,EQUAL,QBRANCH,SVIP2,DROP
        .int LIT,0
SVIP1:  .int BRANCH,SVIP7
SVIP2:  .int LIT,-1,OVER,EQUAL,QBRANCH,SVIP4,DROP
        .int LIT,0
SVIP3:  .int BRANCH,SVIP7
SVIP4:  .int TWODUP,TOR,SWAP,FILEPOSITION,THROW,ROT,ONEPLUS
        .int LIT,0,DMINUS,LIT,2,RFROM
        .int DROP
SVIP5:  .int BRANCH,SVIP7
SVIP6:  .int LIT,0
SVIP7:  .int SOURCEID,BLK,FETCH
        .int ROT,LIT,5,PLUS,EXIT

// X  restore-input ( x1...xn n -- flag )  \ restore input stream context 
//   ?dup 0= if  true exit  then      \ can't restore a null input
//   swap >r                          \ put blk on return stack
//   r@ if
//        dup 5 =                   \ for block input, okay with 5 items
//   else
//       over case
//           0 of  dup 5 =  endof              // okay with 5 items for tib
//           -1 of  dup 5 =  endof              // or for evaluate string
//           >r dup 7 =                         // otherwise need 7 items
//           if  drop reposition-file throw     // reposition to start of line
//               input-fbuf input-flen r@ read-line throw       // re-read it
//               drop over =                    // true if same line length
//               r@ 5 rot                      // insert fileid & new item count
//           else  false  then                  // not correct items for file
//           r>
//       endcase
//   then
//  r> rot rot                    // put blk back
//  if  drop source-id >r               // drop unneeded count, save old id
//       blk !  'source-id !             // restore blk, source-id
//       blk @ ?dup
//       if  rot drop block swap  then   // for blk<>0, get valid buffer
//       'source 2!  >in !               // restore source and >in
//       r@ 0= r@ -1 = or 0=
//       if  r@ source-id <>    // close any file other current
//          if r@ close-file throw then
//       then
//       r> drop false
//  else 
//          begin drop 1- ?dup 0< until  true // discard bad parameters
//   then ;

    head RESTOREINPUT,13,"restore-input",docolon
        .int QDUP,ZEROEQUAL,QBRANCH,RSTIP1
        .int LIT,-1,EXIT
RSTIP1: .int SWAP,TOR,RFETCH
        .int QBRANCH,RSTIPD
        .int DUPE,LIT,5,EQUAL
        .int BRANCH,RSTIP8
RSTIPD: .int OVER,LIT,-1,QBRANCH,RSTIP2
        .int LIT,0,OVER,EQUAL,QBRANCH,RSTIP3,DROP
        .int DUPE,LIT,5,EQUAL
RSTIP2: .int BRANCH,RSTIP8
RSTIP3: .int LIT,-1,OVER,EQUAL,QBRANCH,RSTIP5,DROP
        .int DUPE,LIT,5,EQUAL
RSTIP4: .int BRANCH,RSTIP8
RSTIP5: .int TOR,DUPE,LIT,7,EQUAL
        .int QBRANCH,RSTIP6
        .int DROP,REPOSITIONFILE,THROW
        .int INPUTFBUF,INPUTFLEN,RFETCH,READLINE,THROW
        .int DROP,OVER,EQUAL
        .int RFETCH,LIT,5,ROT
        .int BRANCH,RSTIP7
RSTIP6: .int LIT,0
RSTIP7: .int RFROM,DROP
RSTIP8: .int RFROM,ROT,ROT
        .int QBRANCH,RSTIP9
        .int DROP,SOURCEID,TOR
        .int BLK,STORE,TICKSOURCEID,STORE
        .int BLK,FETCH,QDUP,QBRANCH,RSTIPE
        .int ROT,DROP,BLOCK,SWAP
RSTIPE: .int TICKSOURCE,TWOSTORE,TOIN,STORE
        .int RFETCH,ZEROEQUAL,RFETCH,LIT,-1,EQUAL,ORR
        .int ZEROEQUAL,QBRANCH,RSTIPC
        .int RFETCH,SOURCEID,NOTEQUAL,QBRANCH,RSTIPC
        .int RFETCH,CLOSEFILE,THROW
RSTIPC: .int RFROM,DROP,LIT,0
        .int EXIT
RSTIP9: .int LIT,0,XDO
// FIXME remove for loop
RSTIPA: .int DROP,XLOOP,RSTIPA
        .int LIT,-1
RSTIPB: .int EXIT

// Z  nest-input ( xt caddr u id blk -- )  \  ??
//   save-input dup
//   begin  rot >r 1- ?dup 0= until >r   // save current input specification
//   blk !  'source-id !  'source 2!     // set blk, source-id and source
//   catch                               // interpret single/multiple
//   r> dup
//  begin  r> rot rot 1- ?dup 0=  until
//   restore-input                       // restore old input specification
//   if  -4094 throw  then               // exception -4094 if unable to
//   throw ;                             // rethrow any interpretation exception
    head NESTINPUT,10,"nest-input",docolon
        .int SAVEINPUT,DUPE
NINPT2: .int ROT,TOR,ONEMINUS,QDUP,ZEROEQUAL,QBRANCH,NINPT2,TOR
        .int BLK,STORE,TICKSOURCEID,STORE,TICKSOURCE,TWOSTORE
        .int CATCH
        .int RFROM,DUPE
NINPT3: .int RFROM,ROT,ROT,ONEMINUS,QDUP,ZEROEQUAL,QBRANCH,NINPT3
        .int RESTOREINPUT
        .int QBRANCH,NINPT1
        .int LIT,-4094,THROW
NINPT1: .int THROW,EXIT


// X  refill  (  -- f )  \ fill input buffer from input source 
//   blk @ if  1 blk +!                    // for block input, use next
//             blk @ block pause 1024 true
//   else
//      source-id case
//          0 of  tib dup tibsize accept space true  endof
//          -1 of  false  endof
//          >r
//          input-fbuf input-flen r@ read-line throw pause     // read a line
//          if  input-fbuf swap true  then             // leave source-spec 
//          r>
//      endcase
//   then
//   dup if  >r 'source 2!  0 >in ! r>  then ; 
    head REFILL,6,"refill",docolon
        .int BLK,FETCH,QBRANCH,RFIL2
        .int LIT,1,BLK,PLUSSTORE,BLK
        .int FETCH,BLOCK,PAUSE,LIT,1024
        .int LIT,-1,BRANCH,RFIL7
RFIL2:  .int SOURCEID
        .int LIT,-1,QBRANCH,RFIL7
        .int LIT,0,OVER,EQUAL,QBRANCH,RFIL3
        .int DROP,TIB,DUPE,TIBSIZE,ACCEPT,SPACE,LIT,-1
        .int BRANCH,RFIL7
RFIL3:  .int LIT,-1,OVER,EQUAL,QBRANCH,RFIL4
        .int DROP,LIT,0,BRANCH,RFIL7
RFIL4:  .int TOR,INPUTFBUF,INPUTFLEN,RFETCH
        .int READLINE,THROW,PAUSE
        .int QBRANCH,RFIL5,INPUTFBUF,SWAP
        .int LIT,-1
RFIL5:  .int RFROM,DROP
RFIL7:  .int DUPE,QBRANCH,RFIL8,TOR,TICKSOURCE,TWOSTORE
        .int LIT,0,TOIN,STORE,RFROM
RFIL8:  .int EXIT


// C  evaluate  ( i*x c-addr u -- j*x )  \ interprt string
//   ['] interpret  rot rot -1 0  nest-input ;
 
    head EVALUATE,8,"evaluate",docolon
        .int LIT,INTERPRET,ROT,ROT,LIT,-1,LIT,0
        .int NESTINPUT,EXIT

// C  quit  ( -- ;r: i*x -- )  \  outer interpreter
//   r0 rp!  l0 lp! 0 state ! 
//   begin  ['] interpretk catch ?dup
//          if  case  -1 of  endof
//                    -2 of  abort"s @ abort"s 1+ 1+ @ type  endof
//                    dup ." exception # " .
//              endcase
//              0 state ! \  forth
//          else  state @ 0= if cr  ."  ok"  then
//          then
//   again ;
    head QUIT,4,"quit",docolon
        .int R0,RPSTORE,L0,LP,STORE
#ifdef COREBOOT
        .int LIT,0,STATE,STORE,BRCOLD
#endif
QUIT2:  .int LIT,0,STATE,STORE
QUIT1:  .int LIT,INTERPRETK,CATCH,QDUP
        .int QBRANCH,QUIT11
        .int LIT,-1,QBRANCH,QUIT5
        .int LIT,-1,OVER,EQUAL,QBRANCH,QUIT6,DROP
QUIT5:  .int BRANCH,QUIT8
QUIT6:  .int LIT,-2,OVER,EQUAL,QBRANCH,QUIT10,DROP
        .int XSQUOTE
        .byte 12
        .ascii " !!Aborted!!"
        .int TYPE
QUIT9:  .int BRANCH,QUIT8
QUIT10: .int DUPE,XSQUOTE
        .byte 12
        .ascii "Exception # "
        .int TYPE,DOT
        .int DROP
// QUIT8:  .int REVEAL,FORTH,BRANCH,QUIT2
QUIT8:  .int FORTH,BRANCH,QUIT2
QUIT11: .int STATE,FETCH,ZEROEQUAL,QBRANCH,QUIT1
        .int CR,XSQUOTE
        .byte 3
        .ascii "ok "
        .int TYPE
QUIT7:  .int BRANCH,QUIT1         /*  eternal loop  */

// C  abort  ( i*x -- ; R: j*x -- )  \ 
//     -1 throw ;
    head ABORT,5,"abort",docolon
        .int LIT,-1,THROW,EXIT

// Z  ?abort  ( f c-addr u --  )  \ abort & print msg
//    rot if type  -2 throw  then 2drop ;
    head QABORT,6,"?abort",docolon
        .int ROT,QBRANCH,QABO1,TYPE
        .int LIT, -2,THROW
QABO1:  .int TWODROP,EXIT

// C  abort"  ( i*x 0  -- i*x; R: j*x -- j*x )  \ if x1=0
// C          ( i*x x1 --    ; R: j*x --   )  \ if x1<>0
//    postpone s" postpone ?abort ;  immediate
    head ABORTQUOTE,6,"abort\"",docolon,1
        .int SQUOTE
        .int LIT,QABORT,COMMAXT
        .int EXIT

// C  '  ( -- xt )  \ find word in dictionary
//    bl word find
//    0= abort" ?" ;
    head TICK,1,"\'",docolon
        .int BLANK,WORDD,FIND,ZEROEQUAL,XSQUOTE
        .byte 1,'?'
        .int QABORT,EXIT

// C  char  ( -- char )  \  parse ASCII character
//    bl word 1+ c@ ; 
    head CHAR,4,"char",docolon
        .int BLANK,WORDD,ONEPLUS,CFETCH,EXIT

// C  [char]  ( -- )  \ compile character literal
//    char  ['] lit ,xt  , ;  immediate
    head BRACCHAR,6,"[char]",docolon,1
        .int CHAR
        .int LIT,LIT,COMMAXT
        .int COMMA,EXIT

// C  (  ( -- )  \ skip input until )
//    source-id
//    if begin [char] ) parse 2drop
//       source nip >in @ =
//       dup if drop source + 1- c@ [char] ) <>
//              dup if drop refill then
//           then
//       0= until  
//    else
//       [char] ) parse 2drop
//    then
//     ;  immediate
    head PAREN,1,"(",docolon,1
        .int SOURCEID,QBRANCH,PAREN3
PAREN1: .int LIT,0x29,PARSE,TWODROP
        .int SOURCE,NIP,TOIN,FETCH,EQUAL
        .int DUPE
        .int QBRANCH,PAREN2
        .int DROP,SOURCE,PLUS,ONEMINUS,CFETCH,LIT,0x29,NOTEQUAL
        .int DUPE
        .int QBRANCH,PAREN2
        .int DROP,REFILL
PAREN2: .int ZEROEQUAL,QBRANCH,PAREN1
        .int EXIT
PAREN3: .int LIT,0x29,PARSE,TWODROP
PAREN4: .int EXIT

// X  \  ( -- )  \ ignore line
//    blk @ 0<> if
//       >in @  64 / 1+ 16 MIN 64 *
//    else source nip
//    >in ! ; immediate
    head BSLASH,1,"\\",docolon,1
        .int BLK,FETCH,ZERONEQUAL,QBRANCH,BSLH1
        .int TOIN,FETCH,LIT,64,SLASH,ONEPLUS
        .int LIT,16,MIN,LIT,64,STAR,BRANCH,BSLH2
BSLH1:  .int SOURCE,NIP
BSLH2:  .int TOIN,STORE,EXIT

// C  defer@   ( xt1 -- xt2 )  \ get action of a defered word
//  >body @ ;
    head DEFERFETCH,6,"defer@",docolon
        .int TOBODY,FETCH,EXIT

// C  defer!  ( xt2 xt1 -- )  \ set action of a defered word
//  >body ! ;
    head DEFERSTORE,6,"defer!",docolon
        .int TOBODY,STORE,EXIT

// C  action-of  ( ???
//  state @ if
//      postpone ['] postpone defer@
//  else
//      ' defer@
//  then ; immediate
    head ACTIONOF,9,"action-of",docolon,1
        .int STATE,FETCH,QBRANCH,ACTSN1
        .int BRACTICK,LIT,DEFERFETCH,COMMAXT,BRANCH,ACTSN2
ACTSN1: .int TICK,DEFERFETCH
ACTSN2: .int EXIT

// C  defer  ( "name" -- )  \ define a deferred word
//    create  -4 allot dodefer .cf ['] (null) , ;
    head DEFER,5,"defer",docolon
        .int CREATE,LIT,-4,ALLOT
//        .int HERE,lastcfa,STORE
        .int LIT,dodefer,COMMACF
        .int LIT,PNULL,COMMA,EXIT

// X  value   ( x "<spaces>name" -- )  \ create value
//   create , does> @
    head VALUE,5,"value",docolon
        .int CREATE,COMMA,XDOES
        call dodoes
        .int FETCH,EXIT

// X  to  ( x "<spaces>name" -- )  \ set a value
//   ' >body state @ if  ,lit postpone !
//                   else  !  then // immediate
    head TO,2,"to",docolon,1
        .int TICK,TOBODY,STATE,FETCH,QBRANCH,TO1
        .int LIT,LIT,COMMAXT,COMMAXT,LIT,STORE,COMMAXT,BRANCH,TO2
TO1:    .int STORE
TO2:    .int EXIT

// Z  is  ( xt "<spaces>name" -- )  \ set a deferred word
//  state @ if
//      postpone ['] postpone defer!
//  else
//      ' defer!
//  then; immediate
    head IS,2,"is",docolon,1
        .int STATE,FETCH,QBRANCH,IS1
        .int BRACTICK,LIT,DEFERSTORE,COMMAXT,BRANCH,IS2
IS1:    .int TICK,DEFERSTORE
IS2:    .int EXIT

// =====================BLOCK words======================
// User variables (initially zero)
//  #BUFS                ; number of buffers 1..4
//  BLOCK-IDS            ; IDs of files mapped to blocks (4 cells or zero)
//  'BUFFER              ; address of block buffer data structure or zero
//
// The block buffer data structure is as follows:
//  +0 (4) Buffer MRU list
//  +4 (16) Block number for each buffer (or 0) with bit 31=dirty flag
//  +20 (x) 1024 bytes per block buffer
// 1-4 block buffers are allocated in RAM with BLKS. EMPTY-BUFFERS is used by
// BLKS and COLD to initialise the structure.
// If any attempt to access blocks is made before this, exception -35
// is THROWn. Interpretation words using blocks may THROW implementation-
// defined file exceptions.

// B  empty-buffers ( -- )  \ clear file buffers. no flush to disk
//   'buffer @ ?dup
//   if  0x4321 over ! 1 cell +
//       16 0 fill
//   then ;
   head EMPTYBUFFERS,13,"empty-buffers",docolon
        .int TICKBUFFER,FETCH,QDUP,QBRANCH,MTB2
        .int LIT,0x04030201,OVER,STORE,LIT,4,PLUS
        .int LIT,16,ERASE
MTB2:   .int EXIT

// Z  blks ( n -- )   \  initialize n block buffers
//   'buffer @ over > or
//   if  -4092 throw  then       ; exception if blocks already allocated
//   dup #bufs !
//   here 'buffer !          ; store address of structure in ram
//   10 lshift 20 + allot           ; allocate region
//   empty-buffers ;             ; reset dataspace & initialise struct
    head BLKS,4,"blks",docolon
        .int TICKBUFFER,FETCH,OVER,LIT,4,UGREATER,ORR
        .int QBRANCH,BLKS1
        .int LIT,-4092,THROW
BLKS1:  .int DUPE,NUMBUFS,STORE
        .int HERE,TICKBUFFER,STORE
        .int LIT,10,LSHIFT,LIT,20,PLUS,ALLOT
        .int EMPTYBUFFERS,EXIT

// Four block mappings are available to map files to blocks :
//  n=0  blocks     0...0x1fffffff
//  n=1  blocks     2...0x3fffffff
//  n=2  blocks     4...0x5fffffff
//  n=3  blocks     6...0x7fffffff

// Z  map  ( caddr u n -- )  \ map range of blocks to a file 0-3
//   dup unmap                  clear bitmap
//   >r  r/w open-file throw
//   r@ mapping bitset           set bitmap
//   r> cells blockid + ! ;
    head MAP,3,"map",docolon
        .int DUPE,UNMAP
        .int TOR,RSLASHW,OPENFILE,THROW
        .int RFETCH,MAPPING,BITSET
        .int RFROM,CELLS,BLOCKID,PLUS,STORE,EXIT

// Z  unmap  ( n -- )  \ unmap file from range of blocks
//   dup 3 u> if  -35 throw  then     invalid block range
//   flush
//   dup mapping bittest if            ( n )
//      blockid over cells + dup  @         ( n a fid ) 
//      close-file throw
//      0 swap !
//   then
//      mapping bitclear ;
    head UNMAP,5,"unmap",docolon
        .int DUPE,LIT,3,UGREATER,QBRANCH,UNMAP2
        .int LIT,-35,THROW
UNMAP2: .int FLUSH,DUPE,MAPPING,BITTEST,QBRANCH,UNMAP1
        .int BLOCKID,OVER,CELLS,PLUS,DUPE,FETCH
        .int CLOSEFILE,THROW
        .int LIT,0,SWAP,STORE
UNMAP1: .int MAPPING,BITCLEAR,EXIT

// Z  mapped?  ( u -- flag )  \ block is mapped to file
//  29 rshift 3 and mapping bittest ;
    head MAPPEDQ,7,"mapped?",docolon
        .int LIT,29,RSHIFT,LIT,03,ANDD
        .int MAPPING,BITTEST,EXIT

// Z  block-map  ( u -- u )   \ convert block number to mapped block number
//  29 lshift + ;
    head BLOCKTOMAP,9,"block-map",docolon
        .int LIT,29,LSHIFT,PLUS,EXIT

// Z  block-seek  ( u -- fileid )  \ seek to a block. mapped files only
//   blocks? 1-dup 29 rshift 3 and     \  u n
//   cells blockid + @ >r           \ u  R: fid  
//   1024 um* r@ reposition-file throw
//   r> ;
    head BLOCKSEEK,10,"block-seek",docolon
        .int BLOCKSQ
        .int ONEMINUS,DUPE,LIT,29,RSHIFT
        .int LIT,3,ANDD,CELLS,BLOCKID,PLUS,FETCH,TOR
        .int LIT,1024,UMSTAR,RFETCH,REPOSITIONFILE,THROW
        .int RFROM,EXIT

// Z  SAVEBUF  ( n --  )  \ Save block in buffer n if necessary
//  dup update? if
//      dup mapped? if
//          dup id>blk swap id>buf
//          1024 rot blockseek writefile throw
//      else         \ n
//          dup id>buf swap     \ addr n
//          (blockdev!)
//      then
//  else
//       drop
//  then ;
    head SAVEBUF,7,"savebuf",docolon
// if buffer is dirty write to file
        .int DUPE,UPDATEQ,QBRANCH,SVBUF2
        .int DUPE,MAPPEDQ,QBRANCH,SVBUF1
        .int DUPE,BLKNFETCH,SWAP,IDTOBUF
        .int LIT,1024,ROT,BLOCKSEEK
        .int WRITEFILE,THROW,EXIT
SVBUF1: .int IDTOBUF,SWAP,BLOCKDEVSTORE,EXIT
SVBUF2: .int DROP,EXIT

// Z  blocks?  ( -- )  \  throw an exception if no buffer blocks defined
//   'buffer @ 0=
//   if  -35 throw  then ;
    head BLOCKSQ,7,"blocks?",docolon
        .int TICKBUFFER,FETCH,ZEROEQUAL
        .int QBRANCH,QBUF1,LIT,-35,THROW
QBUF1:  .int EXIT

// H  id>blk  ( n -- addr )  \ get block-id address
// block id 1 ... 4
//  cells 'buffer @ + 
//  ;
    nohead IDTOADDR,docolon
        .int CELLS,TICKBUFFER,FETCH,PLUS
        .int EXIT

// H  block#@  ( n -- u )  \ get block 
//  id>blk @ 0x80000000 invert and ;
    nohead BLKNFETCH,docolon
        .int IDTOADDR,FETCH,LIT,0X7FFFFFFF,ANDD
        .int EXIT

// H  id@buf  n  -- caddr    get buffer address
// 1- 1024 * 'buffer @ 5 cells + + ;
    nohead IDTOBUF,docolon
        .int ONEMINUS,LIT,1024,STAR,TICKBUFFER,FETCH
        .int LIT,5,CELLS,PLUS,PLUS,EXIT

// H  update?  n -- flg    buffer update status - dirty flag
//  id@blk @ 0< ;
    nohead UPDATEQ,docolon
        .int IDTOBUF,FETCH,ZEROLESS
        .int EXIT

// H  update!  n --  set buffer update status
// id@blk dup @ 0x8000000 or swap !;
    nohead UPDATESTORE,docolon
        .int IDTOADDR,DUPE,FETCH,LIT,0x80000000
        .int ORR,SWAP,STORE,EXIT

// H  mru!   n --   \ set buffer n ( 1-#bufs) as recent used 
// dup #nubfs > if -303 throw then
// >r 0 begin
//  dup #bufs < while 
//  'buffer @ over + c@ r@ <> while 
//     1+
//  repeat then
//  r> swap 'buffer @ tuck tuck 1+ swap
//  move c!
// ;
    nohead MRUSTORE,docolon
//    head MRUSTORE,4,"mru!",docolon
        .int DUPE,NUMBUFS,FETCH,GREATER,QBRANCH,MRUST0
        .int LIT,-303,THROW
MRUST0: .int TOR,LIT,0x0
MRUST1: .int DUPE,NUMBUFS,FETCH,LESS,QBRANCH,MRUST2
        .int TICKBUFFER,FETCH,OVER,PLUS,CFETCH
        .int RFETCH,NOTEQUAL,QBRANCH,MRUST2
        .int ONEPLUS,BRANCH,MRUST1
MRUST2: .int RFROM,SWAP,TICKBUFFER,FETCH
        .int TUCK,TUCK,ONEPLUS,SWAP,MOVE,CSTORE
        .int EXIT

// Z  findbuf  ( u -- addr flag )  \ Find buffer for blk# u flag TRUE if loaded
//                                 Updates MRU list. 
// blocks?
// dup 1< if -35 throw then
// >r 0 0  begin                    \ flg bufid          
//      1+ dup block@# r@ = if     \ already assigned
//          swap invert swap        \ set flag
//      then                        \ flag bufid      
//      2dup #buf @ = or
//  until                           \ flag bufid
//  
//  over if 
//      r> drop      \ block found 
//  else             \ use LRU 
//      drop 'buffer #buf @ 1- chars + c@ 
//      dup savebuf dup mru! r> over id>blk !
//  then
//  id>buf swap
//  ;
    head FINDBUF,7,"findbuf",docolon
        .int BLOCKSQ,DUPE,ZEROLESS
        .int QBRANCH,FNDBFA,LIT,-35,THROW
FNDBFA: .int TOR,LIT,0,LIT,0
FNDBF1: .int ONEPLUS,DUPE,BLKNFETCH,RFETCH,EQUAL,QBRANCH,FNDBF2
        .int SWAP,INVERT,SWAP
FNDBF2: .int TWODUP,NUMBUFS,FETCH,EQUAL,ORR
        .int QBRANCH,FNDBF1
        .int OVER,QBRANCH,FNDBF3
        .int RFROM,DROP,BRANCH,FNDBF4
FNDBF3: .int DROP,TICKBUFFER,FETCH,NUMBUFS,FETCH
        .int ONEMINUS,CHARS,PLUS,CFETCH
        .int DUPE,SAVEBUF,DUPE,MRUSTORE,RFROM,OVER
        .int IDTOADDR,STORE
FNDBF4: .int IDTOBUF,SWAP,EXIT

// B  update ( -- )  \  mark current buffer as dirty 
//   blocks?
//   'buffer @ dup c@   \  get MRU buffer
//   cells + dup @ 1 31 lshift or swap ! ;  // set update flag
    head UPDATE,6,"update",docolon
        .int BLOCKSQ,TICKBUFFER,FETCH
        .int DUPE,CFETCH,CELLS,PLUS,DUPE
        .int FETCH,LIT,0X80000000,ORR
        .int SWAP,STORE,EXIT

// B  save-buffers ( -- )  \ Write buffers to disk
//   blocks? 
//   #bufs @ 0 do  i savebuf  loop ;
    head SAVEBUFFERS,12,"save-buffers",docolon
        .int BLOCKSQ
        .int NUMBUFS,FETCH,LIT,0,XDO
SAVEB1: .int II,SAVEBUF,XLOOP,SAVEB1
        .int EXIT

// B  flush  ( -- )  \ write to disk then clear block buffers
//   save-buffers empty-buffers ;
    head FLUSH,5,"flush",docolon
        .int SAVEBUFFERS,EMPTYBUFFERS,EXIT

// B  buffer  ( u -- addr )  \ find buffer for block u
//   findbuf drop ;
    head BUFFER,6,"buffer",docolon
        .int FINDBUF,DROP
        .int EXIT

// B  block  ( u -- addr )  \ read blk# u from device and place in buffer
//   dup findbuf if
//      nip
//   else                 \ not in buffer
//      over mapped? if   \ u addr 
//            tuck 1024 rot block-seek read-file throw drop
//      else
//          dup rot       addr addr u
//          (blockdev@)
//      then
//   then ;
    head BLOCK,5,"block",docolon
        .int DUPE,FINDBUF,QBRANCH,BLOCK1
        .int NIP,EXIT
BLOCK1: .int OVER,MAPPEDQ,QBRANCH,BLOCK2
        .int TUCK,LIT,1024,ROT
        .int BLOCKSEEK,READFILE,THROW,DROP
        .int EXIT
BLOCK2: .int DUPE,ROT,BLOCKDEVFETCH,EXIT

// B  load  ( i*x u -- j*x )  \ interpret form block u
//   >r
//   ['] interpret r@ block 1024 0 r>
//   nest-input ; 
    head LOAD,4,"load",docolon
        .int TOR
        .int LIT,INTERPRET,RFETCH
        .int BLOCK,LIT,1024,LIT,0,RFROM
        .int NESTINPUT,EXIT

// B  thru  ( i*x u1 u2 -- j*x )  \ load blocks from u1 to u2
//   1+ swap do  i load  loop ;
    head THRU,4,"thru",docolon
        .int ONEPLUS,SWAP,XDO
THRU1:  .int II,LOAD,XLOOP,THRU1
        .int EXIT

// B  list  ( u -- )  \ dump the contents of block u to screen
// base @ >r decimal dup scr ! block
// 16 0 do
//     i 2 .r space dup i 64 * + 64
//     -trailing type cr
// loop
// drop
// r> base ! ;
    head LIST,4,"list",docolon
        .int BASE,FETCH,TOR,DECIMAL
        .int DUPE,SCR,STORE,BLOCK
        .int LIT,16,LIT,0,XDO
LST1:   .int II,LIT,2,DOTR,SPACE,DUPE
        .int II,LIT,64,STAR,PLUS,LIT,64
        .int MINUSTRAILING,TYPE,CR,XLOOP,LST1
        .int DROP,RFROM,BASE,STORE,EXIT

// vectored user words for block access

// Z  blockdev@ ( addr u -- )  \ fetch disk block u to memory addr
//  (block@) @ execute ;
    head BLOCKDEVFETCH,9,"blockdev@",docolon
        .int BLKFETCHV,FETCH,EXECUTE,EXIT

// Z blockdev! ( addr u  --  )  \ store buffer a addr to disk block u
//  (block!) @ execute ;
    head BLOCKDEVSTORE,9,"blockdev!",docolon
        .int BLKSTOREV,FETCH,EXECUTE,EXIT

// ====================FILE words========================
// 0x00   o_rdonly
// 0x01   w_only
// 0x02   o_rdwr
// 0x40   o_creat
// 0x80   o_excl
// 0x400  o_append
// 0x00   seek_set
// 0x01   seek_cur
// 0x02   seek_end

// F  r/o  ( -- fam )  \ read only file access mode
//   0 CONSTANT R/O
    head RSLASHO,3,"r/o",docon
        .int 0

// F  w/o  ( -- fam )  \ write only file access mode
//   1 CONSTANT W/O
    head WSLASHO,3,"w/o",docon
        .int 1

// F  r/w  ( -- fam )  \ read write file access mode
//   2 CONSTANT R/W
    head RSLASHW,3,"r/w",docon
        .int 2

// F  bin  ( fam -- fam  )  \ set file access mode to binary
//  1 or
    head BIN,3,"bin",docolon
        .int EXIT

// H  o_creat  ( -- fam )   \ create file access mode
//  0x040 constant o/creat
    nohead OCREAT,docon
        .int 0x040

// H  inputflen
//   (inputflen) @;
    nohead INPUTFLEN,docolon
        .int BRINPUTFLEN,FETCH,EXIT

// H  input-fbuf (--c-addr)
//  (input-fbuf) @ ;
    nohead INPUTFBUF,docolon
        .int BRINPUTFBUF,FETCH,EXIT

 
// F  create-file  ( caddr u fam -- fileid ior )  \ create a file
//   o_creat or (createfile) ;
     head CREATEFILE,11,"create-file",docolon
        .int LIT,0x40,ORR,BRCREATEFILE,EXIT

// F  open-file  ( caddr u fam -- fileid ior )  \ open a file
//   dup w/o = if  drop r/w  then
//   (open-file) ;
    head OPENFILE,9,"open-file",docolon
        .int DUPE,WSLASHO,EQUAL
        .int QBRANCH,OPFIL2
        .int DROP,RSLASHW
OPFIL2: .int BROPENFILE,EXIT

// F  delete-file  ( caddr u -- ior )  \ delete a file
//  (deletefile) ; 
    head DELETEFILE,11,"delete-file",docolon
        .int BRDELETEFILE,EXIT

// F  file-position  ( fileid -- ud ior )  \ fetch file pointer location
//  (fileposition) ;
    head FILEPOSITION,13,"file-position",docolon
        .int BRFILEPOSITION,EXIT

// F  reposition-file  ( ud fileid -- ior ) \ set file pointer location
//  (repositionfile) ; 
    head REPOSITIONFILE,15,"reposition-file",docolon
        .int BRREPOSITIONFILE,EXIT

// F  file-size  ( fileid -- ud ior )  \ size of file
//  (filesize)
    head FILESIZE,9,"file-size",docolon
        .int BRFILESIZE,EXIT

// F  read-file  ( caddr u1 fileid -- u2 ior )  \ read up to u1 bytes form file u2 is actual count read
//   >r swap r>  (readfile) 
    head READFILE,9,"read-file",docolon
        .int TOR,SWAP,RFROM,BRREADFILE
        .int EXIT

// F  write-file  ( caddr u1 fileid -- ior )  \ write u1 bytes to file
//   rot swap (writefile);
    head WRITEFILE,10,"write-file",docolon
        .int ROT,SWAP,BRWRITEFILE,EXIT
    
// F  close-file  ( fileid -- ior )  \ close a file
    head CLOSEFILE,10,"close-file",docolon
        .int BRCLOSEFILE,EXIT

// F  read-line  ( caddr u1 fileid -- u2 flag ior )  \ read u1 bytes from a file, u2 is actual read
// read a line from file
// success : flg = true,ior, u2= #read
// if eof and u2=0 : flag=false,ior=0
// if ior<>0 , exception occured, flag=false,u2=0
//
// >r 0 begin     ( a u c R:f )
//     2dup >     
// while          ( a u c )
//     rot dup 1 r@ read-file  ( u c a u2 ior )
//     if   ( ior errors )
//          >r rot 2drop false 2r> nip exit
//     then      ( u c a u2)
//     0=  if   \ eof?  ( u c a)
//         rot 2drop r> drop    ( c )
//         dup 0<> 0 exit
//    then  ( u c a )
//    dup c@ EOL = if   ( u c a )
//         rot 2drop r> drop true 0 exit
//    then  ( u c a )
//   1+ rot rot 1+
// repeat  ( a u c)
//  >r 2drop 2r> nip  true 0
// ;

    head READLINE,9,"read-line",docolon
        .int TOR,LIT,0
RDLIN1: .int TWODUP,GREATER,QBRANCH,RDLIN8
        .int ROT,DUPE,LIT,1,RFETCH,READFILE
        .int QBRANCH,RDLIN2,TOR,ROT,TWODROP
        .int LIT,0,TWORFROM,NIP,EXIT
RDLIN2: .int ZEROEQUAL,QBRANCH,RDLIN4
        .int ROT,TWODROP,RFROM,DROP
        .int DUPE,ZERONEQUAL,LIT,0,EXIT
RDLIN4: .int DUPE,CFETCH,LIT,0xa,EQUAL,QBRANCH,RDLIN5
        .int ROT,TWODROP,RFROM,DROP,LIT,-1,LIT,0,EXIT
RDLIN5: .int ONEPLUS,ROT,ROT,ONEPLUS,BRANCH,RDLIN1
RDLIN8: .int TOR,TWODROP,TWORFROM,NIP,LIT,-1,LIT,0,EXIT

// F  write-line  ( caddr u1 fileid -- ior )  \ write u1 bytes to a file
// dup >r write-file
// here NL over c! 1 r> write-file
// or ;
    head WRITELINE,10,"write-line",docolon
        .int DUPE,TOR,WRITEFILE,HERE
        .int LIT,0x0a,OVER,CSTORE,LIT,1
        .int RFROM,WRITEFILE,ORR,EXIT

// F  resize-file  ( ud fileid -- ior )  \ truncate a file ( TODO)

// F  file-status  ( caddr u -- x ior )  \ get file status (TODO)?

// F  flush-file   ( fileid -- ior )  \ write file buffers to storage
//   file-position >r 2drop r> ;         just checks fileid is valid
    head FLUSHFILE,10,"flush-file",docolon
        .int FILEPOSITION,NIP,NIP,EXIT

// F  rename-file  ( caddr1 u1 caddr2 u2 -- ior )  \ rename a file
    head RENAMEFILE,11,"rename-file",docolon
        .int BRRENAMEFILE,EXIT

// ===================COMPILER==========================
// C  create  ( -- )  \  create an empty definition
//    get-current @ , 0 c,         link & immed field
//    here get-current !            
//    here latest !          new "latest" link
//    bl word c@ 1+ allot    name field
//    docreate ,cf           store code field
    head CREATE,6,"create",docolon
        .int GETCURRENT,FETCH,COMMA,LIT,0,CCOMMA
        .int HERE,GETCURRENT,STORE
        .int HERE,LATEST,STORE
        .int BLANK,WORDD,CFETCH,ONEPLUS,ALLOT
        .int LIT,docreate,COMMA
        .int EXIT
        
// H  (does>)  --      run-time action of DOES>
//    r>              adrs of headless DOES> def'n
//    latest @ nfa>cfa    code field to fix up
//    !cf ; 
    nohead XDOES,docolon
        .int RFROM,LATEST,FETCH,NFATOCFA,STORECF
        .int EXIT

// C  does>  ( csys : ; rsys :  -- addr )  \  change action of latest def'n
//    compile (does>)
//    dodoes ,call ;  immediate
    head DOES,5,"does>",docolon,1
        .int LIT,XDOES,COMMAXT
        .int LIT,dodoes,COMMACALL,EXIT

// C  recurse  ( -- )  \ recurse current definition
//    latest @ nfa>cfa ,xt ;  immediate
    head RECURSE,7,"recurse",docolon,1
        .int LIT,lastcfa,FETCH,COMMAXT,EXIT
        .int LATEST,FETCH,NFATOCFA,COMMAXT,EXIT

// C  [  ( -- )  \ enter interpretive state
//    0 state ! ;  immediate
    head LEFTBRACKET,1,"[",docolon,1
        .int LIT,0,STATE,STORE,EXIT

// C  ]  ( --  )  \  enter compiling state
//    -1 state ! ; 
    head RIGHTBRACKET,1,"]",docolon
        .int LIT,-1,STATE,STORE,EXIT

// Z  hide  (  --  )  \ "hide" latest definition
//    latest @ dup c@ 80 or swap c! ; 
    head HIDE,4,"hide",docolon
        .int LATEST,FETCH,DUPE,CFETCH,LIT,0x80,ORR
        .int SWAP,CSTORE,EXIT

// Z  reveal  ( -- )  \ "reveal" latest definition
//    latest @ dup c@ 7f and swap c! ; 
    head REVEAL,6,"reveal",docolon
        .int LATEST,FETCH,DUPE,CFETCH,LIT,0x7F,ANDD
        .int SWAP,CSTORE,EXIT

// C  immediate  ( --   )  \ make last def'n immediate
//    1 latest @ 1- c! ;    set immediate flag
    head IMMEDIATE,9,"immediate",docolon
        .int LIT,1,LATEST,FETCH,ONEMINUS,CSTORE
        .int EXIT

// C  :  (  --  )  \ begin a colon definition
//    create hide ] !colon ; 
    head COLON,1,":",docolon
        .int CREATE,HIDE,RIGHTBRACKET
        .int STORCOLON,EXIT

// C  ;  ( -- )  \ end colon definition 
//    reveal  ,exit
//    postpone [  ;  immediate
    head SEMICOLON,1,";",docolon,1
        .int REVEAL,CEXIT
        .int LEFTBRACKET,EXIT

// C  [']   ( -- )  \  find word & compile as literal
//    '  ['] lit ,xt  , ;  immediate
//  When encountered in a colon definition, the
//  phrase  ['] xxx  will cause   LIT,xxt  to be
//  compiled into the colon definition (where
//  (where xxt is the execution token of word xxx).
//  When the colon definition executes, xxt will
//  be put on the stack.  (All xt's are one cell.)
    head BRACTICK,3,"[']",docolon,1
        .int TICK               /*  get xt of 'xxx' */
        .int LIT,LIT,COMMAXT    /*  append LIT action */
        .int COMMA,EXIT        /*  append xt literal */

// C  postpone  ( -- )  \ postpone compile action of word
//    bl word find
//    dup 0= abort" ?"
//    0< if   -- xt  non immed: add code to current
//                   def'n to compile xt later.
//        ['] lit ,xt  i,     add "lit,xt,commaxt"
//        ['] ,xt ,xt         to current definition
//    else  ,xt      immed: compile into cur. def'n
//    then ;  immediate
    head POSTPONE,8,"postpone",docolon,1
        .int BLANK,WORDD,FIND,DUPE,ZEROEQUAL,XSQUOTE
        .byte 1,'?'
        .int QABORT,ZEROLESS,QBRANCH,POST1
        .int LIT,LIT,COMMAXT,COMMA
        .int LIT,COMMAXT,COMMAXT,BRANCH,POST2
POST1:  .int COMMAXT
POST2:  .int EXIT
               
// Z  compile  ( -- )  \ append inline execution token
//    r> dup cell+ >r @ ,xt ; 
//  The phrase ['] xxx ,XT appears so often that
//  this word was created to combine the actions
//  of LIT and ,XT.  It takes an inline literal
//  execution token and appends it to the dict.
//     head COMPILE,7,'compile',docolon
//         .int RFROM,DUPE,CELLPLUS,TOR
//         .int FETCH,COMMAXT,EXIT
//  N.B.: not used in the current implementation


// C  :noname  ( -- xt )  \ create unamed forth definition 
// fails if it is the first word in the user dictionary !
// TODO store cfa for use by recurse 
    head NONAME,7,":noname",docolon
//  .int HERE,LIT,docolon,COMMACF
        .int HERE,DUPE,LIT,lastcfa,STORE,LIT,docolon,COMMACF
        .int RIGHTBRACKET,EXIT

// Z  internal  ( -- a-addr )  \ current and new definitions are hidden
//   latest @ nfa>lfa @ ; immediate
    head INTERNAL,8,"internal",docolon,1
        .int LATEST,FETCH,NFATOLFA,FETCH,EXIT

// Z  external  ( -- a-addr )  \ current and new definitions are in dictionary
//   latest @ nfa>lfa ; immediate
    head EXTERNAL,8,"external",docolon,1
        .int LATEST,FETCH,NFATOLFA,EXIT

// Z  module  ( x1 x2 --  )  \ finalize action of internal and external
//   ! ; immediate
    head MODULE,6,"module",docolon,1
        .int STORE,EXIT

// O  +field  ( n "<spaces>name" -- Exec: addr -- 'addr )  \ add n bytes to structure
// create over , +
//  does> @ + ;
    head PLUSFIELD,6,"+field",docolon
        .int CREATE,OVER,COMMA,PLUS,XDOES
        call dodoes
        .int FETCH,PLUS,EXIT

// O  field:  ( -- )  \ add 1 cell field to structure
//  1 cells +field ;
    head FIELDD,6,"field:",docolon
        .int LIT,0x01,CELLS,PLUSFIELD,EXIT

// O  cfield:  ( -- )  \ add 1 byte field to structure
//  1 chars +field ;
    head FIELDC,7,"cfield:",docolon
        .int LIT,0x01,CHARS,PLUSFIELD,EXIT

// O  wfield:  ( -- )  \ add 2-byte field to structure
//  waligned 2 chars + field ;
    head FIELDW,7,"wfield:",docolon
        .int LIT,0x02,CHARS,PLUSFIELD,EXIT

// O  begin-structure  ( " <spaces>name" -- addr 0 ; -- size )  \ define structure
//  create here 0 0 ,
// does> @ ;
    head STRUCTBEG,15,"begin-structure",docolon
        .int CREATE,HERE,LIT,0x00,LIT,0x00
        .int COMMA,XDOES
        call dodoes
        .int FETCH,EXIT

// O  end-structure  ( addr n -- )   \ end structure definition
// swap ! ;
    head STRUCTEND,13,"end-structure",docolon
        .int SWAP,STORE,EXIT

//  CONTROL STRUCTURES ============================

// C  ahead  ( -- addr  )  \ forward branch target
//    ['] branch ,branch  here dup ,dest;  immediate
    head AHEAD,5,"ahead",docolon,1
        .int LIT,BRANCH,COMMABRANCH
        .int HERE,DUPE,COMMADEST,EXIT

// C  if  (  -- addr  )  \ conditional forward branch
//    ['] qbranch ,branch  here dup ,dest ; 
//    immediate
    head IFF,2,"if",docolon,1
        .int LIT,QBRANCH,COMMABRANCH
        .int HERE,DUPE,COMMADEST,EXIT

// C  then  ( addr -- )  \ resolve forward branch
//    here swap !dest ;  immediate
    head THEN,4,"then",docolon,1
        .int HERE,SWAP,STOREDEST,EXIT

// C  else  ( csys: addr1 -- addr2 )  \ branch for IF..ELSE
//    ['] branch ,branch  here dup ,dest
//    swap  postpone then ;  immediate
    head ELSS,4,"else",docolon,1
        .int LIT,BRANCH,COMMABRANCH
        .int HERE,DUPE,COMMADEST
        .int SWAP,THEN,EXIT
// C  begin  ( csys:  -- adrs ) \ target for backward branch
//    here ;  immediate
    head BEGIN,5,"begin",docode,1
        mov $HERE, %eax
        jmp *(%eax)

// C  until  ( csys: adrs --  )  \ conditional backward branch
//    ['] qbranch ,branch  ,dest ;  immediate
//    conditional backward branch
    head UNTIL,5,"until",docolon,1
        .int LIT,QBRANCH,COMMABRANCH
        .int COMMADEST,EXIT

// X  again  ( csys: adrs -- )  \ unconditonal backward branch
//    ['] branch ,branch  ,dest ;  immediate
//    unconditional backward branch
    head AGAIN,5,"again",docolon,1
        .int LIT,BRANCH,COMMABRANCH
        .int COMMADEST,EXIT

// C  while  ( csys: adrs1 -- adrs2 adrs1 )  \  branch for WHILE loop
//    postpone if swap ;  immediate
    head WHILE,5,"while",docolon,1
        .int IFF,SWAP,EXIT

// C  repeat  ( csys: adrs2 adrs1 --  )  \ resolve WHILE loop
//    postpone again postpone then ;  immediate
    head REPEAT,6,"repeat",docolon,1
        .int AGAIN,THEN,EXIT

// Z  >l  ( x --   L: -- x )  \ move to leave stack
//    cell lp +!  lp @ ! ;       (L stack grows up)
    head TOL,2,">l",docolon
        .int CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

// Z  l>  (  -- x   L: x -- )  \ move from leave stack
//    lp @ @  cell negate lp +! ; 
    head LFROM,2,"l>",docolon
        .int LP,FETCH,FETCH
        .int CELL,NEGATE,LP,PLUSSTORE,EXIT

// C  do  ( csys:  -- adrs   L: -- 0 rsys x1 x2 -- ) \ start loop
//    ['] XDO ,xt   here     target for bwd branch
//    0 >l ;  immediate           marker for LEAVEs
    head DO,2,"do",docolon,1
        .int LIT,XDO,COMMAXT,HERE
        .int LIT,0,TOL,EXIT

// X  ?do  ( csys: -- adrs   L:  S: x1 x2 -- )  \  start loop if x2 > 0  
//   0 >l postpone 2dup postpone = postpone if
//   postpone 2drop postpone else >l
//   ['] (do) compile, here ; immediate
    head QXDO,3,"?do",docolon,1
        .int LIT,0X0,TOL,LIT,TWODUP,COMMAXT
        .int LIT,EQUAL,COMMAXT,IFF
        .int LIT,TWODROP,COMMAXT,ELSS
        .int TOL,LIT,XDO,COMMAXT,HERE,EXIT

// H endloop   adrs xt --   L: 0 a1 a2 .. aN --
//    ,branch  ,dest                backward loop
//    begin l> ?dup while postpone then repeat // 
//                                  resolve LEAVEs
//  This is a common factor of LOOP and +LOOP.
    head ENDLOOP,7,"endloop",docolon
        .int COMMABRANCH,COMMADEST
LOOP1:  .int LFROM,QDUP,QBRANCH,LOOP2
        .int THEN,BRANCH,LOOP1
LOOP2:  .int EXIT

// C  loop  ( adrs --  L: 0 a1 a2 .. aN -- )  \ do loop terminiator
//    ['] xloop endloop ;   immediate
    head LOOP,4,"loop",docolon,1
        .int LIT,XLOOP,ENDLOOP,EXIT

// C  +loop  ( addrs --  L: 0 a1 a2 .. aN -- S: n -- ) \ add n to loop counter
//    ['] xplusloop endloop ;   immediate
    head PLUSLOOP,5,"+loop",docolon,1
        .int LIT,XPLUSLOOP,ENDLOOP,EXIT

// C  leave  ( --  L: -- adrs )  \ exit do loop
//    ['] unloop ,xt
//    ['] branch ,branch   here dup ,dest  >l
//    ;  immediate      unconditional forward branch
    head LEAVE,5,"leave",docolon,1
        .int LIT,UNLOOP,COMMAXT
        .int LIT,BRANCH,COMMABRANCH
        .int HERE,DUPE,COMMADEST,TOL,EXIT

// X  case  ( c: -- csys )  \ begin case condition
//   postpone true postpone if ; immediate
    head CASE,4,"case",docolon,1
        .int LIT,LIT,COMMAXT,LIT,-1,COMMAXT
        .int IFF,EXIT

// X  of  ( c: -- ofsys  runtime: ( x1 x2 -- | x1 )  \ start case condition
//   postpone over postpone = postpone if postpone drop ; immediate
    head OF,2,"of",docolon,1
        .int LIT,OVER,COMMAXT
        .int LIT,EQUAL,COMMAXT
        .int IFF,LIT,DROP,COMMAXT,EXIT

// X  endof  ( c: csys1 ofsys -- csys2 )  \ terminate case condition
//   swap postpone then postpone else ; immediate
    head ENDOF,5,"endof",docolon,1
        .int SWAP,THEN,ELSS,EXIT

// X  endcase   ( c: csys --   runtime: ( x -- ) )  \ terminate case block
//   postpone drop postpone then ; immediate
    head ENDCASE,7,"endcase",docolon,1
        .int LIT,DROP,COMMAXT,THEN,EXIT


//  =========OTHER OPERATIONS=====================

// X  within  ( x1 x2 x3 -- flag )  \ true if  x2<=x1<x3
//   over - >r - r> u< ;           per ANS document
    head WITHIN,6,"within",docolon
        .int OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

// C  move  ( addr1 addr2 u -- )  \ smart move
//              VERSION FOR 1 ADDRESS UNIT = 1 CHAR
//   >r 2dup swap dup r@ +     -- ... dst src src+n
//   within if  r> cmove>        src <= dst < src+n
//        else  r> cmove  then ;           otherwise
    head MOVE,4,"move",docolon
        .int TOR,TWODUP,SWAP,DUPE,RFETCH,PLUS
        .int WITHIN,QBRANCH,MOVE1
        .int RFROM,CMOVEUP,BRANCH,MOVE2
MOVE1:  .int RFROM,CMOVE
MOVE2:  .int EXIT

// C  depth  ( -- +n )   \ number of items on stack
 //    s0 sp@ - 4 -  2/ 2/   ;    32-BIT VERSION!
    head DEPTH,5,"depth",docolon
        .int S0,SPFETCH,MINUS,LIT,4,MINUS,TWOSLASH,TWOSLASH,EXIT

// C  environment?  ( c-addr u -- false )  \ system query (not implemented)
//                          -- i*x true
//    2drop 0 ;        the minimal definition!
    head ENVIRONMENTQ,12,"environment?",docolon
        .int TWODROP,LIT,0,EXIT

// ============Multitasking==================
//
// Z  user'  ( task a -- a' )  \  task user variables
// taken as offsets from terminal task u0
//   u0 - + ;
    head USERTSK,5,"user'",docolon
        .int U0,MINUS,PLUS,EXIT

// Z  task:  ( " <name>"  -- )  \  create a task
// ( save address of new task's user variables)
//  here aligned-dw 0x80 allot  ( space for TIB )
//  up@ over #task-size dup allot  ( fr to #) 
//  move                         \ copy user area
//  constant                     \ create name
//   ;
    head TASKDEF,5,"task:",docolon
        .int HERE,DWALIGNED,LIT,0x80,ALLOT
        .int UPFETCH,OVER,NTASKSIZE,DUPE,ALLOT
        .int MOVE,CONSTANT,EXIT

// Z  task!  ( xt task -- )  \  initialize task
//  dup lo user' lp user' !   \ set leave pointer
//  dup s0 user'         ( xt task stack)
//  cell -  0deadbeef over ! \ 0xdeadbeef for testing
//  cell -  0 over !    \ loop counter
//  cell -  rot  over !   ( task stack) \ TOS=xt on stack
//  cell - ['] run >body over ! ( task stack) \ IP=xt-run on stack
//  cell -  over r0 user' over ! ( task stack) \ RP  on stack
//  swap 'sp user' !                          \ save sp to 'sp
//  ;
    head TASKSTORE,5,"task!",docolon
        .int DUPE,L0,USERTSK,OVER,LP,USERTSK,STORE
        .int DUPE,S0,USERTSK
        .int CELL,MINUS,LIT,0xdeaddeed,OVER,STORE
        .int CELL,MINUS,LIT,0,OVER,STORE    
        .int CELL,MINUS,ROT,OVER,STORE
        .int CELL,MINUS,LIT,RUN,TOBODY,OVER,STORE
        .int CELL,MINUS,OVER,R0,USERTSK,OVER,STORE
        .int SWAP,TICKSP,USERTSK,STORE
        .int EXIT

// Z  awake?  ( task -- addr|0 )  \ address of prev link or 0
// >r #tasks @ link begin
//     over
// while
//     swap 1- over @ dup r@ = if
//         r> 2drop drop exit
//     then
//     rot drop
// repeat
// r> 2drop drop 0 ;
    head AWAKEQ,6,"awake?",docolon
        .int TOR,NUMTASK,FETCH,U0
AWKQ1:  .int OVER,QBRANCH,AWKQ3
        .int SWAP,ONEMINUS,OVER,FETCH,DUPE
        .int RFETCH,EQUAL,QBRANCH,AWKQ2
        .int RFROM,TWODROP,DROP,EXIT
AWKQ2:  .int ROT,DROP,BRANCH,AWKQ1
AWKQ3:  .int RFROM,TWODROP,DROP,LIT,0,EXIT

// Z  sleep  ( task -- )  \  kill task unless terminal task
//   dup terminal = if drop exit  then
//   dup awake? ?dup
//   if ( task prev.task)
//     over @ swap !  ( unlink task)
//     -1 #tasks +!
//  then  drop  ;
 
    head SLEEP,5,"sleep",docolon
        .int DUPE,TERM,NOTEQUAL,QBRANCH,SLP1
        .int DUPE,AWAKEQ,QDUP,QBRANCH,SLP1
        .int OVER,FETCH,SWAP,STORE,LIT,-1
        .int NUMTASK,PLUSSTORE
SLP1:   .int DROP,EXIT

// Z  stop  ( -- )  \ put current task to sleep
// link sleep pause ;
    head STOP,4,"stop",docolon
        .int UPFETCH,SLEEP,PAUSE,EXIT

// Z  wake  ( task -- )  \ add task to chain
//  dup awake? 0=        ( new flag)
//  if ( new) dup dup     ( new new new)
//     link               ( new new new curr)
//     dup @ >r           ( ie save next on return stack)
//     !  ( make new task successor of current task)  ( new new)
//     r>  swap           ( new next new)
//     !  (  make next task successor of new task)     ( new)
//     1 #task +!
//  then ( new)  
//  drop  ;
    head WAKE,4,"wake",docolon
        .int DUPE,AWAKEQ,ZEROEQUAL,QBRANCH,WK1
        .int DUPE,DUPE,UPFETCH,DUPE,FETCH,TOR
        .int STORE,RFROM,SWAP,STORE,LIT,1
        .int NUMTASK,PLUSSTORE
WK1:    .int DROP,EXIT

// Z  purge  (  -- )  \ kill all running task
//  term #task 0 do
//     dup @ sleep
//  loop
// drop single ;
    head PURGE,5,"purge",docolon
        .int TERM,NUMTASK,FETCH,LIT,0,XDO
PRG1:   .int DUPE,FETCH,SLEEP,XLOOP,PRG1
        .int DROP,SINGLE,EXIT

// Z run ( xt -- )  \ commence task at xt
    head RUN,3,"run",docolon
        .int RUNV,FETCH,EXECUTE,EXIT

// Z  (run)  ( xt -- )  \ ????
//  catch drop stop ;
    head BRRUN,5,"(run)",docolon
        .int CATCH,DROP,STOP,EXIT

// Z  single  ( -- )  \ stop cooperative multitasker 
//  ['] (null) pausev ! ;
    head SINGLE,6,"single",docolon
        .int LIT,PNULL,PAUSEV,STORE,EXIT

// Z  multi  ( -- )  \ start cooperative multitasker 
// ['] (pause) pausev ! ;
    head MULTI,5,"multi",docolon
        .int LIT,BRPAUSE,PAUSEV,STORE,EXIT

//  ==========TOOLS================
// Z  .hex8  ( d --- )  \ print 32-bit value in hex format
// base @ >r s>d
// <# # # # # # # # # 
// [char] x hold [char] 0 hold #>
//  type r> base ! ;

    head DOTHEX8,5,".hex8",docolon
        .int BASE,FETCH,TOR,STOD,LESSNUM
        .int NUM,NUM,NUM,NUM,NUM,NUM,NUM,NUM
        .int LIT,0x78,HOLD,LIT,0X30,HOLD
        .int NUMGREATER,TYPE,RFROM,BASE,STORE,EXIT
 
// P  words  ( -- )  \ list all words in dict.
//    sorder @ if
//    sorder cell+ @ begin
//        @ ?dup  while
//       dup count 0x7f and 
//       type space nfa>lfa
//        repeat
//    then ; 
    head WORDS,5,"words",docolon
        .int SORDER,FETCH,QBRANCH,WDS2
        .int SORDER,CELLPLUS,FETCH
WDS1:   .int FETCH,QDUP,QBRANCH,WDS2
        .int DUPE,COUNT,LIT,0x7F,ANDD,TYPE,SPACE
        .int NFATOLFA,BRANCH,WDS1
WDS2:   .int EXIT

// P  dump  (addr n -- )  \ display n bytes of memory from addr
//   base @ -rot hex
//   cr 0b spaces 10 0 do
//        i 3 .r loop
//    2 spaces 16 0 do
//        i 0 <# # #> loop
//    over plus swap    (e-a s-a )
//    dup 0xf and xor  do   ( start on 16-byte boundary ) 
//        cr i dup .hex8 ( 8 u.r ) space 10 + (e-a s-a s-a+10 )  
//        i over over do
//            i c@ space 0  
//            <# # # #> loop
//        2 spaces do 
//           i c@ dup 20 < if
//              drop 2e then
//           dup 7e > if >
//              drop 2e then
//           emit   loop
//         16 +loop
//         cr base ! 
//          ;
//
    head DUMP,4,"dump",docolon
        .int BASE,FETCH,ROT,ROT,HEX
        .int CR,LIT,11,SPACES,LIT,16,LIT,0,XDO
DUMP1:  .int II,LIT,3,DOTR,XLOOP,DUMP1
        .int LIT,2,SPACES,LIT,16,LIT,0,XDO
DUMP2:  .int II,LIT,0,LESSNUM,NUM,NUMGREATER,TYPE
        .int XLOOP,DUMP2
        .int OVER,PLUS,SWAP,DUPE,LIT,0x0f,ANDD,XORR,XDO
DUMP3:  .int CR,II,DUPE,DOTHEX8 // LIT,8,UDOTR
        .int SPACE,LIT,16,PLUS,II
        .int OVER,OVER,XDO
DUMP4:  .int II,CFETCH,SPACE,LIT,0,LESSNUM,NUM,NUM
        .int NUMGREATER,TYPE,XLOOP,DUMP4
        .int LIT,2,SPACES,XDO
DUMP5:  .int II,CFETCH,DUPE,LIT,32,LESS
        .int QBRANCH,DUMP6
        .int DROP,LIT,46
DUMP6:  .int DUPE,LIT,126,GREATER
        .int QBRANCH,DUMP7
        .int DROP,LIT,46
DUMP7:  .int EMIT
        .int XLOOP,DUMP5
        .int LIT,16
        .int XPLUSLOOP,DUMP3
        .int CR,BASE,STORE,EXIT

// P  .s  ( -- )  \ dump P-Stack contents
//    base @ >r hex
//    sp@ s0 - 0< if
//        sp@ s0 4 - do i @ u. -4 +loop
//    then
//    r> base !
//    ; 
    head DOTS,2,".s",docolon
        .int BASE,FETCH,TOR,HEX
        .int SPFETCH,S0,MINUS,ZEROLESS,QBRANCH,DOTS2
        .int SPFETCH,S0,LIT,4,MINUS,XDO
DOTS1:  .int II,FETCH,UDOT,LIT,-4,XPLUSLOOP,DOTS1
DOTS2:  .int RFROM,BASE,STORE,EXIT

// Z  .rs  ( -- )  \ dump R-Stack contents
//    hex base @
//    rp@ r0 - 0< if
//        rp@ r0 4 - do i @ u. -4 +loop
//    then
//    base !
//    ; 
    head DOTRS,3,".rs",docolon
        .int BASE,FETCH,HEX
        .int RPFETCH,R0,MINUS,QBRANCH,DOTRS2
        .int RPFETCH,R0,LIT,4,MINUS,XDO
DOTRS1:  .int II,FETCH,UDOT,LIT,-4,XPLUSLOOP,DOTRS1
// DOTRS1:  .int II,FETCH,FETCH,CFATONFA,LIT,-4,XPLUSLOOP,DOTRS1
DOTRS2:  .int BASE,STORE,EXIT

// P  [defined]  ("<name>" - flg )  \ set flag if name is defined
// bl word find nip 0<> ; immediate
    head BRDEFINED,9,"[defined]",docolon,1
        .int BLANK,WORDD,FIND,NIP,ZERONEQUAL,EXIT

// P  [undefined]  ( "<name>" - flg )  \ set flag if <name> is undefined
// bl word find nip 0= ; immediate
    head BRUNDEFINED,11,"[undefined]",docolon,1
        .int BLANK,WORDD,FIND,NIP,ZEROEQUAL,EXIT

// P  [then]  ( --  )  \ resume scanning of input stream
//  ; immediate
    head BRTHEN,6,"[then]",docolon,1
        .int EXIT

// P  [if]  ( flg -- )  \ if tos is true ignore input stream untl [then]
//  0= if postpone [else] 
//     then ; immediate
    head BRIF,4,"[if]",docolon,1
        .int ZEROEQUAL,QBRANCH,BRIF1
        .int BRELSE
BRIF1:  .int EXIT

// P  [else]  ( -- ) \ ignore input stream until [then] is seen
// 1 begin           \ level
//  begin
//      bl word count dup
//  while           \ level addr len
//      2dup s" [if]" compare 0= if
//          2drop 1+
//      else
//          2dup s" [else]" 
//          compare 0= if
//              2drop 1- dup 
//              if 1+ then  \ level1
//          else
//              s" [then] compare 0=
//              if 1- then
//          then
//      then
//      ?dup 0= if exit then
//  repeat 2drop
//  refill 0= until
// drop ;   immediate
    head BRELSE,6,"[else]",docolon,1
        .int LIT,1
BRELS1: .int BLANK,WORDD,COUNT,DUPE
        .int QBRANCH,BRELS7,TWODUP,XSQUOTE
        .byte 4
        .ascii "[if]"
        .int COMPARE,ZEROEQUAL,QBRANCH,BRELS2
        .int TWODROP,ONEPLUS,BRANCH,BRELS5
BRELS2: .int TWODUP,XSQUOTE
        .byte 6
        .ascii "[else]"
        .int COMPARE,ZEROEQUAL,QBRANCH,BRELS3
        .int TWODROP,ONEMINUS,DUPE
        .int QBRANCH,BRELS5,ONEPLUS
        .int BRANCH,BRELS5
BRELS3: .int XSQUOTE
         .byte 6
        .ascii "[then]"
        .int COMPARE,ZEROEQUAL,QBRANCH,BRELS5
        .int ONEMINUS
BRELS5: .int QDUP,ZEROEQUAL,QBRANCH,BRELS1,EXIT
        .int BRANCH,BRELS1
BRELS7: .int TWODROP,REFILL,ZEROEQUAL,QBRANCH,BRELS1
        .int DROP,EXIT

//Z io!  Initialize IO system
    head IOSTORE,3,"io!",docolon
#ifdef LINUX
        .int LIT,LEMIT,EMITV,STORE
        .int LIT,LKEY,KEYV,STORE
        .int LIT,LKEYQ,KEYQV,STORE
#endif
#ifdef COREBOOT
        .int LIT,EMITSIO_PORT,EMITV,STORE
        .int LIT,KEYSIO_PORT,KEYV,STORE
        .int LIT,KEYQSIO_PORT,KEYQV,STORE
#endif
        .int LIT,PNULL,IOINITV,STORE
        .int LIT,PNULL,PAUSEV,STORE
        .int LIT,TWODROP,BLKFETCHV,STORE
        .int LIT,TWODROP,BLKSTOREV,STORE
        .int EXIT

// Z  cold  ( -- )  \ cold start Forth system
//    uinit u0 #init cmove      init user area
//    io! ioinit                input/out
//    u0 u0 !
//    ' (run) runv !
//    nextlist cell+ nextlist !
//    latest @ forth-wordlist !
//    forth-wordlist sorder cell+ !
//    ['] cold handler !
//    ." 80x86 camelforth etc."
//    so sp! 
//    quit ; 
    head COLD,4,"cold",docolon
        .int UINIT,U0,NINIT,CMOVE
        .int IOSTORE,IOINIT
        .int U0,U0,STORE
        .int LIT,BRRUN,RUNV,STORE
        .int NEXTLIST,CELLPLUS,NEXTLIST,STORE
        .int LATEST,FETCH,FORTHWORDLIST,STORE
        .int FORTHWORDLIST,SORDER,CELLPLUS,STORE
        .int LIT,COLD,HANDLER,STORE,XSQUOTE
        .byte 40
        .ascii "80x86 CamelForth v2015.001 23-Sep-2015"
        .byte  0x0d, 0x0a
        .int TYPE,S0,SPSTORE,QUIT      /*  QUIT never returns */
        .int EXIT

