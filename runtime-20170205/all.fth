\ @@FILE:startup.fth
\ vocabulary lay out for thinclient
\ forth :- system,filesystem,memory,graphics,console,diagnostic
\ system :- gx1,pci,pnp,DOC,cs5530,usb,ide,serial,parport,spi,i2c
\ filesystem :- file-ids,ext2fs
\ memory :- init-memory,free,allocate,resize
\ graphics :- x-max y-max fg bg plot-xy line xnow ynow 
\ console :- at-xy 
\ diagnostic
 
only forth definitions
vocabulary system
vocabulary filesystem
vocabulary memory
vocabulary graphics
vocabulary console
vocabulary diagnostic
\ @@FILE:util.fth
\ define structures, pretty number input and output
\ 
\ 

only forth definitions

base @  hex

[undefined] 2rot [if]
: 2rot ( d1 d2 d3 -- d2 d3 d1 )
  2>r 2swap 2r> 2swap
;
[then]
[undefined] cell- [if]
: cell- ( u -- 'u) cell - ;
[then]

\ @@C smart move for cell-sized array
: lmove> ( src dest len -- ) \ move len cells from src to dest. src > dest.
    0 do
        over @ over !
        cell+ swap cell+ swap
    loop
;

: lmove< ( src dest len -- ) \ move len cells from src to dest. src < dest.
    tuck + -rot
    tuck + -rot
    0 do
        over @ over !
        cell- swap cell- swap
    loop
;

: lmove ( src dest len -- )
    true 2over > and 
    if lmove> else lmove< then
;

\ @@C 
: bounds ( addr len -- addr1 addr2 ) over + swap ;
\ @@C true if min <= n < max 
: between ( n min max -- flg )
 >r over > invert swap
 r> > invert and
;

\ @@C write count wrd at addr
: wfill ( addr count wrd -- )
  swap 2* ( addr wrd n )
  rot tuck + swap ( wrd e a )
  do
    dup i w!
    2 +loop
  drop
;

\ @@C write count dwords at addr
: lfill ( addr count x -- )
  swap 4 *   ( a x n )
  rot tuck + swap
  do
     dup i !
     4 +loop
  drop
;

: low-byte ( x --b0 ) 0ff and ;
: low-word ( x -- w0 ) 0ffff and ;

\ @@C convert word to bytes lo hi
: wbsplit ( w -- b0 b1 )
 dup low-byte swap 8 rshift low-byte ;

\ @@C convert dword to words hi lo
: lwsplit ( u -- w0 w1 )
 dup low-word swap h# 10 rshift low-word ;

\ @@C convert dword to bytes 
: lbsplit ( l -- b0 b1 b2 b3 )
 lwsplit >r wbsplit r> wbsplit ;

\ @@C combine 2 bytes b0 b1 to a word 
: bwjoin ( b0 b1 -- w ) 8 lshift or ;

\ @@C combine two words w0 w1 to a dword
: wljoin ( w0 w1 -- b ) h# 10 lshift or ;

\ @@C combine 4 bytes b0 b1 b2 b3 a dword
: bljoin ( b0 b1 b2 b3 -- l ) bwjoin >r bwjoin r> wljoin ;

\ alignment words
\ @@C  align u to a multiple of n i.e u1 n mod = 0
: round-up ( u n  -- u1 ) 1- tuck + swap invert and ;

\ @@C define structures. usage: begin-struct <name> field-list end-structure
: begin-structure ( "<spaces>name" -- addr 0 ; X:-- size )
    create here 0 0 , does> @ 
;

: end-structure ( addr n -- )  swap ! ;

\ @@C define a field of size n2 chars within a struct. 
: +field ( n1 n2 "<spaces>name" -- n3 X: addr -- addr+n1 )
 create over , + 
 does> @ + 
;

\ @@C define a byte field within a struct. 
: cfield: ( n "<spaces>name" -- n2 X: addr -- addr+1)
  1 chars +field 
;

\ @@C define a dword field within a struct. 
: field: ( n "<spaces>name" -- n2 X: addr -- addr+4)
  1 cells +field
;

\ @@C define number prefixes b#, o#  d# and h#
: #: ( n "<spaces>name" -- X: "<space>cccc" -- n)
 create c, immediate
 does> 
    base @ >r c@ base !
    bl word ?number if
    postpone literal else
    drop abort then
    r> base ! 
;

2  #: b#
8  #: o#
0a #: d#
10 #: h#

\ @@C b# parse next word as binary number
\ @@C o# parse next word a octal number
\ @@C d# parse next word a decimal number
\ @@C h# parse next word a hex number

\ some pretty print words
\ define bin. oct. dec. hex.

: #. create c, 
  does>
     base @ >r c@ base !
     u. 
     r> base !
;

2  #. bin.
8  #. oct.
0a #. dec.
10 #. hex.

\ @@C bin. ( u --) print in binary base
\ @@C oct. ( u --) print in octal base
\ @@C dec. ( u --) print in decimal base
\ @@C hex. ( u --) print in hexadecimal base

\ random number generator

h# 7fffffff constant randmax#

internal

variable seed 
1 seed !

: randr   ( addr -- u i)
\ update seed and generate number 
  dup @ d# 1103515245 * d# 12345 + dup rot !
  randmax# 1+ mod   
;

\ @@C  set seed value of random number generator
: srand  ( x -- ) seed !  ; \   change seed 

external

\ @@C generate 32bit random number 
: rand ( -- x ) seed randr ;

module

base !

\         *** Assembler for the Intel i486 ***         07nov92py

\ Copyright (C) 1992-2000 by Bernd Paysan

\ Copyright (C) 2000,2001,2003,2007 Free Software Foundation, Inc.

\ This file is part of Gforth.

\ Gforth is free software; you can redistribute it and/or
\ modify it under the terms of the GNU General Public License
\ as published by the Free Software Foundation, either version 3
\ of the License, or (at your option) any later version.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.

\ You should have received a copy of the GNU General Public License
\ along with this program. If not, see http://www.gnu.org/licenses/.
\ 
\ The syntax is reverse polish. Source and destination are
\ reversed. Size prefixes are used instead of AX/EAX. Example:
\ Intel                gives
\ mov  ax,bx           .w bx ax mov
\ mov  eax,[ebx]       .d bx ) ax mov
\ add  eax,4           .d 4 # ax add
\ mov  byte ptr 1000   .d 1000 #) ax  mov  ????
\
\ 
\ in .86 mode  .w is the default size, in .386 mode  .d is default
\ .wa and .da change address size. .b, .w(a) and .d(a) are not
\ switches like in my assem68k, they are prefixes.
\ [A-D][L|H] implicitely set the .b size. So
\ AH AL mov
\ generates a byte move. Sure you need .b for memory operations
\ like .b ax ) inc    which is  inc  BYTE PTR [eAX]
\ Here are some examples of addressing modes in various syntaxes:
\
\ Gforth          Intel (NASM)   AT&T (gas)      Name
\ al              al             %al             register (8 bit)
\ .w ax           ax             %ax             register (16 bit)
\ ax              eax            %eax            register (32 bit)
\ 3 #             offset 3       $3              immediate
\ 1000 #)         byte ptr 1000  1000            displacement
\ bx )            [ebx]          (%ebx)          base or indirect
\ 100 di d)       100[edi]       100(%edi)       base+displacement
\ 20 ax *4 i#)    20[eax*4]      20(,%eax,4)     (index*scale)+displacement
\ di ax *4 i)     [edi][eax*4]   (%edi,%eax,4)   base+(index*scale)
\ 4 bx cx di)     4[ebx][ecx]    4(%ebx,%ecx)    base+index+displacement
\ 12 sp ax *2 di) 12[esp][eax*2] 12(%esp,%eax,2) base+(index*scale)+displacement
\
\ You can use L) and LI) instead of D) and DI) to enforce 32-bit displacement
\  fields (useful for later patching).
\
\ Some example of instructions are:
\
\     ax bx mov            \ movl %eax, %ebx
\     3 # ax mov           \ movl $3, %eax
\     100 di d) ax mov     \ mov eax,100[edi]  movl 100(%edi), %eax
\     4 bx cx d) ax mov   \ mov eax,4[ebx][ecx] movl 4(%ebx,%ecx), %eax
\     .w ax bx mov         \ mov bx,ax
\
\ The following forms are supported for binary instructions:
\
\     <reg> <reg> <inst>
\     <n> # <reg> <inst>
\     <mem> <reg> <inst>
\     <reg> <mem> <inst>
\     <n> # <mem> <inst>
\
\ The shift/rotate syntax is:
\
\     <reg/mem> 1 # shl \ shortens to shift without immediate
\     <reg/mem> 4 # shl
\     <reg/mem> cl shl
\ The out/in syntax is:
\     <n> # out
\     <n> # .b out
\     .b out
\     .w out
\
\ Precede string instructions (movs etc.) with .b to get the byte version.
\
\ The control structure words IF UNTIL etc. must be preceded by one of 
\ these conditions: 
\      vs vc u< u>= 0= 0<> u<= u> 0< 0>= ps pc < >= <= >. 
\ (Note that most of these words shadow some Forth words 
\ when assembler is in front of forth in the search path, 
\   e.g., in code words). Currently the control structure words use one stack
\  item, so you have to use roll instead of cs-roll to shuffle them 
\ (you can also use swap etc.).
\ 
\ Here is an example of a code word (assumes that the stack pointer 
\ is in esp and the TOS is in ebx):
\ 
\     code my+ ( n1 n2 -- n )
\         sp ) bx add
\         4 # sp add
\     ;code  ( next end-code )

\ 80486 Assembler Load Screen                          21apr00py


only forth also definitions
vocabulary assembler

base @  hex 8 base !
assembler also definitions 

: off ( a-addr --- )
false swap !
;

: on ( a-addr -- )
true swap !
;

: alias create ,
  does> @ execute
;

: [f]  forth     ; immediate
: [a]  assembler ; immediate

\ Assembler Forth words                                11mar00py

: user' ' >body @ ; immediate
: case? ( n1 n2 -- t / n1 f )
    over = if  drop true  else  false  then ;

\ Code generating primitives                           07mar93py

variable >codes
: (+rel      ;
create nrc  ' c, , ' here , ' allot , ' c! , ' (+rel ,

: nonrelocate   nrc >codes ! ;      nonrelocate

: >exec   create  dup c,  cell+
            does>  c@  >codes @  +  @ execute ( perform ) ;

0
>exec ,       >exec here    >exec allot   >exec c!
>exec +rel
drop

\ Stack-Buffer for Extra-Werte                         22dec93py

variable modr/m               variable modr/m#
variable sib                  variable sib#
variable disp                 variable disp#
variable imm                  variable imm#
variable aimm?                variable adisp?
variable byte?                variable seg
variable .asize               variable .anow
variable .osize               variable .onow
: pre-    seg off  .asize @ .anow !  .osize @ .onow !  ;
: sclear  pre-  aimm? off  adisp? off
    modr/m# off  sib# off  disp# off  imm# off  byte? off ;

: .b  1 byte? !  imm# @ 1 min imm# ! ;

: .w   .onow off ;              : .wa  .anow off ;
: .d   .onow on  ;              : .da  .anow on  ;

\ extra-werte compilieren                              01may95py
: bytes,  ( nr x n -- )
    0 ?do  over 0< if  +rel  swap 1+ swap  then  dup ,  h# 8 rshift
    loop   2drop ;
: opcode, ( opcode -- )
    .asize @ .anow @  <> if  h# 67 ,  then
    .osize @ .onow @  <> if  h# 66 ,  then
    seg     @ if  seg @ ,  then  ,  pre- ;
: finish ( opcode -- )  opcode,
    modr/m# @ if  modr/m @ ,  then
    sib#    @ if  sib    @ ,  then
    adisp?  @ disp @ disp# @ bytes,
    aimm?   @ imm  @ imm#  @ bytes,    sclear  ;
: finishb  ( opcode -- )       byte? @ xor  finish ;
: 0f,  h# 0f opcode, ;
: finish0f ( opcode -- )       0f,  finish ;

\ register                                             29mar94py

: regs  ( mod n -- ) 1+ 0 do   dup constant 11 +  loop  drop ;
: breg  ( reg -- )  create c,  does> c@  .b ;
: bregs ( mod n -- ) 1+ 0 do   dup breg     11 +  loop  drop ;
: wadr: ( reg -- )  create c,  does> c@  .wa ;
: wadr  ( mod n -- ) 1+ 0 do   dup wadr:    11 +  loop  drop ;
   0 7 wadr [bx+si] [bx+di] [bp+si] [bp+di] [si] [di] [bp] [bx]
 300 7 regs  ax cx dx bx sp bp si di
 300 7 bregs al cl dl bl ah ch dh bh
2300 5 regs es cs ss ds fs gs
' si alias rp   ' bp alias up   ' di alias op
: .386  .asize on   .osize on  sclear ;  .386
: .86   .asize off  .osize off sclear ;
: asize@  2 .anow @ if  2*  then ;
: osize@  2 .onow @ if  2*  then ;

\ address modes                                        01may95py
: #) ( disp -- reg )
  disp ! .anow @ if  55 4  else  66 2  then  disp# ! ;
: *2   100 xor ;    : *4   200 xor ;    : *8   300 xor ;
: index  ( reg1 reg2 -- modr/m )  370 and swap 7 and or ;
: i) ( reg1 reg2 -- ireg )  .anow @ 0= abort" no index!"
  *8  index  sib ! 1 sib# ! 44 ;
: i#) ( disp32 reg -- ireg ) bp swap i) swap #) drop ;
: seg)  ( seg disp -- -1 )
  disp !  asize@ disp# !  imm ! 2 imm# !  -1 ;
: )  ( reg -- reg )  dup sp = if dup i) else 77 and then ;
: d) ( disp reg -- reg )  ) >r dup disp !  h# 80 h# -80 within
  adisp? @ or if  200 asize@  else  100 1  then disp# ! r> or ;
: di) ( disp reg1 reg2 -- ireg )  i) d) ;
: a: ( -- )  adisp? on ;        : a::  ( -- )  -2 adisp? ! ;
: a#) ( imm -- )  a: #) ;       : aseg) ( * -- ) a: seg) ;

\ # a# rel) cr dr tr st <st stp                        01jan98py
: # ( imm -- ) dup imm !  h# -80 h# 80 within  byte? @ or
  if  1  else  osize@  then  imm# ! ;
: l#  ( imm -- )  imm !  osize@ imm# ! ;
: a#  ( imm -- )  aimm? on  l# ;
: rel)  ( addr -- -2 )  disp ! asize@ disp# ! -2 ;
: l) ( disp reg -- reg ) ) >r disp ! 200 asize@ disp# ! r> or ;
: li) ( disp reg1 reg2 -- reg ) i) l) ;
: >>mod ( reg1 reg2 -- mod )  70 and swap 307 and or ;
: >mod ( reg1 reg2 -- )  >>mod modr/m !  1 modr/m# ! ;
: cr  ( n -- )  7 and 11 *  h# 1c0 or ;    0 cr constant cr0
: dr  ( n -- )  7 and 11 *  h# 2c0 or ;
: tr  ( n -- )  7 and 11 *  h# 3c0 or ;
: st  ( n -- )  7 and       h# 5c0 or ;
: <st ( n -- )  7 and       h# 7c0 or ;
: stp ( n -- )  7 and       h# 8c0 or ;

\ reg?                                                 10apr93py
: reg= ( reg flag mask -- flag ) ( 2 pick ) >r over r> swap  and = ;
: reg? ( reg -- reg flag )  h# c0 h# -40 reg= ;
: ?reg ( reg -- reg )  reg? 0= abort" reg expected!" ;
: ?mem ( mem -- mem )  dup h# c0 < 0= abort" mem expected!" ;
: ?ax  ( reg -- reg )  dup ax <> abort" ax/al expected!" ;
: cr?  ( reg -- reg flag ) h# 100 h# -100 reg= ;
: dr?  ( reg -- reg flag ) h# 200 h# -100 reg= ;
: tr?  ( reg -- reg flag ) h# 300 h# -100 reg= ;
: sr?  ( reg -- reg flag ) h# 400 h# -100 reg= ;
: st?  ( reg -- reg flag ) dup h# 8 rshift 5 - ;
: ?st  ( reg -- reg ) st? 0< abort" st expected!" ;
: xr?  ( reg -- reg flag ) dup h# ff > ;
: ?xr  ( reg -- reg )  xr? 0= abort" xr expected!" ;
: rel? ( reg -- reg flag ) dup -2 = ;
: seg? ( reg -- reg flag ) dup -1 = ;

\ single byte instruction                              27mar94py

: bc:   ( opcode -- )  create c, does> c@ ,        ;
: bc.b: ( opcode -- )  create c, does> c@ finishb  ;
: bc0f: ( opcode -- )  create c, does> c@ finish0f ;

: seg:  ( opcode -- )  create c, does> c@ seg ! ;

h# 26 seg: es:    h# 2e seg: cs:    h# 36 seg: ss:    h# 3e seg: ds:
h# 64 seg: fs:    h# 65 seg: gs:

forth

\ arithmetics                                          07nov92py

: reg>mod ( reg1 reg2 -- 1 / 3 )
    reg? if  >mod 3  else  swap ?reg >mod 1  then  ;
: ari: ( n -- ) create c,
    does> ( reg1 reg2 / reg -- )  c@ >r imm# @
    if    imm# @ byte? @ + 1 > over ax = and
          if    drop h# 05 r> 70 and or
          else  r> >mod h# 81 imm# @ 1 byte? @ + = if 2 + then
          then
    else  reg>mod  r> 70 and or
    then  finishb  ;

00 ari: add     11 ari: or      22 ari: adc     33 ari: sbb
44 ari: and     55 ari: sub     66 ari: xor     77 ari: cmp

\ bit shifts    strings                                07nov92py

: shift: ( n -- )  create c,
    does> ( r/m -- )  c@ >mod  imm# @
    if    imm @ 1 =
          if  h# d1 0  else  h# c1 1  then   imm# !
    else  h# d3
    then  finishb ;

00 shift: rol   11 shift: ror   22 shift: rcl   33 shift: rcr
44 shift: shl   55 shift: shr   66 shift: sal   77 shift: sar

h# 6d bc.b: ins   h# 6f bc.b: outs
h# a5 bc.b: movs  h# a7 bc.b: cmps
h# ab bc.b: stos  h# ad bc.b: lods  h# af bc.b: scas

\ movxr                                                07feb93py

: xr>mod  ( reg1 reg2 -- 0 / 2 )
    xr?  if  >mod  2  else  swap ?xr >mod  0  then  ;

: movxr  ( reg1 reg2 -- )
    2dup or sr? nip
    if    xr>mod  h# 8c
    else  2dup or h# 8 rshift 1+ -3 and >r  xr>mod  0f,  r> h# 20 or
    then  or  finish ;

\ mov                                                  23jan93py

: assign#  byte? @ 0= if  osize@ imm# !  else 1 imm# ! then ;

: ?ofax ( reg ax -- flag ) .anow @ if 55 else 66 then ax d= ;
: mov ( r/m reg / reg r/m / reg -- )  2dup or 0> imm# @ and
  if    assign#  reg?
        if    7 and  h# b8 or byte? @ 3 lshift xor  byte? off
        else  0 >mod  h# c7  then
  else  2dup or h# ff > if  movxr exit  then
        2dup ?ofax
        if  2drop h# a1  else  2dup swap  ?ofax
            if  2drop h# a3  else  reg>mod h# 88 or  then
        then
  then  finishb ;

\ not neg mul (imul div idiv                           29mar94py

: modf  ( r/m reg opcode -- )  -rot >mod finish   ;
: modfb ( r/m reg opcode -- )  -rot >mod finishb  ;
: mod0f ( r/m reg opcode -- )  -rot >mod finish0f ;
: modf:  create  c,  does>  c@ modf ;
: not: ( mode -- )  create c, does> ( r/m -- ) c@ h# f7 modfb ;

00 not: test#                 22 not: not     33 not: neg
44 not: mul     55 not: (imul 66 not: div     77 not: idiv

: inc: ( mode -- )  create c,
    does>  ( r/m -- ) c@ >r reg?  byte? @ 0=  and
    if    107 and r> 70 and or finish
    else  r> h# ff modfb   then ;
00 inc: inc     11 inc: dec

\ test shld shrd                                       07feb93py

: test  ( reg1 reg2 / reg -- )  imm# @
  if    assign#  ax case?
        if  h# a9  else  test#  exit  then
  else  ?reg >mod  h# 85  then  finishb ;

: shd ( r/m reg opcode -- )
    imm# @ if  1 imm# ! 1-  then  mod0f ;
: shld  swap 245 shd ;          : shrd  swap 255 shd ;

: btx: ( r/m reg/# code -- )  create c,
    does> c@ >r imm# @
    if    1 imm# !  r> h# ba
    else  swap 203 r> >>mod  then  mod0f ;
44 btx: bt      55 btx: bts     66 btx: btr     77 btx: btc

\ push pop                                             05jun92py

: pushs   swap  fs case?  if  h# a0 or finish0f exit  then
                  gs case?  if  h# a8 or finish0f exit  then
    30 and 6 or or finish ;

: push  ( reg -- )
  imm# @ 1 = if  h# 6a finish exit  then
  imm# @     if  h# 68 finish exit  then
  reg?       if  7 and h# 50 or finish exit  then
  sr?        if  0 pushs  exit  then
  66 h# ff modf ;
: pop   ( reg -- )
  reg?       if  7 and h# 58 or finish exit  then
  sr?        if  1 pushs  exit  then
  06 h# 8f modf ;

\ ascii arithmetics                                    22may93py

h# 27 bc: daa     h# 2f bc: das     h# 37 bc: aaa     h# 3f bc: aas

: aa:  create c,
    does> ( -- ) c@
    imm# @ 0= if  d# 10 imm !  then  1 imm# ! finish ;
h# d4 aa: aam     h# d5 aa: aad     h# d6 bc: salc    h# d7 bc: xlat

h# 60 bc: pusha   h# 61 bc: popa
h# 90 bc: nop
h# 98 bc: cbw     h# 99 bc: cwd                     h# 9b bc: fwait
h# 9c bc: pushf   h# 9d bc: popf    h# 9e bc: sahf    h# 9f bc: lahf
                h# c9 bc: leave
h# cc bc: int3                    h# ce bc: into    h# cf bc: iret
' fwait alias wait

\ one byte opcodes                                     25dec92py

h# f0 bc: lock                    h# f2 bc: rep     h# f3 bc: repe
h# f4 bc: hlt     h# f5 bc: cmc
h# f8 bc: clc     h# f9 bc: stc     h# fa bc: cli     h# fb bc: sti
h# fc bc: cld     h# fd bc: std

: ?brange ( offword --- offbyte )  dup h# 80 h# -80 within
    if ." branch offset out of 1-byte range" then ;
: sb: ( opcode -- )  create c,
    does> ( addr -- ) >r  [a] here [f] 2 + - ?brange
    disp !  1 disp# !  r> c@ finish ;
h# e0 sb: loopne  h# e1 sb: loope   h# e2 sb: loop    h# e3 sb: jcxz
: (ret ( op -- )  imm# @  if  2 imm# !  1-  then  finish ;
: ret  ( -- )  h# c3  (ret ;
: retf ( -- )  h# cb  (ret ;

\ call jmp                                             22dec93py

: call  ( reg / disp -- ) rel?
  if  drop h# e8 disp @ [a] here [f] 1+ asize@ + - disp ! finish
      exit  then  22 h# ff modf ;
: callf ( reg / seg -- )
  seg? if  drop h# 9a  finish exit  then  33 h# ff modf ;

: jmp   ( reg / disp -- )
  rel? if  drop disp @ [a] here [f] 2 + - dup h# -80 h# 80 within
           if    disp ! 1 disp# !  h# eb
           else  3 - disp ! h# e9  then  finish exit  then
  44 h# ff modf ;
: jmpf  ( reg / seg -- )
  seg? if  drop h# ea  finish exit  then  55 h# ff modf ;

: next .d lods .d ax ) jmp ;

\ jump if                                              22dec93py

: cond: 0 do  i constant  loop ;

h# 10 cond: vs vc   u< u>=  0= 0<>  u<= u>   0< 0>=  ps pc   <  >=   <=  >
h# 10 cond: o  no   b  nb   z  nz   be  nbe  s  ns   pe po   l  nl   le  nle
: jmpif  ( addr cond -- )
  swap [a] here [f] 2 + - dup h# -80 h# 80 within
  if            disp ! h# 70 1
  else  0f,  4 - disp ! h# 80 4  then  disp# ! or finish ;
: jmp:  create c,  does> c@ jmpif ;
: jmps  0 do  i jmp:  loop ;
h# 10 jmps jo jno jb jnb jz jnz jbe jnbe js jns jpe jpo jl jnl jle jnle

\ xchg                                                 22dec93py

: setif ( r/m cond -- ) 0 swap h# 90 or mod0f ;
: set: ( cond -- )  create c,  does>  c@ setif ;
: sets: ( n -- )  0 do  i set:  loop ;
h# 10 sets: seto setno setb setnb sete setne setna seta sets setns setpe setpo setl setge setle setg
: xchg ( r/m reg / reg r/m -- )
  over ax = if  swap  then  reg?  0= if  swap  then  ?reg
  byte? @ 0=  if ax case?
  if reg? if 7 and h# 90 or finish exit then  ax  then then
  h# 87 modfb ;

: movx ( r/m reg opcode -- ) 0f, modfb ;
: movsx ( r/m reg -- )  h# bf movx ;
: movzx ( r/m reg -- )  h# b7 movx ;

\ misc                                                 16nov97py

: enter ( imm8 -- ) 2 imm# ! h# c8 finish [a] , [f] ;
: arpl ( reg r/m -- )  swap h# 63 modf ;
h# 62 modf: bound ( mem reg -- )

: mod0f:  create c,  does> c@ mod0f ;
h# bc mod0f: bsf ( r/m reg -- )   h# bd mod0f: bsr ( r/m reg -- )

h# 06 bc0f: clts
h# 08 bc0f: invd  h# 09 bc0f: wbinvd

: cmpxchg ( reg r/m -- ) swap h# a7 movx ;
: cmpxchg8b ( r/m -- )   h# 8 h# c7 movx ;
: bswap ( reg -- )       7 and h# c8 or finish0f ;
: xadd ( r/m reg -- )    h# c1 movx ;

\ misc                                                 20may93py

: imul ( r/m reg -- )  imm# @ 0=
  if  dup ax =  if  drop (imul exit  then
      h# af mod0f exit  then
  >mod imm# @ 1 = if  h# 6b  else  h# 69  then  finish ;
: io ( oc -- )  imm# @ if  1 imm# !  else  h# 8 +  then finishb ;
: in  ( -- ) h# e5 io ;
: out ( -- ) h# e7 io ;
: int ( -- ) 1 imm# ! h# cd finish ;
: 0f.0: ( r/m -- ) create c, does> c@ h# 00 mod0f ;
00 0f.0: sldt   11 0f.0: str    22 0f.0: lldt   33 0f.0: ltr
44 0f.0: verr   55 0f.0: verw
: 0f.1: ( r/m -- ) create c, does> c@ h# 01 mod0f ;
00 0f.1: sgdt   11 0f.1: sidt   22 0f.1: lgdt   33 0f.1: lidt
44 0f.1: smsw                   66 0f.1: lmsw   77 0f.1: invlpg

\ misc                                                 29mar94py

h# 02 mod0f: lar ( r/m reg -- )
h# 8d modf:  lea ( m reg -- )
h# c4 modf:  les ( m reg -- )
h# c5 modf:  lds ( m reg -- )
h# b2 mod0f: lss ( m reg -- )
h# b4 mod0f: lfs ( m reg -- )
h# b5 mod0f: lgs ( m reg -- )
\ pentium/amd k5 codes
: cpuid ( -- )  0f, h# a2 [a] , [f] ;
: cmpchx8b ( m -- ) 0 h# c7 mod0f ;
: rdtsc ( -- )  0f, h# 31 [a] , [f] ;
: rdmsr ( -- )  0f, h# 32 [a] , [f] ;
: wrmsr ( -- )  0f, h# 30 [a] , [f] ;
: rsm ( -- )  0f, h# aa [a] , [f] ;

\ amd/cyrix/nsc geode codes
: bb0_reset ( -- ) 0f, h# 3a [a] , [f] ;
: bb1_reset ( -- ) 0f, h# 3b [a] , [f] ;
: cpu_write ( -- ) 0f, h# 3c [a] , [f] ;
: cpu_read  ( -- ) 0f, h# 3d [a] , [f] ;


\ floating point instructions                          22dec93py

h# d8 bc: d8,   h# d9 bc: d9,   h# da bc: da,   h# db bc: db,
h# dc bc: dc,   h# dd bc: dd,   h# de bc: de,   h# df bc: df,

: d9: create c, does> d9, c@ finish ;

variable fsize
: .fs   0 fsize ! ;  : .fl   4 fsize ! ;  : .fx   3 fsize ! ;
: .fw   6 fsize ! ;  : .fd   2 fsize ! ;  : .fq   7 fsize ! ;
.fx
: fop:  create c,  does>  ( fr/m -- ) c@ >r
    st? dup 0< 0= if  swap r> >mod 2* h# d8 + finish exit  then
    drop ?mem r> >mod h# d8 fsize @ dup 1 and dup 2* + - +
    finish ;
: f@!: create c,  does>  ( fm -- ) c@ h# d9 modf ;

\ floating point instructions                          08jun92py

h# d0 d9: fnop

h# e0 d9: fchs    h# e1 d9: fabs
h# e4 d9: ftst    h# e5 d9: fxam
h# e8 d9: fld1    h# e9 d9: fldl2t  h# ea d9: fldl2e  h# eb d9: fldpi
h# ec d9: fldlg2  h# ed d9: fldln2  h# ee d9: fldz
h# f0 d9: f2xm1   h# f1 d9: fyl2x   h# f2 d9: fptan   h# f3 d9: fpatan
h# f4 d9: fxtract h# f5 d9: fprem1  h# f6 d9: fdecstp h# f7 d9: fincstp
h# f8 d9: fprem   h# f9 d9: fyl2xp1 h# fa d9: fsqrt   h# fb d9: fsincos
h# fc d9: frndint h# fd d9: fscale  h# fe d9: fsin    h# ff d9: fcos

\ floating point instructions                          23jan94py

00 fop: fadd    11 fop: fmul    22 fop: fcom    33 fop: fcomp
44 fop: fsub    55 fop: fsubr   66 fop: fdiv    77 fop: fdivr

: fcompp ( -- )  [a] 1 stp fcomp [f] ;
: fbld   ( fm -- ) 44 h# d8 modf ;
: fbstp  ( fm -- ) 66 h# df modf ;
: ffree  ( st -- ) 00 h# dd modf ;
: fsave  ( fm -- ) 66 h# dd modf ;
: frstor ( fm -- ) 44 h# dd modf ;
: finit  ( -- )  [a] db, h# e3 , [f] ;
: fxch   ( st -- ) 11 h# d9 modf ;

44 f@!: fldenv  55 f@!: fldcw   66 f@!: fstenv  77 f@!: fstcw

\ fild fst fstsw fucom                                 22may93py
: fucom ( st -- )  ?st st? if 77 else 66 then h# dd modf ;
: fucompp ( -- )  [a] da, h# e9 , [f] ;
: fnclex  ( -- )  [a] db, h# e2 , [f] ;
: fclex   ( -- )  [a] fwait fnclex [f] ;
: fstsw ( r/m -- )
  dup ax = if  44  else  ?mem 77  then  h# df modf ;
: f@!,  fsize @ 1 and if  drop  else  nip  then
    fsize @ h# d9 or modf ;
: fx@!, ( mem/st l x -- )  rot  st? 0=
    if  swap h# dd modf drop exit  then  ?mem -rot
    fsize @ 3 = if drop h# db modf exit then  f@!, ;
: fst  ( st/m -- ) st?  0=
  if  22 h# dd modf exit  then  ?mem 77 22 f@!, ;
: fld  ( st/m -- )  st? 0= if 0 h# d9 modf exit then 55 0 fx@!, ;
: fstp ( st/m -- )  77 33 fx@!, ;

\ ppro instructions                                    28feb97py


: cmovif ( r/m r flag -- )  h# 40 or mod0f ;
: cmov:  create c, does> c@ cmovif ;
: cmovs:  0 do  i cmov:  loop ;
h# 10 cmovs: cmovo cmovno cmovb cmovnb cmovz cmovnz cmovbe cmovnbe cmovs cmovns cmovpe cmovpo cmovl cmovnl cmovle cmovnle

\ mmx opcodes                                          02mar97py

300 7 regs mm0 mm1 mm2 mm3 mm4 mm5 mm6 mm7

: mmxs ?do  i mod0f:  loop ;
h# 64 h# 60 mmxs punpcklbw punpcklwd punockldq packusdw
h# 68 h# 64 mmxs pcmpgtb   pcmpgtw   pcmpgtd   packsswb
h# 6c h# 68 mmxs punpckhbw punpckhwd punpckhdq packssdw
h# 78 h# 74 mmxs pcmpeqb   pcmpeqw   pcmpeqd   emms
h# da h# d8 mmxs psubusb   psubusw
h# ea h# e8 mmxs psubsb    psubsw
h# fb h# f8 mmxs psubb     psubw     psubd
h# de h# dc mmxs paddusb   paddusw
h# ee h# ec mmxs paddsb    paddsw
h# ff h# fc mmxs paddb     paddw     paddd

\ mmx opcodes                                          02mar97py

h# d5 mod0f: pmullw               h# e5 mod0f: pmulhw
h# f5 mod0f: pmaddwd
h# db mod0f: pand                 h# df mod0f: pandn
h# eb mod0f: por                  h# ef mod0f: pxor
: pshift ( mmx imm/m mod op -- )
  imm# @ if  1 imm# !  else  + h# 50 +  then  mod0f ;
: psrlw ( mmx imm/m -- )  020 h# 71 pshift ;
: psrld ( mmx imm/m -- )  020 h# 72 pshift ;
: psrlq ( mmx imm/m -- )  020 h# 73 pshift ;
: psraw ( mmx imm/m -- )  040 h# 71 pshift ;
: psrad ( mmx imm/m -- )  040 h# 72 pshift ;
: psllw ( mmx imm/m -- )  060 h# 71 pshift ;
: pslld ( mmx imm/m -- )  060 h# 72 pshift ;
: psllq ( mmx imm/m -- )  060 h# 73 pshift ;

\ mmx opcodes                                         27jun99beu

\ mmxreg --> mmxreg move
h# 6f mod0f: movq

\ memory/reg32 --> mmxreg load
h# 6f mod0f: pldq  \ intel: movq mm,m64
h# 6e mod0f: pldd  \ intel: movd mm,m32/r

\ mmxreg --> memory/reg32
: pstq ( mm m64   -- ) swap  h# 7f mod0f ; \ intel: movq m64,mm
: pstd ( mm m32/r -- ) swap  h# 7e mod0f ; \ intel: movd m32/r,mm

\ 3dnow! opcodes (k6)                                  21apr00py
: mod0f# ( code imm -- )  # 1 imm ! mod0f ;
: 3dnow: ( imm -- )  create c,  does> c@ mod0f# ;
h# 0d 3dnow: pi2fd                h# 1d 3dnow: pf2id
h# 90 3dnow: pfcmpge              h# a0 3dnow: pfcmpgt
h# 94 3dnow: pfmin                h# a4 3dnow: pfmax
h# 96 3dnow: pfrcp                h# a6 3dnow: pfrcpit1
h# 97 3dnow: pfrsqrt              h# a7 3dnow: pfrsqit1
h# 9a 3dnow: pfsub                h# aa 3dnow: pfsubr
h# 9e 3dnow: pfadd                h# ae 3dnow: pfacc
h# b0 3dnow: pfcmpeq              h# b4 3dnow: pfmul
h# b6 3dnow: pfrcpit2             h# b7 3dnow: pmulhrw
h# bf 3dnow: pavgusb

: femms  h# 0e finish0f ;
: prefetch  000 h# 0d mod0f ;    : prefetchw  010 h# 0d mod0f ;

\ 3dnow!+mmx opcodes (athlon)                          21apr00py

h# f7 mod0f: maskmovq             h# e7 mod0f: movntq
h# e0 mod0f: pavgb                h# e3 mod0f: pavgw
h# c5 mod0f: pextrw               h# c4 mod0f: pinsrw
h# ee mod0f: pmaxsw               h# de mod0f: pmaxub
h# ea mod0f: pminsw               h# da mod0f: pminub
h# d7 mod0f: pmovmskb             h# e4 mod0f: pmulhuw
h# f6 mod0f: psadbw               h# 70 mod0f: pshufw

h# 0c 3dnow: pi2fw                h# 1c 3dnow: pf2iw
h# 8a 3dnow: pfnacc               h# 8e 3dnow: pfpnacc
h# bb 3dnow: pswabd               : sfence   h# ae h# 07 mod0f# ;
: prefetchnta  000 h# 18 mod0f ;  : prefetcht0  010 h# 18 mod0f ;
: prefetcht1   020 h# 18 mod0f ;  : prefetcht2  030 h# 18 mod0f ;

\ assembler conditionals                               22dec93py
: ~cond ( cond -- ~cond )  1 xor ;
: >offset ( start dest --- offbyte )  swap  2 + -  ?brange ;
: if ( cond -- here )  [a] here [f] dup 2 + rot  ~cond  jmpif ;
: then       dup [a] here >offset swap 1+ c! [f] ;
: ahead      [a] here [f] dup 2 + rel) jmp ;
: else       [a] ahead swap then [f] ;
: begin      [a] here ;         ' begin alias do  [f]
: while      [a] if [f] swap ;
: until      ~cond  jmpif ;
: again      rel) jmp ;
: repeat     [a] again  then [f] ;
: ?do        [a] here [f] dup 2 + dup jcxz ;
: but        swap ;
: yet        dup ;
: makeflag   [a] ~cond al swap setif  1 # ax and  ax dec [f] ;


: init-asm ( -- ) \ gforth start assembly
    also assembler ;

\ : code  create here -1 cells allot , init-asm ;    \ working


: (align) ( a  - a ) \ align DP on dword boundary
 here dup 
 cell 1- dup invert -rot + and
 swap - allot
;

: end-code ( -- )     \ end assembly 
 previous ;

: ;code   ( -- ) \ end code definition
 next end-code ;


forth definitions
: code ( "name" -- colon-sys )   \ start a native code definition
    create here ( align ) here   \ cfa+1 a-addr
    swap cell - !                \ store pfa in cfa 
    init-asm
    ;

: label  (  --  )  \ "name"
\ code is aligned on dword boundary
    create here cell allot
    (align) here swap ! init-asm
    does> @
; 

 
only forth definitions
base !

\ @@FILE:timer.fth
\ define timer words ala FD
\ use tsc or pit
\  part of system
\ requires : 

only forth also definitions

base @ hex

: cpuspeed ( --   cpu_speed )
\  uses PIT, enables T2, disables speaker
   61 pc@ fd and 1 or 61 pc!
\ mode 0 counter 2
   b0 43 pc!
\ maximum count
   ff 42 pc!
   ff 42 pc!
\  get ticks 
   rdtsc
\ wait for counter to run down
   begin  61 pc@ 20 and until
   rdtsc
   2swap d- drop   ( convert to single )
   1234dc 3e8  ffff * */
;

internal

\ internal applies to previous word up to external

\ @@C delay for 2cell clock cycles
: tdelay ( d1 --  ) 
\ convert to code word
   rdtsc d+
   begin rdtsc 2over d- drop 0> until 
   2drop 
;

cpuspeed constant cpukhz#

external
\ applies to previous word till module


\ @@C delay u nanoseconds
: ndelay ( u -- )  cpukhz# d# 1000000 */ 0 tdelay ;

\ @@C delay for u microseconds
: udelay ( u -- ) cpukhz# d# 1000 */ 0 tdelay ;

\ @@C delay for u milliseconds
: ms ( u -- )    cpukhz# * 0 tdelay ;

\ @@C delay for u seconds
: delay ( u  -- )  begin d# 1000 ms 1- dup 0= until drop ;

\ @@C get millisecond count from tsc
: get-ms ( -- ms)  rdtsc cpukhz# um/mod nip ;

: set-sys-phase ( hz -- )
 1234dc swap /
 36 43 pc!
 dup ff and 40 pc!
 8 rshift 40 pc!
;

.( default system rate is 100 Hz)
64 set-sys-phase

module

base !

\ @@FILE: gx1cpu.fth
\ @@REQUIRES:util.fth timer.fth
\  amd gx1 cpu words
\  system vocabulary

base @ hex

only forth also
system definitions

\ configuration index at 0x22 data at 0x23
: gx1-conf! ( val reg -- )  22 pc! 23 pc! ;
internal

: gx1-conf@ ( reg -- byte ) 22 pc! 23 pc@ ;

\ lock configuration registers 0xc3[4]
: gx1-lock ( -- ) c3 gx1-conf@ 7f and c3 gx1-conf! ;

\  unlock configuration registers 
: gx1-unlock ( -- ) c3 gx1-conf@ 10 or c3 gx1-conf! ;  

\ @@C write to gx1 register
: gx1! ( val reg -- ) gx1-unlock gx1-conf! gx1-lock ; 
external

\ @@C read gx1 register
: gx1@ ( reg -- val ) gx1-unlock gx1-conf@ gx1-lock ;

\ Graphics Control Register
\  GX_BASE defined in index 0xb8[1:0]

: (gx-base) 0b8 gx1@ 3 and 1e lshift ;

\ @@ gx-base# ( -- u) graphics base address
(gx-base) value gx-base#

\ @@C read dword graphics index register
: gx-base@ ( idx -- val ) gx-base# + @ ;

\ @@C write dword graphics index register
: gx-base! ( val idx -- ) gx-base# + ! ;

\ @@C read byte from graphics index register
: gx-base-c@ ( idx -- val ) gx-base# + c@ ;

\ @@C write byte to graphics index register
: gx-base-c! ( val idx  -- ) gx-base# + c! ;

\ Internal Bus Interface unit register
: ibir-base ( -- addr) gx-base# 8000 + ;
: gx1-ibir@ ( idx -- u ) ibir-base + @ ;
: gx1-ibir! ( u idx -- ) ibir-base + ! ;

\ Graphics Pipeline Control Registers
: gpr-base ( -- addr) gx-base# 8100 + ;
: gx1-gpr@ ( idx -- u) gpr-base + @ ;
: gx1-gpr! ( u idx -- ) gpr-base + ! ;
: gx1-gpr-w@ ( idx -- w) gpr-base + w@ ;
: gx1-gpr-w! ( w idx -- ) gpr-base + w! ;

\ Display controller registers
: dcr-base    ( -- adr )  gx-base# 8300 +  ;    
: gx1-dcr@  ( idx -- u )  dcr-base + @  ;
: gx1-dcr!  ( u idx -- )  dcr-base + !  ;

: gx1-dcr-unlock  ( -- ) 4758 0 gx1-dcr!  ;
: gx1-dcr-lock    ( -- )  0 0 gx1-dcr!  ;
: gx1-dcr-lock-push ( --  x) 0 gx1-dcr@ gx1-dcr-unlock ;
: gx1-dcr-lock-pop ( x -- ) 0 gx1-dcr! ;

\ Memory controller register
: mcr-base ( -- addr) gx-base# 8400 + ;
: gx1-mcr@ ( idx -- u ) mcr-base + @ ;
: gx1-mcr! ( u idx -- ) mcr-base + ! ;

\ Power management control register
: pmr-base ( -- addr) gx-base# 8500 + ;
: gx1-pmr@ ( idx -- u) pmr-base + @ ;
: gx1-pmr! ( u idx -- ) pmr-base + ! ;

: video-off ( -- ) 
   gx1-dcr-lock-push 
        08 gx1-dcr@ 20 invert and 08 gx1-dcr! 
   gx1-dcr-lock-pop  
;

: video-on  ( -- ) 
   gx1-dcr-lock-push
      08 gx1-dcr@ 20 or 08 gx1-dcr!
   gx1-dcr-lock-pop  
;

\ palette access
: gx1-palette@  ( -- u )  74 gx1-dcr@  ;
: gx1-palette!  ( u -- )  74 gx1-dcr!  ;
: gx1-palette-index!  ( index -- )  70 gx1-dcr!  ;
: gx1-palette-reg@ ( reg -- u ) gx1-palette-index! gx1-palette@ ;
: gx1-palette-reg! ( u reg -- u ) gx1-palette-index! gx1-palette! ;

\ graphics memory base address
\ index 8414[10:0] corresponds to address bits 29:19
8414 gx-base@ 13 lshift constant gbadd#
\ @@ gbadd# ( -- u) graphics memory base address

module 
also forth definitions
\ @@  tom ( -- u) returns top of memory 
8000 gx-base@ value tom

previous previous definitions

base !
\ @@FILE:gx1-scratchpad.fth
\ @@REQUIRES:asm.fth, util.fth, startup.fth
\ scratchpad initialization 
\ author Juergen ? in a COREBOOT mail thread

only forth also
system definitions

base @ hex

code invalidate-cache ( -- ) invd ;code

code disable-cache ( -- )
   wbinvd
   0 cr ax mov
   gx-base# # ax or     \ gx-base# = 40000000
   ax 0 cr  mov
   wbinvd
;code

code  enable-cache ( -- ) 
  0 cr ax mov
  9fffffff # ax and
  ax 0 cr mov
;code

code  setup-fill-buffer ( u -- )
\ this data will be cacheline's content after mapping
  0 # ax mov  ax 5 tr mov 
  bx 3 tr mov
  4 # ax mov  ax 5 tr mov
  bx 3 tr mov
  8 # ax mov  ax 5 tr mov 
  bx 3 tr mov 
  0c # ax mov ax 5 tr mov 
  bx 3 tr mov 
  bx pop
;code 

code  map-cache-line ( u -- )
\ map cacheline of set 0 to physical address u
\ only set 0 is used as scratchpad ram
  bx ax mov 
  0fffff000 # ax and 400 # ax  or
  ax 4 tr mov
  bx ax mov 
  00ff0 # ax and  1 # ax  or
  ax 5 tr mov
  bx pop
;code

\ gx1 cpu internal L1 cache BLT registers
\  ebx = register address  eax = data
\ cpu-read
\ cpu-write
\ base ffffff0c base address bb0 
\      ffffff1c              bb1
\ pointer   ffffff2c reset pointer  bb0
\           ffffff3c                bb1

\ pm_base   ffffff6c power management
\ pm_mask   ffffff7c

code gx1-bbxbase@ ( 0|1 -- w)
    1 # bx and
    bx 4 # shl
    h# ffffff0c # bx or
    cpu_read
    ax bx mov
;code

code gx1-bbxbase! ( w 0|1 --)
    1 # bx and
    bx 4 # shl
    h# ffffff0c # bx or
    ax pop
    cpu_write
    bx pop
;code

: set-scratchpad-size  ( u -- )
\ u 2048,3072,4096kb   1024 disables cache
 1000 min 400 max        \ size is between 1k and 4k
 lock[  0c3 gx1@    \ ccr3
 tuck 0f and 10 or 0c3 gx1!  \ enable MAPEN
 8 rshift 4 - 
 0b8 gx1@ 3 and or   \ GCR@ 
 0b8 gx1! 0c3 gx1!
 ]unlock
;

: init-scratchpad ( u -- )
\ initialize scratchpad size  u = 2048,3072,4096
\ base address from gcr 0xb8[1:0]   0= disabled 
 1000 over - gx-base# +         \ base address of buffers in L1 cache 
 disable-cache invalidate-cache
 \ free whole cache and invalidate it
 400 set-scratchpad-size invalidate-cache  \ 400 set cache size to 0
\ remap the cache lines again
 over 0 do
   h# deadbeef setup-fill-buffer
   dup map-cache-line 10 +
   10 +loop
 drop
\ freeze mapped cache lines and enable remainder for normal use
 set-scratchpad-size
 enable-cache
\ set scratchpad base registers   leave 0x2c0  for user? 
   0 0 gx1-bbxbase!
 640 1 gx1-bbxbase!   
;

base !
previous definitions

\ @@FILE:pci.fth
\ 
\ depends on NONE
\ from coreboot tc7010 devices
\          f d b  
\ video at 4 12 0   
\ audio    3 12 0
\ ide      2 12 0
\ smi      1 12 0
\ usb      0 13 0
\ eth      0 15 0
\ index 10 has base address register: bar

base @ hex

only forth also
system definitions

00 constant VENDOR#   internal 
04 constant COMMAND#
0A constant CLASS#
0E constant HEADERTYPE# 
18 constant PRIMARYBUS#
2C constant SUBSYSVENDOR#
2E constant SUBSYS#

0 constant NORMAL#
1 constant BRIDGE#
2 constant CARDBUS#  

: pci-dev  ( bus dev func -- pci-dev )
   8 lshift  80000000 or   ( fn# << 8)
   swap 0b lshift  or  ( dev# << 0b)
   swap 10 lshift or  ( bus# << 10  )
;     

external


: pci-addr ( reg bus dev func -- int )
   pci-dev 
   3 invert and or ( reg# & ~3)
;

: pci-bus ( pci-dev -- bus) \ extract bus# from pci handle
  10 rshift ff and 
;

: pci-slot ( pci-dev -- dev) \   extract slot# from pci handle
  0b rshift 1f and 
;

: pci-func ( x1 -- func) \   extract func# from pci handle 
  8 rshift 07 and 
;

: pci-config-reg ( reg pci-dev  -- pciadr ) \ set config reg#
\ common factor for pci-x@
  swap 3 invert and or 0cf8 p!   
  0cfc 
;

: pci-c@ ( reg pci-dev   -- byte   )
 pci-config-reg   ( pcicdr# )
 pc@    
;

: pci-w@ ( reg pci-dev   -- word   )
 pci-config-reg 
 pw@
;


: pci@ (  reg pci-dev   -- dword   )
 pci-config-reg
 p@
;

: pci-c! ( val reg pci-dev  --   set pci reg )
 pci-config-reg   ( val pcicdr# )
 pc!    
;

: pci-w! ( val reg  pci-dev  --   set pci reg ) 
 pci-config-reg 
 pw!
;


: pci! (  val reg pci-dev  --   set pci reg ) 
 pci-config-reg
 p!
;


: pci-find-dev-on-bus ( dev ven bus -- x4 0|-1  id# ven# bus#  pci-dev flag )
   drop
;

: pci-read-res ( bar pci-dev  --    return ? )
 swap 2 lshift 10 + swap
 pci@
 ;


: pcisetbusmaster ( x1 --  set pci-dev as master )
  COMMAND# over pci-w@   ( bus w )
  COMMAND# or
  COMMAND# swap rot pci-w!
 ;


\  define base address registers for the device
10 0 15 0 pci-dev pci@ f invert and constant eth-io#  \ eth-io
14 0 15 0 pci-dev pci@ constant eth-bar#  \ eth
10 0 13 0 pci-dev pci@ constant usb-bar#  \ usb
10 0 12 4 pci-dev pci@ constant f4-bar#   \ video
10 0 12 3 pci-dev pci@ constant f3-bar#   \ audio
10 0 12 2 pci-dev pci@ constant f2-bar#   \ ide
20 0 12 2 pci-dev pci@ constant f2-io#   \ ide-io
10 0 12 1 pci-dev pci@ constant f1-bar#   \ smi
   0 12 0 pci-dev constant      f0-index  \ bridge

\ eth io register
: eth-io@ ( reg -- x) eth-io# + p@ ;
: eth-io! ( val reg -- ) eth-io# + p! ;

\ eth function register
: eth@ ( reg -- x) eth-bar# + @ ;
: eth! ( val reg -- ) eth-bar# + ! ;

\ usb function register
: usb@  ( reg -- x) usb-bar# + @ ;
: usb!  ( val reg -- ) usb-bar# + ! ;

\ read vga function registers
: f4@  ( reg -- x ) f4-bar# + @ ;
: f4! ( val reg  -- ) f4-bar# + ! ;

\ audio
: f3@  ( reg -- x ) f3-bar# + @ ;
: f3! ( val reg  -- ) f3-bar# + ! ;

\ ide 
: f2@  ( reg -- x ) f2-bar# + @ ;
: f2! ( val reg  -- ) f2-bar# + ! ;

\ ide io 
: f2-io@  ( reg -- x ) f2-io# + p@ ;
: f2-io! ( val reg  -- ) f2-io# + p! ;

\ pci bridge
: f0@ ( reg -- x) f0-index pci@ ;
: f0-c@ ( reg -- c ) f0-index pci-c@ ;
: f0-w@ ( reg -- w ) f0-index pci-w@ ;
: f0! ( val reg -- ) f0-index pc! ;
: f0-c! ( c reg -- ) f0-index pci-c! ;
: f0-w! ( w reg -- ) f0-index pci-w! ;

module 

previous definitions

base !

\ @@FILE:cs5530.fth
\ @@REQUIRES:gx1-cpu.fth 
\ words for glue chip which handles ide,graphics,usb,sound


base @  hex

only forth also
system definitions

\  flash chip write access 

: enable-flash
 52 dup f0-c@ 3 or swap f0-c!
 5b dup f0-c@ 20 or swap f0-c!
;

: disable-flash
 52 dup f0-c@ 3 invert and swap f0-c!
 5b dup f0-c@ 20 invert and swap f0-c!
;

\  crt 
\ enable crt and display logic
: crt-enable ( -- )  04 f4@  2f or 04 f4! ;
 
\ disable crt and display logic
: crt-disable ( -- )  04 f4@ 2f invert and 04 f4! ;


\ video initialization


\ video is at pci device function 4
\  f4@  and f4!

\ Access functions for various register banks


: set-video-clock  ( pll-value -- )
   80000920 invert and           \ pll value
   24 f4@ 20 invert and          \ disable,reset and pwr-down pll
   80000100 or 24 f4!
   1 ms 
   dup 24 f4!                \ set pll and give it time to settle
   1 ms

   800 or dup 24 f4!             \ enable pll
   80000000 invert and dup       \ clear reset
   24 f4!

   100 invert and 24 f4!        \ clear bypass
;

: activate-video ( sync_pol -- )  8 lshift 20002f or 04 f4! ;

: init-pic
\ initialize the interrupt controllers
\ pic1 command:0x20  data:0x21 
\ pic2 command:0xa0  data:0xa1
\ command icw1 0x11   icw4 0x01

\ send icw1
11 dup 20 pc! a0 pc!

\ remap pics
20 21 pc! 28 a1 pc!

\ connect irq2 to slave
4 21 pc! 2 a1 pc!
1 21 pc! 1 a1 pc!

\ disable all irqs
ff 21 pc!
;



base !

\ @@FILE:pnp.fth 
\ plug and play words
\ control register at 0x2e
\ data register at 0x2f
\ device 0:kbc 1:mouse 2:rtc 3:fdc
\        4:pport  5:uart2 6:uart1 7:gpio
\        8:power 
\  
base @ hex

only forth also
system  definitions

\ write val to pnp register
: pnp! ( val reg -- ) 2e pc!  2f pc!  ;

\ read pnp register
: pnp@ ( reg -- x) 2e pc!  2f pc@ ;

\ set logical device
: pnpldn! ( ldn -- ) 7 pnp!  ;

\ ldn 8 io address   Power management
  8 pnpldn!
  60 pnp@ 8 lshift 61 pnp@ or constant pnppm# 

: pnppm@ ( reg -- val ) pnppm# pc!  pnppm# 1+ pc@ ;

: pnppm! ( val reg -- ) pnppm# pc!  pnppm# 1+ pc! ;


\ ldn 7 io address   gpio 
 7 pnpldn!
 60 pnp@ 8 lshift 61 pnp@ or constant pnpgpio# 

: pnpgpio@ ( reg -- val ) pnpgpio# + pc@ ; 
 
: pnpgpio! ( val reg -- ) pnpgpio# + pc! ;

\ select gpio bank 0
: gpio0 ( -- ) 22 pnp@ 7f and 22 pnp! ;

\ select gpio bank 1
: gpio1 ( --) 22 pnp@ 80 or 22 pnp! ;

base !


previous definitions

\ @@FILE:gx-display.fth
\ @@REQUIRES:gx1-cpu.fth
\ display controller on TC7010 experiments
\ or just make a part of the gx1 words
\ frame buffer delta of 4k at 1280x1024@16bpp does not work on gx1 revision 8.1
\ memory organised as :
\ <--line/comp delta------>
\ scan1 comp1 cursor1 fill1
\       :
\ scanN compN cursorN fillN
\     Alternative
\ <line delta>
\   scan 1
\     :
\   scan n
\ < comp delta>
\ compression 1
\    :
\ compression n
\ cursor
\ 
\ default mode is 1024x768@16bpp
\
\ xres,yres,bpp,cb-offset,cb-delta,cb-size,cu-offset,offscreen-offset
\ 640,  480, 16,     1280,    2048,    272,    0x610,         0xF0000
\ 800,  600, 16,     1600,    2048,    272, 0x12C000,        0x12C100
\ 1024, 768, 16, 0x180000,     272,    272, 0x1B3000,        0x1B3100
\ 1152, 864, 16,     2304,    4096,    272,    0xA10,        0x360000
\ 1280,1024, 16,     2560,    4096,    272,    0xB10,       0x4000000
base @ hex

only forth also
system definitions

\ Graphics Frame Buffer
gx-base# h# 800000 + value fbuf

\ write  into frame buffer
defer fb!    ( x a --)
\  read from frame buffer
defer fb@ ( a -- x)
\  fill frame buffer
defer fb-fill ( a u x --)

: fb-bpp-16 ( --) 
    ['] w! is fb!
    ['] w@ is fb@
    ['] wfill is fb-fill
;

: fb-bpp-8 ( --) 
    ['] c! is fb!
    ['] c@ is fb@
    ['] fill is fb-fill
;

fb-bpp-16    \ default is 16 bpp

\ dcr display controller register at gx-base#+83xx
\ frame buffer offset  in ram
: fb-offset@ ( -- u ) 10 gx1-dcr@  3fffff and ;

\ framebuffer line delta ( pitch)  in bytes 
: fb-line-delta@ ( -- u ) 24 gx1-dcr@ 7ff and 2 lshift ;

: fb-line-delta! ( u --)
  dup 2 rshift 24 gx1-dcr@ 0fffff000 and or
  gx1-dcr-lock-push swap 
    24 gx1-dcr! 
  gx1-dcr-lock-pop
  \ update graphics pipeline
  dup 800 > if drop 1000
  else 400 > if 800 else 400 then
  then
  10c gx1-gpr@ 600 invert and or 
  10c gx1-gpr!
;

\ framebuffer line size in bytes
: fb-line-size@ ( -- u ) 28 gx1-dcr@ 1ff and 3 lshift ;

\ framebuffer pixels  h_active
: fb-pixels/line ( --- u ) 30 gx1-dcr@ 7ff and 1+ ;

\ framebuffer lines   v_active
: fb-lines@  (  -- x ) 40 gx1-dcr@ 7ff and 1+ ;

\ compression buffer offset
: cb-offset@ ( -- u ) 14 gx1-dcr@ 3fffff and ;

\ compression buffer line delta ( pitch) in bytes
: cb-line-delta@ ( -- x ) 
  24 gx1-dcr@ 0c rshift 03ff and
  2 lshift 
;
\ compression buffer pitch
: cb-line-delta! ( u-- )
  gx1-dcr-lock-push swap
    0a lshift 7ff000 and 
    24 gx1-dcr@  0ff800fff and or
    24 gx1-dcr! 
  gx1-dcr-lock-pop
;

\ compression buffer line size in bytes
\ controller always writes 2 extra qwords so 
\ add 16 bytes to the value obtained
: cb-line-size@ ( -- x ) 
   28 gx1-dcr@ 9 rshift 7f and 1-
   2 lshift 10 +
;

\ controller always writes 2 extra qwords so 
\ subtract 16 bytes from specified size
: cb-line-size! ( u -- )
  10 - 2/ 1+ 7f and       \ convert to dwords + 1
  9 lshift                \  bits 15:9
  gx1-dcr-lock-push swap 
       swap  28 gx1-dcr@  0ffff01ff and or 
       28 gx1-dcr! 
  gx1-dcr-lock-pop
;


\ video buffer  offset
: vb-offset@ ( -- u ) 20 gx1-dcr@  3fffff and ;

: vb-size! ( x y -- )
 * 2* 3f + 0a lshift 0ffff0000 and
 28 gx1-dcr@ 0ffff and or
 gx1-dcr-lock-push swap 28 gx1-dcr! gx1-dcr-lock-pop
;

\ videobuffer size in  bytes
: vb-size@ ( -- x ) 28 gx1-dcr@ 10 rshift 0fffc0 and ;

\ vertical blank status 
: gx1-vblank? ( -- flg  )  08 gx1-dcr@ 40000000 and 0= ;

\ timing generator active
: gx1-tgen? ( -- flg) 08 gx1-dcr@ 20 and ;
 
: gx1-wait-vblank ( -- ) 
  08 gx1-dcr@ 20 and if    \ timing generator active?
      begin gx1-vblank? until
      begin gx1-vblank? 0= until
  then
;

\ enable video u size in bytes
: gx1-video-enable ( x y -- )
  gx1-dcr-lock-push
    04 gx1-dcr@ 30000000 or 04 gx1-dcr!
  gx1-dcr-lock-pop  
  vb-size!
;

: gx1-video-disable ( -- )
  gx1-dcr-lock-push
   04 gx1-dcr@ 30000000 invert and 04 gx1-dcr!
   28 gx1-dcr@ 0ffff and  28 gx1-dcr!
 gx1-dcr-lock-pop   
;

: cub-offset@ ( -- u) 18 gx1-dcr@ 3fffff and ;

: gx1-cursor-enable ( -- )  \ enable hardware cursor 
  gx1-dcr-lock-push
     04 gx1-dcr@ 2 or 04 gx1-dcr!
  gx1-dcr-lock-pop
;

: gx1-cursor-disable ( -- )  \ disable hardware cursor  
 gx1-dcr-lock-push 
     04 gx1-dcr@ 2 invert and 04 gx1-dcr!
 gx1-dcr-lock-pop
;

: gx1-cursor-color@ ( pal-reg -- u) \ 
    gx1-palette-reg@
    dup  6 lshift 0fc0000 and
    over 4 lshift 0fc00 and or
    swap 2 lshift 0fc and or
;

: gx1-cursor-color! ( u pal-reg -- )
    swap dup h# 0fc and 2 rshift
    over h# 0fc00 and 4 rshift or
    swap h# 0fc0000 and 6 rshift or
    swap gx1-palette-reg!
;

: gx1-cursor-bg@ ( -- u)  100 gx1-cursor-color@ ;
: gx1-cursor-bg! ( u -- ) 100 gx1-cursor-color! ;

: gx1-cursor-fg@ ( -- u)  101 gx1-cursor-color@ ;
: gx1-cursor-fg! ( u -- ) 101 gx1-cursor-color! ;

: gx1-cursor-position@ ( -- x y)   \ get cursor xy cordinates
  50 gx1-dcr@ 7ff and
  58 gx1-dcr@ 3ff and
;

: gx1-cursor-position! ( x y --)  \ set cursor xy cordinates
  3ff tuck and swap invert 58 gx1-dcr@ and or 58 gx1-dcr!
  7ff tuck and swap invert 50 gx1-dcr@ and or 50 gx1-dcr!
;

\ hardware compressor
: gx1-valid-bit@ ( u -- flg )
 18 gx1-mcr!               \ dirty ram index
 1c gx1-mcr@ 1 and         \ access dirty ram
;
 
: gx1-comp-disable ( -- )
 gx1-dcr-lock-push
    04 gx1-dcr@ 10 invert and 04 gx1-dcr!
 gx1-dcr-lock-pop
;

: gx1-decomp-disable ( -- )
 gx1-dcr-lock-push
    04 gx1-dcr@ 20 invert and 04 gx1-dcr!
 gx1-dcr-lock-pop
;
 
: gx1-comp-enable ( -- )
 fb-offset@ 0<> if exit then    \ offset must be zero for compression 
 400 0 do i 18 gx1-mcr! 0 1c  gx1-mcr! loop  \ clear dirty/valid RAM
 gx1-dcr-lock-push
    04 gx1-dcr@ 10 or 04 gx1-dcr!
 gx1-dcr-lock-pop
;

: gx1-decomp-enable ( -- )
 gx1-dcr-lock-push
    04 gx1-dcr@ 20 or 04 gx1-dcr!
 gx1-dcr-lock-pop
;

\ compression status
: gx1-compression? ( -- t) 04 gx1-dcr@ 10 and 0<> ;

: gx1-fb-comp-enable  ( -- ) 
 fb-offset@ 0<> if exit then
 400 0 do i 18 gx1-mcr! 0 1c  gx1-mcr! loop  \ clear dirty/valid RAM
 gx1-dcr-lock-push
    04 gx1-dcr@ 30 or 04 gx1-dcr!
 gx1-dcr-lock-pop
;

: gx1-fb-comp-disable ( -- ) 
 gx1-dcr-lock-push
     04 gx1-dcr@ 30 invert and 04 gx1-dcr!
 gx1-dcr-lock-pop
;

: gx1-reset-video ( -- )
  gx1-video-disable
  0 0 vb-size!
  gx1-tgen? if
      begin gx1-vblank? until
      begin gx1-vblank? 0= until
      begin gx1-vblank? until
      begin gx1-vblank? 0= until
  then
;

10 value bits/pixel   
2  value bytes/pixel
0 value video-mode
d# 640 value pixels/line    \ x-resolution
d# 480 value lines/frame    \ y-resolution

: (htiming!) ( hactive htotal -- ) \ horizontal timing 
 1- 10 lshift swap 1- or            \ active and blank
 gx1-dcr-lock-push swap 
    30 gx1-dcr! 
 gx1-dcr-lock-pop
;
 
: (hblank!) ( start end -- )      \ horizontal timing 
 1- 10 lshift swap 1- or          \ active and blank
 gx1-dcr-lock-push swap
    34 gx1-dcr! 
 gx1-dcr-lock-pop
;
 
: (hsync!) ( start end -- )       \ horizontal sync
1- 10 lshift swap 1- or          
 gx1-dcr-lock-push swap
    38 gx1-dcr! 
 gx1-dcr-lock-pop
;

: (hsync-fp!) ( start end -- )    \ Panel horizontal sync
1- 10 lshift swap 1- or     
 gx1-dcr-lock-push swap 
    3c gx1-dcr! 
 gx1-dcr-lock-pop
;

: (vtiming!) ( vactive vtotal -- ) \ horizontal timing 
 1- 10 lshift swap 1- or            \ active and blank
 gx1-dcr-lock-push swap 
    40 gx1-dcr! 
 gx1-dcr-lock-pop
;
 
: (vblank!) ( start end -- )      \ horizontal timing 
 1- 10 lshift swap 1- or          \ active and blank
 gx1-dcr-lock-push swap 
    44 gx1-dcr! 
 gx1-dcr-lock-pop
;
 
: (vsync!) ( start end -- )       \ horizontal sync
 1- 10 lshift swap 1- or          
 gx1-dcr-lock-push swap 
    48 gx1-dcr! 
 gx1-dcr-lock-pop
;

: (vsync-fp!) ( start end -- )    \ Panel horizontal sync
 2 - 10 lshift swap 2 - or     
 gx1-dcr-lock-push swap 
    4c gx1-dcr! 
 gx1-dcr-lock-pop
;

: mode640x480@16  ( -- )
\  "640x480" 33915801 31.5  640 664 704 832
\                           480 489 491 520
\                           -hsync -vsync
\ 640x480-16@72
\ xres,yres,bpp,cb-offset,cb-delta,cb-size,cu-offset,offscreen-offset
\ 640,  480, 16,     1280,    2048,    272,    0x610,         0xF0000
  2 to bytes/pixel
  10 to bits/pixel
  fb-bpp-16
  0 to video-mode
  d# 640 to pixels/line
  d# 480 to lines/frame
 
  gx1-fb-comp-disable        \ disable compression   
  gx1-reset-video            \ 
  gx1-dcr-lock-push   
\ blank display and disable tgen
  08 gx1-dcr@ 28 invert and 08 gx1-dcr! 1 ms
  
\ disable display fifo and compression
  04 gx1-dcr@ 31 invert and 04 gx1-dcr!
\ clear dot clock multiplier
  04 gx1-dcr@ 0c00000c0 invert and 04 gx1-dcr!
\ set the dot clock frequency
  33915801 set-video-clock 1 ms
\ set dot clock multiplier
  04 gx1-dcr@ 80 or 04 gx1-dcr! 1 ms
  
\ set buffers 
  0 10 gx1-dcr!             \ frame buffer offset 
  500 14 gx1-dcr!           \ compress buffer offset size 256 bytes
  610 18 gx1-dcr!           \ cursor buffer
  
\ set line size and delta ( pitch )
  200200  24 gx1-dcr!       \ cb and fb line delta
  8aa2    28 gx1-dcr!       \ cb size less 16 bytes +1  fb size + 2 quadwords
  
\ set timing  
 
 d# 640 d# 832 (htiming!)  \ horizontal timing active total 
 d# 640 d# 832 (hblank!)   \ blank start end
 d# 664 d# 704 (hsync!)    \ sync start end
 d# 664 d# 704 (hsync-fp!)  

 d# 480 d# 520 (vtiming!)  \ vertical timing lines active total 
 d# 480 d# 520 (vblank!)   \ blank  start end
 d# 489 d# 491 (vsync!)    \ sync start end 
 d# 489 d# 491 (vsync-fp!)   \
  
 3004     0c gx1-dcr!      \ output config
 2f       08 gx1-dcr! 1 ms \ timing config
 30006581 04 gx1-dcr!      \ general config 

\ video processor sync
 3 activate-video 
 
\ update graphics pipeline delta
  10c gx1-gpr@  300 or 10c gx1-gpr!
  
  gx1-dcr-lock-pop 
 
;

: mode800x600@16 (  -- )
\  "800x600" 23088801  50   800 856 976 1040
\                           600 637 643 666
\                           +hsync +vsync
\ xres,yres,bpp,cb-offset,cb-delta,cb-size,cu-offset,offscreen-offset
\ 800,  600, 16,     1600,    2048,    272, 0x12C000,        0x12C100
  2 to bytes/pixel
  10 to bits/pixel
  fb-bpp-16
  1 to video-mode
  d# 800 to pixels/line
  d# 600 to lines/frame

  gx1-fb-comp-disable        \ disable compression   
  gx1-reset-video            \ 
  gx1-dcr-lock-push   
\ blank display and disable tgen
  08 gx1-dcr@ 28 invert and 08 gx1-dcr! 1 ms
  
\ disable display fifo and compression
  04 gx1-dcr@ 31 invert and 04 gx1-dcr!
\ clear dot clock multiplier
  04 gx1-dcr@ 0c00000c0 invert and 04 gx1-dcr!
\ set the dot clock frequency
  23088801 set-video-clock 1 ms
\ set dot clock multiplier
  04 gx1-dcr@ 80 or 04 gx1-dcr! 1 ms
  
\ set buffers 
  0 10 gx1-dcr!             \ frame buffer offset 
  640 14 gx1-dcr!           \ compress buffer offset size 256 bytes
  750 18 gx1-dcr!           \ cursor buffer
  
\ set buffers 
  0 10 gx1-dcr!             \ frame buffer offset 
  500 14 gx1-dcr!           \ compress buffer offset size 256 bytes
  610 18 gx1-dcr!           \ cursor buffer
  
\ set line size and delta ( pitch )
  200200  24 gx1-dcr!       \ cb and fb line delta
    82ca  28 gx1-dcr!       \ cb size less 16 bytes +1  fb size + 2 quadwords
  
\ set timing  
 
  d# 800 d# 1040 (htiming!)  \ horizontal timing active total 
  d# 800 d# 1040 (hblank!)   \ blank start end
  d# 856 d# 976  (hsync!)    \ sync start end
  d# 856 d# 976  (hsync-fp!)  

  d# 600 d# 666 (vtiming!)  \ vertical timing lines active total 
  d# 600 d# 666 (vblank!)   \ blank  start end
  d# 637 d# 643 (vsync!)    \ sync start end 
  d# 637 d# 643 (vsync-fp!)   \
  
  3004     0c gx1-dcr!      \ output config
  2f       08 gx1-dcr! 1 ms \ timing config
  30006581 04 gx1-dcr!      \ general config
 
  0 50 gx1-dcr!         \ cursor x
  0 58 gx1-dcr!         \ cursor y
  0 60 gx1-dcr!         \ cursor color
  0 68 gx1-dcr!         \ screen border
\ video processor sync
  0 activate-video 
 
\ update graphics pipeline delta
  10c gx1-gpr@  300 or 10c gx1-gpr!
  
  gx1-dcr-lock-pop 
;
 
: mode1024x768@16  ( -- )
\  "1024x768" 37E22801 75 1024 1048 1184 1328
\                          768  771  777  806  
\                          -hsync -vsync
\ xres,yres,bpp,cb-offset,cb-delta,cb-size,cu-offset,offscreen-offset
\ 1024, 768, 16, 0x180000,     272,    272, 0x1B3000,        0x1B3100
  2 to bytes/pixel
  10 to bits/pixel
  2 to video-mode
  fb-bpp-16
  d# 1024 to pixels/line
  d# 768 to lines/frame

  gx1-fb-comp-disable        \ disable compression   
  gx1-reset-video            \ 
  gx1-dcr-lock-push   
\ blank display and disable tgen
  08 gx1-dcr@ 28 invert and 08 gx1-dcr! 1 ms
  
\ disable display fifo and compression
  04 gx1-dcr@ 31 invert and 04 gx1-dcr!
\ clear dot clock multiplier
  04 gx1-dcr@ 0c00000c0 invert and 04 gx1-dcr!
\ set the dot clock frequency
  37E22801 set-video-clock 1 ms
\ set dot clock multiplier
  04 gx1-dcr@ 80 or 04 gx1-dcr! 1 ms
  
\ set buffers 
  0      10 gx1-dcr!           \ frame buffer offset 
  180000 14 gx1-dcr!           \ compress buffer offset size 256 bytes
  1b3000 18 gx1-dcr!           \ cursor buffer
  0      20 gx1-dcr!           \  video buffer
\ set line size and delta ( pitch )
  110200  24 gx1-dcr!       \ cb and fb line delta
  8302    28 gx1-dcr!       \ cb size less 16 bytes +1  fb size + 2 quadwords
  
\ set timing  
  d# 1024 d# 1328 (htiming!)  \ horizontal timing active total 
  d# 1024 d# 1328 (hblank!)   \ blank start end
  d# 1048 d# 1184 (hsync!)    \ sync start end
  d# 1048 d# 1184 (hsync-fp!)  

  d# 768 d# 806 (vtiming!)  \ vertical timing lines active total 
  d# 768 d# 806 (vblank!)   \ blank  start end
  d# 771 d# 777 (vsync!)    \ sync start end 
  d# 771 d# 777 (vsync-fp!) \
  
  3004     0c gx1-dcr!      \ output config
  2f       08 gx1-dcr! 1 ms \ timing config
  30006581 04 gx1-dcr!      \ FIFO CLK_DIV , CMP disabled

  0 50 gx1-dcr!         \ cursor x
  0 58 gx1-dcr!         \ cursor y
  0 60 gx1-dcr!         \ cursor color
  0 68 gx1-dcr!         \ screen border
 
 \ video processor sync
  3 activate-video 
  
\ update graphics pipeline delta
  10c gx1-gpr@  300 or 10c gx1-gpr!

  gx1-dcr-lock-pop 
;

: mode1280x960@16  ( -- )
\  "1280x960" 2710C805 108 1280 1376 1480 1800
\                           960  961  964 1000  
\                           +hsync +vsync
\ xres,yres,bpp,cb-offset,cb-delta,cb-size,cu-offset,offscreen-offset
\ 1280,1024, 16,     2560,    4096,    272,    0xB10,       0x3c0000
  2 to bytes/pixel
  10 to bits/pixel
  fb-bpp-16
  3 to video-mode
  d# 1280 to pixels/line
  d# 960 to lines/frame
  
  gx1-fb-comp-disable        \ disable compression   
  gx1-reset-video            \ 
  gx1-dcr-lock-push   
\ blank display and disable tgen
  08 gx1-dcr@ 28 invert and 08 gx1-dcr! 1 ms
  
\ disable display fifo and compression
  04 gx1-dcr@ 31 invert and 04 gx1-dcr!
\ clear dot clock multiplier
  04 gx1-dcr@ 0c00000c0 invert and 04 gx1-dcr!
\ set the dot clock frequency
  2710C805 set-video-clock 1 ms
\ set dot clock multiplier
  04 gx1-dcr@ 80 or 04 gx1-dcr! 1 ms
 
\ set buffers 
  0 10 gx1-dcr!             \ frame buffer offset 
  a00 14 gx1-dcr!           \ compress buffer offset size 256 bytes
  b10 18 gx1-dcr!           \ cursor buffer
  3c0000 20 gx1-dcr!        \ video buffer offset
   
 \ set line size and delta ( pitch )
\  280280  24 gx1-dcr!       \ cb and fb line delta
  400400  24 gx1-dcr!       \ cb and fb line delta
  8342    28 gx1-dcr!       \ cb size less 16 bytes +1  fb size + 2 quadwords
  
\ set timing 
\  1280 1328 1440 1688 1024 1025 1028 1066  
 d# 1280 d# 1800 (htiming!)  \ horizontal timing active total 
 d# 1280 d# 1800 (hblank!)   \ blank start end
 d# 1376 d# 1440 (hsync!)    \ sync start end
 d# 1376 d# 1440 (hsync-fp!)  

 d# 960 d# 1000 (vtiming!)  \ vertical timing lines active total 
 d# 960 d# 1000 (vblank!)   \ blank  start end
 d# 961 d# 964 (vsync!)    \ sync start end 
 d# 961 d# 964 (vsync-fp!) 
  
 3004     0c gx1-dcr!      \ output config
 2f       08 gx1-dcr! 1 ms \ timing config
 30006581 04 gx1-dcr!      \ general config 
 
 200 50 gx1-dcr! 
 200 58 gx1-dcr!           \ cursor x y
   0 60 gx1-dcr!           \ cursor color
   0 68 gx1-dcr!           \ screen border
 
  0 activate-video         \ turn on video +v +h
 
\ update graphics pipeline delta to 4k
  10c gx1-gpr@  500 or 10c gx1-gpr!
  
  gx1-dcr-lock-pop 
;
 
: mode1280x1024@16   ( -- )
\  "1280x1024" 2710C805 108 1280 1328 1440 1688
\                           1024 1025 1028 1066  
\                           +hsync +vsync
\ xres,yres,bpp,cb-offset,cb-delta,cb-size,cu-offset,offscreen-offset
\ 1280,1024, 16,     2560,    4096,    272,    0xB10,       0x400000

  2 to bytes/pixel
  10 to bits/pixel
  4 to video-mode
  fb-bpp-16
  d# 1280 to pixels/line
  d# 1024 to lines/frame
 
  gx1-fb-comp-disable        \ disable compression   
  gx1-reset-video            \ 
  gx1-dcr-lock-push   
\ blank display and disable tgen
  08 gx1-dcr@ 28 invert and 08 gx1-dcr! 1 ms
  
\ disable display fifo and compression
  04 gx1-dcr@ 31 invert and 04 gx1-dcr!
\ clear dot clock multiplier
  04 gx1-dcr@ 0c00000c0 invert and 04 gx1-dcr!
\ set the dot clock frequency
  2710C805 set-video-clock 1 ms
\ set dot clock multiplier
  04 gx1-dcr@ 80 or 04 gx1-dcr! 1 ms
 
\ set buffers 
  0    10 gx1-dcr!             \ frame buffer offset 
  a00  14 gx1-dcr!           \ compress buffer offset size 256 bytes
  b10  18 gx1-dcr!           \ cursor buffer
    0  20 gx1-dcr!           \ video buffer offset
   
\ set line size and delta ( pitch )
  400400 24 gx1-dcr!       \ cb and fb line delta
  8342   28 gx1-dcr!       \ cb size less 16 bytes +1  fb size + 2 quadwords
  
\ set timing 
\  1280 1328 1440 1688 1024 1025 1028 1066  
 d# 1280 d# 1688 (htiming!)  \ horizontal timing active total 
 d# 1280 d# 1688 (hblank!)   \ blank start end
 d# 1328 d# 1440 (hsync!)    \ sync start end
 d# 1328 d# 1440 (hsync-fp!)  

 d# 1024 d# 1066 (vtiming!)  \ vertical timing lines active total 
 d# 1024 d# 1066 (vblank!)   \ blank  start end
 d# 1025 d# 1028 (vsync!)    \ sync start end 
 d# 1025 d# 1028 (vsync-fp!) 
  
 004     0c gx1-dcr!      \ output config panel disabled
 2f       08 gx1-dcr! 1 ms \ timing config
 30006581 04 gx1-dcr!      \ general config 
 
 200 50 gx1-dcr! 
 200 58 gx1-dcr!           \ cursor x y
   0 60 gx1-dcr!           \ cursor color
   0 68 gx1-dcr!           \ screen border
 
  0 activate-video         \ turn on video +v +h
 
\ update graphics pipeline delta to 4k 16bpp
  10c gx1-gpr@  500 or 10c gx1-gpr!
  
  gx1-dcr-lock-pop 
;

: mode1280x1024@8   ( -- )
\  "1280x1024" 2710C805 108 1280 1328 1440 1688
\                           1024 1025 1028 1066  
\                           +hsync +vsync
\ xres,yres,bpp,cb-offset,cb-delta,cb-size,cu-offset,offscreen-offset
\ 1280,1024,  8,     1280,    2048,    272,    0x610,       0xF0000

  1 to bytes/pixel
  8 to bits/pixel
  5 to video-mode
  d# 1280 to pixels/line
  d# 1024 to lines/frame
  fb-bpp-8 
  gx1-fb-comp-disable        \ disable compression   
  gx1-reset-video            \ 
  gx1-dcr-lock-push   
\ blank display and disable tgen
  08 gx1-dcr@ 28 invert and 08 gx1-dcr! 1 ms
  
\ disable display fifo and compression
  04 gx1-dcr@ 31 invert and 04 gx1-dcr!
\ clear dot clock multiplier
  04 gx1-dcr@ 0c00000c0 invert and 04 gx1-dcr!
\ set the dot clock frequency
  2710C805 set-video-clock 1 ms
\ set dot clock multiplier
  04 gx1-dcr@ 80 or 04 gx1-dcr! 1 ms
 
\ set buffers 
  0 10 gx1-dcr!             \ frame buffer offset 
  500 14 gx1-dcr!           \ compress buffer offset size 256 bytes
  610 18 gx1-dcr!           \ cursor buffer
    0 20 gx1-dcr!           \ video buffer offset
   
\ set line size and delta ( pitch )
  200200  24 gx1-dcr!       \ cb and fb line delta
  8aa2    28 gx1-dcr!       \ cb size less 16 bytes +1  fb size + 2 quadwords
  
\ set timing 
\  1280 1328 1448 1688 1024 1025 1028 1066  
 d# 1280 d# 1688 (htiming!)  \ horizontal timing active total 
 d# 1280 d# 1688 (hblank!)   \ blank start end
 d# 1328 d# 1440 (hsync!)    \ sync start end
 d# 1328 d# 1440 (hsync-fp!)  

 d# 1024 d# 1066 (vtiming!)  \ vertical timing lines active total 
 d# 1024 d# 1066 (vblank!)   \ blank  start end
 d# 1025 d# 1028 (vsync!)    \ sync start end 
 d# 1025 d# 1028 (vsync-fp!) 
  
 3005     0c gx1-dcr!      \ output config
 2f       08 gx1-dcr! 1 ms \ timing config
 30006581 04 gx1-dcr!      \ general config 
 
 200 50 gx1-dcr! 
 200 58 gx1-dcr!           \ cursor x y
   0 60 gx1-dcr!           \ cursor color
   0 68 gx1-dcr!           \ screen border
 
  0 activate-video         \ turn on video +v +h
 
\ update graphics pipeline delta to 2k and 8 bpp
  10c gx1-gpr@  200 or 10c gx1-gpr!
  
  gx1-dcr-lock-pop 
;
 
 : set-graphic-mode ( mode -- )
  video-off
  case
   0 of mode640x480@16 endof
   1 of mode800x600@16 endof
   2 of mode1024x768@16 endof
   3 of mode1280x960@16 endof
   4 of mode1280x1024@16 endof
   5 of mode1280x1024@8 endof
   ( default ) mode640x480@16
  endcase
  video-on
;


also forth definitions

: init-controller  ( -- )
   crt-disable 
   video-mode set-graphic-mode
   crt-enable
;

previous previous definitions

base !

\ @@FILE:gx1-gaccel.fth
\ blt buffer sizes : 0x640
\ T gx1-ge-bpp ( -- )
\ T gx1-ge-solid-pattern! ( s -- ) 
\ T gx1-ge-mono-source!  ( bg fg tr -- )
\ T gx1-ge-solid-source! ( w --)
\ T gx1-ge-mono-pattern! ( fg bg d0 d1 tr -- )
\ T gx1-ge-color-pattern! ( fg bg d0 d1 d2 d3 tr -- )
\ T gx1-ge-color-pattern-line!  ( y pattern[] ) 
\ T gx1-ge-raster-operation! ( rop -- )
\ T gx1-ge-pattern-fill ( x y w h -- )
\ T gx1-ge-screen-screen-blit ( sx sy dx dy w h -- )
\ T gx1-ge-screen-to-screen-xblt ( sx sy dx dy w h tr -- )
\ T gx1-color-bitmap-to-screen-blt
\ T gx1-color-bitmap-to_screen-xblt
\ T gx1-mono-bitmap-to_screen-blt
\ T gx1-ge-bresenham-line ( x y length ie ae de flgs)
\ T gx1-ge-wait-until-idle ( -- )

only forth also 
system  definitions
base @ hex

[undefined] off [if]
: off ( addr -- )  0 swap ! ;
[then]
[undefined] or! [if]
: or! ( n addr -- ) tuck @ or swap ! ;
[then]
[undefined] 4dup [if]
: 4dup ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 x3 x4  ) 2over 2over ;
[then]

\ state variables used in this module  
\ value bits/pixel is 16 bpp default defined elsewhere
variable GFXbb0Base         \ address of blt buffer 0
variable GFXbb1Base         \ address of blt buffera 1
variable GFXbufferWidthPixels    \ 400
variable GFXpatternFlags
variable GFXsourceFlags
variable GFXsavedColor
variable GFXsavedRop
variable GFXusesDstData 

\ global variables defined in this module

variable gx1-ge-section
variable gx1-ge-buffer-width
variable gx1-ge-blt-mode
variable gx1-ge-temp-height
variable gx1-ge-bpp-shift
variable gx1-ge-offset
variable gx1-ge-scratch-base

\  check for pending BLT operation
: gx1-ge-blt-pending? ( -- flg) 10c gx1-gpr@ 4 and ;

\ wait until pending BLTs complete 
: gx1-ge-wait-pending ( -- )  begin gx1-ge-blt-pending? 0= until ;

\ wait until current BLT completes 
: gx1-ge-wait-busy ( -- )  begin 10c gx1-gpr@ 1 and 0= until ;

\ wait until pipeline is idle
: gx1-ge-wait-pipeline ( -- )  begin 10c gx1-gpr@ 2 and 0= until ;

\ wait until the graphics engine is idle. 
: gx1-ge-wait-until-idle ( -- )  gx1-ge-wait-busy ;

\ Graphics engine register operations 
: gx1-ge-rop! ( w -- ) 100  gx1-gpr-w! ;
: gx1-ge-vect! ( w -- ) 104 gx1-gpr-w! ;
: gx1-ge-blt! ( w -- ) 108 gx1-gpr-w! ;

: gx1-ge-dst-x! ( w -- ) 00 gx1-gpr-w! ;    \ x destination
: gx1-ge-dst-y! ( w -- ) 02 gx1-gpr-w! ;    \ y destination
: gx1-ge-width! ( w -- ) 04 gx1-gpr-w!  ;   \  width
: gx1-ge-height! ( w -- ) 06 gx1-gpr-w! ;   \ height
: gx1-ge-src-x! ( w -- ) 08 gx1-gpr-w! ;    \ x  source
: gx1-ge-src-y! ( w -- ) 0a gx1-gpr-w! ;    \ y source
: gx1-ge-src-color0! ( w -- )  0c gx1-gpr-w! ;
: gx1-ge-src-color1! ( w -- )  0e gx1-gpr-w! ;
: gx1-ge-pat-color0! ( w -- )  10 gx1-gpr-w! ;
: gx1-ge-pat-color1! ( w -- )  12 gx1-gpr-w! ;
: gx1-ge-pat-color2! ( w -- )  14 gx1-gpr-w! ;
: gx1-ge-pat-color3! ( w -- )  16 gx1-gpr-w! ;
: gx1-ge-pat-data0! ( w -- )  20 gx1-gpr! ;
: gx1-ge-pat-data1! ( w -- )  24 gx1-gpr! ;
: gx1-ge-pat-data2! ( w -- )  28 gx1-gpr! ;
: gx1-ge-pat-data3! ( w -- )  2c gx1-gpr! ;


\ set scratchpad base for blt operations
: gx1-ge-scratch-base! ( u -- )  gx-base# + gx1-ge-scratch-base ! ;

: gx1-ge-scratch! ( u bbxbase -- ) gx-base# + ! ;

\ write multiple bytes to buffer specified in gx1-ge-scratch-base
\ writes are dword aligned. 
\ array : data to be copied
\ offset : start offset in array
\ len : number of bytes to copy
: gx1-ge-scratch-str! ( array offset len -- )
    >r + gx1-ge-scratch-base @ r> move
;

\ set the bpp value in the graphics engine and initialize GFXbufferWidthPixels
: gx1-ge-bpp ( -- ) 
  bits/pixel 
  1 gx1-bbxbase@  dup GFXbb1Base  !
  0 gx1-bbxbase@  dup GFXbb0Base  !
  - 10 - 
  over 8 > if 2/ then  GFXbufferWidthPixels !
  8 > if 10 else 0 then
  fb-line-delta@ 
  dup d# 1024 > if swap 20 or swap then
  d# 2048 > if 40 or then
  gx1-ge-wait-busy 10c gx1-gpr!         \ update status register
;

: gx1-ge-8bpp ( color -- 'color )  \ copy low bits into high bits
    bits/pixel 8 = if   \ format 8bpp [15:8] is duplicate of [7:0]
        h# ff and
        dup 8 lshift or
    then
;

\ specify a solid source colour. 
: gx1-ge-solid-source! ( color --)

    \ CLEAR TRANSPARENCY FLAG
    GFXsourceFlags off

    \ FORMAT 8 BPP COLOR
    gx1-ge-8bpp
    gx1-ge-wait-pending 
    dup gx1-ge-src-color0!  gx1-ge-src-color1!
;

\ specify the monochrome source colour.  
\ must be called *after* loading any pattern data
: gx1-ge-mono-source! ( bg fg tr -- )
    \ SET TRANSPARENCY FLAG
    0<>  h# 0800 and GFXsourceFlags !  \ RM_SC_TRANSPARENT
    
    \ FORMAT 8BPP COLOR 
    gx1-ge-8bpp swap
    gx1-ge-8bpp swap
    
    \ poll until able to write the source colour 
    gx1-ge-wait-pending
    gx1-ge-src-color1! gx1-ge-src-color0!   \ GP_SRC_COLOR_1/0
;   

\  specify a solid pattern colour.
\  called before FILLs or BLTs that use a solid pattern color. 
\  gfx-ge-raster-operation! should always be called after
: gx1-ge-solid-pattern! ( color -- )
  GFXsourceFlags off  GFXpatternFlags off
  gx1-ge-8bpp
  dup GFXsavedColor !  
\ poll until able to write the pattern color 
  gx1-ge-wait-pending gx1-ge-pat-color0!    \ pattern colour_0
;   

\ specify a monochrome pattern. 
: gx1-ge-mono-pattern! ( bg fg data0 data1 transparent )
  GFXsourceFlags off      \ clear transparency, set pattern flags
  if h# 500                  \ RM_PAT_MONO|RM_PAT_TRANSPARENT
  else h# 100  then          \ RM_PAT_MONO 
  GFXpatternFlags !  \ pattern flags
  2swap gx1-ge-8bpp swap gx1-ge-8bpp swap 2swap

\ poll until able to write the pattern colors and data 
   gx1-ge-wait-pending
   gx1-ge-pat-data1!   gx1-ge-pat-data0!    \ pattern data_1/0
   gx1-ge-pat-color1!  gx1-ge-pat-color0!   \ colour_1/0
;

\ specify a color pattern. 
: gx1-ge-color-pattern! ( bg fg data0 data1 data2 data3 transparent)
    GFXsourceFlags off       \ clear transparency, set pattern  flags
    if h# 700                \ RM_PAT_MONO|RM_PAT_TRANSPARENT|RM_PAT_COLOR 
    else h# 300  then        \ RM_PAT_MONO|RM_PAT_COLOR
    GFXpatternFlags !
    2rot  gx1-ge-8bpp swap gx1-ge-8bpp swap 2rot 2rot 

\ poll until able to write the pattern colors and data 
    gx1-ge-wait-pending
    bits/pixel 8 > if 
        gx1-ge-pat-data3! gx1-ge-pat-data2! \ pattern data_3/2
    else 2drop then 
    gx1-ge-pat-data1!   gx1-ge-pat-data0!    \ pattern data_1/0
    gx1-ge-pat-color1!  gx1-ge-pat-color0!   \ colour_1/0
;

\ load a single line of a 8x8 color pattern.   
: gx1-ge-color-pattern-line!  ( y pattern[] ) 
  GFXsourceFlags off         \ clear transparency flag
  300  GFXpatternFlags !     \ RM_PAT_COLOR
  swap 7 and 
  bits/pixel 8 > if 2 else 1 then lshift  +
\ poll until able to write the pattern colors and data
   gx1-ge-wait-pending
   dup        gx1-ge-pat-data0!
   cell+ dup  gx1-ge-pat-data1!
   bits/pixel 8 > if 
      cell+ dup  gx1-ge-pat-data2!
      cell+      gx1-ge-pat-data3!
   else  drop  then
;

\ set raster operation pattern flags
: gx1-ge-raster-operation! ( rop -- )
\ set flag indicating rop requires destination data 
    55 over and over 1 rshift 55 and xor GFXusesDstData !
    
\ generate 16-bit version of rop with pattern flags 
    dup GFXpatternFlags @ or swap
    33 over and over 2 rshift 33 and xor
    if GFXsourceFlags @ or then
    dup GFXsavedRop ! 
    gx1-ge-wait-pending
    gx1-ge-rop! 
    drop
;

\ This routine MUST be used when performing a solid rectangle fill with 
\ the ROPs of PATCOPY (0xF0), BLACKNESS (0x00), WHITENESS (0xFF), or 
\ PATINVERT (0x0F).  There is a bug in GXm for these cases that requires a 
\ workaround.  
\ 
\ For BLACKNESS (ROP = 0x00), set the color to 0x0000.  
\ For WHITENESS (ROP = 0xFF), set the color to 0xFFFF.
\ For PATINVERT (ROP = 0x0F), invert the desired color.
\ 
\       X               screen X position (left)
\       Y               screen Y position (top)
\       WIDTH           width of rectangle, in pixels
\       HEIGHT          height of rectangle, in scanlines
\       COLOR           fill color
\ ---------------------------------------------------------------------------
: (gx1-ge-solid-fill) ( x y width height color)
    gx1-ge-wait-pending
    
    \ SET REGISTERS TO DRAW RECTANGLE 
    gx1-ge-pat-color0!       \ pattern colour-0
    2over
    gx1-ge-dst-y! gx1-ge-dst-x! gx1-ge-height!
    0f0 gx1-ge-rop! 
    
    \  ** HARDWARE BUG FIX  two passes for large areas
    ( x y width)  
    dup  10 > 0= if 
        \  OK TO DRAW RECTANGLE IN ONE PASS
        gx1-ge-width! 0 gx1-ge-blt! 
        2drop
    else
        \ first part of rectangle up to a 16 pixel boundary
        10 2over drop 0f and -  \ ( x y w s )
        dup gx1-ge-width! 0 gx1-ge-blt!
        
        \ LOAD THE SECOND part RECTANGLE 
        gx1-ge-wait-pending
        tuck - gx1-ge-width!
        rot +  gx1-ge-dst-x! gx1-ge-dst-y!
        0 gx1-ge-blt!    \ start blt operation
    then
;

\ fill a rectangular region with pattern loaded using one of pattern routines.
\ The raster operation must be specified.
\   X               screen X position (left)
\   Y               screen Y position (top)
\   WIDTH           width of rectangle, in pixels
\   HEIGHT          height of rectangle, in scanlines
\ -------------------------------------------------------------------------
: gx1-ge-pattern-fill ( x y width height)  

    \ check if optimized solid cases 
    \ Check all 16 bits of the ROP to include solid pattern flags.
    GFXsavedRop @ case
        000 of 4dup 0 (gx1-ge-solid-fill) endof
        00f of 4dup GFXsavedColor @ invert (gx1-ge-solid-fill) endof
        0f0 of 4dup GFXsavedColor @ (gx1-ge-solid-fill) endof
        0ff of 4dup 0ffff (gx1-ge-solid-fill) endof
        ( default )    \ destination data needed
    
        \ determine blt mode value 
        \ set source expansion mode 
        \ If the ROP requires source data, then the source data is all 1's 
        \ and then expanded into the desired color in GP_SRC_COLOR_1. 
        40                               \ set BM_SOURCE_EXPAND
        GFXusesDstData @ if 10 or then   \ set BM_READ_DST_FB0  
        \ poll until able to write to the registers 
        \ Write the registers that do not change for each section. 

        gx1-ge-wait-pending
        swap gx1-ge-height!

        \ since only destination data, we can use both bb0 and bb1.
        \ Therefore, width available = BLT buffer width * 2. 
        \ repeat until finished with rectangle 
        \ Perform BLT in vertical sections, as wide as the BLT buffer
        \ allows.  Hardware does not split the operations, so 
        \ software must do it to avoid large scanlines that would 
        \ overflow the BLT buffers.
        GFXbufferWidthPixels @ 2* gx1-ge-buffer-width !
        ( x y w m )
        begin   over 0> while
            \ determine width of section 
            over gx1-ge-buffer-width @ min 
            ( x y w m s)
            \ poll until able to write to the registers 
            gx1-ge-wait-pending
            dup >r gx1-ge-width!
            2over  gx1-ge-dst-y! gx1-ge-dst-x!
            dup   gx1-ge-blt!       \ start blt operation
            swap r@ - swap          \ width -= section
            2swap swap r> + swap 2swap \ x +=section 
            ( x y w m)               
        repeat
    endcase
    2drop 2drop
;    

\  render a rectangle using the current raster operation and
\  the specified color pattern. 
\      X               screen X position (left)
\      Y               screen Y position (top)
\      WIDTH           width of rectangle, in pixels
\      HEIGHT          height of rectangle, in scanlines
\      *PATTERN     pointer to 8x8 color pattern data
: gx1-ge-color-pattern-fill ( x y width height pattern)
    \ SET INCREMENT
    bits/pixel 8 > if 2 else 1 then gx1-ge-bpp-shift !

    \ SET SOURCE EXPANSION and DESTINATION REQUIRED
    40 GFXusesDstData @ if  8 or then \ BM_SOURCE_EXPAND |[BM_READ_DST_FB0] 
    gx1-ge-blt-mode !

    \ OVERRIDE RASTER MODE TO FORCE A COLOR PATTERN 
    gx1-ge-wait-pending
    GFXsavedRop @ 0fffffbff and gx1-ge-rop!
    
    \ WRITE THE REGISTERS THAT DO NOT CHANGE
    1      gx1-ge-height!
    2 pick gx1-ge-width!
    4 pick gx1-ge-dst-x!

    \ SINCE ONLY DESTINATION DATA, WE CAN USE BOTH BB0 AND BB1. */
    GFXbufferWidthPixels @ 2* gx1-ge-buffer-width !
    1 pick 8 min 0 do            \  height should be no more than 8

        \ SET APPROPRIATE INCREMENT
        3 pick i + dup  7 and gx1-ge-bpp-shift @ lshift
        ( cur_y pat_y)
        \ WRITE THE PATTERN DATA FOR THE ACTIVE LINE
        gx1-ge-wait-pending
        2 pick +                    \ pattern data 
        dup  @ gx1-ge-pat-data0! 
        cell+ dup @ gx1-ge-pat-data1!
        bits/pixel 8 > if 
            cell+ dup @ gx1-ge-pat-data2!
            cell+     @ gx1-ge-pat-data3!
        else drop then
        ( cur_y)
        \ SPLIT BLT LINE INTO SECTIONS IF REQUIRED
        \ If no destination data is required, we can ignore
        \ the BLT buffers.  Otherwise, we must separate the BLT
        \ so as not to overflow the buffers
        gx1-ge-blt-mode @ 8 and if         \ BM_READ_DST_BB0 
            5 pick 4 pick                   ( cur_y cur_x lw)
            begin dup while                 \    line_width
                gx1-ge-buffer-width @ over min gx1-ge-section !      
                rot drop 5 pick i + -rot        \ cur_y = y+i
                gx1-ge-wait-pending
                over gx1-ge-dst-x!
                gx1-ge-section @ gx1-ge-width!
                begin
                2 pick 5 pick 8 pick + < while  \ cur_y < y + height
                    gx1-ge-wait-pending
                    2 pick gx1-ge-dst-y!
                    gx1-ge-blt-mode @ gx1-ge-blt!
                    rot 8 + -rot
                repeat
                gx1-ge-section @ tuck +       \ cur_x section +
                -rot + swap                 \ line_width section +
            repeat
            2drop
        else  ( cur_y)
            begin 2 pick 5 pick + over > while    \ cur_y < y + height
                gx1-ge-wait-pending
                dup gx1-ge-dst-y!
                gx1-ge-blt-mode @ gx1-ge-blt!
                8 +
            repeat
        then
        drop
    loop
    \ RESTORE ORIGINAL ROP AND FLAGS */
    gx1-ge-wait-pending
    GFXsavedRop @ gx1-ge-rop!

    \ drop parameters
    2drop 2drop drop
;

\ factor of screen to screen BLT.
\ Perform BLT in vertical sections, as wide as the BLT buffer allows. 
\ Hardware does not split the operations, so software must do it to  
\ avoid large scanlines that would overflow the BLT buffers. 
\      SRCX            screen X position to copy from
\      SRCY            screen Y position to copy from
\      DSTX            screen X position to copy to
\      DSTY            screen Y position to copy to
\      WIDTH           width of rectangle, in pixels
: (blt-rectangle) ( srcx srcy dstx dsty width --) 
    begin dup while
        \ CHECK WIDTH OF CURRENT SECTION 
        gx1-ge-buffer-width @ over min gx1-ge-section !

        \ PROGRAM REGISTERS THAT ARE THE SAME FOR EITHER X DIRECTION
        gx1-ge-wait-pending
        gx1-ge-section @ gx1-ge-width!
        3 pick gx1-ge-src-y!
        over gx1-ge-dst-y!

        \ CHECK X DIRECTION 
        gx1-ge-section @ dup >r
        3 pick 6 pick > if     ( dstx > srcx )
            \ NEGATIVE X DIRECTION
            2rot swap r@ - swap
            2rot swap r> - swap 2rot
            3 pick gx1-ge-dst-x!
            5 pick gx1-ge-src-x! 
            gx1-ge-blt-mode @ gx1-ge-blt! 
        else
            \ POSITIVE X DIRECTION 
            3 pick gx1-ge-dst-x!
            5 pick gx1-ge-src-x! 
            gx1-ge-blt-mode @ gx1-ge-blt! 
             
            2rot swap r@ + swap
            2rot swap r> + swap 2rot
        then
        -
    repeat
2drop 2drop drop
;

\ screen to screen BLT when the ROP does not require destination data.
\      SRCX            screen X position to copy from
\      SRCY            screen Y position to copy from
\      DSTX            screen X position to copy to
\      DSTY            screen Y position to copy to
\      WIDTH           width of rectangle, in pixels
\      HEIGHT          height of rectangle, in scanlines
\ ----------------------------------------------------------------------------
: gx1-ge-screen-to-screen-blt ( srcx srcy dstx dsty width height --)

\ CHECK IF RASTER OPERATION REQUIRES DESTINATION DATA
    1 GFXusesDstData @ if h# 14 or then \ BM_READ_DST_FB1  BM_READ_SRC_FB
    gx1-ge-blt-mode !
\ CHECK Y DIRECTION

    2 pick 5 pick > if   \ dsty > srcy
        dup 1- >r
        2rot r@ + 2rot r> + 2rot
        h# 100 gx1-ge-blt-mode or!     \ BM_REVERSE_Y
    then

    \ CHECK X DIRECTION
    \ Hardware does not support negative X direction since at the time 
    \ of development all supported resolutions could fit a scanline of 
    \ data at once into the BLT buffers (using both BB0 and BB1).  This 
    \ code is more generic to allow for any size BLT buffer. 

    3 pick 6 pick > if          \ dstx > srcx
        over >r
        2rot swap r@ + swap
        2rot swap r> + swap
        2rot
    then

    \ POLL UNTIL ABLE TO WRITE TO THE REGISTERS
    \ Write the registers that do not change for each section. 

    gx1-ge-wait-pending
    gx1-ge-height!

    \ CHECK AVAILABLE BLT BUFFER SIZE 
    \ Can use both BLT buffers if no destination data is required. 

    GFXbufferWidthPixels @ GFXusesDstData @ 0= if 2* then gx1-ge-buffer-width !
    (blt-rectangle)
;

\ perform a screen to screen BLT when a specified color should by transparent.
\ The only supported ROP is SRCCOPY.
\ 
\      SRCX            screen X position to copy from
\      SRCY            screen Y position to copy from
\      DSTX            screen X position to copy to
\      DSTY            screen Y position to copy to
\      WIDTH           width of rectangle, in pixels
\      HEIGHT          height of rectangle, in scanlines
\      COLOR           transparent color
\ ----------------------------------------------------------------------------
: gx1-ge-screen_to_screen_xblt ( srcx srcy dstx dsty width height color)
    1 gx1-ge-blt-mode !      \ BM_READ_SRC_FB
    >r                      \ save color    
    \ CHECK Y DIRECTION
    \ Hardware has support for negative Y direction.

    2 pick 2 pick  > if     \ dsty > srcy
       dup 1- >r
        2rot r@ + 2rot r> + 2rot
        h# 100 gx1-ge-blt-mode or!   \ BM_REVERSE_Y
    then
    \ CHECK X DIRECTION 
    \ Hardware does not support negative X direction since at the time 
    \ of development all supported resolutions could fit a scanline of 
    \ data at once into the BLT buffers (using both BB0 and BB1).  This 
    \ code is more generic to allow for any size BLT buffer. 

    3 pick 6 pick > if          \ dstx > srcx
        over >r
        2rot swap r@ + swap
        2rot swap r> + swap
        2rot
    then
    r>

    \ CALCULATE BLT BUFFER SIZE
    \ Need to use BB1 to store the BLT buffer data.

    GFXbufferWidthPixels @ gx1-ge-buffer-width !

    \ WRITE TRANSPARENCY COLOR TO BLT BUFFER 1
    gx1-ge-8bpp 
    dup h# ffff and swap d# 16 lshift and

    \ WAIT UNTIL PIPELINE IS NOT BUSY BEFORE LOADING DATA INTO BB1 
    \ Need to make sure any previous BLT using BB1 is complete. 
    \ Only need to load 32 bits of BB1 for the 1 pixel BLT that follows. 
    
    gx1-ge-wait-busy
    GFXbb1Base @  gx1-ge-scratch!

    \ DO BOGUS BLT TO LATCH DATA FROM BB1 
    \ Already know graphics pipeline is idle.
    \ Only need to latch data into the holding registers for the current 
    \ data from BB1.  A 1 pixel wide BLT will suffice. 

    0 0 gx1-gpr!
    11 4 gx1-gpr!
    0 8 gx1-gpr!
    h# 0cc gx1-ge-rop!
    h# 0d  gx1-ge-blt!
    
    \ WRITE REGISTERS FOR REAL SCREEN TO SCREEN BLT

    gx1-ge-wait-pending
    gx1-ge-height!
    h# 10c6  gx1-ge-rop!
    h# ffffffff gx1-ge-pat-color0!   \ GP_PAT_COLOR_0
    
    (blt-rectangle)
;

\ common factor of color bitmap data to screen transfers .
\      SRCX            X offset within source bitmap
\      SRCY            Y offset within source bitmap
\      DSTX            screen X position to render data
\      DSTY            screen Y position to render data
\      WIDTH           width of rectangle, in pixels
\      HEIGHT          height of rectangle, in scanlines
\      *DATA           pointer to bitmap data
\      PITCH           pitch of bitmap data (bytes between scanlines)
: (bitmap-to-screen-blt)  ( srcx srcy dstx dsty width height *data pitch)

    bits/pixel  7 + 2/ 2/ gx1-ge-bpp-shift !

    begin 3 pick dup while     \ width > 0 --w
        gx1-ge-buffer-width @ min dup gx1-ge-section !

        gx1-ge-bpp-shift @ lshift >r   \ ( R:len )
        2 pick gx1-ge-temp-height !

        \ WRITE THE REGISTERS FOR EACH SECTION 
        \ The GX hardware will auto-increment the Y coordinate, meaning 
        \ that we don't have to. 

        gx1-ge-section @ gx1-ge-width!
        5 pick  gx1-ge-dst-x!
        4 pick  gx1-ge-dst-y!

        \ CALCULATE THE BITMAP OFFSET
        6 pick over * 8 pick gx1-ge-bpp-shift @ lshift + 
        gx1-ge-offset !
        swap
        begin  gx1-ge-temp-height -1 over +! @ while \ temp_height--
            gx1-ge-wait-pipeline

            \ WRITE ALL DATA TO THE BLT BUFFERS
            dup gx1-ge-offset @ r@ gx1-ge-scratch-str! 
            gx1-ge-blt-mode @ gx1-ge-blt!
            over gx1-ge-offset +!
        repeat
        r> drop swap
        2>r gx1-ge-section @ >r 
        swap r@ - swap          \  width -= section
        2rot swap r@ + swap     \  srcx  += section
        2rot swap r> + swap     \  dstx  += section
        2rot 2r>
    repeat drop
    2drop 2drop 2drop 2drop
;


\ transfers color bitmap data to the screen.  For most cases,
\ when the ROP is SRCCOPY, it may be faster to write a separate routine that
\ copies the data to the frame buffer directly.  This routine should be 
\ used when the ROP requires destination data.
\ Transparency is handled by another routine.
\      SRCX            X offset within source bitmap
\      SRCY            Y offset within source bitmap
\      DSTX            screen X position to render data
\      DSTY            screen Y position to render data
\      WIDTH           width of rectangle, in pixels
\      HEIGHT          height of rectangle, in scanlines
\      *DATA           pointer to bitmap data
\      PITCH           pitch of bitmap data (bytes between scanlines)
\ ----------------------------------------------------------------------------
: gx1-ge-color-bitmap-to-screen-blt ( srcx srcy dstx dsty width height *data pitch)

    \ CHECK SIZE OF BLT BUFFER
    
    GFXbufferWidthPixels @ gx1-ge-buffer-width !
    2 gx1-ge-blt-mode !

    \ CHECK IF RASTER OPERATION REQUIRES DESTINATION DATA
    \ If no destination data, we have twice the room for
    \ source data.

    GFXusesDstData @ if
        14 gx1-ge-blt-mode or!     \  BM_READ_DST_FB1
    else
        gx1-ge-buffer-width @ 2* gx1-ge-buffer-width ! 
    then 
    
    \ SET THE SCRATCHPAD BASE

    GFXbb0Base @  gx1-ge-scratch-base! 

    \ POLL UNTIL ABLE TO WRITE TO THE REGISTERS  
    \ Write the registers that do not change for each section. 

    gx1-ge-wait-pending
    1 gx1-ge-height!        \ GP_HEIGHT
    (bitmap-to-screen-blt) 
;    

\ transfers color bitmap data to the screen with transparency.
\ The transparent color is specified.  The only supported ROP is SRCCOPY, 
\ meaning that transparency cannot be applied if the ROP requires 
\ destination data (this is a hardware restriction).
\      SRCX            X offset within source bitmap
\      SRCY            Y offset within source bitmap
\      DSTX            screen X position to render data
\      DSTY            screen Y position to render data
\      WIDTH           width of rectangle, in pixels
\      HEIGHT          height of rectangle, in scanlines
\      *DATA           pointer to bitmap data
\      PITCH           pitch of bitmap data (bytes between scanlines)
\      COLOR           transparent color
\ ----------------------------------------------------------------------------
: gx1-ge-color-bitmap-to-screen-xblt ( srcx srcy dstx dsty width height *data pitch color)

    \ CHECK SIZE OF BLT BUFFER 

    GFXbufferWidthPixels @ gx1-ge-buffer-width !
    
    \ WRITE TRANSPARENCY COLOR TO BLT BUFFER 1

    gx1-ge-8bpp
    h# ffff and dup d# 16 lshift or 

    \ WAIT UNTIL PIPELINE IS NOT BUSY BEFORE LOADING DATA INTO BB1
    \ Need to make sure any previous BLT using BB1 is complete. 
    \ Only need to load 32 bits of BB1 for the 1 pixel BLT that follows.
    
    gx1-ge-wait-pipeline gx1-ge-wait-pending
    GFXbb1Base @ gx1-ge-scratch!

    \ DO BOGUS BLT TO LATCH DATA FROM BB1
    \ Already know graphics pipeline is idle.
    \ Only need to latch data into the holding registers for the current
    \ data from BB1.  A 1 pixel wide BLT will suffice.

    0  0  gx1-gpr!          \ GP_DST_XCOOR
    11 04 gx1-gpr!          \ GP_WIDTH
    0  8  gx1-gpr!          \ GP_SRC_XCOOR
    h# cc gx1-ge-rop! h# 0d gx1-ge-blt!

    \ POLL UNTIL ABLE TO WRITE TO THE REGISTERS 
    \ Write the registers that do not change for each section. 

    gx1-ge-wait-pending 
    1  gx1-ge-height!            \ GP_HEIGHT
    h# 10c6 gx1-ge-rop!
    h# ffffffff gx1-ge-pat-color0!     \ GP_PAT_COLOR_0

    \ SET THE SCRATCHPAD BASE

    GFXbb0Base @ gx1-ge-scratch-base!
    (bitmap-to-screen-blt) 
;    

\ transfers monochrome bitmap data to the screen.  
\     SRCX            X offset within source bitmap
\     SRCY            Y offset within source bitmap
\     DSTX            screen X position to render data
\     DSTY            screen Y position to render data
\     WIDTH           width of rectangle, in pixels
\     HEIGHT          height of rectangle, in scanlines
\     *DATA           pointer to bitmap data
\     PITCH           pitch of bitmap data (bytes between scanlines)
\ ----------------------------------------------------------------------------
: gx1-ge-mono-bitmap-to-screen-blt ( srcx srcy dstx dsty width height *data pitch) 
    42 gx1-ge-blt-mode !       \ BM_READ_SRC_BB0  BM_SOURCE_EXPAND

    \ CHECK IF RASTER OPERATION REQUIRES DESTINATION DATA
    \ If no destination data, the source data will always fit.
    \ So, in that event we will set the buffer width to a
    \ fictitiously large value such that the BLT is never split

    GFXusesDstData @ if 
        14 gx1-ge-blt-mode or!
        GFXbufferWidthPixels @
    else  d# 3200 then gx1-ge-buffer-width !

    \ CHECK IF DATA ALREADY IN BLT BUFFER. NULL indicates data already there
    \ WARNING: This could cause problems if destination data is
    \ involved and it overflows the BLT buffer.  Need to remove 
    \ this option and change the drivers to use a temporary buffer.

    over 0= if
        gx1-ge-wait-pending
        2drop
        gx1-ge-height! gx1-ge-width! 
        gx1-ge-dst-y! gx1-ge-dst-x!
        drop                \ src-y
        7 and gx1-ge-src-x!
        gx1-ge-blt-mode @ gx1-ge-blt!      \ start blt operation
        exit
    then

    \ SET THE SCRATCHPAD BASE 
    GFXbb0Base @ gx1-ge-scratch-base!

    \ POLL UNTIL ABLE TO WRITE TO THE REGISTERS
    \ Write the registers that do not change for each section.
    gx1-ge-wait-pending
    01 gx1-ge-height!       \ height
    begin 3 pick dup 0> while       \ width > 0
        gx1-ge-buffer-width @ min gx1-ge-section !
        
        \ CALCULATE BYTES NEEDED. Add 1 for possible alignment issues.
        \ src-x 7 and section + 7 + 3 rshift

        7 pick 7 and gx1-ge-section @ + 7 + 3 lshift >r  ( R: len)
        2 pick gx1-ge-temp-height !
            
        \ WRITE THE REGISTERS FOR EACH SECTION srcy auto-incremented by hardware
        gx1-ge-section @ gx1-ge-width!
        4 pick gx1-ge-dst-y!
        5 pick gx1-ge-dst-x!
        7 pick 7 and gx1-ge-src-x!

        \ CALCULATE THE BITMAP OFFSET
        6 pick over * 8 pick 3 rshift +  gx1-ge-offset !   \ array offset  
        begin gx1-ge-temp-height -1 over +! @ while \ temp-height--
            gx1-ge-wait-pipeline
            \ WRITE ALL DATA TO THE BLT BUFFERS 
            over gx1-ge-offset @ r@ gx1-ge-scratch-str! 
            gx1-ge-blt-mode @ gx1-ge-blt!    \ start blit operation
            dup gx1-ge-offset +!            \ offset += pitch
        repeat
        r> drop
        2>r gx1-ge-section @ >r
        2rot swap r@ + swap
        2rot swap r@ + swap
        2rot swap r> - swap
        r> drop 2r>    
    repeat drop
    2drop 2drop 2drop 2drop
;    

\ Monochrome text blt: transfer contiguous monochrome text data to the screen.  
\    DSTX            screen X position to render data
\    DSTY            screen Y position to render data
\    WIDTH           width of rectangle, in pixels
\    HEIGHT          height of rectangle, in scanlines
\    *DATA           pointer to bitmap data

: gx1-ge-text-blt ( dstx dsty width height data)

    \ calculate data size 
    2 pick 7 + 3 rshift     \ ( -- pitch)
    2 pick over *           \ ( -- len )

    \ This routine renders a source copy text glyph.
    \ If destination data is required or the source data will not fit
    \ use the more versatile (and slow) mono bitmap routine.
    GFXbufferWidthPixels @ bits/pixel 8 > if 2* then     \ required buffer size
    over < GFXusesDstData @ or if           \ ( pitch len)
        drop 2>r 0 0 2rot 2rot 2r>
        gx1-ge-mono-bitmap-to-screen-blt
        exit
    then

    \ SET THE SCRATCHPAD BASE
    GFXbb0Base @ gx1-ge-scratch-base!  
    nip 
    ( x y w h d l )    
    gx1-ge-wait-pending
    2swap gx1-ge-height! gx1-ge-width!
    2swap gx1-ge-dst-y!  gx1-ge-dst-x!
    ( d l )
    0 tuck gx1-ge-src-x! 
    gx1-ge-wait-pipeline

    gx1-ge-scratch-str!     \ write data to blt buffers
    h# 0c2 gx1-ge-blt!      \ start blt operation READ_SRC_BB0|SOURCE_TEXT 
;


\ draw a vector using the specified Bresenham parameters.  
\      X               screen X position to start vector
\      Y               screen Y position to start vector
\      LENGTH          length of the vector, in pixels
\      INITERR         Bresenham initial error term
\      AXIALERR        Bresenham axial error term
\      DIAGERR         Bresenham diagonal error term
\      FLAGS           VM_YMAJOR, VM_MAJOR_INC, VM_MINOR_INC

: gx1-ge-bresenham-line ( x y len initerr axialerr diagerr flags)
   GFXusesDstData @ if 8 or then  \ VM_READ_DST_FB 

\ check null length 
    over 0= if 2drop 2drop 2drop drop exit then
    >r
    gx1-ge-wait-pending
     gx1-ge-src-x!       \ diagonal error increment
     gx1-ge-src-y!       \ axial error increment
     gx1-ge-height!     \ vector initial error increment
     gx1-ge-width!      \ length
     gx1-ge-dst-y!       \ y
     gx1-ge-dst-x!       \ x
     r>
     gx1-ge-vect!       \ vector mode
;

\ initialize 2D graphics engine

: init-gaccel ( -- ) 
    gx1-ge-bpp 
;

d# 4096 init-scratchpad     \  set the scratchpad size to 4kB. and sets the
                            \ base register sizes to h# 640

init-controller     
init-gaccel
init-controller
gx1-fb-comp-enable


base !
only forth definitions

\ @@FILE:nvram.fth
\ @@REQUIRES:util.fth
\ nvram access words

base @ hex

: (port) ( addr -- addr ) 80 and if 72 else 70 then ;

internal

\ send command 
: (nvram-cmd) ( offset -- data-port )  dup (port) tuck pc! 1+ ;

: (in-update?) ( -- flg )  0a (nvram-cmd) pc@ 80 and 0<> ;

: nvram@ ( offset -- c) (nvram-cmd) pc@ ;  

external

: nvram! ( val offset -- ) (nvram-cmd) pc! ;

: bcd>bin ( c - n)
  dup 4 rshift 
  dup dup 3 lshift + +
  swap 0f and +
;

: bin>bcd ( n - c)  d# 10 /mod 4 lshift or ;

: rtc-date ( ---- s m h d m y )
  begin (in-update?) 0= until \ wait for end of update
  00 nvram@ bcd>bin
  02 nvram@ bcd>bin
  04 nvram@ bcd>bin
  07 nvram@
  08 nvram@
  09 nvram@
;
  

module

base !


\ this is freeware, copyright gordon charlton, 12th of september 1994.
\ copy and distribute it. use it. don't mess with this file. acknowledge
\ its use. i make no guarentees as to its fitness for any purpose. tell
\ me about any bugs. tell me how much you like it.

\               an ans heap

\ this is an implementation of the ans forth memory-allocation word set.
\ this is an ans standard program that has the following environmental
\ dependency - two's complement arithmetic.  it requires four words
\ from the core extension:   0> nip tuck \

\ (if you go to the trouble of checking these claims, please e-mail me
\ with your findings; gordon@charlton.demon.co.uk)

\ there are five broad areas that the program covers;

\      1, general purpose extensions to the forth system.

\      2, creation of the heap and associated use of the data space.

\      3, allocation of space from the heap.

\      4, releasing space back to the heap.

\      5, altering the size of allocated heap space.


\ the ans word set consists of three words, allocate, free, and resize
\ which give the minimum functionality required to use the heap. these are
\ given in areas 3, 4 and 5 respectively.

\ the heap is maintained as a doubly linked ordered circular list of nodes
\ with an additional field noting the size of each node and whether it is in
\ use. the size of the heap is specified by the constant heapsize. the
\ constant hysteresis controls the amount of spare space that is added to
\ an allocation, to reduce the need for block moves during resizing.

\ initially there is only one node, the size of the heap. aditional nodes
\ are created by dividing an existing node into two parts. nodes are removed
\ by marking as free, and merging with adjoining free nodes. nodes are
\ altered in size by merging with a following free node, if possible, and a
\ node being created above the new size of the node, if needed, or by
\ allocating a new node and block moving the data field if necessary.

\ finding an available node is done by sequential search and comparison. the
\ first node to be found that is large enough is used for allocation. each
\ search starts from the node most recently allocated, making this a
\ "nextfit" algorithm. the redundancy in the head fields is required to
\ optimise the search loop, as is the use of a sentinel to terminate the
\ search once every node has been looked at, by always succeeding. a final
\ refinement is the use of the sign bit of the size field to mark "in-use"
\ nodes so that they are disregarded without a separate test.

\ modified for my camel forth
\  modules, heap built at top of RAM
\  TODO add heap expansion

base @ decimal
only forth also definitions 
\ **1** general purpose extensions

: unique (  )  variable ;     internal
\
\ defining word. each child returns a different non-zero number. the
\ standard introduces the need for unique identifiers in the form of iors
\ and throw codes, but provides no means for generating them. this does
\ the trick.

: k ( n--n)  1024 * ;
\
\ a convenient way of referring to large numbers. multiplies a number by
\ 1024.

0 1 2 um/mod nip 1- constant maxpos
\
\ the largest positive single length integer.


\ **2** heap creation

\ ansi heap  ---  constants

2048 k cells constant heapsize
\
\ number of address units of data space that the heap occupies.

4 cells 1- constant hysteresis
\
\ node lengths are rounded up according to the value of hysteresis to
\ reduce the number of block moves during resize operations. the value of
\ this constant must be one less than a power of two and at least equal to
\ one less than the size of a cell.

unique allocationerror
\
\ indicates there is less contiguous heap space available than required.

3 cells constant headsize
\
\ a node on the heap consists of a three cell head followed by a variable
\ length data space. the first cell in the head points to the next node in
\ the heap. the second cell indicates the size of the node, and the third
\ points to the previous node. the second cell is negated to indicate the
\ node is in use. the heap consists of a doubly linked circular list. there
\ is no special notation to indicate an empty list, as this situation
\ cannot occur.

: adjustsize ( n--n)  headsize +  hysteresis or  1+ ;
\
\ the amount of space that is requested for a node needs adjusting to
\ include the length of the head, and to incorporate the hysteresis.

0 adjustsize constant overhead
\
\ the size of the smallest possible node.


\ ansi heap  ---  structure

create sentinel  here cell+ ,   maxpos ,  0 ,  0 ,
\
\ a dummy node used to speed up searching the heap. the search, which is
\ for a node larger than or equal to the specified size will always succeed.
\ the cell that points to the next node is set up so that the there is a zero
\ three cells ahead of where it points, where the pointer to the previous
\ node (ie the sentinel) should be. this is a special value that indicates the
\ search has failed.

\ \\\\\\\\\ create heap  heapsize allot

tom heapsize - to tom

tom 1+ constant  heap
\
\ the heap is as described in headsize.

variable nextnode
\
\ searching is done using a "nextfit" algorithm. nextnode points to the
\ most recently allocated node to indicate where the next search is to
\ start from.

: >size ( addr--addr)  cell+ ;
\
\ move from the "next" cell in the node head to the "size" cell. within the
\ word set nodes are referred to by the address of the "next" cell.
\ externally they are referred to by the address of the start of the data
\ field.

: >prev ( addr--addr)  2 cells + ;
\
\ move from the "next" cell to the "previous" cell.

: init-heap (  )  heap dup nextnode !
          dup dup !
          dup heapsize  over >size !
          >prev ! ;
\
\ initially the heap contains only one node, which is the same size as the
\ heap. both the "next" cell and the "previous" cell point to the "next"
\ cell, as does nextnode.

init-heap

\ **3** heap allocation

\ ansi heap  ---  list searching

: attach ( addr)  >prev @
          dup sentinel rot !
          sentinel >prev ! ;
\
\ the sentinel is joined into the nodelist. the "next" field of the node
\ preceding the one specified (addr) is set to point to the sentinel, and
\ the "prev" field of the sentinel to point to the node that points to the
\ sentinel.

: search  ( addr size--addr|0)
      >r begin 2@ swap r@ < invert until
      r> drop  >prev @ ;
\
\ search the nodelist, starting at the node specified (addr), for a free
\ node larger than or equal to the specified size. return the address of the
\ first node that matches, or zero for no match. the heap structure is set up
\ to make this a near optimal search loop. the "size" field is next to the "next"
\ field so that both can be collected in a single operation (2@). nodes in
\ use have negated sizes so they never match the search. the "previous"
\ field is included to allow the search to overshoot the match by one node
\ and then link back outside the loop, rather than remembering the address
\ of the node just examined. the sentinel removes the need for a separate
\ test for failure. search assumes the sentinel is in place.

: detach ( addr)  dup >prev @ ! ;
\
\ remake the link from the node prior to the one specified to the one
\ specified. this will remove the sentinel if it is attached here. (it will
\ be.)

: findspace ( size--addr|0)  nextnode @
                 dup      attach
                 dup rot  search
                 swap     detach ;
\
\ search the nodelist for a node larger or equal to that specified. return
\ the address of a suitable node, or zero if none found. the search starts at
\ the node pointed to by nextnode, the sentinal temporarily attached, the
\ search proceeded with and the sentinel detached.


\ ansi heap  ---  head creation

: fits ( size addr--flag)  >size @ swap -  overhead  < ;
\
\ returns true if the size of the node specified is the same as the
\ specified size, or larger than it by less than the size of the smallest
\ possible node. returns false otherwise.

: togglesize ( addr)  >size dup @  negate swap ! ;
\
\ negate the contents of the "size" field of the specified node. if the
\ node was available it is marked as in use, and vice versa.

: next! ( addr)  nextnode ! ;
\
\ make the specified node the starting node for future searches of the node
\ list.

: sizes! ( size addr--addr)  2dup + >r
                 >size 2dup @ swap -
                 r@ >size !
                 swap negate swap !  r> ;
\
\ given a free node (addr), reduce its size to that specified and mark it
\ as in use. start to construct a new node within the specified node beyond
\ its new length, by storing the length of the remainder of the node in the
\ size field of the new node. return the address of the partially
\ constructed node.

: links! ( addr1 addr2)  2dup swap @  2dup  swap !  >prev !
                      2dup >prev !   swap ! ;

\
\ addr1 is an existing node. addr2 is the address of a new node just above
\ the existing node. break the links from the existing node to the next
\ node and from the next node to the existing node and join the new node to
\ them.


\ ansi heap  ---  node construction allocate

: newnode ( size addr)  tuck sizes!  links! ;
\
\ given a free node at addr split it into an in-use node of the specified
\ size and a new free node above the in-use node.

: makenode ( size addr)  2dup fits if  togglesize drop
                 else  newnode
                 then ;
\
\ given a free node at addr make an in-use node of the specified size
\ and free the remainder, if there is any usable space left.

: allocate ( u--addr ior)
      dup 0< if  allocationerror
           else  adjustsize
             dup findspace
             dup if  dup next!
                 tuck makenode
                 headsize +  0
               else  drop allocationerror
               then
           then ;
\
\ make an in-use node with a data field at least u address units long.
\ return the address of the data field and an ior of 0 to indicate success.
\ if the space is not available return any old number and an ior equal to the
\ constant allocationerror. the standard specifies that the argument to
\ allocate is unsigned. as the implementation uses the sign bit of the size
\ field for its own purposes any request for an amount of space greater
\ than maxpos must fail. as this would be a request for half the
\ addressable memory or more this is not unreasonable.

\ **4** releasing space

\ ansi heap  ---  head destruction

: mergesizes ( addr addr)
         >size @ swap >size +! ;
\
\ make the size field of the node at addr1 equal to the sum of the sizes of
\ the two specified nodes. in usage the node at addr2 will be the one
\ immediately above addr1.

: mergelinks ( addr addr)
         @ 2dup swap !
           >prev ! ;
\
\ the node at addr2 is removed from the node list. as with mergesizes the
\ node at addr2 will be immediately above that at addr1. destroy the link
\ from node1 to node2 and relink node1 to the node above node2. destroy the
\ backward link from the node above node2 and relink it to node1.

: jiggle (  )
     nextnode @ @  >prev @  next! ;
\
\ there is a possibility when a node is removed from the node list that
\ nextnode may point to it. this is cured by making it point to the node
\ prior to the one removed. we do not want to alter the pointer if it does
\ not point to the removed node as that could be detrimental to the
\ efficiency of the nextfit search algorithm. rather than testing for this
\ condition we jiggle the pointer about a bit to settle it into a linked
\ node. this is done for reasons of programmer amusement. specifically
\ nextnode is set to point to the node pointed to by the "previous" field
\ of the node pointed to in the "next" field of the node pointed to by
\ nextnode. ordinarily this is a no-op (ie i am my father's son) but when
\ the node has had its links merged it sets nextnode to point to the node
\ prior to the node it pointed to (ie when i died my father adopted my son,
\ so now my son is my father's son).

: merge ( addr)
    dup @ 2dup mergesizes
           mergelinks  jiggle ;
\
\ combine the node specified with the node above it. merge the sizes, merge
\ the lengths and jiggle.


\ ansi heap  ---  node removal      free

: ?merge ( addr1 addr2)  >size @
             0> if  dup dup @
                u< if  dup merge
                   then
                then  drop ;
\
\ merge the node at addr1 with the one above it on two conditions, firstly
\ that the node at addr2 is free, and secondly that the node pointed to by
\ the next field in addr1 is actually above addr1 (ie that it does not wrap
\ around because it is the topmost node). in usage addr2 will be either
\ addr1 or the node above it. in each instance the other affected node
\ (either the node above addr1 or addr1) is known to be free, so no test is
\ needed for this.

: ?mergenext ( addr)  dup @ ?merge ;
\
\ merge the node following the specified node with the specified node, if
\ following node is free.

: ?mergeprev ( addr)  >prev @ dup ?merge ;
\
\ merge the specified node with the one preceding it, if the preceding node
\ is free.

: free ( addr--ior)  headsize -
             dup togglesize
             dup ?mergenext
             ?mergeprev  0 ;
\
\ mark the specified in-use word as free, and merge with any adjacent free
\ space. as this is a standard word addr is the address of the data field
\ rather than the "next" field. as there is no compelling reason for this
\ to fail the ior is zero.


\ **5** resizing allocated space

\ ansi heap  ---  node repairing

variable stash
\
\ the resize algorithm is simplified and made faster by assuming that it
\ will always succeed. stash holds the minimum information required to make
\ good when it fails.

: savelink ( addr)  @ stash ! ;
\
\ saves the contents of the >next field of the node being resized in stash
\ (above).

: restorelink ( addr)  stash @  swap ! ;
\
\ converse operation to savelink (above).

: fixprev ( addr)  dup >prev @ ! ;
\
\ the >next field of the node prior to the node being resized should point
\ to the node being resized. it may very well do already, but this makes
\ sure.

: fixnext ( addr)  dup @ >prev ! ;
\
\ the >prev field of the node after the node resized may need correcting.
\ this corrects it whether it needs it or not. (its quicker just to do it
\ than to check first.)

: fixlinks ( addr)  dup fixprev  dup fixnext  @ fixnext ;
\
\ resize may very well merge its argument node with the previous one. it
\ may very well merge that with the next one. this means we need to fix the
\ previous one, the next one and the one after next. to extend the metaphor
\ started in the description of jiggle (above), not only did i die, but my
\ father did too. this brings my grandfather into the picture as guardian
\ of my son. now to confound things we have all come back to life. i still
\ remember who my son is, and my father remembers who his father is. once i
\ know who my father is i can tell my son that i am his father, i can tell
\ my father that i am his son and my grandfather who his son is. thankfully
\ we are only concerned about the male lineage here! (in fact nodes
\ reproduce by division, like amoebae, which is where the metaphor breaks
\ down -- (1) they are sexless and (2) which half is parent and which
\ child?)

: fixsize ( addr)  dup >size @ 0>
           if  dup @  2dup <
               if  over - swap >size !
             else 2drop
             then
         else  drop
         then ;
\
\ reconstruct the size field of a node from the address of the head and the
\ contents of the >next field provided that the node is free and it is not
\ the topmost node in the heap (ie there is no wraparound). both these
\ conditions need to be true for the node to have been merged with its
\ successor.

: fixsizes ( addr)  dup fixsize  >prev @ fixsize ;
\
\ the two nodes whose size fields may need repairing are the one passed as
\ an argument to resize (damaged by ?mergenext) and its predecessor
\ (damaged by ?mergeprev).

: repair ( addr)  dup restorelink
          dup fixlinks  dup fixsizes
          togglesize ;
\
\ make good the damage done by resize. restore the >next field, fix the
\ links, fix the size fields and mark the node as in-use. note that this
\ may not restore the system to exactly how it was. in particular the pointer
\ nextnode may have moved back one or two nodes by virtue of having been
\ jiggled about if it happened to be pointing to the wrong node. this is not
\ serious, so i have chosen to ignore it.


\ ansi heap  ---  node movement

: toobig? ( addr size--flag)
      swap  >size @  > ;
\
\ flag is true if the node at addr is smaller than the specified size.

: copynode ( addr1 addr2)
       over >size @  headsize -
       rot  headsize + rot rot move ;
\
\ move the contents of the data field of the node at addr1 to the data
\ field at addr2. assumes addr2 is large enough. it will be.

: enlarge ( addr1 size--addr2 ior)
      over  ?mergeprev
      allocate dup >r
      if  swap repair
    else  tuck copynode
    then r> ;
\
\ make a new node of the size specified. copy the data field of addr1 to
\ the new node. merge the node at addr1 with the one preceding it, if
\ possible. this last behaviour is to finish off removing the node at
\ addr1. the word adjust (below) starts removing the node. the node is
\ removed before allocation to increase the probability of allocate
\ succeeding. the address returned by enlarge is that returned by allocate,
\ which is that of the data field, not the head. if the allocation fails
\ repair the damage done by removing the node at addr1.


\ ansi heap  ---  node restructuring    resize

: adjust ( addr1 size1--addr2 size2)  adjustsize >r
                      headsize -
                      dup savelink
                      dup togglesize
                      dup ?mergenext r> ;
\
\ addr1 points to the data field of a node, not the "next" field. this
\ needs correcting. size1 also needs adjusting as per adjustsize. in
\ addition it is easier to work with free nodes than live ones as the size
\ field is correct, and, as we intend to change the nodes size we will
\ inevitably want to muck about with the next node, if its free, so lets
\ merge with it straight away. sufficient information is first saved to put
\ the heap back as it was, if necessary. now we are ready to get down to
\ business.

: allocate ( u -- a ior )
   allocate  ;   external

: free ( a -- ior )
   free ;

: resize ( addr1 u--addr2 ior)
     dup 0< if  drop allocationerror
          else  adjust  2dup
            toobig?  if  enlarge
               else  over makenode
                 headsize +  0
               then
          then ;
module

base !
\
\ resize the node at addr1 to the specified size. return the address of the
\ resized node (addr2) along with an ior of zero if successful and
\ allocationerror if not. addr2 may be the same as, or different to, addr1.
\ if ior is non-zero then addr2 is not meaningful. being a standard word
\ the arguments need adjusting to the internal representation on entry, and
\ back again on exit. if after the first merge the requested size is still
\ too large to reuse the specified node then it is moved to a larger node
\ and the specified node released. if, on the other hand the request is not
\ too big for the node, then we remake the node at the right length, and
\ free any space at the top using makenode, which has just the right
\ functionality. in this case the ior is zero. as this is a standard word it
\ takes an unsigned size argument, but excessive requests fail
\ automatically, as with allocate.
\ @@FILE:font8x16.fth
\ fonts from corebot- libpayload
\ 20170205 - support for other font sizes
base @ hex

only forth also
system definitions

100 value #font-glyphs
10  value /font-height
8   value /font-width
80  value /font-size
0   value font-array

create font8x16 
\  0 h# 00 '^@' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  1 h# 01 '^A' 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 81 c, \  10000001 
 a5 c, \  10100101 
 81 c, \  10000001 
 81 c, \  10000001 
 bd c, \  10111101 
 99 c, \  10011001 
 81 c, \  10000001 
 81 c, \  10000001 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  2 h# 02 '^B' 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 ff c, \  11111111 
 db c, \  11011011 
 ff c, \  11111111 
 ff c, \  11111111 
 c3 c, \  11000011 
 e7 c, \  11100111 
 ff c, \  11111111 
 ff c, \  11111111 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  3 h# 03 '^C' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 6c c, \  01101100 
 fe c, \  11111110 
 fe c, \  11111110 
 fe c, \  11111110 
 fe c, \  11111110 
 7c c, \  01111100 
 38 c, \  00111000 
 10 c, \  00010000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  4 h# 04 '^D' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 10 c, \  00010000 
 38 c, \  00111000 
 7c c, \  01111100 
 fe c, \  11111110 
 7c c, \  01111100 
 38 c, \  00111000 
 10 c, \  00010000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  5 h# 05 '^E' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 3c c, \  00111100 
 3c c, \  00111100 
 e7 c, \  11100111 
 e7 c, \  11100111 
 e7 c, \  11100111 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  6 h# 06 '^F' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 3c c, \  00111100 
 7e c, \  01111110 
 ff c, \  11111111 
 ff c, \  11111111 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  7 h# 07 '^G' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 3c c, \  00111100 
 3c c, \  00111100 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  8 h# 08 '^H' 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 e7 c, \  11100111 
 c3 c, \  11000011 
 c3 c, \  11000011 
 e7 c, \  11100111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 

\  9 h# 09 '^I' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 66 c, \  01100110 
 42 c, \  01000010 
 42 c, \  01000010 
 66 c, \  01100110 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  10 h# 0a '^J' 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 c3 c, \  11000011 
 99 c, \  10011001 
 bd c, \  10111101 
 bd c, \  10111101 
 99 c, \  10011001 
 c3 c, \  11000011 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 

\  11 h# 0b '^K' 
 00 c, \  00000000 
 00 c, \  00000000 
 1e c, \  00011110 
 0e c, \  00001110 
 1a c, \  00011010 
 32 c, \  00110010 
 78 c, \  01111000 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 78 c, \  01111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  12 h# 0c '^L' 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 3c c, \  00111100 
 18 c, \  00011000 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  13 h# 0d '^M' 
 00 c, \  00000000 
 00 c, \  00000000 
 3f c, \  00111111 
 33 c, \  00110011 
 3f c, \  00111111 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 70 c, \  01110000 
 f0 c, \  11110000 
 e0 c, \  11100000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  14 h# 0e '^N' 
 00 c, \  00000000 
 00 c, \  00000000 
 7f c, \  01111111 
 63 c, \  01100011 
 7f c, \  01111111 
 63 c, \  01100011 
 63 c, \  01100011 
 63 c, \  01100011 
 63 c, \  01100011 
 67 c, \  01100111 
 e7 c, \  11100111 
 e6 c, \  11100110 
 c0 c, \  11000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  15 h# 0f '^O' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 db c, \  11011011 
 3c c, \  00111100 
 e7 c, \  11100111 
 3c c, \  00111100 
 db c, \  11011011 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  16 h# 10 '^P' 
 00 c, \  00000000 
 80 c, \  10000000 
 c0 c, \  11000000 
 e0 c, \  11100000 
 f0 c, \  11110000 
 f8 c, \  11111000 
 fe c, \  11111110 
 f8 c, \  11111000 
 f0 c, \  11110000 
 e0 c, \  11100000 
 c0 c, \  11000000 
 80 c, \  10000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  17 h# 11 '^Q' 
 00 c, \  00000000 
 02 c, \  00000010 
 06 c, \  00000110 
 0e c, \  00001110 
 1e c, \  00011110 
 3e c, \  00111110 
 fe c, \  11111110 
 3e c, \  00111110 
 1e c, \  00011110 
 0e c, \  00001110 
 06 c, \  00000110 
 02 c, \  00000010 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  18 h# 12 '^R' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 3c c, \  00111100 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 7e c, \  01111110 
 3c c, \  00111100 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  19 h# 13 '^S' 
 00 c, \  00000000 
 00 c, \  00000000 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 00 c, \  00000000 
 66 c, \  01100110 
 66 c, \  01100110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  20 h# 14 '^T' 
 00 c, \  00000000 
 00 c, \  00000000 
 7f c, \  01111111 
 db c, \  11011011 
 db c, \  11011011 
 db c, \  11011011 
 7b c, \  01111011 
 1b c, \  00011011 
 1b c, \  00011011 
 1b c, \  00011011 
 1b c, \  00011011 
 1b c, \  00011011 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  21 h# 15 '^U' 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 60 c, \  01100000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 6c c, \  01101100 
 38 c, \  00111000 
 0c c, \  00001100 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  22 h# 16 '^V' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 fe c, \  11111110 
 fe c, \  11111110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  23 h# 17 '^W' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 3c c, \  00111100 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 7e c, \  01111110 
 3c c, \  00111100 
 18 c, \  00011000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  24 h# 18 '^X' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 3c c, \  00111100 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  25 h# 19 '^Y' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 7e c, \  01111110 
 3c c, \  00111100 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  26 h# 1a '^Z' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 0c c, \  00001100 
 fe c, \  11111110 
 0c c, \  00001100 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  27 h# 1b '^[' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 30 c, \  00110000 
 60 c, \  01100000 
 fe c, \  11111110 
 60 c, \  01100000 
 30 c, \  00110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  28 h# 1c '^\' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  29 h# 1d '^]' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 28 c, \  00101000 
 6c c, \  01101100 
 fe c, \  11111110 
 6c c, \  01101100 
 28 c, \  00101000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  30 h# 1e '^^' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 10 c, \  00010000 
 38 c, \  00111000 
 38 c, \  00111000 
 7c c, \  01111100 
 7c c, \  01111100 
 fe c, \  11111110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  31 h# 1f '^_' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 fe c, \  11111110 
 7c c, \  01111100 
 7c c, \  01111100 
 38 c, \  00111000 
 38 c, \  00111000 
 10 c, \  00010000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  32 h# 20 ' ' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

\  33 h# 21 '!' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 3c c, \  00111100 
 3c c, \  00111100 
 3c c, \  00111100 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  34 h# 22 '"' 
 00 c, \  00000000 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 24 c, \  00100100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  35 h# 23 '#' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 6c c, \  01101100 
 6c c, \  01101100 
 fe c, \  11111110 
 6c c, \  01101100 
 6c c, \  01101100 
 6c c, \  01101100 
 fe c, \  11111110 
 6c c, \  01101100 
 6c c, \  01101100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  36 h# 24 '$' 
 18 c, \  00011000 
 18 c, \  00011000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c2 c, \  11000010 
 c0 c, \  11000000 
 7c c, \  01111100 
 06 c, \  00000110 
 06 c, \  00000110 
 86 c, \  10000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  37 h# 25 '%' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 c2 c, \  11000010 
 c6 c, \  11000110 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 c6 c, \  11000110 
 86 c, \  10000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  38 h# 26 '&' 
 00 c, \  00000000 
 00 c, \  00000000 
 38 c, \  00111000 
 6c c, \  01101100 
 6c c, \  01101100 
 38 c, \  00111000 
 76 c, \  01110110 
 dc c, \  11011100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  39 h# 27 ''' 
 00 c, \  00000000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 60 c, \  01100000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  40 h# 28 '(' 
 00 c, \  00000000 
 00 c, \  00000000 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 18 c, \  00011000 
 0c c, \  00001100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  41 h# 29 ')' 
 00 c, \  00000000 
 00 c, \  00000000 
 30 c, \  00110000 
 18 c, \  00011000 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  42 h# 2a '*' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 66 c, \  01100110 
 3c c, \  00111100 
 ff c, \  11111111 
 3c c, \  00111100 
 66 c, \  01100110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  43 h# 2b '+' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  44 h# 2c ',' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 30 c, \  00110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  45 h# 2d '-' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  46 h# 2e '.' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  47 h# 2f '/' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 02 c, \  00000010 
 06 c, \  00000110 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 c0 c, \  11000000 
 80 c, \  10000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  48 h# 30 '0' 
 00 c, \  00000000 
 00 c, \  00000000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 d6 c, \  11010110 
 d6 c, \  11010110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 6c c, \  01101100 
 38 c, \  00111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  49 h# 31 '1' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 38 c, \  00111000 
 78 c, \  01111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  50 h# 32 '2' 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 06 c, \  00000110 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  51 h# 33 '3' 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 06 c, \  00000110 
 06 c, \  00000110 
 3c c, \  00111100 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  52 h# 34 '4' 
 00 c, \  00000000 
 00 c, \  00000000 
 0c c, \  00001100 
 1c c, \  00011100 
 3c c, \  00111100 
 6c c, \  01101100 
 cc c, \  11001100 
 fe c, \  11111110 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 1e c, \  00011110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  53 h# 35 '5' 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 fc c, \  11111100 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  54 h# 36 '6' 
 00 c, \  00000000 
 00 c, \  00000000 
 38 c, \  00111000 
 60 c, \  01100000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 fc c, \  11111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  55 h# 37 '7' 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 c6 c, \  11000110 
 06 c, \  00000110 
 06 c, \  00000110 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  56 h# 38 '8' 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  57 h# 39 '9' 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7e c, \  01111110 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 0c c, \  00001100 
 78 c, \  01111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  58 h# 3a ':' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  59 h# 3b ';' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 30 c, \  00110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  60 h# 3c '<' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 06 c, \  00000110 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 0c c, \  00001100 
 06 c, \  00000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  61 h# 3d '=' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  62 h# 3e '>' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 0c c, \  00001100 
 06 c, \  00000110 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  63 h# 3f '?' 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 0c c, \  00001100 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  64 h# 40 '@' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 de c, \  11011110 
 de c, \  11011110 
 de c, \  11011110 
 dc c, \  11011100 
 c0 c, \  11000000 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  65 h# 41 'A' 
 00 c, \  00000000 
 00 c, \  00000000 
 10 c, \  00010000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 fe c, \  11111110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  66 h# 42 'B' 
 00 c, \  00000000 
 00 c, \  00000000 
 fc c, \  11111100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 7c c, \  01111100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 fc c, \  11111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  67 h# 43 'C' 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 66 c, \  01100110 
 c2 c, \  11000010 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c2 c, \  11000010 
 66 c, \  01100110 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  68 h# 44 'D' 
 00 c, \  00000000 
 00 c, \  00000000 
 f8 c, \  11111000 
 6c c, \  01101100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 6c c, \  01101100 
 f8 c, \  11111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  69 h# 45 'E' 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 66 c, \  01100110 
 62 c, \  01100010 
 68 c, \  01101000 
 78 c, \  01111000 
 68 c, \  01101000 
 60 c, \  01100000 
 62 c, \  01100010 
 66 c, \  01100110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  70 h# 46 'F' 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 66 c, \  01100110 
 62 c, \  01100010 
 68 c, \  01101000 
 78 c, \  01111000 
 68 c, \  01101000 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 f0 c, \  11110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  71 h# 47 'G' 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 66 c, \  01100110 
 c2 c, \  11000010 
 c0 c, \  11000000 
 c0 c, \  11000000 
 de c, \  11011110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 66 c, \  01100110 
 3a c, \  00111010 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  72 h# 48 'H' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 fe c, \  11111110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  73 h# 49 'I' 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  74 h# 4a 'J' 
 00 c, \  00000000 
 00 c, \  00000000 
 1e c, \  00011110 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 78 c, \  01111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  75 h# 4b 'K' 
 00 c, \  00000000 
 00 c, \  00000000 
 e6 c, \  11100110 
 66 c, \  01100110 
 66 c, \  01100110 
 6c c, \  01101100 
 78 c, \  01111000 
 78 c, \  01111000 
 6c c, \  01101100 
 66 c, \  01100110 
 66 c, \  01100110 
 e6 c, \  11100110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  76 h# 4c 'L' 
 00 c, \  00000000 
 00 c, \  00000000 
 f0 c, \  11110000 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 62 c, \  01100010 
 66 c, \  01100110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  77 h# 4d 'M' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 ee c, \  11101110 
 fe c, \  11111110 
 fe c, \  11111110 
 d6 c, \  11010110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  78 h# 4e 'N' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 e6 c, \  11100110 
 f6 c, \  11110110 
 fe c, \  11111110 
 de c, \  11011110 
 ce c, \  11001110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  79 h# 4f 'O' 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  80 h# 50 'P' 
 00 c, \  00000000 
 00 c, \  00000000 
 fc c, \  11111100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 7c c, \  01111100 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 f0 c, \  11110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  81 h# 51 'Q' 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 d6 c, \  11010110 
 de c, \  11011110 
 7c c, \  01111100 
 0c c, \  00001100 
 0e c, \  00001110 
 00 c, \  00000000 
 00 c, \  00000000 

    \  82 h# 52 'R' 
 00 c, \  00000000 
 00 c, \  00000000 
 fc c, \  11111100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 7c c, \  01111100 
 6c c, \  01101100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 e6 c, \  11100110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  83 h# 53 'S' 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 60 c, \  01100000 
 38 c, \  00111000 
 0c c, \  00001100 
 06 c, \  00000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  84 h# 54 'T' 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 7e c, \  01111110 
 5a c, \  01011010 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  85 h# 55 'U' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  86 h# 56 'V' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 6c c, \  01101100 
 38 c, \  00111000 
 10 c, \  00010000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  87 h# 57 'W' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 d6 c, \  11010110 
 d6 c, \  11010110 
 d6 c, \  11010110 
 fe c, \  11111110 
 ee c, \  11101110 
 6c c, \  01101100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  88 h# 58 'X' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 6c c, \  01101100 
 7c c, \  01111100 
 38 c, \  00111000 
 38 c, \  00111000 
 7c c, \  01111100 
 6c c, \  01101100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  89 h# 59 'Y' 
 00 c, \  00000000 
 00 c, \  00000000 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 3c c, \  00111100 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  90 h# 5a 'Z' 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 c6 c, \  11000110 
 86 c, \  10000110 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 c2 c, \  11000010 
 c6 c, \  11000110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  91 h# 5b '[' 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  92 h# 5c '\' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 80 c, \  10000000 
 c0 c, \  11000000 
 e0 c, \  11100000 
 70 c, \  01110000 
 38 c, \  00111000 
 1c c, \  00011100 
 0e c, \  00001110 
 06 c, \  00000110 
 02 c, \  00000010 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  93 h# 5d ']' 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  94 h# 5e '^' 
 10 c, \  00010000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  95 h# 5f '_' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ff c, \  11111111 
 00 c, \  00000000 
 00 c, \  00000000 

    \  96 h# 60 '`' 
 00 c, \  00000000 
 30 c, \  00110000 
 18 c, \  00011000 
 0c c, \  00001100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  97 h# 61 'a' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 78 c, \  01111000 
 0c c, \  00001100 
 7c c, \  01111100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  98 h# 62 'b' 
 00 c, \  00000000 
 00 c, \  00000000 
 e0 c, \  11100000 
 60 c, \  01100000 
 60 c, \  01100000 
 78 c, \  01111000 
 6c c, \  01101100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  99 h# 63 'c' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  100 h# 64 'd' 
 00 c, \  00000000 
 00 c, \  00000000 
 1c c, \  00011100 
 0c c, \  00001100 
 0c c, \  00001100 
 3c c, \  00111100 
 6c c, \  01101100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  101 h# 65 'e' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 fe c, \  11111110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  102 h# 66 'f' 
 00 c, \  00000000 
 00 c, \  00000000 
 1c c, \  00011100 
 36 c, \  00110110 
 32 c, \  00110010 
 30 c, \  00110000 
 78 c, \  01111000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 78 c, \  01111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  103 h# 67 'g' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 76 c, \  01110110 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 7c c, \  01111100 
 0c c, \  00001100 
 cc c, \  11001100 
 78 c, \  01111000 
 00 c, \  00000000 

    \  104 h# 68 'h' 
 00 c, \  00000000 
 00 c, \  00000000 
 e0 c, \  11100000 
 60 c, \  01100000 
 60 c, \  01100000 
 6c c, \  01101100 
 76 c, \  01110110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 e6 c, \  11100110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  105 h# 69 'i' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 38 c, \  00111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  106 h# 6a 'j' 
 00 c, \  00000000 
 00 c, \  00000000 
 06 c, \  00000110 
 06 c, \  00000110 
 00 c, \  00000000 
 0e c, \  00001110 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 66 c, \  01100110 
 66 c, \  01100110 
 3c c, \  00111100 
 00 c, \  00000000 

    \  107 h# 6b 'k' 
 00 c, \  00000000 
 00 c, \  00000000 
 e0 c, \  11100000 
 60 c, \  01100000 
 60 c, \  01100000 
 66 c, \  01100110 
 6c c, \  01101100 
 78 c, \  01111000 
 78 c, \  01111000 
 6c c, \  01101100 
 66 c, \  01100110 
 e6 c, \  11100110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  108 h# 6c 'l' 
 00 c, \  00000000 
 00 c, \  00000000 
 38 c, \  00111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  109 h# 6d 'm' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ec c, \  11101100 
 fe c, \  11111110 
 d6 c, \  11010110 
 d6 c, \  11010110 
 d6 c, \  11010110 
 d6 c, \  11010110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  110 h# 6e 'n' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 dc c, \  11011100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  111 h# 6f 'o' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  112 h# 70 'p' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 dc c, \  11011100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 7c c, \  01111100 
 60 c, \  01100000 
 60 c, \  01100000 
 f0 c, \  11110000 
 00 c, \  00000000 

    \  113 h# 71 'q' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 76 c, \  01110110 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 7c c, \  01111100 
 0c c, \  00001100 
 0c c, \  00001100 
 1e c, \  00011110 
 00 c, \  00000000 

    \  114 h# 72 'r' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 dc c, \  11011100 
 76 c, \  01110110 
 66 c, \  01100110 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 f0 c, \  11110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  115 h# 73 's' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 60 c, \  01100000 
 38 c, \  00111000 
 0c c, \  00001100 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  116 h# 74 't' 
 00 c, \  00000000 
 00 c, \  00000000 
 10 c, \  00010000 
 30 c, \  00110000 
 30 c, \  00110000 
 fc c, \  11111100 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 30 c, \  00110000 
 36 c, \  00110110 
 1c c, \  00011100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  117 h# 75 'u' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  118 h# 76 'v' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 6c c, \  01101100 
 38 c, \  00111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  119 h# 77 'w' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 d6 c, \  11010110 
 d6 c, \  11010110 
 d6 c, \  11010110 
 fe c, \  11111110 
 6c c, \  01101100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  120 h# 78 'x' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 6c c, \  01101100 
 38 c, \  00111000 
 38 c, \  00111000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  121 h# 79 'y' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7e c, \  01111110 
 06 c, \  00000110 
 0c c, \  00001100 
 f8 c, \  11111000 
 00 c, \  00000000 

    \  122 h# 7a 'z' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 cc c, \  11001100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 c6 c, \  11000110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  123 h# 7b '{' 
 00 c, \  00000000 
 00 c, \  00000000 
 0e c, \  00001110 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 70 c, \  01110000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 0e c, \  00001110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  124 h# 7c '|' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  125 h# 7d '}' 
 00 c, \  00000000 
 00 c, \  00000000 
 70 c, \  01110000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 0e c, \  00001110 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 70 c, \  01110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  126 h# 7e '~' 
 00 c, \  00000000 
 76 c, \  01110110 
 dc c, \  11011100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  127 h# 7f '' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 10 c, \  00010000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  128 h# 80 '€' 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 66 c, \  01100110 
 c2 c, \  11000010 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c2 c, \  11000010 
 66 c, \  01100110 
 3c c, \  00111100 
 18 c, \  00011000 
 70 c, \  01110000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  129 h# 81 '' 
 00 c, \  00000000 
 00 c, \  00000000 
 cc c, \  11001100 
 00 c, \  00000000 
 00 c, \  00000000 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  130 h# 82 '‚' 
 00 c, \  00000000 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 fe c, \  11111110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  131 h# 83 'ƒ' 
 00 c, \  00000000 
 10 c, \  00010000 
 38 c, \  00111000 
 6c c, \  01101100 
 00 c, \  00000000 
 78 c, \  01111000 
 0c c, \  00001100 
 7c c, \  01111100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  132 h# 84 '„' 
 00 c, \  00000000 
 00 c, \  00000000 
 cc c, \  11001100 
 00 c, \  00000000 
 00 c, \  00000000 
 78 c, \  01111000 
 0c c, \  00001100 
 7c c, \  01111100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  133 h# 85 '…' 
 00 c, \  00000000 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 00 c, \  00000000 
 78 c, \  01111000 
 0c c, \  00001100 
 7c c, \  01111100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  134 h# 86 '†' 
 00 c, \  00000000 
 38 c, \  00111000 
 6c c, \  01101100 
 38 c, \  00111000 
 00 c, \  00000000 
 78 c, \  01111000 
 0c c, \  00001100 
 7c c, \  01111100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  135 h# 87 '‡' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 7c c, \  01111100 
 18 c, \  00011000 
 70 c, \  01110000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  136 h# 88 'ˆ' 
 00 c, \  00000000 
 10 c, \  00010000 
 38 c, \  00111000 
 6c c, \  01101100 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 fe c, \  11111110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  137 h# 89 '‰' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 fe c, \  11111110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  138 h# 8a 'Š' 
 00 c, \  00000000 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 fe c, \  11111110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  139 h# 8b '‹' 
 00 c, \  00000000 
 00 c, \  00000000 
 66 c, \  01100110 
 00 c, \  00000000 
 00 c, \  00000000 
 38 c, \  00111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  140 h# 8c 'Œ' 
 00 c, \  00000000 
 18 c, \  00011000 
 3c c, \  00111100 
 66 c, \  01100110 
 00 c, \  00000000 
 38 c, \  00111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  141 h# 8d '' 
 00 c, \  00000000 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 00 c, \  00000000 
 38 c, \  00111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  142 h# 8e 'Ž' 
 00 c, \  00000000 
 c6 c, \  11000110 
 00 c, \  00000000 
 10 c, \  00010000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 fe c, \  11111110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  143 h# 8f '' 
 38 c, \  00111000 
 6c c, \  01101100 
 38 c, \  00111000 
 10 c, \  00010000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 fe c, \  11111110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  144 h# 90 '' 
 0c c, \  00001100 
 18 c, \  00011000 
 00 c, \  00000000 
 fe c, \  11111110 
 66 c, \  01100110 
 62 c, \  01100010 
 68 c, \  01101000 
 78 c, \  01111000 
 68 c, \  01101000 
 62 c, \  01100010 
 66 c, \  01100110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  145 h# 91 '‘' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ec c, \  11101100 
 36 c, \  00110110 
 36 c, \  00110110 
 7e c, \  01111110 
 d8 c, \  11011000 
 d8 c, \  11011000 
 6e c, \  01101110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  146 h# 92 '’' 
 00 c, \  00000000 
 00 c, \  00000000 
 3e c, \  00111110 
 6c c, \  01101100 
 cc c, \  11001100 
 cc c, \  11001100 
 fe c, \  11111110 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 ce c, \  11001110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  147 h# 93 '“' 
 00 c, \  00000000 
 10 c, \  00010000 
 38 c, \  00111000 
 6c c, \  01101100 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  148 h# 94 '”' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  149 h# 95 '•' 
 00 c, \  00000000 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  150 h# 96 '–' 
 00 c, \  00000000 
 30 c, \  00110000 
 78 c, \  01111000 
 cc c, \  11001100 
 00 c, \  00000000 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  151 h# 97 '—' 
 00 c, \  00000000 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 00 c, \  00000000 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  152 h# 98 '˜' 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7e c, \  01111110 
 06 c, \  00000110 
 0c c, \  00001100 
 78 c, \  01111000 
 00 c, \  00000000 

    \  153 h# 99 '™' 
 00 c, \  00000000 
 c6 c, \  11000110 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  154 h# 9a 'š' 
 00 c, \  00000000 
 c6 c, \  11000110 
 00 c, \  00000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  155 h# 9b '›' 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 7c c, \  01111100 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  156 h# 9c 'œ' 
 00 c, \  00000000 
 38 c, \  00111000 
 6c c, \  01101100 
 64 c, \  01100100 
 60 c, \  01100000 
 f0 c, \  11110000 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 e6 c, \  11100110 
 fc c, \  11111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  157 h# 9d '' 
 00 c, \  00000000 
 00 c, \  00000000 
 66 c, \  01100110 
 66 c, \  01100110 
 3c c, \  00111100 
 18 c, \  00011000 
 7e c, \  01111110 
 18 c, \  00011000 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  158 h# 9e 'ž' 
 00 c, \  00000000 
 f8 c, \  11111000 
 cc c, \  11001100 
 cc c, \  11001100 
 f8 c, \  11111000 
 c4 c, \  11000100 
 cc c, \  11001100 
 de c, \  11011110 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  159 h# 9f 'Ÿ' 
 00 c, \  00000000 
 0e c, \  00001110 
 1b c, \  00011011 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 d8 c, \  11011000 
 70 c, \  01110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  160 h# a0 ' ' 
 00 c, \  00000000 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 00 c, \  00000000 
 78 c, \  01111000 
 0c c, \  00001100 
 7c c, \  01111100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  161 h# a1 '¡' 
 00 c, \  00000000 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 00 c, \  00000000 
 38 c, \  00111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  162 h# a2 '¢' 
 00 c, \  00000000 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  163 h# a3 '£' 
 00 c, \  00000000 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 00 c, \  00000000 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  164 h# a4 '¤' 
 00 c, \  00000000 
 00 c, \  00000000 
 76 c, \  01110110 
 dc c, \  11011100 
 00 c, \  00000000 
 dc c, \  11011100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  165 h# a5 '¥' 
 76 c, \  01110110 
 dc c, \  11011100 
 00 c, \  00000000 
 c6 c, \  11000110 
 e6 c, \  11100110 
 f6 c, \  11110110 
 fe c, \  11111110 
 de c, \  11011110 
 ce c, \  11001110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  166 h# a6 '¦' 
 00 c, \  00000000 
 00 c, \  00000000 
 3c c, \  00111100 
 6c c, \  01101100 
 6c c, \  01101100 
 3e c, \  00111110 
 00 c, \  00000000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  167 h# a7 '§' 
 00 c, \  00000000 
 00 c, \  00000000 
 38 c, \  00111000 
 6c c, \  01101100 
 6c c, \  01101100 
 38 c, \  00111000 
 00 c, \  00000000 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  168 h# a8 '¨' 
 00 c, \  00000000 
 00 c, \  00000000 
 30 c, \  00110000 
 30 c, \  00110000 
 00 c, \  00000000 
 30 c, \  00110000 
 30 c, \  00110000 
 60 c, \  01100000 
 c0 c, \  11000000 
 c6 c, \  11000110 
 c6 c, \  11000110 
 7c c, \  01111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  169 h# a9 '©' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  170 h# aa 'ª' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 06 c, \  00000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  171 h# ab '«' 
 00 c, \  00000000 
 60 c, \  01100000 
 e0 c, \  11100000 
 62 c, \  01100010 
 66 c, \  01100110 
 6c c, \  01101100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 dc c, \  11011100 
 86 c, \  10000110 
 0c c, \  00001100 
 18 c, \  00011000 
 3e c, \  00111110 
 00 c, \  00000000 
 00 c, \  00000000 

    \  172 h# ac '¬' 
 00 c, \  00000000 
 60 c, \  01100000 
 e0 c, \  11100000 
 62 c, \  01100010 
 66 c, \  01100110 
 6c c, \  01101100 
 18 c, \  00011000 
 30 c, \  00110000 
 66 c, \  01100110 
 ce c, \  11001110 
 9a c, \  10011010 
 3f c, \  00111111 
 06 c, \  00000110 
 06 c, \  00000110 
 00 c, \  00000000 
 00 c, \  00000000 

    \  173 h# ad '­' 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 3c c, \  00111100 
 3c c, \  00111100 
 3c c, \  00111100 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  174 h# ae '®' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 36 c, \  00110110 
 6c c, \  01101100 
 d8 c, \  11011000 
 6c c, \  01101100 
 36 c, \  00110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  175 h# af '¯' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 d8 c, \  11011000 
 6c c, \  01101100 
 36 c, \  00110110 
 6c c, \  01101100 
 d8 c, \  11011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  176 h# b0 '°' 
 11 c, \  00010001 
 44 c, \  01000100 
 11 c, \  00010001 
 44 c, \  01000100 
 11 c, \  00010001 
 44 c, \  01000100 
 11 c, \  00010001 
 44 c, \  01000100 
 11 c, \  00010001 
 44 c, \  01000100 
 11 c, \  00010001 
 44 c, \  01000100 
 11 c, \  00010001 
 44 c, \  01000100 
 11 c, \  00010001 
 44 c, \  01000100 

    \  177 h# b1 '±' 
 55 c, \  01010101 
 aa c, \  10101010 
 55 c, \  01010101 
 aa c, \  10101010 
 55 c, \  01010101 
 aa c, \  10101010 
 55 c, \  01010101 
 aa c, \  10101010 
 55 c, \  01010101 
 aa c, \  10101010 
 55 c, \  01010101 
 aa c, \  10101010 
 55 c, \  01010101 
 aa c, \  10101010 
 55 c, \  01010101 
 aa c, \  10101010 

    \  178 h# b2 '²' 
 dd c, \  11011101 
 77 c, \  01110111 
 dd c, \  11011101 
 77 c, \  01110111 
 dd c, \  11011101 
 77 c, \  01110111 
 dd c, \  11011101 
 77 c, \  01110111 
 dd c, \  11011101 
 77 c, \  01110111 
 dd c, \  11011101 
 77 c, \  01110111 
 dd c, \  11011101 
 77 c, \  01110111 
 dd c, \  11011101 
 77 c, \  01110111 

    \  179 h# b3 '³' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  180 h# b4 '´' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 f8 c, \  11111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  181 h# b5 'µ' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 f8 c, \  11111000 
 18 c, \  00011000 
 f8 c, \  11111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  182 h# b6 '¶' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 f6 c, \  11110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  183 h# b7 '·' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  184 h# b8 '¸' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 f8 c, \  11111000 
 18 c, \  00011000 
 f8 c, \  11111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  185 h# b9 '¹' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 f6 c, \  11110110 
 06 c, \  00000110 
 f6 c, \  11110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  186 h# ba 'º' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  187 h# bb '»' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 06 c, \  00000110 
 f6 c, \  11110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  188 h# bc '¼' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 f6 c, \  11110110 
 06 c, \  00000110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  189 h# bd '½' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  190 h# be '¾' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 f8 c, \  11111000 
 18 c, \  00011000 
 f8 c, \  11111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  191 h# bf '¿' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 f8 c, \  11111000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  192 h# c0 'À' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 1f c, \  00011111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  193 h# c1 'Á' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 ff c, \  11111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  194 h# c2 'Â' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ff c, \  11111111 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  195 h# c3 'Ã' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 1f c, \  00011111 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  196 h# c4 'Ä' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ff c, \  11111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  197 h# c5 'Å' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 ff c, \  11111111 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  198 h# c6 'Æ' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 1f c, \  00011111 
 18 c, \  00011000 
 1f c, \  00011111 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  199 h# c7 'Ç' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 37 c, \  00110111 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  200 h# c8 'È' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 37 c, \  00110111 
 30 c, \  00110000 
 3f c, \  00111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  201 h# c9 'É' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 3f c, \  00111111 
 30 c, \  00110000 
 37 c, \  00110111 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  202 h# ca 'Ê' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 f7 c, \  11110111 
 00 c, \  00000000 
 ff c, \  11111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  203 h# cb 'Ë' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ff c, \  11111111 
 00 c, \  00000000 
 f7 c, \  11110111 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  204 h# cc 'Ì' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 37 c, \  00110111 
 30 c, \  00110000 
 37 c, \  00110111 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  205 h# cd 'Í' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ff c, \  11111111 
 00 c, \  00000000 
 ff c, \  11111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  206 h# ce 'Î' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 f7 c, \  11110111 
 00 c, \  00000000 
 f7 c, \  11110111 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  207 h# cf 'Ï' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 ff c, \  11111111 
 00 c, \  00000000 
 ff c, \  11111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  208 h# d0 'Ð' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 ff c, \  11111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  209 h# d1 'Ñ' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ff c, \  11111111 
 00 c, \  00000000 
 ff c, \  11111111 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  210 h# d2 'Ò' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ff c, \  11111111 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  211 h# d3 'Ó' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 3f c, \  00111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  212 h# d4 'Ô' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 1f c, \  00011111 
 18 c, \  00011000 
 1f c, \  00011111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  213 h# d5 'Õ' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 1f c, \  00011111 
 18 c, \  00011000 
 1f c, \  00011111 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  214 h# d6 'Ö' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 3f c, \  00111111 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  215 h# d7 '×' 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 ff c, \  11111111 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 

    \  216 h# d8 'Ø' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 ff c, \  11111111 
 18 c, \  00011000 
 ff c, \  11111111 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  217 h# d9 'Ù' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 f8 c, \  11111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  218 h# da 'Ú' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 1f c, \  00011111 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  219 h# db 'Û' 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 

    \  220 h# dc 'Ü' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 

    \  221 h# dd 'Ý' 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 
 f0 c, \  11110000 

    \  222 h# de 'Þ' 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 
 0f c, \  00001111 

    \  223 h# df 'ß' 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 ff c, \  11111111 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  224 h# e0 'à' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 76 c, \  01110110 
 dc c, \  11011100 
 d8 c, \  11011000 
 d8 c, \  11011000 
 d8 c, \  11011000 
 dc c, \  11011100 
 76 c, \  01110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  225 h# e1 'á' 
 00 c, \  00000000 
 00 c, \  00000000 
 78 c, \  01111000 
 cc c, \  11001100 
 cc c, \  11001100 
 cc c, \  11001100 
 d8 c, \  11011000 
 cc c, \  11001100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 cc c, \  11001100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  226 h# e2 'â' 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 c0 c, \  11000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  227 h# e3 'ã' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 6c c, \  01101100 
 6c c, \  01101100 
 6c c, \  01101100 
 6c c, \  01101100 
 6c c, \  01101100 
 6c c, \  01101100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  228 h# e4 'ä' 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 c6 c, \  11000110 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 c6 c, \  11000110 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  229 h# e5 'å' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 d8 c, \  11011000 
 d8 c, \  11011000 
 d8 c, \  11011000 
 d8 c, \  11011000 
 d8 c, \  11011000 
 70 c, \  01110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  230 h# e6 'æ' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 7c c, \  01111100 
 60 c, \  01100000 
 60 c, \  01100000 
 c0 c, \  11000000 
 00 c, \  00000000 

    \  231 h# e7 'ç' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 76 c, \  01110110 
 dc c, \  11011100 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  232 h# e8 'è' 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 18 c, \  00011000 
 3c c, \  00111100 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 3c c, \  00111100 
 18 c, \  00011000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  233 h# e9 'é' 
 00 c, \  00000000 
 00 c, \  00000000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 fe c, \  11111110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 6c c, \  01101100 
 38 c, \  00111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  234 h# ea 'ê' 
 00 c, \  00000000 
 00 c, \  00000000 
 38 c, \  00111000 
 6c c, \  01101100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 6c c, \  01101100 
 6c c, \  01101100 
 6c c, \  01101100 
 6c c, \  01101100 
 ee c, \  11101110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  235 h# eb 'ë' 
 00 c, \  00000000 
 00 c, \  00000000 
 1e c, \  00011110 
 30 c, \  00110000 
 18 c, \  00011000 
 0c c, \  00001100 
 3e c, \  00111110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 66 c, \  01100110 
 3c c, \  00111100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  236 h# ec 'ì' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 db c, \  11011011 
 db c, \  11011011 
 db c, \  11011011 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  237 h# ed 'í' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 03 c, \  00000011 
 06 c, \  00000110 
 7e c, \  01111110 
 db c, \  11011011 
 db c, \  11011011 
 f3 c, \  11110011 
 7e c, \  01111110 
 60 c, \  01100000 
 c0 c, \  11000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  238 h# ee 'î' 
 00 c, \  00000000 
 00 c, \  00000000 
 1c c, \  00011100 
 30 c, \  00110000 
 60 c, \  01100000 
 60 c, \  01100000 
 7c c, \  01111100 
 60 c, \  01100000 
 60 c, \  01100000 
 60 c, \  01100000 
 30 c, \  00110000 
 1c c, \  00011100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  239 h# ef 'ï' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7c c, \  01111100 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 c6 c, \  11000110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  240 h# f0 'ð' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 fe c, \  11111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  241 h# f1 'ñ' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 7e c, \  01111110 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  242 h# f2 'ò' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 30 c, \  00110000 
 18 c, \  00011000 
 0c c, \  00001100 
 06 c, \  00000110 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 00 c, \  00000000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  243 h# f3 'ó' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 0c c, \  00001100 
 18 c, \  00011000 
 30 c, \  00110000 
 60 c, \  01100000 
 30 c, \  00110000 
 18 c, \  00011000 
 0c c, \  00001100 
 00 c, \  00000000 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  244 h# f4 'ô' 
 00 c, \  00000000 
 00 c, \  00000000 
 0e c, \  00001110 
 1b c, \  00011011 
 1b c, \  00011011 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 

    \  245 h# f5 'õ' 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 18 c, \  00011000 
 d8 c, \  11011000 
 d8 c, \  11011000 
 d8 c, \  11011000 
 70 c, \  01110000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  246 h# f6 'ö' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 00 c, \  00000000 
 7e c, \  01111110 
 00 c, \  00000000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  247 h# f7 '÷' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 76 c, \  01110110 
 dc c, \  11011100 
 00 c, \  00000000 
 76 c, \  01110110 
 dc c, \  11011100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  248 h# f8 'ø' 
 00 c, \  00000000 
 38 c, \  00111000 
 6c c, \  01101100 
 6c c, \  01101100 
 38 c, \  00111000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  249 h# f9 'ù' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  250 h# fa 'ú' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 18 c, \  00011000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  251 h# fb 'û' 
 00 c, \  00000000 
 0f c, \  00001111 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 0c c, \  00001100 
 ec c, \  11101100 
 6c c, \  01101100 
 6c c, \  01101100 
 3c c, \  00111100 
 1c c, \  00011100 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  252 h# fc 'ü' 
 00 c, \  00000000 
 6c c, \  01101100 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 36 c, \  00110110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  253 h# fd 'ý' 
 00 c, \  00000000 
 3c c, \  00111100 
 66 c, \  01100110 
 0c c, \  00001100 
 18 c, \  00011000 
 32 c, \  00110010 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  254 h# fe 'þ' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 7e c, \  01111110 
 7e c, \  01111110 
 7e c, \  01111110 
 7e c, \  01111110 
 7e c, \  01111110 
 7e c, \  01111110 
 7e c, \  01111110 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

    \  255 h# ff 'ÿ' 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 
 00 c, \  00000000 

font8x16 to font-array

previous definitions 

base !

base @ hex
also system definitions
\ 6  to /font-height
\ 4  to /font-width
\ 30 to /font-size
\ font4x6 to font-array
create font4x6      \ width =4 height =6
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
000 c, 000 c, 000 c, 000 c, 000 c, 000 c, 
044 c, 044 c, 044 c, 000 c, 044 c, 000 c, 
0aa c, 0aa c, 000 c, 000 c, 000 c, 000 c, 
0aa c, 0ff c, 0ff c, 0aa c, 000 c, 000 c, 
044 c, 066 c, 0ee c, 0cc c, 044 c, 000 c, 
0aa c, 022 c, 044 c, 088 c, 0aa c, 000 c, 
066 c, 099 c, 066 c, 0aa c, 0dd c, 000 c, 
022 c, 044 c, 000 c, 000 c, 000 c, 000 c, 
022 c, 044 c, 044 c, 044 c, 022 c, 000 c, 
044 c, 022 c, 022 c, 022 c, 044 c, 000 c, 
000 c, 0ee c, 0ee c, 0ee c, 000 c, 000 c, 
000 c, 044 c, 0ee c, 044 c, 000 c, 000 c, 
000 c, 000 c, 000 c, 044 c, 088 c, 000 c, 
000 c, 000 c, 0ee c, 000 c, 000 c, 000 c, 
000 c, 000 c, 000 c, 000 c, 044 c, 000 c, 
000 c, 022 c, 044 c, 088 c, 000 c, 000 c, 
044 c, 0aa c, 0aa c, 0aa c, 044 c, 000 c, 
044 c, 0cc c, 044 c, 044 c, 0ee c, 000 c, 
0cc c, 022 c, 044 c, 088 c, 0ee c, 000 c, 
0ee c, 022 c, 066 c, 022 c, 0ee c, 000 c, 
0aa c, 0aa c, 0ee c, 022 c, 022 c, 000 c, 
0ee c, 088 c, 0ee c, 022 c, 0ee c, 000 c, 
0ee c, 088 c, 0ee c, 0aa c, 0ee c, 000 c, 
0ee c, 022 c, 022 c, 022 c, 022 c, 000 c, 
0ee c, 0aa c, 0ee c, 0aa c, 0ee c, 000 c, 
0ee c, 0aa c, 0ee c, 022 c, 022 c, 000 c, 
000 c, 000 c, 044 c, 000 c, 044 c, 000 c, 
000 c, 000 c, 044 c, 000 c, 044 c, 088 c, 
022 c, 044 c, 088 c, 044 c, 022 c, 000 c, 
000 c, 0ee c, 000 c, 0ee c, 000 c, 000 c, 
088 c, 044 c, 022 c, 044 c, 088 c, 000 c, 
0ee c, 022 c, 066 c, 000 c, 044 c, 000 c, 
044 c, 0ee c, 0ee c, 088 c, 044 c, 000 c, 
044 c, 0aa c, 0ee c, 0aa c, 0aa c, 000 c, 
0cc c, 0aa c, 0cc c, 0aa c, 0cc c, 000 c, 
066 c, 088 c, 088 c, 088 c, 066 c, 000 c, 
0cc c, 0aa c, 0aa c, 0aa c, 0cc c, 000 c, 
0ee c, 088 c, 0ee c, 088 c, 0ee c, 000 c, 
0ee c, 088 c, 0ee c, 088 c, 088 c, 000 c, 
066 c, 088 c, 0ee c, 0aa c, 066 c, 000 c, 
0aa c, 0aa c, 0ee c, 0aa c, 0aa c, 000 c, 
0ee c, 044 c, 044 c, 044 c, 0ee c, 000 c, 
022 c, 022 c, 022 c, 0aa c, 044 c, 000 c, 
0aa c, 0aa c, 0cc c, 0aa c, 0aa c, 000 c, 
088 c, 088 c, 088 c, 088 c, 0ee c, 000 c, 
0aa c, 0ee c, 0ee c, 0aa c, 0aa c, 000 c, 
0aa c, 0ee c, 0ee c, 0ee c, 0aa c, 000 c, 
044 c, 0aa c, 0aa c, 0aa c, 044 c, 000 c, 
0cc c, 0aa c, 0cc c, 088 c, 088 c, 000 c, 
044 c, 0aa c, 0aa c, 0ee c, 066 c, 000 c, 
0cc c, 0aa c, 0ee c, 0cc c, 0aa c, 000 c, 
066 c, 088 c, 044 c, 022 c, 0cc c, 000 c, 
0ee c, 044 c, 044 c, 044 c, 044 c, 000 c, 
0aa c, 0aa c, 0aa c, 0aa c, 066 c, 000 c, 
0aa c, 0aa c, 0aa c, 044 c, 044 c, 000 c, 
0aa c, 0aa c, 0ee c, 0ee c, 0aa c, 000 c, 
0aa c, 0aa c, 044 c, 0aa c, 0aa c, 000 c, 
0aa c, 0aa c, 044 c, 044 c, 044 c, 000 c, 
0ee c, 022 c, 044 c, 088 c, 0ee c, 000 c, 
066 c, 044 c, 044 c, 044 c, 066 c, 000 c, 
000 c, 088 c, 044 c, 022 c, 000 c, 000 c, 
066 c, 022 c, 022 c, 022 c, 066 c, 000 c, 
044 c, 0aa c, 000 c, 000 c, 000 c, 000 c, 
000 c, 000 c, 000 c, 000 c, 000 c, 0ff c, 
088 c, 044 c, 000 c, 000 c, 000 c, 000 c, 
000 c, 000 c, 066 c, 0aa c, 0ee c, 000 c, 
088 c, 088 c, 0cc c, 0aa c, 0cc c, 000 c, 
000 c, 000 c, 066 c, 088 c, 066 c, 000 c, 
022 c, 022 c, 066 c, 0aa c, 066 c, 000 c, 
000 c, 0ee c, 0ee c, 088 c, 066 c, 000 c, 
022 c, 044 c, 0ee c, 044 c, 044 c, 000 c, 
000 c, 066 c, 0aa c, 066 c, 0ee c, 000 c, 
088 c, 088 c, 0cc c, 0aa c, 0aa c, 000 c, 
044 c, 000 c, 044 c, 044 c, 044 c, 000 c, 
044 c, 000 c, 044 c, 044 c, 088 c, 000 c, 
000 c, 088 c, 0aa c, 0cc c, 0aa c, 000 c, 
000 c, 0cc c, 044 c, 044 c, 0ee c, 000 c, 
000 c, 000 c, 0ee c, 0ee c, 0aa c, 000 c, 
000 c, 000 c, 0cc c, 0aa c, 0aa c, 000 c, 
000 c, 044 c, 0aa c, 0aa c, 044 c, 000 c, 
000 c, 000 c, 0cc c, 0aa c, 0cc c, 088 c, 
000 c, 000 c, 066 c, 0aa c, 066 c, 022 c, 
000 c, 0cc c, 0aa c, 088 c, 088 c, 000 c, 
000 c, 066 c, 0cc c, 022 c, 0cc c, 000 c, 
000 c, 044 c, 0ee c, 044 c, 044 c, 000 c, 
000 c, 000 c, 0aa c, 0aa c, 066 c, 000 c, 
000 c, 000 c, 0aa c, 0ee c, 044 c, 000 c, 
000 c, 000 c, 0aa c, 0ee c, 0ee c, 000 c, 
000 c, 000 c, 0aa c, 044 c, 0aa c, 000 c, 
000 c, 000 c, 0aa c, 0ee c, 022 c, 0cc c, 
000 c, 0ee c, 066 c, 0cc c, 0ee c, 000 c, 
022 c, 044 c, 0cc c, 044 c, 022 c, 000 c, 
044 c, 044 c, 044 c, 044 c, 044 c, 000 c, 
088 c, 044 c, 066 c, 044 c, 088 c, 000 c, 
055 c, 0aa c, 000 c, 000 c, 000 c, 000 c, 
044 c, 0aa c, 0aa c, 0ee c, 000 c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
000 c, 066 c, 0cc c, 066 c, 000 c, 000 c, 
000 c, 0cc c, 066 c, 0cc c, 000 c, 000 c, 
088 c, 022 c, 088 c, 022 c, 088 c, 022 c, 
0aa c, 055 c, 0aa c, 055 c, 0aa c, 055 c, 
0dd c, 0bb c, 0dd c, 0bb c, 0dd c, 0bb c, 
044 c, 044 c, 044 c, 044 c, 044 c, 044 c, 
044 c, 044 c, 0cc c, 044 c, 044 c, 044 c, 
044 c, 044 c, 0cc c, 0cc c, 044 c, 044 c, 
066 c, 066 c, 0ee c, 066 c, 066 c, 066 c, 
000 c, 000 c, 0ee c, 066 c, 066 c, 066 c, 
000 c, 000 c, 0cc c, 0cc c, 044 c, 044 c, 
066 c, 066 c, 0ee c, 0ee c, 066 c, 066 c, 
066 c, 066 c, 066 c, 066 c, 066 c, 066 c, 
000 c, 000 c, 0ee c, 0ee c, 066 c, 066 c, 
066 c, 066 c, 0ee c, 0ee c, 000 c, 000 c, 
066 c, 066 c, 0ee c, 000 c, 000 c, 000 c, 
044 c, 044 c, 0cc c, 0cc c, 000 c, 000 c, 
000 c, 000 c, 0cc c, 044 c, 044 c, 044 c, 
044 c, 044 c, 077 c, 000 c, 000 c, 000 c, 
044 c, 044 c, 0ff c, 000 c, 000 c, 000 c, 
000 c, 000 c, 0ff c, 044 c, 044 c, 044 c, 
044 c, 044 c, 077 c, 044 c, 044 c, 044 c, 
000 c, 000 c, 0ff c, 000 c, 000 c, 000 c, 
044 c, 044 c, 0ff c, 044 c, 044 c, 044 c, 
044 c, 044 c, 077 c, 077 c, 044 c, 044 c, 
066 c, 066 c, 077 c, 066 c, 066 c, 066 c, 
066 c, 066 c, 077 c, 077 c, 000 c, 000 c, 
000 c, 000 c, 077 c, 077 c, 066 c, 066 c, 
066 c, 066 c, 0ff c, 0ff c, 000 c, 000 c, 
000 c, 000 c, 0ff c, 0ff c, 066 c, 066 c, 
066 c, 066 c, 077 c, 077 c, 066 c, 066 c, 
000 c, 000 c, 0ff c, 0ff c, 000 c, 000 c, 
066 c, 066 c, 0ff c, 0ff c, 066 c, 066 c, 
044 c, 044 c, 0ff c, 0ff c, 000 c, 000 c, 
066 c, 066 c, 0ff c, 000 c, 000 c, 000 c, 
000 c, 000 c, 0ff c, 0ff c, 044 c, 044 c, 
000 c, 000 c, 0ff c, 066 c, 066 c, 066 c, 
066 c, 066 c, 077 c, 000 c, 000 c, 000 c, 
044 c, 044 c, 077 c, 077 c, 000 c, 000 c, 
000 c, 000 c, 077 c, 077 c, 044 c, 044 c, 
000 c, 000 c, 077 c, 066 c, 066 c, 066 c, 
066 c, 066 c, 0ff c, 066 c, 066 c, 066 c, 
044 c, 044 c, 0ff c, 0ff c, 044 c, 044 c, 
044 c, 044 c, 0cc c, 000 c, 000 c, 000 c, 
000 c, 000 c, 077 c, 044 c, 044 c, 044 c, 
0ff c, 0ff c, 0ff c, 0ff c, 0ff c, 0ff c, 
000 c, 000 c, 000 c, 0ff c, 0ff c, 0ff c, 
0cc c, 0cc c, 0cc c, 0cc c, 0cc c, 0cc c, 
033 c, 033 c, 033 c, 033 c, 033 c, 033 c, 
0ff c, 0ff c, 0ff c, 000 c, 000 c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
000 c, 000 c, 066 c, 066 c, 000 c, 000 c, 
0ee c, 0ee c, 0ee c, 0ee c, 0ee c, 000 c, 
base !
previous definitions
\ @@File: doc.fth 
\ @@REQUIRES: timer.fth,utils.fth
\ Disk on chip words 
\ doc-init ( -- )  
\ doc-probe ( addr -- chip-id )
\ doc-block@ ( buffer blk# -- ior )
\ doc-block! ( buffer blk# -- ior )
\ page and block numbers start from 0
\
\ 
\  we have a millenium chip on the TC7010
\  three areas a,b,c defined in a page
\  address format is offset in area (ad0-ad7) page (ad9-ad22)
\  ad8 is set by area select command
  
base @ hex

only forth also
system definitions

\ NAND FLASH modes 
0 constant reset#      internal
1 constant normal#
4 constant mdwren#
8 constant bdect#
10 constant rstlatch#
80 constant clrerr#

\ DOC chip ids
\ 20 constant DOC2k#
\ 21 constant DOC2kTSOP#
30 constant DOC-mil
\ 40 constant DOCmil+32
\ 41 constant DOCmil+16

\ CDSN control Write
01 constant CDSN-ctrl-ce#
02 constant CDSN-ctrl-cle#
04 constant CDSN-ctrl-ale#
08 constant CDSN-ctrl-wp#
10 constant CDSN-ctrl-flash-io#
20 constant CDSN-ctrl-ecc-io#
\  CDSN Status bits on read
40 constant CDSN-ctrl-b0#
80 constant CDSN-ctrl-b1#     \ use to getstatus 1 - ready
80 constant CDSN-ctrl-frb#    \ ready busy flag 

\ ECC ECD 
0  constant ECC-reset# 
01 constant ECC-ignore#
02 constant ECC-disable#
04 constant ECC-togglebit#
0a constant ECC-enable#
22 constant ECC-rw#


\ registers  for millenium

0800 constant CDSN-io-mil#
1000 constant chip-id# 
1001 constant DOC-status#
1002 constant DOC-control#
1003 constant floor-select#   \  ASIC control
1004 constant CDSN-control#
1005 constant CDSN-dev#
1006 constant ECC-conf#
1006 constant ECC-status#

100d constant CDSN-slow-io#
1010 constant ECC-syndrome0#
1011 constant ECC-syndrome1#
1012 constant ECC-syndrome2#
1013 constant ECC-syndrome3#
1014 constant ECC-syndrome4#
1015 constant ECC-syndrome5#
101b constant alias#
100c constant confing-input#
101d constant read-pipe-init#
101e constant write-pipe-term#
101f constant last-data-read#
1020 constant NOP#
1800 constant ipl-sram#


\ NAND chip  commands
0  constant NAND-cmd-read-a#
1  constant NAND-cmd-read-b#
50 constant NAND-cmd-read-c#     \ read extra bytes
10 constant NAND-cmd-page-prog#   \ setup write auto program
60 constant NAND-cmd-erase1#     \ setup auto block erase
70 constant NAND-cmd-status#     \ status read
80 constant NAND-cmd-seqin#      \ serial data in
90 constant NAND-cmd-read-id#    
b0 constant NAND-cmd-suspend-erase# 
d0 constant NAND-cmd-erase2#     \ confirm auto block erase
e0 constant NAND-cmd-register-read#
ff constant NAND-cmd-reset#      \ reset


98 constant NAND-mfr-toshiba#
ec constant NAND-mfr-samsung


\ "Toshiba TC58V64AFT/DC", 0xe6
10  constant doc-oob-size#
200 constant doc-page-size#
9   constant doc-page-bits#
0d  constant doc-erase-block-bits#
8   constant doc-sign-offset# 
6   constant ecc-size# 


d2000 value docaddr#

\  out of band buffer

create oob-buffer 10 allot

\ ecc buffer

create ecc-buffer 6 allot

\ syndrome buffer

create doc-syndrome 6 allot

: doc@ ( reg -- byte)
\ read DoC register
 docaddr# + c@ ; 

: doc! ( val reg -- ) 
\ Write to DoC register
 docaddr# + c! ;

: doc-delay ( U --)
\ u > 0 
0 do 
  NOP# doc@ drop
loop
;

: doc-status ( -- flg)
\ see if chip is ready 
4 doc-delay CDSN-control# doc@
CDSN-ctrl-frb# and dup 
if 2 doc-delay then
;

: (waitready) ( -- flg)
ffff begin
    1- doc-status over 0= or
( CDSN-control# doc@ CDSN-ctrl-frb# and over 0- or )
until
0= dup if ." DoCReady timeout" cr then
;

: doc-wait-ready ( -- flg )
  4 doc-delay doc-status
  0= dup if drop (waitready) then
  2 doc-delay
;

: CDSN-ctrl! ( x -- )
\ write to CDSN-control reg with delay
  CDSN-control# doc! 4 doc-delay 
;

: doc-ctrl! ( x -- )
\ write to DoC control register
  DOC-control# doc!
;

: doc-io! ( c -- )
\ write to CDSN-io register
  CDSN-io-mil# doc!
;

: doc-io@ (  -- c )
\ read from CDSN-io register
  CDSN-io-mil# doc@
;


: doc-mode! ( x -- )
  dup doc-ctrl! doc-ctrl!
;

: slow-write ( c --)
\  for millenium chips
  dup CDSN-slow-io# doc! 4 doc-delay
  doc-io!
;

: doc-seq-start ( -- )
\ start read sequential data stream
  read-pipe-init# doc@ drop
;

: doc-seq-end ( -- )
\ end sequential data stream
 0 write-pipe-term# doc!
;


: doc-cmd ( cmd -- )
\ send command with WP off
\ assert CLE
  CDSN-ctrl-ce# or CDSN-ctrl-cle# or CDSN-ctrl!
  doc-io! doc-seq-end
  CDSN-ctrl-ce# or CDSN-ctrl!
;

: doc-wp-cmd ( cmd -- )
\ send commands with write protect enabled
\ assert command latch
CDSN-ctrl-wp# CDSN-ctrl-ce# or CDSN-ctrl-cle# or CDSN-ctrl!
doc-io!  doc-seq-end
\ deassert Command latch
CDSN-ctrl-wp# CDSN-ctrl-ce# or CDSN-ctrl!
;

: doc-prog-status ( -- flg)
\ bit 0 prog/erase , pass:0 fail:1
\ bit 6 Read/Busy , Busy:0 Ready:1
\ bit 7 Write protect  protected:0 unprotected:1
  NAND-cmd-status# doc-wp-cmd
  doc-seq-start
  2 doc-delay doc-io@ 1 and
  last-data-read# doc@ drop
;


: doc-address8 ( addr xflags   -- )
\ assert ALE
  swap over
  CDSN-ctrl-ce# or CDSN-ctrl-ale# or CDSN-ctrl!
\ send address
  doc-io!  \ column address
  doc-seq-end 
\ deassert ALE
  CDSN-ctrl-ce# or CDSN-ctrl!
;

: doc-address16 ( addr xflags   -- )
\ 
  swap over
  CDSN-ctrl-ce# or CDSN-ctrl-ale# or CDSN-ctrl!
\ send address
  dup doc-io!
  9 rshift doc-io!  
  doc-seq-end
\ deassert ALE
  CDSN-ctrl-ce# or CDSN-ctrl!
;

: doc-address24 ( addr xflags   -- )
\
\ a0 - a7 column address
\ a13 - a22 block address
\ a9 - a12 NAND address in block
   swap over
   CDSN-ctrl-ce# or CDSN-ctrl-ale# or CDSN-ctrl!
\ send address
   dup doc-io!
   9 rshift dup doc-io!
\ bits 7,6 must be 0
   8 rshift 3f and doc-io!
   doc-seq-end
\ deassert ALE
   CDSN-ctrl-ce# or CDSN-ctrl!
;


: doc-select-chip ( u -- )
  CDSN-dev# doc! 4 doc-delay
  doc-wait-ready drop
;

: doc-select-floor ( u --  )
  floor-select# doc! 4 doc-delay
  doc-wait-ready drop
;

: doc-map-window ( chip floor -- )
   doc-select-floor doc-select-chip 
;

: mil-ecc-reset ( -- ) 
\ reset ECC hardware
  ECC-reset# ECC-conf# doc!
;

: mil-enable-ecc-write (   )
\ enable ecc hardware for writes
  mil-ecc-reset
  ECC-enable# ECC-rw# or ECC-conf# doc!
;

: mil-enable-ecc-read (   )
\ enable ecc hardware for reads
  mil-ecc-reset
  ECC-enable#  ECC-conf# doc!
;

: mil-ecc-flush ( -- )
  ECC-conf# dup doc@ drop doc@ drop
;
 
: mil-disable-ecc ( -- )
  mil-ecc-reset
  ECC-disable# ECC-conf# doc! 
;

: doc-c@ ( -- c)
 2 doc-delay
 read-pipe-init# doc@
 last-data-read# doc@
;

: doc-c! ( c -- )
 dup CDSN-slow-io# doc!
 dup doc-io!
 write-pipe-term# doc!
;


: doc-!buffer ( addr u -- )
\ write buffer to DoC
 0 do 
    c@+ CDSN-io-mil# i + doc!
 loop drop
 doc-seq-end
;

external 

: doc-@syndrome ( addr -- )
 6 0 do
   ECC-syndrome0# i + doc@ c!+
 loop
 drop 
;

: doc-!syndrome ( addr -- )
 6 0 do
   c@+ ECC-syndrome0# i + doc!
 loop
 drop
;

: doc-!ecc ( addr -- )
 6 0 do
   c@+ CDSN-io-mil# i + doc!
 loop
 drop
 ;


: doc-@buffer ( addr u -- )
 doc-seq-start 
  1- 0 do
    i ff and CDSN-io-mil# + doc@ c!+
 loop
 last-data-read# doc@ swap c!
;

: doc-do-write ( -- ior )
\ start flash programming
  NAND-cmd-page-prog# doc-cmd
  doc-wait-ready drop 
\ read status
  doc-prog-status
;



: doc-verify-buf ( addr u -- flg)
\ compare buffer with current DoC page 
 doc-seq-start
 true -rot 0 do 
   c@+ i ff and CDSN-io-mil# + doc@ <>
   if nip false swap leave then
 loop
 c@ last-data-read# doc@ = and
;
 


: doc-id-chip ( chip floor -- mfr id ) 
\ identifier DOC chips  
  doc-map-window  
  NAND-cmd-reset# doc-wp-cmd
  doc-wait-ready drop
  NAND-cmd-read-id# doc-wp-cmd
  0 CDSN-ctrl-wp# doc-address8
  doc-seq-start 2 doc-delay
  doc-io@ 2 doc-delay     \ mfr
  doc-io@                \ id
  last-data-read# doc@ drop
;


: doc-probe  ( win -- chipid )
\ initialize millenium chip
  docaddr#  swap to docaddr#
\ reset ASIC CLR_ERR|MDWREN|RESET
  clrerr# mdwren# or dup reset# or
  doc-mode! 
\ enable ASIC CLR_ERR|MDWREN|normal#
  normal# or doc-mode!
  chip-id# doc@  ( addr chip_id )
  swap to docaddr#   ( chip id )
 ;  


: doc-oob@ ( addr pg#--  )
\ out of band area 0x10 bytes for mill
  doc-page-bits# lshift     ( a a )
  0 0 doc-map-window  mil-disable-ecc
\ read command for area C (oob)
  NAND-cmd-read-c# doc-wp-cmd
  CDSN-ctrl-wp# doc-address24
  doc-wait-ready drop
  doc-oob-size# doc-@buffer
;

: doc-oob! ( addr pg# -- ) \ FIXME
  doc-page-bits# lshift
\ select chip and floor, 
  0 0 doc-map-window mil-disable-ecc
\ reset chip
  NAND-cmd-reset# doc-wp-cmd
\ read command for area C (oob)
  NAND-cmd-read-c# doc-wp-cmd
  NAND-cmd-seqin# doc-cmd
 \ send address24
  0 doc-address24
  doc-wait-ready drop
\ write data through CDSN-io-mil#
  doc-oob-size# doc-!buffer
\ send page program command
  doc-do-write
;


: .id ( n --)
\ print chip id
 case
    e6 of ." TC58V64AFT/DC" endof
    e5 of ." Some other" endof
    ." Unknown " 
endcase
;

: .mfr ( n --)
\ print manufacturer
 case
    93 of ."  Samsung" endof
    98 of ."  Toshiba" endof
    ." Unknown"
endcase
;

: doc-extra@ ( buf u blk# -- )
\ read u bytes from first page block to get signature
\ get address
  doc-erase-block-bits# lshift doc-sign-offset# +
  0 0 doc-map-window
\ read from area-c
  NAND-cmd-read-c# doc-wp-cmd
  CDSN-ctrl-wp# doc-address24
  doc-wait-ready drop
  doc-@buffer
;

: doc-page@ ( buf pg# -- ior )
\ page is 512 bytes
  doc-page-bits# lshift     ( buf addr ) 
  0 0 doc-map-window 
\ read from area A
  NAND-cmd-read-a# doc-wp-cmd
  CDSN-ctrl-wp# doc-address24
  doc-wait-ready drop
  mil-enable-ecc-read
  doc-page-size# doc-@buffer \ ( -- )
\ Error detection
  ecc-buffer ecc-size# doc-@buffer
  ECC-conf# doc@ drop ECC-conf# doc@ drop 
  ECC-conf# doc@ 80 and dup
  if ." ECC read error" cr
        doc-syndrome doc-@syndrome
  then        
  mil-disable-ecc
;

: doc-page! ( buffer pg# -- )
\ write 512 page to DocC
   doc-page-bits# lshift   ( a u )
   0  0 doc-map-window
\ write  to area A
  NAND-cmd-reset# doc-cmd
  doc-wait-ready drop
  NAND-cmd-read-a# doc-cmd
  NAND-cmd-seqin# doc-cmd
  0 doc-address24
  doc-wait-ready drop
  mil-enable-ecc-write   ( a )
  doc-page-size# doc-!buffer
\ write ECC data
\ flush the buffer
  0 NOP# doc!  0 NOP# doc!  0 NOP# doc!  
\ get ECC data from ECC logic  
  ecc-buffer doc-@syndrome
  mil-disable-ecc
\ save ecc data to flash
  ecc-buffer doc-!ecc
\ append info in extra area
  5A doc-io!
  A5 CDSN-io-mil# 1+ doc!
  doc-seq-end doc-do-write   ( ior)
;


: doc-erase-block ( blk# -- )
\  erase NAND block
   doc-erase-block-bits# lshift
\ select chip and floor
   0 0 doc-map-window
\ send erase1 command
   NAND-cmd-erase1# doc-cmd
\ send address16 of blk#
   0 doc-address16
\ send erase cycle 2 command
   NAND-cmd-erase2# doc-cmd
   doc-wait-ready drop
\ check status
   NAND-cmd-status# doc-wp-cmd
   doc-seq-start doc-io@ 1 and dup 
   if ." ERASE error" then
   last-data-read# doc@ drop
;


also forth definitions

: doc-block@ ( buffer blk# -- ior )
\ read 1024 byte block from doc
   2* 2dup  doc-page@ -rot
   1+ swap doc-page-size# + swap 
   doc-page@ or
;

: doc-block! ( buffer blk# -- ior )
\ write 1024 byte block to DoC
   2* 2dup doc-page!  
   1+ swap doc-page-size# + swap
   doc-page! ;

: doc-init
\ initialze millenium chip on tc7010

 d2000 doc-probe 
 30 = if ." DoC initialized" cr then
 0 0 doc-id-chip .id space .mfr cr
;

previous definitions

module

previous definitions

base !

\ @@FILE:ide.fth
\ @@REQUIRES:
\ lifted from forthOS
\ Block I/O routines for IDE
\ depends on NONE

base @ decimal

only forth also
system definitions

defer (rdwt)

1024 constant #blk-size
512 constant #sec-size
#sec-size 2/ constant #sec-words
#blk-size #sec-size / constant secs/blk
 
\ ide unit for current operation
variable ide-unit 0 ide-unit !
 
\ offset into disk to address active partition
variable ide-offset

\ i/o base ports
h# 1f0 constant ide-base   
h# 206 constant ide-ctlr
 
\ ide controller ports
0 constant ide_data     1 constant ide_error    2 constant ide_scnt
3 constant ide_snum     4 constant ide_cyl0     5 constant ide_cyl1
6 constant ide_sdh      7 constant ide-status   7 constant ide_cmd
 
\ ide status bits
h# 01 constant ides_error h# 04 constant ides_ecc
h# 08 constant ides_drq   h# 80 constant ides_busy
 
\ ide mode of operation--lba/ibm
h# 0e0 constant ide_mode
 
\ ide commands
h# 10  constant idecmd-calibrate h# 20 constant idecmd-read
h# 30  constant idecmd-write     h# 40 constant idecmd-verify
h# 50  constant idecmd-format    h# 70 constant idecmd-seek
h# 90  constant idecmd-diag      h# 91 constant idecmd-set-drive-parms
h# 0ec constant idecmd-identify

 
\ return match of ide status bit
: ide-stat-check        ( n -- bool )
ide-base ide-status + pc@ and 0= invert
;
 
\ tell if controller indicates it's busy with a command
: ide-busy   ( -- bool )
    ides_busy ide-stat-check
;
 
\ tell if controller has a data request
: idedrq   ( -- bool )
    ides_drq ide-stat-check
;

\ block i/o into buffer
: repinsw       ( a port count -- )
    2* rot swap over + swap do dup pw@ i w! 2 +loop drop
;

: repoutsw      ( a port count -- )
    2* rot swap over + swap do i w@ over pw! 2 +loop drop
;
 
\ wait-drq wait for ide drq to indicate "ready", pause'ing in between
\ add timeout feature
: wait-drq ( -- )
  begin   idedrq 0=   while pause repeat 
;

\ wait-busy   wait for ide busy flag to clear , pausing in between
\ add time out feature

: wait-busy ( -- )
  begin ide-busy while   pause   repeat
;

\ ide-io actual operation of ide controller
\    wait for drive ready--this can happen unexepectedly
\     due to power management spinning down the drive.
\    i/o size--always a full block
\    program block #, using lba addressing
\    send command
\   for write command, send data as controller asks
\   for read command, pull data as data available
\   drop buffer address... all done!
 
: ide-io ( a blk rw -- )
    wait-busy
   secs/blk  ide-base ide_scnt +  pc!
   -rot dup  ide-base ide_snum +  pc!
   dup 8 rshift  ide-base ide_cyl0 + pc!
   dup 16 rshift ide-base ide_cyl1 + pc!
   24 rshift ide_mode or ide-unit @ 4 lshift or
   ide-base ide_sdh +   pc!
   swap dup   ide-base ide_cmd +   pc!
   idecmd-write = if
        secs/blk 0 do
            wait-drq
            dup ide-base ide_data + #sec-words repoutsw
        #sec-size + loop
   else
      secs/blk 0 do   wait-drq
         dup ide-base ide_data +   #sec-words repinsw
      #sec-size + loop
   then
   drop 
;

\ get block pointer, doing i/o if needed
: ide-rdwt       ( a blk rw -- err )
   \ convert block number to sector index
   swap secs/blk * ide-offset @ + swap
 
   \ convert r/w argument to ide read or write command; 0 means write,
   \  any other value means read
   if idecmd-read else idecmd-write then
   ide-io 0
;

\ disk partition parsing
 
\ # of fdisk entries in sector 0 of a disk
4 constant nfdisk 

\ partition type for forthos storage
158 constant pt_forthos

\ words to access the needed fields in a partition; array of partitions
: sec>parts ( a -- a' )   446 + 
;
 
: part>type ( a -- a' )   4 +  \  partition type 
;

: part>start ( a -- a' )   2 cells +  \   starting sector # 
;

\   overall size of one partition entry
16 constant part.size

\ tell if magic # is ok for partition sector
: partok? ( a -- ? )   510 + @   h# ffff and h# aa55 = 
;
 
\  read fdisk label, find our partition and set ide-offset to
\  make this the start of our blocks.  if can't find partition, clear
\  ide-offset to make "block" see the whole raw disk.
: init-ideoff ( -- )
   0 ide-offset !
   align here   dup 0 idecmd-read ide-io ( a-sec0 )
   dup partok? 0= if   drop exit   then
   sec>parts   nfdisk 0 do
        dup part>type c@ pt_forthos = if
            part>start @   ide-offset !   unloop exit
        then
        part.size +
   loop drop
;
 
\ disk startup/initialization  
: boot-ide ( bool -- n | )
if
    ['] ide-rdwt is (rdwt)
    init-ideoff
else
    1000
then
;
 
previous definitions
 
base ! 
\ @@FILE:keyboard.fth
\ from coreboot libpayload

\ I8042_CMD_READ_MODE  0x20
\ I8042_CMD_WRITE_MODE 0x60
\ I8042_MODE_XLATE     0x40

base @ hex

[defined] system [if]
    only forth also
    system definitions
[then]

[undefined] ms [if]
: ms drop ;
[then]

[undefined] udelay [if]
: udelay drop ;
[then]

d# 87 constant #size-of-map-table  internal 
create key-map
\ No modifier 
     00 c, 1b c, 31 c, 32 c, 33 c, 34 c, 35 c, 36 c, 
     37 c, 38 c, 39 c, 30 c, 2d c, 3d c, 08 c, 09 c, 
     71 c, 77 c, 65 c, 72 c, 74 c, 79 c, 75 c, 69 c, 
     6f c, 70 c, 5b c, 5d c, 0a c, 00 c, 61 c, 73 c, 
     64 c, 66 c, 67 c, 68 c, 6a c, 6b c, 6c c, 3b c, 
     27 c, 60 c, 00 c, 5c c, 7a c, 78 c, 63 c, 76 c, 
     62 c, 6e c, 6d c, 2c c, 2e c, 2f c, 00 c, 2a c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\  Shift 
     00 c, 1b c, 21 c, 40 c, 23 c, 24 c, 25 c, 5e c, 
     26 c, 2a c, 28 c, 29 c, 5f c, 2b c, 08 c, 00 c, 
     51 c, 57 c, 45 c, 52 c, 54 c, 59 c, 55 c, 49 c, 
     4f c, 50 c, 7b c, 7d c, 0a c, 00 c, 41 c, 53 c, 
     44 c, 46 c, 47 c, 48 c, 4a c, 4b c, 4c c, 3a c, 
     22 c, 7e c, 00 c, 7c c, 5a c, 58 c, 43 c, 56 c, 
     42 c, 4e c, 4d c, 3c c, 3e c, 3f c, 00 c, 2a c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\ ALT 
     00 c, 1b c, 31 c, 32 c, 33 c, 34 c, 35 c, 36 c, 
     37 c, 38 c, 39 c, 30 c, 2d c, 3d c, 08 c, 09 c, 
     71 c, 77 c, 65 c, 72 c, 74 c, 79 c, 75 c, 69 c, 
     6f c, 70 c, 5b c, 5d c, 0a c, 00 c, 61 c, 73 c, 
     64 c, 66 c, 67 c, 68 c, 6a c, 6b c, 6c c, 3b c, 
     27 c, 60 c, 00 c, 5c c, 7a c, 78 c, 63 c, 76 c, 
     62 c, 6e c, 6d c, 2c c, 2e c, 2f c, 00 c, 2a c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\ Shift-ALT
     00 c, 1b c, 21 c, 40 c, 23 c, 24 c, 25 c, 5e c, 
     26 c, 2a c, 28 c, 29 c, 5f c, 2b c, 08 c, 00 c, 
     51 c, 57 c, 45 c, 52 c, 54 c, 59 c, 55 c, 49 c, 
     4f c, 50 c, 7b c, 7d c, 0a c, 00 c, 41 c, 53 c, 
     44 c, 46 c, 47 c, 48 c, 4a c, 4b c, 4c c, 3a c, 
     22 c, 7e c, 00 c, 7c c, 5a c, 58 c, 43 c, 56 c, 
     42 c, 4e c, 4d c, 3c c, 3e c, 3f c, 00 c, 2a c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,

1 constant MOD_SHIFT    \  (1 << 0)
2 constant MOD_CTRL     \  (1 << 1)
4 constant MOD_CAPSLOCK \  (1 << 2)
8 constant MOD_ALT      \  (1 << 3)


: keyboard-cmd ( val cmd -- )
  begin h# 64 pc@ 2 and 0= until
  h# 60 pc! 20 ms

  begin h# 64 pc@ 2 and 0= until
  h# 60 pc! 20 ms
;

: keyboard-char? ( -- flg )
  h# 64 pc@
  dup h# ff = if drop 0 else 1 and 0<> then
;

: keyboard-scancode@ ( -- 0|ch )
    keyboard-char? if h# 60 pc@ else 0 then
;

0 value kbd-modifier
0 value kbd-shift
0 value keyboard-char

: keyboard-char@ ( -- 0|ch )
    begin pause keyboard-char? until
    keyboard-scancode@
    dup case    ( ch )
    h# 36 of 
          kbd-modifier MOD_SHIFT or endof
    h# 2a of 
          kbd-modifier MOD_SHIFT or endof
    h# 0b6 of 
          kbd-modifier MOD_SHIFT invert and endof
    h# 0aa of 
           kbd-modifier MOD_SHIFT invert and endof
    h# 38  of 
           kbd-modifier MOD_ALT or endof
    h# 0b8 of 
           kbd-modifier MOD_ALT invert and endof
    h# 1d  of 
           kbd-modifier MOD_CTRL or endof
    h# 9d  of 
           kbd-modifier MOD_CTRL invert and endof
    h# 3a  of 
           kbd-modifier dup MOD_CAPSLOCK and if 
                MOD_CAPSLOCK invert and  0 
                else  MOD_CAPSLOCK or 4 then
                h# ed keyboard-cmd
            endof
   endcase
    to kbd-modifier
   dup h# 80 and 0= over h# 57 < and if ( ch )
         kbd-modifier MOD_SHIFT and kbd-modifier MOD_CAPSLOCK and xor if
             1 else 0 then to kbd-shift
         kbd-modifier MOD_ALT and if kbd-shift 2 + to kbd-shift then

         kbd-shift #size-of-map-table  * key-map + + c@ \ map->map[shift][ch]
( ch ret )
         kbd-modifier MOD_CTRL and if 
            dup [char] a > over [char] z < and if  \ need >= <= 
                h# 1f and
\            else
\            dup h# 53  = if  \ vulcan nerve pinch
\                kbd-modifier MOD_ALT and if reset_handler then )
             else 
                drop 0
            then
        then
    else
        drop 0
    then
;


: keyboard-wait-read ( -- flg )
\ true if no timeout
  10000  begin
        1- dup h#  64 pc@ 1 and 0=  and
   while
        50 udelay
   repeat
   0 >
;

: keyboard-wait-write ( -- flg )
\ true if no timeout
  10000 begin
        1- dup h# 64 pc@ 2 and 0= and
  while
        50  udelay 
  repeat
  0 >
;

: keyboard-mode@ ( -- 0|mode )
    h# 20 h# 64 pc!
    keyboard-wait-read if
        h# 60 pc@  else 0 
    then
;

: keyboard-mode! ( mode -- )
\
    h# 60 h# 64 pc!
    keyboard-wait-write if
        h# 60 pc! else drop 
    then
;


: keyboard-init ( -- )
\  If 64 returns ff , then we have no keyboard controller
    h# 64 pc@ h# ff =  if abort" No keyboard" then
\ Empty keyboard buffer
    begin keyboard-char?  while
        keyboard-char@ drop
    repeat
;  external


: key-keybd? ( -- flg )
  keyboard-char 0= if
     keyboard-char? if keyboard-char@ to keyboard-char then
  then
  keyboard-char 0<>
;

: key-keybd ( -- ch )
 begin key-keybd? until
 keyboard-char  0 to keyboard-char
;   module

[defined] system [if]
  previous definitions
[then]
base !

\ @@FILE:graphics-blt.fth
\ @@REQUIRES:gx1cpu.fth
\ graphics utilities
\
\ 
\ export point,line,gotoxy xmax,ymax

base @ hex

only forth also system
also forth definitions

\ screen limits in pixels

: xmax ( -- u) fb-pixels/line 1- ;
: ymax ( -- u ) fb-lines@ 1- ;

variable colour
variable fg         \ foreground 
variable bg         \ background

\ text cursor location
variable col#
variable row# 

0 fg ! 0 bg ! 0 colour !
0 row# ! 0 col# !

previous definitions

: rgb>565 ( r g b -- w )
 3 rshift
 swap 2 rshift 5 lshift or
 swap 3 rshift d# 11 lshift or
;

create colours565#  \ array of standard colours
00 00 00 rgb>565 ,  \ 0 Black
00 00 aa rgb>565 ,  \ 1 Dark blue
00 aa 00 rgb>565 ,  \ 2 Dark green
00 aa aa rgb>565 ,  \ 3 Dark cyan
aa 00 00 rgb>565 ,  \ 4 Dark red
aa 00 aa rgb>565 ,  \ 5 Dark magenta
aa 55 aa rgb>565 ,  \ 6 Brown
aa aa aa rgb>565 ,  \ 7 Light gray
55 55 55 rgb>565 ,  \ 8 Dark gray
55 55 ff rgb>565 ,  \ 9 Light blue
55 ff 55 rgb>565 ,  \ 10 Light green
55 ff ff rgb>565 ,  \ 11 Light cyan
ff 55 55 rgb>565 ,  \ 12 Light red (pink)
ff 55 ff rgb>565 ,  \ 13 Light magenta
ff ff 55 rgb>565 ,  \ 14 Light yellow
ff ff ff rgb>565 ,  \ 15 White

\ calulate framebuffer offset for pixel x,y
\ fbuf + y * bytes/line + x * bytes/pixel
: pixelxy ( x y -- a )
   fb-line-delta@ *
   swap bytes/pixel * + fbuf +
;

variable vect-mode-flag

: draw-line ( x1 y1 x2 y2 -- )
    0 vect-mode-flag !      \ x-major increment
    rot tuck -
    2swap over - rot    \ y1 x1 dx dy
    over abs over abs < 0= if      \ adx >= ady
        dup  0> if 4 vect-mode-flag or! then    \ dy >= 0
        over 0> if 2 vect-mode-flag or! then    \ dx >= 0
        abs swap abs        \ ( ady=dmin adx=dmaj )
    else
        1 vect-mode-flag !
        dup  0>  if 2 vect-mode-flag or! then    \ dy >= 0
        over 0>  if 4 vect-mode-flag or! then    \ dx >= 0    
        abs swap abs swap               \ ( adx=dmin ady=dmaj )
    then
    \ y1 x1 dmin dmaj
    swap 2*         \ dmaj ae
    2dup swap 2* -  \ dmaj ae de
    rot rot
    2dup swap -     \ de dmaj ae ie
    vect-mode-flag @ 4 and if 1- then
    fg @ gx1-ge-solid-pattern!
    h# 0f0 gx1-ge-raster-operation!
    gx1-ge-wait-pending
    gx1-ge-height! gx1-ge-src-x!
    gx1-ge-width! gx1-ge-src-y!
    gx1-ge-dst-x! gx1-ge-dst-y!
    vect-mode-flag @ gx1-ge-vect!    
;


: draw-fill ( x y w h )  \ tested
    fg @ gx1-ge-solid-pattern!
    h# 0f0 gx1-ge-raster-operation!
    gx1-ge-pattern-fill
;

: draw-rect ( x y w h ) \ tested  init-controller again!
    fg @ gx1-ge-solid-pattern!
    h# 0f0 gx1-ge-raster-operation!
    4dup drop 1 ( x y w 1) gx1-ge-pattern-fill
    4dup 1- rot + swap 1 ( x y+h-1 w 1) gx1-ge-pattern-fill
    4dup nip 2 - swap 1+ 1 rot ( x y+1 1 h-2 ) gx1-ge-pattern-fill
    >r rot + 1- swap 1+ 1 r> 2 - ( x+w-1 y+1 1 h-2 ) gx1-ge-pattern-fill
;

: draw-blt ( x y dx dy w h )   \ tested
    fg @ gx1-ge-solid-pattern!
    h# 0cc gx1-ge-raster-operation!
    gx1-ge-screen-to-screen-blt
;
also forth definitions

: colour! ( idx -- ) cells colours565# + @ colour ! ;
: fg! ( idx -- ) cells colours565# + @ fg ! ;
: bg! ( idx -- ) cells colours565# + @ bg ! ;

: plot-point ( x y colour -- ) -rot pixelxy ( c a ) fb! ;

: clear-screen ( -- )  \ clear screen using bitblt
    fg @ bg @ fg !
    0 0 xmax 1+ ymax 1+ draw-fill
    fg !
;

: clr ( -- )   \ black background white foreground
  0 bg! h# 0f fg! clear-screen
;

: #rows fb-lines@ /font-height / ;      \  value #rows
: #cols fb-pixels/line /font-width  / ; \  value #cols

\ graphics cursor location
variable xnow 0 xnow !
variable ynow 0 ynow !

: gotoxy ( x y --) ynow ! xnow ! ;

\ pixel at (x,y) 
: point ( x y -- ) fg @ plot-point ;

: linev ( x y -- )    \ vertical line
    nip xnow @ over ynow @ 2dup > if     \ y > ynow 
        tuck else over then
    - 1 swap draw-fill
    ynow !
;

: lineh ( x y -- )    \ horizontal line 
    drop dup xnow @ 2dup > if     \ x > xnow 
        tuck else over then
    - ynow @ swap 1 draw-fill
    xnow !
;

: line ( x1 y1 x2 y2 )
    3 pick 2 pick = if gotoxy linev exit then
    2 pick 1 pick = if gotoxy lineh exit then
    2over gotoxy draw-line
;

: lineto  ( x y -- )
    xnow @ ynow @ line
;

previous previous definitions  
base !
 
\ @@FILE:charbuf.fth
\ @@REQUIRES:graphics.fth, font8x16.fth
\ console text words

base @ hex

only forth also
system definitions

create chmap bytes/pixel /font-width * allot   \ size varies with font

defer char-map@

' c@ is char-map@

: 3dup ( x y z -- x y z x y z) >r 2dup r@ -rot r>  ;

: 3drop ( x1 x2 x3 -- ) 2drop drop ;

: char-glyph ( ch -- a) /font-size 3 rshift * font-array + ;
 
: chmap-pixels ( bm -- )
\ expand character bitmaps into chmap
/font-width 1+  1 do
  dup 1 and     ( ch bn )
  if fg else bg then
  @  bytes/pixel /font-width i - * ( ch cl n )
  chmap +         ( ch cl a )
  fb!             ( ch )
  1 rshift
loop
drop
;

\ char row col to pixel xy mapping

: position ( row col --  x y)  /font-width * swap /font-height * ;

: draw-char ( x y c -- )   \ paint char at pixel x y
    0 gx1-ge-solid-pattern!
    bg @ fg @ 0 gx1-ge-mono-source!
    h# 0cc gx1-ge-raster-operation!
    >r /font-width /font-height 
    r> char-glyph
    gx1-ge-text-blt
;

: char-reverse ( x y -- )  \ invert char background 
    0 gx1-ge-solid-pattern!
    h# 055 gx1-ge-raster-operation!
    2dup /font-width /font-height 
    gx1-ge-screen-to-screen-blt
;

\ char row col to fbuf mapping
: row-col-addr ( row col -- fba )  position pixelxy ;

: row-addr ( row -- fba)  0 row-col-addr ;

: clear-row ( row -- )  \ 
    fg @ bg @ fg ! swap
    0 position xmax 1+ /font-height
    draw-fill fg !
;

also forth definitions

: scroll ( -- ) 
    1 0 position 0 0 position
    xmax 1+ ymax 1+ /font-height -
    draw-blt
    #rows 1- clear-row
;

\ paint char at row col
: putch ( row col ch -- ) >r position r> draw-char ;

previous definitions

: row#-incr ( -- ) \ next row
 row# @ #rows 1- < if 
    1 row# +!  else scroll then
;

: col#-incr ( -- ) \ next column
 1 col# +!
 col# @ #cols 1- > if
     0 col# !  row#-incr then
;

\ move cursor to row col
: at-row-col ( row col -- )  col# ! row# ! ;

also forth definitions

: emit-console ( c --)
 case
     0a of row#-incr endof
     0d of 0 col# ! endof
     8 of row# @ col# @ 1- 0 max dup col# ! bl putch  endof
    (   default )
     dup row# @ col# @ rot  ( r c ch )
     putch col#-incr
 endcase 
;

\ send output to both serial and screen
: my-emit ( c --) dup emit-console emit-sio ;

: stand-alone ( -- ) \ use graphics screen and ps/2 keybd
    keyboard-init
  ['] emit-console emitv !
  ['] key-keybd keyv !
  ['] key-keybd? key?v !
;

: attached ( -- )
  ['] emit-sio emitv !
  ['] key-sio keyv !
  ['] key-sio? key?v !
;

previous previous definitions

0 bg !  0f colour! colour @ fg !

base !

\ 80386 Disassembler
\ Andrew McKewan, April 1994
\ Tom Zimmer,  05/18/94 port to Win32f
\ Modified to word in decimal 08/03/94 10:04 tjz
\ 06-??-95 SMuB NEXT sequence defined in FKERNEL
\ 06-21-95 SMuB removed redundant COUNT calls from txb, lxs.
\ 04-??-97 Extended by C.L. to include P6 and MMX instructions
\ ??-??-14 adapted for CAmelforth Project
\ cr .( Loading 80486 Disassembler...)

\ Gforth stuff

\ original stuff

only forth also definitions
vocabulary disassembler
disassembler also definitions
decimal
260 chars constant maxstring
128 constant spcs-max  ( optimization for spaces )
create spcs  spcs-max allot spcs spcs-max bl fill

255 constant maxcounted   \ maximum length of contents of a counted string
\   words needed but not in camelforth

\ # rows and column that the terminal has.
\ these words are also present in PFE.

[undefined] form [if]
: form d# 24 d# 80 ;
: rows ( -- u )    form drop ;
: cols ( -- u )    form nip ;
[then]

: hex.   ( u -- )  base @ >r hex u. r> base ! ;
: off  ( a-addr -- ) 0 swap ! ;
: cincr ( c-addr -- ) dup c@ 1+ swap c! ;
: place ( str len addr -- ) 2dup 2>r 1+ swap move 2r> c! ;
: +place ( adr len adr )
        2dup >r >r
        dup c@ char+ + swap move
       r> r> dup c@ rot + swap c! ;
: c+place       ( c1 a1 -- )    \ append char c1 to the counted string at a1
     dup cincr count + 1- c! ;
: (d.)          ( d -- addr len )    tuck dabs  <# #s rot sign #> ;
\ : nfa-count ( nfa -- addr u ) name>string ;
\ : w@ ( addr -- w )  @ h# ffff and ;
: col ( n -- ) drop space ;

0 value default-16bit?
: default-16bit ( -- )  true to default-16bit? ;
: default-32bit ( -- )  false to default-16bit? ;

defer show-name   ( cfa -- )      \ display nearest symbol
' abort is show-name
0 value base-addr


create s-buf maxstring allot

: >s  ( a1 n1 -- )   s-buf +place ;

: 0>s ( -- )  \ reset s-buf
                s-buf   off  ;  

: sspaces       ( n1 -- )
                0 max spcs swap s-buf +place ;

: sspace ( -- ) 1 sspaces ;

\ : emit>s ( c1 -- )  s-buf c+place ;

: s>  ( -- a1 n1 ) s-buf count ;

: .s"  ( 'text' -- )
    [char] " parse postpone sliteral 
    postpone s-buf postpone +place ; immediate

: d.r>s ( d w -- )  >r (d.) r> over - sspaces >s ;

: .r>s ( n w -- )  >r  s>d  r>  d.r>s ;

\ : u.r>s   ( u w -- )  0 swap d.r>s ;

: h.>s          ( u -- )
                base @ swap  hex 0 (d.) >s sspace   base ! ;

\ : h.r>s           ( n1 n2 -- )
\                 base @ >r hex >r
\                 0 <# #s #> r> over - sspaces >s
\                 r> base ! ;

\ : .id>s         ( nfa -- )
\                 nfa-count >s sspace ;

\ : .name>s       ( xt -- )
\          dup >name dup 0=     \ if not found
\          if      drop [char] " emit>s .s" 0x" 1 h.r>s [char] " emit>s sspace
\         else    .id>s drop
\         then    ;

\ : ?.name>s      ( cfa -- )
\ \ eliminate " 0x"
\                 dup ?name ?dup
\                 if      .name>s
\                 else    dup 1 h.r>s sspace
\                 then    drop ;

\ ' ?.name>s is show-name
' h.>s is show-name

32 constant comment-col

0 value size
0 value 16-bit-data
0 value 16-bit-addr
0 value prefix-op
0 value mmx-reg

: @+  ( addr -- addr n )  dup cell+ swap @ ;
: w@+ ( addr -- addr n )  dup 2 + swap w@ ;

: sext  ( byte -- n )  dup h# 80 and if h# FFFFFF00 or then ;
: mod/sib ( mod-r-r/m -- r/m r mod ) \ including general, special, segment, MMX
          ( mod-op-r/m -- r/m op mod )
          ( s-i-b -- b i s )
          255 and 8 /mod 8 /mod ;

: ???   ( n1 -- ) .s" ??? " drop ;

: ss. ( n adr len w )  >r drop  swap r@ * +  r> >s sspace ;

: tttn ( code -- ) 15 and s" o nob aee nebea s nsp npl geleg " 2 ss. ;

: sreg  ( sreg -- )  3 rshift 7 and s" escsssdsfsgsXXXX" 2 ss. ;
: creg  ( eee --  )  3 rshift 7 and s" cr0???cr2cr3cr4?????????" 3 ss. ;
: dreg  ( eee --  )  3 rshift 7 and s" dr0dr1dr2dr3??????dr6dr7" 3 ss. ;
: treg  ( eee --  )  3 rshift 7 and 
     s" ?????????tr3tr4tr5tr6tr7" 3 ss. ; \ obsolete
: mreg  ( n -- )  7 and s" mm0mm1mm2mm3mm4mm5mm6mm7" 3 ss. ;

: reg8  ( n -- )  7 and s" alcldlblahchdhbh" 2 ss. ;
: reg16 ( n -- )  7 and s" axcxdxbxspbpsidi" 2 ss. ;
: reg32 ( n -- )  7 and s" eaxecxedxebxespebpesiedi" 3 ss. ;
: reg16/32      ( n -- )
                16-bit-data
                if   reg16
                else reg32
                then ;
: reg   ( a n -- a )
        mmx-reg
        if   mreg
        else size
             if   reg16/32
             else reg8
             then
        then ;

: [base16] ( r/m -- )   4 - s" [si][di][bp][bx]" 4 ss. ;
                        \ r/m = 4 , 5 , 6 , 7
: [ind16]  ( r/m -- )   s" [bx+si][bx+di][bp+si][bp+di]" 7 ss. ;
                        \ r/m = 0  ,   1  ,   2  ,   3
: [reg16]  ( r/m -- )   dup 4 <
                        if    [ind16]
                        else  [base16]
                        then ;
: [reg32]  ( n -- )     7 and 
         s" [eax][ecx][edx][ebx][esp][ebp][esi][edi]" 5 ss. ;

\ : [reg]    ( r/m -- )  16-bit-addr
\                        if   [reg16]
\                        else [reg32]
\                        then sspace ;

\ : [reg] ( n -- )
\        7 and
\        16-bit-addr
\        if      s" [bx+si] [bx+di] [bp+si] [bp+di] [si] [di] [bp] [bx]"
\                rot 0
\                ?do     bl skip bl scan
\                loop    bl skip 2dup bl scan nip - >s 2 sspaces
\        else    s" [eax][ecx][edx][ebx][esp][ebp][esi][edi]" 5 ss. sspace
\        then    ;

: [reg*2]  ( i -- )
   s" [eax*2][ecx*2][edx*2][ebx*2][XXX*2][ebp*2][esi*2][edi*2]" 7 ss. ;
: [reg*4]  ( i -- )
   s" [eax*4][ecx*4][edx*4][ebx*4][XXX*4][ebp*4][esi*4][edi*4]" 7 ss. ;
: [reg*8]  ( i -- ) 
   s" [eax*8][ecx*8][edx*8][ebx*8][XXX*8][ebp*8][esi*8][edi*8]" 7 ss. ;
: [index]  ( sib -- )   mod/sib over 4 =
                        if    2drop                  \ no esp scaled index
                        else  case ( s )
                                0 of [reg32] endof
                                1 of [reg*2] endof
                                2 of [reg*4] endof
                                3 of [reg*8] endof
                              endcase
                        then drop ;

: disp8  ( adr -- adr' )  count h.>s ;
: disp16 ( adr -- adr' )  w@+ show-name ;
: disp32 ( adr -- adr' ) @+ ( body> ) show-name ;
: disp16/32 ( adr -- adr' )
            16-bit-addr
            if   disp16
            else disp32
            then ;

: .,     ( -- )           .s" , " ;

: .#  ., .s" # " ;

: imm8   ( adr -- adr' )  .# count h.>s ;

\ : imm16  ( adr -- adr' )  .# w@+ h.>s ;

: imm16/32  ( adr -- adr' )
        .# 16-bit-data
        if   w@+
        else @+
        then h.>s ;

: sib   ( adr mod -- adr )
        >r count tuck 7 and 5 = r@ 0= and
        if    disp32 swap [index] r> drop       \ ebp base and mod = 00
        else  r> case ( mod )
                   1 of disp8  endof
                   2 of disp32 endof
                 endcase
              swap dup [reg32] [index]
        then ;

\ : [*]  ( sib --  )
\       .s" sib = " h.>s ;

\ : sib ( adr ext -- adr' )
\ ?? wrong version
\        swap count >r swap  6 rshift 3 and
\        ?dup if 1 = if disp8 else disp32 then then
\        r> dup 7 and dup 5 =
\        if      drop [*]
\        else    [reg]
\                dup h# 38 and h# 20 =
\                if      drop
\                else    .s" [" dup 3 rshift reg32 -1 s-buf c+!
\                        5 rshift 6 and
\                        dup 6 = if 2 + then
\                       ?dup if .s" *" 0 .r>s  then .s" ] "
\                then
\        then ;

: mod-r/m32     ( adr r/m mod -- adr' )
                dup 3 =
                if    drop reg              \ mod = 3, register case
        else  size if  16-bit-data 0= if .s" d" then .s" word"
              else  .s" byte" then .s"  ptr "
              over 4 =
                      if nip sib                        \ r/m = 4, sib case
                      else  2dup 0= swap 5 = and        \ mod = 0, r/m = 5,
                            if 2drop disp32             \ disp32 case
                            else rot swap
                                 case ( mod )
                                   1 of disp8  endof
                                   2 of disp32 endof
                                 endcase
                                 swap [reg32]
                            then
                      then
                then ;

: mod-r/m16     ( adr r/m mod -- adr' )
                2dup 0= swap 6 = and
                if   2drop disp16                       \ disp16 case
                else case ( mod )
                       0 of [reg16]                     endof
                       1 of swap disp8  swap [reg16]    endof
                       2 of swap disp16 swap [reg16]    endof
                       3 of reg                         endof
                     endcase
                then ;

: mod-r/m ( adr modr/m -- adr' )
          mod/sib nip 16-bit-addr
          if    mod-r/m16
          else  mod-r/m32
          then ;

\ : mod-r/m  ( adr ext -- adr' )
\        dup 0xC7 and 5 =                \ 32bit displacement
\        16-bit-addr 0= and              \ and not 16bit addressing
\        if      drop disp32 .s" [] "
\                EXIT
\        then
\        dup 0xC0 and 0xC0 < over 7 and 4 = and
\        16-bit-addr 0= and              \ and not 16bit addressing
\        if      sib
\                EXIT
\        then
\        dup 0xC7 and 6 =                \ 16bit displacement
\        16-bit-addr and                 \ and 16bit addressing
\        if      drop disp32 .s" [] "
\                EXIT
\        then
\        dup 6 rshift
\        case
\          0 of  .s" 0 " [reg]  endof
\          1 of  swap disp8  swap [reg]  endof
\          2 of  swap disp32 swap [reg]  endof
\          3 of  reg  endof
\        endcase ;




: r/m8      0 to size mod-r/m ;
: r/m16/32  1 to size mod-r/m ;
: r/m16     true to 16-bit-data r/m16/32 ;

: r,r/m  ( adr -- adr' )
        count dup 3 rshift reg ., mod-r/m ;

: r/m,r  ( adr -- adr' )
        count dup >r mod-r/m ., r> 3 rshift reg ;

: r/m  ( adr op -- adr' )
        2 and if r,r/m else r/m,r then ;

\ -------------------- Simple Opcodes --------------------

: inh   ( -<name>- )
        create
        bl word count here place
        here c@ 1+ allot
        does> count >s sspace drop ;

inh clc  clc
inh stc  stc
inh cld  cld
inh std  std
\ inh rpnz repnz
\ inh repz repz
inh cbw  cbw
inh cdq  cdq
inh daa  daa
inh das  das
inh aaa  aaa
inh aas  aas
\ inh lock lock
inh inb  insb
inh osb  outsb
inh sah  sahf
inh lah  lahf
\ inh aam  aam
\ inh aad  aad
inh hlt  hlt
inh cmc  cmc
inh xlt  xlat
inh cli  cli
inh sti  sti

inh clt clts
inh inv invd
inh wiv wbinvd
inh ud2 ud2
inh wmr wrmsr
inh rtc rdtsc
inh rmr rdmsr
inh rpc rdpmc
inh ems emms
inh rsm rsm
inh cpu cpuid
inh ud1 ud1
\ inh lss lss
\ inh lfs lfs
\ inh lgs lgs

\ inh d16: d16:
\ inh a16: a16:
\ inh es:  es:
\ inh cs:  cs:
\ inh ds:  ds:
\ inh fs:  fs:
\ inh gs:  gs:

: aam   ( adr code -- adr' )
        .s" aam" drop count drop ;

: aad   ( adr code -- adr' )
        .s" aad" drop count drop ;

: d16   ( adr code -- adr' )
        drop .s" d16:"
        true to 16-bit-data
        true to prefix-op
        ;

: a16   ( adr code -- adr' )
        drop .s" a16:"
        true to 16-bit-addr
        true to prefix-op
        ;

: rpz   ( adr code -- adr' )
        drop .s" repnz"
        true to prefix-op
        ;

: rep   ( adr code -- adr' )
        drop .s" repz"
        true to prefix-op
        ;

: lok   ( adr code -- adr' )  \ This should have error checking added
        drop .s" lock"
        true to prefix-op
        ;

: cs:   ( adr code -- adr' )
        drop .s" cs:"
        true to prefix-op
        ;

: ds:   ( adr code -- adr' )
        drop .s" ds:"
        true to prefix-op
        ;

: ss:   ( adr code -- adr' )
        drop .s" ss:"
        true to prefix-op
        ;

: es:   ( adr code -- adr' )
        drop .s" es:"
        true to prefix-op
        ;

: gs:   ( adr code -- adr' )
        drop .s" gs:"
        true to prefix-op
        ;

: fs:   ( adr code -- adr' )
        drop .s" fs:"
        true to prefix-op
        ;

: isd   ( adr code -- adr' )
        drop 16-bit-data
        if      .s" insw    "
        else    .s" insd    "
        then ;

: osd   ( adr code -- adr' )
        drop 16-bit-data
        if      .s" outsw    "
        else    .s" outsd    "
        then ;

: inp   ( addr code -- addr' )
        .s" in      " 1 and
        if      16-bit-data
                if      .s" ax , "
                else    .s" eax , "
                then
        else    .s" al , "
        then
        count h.>s ;

: otp   ( addr code -- addr' )
        .s" out     " 1 and
        if      count h.>s 16-bit-data
                if      .s" , ax"
                else    .s" , eax"
                then
        else    count h.>s .s" , al"
        then
        ;

: ind   ( addr code -- addr' )
        .s" in      " 1 and
        if      16-bit-data
                if      .s" ax , dx"
                else    .s" eax , dx"
                then
        else    .s" al , dx"
        then
        ;

: otd   ( addr code -- addr' )
        .s" out     " 1 and
        if      16-bit-data
                if      .s" dx , ax"
                else    .s" dx , eax"
                then
        else    .s" dx , al"
        then
        ;

\ -------------------- alu opcodes --------------------

: .alu  ( n -- )
        7 and s" addor adcsbbandsubxorcmp" 3 ss. 4 sspaces ;

: alu  ( adr op -- adr' )
        dup 3 rshift .alu r/m ;

: ali ( adr op -- adr' )
        >r count
        dup 3 rshift .alu
        mod-r/m
        r> 3 and ?dup
        if      1 =
                if      imm16/32
                else   .# count sext 0 .r>s sspace
                then
        else    imm8
        then ;

: ala  ( adr op -- adr' )
        dup 3 rshift .alu
        1 and if 0 reg imm16/32 else 0 reg8 imm8 then ;


\ -------------------- test/xchg --------------------

: txb   ( addr op -- addr' )
        dup 3 and s" testtestxchgxchg" 4 ss. 3 sspaces
        1 and
        if      1 to size r,r/m     \ smub removed count
        else    0 to size r,r/m     \ smub removed count
        then
        ;

: tst   ( addr op -- addr' )
        .s" test    " 1 and
        if      16-bit-data
                if   .s" ax , "
                else .s" eax , "
                then
                imm16/32
        else    .s" al , " imm8
        then
        ;

\ -------------------- inc/dec ----------------------

: inc  ( addr op -- addr' )
        .s" inc     " reg16/32 ;

: dec  ( addr op -- addr' )
        .s" dec     " reg16/32 ;


\ -------------------- push/pop --------------------

: psh   ( addr op -- addr' )
        .s" push    " reg16/32 ;

: pop   ( addr op -- addr' )
        .s" pop     " reg16/32 ;

: pss   ( addr op -- addr' )
        .s" push    " sreg ;

: pps   ( addr op -- addr' )
        .s" pop     " sreg ;

: psa   ( addr op -- addr' )
        drop 16-bit-data
        if      .s" pusha   "
        else    .s" pushad  "
        then ;

: ppa   ( addr op -- addr' )
        drop 16-bit-data
        if      .s" popa    "
        else    .s" popad   "
        then ;

: psi   ( addr op -- addr' )
        .s" push    " 2 and
        if      imm8
        else    imm16/32
        then ;

: psf   ( addr op -- addr' )
        drop 16-bit-data
        if      .s" pushf   "
        else    .s" pushfd  "
        then ;

: ppf   ( addr op -- addr' )
        drop 16-bit-data
        if      .s" popf    "
        else    .s" popfd   "
        then ;

: 8F.   ( addr op -- addr' )
        drop count .s" pop     " r/m16/32 ;

\ -------------------- move --------------------

: mov  ( addr op -- addr' )
        .s" mov     " r/m ;

: mri  ( addr op -- addr' ) ( mov register, imm )
        .s" mov     " dup 8 and
        if      reg16/32 imm16/32
        else    reg8 imm8
        then ;

: mvi  ( adr op -- adr' )   ( mov mem, imm )
        .s" mov     " drop count mod-r/m
        size
        if      imm16/32
        else    imm8
        then
        ;

: mrs   ( addr op -- addr' )
\ ? remove redundant >r , r>
        16-bit-data
        if      .s" mov     " drop
                1 to size
                count dup mod-r/m .,
                sreg
        else    ???
        then ;

: msr   ( addr op -- addr' )
        16-bit-data
        if      .s" mov     " drop
                1 to size
                count dup sreg .,
                mod-r/m
        else    ???
        then ;

: mrc   ( addr op -- addr' )
        .s" mov     "
        drop count dup reg32 .s" , "
        creg ;

: mcr   ( addr op -- addr' )
        .s" mov     "
        drop count dup creg .s" , "
        reg32 ;

: mrd   ( addr op -- addr' )
        .s" mov     "
        drop count dup reg32 .s" , "
        dreg ;

: mdr   ( addr op -- addr' )
        .s" mov     "
        drop count dup dreg .s" , "
        reg32 ;

: mrt   ( addr op -- addr' )
\ obsolete
        .s" mov     "
        drop count dup reg32 .s" , "
        treg ;

: mtr   ( addr op -- addr' )
\ obsolete
        .s" mov     "
        drop count dup treg .s" , "
        reg32 ;

: mv1   ( addr op -- addr' )
        .s" mov     " 1 and
        if      16-bit-data
                if      .s" ax , "
                else    .s" eax , "
                then
        else    .s" al , "
        then
        disp16/32 ;

: mv2   ( addr op -- addr' )
        >r .s" mov     " disp16/32 .,
        r> 1 and
        if      16-bit-data
                if      .s"  ax"
                else    .s"  eax"
                then
        else    .s"  al"
        then
        ;

: lea  ( addr op -- addr' )
        .s" lea     " drop  1 to size r,r/m ;

: lxs   ( addr op -- addr' )
        1 and
        if      .s" lds     "
        else    .s" les     "
        then
        r,r/m   \ smub removed count
        ;

: bnd  ( addr op -- addr' )
        .s" bound   " drop  1 to size r,r/m ;

: arp   ( addr op -- addr' )
        .s" arpl    " drop
        1 to size
        true to 16-bit-data
        r,r/m
        ;

: mli   ( addr op -- addr' )
        1 to size
        .s" imul    " h# 69 =
        if      r,r/m imm16/32
        else    r,r/m imm8
        then ;

\ -------------------- jumps and calls --------------------

: rel8  ( addr op -- addr' )
        count sext over + h.>s ;

: rel16/32 ( addr op -- addr' )
        16-bit-addr
        if      w@+
        else    @+
        then    over + base-addr - show-name ;

: jsr  ( addr op -- addr' )
        .s" call    " drop rel16/32 ;

: jmp  ( addr op -- addr' )
        .s" jmp     " 2 and if rel8 else rel16/32 then ;

: .jxx  ( addr op -- addr' )
        .s" j" tttn 4 sspaces ;

: bra  ( addr op -- addr' )
        .jxx rel8 ;

: lup  ( addr op -- addr' )
        3 and s" loopnzloopz loop  jecxz " 6 ss. 1 sspaces rel8 ;

: lbr  ( addr op -- addr' )
        .jxx rel16/32 ;

: rtn  ( addr op -- addr' )
        .s" ret     near " 1 and 0=
        if      w@+ h.>s
        then ;

: rtf  ( addr op -- addr' )
        .s" ret     far " 1 and 0=
        if      w@+ h.>s
        then ;

: ent  ( addr op -- addr' )
        .s" enter   " drop w@+ h.>s ., count h.>s ;

: cis   ( addr op -- addr' )
        h# 9a =
        if      .s" call    "
        else    .s" jmp     "
        then
        16-bit-data
        if      .s" ptr16:16 "
        else    .s" ptr16:32 "
        then
        count mod-r/m ;

: nt3   ( addr op -- addr' )
        drop .s" int     3 "
        ;

: int   ( addr op -- addr' )
        drop .s" int     "
        count h.>s ;

inh lev leave
inh irt  iret
inh nto  into

\ -------------------- string ops --------------------

: str   inh does> count >s  1 and if .s" d" else .s" b" then ;

str mvs movs
str cps cmps
str sts stos
str lds lods
str scs scas

\ -------------------- exchange --------------------

: xga  ( addr op -- addr' )
        .s" xchg     eax, " reg16/32 ;

\ : xch  ( addr op -- addr' )
\       .s" xchg    " drop r,r/m ;


\ -------------------- shifts & rotates --------------------

: .shift ( n -- )
        7 and s" rolrorrclrcrshlshrxxxsar" 3 ss.  4 sspaces ;

: shf  ( addr op -- addr' )
        >r count
        dup 3 rshift .shift
        mod-r/m .,
        r> h# d2 and
        case
           h# c0 of count h.>s      endof
           h# d0 of 1 h.>s          endof
           h# d2 of 1 reg8          endof
        endcase ;

\ -------------------- extended opcodes --------------------

: wf1  ( addr -- addr' )
        1+ count dup
        h# 0c0 <
        if      dup
                3 rshift 7 and
                case 6 of     .s" fstenv  "      mod-r/m   endof
                     7 of     .s" fstcw   word " mod-r/m   endof
                     2drop 2 - dup .s" fwait   "
                endcase
        else    drop 2 - .s" fwait   "
        then ;

: wf2  ( addr -- addr' )
        1+ count
        case h# e2 of   .s" fclex   "  endof
             h# e3 of   .s" finit   "  endof
             swap 2 - swap .s" fwait   "
        endcase ;

: wf3  ( addr -- addr' )
        1+ count dup 3 rshift 7 and
        case 6 of     .s" fsave   "      mod-r/m   endof
             7 of     .s" fstsw   word " mod-r/m   endof
             2drop 2 - dup .s" fwait   "
        endcase ;

: wf4  ( addr -- addr' )
        1+ count h# e0 =
        if      .s" fstsw   ax "
        else    2 - .s" fwait   "
        then ;

: fwaitops   ( addr op -- addr' )
        case h# d9 of    wf1     endof
             h# db of    wf2     endof
             h# dd of    wf3     endof
             h# df of    wf4     endof
             .s" fwait   "
        endcase ;

: w8f   ( addr op -- addr' )
        drop dup c@ dup h# f8 and h# d8 =
        if      fwaitops
        else    drop .s" wait    "
        then ;

: falu1   ( xopcode -- )
        3 rshift 7 and
        s" fadd fmul fcom fcompfsub fsubrfdiv fdivr"
        5 ss. 2 sspaces ;

: falu5   ( xopcode -- )
        3 rshift 7 and
        s" fadd fmul ???? ???? fsubrfsub fdivrfdiv "
        5 ss. 2 sspaces ;

: sti.   ( op -- )
        7 and .s" st(" 1 .r>s .s" )";

\ : sti.st   ( op -- )
\        7 and
\        .s" st(" 1 .r>s .s" )" .s"  st " ;

: fd8   ( addr opcode -- addr' )
        drop count dup falu1
        dup h# c0 <
        if      .s" float " mod-r/m
        else    dup h# f0 and h# d0 =
                if      sti.
                else    .s" st , " sti.
                then
        then ;

: fdc   ( addr opcode -- addr' )
        drop count
        dup dup h# c0 <
        if      falu1 .s" double " mod-r/m
        else    falu5 sti. .s"  , st"
        then ;

: fnullary-f   ( op -- )
        h# 0f and dup 8 <
        if
           s" f2xm1  fyl2x  fptan  fpatan fxtractfprem1 fdecstpfincstp"
        else  8 -
           s" fprem  fyl2xp1fsqrt  fsincosfrndintfscale fsin   fcos   "
        then
        7 ss. ;

: fnullary-e   ( op -- )
        h# 0f and dup 8 <
        if
           s" fchs   fabs   ???    ???    ftst   fxam   ???    ???    "
        else  8 -
           s" fld1   fldl2t fldl2e fldpi  fldlg2 fldln2 fldz   ???    "
        then
        7 ss. ;

: fnullary   ( op -- )
        dup h# ef >
        if      fnullary-f exit
        then
        dup h# e0 <
        if      h# d0 =
                if      .s" fnop"
                else    dup ???
                then
                exit
        then
        fnullary-e ;


\ : falu2   ( op -- )
\        3 rshift 7 and
\        s" fld    ???    fst    fstp   fldenv fldcw  fnstenvfnstcw "
\        7 ss. ;

: fd9   ( addr op -- addr' )
        drop count dup h# c0 <
        if      dup h# 38 and
                case
                        h# 00 of .s" fld     float "  endof
                        h# 10 of .s" fst     float "  endof
                        h# 18 of .s" fstp    float "  endof
                        h# 20 of .s" fldenv  "        endof
                        h# 28 of .s" fldcw   word "   endof
                        h# 30 of .s" fnstenv "        endof
                        h# 38 of .s" fnstcw  word "   endof
                            dup ???
                endcase
                mod-r/m
        else
                dup h# d0 <
                if      dup h# c8 <
                        if      .s" fld     "
                        else    .s" fxch    "
                        then
                        sti.
                else    fnullary
                then
        then ;

: falu3   ( op -- )
        3 rshift 7 and
        s" fiadd fimul ficom ficompfisub fisubrfidiv fidivr"
        6 ss. 1 sspaces ;

: fcmova  ( op -- )
        3 rshift 7 and
        s" fcmovb fcmove fcmovbefcmovu ???    ???    ???    ???    "
        7 ss. ;

: fda   ( addr op -- addr' )
    drop count dup h# c0 < if ( addr1 op1 )
    dup falu3 .s" dword " mod-r/m
    else
    dup h# e9 = if ( addr1 op1 )
        drop .s" fucompp"
    else
        dup fcmova sti.
    then
    then ;

: falu7  ( op -- )
        3 rshift 7 and
        s" faddp fmulp ???   ???   fsubrpfsubp fdivrpfdivp "
        6 ss. sspace ;

: fde   ( addr op -- addr' )
        drop count dup h# c0 <
        if      dup falu3 .s" word " mod-r/m
        else    dup h# d9 =
                if    .s" fcompp" drop
                else  dup falu7 sti.
                then
        then ;

: fcmovb  ( op -- )
        3 rshift 7 and
        s" fcmovnb fcmovne fcmovnbefcmovnu ???     fucomi  fcomi   ???     "
        8 ss. ;

: fdb   ( addr op -- addr' )
        drop count dup h# c0 <
        if      dup h# 38 and
                case    h# 00 of .s" fild    dword "    endof
                        h# 10 of .s" fist    dword "    endof
                        h# 18 of .s" fistp   dword "    endof
                        h# 28 of .s" fld     extended " endof
                        h# 38 of .s" fstp    extended " endof
                            dup ???
                endcase
                mod-r/m
        else
                case    h# e2 of .s" fnclex" endof
                        h# e3 of .s" fninit" endof
                            dup dup fcmovb sti.
                endcase
        then ;

: falu6  ( op -- )
        3 rshift 7 and
        s" ffree ???   fst   fstp  fucom fucomp???   ???   "
        6 ss. sspace ;

: fdd   ( addr op -- addr' )
        drop count dup h# c0 <
        if      dup h# 38 and
                case    h# 00 of .s" fld     double "  endof
                        h# 10 of .s" fst     double "  endof
                        h# 18 of .s" fstp    double "  endof
                        h# 20 of .s" frstor  "         endof
                        h# 30 of .s" fnsave  "         endof
                        h# 38 of .s" fnstsw  word   "  endof
                            dup ???
                endcase
                mod-r/m
        else    dup falu6 sti.
        then ;

: fdf   ( addr op -- addr' )
        drop count dup h# c0 <
        if      dup h# 38 and
                case    h# 00 of .s" fild    word "   endof
                        h# 10 of .s" fist    word "   endof
                        h# 18 of .s" fistp   word "   endof
                        h# 20 of .s" fbld    tbyte "  endof
                        h# 28 of .s" fild    qword "  endof
                        h# 30 of .s" fbstp   tbyte "  endof
                        h# 38 of .s" fistp   qword "  endof
                            dup ???
                endcase
                mod-r/m
        else    dup h# e0 =
                if      .s" fnstsw  ax " drop
                else    dup h# 38 and
                        case    h# 28 of .s" fucomip " sti. endof
                                h# 30 of .s" fcomip  " sti. endof
                                        ???
                        endcase
                then
        then ;

: gp6 ( addr op -- addr' )
        drop count dup 3 rshift
        7 and s" sldtstr lldtltr verrverw??? ???" 4 ss. 3 sspaces
        r/m16 ;

: gp7 ( addr op -- addr' )
        drop count dup 3 rshift
        7 and dup
        s" sgdt  sidt  lgdt  lidt  smsw  ???   lmsw  invlpg" 6 ss. 1 sspaces
        4 and 4 =
        if   r/m16
        else r/m16/32
        then ;

: btx.  ( n -- )
        3 rshift
        3 and s" bt btsbtrbtc" 3 ss. 4 sspaces ;

: gp8 ( addr op -- addr' )
        drop count dup btx.
        r/m16/32 imm8 ;

: lar ( addr op -- addr' )
        .s" lar     " drop r,r/m ;

: lsl ( addr op -- addr' )
        .s" lsl     " drop r,r/m ;

: lss ( addr op -- addr' )
        .s" lss     " drop r,r/m ;

: lfs ( addr op -- addr' )
        .s" lfs     " drop r,r/m ;

: lgs ( addr op -- addr' )
        .s" lgs     " drop r,r/m ;

: btx ( addr op -- addr' )
        btx. r/m,r ;

: sli ( addr op -- addr' )
        .s" shld    " drop r/m,r imm8 ;

: sri ( addr op -- addr' )
        .s" shrd    " drop r/m,r imm8 ;

: slc ( addr op -- addr' )
        .s" shld    " drop r/m,r .s" , cl" ;

: src ( addr op -- addr' )
        .s" shrd    " drop r/m,r .s" , cl" ;

: iml ( addr op -- addr' )
        .s" imul    " drop r,r/m ;

: cxc ( addr op -- addr' )
        .s" cmpxchg " 1 and to size r/m,r ;

: mvx ( addr op -- addr' )
        dup 8 and
        if      .s" movsx   "
        else    .s" movzx   "
        then
        1 and >r
        count mod/sib r>                        \ size bit
        if    swap reg32 .,                     \ word to dword case
              3 =
              if   reg16
              else .s" word ptr " mod-r/m
              then
        else  swap reg16/32 .,                  \ byte case
              3 =
              if   reg8
              else .s" byte ptr " mod-r/m
              then
        then ;

: xad ( addr op -- addr' )
        .s" xadd    " 1 and to size r/m,r ;

: bsf ( addr op -- addr' )
        .s" bsf     " drop r,r/m ;

: bsr ( addr op -- addr' )
        .s" bsr     " drop r,r/m ;

: cx8 ( addr op -- addr' )
        .s" cmpxchg8b " drop count r/m16/32 ;

: bsp ( addr op -- addr' )
        .s" bswap   " reg32 ;

\ : 0F.   ( addr op -- addr' )
\        drop count
\        case
\                0x00 of   gp6                             endof
\                0x01 of   gp7                             endof
\                0x02 of   .s" lar     " 1 to size r,r/m   endof
\                0x03 of   .s" lsl     " 1 to size r,r/m   endof
\                0x06 of   .s" clts    "                   endof
\                0x08 of   .s" invd    "                   endof
\                0x09 of   .s" wbinvd  "                   endof
\                0x20 of   mrc                             endof
\                0x21 of   mrd                             endof
\                0x22 of   mcr                             endof
\                0x23 of   mdr                             endof
\                0x24 of   mrt                             endof \ obsolete
\                0x26 of   mtr                             endof \ obsolete
\                0x30 of   .s" wrmsr   "                   endof
\                0x31 of   .s" rdtsc   "                   endof
\                0x32 of   .s" rdmsr   "                   endof
\                0x80 of   .s" jo      " rel16/32          endof
\                0x81 of   .s" jno     " rel16/32          endof
\                0x82 of   .s" jc      " rel16/32          endof
\                0x83 of   .s" jnc     " rel16/32          endof
\                0x84 of   .s" jz      " rel16/32          endof
\                0x85 of   .s" jne     " rel16/32          endof
\                0x86 of   .s" jbe     " rel16/32          endof
\                0x87 of   .s" ja      " rel16/32          endof
\                0x88 of   .s" js      " rel16/32          endof
\                0x89 of   .s" jns     " rel16/32          endof
\                0x8A of   .s" jpe     " rel16/32          endof
\                0x8B of   .s" jpo     " rel16/32          endof
\                0x8C of   .s" jnge    " rel16/32          endof
\                0x8D of   .s" jge     " rel16/32          endof
\                0x8E of   .s" jng     " rel16/32          endof
\                0x8F of   .s" jg      " rel16/32          endof
\                0x90 of   .s" seto    byte " r/m8         endof
\                0x91 of   .s" setno   byte " r/m8         endof
\                0x92 of   .s" setc    byte " r/m8         endof
\                0x93 of   .s" setnc   byte " r/m8         endof
\                0x94 of   .s" setz    byte " r/m8         endof
\                0x95 of   .s" setnz   byte " r/m8         endof
\                0x96 of   .s" setbe   byte " r/m8         endof
\                0x97 of   .s" seta    byte " r/m8         endof
\                0x98 of   .s" sets    byte " r/m8         endof
\                0x99 of   .s" setns   byte " r/m8         endof
\                0x9A of   .s" setp    byte " r/m8         endof
\                0x9B of   .s" setnp   byte " r/m8         endof
\                0x9C of   .s" setl    byte " r/m8         endof
\                0x9D of   .s" setge   byte " r/m8         endof
\                0x9E of   .s" setle   byte " r/m8         endof
\                0x9F of   .s" setg    byte " r/m8         endof
\                0xA0 of   .s" push    fs "                endof
\                0xA1 of   .s" pop     fs "                endof
\                0xA2 of   .s" cpuid      "                endof
\                0xA3 of   .s" bt      " 1 to size r/m,r   endof
\                0xA4 of   .s" shld    " r/m,r imm8        endof
\                0xA5 of   .s" shld    " r/m,r .s" , cl"   endof
\                0xA8 of   .s" push    gs "                endof
\                0xA9 of   .s" pop     gs "                endof
\                0xAA of   .s" rsm     "                   endof
\                0xAB of   .s" bts     " 1 to size r/m,r   endof
\                0xAC of   .s" shrd    " r/m,r imm8        endof
\                0xAD of   .s" shrd    " r/m,r .s" , cl"   endof
\                0xAF of   .s" imul    " r,r/m             endof
\                0xB0 of   .s" cmpxch  " 0 to size r/m,r   endof
\                0xB1 of   .s" cmpxch  " 1 to size r/m,r   endof
\                0xB2 of   .s" lss     " 1 to size r,r/m   endof
\                0xB3 of   .s" btr     " 1 to size r/m,r   endof
\                0xB4 of   .s" lfs     " 1 to size r,r/m   endof
\                0xB5 of   .s" lgs     " 1 to size r,r/m   endof
\                0xB6 of   .s" movzx   " 0 to size r,r/m   endof
\                0xB7 of   .s" movzx   " 1 to size r,r/m   endof
\                0xBA of   gp8                             endof
\                0xBB of   .s" btc     " 1 to size r/m,r   endof
\                0xBC of   .s" bsf     " 1 to size r,r/m   endof
\                0xBD of   .s" bsr     " 1 to size r,r/m   endof
\                0xBE of   .s" movsx   " 0 to size r,r/m   endof
\                0xBF of   .s" movsx   " 1 to size r,r/m   endof
\                0xC0 of   .s" xadd    " 0 to size r/m,r   endof
\                0xC1 of   .s" xadd    " 1 to size r/m,r   endof
\                0xC7 of   .s" cmpxchg8b " r/m16/32        endof
\                0xC8 of   .s" bswap   eax "               endof
\                0xC9 of   .s" bswap   ecx "               endof
\                0xCA of   .s" bswap   edx "               endof
\                0xCB of   .s" bswap   ebx "               endof
\                0xCC of   .s" bswap   esp "               endof
\                0xCD of   .s" bswap   ebp "               endof
\                0xCE of   .s" bswap   esi "               endof
\                0xCF of   .s" bswap   edi "               endof
\                ( else )  dup ???
\        endcase
\        ;

: F6.  ( addr op -- addr' )
\ ??
        >r count
        dup 3 rshift 7 and dup >r 
        s" testXXXXnot neg mul imuldiv idiv" 4 ss. 3 sspaces
        mod-r/m
        r> 0= if
                r@ 1 and if imm16/32
                         else imm8
                         then
              then
        r> drop ;

: FE.  ( addr op -- addr' )
        drop count
        dup 3 rshift 7 and
        case
                0 of .s" inc     "  endof
                1 of .s" dec     "  endof
                     ???
        endcase r/m8 ;

: FF.  ( addr op -- addr' )
        drop count
        dup 3 rshift 7 and
        case
                0 of .s" inc     "      endof
                1 of .s" dec     "      endof
                2 of .s" call    "      endof
                3 of .s" call    far "  endof
                4 of .s" jmp     "      endof
                5 of .s" jmp     far "  endof
                6 of .s" push    "      endof
                     ???
        endcase r/m16/32 ;

\ --------------------- conditional move ---------------

: set   ( adr op -- )
        .s" set"
        tttn 2 sspaces
        count r/m8 ;

: cmv   ( adr op -- )
        .s" cmov"
        tttn 1 sspaces
        count r,r/m ;

\ --------------------- MMX Operations -----------------

: mmx-size ( op -- )
        3 and s" bwdq" 1 ss. ;

: upl   ( adr op -- adr' )
        3 and s" punpcklbwpunpcklwdpunpckldq" 9 ss. r,r/m ;

: uph   ( adr op -- adr' )
        3 and s" punpckhbwpunpckhwdpunpckhdq" 9 ss. r,r/m ;

: cgt   ( adr op -- adr' )
        .s" pcmpgt" mmx-size r,r/m ;

: ceq   ( adr op -- adr' )
        .s" pcmpeq" mmx-size r,r/m ;

: psh.  ( op -- )
        h# 30 and
        case
             h# 10 of .s" psrl" endof
             h# 20 of .s" psra" endof
             h# 30 of .s" psll" endof
        endcase ;

: gpa   ( adr op -- adr' )
        >r count dup psh. r> mmx-size 2 sspaces mreg imm8 ;

: puw   ( adr op -- adr' )
        .s" packusdw " drop r,r/m ;

: psb   ( adr op -- adr' )
        .s" packsswb " drop r,r/m ;

: psw   ( adr op -- adr' )
        .s" packssdw " drop r,r/m ;

: mpd   ( adr op -- adr' )
        .s" movd    " drop count mod/sib
        swap mreg ., 3 =
        if   reg32
        else mod-r/m
        then ;

: mdp   ( adr op -- adr' )
        .s" movd    " drop count mod/sib
        3 =
        if   swap reg32
        else swap mod-r/m
        then ., mreg ;

: mpq   ( adr op -- adr' )
        .s" movq    " drop r,r/m ;

: mqp   ( adr op -- adr' )
        .s" movq    " drop r/m,r ;

: shx   ( adr op -- adr' )
        dup psh. mmx-size 2 sspaces r,r/m ;

: mll   ( adr op -- adr' )
        .s" pmullw  " drop r,r/m ;

: mlh   ( adr op -- adr' )
        .s" pmulhw  " drop r,r/m ;

: mad   ( adr op -- adr' )
        .s" pmaddwd " drop r,r/m ;

: sus   ( adr op -- adr' )
        .s" psubus" mmx-size r,r/m ;

: sbs   ( adr op -- adr' )
        .s" psubs" mmx-size sspace r,r/m ;

: sub   ( adr op -- adr' )
        .s" psub" mmx-size 2 sspaces r,r/m ;

: aus   ( adr op -- adr' )
        .s" paddus" mmx-size r,r/m ;

: ads   ( adr op -- adr' )
        .s" padds" mmx-size sspace r,r/m ;

: add   ( adr op -- adr' )
        .s" padd" mmx-size 2 sspaces r,r/m ;

: pad   ( adr op -- adr' )
        .s" pand    " drop r,r/m ;

: por   ( adr op -- adr' )
        .s" por     " drop r,r/m ;

: pan   ( adr op -- adr' )
        .s" pandn   " drop r,r/m ;

: pxr   ( adr op -- adr' )
        .s" pxor    " drop r,r/m ;

\ -------------------- Opcode Table --------------------

: ops h# 10 0 do ' , loop ;

create op-table2

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

ops  gp6 gp7 lar lsl  ??? ??? clt ???  inv wiv ??? ud2  ??? ??? ??? ???  \ 0
ops  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  \ 1
ops  mrc mrd mcr mdr  mrt ??? mtr ???  ??? ??? ??? ???  ??? ??? ??? ???  \ 2
ops  wmr rtc rmr rpc  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  \ 3

ops  cmv cmv cmv cmv  cmv cmv cmv cmv  cmv cmv cmv cmv  cmv cmv cmv cmv  \ 4
ops  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  ??? ??? ??? ???  \ 5
ops  upl upl upl puw  cgt cgt cgt psb  uph uph uph psw  ??? ??? mpd mpq  \ 6
ops  ??? gpa gpa gpa  ceq ceq ceq ems  ??? ??? ??? ???  ??? ??? mdp mqp  \ 7

ops  lbr lbr lbr lbr  lbr lbr lbr lbr  lbr lbr lbr lbr  lbr lbr lbr lbr  \ 8
ops  set set set set  set set set set  set set set set  set set set set  \ 9
ops  pss pps cpu btx  sli slc ??? ???  pss pps rsm btx  sri src ??? iml  \ A
ops  cxc cxc lss btx  lfs lgs mvx mvx  ??? ud1 gp8 btx  bsf bsr mvx mvx  \ B

ops  xad xad ??? ???  ??? ??? ??? cx8  bsp bsp bsp bsp  bsp bsp bsp bsp  \ C
ops  ??? shx shx shx  ??? mll ??? ???  sus sus ??? pad  aus aus ??? pan  \ D
ops  ??? shx shx ???  ??? mlh ??? ???  sbs sbs ??? por  ads ads ??? pxr  \ E
ops  ??? ??? shx shx  ??? mad ??? ???  sub sub sub ???  add add add ???  \ F

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

: 0F.  ( adr code -- )
        drop count dup
        dup h# 70 and h# 50 h# 80 within to mmx-reg
        cells op-table2 + @ execute
        0 to mmx-reg ;

create op-table

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

ops  alu alu alu alu  ala ala pss pps  alu alu alu alu  ala ala pss 0F.  \ 0
ops  alu alu alu alu  ala ala pss pps  alu alu alu alu  ala ala pss pps  \ 1
ops  alu alu alu alu  ala ala es: daa  alu alu alu alu  ala ala cs: das  \ 2
ops  alu alu alu alu  ala ala ss: aaa  alu alu alu alu  ala ala ds: aas  \ 3

ops  inc inc inc inc  inc inc inc inc  dec dec dec dec  dec dec dec dec  \ 4
ops  psh psh psh psh  psh psh psh psh  pop pop pop pop  pop pop pop pop  \ 5
ops  psa ppa bnd arp  fs: gs: d16 a16  psi mli psi mli  inb isd osb osd  \ 6
ops  bra bra bra bra  bra bra bra bra  bra bra bra bra  bra bra bra bra  \ 7

ops  ali ali ??? ali  txb txb txb txb  mov mov mov mov  mrs lea msr 8F.  \ 8
ops  xga xga xga xga  xga xga xga xga  cbw cdq cis w8f  psf ppf sah lah  \ 9
ops  mv1 mv1 mv2 mv2  mvs mvs cps cps  tst tst sts sts  lds lds scs scs  \ A
ops  mri mri mri mri  mri mri mri mri  mri mri mri mri  mri mri mri mri  \ B

ops  shf shf rtn rtn  lxs lxs mvi mvi  ent lev rtf rtf  nt3 int nto irt  \ C
ops  shf shf shf shf  aam aad ??? xlt  fd8 fd9 fda fdb  fdc fdd fde fdf  \ D
ops  lup lup lup lup  inp inp otp otp  jsr jmp cis jmp  ind ind otd otd  \ E
ops  lok ??? rpz rep  hlt cmc F6. F6.  clc stc cli sti  cld std FE. FF.  \ F

\     0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F

: dis-op  ( adr -- adr' )
        0>s
        false to prefix-op           \ SMuB
        count
        dup 1 and to size
        dup cells op-table +  @ execute
        prefix-op 0=
        if default-16bit? 0=
           if   false to 16-bit-data
                false to 16-bit-addr
           else true  to 16-bit-data
                true  to 16-bit-addr
           then
        then ;

0 value next-inst

: inst  ( adr -- adr' )
        dup to next-inst
        cols h# 29 <
        if      dis-op
                s-buf count type
        else    dup dis-op
                over base-addr - ." ( " hex. ( 6 h.r )  ." ) "
                comment-col col s-buf count type ."  \ "
        dup rot
                2dup - h# 10 u> abort" decompiler error"
                do i c@ hex. ( 2 h.n ) loop
        then
        dup to next-inst ;

forth definitions

: disasm ( addr u -- ) \ gforth
    over + >r
    begin
          dup r@ u<
    while
          cr inst
    repeat
    cr r> 2drop ;
    
\ ' disasm is discode

only forth also definitions

\ diagnostics routines
\ in its own vocabulary

base @ hex

only forth also system also
diagnostic definitions

\ stop test if 'q' key is pressed
: stop-test? ( -- flg )  key? dup if key [char] q = or then ;

\ test random number generator


[defined] doc-probe [if]

\ for testing
create mybuf 420 allot
\ initialize DoC 

doc-init

: test-doc-1
\ dump 80 blocks from DOC
  80 0 do
    mybuf i doc-block@ drop
    mybuf 400 dump
    stop-test? if leave then
 loop
;

: test-doc-2
\ dump extra pages rom DOC
  800 0 do
    mybuf f + f invert and dup 
    10 i  doc-extra@
    10 dump
    stop-test? ( [char] q = ) if leave then
  loop
;


: test-doc-3
\ dump page from DOC with user interruption
4000 0 do
    here 1000 + ff invert and dup i doc-page@ drop
    ." page #" i u. cr 200 dump
    stop-test? ( [char] q = ) if leave then
    loop
;

[then]

[defined] scan2asc  [if]

: keybd-test 
\ wait for keypress
\ display
\ end if key pressed on console
begin 
    begin keybd-chr? until
    emit
 key? until
;
[then]

[defined]  lineto  [if]
: gey0 ymax  2/ ; : gex0 xmax  2/ ;

: godseye  ( -- )
   gex0 gey0 min  0  do
      i         gey0         gotoxy
      gex0      gey0 i -     lineto
      xmax i -  gey0         lineto
      gex0      gey0 i +     lineto
      i         gey0         lineto
   4 +loop
;

: godseyes ( -- )
    begin
        rand h# 0ffff and fg ! godseye
        scroll
    stop-test? until
;

: graph-test-1
 begin
   rand abs xmax  mod
   rand abs ymax  mod
   rand h# 0ffff  and
   plot-point
 stop-test? until
;

: graph-test-2 ( -- )
 0 0 gotoxy
 begin
    rand abs xmax 1+ mod
    rand abs ymax 1+ mod
    rand abs h# 0ffff and fg !
    lineto
 stop-test? until
;

[then]

[defined]  font8x16 [if]
: 8.b ( u -- )
   base @ >r 2 base !
   0 <# # # # # # # # # #> type
   r> base !
;

: dump-font ( c -- )
cr
/font-size 3 rshift *
font8x16 +
/font-height 0 do
   i over + c@
   8.b cr
loop
drop
;

 
0 value chnext

: ch-next 
 chnext dup h# ff =
 if 0 else chnext 1+ then
to chnext
;

: graph-test-3
 #rows 0 do
    #cols 0 do
       j i ch-next putch
    loop
loop
;

: graph-test-4
 #rows 0 do
    i 1 [char] 0 i d# 10 mod +
    putch
    i 0 [char] 0 i d# 10 / +
    putch
loop
#cols 0 do
   1 i [char] 0 i d# 10 mod +
   putch
   0 i [char] 0 i d# 10 / +
   putch
loop
;

: graph-test-5
begin
 h# 100 0 do
    i h# 10 mod fg!
    0 0 i putch
loop
stop-test? until
;

: blt-test-1
  ymax 0 do
    xmax 0 do
        i j ch-next draw-char
    /font-width +loop
  /font-height +loop
;

: blt-test-2
begin
 h# 100 0 do
    i h# 10 mod fg!
    rand xmax 1+ mod /font-width - 0 max
    rand ymax 1+ mod /font-height - 0 max
    i draw-char
loop
stop-test? until
;

: blt-test-3
 #rows 0 do
    #cols 0 do
       j i position ch-next draw-char
    loop
loop
;

: blt-test-4
    0 0 begin
        rand h# 0ffff and fg !
        rand abs xmax 1+ mod
        rand abs ymax 1+ mod
        2swap 2over line
    stop-test? until
    2drop
;

: cursor-test-1
begin
 rand abs xmax 1+ mod 20 - 0 max
 rand abs ymax 1+ mod 20 - 0 max
 rand h# ffff and gx1-cursor-fg!
 gx1-cursor-position!
key? until
;


[then]

: dcr? ( u --)  dup u. [char] = emit gx1-dcr@ u. cr ;

: dump-dcr ( -- )
cr
4 dcr? 8 dcr? c dcr? 
10 dcr? 14 dcr? 18 dcr?
20 dcr? 24 dcr? 28 dcr? 
30 dcr? 34 dcr? 38 dcr? 3c dcr?
40 dcr? 44 dcr? 48 dcr? 4c dcr?
cr
;


previous previous definitions

base !

\ dwnldr.fth
\ Simple downloader
\  Redirect keyv to read from memory buffer.
\ normal quit proceeds when buffer is exhausted,
\ restores previously saved keyv. 
\ Incomplete compilation may hang ( quit? )
\ ixfer xfer
 
base @ hex 
only forth definitons
also system

\ start of buffer
variable sob   internal
variable eob    \ end of buffer
80000 constant /xfer-buffer  \ buffer size  

\ create buffer /buffer allot
tom /xfer-buffer - to tom
tom constant xfer-buffer
xfer-buffer /xfer-buffer 1- 13 fill

\ save area for key execution vector
variable keyvsave

\ redirect key reads to buffer.
\ resets on exhaustion of buffer
: keym ( -- ch)
sob @ eob @ < if
   sob @ dup c@
   swap 1+ sob !
else
   keyvsave @ dup keyv !
   execute 
then
;

\  redirect key to fetch from buffer 
: load ( addr len -- )
over + eob ! sob !
keyv @ keyvsave !
['] keym keyv !
;

\  add input to buffer until EOI (ctrl-Z) is seen
: rcv  ( addr -- addr len)
 0 over begin
    key dup 4 <> while
    dup emit over c! 1+ 
    swap 1+ swap
 repeat
 2drop
;

: fill-buffer ( -- )  xfer-buffer rcv load ;

\ intel hex record loader.
\ address and checksum are ignored

: getc ( -- c) key dup 3 = if drop abort then ;

\ convert and emit 
: ce ( char -- ) dup a < if 30 else 37 then + emit ;
: 2.r ( n -- )   ff and 10 /mod ce ce ;
: 4.r ( n -- )   0 100 um/mod 2.r 2.r ;

: 1# ( -- c )  getc 30 - dup 9 > if 7 - then 0 max f min ;
: 2# ( -- c )  1# 4 lshift  1# + ;
: 4# ( -- u )  2# 8 lshift 2# or ;

\ clear input

: drain ( -- )  begin key? while key drop repeat ;

variable chksum

: ihex< ( addr -- )
   cr ." ready to load, offset = " dup u. cr
   begin
      getc 3a = if         \ ":"
         2# over +         \ sa ea
         4# drop           \ ignore address
         2# 1 = if 2drop drain exit then
         tuck swap do
            2#  i c!
         loop
         [char] . emit
         2# drop          \ ignore chksum`
      then
   again 
;
external


: ihex> ( addr n -- )
   over + swap                  \ bounds
   begin                        \
      cr                        \
      2dup 20 + min             \ next line of output up to 32 bytes long
      swap                      \ start and end
      ." :"                     \ begin the record
      2dup -                    \ find out # of bytes in this record
      dup chksum !              \ begin chksum computation
      2.r                       \ # bytes, in two digit field
      dup 8 lshift ff and       \
      over ff and + chksum +!   \ add start address to chksum
      dup 4.r                   \ start address, four digit field
      ." 00"                    \ record type
      >r dup r>                 \ make start stop #s for do loop
      do                        \
        i c@ dup 2.r            \ hex byte, two digit field
        chksum +!               \ update chksum
      loop                      \
      chksum 1+ c@ negate 2.r   \ checksum
   2dup = until                 \ end
   cr ." :00000001FF" cr        \ tack on end record
   2drop 
;


\ load intel hex to address 
: ixfer ( addr -- )
  cr cr ." Send intel hex data..." cr    
  fill-buffer ihex<
;

:  xfer ( -- )
  cr cr ." Send ascii data..." cr    
  fill-buffer 
;

module

base !



