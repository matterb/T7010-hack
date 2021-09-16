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

