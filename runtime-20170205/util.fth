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

