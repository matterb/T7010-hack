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
: cell- ( u -- 'u) -1 cells + ;
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


: c@+ ( addr -- addr+1 c )
  dup 1+ swap c@
;

: c!+ ( addr u -- addr+1)
 over c! 1+
;

\ @@C 
: bounds ( addr len -- addr1 addr2 ) over + swap ;
\ @@C true if min <= n < max 
: between ( n min max -- flg )
 >r over > invert swap
 r> > invert and
;

\ @@C write u words at address a
: wfill ( a u wrd -- )
  swap 2* ( a wrd n )
  rot tuck + swap ( wrd e a )
  do
    dup i w!
    2 +loop
  drop
;

\ @@C write u dwords at address a
: lfill ( a u x -- )
  swap 4 *   ( a x n )
  rot tuck + swap
  do
     dup i !
     4 +loop
  drop
;

\ @@C convert word to bytes hi lo
: wbsplit ( w -- b1 b0 )
 dup 8 rshift
 swap ff and
;

\ @@C convert dword to words hi lo
: lwsplit ( u -- w1 w0 )
 dup 10 rshift
 swap ffff and
;

\ @@C convert dword to bytes b4 b3 bb3 b1 b0
: lbsplit ( l -- b3 b2 b1 b0 )
 lwsplit >r
 wbsplit r> wbsplit
;

\ @@C combine 2 bytes b1 b0 to a word 
: bwjoin ( b1 b0 -- w ) swap 8 lshift or ;

\ @@C combine two words w1 w0 to a dword
: wljoin ( w1 w0 -- b ) swap 10 lshift or ;

\ @@C combine 4 bytes b3 b2 b1 b0 to a dword
: bljoin ( b3 b2 b1 b0 -- l ) bwjoin >r bwjoin r> wljoin ;

\ alignment words
\ @@C  align u to a multiple of n i.e u1 n mod = 0
: round-up ( u n  -- u1 ) 1- tuck + swap invert and ;


\ define b#, o#  d# and h#
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

