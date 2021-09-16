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

