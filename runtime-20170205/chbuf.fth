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

