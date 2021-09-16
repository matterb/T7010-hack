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
 
