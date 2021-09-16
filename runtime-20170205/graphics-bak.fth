\ @@FILE:graphics.fth
\ @@REQUIRES:gx1cpu.fth
\ graphics utilities
\ Phil Koopman FD
\ 
\ export point,line,gotoxy xmax,ymax

base @ hex

only forth also
system definitions

: rgb>565 ( r g b -- w )
 3 rshift
 swap 2 rshift 5 lshift or
 swap 3 rshift d# 11 lshift or
;

create colours565#
00 00 00 rgb>565 , \ 0 Black
00 00 aa rgb>565 , \ 1 Dark blue
00 aa 00 rgb>565 , \ Dark green
00 aa aa rgb>565 , \ Dark cyan
aa 00 00 rgb>565 , \ Dark red
aa 00 aa rgb>565 , \ Dark magenta
aa 55 aa rgb>565 , \ Brown
aa aa aa rgb>565 , \ Light gray
55 55 55 rgb>565 , \ Dark gray
55 55 ff rgb>565 , \ Light blue
55 ff 55 rgb>565 , \ Light green
55 ff ff rgb>565 , \ Light cyan
ff 55 55 rgb>565 , \ Light red (pink)
ff 55 ff rgb>565 , \ Light magenta
ff ff 55 rgb>565 , \ Light yellow
ff ff ff rgb>565 , \ White

\ calulate framebuffer offset for pixel x,y
\ fbuf + y * pixels + x * bytes/pixel
: pixelxy ( x y -- a )
   fb-line-delta@ *
   swap bytes/pixel * + fbuf +
;

also forth definitions

variable colour
variable fg         \ foreground 
variable bg         \ background

\ text cursor location
\ TODO hardware cursor
variable col#
variable row# 

0 fg ! 0 bg ! 0 colour !
0 row# ! 0 col# !

: colour! ( idx -- ) cells colours565# + @ colour ! ;
: fg! ( idx -- ) cells colours565# + @ fg ! ;
: bg! ( idx -- ) cells colours565# + @ bg ! ;

: plot-point ( x y colour -- ) -rot pixelxy ( c a ) fb! ;

: clearscr ( -- )       \ screen filled with colour stored in colour variable
 fbuf fb-lines@ 0 do
    i fb-line-delta@ * over +
    fb-pixels/line colour @ fb-fill
 loop
 drop
 ;

: clr ( -- )   \ black background white foreground
  0 bg! clearscr h# 0f fg!
;

\ screen limits in pixels

: xmax ( -- u) fb-pixels/line 1- ;
: ymax ( -- u ) fb-lines@ 1- ;

previous definitions
 
h# ffff value #colours

: #rows fb-lines@ /font-height / ;          \  value #rows
: #cols fb-pixels/line /font-width  / ;     \  value #cols

\ graphics cursor location

variable xnow 0 xnow !
variable ynow 0 ynow !

\ for bresenham line drawing

variable incr1  variable incr2
variable deltax variable deltay

also forth definitions
: gotoxy ( x y --) ynow ! xnow ! ;

\ pixel at (x,y) 
: point ( x y -- ) colour @ plot-point ;

previous definitions

: b-point ( x y delta --) >r 2dup point r> ;

: +x ( x1 y1 delta -- x2 y2 delta ) rot 1+ -rot ;

: -x ( x1 y1 delta -- x2 y2 delta ) rot 1- -rot ;

: +y ( -- ) swap 1+ swap ;

: -y ( -- ) swap 1- swap ;

\ bresenham line for 0 < slope < 1

: lineoct1  ( newx newy --)
 deltay @ 2* incr1 ! 
 deltay @ deltax @ - 2* incr2 !
 over xnow @ > if
    2drop xnow @ ynow @ then
 2dup point
 incr1 @ deltax @ -
 deltax @ 0 do 
   dup 0< if
     +x b-point incr1 @ + 
   else
     +x +y b-point incr2 @ + then
 loop
  drop 2drop
;

: lineoct2 ( newx newy --)
\ 1 < m < infty
 deltax @ 2* incr1 ! 
 deltax @ deltay @ - 2* incr2 !
 dup ynow @ > if
    2drop xnow @ ynow @ then
 2dup point
 incr1 @ deltay @ -
 deltay @ 0 do 
   dup 0< if
     +y b-point incr1 @ + 
   else
     +x +y b-point incr2 @ + then
 loop
  drop 2drop
;

: lineoct3 ( newx newy --)
\ -infty < m < -1
 deltax @ 2* incr1 ! 
 deltax @ deltay @ - 2* incr2 !
 dup ynow @ > if
    2drop xnow @ ynow @ then
 2dup point
 incr1 @ deltay @ -
 deltay @ 0 do 
   dup 0< if
     +y b-point incr1 @ + 
   else
     -x +y b-point incr2 @ + then
 loop
  drop 2drop
;

: lineoct4  ( newx newy --)
\ -1 < m < 0
 deltay @ 2* incr1 ! 
 deltay @ deltax @ - 2* incr2 !
 over xnow @ > if
    2drop xnow @ ynow @ then
 2dup point
 incr1 @ deltax @ -
 deltax @ 0 do 
   dup 0< if
     +x b-point incr1 @ + 
   else
     +x -y b-point incr2 @ + then
 loop
  drop 2drop
;

: linev ( newx newy --     vertical line )
 dup ynow @ > if
  2drop xnow @ ynow @ then 
 2dup point 0 deltay @ 0 do
    +y b-point
 loop
 drop 2drop
 ;

: lineh ( newx newy --     horizontal line )
 over xnow @ > if
  2drop xnow @ ynow @ then 
 2dup point 0 deltax @ 0 do
    +x b-point
 loop
 drop 2drop 
 ;

also forth definitions

: lineto  ( xnew ynew -- )
 2dup 
 over xnow @ - dup abs deltax !
 over ynow @ - dup abs deltay !
 xor 0< deltay @ 0<> if
   deltax @ 0<> if
   if
     deltax @ deltay @ > if
         lineoct4  else lineoct3 then
   else
     deltax @ deltay @ > if
         lineoct1 else  lineoct2 then
   then
 else
     drop linev then
 else
     drop lineh then
 gotoxy
;

: line ( x1 y1 x2 y2 -- ) gotoxy lineto ;
  
previous previous definitions


base !
 
