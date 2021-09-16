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

