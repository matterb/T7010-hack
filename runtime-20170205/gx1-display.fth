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

