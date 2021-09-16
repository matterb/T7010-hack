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

