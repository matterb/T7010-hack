\ audio.fth
\ audio driver for CS5530
\ based on openfirmware driver in misc\audio

: <= > 0= ;
: dma-alloc allocate ;
: dma-free free ;

base @ hex
only forth also system definitions


false value fatal-error?
d# 48000 constant frame-rate
frame-rate d# 1000 / constant f/ms


10 0 12 3 pci-dev pci@ value f3bar#

\ PRD buffers
begin-structure /prd-entry  ( prd-entry )
   field: >prd-addr
   field: >prd-length
   wfield: >prd-status
end-structure

h# 8000 constant eot
h# 4000 constant eop
h# 2000 constant prd-loop

3 constant #prd-entry
/prd-entry #prd-entry * constant /prds
0 value prd-in
0 value prd-out


: >prd-in-entry  ( entry# -- phys-adr )  
 /prd-entry * prd-in +  
;

: set-prd-in-entry  ( phys-adr len flags entry# -- )
   >prd-in-entry  >r
   r@ >prd-status w!
   r@ >prd-length w!
   r> >prd-addr   !
;

: >prd-out-entry   ( entry# -- phys-adr )  
  /prd-entry * prd-out +  
;

: set-prd-out-entry  ( phys-adr len flags entry# -- )
   >prd-out-entry  >r
   r@ >prd-status w!
   r@ >prd-length w!
   r> >prd-addr   !
;

: set-prd-in-flags-len  ( len flags entry# -- )
   >prd-in-entry tuck >prd-status w!
   >prd-length w!
;

: set-prd-out-flags-len  ( len flags entry# -- )
   >prd-out-entry tuck >prd-status w!
   >prd-length w!
;

\ DMA buffers
h# f000 constant /dma-buf
#prd-entry 1- constant #dma-buf
/dma-buf #dma-buf * constant /bufs
0 value cur-dma-in
0 value cur-dma-out

0 value dma-in-phys

0 value dma-out-phys

: next-dma-entry  ( cur -- next )  1+ dup  #dma-buf  =  if  drop 0  then  ;

: cur-dma-in+   ( -- )  cur-dma-in  next-dma-entry to cur-dma-in   ;

: cur-dma-out+  ( -- )  cur-dma-out next-dma-entry to cur-dma-out  ;

\ : >dma-in-entry  ( entry# -- virt-adr )  /dma-buf * dma-in +  ;

: >dma-in-phys   ( entry# -- phys-adr )  /dma-buf * dma-in-phys +  ;

\ : >dma-out-entry  ( entry# -- virt-adr )  /dma-buf * dma-out-virt +  ;

: >dma-out-phys   ( entry# -- phys-adr )  /dma-buf * dma-out-phys +  ;

\ buffer allocation
: init-prds-in  ( -- )
   #dma-buf 0 do
      i >dma-in-phys   /dma-buf  eop  i  set-prd-in-entry
   loop
   prd-in   0  prd-loop  #dma-buf  set-prd-in-entry
;

: init-prds-out  ( -- )
   #dma-buf 0 do
      i >dma-out-phys  /dma-buf  eop  i  set-prd-out-entry
   loop
   prd-out  0  prd-loop  #dma-buf  set-prd-out-entry
;

: init-buffers  ( -- )
\   \ allocate data buffers for bus mastering
   /bufs dma-alloc to dma-in-phys
   /bufs dma-alloc to dma-out-phys

   \ allocate PRDs
   /prds dma-alloc to prd-in
   /prds dma-alloc to prd-out

   \ init PRDs
   init-prds-in
   init-prds-out
;
: free-buffers  ( -- )
   dma-in-phys dma-free
   dma-out-phys dma-free

   prd-in dma-free
   prd-out dma-free
;
\ AC '97 CODEC controller stuff
begin-structure /dma-regs
   cfield: >dma-cmd
   cfield: >dma-status
   2 +  ( reserved )
   field: >dma-prd
end-structure


: stat@ ( -- val ) f3bar# 8 + @ ;
: stat! ( val -- ) f3bar# 8 + ! ;
: cmd@  ( -- val ) f3bar# c + @ ;
: cmd-wait  ( -- ) \ wait for ac'97 ready
 true d# 192 0 do
   cmd@ h# 10000 and 0= if
      drop false leave then
   loop
 to fatal-error? 
;


: cmd! ( val -- ) cmd-wait f3bar# c + ! ;
: rst h# 400000 stat! 1 udelay 0 stat! ;

: codec!  ( val reg --) d# 24 lshift or cmd! ;
: codec@  ( reg -- val) 
    \ 192 is a ~60us delay to wait until the status tag is cleared.
   d# 192 0 do
      stat@  h# 30000 and  h# 10000 =  if  leave  then
   loop
   h# 80 or  d# 24 lshift cmd!
   \ wait until status valid and status tag is set.
   true d# 192 0 do
      stat@  h# 30000 and  h# 30000 =
      if  drop false leave  then
   loop
   to fatal-error?
   stat@ ffff and
;


: >dma-regs  ( channel# -- adr ) 
  /dma-regs *  h# 20 +  f3bar# +
;

: set-dma  ( prd-phys-adr in? channel# -- )
   >dma-regs >r
   8 and r@ >dma-cmd c!   \ Set direction bit
   r@ >dma-status c@ drop \ Clear errors
   r> >dma-prd    !      \ Set address
;

: dma-go  ( channel# -- )  >dma-regs  dup c@  1 or  swap c!  ;
: dma-wait  ( channel# -- )
   >dma-regs  begin  dup >dma-status c@ 1 and  until drop   ( regs-adr )
;
: dma-done  ( channel# -- )
   >dma-regs dup >dma-cmd c@ 1 invert and swap >dma-cmd c!
;


\  AC '97 CODEC stuff
: set-master-volume  ( value -- )
   \ XXX handle balance too
   ( db>volume )  2 codec!
;
: set-headphone-volume  ( value -- )
   ( db>volume )  4 codec!
;
: set-mono-volume  ( value -- )
   ( db>volume )  6 codec!
;
: set-tone-volume  ( value -- )
   \ XXX handle balance too
   ( db>volume )  8 codec!
;
: set-pcbeep-volume  ( value -- )
   ( db>volume )  a codec!
;
: set-pcm-gain  ( db -- )
   ( db>volume )  h# 18 codec!
;
: set-record-gain  ( db -- )
   ( db>volume )  h# 1c codec!
;
: set-linein  ( -- )
   404 h# 1a codec!
;
: set-linein-volume  ( value -- )
   ( db>volume )  10 codec!
;
: enable-playback  ( -- )
   h# 808 set-linein-volume
;
: disable-playback  ( -- )
   h# 8808 set-linein-volume
;

0 value vendor-id
: get-vendor-id  ( -- )
   h# 7c codec@ 8 lshift 
   h# 7e codec@ 8 rshift or
  to vendor-id
;

d# 48000 value sample-rate
0 value s/ms

: open-in  ( -- )
   sample-rate dup d# 1000 / to s/ms
   32  codec!
   0 set-record-gain
;
: close-in  ( -- )
   h# 8000 set-record-gain      \ mute
;
: open-out  ( -- )
   disable-playback
   sample-rate d# 1000 / to s/ms
   frame-rate 2c  codec!
   h# 808 set-pcm-gain          \ enable line-out
   0 set-master-volume
   0 set-mono-volume
;

: close-out  ( -- )
   h# 8808 set-pcm-gain          \ mute
   h# 8000 set-master-volume
   h# 8000 set-mono-volume
;

\   start from here and FIX it

200 value timeout-threshold
0 value #non-eop
false value audio-alarm-installed?

defer audio-in-hook     ' (null) to audio-in-hook
: audio-alarm  ( -- )
   1 >dma-regs >dma-status c@ 1 and
   if
      audio-in-hook
      cur-dma-in+
      0 to #non-eop
   else
      #non-eop 1+ to #non-eop
   then
;
: .audio-in-timeout  ( -- )  ." FATAL ERROR: Audio input timeout." cr  ;
: audio-in-timeout?  ( -- timeout? )  
  #non-eop timeout-threshold >  
;

: install-audio-alarm  ( -- )  
  ['] audio-alarm d# 18 \ alarm  
;

: ?uninstall-audio-alarm  ( -- )
   audio-alarm-installed?  if
      false to audio-alarm-installed?
      1 dma-done
      ['] audio-alarm 0  \ alarm
   then
;
: ?install-audio-alarm  ( -- )
   audio-alarm-installed? 0=  if
      init-prds-in prd-in true 1 set-dma
      0 to cur-dma-in
      0 to #non-eop
      true to audio-alarm-installed?
      install-audio-alarm
      1 dma-go
   then
;

0 value badr
0 value blen
0 value blen/dma-buf
0 value #eop        \ # of eop expected
0 value last-eop
0 value /last-dma
4 value /sample    \ # bytes per sample
defer sample@       ' @ to sample@
defer sample!       ' ! to sample!
0 value sample>
defer >sample       ' (null) to >sample

: setup-prds-in  ( adr len -- )
   to blen to badr
   /dma-buf 4 /sample / / to blen/dma-buf
   blen blen/dma-buf /mod swap 0>  if  1+  then
   to #eop
;
: conv-in-sample  ( src dst len -- )
   0 ?do                    ( src dst )
      over @ >sample        ( src dst sample )
      over i + sample!      ( src dst )
      swap 4 + swap         ( src' dst )
   /sample +loop  2drop     ( --  )
 ;

0 value eop-cnt

: (audio-in)  ( -- )
   eop-cnt 1+ to eop-cnt
   cur-dma-in >dma-in-phys badr blen blen/dma-buf min dup >r conv-in-sample
   badr r@ + to badr
   blen r> - to blen
   eop-cnt #eop =  if  ['] (null) to audio-in-hook  then
;

: audio-in  ( adr len -- actual )
   tuck  setup-prds-in              ( actual )
   dup blen/dma-buf mod ?dup  0=  if  blen/dma-buf  then  to  /last-dma

   0 to eop-cnt
   ['] (audio-in) to audio-in-hook
   ?install-audio-alarm

   begin  eop-cnt #eop =  audio-in-timeout? or  until
   audio-in-timeout?  if
      .audio-in-timeout
      drop 0
   then
;

\ fload ${BP}/dev/mulaw.fth
\ fload ${BP}/dev/mediagx/convert.fth

: convert-frequency

;

: mono>stereo

;

: lin8mono

;

: lin16mono

;

: lin16stereo

;


: ulaw8mono 

;

0 value stereo?     \ source buffer in stereo?
0 value ls      \ previous left sample value
0 value rs      \ previous right sample value
0 value #lc     \ internal counts til next left input sample
0 value #rc     \ internal counts til next right input sample
0 value outs        \ sample source buffer
0 value outd        \ sample dest buffer
0 value outl        \ sample length (# samples in source buffer)

: setup-prds-out  ( adr len -- )
   to blen to badr
   0 to cur-dma-out         \ dma buffer to output from
   init-prds-out
   /dma-buf 4 /sample / / s/ms * f/ms / to blen/dma-buf
   blen blen/dma-buf /mod over 0>  if  1+  then ( mod #eop )
   dup to #eop                  ( mod #eop )
   #dma-buf <=  if              ( mod )
      ?dup  0=  if  /dma-buf  then      ( last-dma-len )
      eop eot or #eop 1- set-prd-out-flags-len  ( )
   else                     ( mod )
      drop                  ( )
   then                     ( )
   #eop #dma-buf - to last-eop
;

0 value r
: conv-out-sample2  ( src dst len -- )
   rot swap ( bounds ?) ?do                          ( dst )
      f/ms r + s/ms /mod swap to r              ( dst #repeat )
      4 * 2dup + -rot                            ( dst' dst #repeat*4 )
      i sample@ lfill                           ( dst' )
   /sample +loop drop                           ( )
;

: conv-out-sample  ( src dst len -- )
   /sample / to outl to outd to outs
   ls #lc s/ms f/ms outs outl outd sample> convert-frequency
   to #lc to ls
   stereo?  if
      rs #rc s/ms f/ms outs 2 + outl outd 2 + sample> convert-frequency
      to #rc to rs
   else
      outd outl mono>stereo
   then
;
: audio-out  ( adr len -- actual )
   prd-out false 0 set-dma          ( adr len )
   tuck setup-prds-out              ( actual )

   badr sample@  stereo?  if  lwsplit  else  0 swap  then
   to ls to rs
   s/ms dup to #lc to #rc

   \ advance slen, and badr
\   badr dma-out-virt blen blen/dma-buf #dma-buf * min  ( actual src dst len )
   dup >r conv-out-sample           ( actual )
   blen r@ - to blen                ( actual )
   badr r> + to badr                ( actual )

   0 dma-go                 ( actual )
   #eop 1+ 1 ?do                ( actual )
      0 dma-wait                ( actual )
      blen 0>  if               ( actual )
         badr cur-dma-out  ( >dma-out-entry )    ( actual src dst )
     blen blen/dma-buf min dup >r       ( actual src dst len )
         conv-out-sample r>         ( actual len )
         i last-eop =  if
            blen  eop eot or  cur-dma-out  set-prd-out-flags-len
         then
         blen over - to blen            ( actual len )
         badr + to badr             ( actual )
         cur-dma-out+               ( actual )
      then
   loop                     ( actual )
   0 dma-done
;


\  external

: playback  ( -- )  open-out enable-playback  ;

: 8khz    ( -- )  d# 8000 to sample-rate  ;
: 48khz   ( -- )  d# 48000 to sample-rate  ;
: linear  ( -- )
   ['] (null)  to sample>
;
: stereo  ( -- )
   linear
   true to stereo?
   4 to /sample
   ['] @ to sample@
   ['] ! to sample!
   lin16stereo to sample>  
;
: 8bit    ( -- )
   false to stereo?
   1 to /sample
   ['] c@ to sample@
   ['] c! to sample!
   lin8mono to sample>  
;
: 16bit   ( -- )
   linear
   false to stereo?
   ['] w@ to sample@
   ['] w! to sample!
   2 to /sample
   lin16mono to sample>  
;
: mulaw   ( -- )
   8bit
   ulaw8mono to sample>
;
: default ( -- )  stereo 48khz disable-playback  ;

: open  ( -- ok? )
   fatal-error?  if  false exit  then
\   map-regs
   get-vendor-id
   default
   set-linein
\   parse-args  0=  if  ( unmap-regs ) false exit  then
   init-buffers
   true
;
: close  ( -- )
   ?uninstall-audio-alarm free-buffers  
;

: read   ( adr len -- actual )
   fatal-error?  if  2drop 0 exit  then
   open-in  audio-in  close-in
;

: write  ( adr len -- actual )
   fatal-error?  if  2drop 0 exit  then
   open-out  audio-out  close-out  
;


: init-audio  ( -- )
\   fill-table               \ Fill mulaw table
\   punch-table
   set-linein
   close-out
   close-in
   disable-playback   

   fatal-error?  if  ." ERROR:  Audio is broken." cr  then
;

previous definitions

base !

