\ audio.fth
\ audio driver for CS5530 based on openfirmware and BSD native drivers 
\ The Geode "Audio Engine" has 6 PCI Bus Masters. Each is controlled by
\ 3 registers.  Each bus masters has a hard-wired function :
\ DMA 0 - 32-bit stereo output (PLAY device, 2 16-bit PCM channels).
\ DMA 1 - 32-bit stereo input  (RECORD device, 2 16-bit PCM channels).
\ DMA 2 - 16-bit mono output   (MONO-PLAY).
\ DMA 3 - 16-bit mono input    (MONO-RECORD).
\ DMA 4 - 16-bit mono output, codec slot programmable.
\ DMA 5 - 16-bit mono input, codec slot programmable.
\ 
base @ hex
only forth also system definitions

: dma-alloc ( size -- phy-addr addr )  \ aligned on 32 byte boundary
  20 tuck + allocate throw
   dup rot tuck + swap 1- invert and 
;
  
: dma-free ( phys-addr -- ) free throw ;

: <= > 0= ;

false value fatal-error?
d# 48000 constant frame-rate
frame-rate d# 1000 / constant f/ms

10 0 12 3 pci-dev pci@ value f3bar#

0 value geode-dma-poll

\ PRD buffers
begin-structure /prd-entry  ( prd-entry )
   field:  >prd-addr
   wfield: >prd-length
   wfield: >prd-status
end-structure

h# 8000 constant eot
h# 4000 constant eop
h# 2000 constant prd-loop

3 constant #prd-entry       \ 3 entry prd table
/prd-entry #prd-entry * constant /prds
0 value prd-in
0 value prd-in-phys
0 value prd-out
0 value prd-out-phys

: >prd-in-entry  ( entry# -- phys-adr )  
 /prd-entry * prd-in +  
;

: (set-prd) ( buffer len flags prd -- )
   tuck >prd-status w!
   tuck >prd-length w!
   >prd-addr         !
;

: set-prd-in-entry ( phys-adr len flags entry# -- )
  >prd-in-entry  (set-prd) 
;

: >prd-out-entry   ( entry# -- phys-adr )  
  /prd-entry * prd-out +  
;

: set-prd-out-entry  ( phys-adr len flags entry# -- )
   >prd-out-entry
   (set-prd)
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
#prd-entry 1- constant #dma-buf   \ 2 DMA buffers
/dma-buf #dma-buf * constant /bufs
0 value cur-dma-in
0 value cur-dma-out

0 value dma-in-phys
0 value dma-in1
0 value dma-out-phys
0 value dma-out1

: next-dma-entry ( cur -- next ) 1+ dup #dma-buf = if drop 0 then ;

: cur-dma-in+ ( -- )  cur-dma-in next-dma-entry to cur-dma-in ;

: cur-dma-out+ ( -- )  cur-dma-out next-dma-entry to cur-dma-out ;

\ : >dma-in-entry ( entry# -- virt-adr )  /dma-buf * dma-in +  ;

: >dma-in-phys ( entry# -- phys-adr )  /dma-buf * dma-in-phys + ;

\ : >dma-out-entry ( entry# -- virt-adr )  /dma-buf * dma-out-virt + ;

: >dma-out-phys ( entry# -- phys-adr )  /dma-buf * dma-out-phys + ;

\ buffer allocation
\ The first 2 PRD entries each point to countiguous data buffers,
\ while the last PRD entry points back to the first PRD entry.
\ The bus-engine thus runs in a loop. The EOP flag in the 2 data buffer
\ descriptors causes an interrupt when the corresponding DMA is complete.
\ The two buffers provide a double-buffering scheme in conjunction with
\ the PCM driver.

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
   \ allocate data buffers for bus mastering
   /bufs dma-alloc to dma-in-phys to dma-in1
   /bufs dma-alloc to dma-out-phys to dma-out1 

   \ allocate PRDs
   /prds dma-alloc to prd-in  to prd-in-phys  
   /prds dma-alloc to prd-out to prd-out-phys 

   \ init PRDs
   init-prds-in
   init-prds-out
;

: free-buffers  ( -- )
   dma-in1  dma-free    
   dma-out1 dma-free

   prd-in-phys dma-free
   prd-out-phys dma-free
   0 dup to prd-in to prd-out
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
: cmd-wait  ( -- )
 true d# 192 0 do
   cmd@ h# 10000 and 0= if
      drop false leave then
   loop
 to fatal-error? 
;

: cmd! ( val -- ) cmd-wait f3bar# c + ! cmd-wait ;
: rst h# 400000 stat! 1 udelay 0 stat! ;

: codec!  ( val reg --) d# 24 lshift or cmd! ;
: (codec@)  ( reg -- val) 
    \ 192 is a ~60us delay to wait until the status tag is cleared.
   d# 192 0 do
      stat@  h# 30000 and  h# 10000 =  if  leave  then
   loop
   h# 80 or  d# 24 lshift cmd!
   \ wait until status valid and status tag is set
   true d# 192 0 do
      stat@  h# 30000 and  h# 30000 =
      if  drop false leave  then
   loop
   to fatal-error?
   stat@ ffff and
;

: codec@ ( reg -- val)     \ buggy serial protocol make sure we read reg
  begin 
    dup (codec@) 
    over stat@ d# 24 rshift <>
  while
    drop
  repeat
  nip
;

: >dma-regs  ( channel# -- adr ) 
  /dma-regs *  h# 20 +  f3bar# +
;
: set-dma  ( prd-adr in? channel# -- )
   >dma-regs >r
   8 and r@ >dma-cmd c!   \ Set direction bit
   r@ >dma-status c@ drop \ Clear errors
   r> >dma-prd    !      \ Set address
;

: dma-go  ( channel# -- )       \ start DMA master
  >dma-regs  dup c@  1 or  swap c!  
;
: dma-wait  ( channel# -- )     \ wait for DMA  master to pause
   >dma-regs  begin  dup >dma-status c@ 1 and  until drop   
;
: dma-done  ( channel# -- )     \ stop DMA master
   >dma-regs dup >dma-cmd c@ 1 invert and swap >dma-cmd c!
;

\  AC '97 CODEC stuff
: set-master-volume  ( value -- )
   \ XXX handle balance too
   ( db>volume )  2 codec!
;
: set-headphone-volume  ( value -- ) ( db>volume )  4 codec! ;
: set-mono-volume  ( value -- ) ( db>volume )  6 codec! ;
: set-tone-volume  ( value -- ) ( db>volume )  8 codec! ;
: set-pcbeep-volume  ( value -- ) ( db>volume )  a codec! ;
: set-pcm-gain  ( db -- ) ( db>volume )  h# 18 codec! ;
: set-record-gain  ( db -- )  ( db>volume )  h# 1c codec! ;
: set-mic  ( -- ) 0 h# 1a codec! ;
: set-mic-volume ( val -- ) ( db>volme ) h# 0e codec! ;
: set-volume  ( value -- )  ( db>volume )  10 codec! ;
: enable-playback  ( -- )   h# 407 set-mic-volume ;
: disable-playback  ( -- )  h# 8008 set-mic-volume ;
: set-vra ( -- ) 1 h# 2a codec! ;    \ set variable rate

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
: close-in  ( -- )  h# 8000 set-record-gain   ;   \ mute

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

\   start from here and FIX it poll nstead of timers????

200 value timeout-threshold
0   value #non-eop
0   value last-prd-dma-adr
0   value cur-dma-blk
0   value cur-channel

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
 
: audio-in-timeout?  ( -- timeout? )  
  #non-eop timeout-threshold >  
;

: ?install-audio-alarm  ( -- )
      init-prds-in prd-in true 1 set-dma
      0 to cur-dma-in
      0 to #non-eop
      1 dma-go
;

0 value badr
0 value blen
0 value blen/dma-buf
0 value #eop        \ # of eop expected
0 value last-eop
0 value /last-dma
4 value /sample    \ # bytes per sample


: setup-prds-in  ( adr len -- )
   to blen to badr
   /dma-buf 4 /sample / / to blen/dma-buf
   blen blen/dma-buf /mod swap 0>  if  1+  then
   to #eop
;

0 value eop-cnt

: (audio-in)  ( -- )
   eop-cnt 1+ to eop-cnt
   cur-dma-in >dma-in-phys badr blen blen/dma-buf min dup >r 
   badr r@ + to badr
   blen r> - to blen
   eop-cnt #eop = 
;

: audio-in  ( adr len -- actual )
   tuck  setup-prds-in              ( actual )
   dup blen/dma-buf mod ?dup  0=  if  blen/dma-buf  then  to  /last-dma

   0 to eop-cnt
   ?install-audio-alarm

   begin  audio-alarm eop-cnt #eop =  audio-in-timeout? or  until
   audio-in-timeout?  if
      drop 0
   then
;

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


: audio-out  ( adr len -- actual )
   prd-out false 0 set-dma          ( adr len )
   tuck setup-prds-out              ( actual )

   s/ms drop

   0 dma-go                 ( actual )
   #eop 1+ 1 ?do                ( actual )
      0 dma-wait                ( actual )
      blen 0>  if               ( actual )
         badr cur-dma-out  ( >dma-out-entry )    ( actual src dst )
     blen blen/dma-buf min dup >r       ( actual src dst len )
         r>         ( actual len )
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

: default ( -- )   48khz disable-playback  set-vra ;

: open  ( -- ok? )
   fatal-error?  if  false exit  then
   get-vendor-id
   default
   set-mic
   init-buffers
   true
;
: close  ( -- ) free-buffers  ;

: read   ( adr len -- init-buffersactual )
   fatal-error?  if  2drop 0 exit  then
   open-in  audio-in  close-in
;

: write  ( adr len -- actual )
   fatal-error?  if  2drop 0 exit  then
   open-out  audio-out  close-out  
;


: init-audio  ( -- )
   set-mic
   close-out
   close-in
   disable-playback   
   fatal-error?  if  ." ERROR:  Audio is broken." cr  then
;


\ notes to implement polling

\ The PRD DMA descriptor table that corresponds to the data buffers
\ never needs to change (the DMA engine just keeps processing it
\ "in a circle"). The interrupt routine uses the change in the
\ "processed PRD address" contained in the memory-mapped audio
\ function registers of the appropriate bus-master to detect the
\ end of a "DMA" operation.
\ 
\ The EOP bit needs to be read after DMA completion, or the bus
\ master (and associated transfer) will halted on "double EOP
\ write". The EOP apparently is not to be trusted due to a hardware
\ bug (it cannot be used to detect end of the DMA for the respective
\ bus-master).


: geode-intr ( -- )   \ only one channel active at a time

    geode-dma-poll if       \ DMA is running 

        cur-channel >dma-regs >dma-prd @    \ Current DMA PRD.
        dup last-prd-dma-adr  <> if
            dup to last-prd-dma-adr
            >dma-status c@       \ Clear EOP.
            \ update pointer to next DMA buffer.*/
            cur-dma-blk 1+ #dma-buf mod to cur-dma-blk
        then
    then
;

\ * Poll to see if any active DMA has completed, and if so fake an interrupt
\ * by calling the driver's interrupt routine. Polling for DMA completion is
\ * done by examining the PCI memory-mapped "DMA PRD address" register for
\ * each bus master. This register is updated to point to the "next PRD" 
\ * element upon completion of each DMA transfer.
\ *
\ * This routine is either called from "timeout()" or custom code in "hardclock()".

\
: do-audio-poll ( -- )
  geode-dma-poll if 
    cur-channel >dma-regs >dma-prd @        \ DMA PRD adr value.
    last-prd-dma-adr -1 = if
        to last-prd-dma-adr
    else
        last-prd-dma-adr <> if geode-intr then
    then
  then
;

\
\ DMA Start. We always start at the 1st PRD descriptor, after which DMA
\ loops forever. cur-dma-blk tracks the block being DMAed, so on a 
\ completion trigger it points to the "finished" block. 
\
: start-geode-dma ( channel# in? -- )
    over >dma-regs 
    dup >dma-status c@ drop  \ DMA Status, Paranoid Read clears EOP flgs
    true to geode-dma-poll  \ Start polling.
    0 to cur-dma-blk
    dup >dma-prd to last-prd-dma-adr  
    over if dma-in-phys else dma-out-phys then
    over !           \ DMA PRD. Physical adress ito PRD table.
    swap if 1 else 9 then       \ start write or read DMA
    swap >dma-cmd c!
;

\ Stop a bus master; called on abort and at end of overall DMA operation
\ (that is, when there is no more data to be transfered).
\ EOT should be written to PRD entry delay sample rate * buffer then clear!

: stop-geode-dma ( channel#  --  ) 
   dma-done
   0 to geode-dma-poll
;


\ debugging 

create ac97-regs 1c c,   \ number of bytes 
                 00 c, 02 c, 04 c, 06 c, 08 c, 0a c, 0c c, 0e c,
                 10 c, 12 c, 14 c, 16 c, 18 c, 1a c, 1c c, 1e c,
                 20 c, 22 c, 24 c, 26 c, 28 c, 2a c, 2c c, 
                 32 c, 5a c, 7a c, 7c c, 7e c,
                 
: aregs ( -- ) \ dump ac'97 codec registers
    base @ hex cr
    ac97-regs dup 1+ swap c@ 0 do
            dup c@ dup . codec@ . cr
            1+
    loop 
    drop base !
;
 
: dregs ( --  )    \ F3BAR
   f3bar# cr ." **** MEM-MAPPED AUDIO REGS " cr
   dup @ ." --> CODEC GPIO status =" . cr
   4  over + @  ." --> CODEC GPIO control =" . cr
   8  over + @  ." --> CODEC status =" . cr 
   0c over + @  ." --> CODEC command =" . cr
   10 over + w@  ." --> Audio SMI src =" . cr
   14 over + @  ." --> I/O trap SMI =" . cr
   18 over + w@ ." --> I/O trap SMI enable =" . cr
   1a over + w@ ." --> Internal IRQ enable=" . cr
   1c over + @  ." --> Internal IRQ control=" . cr 
   20 over + c@ ." --> DMA 0 cmd =" . cr
   21 over + c@ ." --> DMA 0 SMI status =" . cr
   24 over + @  ." --> DMA 0 PRD table adr =" . cr
   28 over + c@ ." --> DMA 1 cmd =" . cr
   29 over + c@ ." --> DMA 1 SMI status =" . cr
   2c over + @  ." --> DMA 1 PRD table adr =" . cr   drop
;

: loop-back ( -- )  \ loop ADC in to DAC
  80 20 codec!     \ loopback
  0  02 codec!     \ master volume
  40 0e codec!     \ mic gain +20 db
;

: fill-sq ( -- ) \ fill with sq-wave
    prd-out @ h# f000 0 do ( a )
      i over + d# 24 h# 7fff lfill
      i d# 24 cells + over + d# 24 h# 7fff0000 lfill
    d# 48 cells +loop
    drop
    ; 
previous definitions

base !
0 [if] sequence : sending receiving audio
    open if init-audio then
    
    open-out
    prd-out false 0 set-dma
    prd-out dup @ 3c00 d# 1000 wave
            8 + @ d# 3c00 d# 1000 wave  
    
    0 dma-go
    
    open-in
    prd-in true 1 set-dma
    1 dma-go
    
      
[then]

