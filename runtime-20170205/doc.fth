\ @@File: doc.fth 
\ @@REQUIRES: timer.fth,utils.fth
\ Disk on chip words 
\ doc-init ( -- )  
\ doc-probe ( addr -- chip-id )
\ doc-block@ ( buffer blk# -- ior )
\ doc-block! ( buffer blk# -- ior )
\ page and block numbers start from 0
\
\ 
\  we have a millenium chip on the TC7010
\  three areas a,b,c defined in a page
\  address format is offset in area (ad0-ad7) page (ad9-ad22)
\  ad8 is set by area select command
  
base @ hex

only forth also
system definitions

\ NAND FLASH modes 
0 constant reset#      internal
1 constant normal#
4 constant mdwren#
8 constant bdect#
10 constant rstlatch#
80 constant clrerr#

\ DOC chip ids
\ 20 constant DOC2k#
\ 21 constant DOC2kTSOP#
30 constant DOC-mil
\ 40 constant DOCmil+32
\ 41 constant DOCmil+16

\ CDSN control Write
01 constant CDSN-ctrl-ce#
02 constant CDSN-ctrl-cle#
04 constant CDSN-ctrl-ale#
08 constant CDSN-ctrl-wp#
10 constant CDSN-ctrl-flash-io#
20 constant CDSN-ctrl-ecc-io#
\  CDSN Status bits on read
40 constant CDSN-ctrl-b0#
80 constant CDSN-ctrl-b1#     \ use to getstatus 1 - ready
80 constant CDSN-ctrl-frb#    \ ready busy flag 

\ ECC ECD 
0  constant ECC-reset# 
01 constant ECC-ignore#
02 constant ECC-disable#
04 constant ECC-togglebit#
0a constant ECC-enable#
22 constant ECC-rw#


\ registers  for millenium

0800 constant CDSN-io-mil#
1000 constant chip-id# 
1001 constant DOC-status#
1002 constant DOC-control#
1003 constant floor-select#   \  ASIC control
1004 constant CDSN-control#
1005 constant CDSN-dev#
1006 constant ECC-conf#
1006 constant ECC-status#

100d constant CDSN-slow-io#
1010 constant ECC-syndrome0#
1011 constant ECC-syndrome1#
1012 constant ECC-syndrome2#
1013 constant ECC-syndrome3#
1014 constant ECC-syndrome4#
1015 constant ECC-syndrome5#
101b constant alias#
100c constant confing-input#
101d constant read-pipe-init#
101e constant write-pipe-term#
101f constant last-data-read#
1020 constant NOP#
1800 constant ipl-sram#


\ NAND chip  commands
0  constant NAND-cmd-read-a#
1  constant NAND-cmd-read-b#
50 constant NAND-cmd-read-c#     \ read extra bytes
10 constant NAND-cmd-page-prog#   \ setup write auto program
60 constant NAND-cmd-erase1#     \ setup auto block erase
70 constant NAND-cmd-status#     \ status read
80 constant NAND-cmd-seqin#      \ serial data in
90 constant NAND-cmd-read-id#    
b0 constant NAND-cmd-suspend-erase# 
d0 constant NAND-cmd-erase2#     \ confirm auto block erase
e0 constant NAND-cmd-register-read#
ff constant NAND-cmd-reset#      \ reset


98 constant NAND-mfr-toshiba#
ec constant NAND-mfr-samsung


\ "Toshiba TC58V64AFT/DC", 0xe6
10  constant doc-oob-size#
200 constant doc-page-size#
9   constant doc-page-bits#
0d  constant doc-erase-block-bits#
8   constant doc-sign-offset# 
6   constant ecc-size# 


d2000 value docaddr#

\  out of band buffer

create oob-buffer 10 allot

\ ecc buffer

create ecc-buffer 6 allot

\ syndrome buffer

create doc-syndrome 6 allot

: doc@ ( reg -- byte)
\ read DoC register
 docaddr# + c@ ; 

: doc! ( val reg -- ) 
\ Write to DoC register
 docaddr# + c! ;

: doc-delay ( U --)
\ u > 0 
0 do 
  NOP# doc@ drop
loop
;

: doc-status ( -- flg)
\ see if chip is ready 
4 doc-delay CDSN-control# doc@
CDSN-ctrl-frb# and dup 
if 2 doc-delay then
;

: (waitready) ( -- flg)
ffff begin
    1- doc-status over 0= or
( CDSN-control# doc@ CDSN-ctrl-frb# and over 0- or )
until
0= dup if ." DoCReady timeout" cr then
;

: doc-wait-ready ( -- flg )
  4 doc-delay doc-status
  0= dup if drop (waitready) then
  2 doc-delay
;

: CDSN-ctrl! ( x -- )
\ write to CDSN-control reg with delay
  CDSN-control# doc! 4 doc-delay 
;

: doc-ctrl! ( x -- )
\ write to DoC control register
  DOC-control# doc!
;

: doc-io! ( c -- )
\ write to CDSN-io register
  CDSN-io-mil# doc!
;

: doc-io@ (  -- c )
\ read from CDSN-io register
  CDSN-io-mil# doc@
;


: doc-mode! ( x -- )
  dup doc-ctrl! doc-ctrl!
;

: slow-write ( c --)
\  for millenium chips
  dup CDSN-slow-io# doc! 4 doc-delay
  doc-io!
;

: doc-seq-start ( -- )
\ start read sequential data stream
  read-pipe-init# doc@ drop
;

: doc-seq-end ( -- )
\ end sequential data stream
 0 write-pipe-term# doc!
;


: doc-cmd ( cmd -- )
\ send command with WP off
\ assert CLE
  CDSN-ctrl-ce# or CDSN-ctrl-cle# or CDSN-ctrl!
  doc-io! doc-seq-end
  CDSN-ctrl-ce# or CDSN-ctrl!
;

: doc-wp-cmd ( cmd -- )
\ send commands with write protect enabled
\ assert command latch
CDSN-ctrl-wp# CDSN-ctrl-ce# or CDSN-ctrl-cle# or CDSN-ctrl!
doc-io!  doc-seq-end
\ deassert Command latch
CDSN-ctrl-wp# CDSN-ctrl-ce# or CDSN-ctrl!
;

: doc-prog-status ( -- flg)
\ bit 0 prog/erase , pass:0 fail:1
\ bit 6 Read/Busy , Busy:0 Ready:1
\ bit 7 Write protect  protected:0 unprotected:1
  NAND-cmd-status# doc-wp-cmd
  doc-seq-start
  2 doc-delay doc-io@ 1 and
  last-data-read# doc@ drop
;


: doc-address8 ( addr xflags   -- )
\ assert ALE
  swap over
  CDSN-ctrl-ce# or CDSN-ctrl-ale# or CDSN-ctrl!
\ send address
  doc-io!  \ column address
  doc-seq-end 
\ deassert ALE
  CDSN-ctrl-ce# or CDSN-ctrl!
;

: doc-address16 ( addr xflags   -- )
\ 
  swap over
  CDSN-ctrl-ce# or CDSN-ctrl-ale# or CDSN-ctrl!
\ send address
  dup doc-io!
  9 rshift doc-io!  
  doc-seq-end
\ deassert ALE
  CDSN-ctrl-ce# or CDSN-ctrl!
;

: doc-address24 ( addr xflags   -- )
\
\ a0 - a7 column address
\ a13 - a22 block address
\ a9 - a12 NAND address in block
   swap over
   CDSN-ctrl-ce# or CDSN-ctrl-ale# or CDSN-ctrl!
\ send address
   dup doc-io!
   9 rshift dup doc-io!
\ bits 7,6 must be 0
   8 rshift 3f and doc-io!
   doc-seq-end
\ deassert ALE
   CDSN-ctrl-ce# or CDSN-ctrl!
;


: doc-select-chip ( u -- )
  CDSN-dev# doc! 4 doc-delay
  doc-wait-ready drop
;

: doc-select-floor ( u --  )
  floor-select# doc! 4 doc-delay
  doc-wait-ready drop
;

: doc-map-window ( chip floor -- )
   doc-select-floor doc-select-chip 
;

: mil-ecc-reset ( -- ) 
\ reset ECC hardware
  ECC-reset# ECC-conf# doc!
;

: mil-enable-ecc-write (   )
\ enable ecc hardware for writes
  mil-ecc-reset
  ECC-enable# ECC-rw# or ECC-conf# doc!
;

: mil-enable-ecc-read (   )
\ enable ecc hardware for reads
  mil-ecc-reset
  ECC-enable#  ECC-conf# doc!
;

: mil-ecc-flush ( -- )
  ECC-conf# dup doc@ drop doc@ drop
;
 
: mil-disable-ecc ( -- )
  mil-ecc-reset
  ECC-disable# ECC-conf# doc! 
;

: doc-c@ ( -- c)
 2 doc-delay
 read-pipe-init# doc@
 last-data-read# doc@
;

: doc-c! ( c -- )
 dup CDSN-slow-io# doc!
 dup doc-io!
 write-pipe-term# doc!
;


: doc-!buffer ( addr u -- )
\ write buffer to DoC
 0 do 
    c@+ CDSN-io-mil# i + doc!
 loop drop
 doc-seq-end
;

external 

: doc-@syndrome ( addr -- )
 6 0 do
   ECC-syndrome0# i + doc@ c!+
 loop
 drop 
;

: doc-!syndrome ( addr -- )
 6 0 do
   c@+ ECC-syndrome0# i + doc!
 loop
 drop
;

: doc-!ecc ( addr -- )
 6 0 do
   c@+ CDSN-io-mil# i + doc!
 loop
 drop
 ;


: doc-@buffer ( addr u -- )
 doc-seq-start 
  1- 0 do
    i ff and CDSN-io-mil# + doc@ c!+
 loop
 last-data-read# doc@ swap c!
;

: doc-do-write ( -- ior )
\ start flash programming
  NAND-cmd-page-prog# doc-cmd
  doc-wait-ready drop 
\ read status
  doc-prog-status
;



: doc-verify-buf ( addr u -- flg)
\ compare buffer with current DoC page 
 doc-seq-start
 true -rot 0 do 
   c@+ i ff and CDSN-io-mil# + doc@ <>
   if nip false swap leave then
 loop
 c@ last-data-read# doc@ = and
;
 


: doc-id-chip ( chip floor -- mfr id ) 
\ identifier DOC chips  
  doc-map-window  
  NAND-cmd-reset# doc-wp-cmd
  doc-wait-ready drop
  NAND-cmd-read-id# doc-wp-cmd
  0 CDSN-ctrl-wp# doc-address8
  doc-seq-start 2 doc-delay
  doc-io@ 2 doc-delay     \ mfr
  doc-io@                \ id
  last-data-read# doc@ drop
;


: doc-probe  ( win -- chipid )
\ initialize millenium chip
  docaddr#  swap to docaddr#
\ reset ASIC CLR_ERR|MDWREN|RESET
  clrerr# mdwren# or dup reset# or
  doc-mode! 
\ enable ASIC CLR_ERR|MDWREN|normal#
  normal# or doc-mode!
  chip-id# doc@  ( addr chip_id )
  swap to docaddr#   ( chip id )
 ;  


: doc-oob@ ( addr pg#--  )
\ out of band area 0x10 bytes for mill
  doc-page-bits# lshift     ( a a )
  0 0 doc-map-window  mil-disable-ecc
\ read command for area C (oob)
  NAND-cmd-read-c# doc-wp-cmd
  CDSN-ctrl-wp# doc-address24
  doc-wait-ready drop
  doc-oob-size# doc-@buffer
;

: doc-oob! ( addr pg# -- ) \ FIXME
  doc-page-bits# lshift
\ select chip and floor, 
  0 0 doc-map-window mil-disable-ecc
\ reset chip
  NAND-cmd-reset# doc-wp-cmd
\ read command for area C (oob)
  NAND-cmd-read-c# doc-wp-cmd
  NAND-cmd-seqin# doc-cmd
 \ send address24
  0 doc-address24
  doc-wait-ready drop
\ write data through CDSN-io-mil#
  doc-oob-size# doc-!buffer
\ send page program command
  doc-do-write
;


: .id ( n --)
\ print chip id
 case
    e6 of ." TC58V64AFT/DC" endof
    e5 of ." Some other" endof
    ." Unknown " 
endcase
;

: .mfr ( n --)
\ print manufacturer
 case
    93 of ."  Samsung" endof
    98 of ."  Toshiba" endof
    ." Unknown"
endcase
;

: doc-extra@ ( buf u blk# -- )
\ read u bytes from first page block to get signature
\ get address
  doc-erase-block-bits# lshift doc-sign-offset# +
  0 0 doc-map-window
\ read from area-c
  NAND-cmd-read-c# doc-wp-cmd
  CDSN-ctrl-wp# doc-address24
  doc-wait-ready drop
  doc-@buffer
;

: doc-page@ ( buf pg# -- ior )
\ page is 512 bytes
  doc-page-bits# lshift     ( buf addr ) 
  0 0 doc-map-window 
\ read from area A
  NAND-cmd-read-a# doc-wp-cmd
  CDSN-ctrl-wp# doc-address24
  doc-wait-ready drop
  mil-enable-ecc-read
  doc-page-size# doc-@buffer \ ( -- )
\ Error detection
  ecc-buffer ecc-size# doc-@buffer
  ECC-conf# doc@ drop ECC-conf# doc@ drop 
  ECC-conf# doc@ 80 and dup
  if ." ECC read error" cr
        doc-syndrome doc-@syndrome
  then        
  mil-disable-ecc
;

: doc-page! ( buffer pg# -- )
\ write 512 page to DocC
   doc-page-bits# lshift   ( a u )
   0  0 doc-map-window
\ write  to area A
  NAND-cmd-reset# doc-cmd
  doc-wait-ready drop
  NAND-cmd-read-a# doc-cmd
  NAND-cmd-seqin# doc-cmd
  0 doc-address24
  doc-wait-ready drop
  mil-enable-ecc-write   ( a )
  doc-page-size# doc-!buffer
\ write ECC data
\ flush the buffer
  0 NOP# doc!  0 NOP# doc!  0 NOP# doc!  
\ get ECC data from ECC logic  
  ecc-buffer doc-@syndrome
  mil-disable-ecc
\ save ecc data to flash
  ecc-buffer doc-!ecc
\ append info in extra area
  5A doc-io!
  A5 CDSN-io-mil# 1+ doc!
  doc-seq-end doc-do-write   ( ior)
;


: doc-erase-block ( blk# -- )
\  erase NAND block
   doc-erase-block-bits# lshift
\ select chip and floor
   0 0 doc-map-window
\ send erase1 command
   NAND-cmd-erase1# doc-cmd
\ send address16 of blk#
   0 doc-address16
\ send erase cycle 2 command
   NAND-cmd-erase2# doc-cmd
   doc-wait-ready drop
\ check status
   NAND-cmd-status# doc-wp-cmd
   doc-seq-start doc-io@ 1 and dup 
   if ." ERASE error" then
   last-data-read# doc@ drop
;


also forth definitions

: doc-block@ ( buffer blk# -- ior )
\ read 1024 byte block from doc
   2* 2dup  doc-page@ -rot
   1+ swap doc-page-size# + swap 
   doc-page@ or
;

: doc-block! ( buffer blk# -- ior )
\ write 1024 byte block to DoC
   2* 2dup doc-page!  
   1+ swap doc-page-size# + swap
   doc-page! ;

: doc-init
\ initialze millenium chip on tc7010

 d2000 doc-probe 
 30 = if ." DoC initialized" cr then
 0 0 doc-id-chip .id space .mfr cr
;

previous definitions

module

previous definitions

base !

