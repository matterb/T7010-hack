\ at93c06 driver
\ when org pin is high x16 mode else x8 mode
\ valid instructions sequence :
\ cs goes high,start-bit n-bit-op-code n-bit-addr 
\ cs-high send-start send-cmd send-addr
\ 
\ commands
\ read : data on sdo synchronized with rising edge of cs
\        dummy bit preceded actual data
\ write-enable:  must be executed before any programming cmmds
\ device bytes addr-len cmd-len layout    
\ 93c06   32    6       2       16x16
\ 93c13   32    7
\ 93c46  128    7
\ 93c56  256    8
\ 93c66  512    8
\ 93c76 1024   10
\ 93c86 2048   11
\ commands
\ read  10 an...a0
\ write 01 an...a0
\ erase 11 an...a0
\ wen   00 11X...X
\ wral  00 01X...X
\ eal   00 10X...X
\ ewds  00 00X...X
only forth also system definitions

base @ hex
1 value T1
10 value eep-org  \ eeprom mode x16 or x8
1 eep-org 1- lshift value eep-mask
6 value addr-len
1 addr-len 1- lshift value addr-mask

: mear@ ( -- b )  8 eth-io@ ;
: mear! ( b -- ) 8 eth-io! ;

: eedi-high ( -- ) mear@ 1 or mear! ; \ internal
: eedi-low ( -- ) mear@ 1 invert and mear! ;
: eeclk-low ( -- ) mear@ 4 invert and mear! ;
: eeclk-high ( -- ) mear@ 4 or mear! ;
: eeclk-toggle ( -- ) T1 ms eeclk-high T1 ms eeclk-low ;
: eedo-@ ( -- ) mear@ 1 rshift 1 and ;
: eesel-high ( -- ) mear@ 8 or mear! ; \ enable chip
: eesel-low  ( -- ) mear@ 8 invert and mear! ; \ disable chip

: eep-stop ( -- ) eeclk-low eesel-low ;

: send-bit ( flg --)
  if eedi-high else eedi-low then
      eeclk-toggle
;


: send-cmd ( b -- )         \ command is start + 2 bits
 eesel-high
 4 3 0 do
      2dup and send-bit 1 rshift
 loop
 2drop 
;

\ extra 0 dummy bit on reads
: get-data ( -- u )
\  scl-toggle          \ disregard first bit
  0 eep-org 0 do
     1 lshift eeclk-toggle eedo-@ or
  loop
;

: send-data ( w -- )
  eep-mask eep-org 0 do
     2dup and send-bit 1 rshift
  loop
  2drop 
;

\ send addr.
\ an-a0   4-bit address shifted left!

: send-addr ( addr -- )
  addr-mask addr-len 0 do
      2dup and send-bit 1 rshift
  loop
  2drop
;

: eep-cmd@ ( addr --  ) 6 send-cmd send-addr ;
: eep-cmd! ( w addr  -- ) 5 send-cmd send-addr send-data ;
: eep-cmd-wen ( -- ) 4 send-cmd 30 send-addr ;
: eep-cmd-wds ( -- ) 4 send-cmd 00 send-addr ;
: eep-cmd-wrall ( w -- ) 4 send-cmd 10 send-addr send-data ;
: eep-cmd-erall ( -- ) 4 send-cmd 20 send-addr ;
: eep-cmd-eras ( addr  -- ) 7 send-cmd send-addr ;

\ check programming status
: eep-rdy-bsy ( -- flg )
\ sdo stays low while chip is busy
\ goes High when finished processing
\ put a timer of < 10 ms !
  eesel-high
  begin 
      2e emit 
      eedo-@ 
  until
;

: eep-chk ( -- flg) ; 

: eep-@ ( addr --- w ) eep-cmd@ get-data eep-stop ;

: eep-! ( w addr -- )  eep-cmd! eep-stop ;

: eep-wen ( -- ) eep-cmd-wen eep-stop ;

: eep-wds ( -- ) eep-cmd-wds eep-stop ;

: eep-eral ( -- ) eep-cmd-erall eep-stop ;

: eep-erase ( addr -- ) eep-cmd-eras eep-stop ;

: eep-wrall ( w -- ) eep-cmd-wrall eep-stop ;

: eep-reset  eesel-low eeclk-toggle ;

: eep-66 eep-reset 
\  8 to addr-len
\  80 to addr-mask
;

  
: test1
  10 0 do 
      cr i dup 
      u. space eep-@ u.
  loop 
;

: test3
  10 0 do
      cr ." write :" 
      i dup dup u. eep-!
      i eep-@
      ." Read :" u.
      loop
;

: test5 
 \ dump of eeprom
  10 0 do
     i u. space
     8 0 do
       i j + eep-@
       pause
       u. space
     loop
     cr 8 
  +loop
;

: cc eeclk-toggle ;
: d1 eedi-high ;
: d0 eedi-low ; 

\ module
base !
only forth definitions
