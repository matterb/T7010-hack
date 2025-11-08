\ file ohci.fth
\ purpose: 
\ See license at end of file
hex


\ Configuration space registers

1 constant potpgt           \ PowerONToPowerGoodTime

true value first-open?
0 value open-count
0 value ohci-reg


: ohci-reg@  ( idx -- data )  usb@  ( ohci-reg + @ )  ;
: ohci-reg!  ( data idx -- )  usb!  ( ohci-reg + ! ) ;

: hc-cntl@  ( -- data )   4 usb@  ;
: hc-cntl!  ( data -- )   4 usb!  ;
: hc-stat@  ( -- data )   8 usb@  ;
: hc-cmd!   ( data -- )   8 usb!  ;
: hc-intr@  ( -- data )   c usb@  ;
: hc-intr!  ( data -- )   c usb!  ;
: hc-hcca@  ( -- data )  18 usb@  ;
: hc-hcca!  ( data -- )  18 usb!  ;


: hc-rh-desA@  ( -- data )  48 usb@  ;
: hc-rh-desA!  ( data -- )  48 usb!  ;
: hc-rh-desB@  ( -- data )  4c usb@  ;
: hc-rh-desB!  ( data -- )  4c usb!  ;
: hc-rh-stat@  ( -- data )  50 usb@  ;
: hc-rh-stat!  ( data -- )  50 usb!  ;

: hc-rh-psta@  ( port -- data )  4 * 54 + usb@  ;
: hc-rh-psta!  ( data port -- )  4 * 54 + usb!  ;

: hc-cntl-clr  ( bit-mask -- )  hc-cntl@ swap invert and hc-cntl!  ;
: hc-cntl-set  ( bit-mask -- )  hc-cntl@ swap or hc-cntl!  ;

: hc-periodic-init ( -- )
    34 usb@ dup 3fff and swap 80000000 and      ( fi FIT ) 
    80000000 xor 34 usb@ 7fffffff and or 34 usb!    ( fi ) \ FIT toggled?     
    9 d# 10 */ 3fff and  40 usb!        \ periodicStart
;

: reset-port  ( port -- )   
\ debug? if ." reset-port" cr then
   >r
   h# 10002 r@ hc-rh-psta!      \ enable port
   h# 10 r@ hc-rh-psta!         \ reset port
   r@ d# 10 0  do
      d# 10 ms
      dup hc-rh-psta@ h# 100000 and  if leave then
   loop  drop
   r@ hc-rh-psta@ 100000 and 0=  if  abort  then
   h# 1f0000 r> hc-rh-psta!     \ clear status change bits
   d# 256 ms
;

: reset-usb  ( -- )
   1 hc-rh-stat!        \ power-off root hub
   1 hc-cmd!            \ reset usb host controller
   100 begin 
        hc-stat@ 1 and
        over 0> and 
    while 1- 10 ms repeat
   0= if ." Reset Time" then 
;

: init-usb  ( -- )  
\ debug? if ." init-usb" cr then
   hcca-phys hc-hcca!   \ address of hcca
   81 hc-cntl!          \ USB operational, 2:1 ControlBulkServiceRatio
   d# 10 ms
   h# a6682edf 34 usb!  \ FIT|FSLargestDataPacket|FrameInterval
   2580 40 usb!     \ HcPeriodicStart
;

: (process-hc-status)  ( -- )  
   hc-intr@ dup hc-intr!
   h# 10 and  if ." Unrecoverable error" then \ not implemented on tc7010
;

' (process-hc-status) is process-hc-status

: wait-for-frame  ( -- )  
\ debug if ." wait-for-frame" cr then 
  begin  hc-intr@ 4 and until  ;
: next-frame      ( -- )  4 hc-intr!  wait-for-frame    ;


\ Kick the USB controller into operation mode.
: start-usb     ( -- )  c0 hc-cntl-clr 80 hc-cntl-set  ;
: suspend-usb   ( -- )  c0 hc-cntl-set  ;

