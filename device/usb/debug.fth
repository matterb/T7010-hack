\ File: debug.fth
\ debugging words for ohci

hex

only forth
[defined] usbdefs [if]
     also system also usbdefs
[then]
definitions

: .td-status ( td -- )  \ display td completion code
  >hctd-control @ d# 28 rshift case           \ FIXME this is a bitfield
    1 of ." CRC errors" endof
    2 of ." BIT stuffing violations: endof
    3 of ." PID unexpected" endof
    4 of ." STALL PID received" endof
    5 of ." Device IN: no response or OUT : no handshake" endof
    6 of ." Failed check bits: endof
  endcase
  cr
;


: .hc-rh-psta ( port -- )
  dup 1+ ." Port " u. 
  hc-rh-psta@ dup h# 10000 and if
    dup h# 200 and if ." Lowspeed," else ." Highspeed," then
    dup 1 and if ." dev. attached -" ( reset-port ) 
    else ." dev. removed -" ( delete device ) then
    dup 2 and if ." enabled -" ( setup-usb-device) 
    else  ." disabled -" then
    dup 100 and if ." suspended -" else ." not susp -" then
    8 and if ." overcurrent" then
  then
;

only forth definitions

        
