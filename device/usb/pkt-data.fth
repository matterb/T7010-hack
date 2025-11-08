\ file: pkt-data.fth
\ purpose: USB Data Packet Definitions
\ See license at end of file

hex
\ headers

d# 128 constant #max-dev
d#  16 constant #max-endpoint

0 constant speed-full
1 constant speed-low
2 constant speed-high

\    8 constant /pipe0
 d# 64 constant /pipe0
10000 constant /cfg
  100 constant /str

\ Structure of devices requests as defined in USB spec.
begin-structure /dr         ( standard device requests )
    cfield: >dr-rtype   \ bmRequestType
    cfield: >dr-request \ bRequest
    wfield: >dr-value   \ wValue
    wfield: >dr-index   \ wIndex
    wfield: >dr-len     \ wLength
end-structure

\ >dr-type constants
00 constant DR_OUT
80 constant DR_IN
00 constant DR_STANDARD
20 constant DR_CLASS
40 constant DR_VENDOR
00 constant DR_DEVICE
01 constant DR_INTERFACE
02 constant DR_ENDPOINT
03 constant DR_OTHERS
DR_CLASS DR_DEVICE    or constant DR_HUB
DR_CLASS DR_OTHERS    or constant DR_PORT
DR_CLASS DR_INTERFACE or constant DR_HIDD

\ >dr-request constants
01 constant CLEAR_FEATURE
08 constant GET_CONFIGURATION
06 constant GET_DESCRIPTOR
0a constant GET_INTERFACE
02 constant GET_STATE
00 constant GET_STATUS
05 constant SET_ADDRESS
09 constant SET_CONFIGURATION
07 constant SET_DESCRIPTOR
03 constant SET_FEATURE
0b constant SET_INTERFACE
0c constant SYNCH_FRAME

\ >dr-value (upper byte) for get-/set-descriptor constants
\ lower-byte is descriptor index
01 constant DEVICE
02 constant CONFIGURATION
03 constant STRING
04 constant INTERFACE
05 constant ENDPOINT
0b constant INTERFACE_ASSO
29 constant HUB

\ Hub Class Feature Selectors (dr-value)
00 constant C_HUB_LOCAL_POWER
01 constant C_HUB_OVER_CURRENT
00 constant PORT_CONNECTION
01 constant PORT_ENABLE
02 constant PORT_SUSPEND
03 constant PORT_OVER_CURRENT
04 constant PORT_RESET
08 constant PORT_POWER
09 constant PORT_LOW_SPEED
d# 16 constant C_PORT_CONNECTION
d# 17 constant C_PORT_ENABLE
d# 18 constant C_PORT_SUSPEND
d# 19 constant C_PORT_OVER_CURRENT
d# 20 constant C_PORT_RESET
d# 21 constant PORT_TEST
d# 22 constant PORT_INDICATOR

\ Use tmp-l to make sure that le-l! and le-w! are atomic writes

variable tmp-l
: le-w@   ( a -- w )    dup c@ swap 1+ c@ bwjoin  ;
: (le-w!) ( w a -- )   >r  wbsplit r@ 1+ c! r> c!  ;
: le-w!   ( w a -- )   swap tmp-l (le-w!) tmp-l w@ swap w!  ;

: le-l@   ( a -- l )   >r  r@ c@  r@ 1+ c@  r@ 2+ c@  r> 3 + c@  bljoin  ;
: (le-l!) ( l a -- )   >r lbsplit  r@ 3 + c!  r@ 2+ c!  r@ 1+ c!  r> c!  ;
: le-l!   ( l a -- )   swap tmp-l (le-l!) tmp-l @ swap !  ;

