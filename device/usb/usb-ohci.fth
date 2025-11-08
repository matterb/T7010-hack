\ file: preamble.fth
\ purpose: define words absent from camelforth
\ only forth definitions
[defined] system [if]  also system  definitions
    vocabulary usbdefs
[then]

: 2+ 2 + ;
: >= < 0= ;
: push-hex r> base @ >r >r hex ;
: pop-base r> r> base ! >r ;
: noop (null) ;
 [undefined] ms [if] : ms drop ; [then]
: alloc-mem ( size -- addr )  allocate throw  ;
: free-mem ( address size -- )  drop free throw ;

\ -----------------------------------------------------

[defined] usbdefs [if]
    also usbdefs definitions
[then]

 
\ ================need definitions ======================
: my-address 0 ;    \ interface
: my-self ;
: find-method ;
: push-package ;
: pop-package ;
: new-instance ;
: destroy-instance ;
: set-default-unit ;
: behavior ( xt1 -- xt2) defer@ ;    \ get the action-of a deferred word
: property type type cr ;
: get-my-property ;
: get-package-property ;
: get-inherited-property ;
: new-device ." new-device" cr ;
: set-args ( arg$ reg$ -- ) 2drop ;
: encode-string ;
: encode-bytes ;
: encode-phys ;
: encode-int ( x .. addr len ) here swap , cell ; \ encoded in dictionary
: decode-int ( addr len -- addr' len' n) over @ >r cell /string r>  ;
: decode-string ;
: encode+ ;
: ihandle>phandle ;
: child ;
: peer ;
: parse-2int ( addr len -- u1 u2) ;

\ ======================= end need defitions ===========================

: $hold  ( adr len -- )
   dup  if  bounds swap 1-
   ?do  i c@ hold  -1 +loop  else  2drop  then
;
: u#>  #> ;
: u#s  #s ;
: usb#>   ( n -- )  s" usb" $hold  0 u#> ;     \ Prepends: usb
: #usb#>  ( n -- )  u#s drop  usb#>  ;        \ Prepends: usbN
: #,      ( n -- )  u#s drop [char] , hold  ;  \ Prepends: ,N
: #.      ( n -- )  u#s drop [char] . hold  ;  \ Prepends: .N
: dma-push     ( virt phys size -- )  2drop drop ;
: dma-pull     ( virt phys size -- )  2drop drop ;
: dma-alloc    ( size -- virt ) allocate  throw ;
: dma-free     ( virt size -- ) drop free throw ;
: dma-map-in   ( virt size cache? -- phys )  2drop  ;
: dma-map-out  ( virt phys size -- ) 2drop drop ;

d# 256 constant /align256
d#  16 constant /align16
d#  32 constant /align32
: aligned-alloc  ( size align -- unaligned-virt aligned-virtual )
   dup >r + dma-alloc  dup r> round-up
;
: aligned-free  ( virtual size align -- )  + dma-free  ;

: aligned16-alloc  ( size -- unaligned-virt aligned-virtual )
   /align16 aligned-alloc
;
: aligned16-free  ( virtual size -- )
   /align16 aligned-free
;

: aligned16-alloc-map-in  ( size -- unaligned-virt aligned-virt phys )
   dup >r aligned16-alloc
   dup r> true dma-map-in
;
: aligned16-free-map-out  ( unaligned virt phys size -- )
   dup >r dma-map-out
   r> aligned16-free
;

: aligned32-alloc  ( size -- unaligned-virt aligned-virtual )
   /align32 aligned-alloc
;
: aligned32-free  ( virtual size -- )
   /align32 aligned-free
;

: aligned32-alloc-map-in  ( size -- unaligned-virt aligned-virt phys )
   dup >r aligned32-alloc
   dup r> true dma-map-in
;
: aligned32-free-map-out  ( unaligned virt phys size -- )
   dup >r dma-map-out
   r> aligned32-free
;

: aligned256-alloc  ( size -- unaligned-virt aligned-virtual )
   /align256 aligned-alloc
;
: aligned256-free  ( virtual size -- )
   /align256 aligned-free
;

[undefined] usbdefs [if]
    : usb@ ( -- ) 0 ;
    : usb! ( val port --) 2drop ;
[then]

defer end-extra     ' noop is end-extra
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

\ file: pkt-func.fth
\ purpose: USB Data Packet Manipulation
\ See license at end of file

hex

\ XXX This code assumes the device and configuration descriptors are ok.

false value class-in-dev?

: find-desc  ( adr type -- adr' )
   ." find-desc" cr
   swap  begin  ?dup  while     ( type adr )
      dup 1+ c@ 2 pick =  if  0  else  dup c@ +  then
      ( type adr' )
   repeat  nip              ( adr )
;

: find-intf-desc  ( adr intfidx -- adr )
   ." find-intf-desc " .s cr 
   swap  begin              ( intfidx adr )
      INTERFACE find-desc       ( intfidx adr' )
   swap ?dup  while         ( adr intfidx )
      1- swap               ( intfidx' adr )
      dup c@ +              ( intfidx adr' )
   repeat
;

: unicode$>ascii$  ( adr -- actual )
   dup c@ 2 - 2/ swap 2 + over 0  ?do   ( actual adr' )
      dup i 2* 1+ + c@ 0=  if       \ ASCII
         dup i 2* + c@          ( actual adr c )
      else              \ Non-ascii
         [char] ?           ( actual adr c )
      then
      over 2 - i + c!           ( actual adr )
   loop  drop
;

\ XXX In the future, maybe we can decode more languages.
: encoded$>ascii$  ( adr lang -- actual )
   drop unicode$>ascii$
;


.( file: hcd.fth )
\ purpose: Generic HCD Driver
\ See license at end of file

hex

defer unstall-pipe  ( pipe -- )     ' drop to unstall-pipe

d#  500 constant nodata-timeout
d# 5000 constant data-timeout

: int-property  ( n name$ -- )     rot encode-int  2swap property  ;
: str-property  ( str$ name$ -- )  2swap encode-string 2swap  property  ;
\ ---------------------------------------------------------------------------
\ Common variables
\ ---------------------------------------------------------------------------

0 value target
true value debug?
false value noprobe?

\ Setup and descriptor DMA data buffers
0 value setup-buf           \ SETUP packet buffer
0 value setup-buf-phys
0 value cfg-buf             \ Descriptor packet buffer
0 value cfg-buf-phys

: alloc-dma-buf  ( -- ) 
   setup-buf 0=  if
      /dr dma-alloc dup to setup-buf
      /dr true dma-map-in to setup-buf-phys
   then
   cfg-buf 0=  if
      /cfg dma-alloc dup to cfg-buf
      /cfg true dma-map-in to cfg-buf-phys
   then
;
: free-dma-buf  ( -- )  
   setup-buf  if 
      setup-buf setup-buf-phys /dr dma-map-out
      setup-buf /dr dma-free
      0 to setup-buf 0 to setup-buf-phys
   then
   cfg-buf  if   
      cfg-buf cfg-buf-phys /cfg dma-map-out
      cfg-buf /cfg dma-free
      0 to cfg-buf 0 to cfg-buf-phys
   then
;

0 value locked?  \ Interrupt lockout for USB keyboard get-data?
: lock    ( -- )  true  to locked?  ;
: unlock  ( -- )  false to locked?  ;

\ ---------------------------------------------------------------------------
\ Common routines
\ ---------------------------------------------------------------------------

\ XXX Room for improvement: keep tab of hcd-map-in's to improve performance.
: hcd-map-in   ( virt size -- phys )  false dma-map-in  ;
: hcd-map-out  ( virt phys size -- )  dma-map-out  ;


: log2  ( n -- log2-of-n )
   0  begin        ( n log )
      swap  2/     ( log n' )
   ?dup  while     ( log n' )
      swap 1+      ( n' log' )
   repeat          ( log )
;
: exp2  ( n -- 2**n )  1 swap 0 ?do  2*  loop  ;
: interval  ( interval -- interval' )  log2 exp2  ;

: 3dup   ( n1 n2 n3 -- n1 n2 n3 n1 n2 n3 )  2 pick 2 pick 2 pick  ;
: 3drop  ( n1 n2 n3 -- )  2drop drop  ;

: 4dup   ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 n3 n4 )  2over 2over  ;
: 4drop  ( n1 n2 n3 n4 -- )  2drop 2drop  ;

: 5dup   ( n1 n2 n3 n4 n5 -- n1 n2 n3 n4 n5 n1 n2 n3 n4 n5 )
   4 pick 4 pick 4 pick 4 pick 4 pick
;
: 5drop  ( n1 n2 n3 n4 n5 -- )  2drop 3drop  ;

: $=  ( adr len adr len -- =? )
   rot tuck <> if    
      3drop false exit 
   then  s= 0= 
; 

\ ---------------------------------------------------------------------------
\ Exported methods
\ ---------------------------------------------------------------------------


: debug-on  ( -- )  true to debug?  ;

: set-target  ( target -- )  to target ." set-target" cr  ;

\ A usb device node defines an address space of the form
\ "port,interface".  port and interface are both integers.
\ parse-2int converts a text string (e.g. "3,1") into a pair of
\ binary integers.

: decode-unit  ( addr len -- interface port )  parse-2int  ;
: encode-unit  ( interface port -- adr len )
   >r s>d <# u#s drop [char] , hold r> swap u#s u#>   \ " port,interface"
2dup type cr  ;

false value fisheye?


\ file: error.fth
\ purpose: USB error codes for use by USB device drivers

hex

\ Common error codes so that device drivers are EHCI/OHCI/UHCI independent
00000000 constant USB_ERR_NONE
00000001 constant USB_ERR_CRC
00000002 constant USB_ERR_BITSTUFFING
00000004 constant USB_ERR_DATATOGGLEMISMATCH
00000008 constant USB_ERR_STALL
00000010 constant USB_ERR_DEVICENOTRESPONDING
00000020 constant USB_ERR_PIDCHECKFAILURE
00000040 constant USB_ERR_UNEXPECTEDPIC
00000080 constant USB_ERR_DATAOVERRUN
00000100 constant USB_ERR_DATAUNDERRUN
00000200 constant USB_ERR_BUFFEROVERRUN
00000400 constant USB_ERR_BUFFERUNDERRUN
00000800 constant USB_ERR_NOTACCESSED
00001000 constant USB_ERR_HCHALTED
00002000 constant USB_ERR_DBUFERR
00004000 constant USB_ERR_BABBLE
00008000 constant USB_ERR_NAK
00010000 constant USB_ERR_MICRO_FRAME
00020000 constant USB_ERR_SPLIT
00040000 constant USB_ERR_HCERROR
00080000 constant USB_ERR_HOSTERROR
10000000 constant USB_ERR_TIMEOUT
20000000 constant USB_ERR_INV_OP
40000000 constant USB_ERR_BAD_PARAM
80000000 constant USB_ERR_UNKNOWN

0 value usb-error

: clear-usb-error  ( -- ) 0 to usb-error  ;
: set-usb-error  ( $ err -- )
   usb-error or to usb-error
   debug?  if  ." set-usb-error " type  cr  else  2drop  then
;


\ file: dev-info.fth
\ purpose: Data structures and manuipulation routines for all USB Controllers
\ See license at end of  module

hex
\ ---------------------------------------------------------------------------
\ device-info (DI): data structure internal to the OHCI driver.
\ It keeps track of assigned USB addresses.  For each USB device, its speed.
\ And for each endpoint in a device, its max packet size and data toggle bit.
\
\ This data structure, once created, stays around at all time.
\ ---------------------------------------------------------------------------

begin-structure  /di-ep-struct
    wfield: >di-ep-maxpayload       \ Max packet size for the endpoint
    cfield: >di-ep-in-toggle        \ Data toggle state
    cfield: >di-ep-out-toggle       \ Data toggle state
    cfield: >di-ep-type             \ contrl,bulk,intr,isochr
    cfield: >di-ep-interval         \ binary log of microframes count
                                    \ 125usec*2^interval
    cfield: >di-ep-device           \ device number
end-structure

begin-structure  /di-entry
    field:  >di-name        \ name of device
    cfield: >di-speed       \ Device speed
    cfield: >di-hub         \ Hub address (EHCI only)
    cfield: >di-port        \ Port number (EHCI only)
    cfield: >di-reset       \ reset flag- 0 initially or after a resume, then 1
    field:  >di-#ep         \ number of endpoints
    field:  >di-desc        \ device descriptor
    field:  >di-configs     \ all device configurations
    field:  >di-act-config  \ active configuration 
    field:  >di-data        \ auxillary data buffer?
    field:  >di-lang-id     \ language id for strings
    field:  >di-product
    field:  >di-manufac     \ manufacturer
    field:  >di-serial
/di-ep-struct #max-endpoint * 
    +field >di-ep           \ Endpoint structure
end-structure

/di-entry #max-dev * constant /di
0 value di              \ device-info
0 value cur-dev         \ Value of the last assigned usb address

: 'di     ( idx -- adr )       /di-entry * di +  ;
: 'di-ep  ( pipe idx -- adr )  'di >di-ep swap /di-ep-struct * +  ;
: di-desc! ( addr idx -- ) 'di >di-desc ! ;
: di-desc@ ( idx -- addr ) 'di >di-desc @ ;
: di-configs! ( addr idx -- ) 'di >di-configs ! ;
: di-configs@ ( idx -- addr ) 'di >di-configs @ ;

: di-act-config! ( addr idx -- ) 'di >di-act-config ! ;
: di-act-config@ ( idx -- addr ) 'di >di-act-config @ ;
: di-langid! ( addr idx -- ) 'di >di-lang-id ! ;
: di-langid@ ( idx --addr ) 'di >di-lang-id @ ;
: di-product! ( addr idx -- ) 'di >di-product ! ;
: di-product@ ( idx --addr ) 'di >di-product @ ;
: di-manufac! ( addr idx -- ) 'di >di-manufac ! ;
: di-manufac@ ( idx --addr ) 'di >di-manufac @ ;
: di-serial! ( addr idx -- ) 'di >di-serial ! ;
: di-serial@ ( idx --addr ) 'di >di-serial @ ;
: di-speed!  ( speed idx -- )  'di >di-speed c!  ;
: di-speed@  ( idx -- speed )  'di >di-speed c@  ;
: di-hub!    ( hub idx -- )    'di >di-hub   c!  ;
: di-hub@    ( idx -- hub )    'di >di-hub   c@  ;
: di-port!   ( port idx -- )   'di >di-port  c!  ;
: di-port@   ( idx -- port )   'di >di-port  c@  ;
: di-is-reset  ( idx -- )
   'di >di-reset          ( adr )
   0 swap c!
;
: di-reset?  ( idx -- flag )
   'di >di-reset          ( adr )
   dup c@ 0=              ( adr reset? )
   1 rot c!               ( reset? )
;

: 'di-maxpayload  ( pipe idx -- adr )  'di-ep >di-ep-maxpayload  ;
: di-maxpayload!  ( len pipe idx -- )  'di-maxpayload w!  ;
: di-maxpayload@  ( pipe idx -- len )  'di-maxpayload w@  ;

: di-in-data!   ( n pipe id -- )       'di-ep >di-ep-in-toggle  c!  ;
: di-in-data@   ( pipe id -- n )       'di-ep >di-ep-in-toggle  c@  ;
: di-out-data!  ( n pipe id -- )       'di-ep >di-ep-out-toggle c!  ;
: di-out-data@  ( pipe id -- n )       'di-ep >di-ep-out-toggle c@  ;
: di-in-data-toggle   ( pipe idx -- )  2dup di-in-data@  1 xor -rot di-in-data!   ;
: di-out-data-toggle  ( pipe idx -- )  2dup di-out-data@ 1 xor -rot di-out-data!  ;
: di-name! ( addr idx -- ) 'di >di-name ! ;
: di-name@ ( idx -- addr len ) 'di >di-name @ count ;



: next-device#  ( -- true | dev false )
   cur-dev 1+ #max-dev >=  if  true exit  then
   cur-dev 1+ dup to cur-dev  
   /pipe0 0 cur-dev di-maxpayload!
   false
;

: init-di  ( -- )  
   di 0=  if
      \ allocate and initialize the device descriptors
      /di alloc-mem to di
   then
   di /di erase
   /pipe0 0 0 di-maxpayload!        \ Default max payload
;

: init-struct  ( -- )
   init-di
   0 to cur-dev
;


\ file: edtd.fth
\ purpose: Data structures and manuipulation routines for OHCI USB Controller

hex

\ ---------------------------------------------------------------------------
\ Data structures for this implementation of the OHCI USB Driver include:
\   - hcca      256 bytes defined by OCHI Spec for USB HC
\   - ed-control    pointer to the control ED list
\   - ed-bulk       pointer to the bulk ED list
\   - intr      internal array of interrupts (to complement the hcca)
\ ---------------------------------------------------------------------------

\ ---------------------------------------------------------------------------
\ HcHCCA as defined by the OHCI Spec; 256-byte aligned
\ ---------------------------------------------------------------------------

0 value intr            \ Software interrupt buffer
0 value hcca            \ Virtual address of HcHCCA
0 value hcca-unaligned      \ Unaligned virtual address of HcHCCA
0 value hcca-phys       \ Physical address of HcHCCA

\ HCCA
d# 32 constant #intr

begin-structure /hcca  ( hcca )
#intr 4 * 
    +field  >hcca-intr  \ Physical addresses of interrupt EDs
    wfield: >hcca-frame
    wfield: >hcca-pad
    field:  >hcca-done      \ Physical addresses of done EDs
d# 120
    +field  >hcca-reserved
end-structure

: hcca!  ( padr idx -- )  4 * hcca + !  ;

: init-hcca  ( -- ) 
   \ Allocate hcca
   /hcca aligned256-alloc
   dup to hcca              \ Aligned address
   swap to hcca-unaligned       \ Unaligned address
   /hcca true dma-map-in to hcca-phys   \ Physical address

   \ Initialize hcca
   hcca /hcca erase
   hcca hcca-phys /hcca dma-push
 ." init-hcca " hcca u. cr
;

\ debugging aids
: hcca?  
 push-hex 
   hcca dup ." hcca at :" u. cr
   h# 100 dump cr
 pop-base
;


\ ---------------------------------------------------------------------------
\ Internal interrupt list per >hcca-intr entry
\
\ XXX I can see how this can be expanded to >intr-head32ms, >intr-tail32ms,
\ XXX and so on, to support the various poll intervals.  See comment on
\ XXX interrupt scheduling below.
\ ---------------------------------------------------------------------------
begin-structure  /intr-entry   \ An entry of intr
    field: >intr-head       \ address of interrupt head
    field: >intr-tail       \ address of interrupt tail
    field: >iso-head        \ address of isochronous head
    field: >iso-tail        \ address of isochronous tail
end-structure
/intr-entry #intr * constant /intr

: init-intr  ( -- )
   /intr alloc-mem dup to intr      \ Allocate intr
   /intr erase              \ Initialize intr
." init-intr " intr u. cr
;

: 'intr  ( idx -- adr )   /intr-entry * intr +  ;
: intr-head@  ( idx -- adr )  'intr >intr-head @  ;
: intr-head!  ( adr idx -- )  'intr >intr-head !  ;
: intr-tail@  ( idx -- adr )  'intr >intr-tail @  ;
: intr-tail!  ( adr idx -- )  'intr >intr-tail !  ;
: iso-head@   ( idx -- adr )  'intr >iso-head @   ;
: iso-head!   ( adr idx -- )  'intr >iso-head !   ;
: iso-tail@   ( idx -- adr )  'intr >iso-tail @   ;
: iso-tail!   ( adr idx -- )  'intr >iso-tail !   ;

\ ---------------------------------------------------------------------------
\ Endpoint descriptor (ED) as defined by the OHCI Spec; 16-byte aligned
\ ---------------------------------------------------------------------------

\ XXX If we add ed-control-tail & ed-bulk-tail, then insert-* does not have
\ XXX to disable the function, we need to skip tail until insert is done.

0 value ed-control      \ address of head of control ED list
0 value ed-bulk         \ address of head of bulk ED list

begin-structure /ed            \ Beginning of ED
    field: >hced-control       \ ED control info
    field: >hced-tdtail        \ Physical address of TD tail
    field: >hced-tdhead        \ Physical address of TD head
    field: >hced-next      \ Physical address of next ED
dup constant /hced
\ Driver specific fields
    field: >ed-phys        \ Physical address of HC ED
    field: >ed-next        \ Pointer to the next endpoint
    field: >ed-prev        \ Pointer to the previous endpoint
    field: >ed-unaligned   \ Unaligned virtual address of the ED
    field: >ed-size        \ Size of EDs+TDs
d# 32 round-up          \ Multiple of 32 bytes
                \ 32 bytes because there are cases where
                \ EDs and TDs are allocated together
end-structure
/ed #intr * constant /eds       \ Size of all eds allocated at a time

\ >hced-control constants
0000 constant ED_DIR_TD
0800 constant ED_DIR_OUT
1000 constant ED_DIR_IN
1800 constant ED_DIR_MASK

0000 constant ED_SPEED_FULL
2000 constant ED_SPEED_LO
2000 constant ED_SPEED_MASK

0000 constant ED_SKIP_OFF
4000 constant ED_SKIP_ON
4000 constant ED_SKIP_MASK

0000 constant ED_FORMAT_G
8000 constant ED_FORMAT_I
8000 constant ED_FORMAT_MASK

0000 constant ED_TOGGLE_DATA0
0002 constant ED_TOGGLE_DATA1
0002 constant ED_TOGGLE_MASK

0001 constant ED_HALTED

: ed-data>di-data  ( n -- n' )  ED_TOGGLE_MASK and  if  1  else  0  then  ;
: di-data>ed-data  ( n -- n' )  if  ED_TOGGLE_DATA1  else  ED_TOGGLE_DATA0  then  ;

: (set-skip)  ( ed skip-bit -- )
   >r
   dup >hced-control dup @
   ED_SKIP_MASK invert and r> or
   swap !
   dup >ed-phys @ /hced dma-push
;
: ed-set-skip    ( ed -- )  ED_SKIP_ON  (set-skip)  ;
: ed-unset-skip  ( ed -- )  ED_SKIP_OFF (set-skip)  ;

\ ---------------------------------------------------------------------------
\ Transfer Descriptor (TD) as defined by the OHCI Spec:
\ general TDs are 16-byte aligned; isochronous TDs are 32-byte aligned.
\ ---------------------------------------------------------------------------

begin-structure    /td     \ Beginning of General TD fields
    field: >hctd-control       \ TD control info
    field: >hctd-cbp       \ Physical address of current buffer pointer
    field: >hctd-next      \ physical address of next TD
    field: >hctd-be        \ physical address of buffer end
dup constant /gtd
                \ Isochronous TD fields
    wfield: >hctd-offset0       \ Offset 0 / PSW 0
    wfield: >hctd-offset1       \ Offset 1 / PSW 1
    wfield: >hctd-offset2       \ Offset 2 / PSW 2
    wfield: >hctd-offset3       \ Offset 3 / PSW 3
    wfield: >hctd-offset4       \ Offset 4 / PSW 4
    wfield: >hctd-offset5       \ Offset 5 / PSW 5
    wfield: >hctd-offset6       \ Offset 6 / PSW 6
    wfield: >hctd-offset7       \ Offset 7 / PSW 7
dup constant /itd
                \ Driver specific fields
    field: >td-phys        \ Physical address of HC TD
    field: >td-next        \ Virtual address of next TD
    field: >td-cbp         \ Virtual address of current buffer pointer
    field: >td-pcbp        \ Physical address of current buffer pointer
    field: >td-/cbp-all        \ Buffer length (size of the entire buffer)
                \ For bulk and intr TDs
d# 32 round-up          \ Multiple of 32 bytes
end-structure

\ >hctd-control constants
00040000 constant TD_ROUND_ON
00000000 constant TD_ROUND_ERR
00040000 constant TD_ROUND_MASK

00000000 constant TD_DIR_SETUP
00080000 constant TD_DIR_OUT
00100000 constant TD_DIR_IN
00180000 constant TD_DIR_MASK

00c00000 constant TD_INTR_MIN
00e00000 constant TD_INTR_OFF
00e00000 constant TD_INTR_MASK

00000000 constant TD_TOGGLE_USE_ED
02000000 constant TD_TOGGLE_USE_LSB0
03000000 constant TD_TOGGLE_USE_LSB1
01000000 constant TD_TOGGLE_MASK
0c000000 constant TD_ERR_CNT_MASK

00000000 constant TD_CC_NOERROR
10000000 constant TD_CC_CRC
20000000 constant TD_CC_BITSTUFFING
30000000 constant TD_CC_DATATOGGLEMISMATCH
40000000 constant TD_CC_STALL
50000000 constant TD_CC_DEVICENOTRESPONDING
60000000 constant TD_CC_PIDCHECKFAILURE
70000000 constant TD_CC_UNEXPECTEDPID
80000000 constant TD_CC_DATAOVERRUN
90000000 constant TD_CC_DATAUNDERRUN
c0000000 constant TD_CC_BUFFEROVERRUN
d0000000 constant TD_CC_BUFFERUNDERRUN
f0000000 constant TD_CC_NOTACCESSED
f0000000 constant TD_CC_MASK

: td-data>di-data  ( n -- n' )  TD_TOGGLE_MASK and  if  1  else  0  then  ;
: di-data>td-data  ( n -- n' )  if  TD_TOGGLE_USE_LSB1  else  TD_TOGGLE_USE_LSB0  then  ;

\ ---------------------------------------------------------------------------

: .tds ( td-head --) ." >>TD list:" cr 
 begin
    dup >hctd-control  @ ." >>>>TD-Control:" .hex8 cr
    dup >hctd-cbp @ ." >>>>TD-CBP:" .hex8 cr
    dup >hctd-be @ ." >>>>TD-Buffer-End:" .hex8 cr
    >hctd-next @ dup ." >>>>Next-TD:" .hex8 cr
 dup 0=  until
 drop ." >>TD-ends" cr
;

: .eds ( ed -- )    \ dump eds and tds
 begin
    dup >hced-control  @ ." ED-Control:" .hex8 cr
    dup >hced-tdhead @ dup ." TD-Head:" .hex8 cr fffffff0 and 
    over >hced-tdtail @ dup ." TD-Tail:" .hex8 cr
    over <> if cr .tds cr else drop then
    >hced-next @ dup ." Next-ED:" .hex8 cr
dup 0= until
drop
;


: init-struct  ( -- )  
   init-struct
   0 to ed-control 0 to ed-bulk
   init-hcca
   init-intr
;

\ ---------------------------------------------------------------------------
\ ED and TDs for bulk, control and interrupt operations.
\ ED and its list of TDs are allocated as needed.
\ ---------------------------------------------------------------------------

: init-ed  ( ed.u,v,p len -- )  
   2 pick >ed-size !           ( ed.u,v,p )
   over >ed-phys !         ( ed,u,v )
   >ed-unaligned !         ( )
;

: link-tds  ( td.v td.p #tds -- ) 
   1- 0  ?do                ( td.v td.p )
      2dup swap >td-phys !     ( td.v td.p )
      /td + tuck over >hctd-next !  ( td.p' td.v )
      dup /td + tuck swap       ( td.p td.v' td.v' td.v )
      >td-next !           ( td.p td.v )
      swap              ( td.v td.p )
   loop
   swap >td-phys !         ( )
;
: link-edtd  ( td.p #tds ed -- )  
   >r                   ( td.p #tds )  ( R: ed )
   1- /td * over +          ( td.p ptail )  ( R: ed )
   r@ >hced-tdtail !        ( td.p )  ( R: ed )
   r> >hced-tdhead !        ( )
;
: link-edtds  ( td.v td.p #tds ed -- )
   >r 2dup r> link-edtd         ( td.v td.p #tds )  \ Link ED to TD
   link-tds             ( )         \ Link TDs
;
: alloc-edtds  ( #tds -- ed td )  
\   debug? if ." alloc-edtds" cr then
   dup >r /td * /ed + dup >r        ( len )  ( R: #tds len )
   aligned32-alloc-map-in       ( ed.u,v,p )  ( R: #tds len )
   over r@ erase            ( ed.u,v,p )  ( R: #tds len )
   3dup r> init-ed          ( ed.u,v,p )  ( R: #tds )
   rot drop             ( ed.v,p )  ( R: #tds )
   over /ed + dup -rot          ( ed td ed.p td.v )  ( R: #tds )
   swap /ed +               ( ed td td.v td.p )  ( R: #tds )
   r> 4 pick link-edtds         ( ed td )
;
: free-edtds  ( ed -- )  
   >r                   ( R: ed )
   r@ >ed-unaligned @          ( ed.u )  ( R: ed )
   r@ dup >ed-phys @           ( ed.u,v,p )  ( R: ed )
   r> >ed-size @           ( ed.u,v,p size )
   aligned32-free-map-out       ( )
;
: push-edtds  ( ed -- )   
   dup >ed-phys @          ( ed.v,p )
   over >ed-size @         ( ed.v,p len )
   dma-push             ( )
;
: pull-edtds  ( ed -- )  
   dup >ed-phys @          ( ed.v,p )
   over >ed-size @         ( ed.v,p len )
   dma-pull             ( )
;
: map-out-cbp  ( td -- )
   dup >td-cbp @ over >td-pcbp @ rot >td-/cbp-all @ hcd-map-out
;

\ ---------------------------------------------------------------------------
\ Control scheduling
\ ---------------------------------------------------------------------------

: fixup-ed-next-prev  ( ed -- ed )  
   dup >ed-prev @ ?dup  if  over >ed-next @ swap >ed-next !  then
   dup >ed-next @ ?dup  if  over >ed-prev @ swap >ed-prev !  then
;

: insert-ed  ( new-ed old-ed -- )  
   ?dup 0=  if  drop exit  then     \ No old-ed, done
   2dup >ed-prev !         \ old-ed's prev is new-ed
   2dup swap >ed-next !        \ new-ed's next is old-ed
   >ed-phys @ swap >hced-next !    \ new-ed's hced-next is old-ed's phys
;

: insert-control-ed  ( ed -- )   
   dup ed-control insert-ed
   to ed-control
;
: remove-control-ed  ( ed -- )  
   fixup-ed-next-prev           ( ed )
   dup ed-control =  if  >ed-next @ to ed-control  else  drop  then
;

\ ---------------------------------------------------------------------------
\ Bulk scheduling
\ ---------------------------------------------------------------------------

: insert-bulk-ed  ( ed -- )
   dup ed-bulk insert-ed
   to ed-bulk
;
: remove-bulk-ed  ( ed -- )
   fixup-ed-next-prev           ( ed )
   dup ed-bulk =  if  >ed-next @ to ed-bulk  else  drop  then
;

\ ---------------------------------------------------------------------------
\ Interrupt scheduling
\ Schedule interrupt at the rate min(interval,2**x).
\
\ XXX Need to determines which scheduling queue for that rate has the smallest
\ committed bandwidth.
\
\ XXX To really implement the various poll intervals, the simplistic way is
\ XXX to have 32 dummy EDs for 1ms interval; 16 dummy EDs for 2ms interval;
\ XXX 8 dummy EDs for 4ms interval; 4 dummy EDs for 8ms interval; 
\ XXX 2 dummy EDs for 16ms interval; and, 1 dummy ED for 32ms interval.
\ XXX Then you link to the end of the lists of EDs for each interval.  Ughhh!
\
\ XXX For now, just implement fixed poll interval.
\
\ XXX On further thought, since we're polling the intr pipeline from the
\ XXX device driver, the driver driver can poll the intr at the interval
\ XXX specified.  And thus, the need to fully implement poll intervals at
\ XXX the HCD level is redundant.
\ ---------------------------------------------------------------------------

8 constant intr-interval

: (insert-intr-ed)  ( ed idx -- )
   dup >r               ( ed idx )  ( R: idx )
   intr-tail@ ?dup 0=  if       ( ed )  ( R: idx )
      dup r@ intr-head!         ( ed )  ( R: idx )
      dup >ed-phys @ r@ hcca!      ( ed )  ( R: idx )
   else                 ( ed ted )  ( R: idx )
      2dup >ed-next !          ( ed ted )  ( R: idx )
      over >ed-phys @ over >hced-next !    ( ed ted )  ( R: idx )
      over >ed-prev !          ( ed )  ( R: idx )
   then
   r@ iso-head@ over >ed-next !    ( ed )  ( R: idx )
   r> intr-tail!            ( )
;
: insert-intr-ed  ( ed interval -- )
   drop
   #intr 0  do  dup i (insert-intr-ed)  intr-interval +loop  drop
;

: (remove-intr-ed)  ( ed idx -- )
   >r                   ( ed )  ( R: idx )
   fixup-ed-next-prev           ( ed )  ( R: idx )
   r@ intr-head@ over =  if     ( ed )  ( R: idx )
      dup >ed-next @ dup r@ intr-head! ( ed ped )  ( R: idx )
      dup  if  >ed-phys @  then  r@ hcca!
                        ( ed )  ( R: idx )
   then
   r@ intr-tail@ over =  if     ( ed )  ( R: idx )
      dup >ed-prev @ r@ intr-tail! ( ed )  ( R: idx )
   then
   r> 2drop
;
: remove-intr-ed  ( ed -- )
   #intr 0  do  dup i (remove-intr-ed)  intr-interval +loop  drop
;

\ ---------------------------------------------------------------------------
\ Wait for an ED to be done and process any errors.
\
\ When done? returns no error found yet, the caller should  check if errors
\ were found in the TDs.
\ ---------------------------------------------------------------------------

defer process-hc-status

0 value timeout

: .td-error  ( cc -- )
   case
      TD_CC_CRC             of  s" CRC"   USB_ERR_CRC  endof
      TD_CC_BITSTUFFING     of  s" Bit Stuffing"  USB_ERR_BITSTUFFING  endof
      TD_CC_DATATOGGLEMISMATCH  of  s" Data Toggle Mismatch" USB_ERR_DATATOGGLEMISMATCH  endof
      TD_CC_STALL   of  s" Stall"   USB_ERR_STALL  endof
      TD_CC_DEVICENOTRESPONDING of s" Device Not Responding" USB_ERR_DEVICENOTRESPONDING  endof
      TD_CC_PIDCHECKFAILURE of  s" PID Check Failure"    USB_ERR_PIDCHECKFAILURE  endof
      TD_CC_UNEXPECTEDPID   of  s" Unexpected PID"  USB_ERR_UNEXPECTEDPIC  endof
      TD_CC_DATAOVERRUN     of  s" Data Overrun"   USB_ERR_DATAOVERRUN  endof
      TD_CC_DATAUNDERRUN    of  s" Data Underrun"  USB_ERR_DATAUNDERRUN  endof
      TD_CC_BUFFEROVERRUN   of  s" Buffer Overrun" USB_ERR_BUFFEROVERRUN  endof
      TD_CC_BUFFERUNDERRUN  of  s" Buffer Underrun" USB_ERR_BUFFERUNDERRUN  endof
      TD_CC_NOTACCESSED     of  s" Not Accessed"   USB_ERR_NOTACCESSED  endof
      ( default )  s" Unknown Error" rot USB_ERR_UNKNOWN swap
   endcase
   set-usb-error
;

: error?  ( td -- usberr )
   begin
      dup >td-next @  if       \ Process a real TD
         dup >hctd-control @ TD_CC_MASK and  ?dup  if
            .td-error  drop 0       \ Error found in TD
         else
            >td-next @         \ TD's ok, examine the next TD
         then
      else              \ Don't need to process last dummy TD
         drop 0
      then
   ?dup 0=  until
   usb-error
;

: ed-done?  ( ed -- done? )   
   dup  >hced-tdhead @ dup ED_HALTED and    ( ed head halted? )
   swap h# fffffff0 and            ( ed halted? head )
   rot >hced-tdtail @ h# fffffff0 and =    ( halted? head=tail? )
   or                       ( done? )
;

: done?  ( ed -- error? )   
\   debug? if ." done?" cr then
   begin
      process-hc-status
      dup pull-edtds
      dup ed-done? ?dup 0=  if
         1 ms
         timeout 1- dup to timeout 0=
      then
   until 
   ed-done? 0=  if  s" Timeout" USB_ERR_TIMEOUT set-usb-error  then
   usb-error
;

: get-actual  ( td -- actual )
   dup >hctd-cbp @ ?dup  if
      swap >td-pcbp @ -
   else
      dup >hctd-be @ swap >td-pcbp @ - 1+
   then
;


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

\ file: control-ohci.fth
\ purpose: OHCI USB Controller transaction processing

hex

: disable-control  ( -- ) 
  10 hc-cntl-clr  next-frame  0 20 ohci-reg!  ;

: enable-control   ( -- ) 
   ed-control >ed-phys @ 20 ohci-reg!  \ set HcControlHeadED
   2 hc-cmd!                \ mark TD added in control list
   10 hc-cntl-set           \ enable control list processing
;

: insert-control  ( ed -- )  
\   debug? if ." insert-control"  cr then
   ed-control  if  disable-control  then
   ( ed ) insert-control-ed
   enable-control
;
: remove-control  ( ed -- ) 
   disable-control
   ( ed ) remove-control-ed
   ed-control  if  enable-control  then
;

\ Local temporary variables (common for control, bulk & interrupt)

\ my-dev and my-real-dev are created here to deal with set-address.
\ Normally my-dev and my-real-dev are both of the value of target.
\ However, during set-address, target=my-dev=0, my-real-dev is the
\ address to be assigned to my-real-dev.  The correct path to get
\ a device's characteristics is via my-real-dev.

0 value my-dev                  \ Equals to target
0 value my-real-dev             \ Path to device's characteristics
0 value my-dev/pipe             \ Device/pipe for ED

0 value my-speed                \ Speed of my-real-dev
0 value my-maxpayload           \ Pipe's max payload

0 value my-buf                  \ Virtual address of data buffer
0 value my-buf-phys             \ Physical address of data buffer
0 value /my-buf                 \ Size of data buffer

0 value my-td                   \ Current TD head
0 value my-ed                   \ Current ED

: set-real-dev  ( real-dev target -- )      \ For set-address only
   to my-dev to my-real-dev
;
: set-normal-dev   ( -- )           \ Normal operation
   target dup to my-dev to my-real-dev
;
defer set-my-dev        ' set-normal-dev is set-my-dev

: set-my-char  ( pipe -- )   \ Set device's characteristics
\   debug? if ." set-my-char" cr then 
   dup 7 lshift my-dev or to my-dev/pipe        ( pipe )
   my-real-dev dup di-speed@            ( pipe dev speed )
   speed-full =  if  ED_SPEED_FULL  else  ED_SPEED_LO  then  to my-speed
                        ( pipe dev )
   di-maxpayload@ 7ff and  to my-maxpayload     ( )
;
: process-control-args  ( buf phy len -- ) 
\   debug? if ." process-control-args" cr then
   to /my-buf to my-buf-phys to my-buf
   clear-usb-error
   set-my-dev
   0 set-my-char
;

: alloc-control-edtds  ( extra-tds -- )  
\   debug? if ." alloc-control-edtds" cr then 
   /my-buf  if  1+ data-timeout  else  nodata-timeout  then  to timeout
   alloc-edtds to my-td to my-ed
;

: fill-setup-td  ( sbuf sphy slen control -- )  
   TD_CC_NOTACCESSED or TD_DIR_SETUP or TD_INTR_OFF or TD_TOGGLE_USE_LSB0 or
   my-td >hctd-control !
   over + 1-  my-td >hctd-be !
   ( sphy ) my-td 2dup >hctd-cbp !
   ( sphy ) >td-pcbp !
   ( sbuf ) my-td >td-cbp !
;

: fill-io-tds  ( td control -- )
   over >hctd-control !
   my-buf over >td-cbp !
   my-buf-phys over 2dup >hctd-cbp !
   >td-pcbp !
   my-buf-phys /my-buf + 1- swap >hctd-be !
;
: fill-control-io-tds  ( dir -- std )  
   my-td >td-next @                ( dir td )
   /my-buf 0=  if  nip exit  then       ( dir td )
   dup rot                  ( td td dir )
   TD_CC_NOTACCESSED or TD_INTR_OFF or TD_TOGGLE_USE_LSB1 or
                        ( td td control )
   fill-io-tds                  ( td )
   >td-next @                  ( std )
;

: fill-control-ed  ( ed -- )    
   my-dev my-speed or ED_DIR_TD or ED_SKIP_OFF or ED_FORMAT_G or
   my-maxpayload d# 16 lshift or
   swap >hced-control !
;

: insert-my-control  ( -- )  
   my-ed dup fill-control-ed
   dup push-edtds
   insert-control   
;

: remove-my-control  ( -- ) 
   my-ed dup remove-control
   free-edtds
;


\ ---------------------------------------------------------------------------
\ CONTROL pipe operations
\ ---------------------------------------------------------------------------

: (control-get)  ( sbuf sphy slen buf phy len -- actual usberr )
\    debug? if ." (control-get)" cr then
   process-control-args             ( sbuf sphy slen )
   /my-buf 0=  if  3drop 0 USB_ERR_INV_OP exit  then
   3 alloc-control-edtds            ( sbuf sphy slen )

   \ SETUP TD
   TD_ROUND_ON fill-setup-td            ( )

   \ IN TD
   TD_DIR_IN TD_ROUND_ON or fill-control-io-tds ( std )

   \ Status TD (OUT)
   TD_CC_NOTACCESSED TD_DIR_OUT or TD_INTR_MIN or TD_TOGGLE_USE_LSB1 or
   TD_ROUND_ON or
   swap >hctd-control !         ( )

   \ Start control transaction
   insert-my-control                ( )

   \ Process results
   my-ed done?  if              ( )
      0  ." syserror" cr                   ( actual )  \ System error, timeout
   else
      my-td error?  if              ( )
     0                  ( actual )  \ USB error
      else
         my-td >td-next @ dup get-actual   ( td actual )
         over >td-cbp @ rot >td-pcbp @ 2 pick dma-pull    ( actual )
      then
   then

   remove-my-control                ( actual )
   usb-error                    ( actual usberr )
;

: (control-set)  ( sbuf sphy slen buf phy len -- usberr )
\   debug? if ." (control-set)" cr then
   process-control-args             ( sbuf sphy slen )
   3 alloc-control-edtds            ( sbuf sphy slen )

   \ SETUP TD
   0 fill-setup-td              ( )

   \ OUT TD
   TD_DIR_OUT fill-control-io-tds       ( std )

   \ Status TD (IN)                 ( std )
   TD_CC_NOTACCESSED TD_DIR_IN or TD_INTR_MIN or TD_TOGGLE_USE_LSB1 or
   ( TD_ROUND_ON or )
   swap >hctd-control !         ( )

   \ Start control transaction
   insert-my-control

   \ Process results
   my-ed done? 0=  if  my-td error? drop  then
   remove-my-control                ( )
   usb-error                    ( usberr )
;

: (control-set-nostat)  ( sbuf sphy slen buf phy len -- usberr )
   process-control-args             ( sbuf sphy slen )
   2 alloc-control-edtds            ( sbuf sphy slen )

   \ SETUP TD
   0 fill-setup-td              ( )

   \ OUT TD
   TD_DIR_OUT fill-control-io-tds drop      ( )

   \ Start control transaction
   insert-my-control                ( )

   \ Process results
   my-ed done? 0=  if  my-td error? drop then

   remove-my-control                ( )
   usb-error                    ( usberr )
;


\ file: bulk.fth
\ purpose: OHCI USB Controller bulk pipes transaction processing

hex
\ headers

d# 500 value bulk-in-timeout
d# 500 constant bulk-out-timeout

0 value bulk-in-pipe
0 value bulk-out-pipe

0 value bulk-in-ed      \ Instance variables for begin-bulk-in, bulk-in?,
0 value bulk-in-td      \ restart-bulk-in and end-bulk-in.

: disable-bulk  ( -- )  20 hc-cntl-clr  next-frame  0 28 ohci-reg!  ;
: enable-bulk   ( -- )
   ed-bulk >ed-phys @ 28 ohci-reg! \ Set HcBulkHeadED
   4 hc-cmd!                \ Mark TD added in bulk list
   20 hc-cntl-set           \ Enable bulk list processing
;

: insert-bulk  ( ed -- )
   ed-bulk  if  disable-bulk  then
   ( ed ) insert-bulk-ed
   enable-bulk
;
: remove-bulk  ( ed -- )
   disable-bulk
   ( ed ) remove-bulk-ed
   ed-bulk  if  enable-bulk  then
;

: bulk-in-data@  ( -- n )  bulk-in-pipe  target di-in-data@  di-data>ed-data  ;
: bulk-out-data@  ( -- n )  bulk-out-pipe target di-out-data@ di-data>ed-data  ;
: bulk-in-data!   ( n -- )  ed-data>di-data bulk-in-pipe  target di-in-data!   ;
: bulk-out-data!  ( n -- )  ed-data>di-data bulk-out-pipe target di-out-data!  ;
: fixup-bulk-in-data   ( ed -- )
   usb-error USB_ERR_STALL and  if
      drop bulk-in-pipe h# 80 or unstall-pipe
      ED_TOGGLE_DATA0
   else
      >hced-tdhead @ 
   then
   bulk-in-data!
;
: fixup-bulk-out-data  ( ed -- )
   usb-error USB_ERR_STALL and  if
      drop bulk-out-pipe unstall-pipe
      ED_TOGGLE_DATA0
   else
      >hced-tdhead @ 
   then
   bulk-out-data!  
;

: process-bulk-args  ( buf len pipe timeout -- )
   to timeout
   clear-usb-error
   set-my-dev
   ( pipe ) set-my-char
   2dup hcd-map-in  to my-buf-phys to /my-buf to my-buf
;
: alloc-bulk-edtds  ( -- ed td )
   /my-buf  if  2  else  1  then
   ( #tds )  alloc-edtds
;
: fill-bulk-io-tds  ( td -- )
   /my-buf ?dup 0=  if  drop exit  then     ( td len )
   over >td-/cbp-all !             ( td )
   TD_CC_NOTACCESSED TD_INTR_OFF or TD_ROUND_ON or TD_TOGGLE_USE_ED or
                        ( td control )
   fill-io-tds
;
: fill-bulk-ed  ( dir ed -- )
   over my-dev/pipe or my-speed or ED_SKIP_OFF or ED_FORMAT_G or
   my-maxpayload d# 16 lshift  or
   over >hced-control !
   ( ed ) dup >hced-tdhead @        ( dir ed head )
   rot ED_DIR_IN =  if  bulk-in-data@  else  bulk-out-data@  then  or
   over >hced-tdhead !          ( ed )
   ( ed ) push-edtds
;
: insert-my-bulk     ( ed dir -- )  over fill-bulk-ed  insert-bulk  ;
: insert-my-bulk-in  ( ed -- )  ED_DIR_IN  insert-my-bulk  ;
: insert-my-bulk-out ( ed -- )  ED_DIR_OUT insert-my-bulk  ;
: remove-my-bulk     ( ed -- )  dup remove-bulk  free-edtds  ;

\ external

: set-bulk-in-timeout  ( t -- )  ?dup  if  to bulk-in-timeout  then  ;

: begin-bulk-in  ( buf len pipe -- )
   debug?  if  ." begin-bulk-in" cr  then
   bulk-in-ed  if  3drop exit  then     \ Already started

   dup to bulk-in-pipe
   bulk-in-timeout process-bulk-args
   alloc-bulk-edtds to bulk-in-td to bulk-in-ed

   \ IN TD
   bulk-in-td fill-bulk-io-tds

   \ Start bulk in transaction
   bulk-in-ed insert-my-bulk-in
;

: bulk-in-ready?  ( -- false | error true |  buf actual 0 true )
   clear-usb-error              ( )
   process-hc-status                ( )
   bulk-in-ed dup pull-edtds            ( ed )
   ed-done?  if                 ( )
      bulk-in-td error? ?dup 0=  if     ( )
         bulk-in-td >td-cbp @          ( buf )
         bulk-in-td get-actual          ( buf actual )
         2dup bulk-in-td >td-pcbp @ swap dma-pull  ( buf actual )
         0                  ( buf actual 0 )
      then                  ( error | buf actual 0 )
      bulk-in-ed fixup-bulk-in-data     ( error | buf actual 0 )
   else
      false                                     ( false )
   then
;

: bulk-in?  ( -- actual usberr )
   bulk-in-ed 0=  if  0 USB_ERR_INV_OP exit  then
   lock
   clear-usb-error              ( )
   process-hc-status                ( )
   bulk-in-ed dup pull-edtds            ( ed )
   ed-done?  if                 ( )
      bulk-in-td error?  if
         0                  ( actual )
      else
         bulk-in-td dup get-actual      ( td actual )
         over >td-cbp @ rot >td-pcbp @ 2 pick dma-pull    ( actual )
      then
      usb-error                 ( actual usberr )
      bulk-in-ed fixup-bulk-in-data     ( actual usberr )
   else
      0 usb-error               ( actual usberr )
   then
   unlock
;

\ headers
: restart-bulk-in-td  ( td -- )
   begin  ?dup  while
      dup >td-next @  if
         TD_CC_NOTACCESSED TD_DIR_IN or TD_INTR_OFF or TD_ROUND_ON or
         over >hctd-control !
         dup >td-pcbp @ over >hctd-cbp !
         dup >td-next @ >td-phys @ over >hctd-next !
      then
      >td-next @
   repeat
;

\ external
: restart-bulk-in  ( -- )
   debug?  if  ." restart-bulk-in" cr  then
   lock
   bulk-in-ed 0=  if  exit  then
   bulk-in-ed ed-set-skip

   \ Setup TD again
   bulk-in-td restart-bulk-in-td

   \ Setup ED again
   bulk-in-td >td-phys @ bulk-in-data@ or bulk-in-ed >hced-tdhead !
   bulk-in-ed dup push-edtds
   ed-unset-skip
   enable-bulk
   unlock
;

: end-bulk-in  ( -- )
   debug?  if  ." end-bulk-in" cr  then
   lock
   bulk-in-ed 0=  if  exit  then
   bulk-in-td map-out-cbp
   bulk-in-ed remove-my-bulk
   0 to bulk-in-ed  0 to bulk-in-td
   unlock
;

: bulk-in  ( buf len pipe -- actual usberr )
   debug?  if  ." bulk-in" cr  then
   lock
   dup to bulk-in-pipe
   bulk-in-timeout process-bulk-args        ( )
   alloc-bulk-edtds to my-td to my-ed       ( )

   \ IN TDs
   my-td fill-bulk-io-tds           ( )

   \ Start bulk in transaction
   my-ed insert-my-bulk-in          ( )

   \ Process results
   my-ed done?  if
      0                     ( actual )  \ System error, timeout
   else
      my-td error?  if  
         0                  ( actual )  \ USB error
      else
         my-td dup get-actual           ( td actual )
         over >td-cbp @ rot >td-pcbp @ 2 pick dma-pull    ( actual )
      then
   then

   usb-error                    ( actual usberr )
   my-td map-out-cbp                ( actual usberr ed )
   my-ed dup fixup-bulk-in-data         ( actual usberr ed )
   remove-my-bulk               ( actual usberr )
   unlock
;

: bulk-out  ( buf len pipe -- usberr )
   debug?  if  ." bulk-out" cr  then
   lock
   dup to bulk-out-pipe
   bulk-out-timeout process-bulk-args
   alloc-bulk-edtds to my-td to my-ed

   \ OUT TDs
   my-td fill-bulk-io-tds

   \ Start bulk out transaction
   my-ed insert-my-bulk-out

   \ Process results
   my-ed done? 0=  if  my-td error? drop  then

   usb-error                    ( actual usberr )
   my-td map-out-cbp
   my-ed dup fixup-bulk-out-data
   remove-my-bulk
   unlock
;


: (end-extra)  ( -- )  end-bulk-in  ;


\ file: intr.fth
\ purpose: OHCI USB Controller interrupt pipes transaction processing

hex

d# 500 constant intr-in-timeout

0 value #intr-in            \ Count of outstanding begin-intr-in

0 value intr-in-pipe
0 value intr-in-interval

0 value intr-in-ed     \ Instance variables for begin-intr-in, intr-in?,
0 value intr-in-td     \ restart-intr-in and end-intr-in

: #intr-in++  ( -- )  #intr-in 1+ to #intr-in  ;
: #intr-in--  ( -- )  #intr-in 1- to #intr-in  ;

: disable-periodic  ( -- )  4 hc-cntl-clr  next-frame  ;
: enable-periodic   ( -- )  4 hc-cntl-set  ;

: insert-in-intr  ( ed -- )
   ( ed ) intr-in-interval insert-intr-ed
   enable-periodic
;
: remove-intr  ( ed -- )
   disable-periodic
   ( ed ) remove-intr-ed
   #intr-in  if  enable-periodic  then
;

: intr-in-data@  ( -- n )  intr-in-pipe  target di-in-data@ di-data>ed-data  ;
: intr-in-data!  ( n -- )  ed-data>di-data intr-in-pipe  target di-in-data!  ;
: fixup-intr-in-data   ( ed -- )
   usb-error USB_ERR_STALL and  if
      drop intr-in-pipe h# 80 or unstall-pipe
      ED_TOGGLE_DATA0
   else
      >hced-tdhead @
   then
   intr-in-data!   
;

: process-intr-args  ( buf len pipe timeout -- )  process-bulk-args  ;
: alloc-intr-edtds  ( -- ed td )   alloc-bulk-edtds  ;
: fill-intr-io-tds  ( td -- )      fill-bulk-io-tds  ;
: fill-intr-in-ed  ( ed -- )
   my-dev/pipe my-speed or ED_DIR_IN or ED_SKIP_OFF or ED_FORMAT_G or
   my-maxpayload d# 16 lshift or
   over >hced-control !
   ( ed ) dup >hced-tdhead @        ( ed head )
   intr-in-data@  or                ( ed head' )
   over >hced-tdhead !          ( ed )
   ( ed ) push-edtds                ( )
;
: remove-my-intr  ( ed -- )  dup remove-intr  free-edtds  ;

\ external

: begin-intr-in  ( buf len pipe interval -- )
   debug?  if  ." begin-intr-in" cr  then
   intr-in-ed  if  4drop exit  then     \ Already started
   lock
   #intr-in++

   to intr-in-interval
   dup to intr-in-pipe
   intr-in-timeout process-intr-args
   alloc-intr-edtds  to intr-in-td  to intr-in-ed

   \ IN TD
   intr-in-td fill-intr-io-tds

   \ Start intr transaction
   intr-in-ed dup fill-intr-in-ed
   insert-in-intr
   unlock
;

: intr-in?  ( -- actual usberr )
   intr-in-ed 0=  if  0 USB_ERR_INV_OP exit  then
   lock
   clear-usb-error              ( )
   process-hc-status                ( )
   intr-in-ed dup pull-edtds            ( ed )
   ed-done?  if                 ( )
      intr-in-td error?  if
         0                  ( actual )
      else                  ( )
     intr-in-td dup get-actual      ( td actual )
         over >td-cbp @ rot >td-pcbp @ 2 pick dma-pull    ( actual )
      then
      usb-error                 ( actual usberr )
      intr-in-ed fixup-intr-in-data     ( actual usberr )
   else
      0 usb-error               ( actual usberr )
   then
   unlock
;

\ headers
: restart-intr-in-td  ( td -- )
   begin  ?dup  while
      dup >td-next @  if
         TD_CC_NOTACCESSED TD_DIR_IN or TD_INTR_OFF or TD_ROUND_ON or
         over >hctd-control !
         dup >td-pcbp @ over >hctd-cbp !
         dup >td-next @ >td-phys @ over >hctd-next !
      then
      >td-next @
   repeat
;

\ external
: restart-intr-in  ( -- )
   intr-in-ed 0=  if  exit  then
   lock
   intr-in-ed ed-set-skip

   \ Setup TD again
   intr-in-td restart-intr-in-td

   \ Setup ED again
   intr-in-td >td-phys @ intr-in-data@ or intr-in-ed >hced-tdhead !
   intr-in-ed dup push-edtds
   ed-unset-skip
   unlock
;

: end-intr-in  ( -- )
   debug?  if  ." end-intr-in" cr  then
   intr-in-ed 0=  if  exit  then
   lock
   #intr-in--
   intr-in-td map-out-cbp
   intr-in-ed remove-my-intr
   0 to intr-in-ed  0 to intr-in-td
   unlock
;

\ headers

: (end-extra)  ( -- )  (end-extra) end-intr-in  ;
' (end-extra) to end-extra


\ file: control-hcd.fth
\ purpose: Common USB control pipe API

hex

: setup-buf-arg  ( -- sbuf sphy slen )  setup-buf setup-buf-phys /dr  ;
: cfg-buf-arg    ( -- cbuf cphy )       cfg-buf cfg-buf-phys  ;

: fill-setup-buf  ( len idx value rtype req -- )  
   setup-buf dup /dr  erase      ( len idx value rtype req vpcbp )
   tuck >dr-request c!           ( len idx value rtype vpcbp )
   tuck >dr-rtype c!             ( len idx value vpcbp )
   tuck >dr-value w!             ( len idx vpcbp )
   tuck >dr-index w!             ( len vpcbp )
   >dr-len w!                    ( )
   setup-buf setup-buf-phys /dr dma-push    ( )
;

\ external
: control-get  ( adr len idx value rtype req -- actual usberr )
\   debug? if ." control-get" cr then
   4 pick >r                    ( adr len idx value rtype req )  ( R: len )
   fill-setup-buf               ( adr )  ( R: len )
   setup-buf-arg cfg-buf-arg r@ (control-get)  ( adr actual usberr )  ( R: len )
   dup  if
      r> drop nip nip 0 swap    ( actual usberr )
   else
      -rot r> min tuck cfg-buf -rot move swap   ( actual usberr )
   then
;

: control-set  ( adr len idx value rtype req -- usberr )
\   debug? if ." control-set" cr then
   5 pick ?dup  if  cfg-buf 6 pick move  then   ( adr len idx value rtype req )
   4 pick >r                    ( adr )  ( R: len )
   fill-setup-buf drop              ( )  ( R: len )
   setup-buf-arg cfg-buf-arg r>  (control-set)  ( usberr )
;

: control-set-nostat  ( adr len idx value rtype req -- usberr )
   5 pick ?dup  if  cfg-buf 6 pick move  then   ( adr len idx value rtype req )
   4 pick >r                    ( adr )  ( R: len )
   fill-setup-buf drop              ( )  ( R: len )
   setup-buf-arg cfg-buf-arg r>  (control-set-nostat)   ( usberr )
;

\ headers

: set-address  ( dev -- usberr )
\   debug? if ." set-address" cr then
\   s" usb-delay" ['] evaluate catch  if  2drop  else  ms  then
    50 ms 
   \ To get the right characteristics for dev in control-set, then normal
   \ set-my-dev is nooped.  We set my-dev and my-real-dev here instead.
   ['] set-my-dev behavior swap         ( xt dev )  \ Save set-my-dev
   ['] noop is set-my-dev           ( xt dev )  \ Make it noop
   dup >r                   ( xt dev )  ( R: dev )
   0 set-real-dev               ( xt )  ( R: dev )

   0 0 0 r@ DR_OUT DR_DEVICE or SET_ADDRESS control-set  if
      ." Failed to set device address: " r> u. cr
   else
      r> drop
      d# 10 ms                      \ Let the SET_ADDRESS settle
   then                     ( xt )

   is set-my-dev                \ Restore set-my-dev
   usb-error
;

\ external

: get-desc  ( adr len lang didx dtype rtype -- actual usberr ) 
\    debug? if ." get-desc" cr  then 
   -rot bwjoin swap DR_IN or GET_DESCRIPTOR control-get
;

: get-status  ( adr len intf/endp rtype -- actual usberr )
   0 swap DR_IN or GET_STATUS control-get
;

\ Must be called after set-config for any device with bulk-in or
\ bulk-out pipes.  Pass in 0 if one of the pipes is nonexistent.
: reset-bulk-toggles  ( bulk-in-pipe bulk-out-pipe -- )
   ?dup   if  0 swap  target  di-out-data!  then
   ?dup   if  0 swap  target  di-in-data!   then
;

: set-config  ( cfg -- usberr )
   >r 0 0 0 r> DR_DEVICE DR_OUT or SET_CONFIGURATION control-set 
;

: set-interface  ( intf alt -- usberr )
   0 0 2swap DR_INTERFACE DR_OUT or SET_INTERFACE control-set
;

: clear-feature  ( intf/endp feature rtype -- usberr )
   >r 0 0 2swap r> DR_OUT or CLEAR_FEATURE control-set
;
: set-feature  ( intf/endp feature rtype -- usberr )
   >r 0 0 2swap r> DR_OUT or SET_FEATURE control-set
;

: set-pipe-maxpayload  ( size pipe -- )
   target di-maxpayload!
;

\ headers

: (unstall-pipe)  ( pipe -- )  0 DR_ENDPOINT clear-feature drop  ;
' (unstall-pipe) to unstall-pipe

\ external
: get-cfg-desc  ( adr idx -- actual )
\   debug? if ." get-cfg-desc" cr then
   swap >r                  ( idx )  ( R: adr )
   r@ 9 0 3 pick CONFIGURATION DR_DEVICE get-desc nip 0=  if
      r> dup 2 + w@ rot 0 swap CONFIGURATION DR_DEVICE get-desc drop ( actual )
   else
      r> 2drop 0                ( actual )
   then
;

: get-dev-desc  ( adr len -- actual ) 
\   debug? if ." get-dev-desc" cr then
   0 0 DEVICE DR_DEVICE get-desc drop       ( actual )
;
: (get-str-desc)  ( adr len lang idx -- actual )
   STRING DR_DEVICE get-desc drop       ( actual )
;
: get-str-desc  ( adr lang idx -- actual )  
   3dup 2 -rot (get-str-desc) 0=  if  3drop 0 exit  then    \ Read the length
   >r 2dup r>                   ( adr lang adr lang idx )
   2 pick c@ -rot  (get-str-desc) 0=  if  2drop 0 exit  then    \ Then read the whole string
                        ( adr lang )
   encoded$>ascii$              ( )
;

\ Returns true the first time it is called for a given target device
\ after a reset of the USB subsystem.  Used for reinitializing hardware.

: reset?  ( -- flag )  target di-reset?  ;

\ file: device.fth
\ purpose: USB device node

hex
\ headers

defer make-dev-property-hook  ( speed dev port -- )
' 3drop to make-dev-property-hook

\ Buffers for descriptor manipulation
0 value cfg-desc-buf        \ Configuration Descriptor
0 value dev-desc-buf        \ Device Descriptor
0 value d$-desc-buf         \ Device String Descriptor
0 value v$-desc-buf         \ Vendor String Descriptor
0 value s$-desc-buf         \ Serial Number String Descriptor

0 value /cfg-desc-buf       \ Length of data in cfg-desc-buf
0 value /dev-desc-buf       \ Length of data in dev-desc-buf
0 value /d$-desc-buf        \ Length of data in d$-desc-buf
0 value /v$-desc-buf        \ Length of data in v$-desc-buf
0 value /s$-desc-buf        \ Length of data in s$-desc-buf

: alloc-pkt-buf  ( -- )
   cfg-desc-buf 0=  if
      /cfg alloc-mem dup to cfg-desc-buf /cfg erase
      /cfg alloc-mem dup to dev-desc-buf /cfg erase
      /str alloc-mem dup to d$-desc-buf /str erase
      /str alloc-mem dup to v$-desc-buf /str erase
      /str alloc-mem dup to s$-desc-buf /str erase
   then
;
: free-pkt-buf  ( -- ) 
   debug? if ." free-pkt-buf" cr then 
   cfg-desc-buf ?dup  if  /cfg free-mem  0 to cfg-desc-buf  then
   dev-desc-buf ?dup  if  /cfg free-mem  0 to dev-desc-buf  then
   d$-desc-buf  ?dup  if  /str free-mem  0 to d$-desc-buf   then
   v$-desc-buf  ?dup  if  /str free-mem  0 to v$-desc-buf   then
   s$-desc-buf  ?dup  if  /str free-mem  0 to s$-desc-buf   then
;

: dev-desc@  ( index -- byte )  dev-desc-buf + c@  ;
: asso-class?  ( -- asso? )
   4 dev-desc@ h# ef =  5 dev-desc@ 2 =  and  6 dev-desc@ 1 =  and
;
: get-class  ( -- class subclass protocol )
   asso-class?  if
      \ Class is in interface association descriptor
      true to class-in-dev?
      cfg-desc-buf INTERFACE_ASSO  find-desc ( intf-adr )
      >r  r@ 4 + c@  r@ 5 + c@   r> 6 + c@  ( class subclass protocol )
   else
   4 dev-desc@ ?dup  if                 ( class )
      \ Class is in device descriptor
      true to class-in-dev?         ( class )
      5 dev-desc@  6 dev-desc@          ( class subclass protocol )
   else
      \ Class is in interface descriptor
      false to class-in-dev?
      cfg-desc-buf my-address find-intf-desc    ( intf-adr )
      >r  r@ 5 + c@  r@ 6 + c@   r> 7 + c@  ( class subclass protocol )
   then  then
;

: make-class-properties  ( -- )
   debug? if ." make-class-properties" .s cr then
   get-class  ( class subclass protocol )
   s" protocol: "  type u. cr 
   s" subclass: " type u. cr 
   s" class: "    type u. cr
;

: make-name-property  ( -- )
   debug? if ." make-name-property" .s cr then
    get-class
   swap rot                 ( protocol subclass class )
   case
      1  of  2drop s" audio"  endof      ( name$ )
      2  of  2drop s" network"  endof        ( name$ )
      3  of  case
             1  of  case
                1  of  s" keyboard"  endof
                2  of  s" mouse"  endof
                4  of  s" joystick"  endof
                5  of  s" gamepad"  endof
                39 of  s" hatswitch"  endof
                ( default )  s" device" rot
            endcase        endof
           ( default ) nip s" hid" rot
        endcase    endof
      7  of  2drop s" printer"  endof        ( name$ )
      8  of  case
             1  of  drop s" flash"  endof
             2  of  drop s" cdrom"  endof
             3  of  drop s" tape"  endof
             4  of  drop s" floppy"  endof
             5  of  drop s" scsi"  endof     \ removable
             6  of  drop s" scsi"  endof
             ( default ) nip s" storage" rot
        endcase    endof
      9  of  2drop s" hub"  endof        ( name$ )
      ( default )  nip nip s" device" rot    ( name$ )
   endcase
debug? if 2dup type cr then
   drop 1- cur-dev di-name!
;

: get-vid  ( adr -- vendor product rev )
   dev-desc-buf 8 + w@
   dev-desc-buf d# 10 + w@
   dev-desc-buf c + w@
;

: make-vendor-properties  ( -- )
   debug? if ." make-vendor-properties" .s cr then
   get-vid          ( vendor product rev )
   s" release: "   type u. cr  
   s" device-id: " type  . cr 
   s" vendor-id: " type  . cr
;

\ A little tool so "make-compatible-property" reads better
0 value sadr
0 value slen
: +$  ( add$ -- )
   sadr slen 2swap encode-string encode+  to slen  to sadr
;

: make-string-properties  ( -- )
   debug? if ." make-string-properties:" .s cr then
   v$-desc-buf /v$-desc-buf s" vendor$" str-property
   d$-desc-buf /d$-desc-buf s" device$" str-property
   s$-desc-buf /s$-desc-buf s" serial$" str-property
;

: make-misc-properties  ( -- )
   debug? if ." make-misc-property[" .s cr then
   cfg-desc-buf 5 + c@ s" configuration# " type u. cr 
;

: register-pipe  ( pipe size -- )
debug? if ." register-pipe[" .s cr then
  2drop
;

: make-ctrl-pipe-property  ( pipe size interval -- )
   drop                 ( pipe size )
   over h# f and rot h# 80 and  if  ( size pipe )
      s" control-in-pipe"  type u. cr  \ int-property
      s" control-in-size"
   else
      s" control-out-pipe"  type u. cr  \ int-property
      s" control-out-size"
   then   type u. cr  \ int-property
;
: make-iso-pipe-property  ( pipe size interval -- )
   drop                 ( pipe size )
   over h# 0f and rot h# 80 and  if ( size pipe )
      s" iso-in-pipe"  type u. cr \ int-property
      s" iso-in-size"
   else
      s" iso-out-pipe"  type u. cr  \ int-property
      s" iso-out-size"
   then  type u. cr   \ int-property
;
: make-bulk-pipe-property  ( pipe size interval -- )
   drop                 ( pipe size )
   over h# f and rot h# 80 and  if  ( size pipe )
      s" bulk-in-pipe"   type u. cr \ int-property
      s" bulk-in-size"
   else
      s" bulk-out-pipe" type u. cr  \ int-property
      s" bulk-out-size" 
   then   type u. cr  \ int-property
;
: make-intr-pipe-property  ( pipe size interval -- )
   rot dup h# f and swap h# 80 and  if  ( size interval pipe )
      s" intr-in-pipe"  type u. cr  \     int-property
      s" intr-in-interval"  type u. cr  \ int-property
      s" intr-in-size"
   else
      s" intr-out-pipe"     type u. cr  \ int-property
      s" intr-out-interval"  type u. cr  \ int-property
      s" intr-out-size"
   then   type u. cr  \ int-property
;
: make-pipe-properties  ( adr -- )
   debug? if ." make-pipe-properties" cr then
   dup c@ over + swap 4 + c@        ( adr' #endpoints )
   swap ENDPOINT find-desc swap 0  ?do  ( adr' )
      dup 2 + c@            ( adr pipe )
      over 4 + w@            ( adr pipe size )
      2dup register-pipe        ( adr pipe size )
      2 pick 6 + c@         ( adr pipe size interval )
      3 pick 3 + c@ 3 and  case     ( adr pipe size interval type )
         0  of  make-ctrl-pipe-property  endof
         1  of  make-iso-pipe-property   endof
         2  of  make-bulk-pipe-property  endof
         3  of  make-intr-pipe-property  endof
      endcase
      dup c@ +              ( adr' )
   loop  drop
;

: make-descriptor-properties  ( -- )
   debug? if ." make-descriptor-properties" cr then
   make-class-properties        \ Must make class properties first
   make-name-property
   make-vendor-properties
   make-string-properties 
\ my-address = intf my-space=port
   cfg-desc-buf my-address find-intf-desc make-pipe-properties
   make-misc-properties

;

: make-common-properties  ( dev -- )
   debug? if ." make-common-properties:" .s cr then
    drop
;

\ Sets the di-maxpayload fields in the dev-info endpoint descriptor array
: reregister-pipes  ( dev intf -- )
   cfg-desc-buf swap find-intf-desc ( dev adr )
   dup c@  over +  swap 4 + c@      ( dev adr' #endpoints )
   swap  ENDPOINT find-desc     ( dev #endpoints adr' )
   swap 0  ?do              ( dev adr' )
      over di-is-reset                  ( dev adr )
      dup 4 + w@         ( dev adr size )
      over 2 + c@  h# f and     ( dev adr size pipe )
      3 pick di-maxpayload!     ( dev adr )
      dup c@ +              ( dev adr' )
   loop  2drop              ( )
;

: be-l!  ( n adr -- )
   >r lbsplit r@ c!  r@ 1+ c!  r@ 2+ c!  r> 3 + c!
;

: probe-hub-node  ( phandle -- )
   >r                                       ( r: phandle )
   s" probe-hub" r@ find-method  if          ( xt r: phandle )
      r@ push-package                       ( xt r: phandle )
      s" " new-instance                      ( xt r: phandle )
      set-default-unit                      ( xt r: phandle )
      execute                               ( r: phandle )
      destroy-instance                      ( r: phandle )
      pop-package                           ( r: phandle )
   then                                     ( r: phandle )
   r> drop
;
: reuse-node  ( dev intf port phandle -- )
   debug? if ." reuse-node" cr then
   >r drop            ( dev intf r: phandle )

   2dup reregister-pipes      ( dev intf r: phandle )
   drop                           ( dev      r: phandle )

   \ Change the assigned-address property without leaking memory
   s" assigned-address" r@ get-package-property  if  ( dev r: phandle )
      drop                                  ( r: phandle )
   else                                     ( dev adr len r: phandle )
      drop be-l!                            ( r: phandle )
   then                                     ( r: phandle )

   r> probe-hub-node
;
: id-match?  ( dev intf port phandle -- dev intf port phandle flag? )
   s" vendor-id" 2 pick get-package-property  if  false exit  then
   decode-int nip nip   >r     ( dev intf port phandle r: vid )
   s" device-id" 2 pick get-package-property  if  r> drop  false exit  then
   decode-int nip nip   >r     ( dev intf port phandle r: vid did )
   s" release" 2 pick get-package-property  if  r> r> 2drop  false exit  then
   decode-int nip nip   >r     ( dev intf port phandle r: vid did rev )
   get-vid                     ( dev intf port phandle  vid1 did1 rev1 r: vid did rev )
   r> = -rot  r> = -rot  r> =  and and
;

: reuse-old-node?  ( dev intf port -- reused? ) \ check for old node
   debug? if ." reuse-old-node" cr then 3drop false exit
;

: disable-old-nodes  ( port -- ) 
   debug? if ." disable-old-node" cr then
   drop                                          ( )
;

: find-port-node  ( port -- true | phandle false)
   my-self ihandle>phandle child                 ( port phandle )
   begin  ?dup  while                            ( port phandle )
      s" reg" 2 pick get-package-property 0=  if  ( port phandle adr len )
         decode-int  nip nip                     ( port phandle port1 )
         2 pick  =  if                           ( port phandle )
            \ Check if the node has been disabled
            s" assigned-address"                  ( port phandle propname$ )
            2 pick  get-package-property 0=  if  ( port phandle adr len )
               decode-int  nip nip               ( port phandle assigned-address )
               -1 <>  if                         ( port phandle )
                  nip false exit
               then
            then                                 ( port phandle )
         then                                    ( port phandle )
      then                                       ( port phandle )
      peer                                       ( port phandle' )
   repeat                                        ( port )
   drop                                          ( )
   true
;

: .phandle-property  ( phandle prop-name$ -- )
   rot get-package-property 0=  if    ( adr len )
      decode-string type  2drop
   then
;
: .usb-device  ( port -- )
   find-port-node  if
      ." Can't find device node for USB port!" cr abort
   else                                      ( phandle )
      dup s" device_type" .phandle-property   ( phandle )
      ." ,"
      dup s" vendor$" .phandle-property       ( phandle )
      ." ,"
      dup s" device$" .phandle-property       ( phandle )
      drop                                   ( )
   then
;

: (make-device-node)  ( dev port intf -- )
   debug? if ." (make-device-node):" .s cr then
   swap                              ( dev intf port )
   3dup  reuse-old-node?  if         ( dev intf port )
      3drop exit
   then
\ here we update di structure
   2drop     ( dev )
   make-common-properties          \ Make non-descriptor based properties
   make-descriptor-properties      \ Make descriptor based properties
;

\ Get all the descriptors we need in making properties now because target is
\ questionable in the child's context.  The descriptor buffers are not instance
\ data, so they can be accessed by code that is defined in the root hub node
\ but executing in a subordinate hub node context or a child node context.

h# 409 constant language            \ Unicode id
\ Executed in root hub node context
: get-string ( lang idx adr -- actual )
   over 0=  if  3drop  0 exit  then     \ No string index
   -rot get-str-desc
;

\ Executed in root hub node context
: get-str-descriptors  ( -- )
   debug? if ." get-str-descriptors" cr then
   language                 ( lang )
   dup d# 14 dev-desc@ v$-desc-buf get-string to /v$-desc-buf
   dup d# 15 dev-desc@ d$-desc-buf get-string to /d$-desc-buf
       d# 16 dev-desc@ s$-desc-buf get-string to /s$-desc-buf
;

\ Executed in root hub node context
: refresh-desc-bufs  ( dev -- )    
debug? if ." refresh-desc-bufs" cr then 
   set-target
   dev-desc-buf d# 18 get-dev-desc to /dev-desc-buf     \ Refresh dev-desc-buf
   cfg-desc-buf     0 get-cfg-desc to /cfg-desc-buf     \ Refresh cfg-desc-buf
   get-str-descriptors
;

: get-initial-dev-desc  ( dev -- ) 
   debug? if ." get-initial-dev-desc" cr then
   dev-desc-buf d# 18 erase                     ( dev )

   \ Until we know the size of the control endpoint, we must be
   \ conservative about the transfer size.
   dev-desc-buf /pipe0 get-dev-desc  if     ( dev )
      7 dev-desc@                           ( dev maxtransfer )
      tuck  0 rot di-maxpayload!            ( maxtransfer )
      d# 18 >=  if                              ( )
         dev-desc-buf d# 18 get-dev-desc drop   ( )
." get-initial-dev-desc.." dev-desc-buf dup . d# 18 dump cr
      then                                      ( )
   else                     ( dev )
      drop                  ( )
      ." >>failed " cr
   then                     ( )
;

\ Executed in root hub node context
: get-initial-descriptors  ( dev -- )
   get-initial-dev-desc                         ( )
   cfg-desc-buf 0 get-cfg-desc to /cfg-desc-buf ( )
;

\ Executed in hub node context (root hub or subordinate hub) - creates new child nodes via (make-device-node)
: make-device-node  ( port dev -- )  
    debug? if ." make-device-node:" .s cr then
   dup get-initial-descriptors  ( port dev )
   /cfg-desc-buf 0=  if  2drop ." -exiting" exit  then      ( port dev )

   asso-class?  if  1  else  cfg-desc-buf 4 + c@  then  ( port dev #intf )
   0  ?do                               ( port dev )
      dup refresh-desc-bufs ( port dev )
      2dup swap i (make-device-node)            ( port dev )
   loop  2drop                      ( )
debug? if ." ...make-device-node" cr then
;

\ See hcd/ehci/probehub.fth for information about hub20-dev and hub20-port

: get-hub20-dev  ( -- hub-dev )
   s" hub20-dev" get-inherited-property 0=  if   ( value$ )
      decode-int nip nip                        ( hub-dev )
   else                                         ( )
      1                                         ( hub-dev )
   then                                         ( hub-dev )
;

: get-hub20-port  ( port -- port' )
   s" hub20-port" get-inherited-property 0=  if  ( port value$ )
      rot drop                      ( value$ )
      decode-int nip nip                        ( port' )
   then                                         ( port )
;

\ Executed in the root hub node context
: setup-new-node  ( port speed hub-port hub-dev -- false | port dev xt true )
." setup-new-node" cr
  \ Allocate device number
   next-device#  if 4drop false exit then  ( port speed hub-port hub-dev dev )

   tuck di-hub!             ( port speed hub-port dev )
   tuck di-port!            ( port speed dev )
   tuck di-speed!           ( port dev )

   0 set-target             ( port dev )    \ Address it as device 0

   \ Some devices (e.g. Lexar USB-to-SD and at least one USB FLASH drive) fail
   \ on set-address unless you first read the device descriptor from address 0.
   \ On other devices, this will fail, but it won't cause problems, and the
   \ descriptor will be re-read later by make-device-node
   dup get-initial-dev-desc             ( port dev )

   over reset-port         ( port dev )    \ second reset 

   dup set-address  if          ( port dev )    \ Assign it usb addr dev
      ." Retrying with a delay" cr
      over reset-port  d# 5000 ms
      dup set-address  if       ( port dev )    \ Assign it usb addr dev
         \ Recycle device number?
         2drop false exit       ( -- false )
      then              ( port dev )
   then                 ( port dev )

   dup set-target           ( port dev )    \ Address it as device dev
   ['] make-device-node  true       ( port dev xt )
debug? if ." .....setup-new-node" cr .s then
;

\ Begins execution in a (root or subordinate) hub node context, creates an instance record
\ for the subordinate hub node "phandle", switches to that instance context, executes
\ "reprobe-hub" in that context, destroys the instance, and returns to the original context.
: reprobe-hub-node  ( phandle -- ) ." reprobe-hub-port" cr 
   >r                                       ( r: phandle )
   s" reprobe-hub" r@ find-method  if        ( xt r: phandle )
      r@ push-package                       ( xt r: phandle )
      s" " new-instance                      ( xt r: phandle )
      set-default-unit                      ( xt r: phandle )
      execute                               ( r: phandle )
      destroy-instance                      ( r: phandle )
      pop-package                           ( r: phandle )
   then                                     ( r: phandle )
   r> drop
;

\ Returns true if there is a child hub node associated with port
: port-is-hub?  ( port -- false | phandle true ) 
debug? if ." port-is-hub? "  .s cr then
   drop false                                         ( false )
;

: probe-setup  ( -- )
   \ Set active-package so device nodes can be added and removed
   my-self ihandle>phandle push-package

   alloc-pkt-buf
;
: probe-teardown  ( -- )
   free-pkt-buf
   pop-package
;


\ file probe.fth
\ purpose: OHCI USB Controller probe
\ See license at end of file

hex
\ headers
\ device enumeration
\ 1.  port stabilization after power up
\     - no port connect changes for at least 100 ms
\ 2.  reset port - timeout  of 5 sec. 3 retries with 500 ms delay
\ 3.  device descriptor request #1
\ 4.  reset port again
\ 5.  set address 
\ 6.  device descriptor request
\ 7.  configuration descriptor request
\ 8.  other descriptor request....
\ 9.  add to device list

: ohci-pci ( -- )   \ make device pci bus master
    0 13 0 pci-dev 4 over pci-w@ 4 or 4 rot pci-w!
;

: probe-root-hub-port  ( port -- )  
   debug? if ." probe-root-hub-port" cr then
   dup disable-old-nodes                ( port )
   dup hc-rh-psta@ 1 and 0=  if  drop ." ---exiting" cr exit  then    ( port )

   \ Reset the port to determine the speed
   dup reset-port                   ( port )
   dup hc-rh-psta@ 200 and if speed-low else  speed-full then   ( port speed )

   \ hub-port and hub-speed are irrelevant for OHCI (USB 1.1)
   0 0                      ( port speed hub-port hub-dev )

   \ Execute setup-new-node in root context and make-device-node in hub node context
   setup-new-node  if  execute  then        ( port dev xt )
;

false value ports-powered?

\ number of root hub ports 
: #ports  ( -- n )  hc-rh-desA@ h# ff and  ;


: power-ports  ( -- )
   hc-rh-desA@  dup h# 200  and  0=  if
      \ ports are power switched
      hc-rh-stat@ h# 10000 or hc-rh-stat!  \ power all ports
      hc-rh-desB@ d# 17 rshift over h# ff and 0  ?do
         dup 1 i lshift and  if
            i hc-rh-psta@  h# 100 or i hc-rh-psta!  \ power port
         then
      loop  drop
   then  drop
   potpgt 2* ms         \ Wait until powergood

   \ Setup PowerOnToPowerGoodTime and OverCurrentProtectionMode
   hc-rh-desA@  h# 00ffffff and  potpgt d# 24 lshift or
   h# 800 or   hc-rh-desA!  \ per-port over-current status

   true to ports-powered?
;


: probe-root-hub  ( -- )
   debug? if ." probe-root-hub [" .s cr then
   \ Power on ports
   ports-powered? 0=  if ." -powering ports" cr power-ports  then
   0 hc-rh-psta@ h# 100 and  if
        500 ms          \ delay for port turn-up
   else power-ports then

   alloc-pkt-buf
   #ports 0  ?do
      i hc-rh-psta@ 30000 and  if
i probe-root-hub-port 
\        i rm-obsolete-children         \ Remove obsolete device nodes
\         i ['] probe-root-hub-port catch  if
\                drop ." Failed to probe root port " i u. cr
\         then
         30000 i hc-rh-psta!           \ Clear change bits
      else
         i port-is-hub?  if     ( phandle )     \ Already-connected hub?
            reprobe-hub-node                    \ Check for changes on its ports
         then
      then     
   loop
   free-pkt-buf
;

: usb-open  ( -- flag )
  open-count 0=  if
    first-open?  if
        false to first-open?
        ohci-pci        \ device is pci master
        reset-usb
        init-struct     \ device descriptors, ed and tds
        init-usb
      then
      alloc-dma-buf
   then
   probe-root-hub       \ find devices on root hub
   open-count 1+ to open-count
   true
;

: usb-close  ( -- )   
   open-count 1- to open-count
   end-extra
   open-count 0=  if
        free-dma-buf
        hcca-unaligned ?dup if /hcca dma-free 0 to hcca then
        di ?dup if /di dma-free 0 to di then
    then
;

: .hc-regs ( -- )
    cr ." HcRegisters:" cr
    h# 5c 0 do
        i dup usb@ swap
        hex. hex. cr
    4 +loop
;
 


only forth definitions

\ LICENSE_BEGIN
\ Copyright (c) 2006 FirmWorks
\ 
\ Permission is hereby granted, free of charge, to any person obtaining
\ a copy of this software and associated documentation files (the
\ "Software"), to deal in the Software without restriction, including
\ without limitation the rights to use, copy, modify, merge, publish,
\ distribute, sublicense, and/or sell copies of the Software, and to
\ permit persons to whom the Software is furnished to do so, subject to
\ the following conditions:
\ 
\ The above copyright notice and this permission notice shall be
\ included in all copies or substantial portions of the Software.
\ 
\ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
\ EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
\ MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
\ NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
\ LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
\ OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
\ WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
\
\ LICENSE_END
