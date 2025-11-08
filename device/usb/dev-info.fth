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


