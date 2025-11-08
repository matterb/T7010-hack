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


