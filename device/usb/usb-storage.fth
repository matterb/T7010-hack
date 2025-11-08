\   usb_storage.fth
hex

only forth 
[defined] system [if]  
   also system definitions
  [defined] usbdefs [if] 
      also usbdefs
  [else] vocabulary usbdefs
  [then]
  also usbdefs definitions
[then]

[defined] tr-flag [if] trace-on [endif]

h#  800 constant low-speed-max
h# 2000 constant full-speed-max
h# 4000 constant high-speed-max

\ fload ${BP}/dev/usb2/device/common.fth 
\ USB device driver common routines
\ See license at end of file

0 value device
0 value configuration

\ move iso-x to hcd section
0 value iso-in-pipe
0 value iso-out-pipe
0 value iso-in-interval
0 value iso-out-interval
0 value /iso-in-pipe
0 value /iso-out-pipe

: get-int-property type 0 ;
\ This needs to be called every time that the device could have changed
: set-device  ( -- )  \ get the msd usb dev hard coded to 1
  1 to device  ;
: set-device?  ( -- error?  )  set-device  device -1 =  ;

: init-usb-dev  ( -- )
   set-device device
   dup di-act-config@    to configuration
   dup di-bulk-in@   to bulk-in-pipe
   dup di-bulk-out@  to bulk-out-pipe
   dup di-bulk-in-size@  to /bulk-in-pipe
   dup di-bulk-out-size@ to /bulk-out-pipe
   dup di-iso-in@    to iso-in-pipe
   dup di-iso-out@   to iso-out-pipe
   dup di-iso-in-size@ to /iso-in-pipe
   dup di-iso-out-size@ to /iso-out-pipe
   dup di-iso-in-interval@  to iso-in-interval
   dup di-iso-out-interval@ to iso-out-interval
   dup di-intr-in@  to intr-in-pipe
   dup di-intr-out@ to intr-out-pipe
   dup di-intr-in-size@ to /intr-in-pipe
   dup di-intr-out@ to /intr-out-pipe
   dup di-intr-ininterval@ to intr-in-interval
   di-intr-out-interval@ to intr-out-interval
;


\ fload ${BP}/dev/usb2/device/storage/scsi.fth  \ High level SCSI routines
\ USB Mass Storage Device Driver

0 value max-lun
0 value lun

USB_ERR_STALL constant bus-reset

defer execute-command-hook  ' noop to execute-command-hook
defer init-execute-command  ' noop to init-execute-command


\ Class specific >dr-request constants
h# ff constant DEV_RESET
h# fe constant GET_MAX_LUN

\ Command Block Wrapper
begin-structure /cbw
   field:  >cbw-sig
   field:  >cbw-tag
   field:  >cbw-dlen
   cfield: >cbw-flag
   cfield: >cbw-lun
   cfield: >cbw-cblen
h# 10 +field >cbw-cb
end-structure \   /cbw

\ >cbw-flag definitions
80 constant cbw-flag-in
00 constant cbw-flag-out

\ Command Status Wrapper
begin-structure  /csw
   field:  >csw-sig
   field:  >csw-tag
   field:  >csw-dlen
   cfield: >csw-stat
end-structure \ csw

h# 43425355 constant cbw-signature  \ little-endian (USBC)
h# 53425355 constant csw-signature  \ little-endian (USBS)

0 value cbw-tag
0 value cbw
0 value csw

: init-cbw  ( -- )
   cbw /cbw erase
   cbw-signature cbw >cbw-sig le-l!
   cbw-tag 1+ to cbw-tag
   cbw-tag cbw >cbw-tag le-l!
;
: alloc-bulk  ( -- )
   cbw 0=  if  /cbw dma-alloc to cbw  then
   csw 0=  if  /csw dma-alloc to csw  then
;
: free-bulk  ( -- )
   cbw  if  cbw /cbw dma-free  0 to cbw  then
   csw  if  csw /csw dma-free  0 to csw  then
;

1 buffer: max-lun-buf

: get-max-lun  ( -- )
   max-lun-buf 1 my-address ( interface ) 0 DR_IN DR_CLASS DR_INTERFACE or or
   GET_MAX_LUN control-get  if          ( actual usberr )
      drop 0
   else
      ( actual )  if  max-lun-buf c@  else  0  then
   then
   to max-lun
;

: init-msd  ( -- )
   init-usb-dev
   init-execute-command
   alloc-bulk
   device set-target
   get-max-lun
   free-bulk
;

: transport-reset  ( -- )
   0 0 my-address ( interface ) 0 DR_OUT DR_CLASS DR_INTERFACE or or
   DEV_RESET control-set-nostat  drop
   \ XXX Wait until devices does not NAK anymore
   bulk-in-pipe  h# 80 or unstall-pipe
   bulk-out-pipe          unstall-pipe
;

: wrap-cbw  ( data-len dir cmd-adr,len -- cbw-adr,len )
   init-cbw             ( data-len dir cmd-adr,len )
   cbw >r               ( data-len dir cmd-adr,len )  ( R: cbw )
   dup r@ >cbw-cblen c!         ( data-len dir cmd-adr,len )  ( R: cbw )
   r@ >cbw-cb swap move         ( data-len dir )  ( R: cbw )
   if  cbw-flag-in  else  cbw-flag-out  then    ( data-len cbw-flag )  ( R: cbw )
   r@ >cbw-flag c!          ( data-len )  ( R: cbw )
   r@ >cbw-dlen le-l!           ( )  ( R: cbw )
   lun r@ >cbw-lun c!           ( )  ( R: cbw )
   r> /cbw              ( cbw-adr,len )
;

: (get-csw)  ( -- len usberr )  csw /csw erase  csw /csw bulk-in-pipe bulk-in  ;
: get-csw  ( -- len usberr )
   (get-csw) dup  if  2drop (get-csw)  then
;

\ This used to be 15 seconds but I shortened it so timeouts can be
\ retried without having to wait too long.
d# 2000 constant bulk-timeout

: (execute-command)  ( data-adr,len dir cbw-adr,len -- actual-len cswStatus  )
   debug?  if
      2dup s" dump" evaluate cr
   then

   bulk-out-pipe bulk-out       ( data-adr,len dir usberr )
   USB_ERR_CRC invert and  if       ( data-adr,len dir )
      transport-reset  3drop 0 2 exit   ( actual=0 status=retry )
   then                                 ( data-adr,len dir )

   over  if                             ( data-adr,len dir )
      if                ( data-adr,len )
         bulk-in-pipe bulk-in           ( actual usberror )
      else              ( data-adr,len )
         tuck bulk-out-pipe bulk-out    ( len usberror )
         dup  if  nip 0 swap  then      ( len' usberror )
      then              ( usberror )
   else                 ( data-adr,len dir )
      drop nip  0           ( len usberror )
   then                 ( actual usberror )

   get-csw              ( actual usberror csw-len csw-usberror )

   rot  drop                ( actual csw-len csw-usberror )

   ?dup  if                             ( actual csw-len csw-usberror )
      nip                               ( actual csw-usberror )
      dup h# 10000000 =  if             ( actual csw-usberror )
0 [if]
\ This is for testing the problem described in OLPC trac #9423
\ The problem has been worked around so users no longer see it,
\ apart from a short delay when it happens, but for testing you
\ can enable this code to report the problem and count occurrences.
cr 7 emit ." TIMEOUT " 7 emit
s" h# 72 nvram@ 1+ dup .d h# 72 nvram!" evaluate
cr
[then]
         2drop 0 2      ( 0 2 )  \ Convert timeout error to a retry
      then              ( actual usberror )
      exit
   then                 ( actual csw-len csw-usberror )
   drop                 ( actual )

   debug?  if
      csw /csw s" dump" evaluate cr
   then

   csw >csw-stat c@             ( actual cswStatus )
   dup 2 =  if  transport-reset  then   ( actual cswStatus )
   \ Values are:
   \  0: No error - command is finished
   \  1: Error - do get-sense and possibly retry
   \  2: Phase error - retry after transport-reset
   \  else: Invalid status code - abort command
;

\ external

: execute-command  ( data-adr,len dir cmd-adr,len -- actual cswStatus )
   execute-command-hook                 ( data$ dir cmd$ )
   over c@ h# 1b =                      ( data$ dir cmd$ flag )
   2 pick 4 + c@  1 =  and  >r          ( data$ dir cmd$ r: Start-command? )
   2over 2swap wrap-cbw             ( data-adr,len dir cbw-adr,len )
   (execute-command)                            ( actual cswStatus )
   r>  if  drop 0  then  \ Fake ok if it's a start commmand
;

: set-address  ( lun -- )
   0 max max-lun min  to lun
   reset?  if
      configuration set-config  if
         ." USB storage scsi layer: Failed to set configuration" cr
      then
      bulk-in-pipe bulk-out-pipe reset-bulk-toggles
   then
;
: set-timeout  ( n -- )  bulk-timeout max set-bulk-in-timeout  ;

: reopen-hardware   ( -- ok? )
   set-device?  if  false exit  then  \ The device number may have changed if we recycled the node
   device set-target
   true
;
: open-hardware     ( -- ok? )  alloc-bulk  reopen-hardware  ;
: reclose-hardware  ( -- )  ;
: close-hardware    ( -- )      free-bulk  ;

: reset  ( -- )  transport-reset  ;

: selftest  ( -- 0 | error-code )  0  ;


\ fload ${BP}/dev/usb2/device/storage/atapi.fth \ ATAPI interface support

d# 12 dup constant pkt-len
buffer: pkt-buf

\ ATAPI devices only accept 12 byte long commands
\ Fixup the SCSI commands before sending them out

: translate-atapi-command  ( cmd-adr,len -- pkt-buf,len )
   pkt-buf dup >r pkt-len erase         \ Zero pkt-buf
   r@ swap move                 \ Copy cmd$ to pkt-buf
   r@ c@ dup h# 15 = swap h# 1a = or  if    \ Mode send or mode select command
      r@ c@ h# 40 or r@ c!          \ Fix it
      r@ 4 + c@ r@ 8 + c!
      0 r@ 4 + c!
   else
      r@ c@ dup 8 = swap h# a = or  if      \ Read or write command
         r@ c@ h# 20 or r@ c!           \ Fix it
         r@ 1+ c@ dup h# e0 and r@ 1+ c!
         r@ 4 + c@ r@ 8 + c!
         r@ 3 + c@ r@ 5 + c!
         r@ 2 + c@ r@ 4 + c!
         h# 1f and r@ 3 + c!
         0 r@ 2 + c!
      then
   then
   r> pkt-len
;

: init-atapi-execute-command  ( -- )
   s" is-atapi" get-my-property 0=  if
      2drop
      ['] translate-atapi-command to execute-command-hook
   then
;
' init-atapi-execute-command  to init-execute-command

\ fload ${BP}/dev/usb2/device/storage/hacom.fth \ Basic SCSI routines
\ Common code for SCSI host adapter drivers.

\ The following code is intended to be independent of the details of the
\ SCSI hardware implementation.  It is loaded after the hardware-dependent
\ file that defines execute-command, set-address, open-hardware, etc.

\ headers

-1 value inq-buf                  \ Address of inquiry data buffer
-1 value sense-buf                \ holds extended error information


0 value #retries  ( -- n )        \ number of times to retry SCSI transaction

\ Classifies the sense condition as either okay (0), retryable (1),
\ or non-retryable (-1)
: classify-sense  ( -- 0 | 1 | -1 )
   debug?  if
      base @ >r hex
      ." Sense:  " sense-buf 11 bounds  do  i c@ 3 u.r  loop  ."  ..." cr
      r> base !
   then
   sense-buf

   \ Make sure we understand the error class code
   dup c@  h# 7f and h# 70 <>  if  drop -1 exit  then

   \ Check for filemark, end-of-media, or illegal block length
   dup 2+ c@  h# e0  and  if  drop -1 exit  then

   2 + c@  h# f and   ( sense-key )

   \ no_sense(0) and recoverable(1) are okay
   dup 2 <  if  drop 0 exit  then   ( sense-key )

   \ not-ready(2) may be retryable
   dup 2 =  if
      \ check (tapes, especially) for MEDIA NOT PRESENT: if the
      \ media's not there the command is not retryable
      drop                ( )
      sense-buf h# c + c@  h# 3a =  sense-buf h# d + c@ 0=  and  ( not-present? )
      if  -1  else  1  then  exit
   then

   \ Media-error(3) is not retryable
   dup 3 =  if  drop -1 exit  then

   \ Attention(6), and target aborted (b) are retryable.
   dup 6 =  swap 0b =  or if  1  else  -1  then
;

0 value open-count


\ external
: open  ( -- flag )
  \ my-args  s" debug" $=  if  debug-on  then
   open-count  if
      reopen-hardware  dup  if  open-count 1+ to open-count  then
      exit
   else
      open-hardware  dup  if
         1 to open-count
         100 dma-alloc to sense-buf
         100 dma-alloc to inq-buf
      then
   then
;
: close  ( -- )
   open-count 1- to open-count
   open-count  if
      reclose-hardware
   else
      close-hardware
      inq-buf   100 dma-free
      sense-buf 100 dma-free
   then
;

\ headers

create sense-cmd  3 c, 0 c, 0 c, 0 c, ff c, 0 c,

: get-sense  ( -- failed? )     \ Issue REQUEST SENSE
   sense-buf ff  true  sense-cmd 6  execute-command  ( actual cswStatus )
   if  drop true  else  8 <  then
;

\ Give the device a little time to recover before retrying the command.
: delay-retry  ( -- )   1 ms  ;

\ RETRY-COMMAND executes a SCSI command.  If a check condition is indicated,
\ performs a "get-sense" command.  If the sense bytes indicate a non-fatal
\ condition (e.g. power-on reset occurred, not ready yet, or recoverable
\ error), the command is retried until the condition either goes away or
\ changes to a fatal error.
\
\ The command is retried until:
\ a) The command succeeds, or
\ b) The select fails, or dma fails, or
\ c) The sense bytes indicate an error that we can't retry at this level
\ d) The number of retries is exceeded.

\ #retries is number of times to retry (0: don't retry, -1: retry forever)
\
\ dma-dir is necessary because it is not always possible to infer the DMA
\ direction from the command.

\ Local variables used by retry-command?

0 value dbuf             \ Data transfer buffer
0 value dlen             \ Expected length of data transfer
0 value direction-in     \ Direction for data transfer

-1 value cbuf            \ Command base address
 0 value clen            \ Actual length of this command

\ external

\ errcode values:  0: okay   -1: phase error  otherwise: sense-key

: retry-command?  ( dma-buf dma-len dma-dir cmdbuf cmdlen #retries -- actual errcode )
   to #retries   to clen  to cbuf  to direction-in  to dlen  to dbuf

   begin
      dbuf dlen  direction-in  cbuf clen  execute-command  ( actual cswStatus )

      dup 0=   if  drop  0 exit  then   \ Exit reporting success
      dup 2 >  if  drop -1 exit  then   \ Exit reporting invalid CSW result code

      1 =  if                              ( actual )
         \ Do get-sense to determine what to do next
         get-sense  if                     ( actual )
            \ Treat a get-sense failure like a phase error; just retry the command
            -1                             ( actual errcode )
         else                              ( actual )
            classify-sense  case   ( actual -1|0|1 )
               \ If the sense information says "no sense", return "no-error"
               0  of  0 exit  endof

               \ If the error is fatal, return the sense-key
               -1  of  sense-buf 2+ c@  exit  endof
            endcase
            sense-buf 2+ c@                ( actual errcode )
         then
      else                                 ( actual )
         -1     \ Was phase error          ( actual errcode )
      then                                 ( actual errcode )

      \ If we get here, the command is retryable - either a phase error
      \ or a non-fatal sense code

      #retries 1- dup  to #retries         ( actual errcode #retries )
   while                                   ( actual errcode )
      2drop                                ( )
      delay-retry
   repeat                                  ( actual errcode )
;

\ external

\ Simplified routine for commands with no data transfer phase
\ and simple error checking requirements.

: no-data-command  ( cmdbuf -- error? )
   >r  0 0 true  r> 6  -1  retry-command?  nip
;

\ short-data-command executes a command with the following characteristics:
\  a) The data direction is incoming
\  b) The data length is less than 256 bytes

\ The host adapter driver is responsible for supplying the DMA data
\ buffer; if the command succeeds, the buffer address is returned.
\ The buffer contents become invalid when another SCSI command is
\ executed, or when the driver is closed.

: short-data-command  ( data-len cmdbuf cmdlen #retries -- true | buffer len false )
   >r >r >r  inq-buf swap  true  r> r> r>  retry-command?   ( actual error-code )
   if  drop true  else  inq-buf swap false  then
;



\ Here begins the implementation of "show-children", a word that
\ is intended to be executed interactively, showing the user the
\ devices that are attached to the SCSI bus.

\ Tool for storing a big-endian 24-bit number at an unaligned address

: 3c!  ( n addr -- )  >r lbsplit drop  r@ c!  r@ 1+ c!  r> 2+ c!  ;


\ Command block template for Inquiry command

create inquiry-cmd  h# 12 c, 0 c, 0 c, 0 c, ff c, 0 c,

\ external

: inquiry  ( -- error? )
   \ 8 retries should be more than enough; inquiry commands aren't
   \ supposed to respond with "check condition".
   \ However, empirically, on MC2 EVT1, 8 proves insufficient.

   inq-buf ff  true  inquiry-cmd 6  10  retry-command?  nip
;

\ headers

\ Reads the indicated byte from the Inquiry data buffer

: inq@  ( offset -- value )  inq-buf +  c@  ;

: .scsi1-inquiry  ( -- )  inq-buf 5 +  4 inq@  fa min  type  ;
: .scsi2-inquiry  ( -- )  inq-buf 8 +  d# 28 type    ;

\ Displays the results of an Inquiry command to the indicated device

: show-lun  ( unit -- )
   dup  set-address                               ( unit )
   inquiry  if  drop exit  then                   ( unit )
   0 inq@  h# 60 and  if  drop exit  then         ( unit )
   ."   Unit " . ."   "                           ( )
   1 inq@  h# 80 and  if  ." Removable "  then    ( )
   0 inq@  case                                   ( )

      0 of  ." Disk "              endof
      1 of  ." Tape "              endof
      2 of  ." Printer "           endof
      3 of  ." Processor "         endof
      4 of  ." WORM "              endof
      5 of  ." Read Only device"   endof
      ( default ) ." Device type " dup hex.
   endcase                                        ( )

   4 spaces
   3 inq@ 0f and  2 =  if  .scsi2-inquiry  else  .scsi1-inquiry  then
   cr
;

\ external

\ Searches for devices on the SCSI bus, displaying the Inquiry information
\ for each device that responds.

: show-children  ( -- )
   open  0=  if  ." Can't open SCSI host adapter" cr  exit  then

   max-lun 1+ 0  do  i show-lun  loop

   close
;

\ Inquire into the specified scsi device type and return the scsi
\ type and true if the device at the specified scsi address is found.

: get-scsi-type  ( lun -- false | type true )
   open  0=  if  2drop false exit  then
   set-address inquiry
   if  false  else  0 inq@ dup 7f =  if  drop false  else  true  then  then
   close
;



\ The diagnose command is useful for generic SCSI devices.
\ It executes both the "test-unit-ready" and "send-diagnostic"
\ commands, decoding the error status information they return.

create test-unit-rdy-cmd        0 c, 0 c, 0 c, 0 c, 0 c, 0 c,
create send-diagnostic-cmd  h# 1d c, 4 c, 0 c, 0 c, 0 c, 0 c,

: send-diagnostic ( -- error? )  send-diagnostic-cmd  no-data-command  ;


\ external

: diagnose  ( -- flag )
   0 0 true  test-unit-rdy-cmd 6   -1   ( dma$ dir cmd$ #retries )
   retry-command?  ?dup  if             ( actual error-code )
      nip                               ( error-code )
      ." Test unit ready failed - "     ( error-code )
      dup -1  if                        ( error-code )
         ." phase error " . cr          ( )
      else                              ( error-code )
         ." Sense code " .              ( )
         ." extended status = " cr      ( )
         base @ >r  hex                 ( )
         sense-buf 8 bounds ?do  i 3 u.r  loop cr ( )
         r> base !
      then
      true
   else                                 ( actual )
      drop                              ( )
      send-diagnostic                   ( fail? )
   then
;

\ fload ${BP}/dev/usb2/device/storage/scsidisk.fth
\  SCSI disk package implementing a "block" device-type interface.

\ \fload ${BP}/dev/usb2/device/storage/scsicom.fth  \ Utility routines for SCSI commands
\ purpose: words which are useful for both SCSI disk and SCSI tape device drivers.
\ See license at end of file


: parent-max-transfer  ( -- n )  s" max-transfer"  type 0 ;
: parent-set-address  ( lun -- )  s" set-address" type ;


\ Calls the parent device's "retry-command?" method.  The parent device is
\ assumed to be a driver for a SCSI host adapter (device-type = "scsi")

: retry-command?  ( dma-addr dma-len dma-dir cmd-addr cmd-len #retries -- actual errcode )
   s" retry-command?" 4drop
;


\ Simplified command execution routines for common simple command forms

: no-data-command  ( cmdbuf -- error? )  s" no-data-command" type 0 ;

: short-data-command  ( data-len cmdbuf cmdlen #retries -- true | buffer len false )
   s" short-data-command" type true
;


\ Some tools for reading and writing 2, 3, and 4 byte numbers to and from
\ SCSI command and data buffers.  The ones defined below are used both in
\ the SCSI disk and the SCSI tape packages.  Other variations that are
\ used only by one of the packages are defined in the package where they
\ are used.

: +c!  ( n addr -- addr' )  tuck c! 1+  ;
: 3c!  ( n addr -- )  >r lbsplit drop  r> +c! +c! c!  ;

: -c@  ( addr -- n addr' )  dup c@  swap 1-  ;
: 3c@  ( addr -- n )  2 +  -c@ -c@  c@       0  bljoin  ;
: 4c@  ( addr -- n )  3 +  -c@ -c@ -c@  c@      bljoin  ;


\ "Scratch" command buffer useful for construction of read and write commands

d# 10 constant /cmdbuf
create cmdbuf  0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c,
: cb!  ( byte index -- )  cmdbuf + c!  ;        \ Write byte to command buffer

create eject-cmd  h# 1b c, 1 c, 0 c, 0 c, 2 c, 0 c,

\ external
: device-present?  ( lun -- present? )
  \ parent-set-address
   s" inquiry"  type 0  0=
;

: eject ( -- )
  \ my-unit device-present?  
  if
      eject-cmd no-data-command  drop
   then
;
 
\ The deblocker converts a block/record-oriented interface to a byte-oriented
\ interface, using internal buffering.  Disk and tape devices are usually
\ block or record oriented, but the OBP external interface is byte-oriented,
\ in order to be independent of particular device block sizes.

0 value deblocker
: init-deblocker  ( -- okay? )
   s" deblocker"  type 0 to deblocker
   deblocker if
      true
   else
      ." Can't open deblocker package"  cr  false
   then
;

\ 0 means no timeout
: set-timeout  ( msecs -- )  s" set-timeout" type u. ;

0 value offset-low     \ Offset to start of partition
0 value offset-high

\ external
0  value label-package
true value report-failure


\ Sets offset-low and offset-high, reflecting the starting location of the
\ partition specified by the "my-args" string.

: init-label-package  ( -- okay? )
   0 to offset-high  0 to offset-low
 \  my-args  s" disk-label"  type 0 to label-package
   label-package dup  if
\      0 0  s" offset" label-package $call-method to offset-high to offset-low
   else
      report-failure  if
         ." Can't open disk label package"  cr
      then
   then
;

\ Checks to see if a device is ready

: unit-ready?  ( -- ready? )
   s" (00 00 00 00 00 00)" drop  no-data-command  0=
;

\ Some devices require a second TEST UNIT READY, despite returning
\ CHECK CONDITION, with sense NOT READY and MEDIUM NOT PRESENT.

: retry-unit-ready?  ( -- ready? )
   unit-ready?  ?dup  if  exit  then
   unit-ready?
;

\ Ensures that the disk is spinning, but doesn't wait forever

create sstart-cmd  h# 1b c, 0 c, 0 c, 0 c, 1 c, 0 c,

: timed-spin  ( -- error? )
   0 0 true  sstart-cmd 6  -1 retry-command?  nip  ?dup  if  ( error-code )
      \ true on top of the stack indicates a hardware error.
      \ We don't treat "illegal request" as an error because some drives
      \ don't support the start command.  Everything else other than
      \ success is considered an error.
      5 <>                                       ( error? )
   else                                          ( )
      false                                      ( false )
   then                                          ( error? )

   0 set-timeout
;

create read-capacity-cmd h# 25 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 0 c, 

: get-capacity  ( -- false | block-size #blocks false true )
   8  read-capacity-cmd 0a  0  short-data-command  if  ( )
      false
   else                                        ( adr len )
      8 <>  if  drop false exit  then          ( adr )
      dup 4 + 4c@  swap 4c@  1+  false true
   then
;

0 [if]
\ This is a "read for nothing", discarding the result.  It's a
\ workaround for a problem with the "Silicon Motion SMI331" controller
\ as used in the "Transcend TS2GUSD-S3" USB / MicroSD reader.  That
\ device stalls "read capacity" commands until you do the first block
\ read. The first block read stalls too, but afterwards everything works. 
: nonce-read  ( -- )
   d# 512 dma-alloc  >r
   r@ d# 512 true  s" (28 00 00 00 00 00 00 00 01 00)"  ( data$ in? cmd$ )
   0  retry-command? 2drop
   r> d# 512 dma-free
;
[then]

: read-block-extent  ( -- true | block-size #blocks false )
   \ Try "read capacity" a few times.  Support for that command is
   \ mandatory, but some devices aren't ready for it immediately.
   d# 20  0  do
      get-capacity  if  unloop exit  then  ( )
      d# 200 ms
   loop

0 [if]
   \ At least one device stalls read-capacity until the first block read
   nonce-read

   \ Retry it a few more times
   d# 18  0  do
      get-capacity  if  unloop exit  then  ( )
      d# 200 ms
   loop
[then]

   \ If it fails, we just guess.  Some devices violate the spec and
   \ fail to implement read_capacity
   d# 512  h# ffffffff  false
;

\ external

[defined] report-geometry [if]
create mode-sense-geometry    h# 1a c, 0 c, 4 c, 0 c, d# 36 c, 0 c,

\ The sector/track value reported below is an average, because modern SCSI
\ disks often have variable geometry - fewer sectors on the inner cylinders
\ and spare sectors and tracks located at various places on the disk.
\ If you multiply the sectors/track number obtained from the format info
\ mode sense code page by the heads and cylinders obtained from the geometry
\ page, the number of blocks thus calculated usually exceeds the number of
\ logical blocks reported in the mode sense block descriptor, often by a
\ factor of about 25%.

\ Return true for error, otherwise disk geometry and false
: geometry  ( -- true | sectors/track #heads #cylinders false )
   d# 36  mode-sense-geometry  6  2  ( len cmd$ #retries )
   short-data-command  if  true exit  then   ( adr len )
   d# 36 <>  if  drop true exit  then        ( adr )
   >r                                ( r: adr )
   r@ d# 17 + c@   r@ d# 14 + 3c@    ( heads cylinders )
   2dup *  r> d# 4 + 4c@             ( heads cylinders heads*cylinders #blocks )
   swap /  -rot                      ( sectors/track heads cylinders )
   false   
;
[then]

\ This method is called by the deblocker

0 value #blocks
0 value block-size



\ Read or write "#blks" blocks starting at "block#" into memory at "addr"
\ Input? is true for reading or false for writing.
\ command is  8  for reading or  h# a  for writing
\ We use the 6-byte forms of the disk read and write commands where possible.

: 2c!  ( n addr -- )  >r lbsplit 2drop  r> +c!         c!  ;
: 4c!  ( n addr -- )  >r lbsplit        r> +c! +c! +c! c!  ;

: r/w-blocks  ( addr block# #blks input? command -- actual# )
   cmdbuf /cmdbuf erase
[defined] use-short-form [if]
   2over  h# 100 u>  swap h# 20.0000 u>=  or  if  ( addr block# #blks dir cmd )
[then]
      \ Use 10-byte form
      h# 20 or  0 cb!  \ 28 (read) or 2a (write)  ( addr block# #blks dir )
      -rot swap                                   ( addr dir #blks block# )
      cmdbuf 2 + 4c!                              ( addr dir #blks )
      dup cmdbuf 7 + 2c!                          ( addr dir #blks )
      d# 10                                       ( addr dir #blks cmdlen )
[defined] use-short-form [if]
   else                                           ( addr block# #blks dir cmd )
      \ Use 6-byte form
      0 cb!                                       ( addr block# #blks dir )
      -rot swap                                   ( addr dir #blks block# )
      cmdbuf 1+ 3c!                               ( addr dir #blks )
      dup 4 cb!                                   ( addr dir #blks )
      6                                           ( addr dir #blks cmdlen )
   then
[then]
   swap                                           ( addr dir cmdlen #blks )
   dup >r                                         ( addr input? cmdlen #blks )
   block-size *  -rot  cmdbuf swap  -1  ( data-adr,len in? cmd-adr,len #retries )
   retry-command?  nip  if                        ( r: #blks )
      r> drop 0
   else
      r>
   then    ( actual# )
;

\ external

\ These three methods are called by the deblocker.

: max-transfer  ( -- n )   parent-max-transfer  ;
: read-blocks   ( addr block# #blocks -- #read )   true  d# 8  r/w-blocks  ;
: write-blocks  ( addr block# #blocks -- #written )  false d# 10 r/w-blocks  ;

\ Methods used by external clients

0 value open-count

: open  ( -- flag )
 \  my-unit parent-set-address

   open-count  if
      d# 2000 set-timeout
   else

      \ Set timeout to 45 sec: some large (>1GB) drives take
      \ up to 30 secs to spin up.
      d# 45 d# 1000 *  set-timeout

      retry-unit-ready?  0=  if  false  exit  then

      \ It might be a good idea to do an inquiry here to determine the
      \ device configuration, checking the result to see if the device
      \ really is a disk.

      \ Make sure the disk is spinning

      timed-spin  if  false exit  then

      read-block-extent  if  false exit  then  ( block-size #blocks )
      to #blocks  to block-size

      d# 2000 set-timeout
      init-deblocker  0=  if  false exit  then
   then

   init-label-package  0=  if
      open-count 0=  if
         deblocker 
      then
      false exit
   then

   open-count 1+ to open-count

   true
;

: close  ( -- )
   open-count dup  1- 0 max to open-count  ( old-open-count )
   label-package            ( old-open-count )
   1 =  if
      deblocker 
   then
;

: seek  ( offset.low offset.high -- okay? )
   offset-low offset-high d+  s" seek"   type
;

: read  ( addr len -- actual-len )  s" read"  type 0 ;
: write ( addr len -- actual-len )  s" write" type 0 ;
: load  ( addr -- size )            s" load"  type 0 ;

: size  ( -- d.size )  s" size" type 0 ;

\ init
[defined] tr-flag [if] trace-off [then]
only forth definitions
