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


