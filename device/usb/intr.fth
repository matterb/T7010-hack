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


