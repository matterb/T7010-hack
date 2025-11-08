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


