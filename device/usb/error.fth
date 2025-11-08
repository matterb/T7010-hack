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


