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


