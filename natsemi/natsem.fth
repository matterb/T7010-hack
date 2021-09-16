only forth also system definitions
base @ hex

\ Copyright (C) 2012 Michael Brown <mbrown@fensystems.co.uk>.

0 constant NATSEMI_CR                   \ configuration register
h# 600 constant NATSEMI_RX_MAX_LEN
4 constant RING_SIZE


\ eeprom access

1 value T1
10 value eep-org  \ eeprom mode x16 or x8
1 eep-org 1- lshift value eep-mask
6 value addr-len
1 addr-len 1- lshift value addr-mask

: mear@ ( -- b )  8 eth-io@ ;
: mear! ( b -- ) 8 eth-io! ;

: eedi-high ( -- ) mear@ 1 or mear! ; \    DI 
: eedi-low ( -- ) mear@ 1 invert and mear! ;
: eeclk-low ( -- ) mear@ 4 invert and mear! ;    \ CLK
: eeclk-high ( -- ) mear@ 4 or mear! ;
: eeclk-toggle ( -- ) T1 ms eeclk-high T1 ms eeclk-low ;
: eedo-@ ( -- ) mear@ 1 rshift 1 and ;    \ DO  is MEAR[1]
: eesel-high ( -- ) mear@ 8 or mear! ; \ enable chip CS
: eesel-low  ( -- ) mear@ 8 invert and mear! ; \ disable chip

: eep-stop ( -- ) eeclk-low eesel-low ;

: send-bit ( flg --) if eedi-high else eedi-low then eeclk-toggle ;

: send-cmd ( b -- )         \ command is start + 2 bits
 eesel-high
 4 3 0 do  2dup and send-bit 1 rshift loop
 2drop 
;

\ extra 0 dummy bit on reads
: get-data ( -- u )
\  scl-toggle          \ disregard first bit
  0 eep-org 0 do 1 lshift eeclk-toggle eedo-@ or  loop
;

: send-data ( w -- )
  eep-mask eep-org 0 do 2dup and send-bit 1 rshift loop
  2drop 
;

\ send addr.
\ an-a0   4-bit address shifted left!

: send-addr ( addr -- )
  addr-mask addr-len 0 do 2dup and send-bit 1 rshift loop
  2drop
;

: eep-cmd@ ( addr --  ) 6 send-cmd send-addr ;
: eep-cmd! ( w addr  -- ) 5 send-cmd send-addr send-data ;
: eep-cmd-wen ( -- ) 4 send-cmd 30 send-addr ;
: eep-cmd-wds ( -- ) 4 send-cmd 00 send-addr ;
: eep-cmd-wrall ( w -- ) 4 send-cmd 10 send-addr send-data ;
: eep-cmd-erall ( -- ) 4 send-cmd 20 send-addr ;
: eep-cmd-eras ( addr  -- ) 7 send-cmd send-addr ;

\ check programming status
: eep-rdy-bsy ( -- flg )
\ sdo stays low while chip is busy
\ goes High when finished processing
\ put a timer of < 10 ms !
  eesel-high
  begin 2e emit eedo-@ until
;

: eep-chk ( -- flg) ; 
: eep-@ ( addr --- w ) eep-cmd@ get-data eep-stop ;
: eep-! ( w addr -- )  eep-cmd! eep-stop ;
: eep-wen ( -- ) eep-cmd-wen eep-stop ;
: eep-wds ( -- ) eep-cmd-wds eep-stop ;
: eep-eral ( -- ) eep-cmd-erall eep-stop ;
: eep-erase ( addr -- ) eep-cmd-eras eep-stop ;
: eep-wrall ( w -- ) eep-cmd-wrall eep-stop ;
: eep-reset  eesel-low eeclk-toggle ;

\ compute eeprom checksum 
: low-byte ( w -- b) ff and ;
: high-byte ( w -- b) 8 rshift low-byte ;
: (crchk) ( w -- b)  dup low-byte swap high-byte + ff and ;

: eeprom-crc ( -- w )
0 h# 0b 0 do i eep-@ (crchk) .s cr + loop
h# 55 + low-byte negate  
low-byte 8 lshift h# 55 or
;
  
: eep-show (  -- )
  h# 0c 0 do  
      cr i dup 
      u. ." = " space eep-@ u.
  loop 
;

0 value word1
0 value word2
0 value word3
0 value word4
variable mask
 
: hex1 ( c -- ) h# 0f and dup h# 0a < if h# 30 else h# 37 then + emit ;
: hex2 ( c -- ) dup d# 4 rshift hex1 hex1 ;
: hex4 ( w -- ) dup d# 8 rshift hex2 hex2 ;
: hex8 ( x -- ) dup d# 16 rshift hex4 hex4 ;
: swab ( w -- w ) dup d# 8 lshift swap d# 8 rshift h# 00ff and or h# ffff and ;
: endian ( u -- ) dup h# ffff and swab d# 16 lshift  swap d# 16 rshift swab or ;
: 2endian ( d -- ) endian swap endian ;
: 2+ ( x -- x ) 2 + ; 
: 2- ( x -- x ) 2 - ;
: +h ( u1 u2 -- u1+u2/2**16 ) 
    over +     ( a a+b ) 
    u> d# 1 and 
; 

: +1c ( u1 u2 -- u3)   \ one's complement add, as in TCP checksum 
    2dup +h + + 
; 

: (macfactor) ( nic word -- nic )
 2 mask !
   15 begin
    dup while
       over h# 8000 and if
            rot mask @ or -rot
        then
        swap 2* swap
        mask dup @ 2* swap !
        1-
    repeat
    2drop h# ffff and
;

: (macfactor1) ( word nic --word)
 0 swap (macfactor) over 1 and or
;

: eep-mac@ ( -- nic0 nic1 nic2  )
 \  Read 16 bit words 6 - 9 from the EEProm.  They contain the hardwares MAC
 \ address in a rather cryptic format.
   6 eep-@ to word1 7 eep-@ to word2 
   8 eep-@ to word3 9 eep-@ to word4   
 
   word1 1 and word2 (macfactor) swab  \ nic 0  
   word2 1 and word3 (macfactor) swab  \ nic 1
   word3 1 and word4 (macfactor) swab  \ nic 2
;
 
: eep-mac! ( nic2 nic1 nic0 -- )
 0 over 1 and or u.    \ [6]
 (macfactor1)  u.      \ [7]
 (macfactor1)  u.      \ [8]
 0 swap (macfactor) u. \ [9]
;

\ A 32-bit packet descriptor dword aligned single fragment per descriptor
 
begin-structure natsemi-desc
        field: nsd>link      \ Link to next descriptor dword aligned
        field: nsd>cmdsts    \ Command / status 
        field: nsd>bufptr    \ Buffer pointer dword aligned for rcv 
end-structure

\ packet in single descriptor

\ nsd>link=0
\ nsd>cmdsts h# 0fff and = 64 \ buffer-len
\ nsd>cmdsts DSC_MORE and = 0
\ nsd>ptr = buffer = MAC|NTWKHDR|DATA

\ packet in multiple descriptors
\ descriptor1
\ nsd>link= addrrss of descriptor2
\ nsd>cmdsts h# 0fff and = buffer1-len
\ nsd>cmdsts DSC_MORE and = 1
\ nsd>ptr = buffer1 = MAC

\ nsd>link=address of descritor 3
\ nsd>cmdsts h# 0fff and = buffer2-len
\ nsd>cmdsts DSC_MORE and = 1
\ nsd>ptr = buffer2 = NTWKHDR

\ nsd>link=0
\ nsd>cmdsts h# 0fff and = buffer3-len
\ nsd>cmdsts DSC_MORE and =0
\ nsd>ptr = buffer3 = DATA

\ transmit sequence
\ 1. copy data to transmit descriptor
\ 2. set OWN bit add descriptor to transmit list
\    if list is empty set TXDP to this descriptor
\    else append to end of list
\ 3.  set TXE bit in CR register
\ 4. check last descriptor for completion status

\ receive sequence
\ 1. build receive descriptor ring
\    clear OWN bit
\ 2. set RXDP to address of first descriptor
\ 3. if no RXOVR
\    if OWN is set and MORE is clr then end of packet
\    SIZE has data received
\
\ status definitions

begin-structure natsemi-ring
    field: nsr>descriptor         \ Descriptors
    field: nsr>head
    field: nsr>tail
    field: nsr>count
    field: nsr>unaligned
end-structure
 
 
: (ring-empty) ( ring -- flg) dup nsr>head @ swap nsr>tail @ = ;
: (ring-full)  ( ring -- flg) dup nsr>head @ swap nsr>tail @ nsd>link @ = ;
;

natsemi-ring buffer: ns-tx-ring   \ Transmit descriptor ring
natsemi-ring buffer: ns-rx-ring     \ Receive Descriptor ring

variable ns-tx-bufs
variable ns-rx-bufs 

RING_SIZE NATSEMI_RX_MAX_LEN * 3 + allocate throw
ns-rx-bufs !

d# 1518 allocate throw ns-tx-bufs !


\ link array of descriptor entries
: (link!) ( u -- u' ) dup natsemi-desc + dup rot ! ;

\ Create descriptor ring
: natsemi-create-ring ( ring count --  )
 \ region size + 3 bytes to ensure dword aligned descriptors
 over nsr>count !
 dup nsr>count @ natsemi-desc * 3 + 
 \ allocate memory 
 allocate throw       
 \ should be dword aligned
 over nsr>unaligned !
 dup nsr>unaligned @ 3 + 3 invert and    \ dword align 
 over nsr>descriptor !
 \ initialize descriptors
 dup nsr>descriptor @ over nsr>count @ natsemi-desc *  0 fill
 \ align first descriptor
 dup nsr>descriptor @ over nsr>head !
 dup nsr>descriptor @ over nsr>tail !
 \ insert count-1 decriptors into ring
 \ last one points to head
  dup nsr>count @ swap 
  nsr>descriptor @ dup rot 1 do 
    i NATSEMI_RX_MAX_LEN * ns-rx-bufs @ + over nsd>bufptr !
    NATSEMI_RX_MAX_LEN over nsd>cmdsts !
  (link!) loop
  ns-rx-bufs @ over nsd>bufptr !
  NATSEMI_RX_MAX_LEN over nsd>cmdsts ! 
  !
;

: ns-next-desc ( ring -- desc )
dup (ring-full) 0= if
    dup nsr>tail @
    dup nsd>link @
    rot nsr>tail !
else
    ." overrun.."
    nsr>tail @
then
;

: ns-ring-deque ( ring -- desc )
dup (ring-empty) 0= if
    dup nsr>head @
    dup nsd>link @
    rot nsr>head !
else
    ." underrun "
    nsr>head @
then
;

: natsemi-destroy-ring ( ring -- )
 dup nsr>descriptor off
 dup nsr>head off
 dup nsr>tail off
 dup nsr>unaligned @ free throw
 nsr>unaligned off
;
    

\ Read link status
: natsemi-link ( -- u ) 4 eth-io@ h# 1f rshift ;
: natsemi-full-duplex? ( -- u ) 4 eth-io@ h# 1d rshift 1 and ;

\ Enable or disable interrupts
: natsemi-enable-intr ( -- )  1 h# 18 eth-io! ;
: natsemi-disable-intr ( -- ) 0 h# 18 eth-io! ;


\ * Set perfect match filter address

: pmatch! ( nic2 nic1 nic0 -- )   \ three 16-bit words
 h# 48 eth@ h# 3ff invert and dup >r
 h# 48  eth! h# 4c eth!
 r@ 2 + h# 48 eth! h# 4c eth!
 r> 4 + h# 48 eth! h# 4c eth!
;

: pmatch@ ( -- nic2 nic1 nic0 )
 h# 48 eth@ h# 3ff invert and dup >r
 4 +    h# 48 eth! h# 4c eth@
 r@ 2 + h# 48 eth! h# 4c eth@
 r>     h# 48 eth! h# 4c eth@
;

: natsemi-mac ( -- ) eep-mac@ pmatch! ;

: natsemi-refill-rx ( -- ) \ Refill receive descriptor ring
 begin 
    ns-rx-ring (ring-full) 0= while
        \ Get next receive descriptor 
        ns-rx-ring ns-next-desc
        
        dup nsd>bufptr @ dup 0= if
            drop
            \ Allocate I/O buffer
            NATSEMI_RX_MAX_LEN allocate throw
            \ Populate receive descriptor 
            over nsd>bufptr !
        then
        nsd>cmdsts 
        NATSEMI_RX_MAX_LEN swap !
        
        \ Notify card that there are descriptors available 
        4 0 eth!
repeat
;

: ns-tx-ring-full? ( -- flg) ns-tx-ring (ring-full) ;
  
: transmit ( addr len -- ) \ Transmit packet
  \ Get next transmit descriptor 
  h# 80000000 or ns-tx-ring nsr>head @ tuck
  nsd>cmdsts ! 
  dup h# 20 eth!    \ update tx descriptor pointer
  nsd>bufptr !

  \ Notify card that there are packets ready to transmit
  1 0 eth!
;

: ns-pkt-rcv? ( -- flg ) ns-rx-ring  (ring-empty) 0= ;
: ns-pkt-xmt? ( -- flg ) ns-tx-ring  (ring-empty) 0= ;

: descriptor-cmdsts ( ring -- u )
  dup nsr>descriptor @ swap 
  nsr>head @ natsemi-desc * +
  nsd>cmdsts @
;

: descriptor-busy? ( ring -- flg) descriptor-cmdsts h# 80000000 and 0<> ;
: descriptor-ok? ( ring -- flg ) descriptor-cmdsts h# 8000000 and 0<> ;

: natsemi-poll-rx ( -- )        \ Poll for received packets
\ Check for received packets
    begin 
    ns-pkt-rcv? while
    ns-rx-ring descriptor-busy? 0= while
\       \ Populate I/O buffer
\        iobuf = natsemi->rx_iobuf[rx_idx];
\        natsemi->rx_iobuf[rx_idx] = NULL;
\        len = ( le32_to_cpu ( rx->common.cmdsts ) &
\            NATSEMI_DESC_SIZE_MASK );
\        iob_put ( iobuf, len - 4 /* strip CRC */ );

       \ Hand off to network stack
        ns-tx-ring descriptor-ok?       \ netdev-rx (  iobuf ior )
                
        ns-rx-ring ns-ring-deque        \ buffer len nedev-rx
    repeat then
;

: natsemi-poll-tx ( -- )                \ Poll for completed packets
 begin
 ns-pkt-xmt?    while                   \ check for complete packets
 ns-tx-ring descriptor-busy? 0= while   \ check for xmt finished
      ns-tx-ring descriptor-ok?         \ netdev-tx=complete-next

    ns-tx-ring nsr>head dup @ 1+ 4 /mod swap !
 repeat then
;
 
: natsemi-poll ( -- )     
   natsemi-link       \  Poll for link state. 

\ Check for and acknowledge interrupts
    10 eth-io@  dup if 
       \ Poll for TX completions
       dup 80 and if natsemi-poll-tx then
       \ Poll for RX completions
       2 and if natsemi-poll-rx then
       \ Refill RX ring 
       natsemi-refill-rx
    then
;

: natsemi-reclaim-txbuf ( -- )  ;
: natsemi-start-rx ( -- ) ;

: natsemi-irq ( -- ) \ interrupt handler
 h# 10 eth@
 dup 1 and if natsemi-start-rx then
 dup h# 7c0 and if natsemi-reclaim-txbuf then
 h# 4000 and if ." ETH Link Status Changed" cr then
;

\ Device reset

: natsemi-soft-reset ( -- )
\ Initiate reset NATSEMI_CR_RST  0x00000100
 h# 100 0 eth-io!
 h# 9000 h# 80 eth!     \ autonegotiaion 
\ Wait for reset to complete 
    100 begin
        0 eth-io@ 100 and 
        over and while
            1 ms 1-
     repeat
\ if counter is zero then timout occurred
    drop
;

: natsemi-reload-config ( --)
\ Initiate reload 
  4 h# 0c eth-io!
\ Wait for reload to complete 
    d# 100 begin
        h# 0c eth-io@ 4 and 
        over and while
          1 ms 1-
     repeat
\ if counter is zero then timeout occurred
    drop
;

: natsemi-reset ( -- )      \ Reset hardware
natsemi-reload-config  natsemi-soft-reset 
  4 eth-io@ dup 4 and if
          8000 or 100 or dup 2000 and invert if
          1000 invert and
          then
  then
  4 eth-io!
;

: natsemi-init ( -- )   \ recommend setting for silicon rev. SRR= 302h
     1 h# 0cc eth! h# 189c h# 0e4 eth! 
     0 h# 0fc eth! h# 5040 h# 0f4 eth!
 h# 8c h# 0f8 eth!
;

: natsemi-open ( -- ) \  Open network device
 natsemi-init
 0 15 0 pci-dev     \ enable bus mastering
 4 over pci@ 4 or 4 rot pci!
 
\ Disable PME
    0 h# 3c eth!
\ reset device
    natsemi-reset 

\ Create transmit descriptor ring
   ns-tx-ring RING_SIZE  natsemi-create-ring 

\ Set transmit configuration and descriptor pointer 
   h# 0d0f01020  h# 24 eth! 
   ns-tx-ring nsr>head @ h# 20 eth!

\ Create receive descriptor ring
   ns-rx-ring RING_SIZE  natsemi-create-ring 
   
\ Set receive configuration and descriptor pointer
    h# 10600010 h# 34 eth!
    ns-rx-ring nsr>head @ h# 30 eth!
    
\ Install interrupt vector
\ setup phy capabilities
\    natsemi-phy

\ set mac address
    natsemi-mac

\ Set receive filter configuration
    h# 0f0400000 h# 48 eth!  
    
\ Fill receive ring 
\    natsemi-refill-rx 

\ Unmask transmit and receive interrupts.  (Interrupts will
\ not be generated unless enabled via the IER.)
    
    h# 82 h# 14 eth!
;

: natsemi-close ( -- )  \ Close network device

\ Mask transmit and receive interrupts
   0 h# 14 eth!
\ Reset and disable transmitter and receiver
   h# 30 0 eth!
   h# 0a 0 eth!
\ Discard any unused receive buffers
  ns-rx-ring nsr>head @ nsd>link @ 
  begin
  2dup <> while
    dup nsd>bufptr @ ?dup if 
        free throw
    then
    nsd>link @
 repeat
 2drop
 
\ Destroy receive descriptor ring
    ns-rx-ring natsemi-destroy-ring

\ Destroy transmit descriptor ring 
    ns-tx-ring natsemi-destroy-ring 
;

\  ========= ethernet packets =================

begin-structure /eth-pkt
 6 +field eth>dmac
 6 +field eth>smac
 wfield:  eth>type
 cfield: eth>data
end-structure

  
 
: eth. ( addr  -- )
 w@+ swap w@+ swap w@ swap rot
 swab hex4 [char] - emit
 swab hex4 [char] - emit
 swab hex4
;

: ethaddr-broadcast ( -- ) h# ffff dup dup ;
: net-my-mac ( -- nic0 nic1 nic2) eep-mac@ ;
: mac. ( nic0 nic1 nic2 -- ) 
  swap rot hex4 [char] - emit
  hex4 [char] - emit hex4
;
  

variable cur-pkt
variable writer         \ ptr into out packet

: pkt-buf-begin (  -- )  ns-tx-bufs @ dup cur-pkt ! writer !  ;
: pkt-buf-end ( -- len ) writer @ cur-pkt @ - ;
: pkt-buf-1+  ( n -- ) writer +! ;
: pkt-buf-c,  ( n -- ) writer @ c! 1 pkt-buf-1+ ;
: pkt-buf-w,  ( w -- ) wbsplit pkt-buf-c, pkt-buf-c, ;
: pkt-buf-2,  ( w0 w1 -- ) swap pkt-buf-w, pkt-buf-w, ;
: pkt-buf-l,   ( x -- ) lwsplit swap pkt-buf-2, ;
: pkt-buf-3,  ( w0 w1 w2 -- ) rot pkt-buf-w, pkt-buf-2, ;
: pkt-buf-,0  ( n -- ) 0 do  0 pkt-buf-c, loop ;
: pkt-buf-s,  ( caddr u -- ) \ tuck writer @ swap cmove pkt-buf-1+ ;
    0 do
        dup c@
        pkt-buf-c,
        1+
    loop
    drop
;

: pkt-buf-w@ ( addr -- w ) w@ swab ;
: pkt-buf-@  ( addr -- u ) @ endian ;
: pkt-buf-c@ ( addr -- b ) c@ ;
: pkt-buf-w! ( w addr -- ) swap swab swap w! ;
: pkt-buf-!  ( l addr -- ) swap endian swap ! ;
: pkt-buf-c! ( b addr -- ) c! ;

\ copy n words addr from incoming+offset 
: pkt-buf-src ( n offset -- ) swap 2* pkt-buf-s, ; 

: pkt-buf-chksum ( addr nwords -- sum )
    d# 0 swap
    0 do
        over pkt-buf-w@       ( addr sum v )
        +1c
        swap 2+ swap
    loop
    nip
    invert
;

\ ================================================================
\ It is neat to write IP address literals e.g.
\ ip# 192.168.0.1
\ ================================================================

: octet# ( c -- u ) 0 0 rot parse >number throw 2drop ;

: ip# ( "< spaces>dotquad" -- u)
  base @ >r decimal
  [char] . octet# 8 lshift
  [char] . octet# or 8 lshift
  [char] . octet# or 8 lshift
  bl octet#  or
  r> base !
;
: (ip.) ( b -- ) s>d <# [char] . hold #s #> type ;
: ip. ( x -- ) 
  base @ >r decimal lbsplit (ip.) (ip.) (ip.) u. r> base ! 
;
 
base !

\ call natsem-open to initialize  - in future use a status flag
