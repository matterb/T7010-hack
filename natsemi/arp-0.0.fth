( ARP: Address Resolution Protocol           JCB 13:12 08/24/10)
( verision 0.0 )
\ requires natsemi.fth 

\  ARP uses a small cache of entries.
\  Each entry has an age counter.
\ new entries have an age of 0, 
\ any entry with an age >0 is old.
\  age:2 mac:6 ip:4


\  begin portability words 
: 2+ ( x -- x ) 2 + ; 
: 2- ( x -- x ) 2 - ;
: w+! ( w addr -- ) tuck w@ + swap w! ;
: dxor ( d d -- d) rot xor >r xor r> ;
: dand ( d d -- d ) rot and >r and r> ;
: ?: ( u1 u2 flg -- u1|u2 ) if drop else nip then ;
: hex1 ( c -- ) h# 0f and dup h# 0a < if h# 30 else h# 37 then + emit ;
: hex2 ( c -- ) dup d# 4 rshift hex1 hex1 ;
: hex4 ( w -- ) dup d# 8 rshift hex2 hex2 ;
: hex8 ( x -- ) dup d# 16 rshift hex4 hex4 ;
: swab ( w -- w ) dup d# 8 lshift swap d# 8 rshift h# 00ff and or h# ffff and ;
: endian ( u -- ) dup h# ffff and swab d# 16 lshift  swap d# 16 rshift swab or ;
: 2endian ( d -- ) endian swap endian ;
: eth-send ( -- ) ." eth-send" cr ;
: eth-pkt-src (  -- ) ." eth-pkt-src" cr ;
: net-eth-bcst? ( addr -- flg ) w@+ w@+ swap w@ and and ;
: net-my-mac? ( addr -- flg )  ;
: net-my-ip? ( addr -- flg )  ;

\ end portability

variable ip-address
variable ip-router
variable ip-subnet-mask


d# 12 constant /arp-cache-entry
d# 5 constant #arp-cache-entries
/arp-cache-entry #arp-cache-entries * d# 64 max constant arp-size
create arp-cache arp-size allot
#arp-cache-entries 1- /arp-cache-entry * arp-cache + constant arp-cache-last


: arp-foreach   ( xt -- )
    arp-cache-last 2>r
    begin
        2r@ swap            \  ptr func
        execute
        r> dup /arp-cache-entry - >r
        arp-cache =
    until
    2r> 2drop
;

 
: arp-. ( arp-entry -- )   \ for debugging FIXME
    dup 2+ swap w@ hex4 space       \  age
    w@+ swap             \ mac0 addr
    w@+ swap             \ mac1 addr
    w@+ swap             \ mac2 addr
    @ ip. space
    rot swap mac. cr
;

\ for debugging
: arp-dump ( -- ) ['] arp-. arp-foreach ;


: arp-del  ( arp-entry -- )  h# ffff swap w!  ;
: arp-reset ( -- ) ['] arp-del arp-foreach ;
: arp-used?   ( arp-entry -- flg )  w@ h# ffff <> ;
: arp-age-1  ( arp-entry -- ) dup arp-used? d# 1 and swap w+!  ;
: arp-age  ( -- )  ['] arp-age-1 arp-foreach ;
: arp-cmp  ( arp-entry0 arp-entry1 -- arp-entry ) over w@ over w@ > ?: ;
\  return the address of the oldest ARP entry
: arp-oldest ( -- arp-entry ) arp-cache ['] arp-cmp arp-foreach ;

\ ============== arp packet =============
begin-structure /arp-pkt
 wfield: arp>hw-type
 wfield: arp>prot-type
 cfield: arp>hw-len
 cfield: arp>prot-len
 wfield: arp>opcode
 cfield: arp>data
end-structure

begin-structure /arp-ipv4
 6 +field arpv4>smac
 field:   arpv>sip
 6 +field arpv4>dmac
 field:   arpv4>dip
end-structure
     
\  ARP offsets
\  d# 28 sender ethaddr
\  d# 34 sender ip
\  d# 38 target ethaddr
\  d# 44 target ip
d# 0  constant OFFSET_ETH
d# 0  constant OFFSET_ETH_DST
d# 14 constant OFFSET_ETH_SIZE
d# 6  constant OFFSET_ETH_SRC
d# 12 constant OFFSET_ETH_TYPE

d# 22 constant OFFSET_ARP_SRC_ETH
d# 28 constant OFFSET_ARP_SRC_IP
d# 32 constant OFFSET_ARP_DST_ETH
d# 38 constant OFFSET_ARP_DST_IP

: arp-response? ( arp-pkt --flg ) arp>opcode w@ d# 2 = ;

\  write the current arp response into the cache, replacing the oldest entry
: !--                   \  ( val ptr -- ptr-2 )
    tuck                \  ptr val ptr
    !
    2-
;

\ Current packet is an ARP response; 
\ write it to the given slot in the ARP cache, ageing all others

: arp-cache! ( ip mac2 mac1 mac0 arp-entry  -- )
    arp-age         \  because this new entry will have age d# 0
    d# 0 over w!    \  age d# 0
    \ swap w!+ w!+ w!+ !   
    2+ tuck w!
    2+ tuck w!
    2+ tuck w! 
    ! 
;

\  Comparison of IP
: arp-cmpip  ( ip arp-entry/0 arp-entry -- ip arp-entry )
    dup arp-used? if
        dup d# 8 + @ d# 2 pick <> ?:  \    ???
    else
        drop
    then
;

: arp-cache-find ( ip -- ip arp-entry|0 )
\  Find an IP. Zero if the IP was not found in the cache, ptr to entry otherwise
    d# 0 ['] arp-cmpip arp-foreach ;

: arp-whohas? ( ip  -- len )
    eth-pkt-begin
    ethaddr-broadcast eth-pkt-3,
    eep-mac@ eth-pkt-3,
    h# 806                   \  frame type
    d# 1                       \  hard type
    h# 800                   \  prot type
    eth-pkt-3,
    h# 0604                  \  hard size, prot size
    d# 1                       \  op (1=request)
    eth-pkt-2,
    eep-mac@ eth-pkt-3,
    net-my-ip eth-pkt-l,
    ethaddr-broadcast eth-pkt-3,
    eth-pkt-l,
    eth-pkt-end
;

\  Look up ethaddr for given IP.
\  If found, return pointer to the 6-byte ethaddr
\  If not found, issue an ARP request and return d# 0.

: arp-lookup ( ip -- arp-entry|0 )
    dup 
    ip-router @ xor ip-subnet-mask @ and 0<> if
        drop ip-router @  then
    arp-cache-find          \  ip ptr
    dup 0= if
        swap                \  d# 0 ip
        arp-whohas?    \  d# 0
    else
        nip 2+          \  mac 
    then
;

\  If the current packet is an ARP request for our IP, answer it
: arp-responder ( arp-pkt -- )
    \  is destination ff:ff:ff:ff:ff:ff or my mac
    arp>dmac dup net-eth-bcst?
    swap net-my-mac? or 
    
    \ respond if target is my IP.
    over arp>dip @ net-my-ip?  and
    if
        eth-pkt-begin

        d# 3 OFFSET_ARP_SRC_ETH eth-pkt-src
        net-my-mac eth-pkt-3,
        h# 806                \  frame type
        d# 1                  \  hard type
        h# 800                \  prot type
        eth-pkt-3,
        h# 0604               \  hard size, prot size
        d# 2                  \  op (2=reply)
        eth-pkt-2,
        net-my-mac eth-pkt-3,
        net-my-ip eth-pkt-l,
        dup arp>smac 6 eth-pkt-s,
        dup arp>sip @ eth-pkt-l,

        eth-pkt-end drop
        eth-send
    then
;

: arp-announce ( -- )
    eth-pkt-begin

    ethaddr-broadcast eth-pkt-3,
    net-my-mac eth-pkt-3,
    h# 806                \  frame type
    d# 1                  \  hard type
    h# 800                \  prot type
    eth-pkt-3,
    h# 0604               \  hard size, prot size
    d# 2                  \  op (2=reply)
    eth-pkt-2,
    net-my-mac eth-pkt-3,
    net-my-ip eth-pkt-l,
    ethaddr-broadcast eth-pkt-3,
    net-my-ip eth-pkt-l,

    eth-pkt-end drop
    eth-send
;

: arp-handler ( arp-pkt -- )
    arp-responder
    arp-is-response
    if
        d# 2 OFFSET_ARP_SRC_IP w@ \ get src ip 
        arp-cache-find nip nip
        dup 0= if
            drop arp-oldest
        then
        arp-cache-write
    then
;

( natsemi-open  initializes network adapter )
: test1 h# 0a9fee60a ns-tx-bufs @ tuck arp-issue-whohas transmit ;
