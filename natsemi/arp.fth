( ARP: Address Resolution Protocol           JCB 13:12 08/24/10)
( version 0.1)
\ requires natsemi.fth 

\  ARP uses a small cache of entries.
\  Each entry has an age counter.
\ new entries have an age of 0, 
\ any entry with an age >0 is old.
\  age:2 mac:6 ip:4


\  begin portability words 


: w+! ( w addr -- ) tuck w@ + swap w! ;
: dxor ( d d -- d) rot xor >r xor r> ;
: dand ( d d -- d ) rot and >r and r> ;
: ?: ( u1 u2 flg -- u1|u2 ) if drop else nip then ;




: eth-send ( -- ) ." eth-send" cr ;
: net-eth-bcst? ( addr -- flg ) w@+ w@+ swap w@ and and ;
: net-my-mac? ( addr -- flg ) true ;


\ end portability

variable ip-address
variable ip-router
variable ip-subnet-mask
variable ip-dns

: net-my-ip ( -- u ) ip-address @ ;
: net-my-ip! ( u -- ) ip-address ! ; 
: net-my-ip? ( u -- flg )  ip-address @ = ;

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
    w@+ hex4 space         \  age
    dup eth. space         \ mac0 addr
    6 + @ ip. cr           \ ip addr
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
 field:   arpv4>sip
 6 +field arpv4>dmac
 field:   arpv4>dip
end-structure
     

d# 0  constant OFFSET_ETH
d# 0  constant OFFSET_ETH_DST
d# 6  constant OFFSET_ETH_SRC
d# 12 constant OFFSET_ETH_TYPE
d# 14 constant OFFSET_ETH_SIZE

d# 22 constant OFFSET_ARP_SRC_ETH
d# 28 constant OFFSET_ARP_SRC_IP
d# 32 constant OFFSET_ARP_DST_ETH
d# 38 constant OFFSET_ARP_DST_IP

: arp-response? ( arp-pkt --flg ) arp>opcode pkt-buf-w@ d# 2 = ;
\ eth>type = h#0806 and arp>opcode = 2
\  write the current arp response into the cache, replacing the oldest entry
\ : w!+ ( val addr -- addr+2 ) tuck !  2+ ;

\ Current packet is an ARP response; 
\ write it to the given slot in the ARP cache, ageing all others

: arp-cache-write ( ip mac2 mac1 mac0 arp-entry  -- )
    arp-age         \  because this new entry will have age d# 0
    d# 0 over w!    \  age d# 0
    \ swap w!+ w!+ w!+ !   
    2+ tuck w!
    2+ tuck w!
    2+ tuck w!
    2+ ! 
;

\  Comparison of IP
: arp-cmpip  ( ip arp-entry/0 arp-entry -- ip arp-entry )
    dup arp-used? if
        dup d# 8 + @ d# 3 pick <> ?:  \    ???
    else
        drop
    then
;

: arp-cache-find ( ip -- ip arp-entry|0 )
\  Find an IP. Zero if the IP was not found in the cache, ptr to entry otherwise
    d# 0 ['] arp-cmpip arp-foreach ;

: (arp-opcode) ( op-code -- ) \  op 1=request 2=reply 
    h# 806                    \  frame type
    d# 1                      \  hard type
    h# 800                    \  prot type
    pkt-buf-3,
    h# 0604 pkt-buf-w,          \  hard size, prot size
    pkt-buf-w,                 \ op-code
;

: arp-whohas ( ip -- len )
    pkt-buf-begin
    ethaddr-broadcast pkt-buf-3,
    eep-mac@ pkt-buf-3,
    1 (arp-opcode)
    eep-mac@ pkt-buf-3,
    net-my-ip pkt-buf-l,
    ethaddr-broadcast pkt-buf-3,
    pkt-buf-l,
    pkt-buf-end
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
        arp-whohas    \  d# 0
    else
        nip 2+          \  mac 
    then
;

\  If the current packet is an ARP request for our IP, answer it
: arp-responder ( arp-pkt-data -- len )
    \  is destination ff:ff:ff:ff:ff:ff or my mac
    arpv4>dmac dup net-eth-bcst?
    swap net-my-mac? or 
    
    \ respond if target is my IP.
    over arpv4>dip @ net-my-ip?  and
    if
        pkt-buf-begin
        dup arpv4>smac 6 pkt-buf-s,
        net-my-mac pkt-buf-3,
        2 (arp-opcode)
        net-my-mac pkt-buf-3,
        net-my-ip pkt-buf-l,
        dup arpv4>smac 6 pkt-buf-s,
        dup arpv4>sip @ pkt-buf-l,
        pkt-buf-end 
    then
;

: arp-announce ( -- len )
    pkt-buf-begin

    ethaddr-broadcast pkt-buf-3,
    net-my-mac pkt-buf-3,
    2 (arp-opcode)
    net-my-mac pkt-buf-3,
    net-my-ip pkt-buf-l,
    ethaddr-broadcast pkt-buf-3,
    net-my-ip pkt-buf-l,

    pkt-buf-end 
;

: arp-handler ( arp-pkt -- )
    arp>data dup arp-responder
    arp-response?
    if
        dup arpv4>sip pkt-buf-@ \ get src ip 
        arp-cache-find nip nip
        dup 0= if
            drop arp-oldest
        then
        arp-cache-write
    then
;

\ call natsemi-open to initialize ethernet controller
\ use ip# 192.168.1.250 net-my-ip! to set ip adress
\     ip# 255.255.255.0 ip-subnet-mask ! to set subnet mask
\     ip# 192.168.1.1   ip-router ! to set gateway
: test1 ns-tx-bufs @ h# 0a9fee60a arp-whohas transmit ;
: test2 ns-tx-bufs @ arp-announce transmit ;

