( IP networking: headers and wrapup         JCB 13:21 08/24/10)
( version 0.0)
( natsemi.fth, arp.fth)
begin-structure /ipv4
 cfield:   ipv4>version
 wfield:   ipv4>tos
 wfield:   ipv4>len
 wfield:   ipv4>id
 wfield:   ipv4>offset
 cfield:   ipv4>ttl
 cfield:   ipv4>proto
 wfield:   ipv4>chksum
  field:   ipv4>sip
  field:   ipv4>dip
 cfield:   ipv4>data
end-structure

variable ip-id-counter

\ length of current IP packet in words
: ip-datalength ( ip-pkt -- u )  ipv4>len pkt-buf-w@ 2/ ;

\ true if packet PROTO is u
: ip-proto? ( u ipv4-pkt -- flg ) ipv4>proto pkt-buf-c@ h# ff and = ;

: ip-identification ( -- w ) ip-id-counter d# 1 over +! w@ ;

: @ethaddr ( eth-addr -- mac01 mac23 mac45 )
    ?dup
    if
        dup w@ swap 2+ dup w@ swap 2+ w@
    else
        ethaddr-broadcast
    then
;

: ip-header ( dst-ip src-ip eth-addr protocol -- )
    >r
    pkt-buf-begin

    @ethaddr pkt-buf-3,
    net-my-mac pkt-buf-3,
    h# 800 pkt-buf-w,

    h# 4500
    h# 0000                  \  length
    ip-identification
    pkt-buf-3,
    h# 4000                  \  do not fragment
    h# 4000 r> or            \  TTL, protocol
    d# 0                     \  checksum
    pkt-buf-3,
    pkt-buf-l,              \  src ip
    pkt-buf-l,              \  dst ip
;

: ip-wrapup ( bytelen -- )
    \  write IP length
    cur-pkt @ eth>data -
    cur-pkt @ eth>data ipv4>len pkt-buf-w!

    \  write IP checksum
    cur-pkt @ eth>data dup d# 10 pkt-buf-chksum
    swap ipv4>chksum pkt-buf-w!
;

: ip-packet-srcip ( ip-pkt -- u ) ipv4>sip pkt-buf-@ ;

( ICMP return and originate                  JCB 13:22 08/24/10)

begin-structure /icmp
 cfield:    icmp>type
 cfield:    icmp>code
 wfield:    icmp>chksum
 wfield:    icmp>id
 wfield:    icmp>seq
end-structure


: icmp-id ( addr -- addr )  ;
\  Someone pings us, generate a return packet

: icmp-handler ( ipv4-pkt -- )   
    1 ( ICMP PRROTOCOL) over ip-proto?
    over ipv4>data icmp>type pkt-buf-c@ h# 8 =
    and if
        ip-packet-srcip
        dup arp-lookup
        ?dup if
            \  transmit ICMP reply
                                    \  dstip *ethaddr
            net-my-ip rot           \  dstip srcip *ethaddr
            d# 1 ip-header

            \  Now the ICMP header   \ echo reply
            d# 0 pkt-buf-c,

            s" =====> ICMP seq " type
            cur-pkt @ eth>data ipv4>id pkt-buf-w@ u. cr

            cur-pkt @ eth>data ipv4>data ( icmp>data) icmp-id 
            ip-datalength 2-        ( offset n )
            tuck
            pkt-buf-chksum pkt-buf-w,
            swap ipv4>data icmp-id  pkt-buf-src

            pkt-buf-end
            ip-wrapup
        else
            2drop
        then
        drop
    then
;
    
: ping ( ip -- ) \ originate
    dup arp-lookup
    ?dup if
        \  transmit ICMP request
                                \  dstip *ethaddr
        net-my-ip rot           \  dstip srcip *ethaddr
        d# 1 ip-header

        \  Now the ICMP header
        h# 800 pkt-buf-w,

        \  id is h# 550b, seq is lo word of time
        h# 550b rdtsc drop
        2dup +1c h# 800 +1c
        d# 28 begin swap d# 0 +1c swap 1- dup 0= until drop
        invert pkt-buf-w,     \  checksum
        pkt-buf-2,
        d# 28 pkt-buf-,0

        pkt-buf-end
        ip-wrapup
    else
        drop
    then
;

( UDP header and wrapup                      JCB 13:22 08/24/10)

begin-structure /udp
 wfield:    udp>src-port
 wfield:    udp>dst-port
 wfield:    udp>len
 wfield:    udp>chksum
end-structure

: udp-header ( dst-port src-port dst-ip src-ip *ethaddr -- )
    h# 11 ip-header      \ chksm len dst src
    pkt-buf-w,           \  src port
    pkt-buf-w,           \  dst port
    d# 2 pkt-buf-,0      \  length and checksum
;

variable packetbase
: packet ( u -- u') packetbase @ + ;
0 [if]
: udp-checksum ( addr -- u ) \ compute UDP checksum on packet
    packetbase !
    packetbase udp>len @ d# 1 and if
        ETH.IP.UDP ETH.IP.UDP.LENGTH packet @ + packet
        dup @ h# ff00 and swap !
    then
    ETH.IP.UDP packet
    ETH.IP.UDP.LENGTH packet @ 1+ 2/
    enc-checksum invert
    d# 4 ETH.IP.SRCIP packet enc@n
    +1c +1c +1c +1c
    IP_PROTO_UDP +1c
    ETH.IP.UDP.LENGTH packet @ +1c
    invert
;


: udp-checksum? incoming udp-checksum 0= ;

: udp-wrapup (    ) 
    pkt-buf-end dup  
    ip-wrapup

    OFFSET_UDP -
    OFFSET_UDP_LENGTH packetout-off enc!

    outgoing udp-checksum ETH.IP.UDP.CHECKSUM packetout-off !
;

[then]
