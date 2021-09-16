( UDP header and wrapup                      JCB 13:22 08/24/10)
begin-structure /udp
 wfield: udp>sport
 wfield: udp>dport
 wfield: udp>len
 wfield: udp>chksum
 cfield: udp>data
end-structure


: udp-header ( dst-port src-port dst-ip src-ip *ethaddr -- )
    h# 11 ip-header
    pkt-buf-w,              \  src port
    pkt-buf-w,              \  dst port
    d# 2 pkt-buf-,0        \  length and checksum
;

variable packetbase
: packet packetbase @ + ;

: (psuedo-chksum) ( sip dip prot len -- w ) 
 d# 16 rshift + + + lwsplit +
 ;

: (udp-chksum) ( addr u -- u)
   \ Compute Internet Checksum for u bytes
   \  beginning at location addr.
   \ odd byte ?
   \ test 00 01 f2 03 f4 f5 f6 f7  chksum=ddf2
   
   dup 1 and if
        over c@ 1- swap 1+ swap
    else
        0 -rot
    then
    bounds do
        i w@ +
    2 +loop
    begin           \ Fold 32-bit sum to 16 bits 
    lwsplit dup while
       + 
    repeat
    + invert
;


: udp-checksum? true ;
    \ incoming udp-checksum 0= ;

: udp-wrapup ( -- )
    pkt-buf-end dup
    ip-wrapup

    \ udp length and checksum calculation
;

