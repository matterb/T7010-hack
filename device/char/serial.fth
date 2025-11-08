\ serial.fth
\ wordset for serial port

[defined] system [if]
also system definitions
[then]

base @ hex
h# 3f8 value uart-base
\ 0 constant data
1 constant interrupt-enable
2 constant fifo-control
3 constant line-control
4 constant modem-control
5 constant line-status
6 constant modem-status

0 constant no
1 constant odd
3 constant even

h# 1c200 constant clock-frequency

: uart@ ( reg# -- byte )
 uart-base + pc@ 
;

: uart! ( byte reg# -- )
 uart-base + pc!
;

: divisor! ( n -- )
 line-control uart@ dup >r
 h# 80 or line-control uart!  \ enable dlab
 wbsplit 1 uart! 0 uart!
 r> line-control uart!
;

: divisor@ ( -- n)
 line-control uart@ dup >r
 h# 80 or line-control uart!
 0 uart@ 1 uart@ bwjoin
 r> line-control uart!
;
\ Integer division which rounds to nearest instead of truncating
: rounded-/  ( dividend divisor -- rounded-result )
 swap 2*  swap /  ( result*2 )
 dup 1 and +      \ add 1 to the result if it is odd
 2/               ( rounded-result )
;
: baud-to-divisor  ( baud-rate -- divisor )
  clock-frequency  swap rounded-/    ( baud-rate-divisor )
;
: divisor-to-baud  ( divisor -- baud-rate )
  clock-frequency  swap rounded-/    ( baud-rate )
;

: com-port ( n --) \ set com port
  h# 100 * h# 3f8  swap - to uart-base 
;

: rts-on ( -- )
 4 uart@ 2 or 4 uart!
;

: dtr-on   ( -- )
 4 uart@  1 or 4 uart!
;

: rts-off  ( -- )
 4 uart@  h# fd and  4 uart!
;

: dtr-off ( -- )
 4 uart@ h# fe and 4 uart!
;

: baud ( n -- )
 baud-to-divisor divisor! 
;

: stop-bits ( n -- )
 1- 2 lshift line-control uart@ h# fb and or
 line-control uart!
;

: parity ( n -- )
 3 lshift line-control uart@ h# e7 and or
 line-control uart!
;

: bits  ( n --)
 5 - lshift line-control uart@ h# fc and or
 line-control uart!
;

\ @@C usage: 8 bits no parity 1 stop-bits 57600 baud
  
: consume  ( -- )  0 uart@ drop  ;
\ Test for rcv character.  While consuming (discarding) break characters.
: u-key?    ( -- flag )
   line-status uart@  dup h# 10 and  if
   drop consume false exit  then   ( lstat )
   1 and  0<>
;
: u-emit?   ( -- flag )
   line-status uart@  h# 20 and  0<>  ;  \ Test for xmit ready

: u-key   ( -- char )
  begin  u-key?  pause   until  0 uart@  ;  \ Receive a character
: u-emit  ( char -- )
  begin  u-emit? pause until  0 uart!  ;  \ Transmit a character

[defined] system [if]
previous definitions
[then]


base !

