\ @@FILE:nvram.fth
\ @@REQUIRES:util.fth
\ nvram access words

base @ hex

: (port) ( addr -- addr ) 80 and if 72 else 70 then ;

internal

\ send command 
: (nvram-cmd) ( offset -- data-port )  dup (port) tuck pc! 1+ ;

: (in-update?) ( -- flg )  0a (nvram-cmd) pc@ 80 and 0<> ;

: nvram@ ( offset -- c) (nvram-cmd) pc@ ;  

external

: nvram! ( val offset -- ) (nvram-cmd) pc! ;

: bcd>bin ( c - n)
  dup 4 rshift 
  dup dup 3 lshift + +
  swap 0f and +
;

: bin>bcd ( n - c)  d# 10 /mod 4 lshift or ;

: rtc-date ( ---- s m h d m y )
  begin (in-update?) 0= until \ wait for end of update
  00 nvram@ bcd>bin
  02 nvram@ bcd>bin
  04 nvram@ bcd>bin
  07 nvram@
  08 nvram@
  09 nvram@
;
  

module

base !


