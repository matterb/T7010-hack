\ @@FILE:timer.fth
\ define timer words ala FD
\ use tsc or pit
\  part of system
\ requires : 

only forth also definitions

base @ hex

: cpuspeed ( --   cpu_speed )
\  uses PIT, enables T2, disables speaker
   61 pc@ fd and 1 or 61 pc!
\ mode 0 counter 2
   b0 43 pc!
\ maximum count
   ff 42 pc!
   ff 42 pc!
\  get ticks 
   rdtsc
\ wait for counter to run down
   begin  61 pc@ 20 and until
   rdtsc
   2swap d- drop   ( convert to single )
   1234dc 3e8  ffff * */
;

internal

\ internal applies to previous word up to external

\ @@C delay for 2cell clock cycles
: tdelay ( d1 --  ) 
\ convert to code word
   rdtsc d+
   begin rdtsc 2over d- drop 0> until 
   2drop 
;

cpuspeed constant cpukhz#

external
\ applies to previous word till module


\ @@C delay u nanoseconds
: ndelay ( u -- )  cpukhz# d# 1000000 */ 0 tdelay ;

\ @@C delay for u microseconds
: udelay ( u -- ) cpukhz# d# 1000 */ 0 tdelay ;

\ @@C delay for u milliseconds
: ms ( u -- )    cpukhz# * 0 tdelay ;

\ @@C delay for u seconds
: delay ( u  -- )  begin d# 1000 ms 1- dup 0= until drop ;

\ @@C get millisecond count from tsc
: get-ms ( -- ms)  rdtsc cpukhz# um/mod nip ;

: set-sys-phase ( hz -- )
 1234dc swap /
 36 43 pc!
 dup ff and 40 pc!
 8 rshift 40 pc!
;

.( default system rate is 100 Hz)
64 set-sys-phase

module

base !

