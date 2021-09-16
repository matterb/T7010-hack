\ dwnldr.fth
\ Simple downloader
\  Redirect keyv to read from memory buffer.
\ normal quit proceeds when buffer is exhausted,
\ restores previously saved keyv. 
\ Incomplete compilation may hang ( quit? )
\ ixfer xfer
 
base @ hex 
only forth definitons
also system

\ start of buffer
variable sob   internal
variable eob    \ end of buffer
80000 constant /xfer-buffer  \ buffer size  

\ create buffer /buffer allot
tom /xfer-buffer - to tom
tom constant xfer-buffer
xfer-buffer /xfer-buffer 1- 13 fill

\ save area for key execution vector
variable keyvsave

\ redirect key reads to buffer.
\ resets on exhaustion of buffer
: keym ( -- ch)
sob @ eob @ < if
   sob @ dup c@
   swap 1+ sob !
else
   keyvsave @ dup keyv !
   execute 
then
;

\  redirect key to fetch from buffer 
: load ( addr len -- )
over + eob ! sob !
keyv @ keyvsave !
['] keym keyv !
;

\  add input to buffer until EOI (ctrl-Z) is seen
: rcv  ( addr -- addr len)
 0 over begin
    key dup 4 <> while
    dup emit over c! 1+ 
    swap 1+ swap
 repeat
 2drop
;

: fill-buffer ( -- )  xfer-buffer rcv load ;

\ intel hex record loader.
\ address and checksum are ignored

: getc ( -- c) key dup 3 = if drop abort then ;

\ convert and emit 
: ce ( char -- ) dup a < if 30 else 37 then + emit ;
: 2.r ( n -- )   ff and 10 /mod ce ce ;
: 4.r ( n -- )   0 100 um/mod 2.r 2.r ;

: 1# ( -- c )  getc 30 - dup 9 > if 7 - then 0 max f min ;
: 2# ( -- c )  1# 4 lshift  1# + ;
: 4# ( -- u )  2# 8 lshift 2# or ;

\ clear input

: drain ( -- )  begin key? while key drop repeat ;

variable chksum

: ihex< ( addr -- )
   cr ." ready to load, offset = " dup u. cr
   begin
      getc 3a = if         \ ":"
         2# over +         \ sa ea
         4# drop           \ ignore address
         2# 1 = if 2drop drain exit then
         tuck swap do
            2#  i c!
         loop
         [char] . emit
         2# drop          \ ignore chksum`
      then
   again 
;
external


: ihex> ( addr n -- )
   over + swap                  \ bounds
   begin                        \
      cr                        \
      2dup 20 + min             \ next line of output up to 32 bytes long
      swap                      \ start and end
      ." :"                     \ begin the record
      2dup -                    \ find out # of bytes in this record
      dup chksum !              \ begin chksum computation
      2.r                       \ # bytes, in two digit field
      dup 8 lshift ff and       \
      over ff and + chksum +!   \ add start address to chksum
      dup 4.r                   \ start address, four digit field
      ." 00"                    \ record type
      >r dup r>                 \ make start stop #s for do loop
      do                        \
        i c@ dup 2.r            \ hex byte, two digit field
        chksum +!               \ update chksum
      loop                      \
      chksum 1+ c@ negate 2.r   \ checksum
   2dup = until                 \ end
   cr ." :00000001FF" cr        \ tack on end record
   2drop 
;


\ load intel hex to address 
: ixfer ( addr -- )
  cr cr ." Send intel hex data..." cr    
  fill-buffer ihex<
;

:  xfer ( -- )
  cr cr ." Send ascii data..." cr    
  fill-buffer 
;

module

base !



