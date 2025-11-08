\ file probe.fth
\ purpose: OHCI USB Controller probe
\ See license at end of file

hex
\ headers
\ device enumeration
\ 1.  port stabilization after power up
\     - no port connect changes for at least 100 ms
\ 2.  reset port - timeout  of 5 sec. 3 retries with 500 ms delay
\ 3.  device descriptor request #1
\ 4.  reset port again
\ 5.  set address 
\ 6.  device descriptor request
\ 7.  configuration descriptor request
\ 8.  other descriptor request....
\ 9.  add to device list

: ohci-pci ( -- )   \ make device pci bus master
    0 13 0 pci-dev 4 over pci-w@ 4 or 4 rot pci-w!
;

: probe-root-hub-port  ( port -- )  
   debug? if ." probe-root-hub-port" cr then
   dup disable-old-nodes                ( port )
   dup hc-rh-psta@ 1 and 0=  if  drop ." ---exiting" cr exit  then    ( port )

   \ Reset the port to determine the speed
   dup reset-port                   ( port )
   dup hc-rh-psta@ 200 and if speed-low else  speed-full then   ( port speed )

   \ hub-port and hub-speed are irrelevant for OHCI (USB 1.1)
   0 0                      ( port speed hub-port hub-dev )

   \ Execute setup-new-node in root context and make-device-node in hub node context
   setup-new-node  if  execute  then        ( port dev xt )
;

false value ports-powered?

\ number of root hub ports 
: #ports  ( -- n )  hc-rh-desA@ h# ff and  ;


: power-ports  ( -- )
   hc-rh-desA@  dup h# 200  and  0=  if
      \ ports are power switched
      hc-rh-stat@ h# 10000 or hc-rh-stat!  \ power all ports
      hc-rh-desB@ d# 17 rshift over h# ff and 0  ?do
         dup 1 i lshift and  if
            i hc-rh-psta@  h# 100 or i hc-rh-psta!  \ power port
         then
      loop  drop
   then  drop
   potpgt 2* ms         \ Wait until powergood

   \ Setup PowerOnToPowerGoodTime and OverCurrentProtectionMode
   hc-rh-desA@  h# 00ffffff and  potpgt d# 24 lshift or
   h# 800 or   hc-rh-desA!  \ per-port over-current status

   true to ports-powered?
;


: probe-root-hub  ( -- )
   debug? if ." probe-root-hub [" .s cr then
   \ Power on ports
   ports-powered? 0=  if ." -powering ports" cr power-ports  then
   0 hc-rh-psta@ h# 100 and  if
        500 ms          \ delay for port turn-up
   else power-ports then

   alloc-pkt-buf
   #ports 0  ?do
      i hc-rh-psta@ 30000 and  if
i probe-root-hub-port 
\        i rm-obsolete-children         \ Remove obsolete device nodes
\         i ['] probe-root-hub-port catch  if
\                drop ." Failed to probe root port " i u. cr
\         then
         30000 i hc-rh-psta!           \ Clear change bits
      else
         i port-is-hub?  if     ( phandle )     \ Already-connected hub?
            reprobe-hub-node                    \ Check for changes on its ports
         then
      then     
   loop
   free-pkt-buf
;

: usb-open  ( -- flag )
  open-count 0=  if
    first-open?  if
        false to first-open?
        ohci-pci        \ device is pci master
        reset-usb
        init-struct     \ device descriptors, ed and tds
        init-usb
      then
      alloc-dma-buf
   then
   probe-root-hub       \ find devices on root hub
   open-count 1+ to open-count
   true
;

: usb-close  ( -- )   
   open-count 1- to open-count
   end-extra
   open-count 0=  if
        free-dma-buf
        hcca-unaligned ?dup if /hcca dma-free 0 to hcca then
        di ?dup if /di dma-free 0 to di then
    then
;

: .hc-regs ( -- )
    cr ." HcRegisters:" cr
    h# 5c 0 do
        i dup usb@ swap
        hex. hex. cr
    4 +loop
;
 


only forth definitions

\ LICENSE_BEGIN
\ Copyright (c) 2006 FirmWorks
\ 
\ Permission is hereby granted, free of charge, to any person obtaining
\ a copy of this software and associated documentation files (the
\ "Software"), to deal in the Software without restriction, including
\ without limitation the rights to use, copy, modify, merge, publish,
\ distribute, sublicense, and/or sell copies of the Software, and to
\ permit persons to whom the Software is furnished to do so, subject to
\ the following conditions:
\ 
\ The above copyright notice and this permission notice shall be
\ included in all copies or substantial portions of the Software.
\ 
\ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
\ EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
\ MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
\ NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
\ LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
\ OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
\ WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
\
\ LICENSE_END
