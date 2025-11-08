\ Parallel port routines
\ control c0-c3:1,14,16,17
\ status   s3-s7:15,13,12,10,11
\ data   d0-d7:2-9
\ gnd  18-25
\ 

only forth also system definitions

base @ hex


378 value pp-data
379 value pp-status
37a value pp-control

\ parallel port pin routines
: pin-# ( port bit --  C: "name" )
\ defining word for port pins
  create c, , 
  does> dup c@ swap 1+ @ ( bit port )
;
        
 pp-control 0 pin-# pin-01   \ c0, strobe,serial data in
 pp-data    0 pin-# pin-02   \ d0,
 pp-data    1 pin-# pin-03   \ d1,
 pp-data    2 pin-# pin-04   \ d2
 pp-data    3 pin-# pin-05   \ d3, serial clk,
 pp-data    4 pin-# pin-06   \ d4
 pp-data    5 pin-# pin-07   \ d5
 pp-data    6 pin-# pin-08   \ d6
 pp-data    7 pin-# pin-09   \ d7
 pp-status  6 pin-# pin-10   \ s6,ack 
 pp-status  7 pin-# pin-11   \ s7,busy
 pp-status  5 pin-# pin-12   \ s5,paper-end
 pp-status  4 pin-# pin-13   \ s4,select-out
 pp-control 1 pin-# pin-14   \ c1,auto-feed
 pp-status  3 pin-# pin-15   \ s3,error
 pp-control 2 pin-# pin-16   \ c2,init
 pp-control 3 pin-# pin-17   \ c3,select-in

: pin-set ( bit port -- ) \  set pin high
 tuck pc@
 1 rot lshift or
 swap pc!
;

: pin-clr ( bit port -- ) \ set pin low
 tuck pc@
 1 rot lshift invert and
 swap pc!
;

: pin-@ ( bit port -- bit ) \ fetch pin status
 pc@ swap rshift 1 and 
;

: pport-set ( port -- ) \ set port IO
 dup to pp-data 1+
 dup to pp-status  1+ to pp-control
;

previous definitions

base !

