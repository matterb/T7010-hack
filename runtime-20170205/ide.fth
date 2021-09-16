\ @@FILE:ide.fth
\ @@REQUIRES:
\ lifted from forthOS
\ Block I/O routines for IDE
\ depends on NONE
\ TODO timeout for wait routines
base @ decimal

only forth also
system definitions

defer (rdwt)

1024 constant #blk-size
512  constant #sec-size
#sec-size 2/ constant #sec-words
#blk-size #sec-size / constant secs/blk
 
\ ide unit for current operation
variable ide-unit 0 ide-unit !
 
\ offset into disk to address active partition
variable ide-offset

\ i/o base ports
h# 1f0 constant ide-base   
h# 206 constant ide-ctlr
 
\ ide controller ports
0 constant ide_data     1 constant ide_error    2 constant ide_scnt
3 constant ide_snum     4 constant ide_cyl0     5 constant ide_cyl1
6 constant ide_sdh      7 constant ide-status   7 constant ide_cmd
 
\ ide status bits
h# 01 constant ides_error h# 04 constant ides_ecc
h# 08 constant ides_drq   h# 80 constant ides_busy
 
\ ide mode of operation--lba/ibm
h# 0e0 constant ide_mode
 
\ ide commands
h# 10  constant idecmd-calibrate h# 20 constant idecmd-read
h# 30  constant idecmd-write     h# 40 constant idecmd-verify
h# 50  constant idecmd-format    h# 70 constant idecmd-seek
h# 90  constant idecmd-diag      h# 91 constant idecmd-set-drive-parms
h# 0ec constant idecmd-identify

 
\ return match of ide status bit
: ?ide-stat-check ( n -- bool ) ide-base ide-status + pc@ and 0= invert ;
 
\ tell if controller indicates it's busy with a command
: ?ide-busy ( -- bool ) ides_busy ?ide-stat-check ;
 
\ tell if controller has a data request
: ?ide-drq   ( -- bool ) ides_drq ?ide-stat-check ;

\ block i/o into buffer
: repinsw ( a port count -- )
    2* rot swap over + swap do dup pw@ i w! 2 +loop drop ;

: repoutsw ( a port count -- )
    2* rot swap over + swap do i w@ over pw! 2 +loop drop ;
 
\ wait-drq wait for ide drq to indicate "ready", pause'ing in between
\ add timeout feature
: wait-drq ( -- ) begin ?ide-drq 0= while pause repeat ;

\ wait-busy   wait for ide busy flag to clear , pausing in between
\ add time out feature

: wait-busy ( -- )  begin ?ide-busy while pause repeat ;

\ ide-io actual operation of ide controller
\    wait for drive ready--this can happen unexepectedly
\     due to power management spinning down the drive.
\    i/o size--always a full block
\    program block #, using lba addressing
\    send command
\   for write command, send data as controller asks
\   for read command, pull data as data available
\   drop buffer address... all done!
 
: ide-io ( a blk rw -- )
   wait-busy
   secs/blk  ide-base ide_scnt +  pc!
   -rot dup  ide-base ide_snum +  pc!
   dup 8 rshift  ide-base ide_cyl0 + pc!
   dup 16 rshift ide-base ide_cyl1 + pc!
   24 rshift ide_mode or ide-unit @ 4 lshift or
   ide-base ide_sdh +   pc!
   swap dup   ide-base ide_cmd +   pc!
   idecmd-write = if
        secs/blk 0 do
            wait-drq
            dup ide-base ide_data + #sec-words repoutsw
        #sec-size + loop
   else
      secs/blk 0 do   wait-drq
         dup ide-base ide_data +   #sec-words repinsw
      #sec-size + loop
   then
   drop 
;

\ get block pointer, doing i/o if needed
: ide-rdwt ( a blk rw -- err )
   \ convert block number to sector index
   swap secs/blk * ide-offset @ + swap
 
   \ convert r/w argument to ide read or write command; 0 means write,
   \  any other value means read
   if idecmd-read else idecmd-write then
   ide-io 0
;

\ disk partition parsing
 
\ # of fdisk entries in sector 0 of a disk
4 constant nfdisk 

\ partition type for forthos storage
158 constant pt_forthos

\ words to access the needed fields in a partition array of partitions
: sec>parts ( a -- a' )   446 +  ;
 
: part>type ( a -- a' ) 4 + ; \  partition type 

: part>start ( a -- a' )  2 cells + ;  \   starting sector # 
: part>sectors ( a -- a') 3 cells + ;   \ partition size in sectors

\   overall size of one partition entry
16 constant /partition 

\ tell if magic # is ok for partition sector
: partok? ( a -- ? )  510 + @   h# 0ffff and h# 0aa55 = ;
 
\  read fdisk label, find our partition and set ide-offset to
\  make this the start of our blocks.  if can't find partition, clear
\  ide-offset to make "block" see the whole raw disk.
: init-ideoff ( -- )
   0 ide-offset !
   align here   dup 0 idecmd-read ide-io ( a-sec0 )
   dup partok? 0= if  drop exit  then
   sec>parts   nfdisk 0 do
        dup part>type c@ pt_forthos = if
            part>start @   ide-offset !   unloop exit
        then
        /partition +
   loop drop
;
 
\ disk startup/initialization  
: boot-ide ( bool -- n | )
if
    ['] ide-rdwt is (rdwt)
    init-ideoff
else 1000 then
;
 
previous definitions
 
base ! 
