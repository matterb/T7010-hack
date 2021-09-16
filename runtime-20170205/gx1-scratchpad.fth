\ @@FILE:gx1-scratchpad.fth
\ @@REQUIRES:asm.fth, util.fth, startup.fth
\ scratchpad initialization 
\ author Juergen ? in a COREBOOT mail thread

only forth also
system definitions

base @ hex

code invalidate-cache ( -- ) invd ;code

code disable-cache ( -- )
   wbinvd
   0 cr ax mov
   gx-base# # ax or     \ gx-base# = 40000000
   ax 0 cr  mov
   wbinvd
;code

code  enable-cache ( -- ) 
  0 cr ax mov
  9fffffff # ax and
  ax 0 cr mov
;code

code  setup-fill-buffer ( u -- )
\ this data will be cacheline's content after mapping
  0 # ax mov  ax 5 tr mov 
  bx 3 tr mov
  4 # ax mov  ax 5 tr mov
  bx 3 tr mov
  8 # ax mov  ax 5 tr mov 
  bx 3 tr mov 
  0c # ax mov ax 5 tr mov 
  bx 3 tr mov 
  bx pop
;code 

code  map-cache-line ( u -- )
\ map cacheline of set 0 to physical address u
\ only set 0 is used as scratchpad ram
  bx ax mov 
  0fffff000 # ax and 400 # ax  or
  ax 4 tr mov
  bx ax mov 
  00ff0 # ax and  1 # ax  or
  ax 5 tr mov
  bx pop
;code

\ gx1 cpu internal L1 cache BLT registers
\  ebx = register address  eax = data
\ cpu-read
\ cpu-write
\ base ffffff0c base address bb0 
\      ffffff1c              bb1
\ pointer   ffffff2c reset pointer  bb0
\           ffffff3c                bb1

\ pm_base   ffffff6c power management
\ pm_mask   ffffff7c

code gx1-bbxbase@ ( 0|1 -- w)
    1 # bx and
    bx 4 # shl
    h# ffffff0c # bx or
    cpu_read
    ax bx mov
;code

code gx1-bbxbase! ( w 0|1 --)
    1 # bx and
    bx 4 # shl
    h# ffffff0c # bx or
    ax pop
    cpu_write
    bx pop
;code

: set-scratchpad-size  ( u -- )
\ u 2048,3072,4096kb   1024 disables cache
 1000 min 400 max        \ size is between 1k and 4k
 lock[  0c3 gx1@    \ ccr3
 tuck 0f and 10 or 0c3 gx1!  \ enable MAPEN
 8 rshift 4 - 
 0b8 gx1@ 3 and or   \ GCR@ 
 0b8 gx1! 0c3 gx1!
 ]unlock
;

: init-scratchpad ( u -- )
\ initialize scratchpad size  u = 2048,3072,4096
\ base address from gcr 0xb8[1:0]   0= disabled 
 1000 over - gx-base# +         \ base address of buffers in L1 cache 
 disable-cache invalidate-cache
 \ free whole cache and invalidate it
 400 set-scratchpad-size invalidate-cache  \ 400 set cache size to 0
\ remap the cache lines again
 over 0 do
   h# deadbeef setup-fill-buffer
   dup map-cache-line 10 +
   10 +loop
 drop
\ freeze mapped cache lines and enable remainder for normal use
 set-scratchpad-size
 enable-cache
\ set scratchpad base registers   leave 0x2c0  for user? 
   0 0 gx1-bbxbase!
 640 1 gx1-bbxbase!   
;

base !
previous definitions

