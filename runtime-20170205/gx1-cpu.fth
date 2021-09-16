\ @@FILE: gx1cpu.fth
\ @@REQUIRES:util.fth timer.fth
\  amd gx1 cpu words
\  system vocabulary

base @ hex

only forth also
system definitions

\ configuration index at 0x22 data at 0x23
: gx1-conf! ( val reg -- )  22 pc! 23 pc! ;
internal

: gx1-conf@ ( reg -- byte ) 22 pc! 23 pc@ ;

\ lock configuration registers 0xc3[4]
: gx1-lock ( -- ) c3 gx1-conf@ 7f and c3 gx1-conf! ;

\  unlock configuration registers 
: gx1-unlock ( -- ) c3 gx1-conf@ 10 or c3 gx1-conf! ;  

\ @@C write to gx1 register
: gx1! ( val reg -- ) gx1-unlock gx1-conf! gx1-lock ; 
external

\ @@C read gx1 register
: gx1@ ( reg -- val ) gx1-unlock gx1-conf@ gx1-lock ;

\ Graphics Control Register
\  GX_BASE defined in index 0xb8[1:0]

: (gx-base) 0b8 gx1@ 3 and 1e lshift ;

\ @@ gx-base# ( -- u) graphics base address
(gx-base) value gx-base#

\ @@C read dword graphics index register
: gx-base@ ( idx -- val ) gx-base# + @ ;

\ @@C write dword graphics index register
: gx-base! ( val idx -- ) gx-base# + ! ;

\ @@C read byte from graphics index register
: gx-base-c@ ( idx -- val ) gx-base# + c@ ;

\ @@C write byte to graphics index register
: gx-base-c! ( val idx  -- ) gx-base# + c! ;

\ Internal Bus Interface unit register
: ibir-base ( -- addr) gx-base# 8000 + ;
: gx1-ibir@ ( idx -- u ) ibir-base + @ ;
: gx1-ibir! ( u idx -- ) ibir-base + ! ;

\ Graphics Pipeline Control Registers
: gpr-base ( -- addr) gx-base# 8100 + ;
: gx1-gpr@ ( idx -- u) gpr-base + @ ;
: gx1-gpr! ( u idx -- ) gpr-base + ! ;
: gx1-gpr-w@ ( idx -- w) gpr-base + w@ ;
: gx1-gpr-w! ( w idx -- ) gpr-base + w! ;

\ Display controller registers
: dcr-base    ( -- adr )  gx-base# 8300 +  ;    
: gx1-dcr@  ( idx -- u )  dcr-base + @  ;
: gx1-dcr!  ( u idx -- )  dcr-base + !  ;

: gx1-dcr-unlock  ( -- ) 4758 0 gx1-dcr!  ;
: gx1-dcr-lock    ( -- )  0 0 gx1-dcr!  ;
: gx1-dcr-lock-push ( --  x) 0 gx1-dcr@ gx1-dcr-unlock ;
: gx1-dcr-lock-pop ( x -- ) 0 gx1-dcr! ;

\ Memory controller register
: mcr-base ( -- addr) gx-base# 8400 + ;
: gx1-mcr@ ( idx -- u ) mcr-base + @ ;
: gx1-mcr! ( u idx -- ) mcr-base + ! ;

\ Power management control register
: pmr-base ( -- addr) gx-base# 8500 + ;
: gx1-pmr@ ( idx -- u) pmr-base + @ ;
: gx1-pmr! ( u idx -- ) pmr-base + ! ;

: video-off ( -- ) 
   gx1-dcr-lock-push 
        08 gx1-dcr@ 20 invert and 08 gx1-dcr! 
   gx1-dcr-lock-pop  
;

: video-on  ( -- ) 
   gx1-dcr-lock-push
      08 gx1-dcr@ 20 or 08 gx1-dcr!
   gx1-dcr-lock-pop  
;

\ palette access
: gx1-palette@  ( -- u )  74 gx1-dcr@  ;
: gx1-palette!  ( u -- )  74 gx1-dcr!  ;
: gx1-palette-index!  ( index -- )  70 gx1-dcr!  ;
: gx1-palette-reg@ ( reg -- u ) gx1-palette-index! gx1-palette@ ;
: gx1-palette-reg! ( u reg -- u ) gx1-palette-index! gx1-palette! ;

\ graphics memory base address
\ index 8414[10:0] corresponds to address bits 29:19
8414 gx-base@ 13 lshift constant gbadd#
\ @@ gbadd# ( -- u) graphics memory base address

module 
also forth definitions
\ @@  tom ( -- u) returns top of memory 
8000 gx-base@ value tom

previous previous definitions

base !
