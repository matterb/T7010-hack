\ @@FILE:pci.fth
\ 
\ depends on NONE
\ from coreboot tc7010 devices
\          f d b  
\ video at 4 12 0   
\ audio    3 12 0
\ ide      2 12 0
\ smi      1 12 0
\ usb      0 13 0
\ eth      0 15 0
\ index 10 has base address register: bar

base @ hex

only forth also
system definitions

00 constant VENDOR#   internal 
04 constant COMMAND#
0A constant CLASS#
0E constant HEADERTYPE# 
18 constant PRIMARYBUS#
2C constant SUBSYSVENDOR#
2E constant SUBSYS#

0 constant NORMAL#
1 constant BRIDGE#
2 constant CARDBUS#  

: pci-dev  ( bus dev func -- pci-dev )
   8 lshift  80000000 or   ( fn# << 8)
   swap 0b lshift  or  ( dev# << 0b)
   swap 10 lshift or  ( bus# << 10  )
;     

external


: pci-addr ( reg bus dev func -- int )
   pci-dev 
   3 invert and or ( reg# & ~3)
;

: pci-bus ( pci-dev -- bus) \ extract bus# from pci handle
  10 rshift ff and 
;

: pci-slot ( pci-dev -- dev) \   extract slot# from pci handle
  0b rshift 1f and 
;

: pci-func ( x1 -- func) \   extract func# from pci handle 
  8 rshift 07 and 
;

: pci-config-reg ( reg pci-dev  -- pciadr ) \ set config reg#
\ common factor for pci-x@
  swap 3 invert and or 0cf8 p!   
  0cfc 
;

: pci-c@ ( reg pci-dev   -- byte   )
 pci-config-reg   ( pcicdr# )
 pc@    
;

: pci-w@ ( reg pci-dev   -- word   )
 pci-config-reg 
 pw@
;


: pci@ (  reg pci-dev   -- dword   )
 pci-config-reg
 p@
;

: pci-c! ( val reg pci-dev  --   set pci reg )
 pci-config-reg   ( val pcicdr# )
 pc!    
;

: pci-w! ( val reg  pci-dev  --   set pci reg ) 
 pci-config-reg 
 pw!
;


: pci! (  val reg pci-dev  --   set pci reg ) 
 pci-config-reg
 p!
;


: pci-find-dev-on-bus ( dev ven bus -- x4 0|-1  id# ven# bus#  pci-dev flag )
   drop
;

: pci-read-res ( bar pci-dev  --    return ? )
 swap 2 lshift 10 + swap
 pci@
 ;


: pcisetbusmaster ( x1 --  set pci-dev as master )
  COMMAND# over pci-w@   ( bus w )
  COMMAND# or
  COMMAND# swap rot pci-w!
 ;


\  define base address registers for the device
10 0 15 0 pci-dev pci@ f invert and constant eth-io#  \ eth-io
14 0 15 0 pci-dev pci@ constant eth-bar#  \ eth
10 0 13 0 pci-dev pci@ constant usb-bar#  \ usb
10 0 12 4 pci-dev pci@ constant f4-bar#   \ video
10 0 12 3 pci-dev pci@ constant f3-bar#   \ audio
10 0 12 2 pci-dev pci@ constant f2-bar#   \ ide
20 0 12 2 pci-dev pci@ constant f2-io#   \ ide-io
10 0 12 1 pci-dev pci@ constant f1-bar#   \ smi
   0 12 0 pci-dev constant      f0-index  \ bridge

\ eth io register
: eth-io@ ( reg -- x) eth-io# + p@ ;
: eth-io! ( val reg -- ) eth-io# + p! ;

\ eth function register
: eth@ ( reg -- x) eth-bar# + @ ;
: eth! ( val reg -- ) eth-bar# + ! ;

\ usb function register
: usb@  ( reg -- x) usb-bar# + @ ;
: usb!  ( val reg -- ) usb-bar# + ! ;

\ read vga function registers
: f4@  ( reg -- x ) f4-bar# + @ ;
: f4! ( val reg  -- ) f4-bar# + ! ;

\ audio
: f3@  ( reg -- x ) f3-bar# + @ ;
: f3! ( val reg  -- ) f3-bar# + ! ;

\ ide 
: f2@  ( reg -- x ) f2-bar# + @ ;
: f2! ( val reg  -- ) f2-bar# + ! ;

\ ide io 
: f2-io@  ( reg -- x ) f2-io# + p@ ;
: f2-io! ( val reg  -- ) f2-io# + p! ;

\ pci bridge
: f0@ ( reg -- x) f0-index pci@ ;
: f0-c@ ( reg -- c ) f0-index pci-c@ ;
: f0-w@ ( reg -- w ) f0-index pci-w@ ;
: f0! ( val reg -- ) f0-index pc! ;
: f0-c! ( c reg -- ) f0-index pci-c! ;
: f0-w! ( w reg -- ) f0-index pci-w! ;

module 

previous definitions

base !

