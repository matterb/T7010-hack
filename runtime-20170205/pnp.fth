\ @@FILE:pnp.fth 
\ plug and play words
\ control register at 0x2e
\ data register at 0x2f
\ device 0:kbc 1:mouse 2:rtc 3:fdc
\        4:pport  5:uart2 6:uart1 7:gpio
\        8:power 
\  
base @ hex

only forth also
system  definitions

\ write val to pnp register
: pnp! ( val reg -- ) 2e pc!  2f pc!  ;

\ read pnp register
: pnp@ ( reg -- x) 2e pc!  2f pc@ ;

\ set logical device
: pnpldn! ( ldn -- ) 7 pnp!  ;

\ ldn 8 io address   Power management
  8 pnpldn!
  60 pnp@ 8 lshift 61 pnp@ or constant pnppm# 

: pnppm@ ( reg -- val ) pnppm# pc!  pnppm# 1+ pc@ ;

: pnppm! ( val reg -- ) pnppm# pc!  pnppm# 1+ pc! ;


\ ldn 7 io address   gpio 
 7 pnpldn!
 60 pnp@ 8 lshift 61 pnp@ or constant pnpgpio# 

: pnpgpio@ ( reg -- val ) pnpgpio# + pc@ ; 
 
: pnpgpio! ( val reg -- ) pnpgpio# + pc! ;

\ select gpio bank 0
: gpio0 ( -- ) 22 pnp@ 7f and 22 pnp! ;

\ select gpio bank 1
: gpio1 ( --) 22 pnp@ 80 or 22 pnp! ;

base !


previous definitions

