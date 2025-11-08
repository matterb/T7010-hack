\ file: preamble.fth
\ purpose: define words absent from camelforth
\ only forth definitions
[defined] system [if]  also system 
    vocabulary usbdefs
[then]

: 2+ 2 + ;
: >= < 0= ;
: push-hex r> base @ >r >r hex ;
: pop-base r> r> base ! >r ;
: noop (null) ;
 [undefined] ms [if] : ms drop ; [then]
: alloc-mem ( size -- addr )  allocate throw  ;
: free-mem ( address size -- )  drop free throw ;

\ -----------------------------------------------------

[defined] usbdefs [if]
    also usbdefs definitions
[then]

 
\ ================need definitions ======================
: my-address 0 ;    \ interface
: my-self ;
: find-method ;
: push-package ;
: pop-package ;
: new-instance ;
: destroy-instance ;
: set-default-unit ;
: behavior ( xt1 -- xt2) defer@ ;    \ get the action-of a deferred word
: property type type cr ;
: get-my-property ;
: get-package-property ;
: get-inherited-property ;
: new-device ." new-device" cr ;
: set-args ( arg$ reg$ -- ) 2drop ;
: encode-string ;
: encode-bytes ;
: encode-phys ;
: encode-int ( x .. addr len ) here swap , cell ; \ encoded in dictionary
: decode-int ( addr len -- addr' len' n) over @ >r cell /string r>  ;
: decode-string ;
: encode+ ;
: ihandle>phandle ;
: child ;
: peer ;
: parse-2int ( addr len -- u1 u2) ;

\ ======================= end need defitions ===========================

: $hold  ( adr len -- )
   dup  if  bounds swap 1-
   ?do  i c@ hold  -1 +loop  else  2drop  then
;
: u#>  #> ;
: u#s  #s ;
: usb#>   ( n -- )  s" usb" $hold  0 u#> ;     \ Prepends: usb
: #usb#>  ( n -- )  u#s drop  usb#>  ;        \ Prepends: usbN
: #,      ( n -- )  u#s drop [char] , hold  ;  \ Prepends: ,N
: #.      ( n -- )  u#s drop [char] . hold  ;  \ Prepends: .N
: dma-push     ( virt phys size -- )  2drop drop ;
: dma-pull     ( virt phys size -- )  2drop drop ;
: dma-alloc    ( size -- virt ) allocate  throw ;
: dma-free     ( virt size -- ) drop free throw ;
: dma-map-in   ( virt size cache? -- phys )  2drop  ;
: dma-map-out  ( virt phys size -- ) 2drop drop ;

d# 256 constant /align256
d#  16 constant /align16
d#  32 constant /align32
: aligned-alloc  ( size align -- unaligned-virt aligned-virtual )
   dup >r + dma-alloc  dup r> round-up
;
: aligned-free  ( virtual size align -- )  + dma-free  ;

: aligned16-alloc  ( size -- unaligned-virt aligned-virtual )
   /align16 aligned-alloc
;
: aligned16-free  ( virtual size -- )
   /align16 aligned-free
;

: aligned16-alloc-map-in  ( size -- unaligned-virt aligned-virt phys )
   dup >r aligned16-alloc
   dup r> true dma-map-in
;
: aligned16-free-map-out  ( unaligned virt phys size -- )
   dup >r dma-map-out
   r> aligned16-free
;

: aligned32-alloc  ( size -- unaligned-virt aligned-virtual )
   /align32 aligned-alloc
;
: aligned32-free  ( virtual size -- )
   /align32 aligned-free
;

: aligned32-alloc-map-in  ( size -- unaligned-virt aligned-virt phys )
   dup >r aligned32-alloc
   dup r> true dma-map-in
;
: aligned32-free-map-out  ( unaligned virt phys size -- )
   dup >r dma-map-out
   r> aligned32-free
;

: aligned256-alloc  ( size -- unaligned-virt aligned-virtual )
   /align256 aligned-alloc
;
: aligned256-free  ( virtual size -- )
   /align256 aligned-free
;

[undefined] usbdefs [if]
    : usb@ ( -- ) 0 ;
    : usb! ( val port --) 2drop ;
[then]

defer end-extra     ' noop is end-extra
