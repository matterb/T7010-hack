\ ddc for cs5530 vi vga

only forth also system
system definitions

base @ hex

\  ddc read and write

: ddc-read ( -- u) 04 f4@ ;
: ddc-write ( u -- ) 04 f4! ;

: ddc-clk-high ( -- )
 1 16 lshift  ddc-read or ddcwrite
;

: ddc-clk-low ( -- )
 1 16 lshift invert ddc-read and ddc-write
;


\ set ddc data for input
: ddc-set-read ( -- )
  1 18 lshift invert ddc-read and ddc-write
;

\ set ddc data for output
: ddc-set-write ( -- )
  1 18 lshift ddc-read or ddc-write
;

: ddc-sda-high ( -- )
 3 17 lshift ddc-read or ddc-write
;

: ddc-sda-low
 2 17 lshift invert ddc-read and ddc-write
;

\ read  ddc data
: sda-5530@ ( -- b)
ddc-set-read 2 ms
ddc-read 1f rshift
;

\ receive byte
: ddc-byte> ( --b )

  ddc-set-read
  ddc-sda-high
  0 8 0 do
     ddc-clk-hi 1 lshift
     ddc-sda@ or
     ddc-clk-lo
  loop
  ddc-set-write
  ddc-sda-hi
;



\ initialize 
: ddc-init ( --)
7 17 lshift ddc-read or ddc-write
ddc-clk-hi ddc-sda-hi
ddc-set-out
ddc-start ddc-stop ddc-stop
;

previous definitions
base !

