\ @@FILE:keyboard.fth
\ from coreboot libpayload

\ I8042_CMD_READ_MODE  0x20
\ I8042_CMD_WRITE_MODE 0x60
\ I8042_MODE_XLATE     0x40

base @ hex

[defined] system [if]
    only forth also
    system definitions
[then]

[undefined] ms [if]
: ms drop ;
[then]

[undefined] udelay [if]
: udelay drop ;
[then]

d# 87 constant #size-of-map-table  internal 
create key-map
\ No modifier 
     00 c, 1b c, 31 c, 32 c, 33 c, 34 c, 35 c, 36 c, 
     37 c, 38 c, 39 c, 30 c, 2d c, 3d c, 08 c, 09 c, 
     71 c, 77 c, 65 c, 72 c, 74 c, 79 c, 75 c, 69 c, 
     6f c, 70 c, 5b c, 5d c, 0a c, 00 c, 61 c, 73 c, 
     64 c, 66 c, 67 c, 68 c, 6a c, 6b c, 6c c, 3b c, 
     27 c, 60 c, 00 c, 5c c, 7a c, 78 c, 63 c, 76 c, 
     62 c, 6e c, 6d c, 2c c, 2e c, 2f c, 00 c, 2a c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\  Shift 
     00 c, 1b c, 21 c, 40 c, 23 c, 24 c, 25 c, 5e c, 
     26 c, 2a c, 28 c, 29 c, 5f c, 2b c, 08 c, 00 c, 
     51 c, 57 c, 45 c, 52 c, 54 c, 59 c, 55 c, 49 c, 
     4f c, 50 c, 7b c, 7d c, 0a c, 00 c, 41 c, 53 c, 
     44 c, 46 c, 47 c, 48 c, 4a c, 4b c, 4c c, 3a c, 
     22 c, 7e c, 00 c, 7c c, 5a c, 58 c, 43 c, 56 c, 
     42 c, 4e c, 4d c, 3c c, 3e c, 3f c, 00 c, 2a c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\ ALT 
     00 c, 1b c, 31 c, 32 c, 33 c, 34 c, 35 c, 36 c, 
     37 c, 38 c, 39 c, 30 c, 2d c, 3d c, 08 c, 09 c, 
     71 c, 77 c, 65 c, 72 c, 74 c, 79 c, 75 c, 69 c, 
     6f c, 70 c, 5b c, 5d c, 0a c, 00 c, 61 c, 73 c, 
     64 c, 66 c, 67 c, 68 c, 6a c, 6b c, 6c c, 3b c, 
     27 c, 60 c, 00 c, 5c c, 7a c, 78 c, 63 c, 76 c, 
     62 c, 6e c, 6d c, 2c c, 2e c, 2f c, 00 c, 2a c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\ Shift-ALT
     00 c, 1b c, 21 c, 40 c, 23 c, 24 c, 25 c, 5e c, 
     26 c, 2a c, 28 c, 29 c, 5f c, 2b c, 08 c, 00 c, 
     51 c, 57 c, 45 c, 52 c, 54 c, 59 c, 55 c, 49 c, 
     4f c, 50 c, 7b c, 7d c, 0a c, 00 c, 41 c, 53 c, 
     44 c, 46 c, 47 c, 48 c, 4a c, 4b c, 4c c, 3a c, 
     22 c, 7e c, 00 c, 7c c, 5a c, 58 c, 43 c, 56 c, 
     42 c, 4e c, 4d c, 3c c, 3e c, 3f c, 00 c, 2a c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,

1 constant MOD_SHIFT    \  (1 << 0)
2 constant MOD_CTRL     \  (1 << 1)
4 constant MOD_CAPSLOCK \  (1 << 2)
8 constant MOD_ALT      \  (1 << 3)


: keyboard-cmd ( val cmd -- )
  begin h# 64 pc@ 2 and 0= until
  h# 60 pc! 20 ms

  begin h# 64 pc@ 2 and 0= until
  h# 60 pc! 20 ms
;

: keyboard-char? ( -- flg )
  h# 64 pc@
  dup h# ff = if drop 0 else 1 and 0<> then
;

: keyboard-scancode@ ( -- 0|ch )
    keyboard-char? if h# 60 pc@ else 0 then
;

0 value kbd-modifier
0 value kbd-shift
0 value keyboard-char

: keyboard-char@ ( -- 0|ch )   \ wait for character from the keyboard 
    begin pause keyboard-char? until
    keyboard-scancode@
    dup case    ( ch )
    h# 36 of 
          kbd-modifier MOD_SHIFT or endof
    h# 2a of 
          kbd-modifier MOD_SHIFT or endof
    h# 0b6 of 
          kbd-modifier MOD_SHIFT invert and endof
    h# 0aa of 
           kbd-modifier MOD_SHIFT invert and endof
    h# 38  of 
           kbd-modifier MOD_ALT or endof
    h# 0b8 of 
           kbd-modifier MOD_ALT invert and endof
    h# 1d  of 
           kbd-modifier MOD_CTRL or endof
    h# 9d  of 
           kbd-modifier MOD_CTRL invert and endof
    h# 3a  of 
           kbd-modifier dup MOD_CAPSLOCK and if 
                MOD_CAPSLOCK invert and  0 
                else  MOD_CAPSLOCK or 4 then
                h# ed keyboard-cmd
            endof
    endcase
    to kbd-modifier
    dup h# 80 and 0= over h# 57 < and if ( ch )
         kbd-modifier MOD_SHIFT and kbd-modifier MOD_CAPSLOCK and xor if
             1 else 0 then to kbd-shift
         kbd-modifier MOD_ALT and if kbd-shift 2 + to kbd-shift then

         kbd-shift #size-of-map-table  * key-map + + c@ \ map->map[shift][ch]
( ch ret )
         kbd-modifier MOD_CTRL and if 
            dup [char] a > over [char] z < and if  \ need >= <= 
                h# 1f and
\            else
\            dup h# 53  = if  \ vulcan nerve pinch
\                kbd-modifier MOD_ALT and if reset_handler then )
             else 
                drop 0
            then
        then
    else
        drop 0
    then
;


: keyboard-wait-read ( -- flg )  \ true if no timeout
  10000  begin
        1- dup h#  64 pc@ 1 and 0=  and
   while
        50 udelay
   repeat
   0 >
;

: keyboard-wait-write ( -- flg ) \ true if no timeout
  10000 begin
        1- dup h# 64 pc@ 2 and 0= and
  while
        50  udelay 
  repeat
  0 >
;

: keyboard-mode@ ( -- 0|mode )
    h# 20 h# 64 pc!
    keyboard-wait-read if
        h# 60 pc@  else 0 
    then
;

: keyboard-mode! ( mode -- )  \
    h# 60 h# 64 pc!
    keyboard-wait-write if
        h# 60 pc! else drop 
    then
;

: keyboard-init ( -- )
\  If 64 returns ff , then we have no keyboard controller
    h# 64 pc@ h# ff =  if abort" No keyboard" then
\ Empty keyboard buffer
    begin keyboard-char?  while
        keyboard-char@ drop
    repeat
;  external


: key-keybd? ( -- flg )
  keyboard-char 0= if
     keyboard-char? if keyboard-char@ to keyboard-char then
  then
  keyboard-char 0<>
;

: key-keybd ( -- ch )
 begin key-keybd? until
 keyboard-char  0 to keyboard-char
;   module

[defined] system [if]
  previous definitions
[then]
base !

