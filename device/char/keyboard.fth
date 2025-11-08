\ file keyboard.fth

\ I8042_CMD_READ_MODE  20
\ I8042_CMD_WRITE_MODE 60
\ I8042_MODE_XLATE     40

base @ hex

[undefined] ms [if]
: ms drop ;
[then]

[undefined] udelay [if]
: udelay drop ;
[then]

d# 87 constant #size-of-map-table 
create key-map
\ No modifier 
     00 c, 1B c, 31 c, 32 c, 33 c, 34 c, 35 c, 36 c, 
     37 c, 38 c, 39 c, 30 c, 2D c, 3D c, 08 c, 09 c, 
     71 c, 77 c, 65 c, 72 c, 74 c, 79 c, 75 c, 69 c, 
     6F c, 70 c, 5B c, 5D c, 0A c, 00 c, 61 c, 73 c, 
     64 c, 66 c, 67 c, 68 c, 6A c, 6B c, 6C c, 3B c, 
     27 c, 60 c, 00 c, 5C c, 7A c, 78 c, 63 c, 76 c, 
     62 c, 6E c, 6D c, 2C c, 2E c, 2F c, 00 c, 2A c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\  Shift 
     00 c, 1B c, 21 c, 40 c, 23 c, 24 c, 25 c, 5E c, 
     26 c, 2A c, 28 c, 29 c, 5F c, 2B c, 08 c, 00 c, 
     51 c, 57 c, 45 c, 52 c, 54 c, 59 c, 55 c, 49 c, 
     4F c, 50 c, 7B c, 7D c, 0A c, 00 c, 41 c, 53 c, 
     44 c, 46 c, 47 c, 48 c, 4A c, 4B c, 4C c, 3A c, 
     22 c, 7E c, 00 c, 7C c, 5A c, 58 c, 43 c, 56 c, 
     42 c, 4E c, 4D c, 3C c, 3E c, 3F c, 00 c, 2A c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\ ALT 
     00 c, 1B c, 31 c, 32 c, 33 c, 34 c, 35 c, 36 c, 
     37 c, 38 c, 39 c, 30 c, 2D c, 3D c, 08 c, 09 c, 
     71 c, 77 c, 65 c, 72 c, 74 c, 79 c, 75 c, 69 c, 
     6F c, 70 c, 5B c, 5D c, 0A c, 00 c, 61 c, 73 c, 
     64 c, 66 c, 67 c, 68 c, 6A c, 6B c, 6C c, 3B c, 
     27 c, 60 c, 00 c, 5C c, 7A c, 78 c, 63 c, 76 c, 
     62 c, 6E c, 6D c, 2C c, 2E c, 2F c, 00 c, 2A c, 
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
     00 c, 20 c, 00 c, 3a c, 3b c, 3c c, 3d c, 3e c, 
     40 c, 41 c, 42 c, 43 c, 44 c, 00 c, 00 c, 47 c, 
     48 c, 51 c, 00 c, 4b c, 00 c, 4d c, 00 c, 4f c, 
     50 c, 49 c, 00 c, 53 c, 00 c, 00 c, 00 c,
\ Shift-ALT
     00 c, 1B c, 21 c, 40 c, 23 c, 24 c, 25 c, 5E c, 
     26 c, 2A c, 28 c, 29 c, 5F c, 2B c, 08 c, 00 c, 
     51 c, 57 c, 45 c, 52 c, 54 c, 59 c, 55 c, 49 c, 
     4F c, 50 c, 7B c, 7D c, 0A c, 00 c, 41 c, 53 c, 
     44 c, 46 c, 47 c, 48 c, 4A c, 4B c, 4C c, 3A c, 
     22 c, 7E c, 00 c, 7C c, 5A c, 58 c, 43 c, 56 c, 
     42 c, 4E c, 4D c, 3C c, 3E c, 3F c, 00 c, 2A c, 
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
  h# 60 pc!
  20 ms

  begin h# 64 pc@ 2 and 0= until
  h# 60 pc!
  20 ms
;

: keyboard-char? ( -- flg )
  h# 64 pc@
  dup h# ff = if drop 0 else 1 and 0<> then
;


: keyboard-scancode@ ( -- 0|ch )

    keyboard-char? if h# 60 pc@ else 0 then
;

0 value modifier
0 value shift

: keyboard-char@ ( -- 0|ch )

    begin pause keyboard-char? until

    keyboard-scancode@
    dup case    ( ch )
    h# 36    of 
            modifier MOD_SHIFT or to modifier endof
    h# 2a    of 
           modifier MOD_SHIFT or to modifier endof
    h# 80 h# 36 or of 
           modifier MOD_SHIFT invert and to modifier endof
    h# 80 h# 2a or of 
           modifier MOD_SHIFT invert and to modifier endof
    h# 38     of 
           modifier MOD_ALT or to modifier endof
    h# 80 h# 38 or of 
           modifier MOD_ALT invert and endof
    h# 1d    of 
           modifier MOD_CTRL or to modifier endof
    h# 80 h# 1d or of 
           modifier MOD_CTRL invert and to modifier endof
    h# 3a    of 
            modifier  MOD_CAPSLOCK and if 
                modifier MOD_CAPSLOCK invert and to modifier
                0 h# ed keyboard-cmd
            else
                modifier MOD_CAPSLOCK or to modifier
               4 h# ed keyboard-cmd
            then
            endof
   endcase

   dup h# 80 and 0= over h# 57 < and if ( ch )
         modifier MOD_SHIFT and modifier MOD_CAPSLOCK and xor if
             1 else 0 then to shift
         modifier MOD_ALT and if shift 2 + to shift then

         shift #size-of-map-table  * key-map + + c@ \ map->map[shift][ch]
( ch ret )
         modifier MOD_CTRL and if 
            dup [char] a > over [char] z < and if  \ need >= <= 
                h# 1f and
\            else
\            dup h# 53  = if  \ vulcan nerve pinch
\                modifier MOD_ALT and if reset_handler then )
             else 
                drop 0
            then
        then
    else
        drop 0
    then

;


: keyboard-wait-read ( -- flg )
\ true if no timeout
  10000  begin
        1- dup h#  64 pc@ 1 and 0=  and
   while
        50 udelay
   repeat

   0 >
;

: keyboard-wait-write ( -- flg )
\ true if no timeout
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

: keyboard-mode! ( mode -- )
\
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

;



base !

