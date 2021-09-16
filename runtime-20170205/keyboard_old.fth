\ keyboard.fth

base @ hex 

only forth also
system definitions

create scan-to-ascii#                    \ == scan code ==
  00 c,                               \ db 0            ; 0
  1B c,                               \ db 01bh         ;esc ; 1
  31 c, 32 c, 33 c, 34 c, 35 c, 36 c, \ db '1234567890-='    ; 2-13
  37 c, 38 c, 39 c, 30 c, 2D c, 3D c,
  08 c, 09 c,                         \ db 08h,09h ;backspace, tab; 14,15
  71 c, 77 c, 65 c, 72 c, 74 c, 79 c, \ db 'qwertyuiop[]'  ; 16-27
  75 c, 69 c, 6F c, 70 c, 5B c, 5D c,
  0D c, 01 c,                         \ db 00dh,000h ;enter, left control 
                                      \ ; 28,29 ScrolLock 
  61 c, 73 c, 64 c, 66 c, 67 c, 68 c, \ db 'asdfghjkl;'`; 30-41
  6A c, 6B c, 6C c, 3B c, 27 c, 60 c,
  00 c,                                     \ db 000h left shift    ; 42
  5C c,                                     \ db '\'               ; 43
  7A c, 78 c, 63 c, 76 c, 62 c, 6E c,  \ db 'zxcvbnm,./'     ; 44-53
  6D c, 2C c, 2E c, 2F c,
  00 c,                                \ db 000h  right shift  54
  2A c,                                \ db '*'  * in keypad  55
  00 c,                                \ db 000h left alt   56
  20 c,                                \ db ' '  space     57
  00 c,                                \ db 000h  capslock   58
  00 c, 00 c, 00 ,                     \  f1-f10  59-68
  00 c, 00 c,              \ db 000h,000h  numlock, scrolllock  69 70
  2D000000 , 2B000000 ,    \ db 0,0,0,'-',0,0,0,'+',0,0,0,0,0 keypad 71-83
  00 c, 00 c, 00 c, 00 c, 00 c,
  00 c, 00 c,                              \ db 000h,000h f11,f12 84,85

\ Shift ScanCode ASCII ASCII
create scan-to-asc-shift#
  00 c,                                  \ DB 0               0
  1B c,                                  \ DB 01BH            1    ESC
  21 c, 40 c, 23 c, 24 c, 25 c, 5E c,    \ DB '!@#%^&*()_+'  2-13
  26 c, 2A c, 28 c, 29 c, 5F c, 2B c,
  08 c, 09 c,                            \ DB 08H,09H   14,15 ;BS, TAB
  51 c, 57 c, 45 c, 52 c, 54 c, 59 c, \ DB 'QWERTYUIOP{}'  16-27
  55 c, 49 c, 4F c, 50 c, 7B c, 7D c,
  0D c, 01 c,                          \ DB 00DH,000H 28,29 ;ENTER, LEFT CNTRL
  41 c, 53 c, 44 c, 46 c, 47 c, 48 c, \ DB 'ASDFGHJKL:"~'  30-41
  4A c, 4B c, 4C c, 3A c, 22 c, 7E c,
  00 c,                                \ DB 000H    42 LEFT SHIFT
  7C c,                                    \ DB '|'  43
  5A c, 58 c, 43 c, 56 c, 42 c, 4E c, \ DB 'ZXCVBNM<>?'    44-53
  4D c, 3C c, 3E c, 3F c,
  00 c,                               \ DB 000H   54    RIGHT SHIFT
  2A c,                               \ DB '*'    55    * in KEYPAD
  00 c,                               \ DB 000H   56    LEFT ALT
  20 c,                               \ DB ' '    57
  00 c,                               \ DB 000H   58    CAPSLOCK
  00 c, 00 c, 00 c, 00 c, 00 c, 00 c, \ DB 000H X 10   59-68 F1-F10
  00 c, 00 c, 00 c, 00 c, 00 c, 00 c, \ DB 000H,000H 69-70 NUMLOCK, SCROLLLOCK
  37 c, 38 c, 39 c, 2D c, 34 c, 35 c, \ DB '789-456+1230. 71-83 KEYPAD+NUMLOCK
  36 c, 2B c, 31 c, 32 c, 33 c, 30 c,
  2E c,
  00 c, 00 c,                         \ DB 00H,00H  84,85 F11, F12

\  scan-to-asc    ( scan_code table -- ASCII_code )
\  Translate keyboard scan code from 8042 port 60h to ASCII code
\  Given a normal table SCAN2ASCII or shifted table SCAN2ASCSHIFT.

: scan-to-asc
     swap dup     \ t c c
     d# 85 >         \ t c c>85
     if           \ t c        ; 85 key only
                  \            ; below or equal 85
       drop 0     \ t 0        ; ignore the scan code.
    then         \ t c 
                  \ t c       
    +            \ &table[c]
    c@           \ table[c]
;               

: kbcobf ( -- f )
\        check KBC OutputBuffer

   h# 64 pc@
   h# 01 and
;


: (waitOBF)
    begin
  ( hlt Time tick  )
  kbcobf  ( wait until obf raised )
  until
 ;

defer waitOBF
   
' (waitOBF) is waitOBF

: waitkscan  ( -- scancode .t.| .f. )
\            wait for a key press and then return scan code

 begin
       waitOBF
       h# 60 pc@          \ p60
       dup                \ p60 p60
       h# 80 and 0 =      \ p60 p60%80h==0?   Make code
       if                 \ p60
          true            \ p60 1  make code, done.
       else
          drop false      \ failed  it's a break code
       then
   until ;


: keybd-chr? ( -- F | ascii T )
  kbcobf  if
       h# 60 pc@ dup dup dup 
       h# 80 and 0 =      
       if                 \  it's a make code
          d# 42 = if      \  Left Shift key's scan code
             drop drop waitkscan
             scan-to-asc-shift# scan-to-asc true  \ shift key,
          else
             d# 54 = if
                  drop waitkscan
                  scan-to-asc-shift# scan-to-asc true
             else
                  scan-to-ascii# scan-to-asc true
             then
          then
       else
           drop drop drop false
       then
  else
       false
  then
;

previous definitions

base !


