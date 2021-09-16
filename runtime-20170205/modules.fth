\ modules.fth
\ word specific defintions of internal
\ test
\ dependency :
\ dictionary header link:Precedence[byte]:name[c-bytes]|smudge[bit]
\ bit 1 in precence
\ encapsulation rules :
\ dictionary { vocabulary { modules { internal* | external*}}}
\ usage 
\ module[  word1 internal word2 word3 external word4 internal ]module
\ The factors internal? and clr-flag should inlined to reduce dictionary clutter
: begin-module  ( -- lfa )  latest @ nfa>lfa ;   \ start of module definitions
: internal ( -- ) latest @ 1- dup @ 2 or swap ! ;  \ set internal flag
: external ( -- ) ;  \ syntatic sugar 
\ : internal? ( lfa -- ) cell+ @ 2 and ;   \ check for module marker
\ : clr-flag ( lfa -- ) cell+ dup @ 2 invert and swap ! ; \ remove marker 
: end-module  ( lfa -- )
  latest begin  
  2dup  = 0= while
    dup @ nfa>lfa swap
    over cell+ @ 2 and if  \ internal? check for module marker
        over cell+ dup @ 2 invert and swap !  \ clr-flag remove marker 
        over @ swap !
    else
        drop  
    then
  repeat 2drop
;


