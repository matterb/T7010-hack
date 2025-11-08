\ diagnostic
\ trace words for for forth
[undefined] tr-flag [if]
hex 
0 value tr-flag
0 value tr-indent


: tr-name ( $ -- ) tr-indent spaces count type ;

: (trace) ( -- ) \ runtime show name of current word and stack
  r@ @ tr-indent 1+ to tr-indent 
  cfa>nfa tr-name cr
;
: (;trace) ( -- ) \ trace exit word
 tr-indent 1- to tr-indent
;

: : ( -- ) : tr-flag if 
    postpone (trace)
  then  
; immediate

: ; ( -- ) 
  tr-flag if postpone (;trace) then
  postpone ;
; immediate

: trace-on true to tr-flag ;
: trace-off false to tr-flag ;


[then]
