\ floating point word set
\ convert string to float
: >float ( c-addr u -- true | false ) ( f: -- r | ) or ( c-addr u -- r true | false )
;

D>F ( d -- ) ( F: -- r ) or ( d -- r )
F! ( f-addr -- ) ( F: r -- ) or ( r f-addr -- )
F* 
F**
F+ 
F- 
F. 
f/ 
F0< 
F0= 
F< 
F>D 
F>S 
F@ 
FABS 
FACOS 
FACOSH
FALIGN
FALIGNED 
FALOG 
FALSE 
FASIN 
FASINH
FATAN 
FATAN2
FATANH
: fconstant ( F: r -- )
  create falign here 1 floats allot f!
  does> ( F: -- r) faligned f@ ;
: fvariable ( -- f-addr )
    create falign here1 floats allot
    does> faligned ;

FCOS 
FCOSH
FDEPTH
FDROP 
FDUP 
FE. 
FEXP 
FEXPM1
FFIELD: 
FLITERAL 
FLN 
FLNP1 
FLOAT+
FLOATS
FLOG 
FLOOR 
FMAX 
FMIN 
FNEGATE 
FOVER 
FROT 
FROUND
FS. 
FSIN 
FSINCOS 
FSINH 
FSQRT 
FSWAP 
FTAN 
FTANH 
FTRUNC
FVALUE
FVARIABLE
F~ 
