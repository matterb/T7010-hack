base @ hex

: ?dnumber  ( addr u -- n t | addr u f )  \ string->double; false on error
                                        \ c-addr 0  if convert error
    0 0 2over             \   -- adr u ud adr n
    ?sign >r  >number       \   -- adr u ud adr' n'
    over c@ [char] . = if   \  trailing . for double number
        1- swap 1+ swap then
    if  r> 2drop 2drop 0    \   -- adr u 0   (error)
    else drop 2swap 2drop r> if dnegate then
        true                \   -- n -1   (ok)
    then
 ; 

.( testing new vocabulary implementation ) cr

.( forth definition )
 only forth definitions

vocabulary voc1
.( voc1 created )
vocabulary voc2
also voc1 definitions
vocabulary voc11
vocabulary voc12
previous definitions


base ! 

