\ this is freeware, copyright gordon charlton, 12th of september 1994.
\ copy and distribute it. use it. don't mess with this file. acknowledge
\ its use. i make no guarentees as to its fitness for any purpose. tell
\ me about any bugs. tell me how much you like it.

\               an ans heap

\ this is an implementation of the ans forth memory-allocation word set.
\ this is an ans standard program that has the following environmental
\ dependency - two's complement arithmetic.  it requires four words
\ from the core extension:   0> nip tuck \

\ (if you go to the trouble of checking these claims, please e-mail me
\ with your findings; gordon@charlton.demon.co.uk)

\ there are five broad areas that the program covers;

\      1, general purpose extensions to the forth system.

\      2, creation of the heap and associated use of the data space.

\      3, allocation of space from the heap.

\      4, releasing space back to the heap.

\      5, altering the size of allocated heap space.


\ the ans word set consists of three words, allocate, free, and resize
\ which give the minimum functionality required to use the heap. these are
\ given in areas 3, 4 and 5 respectively.

\ the heap is maintained as a doubly linked ordered circular list of nodes
\ with an additional field noting the size of each node and whether it is in
\ use. the size of the heap is specified by the constant heapsize. the
\ constant hysteresis controls the amount of spare space that is added to
\ an allocation, to reduce the need for block moves during resizing.

\ initially there is only one node, the size of the heap. aditional nodes
\ are created by dividing an existing node into two parts. nodes are removed
\ by marking as free, and merging with adjoining free nodes. nodes are
\ altered in size by merging with a following free node, if possible, and a
\ node being created above the new size of the node, if needed, or by
\ allocating a new node and block moving the data field if necessary.

\ finding an available node is done by sequential search and comparison. the
\ first node to be found that is large enough is used for allocation. each
\ search starts from the node most recently allocated, making this a
\ "nextfit" algorithm. the redundancy in the head fields is required to
\ optimise the search loop, as is the use of a sentinel to terminate the
\ search once every node has been looked at, by always succeeding. a final
\ refinement is the use of the sign bit of the size field to mark "in-use"
\ nodes so that they are disregarded without a separate test.

\ modified for my camel forth
\  modules, heap built at top of RAM
\  TODO add heap expansion

base @ decimal
only forth also definitions 
\ **1** general purpose extensions

: unique (  )  variable ;     internal
\
\ defining word. each child returns a different non-zero number. the
\ standard introduces the need for unique identifiers in the form of iors
\ and throw codes, but provides no means for generating them. this does
\ the trick.

: k ( n--n)  1024 * ;
\
\ a convenient way of referring to large numbers. multiplies a number by
\ 1024.

0 1 2 um/mod nip 1- constant maxpos
\
\ the largest positive single length integer.


\ **2** heap creation

\ ansi heap  ---  constants

2048 k cells constant heapsize
\
\ number of address units of data space that the heap occupies.

4 cells 1- constant hysteresis
\
\ node lengths are rounded up according to the value of hysteresis to
\ reduce the number of block moves during resize operations. the value of
\ this constant must be one less than a power of two and at least equal to
\ one less than the size of a cell.

unique allocationerror
\
\ indicates there is less contiguous heap space available than required.

3 cells constant headsize
\
\ a node on the heap consists of a three cell head followed by a variable
\ length data space. the first cell in the head points to the next node in
\ the heap. the second cell indicates the size of the node, and the third
\ points to the previous node. the second cell is negated to indicate the
\ node is in use. the heap consists of a doubly linked circular list. there
\ is no special notation to indicate an empty list, as this situation
\ cannot occur.

: adjustsize ( n--n)  headsize +  hysteresis or  1+ ;
\
\ the amount of space that is requested for a node needs adjusting to
\ include the length of the head, and to incorporate the hysteresis.

0 adjustsize constant overhead
\
\ the size of the smallest possible node.


\ ansi heap  ---  structure

create sentinel  here cell+ ,   maxpos ,  0 ,  0 ,
\
\ a dummy node used to speed up searching the heap. the search, which is
\ for a node larger than or equal to the specified size will always succeed.
\ the cell that points to the next node is set up so that the there is a zero
\ three cells ahead of where it points, where the pointer to the previous
\ node (ie the sentinel) should be. this is a special value that indicates the
\ search has failed.

\ \\\\\\\\\ create heap  heapsize allot

tom heapsize - to tom

tom 1+ constant  heap
\
\ the heap is as described in headsize.

variable nextnode
\
\ searching is done using a "nextfit" algorithm. nextnode points to the
\ most recently allocated node to indicate where the next search is to
\ start from.

: >size ( addr--addr)  cell+ ;
\
\ move from the "next" cell in the node head to the "size" cell. within the
\ word set nodes are referred to by the address of the "next" cell.
\ externally they are referred to by the address of the start of the data
\ field.

: >prev ( addr--addr)  2 cells + ;
\
\ move from the "next" cell to the "previous" cell.

: init-heap (  )  heap dup nextnode !
          dup dup !
          dup heapsize  over >size !
          >prev ! ;
\
\ initially the heap contains only one node, which is the same size as the
\ heap. both the "next" cell and the "previous" cell point to the "next"
\ cell, as does nextnode.

init-heap

\ **3** heap allocation

\ ansi heap  ---  list searching

: attach ( addr)  >prev @
          dup sentinel rot !
          sentinel >prev ! ;
\
\ the sentinel is joined into the nodelist. the "next" field of the node
\ preceding the one specified (addr) is set to point to the sentinel, and
\ the "prev" field of the sentinel to point to the node that points to the
\ sentinel.

: search  ( addr size--addr|0)
      >r begin 2@ swap r@ < invert until
      r> drop  >prev @ ;
\
\ search the nodelist, starting at the node specified (addr), for a free
\ node larger than or equal to the specified size. return the address of the
\ first node that matches, or zero for no match. the heap structure is set up
\ to make this a near optimal search loop. the "size" field is next to the "next"
\ field so that both can be collected in a single operation (2@). nodes in
\ use have negated sizes so they never match the search. the "previous"
\ field is included to allow the search to overshoot the match by one node
\ and then link back outside the loop, rather than remembering the address
\ of the node just examined. the sentinel removes the need for a separate
\ test for failure. search assumes the sentinel is in place.

: detach ( addr)  dup >prev @ ! ;
\
\ remake the link from the node prior to the one specified to the one
\ specified. this will remove the sentinel if it is attached here. (it will
\ be.)

: findspace ( size--addr|0)  nextnode @
                 dup      attach
                 dup rot  search
                 swap     detach ;
\
\ search the nodelist for a node larger or equal to that specified. return
\ the address of a suitable node, or zero if none found. the search starts at
\ the node pointed to by nextnode, the sentinal temporarily attached, the
\ search proceeded with and the sentinel detached.


\ ansi heap  ---  head creation

: fits ( size addr--flag)  >size @ swap -  overhead  < ;
\
\ returns true if the size of the node specified is the same as the
\ specified size, or larger than it by less than the size of the smallest
\ possible node. returns false otherwise.

: togglesize ( addr)  >size dup @  negate swap ! ;
\
\ negate the contents of the "size" field of the specified node. if the
\ node was available it is marked as in use, and vice versa.

: next! ( addr)  nextnode ! ;
\
\ make the specified node the starting node for future searches of the node
\ list.

: sizes! ( size addr--addr)  2dup + >r
                 >size 2dup @ swap -
                 r@ >size !
                 swap negate swap !  r> ;
\
\ given a free node (addr), reduce its size to that specified and mark it
\ as in use. start to construct a new node within the specified node beyond
\ its new length, by storing the length of the remainder of the node in the
\ size field of the new node. return the address of the partially
\ constructed node.

: links! ( addr1 addr2)  2dup swap @  2dup  swap !  >prev !
                      2dup >prev !   swap ! ;

\
\ addr1 is an existing node. addr2 is the address of a new node just above
\ the existing node. break the links from the existing node to the next
\ node and from the next node to the existing node and join the new node to
\ them.


\ ansi heap  ---  node construction allocate

: newnode ( size addr)  tuck sizes!  links! ;
\
\ given a free node at addr split it into an in-use node of the specified
\ size and a new free node above the in-use node.

: makenode ( size addr)  2dup fits if  togglesize drop
                 else  newnode
                 then ;
\
\ given a free node at addr make an in-use node of the specified size
\ and free the remainder, if there is any usable space left.

: allocate ( u--addr ior)
      dup 0< if  allocationerror
           else  adjustsize
             dup findspace
             dup if  dup next!
                 tuck makenode
                 headsize +  0
               else  drop allocationerror
               then
           then ;
\
\ make an in-use node with a data field at least u address units long.
\ return the address of the data field and an ior of 0 to indicate success.
\ if the space is not available return any old number and an ior equal to the
\ constant allocationerror. the standard specifies that the argument to
\ allocate is unsigned. as the implementation uses the sign bit of the size
\ field for its own purposes any request for an amount of space greater
\ than maxpos must fail. as this would be a request for half the
\ addressable memory or more this is not unreasonable.

\ **4** releasing space

\ ansi heap  ---  head destruction

: mergesizes ( addr addr)
         >size @ swap >size +! ;
\
\ make the size field of the node at addr1 equal to the sum of the sizes of
\ the two specified nodes. in usage the node at addr2 will be the one
\ immediately above addr1.

: mergelinks ( addr addr)
         @ 2dup swap !
           >prev ! ;
\
\ the node at addr2 is removed from the node list. as with mergesizes the
\ node at addr2 will be immediately above that at addr1. destroy the link
\ from node1 to node2 and relink node1 to the node above node2. destroy the
\ backward link from the node above node2 and relink it to node1.

: jiggle (  )
     nextnode @ @  >prev @  next! ;
\
\ there is a possibility when a node is removed from the node list that
\ nextnode may point to it. this is cured by making it point to the node
\ prior to the one removed. we do not want to alter the pointer if it does
\ not point to the removed node as that could be detrimental to the
\ efficiency of the nextfit search algorithm. rather than testing for this
\ condition we jiggle the pointer about a bit to settle it into a linked
\ node. this is done for reasons of programmer amusement. specifically
\ nextnode is set to point to the node pointed to by the "previous" field
\ of the node pointed to in the "next" field of the node pointed to by
\ nextnode. ordinarily this is a no-op (ie i am my father's son) but when
\ the node has had its links merged it sets nextnode to point to the node
\ prior to the node it pointed to (ie when i died my father adopted my son,
\ so now my son is my father's son).

: merge ( addr)
    dup @ 2dup mergesizes
           mergelinks  jiggle ;
\
\ combine the node specified with the node above it. merge the sizes, merge
\ the lengths and jiggle.


\ ansi heap  ---  node removal      free

: ?merge ( addr1 addr2)  >size @
             0> if  dup dup @
                u< if  dup merge
                   then
                then  drop ;
\
\ merge the node at addr1 with the one above it on two conditions, firstly
\ that the node at addr2 is free, and secondly that the node pointed to by
\ the next field in addr1 is actually above addr1 (ie that it does not wrap
\ around because it is the topmost node). in usage addr2 will be either
\ addr1 or the node above it. in each instance the other affected node
\ (either the node above addr1 or addr1) is known to be free, so no test is
\ needed for this.

: ?mergenext ( addr)  dup @ ?merge ;
\
\ merge the node following the specified node with the specified node, if
\ following node is free.

: ?mergeprev ( addr)  >prev @ dup ?merge ;
\
\ merge the specified node with the one preceding it, if the preceding node
\ is free.

: free ( addr--ior)  headsize -
             dup togglesize
             dup ?mergenext
             ?mergeprev  0 ;
\
\ mark the specified in-use word as free, and merge with any adjacent free
\ space. as this is a standard word addr is the address of the data field
\ rather than the "next" field. as there is no compelling reason for this
\ to fail the ior is zero.


\ **5** resizing allocated space

\ ansi heap  ---  node repairing

variable stash
\
\ the resize algorithm is simplified and made faster by assuming that it
\ will always succeed. stash holds the minimum information required to make
\ good when it fails.

: savelink ( addr)  @ stash ! ;
\
\ saves the contents of the >next field of the node being resized in stash
\ (above).

: restorelink ( addr)  stash @  swap ! ;
\
\ converse operation to savelink (above).

: fixprev ( addr)  dup >prev @ ! ;
\
\ the >next field of the node prior to the node being resized should point
\ to the node being resized. it may very well do already, but this makes
\ sure.

: fixnext ( addr)  dup @ >prev ! ;
\
\ the >prev field of the node after the node resized may need correcting.
\ this corrects it whether it needs it or not. (its quicker just to do it
\ than to check first.)

: fixlinks ( addr)  dup fixprev  dup fixnext  @ fixnext ;
\
\ resize may very well merge its argument node with the previous one. it
\ may very well merge that with the next one. this means we need to fix the
\ previous one, the next one and the one after next. to extend the metaphor
\ started in the description of jiggle (above), not only did i die, but my
\ father did too. this brings my grandfather into the picture as guardian
\ of my son. now to confound things we have all come back to life. i still
\ remember who my son is, and my father remembers who his father is. once i
\ know who my father is i can tell my son that i am his father, i can tell
\ my father that i am his son and my grandfather who his son is. thankfully
\ we are only concerned about the male lineage here! (in fact nodes
\ reproduce by division, like amoebae, which is where the metaphor breaks
\ down -- (1) they are sexless and (2) which half is parent and which
\ child?)

: fixsize ( addr)  dup >size @ 0>
           if  dup @  2dup <
               if  over - swap >size !
             else 2drop
             then
         else  drop
         then ;
\
\ reconstruct the size field of a node from the address of the head and the
\ contents of the >next field provided that the node is free and it is not
\ the topmost node in the heap (ie there is no wraparound). both these
\ conditions need to be true for the node to have been merged with its
\ successor.

: fixsizes ( addr)  dup fixsize  >prev @ fixsize ;
\
\ the two nodes whose size fields may need repairing are the one passed as
\ an argument to resize (damaged by ?mergenext) and its predecessor
\ (damaged by ?mergeprev).

: repair ( addr)  dup restorelink
          dup fixlinks  dup fixsizes
          togglesize ;
\
\ make good the damage done by resize. restore the >next field, fix the
\ links, fix the size fields and mark the node as in-use. note that this
\ may not restore the system to exactly how it was. in particular the pointer
\ nextnode may have moved back one or two nodes by virtue of having been
\ jiggled about if it happened to be pointing to the wrong node. this is not
\ serious, so i have chosen to ignore it.


\ ansi heap  ---  node movement

: toobig? ( addr size--flag)
      swap  >size @  > ;
\
\ flag is true if the node at addr is smaller than the specified size.

: copynode ( addr1 addr2)
       over >size @  headsize -
       rot  headsize + rot rot move ;
\
\ move the contents of the data field of the node at addr1 to the data
\ field at addr2. assumes addr2 is large enough. it will be.

: enlarge ( addr1 size--addr2 ior)
      over  ?mergeprev
      allocate dup >r
      if  swap repair
    else  tuck copynode
    then r> ;
\
\ make a new node of the size specified. copy the data field of addr1 to
\ the new node. merge the node at addr1 with the one preceding it, if
\ possible. this last behaviour is to finish off removing the node at
\ addr1. the word adjust (below) starts removing the node. the node is
\ removed before allocation to increase the probability of allocate
\ succeeding. the address returned by enlarge is that returned by allocate,
\ which is that of the data field, not the head. if the allocation fails
\ repair the damage done by removing the node at addr1.


\ ansi heap  ---  node restructuring    resize

: adjust ( addr1 size1--addr2 size2)  adjustsize >r
                      headsize -
                      dup savelink
                      dup togglesize
                      dup ?mergenext r> ;
\
\ addr1 points to the data field of a node, not the "next" field. this
\ needs correcting. size1 also needs adjusting as per adjustsize. in
\ addition it is easier to work with free nodes than live ones as the size
\ field is correct, and, as we intend to change the nodes size we will
\ inevitably want to muck about with the next node, if its free, so lets
\ merge with it straight away. sufficient information is first saved to put
\ the heap back as it was, if necessary. now we are ready to get down to
\ business.

: allocate ( u -- a ior )
   allocate  ;   external

: free ( a -- ior )
   free ;

: resize ( addr1 u--addr2 ior)
     dup 0< if  drop allocationerror
          else  adjust  2dup
            toobig?  if  enlarge
               else  over makenode
                 headsize +  0
               then
          then ;
module

base !
\
\ resize the node at addr1 to the specified size. return the address of the
\ resized node (addr2) along with an ior of zero if successful and
\ allocationerror if not. addr2 may be the same as, or different to, addr1.
\ if ior is non-zero then addr2 is not meaningful. being a standard word
\ the arguments need adjusting to the internal representation on entry, and
\ back again on exit. if after the first merge the requested size is still
\ too large to reuse the specified node then it is moved to a larger node
\ and the specified node released. if, on the other hand the request is not
\ too big for the node, then we remake the node at the right length, and
\ free any space at the top using makenode, which has just the right
\ functionality. in this case the ior is zero. as this is a standard word it
\ takes an unsigned size argument, but excessive requests fail
\ automatically, as with allocate.
