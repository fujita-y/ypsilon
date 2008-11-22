#!nobacktrace

(library (srfi :1)
  
  (export
   ;; constructors
   xcons
   list
   cons*
   make-list
   list-tabulate
   list-copy
   circular-list
   iota

   ;; predicates
   pair?
   null?
   proper-list?
   circular-list?
   dotted-list?
   not-pair?
   null-list?
   list=

   ;; selectors
   car cdr
   caar cadr cdar cddr
   caaar caadr cadar caddr
   cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr
   cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr
   cddaar cddadr cdddar cddddr
   list-ref
   first
   second
   third
   fourth
   fifth
   sixth
   seventh
   eighth
   ninth
   tenth
   car+cdr
   take
   take!
   drop
   take-right
   drop-right
   drop-right!
   split-at
   split-at!
   last
   last-pair

   ;; miscellaneous: length, append, concatenate, reverse, zip & count
   length
   length+
   append
   append!
   concatenate
   concatenate!
   reverse
   reverse!
   append-reverse
   append-reverse!
   zip
   unzip1 unzip2 unzip3 unzip4 unzip5
   count

   ;; fold, unfold & map
   map
   map!
   map/srfi-1
   map!/srfi-1
   for-each
   for-each/srfi-1
   fold
   fold-right
   fold-right/srfi-1
   unfold
   pair-fold
   reduce
   unfold-right
   pair-fold-right
   reduce-right
   append-map
   append-map!
   pair-for-each
   filter-map
   map-in-order

   ;; filtering & partitioning
   filter                                        ; -> (core lists)
   filter!
   partition                                     ; -> (core lists)
   partition!
   remove/srfi-1
   remove!/srfi-1

   ;; seaching
   member
   member/srfi-1
   memq
   memv
   find
   find-tail
   any
   every
   list-index
   take-while
   take-while!
   drop-while
   span
   span!
   break
   break!

   ;; deleting
   delete
   delete!
   delete-duplicates
   delete-duplicates!

   ;; association lists
   assoc
   assoc/srfi-1
   assq
   assv
   alist-cons
   alist-copy
   alist-delete
   alist-delete!

   ;; set operations on lists
   lset<=
   lset=
   lset-adjoin
   lset-adjoin!
   lset-union
   lset-union!
   lset-intersection
   lset-intersection!
   lset-difference
   lset-difference!
   lset-xor
   lset-xor!
   lset-diff+intersection
   lset-diff+intersection!)

  (import (srfi srfi-1)))
