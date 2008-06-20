  
(library (rnrs (6))

  (export

   #; (rnrs base (6))
   define define-syntax
   quote lambda if set! cond case and or
   let let* letrec letrec* let-values let*-values
   begin quasiquote unquote unquote-splicing
   let-syntax letrec-syntax syntax-rules
   identifier-syntax assert
   else => ... _
   eq?
   eqv?
   equal?
   procedure?
   number? complex? real? rational? integer?
   real-valued? rational-valued? integer-valued?
   exact? inexact?
   inexact exact
   = < > <= >=
   zero? positive? negative? odd? even?
   finite? infinite? nan?
   max min + * - / abs
   div-and-mod div mod div0-and-mod0 div0 mod0
   gcd lcm numerator denominator
   floor ceiling truncate round
   rationalize
   exp log sin cos tan asin acos atan
   sqrt
   exact-integer-sqrt
   expt
   make-rectangular make-polar real-part imag-part
   magnitude angle
   number->string string->number
   not boolean?
   pair? cons car cdr
   caar cadr cdar cddr caaar caadr cadar
   caddr cdaar cdadr cddar cdddr caaaar caaadr
   caadar caaddr cadaar cadadr caddar cadddr cdaaar
   cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   null? list? list length append reverse list-tail
   list-ref map for-each
   symbol? symbol->string string->symbol
   char? char->integer integer->char
   char=? char<? char>? char<=? char>=?
   string? make-string string string-length string-ref
   string=? string<? string>? string<=? string>=?
   substring string-append string->list list->string string-copy string-for-each
   vector? make-vector vector vector-length vector-ref vector-set!
   vector->list list->vector vector-fill!
   vector-map vector-for-each
   error assertion-violation
   apply call-with-current-continuation call/cc
   values call-with-values dynamic-wind

   #;(rnrs unicode (6))
   char-upcase char-downcase char-titlecase char-foldcase
   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
   char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? char-title-case?
   char-general-category
   string-upcase string-downcase string-titlecase string-foldcase
   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
   string-normalize-nfd string-normalize-nfkd string-normalize-nfc string-normalize-nfkc

   #;(rnrs bytevectors (6))
   endianness native-endianness
   bytevector? make-bytevector bytevector-length bytevector=?
   bytevector-fill! bytevector-copy! bytevector-copy
   bytevector-u8-ref bytevector-s8-ref bytevector-u8-set! bytevector-s8-set!
   bytevector->u8-list u8-list->bytevector
   bytevector-u16-ref bytevector-s16-ref bytevector-u16-native-ref bytevector-s16-native-ref
   bytevector-u16-set! bytevector-s16-set! bytevector-u16-native-set! bytevector-s16-native-set!
   bytevector-u32-ref bytevector-s32-ref bytevector-u32-native-ref bytevector-s32-native-ref
   bytevector-u32-set! bytevector-s32-set! bytevector-u32-native-set! bytevector-s32-native-set!
   bytevector-u64-ref bytevector-s64-ref bytevector-u64-native-ref bytevector-s64-native-ref
   bytevector-u64-set! bytevector-s64-set! bytevector-u64-native-set! bytevector-s64-native-set!
   bytevector-ieee-single-ref bytevector-ieee-single-native-ref
   bytevector-ieee-single-set! bytevector-ieee-single-native-set!
   bytevector-ieee-double-ref bytevector-ieee-double-native-ref
   bytevector-ieee-double-set! bytevector-ieee-double-native-set!
   bytevector-uint-ref bytevector-sint-ref
   bytevector-uint-set!  bytevector-sint-set!
   bytevector->uint-list bytevector->sint-list
   uint-list->bytevector sint-list->bytevector
   string->utf8 utf8->string
   string->utf16 utf16->string
   string->utf32 utf32->string

   #;(rnrs lists (6))
   find for-all exists
   filter
   partition
   fold-left fold-right
   remp remove remv remq
   memp member memv memq
   assp assoc assv assq cons*

   #;(rnrs sorting (6))
   list-sort vector-sort

   #;(rnrs control (6))
   when unless do case-lambda

   #;(rnrs records syntactic (6))
   define-record-type
   record-type-descriptor
   record-constructor-descriptor

   #;(rnrs records procedural (6))
   make-record-type-descriptor
   record-type-descriptor?
   make-record-constructor-descriptor
   record-constructor
   record-predicate
   record-accessor
   record-mutator

   #;(rnrs records inspection (6))
   record?
   record-rtd
   record-type-name
   record-type-parent
   record-type-uid
   record-type-generative?
   record-type-sealed?
   record-type-opaque?
   record-type-field-names
   record-field-mutable?

   #;(rnrs exceptions (6))
   with-exception-handler guard raise raise-continuable

   #;(rnrs conditions (6))
   &condition
   condition simple-conditions condition?
   condition-predicate condition-accessor
   define-condition-type
   &message make-message-condition message-condition? condition-message
   &warning make-warning warning?
   &serious make-serious-condition serious-condition?
   &error make-error error?
   &violation make-violation violation?
   &assertion make-assertion-violation assertion-violation?
   &irritants make-irritants-condition irritants-condition?
   &who make-who-condition who-condition? condition-who
   &non-continuable make-non-continuable-violation non-continuable-violation?
   &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
   &lexical make-lexical-violation lexical-violation?
   &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
   &undefined make-undefined-violation undefined-violation?

   #;(rnrs io ports (6))
   &i/o make-i/o-error i/o-error?
   &i/o-read make-i/o-read-error i/o-read-error?
   &i/o-write make-i/o-write-error i/o-write-error?
   &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
   &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
   &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
   &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
   &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
   &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
   &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port
   &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
   &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char

   file-options
   buffer-mode
   buffer-mode?
   utf-8-codec
   utf-16-codec
   latin-1-codec
   eol-style
   error-handling-mode
   make-transcoder
   transcoder-codec
   transcoder-eol-style
   transcoder-error-handling-mode
   native-transcoder
   native-eol-style
   bytevector->string
   string->bytevector
   eof-object
   eof-object?
   port?
   port-transcoder
   textual-port?
   binary-port?
   transcoded-port
   port-has-port-position?
   port-position
   port-has-set-port-position!?
   set-port-position!
   close-port
   call-with-port
   input-port?
   port-eof?
   open-file-input-port
   open-bytevector-input-port
   open-string-input-port
   standard-input-port
   current-input-port
   get-u8
   lookahead-u8
   get-bytevector-n
   get-bytevector-n!
   get-bytevector-some
   get-bytevector-all
   get-char
   lookahead-char
   get-string-n
   get-string-n!
   get-string-all
   get-line
   get-datum
   output-port?
   flush-output-port
   output-port-buffer-mode
   open-file-output-port
   open-bytevector-output-port
   call-with-bytevector-output-port
   open-string-output-port
   call-with-string-output-port
   standard-output-port
   standard-error-port
   current-output-port
   current-error-port
   put-u8
   put-bytevector
   put-char
   put-string
   put-datum
   open-file-input/output-port
   make-custom-binary-input-port
   make-custom-textual-input-port
   make-custom-binary-output-port
   make-custom-textual-output-port

   #;(rnrs io simple (6))
   call-with-input-file
   call-with-output-file
   with-input-from-file
   with-output-to-file
   open-input-file
   open-output-file
   close-input-port
   close-output-port
   read-char
   peek-char
   read
   write-char
   newline
   display
   write

   #;(rnrs files (6))
   file-exists? delete-file

   #;(rnrs enums (6))
   make-enumeration
   enum-set-universe
   enum-set-indexer
   enum-set-constructor
   enum-set->list
   enum-set-member?
   enum-set-subset?
   enum-set=?
   enum-set-union
   enum-set-intersection
   enum-set-difference
   enum-set-complement
   enum-set-projection
   define-enumeration

   #;(rnrs programs (6))
   command-line
   exit
   
   #;(rnrs arithmetic fixnums (6))
   fixnum?
   fixnum-width
   least-fixnum
   greatest-fixnum
   fx=?
   fx<?
   fx>?
   fx<=?
   fx>=?
   fxzero?
   fxpositive?
   fxnegative?
   fxodd?
   fxeven?
   fxmax
   fxmin
   fx+
   fx*
   fx-
   fxdiv
   fxmod
   fxdiv-and-mod
   fxdiv0
   fxmod0
   fxdiv0-and-mod0
   fx+/carry
   fx-/carry
   fx*/carry
   fxnot
   fxand
   fxior
   fxxor
   fxif
   fxbit-count
   fxlength
   fxfirst-bit-set
   fxbit-set?
   fxcopy-bit
   fxbit-field
   fxcopy-bit-field
   fxarithmetic-shift
   fxarithmetic-shift-left
   fxarithmetic-shift-right
   fxrotate-bit-field
   fxreverse-bit-field

   #;(rnrs arithmetic flonums (6))
   flonum?
   real->flonum
   fl=?
   fl<?
   fl>?
   fl<=?
   fl>=?
   flinteger?
   flzero?
   flpositive?
   flnegative?
   flodd?
   fleven?
   flfinite?
   flinfinite?
   flnan?
   flmax
   flmin
   fl+
   fl*
   fl-
   fl/
   fldiv-and-mod
   fldiv
   flmod
   fldiv0-and-mod0
   fldiv0
   flmod0
   flnumerator
   fldenominator
   flfloor
   flceiling
   fltruncate
   flround
   flexp
   fllog
   flsin
   flcos
   fltan
   flasin
   flacos
   flatan
   fixnum->flonum
   
   #;(rnrs arithmetic bitwise (6))
   bitwise-not
   bitwise-and
   bitwise-ior
   bitwise-xor
   bitwise-if
   bitwise-bit-count
   bitwise-length
   bitwise-first-bit-set
   bitwise-bit-set?
   bitwise-copy-bit
   bitwise-bit-field
   bitwise-copy-bit-field
   bitwise-arithmetic-shift
   bitwise-arithmetic-shift-left
   bitwise-arithmetic-shift-right
   bitwise-rotate-bit-field
   bitwise-reverse-bit-field

   #;(rnrs syntax-case (6))
   syntax-case syntax
   with-syntax
   make-variable-transformer
   identifier? bound-identifier=? free-identifier=?
   datum->syntax syntax->datum
   generate-temporaries
   quasisyntax
   unsyntax
   unsyntax-splicing
   syntax-violation

   #;(rnrs hashtables (6))
   make-eq-hashtable
   make-eqv-hashtable
   make-hashtable
   hashtable?
   hashtable-size
   hashtable-ref
   hashtable-set!
   hashtable-delete!
   hashtable-contains?
   hashtable-update!
   hashtable-copy
   hashtable-clear!
   hashtable-keys
   hashtable-entries
   hashtable-equivalence-function
   hashtable-hash-function
   hashtable-mutable?
   equal-hash string-hash string-ci-hash symbol-hash)

  (import (rnrs base (6))
          (rnrs unicode (6))
          (rnrs bytevectors (6))
          (rnrs lists (6))
          (rnrs sorting (6))
          (rnrs control (6))
          (rnrs records syntactic (6))
          (rnrs records procedural (6))
          (rnrs records inspection (6))
          (rnrs exceptions (6))
          (rnrs conditions (6))
          (rnrs io ports (6))
          (rnrs io simple (6))
          (rnrs files (6))
          (rnrs programs (6))
          (rnrs arithmetic fixnums (6))
          (rnrs arithmetic flonums (6))
          (rnrs arithmetic bitwise (6))
          (rnrs syntax-case (6))
          (rnrs hashtables (6))
          (rnrs enums (6)))

  ) ;[end]



