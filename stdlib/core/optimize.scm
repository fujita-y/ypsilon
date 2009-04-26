#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core optimize)

  (export coreform-optimize)

  (import (core primitives)
          (core lists)
          (core destructuring)
          (core parameters))

  (define-syntax define-inline-assistant
    (syntax-rules ()
      ((_ func () (args ...) (vars ...) . body)
       (define-syntax func
         (syntax-rules ()
           ((_ args ...)
            (let ((vars args) ...) . body)))))
      ((_ func (e1 e2 ...) (args ...) . more)
       (define-inline-assistant func (e2 ...) (temp args ...) . more))))

  (define-syntax define-inline
    (syntax-rules (lambda)
      ((_ func (lambda (vars ...) body1 body2 ...))
       (define-inline-assistant func (vars ...) () (vars ...) body1 body2 ...))
      ((_ (func vars ...) body1 body2 ...)
       (define-inline-assistant func (vars ...) () (vars ...) body1 body2 ...))))

  ;;; (define-syntax diagnostics (syntax-rules () ((_ form) form)))
  (define-syntax diagnostics (syntax-rules () ((_ _) #f)))

  (define max-transform-pass 5)
  (define limit-arguments 200)

  ;; no side effects, no i/o, safe to remove expression
  (define ht-primitive-functions
    (let ((ht (make-core-hashtable)))
      (for-each (lambda (e) (core-hashtable-set! ht (top-level-value e) #t))
                '(.eq?
                  .eqv?
                  .equal?
                  .procedure?
                  .number? .complex? .real? .rational? .integer?
                  .real-valued? .rational-valued? .integer-valued?
                  .exact? .inexact?
                  .= .< .> .<= .>=
                  .zero? .positive? .negative? .odd? .even?
                  .finite? .infinite? .nan?
                  .not .boolean? .boolean=? .pair? .null? .list?
                  .symbol? .symbol=?
                  .char? .char=? .char<? .char>? .char<=? .char>=?
                  .string? .string=? .string<? .string>? .string<=? .string>=?
                  .vector?

                  .flonum?
                  .fl=? .fl<? .fl>? .fl<=? .fl>=?
                  .flinteger? .flzero? .flpositive? .flnegative? .flodd? .fleven? .flfinite? .flinfinite? .flnan?
                  .fixnum?
                  .fx=? .fx<? .fx>? .fx<=? .fx>=?
                  .fxzero? .fxpositive? .fxnegative? .fxodd? .fxeven?
                  .identifier?
                  .bound-identifier=? .free-identifier=?
                  .record?
                  .record-type-generative? .record-type-sealed? .record-type-opaque? .record-field-mutable? .record-type-descriptor? .record-type?
                  .condition?
                  .message-condition? .warning? .serious-condition? .error? .violation? .assertion-violation? .irritants-condition? .who-condition?
                  .non-continuable-violation? .implementation-restriction-violation? .lexical-violation? .syntax-violation? .undefined-violation?
                  .char-whitespace?
                  .eof-object?
                  .input-port? .output-port? .port?
                  .nonblock-byte-ready?
                  .port-has-port-position?
                  .port-has-set-port-position!?
                  .port-eof?
                  .i/o-error?
                  .i/o-read-error?
                  .i/o-write-error?
                  .i/o-invalid-position-error?
                  .i/o-filename-error?
                  .i/o-file-protection-error?
                  .i/o-file-is-read-only-error?
                  .i/o-file-already-exists-error?
                  .i/o-file-does-not-exist-error?
                  .i/o-port-error?
                  .i/o-decoding-error?
                  .i/o-encoding-error?
                  .file-exists?
                  .bytevector?
                  .bytevector=?
                  .unspecified?
                  .tuple?
                  .weak-mapping?
                  .core-hashtable?
                  .weak-core-hashtable?
                  .core-hashtable-contains?
                  .core-hashtable-mutable?
                  .top-level-bound?
                  .subr?

                  .inexact .exact
                  .max .min .+ .* .- ./ .abs
                  .div-and-mod .div .mod .div0-and-mod0 .div0 .mod0
                  .gcd .lcm .numerator .denominator
                  .floor .ceiling .truncate .round
                  .rationalize
                  .exp .log .sin .cos .tan .asin .acos .atan
                  .sqrt
                  .exact-integer-sqrt
                  .expt
                  .make-rectangular .make-polar .real-part .imag-part
                  .magnitude .angle
                  .number->string .string->number
                  .cons .car .cdr
                  .caar .cadr .cdar .cddr .caaar .caadr .cadar
                  .caddr .cdaar .cdadr .cddar .cdddr .caaaar .caaadr
                  .caadar .caaddr .cadaar .cadadr .caddar .cadddr .cdaaar
                  .cdaadr .cdadar .cdaddr .cddaar .cddadr .cdddar .cddddr
                  .list .length .append .reverse .list-tail
                  .list-ref
                  .symbol->string .string->symbol
                  .char->integer .integer->char
                  .make-string .string .string-length .string-ref
                  .substring .string-append .string->list .list->string .string-copy
                  .make-vector .vector .vector-length .vector-ref
                  .vector->list .list->vector
                  .values

                  .cons*
                  .memq .memv .member
                  .assq .assv .assoc
                  .list-head .list-copy
                  .circular-list? .cyclic-object?
                  .vector-copy
                  .datum->syntax
                  .syntax->datum
                  .syntax/i0n .syntax/i1n .syntax/i2n .syntax/i3n
                  .syntax/c0n .syntax/c1n .syntax/c2n .syntax/c3n
                  .syntax/i0e .syntax/i1e .syntax/i2e .syntax/i3e
                  .syntax/c0e .syntax/c1e .syntax/c2e .syntax/c3e
                  .string-contains
                  .symbol-contains
                  .top-level-value
                  .unspecified
                  .tuple .make-tuple .tuple-ref .tuple-length .tuple-index .tuple->list
                  .make-core-hashtable
                  .make-weak-core-hashtable
                  .core-hashtable-ref
                  .core-hashtable->alist .core-hashtable-size
                  .core-hashtable-copy
                  .core-hashtable-equivalence-function
                  .core-hashtable-hash-function
                  .current-library-infix
                  .current-library-suffix
                  .current-primitive-prefix
                  .current-rename-delimiter

                  .native-endianness
                  .bytevector?
                  .make-bytevector
                  .bytevector-length
                  .bytevector=?
                  .bytevector-copy
                  .bytevector->u8-list .u8-list->bytevector
                  .bytevector-u8-ref .bytevector-s8-ref
                  .bytevector-u16-ref .bytevector-s16-ref .bytevector-u16-native-ref .bytevector-s16-native-ref
                  .bytevector-u32-ref .bytevector-s32-ref .bytevector-u32-native-ref .bytevector-s32-native-ref
                  .bytevector-u64-ref .bytevector-s64-ref .bytevector-u64-native-ref .bytevector-s64-native-ref
                  .bytevector-ieee-single-ref .bytevector-ieee-single-native-ref
                  .bytevector-ieee-double-ref .bytevector-ieee-double-native-ref
                  .bytevector-c-short-ref
                  .bytevector-c-int-ref
                  .bytevector-c-long-ref
                  .bytevector-c-void*-ref
                  .bytevector-c-unsigned-short-ref
                  .bytevector-c-unsigned-int-ref
                  .bytevector-c-unsigned-long-ref
                  .bytevector-c-int8-ref .bytevector-c-int16-ref .bytevector-c-int32-ref .bytevector-c-int64-ref
                  .bytevector-c-uint8-ref .bytevector-c-uint16-ref .bytevector-c-uint32-ref .bytevector-c-uint64-ref
                  .bytevector-c-float-ref .bytevector-c-double-ref
                  .bytevector-c-strlen
                  .string->utf8/nul
                  ))
      (core-hashtable-copy ht)))

  (define ht-inlinable-primitive-functions
    (let ((ht (make-core-hashtable)))
      (for-each (lambda (e) (core-hashtable-set! ht (top-level-value e) #t))
                '(.car .cdr .cadr .cddr))
      (core-hashtable-copy ht)))

  ; r6rs special list functions that all list arguments are immutable
  (define ht-special-list-functions
    (let ((ht (make-core-hashtable)))
      (for-each (lambda (e) (core-hashtable-set! ht (top-level-value e) #t))
                '(.map .for-each))
      (for-each (lambda (e) (core-hashtable-set! ht e #t))
                (list find for-all exists filter partition fold-left fold-right assp memp remp))
      (core-hashtable-copy ht)))

  (define ht-variable-refc (make-core-hashtable))
  (define ht-variable-binding (make-core-hashtable))
  (define ht-variable-letrec (make-core-hashtable))
  (define ht-variable-defined (make-core-hashtable))
  (define ht-variable-formals (make-core-hashtable))
  (define ht-variable-assigned (make-core-hashtable))
  (define ht-variable-operands-refc (make-core-hashtable))
  (define ht-variable-callsites (make-core-hashtable))
  (define ht-variable-pinned (make-core-hashtable))
  (define ht-variable-privates (make-core-hashtable))
  (define ht-variable-stackables (make-core-hashtable))
  (define ht-lambda-node (make-core-hashtable))
  (define ht-binding-body-common (make-core-hashtable))
  (define ht-binding-body-mutual (make-core-hashtable))

  (define clear-context
    (lambda ()
      (core-hashtable-clear! ht-variable-refc)
      (core-hashtable-clear! ht-variable-binding)
      (core-hashtable-clear! ht-variable-letrec)
      (core-hashtable-clear! ht-variable-defined)
      (core-hashtable-clear! ht-variable-formals)
      (core-hashtable-clear! ht-variable-assigned)
      (core-hashtable-clear! ht-variable-operands-refc)
      (core-hashtable-clear! ht-variable-callsites)
      (core-hashtable-clear! ht-variable-pinned)
      (core-hashtable-clear! ht-variable-privates)
      (core-hashtable-clear! ht-variable-stackables)
      (core-hashtable-clear! ht-lambda-node)
      (core-hashtable-clear! ht-binding-body-common)
      (core-hashtable-clear! ht-binding-body-mutual)))

  (define denote-call/cc (top-level-value '.call-with-current-continuation))

  (define dump-lambda-node
    (lambda ()
      (format #t ">>> dump ht-lambda-node~%")
      (for-each (lambda (b)
                  (format #t "  expr: ~s~%" (car b))
                  (format #t "  free: ~s~%---~%" (cdr b)))
                (core-hashtable->alist ht-lambda-node))))

  (define dump-variable-binding
    (lambda ()
      (format #t ">>> dump ht-variable-binding~%")
      (for-each (lambda (b)
                  (format #t "  variable: ~s~%" (car b))
                  (format #t "  expr: ~s~%" (cdr b))
                  (format #t "  stat: ~s~%---~%" (core-hashtable-ref ht-lambda-node (cdr b) #f)))
                (core-hashtable->alist ht-variable-binding))))

  (define get-free-variables
    (lambda (x)
      (cond ((core-hashtable-ref ht-variable-binding x #f)
             => (lambda (e) (core-hashtable-ref ht-lambda-node e #f)))
            (else #f))))

  (define primitive-function?
    (lambda (x)
      (and (top-level-bound? x)
           (core-hashtable-contains? ht-primitive-functions (top-level-value x)))))

  (define special-list-function?
    (lambda (x)
      (and (top-level-bound? x)
           (core-hashtable-contains? ht-special-list-functions (top-level-value x)))))

  (define inlinable-primitive-function?
    (lambda (x)
      (and (symbol? x)
           (top-level-bound? x)
           (core-hashtable-contains? ht-inlinable-primitive-functions (top-level-value x)))))

  (define variable-top-level?
    (lambda (x)
      (or (core-hashtable-contains? ht-variable-defined x)
          (and (not (core-hashtable-contains? ht-variable-binding x))
               (not (core-hashtable-contains? ht-variable-formals x)))
          (symbol? (get-free-variables x)))))

  (define variable-private?
    (lambda (x)
      (or (uninterned-symbol? x)
          (and (symbol-contains x (current-library-suffix)) #t))))

  #;(define variable-private?
    (lambda (x)
      (and (or (symbol-contains x (current-rename-delimiter))
               (symbol-contains x (current-library-suffix)))
           #t)))

  (define variable-functional?
    (lambda (x)
      (and (or (core-hashtable-contains? ht-variable-privates x)
               (not (core-hashtable-contains? ht-variable-defined x)))
           (cond ((core-hashtable-ref ht-variable-binding x #f)
                  => (lambda (e) (eq? (core-hashtable-ref ht-lambda-node e #f) 'functable)))
                 (else #f)))))

  (define variable-iloc?
    (lambda (x)
      (cond ((core-hashtable-contains? ht-variable-formals x))
            ((core-hashtable-ref ht-variable-binding x #f) => symbol?)
            (else #f))))

  (define function?
    (lambda (form)
      (or (not (pair? form))
          (let ((proc (car form)))
            (or (memq proc '(quote lambda))
                (and (symbol? proc)
                     (or (core-hashtable-contains? ht-variable-privates proc)
                         (not (core-hashtable-contains? ht-variable-defined proc)))
                     (or (primitive-function? proc)
                         (variable-functional? proc)
                         (memq proc '(begin if and or)))
                     (for-all function? (cdr form))))))))

  (define constant?
    (lambda (form)
      (cond ((pair? form)
             (destructuring-match form
               (('begin arg)
                (constant? arg))
               (('quote _) #t)
               (_ (for-all constant? form))))
            (else
             (or (number? form)
                 (boolean? form)
                 (char? form)
                 (string? form)
                 (and (symbol? form)
                      (or (primitive-function? form)
                          (and (core-hashtable-contains? ht-variable-binding form)
                               (not (core-hashtable-contains? ht-variable-assigned form)) ;; 081127
                               (or (core-hashtable-contains? ht-variable-privates form)
                                   (not (core-hashtable-contains? ht-variable-defined form)))))
                      (= (core-hashtable-ref ht-variable-operands-refc form 0) 1)))))))

  (define inlinable-expression?
    (lambda (form)
      (cond ((pair? form)
             (destructuring-match form
               (('begin arg)
                (inlinable-expression? arg))
               (('quote arg)
                (and (not (pair? arg))
                     (or (symbol? arg) (fixnum? arg) (boolean? arg) (char? arg) (string? arg))))
               ((proc arg)
                (and (inlinable-primitive-function? proc)
                     (variable-iloc? arg)))
               (_ #f)))
            (else
             (or (symbol? form) (fixnum? form) (boolean? form) (char? form) (string? form))))))

  (define formals->list
    (lambda (lst)
      (if (pair? lst)
          (cons (car lst) (formals->list (cdr lst)))
          (cond ((null? lst) '())
                (else (list lst))))))

  (define flatten-begin
    (lambda (form)

      (define concatenate?
        (lambda (lst)
          (and (pair? (car lst))
               (or (list? (car lst))
                   (syntax-violation #f "expression is not a proper list" (car lst)))
               (eq? 'begin (caar lst)))))

      ((annotate-hook)
       (let loop ((lst form) (ans '()))
         (cond ((null? lst) ans)
               ((concatenate? lst)
                (loop (cdar lst)
                      (loop (cdr lst) ans)))
               (else
                (cond ((null? ans) lst)
                      (else
                       (append lst ans))))))
       form)))

  (define collect-context-each
    (lambda (form bound free)
      (let loop ((lst form) (free free) (oped #f))
        (cond ((null? lst) free)
              (else
               (collect-context (car lst) bound (loop (cdr lst) free #t) oped))))))

  (define collect-context-seq
    (lambda (form bound free)
      (let loop ((lst form) (free free))
        (cond ((null? lst) free)
              (else
               (collect-context (car lst) bound (loop (cdr lst) free) (null? (cdr lst))))))))

  (define collect-context
    (lambda (form bound free oped)

      (cond ((pair? form)
             (case (car form)
               ((quote define lambda let letrec*)
                (destructuring-match form
                  (('quote _) free)
                  (('define e1 e2)
                   (begin
                     (core-hashtable-set! ht-variable-defined e1 #t)
                     (and (variable-private? e1) (core-hashtable-set! ht-variable-privates e1 #t))
                     (core-hashtable-set! ht-variable-binding e1 (or e2 '(begin #f)))
                     (collect-context e2 bound free oped)))
                  (('lambda e1 . e2)
                   (let ((formals (formals->list e1)))
                     (for-each (lambda (e) (core-hashtable-set! ht-variable-formals e #t)) formals)
                     (let ((free2 (collect-context-seq e2 formals '())))
                       (core-hashtable-set! ht-lambda-node form (remove-duplicate-symbols free2))
                       (append (filter (lambda (e) (not (memq e bound))) free2) free))))
                  (('let e1 . e2)
                   (begin
                     (for-each (lambda (b)
                                 (core-hashtable-set! ht-variable-binding (car b) (or (cadr b) '(begin #f)))
                                 (core-hashtable-set! ht-binding-body-common (car b) e2))
                               e1)
                     (let ((inits-free (collect-context-each (map cadr e1) bound free)))
                       (or (for-all (lambda (x)
                                      (or (core-hashtable-contains? ht-variable-binding x)
                                          (not (and (top-level-bound? x)
                                                    (eq? (top-level-value x) denote-call/cc)))))
                                    inits-free)
                           (for-each (lambda (x) (core-hashtable-set! ht-variable-pinned x #t)) (map car e1)))
                       (let ((bound (append (map car e1) bound)))
                         (collect-context-seq e2 bound inits-free)))))
                  (('letrec* e1 . e2)
                   (begin
                     (for-each (lambda (b)
                                 (core-hashtable-set! ht-variable-letrec (car b) #t)
                                 (core-hashtable-set! ht-variable-binding (car b) (or (cadr b) '(begin #f)))
                                 (core-hashtable-set! ht-binding-body-common (car b) e2)
                                 (or (and (pair? (cadr b)) (eq? 'lambda (caadr b))) (core-hashtable-set! ht-variable-pinned (car b) #t)))
                               e1)

                     (let ((mutual-body (map (lambda (b) (or (cadr b) '(begin #f))) e1)))
                       (for-each (lambda (b) (core-hashtable-set! ht-binding-body-mutual (car b) mutual-body)) e1))

                     (let ((bound (append (map car e1) bound)))
                       (let ((inits-free (collect-context-each (map cadr e1) bound free)))
                         (or (for-all (lambda (x)
                                        (or (core-hashtable-contains? ht-variable-binding x)
                                            (not (and (top-level-bound? x)
                                                      (eq? (top-level-value x) denote-call/cc)))))
                                      inits-free)
                             (for-each (lambda (x) (core-hashtable-set! ht-variable-pinned x #t)) (map car e1)))
                         (collect-context-seq e2 bound inits-free)))))
                  (_ (assertion-violation "coreform-optimize" (format "internal inconsistency in ~s" collect-context) form))))

               ((if)
                (collect-context-each (cdr form) bound free))
               ((set!)
                (core-hashtable-set! ht-variable-assigned (cadr form) #t)
                (collect-context-each form bound free))
               ((begin and or)
                (collect-context-seq (cdr form) bound free))
               (else
                (and (symbol? (car form))
                     (core-hashtable-set! ht-variable-callsites
                                          (car form)
                                          (cons form (core-hashtable-ref ht-variable-callsites (car form) '()))))
                (collect-context-each form bound free))))

            ((symbol? form)
             (and oped (core-hashtable-set! ht-variable-operands-refc form (+ (core-hashtable-ref ht-variable-operands-refc form 0) 1)))
             (core-hashtable-set! ht-variable-refc form (+ (core-hashtable-ref ht-variable-refc form 0) 1))
             (cond ((primitive-function? form) free)
                   ((memq form bound) free)
                   (else (cons form free))))

            (else free))))

  (define crawl-lambda-lifting
    (lambda (form pass)

      (define traverse-variable-binding
        (lambda ()
          (fold-left (lambda (seed b)
                       (cond ((core-hashtable-ref ht-lambda-node (cdr b) #f)
                              => (lambda (free)
                                   (cond ((null? free)
                                          (core-hashtable-set! ht-lambda-node (cdr b) 'functable)
                                          #t)
                                         ((not (pair? free)) seed)
                                         ((and (null? (cdr free)) (eq? (car b) (car free)))
                                          (core-hashtable-set! ht-lambda-node (cdr b) 'functable)
                                          #t)
                                         ((for-all (lambda (id) (or (eq? (car b) id) (variable-functional? id))) free)
                                          (core-hashtable-set! ht-lambda-node (cdr b) 'functable)
                                          #t)
                                         ((for-all (lambda (id) (or (eq? (car b) id) (variable-top-level? id))) free)
                                          (core-hashtable-set! ht-lambda-node (cdr b) 'liftable)
                                          #t)
                                         (else seed))))
                             (else seed)))
                     #f
                     (core-hashtable->alist ht-variable-binding))))

      (define traverse-lambda-node
        (lambda ()
          (for-each (lambda (b)
                      (cond ((core-hashtable-ref ht-lambda-node (car b) #f)
                             => (lambda (free)
                                  (cond ((null? free)
                                         (core-hashtable-set! ht-lambda-node (car b) 'functable))
                                        ((not (pair? free)))
                                        ((for-all (lambda (id) (variable-functional? id)) free)
                                         (core-hashtable-set! ht-lambda-node (car b) 'functable))
                                        ((for-all (lambda (id) (variable-top-level? id)) free)
                                         (core-hashtable-set! ht-lambda-node (car b) 'liftable)))))))
                    (core-hashtable->alist ht-lambda-node))))

      (define resolve-mutual-recursion
        (lambda ()

          (define lift-variables
            (lambda (lst)
              (for-each (lambda (x)
                          (core-hashtable-set!
                           ht-lambda-node
                           (core-hashtable-ref ht-variable-binding x #f)
                           'liftable))
                        lst)))

          (define make-depend-list
            (lambda (x)
              (cond ((get-free-variables x)
                     => (lambda (free)
                          (and (list? free)
                               (let ((free (remp variable-top-level? free)))
                                 (and (pair? free)
                                      (cons x free))))))
                    (else #f))))

          (define make-mutual-list
            (lambda (x depend visited mutual)
              (cond ((memq x visited) mutual)
                    ((assq x depend)
                     => (lambda (lst)
                          (let loop ((lst lst) (mutual mutual))
                            (cond ((null? lst) mutual)
                                  (else
                                   (let ((e (car lst)))
                                     (cond ((eq? x e)
                                            (loop (cdr lst) mutual))
                                           (else
                                            (loop (cdr lst)
                                                  (make-mutual-list e depend (cons x visited) (cons e mutual)))))))))))
                    (else
                     (cons x mutual)))))

          (define list-elts=?
            (lambda (lst1 lst2)
              (and (= (length lst1) (length lst2))
                   (for-all (lambda (e) (memq e lst2)) lst1))))

          #;(format #t ">>>  (resolve-mutual-recursion)~%")
          (let ((depend-list
                 (filter values
                         (map (lambda (b) (make-depend-list (car b)))
                              (core-hashtable->alist ht-variable-binding)))))
            #;(format #t "variable depend list: ~s~%" depend-list)
            (and (pair? depend-list)
                 (let ((mutual-list
                        (map (lambda (e)
                               (cons (car e)
                                     (remove-duplicate-symbols
                                      (make-mutual-list (car e) depend-list '() '()))))
                             depend-list)))
                   #;(format #t "variable mutual list: ~s ~%" mutual-list)
                   (exists (lambda (m)
                             (let ((c1 (cdr m)))
                               (and (pair? c1)
                                    (cond ((for-all (lambda (id)
                                                      (cond ((assq id mutual-list) => (lambda (c2) (list-elts=? c1 (cdr c2))))
                                                            (else (variable-top-level? id))))
                                                    c1)
                                           #;(format #t "make variable ~s liftable~%" c1)
                                           (begin (lift-variables c1) #t))
                                          (else #f)))))
                           mutual-list))))))

      (define make-table
        (lambda ()
          (let ((ht (make-core-hashtable)))
            (for-each (lambda (b) (and (symbol? (cdr b)) (core-hashtable-set! ht (car b) (generate-temporary-symbol))))
                      (core-hashtable->alist ht-lambda-node))
            (for-each (lambda (b)
                        (cond ((core-hashtable-ref ht-lambda-node (cdr b) #f)
                               => (lambda (e)
                                    (cond ((symbol? e)
                                           (core-hashtable-delete! ht (cdr b))
                                           (core-hashtable-set! ht (car b) (cdr b))))))))
                      (core-hashtable->alist ht-variable-binding))
            (for-each (lambda (b)
                        (core-hashtable-delete! ht (car b)))
                      (core-hashtable->alist ht-variable-defined))
            ht)))

      (let loop ()
        (cond ((traverse-variable-binding) (loop))
              ((resolve-mutual-recursion) (loop))))
      (traverse-lambda-node)
      (make-table)))

  ;; assume in order argument evaluation
  (define crawl-beta-subst
    (lambda (form)
      (let ((ht (make-core-hashtable)))
        (for-each
         (lambda (b)
           (or (core-hashtable-contains? ht-lambda-node (cdr b))
               (let ((refc (core-hashtable-ref ht-variable-refc (car b) #f)))
                 (cond ((and refc
                             (or (= refc 1) (inlinable-expression? (cdr b)))
                             (not (core-hashtable-contains? ht-variable-pinned (car b)))
                             (core-hashtable-ref ht-binding-body-common (car b) #f))
                        => (lambda (lst)
                             (let* ((const (or (constant? (cdr b))
                                               (and (symbol? (cdr b))
                                                    (not (core-hashtable-contains? ht-variable-assigned (cdr b)))
                                                    (or (core-hashtable-contains? ht-variable-formals (cdr b))
                                                        (and (core-hashtable-contains? ht-variable-binding (cdr b))
                                                             (or (core-hashtable-contains? ht-variable-privates (cdr b))
                                                                 (not (core-hashtable-contains? ht-variable-defined (cdr b)))))))))
                                    (pure (or const
                                              (and (function? (cdr b))
                                                   (or (not (symbol? (cdr b)))
                                                       (<= (core-hashtable-ref ht-variable-operands-refc (cdr b) 0) 1)))))
                                    (inline (and const
                                                 (inlinable-expression? (cdr b)))))
                               (let ((ans
                                      (call/cc
                                       (lambda (done)
                                         (let loop ((lst lst))
                                           (and (< refc 1) (done #t))
                                           (cond ((null? lst) '())
                                                 ((eq? (car lst) (car b))
                                                  (set! refc (- refc 1))
                                                  (loop (cdr lst)))
                                                 ((pair? (car lst))
                                                  (let ((proc (caar lst)) (args (cdar lst)))
                                                    (cond ((pair? proc)
                                                           (loop (car lst))
                                                           (loop (cdr lst)))
                                                          ((eq? proc 'quote)
                                                           (loop (cdr lst)))
                                                          ((eq? proc 'lambda)
                                                           #;(and const (loop (cdr args)))
                                                           (and inline (loop (cdr args)))
                                                           (loop (cdr lst)))
                                                          ((memq proc '(let letrec*))
                                                           (loop (map cadr (car args)))
                                                           (loop (cdr args))
                                                           (loop (cdr lst)))
                                                          ((memq proc '(begin and or))
                                                           (loop args)
                                                           (loop (cdr lst)))
                                                          ((eq? proc 'set!)
                                                           (or const (done #f))
                                                           (and (eq? (car b) (car args)) (done #f))
                                                           (and (eq? (cdr b) (car args)) (done #f)) ;new
                                                           (loop (cdr args))
                                                           (loop (cdr lst)))
                                                          ((eq? proc 'if)
                                                           (loop (list (car args)))
                                                           (or pure (done #f))
                                                           (loop (list (cadr args)))
                                                           (and (pair? (cddr args)) (loop (list (caddr args))))
                                                           (loop (cdr lst)))
                                                          (else
                                                           (and (eq? proc (car b)) (set! refc (- refc 1)))
                                                           (loop args)
                                                           (or const
                                                               (primitive-function? proc)
                                                               (variable-functional? proc)
                                                               ;
                                                               (and (special-list-function? proc)
                                                                    (pair? args)
                                                                    (symbol? (car args))
                                                                    (or (primitive-function? (car args))
                                                                        (variable-functional? (car args))))
                                                               ;
                                                               (done #f))
                                                           (loop (cdr lst))))))
                                                 (else
                                                  (loop (cdr lst)))))))))
                                 (and (eq? ans #t)
                                      (core-hashtable-set! ht (car b) (cdr b)))))))))))
         (core-hashtable->alist ht-variable-binding))
        (for-each (lambda (b)
                    (and (symbol? (cdr b))
                         (core-hashtable-delete! ht (cdr b))))
                  (core-hashtable->alist ht))
        ht)))

  (define check-stackable-each
    (lambda (var form)
      (let loop ((lst form))
        (or (null? lst)
            (and (check-stackable var (car lst) #f)
                 (loop (cdr lst)))))))

  (define check-stackable-seq
    (lambda (var form tail)
      (let loop ((lst form))
        (or (null? lst)
            (and (check-stackable var (car lst) (and tail (null? (cdr lst))))
                 (loop (cdr lst)))))))

  (define check-stackable
    (lambda (var form tail)
      (cond ((pair? form)
             (case (car form)
               ((lambda)
                (check-stackable-seq var (cddr form) #f))
               ((begin and or)
                (check-stackable-seq var (cdr form) tail))
               ((let)
                (and (for-all (lambda (b) (check-stackable var (cadr b) #f)) (cadr form))
                     (check-stackable-seq var (cddr form) tail)))
               ((letrec*)
                (cond (tail
                       (destructuring-match form
                         ((_ ((def ('lambda _ . body))) (opr . e))
                          (eq? def opr)
                          (and (check-stackable-each var e)
                               (check-stackable-seq var body tail)))
                         (_
                          (and (for-all (lambda (b) (check-stackable var (cadr b) #f)) (cadr form))
                               (check-stackable-seq var (cddr form) tail)))))
                      (else
                       (and (for-all (lambda (b) (check-stackable var (cadr b) #f)) (cadr form))
                            (check-stackable-seq var (cddr form) tail)))))
               ((if)
                (and (check-stackable var (cadr form) #f)
                     (check-stackable var (caddr form) tail)
                     (or (null? (cdddr form)) (check-stackable var (cadddr form) tail))))
               ((quote) #t)
               (else
                (cond ((symbol? (car form))
                       (if (eq? (car form) var)
                           (and tail (check-stackable-each var (cdr form)))
                           (check-stackable-each var (cdr form))))
                      (else
                       (check-stackable-each var form))))))
            (else #t))))

  (define transcribe-each
    (lambda (form lift subst)
      (let loop ((lst form))
        (cond ((null? lst) '())
              (else
               (let ((ea (transcribe (car lst) lift subst)) (ed (loop (cdr lst))))
                 (if (and (eq? ea (car lst)) (eq? ed (cdr lst)))
                     lst
                     (cons ea ed))))))))

  (define transcribe-binding-construct
    (lambda (form lift subst)

      (define-inline emit
        (lambda (new)
          (cond ((eq? new form) new)
                (else ((annotate-hook) new form) new))))

      (let ((binding (cadr form)) (body (cddr form)))
        (let ((vars (map car binding)) (inits (map cadr binding)))
          (let ((flags (if (eq? (car form) 'letrec*)
                           (map (lambda (var)
                                  (or (core-hashtable-contains? subst var)
                                      (core-hashtable-contains? lift var)
                                      (and (core-hashtable-contains? ht-variable-pinned var) var)
                                      (and (core-hashtable-contains? ht-variable-refc var) var)))
                                vars)
                           (map (lambda (var)
                                  (or (core-hashtable-contains? subst var)
                                      (core-hashtable-contains? lift var)
                                      (and (core-hashtable-contains? ht-variable-refc var) var)))
                                vars))))
            (cond ((for-all symbol? flags)
                   (let ((new-body (transcribe-each body lift subst)))
                     (let ((new-inits (transcribe-each inits lift subst)))
                       (cond ((and (for-all eq? inits new-inits) (for-all eq? body new-body)) form)
                             (else (emit `(,(car form) ,(map list vars new-inits) ,@new-body)))))))
                  (else
                   (let ((new-body (transcribe-each body lift subst)))
                     (let-values (((motion binding)
                                   (let loop ((vars vars) (flags flags) (inits inits) (motion '()) (binding '()))
                                     (cond ((null? vars)
                                            (values (reverse motion)
                                                    (reverse binding)))
                                           ((eq? (car flags) #t)
                                            (loop (cdr vars) (cdr flags) (cdr inits)
                                                  motion
                                                  binding))
                                           ((eq? (car flags) #f)
                                            (if (function? (car inits))
                                                (loop (cdr vars) (cdr flags) (cdr inits)
                                                      motion
                                                      binding)
                                                (loop (cdr vars) (cdr flags) (cdr inits)
                                                      (cons (transcribe (car inits) lift subst) motion)
                                                      binding)))
                                           ((symbol? (car flags))
                                            (loop (cdr vars) (cdr flags) (cdr inits)
                                                  motion
                                                  (cons (list (car vars) (transcribe (car inits) lift subst)) binding)))))))
                       (cond ((null? motion)
                              (emit `(,(car form) ,binding ,@new-body)))
                             (else
                              (cond ((eq? (car form) 'let)
                                     (emit `(begin ,@motion (,(car form) ,binding ,@new-body))))
                                    ((eq? (car form) 'letrec*)
                                     (emit `(,(car form) ,binding ,@motion ,@new-body)))
                                    (else
                                     (assertion-violation "coreform-optimize" (format "internal inconsistency in ~s" transcribe-binding-construct) form))))))))))))))

  (define transcribe
    (lambda (form lift subst)

      (define-inline emit
        (lambda (new)
          (cond ((eq? new form) new)
                (else ((annotate-hook) new form) new))))

      (define-inline annotate-closure
        (lambda (new soruce)
          (cond ((eq? new soruce) new)
                (else ((annotate-closure-hook) new soruce) new))))

      (cond ((pair? form)
             (case (car form)
               ((let letrec*)
                (transcribe-binding-construct form lift subst))
               ((quote define lambda if)
                (destructuring-match form
                  (('quote _) form)
                  (('lambda e1 . e2)
                   (cond ((core-hashtable-ref lift form #f) => values)
                         (else
                          (let ((e2a (transcribe-each e2 lift subst)))
                            (cond ((eq? e2 e2a) form)
                                  (else (emit (annotate-closure `(lambda ,e1 ,@e2a) form))))))))
                  (('define e1 ('lambda e2 . e3))
                   (begin
                     (let ((e3a (transcribe-each e3 lift subst)))
                       (cond ((eq? e3 e3a) form)
                             (else
                              (let ((body (annotate-closure `(lambda ,e2 ,@e3a) (caddr form))))
                                (emit `(define ,e1 ,body))))))))
                  (else
                   (emit (transcribe-each form lift subst)))))
               (else
                (emit (transcribe-each form lift subst)))))
            ((symbol? form)
             (or (core-hashtable-ref subst form #f) form))
            (else form))))

  (define self-evaluation?
    (lambda (x)
      (or (number? x) (boolean? x) (char? x) (string? x) (bytevector? x))))

  (define pretty-each
    (lambda (form)
      (let loop ((lst form))
        (cond ((null? lst) '())
              (else
               (let ((ea (pretty (car lst))) (ed (loop (cdr lst))))
                 (if (and (eq? ea (car lst)) (eq? ed (cdr lst)))
                     lst
                     (cons ea ed))))))))

  (define pretty
    (lambda (form)

      (define-inline emit
        (lambda (new)
          (cond ((eq? new form) new)
                (else ((annotate-hook) new form) new))))

      (define-inline annotate-closure
        (lambda (new soruce)
          (cond ((eq? new soruce) new)
                (else ((annotate-closure-hook) new soruce) new))))

      (define flatten-expression
        (lambda (form tag)
          ((annotate-hook)
           (let loop ((lst form) (ans '()))
             (cond ((null? lst) ans)
                   ((and (pair? (car lst)) (eq? tag (caar lst)))
                    (loop (cdar lst)
                          (loop (cdr lst) ans)))
                   (else
                    (let ((ed (loop (cdr lst) ans)))
                      (if (eq? ed (cdr lst))
                          lst
                          (cons (car lst) ed))))))
           form)))

      (define divide
        (lambda (form)
          (let* ((limit (- limit-arguments 1))
                 (args (let loop ((lst '()) (expr (cdr form)))
                         (cond ((> (length expr) limit)
                                (loop (cons (list-head expr limit) lst)
                                      (list-tail expr limit)))
                               (else (reverse (cons expr lst)))))))
            (if (and (symbol? (car form))
                     (top-level-bound? (car form))
                     (eq? (top-level-value (car form)) (top-level-value '.list)))
                (emit (pretty `(.append ,@(map (lambda (e) `(.list ,@e)) args))))
                (emit (pretty `(.apply ,(car form) (.append ,@(map (lambda (e) `(.list ,@e)) args)))))))))

      (if (pair? form)
          (if (pair? (car form))
              (destructuring-match form
                ((('lambda (vars ...) . body) . args)
                 (= (length vars) (length args))
                 (let ((body (flatten-begin (pretty-each body))) (args (pretty-each args)))
                   (emit `(let ,(map list vars args) ,@body))))
                (_
                 (if (> (length form) limit-arguments)
                     (divide form)
                     (pretty-each form))))
              (case (car form)
                ((and)
                 (cond ((null? (cdr form)) #t)
                       ((null? (cddr form)) (emit (cadr form)))
                       (else
                        (let ((new (pretty-each (cdr form))))
                          (cond ((eq? new (cdr form))
                                 (emit (flatten-expression form 'and)))
                                (else
                                 (emit (flatten-expression `(and ,@new) 'and))))))))
                ((or)
                 (cond ((null? (cdr form)) #f)
                       ((null? (cddr form)) (emit (cadr form)))
                       (else
                        (let ((new (pretty-each (cdr form))))
                          (cond ((eq? new (cdr form))
                                 (emit (flatten-expression form 'or)))
                                (else
                                 (emit (flatten-expression `(or ,@new) 'or))))))))
                ((if)
                 (destructuring-match (cdr form)
                   #;((('and . e1) e2 #f)
                    (emit (flatten-expression `(and ,@(pretty-each e1) ,(pretty e2)) 'and)))
                   ((#t e1 . _)
                    (emit (pretty e1)))
                   ((#f _ . e2)
                    (if (null? e2)
                        (emit '(.unspecified))
                        (emit (pretty (car e2)))))
                   ((e1 e2 #f)
                    (emit `(and ,(pretty e1) ,(pretty e2))))
                   ((('not e1) e2 e3)
                    (primitive-function? 'not)
                    (emit `(if ,(pretty e1) ,(pretty e3) ,(pretty e2))))
                   (_
                    (pretty-each form))))
                ((quote begin lambda let letrec*)
                 (destructuring-match form
                   (('quote e1)
                    (if (self-evaluation? e1) e1 form))
                   (('begin . e1)
                    (cond ((null? e1) form)
                          ((and (pair? e1) (null? (cdr e1))) (car e1))
                          (else
                           (let ((new (flatten-begin (pretty-each e1))))
                             (cond ((eq? new e1) form)
                                   (else (emit `(begin ,@new))))))))
                   (('lambda e1 . e2)
                    (let ((e2a (flatten-begin (pretty-each e2))))
                      (if (eq? e2a e2)
                          form
                          (emit (annotate-closure `(lambda ,e1 ,@e2a) form)))))
                   (('let e1 . e2)
                    (cond ((null? e1)
                           (emit `(begin ,@(flatten-begin e2))))
                          ((and (null? (cdr e1)) (null? (cdr e2)) (eq? (caar e1) (car e2)))
                           (emit (pretty (cadar e1))))
                          (else
                           (let ((e1a (map cadr e1)))
                             (let ((e1b (pretty-each e1a))
                                   (e2a (flatten-begin (pretty-each e2))))
                               (cond ((and (eq? e2 e2a) (for-all eq? e1a e1b)) form)
                                     (else (emit `(let ,(map list (map car e1) e1b) ,@e2a)))))))))
                   (('letrec* e1 . e2)
                    (if (null? e1)
                        (emit `(begin ,@(flatten-begin e2)))
                        (let ((e1a (map cadr e1)))
                          (let ((e1b (pretty-each e1a)) (e2a (flatten-begin (pretty-each e2))))
                            (cond ((and (eq? e2 e2a) (for-all eq? e1a e1b)) form)
                                  (else (emit `(letrec* ,(map list (map car e1) e1b) ,@e2a))))))))
                   (_
                    (assertion-violation "coreform-optimize" (format "internal inconsistency in ~s" pretty) form))))
                (else
                 (if (> (length form) limit-arguments)
                     (divide form)
                     (pretty-each form)))))
          form)))

  (define process-stackable
    (lambda (form)

      (define ht-alias (make-core-hashtable))
      (define ht-descendant (make-core-hashtable))

      (define trace-lineage-each
        (lambda (form ancestor)
          (let loop ((lst form))
            (cond ((pair? lst)
                   (trace-lineage (car lst) ancestor)
                   (loop (cdr lst)))))))

      (define trace-lineage
        (lambda (form ancestor)
          (cond ((pair? form)
                 (case (car form)
                   ((define)
                    (cond ((core-hashtable-contains? ht-lambda-node (caddr form))
                           (trace-lineage-each (cddr (caddr form)) ancestor))
                          (else
                           (trace-lineage (caddr form) ancestor))))
                   ((lambda)
                    (for-each (lambda (a) (core-hashtable-set! ht-descendant a #t)) ancestor))
                   ((let)
                    (for-each (lambda (b) (trace-lineage (cadr b) ancestor)) (cadr form))
                    (trace-lineage-each (cddr form) ancestor))
                   ((letrec*)
                    (let ((present (filter values (map (lambda (b) (and (core-hashtable-contains? ht-lambda-node (cadr b)) (car b))) (cadr form)))))
                      (for-each (lambda (b)
                                  (cond ((core-hashtable-contains? ht-lambda-node (cadr b))
                                         (for-each (lambda (a)
                                                     (let ((lst (core-hashtable-ref ht-descendant a '())))
                                                       (and (not (eq? lst #t))
                                                            (core-hashtable-set! ht-descendant a (cons (car b) lst)))))
                                                   ancestor)
                                         (trace-lineage-each (cddadr b) (append present ancestor)))
                                        (else
                                         (trace-lineage (cadr b) (append present ancestor)))))
                                (cadr form))
                      (trace-lineage-each (cddr form) ancestor)))
                   ((quote) #f)
                   (else
                    (trace-lineage-each form ancestor))))
                (else #f))))

      (define contain-heap-lambda?
        (lambda (e)
          (cond ((core-hashtable-ref ht-descendant e #f)
                 => (lambda (lst)
                      (or (eq? lst #t)
                          (not (for-all (lambda (e) (core-hashtable-contains? ht-variable-stackables e)) lst)))))
                (else #f))))

      (trace-lineage form '())
      (for-each (lambda (b)
                  (and (symbol? (cdr b))
                       (core-hashtable-set! ht-alias (cdr b) (car b))))
                (core-hashtable->alist ht-variable-binding))
      #;(for-each (lambda (e) (format #t "~&descendant: ~r => ~r~%" (car e) (cdr e))) (core-hashtable->alist ht-descendant))
      (let ((candidates (remp (lambda (b)
                                (or (core-hashtable-contains? ht-variable-operands-refc (car b))
                                    (core-hashtable-contains? ht-variable-defined (car b))
                                    (core-hashtable-contains? ht-variable-pinned (car b))
                                    (not (core-hashtable-contains? ht-variable-letrec (car b)))
                                    (not (list? (core-hashtable-ref ht-lambda-node (cdr b) #f)))
                                    (not (list? (caddr b)))
                                    (let ((len (+ (length (caddr b)) 1)))
                                      (exists (lambda (e) (not (= (length e) len)))
                                              (core-hashtable-ref ht-variable-callsites (car b) '())))))
                              (core-hashtable->alist ht-variable-binding))))
        #;(format #t "~&process-stackable-lambda candidates ~s~%" (map car candidates))
        (let loop ((prev (core-hashtable-size ht-variable-stackables)))
          (for-each (lambda (b)
                      (or (core-hashtable-contains? ht-variable-stackables (car b))
                          (let ((free (core-hashtable-ref ht-lambda-node (cdr b) '())))
                            #;(format #t "candidates ~s heap-lambda: ~s free: ~s~%" (car b) (contain-heap-lambda? (car b)) free)
                            (cond ((and (not (contain-heap-lambda? (car b)))
                                        (for-all (lambda (x)
                                                   #;(format #t "test veriable: ~s~%" x)
                                                   (or (eq? (car b) x)
                                                       (primitive-function? x)
                                                       (variable-top-level? x)
                                                       (core-hashtable-contains? ht-variable-stackables x)
                                                       (and (variable-iloc? x)
                                                            (not (contain-heap-lambda? x)))))
                                                 free)
                                        )
                                   #;(format #t "~&= local stackable ~s~%" (car b))
                                   (core-hashtable-set! ht-variable-stackables (car b) (cdr b)))))))
                    candidates)

          (let ((new (core-hashtable-size ht-variable-stackables)))
            (or (= prev new) (loop new))))

        (let ((candidates (remp (lambda (b) (core-hashtable-contains? ht-variable-stackables (car b)))
                                candidates)))
          (for-each (lambda (b)
                      (and (check-stackable-seq (car b) (cdddr b) #t)
                           (cond ((core-hashtable-ref ht-binding-body-mutual (car b) #f)
                                  => (lambda (lst)
                                       (for-all (lambda (body) (check-stackable (car b) body #f))
                                                (remq (cdr b) lst))))
                                 (else #t))
                           (check-stackable-seq (car b) (core-hashtable-ref ht-binding-body-common (car b) '()) #t)
                           (begin
                             #;(format #t "~&* tail stackable ~s~%" (car b))
                             (core-hashtable-set! ht-variable-stackables (car b) (cdr b)))))
                    candidates)))))


  (define transform
    (lambda (form)

      (define post-transform
        (lambda (form)
          (process-stackable form)
          (for-each (lambda (b) (closure-attribute-set! (cdr b) 'stack))
                    (core-hashtable->alist ht-variable-stackables))

          (diagnostics
           (let ((stack-count (core-hashtable-size ht-variable-stackables)))
             (cond ((> stack-count 0 )
                    (format #t "~&  stackable closure (~s)~%" stack-count)
                    (format #t "    ~s~%" (map car (core-hashtable->alist ht-variable-stackables)))))))

          form))

      (diagnostics
       (format #t "~&----------~%coreform-optimize:~%  ~r~%    ~n~%" form form))

      (let loop ((form (pretty-form form)) (pass 1)) ; form -> (pretty-form form)
        (clear-context)
        (collect-context form '() '() #f)
        (let ((ht-lift-table (crawl-lambda-lifting form pass))
              (ht-bata-subst-table (crawl-beta-subst form)))

          (diagnostics
           (let ((lift-count (core-hashtable-size ht-lift-table))
                 (subst-count (core-hashtable-size ht-bata-subst-table)))
             (cond ((> lift-count 0 )
                    (format #t "~&  pass ~s top-level lambda (~s)~%" pass lift-count)
                    (for-each (lambda (b) (and (symbol? (car b)) (format #t "    ~s := ~r~%" (car b) (cdr b)))) (core-hashtable->alist ht-lift-table))
                    (for-each (lambda (b) (and (symbol? (cdr b)) (format #t "    ~s := ~r~%" (cdr b) (car b)))) (core-hashtable->alist ht-lift-table))))
             (cond ((> subst-count 0 )
                    (format #t "~&  pass ~s eliminate variable (~s)~%" pass subst-count)
                    (for-each (lambda (b) (format #t "    ~s -> ~r~%" (car b) (cdr b))) (core-hashtable->alist ht-bata-subst-table))))))

          (cond ((and (<= pass max-transform-pass)
                      (or (> (core-hashtable-size ht-lift-table) 0)
                          (> (core-hashtable-size ht-bata-subst-table) 0)
                          (and (= pass 1)
                               (exists (lambda (b) (not (core-hashtable-contains? ht-variable-refc (car b))))
                                       (core-hashtable->alist ht-variable-binding)))))
                 (let ((top-level-defs (cons 'begin
                                             #;(map (lambda (e)
                                                    (let ((lhs (car e)) (rhs (cdr e)))
                                                      (if (symbol? lhs)
                                                          `(define ,lhs ,rhs)
                                                          `(define ,rhs ,lhs))))
                                                  (core-hashtable->alist ht-lift-table))
                                             (filter values
                                                     (map (lambda (e)
                                                            (let ((lhs (car e)) (rhs (cdr e)))

                                                              ;; experimental
                                                              #;(and (core-hashtable-contains? ht-variable-refc lhs)
                                                                   (cond ((core-hashtable-ref ht-variable-callsites lhs #f)
                                                                          => (lambda (lst)
                                                                               (and (pair? rhs)
                                                                                    (pair? (cdr rhs))
                                                                                    (list? (cadr rhs))
                                                                                    (let ((len (+ (length (cadr rhs)) 1)))
                                                                                      (and (for-all (lambda (e) (= (length e) len)) lst)
                                                                                           (closure-attribute-set! rhs 'fastcall))))))))

                                                              (if (symbol? lhs)
                                                                  (and (core-hashtable-contains? ht-variable-refc lhs)
                                                                       `(define ,lhs ,rhs))
                                                                  `(define ,rhs ,lhs))))
                                                          (core-hashtable->alist ht-lift-table))))))
                   (let ((form (cons 'begin
                                     (map (lambda (e)
                                            (let loop ((e e))
                                              #;(format #t ">>> TRANSCRIBE ~r~%" e)
                                              (let ((new (transcribe e ht-lift-table ht-bata-subst-table)))
                                                (cond ((eq? new e) e)
                                                      (else (loop new))))))
                                          (flatten-begin `(,top-level-defs ,form))))))
                     (loop form (+ pass 1)))))
                (else
                 (post-transform form)))))))

  (define pretty-form
    (lambda (form)
      (let loop ((e form))
        (let ((new (pretty e)))
          (cond ((eq? new e) e)
                (else (loop new)))))))

  (define annotate-hook (make-parameter (lambda (new prev) new)))
  (define annotate-closure-hook (make-parameter (lambda (new prev) new)))

  (define closure-attribute-set!
    (lambda (form attr)
      ((annotate-closure-hook) form form attr)))

  (define optimize
    (lambda (form proc1 proc2)
      (if (eq? (coreform-optimize) 0)
          (pretty-form form)
          (parameterize ((annotate-hook proc1) (annotate-closure-hook proc2))
            (let ((form (transform form)))
              (clear-context)
              (pretty-form form))))))

  (define coreform-pretty (make-parameter #t))

  (define coreform-optimize
    (make-parameter
     #t
     (lambda (level)
       (case level
         ((#f) (current-after-expansion-hook (lambda (form proc1 proc2) form)))
         (else (current-after-expansion-hook optimize)))
       level)))

  ) ;[end]
