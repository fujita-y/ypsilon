;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library (scheme base)
  (import (rename (except (core)
                          for-each map member assoc string->list string-copy string-fill!
                          vector->list vector-fill! bytevector-copy vector-map)
                  (define-record-type r6rs:define-record-type)
                  (flush-output-port r6rs:flush-output-port)
                  (expt r6rs:expt)
                  (error r6rs:error)
                  (string->utf8 r6rs:string->utf8)
                  (utf8->string r6rs:utf8->string)
                  (bytevector-copy! r6rs:bytevector-copy!)
                  (let-syntax r6rs:let-syntax)
                  (letrec-syntax r6rs:letrec-syntax)))

  (export *
          +
          -
          ...
          /
          <
          <=
          =
          =>
          >
          >=
          _
          abs
          and
          append
          apply
          assoc
          assq
          assv
          begin
          binary-port?
          boolean=?
          boolean?
          bytevector
          bytevector-append
          bytevector-copy
          bytevector-copy!
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector?
          caar
          cadr
          call-with-current-continuation
          call-with-port
          call-with-values
          call/cc
          car
          case
          cdar
          cddr
          cdr
          ceiling
          char->integer
          char-ready?
          char<=?
          char<?
          char=?
          char>=?
          char>?
          char?
          close-input-port
          close-output-port
          close-port
          complex?
          cond
          cond-expand
          cons
          current-error-port
          current-input-port
          current-output-port
          define
          define-record-type
          define-syntax
          define-values
          denominator
          do
          dynamic-wind
          else
          eof-object
          eof-object?
          eq?
          equal?
          eqv?
          error
          error-object-irritants
          error-object-message
          error-object?
          even?
          exact
          exact-integer-sqrt
          exact-integer?
          exact?
          expt
          features
          file-error?
          floor
          floor-quotient
          floor-remainder
          floor/
          flush-output-port
          for-each
          gcd
          get-output-bytevector
          get-output-string
          guard
          if
          include
          include-ci
          inexact
          inexact?
          input-port-open?
          input-port?
          integer->char
          integer?
          lambda
          lcm
          length
          let
          let*
          let*-values
          let-syntax
          let-values
          letrec
          letrec*
          letrec-syntax
          list
          list->string
          list->vector
          list-copy
          list-ref
          list-set!
          list-tail
          list?
          make-bytevector
          make-list
          make-parameter
          make-string
          make-vector
          map
          max
          member
          memq
          memv
          min
          modulo
          negative?
          newline
          not
          null?
          number->string
          number?
          numerator
          odd?
          open-input-bytevector
          open-input-string
          open-output-bytevector
          open-output-string
          or
          output-port-open?
          output-port?
          pair?
          parameterize
          peek-char
          peek-u8
          port?
          positive?
          procedure?
          quasiquote
          quote
          quotient
          raise
          raise-continuable
          rational?
          rationalize
          read-bytevector
          read-bytevector!
          read-char
          read-error?
          read-line
          read-string
          read-u8
          real?
          remainder
          reverse
          round
          set!
          set-car!
          set-cdr!
          square
          string
          string->list
          string->number
          string->symbol
          string->utf8
          string->vector
          string-append
          string-copy
          string-copy!
          string-fill!
          string-for-each
          string-length
          string-map
          string-ref
          string-set!
          string<=?
          string<?
          string=?
          string>=?
          string>?
          string?
          substring
          symbol->string
          symbol=?
          symbol?
          syntax-error
          syntax-rules
          textual-port?
          truncate
          truncate-quotient
          truncate-remainder
          truncate/
          u8-ready?
          unless
          unquote
          unquote-splicing
          utf8->string
          values
          vector
          vector->list
          vector->string
          vector-append
          vector-copy
          vector-copy!
          vector-fill!
          vector-for-each
          vector-length
          vector-map
          vector-ref
          vector-set!
          vector?
          when
          with-exception-handler
          write-bytevector
          write-char
          write-string
          write-u8
          zero?)
  (begin
    (define expt
      (lambda (z1 z2)
        (let ((z (r6rs:expt z1 z2)))
          (cond ((or (inexact? z1) (inexact? z2))
                 (inexact z))
                (else z)))))

    (define vector-map
      (lambda (proc vec1 . vec2)
        (list->vector
         (apply map proc (vector->list vec1)
                (map vector->list vec2)))))

    (define string->utf8
      (lambda (str . options)
        (let-optionals options ((start 0) (end (string-length str)))
          (r6rs:string->utf8 (substring str start end)))))

    (define utf8->string
      (lambda (bv . options)
        (let-optionals options ((start 0) (end (bytevector-length bv)))
          (r6rs:utf8->string (bytevector-copy bv start end)))))

    (define bytevector-copy!
      (lambda (to at from . options)
        (let-optionals options ((start 0) (end (bytevector-length from)))
          (r6rs:bytevector-copy! from start to at (- end start)))))

    (define bytevector-copy
      (lambda (bv . options)
        (let-optionals options ((start 0) (end (bytevector-length bv)))
          (let ((ans (make-bytevector (- end start))))
            (r6rs:bytevector-copy! bv start ans 0 (- end start))
            ans))))

    (define string-fill!
      (lambda (str char . options)
        (let-optionals options ((start 0) (end (string-length str)))
          (let loop ((i (- end 1)))
            (if (>= i start)
                (begin
                  (string-set! str i char)
                  (loop (- i 1))))))))

    (define string-copy
      (lambda (str . options)
        (let-optionals options ((start 0) (end (string-length str)))
          (substring str start end))))

    (define string->list
      (lambda (str . options)
        (let-optionals options ((start 0) (end (string-length str)))
          (let loop ((i (- end 1)) (ans '()))
            (if (< i start)
                ans
                (loop (- i 1) (cons (string-ref str i) ans)))))))

    (define vector-fill!
      (lambda (vec obj . options)
        (let-optionals options ((start 0) (end (vector-length vec)))
          (let loop ((i (- end 1)))
            (if (>= i start)
                (begin
                  (vector-set! vec i obj)
                  (loop (- i 1))))))))

    (define vector->list
      (lambda (vec . options)
        (let-optionals options ((start 0) (end (vector-length vec)))
          (let loop ((i (- end 1)) (ans '()))
            (if (< i start)
                ans
                (loop (- i 1) (cons (vector-ref vec i) ans)))))))

    (define assoc
      (lambda (key lst . options)
        (let-optionals options ((proc equal?))
          (find (lambda (e) (proc key (car e))) lst))))

    (define find-tail
      (lambda (proc lst)
        (let loop ((lst lst))
          (cond ((null? lst) #f)
                ((proc (car lst)) lst)
                (else (loop (cdr lst)))))))

    (define member
      (lambda (x lst . options)
        (let-optionals options ((proc equal?))
          (find-tail (lambda (e) (proc x e)) lst))))

    (define for-each-1
      (lambda (proc lst)
        (cond ((null? lst) (unspecified))
              (else
               (proc (car lst))
               (for-each-1 proc (cdr lst))))))

    (define for-each-n
      (lambda (proc lst)
        (cond ((null? lst) (unspecified))
              (else
               (apply proc (car lst))
               (for-each-n proc (cdr lst))))))

    (define for-each
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (for-each-1 proc lst1)
            (for-each-n proc (apply list-transpose* lst1 lst2)))))

    (define map-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else (cons (proc (car lst))
                          (map-1 proc (cdr lst)))))))

    (define map-n
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else (cons (apply proc (car lst))
                          (map-n proc (cdr lst)))))))

    (define map
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (map-1 proc lst1)
            (map-n proc (apply list-transpose* lst1 lst2)))))

    (define bytevector
      (lambda args
        (u8-list->bytevector args)))

    (define bytevector-append
      (lambda args
        (let ((ans (make-bytevector (apply + (map bytevector-length args)))))
          (let loop ((args args) (p 0))
            (or (null? args)
                (let ((n (bytevector-length (car args))))
                  (r6rs:bytevector-copy! (car args) 0 ans p n)
                  (loop (cdr args) (+ p n)))))
          ans)))

    (define char-ready?
      (lambda options
        (let-optionals options ((port (current-input-port)))
          (not (eof-object? (lookahead-char port))))))

    (define error
      (lambda (message . irritants)
        (apply r6rs:error #f message irritants)))

    (define error-object?
      (lambda (obj)
        (or (error? obj) (violation? obj))))

    (define error-object-irritants
      (lambda (obj)
        (and (irritants-condition? obj) (condition-irritants obj))))

    (define error-object-message
      (lambda (obj)
        (and (message-condition? obj) (condition-message obj))))

    (define exact-integer?
      (lambda (obj)
        (and (integer? obj) (exact? obj))))

    (define features
      (lambda ()
        (feature-identifies)))

    (define file-error?
      (lambda (obj)
        (i/o-filename-error? obj)))

    (define floor-remainder modulo)

    (define floor-quotient
      (lambda (n m)
        (floor (/ n m))))

    (define floor/
      (lambda (n m)
        (values (floor-quotient n m) (modulo n m))))

    (define get-output-bytevector
      (lambda (port)
        (get-accumulated-bytevector port)))

    (define get-output-string
      (lambda (port)
        (get-accumulated-string port)))

    (define input-port-open?
      (lambda (port)
        (and (not (port-closed? port)) (input-port? port))))

    (define list-set!
      (lambda (lst k obj)
        (let loop ((lst lst) (k k))
          (cond ((null? lst)
                 (assertion-violation 'list-set! (format "index out of range, ~s as argument 2" k) (list lst k obj)))
                ((> k 0)
                 (loop (cdr lst) (- k 1)))
                (else
                 (set-car! lst obj))))))

    (define open-input-bytevector
      (lambda (bv)
        (open-bytevector-input-port bv)))

    (define open-input-string
      (lambda (str)
        (open-string-input-port str)))

    (define open-output-bytevector
      (lambda ()
        (call-with-values
         open-bytevector-output-port
         (lambda (port proc) port))))

    (define open-output-string
      (lambda ()
        (call-with-values
         open-string-output-port
         (lambda (port proc) port))))

    (define output-port-open?
      (lambda (port)
        (and (not (port-closed? port)) (output-port? port))))

    (define peek-u8
      (lambda options
        (let-optionals options ((port (current-input-port)))
          (lookahead-u8 port))))

    (define read-bytevector
      (lambda (k . options)
        (let-optionals options ((port (current-input-port)))
          (get-bytevector-n port k))))

    (define read-bytevector!
      (lambda (bv . options)
        (let-optionals options ((port (current-input-port)) (start 0) (end (bytevector-length bv)))
          (get-bytevector-n! port bv start (- end start)))))

    (define read-error?
      (lambda (obj)
        (or (i/o-read-error? obj) (lexical-violation? obj))))

    (define read-line
      (lambda options
        (let-optionals options ((port (current-input-port)))
          (get-line port))))

    (define read-string
      (lambda (k . options)
        (let-optionals options ((port (current-input-port)))
          (get-string-n port k))))

    (define read-u8
      (lambda options
        (let-optionals options ((port (current-input-port)))
          (get-u8 port))))

    (define square
      (lambda (z) (* z z)))

    (define string-copy!
      (lambda (to at from . options)
        (let-optionals options ((start 0) (end (string-length from)))
          (if (> start at)
              (let loop ((at at) (start start))
                (cond ((< start end)
                       (string-set! to at (string-ref from start))
                       (loop (+ at 1) (+ start 1)))))
              (let loop ((at (- (+ at end) start 1)) (end (- end 1)))
                (cond ((<= start end)
                       (string-set! to at (string-ref from end))
                       (loop (- at 1) (- end 1)))))))))

    (define string-map
      (lambda (proc str1 . str2)
        (if (null? str2)
            (list->string (map proc (string->list str1)))
            (list->string (apply map proc (string->list str1) (map string->list str2))))))

    (define string->vector
      (lambda (str . options)
        (let-optionals options ((start 0) (end (string-length str)))
          (apply vector (string->list (substring str start end))))))

    (define truncate-quotient quotient)

    (define truncate-remainder remainder)

    (define truncate/
      (lambda (n m)
        (values (quotient n m) (remainder n m))))

    (define u8-ready?
      (lambda options
        (let-optionals options ((port (current-input-port)))
          (not (eof-object? (lookahead-u8 port))))))

    (define vector-append
      (lambda args
        (let ((ans (make-vector (apply + (map vector-length args)))))
          (let loop ((at 0) (args args))
            (cond ((null? args) ans)
                  (else
                   (vector-copy! ans at (car args))
                   (loop (+ at (vector-length (car args))) (cdr args))))))))

    (define vector-copy!
      (lambda (to at from . options)
        (let-optionals options ((start 0) (end (vector-length from)))
          (if (> start at)
              (let loop ((at at) (start start))
                (cond ((< start end)
                       (vector-set! to at (vector-ref from start))
                       (loop (+ at 1) (+ start 1)))))
              (let loop ((at (- (+ at end) start 1)) (end (- end 1)))
                (cond ((<= start end)
                       (vector-set! to at (vector-ref from end))
                       (loop (- at 1) (- end 1)))))))))

    (define vector->string
      (lambda (vec . options)
        (let-optionals options ((start 0) (end (vector-length vec)))
          (let loop ((start start) (acc '()))
            (if (< start end)
                (loop (+ start 1) (cons (vector-ref vec start) acc))
                (list->string (reverse acc)))))))

    (define write-bytevector
      (lambda (bv . options)
        (let-optionals options ((port (current-output-port)) (start 0) (end (bytevector-length bv)))
          (put-bytevector port bv start (- end start)))))

    (define write-string
      (lambda (str . options)
        (let-optionals options ((port (current-output-port)) (start 0) (end (string-length str)))
          (put-string port str start (- end start)))))

    (define write-u8
      (lambda (b . options)
        (let-optionals options ((port (current-output-port)))
          (put-u8 port b))))

    (define-syntax syntax-error
      (syntax-rules ()
        ((_ msg args ...)
         (syntax-violation #f msg '(args ...)))))

    (define flush-output-port
      (lambda options
        (let-optionals options ((port (current-output-port)))
          (r6rs:flush-output-port port))))

    (define-syntax cond-expand
      (lambda (x)
        (syntax-case x (else)
          ((_)
           #'(begin))
          ((_ (else body ...))
           #'(begin body ...))
          ((_ (else body ...) more ...)
           (syntax-violation 'cond-expand "misplaced else" x))
          ((_ (conditions body ...) more ...)
           (if (fulfill-feature-requirements? x (syntax->datum #'conditions))
               #'(begin body ...)
               #'(cond-expand more ...))))))

    (define-syntax define-values
      (lambda (x)
        (syntax-case x ()
          ((_ var expr)
           (identifier? (syntax var))
           (syntax (define var (call-with-values (lambda () expr) list))))
          ((_ (var ...) expr)
           (with-syntax (((i ...) (iota (length (syntax (var ...))))))
             (syntax
              (begin
                (define temp (call-with-values (lambda () expr) vector))
                (define var (vector-ref temp i)) ...))))
          ((_ (var ... . var2) expr)
           (with-syntax (((i ...) (iota (length (syntax (var ...))))) (n (length (syntax (var ...)))))
             (syntax
              (begin
                (define temp (call-with-values (lambda () expr) list))
                (define var (list-ref temp i)) ...
                (define var2 (list-tail temp n)))))))))

    (define-syntax let-syntax
      (syntax-rules ()
        ((_ e1 e2 ...)
         (r6rs:let-syntax e1
                          (let () e2 ...)))))

    (define-syntax letrec-syntax
      (syntax-rules ()
        ((_ e1 e2 ...)
         (r6rs:letrec-syntax e1
                             (let () e2 ...)))))

    (define-syntax define-record-type
      (lambda (x)
        (define parse-spec
          (lambda (spec)
            (syntax-case spec ()
              ((x y) (syntax (immutable x y)))
              ((x y z) (syntax (mutable x y z))))))
        (syntax-case x ()
          ((_ type (ctor constructor-tag ...) pred spec ...)
           (with-syntax ((((field-tag _ ...) ...)
                          (syntax (spec ...)))
                         ((field-spec ...)
                          (map parse-spec (syntax (spec ...)))))
             (let ((ctags (syntax (constructor-tag ...))))
               (with-syntax (((constructor-arg ...)
                              (map (lambda (ftag)
                                     (if (find (lambda (e) (bound-identifier=? e ftag)) ctags)
                                         ftag
                                         (syntax (unspecified))))
                                   (syntax (field-tag ...)))))
                 (syntax
                   (r6rs:define-record-type
                     (type ctor pred)
                     (protocol
                       (lambda (ctor)
                         (lambda (constructor-tag ...)
                           (ctor constructor-arg ...))))
                     (fields field-spec ...))))))))))))
