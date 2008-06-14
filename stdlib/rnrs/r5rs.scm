
(library (rnrs r5rs (6))

  (export exact->inexact inexact->exact
          quotient remainder modulo force delay
          null-environment scheme-report-environment)

  (import (core primitives) (core r5rs))

#;  (define force (lambda (object) (object)))

#;  (define-syntax delay
    (syntax-rules ()
      ((_ expression)
       (let ((value #f) (ready? #f))
         (lambda ()
           (if ready?
               value
               (let ((ans expression))
                 (if ready?
                     value
                     (begin (set! value ans)
                       (set! ready? #t)
                       value)))))))))

  (define null-environment
    (lambda (n)
      (or (= n 5) (assertion-violation 'null-environment (format "expected 5, but got ~r, as argument 2" n)))
      (environment '(only (rnrs r5rs) delay)
                   '(only (core primitives)
                          define quote lambda if set! cond case and or let
                          let* letrec begin do quasiquote
                          define-syntax let-syntax letrec-syntax
                          => ... else _))))

  (define scheme-report-environment
    (lambda (n)
      (or (= n 5) (assertion-violation 'scheme-report-environment (format "expected 5, but got ~r, as argument 2" n)))
      (environment '(rnrs r5rs)
                   '(rnrs eval)
                   '(rnrs mutable-pairs)
                   '(rnrs mutable-strings)
                   '(only (rnrs)
                          define quote lambda if set! cond case and or let
                          let* letrec begin do quasiquote
                          define-syntax let-syntax letrec-syntax
                          => ... else _
                          eqv? eq? equal?
                          number? complex? real? rational? integer? exact? inexact?
                          = < > <= >= zero? positive? negative? odd? even?
                          max min + * - /
                          abs gcd lcm numerator denominator
                          floor ceiling truncate round rationalize
                          exp log sin cos tan asin acos atan sqrt expt
                          make-rectangular make-polar real-part imag-part magnitude angle 
                          number->string string->number
                          not boolean?
                          pair? cons car cdr caar cadr cdar cddr
                          caaar caadr cadar caddr cdaar cdadr cddar cdddr
                          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                          null? list? list length append reverse list-tail list-ref
                          memq memv member assq assv assoc
                          symbol? symbol->string string->symbol
                          char? char=? char<? char>? char<=? char>=?
                          char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
                          char-alphabetic? char-numeric? char-whitespace?
                          char-upper-case? char-lower-case?
                          char->integer integer->char char-upcase char-downcase
                          string? make-string string string-length string-ref
                          string=? string-ci=? string<? string>? string<=? string>=?
                          string-ci<? string-ci>? string-ci<=? string-ci>=?
                          substring string-append string->list list->string
                          string-copy string-fill!
                          vector? make-vector vector vector-length vector-ref vector-set!
                          vector->list list->vector vector-fill!
                          procedure? apply map for-each
                          call-with-current-continuation
                          values call-with-values dynamic-wind
                          call-with-input-file call-with-output-file
                          input-port? output-port? current-input-port current-output-port
                          with-input-from-file with-output-to-file
                          open-input-file open-output-file close-input-port close-output-port
                          read read-char peek-char eof-object?
                          write display newline write-char
                          ))))
  ) ;[end]

