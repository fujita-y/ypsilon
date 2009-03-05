#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon c-enum)
  (export define-c-enum)
  (import (core))

  (define-syntax define-c-enum
    (lambda (x)
      (syntax-case x ()
        ((stx . elts)
         (let ((exact-integer? (lambda (x) (and (integer? x) (exact? x)))))
           (let loop ((n 0) (defs '()) (elts (syntax->datum #'elts)))
             (destructuring-match elts
               (()
                (datum->syntax #'stx (cons 'begin (reverse defs))))
               ((([? symbol? name] . [? exact-integer? n]) . more)
                (loop (+ n 1) (cons `(define ,name ,n) defs) more))
               (([? symbol? name] . more)
                (loop (+ n 1) (cons `(define ,name ,n) defs) more))
               (_
                (syntax-violation 'define-c-enum "invalid syntax" x (car elts))))))))))

  ) ;[end]
