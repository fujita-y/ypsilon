#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core r5rs)

  (export (rename (inexact exact->inexact) (exact inexact->exact))
          quotient
          remainder
          modulo
          delay
          force)

  (import (core primitives))

  (define force
    (lambda (object)
      (object)))

  (define make-promise
    (lambda (proc)
      (let ((result-ready? #f)
            (result #f))
        (lambda ()
          (if result-ready?
              result
              (let ((x (proc)))
                (if result-ready?
                    result
                    (begin (set! result-ready? #t)
                      (set! result x)
                      result))))))))

  (define-syntax delay
    (syntax-rules ()
      ((delay expression)
       (make-promise (lambda () expression)))))

  ) ;[end]
