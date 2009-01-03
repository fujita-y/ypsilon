#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core chkarg)

  (export check-argument)

  (import (core primitives) (core syntax-case))

  (define-syntax check-argument
    (lambda (x)
      (syntax-case x ()
        ((_ e1 . e2)
         (let ((e (syntax->datum #'e1))) (symbol? e))
         (syntax (check-argument 0 e1 . e2)))
        ((_ 0 who arg expect test)
         (pair? (syntax->datum #'test))
         (with-syntax ((msg (datum->syntax #'k (format "expected ~a, but got ~a" (syntax->datum #'expect) "~s"))))
           (syntax (or test (assertion-violation 'who (format msg arg))))))
        ((_ n who arg expect test)
         (pair? (syntax->datum #'test))
         (with-syntax ((msg (datum->syntax #'k (format "expected ~a, but got ~a as argument ~a" (syntax->datum #'expect) "~s" (syntax->datum #'n)))))
           (syntax (or test (assertion-violation 'who (format msg arg))))))
        ((_ n who arg expect test)
         (symbol? (syntax->datum #'test))
         (syntax (check-argument n who arg expect (test arg))))
        (_ (syntax-violation 'check-argument "expected 4 or 5 clauses (<position> who arg expect test)" x)))))

  ) ;[end]
