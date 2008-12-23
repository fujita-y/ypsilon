#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core control)

  (export when unless do case-lambda)

  (import (core intrinsics)
          (only (core primitives) do define-syntax)
          (only (core syntax-case)
                syntax-case syntax
                datum->syntax))

  (define-syntax syntax-length
    (lambda (x)
      (syntax-case x ()
        ((_ (lst ...)) (datum->syntax #'k (length (syntax (lst ...))))))))

  (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...)
       (if test
           (begin result1 result2 ...)))))

  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...)
       (if (not test)
           (begin result1 result2 ...)))))

  (define-syntax case-lambda-help
    (syntax-rules ()
      ((_ args n)
       (assertion-violation #f "wrong number of arguments" args))
      ((_ args n ((x ...) b1 b2 ...) more ...)
       (if (= n (syntax-length (x ...)))
           (apply (lambda (x ...) b1 b2 ...) args)
           (case-lambda-help args n more ...)))
      ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
       (if (>= n (syntax-length (x1 x2 ...)))
           (apply (lambda (x1 x2 ... . r) b1 b2 ...) args)
           (case-lambda-help args n more ...)))
      ((_ args n (r b1 b2 ...) more ...)
       (apply (lambda r b1 b2 ...) args))))

  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
         (let ((n (length args)))
           (case-lambda-help args n
                             (fmls b1 b2 ...) ...))))))

  ) ;[end]
