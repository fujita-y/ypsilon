#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core optargs)

  (export let-optionals)

  (import (core intrinsics) (core syntax-case))

  (define-syntax syntax-length
    (lambda (x)
      (syntax-case x ()
        ((_ (lst ...))
         (datum->syntax #'k (length (syntax (lst ...))))))))

  (define-syntax let-optionals-aux
    (syntax-rules ()
      ((_ _ _ (value ...) () ())
       (list value ...))
      ((_ argc args () (var1 ... var2) (def1 ... def2))
       (if (= argc (syntax-length (var1 ... var2)))
           args
           (let ((temp def2))
             (let-optionals-aux argc args (temp) (var1 ...) (def1 ...)))))
      ((_ argc args (value ...) (var) (def))
       (if (= argc 1)
           (append args (list value ...))
           (list def value ...)))
      ((_ argc args (value ...) (var1 ... var2) (def1 ... def2))
       (if (= argc (syntax-length (var1 ... var2)))
           (append args (list value ...))
           (let ((temp def2))
             (let-optionals-aux argc args (temp value ...) (var1 ...) (def1 ...)))))))

  (define-syntax let-optionals
    (syntax-rules ()
      ((_ args ((var def)) body1 body2 ...)
       (let ((var (if (pair? args) (car args) def)))
         body1 body2 ...))
      ((_ args ((var def) ...) body1 body2 ...)
       (apply (lambda (var ...) body1 body2 ...)
              (let ((argc (length args)))
                (let-optionals-aux argc args () (var ...) (def ...)))))))

  ) ;[end]
