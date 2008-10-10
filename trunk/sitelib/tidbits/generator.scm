#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (tidbits generator)
  (export define-generator)
  (import (rnrs) (only (core) format))

  (define unexpected-return
    (lambda (name)
      (assertion-violation
       'iteration
       (format "unexpected return of generator function ~s" name))))

  (define-syntax define-generator
    (lambda (x)
      (syntax-case x (lambda)
        ((stx name (lambda formals e0 e1 ...))
         (with-syntax ((yield (datum->syntax #'stx 'yield)))
           #'(define name
               (lambda formals
                 (let ((resume #f) (return #f))
                   (define yield
                     (lambda args
                       (call/cc
                        (lambda (cont)
                          (set! resume cont)
                          (apply return args)))))
                   (lambda ()
                     (call/cc
                      (lambda (cont)
                        (set! return cont)
                        (cond (resume (resume))
                              (else
                               (let () e0 e1 ...)
                               (unexpected-return 'name)))))))))))
        ((stx (name . formals) e0 e1 ...)
         #'(stx name (lambda formals e0 e1 ...))))))

  ) ;[end]
