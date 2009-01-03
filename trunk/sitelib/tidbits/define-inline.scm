#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (tidbits define-inline)
  (export define-inline)
  (import (rnrs))

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

  ) ;[end]
