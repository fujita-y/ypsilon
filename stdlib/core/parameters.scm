#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core parameters)

  (export make-parameter parameterize)

  (import (core intrinsics)
          (only (core primitives) make-parameter))

  (define-syntax parameterize-aux
    (syntax-rules ()
      ((_ () ((save new param value) ...) body ...)
       (let ((save #f) ... (new value) ...)
         (dynamic-wind
          (lambda () (set! save (param)) ... (param new) ...)
          (lambda () body ...)
          (lambda () (param save) ...))))
      ((_ ((e1 e2) . more) (stash ...) body ...)
       (parameterize-aux more (stash ... (tmp1 tmp2 e1 e2)) body ...))))

  (define-syntax parameterize
    (syntax-rules ()
      ((_ ((e1 e2) ...) body ...)
       (parameterize-aux ((e1 e2) ...) () body ...))))

  )