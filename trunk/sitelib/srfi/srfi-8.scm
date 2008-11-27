#!nobacktrace
;;; porting srfi-8 reference implementation to ypsilon -- y.fujita.lwp 2008-11-27

(library (srfi srfi-8)

  (export receive)

  (import (core primitives))

  (define-syntax receive
    (syntax-rules ()
      ((_ formals expr body ...)
       (let-values ((formals expr)) body ...))))

  ) ;[end]

