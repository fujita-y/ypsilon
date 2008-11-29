#!nobacktrace
;;; porting srfi-8 reference implementation to ypsilon
;;; -- y.fujita.lwp

(library (srfi srfi-8)
  (export receive)
  (import (core))

  (define-syntax receive
    (syntax-rules ()
      ((_ formals expr body ...)
       (let-values ((formals expr)) body ...))))

  ) ;[end]
