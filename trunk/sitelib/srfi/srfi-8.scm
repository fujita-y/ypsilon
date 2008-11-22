#!nobacktrace

(library (srfi srfi-8)

  (export receive)

  (import (core primitives))

  (define-syntax receive
    (syntax-rules ()
      ((_ formals expr body ...)
       (let-values ((formals expr)) body ...))))

  ) ;[end]

