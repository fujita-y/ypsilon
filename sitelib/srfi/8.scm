#!nobacktrace
(define-library (srfi 8)
  (import (core))
  (export receive)
  (begin
    (define-syntax receive
      (syntax-rules ()
        ((_ formals expr body ...)
         (let-values ((formals expr)) body ...))))))
