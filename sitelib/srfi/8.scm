#!nobacktrace
(define-library (srfi 8)
  (import (core))
  (export receive)
  (begin
    (define-syntax receive
      (syntax-rules ()
        ((receive formals expression body ...)
         (call-with-values (lambda () expression)
           (lambda formals body ...)))))
