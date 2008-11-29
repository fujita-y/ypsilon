#!nobacktrace
(library (srfi srfi-9)
  (export (rename (define-record-type/srfi-9 define-record-type)))
  (import (core))

  (define-syntax define-record-type/srfi-9
    (lambda (x)
      (define parse
        (lambda (stx)
          (syntax-case stx ()
            ((x y) #'(immutable x y))
            ((x y z) #'(mutable x y z)))))
      (syntax-case x ()
        ((_ type (ctor _ ...)
            pred
            spec ...)
         (with-syntax (((spec ...) (map parse #'(spec ...))))
           #'(define-record-type (type ctor pred)
               (fields spec ...)))))))

  ) ;[end]
