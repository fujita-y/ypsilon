;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define make-parameter
  (lambda (init . rest)
    (let ((param
            (destructuring-match rest
              (() (parameter-proc-0))
              ((converter) (parameter-proc-1 converter))
              ((converter thunk) (parameter-proc-2 converter thunk))
              (_
                (assertion-violation
                  'make-parameter
                  (format  "expected ~a to ~a, but ~a arguments given" 1 3 (+ (length rest) 1))
                  (cons* init rest))))))
      (begin (param init) param))))

(define parameter-proc-0
  (lambda ()
    (let ((value #f))
      (lambda args
        (if (null? args)
            value
            (set! value (car args)))))))

(define parameter-proc-1
  (lambda (converter)
    (if converter
        (let ((value #f))
          (lambda args
            (cond ((null? args) value)
                  ((or (null? (cdr args)) (cadr args))
                   (set! value (converter (car args))))
                  (else
                    (set! value (car args))))))
        (parameter-proc-0))))

(define parameter-proc-2
  (lambda (converter thunk)
    (if thunk
        (if converter
            (let ((value #f))
              (lambda args
                (cond ((null? args) value)
                      ((or (null? (cdr args)) (cadr args))
                       (set! value (converter (car args)))
                       (thunk value))
                      (else
                        (set! value (car args)) (thunk value)))))
            (let ((value #f))
              (lambda args
                (cond ((null? args) value)
                      ((or (null? (cdr args)) (cadr args))
                       (set! value (car args))
                       (thunk value))
                      (else
                        (set! value (car args)) (thunk value))))))
        (parameter-proc-1 converter))))
