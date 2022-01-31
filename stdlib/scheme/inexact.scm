;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library (scheme inexact)
  (import (rename (core primitives)
                  (infinite? r6rs:infinite?)
                  (finite? r6rs:finite?)
                  (nan? r6rs:nan?)))
  (export acos asin atan cos exp finite? infinite? log nan? sin sqrt tan)
  (begin
    (define infinite?
      (lambda (z)
        (if (real? z)
            (r6rs:infinite? z)
            (or (r6rs:infinite? (real-part z))
                (r6rs:infinite? (imag-part z))))))
    (define finite?
      (lambda (z)
        (if (real? z)
            (r6rs:finite? z)
            (and (r6rs:finite? (real-part z))
                 (r6rs:finite? (imag-part z))))))
    (define nan?
      (lambda (z)
        (if (real? z)
            (r6rs:nan? z)
            (or (r6rs:nan? (real-part z))
                (r6rs:nan? (imag-part z))))))
    ))
