#!nobacktrace
(define-library (scheme time)
  (import (core primitives))
  (export current-jiffy jiffies-per-second current-second)
  (begin
    (define current-jiffy microsecond)
    (define jiffies-per-second (lambda () 1000000))
    (define current-second (lambda () (/ (microsecond) 1000000.0)))))
