#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (tidbits dotimes)
  (export dotimes)
  (import (rnrs))

  (define-syntax dotimes
    (syntax-rules ()
      ((_ (i e1) e2 e3 ...)
       (let ((n e1))
         (let loop ((i 0))
           (if (< i n)
               (begin
                 e2 e3 ...
                 (loop (+ i 1)))))))
      ((_ n e1 e2 ...)
       (let loop ((i n))
         (if (> i 0)
             (begin
               e1 e2 ...
               (loop (- i 1))))))))

  ) ;[end]
