#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (tidbits dotimes)
  
  (export dotimes)
  
  (import (core))

  (define-syntax dotimes
    (syntax-rules ()
      ((_ n body1 body2 ...)
       (let loop ((i n))
         (if (> i 0)
             (begin
               body1 body2 ...
               (loop (- i 1))))))))
  
  
  ) ;[end]