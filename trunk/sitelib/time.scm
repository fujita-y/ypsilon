#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (time)

  (export time
          time-usage
          microsecond
          microsecond->utc
          microsecond->string
          decode-microsecond
          encode-microsecond)

  (import (core))

  (define format.6f
    (lambda (x)
      (/ (round (* x 1000000.0)) 1000000.0)))

  (define-syntax time
    (syntax-rules ()
      ((_ expr)
       (let-values (((real-start user-start sys-start) (time-usage)))
         (let ((result (apply (lambda () expr) '())))
           (let-values (((real-end user-end sys-end) (time-usage)))
             (let ((real (format.6f (- real-end real-start)))
                   (user (format.6f (- user-end user-start)))
                   (sys  (format.6f (- sys-end sys-start))))
               (format #t "~%;;  ~s real    ~s user    ~s sys~%~!" real user sys)))
           result)))))

  ) ;[end]
