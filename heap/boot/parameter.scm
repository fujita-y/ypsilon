;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define make-parameter
  (lambda (init . maybe-filter)
    (let ((parameter (if (null? maybe-filter)
                         (parameter-proc-0 (gensym))
                         (parameter-proc-1 (gensym) (car maybe-filter)))))
      (begin (parameter init) parameter))))

(define parameter-proc-0
  (lambda (key)
    (lambda value
      (if (null? value)
          (core-hashtable-ref  (current-dynamic-environment) key #f)
          (core-hashtable-set! (current-dynamic-environment) key (car value))))))

(define parameter-proc-1
  (lambda (key proc)
    (lambda value
      (if (null? value)
          (core-hashtable-ref  (current-dynamic-environment) key #f)
          (core-hashtable-set! (current-dynamic-environment) key (proc (car value)))))))
