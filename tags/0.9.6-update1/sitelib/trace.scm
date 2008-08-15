#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (trace)
  
  (export trace untrace with-trace trace-output-port trace-line-length trace-global-indent-mode trace-global-count-mode)
  (import (core) (rnrs))

  (define trace-output-port (make-parameter #t))
  (define trace-line-length (make-parameter 100))
  (define trace-global-indent-mode (make-parameter #t))
  (define trace-global-count-mode (make-parameter #f))
  (define ht-target-procs (make-core-hashtable))
  
  (define glob-level 0)
  
  (define make-trace-rec
    (lambda (proc count depth)
      (vector proc count depth)))

  (define trace-rec-proc (lambda (rec) (vector-ref rec 0)))
  (define trace-rec-count (lambda (rec) (vector-ref rec 1)))
  (define trace-rec-depth (lambda (rec) (vector-ref rec 2)))

  (define trace-rec-count-set! (lambda (rec value) (vector-set! rec 1 value)))
  (define trace-rec-depth-set! (lambda (rec value) (vector-set! rec 2 value)))

  (define-syntax dotimes
    (syntax-rules ()
      ((_ count body ...)
       (let loop ((i count))
         (if (> i 0)
             (begin
               body ...
               (loop (- i 1))))))))

  (define print-call
    (lambda (self-level call)
      (cond ((trace-output-port)
             (format (trace-output-port) "~&C ")
             (if (trace-global-indent-mode)
                 (dotimes (- glob-level 1) (format (trace-output-port) ": "))
                 (dotimes (- self-level 1) (format (trace-output-port) ": ")))
             (parameterize ((restricted-print-line-length (trace-line-length)))
             (format (trace-output-port) "~r~%" call))))))

  (define print-return
    (lambda (self-level return rec)
      (cond ((trace-output-port)
             (format (trace-output-port) "~&R ")
             (if (trace-global-indent-mode)
                 (dotimes (- glob-level 1) (format (trace-output-port) ": "))
                 (dotimes (- self-level 1) (format (trace-output-port) ": ")))
             (parameterize ((restricted-print-line-length (trace-line-length)))
               (for-each (lambda (e) (format (trace-output-port) "~r" e)) return))
             (cond ((= self-level 1)
                    (format (trace-output-port) " ; ~s call:~a level:~a~%" (trace-rec-proc rec) (trace-rec-count rec) (trace-rec-depth rec))
                    (cond ((not (trace-global-count-mode))
                           (trace-rec-count-set! rec 0)
                           (trace-rec-depth-set! rec 0))))
                   (else
                    (format (trace-output-port) "~%")))))))

  (define invoke-proc
    (lambda (self-level rec args)
      (trace-rec-count-set! rec (+ (trace-rec-count rec) 1))
      (trace-rec-depth-set! rec (max (trace-rec-depth rec) self-level))
      (apply (trace-rec-proc rec) args)))

  (define-syntax trace
    (syntax-rules ()
      ((_) (map car (core-hashtable->alist ht-target-procs)))
      ((_ proc1 proc2 ...)
       (begin
         (or (procedure? proc1)  (error 'trace "expected procedure but got ~s" proc1))
         (or (core-hashtable-contains? ht-target-procs proc1)
             (let ((self-level 0) (rec (make-trace-rec proc1 0 0)))
               (set! proc1
                     (lambda args
                       (dynamic-wind
                        (lambda ()
                          (set! self-level (+ self-level 1))
                          (set! glob-level (+ glob-level 1))
                          (print-call self-level (cons 'proc1 args)))
                        (lambda ()
                          (call-with-values
                           (lambda ()
                             (invoke-proc self-level rec args))
                           (lambda return
                             (print-return self-level return rec)
                             (apply values return))))
                        (lambda ()
                          (set! self-level (- self-level 1))
                          (set! glob-level (- glob-level 1))))))
               (core-hashtable-set! ht-target-procs proc1 rec)))
         (trace proc2 ...)))))

  (define-syntax untrace
    (syntax-rules ()
      ((_) (map car (core-hashtable->alist ht-target-procs)))
      ((_ proc1 proc2 ...)
       (begin
         (cond ((core-hashtable-ref ht-target-procs proc1 #f)
                => (lambda (rec)
                     (core-hashtable-delete! ht-target-procs proc1)
                     (set! proc1 (trace-rec-proc rec)))))
         (untrace proc2 ...)))))

  (define-syntax with-trace
    (syntax-rules ()
      ((_ () body ...)
       (let () body ...))
      ((_ (proc ...) body ...)
       (dynamic-wind
        (lambda () (trace proc ...))
        (lambda () body ...)
        (lambda () (untrace proc ...))))))

  ) ;[end]

#|
(begin
  (import (trace))

  (define (tak x y z)
    (if (<= x y)
        z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
             (tak (- z 1) x y))))

  (define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

  (trace tak factorial)
  (tak 5 4 2)
  (factorial 3)
  (untrace tak factorial)
  (tak 5 4 2)
  (factorial 3))

(begin
  (import (trace))
  (define (local-trace n)
    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))
    (format #t "local trace begin~%")
    (with-trace (factorial)
      (factorial n))
    (format #t "local trace end~%"))
  (local-trace 4))

(library (foo bar)
  (export library-trace)
  (import (rnrs) (trace))

  (define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

  (define library-trace
    (lambda (n)
      (factorial n)))

  (trace factorial))

(import (foo bar))
(library-trace 10)

(begin
  (import (trace))
  (define (ctak x y z)
    (call-with-current-continuation
     (lambda (k) (ctak-aux k x y z))))
  (define (ctak-aux k x y z)
    (if (not (< y x))
        (k z)
        (call-with-current-continuation
         (lambda (k)
           (ctak-aux
            k
            (call-with-current-continuation
             (lambda (k) (ctak-aux k (- x 1) y z)))
            (call-with-current-continuation
             (lambda (k) (ctak-aux k (- y 1) z x)))
            (call-with-current-continuation
             (lambda (k) (ctak-aux k (- z 1) x y))))))))
  (trace ctak-aux)
  (ctak 18 12 6))
|#
