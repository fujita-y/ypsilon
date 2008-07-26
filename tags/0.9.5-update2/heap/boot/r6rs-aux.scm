;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define max
  (lambda args
    (cond ((null? args)
           (assertion-violation 'max "required at least 1, but 0 argument given"))
          ((real-valued? (car args))
           (let loop ((value (car args)) (x? (inexact? (car args))) (lst (cdr args)))
             (cond ((null? lst)
                    (if x? (inexact value) value))
                   ((real-valued? (car lst))
                    (loop (if (> (car lst) value) (car lst) value)
                          (or x? (inexact? (car lst)))
                          (cdr lst)))
                   (else
                    (assertion-violation 'max (format "expected real, but got ~s" (car lst)) args)))))
          (else
           (assertion-violation 'max (format "expected real, but got ~s" (car lst)) args)))))

(define min
  (lambda args
    (cond ((null? args)
           (assertion-violation 'min "required at least 1, but 0 argument given"))
          ((real-valued? (car args))
           (let loop ((value (car args)) (x? (inexact? (car args))) (lst (cdr args)))
             (cond ((null? lst)
                    (if x? (inexact value) value))
                   ((real-valued? (car lst))
                    (loop (if (< (car lst) value) (car lst) value)
                          (or x? (inexact? (car lst)))
                          (cdr lst)))
                   (else
                    (assertion-violation 'min (format "expected real, but got ~s" (car lst)) args)))))
          (else
           (assertion-violation 'min (format "expected real, but got ~s" (car lst)) args)))))

(define gcd2
  (lambda (a b)
    (if (= b 0)
        (abs (if (inexact? b) (inexact a) a))
        (gcd2 b (remainder a b)))))
    
(define gcd
  (lambda args
    (for-each (lambda (a)
                (or (integer-valued? a)
                    (assertion-violation 'gcd (format "expected integer, but got ~s" a) args)))
              args)
    (let loop ((lst args))
      (case (length lst)
        ((2) (gcd2 (car lst) (cadr lst)))
        ((1) (abs (car lst)))
        ((0) 0)
        (else (loop (cons (gcd2 (car lst) (cadr lst)) (cddr lst))))))))

(define lcm
  (lambda args
    
    (define lcm2
      (lambda (a b)
        (if (or (= a 0) (= b 0))
            (if (and (exact? a) (exact? b)) 0 0.0)
            (abs (* (quotient a (gcd2 a b)) b)))))
    
    (for-each (lambda (a)
                (or (integer-valued? a)
                    (assertion-violation 'lcm (format "expected integer, but got ~s" a) args)))
              args)
    (let loop ((lst args))
      (case (length lst)
        ((2) (lcm2 (car lst) (cadr lst)))
        ((1) (abs (car lst)))
        ((0) 1)
        (else (loop (cons (lcm2 (car lst) (cadr lst)) (cddr lst))))))))

(define rationalize
  (lambda (x e)
    (or (real? x) (assertion-violation 'rationalize (format "expected real, but got ~s as argument 1" x) (list x e)))
    (or (real? e) (assertion-violation 'rationalize (format "expected real, but got ~s as argument 2" e) (list x e)))
    (cond ((infinite? e) 
           (if (infinite? x) +nan.0 0.0))
          ((= x 0) 0)
          ((negative? x)
           (- (rationalize (- x) e)))
          (else
           (let ((e (abs e)))
             (let loop ((bottom (- x e)) (top (+ x e)))
               (cond ((= bottom top) bottom)
                     (else
                      (let ((x (ceiling bottom)))
                        (cond ((< x top) x)
                              (else
                               (let ((a (- x 1)))
                                 (+ a (/ 1 (loop (/ 1 (- top a)) (/ 1 (- bottom a)))))))))))))))))

#;(define list->string
  (lambda (lst) (apply string lst)))

#;(define list->vector 
  (lambda (lst) (apply vector lst)))

#;(define string->list
  (lambda (s)
    (let loop ((i (- (string-length s) 1)) (lst '()))
      (if (< i 0)
          lst
          (loop (- i 1) (cons (string-ref s i) lst))))))

(define string->list
  (lambda (s)
    (let ((port (make-string-input-port s #t)))
      (let loop ((lst '()))
        (let ((ch (get-char port)))
          (if (eof-object? ch)
              (reverse lst)
              (loop (cons ch lst))))))))

(define map
  (lambda (proc lst1 . lst2)
    
    (define map-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
               (cons (proc (car lst))
                     (map-1 proc (cdr lst)))))))

    (define map-n
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
               (cons (apply proc (car lst))
                     (map-n proc (cdr lst)))))))
    
    (if (null? lst2)
        (if (list? lst1)
            (map-1 proc lst1)
            (assertion-violation 'map (wrong-type-argument-message "proper list" lst1 2) (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (map-n proc lst)))
              (else
               (assertion-violation 'map "expected same length proper lists" (cons* proc lst1 lst2)))))))

(define for-each
  (lambda (proc lst1 . lst2)
    (define for-each-1 (lambda (proc lst)
                         (if (null? lst)
                             (unspecified)
                             (begin
                               (proc (car lst))
                               (for-each-1 proc (cdr lst))))))
    (define for-each-n (lambda (proc lst)
                         (cond ((null? lst) (unspecified))
                               (else
                                (apply proc (car lst))
                                (for-each-n proc (cdr lst))))))
    (if (null? lst2)
        (if (list? lst1)
            (for-each-1 proc lst1)
            (assertion-violation 'for-each (wrong-type-argument-message "proper list" lst1 2) (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (for-each-n proc lst)))
              (else
               (assertion-violation 'for-each "expected same length proper lists" (cons* proc lst1 lst2)))))))

(define vector-map
  (lambda (proc vec1 . vec2)
    (list->vector
     (apply map proc (vector->list vec1)
            (map vector->list vec2)))))

(define vector-for-each
  (lambda (proc vec1 . vec2)
    (apply for-each proc (vector->list vec1)
           (map vector->list vec2))))

(define string-for-each
  (lambda (proc str1 . str2)
    (apply for-each proc (string->list str1)
           (map string->list str2))))

(define call-with-values
  (lambda (producer consumer)
    (apply-values consumer (producer))))

(define mod
  (lambda (x y)
    (- x (* (div x y) y))))

(define div-and-mod
  (lambda (x y)
    (let ((d (div x y)))
      (values d (- x (* d y))))))

(define mod0
  (lambda (x y)
    (- x (* (div0 x y) y))))

(define div0-and-mod0
  (lambda (x y)
    (let ((d0 (div0 x y)))
      (values d0 (- x (* d0 y))))))
