#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core sorting)

  (export list-sort
          vector-sort
          vector-sort!)

  (import (core primitives))

  (define list-sort
    (lambda (proc lst)

      (define merge
        (lambda (lst1 lst2)
          (cond
           ((null? lst1) lst2)
           ((null? lst2) lst1)
           (else
            (if (proc (car lst2) (car lst1))
                (cons (car lst2) (merge lst1 (cdr lst2)))
                (cons (car lst1) (merge (cdr lst1) lst2)))))))

      (define sort
        (lambda (lst n)
          (cond ((= n 1)
                 (list (car lst)))
                ((= n 2)
                 (if (proc (cadr lst) (car lst))
                     (list (cadr lst) (car lst))
                     (list (car lst) (cadr lst))))
                (else
                 (let ((n/2 (div n 2)))
                   (merge (sort lst n/2)
                          (sort (list-tail lst n/2) (- n n/2))))))))

      (define divide
        (lambda (lst)
          (let loop ((acc 1) (lst lst))
            (cond ((null? (cdr lst)) (values acc '()))
                  (else
                   (if (proc (car lst) (cadr lst))
                       (loop (+ acc 1) (cdr lst))
                       (values acc (cdr lst))))))))

      (cond ((null? lst) '())
            (else
             (let ((len (length lst)))
               (let-values (((n rest) (divide lst)))
                 (cond ((null? rest) lst)
                       (else
                        (merge (list-head lst n)
                               (sort rest (- len n)))))))))))

  (define vector-sort
    (lambda (proc vect)
      (let ((lst (vector->list vect)))
        (let ((lst2 (list-sort proc lst)))
          (cond ((eq? lst lst2) vect)
                (else
                 (list->vector lst2)))))))

  (define vector-sort!
    (lambda (proc vect)
      (let* ((n (vector-length vect)) (work (make-vector (+ (div n 2) 1))))

        (define simple-sort!
          (lambda (first last)
            (let loop1 ((i first))
              (cond ((< i last)
                     (let ((m (vector-ref vect i)) (k i))
                       (let loop2 ((j (+ i 1)))
                         (cond ((<= j last)
                                (if (proc (vector-ref vect j) m)
                                    (begin
                                      (set! m (vector-ref vect j))
                                      (set! k j)))
                                (loop2 (+ j 1)))
                               (else
                                (vector-set! vect k (vector-ref vect i))
                                (vector-set! vect i m)
                                (loop1 (+ i 1)))))))))))

        (define sort!
          (lambda (first last)
            (cond ((> (- last first) 10)
                   (let ((middle (div (+ first last) 2)))
                     (sort! first middle)
                     (sort! (+ middle 1) last)
                     (let loop ((i first) (p2size 0))
                       (cond ((> i middle)
                              (let loop ((p1 (+ middle 1)) (p2 0) (p3 first))
                                (cond ((and (<= p1 last) (< p2 p2size))
                                       (cond ((proc (vector-ref work p2) (vector-ref vect p1))
                                              (vector-set! vect p3 (vector-ref work p2))
                                              (loop p1 (+ p2 1) (+ p3 1)))
                                             (else
                                              (vector-set! vect p3 (vector-ref vect p1))
                                              (loop (+ p1 1) p2 (+ p3 1)))))
                                      (else
                                       (let loop ((s2 p2)(d3 p3))
                                         (cond ((< s2 p2size)
                                                (vector-set! vect d3 (vector-ref work s2))
                                                (loop (+ s2 1) (+ d3 1)))))))))
                             (else
                              (vector-set! work p2size (vector-ref vect i))
                              (loop (+ i 1) (+ p2size 1)))))))
                  (else
                   (simple-sort! first last)))))

        (sort! 0 (- n 1)))))

  ) ;[end]
