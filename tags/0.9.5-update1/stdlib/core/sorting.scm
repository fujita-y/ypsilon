#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
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
             (let-values (((n rest) (divide lst)))
               (cond ((null? rest) lst)
                     (else
                      (merge (list-head lst n)
                             (sort rest (length rest))))))))))

  (define vector-sort
    (lambda (proc vect)
      (let ((lst (vector->list vect)))
        (let ((lst2 (list-sort proc lst)))
          (cond ((eq? lst lst2) vect)
                (else
                 (list->vector lst2)))))))

  (define vector-sort!
    (lambda (proc vect)
      (let ((lst (vector->list vect)))
        (let ((lst2 (list-sort proc lst)))
          (cond ((eq? lst lst2) vect)
                (else
                 (let loop ((i 0) (lst lst2))
                   (cond ((null? lst) (unspecified))
                         (else
                          (vector-set! vect i (car lst))
                          (loop (+ i 1) (cdr lst)))))))))))

  ) ;[end]
