;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define top-level-unbound?
  (lambda (id)
    (not (top-level-bound? id))))

(define acons
  (lambda (key datum alist)
    (cons (cons key datum) alist)))

(define count-pair
  (lambda (lst)
    (let loop ((lst lst) (n 0))
      (if (pair? lst) (loop (cdr lst) (+ n 1)) n))))

(define every1
  (lambda (pred lst)
    (or (null? lst)
        (let loop ((head (car lst)) (rest (cdr lst)))
          (and (pred head)
               (or (null? rest)
                   (loop (car rest) (cdr rest))))))))

(define every2
  (lambda (pred lst1 lst2)
    (or (null? lst1)
        (null? lst2)
        (let loop ((head1 (car lst1)) (rest1 (cdr lst1)) (head2 (car lst2)) (rest2 (cdr lst2)))
          (and (pred head1 head2)
               (or (null? rest1)
                   (null? rest2)
                   (loop (car rest1) (cdr rest1) (car rest2) (cdr rest2))))))))

(define any1
  (lambda (pred lst)
    (and (not (null? lst))
         (or (pred (car lst)) (any1 pred (cdr lst))))))

(define any2
  (lambda (pred lst1 lst2)
    (and (not (null? lst1))
         (not (null? lst2))
         (or (pred (car lst1) (car lst2))
             (any2 pred (cdr lst1) (cdr lst2))))))

(define filter
  (lambda (pred lst)
    (let loop ((lst lst))
      (cond ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (loop (cdr lst))))
            (else (loop (cdr lst)))))))

(define partition
  (lambda (pred lst)
    (let loop ((lst lst) (acc1 '()) (acc2 '()))
      (cond ((null? lst) (values (reverse acc1) (reverse acc2)))
            ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc1) acc2))
            (else (loop (cdr lst) acc1 (cons (car lst) acc2)))))))

(define safe-length
  (lambda (lst)
    (let loop ((lst lst) (n 0))
      (if (pair? lst)
          (loop (cdr lst) (+ n 1))
          (or (and (null? lst) n) -1)))))

(define split-at
  (lambda (lst n)
    (values (list-head lst n) (list-tail lst n))))

(define unique-id-list?
  (lambda (lst)
    (and (list? lst)
         (not (let loop ((lst lst))
                (and (pair? lst)
                     (or (not (symbol? (car lst)))
                         (memq (car lst) (cdr lst))
                         (loop (cdr lst)))))))))

(define find-duplicates
  (lambda (lst)
    (and (list? lst)
         (let loop ((lst lst))
           (and (pair? lst)
                (if (memq (car lst) (cdr lst))
                    (car lst)
                    (loop (cdr lst))))))))

(define string-split
  (lambda (str delim)

    (define split->list
      (lambda (str proc)
        (let ((in (make-string-input-port str))
              (out (make-string-output-port)))
          (let loop1 ((lst '()))
            (let loop2 ((c (get-char in)))
              (cond ((eof-object? c)
                     (let ((s (extract-accumulated-string out)))
                       (if (string=? s "")
                           (reverse lst)
                           (reverse (cons s lst)))))
                    ((proc c)
                     (loop1 (cons (extract-accumulated-string out) lst)))
                    (else
                     (put-char out c)
                     (loop2 (get-char in)))))))))

    (cond ((char? delim)
           (split->list str (lambda (c) (char=? c delim))))
          ((string? delim)
           (let ((lst (string->list delim)))
             (split->list str (lambda (c) (any1 (lambda (d) (char=? c d)) lst)))))
          ((procedure? delim)
           (split->list str delim)))))

(define wrong-type-argument-message
  (lambda (expect got . nth)
    (if (null? nth)
        (format "expected ~a, but got ~a" expect got)
        (format "expected ~a, but got ~a, as argument ~a" expect got (car nth)))))
