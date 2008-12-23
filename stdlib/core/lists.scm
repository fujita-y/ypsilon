#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core lists)

  (export find
          for-all
          exists
          filter
          partition
          fold-left
          fold-right
          remp remove remv remq
          memp member memv memq
          assp assoc assv assq cons*
          iota make-list
          list-of-unique-symbols?
          remove-duplicate-symbols
          take drop break)

  (import (core primitives)
          (core optargs))

  (define collect-cdr
    (lambda (lst)
      (let loop ((lst lst))
        (cond ((null? lst) '())
              ((null? (cdar lst)) (loop (cdr lst)))
              (else (cons (cdar lst) (loop (cdr lst))))))))

  (define find
    (lambda (pred lst)
      (cond ((null? lst) #f)
            ((pair? lst)
             (let loop ((head (car lst)) (rest (cdr lst)))
               (cond ((pred head) head)
                     ((null? rest) #f)
                     ((pair? rest) (loop (car rest) (cdr rest)))
                     (else
                      (assertion-violation 'find (format "traversal reached to non-pair element ~s" rest) (list pred lst))))))
            (else
             (assertion-violation 'find (format "expected chain of pairs, but got ~r, as argument 2" lst) (list pred lst))))))

  (define for-all-n
    (lambda (pred list-of-lists)
      (let ((argc (length list-of-lists)))

        (define collect-car
          (lambda (lst)
            (let loop ((lst lst))
              (cond ((null? lst) '())
                    ((pair? (car lst))
                     (cons (caar lst) (loop (cdr lst))))
                    (else
                     (assertion-violation 'for-all (format "traversal reached to non-pair element ~s" (car lst)) list-of-lists))))))

        (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
          (or (= (length head) argc)
              (assertion-violation 'for-all "expected same length chains of pairs" list-of-lists))
          (if (null? rest)
              (apply pred head)
              (and (apply pred head)
                   (loop (collect-car rest) (collect-cdr rest))))))))

  (define exists-n
    (lambda (pred list-of-lists)
      (let ((argc (length list-of-lists)))

        (define collect-car
          (lambda (lst)
            (let loop ((lst lst))
              (cond ((null? lst) '())
                    ((pair? (car lst))
                     (cons (caar lst) (loop (cdr lst))))
                    (else
                     (assertion-violation 'exists (format "traversal reached to non-pair element ~s" (car lst)) list-of-lists))))))

        (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
          (or (= (length head) argc)
              (assertion-violation 'exists "expected same length chains of pairs" list-of-lists))
          (if (null? rest)
              (apply pred head)
              (or (apply pred head)
                  (loop (collect-car rest) (collect-cdr rest))))))))

  (define for-all-n-quick
    (lambda (pred lst)
      (or (null? lst)
          (let loop ((head (car lst)) (rest (cdr lst)))
            (if (null? rest)
                (apply pred head)
                (and (apply pred head)
                     (loop (car rest) (cdr rest))))))))

  (define exists-n-quick
    (lambda (pred lst)
      (and (pair? lst)
           (let loop ((head (car lst)) (rest (cdr lst)))
             (if (null? rest)
                 (apply pred head)
                 (or (apply pred head)
                     (loop (car rest) (cdr rest))))))))

  (define for-all-1
    (lambda (pred lst)
      (cond ((null? lst) #t)
            ((pair? lst)
             (let loop ((head (car lst)) (rest (cdr lst)))
               (cond ((null? rest) (pred head))
                     ((pair? rest)
                      (and (pred head)
                           (loop (car rest) (cdr rest))))
                     (else
                      (and (pred head)
                           (assertion-violation 'for-all (format "traversal reached to non-pair element ~s" rest) (list pred lst)))))))
            (else
             (assertion-violation 'for-all (format "expected chain of pairs, but got ~r, as argument 2" lst) (list pred lst))))))

  (define exists-1
    (lambda (pred lst)
      (cond ((null? lst) #f)
            ((pair? lst)
             (let loop ((head (car lst)) (rest (cdr lst)))
               (cond ((null? rest) (pred head))
                     ((pred head))
                     ((pair? rest) (loop (car rest) (cdr rest)))
                     (else
                      (assertion-violation 'exists (format "traversal reached to non-pair element ~s" rest) (list pred lst))))))
            (else
             (assertion-violation 'exists (format "expected chain of pairs, but got ~r, as argument 2" lst) (list pred lst))))))

  (define for-all
    (lambda (pred lst1 . lst2)
      (cond ((null? lst2)
             (for-all-1 pred lst1))
            ((apply list-transpose+ lst1 lst2)
             => (lambda (lst) (for-all-n-quick pred lst)))
            (else
             (for-all-n pred (cons lst1 lst2))))))

  (define exists
    (lambda (pred lst1 . lst2)
      (cond ((null? lst2)
             (exists-1 pred lst1))
            ((apply list-transpose+ lst1 lst2)
             => (lambda (lst) (exists-n-quick pred lst)))
            (else
             (exists-n pred (cons lst1 lst2))))))

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

  (define fold-left-1
    (lambda (proc seed lst)
      (cond ((null? lst) seed)
            (else
             (fold-left-1 proc (proc seed (car lst)) (cdr lst))))))

  (define fold-left-n
    (lambda (proc seed lst)
      (cond ((null? lst) seed)
            (else
             (fold-left-n proc (apply proc (append (list seed) (car lst))) (cdr lst))))))

  (define fold-left
    (lambda (proc seed lst1 . lst2)
      (if (null? lst2)
          (if (list? lst1)
              (fold-left-1 proc seed lst1)
              (assertion-violation 'fold-left (format "expected proper list, but got ~r, as argument 3" lst1) (cons* proc seed lst1 lst2)))
          (cond ((apply list-transpose+ lst1 lst2)
                 => (lambda (lst) (fold-left-n proc seed lst)))
                (else
                 (assertion-violation 'fold-left "expected same length proper lists" (cons* proc seed lst1 lst2)))))))

  (define fold-right-1
    (lambda (proc seed lst)
      (cond ((null? lst) seed)
            (else
             (proc (car lst) (fold-right-1 proc seed (cdr lst)))))))

  (define fold-right-n
    (lambda (proc seed lst)
      (cond ((null? lst) seed)
            (else
             (apply proc (append (car lst) (list (fold-right-n proc seed (cdr lst)))))))))

  (define fold-right
    (lambda (proc seed lst1 . lst2)
      (if (null? lst2)
          (if (list? lst1)
              (fold-right-1 proc seed lst1)
              (assertion-violation 'fold-right (format "expected proper list, but got ~r, as argument 3" lst1) (cons* proc seed lst1 lst2)))
          (cond ((apply list-transpose+ lst1 lst2)
                 => (lambda (lst) (fold-right-n proc seed lst)))
                (else
                 (assertion-violation 'fold-right "expected same length proper lists" (cons* proc seed lst1 lst2)))))))

  (define remp
    (lambda (pred lst)
      (let loop ((lst lst))
        (cond ((null? lst) '())
              ((pred (car lst))
               (loop (cdr lst)))
              (else
               (cons (car lst)
                     (loop (cdr lst))))))))

  (define remove
    (lambda (obj lst)
      (let loop ((lst lst))
        (cond ((null? lst) '())
              ((equal? (car lst) obj)
               (loop (cdr lst)))
              (else
               (cons (car lst)
                     (loop (cdr lst))))))))

  (define remv
    (lambda (obj lst)
      (let loop ((lst lst))
        (cond ((null? lst) '())
              ((eqv? (car lst) obj)
               (loop (cdr lst)))
              (else
               (cons (car lst)
                     (loop (cdr lst))))))))

  (define remq
    (lambda (obj lst)
      (let loop ((lst lst))
        (cond ((null? lst) '())
              ((eq? (car lst) obj)
               (loop (cdr lst)))
              (else
               (cons (car lst)
                     (loop (cdr lst))))))))

  (define memp
    (lambda (proc lst)
      (cond
       ((null? lst) #f)
       ((proc (car lst)) lst)
       (else
        (memp proc (cdr lst))))))

  (define assp
    (lambda (proc lst)
      (cond
       ((null? lst) #f)
       ((proc (caar lst)) (car lst))
       (else
        (assp proc (cdr lst))))))

  (define iota
    (lambda (n . opt)
      (or (and (number? n) (exact? n) (>= n 0))
          (assertion-violation 'iota (format "expected non-negative exact integer, but got ~u as argument 1" n) (cons n opt)))
      (let-optionals opt ((start 0) (step 1))
        (let loop ((n (- n 1)) (lst '()))
          (cond ((< n 0) lst)
                (else (loop (- n 1) (cons (+ start (* n step)) lst))))))))

  (define make-list
    (lambda (n . opt)
      (or (and (number? n) (exact? n) (>= n 0))
          (assertion-violation 'make-list (format "expected non-negative exact integer, but got ~u as argument 1" n) (cons n opt)))
      (let-optionals opt ((fill #f))
        (let loop ((lst '()) (n n))
          (cond ((<= n 0) lst)
                (else (loop (cons fill lst) (- n 1))))))))

  (define list-of-unique-symbols?
    (lambda (lst)
      (and (list? lst)
           (not (let loop ((lst lst))
                  (and (pair? lst)
                       (or (not (symbol? (car lst)))
                           (memq (car lst) (cdr lst))
                           (loop (cdr lst)))))))))

  (define remove-duplicate-symbols
    (lambda (lst)
      (let loop ((lst lst) (ans '()))
        (if (null? lst)
            (reverse ans)
            (if (memq (car lst) ans)
                (loop (cdr lst) ans)
                (loop (cdr lst) (cons (car lst) ans)))))))

  (define break
    (lambda (pred lst)
      (let ((tail '()))
        (let ((head
               (let loop ((lst lst))
                 (cond ((null? lst) '())
                       ((pred (car lst))
                        (begin (set! tail lst) '()))
                       (else
                        (cons (car lst) (loop (cdr lst))))))))
          (values head tail)))))

  (define take list-head)

  (define drop list-tail)

  ) ;[end]
