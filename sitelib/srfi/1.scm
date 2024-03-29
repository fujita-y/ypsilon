#!nobacktrace
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library (srfi 1)
  (import (core))
  (export xcons
          cons*
          make-list
          list-tabulate
          list-copy
          circular-list
          iota
          (rename list? proper-list?)
          circular-list?
          dotted-list?
          not-pair?
          (rename null? null-list?)
          list=
          first
          second
          third
          fourth
          fifth
          sixth
          seventh
          eighth
          ninth
          tenth
          car+cdr
          take
          drop
          take-right
          drop-right
          take!
          drop-right!
          split-at
          split-at!
          last
          last-pair
          length+
          concatenate
          append!
          (rename concatenate concatenate!)
          (rename reverse reverse!)
          append-reverse
          (rename append-reverse append-reverse!)
          zip
          unzip1
          unzip2
          unzip3
          unzip4
          unzip5
          count
          fold
          unfold
          pair-fold
          reduce
          (rename fold-right/srfi-1 fold-right)
          unfold-right
          pair-fold-right
          reduce-right
          append-map
          (rename append-map append-map!)
          map!
          pair-for-each
          filter-map
          (rename map/srfi-1 map-in-order)
          filter
          partition
          (rename remp remove)
          filter!
          partition!
          remove!
          find
          find-tail
          any
          every
          list-index
          take-while
          drop-while
          take-while!
          span
          break
          span!
          break!
          delete
          delete-duplicates
          delete!
          delete-duplicates!
          alist-cons
          alist-copy
          alist-delete
          alist-delete!
          lset<=
          lset=
          lset-adjoin
          lset-union
          (rename lset-union lset-union!)
          lset-intersection
          (rename lset-intersection lset-intersection!)
          lset-difference
          (rename lset-difference lset-difference!)
          lset-xor
          (rename lset-xor lset-xor!)
          lset-diff+intersection
          (rename lset-diff+intersection lset-diff+intersection!))
  (begin
    (define for-each-1/srfi-1
      (lambda (proc lst)
        (cond ((null? lst) (unspecified)) (else (proc (car lst)) (for-each-1/srfi-1 proc (cdr lst))))))

    (define for-each-n/srfi-1
      (lambda (proc lst)
        (cond ((null? lst) (unspecified)) (else (apply proc (car lst)) (for-each-n/srfi-1 proc (cdr lst))))))

    (define for-each/srfi-1
      (lambda (proc lst1 . lst2)
        (if (null? lst2) (for-each-1/srfi-1 proc lst1) (for-each-n/srfi-1 proc (apply list-transpose* lst1 lst2)))))

    (define map-1/srfi-1
      (lambda (proc lst)
        (cond ((null? lst) '()) (else (cons (proc (car lst)) (map-1/srfi-1 proc (cdr lst)))))))

    (define map-n/srfi-1
      (lambda (proc lst)
        (cond ((null? lst) '()) (else (cons (apply proc (car lst)) (map-n/srfi-1 proc (cdr lst)))))))

    (define map/srfi-1
      (lambda (proc lst1 . lst2)
        (if (null? lst2) (map-1/srfi-1 proc lst1) (map-n/srfi-1 proc (apply list-transpose* lst1 lst2)))))

    (define fold-right/srfi-1
      (lambda (proc seed lst1 . lst2)
        (define fold-right-1
          (lambda (proc seed lst) (if (null? lst) seed (proc (car lst) (fold-right-1 proc seed (cdr lst))))))
        (define fold-right-n
          (lambda (proc seed lst)
            (if (null? lst) seed (apply proc (append (car lst) (list (fold-right-n proc seed (cdr lst))))))))
        (if (null? lst2) (fold-right-1 proc seed lst1) (fold-right-n proc seed (apply list-transpose* lst1 lst2)))))

    (define list-of-subset
      (lambda (lst)
        (let loop ((lst lst) (acc '())) (cond ((null? lst) acc) (else (cons lst (loop (cdr lst) acc)))))))

    (define fold-1
      (lambda (proc seed lst)
        (if (null? lst) seed (fold-1 proc (proc (car lst) seed) (cdr lst)))))

    (define fold-n
      (lambda (proc seed lst)
        (if (null? lst) seed (fold-n proc (apply proc (append (car lst) (list seed))) (cdr lst)))))

    (define lset-member? (lambda (proc x lst) (exists (lambda (e) (proc x e)) lst)))

    (define lset-union-1
      (lambda (proc lst1 lst2)
        (cond ((null? lst2) lst1)
              ((null? lst1) lst2)
              (else
                (let loop ((lst2 lst2) (acc lst1))
                  (if (null? lst2)
                      acc
                      (let ((e (car lst2))) (loop (cdr lst2) (if (lset-member? proc e acc) acc (cons e acc))))))))))

    (define xcons (lambda (d a) (cons a d)))

    (define list-tabulate
      (lambda (n proc)
        (let loop ((lst '()) (n (- n 1))) (if (< n 0) lst (loop (cons (proc n) lst) (- n 1))))))

    (define circular-list
      (lambda lst
        (let ((lst (list-copy lst))) (begin (set-cdr! (last-pair lst) lst) lst))))

    (define dotted-list?
      (lambda (lst)
        (not
          (let loop ((head lst) (tail lst))
            (or (and (pair? head)
                     (or (and (pair? (cdr head)) (or (eq? (cdr head) tail) (loop (cddr head) (cdr tail))))
                         (null? (cdr head))))
                (null? head))))))

    (define not-pair?
      (lambda (x)
        (not (pair? x))))

    (define list=
      (lambda (proc . lists)
        (define list-equal-1
          (lambda (lst1 lst2)
            (let loop ((lst1 lst1) (lst2 lst2))
              (if (null? lst1)
                  (null? lst2)
                  (and (pair? lst1)
                       (pair? lst2)
                       (proc (car lst1) (car lst2))
                       (loop (cdr lst1) (cdr lst2)))))))
        (or (null? lists)
            (null? (cdr lists))
            (let loop ((lst1 (car lists)) (lst2 (cadr lists)) (rest (cddr lists)))
              (if (null? rest)
                  (list-equal-1 lst1 lst2)
                  (and (list-equal-1 lst1 lst2)
                       (loop lst2 (car rest) (cdr rest))))))))

    (define first car)

    (define second cadr)

    (define third caddr)

    (define fourth cadddr)

    (define fifth
      (lambda (lst)
        (list-ref lst 4)))

    (define sixth
      (lambda (lst)
        (list-ref lst 5)))

    (define seventh
      (lambda (lst)
        (list-ref lst 6)))

    (define eighth
      (lambda (lst)
        (list-ref lst 7)))

    (define ninth
      (lambda (lst)
        (list-ref lst 8)))

    (define tenth
      (lambda (lst)
        (list-ref lst 9)))

    (define car+cdr
      (lambda (lst)
        (values (car lst) (cdr lst))))

    (define take-right
      (lambda (lst n)
        (let loop ((head (list-tail lst n)) (tail lst))
          (if (pair? head) (loop (cdr head) (cdr tail)) tail))))

    (define drop-right
      (lambda (lst n)
        (list-head lst (- (count-pair lst) n))))

    (define take!
      (lambda (lst n)
        (cond ((= n 0) '())
              (else (set-cdr! (list-tail lst (- n 1)) '()) lst))))

    (define drop-right!
      (lambda (lst n)
        (take! lst (- (count-pair lst) n))))

    (define split-at
      (lambda (lst n)
        (values (take lst n) (drop lst n))))

    (define split-at!
      (lambda (lst n)
        (let ((rest (drop lst n)))
          (values (take! lst n) rest))))

    (define last
      (lambda (lst)
        (car (last-pair lst))))

    (define length+
      (lambda (lst)
        (and (list? lst) (length lst))))

    (define concatenate
      (lambda (lst)
        (apply append lst)))

    (define append!
      (lambda lsts
        (let loop ((head lsts))
          (cond ((not (pair? head)) head)
                ((null? (car head)) (loop (cdr head)))
                ((pair? (car head))
                 (let loop ((tail (last-pair (car head))) (rest (cdr head)))
                   (cond ((not (pair? rest)) (car head))
                         ((null? (car rest)) (loop tail (cdr rest)))
                         ((not (pair? (car rest)))
                          (cond ((null? (cdr rest)) (set-cdr! tail (car rest)) (car head))
                                (else
                                  (assertion-violation
                                    'append!
                                    (format "expected proper list, but got ~s" (car rest))
                                    lsts))))
                         (else (set-cdr! tail (car rest)) (loop (last-pair (car rest)) (cdr rest))))))
                (else (loop (cdr lsts)))))))

    (define append-reverse
      (lambda (head tail)
        (if (pair? head) (append-reverse (cdr head) (cons (car head) tail)) tail)))

    (define zip
      (lambda lists
        (apply list-transpose* lists)))

    (define unzip1
      (lambda (lst)
        (map-1/srfi-1 first lst)))

    (define unzip2
      (lambda (lst)
        (values (map-1/srfi-1 first lst) (map-1/srfi-1 second lst))))

    (define unzip3
      (lambda (lst)
        (values (map-1/srfi-1 first lst) (map-1/srfi-1 second lst) (map-1/srfi-1 third lst))))

    (define unzip4
      (lambda (lst)
        (values (map-1/srfi-1 first lst) (map-1/srfi-1 second lst) (map-1/srfi-1 third lst) (map-1/srfi-1 fourth lst))))

    (define unzip5
      (lambda (lst)
        (values
          (map-1/srfi-1 first lst)
          (map-1/srfi-1 second lst)
          (map-1/srfi-1 third lst)
          (map-1/srfi-1 fourth lst)
          (map-1/srfi-1 fifth lst))))

    (define count
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (let loop ((lst lst1) (acc 0))
              (cond ((null? lst) acc)
                    (else
                      (loop (cdr lst) (if (proc (car lst)) (+ acc 1) acc )))))
            (let loop ((lst (apply list-transpose* lst1 lst2)) (acc 0))
              (cond ((null? lst) acc)
                    (else
                      (loop (cdr lst) (if (apply proc (car lst)) (+ acc 1) acc ))))))))

    (define fold
      (lambda (proc seed lst1 . lst2)
        (if (null? lst2) (fold-1 proc seed lst1) (fold-n proc seed (apply list-transpose* lst1 lst2)))))

    (define unfold
      (lambda (proc func gen seed . opt)
        (let-optionals opt
            ((tail-gen (lambda (x) '())))
          (let loop ((seed seed)) (if (proc seed) (tail-gen seed) (cons (func seed) (loop (gen seed))))))))

    (define pair-fold
      (lambda (proc seed lst1 . lst2)
        (define pair-fold-1
          (lambda (proc seed lst)
            (cond ((null? lst) seed) (else (let ((lst2 (cdr lst))) (pair-fold-1 proc (proc lst seed) lst2))))))
        (define pair-fold-n
          (lambda (proc seed lst)
            (cond ((null? lst) seed) (else (pair-fold-n proc (apply proc (append (car lst) (list seed))) (cdr lst))))))
        (if (null? lst2)
            (pair-fold-1 proc seed lst1)
            (pair-fold-n proc seed (apply list-transpose* (list-of-subset lst1) (map-1/srfi-1 list-of-subset lst2))))))

    (define reduce
      (lambda (proc seed lst)
        (if (null? lst)
            seed
            (fold proc (car lst) (cdr lst)))))

    (define unfold-right
      (lambda (proc func gen seed . opt)
        (let-optionals opt
            ((tail '()))
          (let loop ((seed seed) (lst tail)) (if (proc seed) lst (loop (gen seed) (cons (func seed) lst)))))))

    (define pair-fold-right
      (lambda (proc seed lst1 . lst2)
        (define pair-fold-right-1
          (lambda (proc seed lst) (cond ((null? lst) seed) (else (proc lst (pair-fold-right-1 proc seed (cdr lst)))))))
        (define pair-fold-right-n
          (lambda (proc seed lst)
            (cond ((null? lst) seed)
                  (else (apply proc (append (car lst) (list (pair-fold-right-n proc seed (cdr lst)))))))))
        (if (null? lst2)
            (pair-fold-right-1 proc seed lst1)
            (pair-fold-right-n
              proc
              seed
              (apply list-transpose* (list-of-subset lst1) (map-1/srfi-1 list-of-subset lst2))))))

    (define reduce-right
      (lambda (proc seed lst)
        (if (null? lst)
            seed
            (let loop ((head (car lst)) (rest (cdr lst)))
              (cond ((pair? rest) (proc head (loop (car rest) (cdr rest))))
                    (else head))))))

    (define append-map
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (apply append (map-1/srfi-1 proc lst1))
            (apply append (map-n/srfi-1 proc (apply list-transpose* lst1 lst2))))))

    (define map!-1
      (lambda (proc lst)
        (cond ((null? lst) '())
              (else
                (set-car! lst (proc (car lst)))
                (map!-1 proc (cdr lst))))))

    (define map!-n
      (lambda (proc lst1 lst2)
        (cond ((null? lst2) '())
              (else
                (set-car! lst1 (apply proc (car lst2)))
                (map!-n proc (cdr lst1) (cdr lst2))))))

    (define map!
      (lambda (proc lst1 . lst2)
        (cond ((null? lst2) (map!-1 proc lst1) lst1)
              (else (map!-n proc lst1 (apply list-transpose* lst1 lst2)) lst1))))

    (define pair-for-each
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (for-each-1/srfi-1 proc (list-of-subset lst1))
            (for-each-n/srfi-1 proc (apply list-transpose* (list-of-subset lst1) (map list-of-subset lst2))))))

    (define filter-map
      (lambda (proc lst1 . lst2)
        (if (null? lst2)
            (filter values (map-1/srfi-1 proc lst1))
            (filter values (map-n/srfi-1 proc (apply list-transpose* lst1 lst2))))))

    (define filter!
      (lambda (proc lst)
        (let loop ((last #f) (head lst) (rest lst))
          (cond ((null? rest)
                 (cond ((eq? head lst) '())
                       (else (set-cdr! last '()) lst)))
                ((proc (car rest))
                 (or (eq? head rest) (set-car! head (car rest)))
                 (loop head (cdr head) (cdr rest)))
                (else (loop last head (cdr rest)))))))

    (define partition!
      (lambda (proc lst)
        (define ans-left #f)
        (define ans-right #f)
        (let loop ((left #f) (right #f) (rest lst))
          (cond ((null? rest)
                 (and left (set-cdr! left '()))
                 (and right (set-cdr! right '()))
                 (values (or ans-left '()) (or ans-right '())))
                (else
                  (if (proc (car rest))
                      (cond ((pair? left)
                             (set-cdr! left rest)
                             (loop rest right (cdr rest)))
                            (else
                              (set! ans-left rest)
                              (loop rest right (cdr rest))))
                      (cond ((pair? right)
                             (set-cdr! right rest)
                             (loop left rest (cdr rest)))
                            (else
                              (set! ans-right rest)
                              (loop left rest (cdr rest))))))))))

    (define remove!
      (lambda (proc lst)
        (filter! (lambda (e) (not (proc e))) lst)))

    (define find-tail
      (lambda (proc lst) (let loop ((lst lst)) (cond ((null? lst) #f) ((proc (car lst)) lst) (else (loop (cdr lst)))))))

    (define any
      (lambda (proc lst1 . lst2)
        (define any-1
          (lambda (proc lst)
            (cond ((null? lst) #f)
                  (else
                    (let loop ((head (car lst)) (rest (cdr lst)))
                      (if (null? rest) (proc head) (or (proc head) (loop (car rest) (cdr rest)))))))))
        (define any-n
          (lambda (proc lst)
            (cond ((null? lst) #f)
                  (else
                    (let loop ((head (car lst)) (rest (cdr lst)))
                      (if (null? rest) (apply proc head) (or (apply proc head) (loop (car rest) (cdr rest)))))))))
        (if (null? lst2) (any-1 proc lst1) (any-n proc (apply list-transpose* lst1 lst2)))))

    (define every
      (lambda (proc lst1 . lst2)
        (define every-1
          (lambda (proc lst)
            (or (null? lst)
                (let loop ((head (car lst)) (rest (cdr lst)))
                  (if (null? rest) (proc head) (and (proc head) (loop (car rest) (cdr rest))))))))
        (define every-n
          (lambda (proc lst)
            (or (null? lst)
                (let loop ((head (car lst)) (rest (cdr lst)))
                  (if (null? rest) (apply proc head) (and (apply proc head) (loop (car rest) (cdr rest))))))))
        (if (null? lst2) (every-1 proc lst1) (every-n proc (apply list-transpose* lst1 lst2)))))

    (define list-index
      (lambda (proc lst1 . lst2)
        (define list-index-1
          (lambda (proc lst)
            (and (not (null? lst))
                 (let loop ((head (car lst)) (rest (cdr lst)) (n 0))
                   (cond ((proc head) n) ((null? rest) #f) (else (loop (car rest) (cdr rest) (+ n 1))))))))
        (define list-index-n
          (lambda (proc lst)
            (and (not (null? lst))
                 (let loop ((head (car lst)) (rest (cdr lst)) (n 0))
                   (cond ((apply proc head) n) ((null? rest) #f) (else (loop (car rest) (cdr rest) (+ n 1))))))))
        (if (null? lst2) (list-index-1 proc lst1) (list-index-n proc (apply list-transpose* lst1 lst2)))))

    (define take-while
      (lambda (proc lst)
        (let loop ((lst lst))
          (cond ((null? lst) '()) ((proc (car lst)) (cons (car lst) (loop (cdr lst)))) (else '())))))

    (define drop-while
      (lambda (proc lst)
        (let loop ((lst lst)) (cond ((null? lst) '()) ((proc (car lst)) (loop (cdr lst))) (else lst)))))

    (define take-while!
      (lambda (proc lst)
        (let loop ((last #f) (head lst) (rest lst))
          (cond ((and (pair? rest) (proc (car rest)))
                 (loop head (cdr head) (cdr rest)))
                ((eq? head lst) '())
                (else (set-cdr! last '()) lst)))))

    (define span
      (lambda (proc lst)
        (values (take-while proc lst) (drop-while proc lst))))

    (define span!
      (lambda (proc lst)
        (let loop ((last #f) (head lst) (rest lst))
          (cond ((and (pair? rest) (proc (car rest)))
                 (loop head (cdr head) (cdr rest)))
                ((eq? head lst) (values '() lst))
                (else (set-cdr! last '()) (values lst rest))))))

    (define break!
      (lambda (proc lst)
        (span! (lambda (e) (not (proc e))) lst)))

    (define delete
      (lambda (x lst . opt)
        (let-optionals opt ((proc equal?)) (remp (lambda (e) (proc x e)) lst))))

    (define delete-duplicates
      (lambda (lst . opt)
        (let-optionals opt ((proc equal?))
          (cond ((null? lst) '())
                (else
                  (let loop ((head (car lst)) (rest (cdr lst)))
                    (cond ((null? rest) (list head))
                          ((memp (lambda (e) (proc head e)) rest)
                           (let ((rest (delete head rest proc)))
                             (cond ((null? rest) (list head))
                                   (else (cons head (loop (car rest) (cdr rest)))))))
                          (else
                            (cons head (loop (car rest) (cdr rest)))))))))))

    (define (delete! x lst . opt)
      (let-optionals opt ((proc equal?))
        (filter! (lambda (e) (not (proc x e))) lst)))

    (define delete-duplicates!
      (lambda (lst . opt)
        (let-optionals opt ((proc equal?))
          (cond ((null? lst) '())
                (else
                  (let loop ((head (car lst)) (rest (cdr lst)))
                    (cond ((null? rest) lst)
                          ((memp (lambda (e) (proc head e)) rest)
                           (let ((rest (delete! head rest proc)))
                             (cond ((null? rest) lst)
                                   (else (loop (car rest) (cdr rest))))))
                          (else
                            (loop (car rest) (cdr rest))))))))))

    (define alist-cons
      (lambda (key val lst)
        (cons (cons key val) lst)))

    (define alist-copy
      (lambda (lst)
        (map (lambda (e) (cons (car e) (cdr e))) lst)))

    (define alist-delete
      (lambda (key lst . opt)
        (let-optionals opt ((proc equal?)) (remp (lambda (e) (proc key (car e))) lst))))

    (define alist-delete!
      (lambda (key lst . opt)
        (let-optionals opt ((proc equal?))
          (filter! (lambda (e) (not (proc key (car e)))) lst))))

    (define lset<=
      (lambda (proc . lst)
        (or (null? lst)
            (let loop ((head (car lst)) (rest (cdr lst)))
              (or (null? rest)
                  (and (for-all (lambda (e) (lset-member? proc e (car rest))) head) (loop (car rest) (cdr rest))))))))

    (define lset=
      (lambda (proc . lst)
        (or (null? lst)
            (let loop ((head (car lst)) (rest (cdr lst)))
              (or (null? rest)
                  (and (for-all (lambda (e1) (exists (lambda (e2) (proc e1 e2)) (car rest))) head)
                       (for-all (lambda (e2) (exists (lambda (e1) (proc e1 e2)) head)) (car rest))
                       (loop (car rest) (cdr rest))))))))

    (define lset-adjoin
      (lambda (proc lst . elts)
        (lset-union-1 proc lst elts)))

    (define lset-union
      (lambda (proc . lst)
        (cond ((null? lst) '())
              ((null? (cdr lst)) (car lst))
              (else
                (let loop ((head (cadr lst)) (rest (cddr lst)) (acc (car lst)))
                  (if (null? rest)
                      (lset-union-1 proc acc head)
                      (loop (car rest) (cdr rest) (lset-union-1 proc acc head))))))))

    (define lset-intersection-1
      (lambda (proc lst1 lst2)
        (cond ((null? lst2) '())
              (else
                (let loop ((acc '()) (lst1 lst1))
                  (if (null? lst1)
                      (reverse acc)
                      (let ((e (car lst1)))
                        (loop (cond ((lset-member? proc e lst2) (cons e acc)) (else acc)) (cdr lst1)))))))))

    (define lset-intersection
      (lambda (proc . lst)
        (cond ((null? lst) '())
              ((null? (cdr lst)) (car lst))
              (else
                (let loop ((head (cadr lst)) (rest (cddr lst)) (acc (car lst)))
                  (if (null? rest)
                      (lset-intersection-1 proc acc head)
                      (loop (car rest) (cdr rest) (lset-intersection-1 proc acc head))))))))

    (define lset-difference-1
      (lambda (proc lst1 lst2)
        (cond ((null? lst2) '())
              (else
                (let loop ((lst1 lst1) (acc '()))
                  (if (null? lst1)
                      (reverse acc)
                      (let ((e (car lst1))) (loop (cdr lst1) (if (lset-member? proc e lst2) acc (cons e acc))))))))))

    (define lset-difference
      (lambda (proc . lst)
        (cond ((null? lst) '())
              ((null? (cdr lst)) (car lst))
              (else
                (let loop ((head (cadr lst)) (rest (cddr lst)) (acc (car lst)))
                  (if (null? rest)
                      (lset-difference-1 proc acc head)
                      (loop (car rest) (cdr rest) (lset-difference-1 proc acc head))))))))

    (define lset-xor-1
      (lambda (proc lst1 lst2)
        (cond ((null? lst2) lst1)
              ((null? lst1) lst2)
              (else (append (lset-difference-1 proc lst1 lst2) (lset-difference-1 proc lst2 lst1))))))

    (define lset-xor
      (lambda (proc . lst)
        (cond ((null? lst) '())
              ((null? (cdr lst)) (car lst))
              (else
                (let loop ((head (cadr lst)) (rest (cddr lst)) (acc (car lst)))
                  (if (null? rest)
                      (lset-xor-1 proc acc head)
                      (loop (car rest) (cdr rest) (lset-xor-1 proc acc head))))))))

    (define lset-diff+intersection
      (lambda (proc . lst)
        (values (apply lset-difference proc lst) (apply lset-intersection proc lst))))))
