#!nobacktrace
(library (srfi srfi-1)
  (export
   ;; constructors
   xcons
   list
   cons*
   make-list
   list-tabulate
   list-copy
   circular-list
   iota
   ;; predicates
   pair?
   null?
   (rename (list? proper-list?))
   circular-list?
   dotted-list?
   not-pair?
   (rename (null? null-list?))
   list=
   ;; selectors
   car cdr
   caar cadr cdar cddr
   caaar caadr cadar caddr
   cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr
   cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr
   cddaar cddadr cdddar cddddr
   list-ref
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
   (rename (take take!))
   drop
   take-right
   drop-right
   (rename (drop-right drop-right!))
   split-at
   (rename (split-at split-at!))
   last
   last-pair
   ;; miscellaneous: length, append, concatenate, reverse, zip & count
   length
   length+
   append
   (rename (append append!))
   concatenate
   (rename (concatenate concatenate!))
   reverse
   (rename (reverse reverse!))
   append-reverse
   (rename (append-reverse append-reverse!))
   zip
   unzip1 unzip2 unzip3 unzip4 unzip5
   count
   ;; fold, unfold & map
   map
   (rename (map map!))
   map/srfi-1
   (rename (map/srfi-1 map!/srfi-1))
   for-each
   for-each/srfi-1
   fold
   fold-right
   fold-right/srfi-1
   unfold
   pair-fold
   (rename (fold reduce))
   unfold-right
   pair-fold-right
   (rename (fold-right/srfi-1 reduce-right))
   append-map
   (rename (append-map append-map!))
   pair-for-each
   filter-map
   map-in-order
   ;; filtering & partitioning
   filter
   (rename (filter filter!))
   partition
   (rename (partition partition!))
   (rename (remp remove/srfi-1))
   (rename (remp remove!/srfi-1))
   ;; seaching
   member
   member/srfi-1
   memq
   memv
   find
   find-tail
   any
   every
   list-index
   take-while
   (rename (take-while take-while!))
   drop-while
   span
   (rename (span span!))
   break
   (rename (break break!))
   ;; deleting
   delete
   (rename (delete delete!))
   delete-duplicates
   (rename (delete-duplicates delete-duplicates!))
   ;; association lists
   assoc
   assoc/srfi-1
   assq
   assv
   alist-cons
   alist-copy
   alist-delete
   (rename (alist-delete alist-delete!))
   ;; set operations on lists
   lset<=
   lset=
   lset-adjoin
   (rename (lset-adjoin lset-adjoin!))
   lset-union
   (rename (lset-union lset-union!))
   lset-intersection
   (rename (lset-intersection lset-intersection!))
   lset-difference
   (rename (lset-difference lset-difference!))
   lset-xor
   (rename (lset-xor lset-xor!))
   lset-diff+intersection
   (rename (lset-diff+intersection lset-diff+intersection!)))
   ;; procedures conflict with r6rs
   #;(rename (map/srfi-1 map)
             (map!/srfi-1 map!)
             (for-each/srfi-1 for-each)
             (fold-right/srfi-1 fold-right)
             (member/srfi-1 member)
             (assoc/srfi-1 assoc)
             (remove/srfi-1 remove)
             (remove!/srfi-1 remove!))
  (import (except (core) remp))

  (define xcons (lambda (d a) (cons a d)))

  (define list-tabulate
    (lambda (n proc)
      (let loop ((lst '()) (n (- n 1)))
        (cond ((< n 0) lst)
              (else (loop (cons (proc n) lst) (- n 1)))))))

  (define circular-list
    (lambda lst
      (let ((lst (list-copy lst)))
        (begin (set-cdr! (last-pair lst) lst) lst))))

  (define dotted-list?
    (lambda (lst)
      (not (let loop ((head lst) (tail lst))
             (or (and (pair? head)
                      (or (and (pair? (cdr head))
                               (or (eq? (cdr head) tail)
                                   (loop (cddr head) (cdr tail))))
                          (null? (cdr head))))
                 (null? head))))))

  (define not-pair?
    (lambda (x)
      (not (pair? x))))

  (define list=
    (lambda (proc . lists)
      (define list-equal-1
        (lambda (lst)
          (let loop ((lst1 (car lists)) (lst2 lst))
            (if (null? lst1)
                (null? lst2)
                (and (proc (car lst1) (car lst2))
                     (loop (cdr lst1) (cdr lst2)))))))
      (or (null? lists)
          (null? (cdr lists))
          (let loop ((head (cadr lists)) (rest (cddr lists)))
            (if (null? rest)
                (list-equal-1 head)
                (and (list-equal-1 head)
                     (loop (car rest) (cdr rest))))))))

  (define first   car)
  (define second  cadr)
  (define third   caddr)
  (define fourth  cadddr)
  (define fifth   (lambda (lst) (list-ref lst 4)))
  (define sixth   (lambda (lst) (list-ref lst 5)))
  (define seventh (lambda (lst) (list-ref lst 6)))
  (define eighth  (lambda (lst) (list-ref lst 7)))
  (define ninth   (lambda (lst) (list-ref lst 8)))
  (define tenth   (lambda (lst) (list-ref lst 9)))
  (define car+cdr (lambda (lst) (values (car lst) (cdr lst))))

  (define count-pair
    (lambda (lst)
      (let loop ((lst lst) (n 0))
        (cond ((pair? lst)
               (loop (cdr lst) (+ n 1)))
              (else n)))))

  (define take-right
    (lambda (lst n)
      (let loop ((head (list-tail lst n)) (tail lst))
        (cond ((pair? head)
               (loop (cdr head) (cdr tail)))
              (else tail)))))

  (define drop-right
    (lambda (lst n)
      (list-head lst (- (count-pair lst) n))))

  (define split-at
    (lambda (lst n)
      (values (take lst n) (drop lst n))))

  (define last (lambda (lst) (car (last-pair lst))))

  (define last-pair
    (lambda (lst)
      (let loop ((lst lst))
        (cond ((pair? (cdr lst)) (loop (cdr lst)))
              (else lst)))))

  (define length+
    (lambda (lst)
      (and (list? lst) (length lst))))

  (define concatenate
    (lambda (lst)
      (apply append lst)))

  (define append-reverse
    (lambda (head tail)
      (cond ((pair? head)
             (append-reverse (cdr head)
                             (cons (car head) tail)))
            (else tail))))

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
      (values (map-1/srfi-1 first lst) (map-1/srfi-1 second lst) (map-1/srfi-1 third lst) (map-1/srfi-1 fourth lst) (map-1/srfi-1 fifth lst))))

  (define count
    (lambda (proc lst1 . lst2)
      (cond ((null? lst2)
             (fold-1 (lambda (args acc)
                       (if (apply proc args) (+ acc 1) acc))
                     0
                     lst1))
            (else
             (fold-n (lambda (arg acc)
                       (if (proc arg) (+ acc 1) acc))
                     0
                     (apply list-transpose* lst1 lst2))))))

  (define fold-1
    (lambda (proc seed lst)
      (cond ((null? lst) seed)
            (else (fold-1 proc (proc (car lst) seed) (cdr lst))))))

  (define fold-n
    (lambda (proc seed lst)
      (cond ((null? lst) seed)
            (else (fold-n proc (apply proc (append (car lst) (list seed))) (cdr lst))))))

  (define fold
    (lambda (proc seed lst1 . lst2)
      (if (null? lst2)
          (fold-1 proc seed lst1)
          (fold-n proc seed (apply list-transpose* lst1 lst2)))))

  (define fold-right/srfi-1
    (lambda (proc seed lst1 . lst2)
      (define fold-right-1 (lambda (proc seed lst)
                             (cond ((null? lst) seed)
                                   (else (proc (car lst) (fold-right-1 proc seed (cdr lst)))))))
      (define fold-right-n (lambda (proc seed lst)
                             (cond ((null? lst) seed)
                                   (else (apply proc (append (car lst) (list (fold-right-n proc seed (cdr lst)))))))))
      (if (null? lst2)
          (fold-right-1 proc seed lst1)
          (fold-right-n proc seed (apply list-transpose* lst1 lst2)))))

  (define unfold
    (lambda (pred func gen seed . opt)
      (let-optionals opt ((tail-gen (lambda (x) '())))
        (let loop ((seed seed))
          (if (pred seed)
              (tail-gen seed)
              (cons (func seed) (loop (gen seed))))))))

  (define unfold-right
    (lambda (pred func gen seed . opt)
      (let-optionals opt ((tail '()))
        (let loop ((seed seed) (lst tail))
          (if (pred seed)
              lst
              (loop (gen seed) (cons (func seed) lst)))))))

  (define reduce fold)

  (define every
    (lambda (proc lst1 . lst2)
      (define every-1 (lambda (proc lst)
                        (or (null? lst)
                            (let loop ((head (car lst)) (rest (cdr lst)))
                              (if (null? rest)
                                  (proc head)
                                  (and (proc head)
                                       (loop (car rest) (cdr rest))))))))
      (define every-n (lambda (proc lst)
                        (or (null? lst)
                            (let loop ((head (car lst)) (rest (cdr lst)))
                              (if (null? rest)
                                  (apply proc head)
                                  (and (apply proc head)
                                       (loop (car rest) (cdr rest))))))))
      (if (null? lst2)
          (every-1 proc lst1)
          (every-n proc (apply list-transpose* lst1 lst2)))))

  (define any
    (lambda (proc lst1 . lst2)
      (define any-1 (lambda (proc lst)
                      (cond ((null? lst) #f)
                            (else (let loop ((head (car lst)) (rest (cdr lst)))
                                    (if (null? rest)
                                        (proc head)
                                        (or (proc head)
                                            (loop (car rest) (cdr rest)))))))))
      (define any-n (lambda (proc lst)
                      (cond ((null? lst) #f)
                            (else (let loop ((head (car lst)) (rest (cdr lst)))
                                    (if (null? rest)
                                        (apply proc head)
                                        (or (apply proc head)
                                            (loop (car rest) (cdr rest)))))))))
      (if (null? lst2)
          (any-1 proc lst1)
          (any-n proc (apply list-transpose* lst1 lst2)))))

  (define map-1/srfi-1
    (lambda (proc lst)
      (cond ((null? lst) '())
            (else
             (cons (proc (car lst))
                   (map-1/srfi-1 proc (cdr lst)))))))

  (define map-n/srfi-1
    (lambda (proc lst)
      (cond ((null? lst) '())
            (else
             (cons (apply proc (car lst))
                   (map-n/srfi-1 proc (cdr lst)))))))

  (define map/srfi-1
    (lambda (proc lst1 . lst2)
      (if (null? lst2)
          (map-1/srfi-1 proc lst1)
          (map-n/srfi-1 proc (apply list-transpose* lst1 lst2)))))

  (define for-each-1/srfi-1
    (lambda (proc lst)
      (if (null? lst)
          (unspecified)
          (begin
            (proc (car lst))
            (for-each-1/srfi-1 proc (cdr lst))))))

  (define for-each-n/srfi-1
    (lambda (proc lst)
      (cond ((null? lst) (unspecified))
            (else
             (apply proc (car lst))
             (for-each-n/srfi-1 proc (cdr lst))))))

  (define for-each/srfi-1
    (lambda (proc lst1 . lst2)
      (if (null? lst2)
          (for-each-1/srfi-1 proc lst1)
          (for-each-n/srfi-1 proc (apply list-transpose* lst1 lst2)))))

  (define list-of-subset
    (lambda (lst)
      (let loop ((lst lst) (acc '()))
        (cond ((null? lst) acc)
              (else
               (cons lst (loop (cdr lst) acc)))))))

  (define pair-fold
    (lambda (proc seed lst1 . lst2)
      (define pair-fold-1 (lambda (proc seed lst)
                            (cond ((null? lst) seed)
                                  (else (pair-fold-1 proc (proc lst seed) (cdr lst))))))
      (define pair-fold-n (lambda (proc seed lst)
                            (cond ((null? lst) seed)
                                  (else (pair-fold-n proc (apply proc (append (car lst) (list seed))) (cdr lst))))))
      (if (null? lst2)
          (pair-fold-1 proc seed lst1)
          (pair-fold-n proc seed (apply list-transpose* (list-of-subset lst1) (map-1/srfi-1 list-of-subset lst2))))))

  (define pair-fold-right
    (lambda (proc seed lst1 . lst2)
      (define pair-fold-right-1 (lambda (proc seed lst)
                                  (cond ((null? lst) seed)
                                        (else (proc lst (pair-fold-right-1 proc seed (cdr lst)))))))
      (define pair-fold-right-n (lambda (proc seed lst)
                                  (cond ((null? lst) seed)
                                        (else (apply proc (append (car lst) (list (pair-fold-right-n proc seed (cdr lst)))))))))
      (if (null? lst2)
          (pair-fold-right-1 proc seed lst1)
          (pair-fold-right-n proc seed (apply list-transpose* (list-of-subset lst1) (map-1/srfi-1 list-of-subset lst2))))))

  (define append-map
    (lambda (proc lst1 . lst2)
      (if (null? lst2)
          (apply append (map-1/srfi-1 proc lst1))
          (apply append (map-n/srfi-1 proc (apply list-transpose* lst1 lst2))))))

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

  (define map-in-order map/srfi-1)

  (define find-tail
    (lambda (proc lst)
      (let loop ((lst lst))
        (cond ((null? lst) #f)
              ((proc (car lst)) lst)
              (else (loop (cdr lst)))))))

  (define list-index
    (lambda (proc lst1 . lst2)
      (define list-index-1
        (lambda (proc lst)
          (and (not (null? lst))
               (let loop ((head (car lst)) (rest (cdr lst)) (n 0))
                 (cond ((proc head) n)
                       ((null? rest) #f)
                       (else
                        (loop (car rest) (cdr rest) (+ n 1))))))))
      (define list-index-n
        (lambda (proc lst)
          (and (not (null? lst))
               (let loop ((head (car lst)) (rest (cdr lst)) (n 0))
                 (cond ((apply proc head) n)
                       ((null? rest) #f)
                       (else
                        (loop (car rest) (cdr rest) (+ n 1))))))))
      (if (null? lst2)
          (list-index-1 proc lst1)
          (list-index-n proc (apply map/srfi-1 list lst1 lst2)))))

  (define take-while
    (lambda (proc lst)
      (let loop ((lst lst))
        (cond ((null? lst) '())
              ((proc (car lst))
               (cons (car lst)
                     (loop (cdr lst))))
              (else '())))))

  (define drop-while
    (lambda (proc lst)
      (let loop ((lst lst))
        (cond ((null? lst) '())
              ((proc (car lst))
               (loop (cdr lst)))
              (else lst)))))

  (define span
    (lambda (proc lst)
      (values (take-while proc lst)
              (drop-while proc lst))))

  (define remp
    (lambda (proc lst)
      (let loop ((lst lst))
        (cond ((null? lst) '())
              ((proc (car lst)) (loop (cdr lst)))
              (else (cons (car lst) (loop (cdr lst))))))))

  (define delete
    (lambda (x lst . opt)
      (let-optionals opt ((proc equal?))
        (remp (lambda (e) (proc x e)) lst))))

  (define delete-duplicates
    (lambda (lst . opt)
      (let-optionals opt ((proc equal?))
        (cond ((null? lst) '())
              (else (let loop ((head (car lst)) (rest (cdr lst)))
                      (cond ((null? rest) (list head))
                            ((memp (lambda (e) (proc head e)) rest)
                             (let ((rest (delete head rest proc)))
                               (cond ((null? rest) (list head))
                                     (else
                                      (cons head (loop (car rest) (cdr rest)))))))
                            (else
                             (cons head (loop (car rest) (cdr rest)))))))))))

  (define alist-cons
    (lambda (key val lst)
      (cons (cons key val) lst)))

  (define alist-copy
    (lambda (lst)
      (map (lambda (e) (cons (car e) (cdr e))) lst)))

  (define alist-delete
    (lambda (key lst . opt)
      (let-optionals opt ((proc equal?))
        (remp (lambda (e) (proc key (car e))) lst))))

  (define assoc/srfi-1
    (lambda (key lst . opt)
      (let-optionals opt ((proc equal?))
        (find (lambda (e) (proc key (car e))) lst))))

  (define member/srfi-1
    (lambda (x lst . opt)
      (let-optionals opt ((proc equal?))
        (find-tail (lambda (e) (proc x e)) lst))))

  (define lset-member?
    (lambda (proc x lst)
      (exists (lambda (e) (proc x e)) lst)))

  (define lset<=
    (lambda (proc . lst)
      (or (null? lst)
          (let loop ((head (car lst)) (rest (cdr lst)))
            (or (null? rest)
                (and (for-all (lambda (e) (lset-member? proc e (car rest))) head)
                     (loop (car rest) (cdr rest))))))))

  (define lset=
    (lambda (proc . lst)
      (or (null? lst)
          (let loop ((head (car lst)) (rest (cdr lst)))
            (or (null? rest)
                (and (for-all (lambda (e1) (exists (lambda (e2) (proc e1 e2)) (car rest))) head)
                     (for-all (lambda (e2) (exists (lambda (e1) (proc e1 e2)) head)) (car rest))
                     (loop (car rest) (cdr rest))))))))

  (define lset-union-1
    (lambda (proc lst1 lst2)
      (cond ((null? lst2) lst1)
            ((null? lst1) lst2)
            (else (let loop ((lst2 lst2) (acc lst1))
                    (if (null? lst2)
                        acc
                        (let ((e (car lst2)))
                          (loop (cdr lst2)
                                (if (lset-member? proc e acc)
                                    acc
                                    (cons e acc))))))))))

  (define lset-adjoin
    (lambda (proc lst . elts)
      (lset-union-1 proc lst elts)))

  (define lset-union
    (lambda (proc . lst)
      (cond ((null? lst) '())
            ((null? (cdr lst)) (car lst))
            (else (let loop ((head (cadr lst)) (rest (cddr lst)) (acc (car lst)))
                    (if (null? rest)
                        (lset-union-1 proc acc head)
                        (loop (car rest) (cdr rest) (lset-union-1 proc acc head))))))))

  (define lset-intersection-1
    (lambda (proc lst1 lst2)
      (cond ((null? lst2) '())
            (else (let loop ((acc '()) (lst1 lst1))
                    (if (null? lst1)
                        (reverse acc)
                        (let ((e (car lst1)))
                          (loop (cond ((lset-member? proc e lst2) (cons e acc))
                                      (else acc))
                                (cdr lst1)))))))))

  (define lset-intersection
    (lambda (proc . lst)
      (cond ((null? lst) '())
            ((null? (cdr lst)) (car lst))
            (else (let loop ((head (cadr lst)) (rest (cddr lst)) (acc (car lst)))
                    (if (null? rest)
                        (lset-intersection-1 proc acc head)
                        (loop (car rest) (cdr rest) (lset-intersection-1 proc acc head))))))))

  (define lset-difference-1
    (lambda (proc lst1 lst2)
      (cond ((null? lst2) '())
            (else (let loop ((lst1 lst1) (acc '()))
                    (if (null? lst1)
                        (reverse acc)
                        (let ((e (car lst1)))
                          (loop (cdr lst1)
                                (if (lset-member? proc e lst2) acc (cons e acc))))))))))

  (define lset-difference
    (lambda (proc . lst)
      (cond ((null? lst) '())
            ((null? (cdr lst)) (car lst))
            (else (let loop ((head (cadr lst)) (rest (cddr lst)) (acc (car lst)))
                    (if (null? rest)
                        (lset-difference-1 proc acc head)
                        (loop (car rest) (cdr rest) (lset-difference-1 proc acc head))))))))

  (define lset-xor-1
    (lambda (proc lst1 lst2)
      (cond ((null? lst2) lst1)
            ((null? lst1) lst2)
            (else
             (append (lset-difference-1 proc lst1 lst2)
                     (lset-difference-1 proc lst2 lst1))))))

  (define lset-xor
    (lambda (proc . lst)
      (cond ((null? lst) '())
            ((null? (cdr lst)) (car lst))
            (else (let loop ((head (cadr lst)) (rest (cddr lst)) (acc (car lst)))
                    (if (null? rest)
                        (lset-xor-1 proc acc head)
                        (loop (car rest) (cdr rest) (lset-xor-1 proc acc head))))))))

  (define lset-diff+intersection
    (lambda (proc . lst)
      (values (apply lset-difference proc lst)
              (apply lset-intersection proc lst))))

  ) ;[end]
