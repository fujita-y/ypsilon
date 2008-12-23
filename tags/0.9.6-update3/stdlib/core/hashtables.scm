#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core hashtables)

  (export make-eq-hashtable
          make-eqv-hashtable
          make-string-hashtable
          make-hashtable
          (rename (make-weak-shared-core-hashtable make-weak-hashtable)
                  (weak-core-hashtable? weak-hashtable?))
          (rename (core-hashtable? hashtable?)
                  (core-hashtable-size hashtable-size)
                  (core-hashtable-ref hashtable-ref)
                  (core-hashtable-set! hashtable-set!)
                  (core-hashtable-delete! hashtable-delete!)
                  (core-hashtable-contains? hashtable-contains?))
          hashtable-update!
          (rename (core-hashtable-copy hashtable-copy)
                  (core-hashtable-clear! hashtable-clear!))
          hashtable-keys
          hashtable-entries
          (rename (core-hashtable-equivalence-function hashtable-equivalence-function)
                  (core-hashtable-hash-function hashtable-hash-function)
                  (core-hashtable-mutable? hashtable-mutable?))
          equal-hash string-hash string-ci-hash symbol-hash
          (rename (core-hashtable->alist hashtable->alist)))

  (import (core intrinsics)
          (only (core primitives)
                make-core-hashtable
                make-shared-core-hashtable
                make-weak-core-hashtable
                make-weak-shared-core-hashtable
                weak-core-hashtable?
                core-hashtable?
                core-hashtable-size
                core-hashtable-ref
                core-hashtable-set!
                core-hashtable-delete!
                core-hashtable-contains?
                core-hashtable-copy
                core-hashtable-clear!
                core-hashtable-equivalence-function
                core-hashtable-hash-function
                core-hashtable-mutable?
                core-hashtable->alist
                set-cdr! unspecified equal-hash string-hash symbol-hash
                format)
          (only (core lists) assp remq)
          (core unicode)
          (core optargs))

  (define make-generic-hashtable
    (lambda (hash-function equiv-function ht-root size mutable?)

      (define generic-hashtable-size (lambda (ht-custom) size))

      (define generic-hashtable-ref
        (lambda (ht-custom key default)
          (let ((slot (hash-function key))
                (equiv? (lambda (e) (equiv-function e key))))
            (cond ((core-hashtable-ref ht-root slot #f)
                   => (lambda (alist)
                        (cond ((assp equiv? alist) => cdr)
                              (else default))))
                  (else default)))))

      (define generic-hashtable-set!
        (lambda (ht-custom key obj)
          (or mutable? (assertion-violation 'hashtable-set! (format "expected mutable hashtable, but ~s is not" ht-custom) (list ht-custom key obj)))
          (let ((slot (hash-function key))
                (equiv? (lambda (e) (equiv-function e key))))
            (cond ((core-hashtable-ref ht-root slot #f)
                   => (lambda (alist)
                        (cond ((assp equiv? alist)
                               => (lambda (a) (set-cdr! a obj)))
                              (else
                               (set! size (+ size 1))
                               (core-hashtable-set! ht-root slot (cons (cons key obj) alist))))))
                  (else
                   (set! size (+ size 1))
                   (core-hashtable-set! ht-root slot (list (cons key obj))))))))

      (define generic-hashtable-delete!
        (lambda (ht-custom key)
          (or mutable? (assertion-violation 'hashtable-delete! (format "expected mutable hashtable, but ~s is not" ht-custom) (list ht-custom key)))
          (let ((slot (hash-function key))
                (equiv? (lambda (e) (equiv-function e key))))
            (cond ((core-hashtable-ref ht-root slot #f)
                   => (lambda (alist)
                        (cond ((assp equiv? alist)
                               => (lambda (p)
                                    (core-hashtable-set! ht-root slot (remq p alist)))))))))
          (unspecified)))

      (define generic-hashtable-contains?
        (lambda (ht-custom key)
          (let ((slot (hash-function key))
                (equiv? (lambda (e) (equiv-function e key))))
            (cond ((core-hashtable-ref ht-root slot #f)
                   => (lambda (alist)
                        (and (assp equiv? alist) #t)))
                  (else #f)))))

      (define generic-hashtable-copy
        (lambda (ht-custom . opt)
          (let-optionals opt ((new-mutable? #f))
            (if mutable?
                (let ((ht-new-root (make-core-hashtable)))
                  (for-each (lambda (a) (core-hashtable-set!
                                         ht-new-root
                                         (car a)
                                         (map (lambda (e) (cons (car e) (cdr e))) (cdr a))))
                            (core-hashtable->alist ht-root))
                  (if new-mutable?
                      (make-generic-hashtable hash-function equiv-function ht-new-root size new-mutable?)
                      (make-generic-hashtable hash-function equiv-function (core-hashtable-copy ht-new-root) size new-mutable?)))
                (make-generic-hashtable hash-function equiv-function (core-hashtable-copy ht-root new-mutable?) size new-mutable?)))))

      (define generic-hashtable-clear!
        (lambda (ht-custom . opt)
          (let-optionals opt ((k 0))
            (or mutable? (assertion-violation 'hashtable-clear! (format "expected mutable hashtable, but ~s is not" ht-custom) (cons ht-custom opt)))
            (core-hashtable-clear! ht-root k)
            (set! size 0))))

      (define generic-hashtable->alist
        (lambda (ht-custom)
          (let loop ((lst (core-hashtable->alist ht-root)) (ans '()))
            (cond ((null? lst) ans)
                  (else
                   (loop (cdr lst) (append (cdar lst) ans)))))))

      (define generic-hashtable-equivalence-function (lambda (ht-custom) equiv-function))

      (define generic-hashtable-hash-function (lambda (ht-custom) hash-function))

      (define generic-hashtable-mutable? (lambda (ht-custom) mutable?))

      (make-core-hashtable 'generic
                           (vector 'hashtable-handler
                                   hash-function
                                   equiv-function
                                   generic-hashtable-size
                                   generic-hashtable-ref
                                   generic-hashtable-set!
                                   generic-hashtable-delete!
                                   generic-hashtable-contains?
                                   generic-hashtable-copy
                                   generic-hashtable-clear!
                                   generic-hashtable-hash-function
                                   generic-hashtable-equivalence-function
                                   generic-hashtable-mutable?
                                   generic-hashtable->alist))))

  (define make-hashtable
    (lambda (hash-function equiv-function . opt)
      (let-optionals opt ((k 0))
        (or (procedure? hash-function)
            (assertion-violation 'make-hashtable (format "expected procedure, but got ~r, as argument 1" hash-function)))
        (or (procedure? equiv-function)
            (assertion-violation 'make-hashtable (format "expected procedure, but got ~r, as argument 2" equiv-function)))
        (make-generic-hashtable hash-function equiv-function (make-core-hashtable 'eq? k) 0 #t))))

  (define make-eq-hashtable
    (lambda opt
      (let-optionals opt ((k 0))
        (make-shared-core-hashtable 'eq? k))))

  (define make-eqv-hashtable
    (lambda opt
      (let-optionals opt ((k 0))
        (make-shared-core-hashtable 'eqv? k))))

  (define make-string-hashtable
    (lambda opt
      (let-optionals opt ((k 0))
        (make-shared-core-hashtable 'string=? k))))

  (define hashtable-update!
    (lambda (ht key proc default)
      (or (core-hashtable-mutable? ht)
          (assertion-violation 'hashtable-update! "expected mutable hashtable" (list ht key proc default)))
      (core-hashtable-set! ht key (proc (core-hashtable-ref ht key default)))))

  (define hashtable-keys
    (lambda (ht)
      (list->vector (map car (core-hashtable->alist ht)))))

  (define hashtable-entries
    (lambda (ht)
      (let ((lst (core-hashtable->alist ht)))
        (values (list->vector (map car lst))
                (list->vector (map cdr lst))))))

  (define string-ci-hash
    (lambda (s)
      (string-hash (string-foldcase s))))

 ) ;[end]
