#!nobacktrace
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library (srfi 125)
  (import (except (core) string-hash string-ci-hash symbol-hash) (only (srfi 1) every) (srfi 128))
  (export make-hash-table
          hash-table
          hash-table-unfold
          alist->hash-table
          (rename hashtable? hash-table?)
          (rename hashtable-contains? hash-table-contains?)
          hash-table-empty?
          hash-table=?
          (rename hashtable-mutable? hash-table-mutable?)
          hash-table-ref
          (rename hashtable-ref hash-table-ref/default)
          hash-table-set!
          hash-table-delete!
          hash-table-intern!
          hash-table-update!
          hash-table-update!/default
          hash-table-pop!
          (rename hashtable-clear! hash-table-clear!)
          (rename hashtable-size hash-table-size)
          hash-table-keys
          hash-table-values
          hash-table-entries
          hash-table-find
          hash-table-count
          hash-table-map
          hash-table-for-each
          hash-table-map!
          hash-table-map->list
          hash-table-fold
          hash-table-prune!
          (rename hashtable-copy hash-table-copy)
          hash-table-empty-copy
          (rename hashtable->alist hash-table->alist)
          hash-table-union!
          hash-table-intersection!
          hash-table-difference!
          hash-table-xor!
          (rename hash/srfi-125 hash)
          (rename string-hash/srfi-125 string-hash)
          (rename string-ci-hash/srfi-125 string-ci-hash)
          (rename hash-by-identity/srfi-125 hash-by-identity)
          (rename hashtable-equivalence-function hash-table-equivalence-function)
          (rename hashtable-hash-function hash-table-hash-function)
          (rename hashtable-contains? hash-table-exists?)
          hash-table-walk
          (rename hash-table-union! hash-table-merge!))
  (begin
    (define make-hash-table
      (lambda args
        (destructuring-match args
          (((? comparator? comparator) . rest)
           (make-hash-table-aux
             (comparator-equality-predicate comparator)
             (comparator-hash-function comparator)
             rest))
          (((? procedure? equality-predicate) (? procedure? hash-function) . rest)
           (make-hash-table-aux equality-predicate hash-function rest))
          (((? procedure? equality-predicate) . rest)
           (make-hash-table-aux equality-predicate #f rest))
          (() (assertion-violation 'make-hash-table "required at least 1, but 0 argument given"))
          ((arg1 . rest)
           (assertion-violation
             'make-hash-table
             (format "expected comparator or procedure, but got ~r as argument 1" arg1)
             args)))))

    (define hash-table
      (lambda (comparator . rest)
        (let ((ht (make-hash-table comparator)))
          (let loop ((lst rest))
            (destructuring-match lst
              ((key value . more)
               (cond ((hashtable-contains? ht key)
                      (assertion-violation
                        'hash-table
                        (format "duplicate key ~r in key-value list" key)
                        (cons* comparator rest)))
                     (else (hashtable-set! ht key value) (loop more))))
              ((key)
               (assertion-violation
                 'hash-table
                 (format "wrong number of elements in key-value list ~r" rest)
                 (cons* comparator rest)))
              (_ (hashtable-copy ht #f)))))))

    (define make-hash-table-aux
      (lambda (equality-predicate hash-function options)
        (cond ((eq? equality-predicate eq?) (make-eq-hashtable))
              ((eq? equality-predicate eqv?) (make-eqv-hashtable))
              ((eq? equality-predicate symbol=?) (make-eq-hashtable))
              ((eq? equality-predicate string=?) (make-string-hashtable))
              ((eq? equality-predicate string-ci=?)
               (make-hashtable string-ci-hash equality-predicate))
              ((eq? equality-predicate equal?) (make-hashtable equal-hash equality-predicate))
              (hash-function (make-hashtable hash-function equality-predicate))
              (else
                (assertion-violation
                  'make-hash-table
                  "missing hash function for unknown equality predicate"
                  equality-predicate)))))

    (define hash-table-unfold
      (lambda (stop? mapper successor seed comparator . rest)
        (let ((ht (apply make-hash-table comparator rest)))
          (let loop ((seed seed))
            (cond ((stop? seed) ht)
                  (else
                    (let-values (((key value) (mapper seed)))
                      (hashtable-set! ht key value)
                      (loop (successor seed)))))))))

    (define alist->hash-table
      (lambda (alist comparator . rest)
        (let ((ht (apply make-hash-table comparator rest)))
          (for-each (lambda (e) (hashtable-set! ht (car e) (cdr e))) (reverse alist))
          ht)))

    (define hash-table-empty? (lambda (ht) (= (hashtable-size ht) 0)))

    (define hash-table=?
      (lambda (value-comparator ht1 ht2)
        (let ((comparison (comparator-equality-predicate value-comparator)))
          (and (= (hashtable-size ht1) (hashtable-size ht2))
               (every
                 (lambda (e)
                   (and (hashtable-contains? ht2 (car e))
                        (comparison (hashtable-ref ht2 (car e) unspecified) (cdr e))))
                 (hashtable->alist ht1))
               (every
                 (lambda (e)
                   (and (hashtable-contains? ht1 (car e))
                        (comparison (hashtable-ref ht1 (car e) unspecified) (cdr e))))
                 (hashtable->alist ht2))))))

    (define hash-table-ref
      (lambda (ht key . rest)
        (let ((value (hashtable-ref ht key unspecified)))
          (destructuring-match rest
            (()
             (cond ((eq? value unspecified)
                    (assertion-violation
                      'hash-table-ref
                      (format "hash table key ~r not exists" key)
                      (cons* ht key rest)))
                   (else value)))
            ((failure) (if (eq? value unspecified) (failure) value))
            ((failure success) (if (eq? value unspecified) (failure) (success value)))
            (_
              (assertion-violation
                'hash-table-ref
                "wrong number of arguments"
                (cons* ht key rest)))))))

    (define hash-table-set!
      (lambda (ht . rest)
        (let loop ((lst rest))
          (destructuring-match lst
            ((key value . more) (hashtable-set! ht key value) (loop more))
            ((key)
             (assertion-violation
               'hash-table-set!
               (format "wrong number of elements in key-value list ~r" rest)
               (cons* ht rest)))
            (_ (unspecified))))))

    (define hash-table-delete!
      (lambda (ht . rest)
        (let loop ((lst rest) (count 0))
          (cond ((null? lst) count)
                ((hashtable-contains? ht (car lst))
                 (hashtable-delete! ht (car lst))
                 (loop (cdr lst) (+ count 1)))
                (else (loop (cdr lst) count))))))

    (define hash-table-intern!
      (lambda (ht key failure)
        (let ((value (hashtable-ref ht key unspecified)))
          (cond ((eq? value unspecified)
                 (let ((value (failure))) (hashtable-set! ht key value) value))
                (else value)))))

    (define hash-table-update!
      (lambda (ht key updater . rest)
        (destructuring-match rest
          (()
           (let ((value (hashtable-ref ht key unspecified)))
             (cond ((eq? value unspecified)
                    (assertion-violation
                      'hash-table-update!
                      (format "hash table key ~r not exists" key)
                      (cons* ht key rest)))
                   (else (hashtable-set! ht key (updater value))))))
          ((failure)
           (let ((value (hash-table-ref ht key failure))) (hashtable-set! ht key (updater value))))
          ((failure success)
           (let ((value (hash-table-ref ht key failure success)))
             (hashtable-set! ht key (updater value))))
          (_
            (assertion-violation
              'hash-table-ref
              "wrong number of arguments"
              (cons* ht key updater rest))))))

    (define hash-table-update!/default
      (lambda (ht key updater default)
        (let ((value (hashtable-ref ht key unspecified)))
          (cond ((eq? value unspecified) (hashtable-set! ht key (updater default)))
                (else (hashtable-set! ht key (updater value)))))))

    (define hash-table-pop!
      (lambda (ht)
        (let-values (((key-lst value-lst) (hashtable-entries ht)))
          (cond ((= (vector-length key-lst) 0)
                 (assertion-violation 'hash-table-pop! "hash table is empty" ht))
                (else
                  (let ((key (vector-ref key-lst 0)) (value (vector-ref value-lst 0)))
                    (hashtable-delete! ht key)
                    (values key value)))))))

    (define hash-table-keys (lambda (ht) (map car (hashtable->alist ht))))

    (define hash-table-values (lambda (ht) (map cdr (hashtable->alist ht))))

    (define hash-table-entries
      (lambda (ht) (let ((lst (hashtable->alist ht))) (values (map car lst) (map cdr lst)))))

    (define hash-table-find
      (lambda (proc ht failure)
        (let loop ((lst (hashtable->alist ht)))
          (cond ((null? lst) (failure)) ((proc (caar lst) (cdar lst))) (else (loop (cdr lst)))))))

    (define hash-table-count
      (lambda (pred ht)
        (let loop ((lst (hashtable->alist ht)) (count 0))
          (cond ((null? lst) count)
                ((pred (caar lst) (cdar lst)) (loop (cdr lst) (+ count 1)))
                (else (loop (cdr lst) count))))))

    (define hash-table-map
      (lambda (proc comparator ht)
        (let ((ans (make-hash-table comparator)))
          (let loop ((lst (hashtable->alist ht)))
            (cond ((null? lst) ans)
                  (else (hashtable-set! ans (caar lst) (proc (cdar lst))) (loop (cdr lst))))))))

    (define hash-table-for-each
      (lambda (proc ht)
        (let loop ((lst (hashtable->alist ht)))
          (cond ((null? lst) (unspecified)) (else (proc (caar lst) (cdar lst)) (loop (cdr lst)))))))

    (define hash-table-map!
      (lambda (proc ht)
        (let loop ((lst (hashtable->alist ht)))
          (cond ((null? lst) (unspecified))
                (else
                  (hashtable-set! ht (caar lst) (proc (caar lst) (cdar lst)))
                  (loop (cdr lst)))))))

    (define hash-table-map->list
      (lambda (proc ht) (map (lambda (e) (proc (car e) (cdr e))) (hashtable->alist ht))))

    (define hash-table-fold
      (lambda (proc seed ht)
        (if (hashtable? proc)
            (hash-table-fold seed ht proc)
            (let loop ((lst (hashtable->alist ht)) (val seed))
              (cond ((null? lst) val) (else (loop (cdr lst) (proc (caar lst) (cdar lst) val))))))))

    (define hash-table-prune!
      (lambda (proc ht)
        (let loop ((lst (hashtable->alist ht)))
          (cond ((null? lst) (unspecified))
                ((proc (caar lst) (cdar lst)) (hashtable-delete! ht (caar lst)) (loop (cdr lst)))
                (else (loop (cdr lst)))))))

    (define hash-table-empty-copy
      (lambda (ht) (let ((ans (hashtable-copy ht #t))) (hashtable-clear! ans) ans)))

    (define hash-table-union!
      (lambda (ht1 ht2)
        (let loop ((lst (hashtable->alist ht2)))
          (cond ((null? lst) ht1)
                ((hashtable-contains? ht1 (caar lst)) (loop (cdr lst)))
                (else (hashtable-set! ht1 (caar lst) (cdar lst)) (loop (cdr lst)))))))

    (define hash-table-intersection!
      (lambda (ht1 ht2)
        (let loop ((lst (hashtable->alist ht1)))
          (cond ((null? lst) ht1)
                ((hashtable-contains? ht2 (caar lst)) (loop (cdr lst)))
                (else (hashtable-delete! ht1 (caar lst)) (loop (cdr lst)))))))

    (define hash-table-difference!
      (lambda (ht1 ht2)
        (let loop ((lst (hashtable->alist ht2)))
          (cond ((null? lst) ht1)
                ((hashtable-contains? ht1 (caar lst))
                 (hashtable-delete! ht1 (caar lst))
                 (loop (cdr lst)))
                (else (loop (cdr lst)))))))

    (define hash-table-xor!
      (lambda (ht1 ht2)
        (let loop ((lst (hashtable->alist ht2)))
          (cond ((null? lst) ht1)
                ((hashtable-contains? ht1 (caar lst))
                 (hashtable-delete! ht1 (caar lst))
                 (loop (cdr lst)))
                (else (hashtable-set! ht1 (caar lst) (cdar lst)) (loop (cdr lst)))))))

    (define hash/srfi-125
      (lambda (obj . rest) (default-hash obj)))

    (define string-hash/srfi-125
      (lambda (obj . rest) (string-hash obj)))

    (define string-ci-hash/srfi-125
      (lambda (obj . rest) (string-ci-hash obj)))

    (define hash-by-identity/srfi-125
      (lambda (obj . rest) (default-hash obj)))

    (define hash-table-walk
      (lambda (ht proc) (hash-table-for-each proc ht)))))
