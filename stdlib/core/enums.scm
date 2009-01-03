#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core enums)

  (export make-enumeration
          enum-set?
          enum-set-universe
          enum-set-indexer
          enum-set-constructor
          enum-set->list
          enum-set-member?
          enum-set-subset?
          enum-set=?
          enum-set-union
          enum-set-intersection
          enum-set-difference
          enum-set-complement
          enum-set-projection
          define-enumeration)

  (import (core primitives)
          (core struct)
          (core lists)
          (core sorting))

  (define-struct enum-type (universe members indexer constructor))
  (define-struct enum-set (type members))

  (define construct-enum-type
    (lambda (universe symbol-list)
      (make-enum-type universe
                      symbol-list
                      (lambda (set)
                        (lambda (symbol)
                          (core-hashtable-ref (enum-type-universe (enum-set-type set)) symbol #f)))
                      (lambda (set)
                        (lambda (symbol-list)
                          (let ((lst (remove-duplicate-symbols symbol-list))
                                (universe (enum-type-universe (enum-set-type set))))
                            (for-each (lambda (e)
                                        (or (symbol? e)
                                            (assertion-violation "enum-set constructor" (format "expected list of symbols, but got ~r as argument 1" symbol-list)))
                                        (or (core-hashtable-ref universe e #f)
                                            (assertion-violation "enum-set constructor" (format "excpectd symbols which belong to the universe, but got ~r as argument 1" symbol-list))))
                                      lst)
                            (make-enum-set (enum-set-type set) lst)))))))

  (define make-enumeration
    (lambda (symbol-list)
      (let ((symbol-list (remove-duplicate-symbols symbol-list)))
        (let ((ht (make-core-hashtable)) (index 0))
          (for-each (lambda (e)
                      (or (symbol? e)
                          (assertion-violation 'make-enumeration (format "expected list of symbols, but got ~r as argument 1" symbol-list)))
                      (core-hashtable-set! ht e index)
                      (set! index (+ index 1)))
                    symbol-list)
          (let ((type (construct-enum-type ht symbol-list)))
            (make-enum-set type symbol-list))))))

  (define enum-set-universe
    (lambda (set)
      (or (enum-set? set)
          (assertion-violation 'enum-set-universe (format "expected enum-set, but got ~r as argument 1" set)))
      (make-enum-set (enum-set-type set)
                     (enum-type-members (enum-set-type set)))))

  (define enum-set-indexer
    (lambda (set)
      (or (enum-set? set)
          (assertion-violation 'enum-set-indexer (format "expected enum-set, but got ~r as argument 1" set)))
      ((enum-type-indexer (enum-set-type set)) set)))

  (define enum-set-constructor
    (lambda (set)
      (or (enum-set? set)
          (assertion-violation 'enum-set-constructor (format "expected enum-set, but got ~r as argument 1" set)))
      ((enum-type-constructor (enum-set-type set)) set)))

  (define enum-set->list
    (lambda (set)
      (or (enum-set? set)
          (assertion-violation 'enum-set->list (format "expected enum-set, but got ~r as argument 1" set)))
      (let ((universe (enum-type-universe (enum-set-type set))))
        (map car
             (list-sort (lambda (a b) (< (cdr a) (cdr b)))
                        (map (lambda (e) (cons e (core-hashtable-ref universe e #f)))
                             (enum-set-members set)))))))

  (define enum-set-member?
    (lambda (symbol set)
      (or (symbol? symbol)
          (assertion-violation 'enum-set-member? (format "expected enum-set, but got ~r as argument 1" symbol) (list symbol set)))
      (or (enum-set? set)
          (assertion-violation 'enum-set-member? (format "expected enum-set, but got ~r as argument 2" set) (list symbol set)))
      (and (memq symbol (enum-set-members set)) #t)))

  (define enum-set-subset?
    (lambda (set1 set2)
      (or (enum-set? set1)
          (assertion-violation 'enum-set-subset? (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
      (or (enum-set? set2)
          (assertion-violation 'enum-set-subset? (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
      (and (for-all (lambda (e) (enum-set-member? e set2)) (enum-set-members set1))
           (let ((m2 (enum-type-members (enum-set-type set2))))
             (for-all (lambda (e) (memq e m2)) (enum-type-members (enum-set-type set1))))
           #t)))

  (define enum-set=?
    (lambda (set1 set2)
      (or (enum-set? set1)
          (assertion-violation 'enum-set=? (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
      (or (enum-set? set2)
          (assertion-violation 'enum-set=? (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
      (and (enum-set-subset? set2 set1)
           (enum-set-subset? set1 set2)
           #t)))

  (define enum-set-union
    (lambda (set1 set2)
      (or (enum-set? set1)
          (assertion-violation 'enum-set-union (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
      (or (enum-set? set2)
          (assertion-violation 'enum-set-union (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
      (or (eq? (enum-set-type set1) (enum-set-type set2))
          (assertion-violation 'enum-set-union "expected same type enum-sets" set1 set2))
      (make-enum-set (enum-set-type set1)
                     (remove-duplicate-symbols (append (enum-set-members set1) (enum-set-members set2))))))

  (define enum-set-intersection
    (lambda (set1 set2)
      (or (enum-set? set1)
          (assertion-violation 'enum-set-intersection (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
      (or (enum-set? set2)
          (assertion-violation 'enum-set-intersection (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
      (or (eq? (enum-set-type set1) (enum-set-type set2))
          (assertion-violation 'enum-set-intersection "expected same type enum-sets" set1 set2))
      (let ((set2-members (enum-set-members set2)))
        (make-enum-set (enum-set-type set1)
                       (filter values (map (lambda (e) (and (memq e set2-members) e))
                                           (enum-set-members set1)))))))

  (define enum-set-difference
    (lambda (set1 set2)
      (or (enum-set? set1)
          (assertion-violation 'enum-set-difference (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
      (or (enum-set? set2)
          (assertion-violation 'enum-set-difference (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
      (or (eq? (enum-set-type set1) (enum-set-type set2))
          (assertion-violation 'enum-set-difference "expected same type enum-sets" set1 set2))
      (let ((set2-members (enum-set-members set2)))
        (make-enum-set (enum-set-type set1)
                       (filter values (map (lambda (e) (and (not (memq e set2-members)) e))
                                           (enum-set-members set1)))))))

  (define enum-set-complement
    (lambda (set)
      (or (enum-set? set)
          (assertion-violation 'enum-set-complement (format "expected enum-set, but got ~r as argument 1" set)))
      (let ((set-members (enum-set-members set)))
        (make-enum-set (enum-set-type set)
                       (filter values (map (lambda (e) (and (not (memq e set-members)) e))
                                           (enum-type-members (enum-set-type set))))))))

  (define enum-set-projection
    (lambda (set1 set2)
      (or (enum-set? set1)
          (assertion-violation 'enum-set-projection (format "expected enum-set, but got ~r as argument 1" set1) (list set1 set2)))
      (or (enum-set? set2)
          (assertion-violation 'enum-set-projection (format "expected enum-set, but got ~r as argument 2" set2) (list set1 set2)))
      (let ((set2-universe-members (enum-type-members (enum-set-type set2))))
        (make-enum-set (enum-set-type set2)
                       (filter values (map (lambda (e) (and (memq e set2-universe-members) e))
                                           (enum-set-members set1)))))))

  (define-syntax define-enumeration
    (syntax-rules ()
      ((_ type-name (symbol1 ...) constructor-syntax)
       (begin
         (define constructor (enum-set-constructor (make-enumeration '(symbol1 ...))))
         (define-syntax type-name
           (lambda (x)
             (syntax-case x ()
               ((_ symbol2)
                (or (memq (syntax->datum (syntax symbol2)) '(symbol1 ...))
                    (syntax-violation 'type-name "excpectd symbols which belong to the universe" x))
                (syntax 'symbol2)))))
         (define-syntax constructor-syntax
           (lambda (x)
             (syntax-case x ()
               ((_ symbol3 (... ...))
                (or (for-all (lambda (e) (memq e '(symbol1 ...)))
                             (syntax->datum (syntax (symbol3 (... ...)))))
                    (syntax-violation 'constructor-syntax "excpectd symbols which belong to the universe" x))
                (syntax (constructor '(symbol3 (... ...))))))))))))

  ) ;[end]
