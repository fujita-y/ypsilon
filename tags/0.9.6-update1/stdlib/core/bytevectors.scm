#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core bytevectors)

  (export endianness native-endianness
          bytevector? make-bytevector bytevector-length bytevector=?
          bytevector-fill! bytevector-copy! bytevector-copy
          bytevector-u8-ref bytevector-s8-ref bytevector-u8-set! bytevector-s8-set!
          bytevector->u8-list u8-list->bytevector
          bytevector-uint-ref bytevector-sint-ref bytevector-uint-set! bytevector-sint-set! 
          bytevector->uint-list bytevector->sint-list
          uint-list->bytevector sint-list->bytevector
          bytevector-u16-ref bytevector-s16-ref bytevector-u16-native-ref bytevector-s16-native-ref
          bytevector-u16-set! bytevector-s16-set! bytevector-u16-native-set! bytevector-s16-native-set!
          bytevector-u32-ref bytevector-s32-ref bytevector-u32-native-ref bytevector-s32-native-ref
          bytevector-u32-set! bytevector-s32-set! bytevector-u32-native-set! bytevector-s32-native-set!
          bytevector-u64-ref bytevector-s64-ref bytevector-u64-native-ref bytevector-s64-native-ref
          bytevector-u64-set! bytevector-s64-set! bytevector-u64-native-set! bytevector-s64-native-set!
          bytevector-ieee-single-ref bytevector-ieee-single-native-ref
          bytevector-ieee-single-set! bytevector-ieee-single-native-set!
          bytevector-ieee-double-ref bytevector-ieee-double-native-ref
          bytevector-ieee-double-set! bytevector-ieee-double-native-set!)

  (import (core primitives) (core arithmetic))
  
  (define-syntax div256
    (syntax-rules ()
      ((_ x) (bitwise-arithmetic-shift x -8))))

  (define-syntax mod256
    (syntax-rules ()
      ((_ x) (bitwise-and x 255))))
  
  (define-syntax endianness
    (syntax-rules (big little native)
      ((_ big) 'big)
      ((_ little) 'little)
      ((_ native) (native-endianness)))) 
  
  (define bytevector-uint-ref
    (lambda (bv index endien size)
      (cond ((eq? endien (endianness big))
             (let ((end (+ index size)))
               (let loop ((i index) (acc 0))
                 (if (>= i end)
                     acc
                     (loop (+ i 1) (+ (* 256 acc) (bytevector-u8-ref bv i)))))))
            ((eq? endien (endianness little))
             (let loop ((i (+ index size -1)) (acc 0))
               (if (< i index)
                   acc
                   (loop (- i 1) (+ (* 256 acc) (bytevector-u8-ref bv i))))))
            (else
             (assertion-violation 'bytevector-uint-ref
                                  (format "expected endianness, but got ~r, as argument 3" endien)
                                  (list bv index endien size))))))
  
  (define bytevector-sint-ref
    (lambda (bv index endien size)
      (cond ((eq? endien (endianness big))
             (if (> (bytevector-u8-ref bv index) 127)
                 (- (bytevector-uint-ref bv index endien size) (expt 256 size))
                 (bytevector-uint-ref bv index endien size)))
            ((eq? endien (endianness little))
             (if (> (bytevector-u8-ref bv (+ index size -1)) 127)
                 (- (bytevector-uint-ref bv index endien size) (expt 256 size))
                 (bytevector-uint-ref bv index endien size)))
            (else
             (assertion-violation 'bytevector-uint-ref
                                  (format "expected endianness, but got ~r, as argument 3" endien)
                                  (list bv index endien size))))))

  (define bytevector-uint-set!
    (lambda (bv index val endien size)
      (cond ((= val 0)
             (let ((end (+ index size)))
               (let loop ((i index))
                 (cond ((>= i end) (unspecified))
                       (else
                        (bytevector-u8-set! bv i 0)
                        (loop (+ i 1)))))))
            ((< 0 val (expt 256 size))
             (cond ((eq? endien (endianness big))
                    (let ((start (- (+ index size) 1)))
                      (let loop ((i start) (acc val))
                        (cond ((< i index) (unspecified))
                              (else
                               (bytevector-u8-set! bv i (mod256 acc))
                               (loop (- i 1) (div256 acc)))))))
                   ((eq? endien (endianness little))
                    (let ((end (+ index size)))
                      (let loop ((i index) (acc val))
                        (cond ((>= i end) (unspecified))
                              (else
                               (bytevector-u8-set! bv i (mod256 acc))
                               (loop (+ i 1) (div256 acc)))))))))
            (else
             (assertion-violation 'bytevector-uint-set!
                                  (format "value out of range, ~s as argument 3" val)
                                  (list bv index val endien size))))
      (unspecified)))

  (define bytevector-sint-set!
    (lambda (bv index val endien size)
      (let* ((p-bound (expt 2 (- (* size 8) 1))) 
             (n-bound (- (+ p-bound 1))))
        (if (< n-bound val p-bound)
            (if (> val 0)
                (bytevector-uint-set! bv index val endien size)
                (bytevector-uint-set! bv index (+ val (expt 256 size)) endien size))
            (assertion-violation 'bytevector-sint-set!
                                 (format "value out of range, ~s as argument 3" val)
                                 (list bv index val endien size))))
      (unspecified)))

  (define bytevector->uint-list
    (lambda (bv endien size)
      (let loop ((i (- (bytevector-length bv) size)) (acc '()))
        (if (> i -1)
            (loop (- i size) (cons (bytevector-uint-ref bv i endien size) acc))
            (if (= i (- size))
                acc
                (assertion-violation 'bytevector->uint-list
                                     (format "expected appropriate element size as argument 3, but got ~r" size)
                                     (list bv endien size)))))))

  (define bytevector->sint-list
    (lambda (bv endien size)
      (let loop ((i (- (bytevector-length bv) size)) (acc '()))
        (if (> i -1)
            (loop (- i size) (cons (bytevector-sint-ref bv i endien size) acc))
            (if (= i (- size))
                acc
                (assertion-violation 'bytevector->sint-list
                                     (format "expected appropriate element size as argument 3, but got ~r" size)
                                     (list bv endien size)))))))
  
  (define uint-list->bytevector
    (lambda (lst endien size)
      (let ((bv (make-bytevector (* size (length lst)))))
        (let loop ((i 0) (lst lst))
          (cond ((null? lst) bv)
                (else
                 (bytevector-uint-set! bv i (car lst) endien size)
                 (loop (+ i size) (cdr lst))))))))

  (define sint-list->bytevector
    (lambda (lst endien size)
      (let ((bv (make-bytevector (* size (length lst)))))
        (let loop ((i 0) (lst lst))
          (cond ((null? lst) bv)
                (else
                 (bytevector-sint-set! bv i (car lst) endien size)
                 (loop (+ i size) (cdr lst))))))))
  
  ) ;[end]




