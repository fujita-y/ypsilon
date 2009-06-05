#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (srfi srfi-27)
  (export make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals
          default-random-source
          random-integer
          random-real)
  (import (core) (rnrs))

  (define cmwc-initial-state (make-cmwc-random-state (microsecond)))

  (define make-random-source
    (lambda ()
      (tuple 'type:random-source (bytevector-copy cmwc-initial-state))))

  (define random-source?
    (lambda (obj)
      (eq? (tuple-ref obj 0) 'type:random-source)))

  (define random-source-state-ref
    (lambda (s)
      (bytevector->uint-list (tuple-ref s 1) (native-endianness) 4)))

  (define random-source-state-set!
    (lambda (s state)
      (tuple-set! s 1 (uint-list->bytevector state (native-endianness) 4))))

  (define random-source-randomize!
    (lambda (s)
      (bytevector-copy! (make-cmwc-random-state (microsecond))
                        0
                        (tuple-ref s 1)
                        0
                        (bytevector-length (tuple-ref s 1)))))

  (define random-source-pseudo-randomize!
    (lambda (s i j)
      (bytevector-copy! (make-cmwc-random-state
                         (+ (bitwise-and i #xffff)
                            (bitwise-arithmetic-shift-left (bitwise-and j #xffff) 16)
                            (bitwise-arithmetic-shift-left (bitwise-and i #xffff0000) 16)
                            (bitwise-arithmetic-shift-left (bitwise-and j #xffff0000) 32)))
                        0
                        (tuple-ref s 1)
                        0
                        (bytevector-length (tuple-ref s 1)))))

  (define make-integer-rng
    (lambda (cmwc-state)
      (lambda (n)
        (if (<= n 4294967296)
            (cmwc-random-u32 cmwc-state n)
            (let ()
              (define rn-count (div (+ (bitwise-length n) 31) 32))
              (define rn-range (- (expt 2 (* rn-count 32)) 1))
              (define quo (div rn-range n))
              (define limit (* quo n))
              (define large-rng
                (lambda ()
                  (let loop ((acc 0) (i 0))
                    (if (>= i rn-count)
                        acc
                        (loop (+ (bitwise-arithmetic-shift-left acc 32)
                                 (cmwc-random-u32 cmwc-state))
                              (+ i 1))))))
              (let loop ((temp (large-rng)))
                (if (>= temp limit)
                    (loop (large-rng))
                    (div temp quo))))))))

  (define make-real-rng
    (lambda (cmwc-state)
      (lambda ()
        (cmwc-random-real cmwc-state))))

  (define random-source-make-integers
    (lambda (s)
      (make-integer-rng (tuple-ref s 1))))

  (define random-source-make-reals
    (lambda (s . unit)
      (make-real-rng (tuple-ref s 1))))

  (define default-random-source (make-random-source))
  (define random-integer (random-source-make-integers default-random-source))
  (define random-real (random-source-make-reals default-random-source))

  ) ;[end]
