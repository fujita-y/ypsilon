#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core arithmetic)

  (export fixnum?
          fixnum-width
          least-fixnum
          greatest-fixnum
          fx=?
          fx<?
          fx>?
          fx<=?
          fx>=?
          fxzero?
          fxpositive?
          fxnegative?
          fxodd?
          fxeven?
          fxmax
          fxmin
          fx+
          fx*
          fx-
          fxdiv
          fxmod
          fxdiv-and-mod
          fxdiv0
          fxmod0
          fxdiv0-and-mod0
          fx+/carry
          fx-/carry
          fx*/carry
          fxnot
          fxand
          fxior
          fxxor
          fxif
          fxbit-count
          fxlength
          fxfirst-bit-set
          fxbit-set?
          fxcopy-bit
          fxbit-field
          fxcopy-bit-field
          fxarithmetic-shift
          fxarithmetic-shift-left
          fxarithmetic-shift-right
          fxrotate-bit-field
          fxreverse-bit-field

          flonum?
          real->flonum
          fl=?
          fl<?
          fl>?
          fl<=?
          fl>=?
          flinteger?
          flzero?
          flpositive?
          flnegative?
          flodd?
          fleven?
          flfinite?
          flinfinite?
          flnan?
          flmax
          flmin
          fl+
          fl*
          fl-
          fl/
          fldiv-and-mod
          fldiv
          flmod
          fldiv0-and-mod0
          fldiv0
          flmod0
          flnumerator
          fldenominator
          flfloor
          flceiling
          fltruncate
          flround
          flexp
          fllog
          flsin
          flcos
          fltan
          flasin
          flacos
          flatan
          flabs
          flsqrt
          fixnum->flonum

          bitwise-not
          bitwise-and
          bitwise-ior
          bitwise-xor
          bitwise-if
          bitwise-bit-count
          bitwise-length
          bitwise-first-bit-set
          bitwise-bit-set?
          bitwise-copy-bit
          bitwise-bit-field
          bitwise-copy-bit-field
          bitwise-arithmetic-shift
          bitwise-arithmetic-shift-left
          bitwise-arithmetic-shift-right
          bitwise-rotate-bit-field
          bitwise-reverse-bit-field)

  (import (core primitives))

  (define flmod
    (lambda (x y)
      (fl- x (fl* (fldiv x y) y))))

  (define fldiv-and-mod
    (lambda (x y)
      (let ((d (fldiv x y)))
        (values d (fl- x (fl* d y))))))

  (define flmod0
    (lambda (x y)
      (fl- x (fl* (fldiv0 x y) y))))

  (define fldiv0-and-mod0
    (lambda (x y)
      (let ((d0 (fldiv0 x y)))
        (values d0 (fl- x (fl* d0 y))))))
  
  (define 2^fixnum-width (expt 2 (fixnum-width)))

  (define fxmod
    (lambda (x y)
      (fx- x (fx* (fxdiv x y) y))))

  (define fxdiv-and-mod
    (lambda (x y)
      (let ((d (fxdiv x y)))
        (values d (fx- x (fx* d y))))))

  (define fxmod0
    (lambda (x y)
      (fx- x (fx* (fxdiv0 x y) y))))

  (define fxdiv0-and-mod0
    (lambda (x y)
      (let ((d0 (fxdiv0 x y)))
        (values d0 (fx- x (fx* d0 y))))))

  (define fx+/carry
    (lambda (fx1 fx2 fx3)
      (let* ((s (+ fx1 fx2 fx3))
             (s0 (mod0 s 2^fixnum-width))
             (s1 (div0 s 2^fixnum-width)))
        (values s0 s1))))

  (define fx-/carry
    (lambda (fx1 fx2 fx3)
      (let* ((d (- fx1 fx2 fx3))
             (d0 (mod0 d 2^fixnum-width))
             (d1 (div0 d 2^fixnum-width)))
        (values d0 d1))))

  (define fx*/carry
    (lambda (fx1 fx2 fx3)
      (let* ((s (+ (* fx1 fx2) fx3))
             (s0 (mod0 s 2^fixnum-width))
             (s1 (div0 s 2^fixnum-width)))
        (values s0 s1))))

  (define fxrotate-bit-field
    (lambda (ei1 ei2 ei3 ei4)
      (let* ((n ei1)
             (start ei2)
             (end ei3)
             (count ei4)
             (width (fx- end start)))
        (if (fxpositive? width)
            (let* ((count (fxmod count width))
                   (field0 (fxbit-field n start end))
                   (field1 (fxarithmetic-shift-left field0 count))
                   (field2 (fxarithmetic-shift-right field0 (fx- width count)))
                   (field (fxior field1 field2)))
              (fxcopy-bit-field n start end field))
            n))))

  (define fxreverse-bit-field
    (lambda (ei1 ei2 ei3)
      (let* ((n ei1)
             (start ei2)
             (end ei3)
             (width (fx- end start)))
        (if (fxpositive? width)
            (let loop ((reversed 0) (field (fxbit-field n start end)) (width width))
              (if (fxzero? width)
                  (fxcopy-bit-field n start end reversed)
                  (if (fxzero? (fxand field 1))
                      (loop (fxarithmetic-shift-left reversed 1)
                            (fxarithmetic-shift-right field 1)
                            (fx- width 1))
                      (loop (fxior (fxarithmetic-shift-left reversed 1) 1)
                            (fxarithmetic-shift-right field 1)
                            (fx- width 1)))))
            n))))

  (define bitwise-arithmetic-shift-left bitwise-arithmetic-shift)
  
  (define bitwise-if
    (lambda (ei1 ei2 ei3)
      (bitwise-ior (bitwise-and ei1 ei2)
                   (bitwise-and (bitwise-not ei1) ei3))))

  (define bitwise-bit-set?
    (lambda (ei1 ei2)
      (not (zero? (bitwise-and (bitwise-arithmetic-shift 1 ei2) ei1)))))
  
  (define bitwise-copy-bit
    (lambda (ei1 ei2 ei3)
      (let* ((mask (bitwise-arithmetic-shift 1 ei2)))
        (bitwise-if mask (bitwise-arithmetic-shift ei3 ei2) ei1))))

  (define bitwise-bit-field
    (lambda (ei1 ei2 ei3)
      (let ((mask (bitwise-not (bitwise-arithmetic-shift -1 ei3))))
        (bitwise-arithmetic-shift-right (bitwise-and ei1 mask) ei2))))

  (define bitwise-copy-bit-field
    (lambda (ei1 ei2 ei3 ei4)
      (let* ((to ei1)
             (start ei2)
             (end ei3)
             (from ei4)
             (mask1 (bitwise-arithmetic-shift -1 start))
             (mask2 (bitwise-not (bitwise-arithmetic-shift -1 end)))
             (mask (bitwise-and mask1 mask2)))
        (bitwise-if mask (bitwise-arithmetic-shift from start) to))))

  (define bitwise-arithmetic-shift-right
    (lambda (ei1 ei2)
      (bitwise-arithmetic-shift ei1 (- ei2))))
  
  (define bitwise-rotate-bit-field
    (lambda (ei1 ei2 ei3 ei4)
      (let* ((n ei1)
             (start ei2)
             (end ei3)
             (count ei4)
             (width (- end start)))
        (if (positive? width)
            (let* ((count (mod count width))
                   (field0 (bitwise-bit-field n start end))
                   (field1 (bitwise-arithmetic-shift field0 count))
                   (field2 (bitwise-arithmetic-shift-right field0 (- width count)))
                   (field (bitwise-ior field1 field2)))
              (bitwise-copy-bit-field n start end field))
            n))))

  (define bitwise-reverse-bit-field
    (lambda (ei1 ei2 ei3)
      (let* ((n ei1)
             (start ei2)
             (end ei3)
             (width (- end start)))
        (if (positive? width)
            (let loop ((reversed 0) (field (bitwise-bit-field n start end)) (width width))
              (if (zero? width)
                  (bitwise-copy-bit-field n start end reversed)
                  (if (zero? (bitwise-and field 1))
                      (loop (bitwise-arithmetic-shift reversed 1)
                            (bitwise-arithmetic-shift-right field 1)
                            (- width 1))
                      (loop (bitwise-ior (bitwise-arithmetic-shift reversed 1) 1)
                            (bitwise-arithmetic-shift-right field 1)
                            (- width 1)))))
            n))))

  ) ;[end]


