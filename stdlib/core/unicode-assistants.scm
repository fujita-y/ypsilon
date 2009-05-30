#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core unicode-assistants)

  (export general-category
          simple-uppercase
          simple-lowercase
          simple-titlecase
          numeric-property?
          other-uppercase-property?
          other-lowercase-property?
          other-alphabetic-property?
          special-casing-lower
          special-casing-title
          special-casing-upper
          foldcase
          pair-wise-composition
          recursive-decomposition
          decompose
          sort-combining-marks!
          compose)

  (import (core primitives)
          (core io)
          (core files)
          (core lists)
          (core bytevectors)
          (core bytevector-transcoders))

  (define load-unicode-table-file
    (lambda (name k)
      (call-with-port
          (open-builtin-data-input-port name)
          (lambda (port)
            (let ((ht (make-core-hashtable 'eqv? k)))
              (let loop ((lst (get-datum port)))
                (cond ((null? lst))
                      (else
                       (core-hashtable-set! ht (caar lst) (cdar lst))
                       (loop (cdr lst)))))
              (core-hashtable-copy ht))))))

  (define load-unicode-list-file
    (lambda (name)
      (call-with-port
          (open-builtin-data-input-port name)
          (lambda (port)
            (get-datum port)))))

  (define-syntax autoload
    (syntax-rules ()
      ((_ var init)
       (begin
         (define data)
         (define var (lambda () (and (unspecified? data) (set! data init)) data))))))

  (autoload
   general-category-table-1
   (load-unicode-table-file "general-category-1" 1000))

  (autoload
   general-category-table-2
   (load-unicode-table-file "general-category-2" 170000))

  (autoload
   simple-uppercase-table
   (load-unicode-table-file "simple-uppercase" 1500))

  (autoload
   simple-lowercase-table
   (load-unicode-table-file "simple-lowercase" 1500))

  (autoload
   simple-titlecase-table
   (load-unicode-table-file "simple-titlecase" 1500))

  (autoload
   numeric-property-table
   (load-unicode-table-file "numeric-property" 1500))

  (autoload
   special-casing-lower-table
   (load-unicode-table-file "special-casing-lower" 300))

  (autoload
   special-casing-title-table
   (load-unicode-table-file "special-casing-title" 300))

  (autoload
   special-casing-upper-table
   (load-unicode-table-file "special-casing-upper" 300))

  (autoload
   case-folding-table
   (load-unicode-table-file "case-folding" 1500))

  (autoload
   other-uppercase-list
   (load-unicode-list-file "other-uppercase"))

  (autoload
   other-lowercase-list
   (load-unicode-list-file "other-lowercase"))

  (autoload
   other-alphabetic-list
   (load-unicode-list-file "other-alphabetic"))

  (define general-category
    (lambda (c)
      (let ((cp (char->integer c)))
        (or (core-hashtable-ref (general-category-table-1) cp #f)
            (core-hashtable-ref (general-category-table-2) cp #f)
            (cond
             ;3400;<CJK Ideograph Extension A, First>;Lo;0;L;;;;;N;;;;;
             ;4DB5;<CJK Ideograph Extension A, Last>;Lo;0;L;;;;;N;;;;;
             ((<= #x3400 cp #x4DB5) 'Lo)
             ; 4E00;<CJK Ideograph, First>;Lo;0;L;;;;;N;;;;;
             ; 9FBB;<CJK Ideograph, Last>;Lo;0;L;;;;;N;;;;;
             ((<= #x4E00 cp #x9FBB) 'Lo)
             ; AC00;<Hangul Syllable, First>;Lo;0;L;;;;;N;;;;;
             ; D7A3;<Hangul Syllable, Last>;Lo;0;L;;;;;N;;;;;
             ((<= #xAC00 cp #xD7A3) 'Lo)
             ; D800;<Non Private Use High Surrogate, First>;Cs;0;L;;;;;N;;;;;
             ; DB7F;<Non Private Use High Surrogate, Last>;Cs;0;L;;;;;N;;;;;
             ((<= #xD800 cp #xDB7F) 'Cs)
             ; DB80;<Private Use High Surrogate, First>;Cs;0;L;;;;;N;;;;;
             ; DBFF;<Private Use High Surrogate, Last>;Cs;0;L;;;;;N;;;;;
             ((<= #xDB80 cp #xDBFF) 'Cs)
             ; DC00;<Low Surrogate, First>;Cs;0;L;;;;;N;;;;;
             ; DFFF;<Low Surrogate, Last>;Cs;0;L;;;;;N;;;;;
             ((<= #xDC00 cp #xDFFF) 'Cs)
             ; E000;<Private Use, First>;Co;0;L;;;;;N;;;;;
             ; F8FF;<Private Use, Last>;Co;0;L;;;;;N;;;;;
             ((<= #xE000 cp #xF8FF) 'Co)
             ; 20000;<CJK Ideograph Extension B, First>;Lo;0;L;;;;;N;;;;;
             ; 2A6D6;<CJK Ideograph Extension B, Last>;Lo;0;L;;;;;N;;;;;
             ((<= #x20000 cp #x2A6D6) 'Lo)
             ; F0000;<Plane 15 Private Use, First>;Co;0;L;;;;;N;;;;;
             ; FFFFD;<Plane 15 Private Use, Last>;Co;0;L;;;;;N;;;;;
             ((<= #xF0000 cp #xFFFFD) 'Co)
             ; 100000;<Plane 16 Private Use, First>;Co;0;L;;;;;N;;;;;
             ; 10FFFD;<Plane 16 Private Use, Last>;Co;0;L;;;;;N;;;;;
             ((<= #x100000 cp #x10FFFD) 'Co)
             (else 'Cn))))))

  (define numeric-property?
    (lambda (c)
      (and (core-hashtable-ref (numeric-property-table) (char->integer c) #f) #t)))

  (define simple-uppercase
    (lambda (c)
      (cond ((core-hashtable-ref (simple-uppercase-table) (char->integer c) #f)
             => (lambda (n) (integer->char n)))
            (else c))))

  (define simple-lowercase
    (lambda (c)
      (cond ((core-hashtable-ref (simple-lowercase-table) (char->integer c) #f)
             => (lambda (n) (integer->char n)))
            (else c))))

  (define simple-titlecase
    (lambda (c)
      (cond ((core-hashtable-ref (simple-titlecase-table) (char->integer c) #f)
             => (lambda (n) (integer->char n)))
            (else (simple-uppercase c)))))

  (define other-uppercase-property?
    (lambda (c)
      (let ((cp (char->integer c)))
        (and (<= #x2160 cp #x24CF)
             (exists (lambda (a) (<= (car a) cp (cdr a))) (other-uppercase-list))))))

  (define other-lowercase-property?
    (lambda (c)
      (let ((cp (char->integer c)))
        (and (<= #x2B0 cp #x24E9)
             (exists (lambda (a) (<= (car a) cp (cdr a))) (other-lowercase-list))))))

  (define other-alphabetic-property?
    (lambda (c)
      (let ((cp (char->integer c)))
        (and (<= #x345 cp #x10A0F)
             (exists (lambda (a) (<= (car a) cp (cdr a))) (other-alphabetic-list))))))

  (define special-casing-lower
    (lambda (c)
      (core-hashtable-ref (special-casing-lower-table) (char->integer c) #f)))

  (define special-casing-title
    (lambda (c)
      (core-hashtable-ref (special-casing-title-table) (char->integer c) #f)))

  (define special-casing-upper
    (lambda (c)
      (core-hashtable-ref (special-casing-upper-table) (char->integer c) #f)))

  (define foldcase
    (lambda (c)
      (core-hashtable-ref (case-folding-table) (char->integer c) #f)))

  (autoload
   canonical-class-table
   (load-unicode-table-file "canonical-class" 1500))

  (autoload
   decompose-table
   (load-unicode-table-file "decompose" 80000))

  (autoload
   compose-table
   (load-unicode-table-file "compose" 5000))

  (autoload
   compatibility-table
   (load-unicode-table-file "compatibility" 5000))

  (define SBase #xAC00)
  (define LBase #x1100)
  (define VBase #x1161)
  (define TBase #x11A7)
  (define LCount 19)
  (define VCount 21)
  (define TCount 28)
  (define NCount (* VCount TCount))
  (define SCount (* LCount NCount))

  (define pair-wise-composition
    (lambda (first second)
      (cond ((or (< first 0) (> first #x10FFFF) (< second 0) (> second #x10FFFF)) #f)
            (else
             (let ((LIndex (- first LBase)) (VIndex (- second VBase)) (SIndex (- first SBase)) (TIndex (- second TBase)))
               (cond ((and (< -1 LIndex LCount) (< -1 VIndex VCount))
                      (+ SBase (* TCount (+ VIndex (* LIndex VCount)))))
                     ((and (< -1 SIndex SCount) (< -1 TIndex TCount) (= 0 (mod SIndex TCount)))
                      (+ first TIndex))
                     (else
                      (core-hashtable-ref (compose-table) (+ (* first #x10000) second) #f))))))))

  (define recursive-decomposition
    (lambda (canonical sv output)
      (let ((lst (core-hashtable-ref (decompose-table) sv #f)))
        (let ((SIndex (- sv SBase)))
          (cond ((and lst (not (and canonical (core-hashtable-ref (compatibility-table) sv #f))))
                 (for-each (lambda (sv) (recursive-decomposition canonical sv output)) lst))
                ((< -1 SIndex SCount)
                 (let ((L (+ LBase (div SIndex NCount)))
                       (V (+ VBase (div (mod SIndex NCount) TCount)))
                       (T (+ TBase (mod SIndex TCount))))
                   (put-char output (integer->char L))
                   (put-char output (integer->char V))
                   (or (= T TBase) (put-char output (integer->char T)))))
                (else
                 (put-char output (integer->char sv))))))))

  (define decompose
    (lambda (input canonical)
      (let ((output (make-string-output-port)))
        (let loop ((ch (get-char input)))
          (cond ((eof-object? ch)
                 (string->utf32 (extract-accumulated-string output) (native-endianness)))
                (else
                 (recursive-decomposition canonical (char->integer ch) output)
                 (loop (get-char input))))))))

  (define canonical-class
    (lambda (sv)
      (core-hashtable-ref (canonical-class-table) sv 0)))

  (define sort-combining-marks!
    (lambda (bv)
      (let ((last (- (bytevector-length bv) 4)))
        (let loop ((i 0))
          (cond ((>= i last) bv)
                (else
                 (let ((this (bytevector-u32-native-ref bv i))
                       (next (bytevector-u32-native-ref bv (+ i 4))))
                   (let ((this-cc (canonical-class this))
                         (next-cc (canonical-class next)))
                     (cond ((and (> this-cc 0) (> next-cc 0) (> this-cc next-cc))
                            (bytevector-u32-native-set! bv i next)
                            (bytevector-u32-native-set! bv (+ i 4) this)
                            (loop (if (>= i 4) (- i 4) 4)))
                           (else
                            (loop (+ i 4))))))))))))

  (define compose
    (lambda (bv)
      (let ((len (bytevector-length bv)))
        (let* ((first (bytevector-u32-native-ref bv 0))
               (first-cc (if (zero? (canonical-class first)) 0 256)))
          (let loop ((i 4) (starter first) (starter-cc first-cc) (starter-pos 0) (comp-pos 4))
            (cond ((>= i len)
                   (let ((output (make-bytevector comp-pos)))
                     (bytevector-copy! bv 0 output 0 comp-pos)
                     (utf32->string output (native-endianness) #t)))
                  (else
                   (let* ((this (bytevector-u32-native-ref bv i))
                          (this-cc (canonical-class this)))
                     (cond ((and (or (= starter-cc 0) (< starter-cc this-cc))
                                 (pair-wise-composition starter this))
                            => (lambda (composit)
                                 (bytevector-u32-native-set! bv starter-pos composit)
                                 (loop (+ i 4) composit (canonical-class composit) starter-pos comp-pos)))
                           (else
                            (bytevector-u32-native-set! bv comp-pos this)
                            (if (= this-cc 0)
                                (loop (+ i 4) this this-cc comp-pos (+ comp-pos 4))
                                (loop (+ i 4) starter this-cc starter-pos (+ comp-pos 4)))))))))))))
  ) ;[end]
