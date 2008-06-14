
;; Note: This file must be utf-8. Confirm you see lambda -> "λ"

(import (core) (test-lite))

(format #t "Test Examples of R6RS Standard Libraries\n\n")

(test-begin "1.1 Characters")
(test-equal "char 1" (char-upcase #\i) => #\I)
(test-equal "char 1" (char-downcase #\i) => #\i)
(test-equal "char 2" (char-titlecase #\i) => #\I)
(test-equal "char 3" (char-foldcase #\i) => #\i)
(test-equal "char 4" (char-upcase #\ß) => #\ß)
(test-equal "char 5" (char-downcase #\ß) => #\ß)
(test-equal "char 6" (char-titlecase #\ß) => #\ß)
(test-equal "char 7" (char-foldcase #\ß) => #\ß)
(test-equal "char 8" (char-upcase #\Σ) => #\Σ)
(test-equal "char 9" (char-downcase #\Σ) => #\σ)
(test-equal "char 10" (char-titlecase #\Σ) => #\Σ)
(test-equal "char 11" (char-foldcase #\Σ) => #\σ)
(test-equal "char 12" (char-upcase #\ς) => #\Σ)
(test-equal "char 13" (char-downcase #\ς) => #\ς)
(test-equal "char 14" (char-titlecase #\ς) => #\Σ)
(test-equal "char 15" (char-foldcase #\ς) => #\σ)
(test-equal "char 16" (char-ci<? #\z #\Z) => #f)
(test-equal "char 17" (char-ci=? #\z #\Z) => #t)
(test-equal "char 18" (char-ci=? #\ς #\σ) => #t)
(test-equal "char 19" (char-alphabetic? #\a) => #t)
(test-equal "char 20" (char-numeric? #\1) => #t)
(test-equal "char 21" (char-whitespace? #\space) => #t)
(test-equal "char 22" (char-whitespace? #\x00A0) => #t)
(test-equal "char 23" (char-upper-case? #\Σ) => #t)
(test-equal "char 24" (char-lower-case? #\σ) => #t)
(test-equal "char 25" (char-lower-case? #\x00AA) => #t)
(test-equal "char 26" (char-title-case? #\I) => #f)
(test-equal "char 27" (char-title-case? #\x01C5) => #t)
(test-equal "char 28" (char-general-category #\a) => Ll)
(test-equal "char 29" (char-general-category #\space) => Zs)
(test-equal "char 30" (char-general-category #\x10FFFF) => Cn)
(test-end)

(test-begin "1.2 Strings")
(test-equal "str 1" (string-upcase "Hi") => "HI")
(test-equal "str 2" (string-downcase "Hi") => "hi")
(test-equal "str 3" (string-foldcase "Hi") => "hi")
(test-equal "str 4" (string-upcase "Straße") => "STRASSE")
(test-equal "str 5" (string-downcase "Straße") => "straße")
(test-equal "str 6" (string-foldcase "Straße") => "strasse")
(test-equal "str 7" (string-downcase "STRASSE") => "strasse")
(test-equal "str 8" (string-downcase "Σ") => "σ")
(test-equal "str 9" (string-upcase "ΧΑΟΣ") => "ΧΑΟΣ")
(test-equal "str 10" (string-downcase "ΧΑΟΣ") => "χαος")
(test-equal "str 11" (string-downcase "ΧΑΟΣΣ") => "χαοσς")
(test-equal "str 12" (string-downcase "ΧΑΟΣ Σ") => "χαος σ")
(test-equal "str 13" (string-foldcase "ΧΑΟΣΣ") => "χαοσσ")
(test-equal "str 14" (string-upcase "χαος") => "ΧΑΟΣ")
(test-equal "str 15" (string-upcase "χαοσ") => "ΧΑΟΣ")
(test-equal "str 16" (string-titlecase "kNock KNoCK") => "Knock Knock")
(test-equal "str 17" (string-titlecase "who's there?") => "Who's There?")
(test-equal "str 18" (string-titlecase "r6rs") => "R6Rs")
(test-equal "str 19" (string-titlecase "R6RS") => "R6Rs")
(test-equal "str 20" (string-ci<? "z" "Z") => #f)
(test-equal "str 21" (string-ci=? "z" "Z") => #t)
(test-equal "str 22" (string-ci=? "Straße" "Strasse") => #t)
(test-equal "str 23" (string-ci=? "Straße" "STRASSE") => #t)
(test-equal "str 24" (string-ci=? "ΧΑΟΣ" "χαοσ") => #t)
(test-equal "str 25" (string-normalize-nfd "\xE9;") => "\x65;\x301;")
(test-equal "str 26" (string-normalize-nfc "\xE9;") => "\xE9;")
(test-equal "str 27" (string-normalize-nfd "\x65;\x301;") => "\x65;\x301;")
(test-equal "str 28" (string-normalize-nfc "\x65;\x301;") => "\xE9;")
(test-end)

(test-begin "2.2 General operations")
(test-equal "bv 1"
            (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
              (bytevector-copy! b 0 b 3 4)
              (bytevector->u8-list b))
            => (1 2 3 1 2 3 4 8))
(test-end)

(test-begin "2.3 Operations on bytes and octets")
(test-equal "bv 2"
            (let ((b1 (make-bytevector 16 -127))
                  (b2 (make-bytevector 16 255)))
              (list
               (bytevector-s8-ref b1 0)
               (bytevector-u8-ref b1 0)
               (bytevector-s8-ref b2 0)
               (bytevector-u8-ref b2 0)))
            => (-127 129 -1 255))
(test-equal "bv 3"
            (let ((b (make-bytevector 16 -127)))
              (bytevector-s8-set! b 0 -126)
              (bytevector-u8-set! b 1 246)
              (list
               (bytevector-s8-ref b 0)
               (bytevector-u8-ref b 0)
               (bytevector-s8-ref b 1)
               (bytevector-u8-ref b 1)))
            => (-126 130 -10 246))
(test-end)

(test-begin "2.4 Operations on integers of arbitrary size")
(test-eval! (define b (make-bytevector 16 -127)))
(test-eval! (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16))
(test-equal "bv 4"
            (bytevector-uint-ref b 0 (endianness little) 16)
            => #xfffffffffffffffffffffffffffffffd)
(test-equal "bv 5" (bytevector-sint-ref b 0 (endianness little) 16) => -3)
(test-equal "bv 6"
            (bytevector->u8-list b)
            => (253 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255))
(test-eval! (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness big) 16))
(test-equal "bv 7"
            (bytevector-uint-ref b 0 (endianness big) 16)
            => #xfffffffffffffffffffffffffffffffd)
(test-equal "bv 8" (bytevector-sint-ref b 0 (endianness big) 16)
            => -3)
(test-equal "bv 9"
            (bytevector->u8-list b)
            => (255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253))
(test-equal "bv 10"
            (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
              (bytevector->sint-list b (endianness little) 2))
            => (513 -253 513 513))
(test-equal "bv 11"
            (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
              (bytevector->uint-list b (endianness little) 2))
            => (513 65283 513 513))
(test-end)

(test-begin "2.5 Operations on 16-bit integers")
(test-eval! (define b
              (u8-list->bytevector
               '(255 255 255 255 255 255 255 255
                     255 255 255 255 255 255 255 253))))
(test-equal "bv 12" (bytevector-u16-ref b 14 (endianness little)) => 65023)
(test-equal "bv 13" (bytevector-s16-ref b 14 (endianness little)) => -513)
(test-equal "bv 14" (bytevector-u16-ref b 14 (endianness big)) => 65533)
(test-equal "bv 15" (bytevector-s16-ref b 14 (endianness big)) => -3)
(test-eval! (bytevector-u16-set! b 0 12345 (endianness little)))
(test-equal "bv 16" (bytevector-u16-ref b 0 (endianness little)) => 12345)
(test-eval! (bytevector-u16-native-set! b 0 12345))
(test-equal "bv 17" (bytevector-u16-native-ref b 0) => 12345)
(test-end)

(test-begin "2.6 Operations on 32-bit integers")
(test-eval! (define b
              (u8-list->bytevector
               '(255 255 255 255 255 255 255 255
                     255 255 255 255 255 255 255 253))))
(test-equal "bv 18" (bytevector-u32-ref b 12 (endianness little)) => 4261412863)
(test-equal "bv 19" (bytevector-s32-ref b 12 (endianness little)) => -33554433)
(test-equal "bv 20" (bytevector-u32-ref b 12 (endianness big)) => 4294967293)
(test-equal "bv 21" (bytevector-s32-ref b 12 (endianness big)) => -3)
(test-end)

(test-begin "2.7 Operations on 64-bit integers")
(test-eval! (define b
              (u8-list->bytevector
               '(255 255 255 255 255 255 255 255
                     255 255 255 255 255 255 255 253))))
(test-equal "bv 22" (bytevector-u64-ref b 8 (endianness little)) => 18302628885633695743)
(test-equal "bv 23" (bytevector-s64-ref b 8 (endianness little)) => -144115188075855873)
(test-equal "bv 24" (bytevector-u64-ref b 8 (endianness big)) => 18446744073709551613)
(test-equal "bv 25" (bytevector-s64-ref b 8 (endianness big)) => -3)
(test-end)

(test-begin "3 List utilities")
(test-equal "lst 1" (find even? '(3 1 4 1 5 9)) => 4)
(test-equal "lst 2" (find even? '(3 1 5 1 5 9)) => #f)
(test-equal "lst 3" (for-all even? '(3 1 4 1 5 9)) => #f)
(test-equal "lst 4" (for-all even? '(3 1 4 1 5 9 . 2)) => #f)
(test-equal "lst 5" (for-all even? '(2 4 14)) => #t)
(test-assertion-violation "lst exn 1" (for-all even? '(2 4 14 . 9)))
(test-equal "lst 6" (for-all (lambda (n) (and (even? n) n)) '(2 4 14)) => 14)
(test-equal "lst 7" (for-all < '(1 2 3) '(2 3 4)) => #t)
(test-equal "lst 8" (for-all < '(1 2 4) '(2 3 4)) => #f)
(test-equal "lst 9" (exists even? '(3 1 4 1 5 9)) => #t)
(test-equal "lst 10" (exists even? '(3 1 1 5 9)) => #f)
(test-assertion-violation "lst exn 2" (exists even? '(3 1 1 5 9 . 2)))
(test-equal "lst 11" (exists (lambda (n) (and (even? n) n)) '(2 1 4 14)) => 2)
(test-equal "lst 12" (exists < '(1 2 4) '(2 3 4)) => #t)
(test-equal "lst 13" (exists > '(1 2 3) '(2 3 4)) => #f)
(test-equal "lst 14" (filter even? '(3 1 4 1 5 9 2 6)) => (4 2 6))
(test-equal "lst 15"
            (call-with-values (lambda () (partition even? '(3 1 4 1 5 9 2 6))) list)
            => ((4 2 6) (3 1 1 5 9)))
(test-equal "lst 16" (fold-left + 0 '(1 2 3 4 5)) => 15)
(test-equal "lst 17"
            (fold-left (lambda (a e) (cons e a)) '()
                       '(1 2 3 4 5))
            => (5 4 3 2 1))
(test-equal "lst 18"
            (fold-left (lambda (count x)
                         (if (odd? x) (+ count 1) count))
                       0
                       '(3 1 4 1 5 9 2 6 5 3))
            => 7)
(test-equal "lst 19"
            (fold-left (lambda (max-len s)
                         (max max-len (string-length s)))
                       0
                       '("longest" "long" "longer"))
            => 7)
(test-equal "lst 20" (fold-left cons '(q) '(a b c)) => ((((q) . a) . b) . c))
(test-equal "lst 21" (fold-left + 0 '(1 2 3) '(4 5 6)) => 21)
(test-equal "lst 22" (fold-right + 0 '(1 2 3 4 5)) => 15)
(test-equal "lst 23" (fold-right cons '() '(1 2 3 4 5)) => (1 2 3 4 5))
(test-equal "lst 24"
            (fold-right (lambda (x l)
                          (if (odd? x) (cons x l) l))
                        '()
                        '(3 1 4 1 5 9 2 6 5))
            => (3 1 1 5 9 5))
(test-equal "lst 25" (fold-right cons '(q) '(a b c)) => (a b c q))
(test-equal "lst 26" (fold-right + 0 '(1 2 3) '(4 5 6)) => 21)
(test-equal "lst 27" (remp even? '(3 1 4 1 5 9 2 6 5)) => (3 1 1 5 9 5))
(test-equal "lst 28" (remove 1 '(3 1 4 1 5 9 2 6 5)) => (3 4 5 9 2 6 5))
(test-equal "lst 29" (remv 1 '(3 1 4 1 5 9 2 6 5)) => (3 4 5 9 2 6 5))
(test-equal "lst 30" (remq 'foo '(bar foo baz)) => (bar baz))
(test-equal "lst 31" (memp even? '(3 1 4 1 5 9 2 6 5)) => (4 1 5 9 2 6 5))
(test-equal "lst 32" (memq 'a '(a b c)) => (a b c))
(test-equal "lst 33" (memq 'b '(a b c)) => (b c))
(test-equal "lst 34" (memq 'a '(b c d)) => #f)
(test-equal "lst 35" (memq (list 'a) '(b (a) c)) => #f)
(test-equal "lst 36" (member (list 'a) '(b (a) c)) => ((a) c))
(test-equal "lst 37" (memv 101 '(100 101 102)) => (101 102))
(test-eval! (define d '((3 a) (1 b) (4 c))))
(test-equal "lst 38" (assp even? d) => (4 c))
(test-equal "lst 39" (assp odd? d) => (3 a))
(test-eval! (define e '((a 1) (b 2) (c 3))))
(test-equal "lst 40" (assq 'a e) => (a 1))
(test-equal "lst 41" (assq 'b e) => (b 2))
(test-equal "lst 42" (assq 'd e) => #f)
(test-equal "lst 43" (assq (list 'a) '(((a)) ((b)) ((c))))
            => #f)
(test-equal "lst 44" (assoc (list 'a) '(((a)) ((b)) ((c))))
            => ((a)))
(test-equal "lst 45" (assv 5 '((2 3) (5 7) (11 13)))
            => (5 7)        )
(test-equal "lst 46" (cons* 1 2 '(3 4 5)) => (1 2 3 4 5))
(test-equal "lst 47" (cons* 1 2 3) => (1 2 . 3))
(test-equal "lst 48" (cons* 1) => 1)
(test-end)

(test-begin "4 Sorting")
(test-equal "sort 1" (list-sort < '(3 5 2 1)) => (1 2 3 5))
(test-equal "sort 2" (vector-sort < '#(3 5 2 1)) => #(1 2 3 5))
(test-eval! (define v (vector 3 5 2 1)))
(test-eval! (vector-sort! < v))
(test-equal "sort 3" v => #(1 2 3 5))
(test-end)

(test-begin "5 Control structures")
(test-equal "ctrl 1" (when (> 3 2) 'greater) => greater)
(test-equal "ctrl 2" (unspecified? (when (< 3 2) 'greater)) => #t)
(test-equal "ctrl 3" (unspecified? (unless (> 3 2) 'less)) => #t)
(test-equal "ctrl 4" (unless (< 3 2) 'less) => less)
(test-equal "ctrl 5" (do ((vec (make-vector 5))
                        (i 0 (+ i 1)))
                       ((= i 5) vec)
                       (vector-set! vec i i)) => #(0 1 2 3 4))
(test-equal "ctrl 6" (let ((x '(1 3 5 7 9)))
                     (do ((x x (cdr x))
                          (sum 0 (+ sum (car x))))
                         ((null? x) sum))) => 25)
(test-eval! (define foo
              (case-lambda
                (() 'zero)
                ((x) (list 'one x))
                ((x y) (list 'two x y))
                ((a b c d . e) (list 'four a b c d e))
                (rest (list 'rest rest)))))
(test-equal "ctrl 7" (foo) => zero)
(test-equal "ctrl 8" (foo 1) => (one 1))
(test-equal "ctrl 9" (foo 1 2) => (two 1 2))
(test-equal "ctrl 10" (foo 1 2 3) => (rest (1 2 3)))
(test-equal "ctrl 11" (foo 1 2 3 4) => (four 1 2 3 4 ()))
(test-end)

(test-begin "6.2 Syntactic layer")
(test-eval! (define-record-type (point make-point point?)
              (fields (immutable x point-x)
                      (mutable y point-y set-point-y!))
              (nongenerative
               point-4893d957-e00b-11d9-817f-00111175eb9e)))
(test-eval! (define-record-type (cpoint make-cpoint cpoint?)
              (parent point)
              (protocol
               (lambda (n)
                 (lambda (x y c)
                   ((n x y) (color->rgb c)))))
              (fields
               (mutable rgb cpoint-rgb cpoint-rgb-set!))))
(test-eval! (define (color->rgb c)
              (cons 'rgb c)))
(test-eval! (define p1 (make-point 1 2)))
(test-eval! (define p2 (make-cpoint 3 4 'red)))
(test-equal "rec stx 1" (point? p1) => #t)
(test-equal "rec stx 2" (point? p2) => #t)
(test-equal "rec stx 3" (point? (vector)) => #f)
(test-equal "rec stx 4" (point? (cons 'a 'b)) => #f)
(test-equal "rec stx 5" (cpoint? p1) => #f)
(test-equal "rec stx 6" (cpoint? p2) => #t)
(test-equal "rec stx 7" (point-x p1) => 1)
(test-equal "rec stx 8" (point-y p1) => 2)
(test-equal "rec stx 9" (point-x p2) => 3)
(test-equal "rec stx 10" (point-y p2) => 4)
(test-equal "rec stx 11" (cpoint-rgb p2) => (rgb . red))
(test-equal "rec stx 12" (unspecified? (set-point-y! p1 17)) => #t)
(test-equal "rec stx 13" (point-y p1) => 17)
(test-equal "rec stx 14" (equal? (record-rtd p1) (record-type-descriptor point)) => #t)
(test-eval! (define-record-type (ex1 make-ex1 ex1?)
              (protocol (lambda (p) (lambda a (p a))))
              (fields (immutable f ex1-f))))

(test-eval! (define ex1-i1 (make-ex1 1 2 3)))
(test-equal "rec stx 15" (ex1-f ex1-i1) => (1 2 3))
(test-eval! (define-record-type (ex2 make-ex2 ex2?)
              (protocol
               (lambda (p) (lambda (a . b) (p a b))))
              (fields (immutable a ex2-a)
                      (immutable b ex2-b))))
(test-eval! (define ex2-i1 (make-ex2 1 2 3)))
(test-equal "rec stx 16" (ex2-a ex2-i1) => 1)
(test-equal "rec stx 17" (ex2-b ex2-i1) => (2 3))
(test-eval! (define-record-type (unit-vector
                                 make-unit-vector
                                 unit-vector?)
              (protocol
               (lambda (p)
                 (lambda (x y z)
                   (let ((length
                          (sqrt (+ (* x x)
                                   (* y y)
                                   (* z z)))))
                     (p (/ x length)
                        (/ y length)
                        (/ z length))))))
              (fields (immutable x unit-vector-x)
                      (immutable y unit-vector-y)
                      (immutable z unit-vector-z))))
(test-eval! (define *ex3-instance* #f))
(test-eval! (define-record-type ex3
              (parent cpoint)
              (protocol
               (lambda (n)
                 (lambda (x y t)
                   (let ((r ((n x y 'red) t)))
                     (set! *ex3-instance* r)
                     r))))
              (fields
               (mutable thickness))
              (sealed #t) (opaque #t)))
(test-eval! (define ex3-i1 (make-ex3 1 2 17)))
(test-equal "rec stx 18" (ex3? ex3-i1) => #t)
(test-equal "rec stx 19" (cpoint-rgb ex3-i1) => (rgb . red))
(test-equal "rec stx 20" (ex3-thickness ex3-i1) => 17)
(test-eval! (ex3-thickness-set! ex3-i1 18))
(test-equal "rec stx 21" (ex3-thickness ex3-i1) => 18)
(test-equal "rec stx 22" (equal? *ex3-instance* ex3-i1) => #t)
(test-equal "rec stx 23" (record? ex3-i1) => #f)
(test-end)

(test-begin "6.3 Procedural layer")
(test-eval! (define rtd1
              (make-record-type-descriptor
               'rtd1 #f #f #f #f
               '#((immutable x1) (immutable x2)))))
(test-eval! (define rtd2
              (make-record-type-descriptor
               'rtd2 rtd1 #f #f #f
               '#((immutable x3) (immutable x4)))))
(test-eval! (define rtd3
              (make-record-type-descriptor
               'rtd3 rtd2 #f #f #f
               '#((immutable x5) (immutable x6)))))
(test-eval! (define protocol1
              (lambda (p)
                (lambda (a b c)
                  (p (+ a b) (+ b c))))))
(test-eval! (define protocol2
              (lambda (n)
                (lambda (a b c d e f)
                  (let ((p (n a b c)))
                    (p (+ d e) (+ e f)))))))
(test-eval! (define protocol3
              (lambda (n)
                (lambda (a b c d e f g h i)
                  (let ((p (n a b c d e f)))
                    (p (+ g h) (+ h i)))))))
(test-eval! (define cd1
              (make-record-constructor-descriptor
               rtd1 #f protocol1)))
(test-eval! (define cd2
              (make-record-constructor-descriptor
               rtd2 cd1 protocol2)))
(test-eval! (define cd3
              (make-record-constructor-descriptor
               rtd3 cd2 protocol3)))
(test-eval! (define make-rtd1 (record-constructor cd1)))
(test-eval! (define make-rtd2 (record-constructor cd2)))
(test-eval! (define make-rtd3 (record-constructor cd3)))
(test-equal "rec prc 1" (format "~s" (make-rtd3 1 2 3 4 5 6 7 8 9))
            =>
            "#<record rtd3 3 5 9 11 15 17>")
(test-eval! (define :point
              (make-record-type-descriptor
               'point #f
               #f #f #f
               '#((mutable x) (mutable y)))))
(test-eval! (define :point-cd
              (make-record-constructor-descriptor :point #f #f)))
(test-eval! (define make-point (record-constructor :point-cd)))
(test-eval! (define point? (record-predicate :point)))
(test-eval! (define point-x (record-accessor :point 0)))
(test-eval! (define point-y (record-accessor :point 1)))
(test-eval! (define point-x-set! (record-mutator :point 0)))
(test-eval! (define point-y-set! (record-mutator :point 1)))
(test-eval! (define p1 (make-point 1 2)))
(test-equal "rec prc 2" (point? p1) => #t)
(test-equal "rec prc 3" (point-x p1) => 1)
(test-equal "rec prc 4" (point-y p1) => 2)
(test-equal "rec prc 5" (unspecified? (point-x-set! p1 5)) => #t)
(test-equal "rec prc 6" (point-x p1) => 5)
(test-eval! (define :point2
              (make-record-type-descriptor
               'point2 :point
               #f #f #f '#((mutable x) (mutable y)))))
(test-eval! (define make-point2
              (record-constructor
               (make-record-constructor-descriptor :point2  #f #f))))
(test-eval! (define point2? (record-predicate :point2)))
(test-eval! (define point2-xx (record-accessor :point2 0)))
(test-eval! (define point2-yy (record-accessor :point2 1)))
(test-eval! (define p2 (make-point2 1 2 3 4)))
(test-equal "rec prc 7" (point? p2) => #t)
(test-equal "rec prc 8" (point-x p2) => 1)
(test-equal "rec prc 9" (point-y p2) => 2)
(test-equal "rec prc 10" (point2-xx p2) => 3)
(test-equal "rec prc 11" (point2-yy p2) => 4)
(test-eval! (define :point-cd/abs
              (make-record-constructor-descriptor
               :point #f
               (lambda (new)
                 (lambda (x y)
                   (new (abs x) (abs y)))))))
(test-eval! (define make-point/abs
              (record-constructor :point-cd/abs)))
(test-equal "rec prc 12" (point-x (make-point/abs -1 -2)) => 1)
(test-equal "rec prc 13" (point-y (make-point/abs -1 -2)) => 2)
(test-eval! (define :cpoint
              (make-record-type-descriptor
               'cpoint :point
               #f #f #f
               '#((mutable rgb)))))
(test-eval! (define make-cpoint
              (record-constructor
               (make-record-constructor-descriptor
                :cpoint :point-cd
                (lambda (p)
                  (lambda (x y c)
                    ((p x y) (color->rgb c))))))))
(test-eval! (define make-cpoint/abs
              (record-constructor
               (make-record-constructor-descriptor
                :cpoint :point-cd/abs
                (lambda (p)
                  (lambda (x y c)
                    ((p x y) (color->rgb c))))))))
(test-eval! (define cpoint-rgb (record-accessor :cpoint 0)))
(test-eval! (define (color->rgb c) (cons 'rgb c)))
(test-equal "rec prc 14" (cpoint-rgb (make-cpoint -1 -3 'red)) => (rgb . red))
(test-equal "rec prc 15" (point-x (make-cpoint -1 -3 'red)) => -1)
(test-equal "rec prc 16" (point-x (make-cpoint/abs -1 -3 'red)) => 1)
(test-end)

(test-begin "7.1 Exceptions")
;; add port
(test-eval! (begin
              (define capture-retval)
              (define capture-port)
              (define capture-proc)
              (let-values (((port proc) (open-string-output-port)))
                (set! capture-port port)
                (set! capture-proc proc))))
(test-eval! (set! capture-retval
                  (guard (con
                          ((error? con)
                           (if (message-condition? con)
                               (display (condition-message con) capture-port)
                               (display "an error has occurred" capture-port))
                           'error)
                          ((violation? con)
                           (if (message-condition? con)
                               (display (condition-message con) capture-port)
                               (display "the program has a bug" capture-port))
                           'violation))
                         (raise
                          (condition
                           (make-error)
                           (make-message-condition "I am an error"))))))
(test-equal "except 1" capture-retval => error)
(test-equal "except 2" (capture-proc) => "I am an error")
(test-violation "except 3"
                (guard (con
                        ((error? con)
                         (if (message-condition? con)
                             (display (condition-message con))
                             (display "an error has occurred"))
                         'error))
                       (raise
                        (condition
                         (make-violation)
                         (make-message-condition "I am an error")))))
(test-eval! (set! capture-retval
                  (guard (con
                          ((error? con)
                           (display "error opening file" capture-port)
                           #f))
                         (call-with-input-file "_no_file_foo.scm" read))))
(test-equal "except 4" capture-retval => #f)
(test-equal "except 5" (capture-proc) => "error opening file")
(test-eval! (set! capture-retval
                  (with-exception-handler
                   (lambda (con)
                     (cond
                      ((not (warning? con))
                       (raise con))
                      ((message-condition? con)
                       (display (condition-message con) capture-port))
                      (else
                       (display "a warning has been issued" capture-port)))
                     42)
                   (lambda ()
                     (+ (raise-continuable
                         (condition
                          (make-warning)
                          (make-message-condition
                           "should be a number")))
                        23)))))
(test-equal "except 6" capture-retval => 65)
(test-equal "except 7" (capture-proc) => "should be a number")
(test-end)

(test-begin "7.2.1 Condition objects")
(test-eval! (define-record-type (&cond1 make-cond1 real-cond1?)
              (parent &condition)
              (fields
               (immutable x real-cond1-x))))
(test-eval! (define cond1?
              (condition-predicate
               (record-type-descriptor &cond1))))
(test-eval! (define cond1-x
              (condition-accessor
               (record-type-descriptor &cond1)
               real-cond1-x)))
(test-eval! (define foo (make-cond1 'foo)))
(test-equal "cond 1" (condition? foo) => #t)
(test-equal "cond 2" (cond1? foo) => #t)
(test-equal "cond 3" (cond1-x foo) => foo)
(test-eval! (define-record-type (&cond2 make-cond2 real-cond2?)
              (parent &condition)
              (fields
               (immutable y real-cond2-y))))
(test-eval! (define cond2?
              (condition-predicate
               (record-type-descriptor &cond2))))
(test-eval! (define cond2-y
              (condition-accessor
               (record-type-descriptor &cond2)
               real-cond2-y)))
(test-eval! (define bar (make-cond2 'bar)))
(test-equal "cond 4" (condition? (condition foo bar)) => #t)
(test-equal "cond 5" (cond1? (condition foo bar)) => #t)
(test-equal "cond 6" (cond2? (condition foo bar)) => #t)
(test-equal "cond 7" (cond1? (condition foo)) => #t)
(test-equal "cond 8" (real-cond1? (condition foo bar)) => #f)
(test-equal "cond 9" (cond1-x (condition foo bar)) => foo)
(test-equal "cond 10" (cond2-y (condition foo bar)) => bar)
(test-equal "cond 11" (equal? (simple-conditions (condition foo bar)) (list foo bar)) => #t)
(test-equal "cond 12" (equal? (simple-conditions (condition foo (condition bar))) (list foo bar)) => #t)
(test-eval! (define-condition-type &c &condition
              make-c c?
              (x c-x)))
(test-eval! (define-condition-type &c1 &c
              make-c1 c1?
              (a c1-a)))
(test-eval! (define-condition-type &c2 &c
              make-c2 c2?
              (b c2-b)))
(test-eval! (define v1 (make-c1 "V1" "a1")))
(test-equal "cond 13" (c? v1) => #t)
(test-equal "cond 14" (c1? v1) => #t)
(test-equal "cond 15" (c2? v1) => #f)
(test-equal "cond 16" (c-x v1) => "V1")
(test-equal "cond 17" (c1-a v1) => "a1")
(test-eval! (define v2 (make-c2 "V2" "b2")))
(test-equal "cond 18" (c? v2) => #t)
(test-equal "cond 19" (c1? v2) => #f)
(test-equal "cond 20" (c2? v2) => #t)
(test-equal "cond 21" (c-x v2) => "V2")
(test-equal "cond 22" (c2-b v2) => "b2")
(test-eval! (define v3 (condition
                        (make-c1 "V3/1" "a3")
                        (make-c2 "V3/2" "b3"))))
(test-equal "cond 23" (c? v3) => #t)
(test-equal "cond 24" (c1? v3) => #t)
(test-equal "cond 25" (c2? v3) => #t)
(test-equal "cond 26" (c-x v3) => "V3/1")
(test-equal "cond 27" (c1-a v3) => "a3")
(test-equal "cond 28" (c2-b v3) => "b3")
(test-eval! (define v4 (condition v1 v2)))
(test-equal "cond 29" (c? v4) => #t)
(test-equal "cond 30" (c1? v4) => #t)
(test-equal "cond 31" (c2? v4) => #t)
(test-equal "cond 32" (c-x v4) => "V1")
(test-equal "cond 33" (c1-a v4) => "a1")
(test-equal "cond 34" (c2-b v4) => "b2")
(test-eval! (define v5 (condition v2 v3)))
(test-equal "cond 35" (c? v5) => #t)
(test-equal "cond 36" (c1? v5) => #t)
(test-equal "cond 37" (c2? v5) => #t)
(test-equal "cond 37" (c-x v5) => "V2")
(test-equal "cond 38" (c1-a v5) => "a3")
(test-equal "cond 39" (c2-b v5) => "b2")
(test-end)

(test-begin "11.2 Fixnums")
(test-equal "fix 1" (fxfirst-bit-set 0) => -1)
(test-equal "fix 2" (fxfirst-bit-set 1) => 0)
(test-equal "fix 3" (fxfirst-bit-set -4) => 2)
(test-equal "fix 4" (fxreverse-bit-field #b1010010 1 4) => #b1011000)
(test-end)

(test-begin "11.3 Flonums")
(test-equal "flo 1" (fl=? +inf.0 +inf.0) => #t)
(test-equal "flo 2" (fl=? -inf.0 +inf.0) => #f)
(test-equal "flo 3" (fl=? -inf.0 -inf.0) => #t)
(test-equal "flo 4" (fl=? 0.0 -0.0) => #t)
(test-equal "flo 5" (fl<? 0.0 -0.0) => #f)
(test-equal "flo 6" (fl=? +nan.0 1.0) => #f)
(test-equal "flo 7" (fl<? +nan.0 1.0) => #f)
(test-equal "flo 8" (flnegative? -0.0) => #f)
(test-equal "flo 9" (flfinite? +inf.0) => #f)
(test-equal "flo 11" (flfinite? 5.0) => #t)
(test-equal "flo 12" (flinfinite? 5.0) => #f)
(test-equal "flo 13" (flinfinite? +inf.0) => #t)
(test-equal "flo 14" (nan? (fl+ +inf.0 -inf.0)) => #t)
(test-equal "flo 15" (nan? (fl+ +nan.0 1.0)) => #t)
(test-equal "flo 16" (nan? (fl* +nan.0 1.0)) => #t)
(test-equal "flo 17" (nan? (fl- +inf.0 +inf.0)) => #t)
(test-equal "flo 18" (fl/ 1.0 0.0) => +inf.0)
(test-equal "flo 19" (fl/ -1.0 0.0) => -inf.0)
(test-equal "flo 20" (nan? (fl/ 0.0 0.0)) => #t)
(test-equal "flo 21" (flnumerator +inf.0) => +inf.0)
(test-equal "flo 22" (flnumerator -inf.0) => -inf.0)
(test-equal "flo 23" (fldenominator +inf.0) => 1.0)
(test-equal "flo 24" (fldenominator -inf.0) => 1.0)
(test-equal "flo 25" (flnumerator 0.75) => 3.0)
(test-equal "flo 26" (fldenominator 0.75) => 4.0)
(test-equal "flo 27" (flnumerator -0.0) => -0.0)
(test-equal "flo 28" (flfloor +inf.0) => +inf.0)
(test-equal "flo 29" (flceiling -inf.0) => -inf.0)
(test-equal "flo 30" (nan? (fltruncate +nan.0)) => #t)
(test-equal "flo 31" (flexp +inf.0) => +inf.0)
(test-equal "flo 32" (flexp -inf.0) => 0.0)
(test-equal "flo 33" (fllog +inf.0) => +inf.0)
(test-equal "flo 34" (fllog 0.0) => -inf.0)
(test-equal "flo 35" (nan? (fllog -inf.0)) => #t)
(test-equal "flo 36" (flatan -inf.0) => -1.5707963267948965)
(test-equal "flo 37" (flatan +inf.0) => 1.5707963267948965)
(test-equal "flo 38" (flsqrt +inf.0) => +inf.0)
(test-equal "flo 39" (flsqrt -0.0) => -0.0)
(test-end)

(test-begin "11.4 Exact bitwise arithmetic")
(test-equal "bit 1" (bitwise-first-bit-set 0) => -1)
(test-equal "bit 2" (bitwise-first-bit-set 1) => 0)
(test-equal "bit 3" (bitwise-first-bit-set -4) => 2)
(test-equal "bit 4" (bitwise-arithmetic-shift -6 -1) => -3)
(test-equal "bit 5" (bitwise-arithmetic-shift -5 -1) => -3)
(test-equal "bit 6" (bitwise-arithmetic-shift -4 -1) => -2)
(test-equal "bit 7" (bitwise-arithmetic-shift -3 -1) => -2)
(test-equal "bit 8" (bitwise-arithmetic-shift -2 -1) => -1)
(test-equal "bit 9" (bitwise-arithmetic-shift -1 -1) => -1                                            )
(test-equal "bit 10" (bitwise-reverse-bit-field #b1010010 1 4) => #b1011000)
(test-end)

(test-begin "12.4 Parsing input and producing output")
(test-eval! (define p (cons 4 5)))
(test-eval! (define-syntax p.car
              (lambda (x)
                (syntax-case x ()
                  [(_ . rest) #'((car p) . rest)]
                  [_  #'(car p)]))))
(test-equal "sc 1" p.car => 4)
(test-syntax-violation "sc exn 1" (set! p.car 15)); => &syntax exception)
(test-eval! (define p (cons 4 5)))
(test-eval! (define-syntax p.car
              (make-variable-transformer
               (lambda (x)
                 (syntax-case x (set!)
                   [(set! _ e) #'(set-car! p e)]
                   [(_ . rest) #'((car p) . rest)]
                   [_  #'(car p)])))))
(test-eval! (set! p.car 15))
(test-equal "sc 2" p.car => 15)
(test-equal "sc 3" p => (15 . 5))
(test-eval! (define-syntax rec
              (lambda (x)
                (syntax-case x ()
                  [(_ x e)
                   (identifier? #'x)
                   #'(letrec ([x e]) x)]))))
(test-equal "sc 4"
            (map (rec fact
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n (fact (- n 1))))))
                 '(1 2 3 4 5))
            => (1 2 6 24 120))
(test-syntax-violation "sc exn 2" (rec 5 (lambda (x) x))); => &syntax exception)
(test-equal "sc 5"
            (let ([fred 17])
              (define-syntax a
                (lambda (x)
                  (syntax-case x ()
                    [(_ id) #'(b id fred)])))
              (define-syntax b
                (lambda (x)
                  (syntax-case x ()
                    [(_ id1 id2)
                     #`(list
                        #,(free-identifier=? #'id1 #'id2)
                        #,(bound-identifier=? #'id1 #'id2))])))
              (a fred)) => (#t #f))
(test-eval! (define-syntax let
              (lambda (x)
                (define unique-ids?
                  (lambda (ls)
                    (or (null? ls)
                        (and (let notmem?
                               ([x (car ls)] [ls (cdr ls)])
                               (or (null? ls)
                                   (and (not (bound-identifier=?
                                              x (car ls)))
                                        (notmem? x (cdr ls)))))
                             (unique-ids? (cdr ls))))))
                (syntax-case x ()
                  [(_ ((i v) ...) e1 e2 ...)
                   (unique-ids? #'(i ...))
                   #'((lambda (i ...) e1 e2 ...) v ...)]))))
(test-syntax-violation (let ([a 3] [a 4]) (+ a a))) ; => &syntax exception)
(test-equal "sc 6"
            (let-syntax
                ([dolet (lambda (x)
                          (syntax-case x ()
                            [(_ b)
                             #'(let ([a 3] [b 4]) (+ a b))]))])
              (dolet a))
            => 7)
(test-eval! (define-syntax case
              (lambda (x)
                (syntax-case x ()
                  [(_ e0 [(k ...) e1 e2 ...] ...
                      [else-key else-e1 else-e2 ...])
                   (and (identifier? #'else-key)
                        (free-identifier=? #'else-key #'else))
                   #'(let ([t e0])
                       (cond
                        [(memv t '(k ...)) e1 e2 ...]
                        ...
                        [else else-e1 else-e2 ...]))]
                  [(_ e0 [(ka ...) e1a e2a ...]
                      [(kb ...) e1b e2b ...] ...)
                   #'(let ([t e0])
                       (cond
                        [(memv t '(ka ...)) e1a e2a ...]
                        [(memv t '(kb ...)) e1b e2b ...]
                        ...))]))))
(test-syntax-violation "sc exn 3" (let ([else #f]) (case 0 [else (write "oops")]))) ; => &syntax exception)
(test-end)

(test-begin "12.6 Syntax-object and datum conversions")
(test-eval! (define-syntax loop
              (lambda (x)
                (syntax-case x ()
                  [(k e ...)
                   (with-syntax
                       ([break (datum->syntax #'k 'break)])
                     #'(call-with-current-continuation
                        (lambda (break)
                          (let f () e ... (f)))))]))))
(test-equal "sc cnv 1"
            (let ((n 3) (ls '()))
              (loop
               (if (= n 0) (break ls))
               (set! ls (cons 'a ls))
               (set! n (- n 1))))
            => (a a a))
(test-eval! (define-syntax include
              (lambda (x)
                (define read-file
                  (lambda (fn k)
                    (let ([p (open-file-input-port fn (file-options) (buffer-mode block) (native-transcoder))])
                      (let f ([x (get-datum p)])
                        (if (eof-object? x)
                            (begin (close-port p) '())
                            (cons (datum->syntax k x)
                                  (f (get-datum p))))))))
                (syntax-case x ()
                  [(k filename)
                   (let ([fn (syntax->datum #'filename)])
                     (with-syntax ([(exp ...)
                                    (read-file fn #'k)])
                       #'(begin exp ...)))]))))
(test-equal "sc cnv 2"
            (let ()
              (include "./test/flib.ss") ;(define f (lambda (x) (g (* x x))))
              (include "./test/glib.ss") ;(define g (lambda (x) (+ x x)))
              (f 5)) => 50)
(test-eval! (define-syntax with-syntax
              (lambda (x)
                (syntax-case x ()
                  ((_ ((p e0) ...) e1 e2 ...)
                   (syntax (syntax-case (list e0 ...) ()
                             ((p ...) (let () e1 e2 ...)))))))))
(test-eval! (define-syntax cond
              (lambda (x)
                (syntax-case x ()
                  [(_ c1 c2 ...)
                   (let f ([c1 #'c1] [c2* #'(c2 ...)])
                     (syntax-case c2* ()
                       [()
                        (syntax-case c1 (else =>)
                          [(else e1 e2 ...) #'(begin e1 e2 ...)]
                          [(e0) #'e0]
                          [(e0 => e1)
                           #'(let ([t e0]) (if t (e1 t)))]
                          [(e0 e1 e2 ...)
                           #'(if e0 (begin e1 e2 ...))])]
                       [(c2 c3 ...)
                        (with-syntax ([rest (f #'c2 #'(c3 ...))])
                          (syntax-case c1 (=>)
                            [(e0) #'(let ([t e0]) (if t t rest))]
                            [(e0 => e1)
                             #'(let ([t e0]) (if t (e1 t) rest))]
                            [(e0 e1 e2 ...)
                             #'(if e0
                                   (begin e1 e2 ...)
                                   rest)]))]))]))))
(test-eval! (define-syntax case
              (lambda (x)
                (syntax-case x ()
                  [(_ e c1 c2 ...)
                   #`(let ([t e])
                       #,(let f ([c1 #'c1] [cmore #'(c2 ...)])
                           (if (null? cmore)
                               (syntax-case c1 (else)
                                 [(else e1 e2 ...)
                                  #'(begin e1 e2 ...)]
                                 [((k ...) e1 e2 ...)
                                  #'(if (memv t '(k ...))
                                        (begin e1 e2 ...))])
                               (syntax-case c1 ()
                                 [((k ...) e1 e2 ...)
                                  #`(if (memv t '(k ...))
                                        (begin e1 e2 ...)
                                        #,(f (car cmore)
                                             (cdr cmore)))]))))]))))
(test-eval! (define-syntax identifier-syntax
              (syntax-rules (set!)
                [(_ e)
                 (lambda (x)
                   (syntax-case x ()
                     [id (identifier? #'id) #'e]
                     [(_ x (... ...)) #'(e x (... ...))]))]
                [(_ (id exp1) ((set! var val) exp2))
                 (and (identifier? #'id) (identifier? #'var))
                 (make-variable-transformer
                  (lambda (x)
                    (syntax-case x (set!)
                      [(set! var val) #'exp2]
                      [(id x (... ...)) #'(exp1 x (... ...))]
                      [id (identifier? #'id) #'exp1])))])))
(test-equal "sc cnv 3"
            (let ((x 'u))
              (define id-value (cond ((case x ((u v) 'foo) ((x y) 'bad) (else 'bar)))))
              (define-syntax id (identifier-syntax (_ id-value) ((set! _ val) (set! id-value val))))
              (set! id (list id))
              id) => (foo))
(test-end)

(test-begin "13.2 Procedures")
(test-equal "ht 1"
            (call-with-values
                (lambda ()
                  (let ((h (make-eqv-hashtable)))
                    (hashtable-set! h 1 'one)
                    (hashtable-set! h 2 'two)
                    (hashtable-set! h 3 'three)
                    (hashtable-entries h)))
                list)
            => (#(2 3 1) #(two three one)))
(test-end)
