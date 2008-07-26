;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

#|

    produce followings from unicode data 5.0.0:

        case-folding.datum
        general-category-1.datum
        general-category-2.datum
        numeric-property.datum
        other-alphabetic.datum
        other-lowercase.datum
        other-uppercase.datum
        simple-lowercase.datum
        simple-titlecase.datum
        simple-uppercase.datum
        special-casing-lower.datum
        special-casing-title.datum
        special-casing-upper.datum

|#

(library (anonymous)

  (export)

  (import (core primitives)
          (core io)
          (core files)
          (core hashtables)
          (core sorting)
          (core destructuring)
          (pregexp))

  (define pregexp-substring
    (lambda (s match index)
      (let ((b (list-ref match index)))
        (substring s (car b) (cdr b)))))

  (define ucd-file
    (lambda (filename)
      (string-append (current-directory) "/unicode-5.0.0/" filename)))

  (define datum-file
    (lambda (filename)
      (string-append (current-directory) "/ucd/" filename)))

  (define skip-whitespace!
    (lambda (port)
      (and (not (port-eof? port))
           (nonblock-byte-ready? port)
           (char-whitespace? (lookahead-char port))
           (get-char port)
           (skip-whitespace! port))))

  (define read-ucd-line
    (lambda (port)
      (skip-whitespace! port)
      (let ((ch (lookahead-char port)))
        (cond ((eof-object? ch)
               (eof-object))
              ((eq? ch #\#)
               (get-line port)
               (read-ucd-line port))
              (else
               (get-line port))))))

  (define for-each-ucd-line
    (lambda (proc input)
      (let loop ()
        (let ((line (read-ucd-line input)))
          (cond ((eof-object? line)
                 (unspecified))
                (else
                 (proc line)
                 (loop)))))))

  #;(define shrink-range-list
    (lambda (lst)
      (let loop ((lst lst) (ans '()))
        (cond ((null? lst) (reverse ans))
              ((null? (cdr lst))
               (loop (cdr lst) (cons (car lst) ans)))
              (else
               (let ((e1 (car lst)) (e2 (cadr lst)))
                 (if (= (cdr e1) (car e2))
                     (loop (cons (cons (car e1) (cdr e2)) (cddr lst)) ans)
                     (loop (cdr lst) (cons e1 ans)))))))))

  (define shrink-range-list
    (lambda (lst)
      (if (null? lst)
          '()
          (let loop ((lst lst) (ans '()))
            (destructuring-match lst
              (((e1 . e2)) (reverse (cons (cons e1 e2) ans)))
              (((e1 . e2) (e3 . e4) more ...)
               (if (= e2 (- e3 1))
                   (loop (cons (cons e1 e4) more) ans)
                   (loop (cdr lst) (cons (cons e1 e2) ans)))))))))

  ; (shrink-range-list '((1 . 2)(3 . 4)(6 . 7)(8 . 15)(16 . 18)))
  ; (code-point) name (general-category) canonical-combining bidi decomposition numeric bidi-mirrored 
  ; unicode-1-name comment (simple-uppercase) (simple-lowercase) (simple-titlecase)

  #;(define add-special-range-area
    (lambda (ht)
      
      (define put-range
        (lambda (cn first last)
          (let loop ((cp first))
            (cond ((> cp last))
                  (else
                   (hashtable-set! ht cp cn)
                   (loop (+ cp 1)))))))
      
      ; 3400;<CJK Ideograph Extension A, First>;Lo;0;L;;;;;N;;;;;
      ; 4DB5;<CJK Ideograph Extension A, Last>;Lo;0;L;;;;;N;;;;;
      (put-range 'Lo #x3400 #x4DB5)
      
      ; 4E00;<CJK Ideograph, First>;Lo;0;L;;;;;N;;;;;
      ; 9FBB;<CJK Ideograph, Last>;Lo;0;L;;;;;N;;;;;
      (put-range 'Lo #x4E00 #x9FBB)

      ; AC00;<Hangul Syllable, First>;Lo;0;L;;;;;N;;;;;
      ; D7A3;<Hangul Syllable, Last>;Lo;0;L;;;;;N;;;;;
      (put-range 'Lo #xAC00 #xD7A3)

      ; D800;<Non Private Use High Surrogate, First>;Cs;0;L;;;;;N;;;;;
      ; DB7F;<Non Private Use High Surrogate, Last>;Cs;0;L;;;;;N;;;;;
      (put-range 'Cs #xD800 #xDB7F)

      ; DB80;<Private Use High Surrogate, First>;Cs;0;L;;;;;N;;;;;
      ; DBFF;<Private Use High Surrogate, Last>;Cs;0;L;;;;;N;;;;;
      (put-range 'Cs #xDB80 #xDBFF)

      ; DC00;<Low Surrogate, First>;Cs;0;L;;;;;N;;;;;
      ; DFFF;<Low Surrogate, Last>;Cs;0;L;;;;;N;;;;;
      (put-range 'Cs #xDC00 #xDFFF)

      ; E000;<Private Use, First>;Co;0;L;;;;;N;;;;;
      ; F8FF;<Private Use, Last>;Co;0;L;;;;;N;;;;;
      (put-range 'Co #xE000 #xF8FF)

      ; 20000;<CJK Ideograph Extension B, First>;Lo;0;L;;;;;N;;;;;
      ; 2A6D6;<CJK Ideograph Extension B, Last>;Lo;0;L;;;;;N;;;;;
      (put-range 'Lo #x20000 #x2A6D6)

      ; F0000;<Plane 15 Private Use, First>;Co;0;L;;;;;N;;;;;
      ; FFFFD;<Plane 15 Private Use, Last>;Co;0;L;;;;;N;;;;;
      (put-range 'Co #xF0000 #xFFFFD)

      ; 100000;<Plane 16 Private Use, First>;Co;0;L;;;;;N;;;;;
      ; 10FFFD;<Plane 16 Private Use, Last>;Co;0;L;;;;;N;;;;;
      (put-range 'Co #x100000 #x10FFFD)))
  
  (define parse-unicodedata
    (lambda ()
      ;(let ((re (pregexp "^([A-F0-9]{4,6});.*;([a-zA-Z]{2});.*;.*;.*;.*;.*;(.*);.*;.*;.*;([A-F0-9]{0,6});([A-F0-9]{0,6});([A-F0-9]{0,6})$")))
      (let ((re (pregexp "^([A-F0-9]{4,6});[^;]*;([a-zA-Z]{2});[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;(.*);[^;]*;[^;]*;[^;]*;([A-F0-9]{0,6});([A-F0-9]{0,6});([A-F0-9]{0,6})$")))

        (define ht-general-category-1 (make-eqv-hashtable)) ; code-point < 0x400
        (define ht-general-category-2 (make-eqv-hashtable)) ; 
        (define ht-numeric-property (make-eqv-hashtable))
        (define ht-simple-uppercase (make-eqv-hashtable))
        (define ht-simple-lowercase (make-eqv-hashtable))
        (define ht-simple-titlecase (make-eqv-hashtable))

        #;(add-special-range-area ht-general-category-2)
        
        (call-with-port
            (open-file-input-port (ucd-file "UnicodeData.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (format #t "~%parsing UnicodeData.txt ...~!");
              (for-each-ucd-line
               (lambda (line)
                 (let ((m (pregexp-match-positions re line)))
                   (let ((code-point       (string->number (pregexp-substring line m 1) 16))
                         (general-category (string->symbol (pregexp-substring line m 2)))
                         (numeric-property (pregexp-substring line m 3))
                         (simple-uppercase (pregexp-substring line m 4))
                         (simple-lowercase (pregexp-substring line m 5))
                         (simple-titlecase (pregexp-substring line m 6)))
                     (if (< code-point #x400)
                         (hashtable-set! ht-general-category-1 code-point general-category)
                         (hashtable-set! ht-general-category-2 code-point general-category))
                     (or (string=? numeric-property "")
                         (hashtable-set! ht-numeric-property code-point (string->number numeric-property)))
                     (or (string=? simple-uppercase "")
                         (hashtable-set! ht-simple-uppercase code-point (string->number simple-uppercase 16)))
                     (or (string=? simple-lowercase "")
                         (hashtable-set! ht-simple-lowercase code-point (string->number simple-lowercase 16)))
                     (or (string=? simple-titlecase "")
                         (hashtable-set! ht-simple-titlecase code-point (string->number simple-titlecase 16))))))
               input)
              (format #t " done~%~!")

              (call-with-port
                  (open-file-output-port (datum-file "general-category-1.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing general-category-1.datum (~a)...~!" (length (hashtable->alist ht-general-category-1)))
                    (put-datum output (hashtable->alist ht-general-category-1))
;              (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-general-category-1))))
;                (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
                    (format #t " done~%~!")))

              (call-with-port
                  (open-file-output-port (datum-file "general-category-2.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing general-category-2.datum (~a)...~!" (length (hashtable->alist ht-general-category-2)))
                    (put-datum output (hashtable->alist ht-general-category-2))
;              (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-general-category-2))))
;                (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
                    (format #t " done~%~!")))

              (call-with-port
                  (open-file-output-port (datum-file "numeric-property.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing numeric-property.datum ...~!")
                    (put-datum output (hashtable->alist ht-numeric-property))
;              (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-numeric-property))))
;                (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
                    (format #t " done~%~!")))

              (call-with-port
                  (open-file-output-port (datum-file "simple-uppercase.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing simple-uppercase.datum ...~!")
                    (put-datum output (hashtable->alist ht-simple-uppercase))
;              (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-simple-uppercase))))
;                (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
                    (format #t " done~%~!")))

              (call-with-port
                  (open-file-output-port (datum-file "simple-lowercase.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing simple-lowercase.datum ...~!")
                    (put-datum output (hashtable->alist ht-simple-lowercase))
;              (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-simple-lowercase))))
;                (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
                    (format #t " done~%~!")))

              (call-with-port
                  (open-file-output-port (datum-file "simple-titlecase.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing simple-titlecase.datum ...~!")
                    (put-datum output (hashtable->alist ht-simple-titlecase))
;              (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-simple-titlecase))))
;                (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
                    (format #t " done~%~!"))))))))

  (define parse-proplist
    (lambda ()
      (let ((re1 (pregexp "^([A-F0-9]{4,6}) *; *([a-zA-Z_]+) # .*$"))
            (re2 (pregexp "^([A-F0-9]{4,6})\\.\\.([A-F0-9]{4,6}) *; *([a-zA-Z_]+) # .*$")))

        (define ht-other-uppercase (make-eqv-hashtable))  ; Other_Uppercase
        (define ht-other-lowercase (make-eqv-hashtable))  ; Other_Lowercase
        (define ht-other-alphabetic (make-eqv-hashtable)) ; Other_Alphabetic

        (call-with-port
            (open-file-input-port (ucd-file "PropList.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (format #t "~%parsing PropList.txt ...~!");
              (for-each-ucd-line
               (lambda (line)
                 (let ((m1 (pregexp-match-positions re1 line)) (m2 (pregexp-match-positions re2 line)))
                   (cond (m1 (let ((code-point (string->number (pregexp-substring line m1 1) 16))
                                   (property (pregexp-substring line m1 2)))
                               (and (string=? property "Other_Uppercase")
                                    (hashtable-set! ht-other-uppercase code-point code-point))
                               (and (string=? property "Other_Lowercase")
                                    (hashtable-set! ht-other-lowercase code-point code-point))
                               (and (string=? property "Other_Alphabetic")
                                    (hashtable-set! ht-other-alphabetic code-point code-point))))
                         (m2 (let ((code-point-from (string->number (pregexp-substring line m2 1) 16))
                                   (code-point-to (string->number (pregexp-substring line m2 2) 16))
                                   (property (pregexp-substring line m2 3)))
                               (and (string=? property "Other_Uppercase")
                                    (hashtable-set! ht-other-uppercase code-point-from code-point-to))
                               (and (string=? property "Other_Lowercase")
                                    (hashtable-set! ht-other-lowercase code-point-from code-point-to))
                               (and (string=? property "Other_Alphabetic")
                                    (hashtable-set! ht-other-alphabetic code-point-from code-point-to))))
                         (else
                          (assertion-violation 'parse-unicode-data "failed to parse unicode data")))))
               input)
              (format #t " done~%~!")

              (call-with-port
                  (open-file-output-port (datum-file "other-uppercase.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing other-uppercase.datum ...~!")
                    (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-other-uppercase))))
                      (put-char output #\() (put-char output #\linefeed)
                      (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) (shrink-range-list lst))
                      (put-char output #\)) (put-char output #\linefeed))
                    (format #t " done~%~!")))

              (call-with-port
                  (open-file-output-port (datum-file "other-lowercase.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing other-lowercase.datum ...~!")
                    (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-other-lowercase))))
                      (put-char output #\() (put-char output #\linefeed)
                      (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) (shrink-range-list lst))
                      (put-char output #\)) (put-char output #\linefeed))
                    (format #t " done~%~!")))

              (call-with-port
                  (open-file-output-port (datum-file "other-alphabetic.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (format #t "processing other-alphabetic.datum ...~!")
                    (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-other-alphabetic))))
                      (put-char output #\() (put-char output #\linefeed)
                      (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) (shrink-range-list lst))
                      (put-char output #\)) (put-char output #\linefeed))
                    (format #t " done~%~!"))))))))

  (define parse-specialcasing ; ignore conditional mappings
    (lambda ()

      (define ht-special-lower (make-eqv-hashtable))
      (define ht-special-title (make-eqv-hashtable))
      (define ht-special-upper (make-eqv-hashtable))

      (format #t "~%parsing SpecialCasing.txt ...~!");

      (let ((re1 (pregexp "^([A-F0-9]{4,6}); ([A-F0-9]{4,6});[^;]*;[^;]*; # .*$"))
            (re2 (pregexp "^([A-F0-9]{4,6}); ([A-F0-9]{4,6}) ([A-F0-9]{4,6});[^;]*;[^;]*; # .*$"))
            (re3 (pregexp "^([A-F0-9]{4,6}); ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6});[^;]*;[^;]*; # .*$")))

;      (let ((re1 (pregexp "^([A-F0-9]{4,6}); ([A-F0-9]{4,6});.*;.*; # .*$"))
;            (re2 (pregexp "^([A-F0-9]{4,6}); ([A-F0-9]{4,6}) ([A-F0-9]{4,6});.*;.*; # .*$"))
;            (re3 (pregexp "^([A-F0-9]{4,6}); ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6});.*;.*; # .*$")))
        (call-with-port
            (open-file-input-port (ucd-file "SpecialCasing.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (for-each-ucd-line
               (lambda (line)
                 (let ((m1 (pregexp-match-positions re1 line))
                       (m2 (pregexp-match-positions re2 line))
                       (m3 (pregexp-match-positions re3 line)))
                   (and m1
                        (let ((code-point (string->number (pregexp-substring line m1 1) 16))
                              (c1 (string->number (pregexp-substring line m1 2) 16)))
                          (hashtable-set! ht-special-lower code-point (list c1))))
                   (and m2
                        (let ((code-point (string->number (pregexp-substring line m2 1) 16))
                              (c1 (string->number (pregexp-substring line m2 2) 16))
                              (c2 (string->number (pregexp-substring line m2 3) 16)))
                          (hashtable-set! ht-special-lower code-point (list c1 c2))))
                   (and m3
                        (let ((code-point (string->number (pregexp-substring line m3 1) 16))
                              (c1 (string->number (pregexp-substring line m3 2) 16))
                              (c2 (string->number (pregexp-substring line m3 2) 16))
                              (c3 (string->number (pregexp-substring line m3 2) 16)))
                          (hashtable-set! ht-special-lower code-point (list c1 c2 c3))))))
               input))))

      (let ((re1 (pregexp "^([A-F0-9]{4,6});[^;]*; ([A-F0-9]{4,6});[^;]*; # .*$"))
            (re2 (pregexp "^([A-F0-9]{4,6});[^;]*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6});[^;]*; # .*$"))
            (re3 (pregexp "^([A-F0-9]{4,6});[^;]*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6});[^;]*; # .*$")))
;      (let ((re1 (pregexp "^([A-F0-9]{4,6}); .*; ([A-F0-9]{4,6}); .*; # .*$"))
;            (re2 (pregexp "^([A-F0-9]{4,6}); .*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); .*; # .*$"))
;            (re3 (pregexp "^([A-F0-9]{4,6}); .*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); .*; # .*$")))
        (call-with-port
            (open-file-input-port (ucd-file "SpecialCasing.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (for-each-ucd-line
               (lambda (line)
                 (let ((m1 (pregexp-match-positions re1 line))
                       (m2 (pregexp-match-positions re2 line))
                       (m3 (pregexp-match-positions re3 line)))
                   (and m1
                        (let ((code-point (string->number (pregexp-substring line m1 1) 16))
                              (c1 (string->number (pregexp-substring line m1 2) 16)))
                          (hashtable-set! ht-special-title code-point (list c1))))
                   (and m2
                        (let ((code-point (string->number (pregexp-substring line m2 1) 16))
                              (c1 (string->number (pregexp-substring line m2 2) 16))
                              (c2 (string->number (pregexp-substring line m2 3) 16)))
                          (hashtable-set! ht-special-title code-point (list c1 c2))))
                   (and m3
                        (let ((code-point (string->number (pregexp-substring line m3 1) 16))
                              (c1 (string->number (pregexp-substring line m3 2) 16))
                              (c2 (string->number (pregexp-substring line m3 2) 16))
                              (c3 (string->number (pregexp-substring line m3 2) 16)))
                          (hashtable-set! ht-special-title code-point (list c1 c2 c3))))))
               input))))

      (let ((re1 (pregexp "^([A-F0-9]{4,6});[^;]*;[^;]*; ([A-F0-9]{4,6}); # .*$"))
            (re2 (pregexp "^([A-F0-9]{4,6});[^;]*;[^;]*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$"))
            (re3 (pregexp "^([A-F0-9]{4,6});[^;]*;[^;]*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$")))
;       (let ((re1 (pregexp "^([A-F0-9]{4,6}); .*; .*; ([A-F0-9]{4,6}); # .*$"))
;            (re2 (pregexp "^([A-F0-9]{4,6}); .*; .*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$"))
;            (re3 (pregexp "^([A-F0-9]{4,6}); .*; .*; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$")))
        (call-with-port
            (open-file-input-port (ucd-file "SpecialCasing.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (for-each-ucd-line
               (lambda (line)
                 (let ((m1 (pregexp-match-positions re1 line))
                       (m2 (pregexp-match-positions re2 line))
                       (m3 (pregexp-match-positions re3 line)))
                   (and m1
                        (let ((code-point (string->number (pregexp-substring line m1 1) 16))
                              (c1 (string->number (pregexp-substring line m1 2) 16)))
                          (hashtable-set! ht-special-upper code-point (list c1))))
                   (and m2
                        (let ((code-point (string->number (pregexp-substring line m2 1) 16))
                              (c1 (string->number (pregexp-substring line m2 2) 16))
                              (c2 (string->number (pregexp-substring line m2 3) 16)))
                          (hashtable-set! ht-special-upper code-point (list c1 c2))))
                   (and m3
                        (let ((code-point (string->number (pregexp-substring line m3 1) 16))
                              (c1 (string->number (pregexp-substring line m3 2) 16))
                              (c2 (string->number (pregexp-substring line m3 2) 16))
                              (c3 (string->number (pregexp-substring line m3 2) 16)))
                          (hashtable-set! ht-special-upper code-point (list c1 c2 c3))))))
               input))))

      (format #t " done~%~!")

      (call-with-port
          (open-file-output-port (datum-file "special-casing-lower.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
          (lambda (output)
            (format #t "special-casing-lower.datum ...~!")
            (put-datum output (hashtable->alist ht-special-lower))
;         (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-special-lower))))
;           (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
            (format #t " done~%~!")))

      (call-with-port
          (open-file-output-port (datum-file "special-casing-title.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
          (lambda (output)
            (format #t "special-casing-title.datum ...~!")
            (put-datum output (hashtable->alist ht-special-title))
;         (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-special-title))))
;           (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
            (format #t " done~%~!")))

      (call-with-port
          (open-file-output-port (datum-file "special-casing-upper.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
          (lambda (output)
            (format #t "special-casing-upper.datum ...~!")
            (put-datum output (hashtable->alist ht-special-upper))
;         (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-special-upper))))
;           (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
            (format #t " done~%~!")))))

  (define parse-casefolding
    (lambda ()

      (define ht-casefolding (make-eqv-hashtable))

      (format #t "~%parsing CaseFolding.txt ...~!");

      (let ((re1 (pregexp "^([A-F0-9]{4,6}); C; ([A-F0-9]{4,6}); # .*$"))
            (re2 (pregexp "^([A-F0-9]{4,6}); C; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$"))
            (re3 (pregexp "^([A-F0-9]{4,6}); C; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$")))

        (call-with-port
            (open-file-input-port (ucd-file "CaseFolding.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (for-each-ucd-line
               (lambda (line)
                 (let ((m1 (pregexp-match-positions re1 line))
                       (m2 (pregexp-match-positions re2 line))
                       (m3 (pregexp-match-positions re3 line)))
                   (and m1
                        (let ((code-point (string->number (pregexp-substring line m1 1) 16))
                              (c1 (string->number (pregexp-substring line m1 2) 16)))
                          (hashtable-set! ht-casefolding code-point (list c1))))
                   (and m2
                        (let ((code-point (string->number (pregexp-substring line m2 1) 16))
                              (c1 (string->number (pregexp-substring line m2 2) 16))
                              (c2 (string->number (pregexp-substring line m2 3) 16)))
                          (hashtable-set! ht-casefolding code-point (list c1 c2))))
                   (and m3
                        (let ((code-point (string->number (pregexp-substring line m3 1) 16))
                              (c1 (string->number (pregexp-substring line m3 2) 16))
                              (c2 (string->number (pregexp-substring line m3 2) 16))
                              (c3 (string->number (pregexp-substring line m3 2) 16)))
                          (hashtable-set! ht-casefolding code-point (list c1 c2 c3))))))
               input))))

      (let ((re1 (pregexp "^([A-F0-9]{4,6}); F; ([A-F0-9]{4,6}); # .*$"))
            (re2 (pregexp "^([A-F0-9]{4,6}); F; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$"))
            (re3 (pregexp "^([A-F0-9]{4,6}); F; ([A-F0-9]{4,6}) ([A-F0-9]{4,6}) ([A-F0-9]{4,6}); # .*$")))

        (call-with-port
            (open-file-input-port (ucd-file "CaseFolding.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (for-each-ucd-line
               (lambda (line)
                 (let ((m1 (pregexp-match-positions re1 line))
                       (m2 (pregexp-match-positions re2 line))
                       (m3 (pregexp-match-positions re3 line)))
                   (and m1
                        (let ((code-point (string->number (pregexp-substring line m1 1) 16))
                              (c1 (string->number (pregexp-substring line m1 2) 16)))
                          (hashtable-set! ht-casefolding code-point (list c1))))
                   (and m2
                        (let ((code-point (string->number (pregexp-substring line m2 1) 16))
                              (c1 (string->number (pregexp-substring line m2 2) 16))
                              (c2 (string->number (pregexp-substring line m2 3) 16)))
                          (hashtable-set! ht-casefolding code-point (list c1 c2))))
                   (and m3
                        (let ((code-point (string->number (pregexp-substring line m3 1) 16))
                              (c1 (string->number (pregexp-substring line m3 2) 16))
                              (c2 (string->number (pregexp-substring line m3 2) 16))
                              (c3 (string->number (pregexp-substring line m3 2) 16)))
                          (hashtable-set! ht-casefolding code-point (list c1 c2 c3))))))
               input))))
      (format #t " done~%~!")
      (call-with-port
          (open-file-output-port (datum-file "case-folding.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
          (lambda (output)
            (format #t "case-folding.datum ...~!")
            (put-datum output (hashtable->alist ht-casefolding))
;         (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-casefolding))))
;           (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
            ))))


  (parse-unicodedata)
  (parse-proplist)
  (parse-specialcasing)
  (parse-casefolding)
  (format #t " done~%~!")

  ) ;[end]

