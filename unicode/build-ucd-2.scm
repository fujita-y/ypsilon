;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

#|

    produce followings from unicode data 5.0.0:

    "canonical-class.datum"
    "decompose.datum"
    "compose.datum"
    "compatibility.datum"

|#

(library (anonymous)

  (export)

  (import (core primitives)
          (core io)
          (core files)
          (core hashtables)
          (core sorting)
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

  ; (code-point) name (general-category) canonical-combining bidi decomposition numeric bidi-mirrored 
  ; unicode-1-name comment (simple-uppercase) (simple-lowercase) (simple-titlecase)

  (define ht-composit-exclusion (make-eqv-hashtable))
  (define ht-canonical-class (make-eqv-hashtable))
  (define ht-decompose (make-eqv-hashtable))
  (define ht-compose (make-eqv-hashtable))
  (define ht-compatibility (make-eqv-hashtable))

  (define parse-composition-exclusions
    (lambda ()
      (let ((re1 (pregexp "^([A-F0-9]{4,6}) *# .*$"))
            (re2 (pregexp "^([A-F0-9]{4,6})\\.\\.([A-F0-9]{4,6}) *# .*$")))
        (call-with-port
            (open-file-input-port (ucd-file "CompositionExclusions.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (format #t "~%parsing CompositionExclusions.txt ...~!");
              (for-each-ucd-line
               (lambda (line)
                 (let ((m1 (pregexp-match-positions re1 line)) (m2 (pregexp-match-positions re2 line)))
                   (cond (m1 (let ((code-point (string->number (pregexp-substring line m1 1) 16)))
                               (hashtable-set! ht-composit-exclusion code-point #t)))
                         (m2 (let ((code-point-from (string->number (pregexp-substring line m2 1) 16))
                                   (code-point-to (string->number (pregexp-substring line m2 2) 16)))
                               (let loop ((i code-point-from))
                                 (cond ((<= i code-point-to)
                                        (hashtable-set! ht-composit-exclusion i #t)
                                        (loop (+ i 1)))))))
                         (else
                          (assertion-violation 'parse-unicode-data "failed to parse compsition exclusions data")))))
               input)
              (format #t " done~%~!"))))))

  (define parse-hex-re1 (pregexp "^<[a-zA-Z]*> (.*)$"))
  (define parse-hex-re2 (pregexp "^([ A-F0-9]*)$"))
  (define parse-hex-inner-re1 (pregexp "^([A-F0-9]+)$"))
  (define parse-hex-inner-re2 (pregexp "^([A-F0-9]+) (.*)$"))

  (define parse-hex
    (lambda (s)
      (let ((m1 (pregexp-match-positions parse-hex-re1 s))
            (m2 (pregexp-match-positions parse-hex-re2 s)))
        (let ((hexnums (cond (m1 (pregexp-substring s m1 1))
                             (m2 (pregexp-substring s m2 1))
                             (else (assertion-violation 'parse-unicode-data "bad decomp in unicode data" s)))))
          (let loop ((s hexnums) (lst '()))
            (let ((m1 (pregexp-match-positions parse-hex-inner-re1 s))
                  (m2 (pregexp-match-positions parse-hex-inner-re2 s)))
              (cond (m1
                     (reverse (cons (string->number (pregexp-substring s m1 1) 16) lst)))
                    (m2
                     (loop (pregexp-substring s m2 2)
                           (cons (string->number (pregexp-substring s m2 1) 16) lst)))
                    (else
                     (assertion-violation 'parse-unicode-data "bad decomp in unicode data" s)))))))))
  
  (define parse-unicodedata
    (lambda ()
      (let ((re (pregexp "^([A-F0-9]{4,6});[^;]*;[a-zA-Z]{2};([0-9]{0,3});[^;]*;([^;]*);.*$")))
        (call-with-port
            (open-file-input-port (ucd-file "UnicodeData.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (format #t "~%parsing UnicodeData.txt ...~!");
              (for-each-ucd-line
               (lambda (line)
                 (let ((m (pregexp-match-positions re line)))
                   (cond (m (let ((code-point (string->number (pregexp-substring line m 1) 16))
                                  (canonical-class (string->number (pregexp-substring line m 2) 10))
                                  (decomp (pregexp-substring line m 3)))
                              (or (<= 0 canonical-class 255)
                                  (assertion-violation 'parse-unicode-data "bad canonical class in unicode data" canonical-class))
                              (and (not (= 0 canonical-class )) (hashtable-set! ht-canonical-class code-point canonical-class))
                              (and (> (string-length decomp) 0)
                                   (let ((compat (char=? (string-ref decomp 0) #\<)))
                                     (and compat (hashtable-set! ht-compatibility code-point #t))
                                     (let ((decomp-lst (parse-hex decomp)))
                                       (or compat
                                           (<= 1 (length decomp-lst) 2)
                                           (assertion-violation 'parse-unicode-data "bad canonical decomp in unicode data" decomp))
                                       (hashtable-set! ht-decompose code-point decomp-lst)
                                       ;(format #t "~s ~s ~s~%~!" code-point canonical-class decomp-lst)
                                       (and (not compat)
                                            (not (hashtable-ref ht-composit-exclusion code-point #f))
                                            (hashtable-set! ht-compose
                                                            (if (= (length decomp-lst) 2)
                                                                (+ (* (car decomp-lst) #x10000) (cadr decomp-lst))
                                                                (car decomp-lst))
                                                            code-point)))))))
                         (else
                          (assertion-violation 'parse-unicode-data "failed to parse unicode data")))))
               input)
              #;(let ((SBase #xAC00) (LBase #x1100) (VBase #x1161) (TBase #x11A7) (LCount 19) (VCount 21) (TCount 28))
             (format #t "add hangul decompositions ...~%~!")
             (let* ((NCount (* VCount TCount))
                    (SCount (* LCount NCount)))
               (let loop ((SIndex 0))
                 (cond ((< SIndex SCount)
                        (let ((TIndex (mod SIndex TCount)))
                          (let ((value (+ SIndex SBase)))
                            (let-values (((first second)
                                          (cond ((zero? TIndex)
                                                 (values (+ LBase (div SIndex NCount))
                                                         (+ VBase (div (mod SIndex NCount) TCount))))
                                                (else
                                                 (values (- (+ SBase SIndex) TIndex)
                                                         (+ TBase TIndex))))))
                              (let ((pair (+ (* first #x10000) second)))
                                ;(format #t "~s ~s ~s~%~!" value pair (list first second))

                                (hashtable-set! ht-decompose value (list first second))
                                (hashtable-set! ht-compose pair value)))))
                        (loop (+ SIndex 1)))))))
              (format #t " done~%~!"))))))

  (parse-composition-exclusions)
  (parse-unicodedata)

  ;(define ht-canonical-class (make-eqv-hashtable))
  ;(define ht-decompose (make-eqv-hashtable))
  ;(define ht-compose (make-eqv-hashtable))
  ;(define ht-compatibility (make-eqv-hashtable))

  (call-with-port
      (open-file-output-port (datum-file "canonical-class.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
      (lambda (output)
        (format #t "processing canonical-class.datum ...~!")
        (put-datum output (hashtable->alist ht-canonical-class))
;     (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-canonical-class))))
;       (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
        (format #t " done~%~!")))

  (call-with-port
      (open-file-output-port (datum-file "decompose.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
      (lambda (output)
        (format #t "processing decompose.datum ...~!")
        (put-datum output (hashtable->alist ht-decompose))
;     (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-decompose))))
;       (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
        (format #t " done~%~!")))

  (call-with-port
      (open-file-output-port (datum-file "compose.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
      (lambda (output)
        (format #t "processing compose.datum ...~!")
        (put-datum output (hashtable->alist ht-compose))
;     (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-compose))))
;       (for-each (lambda (e) (put-datum output e) (put-char output #\linefeed)) lst))
        (format #t " done~%~!")))

  (call-with-port
      (open-file-output-port (datum-file "compatibility.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
      (lambda (output)
        (format #t "processing compatibility.datum ...~!")
        (put-datum output (hashtable->alist ht-compatibility))
;     (let ((lst (list-sort (lambda (e1 e2) (< (car e1) (car e2))) (hashtable->alist ht-compatibility))))
;       (for-each (lambda (e) (put-datum output (list (car e))) (put-char output #\linefeed)) lst))
        (format #t " done~%~!")))

  ) ;[end]
