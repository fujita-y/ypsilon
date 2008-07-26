;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

#|

    produce followings from unicode data 5.0.0:

        lexeme.inc

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

  (define add-special-range-area
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
      (let ((re (pregexp "^([A-F0-9]{4,6});[^;]*;([a-zA-Z]{2});[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;(.*);[^;]*;[^;]*;[^;]*;([A-F0-9]{0,6});([A-F0-9]{0,6});([A-F0-9]{0,6})$")))

        (define ht-general-category (make-eqv-hashtable))

        (add-special-range-area ht-general-category)
        (call-with-port
            (open-file-input-port (ucd-file "UnicodeData.txt") (file-options) (buffer-mode block) (native-transcoder))
            (lambda (input)
              (format #t "~%parsing UnicodeData.txt ...~!");
              (for-each-ucd-line
               (lambda (line)
                 (let ((m (pregexp-match-positions re line)))
                   (let ((code-point       (string->number (pregexp-substring line m 1) 16))
                         (general-category (string->symbol (pregexp-substring line m 2))))
                     (hashtable-set! ht-general-category code-point general-category))))
               input)   
              (format #t " done~%~!")

              (call-with-port
                  (open-file-output-port "lexeme.inc" (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)

                    (define bv-c (make-bytevector (div #x110000 8)))
                    (define bv-s (make-bytevector (div #x110000 8)))

                    (define advance
                      (lambda (cp offset bit)
                        (let ((bit (+ bit bit)))
                          (if (= bit #b100000000)
                              (list (+ cp 1) (+ offset 1) 1)
                              (list (+ cp 1) offset bit)))))

                    (format #t "processing lexeme.inc...~!")

                    (let loop ((cp 0) (offset 0) (bit 1))
                      (cond ((<= #xd800 cp #xdfff) (apply loop (advance cp offset bit)))
                            ((> cp #x10ffff))
                            ((hashtable-ref ht-general-category cp 'Cn)
                             => (lambda (cc)
                                  (cond ((memq cc '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))
                                         (bytevector-u8-set! bv-c offset (+ (bytevector-u8-ref bv-c offset) bit))
                                         (bytevector-u8-set! bv-s offset (+ (bytevector-u8-ref bv-s offset) bit)))
                                        ((memq cc '(Nd Mc Me))
                                         (bytevector-u8-set! bv-s offset (+ (bytevector-u8-ref bv-s offset) bit))))
                                  (apply loop (advance cp offset bit))))))

                    (let ((bv bv-c))
                      (let ((bytes (bytevector-length bv)))
                        (put-string output (format "static const uint8_t s_constituent[~a] = {" bytes))
                        (let loop ((c 0))
                          (if (zero? (mod c 16)) (put-byte output (char->integer #\linefeed)))
                          (let ((b (bytevector-u8-ref bv c)))
                            (cond ((= (+ c 1) bytes)
                                   (put-string output (format "0x~x\n};\n" b)))
                                  (else
                                   (put-string output (format "0x~x," b))
                                   (loop (+ c 1))))))))
                    (let ((bv bv-s))
                      (let ((bytes (bytevector-length bv)))
                        (put-string output (format "static const uint8_t s_subsequent[~a] = {" bytes))
                        (let loop ((c 0))
                          (if (zero? (mod c 16)) (put-byte output (char->integer #\linefeed)))
                          (let ((b (bytevector-u8-ref bv c)))
                            (cond ((= (+ c 1) bytes)
                                   (put-string output (format "0x~x\n};\n" b)))
                                  (else
                                   (put-string output (format "0x~x," b))
                                   (loop (+ c 1))))))))
                    (format #t " done~%~!"))))))))


  (parse-unicodedata)

  ) ;[end]

