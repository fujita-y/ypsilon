#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core unicode)

  (export char-upcase
          char-downcase
          char-titlecase
          char-foldcase
          char-ci=?
          char-ci<?
          char-ci>?
          char-ci<=?
          char-ci>=?
          char-alphabetic?
          char-numeric?
          char-whitespace?
          char-upper-case?
          char-lower-case?
          char-title-case?
          char-general-category
          string-upcase
          string-downcase
          string-foldcase
          string-titlecase
          string-ci=?
          string-ci<?
          string-ci>?
          string-ci<=?
          string-ci>=?
          string-normalize-nfd
          string-normalize-nfkd
          string-normalize-nfc
          string-normalize-nfkc)

  (import (core primitives)
          (core lists)
          (core io)
          (core unicode-assistants)
          (core bytevectors)
          (core bytevector-transcoders))

  (define char-upcase
    (lambda (ch)
      (cond ((char<? ch #\a) ch)
            ((char>? ch #\z)
             (simple-uppercase ch))
            (else
             (integer->char (- (char->integer ch) #x20))))))

  (define char-downcase
    (lambda (ch)
      (cond ((char<? ch #\A) ch)
            ((char<=? ch #\Z)
             (integer->char (+ (char->integer ch) #x20)))
            ((char>? ch #\z)
             (simple-lowercase ch))
            (else ch))))

  (define char-titlecase
    (lambda (ch)
      (simple-titlecase ch)))

  (define char-foldcase
    (lambda (ch)
      (cond ((char<=? ch #\z)
             (char-downcase ch))
            ((or (char=? ch #\x130)
                 (char=? ch #\x131))
             ch)
            (else
             (char-downcase (char-upcase ch))))))

  (define char-ci=?
    (lambda (ch1 ch2)
      (char=? (char-foldcase ch1)
              (char-foldcase ch2))))

  (define char-ci<?
    (lambda (ch1 ch2)
      (char<? (char-foldcase ch1)
              (char-foldcase ch2))))

  (define char-ci>?
    (lambda (ch1 ch2)
      (char>? (char-foldcase ch1)
              (char-foldcase ch2))))

  (define char-ci<=?
    (lambda (ch1 ch2)
      (char<=? (char-foldcase ch1)
               (char-foldcase ch2))))

  (define char-ci>=?
    (lambda (ch1 ch2)
      (char>=? (char-foldcase ch1)
               (char-foldcase ch2))))

  (define char-alphabetic?
    (lambda (ch)
      (or (and (char<=? #\a ch) (char<=? ch #\z))
          (and (char<=? #\A ch) (char<=? ch #\Z))
          (and (char<=? #\x80 ch)
               (case (general-category ch)
                 ((Lu Ll Lt Lm Lo Nl) #t)
                 ((Mn Mc So)
                  (other-alphabetic-property? ch))
                 (else #f))))))

  (define char-numeric?
    (lambda (ch)
      (or (and (char<=? #\0 ch) (char<=? ch #\9))
          (and (char<=? #\x80 ch)
               (eq? (general-category ch) 'Nd)))))

  (define char-upper-case?
    (lambda (ch)
      (or (and (char<=? #\A ch) (char<=? ch #\Z))
          (and (char<=? #\x80 ch)
               (case (general-category ch)
                 ((Lu) #t)
                 ((Nl So)
                  (other-uppercase-property? ch))
                 (else #f))))))

  (define char-lower-case?
    (lambda (ch)
      (or (and (char<=? #\a ch) (char<=? ch #\z))
          (and (char<=? #\x80 ch)
               (case (general-category ch)
                 ((Ll) #t)
                 ((Lm Mn Nl So)
                  (other-lowercase-property? ch))
                 (else #f))))))

  (define char-title-case?
    (lambda (ch)
      (eq? (general-category ch) 'Lt)))

  (define char-general-category
    (lambda (ch)
      (general-category ch)))

  (define string-upcase
    (lambda (s)
      (let ((input (open-string-input-port s))
            (output (make-string-output-port)))
        (let ((new
               (let loop ((ch (get-char input)))
                 (cond ((eof-object? ch) (extract-accumulated-string output))
                       ((special-casing-upper ch)
                        => (lambda (lst)
                             (for-each (lambda (e) (put-char output (char-upcase (integer->char e)))) lst)
                             (loop (get-char input))))
                       (else
                        (put-char output (char-upcase ch))
                        (loop (get-char input)))))))
          (if (string=? s new) s new)))))

  (define final-sigma?
    (lambda (input-port output-port)
      (let ((ch (lookahead-char input-port)))
        (cond ((eof-object? ch)
               (not (= (port-position output-port) 0)))
              ((char-alphabetic? ch) #f)
              ((char-whitespace? ch) #t)
              ((eq? (general-category ch) 'Pd) #t)
              (else
               (let ((pos (port-position input-port)))
                 (get-char input-port)
                 (let ((final?
                        (let loop ((ch (get-char input-port)))
                          (cond ((eof-object? ch)
                                 (not (= (port-position output-port) 0)))
                                ((char-alphabetic? ch) #f)
                                ((char-whitespace? ch) #t)
                                ((eq? (general-category ch) 'Pd) #t)
                                (else
                                 (loop (get-char input-port)))))))
                   (set-port-position! input-port pos)
                   final?)))))))

  (define string-downcase
    (lambda (s)
      (let ((input (open-string-input-port s))
            (output (make-string-output-port)))
        (let ((new
               (let loop ((ch (get-char input)) (last-ch #\space))
                 (cond ((eof-object? ch) (extract-accumulated-string output))
                       ((char=? ch #\x03A3)  ; GREEK CAPITAL LETTER SIGMA
                        (cond ((char-whitespace? last-ch)
                               (put-char output #\x03C3)
                               (loop (get-char input) ch))
                              (else
                               (if (final-sigma? input output)
                                   (put-char output #\x03C2)  ; GREEK SMALL LETTER FINAL SIGMA
                                   (put-char output #\x03C3)) ; GREEK SMALL LETTER SIGMA
                               (loop (get-char input) ch))))
                       ((special-casing-lower ch)
                        => (lambda (lst)
                             (for-each (lambda (e) (put-char output (char-downcase (integer->char e)))) lst)
                             (loop (get-char input) ch)))
                       (else
                        (put-char output (char-downcase ch))
                        (loop (get-char input) ch))))))
          (if (string=? s new) s new)))))

  (define string-foldcase
    (lambda (s)
      (let ((input (open-string-input-port s))
            (output (make-string-output-port)))
        (let ((new
               (let loop ((ch (get-char input)))
                 (cond ((eof-object? ch) (extract-accumulated-string output))
                       ((foldcase ch)
                        => (lambda (lst)
                             (for-each (lambda (e) (put-char output (integer->char e))) lst)
                             (loop (get-char input))))
                       (else
                        (put-char output ch)
                        (loop (get-char input)))))))
          (if (string=? s new) s new)))))

  (define string-titlecase
    (lambda (s)
      (let ((input (open-string-input-port s))
            (output (make-string-output-port)))
        (letrec ((titlecase-first-char
                  (lambda ()
                    (let loop ((ch (get-char input)))
                      (cond ((eof-object? ch)
                             (extract-accumulated-string output))
                            (else
                             (case (general-category ch)
                               ((Ll Lu Lt)
                                (put-char output (char-titlecase ch))
                                (downcase-subsequence))
                               (else
                                (put-char output ch)
                                (loop (get-char input)))))))))
                 (downcase-subsequence
                  (lambda ()
                    (let loop ((ch (get-char input)))
                      (cond ((eof-object? ch)
                             (extract-accumulated-string output))
                            (else
                             (case (general-category ch)
                               ((Ll Lu Lt)
                                (put-char output (char-downcase ch))
                                (loop (get-char input)))
                               ((Po Pf)
                                (case ch
                                  ((#\x0027          ; MidLetter # Po       APOSTROPHE
                                    #\x003A          ; MidLetter # Po       COLON
                                    #\x00B7          ; MidLetter # Po       MIDDLE DOT
                                    #\x05F4          ; MidLetter # Po       HEBREW PUNCTUATION GERSHAYIM
                                    #\x2019          ; MidLetter # Pf       RIGHT SINGLE QUOTATION MARK
                                    #\x2027          ; MidLetter # Po       HYPHENATION POINT
                                    )
                                   (put-char output ch)
                                   (loop (get-char input)))
                                  (else
                                   (put-char output ch)
                                   (titlecase-first-char))))
                               (else
                                (put-char output ch)
                                (titlecase-first-char)))))))))
          (let ((new (titlecase-first-char)))
            (if (string=? s new) s new))))))

  (define string-ci=?
    (lambda strings
      (apply string=? (map string-foldcase strings))))

  (define string-ci<?
    (lambda strings
      (apply string<? (map string-foldcase strings))))

  (define string-ci>?
    (lambda strings
      (apply string>? (map string-foldcase strings))))

  (define string-ci<=?
    (lambda strings
      (apply string<=? (map string-foldcase strings))))

  (define string-ci>=?
    (lambda strings
      (apply string>=? (map string-foldcase strings))))

  (define string-normalize-nfd
    (lambda (s)
      (utf32->string
       (sort-combining-marks! (decompose (open-string-input-port s) #t))
       (native-endianness) #t)))

  (define string-normalize-nfkd
    (lambda (s)
      (utf32->string
       (sort-combining-marks! (decompose (open-string-input-port s) #f))
       (native-endianness) #t)))

  (define string-normalize-nfc
    (lambda (s)
      (compose
       (sort-combining-marks!
        (decompose (open-string-input-port s) #t)))))

  (define string-normalize-nfkc
    (lambda (s)
      (compose
       (sort-combining-marks!
        (decompose (open-string-input-port s) #f)))))
  
  ) ;[end]
