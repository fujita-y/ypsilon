;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(import (core primitives))

(add-library-path (format "~a/../sitelib" (current-directory)))

(import (core destructuring)
        (core optimize)
        (core parameters)
        (core io)
        (core files)
        (pregexp))

(assert (string-contains (current-directory) (home-directory)))

(define convert
  (lambda (name)
    (let ((source-file-name (format "ucd/~a.datum" name))
          (target-file-name (format "~a.inc" name)))
      (format #t "~%;; converting ~a\n;; --> ~a/~a~!" source-file-name (current-directory) target-file-name)

      (call-with-port
          (call-with-port
              (open-file-input-port source-file-name (file-options no-fail) (buffer-mode block) (native-transcoder))
              (lambda (text-input)
                (let ((obj (get-datum text-input)))
                  (let ((fasl-port (open-temporary-file-port)))
                    (put-fasl fasl-port obj)
                    (set-port-position! fasl-port 0)
                    fasl-port))))
          (lambda (input)
            (let ((bytes (let loop ((c 0))
                           (cond ((and (eof-object? (get-u8 input)) c))
                                 (else (loop (+ c 1)))))))
              (format #t "~&;; ~a bytes~%~!" bytes)
              (set-port-position! input 0)
              (call-with-port
                  (open-file-output-port target-file-name (file-options no-fail) (buffer-mode block) (native-transcoder))
                  (lambda (output)
                    (put-string output (format "static const uint8_t s_~a[~a] = {" (pregexp-replace* "-" name "_") bytes))
                    (let loop ((c 0))
                      (if (zero? (mod c 16)) (put-char output #\linefeed))
                      (let ((b (get-u8 input)))
                        (cond ((eof-object? (lookahead-u8 input))
                               (put-string output (format "0x~x\n};\n" b)))
                              (else
                               (put-string output (format "0x~x," b))
                               (loop (+ c 1))))))))))))))

(convert "case-folding")
(convert "general-category-1")
(convert "general-category-2")
(convert "numeric-property")
(convert "other-alphabetic")
(convert "other-lowercase")
(convert "other-uppercase")
(convert "simple-lowercase")
(convert "simple-titlecase")
(convert "simple-uppercase")
(convert "special-casing-lower")
(convert "special-casing-title")
(convert "special-casing-upper")
(convert "canonical-class")
(convert "decompose")
(convert "compose")
(convert "compatibility")



