#!/usr/local/bin/runic

;; Copyright (c) 2007, 2008 Yoshikatsu Fujita. All rights reserved. 
;; runic scheme environment
;; See license.txt for terms and conditions of use.

(begin

  (import (core primitives)
          (core destructuring)
          (core optimize)
          (core parameters)
          (core io)
          (core files))

  (assert (string-contains (current-directory) (home-directory)))

  (define source-file-name "bootimage.vmi")
  (define target-file-name "bootimage.code")


  (format #t "~%;; converting ~a\n;; --> ~a/~a~!" source-file-name (current-directory) target-file-name)

  (call-with-port
   (open-file-input-port source-file-name (file-options no-fail) (buffer-mode block))
   (lambda (input)
     (let ((bytes (let loop ((c 0))
                    (cond ((and (eof-object? (get-u8 input)) c))
                          (else (loop (+ c 1)))))))
       (format #t "~&;; ~a bytes~%~!" bytes)
       (set-port-position! input 0)
       (call-with-port
        (open-file-output-port target-file-name (file-options no-fail) (buffer-mode block) (native-transcoder))
        (lambda (output)
          (put-string output (format "static const uint8_t s_bootimage[~a] = {" bytes))
          (let loop ((c 0))
            (if (zero? (mod c 16)) (put-byte output (char->integer #\linefeed)))
            (let ((b (get-u8 input)))
              (cond ((eof-object? (lookahead-u8 input))
                     (put-string output (format "0x~x\n};\n" b)))
                    (else
                     (put-string output (format "0x~x," b))
                     (loop (+ c 1)))))))))))

  )