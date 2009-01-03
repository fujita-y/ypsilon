#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon stdio)
  (export stdin
          stdout
          stderr
          writeln
          displayln
          printf
          (rename (format fprintf)))
  (import (core))

  (define-syntax stdin (identifier-syntax (current-input-port)))
  (define-syntax stdout (identifier-syntax (current-output-port)))
  (define-syntax stderr (identifier-syntax (current-error-port)))
  (define writeln (lambda (x) (format #t "~s\n" x)))
  (define displayln (lambda (x) (format #t "~a\n" x)))
  (define printf (lambda x (apply format #t x)))

  ) ;[end]
