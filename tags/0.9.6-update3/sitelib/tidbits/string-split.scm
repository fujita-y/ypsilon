#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (tidbits string-split)
  (export string-split)
  (import (core))

  (define split->list
    (lambda (str proc)
      (let ((in (make-string-input-port str))
            (out (make-string-output-port)))
        (let loop1 ((lst '()))
          (let loop2 ((c (get-char in)))
            (cond ((eof-object? c)
                   (let ((s (extract-accumulated-string out)))
                     (if (string=? s "")
                         (reverse lst)
                         (reverse (cons s lst)))))
                  ((proc c)
                   (loop1 (cons (extract-accumulated-string out) lst)))
                  (else
                   (put-char out c)
                   (loop2 (get-char in)))))))))

  (define string-split
    (lambda (str delim)
      (cond ((char? delim)
             (split->list str (lambda (c) (char=? c delim))))
            ((string? delim)
             (let ((lst (string->list delim)))
               (split->list str (lambda (c) (exists (lambda (d) (char=? c d)) lst)))))
            ((procedure? delim)
             (split->list str delim)))))

  ) ;[end]
