#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon apropos)
  (export apropos)
  (import (core) (ypsilon pregexp))

  (define parse-library-name
    (lambda (name)

      (define infix #\.)

      (let ((in (make-string-input-port (symbol->string name)))
            (out (make-string-output-port)))
        (put-char out #\()
        (let loop ((ch (get-char in)))
          (cond ((eof-object? ch)
                 (put-char out #\))
                 (get-datum (make-string-input-port (extract-accumulated-string out))))
                (else
                 (if (char=? ch infix)
                     (put-char out #\space)
                     (put-char out ch))
                 (loop (get-char in))))))))

  (define symbol<?
    (lambda (a b)
      (string<? (symbol->string a)
                (symbol->string b))))

  (define apropos
    (lambda args
      (let ((args (map (lambda (arg)
                         (cond ((symbol? arg) (symbol->string arg))
                               ((string? arg) (pregexp arg))
                               (else (error 'apropos (format "expected symbol or string, but got ~r" arg) args))))
                       args))
            (ht (make-core-hashtable)))

        (define match?
          (lambda (e)
            (for-all (lambda (arg)
                       (cond ((string? arg)
                              (and (string-contains (symbol->string e) arg) e))
                             ((list? arg)
                              (and (pregexp-match arg (symbol->string e)) e))
                             (else #f)))
                     args)))

        (for-each (lambda (lst)
                    (and (pair? (cdr lst))
                         (let ((lib-name (car lst)) (exports (cdr lst)))
                           (let ((found (filter match? (map car exports))))
                             (and (pair? found)
                                  (for-each (lambda (e)
                                              (core-hashtable-set! ht e (list-sort symbol<? (cons lib-name (core-hashtable-ref ht e '())))))
                                            found))))))
                  (core-hashtable->alist (scheme-library-exports)))
        (let ((lst (list-sort (lambda (a b) (symbol<? (car a) (car b)))
                              (core-hashtable->alist ht))))
          (let ((col (+ 4 (apply max 0 (map (lambda (e) (string-length (symbol->string (car e)))) lst)))))
            (for-each (lambda (line)
                        (let ((n (string-length (symbol->string (car line)))))
                          (format #t ";; ~s" (car line))
                          (put-string (current-output-port) (make-string (- col n) #\space))
                          (for-each (lambda (e)
                                      (format #t " ~s" (parse-library-name e)))
                                    (cdr line))
                          (format #t "~%")))
                      lst))))))

  ) ;[end]
