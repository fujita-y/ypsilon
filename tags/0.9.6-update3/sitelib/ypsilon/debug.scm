#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon debug)
  (export debug-expand debug-compile)
  (import (core))

  (define pretty-form
    (lambda (form)

      (define ht-symbols (make-eq-hashtable))

      (define count 1)

      (define pretty-symbol
        (lambda (x)
          (cond ((string-contains x (current-library-suffix))
                 => (lambda (n)
                      (pretty-symbol (substring x (+ n 1) (string-length x)))))
                ((string=? x "...") (string->symbol x))
                ((char=? (string-ref x 0) (current-primitive-prefix))
                 (pretty-symbol (substring x 1 (string-length x))))
                (else
                 (string->symbol x)))))

      (define crawl
        (lambda (lst)
          (let loop ((lst lst))
            (cond ((pair? lst)
                   (loop (car lst))
                   (loop (cdr lst)))
                  ((vector? lst)
                   (loop (vector->list lst)))
                  ((uninterned-symbol? lst)
                   (cond ((hashtable-contains? ht-symbols lst))
                         (else
                          (hashtable-set! ht-symbols lst (pretty-symbol (format "~a.~a" (uninterned-symbol-prefix lst) count)))
                          (set! count (+ count 1)))))
                  ((symbol? lst)
                   (hashtable-set! ht-symbols lst (pretty-symbol (symbol->string lst))))))))

      (define rewrite
        (lambda (lst)
          (let loop ((lst lst))
            (cond ((pair? lst)
                   (cons (loop (car lst)) (loop (cdr lst))))
                  ((symbol? lst)
                   (hashtable-ref ht-symbols lst lst))
                  ((vector? lst)
                   (list->vector (map loop (vector->list lst))))
                  (else lst)))))

      (crawl form)
      (rewrite form)))

  (define debug-expand
    (lambda (form)
      (parameterize ((backtrace #t))
        (pretty-print (pretty-form (macro-expand form))))))

  (define debug-compile
    (lambda (form)
      (parameterize ((backtrace #f))
        (pretty-print (pretty-form (compile form))))))

  ) ;[end]
