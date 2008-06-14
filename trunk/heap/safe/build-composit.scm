
(library (anonymous)

  (export)

  ;; import everything
  (import (core primitives)
          (core optimize)
          (core parameters)
          (core io)
          (core files)
          (core exceptions)
          (core arithmetic)
          (core sorting)
          (core bytevectors)
          (core syntax-case)
          (core r5rs)
          (core control)
          (core optargs)
          (core chkarg)
          (core lists)
          (core records)
          (core conditions)
          (core bytevector-transcoders)
          (core unicode-assistants)
          (core unicode)
          (core hashtables)
          (core struct)
          (core enums)
          (rnrs))

  (define target-file-name (format "~a/../stdlib/core.scm" (current-directory)))

  (define libraries
    '((core primitives)
      (core optimize)
      (core parameters)
      (core io)
      (core files)
      (core exceptions)
      (core arithmetic)
      (core sorting)
      (core bytevectors)
      (core syntax-case)
      (core r5rs)
      (core control)
      (core optargs)
      (core chkarg)
      (core lists)
      (core destructuring)
      (core records)
      (core conditions)
      (core bytevector-transcoders)
      #;(core unicode-assistants)
      (core unicode)
      (core hashtables)
      (core struct)
      (core enums)
      (rnrs)))

  (define exported (make-core-hashtable))

  (define library-internal-name
    (lambda (name)

      (define infix ".")

      (define symbol-list->string
        (lambda (ref)
          (apply string-append
                 (cdr (let loop ((lst ref))
                        (cond ((null? lst) '())
                              ((symbol? (car lst))
                               (cons infix
                                     (cons (symbol->string (car lst))
                                           (loop (cdr lst)))))
                              (else
                               (loop (cdr lst)))))))))

      (define malformed-name
        (lambda ()
          (assertion-violation 'library-internal-name "malformed library name" name)))

      (if (and (list? name) (not (null? name)))
          (if (for-all symbol? name)
              (string->symbol (symbol-list->string name))
              (let ((body (list-head name (- (length name) 1))))
                (if (for-all symbol? body)
                    (string->symbol (symbol-list->string body))
                    (malformed-name))))
          (malformed-name))))

  (define symbol<?
    (lambda (e1 e2)
      (string<? (symbol->string e1)
                (symbol->string e2))))

  (begin
    (format #t ";; build ~a~!" target-file-name)
    (for-each (lambda (lst)
                (let ((name (library-internal-name lst)))
                  (format #t "~%;; processing ~s~!" name)
                  (cond ((core-hashtable-ref (scheme-library-exports) name #f)
                         => (lambda (lst)
                              (let ((lst (filter (lambda (e) (not (core-hashtable-ref exported e #f)))
                                                 (map car lst))))
                                (for-each (lambda (e) (core-hashtable-set! exported e #t)) lst)
                                (list-sort symbol<? lst)
                                )))
                        (else
                         (assertion-violation #f "library not imported" name)))))
              libraries)

    (let ((lst (list-sort symbol<? (map car (core-hashtable->alist exported)))))
      (call-with-port
       (open-file-output-port target-file-name (file-options no-fail) (buffer-mode block) (native-transcoder))
       (lambda (out)
         (format out "(library (core)~%~%")
         (format out "  (export")
         (for-each (lambda (i) (format out "~%    ~a" i)) lst)
         (format out ")~%~%")
         (format out "  (import")
         (for-each (lambda (e) (format out "~%    ~a" e)) libraries)
         (format out ")~%~%  ) ;[end]~%~!")
         (format #t "~%~%~!")))))

  ) ;[end]
