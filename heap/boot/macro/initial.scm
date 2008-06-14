;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define current-library-prefix (make-parameter ""))
(define current-library-infix (make-parameter "."))
(define current-library-suffix (make-parameter "'"))

(define current-rename-delimiter (make-parameter "`"))
(define expansion-backtrace (make-parameter 5)) ; #f or fixnum
(define expansion-trace-stack (make-parameter '()))
(define expansion-trace-level (make-parameter 0))
(define current-immutable-identifiers (make-parameter #f))
(define current-expansion-mode (make-parameter '()))
(define current-expansion-environment (make-parameter '()))
(define current-macro-expression (make-parameter #f))
(define current-transformer-environment (make-parameter '()))
(define unexpect-top-level-form (make-parameter #f))
(define current-after-expansion-hook (make-parameter (lambda (form annotate annotate-closure) form)))
(define current-temporary-count (make-parameter 0))
(define current-rename-count (make-parameter 0))
(define current-closure-comments (make-parameter (make-core-hashtable)))
(define current-top-level-exterior (make-parameter #f))

(set-top-level-value! '.set-top-level-macro!
  (lambda (type keyword datum env)
    (and (top-level-bound? keyword)
         (set-top-level-value! keyword .&UNDEF))
    (core-hashtable-set! (current-macro-environment)
                         keyword
                         (case type
                           ((syntax)
                            (make-macro datum env))
                           ((variable)
                            (cond ((procedure? datum)
                                   (make-macro-variable datum env))
                                  ((variable-transformer-token? datum)
                                   (make-macro-variable (tuple-ref datum 1) env))
                                  (else
                                   (scheme-error "internal error in .set-top-level-macro!: bad transformer type:~s keyword:~s datum:~s" type keyword datum))))))))

(define generate-temporary-symbol
  (lambda ()
    (let ((count (current-temporary-count)))
      (current-temporary-count (+ count 1))
      (string->symbol (format ".L~a" count)))))

(define generate-local-macro-symbol
  (lambda (id)
    (let ((count (current-temporary-count)))
      (current-temporary-count (+ count 1))
      (string->symbol (format ".local-macro-~a.~a~a~a" count id (current-rename-delimiter) (current-rename-count))))))

(define fresh-rename-count
  (lambda ()
    (current-rename-count (+ (current-rename-count) 1))
    (current-rename-count)))

(define rename-id
  (lambda (id count)
    (or (symbol? id) (scheme-error "internal error in rename-id: expect symbol but got ~s" id))
    (string->symbol (format "~a~a~a" id (current-rename-delimiter) count))))

(define renamed-id?
  (lambda (id)
    (and (symbol? id)
         (string-contains (symbol->string id) (current-rename-delimiter)))))

(define original-id
  (lambda (id)
    (or (symbol? id) (scheme-error "internal error in original-id: expect symbol but got ~s" id))
    (let ((name (symbol->string id)))
      (cond ((string-contains name (current-rename-delimiter))
             => (lambda (mark) (string->symbol (substring name 0 mark))))
            (else id)))))

(define strip-rename-suffix
  (lambda (lst)
    (cond ((pair? lst)
           (let ((a (strip-rename-suffix (car lst)))
                 (d (strip-rename-suffix (cdr lst))))
             (cond ((and (eq? a (car lst)) (eq? d (cdr lst))) lst)
                   (else (cons a d)))))
          ((symbol? lst) (original-id lst))
          ((vector? lst) (list->vector (map strip-rename-suffix (vector->list lst))))
          (else lst))))

(define retrieve-rename-suffix
  (lambda (id)
    (or (symbol? id) (scheme-error "internal error in retrieve-rename-suffix: expect symbol but got ~s" id))
    (let ((name (symbol->string id)))
      (cond ((string-contains name (current-rename-delimiter))
             => (lambda (mark) (substring name mark (string-length name))))
            (else "")))))

(define set-closure-comment!
  (lambda (form note)
    (and (current-closure-comments)
         (core-hashtable-set! (current-closure-comments) form (cons 'heap note)))))

(define annotate-closure
  (lambda (form source . attr)
    (and (current-closure-comments)
         (cond ((core-hashtable-ref (current-closure-comments) source #f)
                => (lambda (note)
                     (if (null? attr)
                         (core-hashtable-set! (current-closure-comments) form note)
                         (core-hashtable-set! (current-closure-comments) form (cons (car attr) (cdr note))))))))))

(define annotated?
  (lambda (form)
    (and (current-source-comments)
         (core-hashtable-ref (current-source-comments) form #f)
         #t)))

(define get-annotation
  (lambda (form)
    (and (pair? form)
         (current-source-comments)
         (core-hashtable-ref (current-source-comments) form #f))))

(define put-annotation
  (lambda (form note)
    (and (pair? form)
         (current-source-comments)
         (core-hashtable-set! (current-source-comments) form note))
    form))

(define annotate
  (lambda (form source)

    (define put-note!
      (lambda (form note)
        (and note
             (let loop ((lst form))
               (and (list? lst)
                    (or (core-hashtable-ref (current-source-comments) lst #f)
                        (begin
                          (core-hashtable-set! (current-source-comments) lst note)
                          (for-each loop lst))))))))

    (define get-note
      (lambda (source)
        (let loop ((lst source))
          (and (pair? lst)
               (or (core-hashtable-ref (current-source-comments) lst #f)
                   (loop (car lst))
                   (loop (cdr lst)))))))

    (and (pair? form)
         (pair? source)
         (not (eq? form source))
         (begin
           (cond ((and (current-source-comments) (get-note source))
                  => (lambda (e) (put-note! form e))))
           (cond ((and (current-closure-comments) (core-hashtable-ref (current-closure-comments) source #f))
                  => (lambda (e) (core-hashtable-set! (current-closure-comments) form e))))))
    form))

(define abbreviated-take
  (lambda (form n)
    (annotate
     (let loop ((lst form) (n n))
       (cond ((not (pair? lst)) lst)
             ((<= n 0) (list '...))
             (else (cons (car lst) (loop (cdr lst) (- n 1))))))
     form)))

(define abbreviated-take-form
  (lambda (form ncar ncdr)
    (annotate
     (let loop ((lst form) (na ncar) (nd ncdr))
       (cond ((not (pair? lst)) lst)
             ((or (<= na 0) (<= nd 0)) (list '...))
             (else (cons (loop (car lst) (- na 1) nd) (loop (cdr lst) ncar (- nd 1))))))
     form)))
