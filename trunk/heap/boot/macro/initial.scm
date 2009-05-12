;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define expansion-backtrace (make-parameter 5)) ; #f or fixnum
(define expansion-trace-stack (make-parameter '()))
(define expansion-trace-level (make-parameter 0))
(define current-immutable-identifiers (make-parameter #f))
(define current-expansion-environment (make-parameter '()))
(define current-transformer-environment (make-parameter '()))
(define current-template-environment (make-parameter #f))
(define current-expansion-mode (make-parameter '()))
(define current-macro-expression (make-parameter #f))
(define unexpect-top-level-form (make-parameter #f))
(define current-after-expansion-hook (make-parameter (lambda (form annotate annotate-closure) form)))
(define current-temporary-count (make-parameter 0))
(define current-rename-count (make-parameter 0))
(define current-temporaries (make-parameter #f))
(define current-closure-comments (make-parameter #f))
(define current-top-level-exterior (make-parameter #f))
(define current-top-level-renames (make-parameter (make-core-hashtable)))

(set-top-level-value! '.set-top-level-macro!
  (lambda (type keyword spec env)
    (and (top-level-bound? keyword) (set-top-level-value! keyword .&UNDEF))
    (core-hashtable-set! (current-macro-environment)
                         keyword
                         (case type
                           ((syntax)
                            (make-macro spec env))
                           ((variable)
                            (cond ((procedure? spec)
                                   (make-macro-variable spec env))
                                  ((variable-transformer-token? spec)
                                   (make-macro-variable (tuple-ref spec 1) env))
                                  (else
                                   (scheme-error "internal error: .set-top-level-macro! ~s" (list type keyword spec)))))
                           (else
                            (scheme-error "internal error: .set-top-level-macro! ~s" (list type keyword spec)))))))

(define core-primitive-name
  (lambda (e)
    (string->symbol (format "~a~a" (current-primitive-prefix) e))))

(define generate-global-id
  (lambda (library-id symbol)
    (string->symbol (format "~a~a~a" library-id (current-library-suffix) symbol))))

(define make-temporary-symbol
  (lambda (name prefix)
    (let ((temps (current-temporaries)))
      (or (core-hashtable-ref temps name #f)
          (let ((new (string->uninterned-symbol name prefix)))
            (core-hashtable-set! temps name new)
            new)))))

(define generate-temporary-symbol
  (lambda ()
    (let ((count (current-temporary-count)))
      (current-temporary-count (+ count 1))
      (let ((name (format ".L~a" count)))
        (make-temporary-symbol name (string-length name))))))

(define generate-local-macro-symbol
  (lambda (id)
    (let ((count (current-temporary-count)))
      (current-temporary-count (+ count 1))
      (make-temporary-symbol (format ".MACRO~a.~a" count id) 6))))

(define local-macro-symbol?
  (lambda (id)
    (and (uninterned-symbol? id) (string=? (uninterned-symbol-prefix id) ".MACRO"))))

(define rename-id
  (lambda (id count)
    (if (uninterned-symbol? id)
        (make-temporary-symbol (format "~a~a~a" id (current-rename-delimiter) count) (string-length (uninterned-symbol-prefix id)))
        (make-temporary-symbol (format "~a~a~a" id (current-rename-delimiter) count) (string-length (symbol->string id))))))

(define renamed-id?
  (lambda (id)
    (and (uninterned-symbol? id)
         (string-contains (uninterned-symbol-suffix id) (current-rename-delimiter)))))

(define rename-variable-id
  (lambda (id count)
    (if (uninterned-symbol? id)
        (make-temporary-symbol (format "~a~a~a*" id (current-rename-delimiter) count) (string-length (uninterned-symbol-prefix id)))
        (make-temporary-symbol (format "~a~a~a*" id (current-rename-delimiter) count) (string-length (symbol->string id))))))

(define renamed-variable-id?
  (lambda (id)
    (and (uninterned-symbol? id)
         (string-contains (uninterned-symbol-suffix id) (current-rename-delimiter))
         (string-contains (uninterned-symbol-suffix id) #\*))))

(define compose-id
  (lambda (id suffix)
    (if (uninterned-symbol? id)
        (make-temporary-symbol (format "~a~a" id suffix) (string-length (uninterned-symbol-prefix id)))
        (make-temporary-symbol (format "~a~a" id suffix) (string-length (symbol->string id))))))

(define original-id
  (lambda (id)
    (if (renamed-id? id) (string->symbol (uninterned-symbol-prefix id)) id)))

(define strip-rename-suffix
  (lambda (lst)
    (if (cyclic-object? lst)
        lst
        (let loop ((lst lst))
          (cond ((pair? lst)
                 (let ((a (loop (car lst))) (d (loop (cdr lst))))
                   (if (and (eq? a (car lst)) (eq? d (cdr lst))) lst (cons a d))))
                ((symbol? lst)
                 (original-id lst))
                ((vector? lst)
                 (list->vector (map loop (vector->list lst))))
                (else lst))))))

(define retrieve-rename-suffix
  (lambda (id)
    (cond ((renamed-id? id) (uninterned-symbol-suffix id))
          (else ""))))

(define fresh-rename-count
  (lambda ()
    (current-rename-count (+ (current-rename-count) 1))
    (current-rename-count)))

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
    (let ((ht-comments (current-source-comments)))

      (define put-note!
        (lambda (form note)
          (and note
               (let loop ((lst form))
                 (and (list? lst)
                      (or (core-hashtable-ref ht-comments lst #f)
                          (begin
                            (core-hashtable-set! ht-comments lst note)
                            (for-each loop lst))))))))

      (define get-note
        (lambda (source)
          (let loop ((lst source))
            (and (pair? lst)
                 (or (core-hashtable-ref ht-comments lst #f)
                     (loop (car lst))
                     (loop (cdr lst)))))))

      (and ht-comments
           (pair? form)
           (pair? source)
           (not (eq? form source))
           (cond ((core-hashtable-ref ht-comments source #f)
                  => (lambda (e) (core-hashtable-set! ht-comments form e)))
                 ((get-note source)
                  => (lambda (e) (put-note! form e))))))
    form))

(define annotate-macro!
  (lambda (form source)
    (and (current-source-comments)
         (annotate (if (wrapped-syntax-object? form) (syntax-object-expr form) form)
                   (if (wrapped-syntax-object? source) (syntax-object-expr source) source)))
    (unspecified)))

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
