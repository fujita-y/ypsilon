;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define extend-env
  (lambda (bindings env)
    (if (null? bindings)
        env
        (append bindings env))))

(define env-lookup
  (lambda (env id)
    (or (symbol? id)
        (scheme-error "internal error: env-lookup: expect symbol but got ~s" id))
    (cond ((assq id env)
           => (lambda (binding)
                (cond ((import? (cdr binding))
                       (let ((extern (cddr binding)))
                         (core-hashtable-ref (current-macro-environment) extern extern)))
                      (else
                       (cdr binding)))))
          (else
           (core-hashtable-ref (current-macro-environment) id id)))))

(define env-delete!
  (lambda (env id)
    (cond ((assq id env)
           => (lambda (binding)
                (set-car! binding #f)
                (set-cdr! binding 'no-use))))))

#;(define free-id=?
  (lambda (id1 id2)
    (let ((env (current-expansion-environment)))

      (define free-lexical-name=?
        (lambda (id1 id2)
          (and (eq? (original-id id1) (original-id id2))
               (or (eq? id1 id2)
                   (let ((deno1 (env-lookup env id1))
                         (deno2 (env-lookup env id2)))
                     (or (eq? deno1 deno2)
                         (and (or (eq? id1 deno1) (unbound? deno1))
                              (or (eq? id2 deno2) (unbound? deno2)))))))))
      
      (define lexical-name
        (lambda (id)
          (if (symbol? id)
              (lookup-lexical-name id env)
              (or (syntax-object-lexname id)
                  (lookup-lexical-name (syntax-object-expr id) env)))))

      (free-lexical-name=? (lexical-name id1) (lexical-name id2)))))

(define free-id=?
  (lambda (id1 id2)
    
    (define lexical=?
      (lambda (env s1 s2)
        (or (eq? s1 s2)
            (and (eq? (original-id s1) (original-id s2))
                 (let ((deno1 (env-lookup env s1)) (deno2 (env-lookup env s2)))
                   (or (eq? deno1 deno2)
                       (and (or (eq? s1 deno1) (unbound? deno1))
                            (or (eq? s2 deno2) (unbound? deno2)))))))))
    
    (or (eq? id1 id2)
        (let* ((env (current-expansion-environment)) (s1 (lookup-lexical-name id1 env)))
          (if (symbol? id2)
              (lexical=? env s1 (lookup-lexical-name id2 env))
              (cond ((syntax-object-lexname id2) => (lambda (s2) (eq? s1 s2)))
                    (else
                     (let ((s2 (lookup-lexical-name (syntax-object-expr id2) env)))
                       (if (renamed-id? s1) (eq? s1 s2) (lexical=? env s1 s2))))))))))

(define make-import
  (lambda (id)
    (cons 'import id)))

(define make-unbound
  (lambda ()
    '(unbound)))

(define make-out-of-context
  (lambda (template)
    (if template
        (cons 'out-of-context template)
        '(out-of-context . #f))))

(define make-pattern-variable
  (lambda (rank)
    (cons 'pattern-variable rank)))

(define make-macro
  (lambda (spec env)
    (cons* 'macro spec env)))

(define make-macro-variable
  (lambda (spec env)
    (cons* 'macro-variable spec env)))
  
(define make-special
  (lambda (proc)
    (cons 'special proc)))

(define import?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'import))))

(define unbound?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'unbound))))

(define out-of-context?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'out-of-context))))

(define macro?
  (lambda (den)
    (and (pair? den)
         (or (eq? (car den) 'macro)
             (eq? (car den) 'macro-variable)))))

(define macro-variable?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'macro-variable))))

(define pattern-variable?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'pattern-variable))))

(define special?
  (lambda (den)
    (and (pair? den)
         (eq? (car den) 'special))))

(define unexpected-unquote
  (lambda (expr env)
    (syntax-violation (car expr) "unquote appear outside of quasiquote" expr)))

(define unexpected-unquote-splicing
  (lambda (expr env)
    (syntax-violation (car expr) "unquote-splicing appear outside of quasiquote" expr)))

(define unexpected-auxiliary-syntax
  (lambda (expr env)
    (syntax-violation (car expr) "misplaced auxiliary syntactic keyword" expr)))

(define unexpected-syntax
  (lambda (expr env)
    (syntax-violation (car expr) "misplaced syntactic keyword" expr)))

(define core-env (make-core-hashtable))

(let ()
  
  (define init-core-macro 
    (lambda (id deno)
      (core-hashtable-set! core-env id deno)
      (core-hashtable-set! core-env (core-primitive-name id) deno)))

  (init-core-macro 'lambda            (make-special expand-lambda))
  (init-core-macro 'quote             (make-special expand-quote))
  (init-core-macro 'if                (make-special expand-if))
  (init-core-macro 'set!              (make-special expand-set!))
  (init-core-macro 'define-syntax     (make-special expand-define-syntax))
  (init-core-macro 'let-syntax        (make-special expand-let-syntax))
  (init-core-macro 'letrec-syntax     (make-special expand-letrec-syntax))
  (init-core-macro 'begin             (make-special expand-begin))
  (init-core-macro 'define            (make-special expand-define))
  (init-core-macro 'quasiquote        (make-special expand-quasiquote))
  (init-core-macro 'let               (make-special expand-let))
  (init-core-macro 'letrec            (make-special expand-letrec))
  (init-core-macro 'let*              (make-special expand-let*))
  (init-core-macro 'cond              (make-special expand-cond))
  (init-core-macro 'case              (make-special expand-case))
  (init-core-macro 'do                (make-special expand-do))
  (init-core-macro 'and               (make-special expand-and))
  (init-core-macro 'or                (make-special expand-or))
  (init-core-macro 'letrec*           (make-special expand-letrec*))
  (init-core-macro 'library           (make-special expand-library))
  (init-core-macro 'define-macro      (make-special expand-define-macro))
  (init-core-macro 'let*-values       (make-special expand-let*-values))
  (init-core-macro 'let-values        (make-special expand-let-values))
  (init-core-macro 'syntax            (make-special expand-syntax))
  (init-core-macro 'syntax-case       (make-special expand-syntax-case))
  (init-core-macro 'identifier-syntax (make-special expand-identifier-syntax))
  (init-core-macro 'assert            (make-special expand-assert))
  (init-core-macro 'unquote           (make-special unexpected-unquote))
  (init-core-macro 'unquote-splicing  (make-special unexpected-unquote-splicing))
  (init-core-macro 'syntax-rules      (make-special unexpected-syntax))
  (init-core-macro 'else              (make-special unexpected-auxiliary-syntax))
  (init-core-macro '=>                (make-special unexpected-auxiliary-syntax))
  (init-core-macro '...               (make-special unexpected-auxiliary-syntax))
  (init-core-macro '_                 (make-special unexpected-auxiliary-syntax))
  (init-core-macro 'import            (make-special expand-import)))

(define denote-lambda           (core-hashtable-ref core-env 'lambda #f))
(define denote-begin            (core-hashtable-ref core-env 'begin #f))
(define denote-define           (core-hashtable-ref core-env 'define #f))
(define denote-define-syntax    (core-hashtable-ref core-env 'define-syntax #f))
(define denote-let-syntax       (core-hashtable-ref core-env 'let-syntax #f))
(define denote-letrec-syntax    (core-hashtable-ref core-env 'letrec-syntax #f))
(define denote-define-macro     (core-hashtable-ref core-env 'define-macro #f))
(define denote-library          (core-hashtable-ref core-env 'library #f))
(define denote-quasiquote       (core-hashtable-ref core-env 'quasiquote #f))
(define denote-quote            (core-hashtable-ref core-env 'quote #f))
(define denote-if               (core-hashtable-ref core-env 'if #f))
(define denote-set!             (core-hashtable-ref core-env 'set! #f))
(define denote-unquote          (core-hashtable-ref core-env 'unquote #f))
(define denote-unquote-splicing (core-hashtable-ref core-env 'unquote-splicing #f))
(define denote-let              (core-hashtable-ref core-env 'let #f))
(define denote-letrec           (core-hashtable-ref core-env 'letrec #f))
(define denote-let*             (core-hashtable-ref core-env 'let* #f))
(define denote-cond             (core-hashtable-ref core-env 'cond #f))
(define denote-case             (core-hashtable-ref core-env 'case #f))
(define denote-do               (core-hashtable-ref core-env 'do #f))
(define denote-and              (core-hashtable-ref core-env 'and #f))
(define denote-or               (core-hashtable-ref core-env 'or #f))
(define denote-letrec*          (core-hashtable-ref core-env 'letrec* #f))
(define denote-let*-values      (core-hashtable-ref core-env 'let*-values #f))
(define denote-let-values       (core-hashtable-ref core-env 'let-values #f))
(define denote-syntax-quote     (core-hashtable-ref core-env 'syntax-quote #f))
(define denote-syntax           (core-hashtable-ref core-env 'syntax #f))
(define denote-syntax-case      (core-hashtable-ref core-env 'syntax-case #f))
(define denote-syntax-rules     (core-hashtable-ref core-env 'syntax-rules #f))
(define denote-else             (core-hashtable-ref core-env 'else #f))
(define denote-=>               (core-hashtable-ref core-env '=> #f))
(define denote-import           (core-hashtable-ref core-env 'import #f))

(define denote-macro?
  (lambda (env obj)
    (and (symbol? obj)
         (macro? (env-lookup env obj)))))

(define denote-special?
  (lambda (env obj)
    (and (symbol? obj)
         (special? (env-lookup env obj)))))

(define denote-lambda?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-lambda))))

(define denote-begin?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-begin))))

(define denote-let?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-let))))

(define denote-define-syntax?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-define-syntax))))

(define denote-let-syntax?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-let-syntax))))

(define denote-letrec-syntax?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-letrec-syntax))))

(define denote-define?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-define))))

(define denote-quote?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-quote))))

(define denote-quasiquote?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-quasiquote))))

(define denote-unquote?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-unquote))))

(define denote-unquote-splicing?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-unquote-splicing))))

(define denote-define-macro?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-define-macro))))

(define denote-syntax-rules?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-syntax-rules))))

(define denote-else?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-else))))

(define denote-=>?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-=>))))

(define denote-set!?
  (lambda (env obj)
    (and (symbol? obj)
         (eq? (env-lookup env obj) denote-set!))))

(define private-primitives-environment
  (list 
   (cons '.LIST '.list)
   (cons '.CONS '.cons)
   (cons '.CONS* '.cons*)
   (cons '.APPEND '.append)
   (cons '.VECTOR '.vector)
   (cons '.LIST->VECTOR '.list->vector)
   (cons '.EQ? '.eq?)
   (cons '.EQV? '.eqv?)
   (cons '.MEMQ '.memq)
   (cons '.MEMV '.memv)
   (cons '.CALL-WITH-VALUES '.call-with-values)
   (cons '.APPLY '.apply)
   (cons '.CDR '.cdr)     
   (cons '.IDENTIFIER? '.identifier?)
   (cons '.MAKE-VARIABLE-TRANSFORMER '.make-variable-transformer)
   (cons '.ASSERTION-VIOLATION '.assertion-violation)
   (cons '.UNSPECIFIED '.unspecified)
   (cons '.QUOTE denote-quote)
   (cons '.LET denote-let)
   (cons '.LETREC* denote-letrec*)
   (cons '.BEGIN denote-begin)
   (cons '.LAMBDA denote-lambda)
   (cons '.IF denote-if)
   (cons '.SET! denote-set!)
   (cons '.OR denote-or)
   (cons '.COND denote-cond)
   (cons '.ELSE denote-else)
   (cons '.DEFINE-SYNTAX denote-define-syntax)
   (cons '.SYNTAX denote-syntax)
   (cons '.SYNTAX-CASE denote-syntax-case)))



(current-macro-environment core-env)

