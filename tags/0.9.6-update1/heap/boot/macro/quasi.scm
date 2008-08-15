;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define expand-quasiquote
  (lambda (form env)

    (define unquote? (lambda (tag) (denote-unquote? env tag)))
    (define quasiquote? (lambda (tag) (denote-quasiquote? env tag)))
    (define unquote-splicing? (lambda (tag) (denote-unquote-splicing? env tag)))
            
    (define quoted?
      (lambda (e)
        (and (pair? e)
             (pair? (cdr e))
             (null? (cddr e))
             (denote-quote? env (car e)))))
    
    (define constant?
      (lambda (e)
        (or (boolean? e)
            (number? e)
            (char? e)
            (string? e)
            (bytevector? e)
            (quoted? e))))

    (define constant-value
      (lambda (e)
        (cond ((quoted? e) (cadr e))
              (else e))))

    (define null-constant?
      (lambda (e)
        (and (quoted? e)
             (null? (cadr e)))))

    (define emit-append
      (lambda (body tail)
        (cond ((null? body) tail)
              ((null-constant? tail)
               (if (= (length body) 1) (car body) `(.APPEND ,@body)))
              (else
               `(.APPEND ,@body ,tail)))))
    
    (define emit-cons*
      (lambda (body tail)
        (if (= (length body) 1)
            (emit-cons (car body) tail)
            (cond ((null? body) tail)
                  ((null-constant? tail)
                   `(.LIST ,@body))
                  ((and (pair? tail) (eq? (car tail) '.LIST))
                   `(.LIST ,@body ,@(cdr tail)))
                  ((and (pair? tail) (or (eq? (car tail) '.CONS) (eq? (car tail) '.CONS*)))
                   `(.CONS* ,@body ,@(cdr tail)))
                  (else
                   `(.CONS* ,@body ,tail))))))

    (define emit-cons
      (lambda (head tail)
        (if (and (constant? head) (constant? tail))
            (list '.QUOTE (cons (constant-value head) (constant-value tail)))
            (cond ((null-constant? tail)
                   `(.LIST ,head))
                  ((and (pair? tail) (eq? (car tail) '.LIST))
                   `(.LIST ,head ,@(cdr tail)))
                  ((and (pair? tail) (or (eq? (car tail) '.CONS) (eq? (car tail) '.CONS*)))
                   `(.CONS* ,head ,@(cdr tail)))
                  (else
                   `(.CONS ,head ,tail))))))

    (define expand-vector
      (lambda (expr nest)
        (let ((lst (expand (vector->list expr) nest)))
          (cond ((null-constant? lst)
                 `(.QUOTE #()))
                ((constant? lst)
                 `(.QUOTE ,(list->vector (constant-value lst))))
                ((eq? (car lst) '.LIST)
                 `(.VECTOR ,@(cdr lst)))
                (else
                 `(.LIST->VECTOR ,lst))))))

    (define expand
      (lambda (expr nest)
        (cond ((pair? expr)
               (if (= nest 0)
                   (destructuring-match expr
                     ((((? unquote? _) e1 ...) . e2)
                      (emit-cons* e1 (expand e2 0)))
                     ((((? unquote-splicing? _) e1 ...) . e2)
                      (emit-append e1 (expand e2 0)))
                     (((? quasiquote? _) _ ...)
                      (emit-cons (expand (car expr) 1) 
                                 (expand (cdr expr) 1)))
                     (((? unquote? _) e1) e1)
                     (((? unquote? _) . _)
                      (syntax-violation 'quasiquote "unquote appear in bad context" form expr))
                     (((? quasiquote? _) . _)
                      (syntax-violation 'quasiquote "nested quasiquote appear in bad context" form expr))
                     (((? unquote-splicing? _) . _)
                      (syntax-violation 'quasiquote "unquote-splicing appear in bad context" form expr))
                     (_ (emit-cons (expand (car expr) 0) 
                                   (expand (cdr expr) 0))))
                   (let ((tag (car expr)))
                     (cond ((or (denote-unquote? env tag) (denote-unquote-splicing? env tag))
                            (emit-cons `(.QUOTE ,tag) 
                                       (expand (cdr expr) (- nest 1))))
                           ((denote-quasiquote? env tag)
                            (emit-cons `(.QUOTE ,tag) 
                                       (expand (cdr expr) (+ nest 1))))
                           (else
                            (emit-cons (expand (car expr) nest) 
                                       (expand (cdr expr) nest)))))))
              ((vector? expr)
               (expand-vector expr nest))
              ((symbol? expr)
               `(.QUOTE ,expr))
              ((null? expr)
               `(.QUOTE ()))
              (else expr))))

    (expand-form (annotate (expand (cadr form) 0) form) env)))
