;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define ellipsis-pair?
  (lambda (form)
    (and (pair? form)
         (pair? (cdr form))
         (eq? (cadr form) '...))))

(define ellipsis-splicing-pair?
  (lambda (form)
    (and (pair? form)
         (pair? (cdr form))
         (eq? (cadr form) '...)
         (pair? (cddr form))
         (eq? (caddr form) '...))))

(define ellipsis-quote?
  (lambda (form)
    (and (pair? form)
         (pair? (car form))
         (eq? (caar form) '...))))

(define collect-unique-ids ; exclude '...
  (lambda (expr)
    (let loop ((lst expr) (ans '()))
      (cond ((pair? lst)
             (loop (cdr lst)
                   (loop (car lst) ans)))
            ((eq? lst '...) ans)
            ((symbol? lst)
             (if (memq lst ans) ans (cons lst ans)))
            ((vector? lst)
             (loop (vector->list lst) ans))
            (else ans)))))

(define collect-vars-ranks
  (lambda (pat lites depth ranks)
    (cond ((eq? pat '_) ranks)
          ((symbol? pat)
           (if (memq pat lites)
               ranks
               (acons pat depth ranks)))
          ((ellipsis-pair? pat)
           (collect-vars-ranks (cddr pat) lites depth
                               (if (symbol? (car pat))
                                   (acons (car pat) (+ depth 1) ranks)
                                   (collect-vars-ranks (car pat) lites (+ depth 1) ranks))))
          ((pair? pat)
           (collect-vars-ranks (cdr pat) lites depth
                               (collect-vars-ranks (car pat) lites depth ranks)))
          ((vector? pat)
           (collect-vars-ranks (vector->list pat) lites depth ranks))
          (else ranks))))

(define check-pattern
  (lambda (pat lites)

    (define check-duplicate-variable
      (lambda (pat lites)
        (let loop ((lst pat) (pool '()))
          (cond ((pair? lst)
                 (loop (cdr lst)
                       (loop (car lst) pool)))
                ((eq? lst '...) pool)
                ((eq? lst '_) pool)
                ((symbol? lst)
                 (if (memq lst lites)
                     pool
                     (if (memq lst pool)
                         (syntax-violation "syntax pattern" "duplicate pattern variables" pat lst)
                         (cons lst pool))))
                ((vector? lst)
                 (loop (vector->list lst) pool))
                (else pool)))))

    (define check-misplaced-ellipsis
      (lambda (pat lites)
        (let loop ((lst pat))
          (cond ((eq? lst '...)
                 (syntax-violation "syntax pattern" "improper use of ellipsis" pat))
                ((ellipsis-pair? lst)
                 (and (symbol? (car lst))
                      (memq (car lst) lites)
                      (syntax-violation "syntax pattern" "ellipsis following literal" pat lst))
                 (let loop ((lst (cddr lst)))
                   (and (pair? lst)
                        (if (eq? (car lst) '...)
                            (syntax-violation "syntax pattern" "ambiguous use of ellipsis" pat)
                            (loop (cdr lst))))))
                ((pair? lst)
                 (or (loop (car lst)) (loop (cdr lst))))
                ((vector? lst)
                 (loop (vector->list lst)))
                (else #f)))))

    (check-misplaced-ellipsis pat lites)
    (check-duplicate-variable pat lites)))

(define match-ellipsis?
  (lambda (expr pat lites)
    (or (null? expr)
        (and (pair? expr)
             (match-pattern? (car expr) (car pat) lites)
             (match-ellipsis? (cdr expr) pat lites)))))

(define match-ellipsis-n?
  (lambda (expr pat n lites)
    (or (= n 0)
        (and (pair? expr)
             (match-pattern? (car expr) (car pat) lites)
             (match-ellipsis-n? (cdr expr) pat (- n 1) lites)))))

(define match-pattern?
  (lambda (expr pat lites)
    (cond ((eq? pat '_) #t)
          ((symbol? pat)
           (cond ((memq pat lites)
                  (and (or (symbol? expr)
                           (identifier? expr))
                       (free-id=? pat expr)))
                 (else #t)))
          ((ellipsis-pair? pat)
           (if (and (null? (cddr pat)) (list? expr))
               (or (symbol? (car pat))
                   (match-ellipsis? expr pat lites))
               (let ((n (- (count-pair expr) (count-pair (cddr pat)))))
                 (if (= n 0)
                     (match-pattern? expr (cddr pat) lites)
                     (and (> n 0)
                          (match-ellipsis-n? expr pat n lites)
                          (match-pattern? (list-tail expr n) (cddr pat) lites))))))
          ((pair? pat)
           (and (pair? expr)
                (match-pattern? (car expr) (car pat) lites)
                (match-pattern? (cdr expr) (cdr pat) lites)))
          ((vector? pat)
           (and (vector? expr)
                (match-pattern? (vector->list expr) (vector->list pat) lites)))
          (else (equal? pat expr)))))

(define union-vars
  (lambda (vars evars)
    (if (null? evars)
        vars
        (union-vars (bind-var! (caar evars) (reverse (cdar evars)) vars)
                    (cdr evars)))))

(define bind-var!
  (lambda (pat expr vars)
    (cond ((eq? pat '_) vars)
          (else
           (let ((slot (assq pat vars)))
             (if slot
                 (begin (set-cdr! slot (cons expr (cdr slot))) vars)
                 (acons pat (list expr) vars)))))))

(define bind-null-ellipsis
  (lambda (pat lites vars)
    (let loop ((lst (collect-unique-ids (car pat))) (vars vars))
      (if (null? lst)
          vars
          (loop (cdr lst)
                (if (memq (car lst) lites)
                    vars
                    (bind-var! (car lst) '() vars)))))))

(define bind-ellipsis
  (lambda (expr pat lites vars evars)
    (if (null? expr)
        (if (null? evars)
            (bind-null-ellipsis pat lites vars)
            (union-vars vars evars))
        (bind-ellipsis (cdr expr) pat lites vars
                       (bind-pattern (car expr) (car pat) lites evars)))))

(define bind-ellipsis-n
  (lambda (expr pat lites n vars evars)
    (if (= n 0)
        (if (null? evars)
            (bind-null-ellipsis pat lites vars)
            (union-vars vars evars))
        (bind-ellipsis-n (cdr expr) pat lites (- n 1) vars
                         (bind-pattern (car expr) (car pat) lites evars)))))

(define bind-pattern
  (lambda (expr pat lites vars)
    (cond ((symbol? pat)
           (if (memq pat lites)
               vars
               (bind-var! pat expr vars)))
          ((ellipsis-pair? pat)
           (if (and (null? (cddr pat)) (list? expr))
               (if (symbol? (car pat))
                   (bind-var! (car pat) expr vars)
                   (bind-ellipsis expr pat lites vars '()))
               (let ((n (- (count-pair expr) (count-pair (cddr pat)))))
                 (bind-pattern (list-tail expr n) (cddr pat) lites
                               (if (and (= n 0) (symbol? (car pat)))
                                   (bind-var! (car pat) '() vars)
                                   (bind-ellipsis-n expr pat lites n vars '()))))))
          ((pair? pat)
           (bind-pattern (cdr expr) (cdr pat) lites
                         (bind-pattern (car expr) (car pat) lites vars)))
          ((vector? pat)
           (bind-pattern (vector->list expr) (vector->list pat) lites vars))
          (else vars))))
