;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define collect-rename-ids
  (lambda (template ranks)
    (let ((ids (collect-unique-ids template)))
      (let loop ((lst ids))
        (if (null? lst)
            lst
            (if (assq (car lst) ranks)
                (loop (cdr lst))
                (cons (car lst) (loop (cdr lst)))))))))

(define parse-ellipsis-splicing
  (lambda (form)
    (let loop ((len 2) (tail (cdddr form)))
      (cond ((and (pair? tail) (eq? (car tail) '...))
             (loop (+ len 1) (cdr tail)))
            (else
             (values (list-head form len) tail len))))))

(define check-template
  (lambda (tmpl ranks)

    (define control-patvar-exists?
      (lambda (tmpl depth)
        (let loop ((lst tmpl) (depth depth))
          (cond ((symbol? lst)
                 (>= (rank-of lst ranks) depth))
                ((ellipsis-quote? lst)
                 (any1 (lambda (id) (>= (rank-of id ranks) depth)) (collect-unique-ids lst)))
                ((ellipsis-splicing-pair? lst)
                 (let-values (((body tail len) (parse-ellipsis-splicing lst)))
                   (or (loop body (+ depth 1))
                       (and (loop body 1)
                            (loop tail depth)))))
                ((ellipsis-pair? lst)
                 (or (loop (car lst) (+ depth 1))
                     (and (loop (car lst) 1)
                          (loop (cddr lst) depth))))
                ((pair? lst)
                 (or (loop (car lst) depth)
                     (loop (cdr lst) depth)))
                ((vector? lst)
                 (loop (vector->list lst) depth))
                (else #f)))))

    (define check-escaped
      (lambda (lst depth)
        (let loop ((lst lst))
          (cond ((symbol? lst)
                 (and (< 0 (rank-of lst ranks) depth)
                      (syntax-violation "syntax template" "too few ellipsis following subtemplate" tmpl lst)))
                ((pair? lst)
                 (loop (car lst))
                 (loop (cdr lst)))
                ((vector? lst)
                 (loop (vector->list lst)))))))

    (if (and (= (safe-length tmpl) 2) (eq? (car tmpl) '...))
        (check-escaped (cadr tmpl) 0)
        (let loop ((lst tmpl) (depth 0))
          (cond ((symbol? lst)
                 (and (eq? lst '...)
                      (syntax-violation "syntax template" "misplaced ellipsis" tmpl))
                 (and (> (rank-of lst ranks) depth)
                      (syntax-violation "syntax template" "too few ellipsis following subtemplate" tmpl lst)))
                ((ellipsis-quote? lst)
                 (check-escaped (cadr lst) depth))
                ((ellipsis-splicing-pair? lst)
                 (let-values (((body tail len) (parse-ellipsis-splicing lst)))
                   (and (= depth 0)
                        (or (control-patvar-exists? (car lst) len)
                            (syntax-violation "syntax template" "missing pattern variable that used in same level as in pattern" tmpl lst)))
                   (loop body (+ depth 1))
                   (loop tail depth)))
                ((ellipsis-pair? lst)
                 (cond ((symbol? (car lst))
                        (let ((rank (rank-of (car lst) ranks)))
                          (cond ((< rank 0)
                                 (syntax-violation "syntax template" "misplace ellipsis following literal" tmpl (car lst)))
                                ((> rank (+ depth 1))
                                 (syntax-violation "syntax template" "too few ellipsis following subtemplate" tmpl (car lst)))
                                (else
                                 (loop (cddr lst) depth)))))
                       ((pair? (car lst))
                        (and (= depth 0)
                             (or (control-patvar-exists? (car lst) (+ depth 1))
                                 (syntax-violation "syntax template" "missing pattern variable that used in same level as in pattern" tmpl (car lst))))
                        (loop (car lst) (+ depth 1))
                        (loop (cddr lst) depth))
                       ((null? (car lst))
                        (syntax-violation "syntax template" "misplaced ellipsis following empty list" tmpl))
                       (else
                        (syntax-violation "syntax template" "misplaced ellipsis following literal" tmpl (car lst)))))
                ((pair? lst)
                 (loop (car lst) depth)
                 (loop (cdr lst) depth))
                ((vector? lst)
                 (loop (vector->list lst) depth)))))))

(define rank-of
  (lambda (name ranks)
    (let ((slot (assq name ranks)))
      (if slot (cdr slot) -1))))

(define subform-of
  (lambda (name vars)
    (cdr (assq name vars))))

(define collect-ellipsis-vars
  (lambda (tmpl ranks depth vars)
    (let ((ids (collect-unique-ids tmpl)))
      (filter values
              (map (lambda (slot)
                     (and (memq (car slot) ids)
                          (let ((rank (cdr (assq (car slot) ranks))))
                            (cond ((< rank depth) slot)
                                  ((null? (cdr slot)) slot)
                                  (else (cons (car slot) (cadr slot)))))))
                   vars)))))

(define consume-ellipsis-vars
  (lambda (ranks depth vars)
    (let ((exhausted #f) (consumed #f))
      ;; consumed exhausted return
      ;; #t       #t    --> #f        error, different size of matched subform
      ;; #t       #f    --> remains   more variable to reveal
      ;; #f       #t    --> #t        all variable revealed
      ;; #f       #f    --> ()        no variable revealed
      (let ((remains
             (let loop ((lst vars))
               (cond ((null? lst) lst)
                     ((< (rank-of (caar lst) ranks) depth)
                      (cons (car lst) (loop (cdr lst))))
                     ((null? (cdar lst))
                      (loop (cdr lst)))
                     ((null? (cddar lst))
                      (set! exhausted #t)
                      (loop (cdr lst)))
                     (else
                      (or (circular-list? (cdar lst)) (set! consumed #t))
                      (acons (caar lst) (cddar lst) (loop (cdr lst))))))))
        (if consumed
            (and (not exhausted) remains)
            (or exhausted '()))))))

(define contain-rank-moved-var?
  (lambda (tmpl ranks vars)

    (define traverse-escaped
      (lambda (lst depth)
        (let loop ((lst lst) (depth depth))
          (cond ((symbol? lst)
                 (< 0 (rank-of lst ranks) depth))
                ((pair? lst)
                 (or (loop (car lst) depth)
                     (loop (cdr lst) depth)))
                ((vector? lst)
                 (loop (vector->list lst) depth))
                (else #f)))))

    (let loop ((lst tmpl) (depth 0))
      (cond ((symbol? lst)
             (< 0 (rank-of lst ranks) depth))
            ((ellipsis-quote? lst)
             (traverse-escaped (cadr lst) depth))
            ((ellipsis-splicing-pair? lst)
             (let-values (((body tail len) (parse-ellipsis-splicing lst)))
               (or (loop body (+ depth 1))
                   (loop tail depth))))
            ((ellipsis-pair? lst)
             (or (loop (car lst) (+ depth 1))
                 (loop (cddr lst) depth)))
            ((pair? lst)
             (or (loop (car lst) depth)
                 (loop (cdr lst) depth)))
            ((vector? lst)
             (loop (vector->list lst) depth))
            (else #f)))))

(define adapt-to-rank-moved-vars
  (lambda (form ranks vars)

    (define rewrite-template-ranks-vars
      (lambda (tmpl ranks vars)
        (let ((moved-ranks (make-core-hashtable)) (moved-vars (make-core-hashtable)))

          (define make-infinite-list
            (lambda (e)
              (let ((lst (list e)))
                (begin (set-cdr! lst lst) lst))))

          (define revealed
            (lambda (name depth)
              (if (< 0 (rank-of name ranks) depth)
                  (let ((renamed (string->symbol (format "~a:~a" (generate-temporary-symbol) name))))
                    (or (core-hashtable-ref moved-ranks renamed #f)
                        (let loop ((i (- depth (rank-of name ranks))) (var (subform-of name vars)))
                          (cond ((> i 0)
                                 (loop (- i 1) (list (make-infinite-list (car var)))))
                                (else
                                 (core-hashtable-set! moved-ranks renamed depth)
                                 (core-hashtable-set! moved-vars renamed var)))))
                    renamed)
                  name)))

          (define traverse-escaped
            (lambda (lst depth)
              (let loop ((lst lst) (depth depth))
                (cond ((symbol? lst)
                       (revealed lst depth))
                      ((pair? lst)
                       (cons (loop (car lst) depth)
                             (loop (cdr lst) depth)))
                      ((vector? lst)
                       (list->vector (loop (vector->list lst) depth)))
                      (else lst)))))

          (let ((rewrited
                 (let loop ((lst tmpl) (depth 0))
                   (cond ((symbol? lst)
                          (revealed lst depth))
                         ((ellipsis-quote? lst)
                          (cons (car lst)
                                (traverse-escaped (cdr lst) depth)))
                         ((ellipsis-splicing-pair? lst)
                          (let-values (((body tail len) (parse-ellipsis-splicing lst)))
                            (append (loop body (+ depth 1)) (cons '... (loop tail depth)))))
                         ((ellipsis-pair? lst)
                          (cons (loop (car lst) (+ depth 1))
                                (cons '... (loop (cddr lst) depth))))
                         ((pair? lst)
                          (cons (loop (car lst) depth)
                                (loop (cdr lst) depth)))
                         ((vector? lst)
                          (list->vector (loop (vector->list lst) depth)))
                         (else lst)))))
            (values rewrited
                    (append ranks (core-hashtable->alist moved-ranks))
                    (append vars (core-hashtable->alist moved-vars)))))))

    (if (contain-rank-moved-var? form ranks vars)
        (rewrite-template-ranks-vars form ranks vars)
        (values form ranks vars))))

(define transcribe-template
  (lambda (in-form in-ranks in-vars aliases emit)

    (define remove-duplicates
      (lambda (alist)
        (or (let loop ((lst alist))
              (cond ((null? lst) alist)
                    ((assq (caar lst) (cdr lst)) #f)
                    (else (loop (cdr lst)))))
            (let loop ((lst alist) (acc '()))
              (cond ((null? lst) acc)
                    ((assq (caar lst) acc)
                     (loop (cdr lst) acc))
                    (else
                     (loop (cdr lst) (cons (car lst) acc))))))))

    (let-values (((tmpl ranks vars) (adapt-to-rank-moved-vars in-form in-ranks (remove-duplicates in-vars))))

      (define expand-var
        (lambda (tmpl vars)
          (cond ((assq tmpl vars)
                 => (lambda (slot)
                      (cond ((null? (cdr slot)) '())
                            (emit (emit (cadr slot)))
                            (else (cadr slot)))))
                (else
                 (assertion-violation "syntax template" "subforms have different size of matched input"
                                      `(template: ,in-form) `(subforms: ,@in-vars))))))

      (define expand-ellipsis-var
        (lambda (tmpl vars)
          (cond ((assq tmpl vars)
                 => (lambda (slot)
                      (cond ((null? (cdr slot)) '())
                            (emit (map emit (cadr slot)))
                            (else (cadr slot)))))
                (else
                 (assertion-violation "syntax template" "subforms have different size of matched input"
                                      `(template: ,in-form) `(subforms: ,@in-vars))))))

      (define expand-ellipsis-template
        (lambda (tmpl depth vars)
          (let loop ((expr '()) (remains (collect-ellipsis-vars tmpl ranks depth vars)))
            (cond ((pair? remains)
                   (loop (cons (expand-template tmpl depth remains) expr)
                         (consume-ellipsis-vars ranks depth remains)))
                  ((null? remains) '())
                  ((eq? remains #t) (reverse expr))
                  (else
                   (assertion-violation "syntax template" "subforms have different size of matched input"
                                        `(template: ,in-form) `(subforms: ,@in-vars)))))))

      (define expand-escaped-template
        (lambda (tmpl depth vars)
          (cond ((symbol? tmpl)
                 (if (< (rank-of tmpl ranks) 0)
                     (cond ((assq tmpl aliases) => cdr) (else tmpl))
                     (expand-var tmpl vars)))
                ((pair? tmpl)
                 (if (and emit (null? (car tmpl)))
                     (cons '.&NIL
                           (expand-escaped-template (cdr tmpl) depth vars))
                     (cons (expand-escaped-template (car tmpl) depth vars)
                           (expand-escaped-template (cdr tmpl) depth vars))))
                ((vector? tmpl)
                 (list->vector (expand-escaped-template (vector->list tmpl) depth vars)))
                (else tmpl))))

      (define expand-template
        (lambda (tmpl depth vars)
          (cond ((symbol? tmpl)
                 (if (< (rank-of tmpl ranks) 0)
                     (cond ((assq tmpl aliases) => cdr) (else tmpl))
                     (expand-var tmpl vars)))
                ((ellipsis-quote? tmpl)
                 (expand-escaped-template (cadr tmpl) depth vars))
                ((ellipsis-splicing-pair? tmpl)
                 (let-values (((body tail len) (parse-ellipsis-splicing tmpl)))
                   (append (apply append (expand-ellipsis-template body (+ depth 1) vars))
                           (expand-template tail depth vars))))
                ((ellipsis-pair? tmpl)
                 (cond ((symbol? (car tmpl))
                        (let ((rank (rank-of (car tmpl) ranks)))
                          (cond ((= rank (+ depth 1))
                                 (append (expand-ellipsis-var (car tmpl) vars)
                                         (expand-template (cddr tmpl) depth vars))))))
                       ((pair? (car tmpl))
                        (append (expand-ellipsis-template (car tmpl) (+ depth 1) vars)
                                (expand-template (cddr tmpl) depth vars)))))
                ((pair? tmpl)
                 (if (and emit (null? (car tmpl)))
                     (cons '.&NIL
                           (expand-template (cdr tmpl) depth vars))
                     (cons (expand-template (car tmpl) depth vars)
                           (expand-template (cdr tmpl) depth vars))))
                ((vector? tmpl)
                 (list->vector (expand-template (vector->list tmpl) depth vars)))
                (else tmpl))))

      (if (and (= (safe-length tmpl) 2) (eq? (car tmpl) '...))
          (expand-escaped-template (cadr tmpl) 0 vars)
          (expand-template tmpl 0 vars)))))
