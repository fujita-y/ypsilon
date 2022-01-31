;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

;; subr forward references
(begin
  (set-top-level-value! '.list? list?)
  (set-top-level-value! '.null? null?)
  (set-top-level-value! '.pair? pair?)
  (set-top-level-value! '.car car)
  (set-top-level-value! '.cdr cdr)
  (set-top-level-value! '.caar caar)
  (set-top-level-value! '.cadr cadr)
  (set-top-level-value! '.cdar cdar)
  (set-top-level-value! '.cddr cddr)
  (set-top-level-value! '.caaar caaar)
  (set-top-level-value! '.caadr caadr)
  (set-top-level-value! '.cadar cadar)
  (set-top-level-value! '.caddr caddr)
  (set-top-level-value! '.cdaar cdaar)
  (set-top-level-value! '.cdadr cdadr)
  (set-top-level-value! '.cddar cddar)
  (set-top-level-value! '.cdddr cdddr)
  (set-top-level-value! '.cdddar cdddar)
  (set-top-level-value! '.caddar caddar)
  (set-top-level-value! '.cddadr cddadr)
  (set-top-level-value! '.cadadr cadadr)
  (set-top-level-value! '.caaadr caaadr)
  (set-top-level-value! '.cddddr cddddr)
  (set-top-level-value! '.cadddr cadddr)
  (set-top-level-value! '.cdaadr cdaadr)
  (set-top-level-value! '.cdaddr cdaddr)
  (set-top-level-value! '.caaddr caaddr)
  (set-top-level-value! '.list list)
  (set-top-level-value! '.cons* cons*)
  (set-top-level-value! '.memq memq)
  (set-top-level-value! '.append append)
  (set-top-level-value! '.apply apply))

;; procedures used in destruction-match generating code
(begin
  (define drop-last-cdr
    (lambda (lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst))
               (cond ((pair? lst) (cons (car lst) (loop (cdr lst))))
                     (else '())))))))
  (define drop-last-pair
    (lambda (lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst))
               (cond ((pair? (cdr lst)) (cons (car lst) (loop (cdr lst))))
                     (else '())))))))
  (define last-pair
    (lambda (lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst))
               (cond ((pair? (cdr lst)) (loop (cdr lst)))
                     (else lst)))))))
  (define last-cdr
    (lambda (lst)
      (cond ((pair? lst)
             (let loop ((lst lst))
               (cond ((pair? (cdr lst)) (loop (cdr lst)))
                     (else (cdr lst)))))
            (else lst))))
  (define count-pair
    (lambda (lst)
      (let loop ((lst lst) (n 0))
        (cond ((pair? lst) (loop (cdr lst) (+ n 1)))
              (else n)))))
  (define last-n-pair
    (lambda (n lst)
      (let ((m (count-pair lst)))
        (cond ((< m n) '())
              (else (list-tail lst (- m n)))))))
  (define drop-last-n-pair
    (lambda (n lst)
      (cond ((null? lst) '())
            (else
             (let loop ((lst lst) (m (- (count-pair lst) n)))
               (cond ((<= m 0) '())
                     ((pair? (cdr lst)) (cons (car lst) (loop (cdr lst) (- m 1))))
                     (else '()))))))))
