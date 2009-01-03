;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

;; Reference:
;;   R. Kent Dybvig / The Scheme Programming Language, Third Edition
;;   Chapter 5. Control Operations, Section 5.6. Continuations

(define dynamic-wind
  (lambda (in body out)
    (in)
    (current-dynamic-wind-record (cons (cons in out) (current-dynamic-wind-record)))
    (call-with-values
        body
        (lambda ans
          (current-dynamic-wind-record (cdr (current-dynamic-wind-record)))
          (out)
          (apply values ans)))))

(define perform-dynamic-wind
  (lambda (new cont args)

    (define common-tail
      (lambda (x y)
        (let ((nx (length x)) (ny (length y)))
          (do ((x (if (> nx ny) (list-tail x (- nx ny)) x) (cdr x))
               (y (if (> ny nx) (list-tail y (- ny nx)) y) (cdr y)))
              ((eq? x y) x)))))

    (let ((tail (common-tail new (current-dynamic-wind-record))))
      (let loop ((rec (current-dynamic-wind-record)))
        (cond ((not (eq? rec tail))
               (current-dynamic-wind-record (cdr rec))
               ((cdar rec))
               (loop (cdr rec)))))
      (let loop ((rec new))
        (cond ((not (eq? rec tail))
               (loop (cdr rec))
               ((caar rec))
               (current-dynamic-wind-record rec)))))
    (apply cont args)))
