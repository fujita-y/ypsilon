;; example of "The Scheme Programming Language Third Edition"

(import (core) (test-lite))

(test-begin "Test Examples of The Scheme Programming Language Third Edition")
(newline)

;(define (SECTION . x) (test-comment x))
(define (SECTION . x) #t)

(SECTION 2 1 "square")
(test-eval! (define square
             (lambda (n)
               (* n n))))
(test-equal (square 5)  25)
(test-equal (square -200)  40000)
(test-equal (square 0.5)  0.25)
(test-equal (square -1/2)  1/4)

(SECTION 2 1 "reciprocal")
(test-eval! (define reciprocal
             (lambda (n)
               (if (= n 0)
                   "oops!"
                   (/ 1 n)))))
(test-equal (reciprocal 10)  1/10)
(test-equal (reciprocal 1/10)  10)
(test-equal (reciprocal 0)  "oops!")
(test-equal (reciprocal (reciprocal 1/10))  1/10)

(SECTION 2 2 "simple expressions")
(test-equal (+ 1/2 1/2)  1)
(test-equal (- 1.5 1/2)  1.0)
(test-equal (* 3 1/2)  3/2)
(test-equal (/ 1.5 3/4)  2.0)
(test-equal (+ (+ 2 2) (+ 2 2))  8)
(test-equal (- 2 (* 4 1/3))  2/3)
(test-equal (* 2 (* 2 (* 2 (* 2 2))))  32)
(test-equal (/ (* 6/7 7/2) (- 4.5 1.5))  1.0)
(test-equal (car '(a b c)) a)
(test-equal (cdr '(a b c)) (b c))
(test-equal (cdr '(a)) ())
(test-equal (car (cdr '(a b c))) b)
(test-equal (cdr (cdr '(a b c))) (c))
(test-equal (car '((a b) (c d))) (a b))
(test-equal (cdr '((a b) (c d))) ((c d)))
(test-equal (cons 'a '()) (a))
(test-equal (cons 'a '(b c)) (a b c))
(test-equal (cons 'a (cons 'b (cons 'c '()))) (a b c))
(test-equal (cons '(a b) '(c d)) ((a b) c d))
(test-equal (car (cons 'a '(b c))) a)
(test-equal (cdr (cons 'a '(b c))) (b c))
(test-equal (cons (car '(a b c)) (cdr '(d e f))) (a e f))
(test-equal (cons (car '(a b c)) (cdr '(a b c))) (a b c))
(test-equal (cons 'a 'b) (a . b))
(test-equal (cdr '(a . b)) b)
(test-equal (cons 'a '(b . c)) (a b . c))
(test-equal '(a . (b . (c . ()))) (a b c))
(test-equal (list 'a 'b 'c) (a b c))
(test-equal (list 'a) (a))
(test-equal (list) ())

(SECTION 2 4 "variables and let expressions")
(test-equal (let ((x 2)) (+ x 3))  5)
(test-equal (let ((y 3)) (+ 2 y))  5)
(test-equal (let ((x 2) (y 3)) (+ x y))  5)
(test-equal (+ (* 4 4) (* 4 4))  32)
(test-equal (let ((a (* 4 4))) (+ a a))  32)
(test-equal (let ((list1 '(a b c)) (list2 '(d e f)))
              (cons (cons (car list1)
                          (car list2))
                    (cons (car (cdr list1))
                          (car (cdr list2)))))
            ((a . d) b . e))

(test-equal (let ((f +)) (f 2 3))  5)
(test-equal (let ((f +) (x 2)) (f x 3))  5)
(test-equal (let ((f +) (x 2) (y 3)) (f x y))  5)
(test-equal (let ((+ *)) (+ 2 3))  6)
(test-equal (+ 2 3)  5)
(test-equal (let ((a 4) (b -3))
              (let ((a-squared (* a a))
                    (b-squared (* b b)))
                (+ a-squared b-squared)))
            25)
(test-equal (let ((x 1)) (let ((x (+ x 1))) (+ x x)))  4)
(test-equal (let ((x 1)) (let ((new-x (+ x 1))) (+ new-x new-x)))  4)

(SECTION 2 5 "lambda expression")
(test-equal ((lambda (x) (+ x x)) (* 3 4))  24)
(test-equal (let ((double (lambda (x) (+ x x))))
              (list (double (* 3 4))
                    (double (/ 99 11))
                    (double (- 2 7))))
            (24 18 -10))
(test-equal (let ((double-cons (lambda (x) (cons x x))))
              (double-cons 'a))
            (a . a))
(test-equal (let ((double-any (lambda (f x) (f x x))))
              (list (double-any + 13)
                    (double-any cons 'a)))
            (26 (a . a)))
(test-equal (let ((x 'a))
              (let ((f (lambda (y) (list x y))))
                (f 'b)))
            (a b))
(test-equal (let ((f (let ((x 'sam))
                       (lambda (y z) (list x y z)))))
              (f 'i 'am))
            (sam i am))
(test-equal (let ((f (let ((x 'sam))
                       (lambda (y z) (list x y z)))))
              (let ((x 'not-sam))
                (f 'i 'am)))
            (sam i am))
(test-equal (let ((f (lambda x x)))
              (f 1 2 3 4))
            (1 2 3 4))
(test-equal
 (let ((f (lambda x x)))
   (f))
 ())
(test-equal
 (let ((g (lambda (x . y) (list x y))))
   (g 1 2 3 4))
 (1 (2 3 4)))
(test-equal
 (let ((h (lambda (x y . z) (list x y z))))
   (h 'a 'b 'c 'd))
 (a b (c d)))

(SECTION 2 6 "top-level definitions")

(test-eval! (define double-any
             (lambda (f x)
               (f x x))))

(test-equal (double-any + 10)  20)
(test-equal (double-any cons 'a) (a . a))

(test-eval! (define xyz '(x y z)))

(test-equal (let ((xyz '(z y x))) xyz) (z y x))

(test-eval! (begin (define doubler
                    (lambda (f)
                      (lambda (x) (f x x))))
                  (define double (doubler +))))

(test-equal (double 13/2)  13)

(test-eval! (define double-cons (doubler cons)))

(test-equal (double-cons 'a) (a . a))

(test-eval! (define double-any
             (lambda (f x)
               ((doubler f) x))))

(test-equal (double-any + 10)  20)
(test-equal (double-any cons 'a) (a . a))

(test-eval! (begin (define proc1
                    (lambda (x y)
                      (proc2 y x)))
                  (define proc2 cons)))

(test-equal (proc1 'a 'b) (b . a))

(SECTION 2 7 "conditional expressions")

(test-eval! (begin (define abs1
                    (lambda (n)
                      (if (>= n 0)
                          n
                          (- 0 n))))

                  (define abs2
                    (lambda (n)
                      (if (not (< n 0))
                          n
                          (- 0 n))))

                  (define abs3
                    (lambda (n)
                      (if (or (> n 0) (= n 0))
                          n
                          (- 0 n))))

                  (define abs4
                    (lambda (n)
                      (if (= n 0)
                          0
                          (if (< n 0)
                              (- 0 n)
                              n))))

                  (define abs5
                    (lambda (n)
                      ((if (>= n 0) + -)
                       0
                       n)))))

(test-equal (= 0 (abs1 0) (abs2 0) (abs3 0) (abs4 0) (abs5 0)) #t)
(test-equal (= 7 (abs1 7) (abs2 7) (abs3 7) (abs4 7) (abs5 7)) #t)
(test-equal (= 5 (abs1 -5) (abs2 -5) (abs3 -5) (abs4 -5) (abs5 -5)) #t)
(test-equal (< -1 0)  #t)
(test-equal (> -1 0)  #f)
(test-equal (if #t 'true 'false) true)
(test-equal (if #f 'true 'false) false)
(test-equal (if '() 'true 'false) true)
(test-equal (if 1 'true 'false) true)
(test-equal (if '(a b c) 'true 'false) true)

(test-equal (not #t)  #f)
(test-equal (not "false")  #f)
(test-equal (not #f)  #t)

(test-equal (or)  #f)
(test-equal (or #f)  #f)
(test-equal (or #f #t)  #t)
(test-equal (or #f 'a #f) a)

(test-eval! (define reciprocal
             (lambda (n)
               (and (not (= n 0))
                    (/ 1 n)))))

(test-equal (reciprocal 3)  1/3)
(test-equal (reciprocal 0.5)  2.0)
(test-equal (reciprocal 0)  #f)

(test-equal (null? '())  #t)
(test-equal (null? 'abc)  #f)
(test-equal (null? '(x y z))  #f)
(test-equal (null? (cdddr '(x y z)))  #t)

(test-eval! (define lisp-cdr
             (lambda (x)
               (if (null? x)
                   '()
                   (cdr x)))))

(test-equal (lisp-cdr '(a b c)) (b c))
(test-equal (lisp-cdr '(c)) ())
(test-equal (lisp-cdr '()) ())

(test-equal (eqv? 'a 'a)  #t)
(test-equal (eqv? 'a 'b)  #f)
(test-equal (eqv? #f #f)  #t)
(test-equal (eqv? #t #t)  #t)
(test-equal (eqv? #f #t)  #f)
(test-equal (eqv? 3 3)  #t)
(test-equal (eqv? 3 2)  #f)

(test-equal (let ((x "Hi Mom!")) (eqv? x x))  #t)
(test-equal (let ((x (cons 'a 'b))) (eqv? x x))  #t)
(test-equal (eqv? (cons 'a 'b) (cons 'a 'b))  #f)

(test-equal (pair? '(a . c))  #t)
(test-equal (pair? '(a b c))  #t)
(test-equal (pair? '())  #f)
(test-equal (pair? 'abc)  #f)
(test-equal (pair? "Hi Mom!")  #f)
(test-equal (pair? 1234567890)  #f)

(test-eval! (define reciprocal
             (lambda (n)
               (if (and (number? n) (not (= n 0)))
                   (/ 1 n)
                   "oops!"))))

(test-equal (reciprocal 2/3)  3/2)
(test-equal (reciprocal 'a)  "oops!")

(test-eval! (define sign
             (lambda (n)
               (if (< n 0)
                   -1
                   (if (> n 0)
                       +1
                       0)))))

(test-equal (sign -88.3)  -1)
(test-equal (sign 0)  0)
(test-equal (sign 333333333333)  1)
(test-equal (* (sign -88.3) (abs -88.3))  -88.3)

(test-eval! (define sign
             (lambda (n)
               (cond
                ((< n 0) -1)
                ((> n 0) +1)
                (else 0)))))

(test-equal (sign -88.3)  -1)
(test-equal (sign 0)  0)
(test-equal (sign 333333333333)  1)
(test-equal (* (sign -88.3) (abs -88.3))  -88.3)

(test-eval! (define sign
             (lambda (n)
               (cond
                ((< n 0) -1)
                ((> n 0) +1)
                ((= n 0) 0)))))

(test-equal (sign -88.3)  -1)
(test-equal (sign 0)  0)
(test-equal (sign 333333333333)  1)
(test-equal (* (sign -88.3) (abs -88.3))  -88.3)

(test-eval! (define income-tax
             (lambda (income)
               (cond
                ((<= income 10000) (* income .05))
                ((<= income 20000) (+ (* (- income 10000) .08) 500.00))
                ((<= income 30000) (+ (* (- income 20000) .13) 1300.00))
                (else (+ (* (- income 30000) .21) 2600.00))))))

(test-equal (income-tax 5000)  250.0)
(test-equal (income-tax 15000)  900.0)
(test-equal (income-tax 25000)  1950.0)
(test-equal (income-tax 50000)  6800.0)

(SECTION 2 8 "simple recursion")

(test-eval! (define proc-length
             (lambda (ls)
               (if (null? ls)
                   0
                   (+ (proc-length (cdr ls)) 1)))))

(test-equal (proc-length '())  0)
(test-equal (proc-length '(a))  1)
(test-equal (proc-length '(a b))  2)

(test-eval! (define proc-list-copy
             (lambda (ls)
               (if (null? ls)
                   '()
                   (cons (car ls)
                         (proc-list-copy (cdr ls)))))))

(test-equal (proc-list-copy '()) ())
(test-equal (proc-list-copy '(a b c)) (a b c))

(test-eval! (define proc-memv
             (lambda (x ls)
               (cond
                ((null? ls) #f)
                ((eqv? (car ls) x) ls)
                (else (proc-memv x (cdr ls)))))))

(test-equal (proc-memv 'a '(a b b d)) (a b b d))
(test-equal (proc-memv 'b '(a b b d)) (b b d))
(test-equal (proc-memv 'c '(a b b d))  #f)
(test-equal (proc-memv 'd '(a b b d)) (d))
(test-equal (if (proc-memv 'b '(a b b d)) "yes" "no")  "yes")

(test-eval! (define proc-remv
             (lambda (x ls)
               (cond
                ((null? ls) '())
                ((eqv? (car ls) x) (proc-remv x (cdr ls)))
                (else (cons (car ls) (proc-remv x (cdr ls))))))))

(test-equal (proc-remv 'a '(a b b d)) (b b d))
(test-equal (proc-remv 'b '(a b b d)) (a d))
(test-equal (proc-remv 'c '(a b b d)) (a b b d))
(test-equal (proc-remv 'd '(a b b d)) (a b b))

(test-eval! (define proc-tree-copy
             (lambda (tr)
               (if (not (pair? tr))
                   tr
                   (cons (proc-tree-copy (car tr))
                         (proc-tree-copy (cdr tr)))))))

(test-equal (proc-tree-copy '((a . b) . c)) ((a . b) . c))

(test-eval! (define abs-all
             (lambda (ls)
               (if (null? ls)
                   '()
                   (cons (abs (car ls))
                         (abs-all (cdr ls)))))))

(test-equal (abs-all '(1 -2 3 -4 5 -6)) (1 2 3 4 5 6))

(test-eval! (define abs-all
             (lambda (ls)
               (map abs ls))))

(test-equal (abs-all '(1 -2 3 -4 5 -6)) (1 2 3 4 5 6))
(test-equal (map abs '(1 -2 3 -4 5 -6)) (1 2 3 4 5 6))
(test-equal (map (lambda (x) (* x x)) '(1 -3 -5 7)) (1 9 25 49))
(test-equal (map cons '(a b c) '(1 2 3)) ((a . 1) (b . 2) (c . 3)))

(test-eval! (define proc-map1
             (lambda (p ls)
               (if (null? ls)
                   '()
                   (cons (p (car ls))
                         (proc-map1 p (cdr ls)))))))

(test-equal (proc-map1 abs '(1 -2 3 -4 5 -6)) (1 2 3 4 5 6))

(SECTION 2 9 "assignment")

(test-eval! (define abcde '(a b c d e)))

(test-equal abcde (a b c d e))

(test-eval! (set! abcde (cdr abcde)))

(test-equal abcde (b c d e))

(test-equal (let ((abcde '(a b c d e))) (set! abcde (reverse abcde)) abcde) (e d c b a))

(test-eval! (define quadratic-formula
             (lambda (a b c)
               (let ((root1 0) (root2 0) (minusb 0) (radical 0) (divisor 0))
                 (set! minusb (- 0 b))
                 (set! radical (sqrt (- (* b b) (* 4 (* a c)))))
                 (set! divisor (* 2 a))
                 (set! root1 (/ (+ minusb radical) divisor))
                 (set! root2 (/ (- minusb radical) divisor))
                 (cons root1 root2)))))

(test-equal (quadratic-formula 2 -4 -6) (3 . -1))

(test-eval! (define quadratic-formula
             (lambda (a b c)
               (let ((minusb (- 0 b))
                     (radical (sqrt (- (* b b) (* 4 (* a c)))))
                     (divisor (* 2 a)))
                 (let ((root1 (/ (+ minusb radical) divisor))
                       (root2 (/ (- minusb radical) divisor)))
                   (cons root1 root2))))))

(test-equal (quadratic-formula 2 -4 -6) (3 . -1))

(test-eval! (begin (define cons-count 0)
                  (define proc-cons
                    (let ((old-cons cons))
                      (lambda (x y)
                        (set! cons-count (+ cons-count 1))
                        (old-cons x y))))))

(test-equal (proc-cons 'a '(b c)) (a b c))
(test-equal cons-count  1)
(test-equal (proc-cons 'a (proc-cons 'b (proc-cons 'c '()))) (a b c))
(test-equal cons-count  4)

(test-eval! (begin (define next 0)
                  (define count
                    (lambda ()
                      (let ((v next))
                        (set! next (+ next 1))
                        v)))))

(test-equal (count)  0)
(test-equal (count)  1)

(test-eval! (define count
             (let ((next 0))
               (lambda ()
                 (let ((v next))
                   (set! next (+ next 1))
                   v)))))

(test-equal (count)  0)
(test-equal (count)  1)

(test-eval! (begin (define make-counter
                    (lambda ()
                      (let ((next 0))
                        (lambda ()
                          (let ((v next))
                            (set! next (+ next 1))
                            v)))))
                  (define count1 (make-counter))
                  (define count2 (make-counter))))

(test-equal (count1)  0)
(test-equal (count2)  0)
(test-equal (count1)  1)
(test-equal (count1)  2)
(test-equal (count2)  1)

(test-eval! (begin (define shhh #f)
                  (define tell #f)
                  (let ((secret 0))
                    (set! shhh
                          (lambda (message)
                            (set! secret message)))
                    (set! tell
                          (lambda ()
                            secret)))
                  (shhh "sally likes harry")))

(test-equal (tell)  "sally likes harry")

(test-eval! (begin (define make-stack
                    (lambda ()
                      (let ((ls '()))
                        (lambda (msg . args)
                          (cond
                           ((eqv? msg 'empty?) (null? ls))
                           ((eqv? msg 'push!)
                            (set! ls (cons (car args) ls)))
                           ((eqv? msg 'top) (car ls))
                           ((eqv? msg 'pop!)
                            (set! ls (cdr ls)))
                           (else "oops"))))))

                  (define stack1 (make-stack))
                  (define stack2 (make-stack))))

(test-equal (stack1 'empty?)  #t)
(test-equal (stack2 'empty?)  #t)

(test-eval! (stack1 'push! 'a))

(test-equal (stack1 'empty?)  #f)
(test-equal (stack2 'empty?)  #t)

(test-eval! (begin (stack1 'push! 'b)
                  (stack2 'push! 'c)))

(test-equal (stack1 'top) b)
(test-equal (stack2 'top) c)

(test-eval! (stack1 'pop!))

(test-equal (stack2 'empty?)  #f)
(test-equal (stack1 'top) a)

(test-eval! (stack2 'pop!))

(test-equal (stack2 'empty?)  #t)

(test-eval! (begin (define p (list 1 2 3))
                  (set-car! (cdr p) 'two)))

(test-equal p (1 two 3))

(test-eval! (set-cdr! p '()))

(test-equal p (1))

(test-eval! (begin (define make-queue
                    (lambda ()
                      (let ((end (cons 'ignored '())))
                        (cons end end))))

                  (define putq!
                    (lambda (q v)
                      (let ((end (cons 'ignored '())))
                        (set-car! (cdr q) v)
                        (set-cdr! (cdr q) end)
                        (set-cdr! q end))))

                  (define getq
                    (lambda (q)
                      (car (car q))))

                  (define delq!
                    (lambda (q)
                      (set-car! q (cdr (car q)))))

                  (define myq (make-queue))

                  (putq! myq 'a)
                  (putq! myq 'b)))

(test-equal (getq myq) a)

(test-eval! (delq! myq))

(test-equal (getq myq) b)

(test-eval! (begin (delq! myq)
                  (putq! myq 'c)
                  (putq! myq 'd)))

(test-equal (getq myq) c)

(test-eval! (delq! myq))

(test-equal (getq myq) d)

;;;
(SECTION 6 2 "eq?")
(test-equal (eq? 'a 3)  #f)
(test-equal (eq? #t 't)  #f)
(test-equal (eq? "abc" 'abc)  #f)
(test-equal (eq? "hi" '(hi))  #f)
(test-equal (eq? "()" '())  #f)
(test-equal (eq? 9/2 7/2)  #f)
(test-equal (eq? 3.4 53344)  #f)
(test-equal (eq? 3 3.0)  #f)
(test-equal (eq? 1/3 #i1/3)  #f)
(test-equal (eq? #\a #\b)  #f)
(test-equal (eq? #t #t)  #t)
(test-equal (eq? #f #f)  #t)
(test-equal (eq? #t #f)  #f)
(test-equal (eq? (null? '()) #t)  #t)
(test-equal (eq? (null? '(a)) #f)  #t)
(test-equal (eq? (cdr '(a)) '())  #t)
(test-equal (eq? 'a 'a)  #t)
(test-equal (eq? 'a 'b)  #f)
(test-equal (eq? 'a (string->symbol "a"))  #t)
(test-equal (eq? '(a) '(b))  #f)
(test-equal (let ((x '(a . b))) (eq? x x))  #t)
(test-equal (let ((x (cons 'a 'b))) (eq? x x))  #t)
(test-equal (eq? (cons 'a 'b) (cons 'a 'b))  #f)
(test-equal (eq? "abc" "cba")  #f)
(test-equal (let ((x "hi")) (eq? x x))  #t)
(test-equal (let ((x (string #\h #\i))) (eq? x x))  #t)
(test-equal (eq? (string #\h #\i) (string #\h #\i))  #f)
(test-equal (eq? '#(a) '#(b))  #f)
(test-equal (let ((x '#(a))) (eq? x x))  #t)
(test-equal (let ((x (vector 'a))) (eq? x x))  #t)
(test-equal (eq? (vector 'a) (vector 'a))  #f)
(test-equal (eq? car car)  #t)
(test-equal (eq? car cdr)  #f)
(test-equal (let ((f (lambda (x) x))) (eq? f f))  #t)
(test-equal (let ((f (lambda (x) (lambda () (set! x (+ x 1)) x)))) (eq? (f 0) (f 0)))  #f)

(SECTION 6 2 "eqv?")
(test-equal (eqv? 'a 3)  #f)
(test-equal (eqv? #t 't)  #f)
(test-equal (eqv? "abc" 'abc)  #f)
(test-equal (eqv? "hi" '(hi))  #f)
(test-equal (eqv? "()" '())  #f)
(test-equal (eqv? 9/2 7/2)  #f)
(test-equal (eqv? 3.4 53344)  #f)
(test-equal (eqv? 3 3.0)  #f)
(test-equal (eqv? 1/3 #i1/3)  #f)
(test-equal (eqv? 9/2 9/2)  #t)
(test-equal (eqv? 3.4 (+ 3.0 .4))  #t)
(test-equal (let ((x (* 12345678987654321 2))) (eqv? x x))  #t)
(test-equal (eqv? #\a #\b)  #f)
(test-equal (eqv? #\a #\a)  #t)
(test-equal (let ((x (string-ref "hi" 0))) (eqv? x x))  #t)
(test-equal (eqv? #t #t)  #t)
(test-equal (eqv? #f #f)  #t)
(test-equal (eqv? #t #f)  #f)
(test-equal (eqv? (null? '()) #t)  #t)
(test-equal (eqv? (null? '(a)) #f)  #t)
(test-equal (eqv? (cdr '(a)) '())  #t)
(test-equal (eqv? 'a 'a)  #t)
(test-equal (eqv? 'a 'b)  #f)
(test-equal (eqv? 'a (string->symbol "a"))  #t)
(test-equal (eqv? '(a) '(b))  #f)
(test-equal (let ((x '(a . b))) (eqv? x x))  #t)
(test-equal (let ((x (cons 'a 'b))) (eqv? x x))  #t)
(test-equal (eqv? (cons 'a 'b) (cons 'a 'b))  #f)
(test-equal (eqv? "abc" "cba")  #f)
(test-equal (let ((x "hi")) (eqv? x x))  #t)
(test-equal (let ((x (string #\h #\i))) (eqv? x x))  #t)
(test-equal (eqv? (string #\h #\i) (string #\h #\i))  #f)
(test-equal (eqv? '#(a) '#(b))  #f)
(test-equal (let ((x '#(a))) (eqv? x x))  #t)
(test-equal (let ((x (vector 'a))) (eqv? x x))  #t)
(test-equal (eqv? (vector 'a) (vector 'a))  #f)
(test-equal (eqv? car car)  #t)
(test-equal (eqv? car cdr)  #f)
(test-equal (let ((f (lambda (x) x))) (eqv? f f))  #t)
(test-equal (let ((f (lambda (x) (lambda () (set! x (+ x 1)) x)))) (eqv? (f 0) (f 0)))  #f)
                                        ;
(test-equal (eqv? 1 'a)  #f)
(test-equal (eqv? 1 #\i)  #f)
(test-equal (eqv? #\i 1)  #f)
(test-equal (eqv? #\i 'a)  #f)
(test-equal (eqv? 'a 1)  #f)

(SECTION 6 2 "equal?")
(test-equal (equal? 'a 3)  #f)
(test-equal (equal? #t 't)  #f)
(test-equal (equal? "abc" 'abc)  #f)
(test-equal (equal? "hi" '(hi))  #f)
(test-equal (equal? "()" '())  #f)
(test-equal (equal? 9/2 7/2)  #f)
(test-equal (equal? 3.4 53344)  #f)
(test-equal (equal? 3 3.0)  #f)
(test-equal (equal? 1/3 #i1/3)  #f)
(test-equal (equal? 9/2 9/2)  #t)
(test-equal (equal? 3.4 (+ 3.0 .4))  #t)
(test-equal (let ((x (* 12345678987654321 2))) (equal? x x))  #t)
(test-equal (equal? #\a #\b)  #f)
(test-equal (equal? #\a #\a)  #t)
(test-equal (let ((x (string-ref "hi" 0))) (equal? x x))  #t)
(test-equal (equal? #t #t)  #t)
(test-equal (equal? #f #f)  #t)
(test-equal (equal? #t #f)  #f)
(test-equal (equal? (null? '()) #t)  #t)
(test-equal (equal? (null? '(a)) #f)  #t)
(test-equal (equal? (cdr '(a)) '())  #t)
(test-equal (equal? 'a 'a)  #t)
(test-equal (equal? 'a 'b)  #f)
(test-equal (equal? 'a (string->symbol "a"))  #t)
(test-equal (equal? '(a) '(b))  #f)
(test-equal (equal? '(a) '(a))  #t)
(test-equal (let ((x '(a . b))) (equal? x x))  #t)
(test-equal (let ((x (cons 'a 'b))) (equal? x x))  #t)
(test-equal (equal? (cons 'a 'b) (cons 'a 'b))  #t)
(test-equal (equal? "abc" "cba")  #f)
(test-equal (equal? "abc" "abc")  #t)
(test-equal (let ((x "hi")) (equal? x x))  #t)
(test-equal (let ((x (string #\h #\i))) (equal? x x))  #t)
(test-equal (equal? (string #\h #\i) (string #\h #\i))  #t)
(test-equal (equal? '#(a) '#(b))  #f)
(test-equal (equal? '#(a) '#(a))  #t)
(test-equal (let ((x '#(a))) (equal? x x))  #t)
(test-equal (let ((x (vector 'a))) (equal? x x))  #t)
(test-equal (equal? (vector 'a) (vector 'a))  #t)
(test-equal (equal? car car)  #t)
(test-equal (equal? car cdr)  #f)
(test-equal (let ((f (lambda (x) x))) (equal? f f))  #t)
(test-equal (let ((f (lambda (x) (lambda () (set! x (+ x 1)) x)))) (equal? (f 0) (f 0)))  #f)
(test-equal (equal? '#(a b c) '#(a b c))  #t)
(test-equal (equal? '#(a b c) '#(a b c d))  #f)
(test-equal (equal? '#(a b c e) '#(a b c d))  #f)

(SECTION 6 2 "boolean?")
(test-equal (boolean? #t)  #t)
(test-equal (boolean? #f)  #t)
(test-equal (boolean? 't)  #f)
(test-equal (if (eq? #f '()) (boolean? '()) (not (boolean? '())))  #t)

(SECTION 6 2 "null?")
(test-equal (null? '())  #t)
(test-equal (null? '(a))  #f)
(test-equal (null? (cdr '(a)))  #t)
(test-equal (null? 3)  #f)
(test-equal (if (eq? #f '()) (null? #f) (not (null? #f)))  #t)

(SECTION 6 2 "pair?")
(test-equal (pair? '(a b c))  #t)
(test-equal (pair? '(3 . 4))  #t)
(test-equal (pair? '())  #f)
(test-equal (pair? '#(a b))  #f)
(test-equal (pair? 3)  #f)

(SECTION 6 2 "numeric predicates")
(test-equal (integer? 1901)  #t)
(test-equal (rational? 1901)  #t)
(test-equal (real? 1901)  #t)
(test-equal (complex? 1901)  #t)
(test-equal (number? 1901)  #t)
(test-equal (integer? -3.0)  #t)
(test-equal (rational? -3.0)  #t)
(test-equal (real? -3.0)  #t)
(test-equal (complex? -3.0)  #t)
(test-equal (number? -3.0)  #t)

; r5rs (test-equal (integer? 7.0+0.0i)  #t)
(test-equal (integer? 7.0+0.0i)         #f) ; r6rs
(test-equal (integer-valued? 7.0+0.0i)  #t) ; r6rs

; r5rs (test-equal (rational? 7.0+0.0i)  #t)
(test-equal (rational? 7.0+0.0i)         #f) ; r6rs
(test-equal (rational-valued? 7.0+0.0i)  #t) ; r6rs

; r5rs (test-equal (real? 7.0+0.0i)  #t)
(test-equal (real? 7.0+0.0i)  #f) ; r6rs
(test-equal (real-valued? 7.0+0.0i)  #t) ; r6rs

(test-equal (complex? 7.0+0.0i)  #t)
(test-equal (number? 7.0+0.0i)  #t)
(test-equal (integer? -2/3)  #f)
(test-equal (rational? -2/3)  #t)
(test-equal (real? -2/3)  #t)
(test-equal (complex? -2/3)  #t)
(test-equal (number? -2/3)  #t)
(test-equal (integer? -2.345)  #f)
(test-equal (rational? -2.345)  #t)
(test-equal (real? -2.345)  #t)
(test-equal (complex? -2.345)  #t)
(test-equal (number? -2.345)  #t)
(test-equal (integer? 3.2-2.01i)  #f)
(test-equal (rational? 3.2-2.01i)  #f)
(test-equal (real? 3.2-2.01i)  #f)
(test-equal (complex? 3.2-2.01i)  #t)
(test-equal (number? 3.2-2.01i)  #t)
(test-equal (integer? 'a)  #f)
(test-equal (rational? '(a b c))  #f)
(test-equal (real? "3")  #f)
(test-equal (complex? '#(1 2))  #f)
(test-equal (number? #\a)  #f)

(SECTION 6 2 "char?")
(test-equal (char? 'a)  #f)
(test-equal (char? 97)  #f)
(test-equal (char? #\a)  #t)
(test-equal (char? "a")  #f)
(test-equal (char? (string-ref (make-string 1) 0))  #t)

(SECTION 6 2 "string?")
(test-equal (string? "hi")  #t)
(test-equal (string? 'hi)  #f)
(test-equal (string? #\h)  #f)

(SECTION 6 2 "vector?")
(test-equal (vector? '#())  #t)
(test-equal (vector? '#(a b c))  #t)
(test-equal (vector? (vector 'a 'b 'c))  #t)
(test-equal (vector? '())  #f)
(test-equal (vector? '(a b c))  #f)
(test-equal (vector? "abc")  #f)

(SECTION 6 2 "symbol?")
(test-equal (symbol? 't)  #t)
(test-equal (symbol? "t")  #f)
(test-equal (symbol? '(t))  #f)
(test-equal (symbol? #\t)  #f)
(test-equal (symbol? 3)  #f)
(test-equal (symbol? #t)  #f)

(SECTION 6 2 "procedure?")
(test-equal (procedure? car)  #t)
(test-equal (procedure? 'car)  #f)
(test-equal (procedure? (lambda (x) x))  #t)
(test-equal (procedure? '(lambda (x) x))  #f)
(test-equal (call-with-current-continuation procedure?)  #t)

(SECTION 6 3 "cons")
(test-equal (cons 'a '()) (a))
(test-equal (cons 'a '(b c)) (a b c))
(test-equal (cons 3 4) (3 . 4))

(SECTION 6 3 "car")
(test-equal (car '(a)) a)
(test-equal (car '(a b c)) a)
(test-equal (car (cons 3 4)) 3)

(SECTION 6 3 "cdr")
(test-equal (cdr '(a)) ())
(test-equal (cdr '(a b c)) (b c))
(test-equal (cdr (cons 3 4)) 4 )

(SECTION 6 3 "set-car!")
(test-equal (let ((x '(a b c))) (set-car! x 1) x) (1 b c))

(SECTION 6 3 "set-cdr!")
(test-equal (let ((x '(a b c))) (set-cdr! x 1) x) (a . 1))

(SECTION 6 3 "cxr")
(test-equal (caar '((a))) a)
(test-equal (cadr '(a b c)) b)
(test-equal (cdddr '(a b c d)) (d))
(test-equal (cadadr '(a (b c))) c)

(SECTION 6 3 "list")
(test-equal (list) ())
(test-equal (list 1 2 3) (1 2 3))
(test-equal (list 3 2 1) (3 2 1))

(SECTION 6 3 "list?")
(test-equal (list? '())  #t)
(test-equal (list? '(a b c))  #t)
(test-equal (list? 'a)  #f)
(test-equal (list? '(3 . 4))  #f)
(test-equal (list? 3)  #f)
(test-equal (let ((x (list 'a 'b 'c))) (set-cdr! (cddr x) x) (list? x))  #f)

(SECTION 6 3 "length?")
(test-equal (length '())  0)
(test-equal (length '(a b c))  3)

(SECTION 6 3 "list-ref")
(test-equal (list-ref '(a b c) 0) a)
(test-equal (list-ref '(a b c) 1) b)
(test-equal (list-ref '(a b c) 2) c)

(SECTION 6 3 "list-tail")
(test-equal (list-tail '(a b c) 0) (a b c))
(test-equal (list-tail '(a b c) 2) (c))
(test-equal (list-tail '(a b c) 3) ())
(test-equal (list-tail '(a b c . d) 2) (c . d))
(test-equal (list-tail '(a b c . d) 3) d)
(test-equal (let ((x (list 1 2 3))) (eq? (list-tail x 2) (cddr x)))  #t)

(SECTION 6 3 "append")
(test-equal (append '(a b c) '()) (a b c))
(test-equal (append '() '(a b c)) (a b c))
(test-equal (append '(a b) '(c d)) (a b c d))
(test-equal (append '(a b) 'c) (a b . c))
(test-equal (let ((x (list 'b))) (eq? x (cdr (append '(a) x))))  #t)

(SECTION 6 3 "reverse")
(test-equal (reverse '()) ())
(test-equal (reverse '(a b c)) (c b a))

(SECTION 6 3 "memq memv member")
(test-equal (memq 'a '(b c a d e)) (a d e))
(test-equal (memq 'a '(b c d e g))  #f)
(test-equal (memq 'a '(b a c a d a)) (a c a d a))

(test-equal (memv 3.4 '(1.2 2.3 3.4 4.5)) (3.4 4.5))
(test-equal (memv 3.4 '(1.3 2.5 3.7 4.9))  #f)
(test-equal (let ((ls (list 'a 'b 'c))) (set-car! (memv 'b ls) 'z) ls) (a z c))

(test-equal (member '(b) '((a) (b) (c))) ((b) (c)))
(test-equal (member '(d) '((a) (b) (c)))  #f)
(test-equal (member "b" '("a" "b" "c")) ("b" "c"))


(SECTION 6 3 "assq assv assoc")
(test-equal (assq 'b '((a . 1) (b . 2))) (b . 2))
(test-equal (cdr (assq 'b '((a . 1) (b . 2)))) 2)
(test-equal (assq 'c '((a . 1) (b . 2)))  #f)

(test-equal (assv 2/3 '((1/3 . 1) (2/3 . 2))) (2/3 . 2))
(test-equal (assv 2/3 '((1/3 . a) (3/4 . b)))  #f)

(test-equal (assoc '(a) '(((a) . a) (-1 . b))) ((a) . a))
(test-equal (assoc '(a) '(((b) . b) (a . c)))  #f)

(test-equal (let ((alist '((2 . a) (3 . b)))) (set-cdr! (assv 3 alist) 'c) alist) ((2 . a) (3 . c)))

(SECTION 6 4 "exact?")
(test-equal (exact? 1)  #t)
(test-equal (exact? -15/16)  #t)
(test-equal (exact? 2.01)  #f)
(test-equal (exact? #i77)  #f)
(test-equal (exact? #i2/3)  #f)
(test-equal (exact? 1.0-2i)  #f)
;(test-equal (exact? -1#i)  #f)

(SECTION 6 4 "exact?")
(test-equal (inexact? -123)  #f)
(test-equal (inexact? #i123)  #t)
(test-equal (inexact? 1e23)  #t)
;(test-equal (inexact? 1###)  #t)
;(test-equal (inexact? 1#/2#)  #t)
;(test-equal (inexact? #e1#/2#)  #f)
(test-equal (inexact? +i)  #f)

(SECTION 6 4 "= < > <= >=")
(test-equal (= 7 7)  #t)
(test-equal (= 7 9)  #f)

(test-equal (< 2e3 3e2)  #f)
(test-equal (<= 1 2 3 3 4 5)  #t)
(test-equal (<= 1 2 3 4 5)  #t)

(test-equal (> 1 2 2 3 3 4)  #f)
(test-equal (>= 1 2 2 3 3 4)  #f)

(test-equal (= -1/2 -0.5)  #t)
(test-equal (= 2/3 .667)  #f)
(test-equal (= 7.2+0i 7.2)  #t)
(test-equal (= 7.2-3i 7)  #f)

(test-equal (< 1/2 2/3 3/4)  #t)
(test-equal (> 8 4.102 2/3 -5)  #t)

(test-equal (let ((x 0.218723452)) (< 0.210 x 0.220))  #t)

(test-equal (let ((i 1) (v (vector 'a 'b 'c))) (< -1 i (vector-length v)))  #t)

(test-equal (apply < '(1 2 3 4))  #t)
(test-equal (apply > '(4 3 3 2))  #f)

(SECTION 6 4 "+")
(test-equal (+)  0)
(test-equal (+ 1 2)  3)
(test-equal (+ 1/2 2/3)  7/6)
(test-equal (+ 3 4 5)  12)
(test-equal (+ 3.0 4)  7.0)
(test-equal (+ 3+4i 4+3i)  7+7i)
(test-equal (apply + '(1 2 3 4 5))  15)

(SECTION 6 4 "-")
(test-equal (- 3)  -3)
(test-equal (- -2/3)  2/3)
(test-equal (- 4 3.0)  1.0)
(test-equal (- 3.25+4.25i 1/4+1/4i)  3.0+4.0i)
(test-equal (- 4 3 2 1)  -2)

(SECTION 6 4 "*")
(test-equal (*)  1)
(test-equal (* 3.4)  3.4)
(test-equal (* 1 1/2)  1/2)
(test-equal (* 3 4 5.5)  66.0)
(test-equal (* 1+2i 3+4i)  -5+10i)
(test-equal (apply * '(1 2 3 4 5))  120)

(SECTION 6 4 "/")
(test-equal (/ -17)  -1/17)
(test-equal (/ 1/2)  2)
(test-equal (/ .5)  2.0)
(test-equal (/ 3 4)  3/4)
(test-equal (/ 3.0 4)  .75)
(test-equal (/ -5+10i 3+4i)  1+2i)
(test-equal (/ 60 5 4 3 2)  1/2)

(SECTION 6 4 "zero?")
(test-equal (zero? 0)  #t)
(test-equal (zero? 1)  #f)
(test-equal (zero? (- 3.0 3.0))  #t)
(test-equal (zero? (+ 1/2 1/2))  #f)
(test-equal (zero? 0+0i)  #t)
(test-equal (zero? 0.0-0.0i)  #t)

(SECTION 6 4 "positive?")
(test-equal (positive? 128)  #t)
(test-equal (positive? 0.0)  #f)
(test-equal (positive? 1.8e-15)  #t)
(test-equal (positive? -2/3)  #f)
(test-equal (positive? .001-0.0i)  #t)

(SECTION 6 4 "negative?")
(test-equal (negative? -65)  #t)
(test-equal (negative? 0)  #f)
(test-equal (negative? -0.0121)  #t)
(test-equal (negative? 15/16)  #f)
(test-equal (negative? -7.0+0.0i)  #t)

(SECTION 6 4 "even?")
(test-equal (even? 0)  #t)
(test-equal (even? 1)  #f)
(test-equal (even? 2.0)  #t)
(test-equal (even? -120762398465)  #f)
(test-equal (even? 2.0+0.0i)  #t)

(SECTION 6 4 "odd?")
(test-equal (odd? 0)  #f)
(test-equal (odd? 1)  #t)
(test-equal (odd? 2.0)  #f)
(test-equal (odd? -120762398465)  #t)
(test-equal (odd? 2.0+0.0i)  #f)

(SECTION 6 4 "quotient")
(test-equal (quotient 45 6)  7)
(test-equal (quotient 6.0 2.0)  3.0)
(test-equal (quotient 3.0 -2)  -1.0)

(SECTION 6 4 "reminder")
(test-equal (remainder 16 4)  0)
(test-equal (remainder 5 2)  1)
(test-equal (remainder -45.0 7)  -3.0)
(test-equal (remainder 10.0 -3.0)  1.0)
(test-equal (remainder -17 -9)  -8)

(SECTION 6 4 "modulo")
(test-equal (modulo 16 4)  0)
(test-equal (modulo 5 2)  1)
(test-equal (modulo -45.0 7)  4.0)
(test-equal (modulo 10.0 -3.0)  -2.0)
(test-equal (modulo -17 -9)  -8)

(SECTION 6 4 "truncate")
(test-equal (truncate 19)  19)
(test-equal (truncate 2/3)  0)
(test-equal (truncate -2/3)  0)
(test-equal (truncate 17.3)  17.0)
(test-equal (truncate -17/2)  -8)

(SECTION 6 4 "floor")
(test-equal (floor 19)  19)
(test-equal (floor 2/3)  0)
(test-equal (floor -2/3)  -1)
(test-equal (floor 17.3)  17.0)
(test-equal (floor -17/2)  -9)

(SECTION 6 4 "ceiling")
(test-equal (ceiling 19)  19)
(test-equal (ceiling 2/3)  1)
(test-equal (ceiling -2/3)  0)
(test-equal (ceiling 17.3)  18.0)
(test-equal (ceiling -17/2)  -8)

(SECTION 6 4 "round")
(test-equal (round 19)  19)
(test-equal (round 2/3)  1)
(test-equal (round -2/3)  -1)
(test-equal (round 17.3)  17.0)
(test-equal (round -17/2)  -8)
(test-equal (round 2.5)  2.0)
(test-equal (round 3.5)  4.0)

(SECTION 6 4 "abs")
(test-equal (abs 1)  1)
(test-equal (abs -3/4)  3/4)
(test-equal (abs 1.83)  1.83)
(test-equal (abs -0.093)  0.093)

(SECTION 6 4 "max")
(test-equal (max 4 -7 2 0 -6)  4)
(test-equal (max 1/2 3/4 4/5 5/6 6/7)  6/7)
(test-equal (max 1.5 1.3 -0.3 0.4 2.0 1.8)  2.0)
(test-equal (max 5 2.0)  5.0)
(test-equal (max -5 -2.0)  -2.0)
(test-equal (let ((ls '(7 3 5 2 9 8))) (apply max ls))  9)

(SECTION 6 4 "min")
(test-equal (min 4 -7 2 0 -6)  -7)
(test-equal (min 1/2 3/4 4/5 5/6 6/7)  1/2)
(test-equal (min 1.5 1.3 -0.3 0.4 2.0 1.8)  -0.3)
(test-equal (min 5 2.0)  2.0)
(test-equal (min -5 -2.0)  -5.0)
(test-equal (let ((ls '(7 3 5 2 9 8))) (apply min ls))  2)

(SECTION 6 4 "gcd")
(test-equal (gcd)  0)
(test-equal (gcd 34)  34)
(test-equal (gcd 33.0 15.0)  3.0)
(test-equal (gcd 70 -42 28)  14)

(SECTION 6 4 "lcm")
(test-equal (lcm)  1)
(test-equal (lcm 34)  34)
(test-equal (lcm 33.0 15.0)  165.0)
(test-equal (lcm 70 -42 28)  420)
(test-equal (lcm 17.0 0)  0.0)

(SECTION 6 4 "expt")
(test-equal (expt 2 10)  1024)
(test-equal (expt 2 -10)  1/1024)
(test-equal (expt 2 -10.0)  9.765625e-4)
(test-equal (expt -1/2 5)  -1/32)
(test-equal (expt 3.0 3)  27.0)
(test-equal (expt +i 2)  -1)

;;;;;

(SECTION 6 4 "exact->inexact")
(test-equal (inexact 3)  3.0)
(test-equal (inexact 3.0)  3.0)
(test-equal (inexact -1/4)  -.25)
(test-equal (inexact 3+4i)  3.0+4.0i)
(test-equal (inexact (expt 10 20))  1e20)

(SECTION 6 4 "inexact->exact")
(test-equal (exact 3.0)  3)
(test-equal (exact 3)  3)
(test-equal (exact -.25)  -1/4)
(test-equal (exact 3.0+4.0i)  3+4i)
(test-equal (exact 1e20)  100000000000000000000)

(SECTION 6 4 "rationalize")
(test-equal (rationalize 3/10 1/10)  1/3)
(test-equal (rationalize .3 1/10)  0.3333333333333333)
(test-equal (eqv? (rationalize .3 1/10) #i1/3)  #t)

(SECTION 6 4 "numerator")
(test-equal (numerator 9)  9)
(test-equal (numerator 9.0)  9.0)
(test-equal (numerator 2/3)  2)
(test-equal (numerator -9/4)  -9)
(test-equal (numerator -2.25)  -9.0)

(SECTION 6 4 "denominator")
(test-equal (denominator 9)  1)
(test-equal (denominator 9.0)  1.0)
(test-equal (denominator 2/3)  3)
(test-equal (denominator -9/4)  4)
(test-equal (denominator -2.25)  4.0)

(SECTION 6 4 "real-part")
(test-equal (real-part 3+4i)  3)
(test-equal (real-part -2.3+0.7i)  -2.3)
(test-equal (real-part -i)  0)
(test-equal (real-part 17.2)  17.2)
(test-equal (real-part -17/100)  -17/100)

(SECTION 6 4 "imag-part")
(test-equal (imag-part 3+4i)  4)
(test-equal (imag-part -2.3+0.7i)  0.7)
(test-equal (imag-part -i)  -1)
(test-equal (imag-part 17.2)  0.0)
(test-equal (imag-part -17/100)  0)

(SECTION 6 4 "make-rectangular")
(test-equal (make-rectangular -2 7)  -2+7i)
(test-equal (make-rectangular 2/3 -1/2)  2/3-1/2i)
(test-equal (make-rectangular 3.2 5.3)  3.2+5.3i)

(SECTION 6 4 "make-polar")
(test-equal (make-polar 2 0)  2)
(test-equal (make-polar 2.0 0.0)  2.0+0.0i)
;(test-equal (make-polar 1.0 (asin -1.0))  0.0-1.0i)
(test-equal (eqv? (make-polar 7.2 -0.588) 7.2@-0.588)  #t)

(SECTION 6 4 "angle")
(test-equal (angle 7.3@1.5708)  1.5708)
(test-equal (angle 5.2)  0.0)

(SECTION 6 4 "magnitude")
(test-equal (magnitude 1)  1)
(test-equal (magnitude -3/4)  3/4)
(test-equal (magnitude 1.83)  1.83)
(test-equal (magnitude -0.093)  0.093)
(test-equal (magnitude 3+4i)  5)
;(test-equal (magnitude 7.25@1.5708)  7.25)

(SECTION 6 4 "sqrt")
(test-equal (sqrt 16)  4)
(test-equal (sqrt 1/4)  1/2)
(test-equal (sqrt 4.84)  2.2)
(test-equal (sqrt -4.84)  0.0+2.2i)
(test-equal (sqrt 3+4i)  2+1i)
(test-equal (sqrt -3.0-4.0i)  1.0-2.0i)

(SECTION 6 4 "exp")
(test-equal (exp 0.0)  1.0)
(test-equal (exp 1.0)  2.718281828459045) ;(test-equal (exp 1.0)  2.7182818284590455)
(test-equal (exp -.5)  0.6065306597126334)

(SECTION 6 4 "log")
(test-equal (log 1.0)  0.0)
(test-equal (log (exp 1.0))  1.0)
(test-equal (/ (log 100) (log 10))  2.0)
;(test-equal (log (make-polar (exp 2.0) 1.0))  2.0+1.0i)

(SECTION 6 4 "string->number")
(test-equal (string->number "0")  0)
(test-equal (string->number "3.4e3")  3400.0)
(test-equal (string->number "#x#e-2e2")  -738)
(test-equal (string->number "#e-2e2" 16)  -738)
(test-equal (string->number "#i15/16")  0.9375)
(test-equal (string->number "10" 16)  16)

(SECTION 6 4 "number->string")
(test-equal (number->string 3.4)  "3.4")
(test-equal (number->string 1e2)  "100.0")
(test-equal (number->string 1e23)  "1e23")
(test-equal (number->string -7/2)  "-7/2")
(test-equal (number->string 220/9 16)  "dc/9" ) ;(test-equal (number->string 220/9 16)  "DC/9" )

;;;;;
(SECTION 5 5 "map for-each")
                                        ; book
(test-equal (map abs '(1 -2 3 -4 5 -6)) (1 2 3 4 5 6))
(test-equal (map (lambda (x y) (* x y)) '(1 2 3 4) '(8 7 6 5)) (8 14 18 20))
(test-equal (map cadr '((a b) (d e) (g h))) (b e h))
                                        ;r4rstest.scm
(test-equal (map + '(1 2 3) '(4 5 6)) (5 7 9))
(test-equal (map + '(1 2 3)) (1 2 3))
(test-equal (map * '(1 2 3)) (1 2 3))
(test-equal (map - '(1 2 3)) (-1 -2 -3))
(test-equal (let ((v (make-vector 5))) (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4)) v) #(0 1 4 9 16))


(test-end)
