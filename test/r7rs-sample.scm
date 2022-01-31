(import (scheme base) (scheme write))

(define (foo lst1 lst2) (map cons lst1 lst2))
(display (foo '(1 2 3) '(a b c)))
(display #\newline)
;=> ((1 . a) (2 . b) (3 . c))

(define (bar lst1 lst2) (map / lst1 lst2))
(display (bar '(1 2 3) '(3 5 7)))
(display #\newline)
;=> (1/3 2/5 3/7)
