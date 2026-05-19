(library (foo)
  (export)
  (import (rnrs))
  (let-syntax
    ((define
       (lambda (stx)
         (syntax-case stx ()
           ((_ arg )
            #'(display arg))))))
    (define "foo text")))

(library (bar)
  (export hoge1 hoge2)
  (import (rnrs))
  (let-syntax ()
    (define hoge1 "hoge1 text")
    (define hoge2 "hoge2 text")))
