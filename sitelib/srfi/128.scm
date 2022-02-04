(define-library (srfi 128)
  (export comparator? comparator-ordered? comparator-hashable?
          make-comparator
          make-pair-comparator make-list-comparator make-vector-comparator
          make-eq-comparator make-eqv-comparator make-equal-comparator
          boolean-hash char-hash char-ci-hash
          string-hash string-ci-hash symbol-hash number-hash
          make-default-comparator default-hash comparator-register-default!
          comparator-type-test-predicate comparator-equality-predicate
          comparator-ordering-predicate comparator-hash-function
          comparator-test-type comparator-check-type comparator-hash
          hash-bound hash-salt
          =? <? >? <=? >=?
          comparator-if<=>
          )
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme inexact)
          (scheme complex))

  (cond-expand ((library (srfi 126))
                (import (only (srfi 126) equal-hash)))
               ((library (rnrs hashtables))
                (import (only (rnrs hashtables) equal-hash)))
               ((library (r6rs hashtables))
                (import (only (r6rs hashtables) equal-hash)))
               ((library (srfi 69))
                (import (rename (only (srfi 69) hash-by-identity)
                                (hash-by-identity equal-hash))))
               (else
                ;; FIXME: This works well enough for the test program,
                ;; but you wouldn't want to use it in a real program.
                (begin (define (equal-hash x) 0))))

  (include "128/128.body1.scm")
  (include "128/128.body2.scm")
)