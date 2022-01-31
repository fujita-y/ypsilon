(define-library (srfi 64 source-info)
  (import
   (scheme base)
   (srfi 64 test-runner))
  (export source-info set-source-info!)
  (include "source-info.body.scm"))
