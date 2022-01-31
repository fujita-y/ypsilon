(define-library (srfi 64 test-runner)
  (import
   (scheme base)
   (scheme case-lambda)
   (srfi 1))
  (include-library-declarations "test-runner.exports.sld")
  (include "test-runner.body.scm"))
