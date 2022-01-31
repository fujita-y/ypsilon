(define-library (srfi 64 test-runner-simple)
  (import
   (scheme base)
   (scheme file)
   (scheme write)
   (srfi 48)
   (srfi 64 test-runner))
  (include-library-declarations "test-runner-simple.exports.sld")
  (include "test-runner-simple.body.scm"))
