#!nobacktrace
(define-library (srfi 6)
  (import (core))
  (export (rename (make-string-input-port open-input-string))
          (rename (make-string-output-port open-output-string))
          (rename (get-accumulated-string get-output-string))))
