#!nobacktrace
(define-library (srfi 98)
  (import (core))
  (export (rename (lookup-process-environment get-environment-variable)
                  (process-environment->alist get-environment-variables))))
