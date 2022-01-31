#!nobacktrace
(define-library (srfi 38)
  (import (core))
  (export write-with-shared-structure
          read-with-shared-structure
          (rename (write-with-shared-structure write/ss)
                  (read-with-shared-structure read/ss))))
