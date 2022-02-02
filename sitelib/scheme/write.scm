#!nobacktrace
(define-library (scheme write)
  (import (core primitives))
  (export display write-shared write write-simple)
  (begin
    (define write-shared write-with-shared-structure)
    (define write-simple write)))
