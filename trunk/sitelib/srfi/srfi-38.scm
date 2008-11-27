#!nobacktrace
(library (srfi srfi-38)
  (export write-with-shared-structure
          read-with-shared-structure
          (rename (write-with-shared-structure write/ss)
                  (read-with-shared-structure read/ss)))
  (import (core primitives)))
