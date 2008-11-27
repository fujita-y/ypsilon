#!nobacktrace
(library (srfi :98)
  (export (rename (lookup-process-environment get-environment-variable)
                  (process-environment->alist get-environment-variables)))
  (import (core primitives)))
