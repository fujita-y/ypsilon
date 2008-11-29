#!nobacktrace
(library (srfi srfi-98)
  (export (rename (lookup-process-environment get-environment-variable)
                  (process-environment->alist get-environment-variables)))
  (import (core)))
