#!nobacktrace
(library (trace)
  (export trace
          trace-global-count-mode
          trace-global-indent-mode
          trace-line-length
          trace-output-port
          untrace
          with-trace)
  (import (ypsilon trace)))
