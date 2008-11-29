#!nobacktrace
(library (time)
  (export decode-microsecond
          encode-microsecond
          microsecond
          microsecond->string
          microsecond->utc
          time
          time-usage
          usleep)
  (import (ypsilon time)))
