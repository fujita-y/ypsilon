#!nobacktrace
(library (ffi)
  (export c-argument
          c-function
          c-function/errno
          c-function/lasterror
          load-shared-object
          on-darwin
          on-freebsd
          on-ia32
          on-linux
          on-openbsd
          on-posix
          on-windows
          on-x64)
  (import (ypsilon ffi)))
