#!nobacktrace
(library (ffi)
  (export c-argument
          c-function
          c-function/errno
          c-function/win32-last-error
          load-shared-object
          on-darwin
          on-freebsd
          on-ia32
          on-linux
          on-posix
          on-windows
          on-x64)
  (import (ypsilon ffi)))
