;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library (scheme file)
  (import (core primitives) (core io))
  (export call-with-input-file
          delete-file
          open-binary-input-file
          open-input-file
          with-input-from-file
          call-with-output-file
          file-exists?
          open-binary-output-file
          open-output-file
          with-output-to-file)
  (begin
    (define open-binary-input-file open-file-input-port)
    (define open-binary-output-file open-file-output-port)))
