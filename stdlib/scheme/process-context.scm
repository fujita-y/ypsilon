
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define-library (scheme process-context)
  (import (rename (core primitives) (exit core:exit))
          (core optargs))
  (export command-line
          exit
          get-environment-variable
          get-environment-variables
          emergency-exit)
  (begin
    (define get-environment-variable lookup-process-environment)
    (define get-environment-variables process-environment->alist)
    (define emergency-exit core:exit)
    (define exit
      (lambda options
        (let-optionals options ((status 0))
          ((continuation-to-exit) status))))))
