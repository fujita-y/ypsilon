#!nobacktrace
;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon process)
  (export process process-spawn process-wait process-shell-command system)
  (import (core))

  (define process
    (lambda args
      (destructuring-bind
          (pid stdin stdout stderr)
          (apply process-spawn #t #f #f #f #f args)
        (apply
          values
          pid
          (map (lambda (port)
                 (and port (transcoded-port port (native-transcoder))))
               (list stdin stdout stderr))))))

  (define process-shell-command
    (lambda (command)
      (process (or (getenv "SHELL") "/bin/sh") "-c" command))))
