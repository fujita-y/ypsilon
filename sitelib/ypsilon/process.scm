#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon process)
  (export process
          process-spawn
          process-shell-command
          process-wait
          call-with-process-ports
          system)

  (import (core))

  (define call-with-process-ports
    (lambda (pinfo proc)
      (destructuring-bind (pid in out err) pinfo
        (call-with-values
          (lambda ()
            (proc (and in (transcoded-port in (native-transcoder)))
                  (and out (transcoded-port out (native-transcoder)))
                  (and err (transcoded-port err (native-transcoder)))))
          (lambda args
            (let ((status (process-wait pid #f)))
              (cond ((= status 0))
                    ((> status 0)
                     (error 'call-with-process-ports (format "child process exit with status ~a" status) (list pinfo proc)))
                    (else
                     (error 'call-with-process-ports (format "child process terminated by signal ~a" (- status)) (list pinfo proc)))))
            (apply values args))))))

  ) ;[end]
