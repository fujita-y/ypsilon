#!nobacktrace
(define-library (scheme file)
  (import (except (core primitives) current-input-port current-output-port current-error-port)
          (except (core io) with-input-from-file with-output-to-file current-input-port current-output-port current-error-port)
          (only (scheme base) current-input-port current-output-port current-error-port))
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

    (define open-binary-output-file open-file-output-port)

    (define with-input-from-file
      (lambda (filename thunk)
        (let ((port (open-input-file filename)) (save (current-input-port)))
          (dynamic-wind
            (lambda () (current-input-port port))
            (lambda () (let ((ans (thunk))) (close-input-port port) ans))
            (lambda () (current-input-port save))))))

    (define with-output-to-file
      (lambda (filename thunk)
        (let ((port (open-output-file filename)) (save (current-output-port)))
          (dynamic-wind
            (lambda () (current-output-port port))
            (lambda () (let ((ans (thunk))) (close-output-port port) ans))
            (lambda () (current-output-port save))))))))
