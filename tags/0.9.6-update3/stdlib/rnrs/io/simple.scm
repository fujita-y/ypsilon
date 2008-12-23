(library (rnrs io simple (6))
  (export &i/o make-i/o-error i/o-error?
          &i/o-read make-i/o-read-error i/o-read-error?
          &i/o-write make-i/o-write-error i/o-write-error?
          &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
          &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
          &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
          &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
          &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
          &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
          &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port
          &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
          &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char
          current-input-port
          current-output-port
          current-error-port
          eof-object
          eof-object?
          input-port?
          output-port?
          call-with-input-file
          call-with-output-file
          with-input-from-file
          with-output-to-file
          open-input-file
          open-output-file
          close-input-port
          close-output-port
          read-char
          peek-char
          read
          write-char
          newline
          display
          write)
  (import (core io)))
