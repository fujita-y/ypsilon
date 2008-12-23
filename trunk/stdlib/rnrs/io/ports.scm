(library (rnrs io ports (6))
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
          file-options
          buffer-mode
          buffer-mode?
          utf-8-codec
          utf-16-codec
          latin-1-codec
          eol-style
          error-handling-mode
          make-transcoder
          transcoder-codec
          transcoder-eol-style
          transcoder-error-handling-mode
          native-transcoder
          native-eol-style
          bytevector->string
          string->bytevector
          eof-object
          eof-object?
          port?
          port-transcoder
          textual-port?
          binary-port?
          transcoded-port
          port-has-port-position?
          port-position
          port-has-set-port-position!?
          set-port-position!
          close-port
          call-with-port
          input-port?
          port-eof?
          open-file-input-port
          open-bytevector-input-port
          open-string-input-port
          standard-input-port
          current-input-port
          get-u8
          lookahead-u8
          get-bytevector-n
          get-bytevector-n!
          get-bytevector-some
          get-bytevector-all
          get-char
          lookahead-char
          get-string-n
          get-string-n!
          get-string-all
          get-line
          get-datum
          output-port?
          flush-output-port
          output-port-buffer-mode
          open-file-output-port
          open-bytevector-output-port
          call-with-bytevector-output-port
          open-string-output-port
          call-with-string-output-port
          standard-output-port
          standard-error-port
          current-output-port
          current-error-port
          put-u8
          put-bytevector
          put-char
          put-string
          put-datum
          open-file-input/output-port
          make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-custom-textual-input/output-port)
  (import (core io)))
