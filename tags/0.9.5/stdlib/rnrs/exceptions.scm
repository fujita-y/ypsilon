
(library (rnrs exceptions (6))
  (export with-exception-handler guard raise raise-continuable)
  (import (core exceptions)))