#!nobacktrace
(library (rnrs records inspection (6))
  (export record?
          record-rtd
          record-type-name
          record-type-parent
          record-type-uid
          record-type-generative?
          record-type-sealed?
          record-type-opaque?
          record-type-field-names
          record-field-mutable?)
  (import (core records)))
