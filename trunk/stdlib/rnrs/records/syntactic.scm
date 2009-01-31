(library (rnrs records syntactic (6))
  (export define-record-type
          record-type-descriptor
          record-constructor-descriptor
          fields
          mutable
          immutable
          parent
          protocol
          sealed
          opaque
          nongenerative
          parent-rtd)
  (import (core records)))
