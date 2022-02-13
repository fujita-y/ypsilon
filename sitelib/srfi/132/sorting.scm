(define-library (srfi 132 sorting)

  (export list-sorted?               vector-sorted?
          list-sort                  vector-sort
          list-stable-sort           vector-stable-sort
          list-sort!                 vector-sort!
          list-stable-sort!          vector-stable-sort!
          list-merge                 vector-merge
          list-merge!                vector-merge!
          list-delete-neighbor-dups  vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median         vector-find-median!
          vector-select!             vector-separate!
          )

  (import (except (scheme base) vector-copy vector-copy!)
          (rename (only (scheme base) vector-copy vector-copy! vector-fill!)
                  (vector-copy  r7rs-vector-copy)
                  (vector-copy! r7rs-vector-copy!)
                  (vector-fill! r7rs-vector-fill!))
          (scheme cxr)
          (only (srfi 27) random-integer))

  (import (only (rnrs base) assert))

  (include "delndups.scm")     ; list-delete-neighbor-dups etc
  (include "lmsort.scm")       ; list-merge, list-merge!
  (include "sortp.scm")        ; list-sorted?, vector-sorted?
  (include "vector-util.scm")
  (include "vhsort.scm")
  (include "visort.scm")
  (include "vmsort.scm")       ; vector-merge, vector-merge!
  (include "vqsort2.scm")
  (include "vqsort3.scm")
  (include "sort.scm")

  (include "select.scm"))
