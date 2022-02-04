(define-library (srfi 132)
  (import (srfi 132 sorting))
  (export list-sorted?
          vector-sorted?
          list-sort
          vector-sort
          list-stable-sort
          vector-stable-sort
          list-sort!
          vector-sort!
          list-stable-sort!
          vector-stable-sort!
          list-merge
          vector-merge
          list-merge!
          vector-merge!
          list-delete-neighbor-dups
          vector-delete-neighbor-dups
          list-delete-neighbor-dups!
          vector-delete-neighbor-dups!
          vector-find-median
          vector-find-median!
          vector-select!
          vector-separate!))
