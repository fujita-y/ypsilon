#!nobacktrace
(library (srfi :42 eager-comprehensions)
  (export :
          :char-range
          :dispatched
          :do
          :integers
          :let
          :list
          :parallel
          :port
          :range
          :real-range
          :string
          :until
          :vector
          :while
          any?-ec
          append-ec
          do-ec
          every?-ec
          first-ec
          fold-ec
          fold3-ec
          last-ec
          list-ec
          max-ec
          min-ec
          product-ec
          string-append-ec
          string-ec
          sum-ec
          vector-ec
          vector-of-length-ec)
  (import (srfi srfi-42)))
