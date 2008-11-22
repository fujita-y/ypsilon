#!nobacktrace

(library (srfi :42)
  (export do-ec list-ec append-ec string-ec string-append-ec vector-ec vector-of-length-ec
          sum-ec product-ec min-ec max-ec any?-ec every?-ec first-ec last-ec fold-ec fold3-ec
          :list :string :vector :integers :range :real-range :char-range :port :dispatched :do
          :let :parallel :while :until :)
  (import (srfi srfi-42)))
