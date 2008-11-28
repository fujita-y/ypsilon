#!nobacktrace
(library (srfi :13)

  (export

   ;; predicates
   string? string-null?
   string-every string-any

   ;; constructors
   make-string string string-tabulate

   ;; list & string conversion
   string->list list->string
   reverse-list->string string-join

   ;; selection
   string-length
   string-ref
   string-copy
   substring/shared
   string-copy!
   string-take string-take-right
   string-drop string-drop-right
   string-pad  string-pad-right
   string-trim string-trim-right string-trim-both

   ;; modification
   string-set! string-fill!

   ;; comparison
   string-compare string-compare-ci
   string<>     string=    string<    string>    string<=    string>=
   string-ci<>  string-ci= string-ci< string-ci> string-ci<= string-ci>=
   string-hash  string-hash-ci

   ;; prefixes & suffixes
   string-prefix-length    string-suffix-length
   string-prefix-length-ci string-suffix-length-ci
   string-prefix?    string-suffix?
   string-prefix-ci? string-suffix-ci?

   ;; searching
   string-index string-index-right
   string-skip  string-skip-right
   string-count
   string-contains string-contains-ci

   ;; alphabetic case mapping
   string-titlecase  string-upcase  string-downcase
   string-titlecase! string-upcase! string-downcase!

   ;; reverse & append
   string-reverse string-reverse!
   string-append
   string-concatenate
   string-concatenate/shared string-append/shared
   string-concatenate-reverse string-concatenate-reverse/shared

   ;; fold, unfold & map
   string-map      string-map!
   string-fold     string-fold-right
   string-unfold   string-unfold-right
   string-for-each string-for-each-index

   ;; replicate & rotate
   xsubstring string-xcopy!

   ;; miscellaneous: insertion, parsing
   string-replace string-tokenize

   ;; filtering & deleting
   string-filter string-delete

   ;; low-level procedures
   string-parse-start+end
   string-parse-final-start+end
   let-string-start+end
   check-substring-spec
   substring-spec-ok?
   make-kmp-restart-vector kmp-step string-kmp-partial-search)

  ;; procedures conflict with r6rs
  #;(rename (string-copy/srfi-13 string-copy)
             (string-for-each/srfi-13 string-for-each)
             (string-hash/srfi-13 string-hash)
             (string-upcase/srfi-13 string-upcase)
             (string-downcase/srfi-13 string-downcase)
             (string-titlecase/srfi-13 string-titlecase)
             (string-fill!/srfi-13 string-fill!)
             (string->list/srfi-13 string->list))

  (import (srfi srfi-13))

  ) ;[end]
