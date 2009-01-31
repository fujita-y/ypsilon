#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core records)

  (export make-record-type-descriptor
          record-type-descriptor?
          make-record-constructor-descriptor
          record-constructor
          record-predicate
          record-accessor
          record-mutator
          record?
          record-rtd
          record-type-name
          record-type-parent
          record-type-uid
          record-type-generative?
          record-type-sealed?
          record-type-opaque?
          record-type-field-names
          record-field-mutable?
          record-type-descriptor
          record-constructor-descriptor
          define-record-type
          fields
          mutable
          immutable
          parent
          protocol
          sealed
          opaque
          nongenerative
          parent-rtd)

  (import (core primitives)
          (core lists)
          (core syntax-case)
          (core destructuring))

  (define-syntax define-record-type
    (lambda (x)
      (let ((stash (make-core-hashtable)))

        (define stash-set!
          (lambda (key value)
            (and (core-hashtable-ref stash key #f)
                 (syntax-violation 'define-record-type (format "duplicate ~a clause" key) x))
            (core-hashtable-set! stash key (list value))))

        (define stash-ref
          (lambda (key default)
            (cond ((core-hashtable-ref stash key #f) => car)
                  (else default))))

        (define parse-record-clauses
          (lambda (first-name record-clauses)
            (for-each
             (lambda (c)
               (syntax-case c (parent protocol parent-rtd sealed opaque nongenerative fields)
                 ((parent e1)
                  (identifier? #'e1)
                  (stash-set! 'parent #'e1))
                 ((protocol e1)
                  (stash-set! 'protocol #'e1))
                 ((parent-rtd e1 e2)
                  (stash-set! 'parent-rtd (cons #'e1 #'e2)))
                 ((sealed e1)
                  (boolean? (syntax->datum #'e1))
                  (stash-set! 'sealed #'e1))
                 ((opaque e1)
                  (boolean? (syntax->datum #'e1))
                  (stash-set! 'opaque #'e1))
                 ((nongenerative e1)
                  (identifier? #'e1)
                  (stash-set! 'nongenerative #'e1))
                 ((nongenerative)
                  (stash-set! 'nongenerative (datum->syntax #'k (string->symbol (format "non-generative-record-type ~a" (gensym))))))
                 ((fields specs ...)
                  (stash-set!
                   'fields
                   (map (lambda (spec)
                          (destructuring-match spec
                            (('immutable name accessor)
                             (and (symbol? name) (symbol? accessor))
                             `((immutable ,name) ,accessor #f))
                            (('mutable name accessor mutator)
                             (and (symbol? name) (symbol? accessor) (symbol? mutator))
                             `((mutable ,name) ,accessor ,mutator))
                            (('immutable name)
                             (symbol? name)
                             `((immutable ,name)
                               ,(string->symbol (format "~a-~a" first-name name)) #f))
                            (('mutable name)
                             (symbol? name)
                             `((mutable ,name)
                               ,(string->symbol (format "~a-~a" first-name name))
                               ,(string->symbol (format "~a-~a-set!" first-name name))))
                            (name
                             (symbol? name)
                             `((immutable ,name)
                               ,(string->symbol (format "~a-~a" first-name name)) #f))
                            (_
                             (syntax-violation 'define-record-type "malformed field spec" x spec))))
                        (syntax->datum (syntax (specs ...))))))
                 (_ (syntax-violation 'define-record-type "malformed record clauses" x (syntax->datum c)))))
             record-clauses)))

        (syntax-case x ()
          ((_ (record-name constructor-name predicate-name) record-clauses ...)
           (and (identifier? #'record-name) (identifier? #'constructor-name) (identifier? #'predicate-name))
           (begin
             (parse-record-clauses (syntax->datum #'record-name) #'(record-clauses ...))
             (and (stash-ref 'parent-rtd #f)
                  (stash-ref 'parent #f)
                  (syntax-violation 'define-record-type "definition have both parent and parent-rtd clause" x))
             (with-syntax
                 ((record-type
                   (with-syntax ((parent (stash-ref 'parent (syntax #f))))
                     (with-syntax ((rtd-parent (cond ((stash-ref 'parent-rtd #f) => car)
                                                     ((stash-ref 'parent #f) (syntax (record-type-rtd parent)))
                                                     (else (syntax #f))))
                                   (rcd-parent (cond ((stash-ref 'parent-rtd #f) => cdr)
                                                     ((stash-ref 'parent #f) (syntax (record-type-rcd parent)))
                                                     (else (syntax #f))))
                                   (uid (stash-ref 'nongenerative (syntax #f)))
                                   (sealed (stash-ref 'sealed (syntax #f)))
                                   (opaque (stash-ref 'opaque (syntax #f)))
                                   (protocol (stash-ref 'protocol (syntax #f)))
                                   (((fields _ _) ...) (datum->syntax #'k (stash-ref 'fields '()))))
                       (syntax (define record-name
                                 (let* ((rtd (make-record-type-descriptor 'record-name rtd-parent 'uid sealed opaque '#(fields ...)))
                                        (rcd (make-record-constructor-descriptor rtd rcd-parent protocol)))
                                   (make-record-type 'record-name rtd rcd)))))))
                  (constructor
                   (syntax (define constructor-name (record-constructor (record-type-rcd record-name)))))
                  (predicate
                   (syntax (define predicate-name (record-predicate (record-type-rtd record-name)))))
                  ((accessors ...)
                   (let ((index -1))
                     (filter values
                             (map (lambda (spec)
                                    (set! index (+ index 1))
                                    (and (cadr spec)
                                         (with-syntax ((name (datum->syntax #'record-name (cadr spec))) (n (datum->syntax #'k index)))
                                           (syntax (define name (record-accessor (record-type-rtd record-name) n))))))
                                  (stash-ref 'fields '())))))
                  ((mutators ...)
                   (let ((index -1))
                     (filter values
                             (map (lambda (spec)
                                    (set! index (+ index 1))
                                    (and (caddr spec)
                                         (with-syntax ((name (datum->syntax #'record-name (caddr spec))) (n (datum->syntax #'k index)))
                                           (syntax (define name (record-mutator (record-type-rtd record-name) n))))))
                                  (stash-ref 'fields '()))))))
               (syntax (begin record-type constructor predicate accessors ... mutators ...)))))

          ((_ record-name record-clauses ...)
           (identifier? #'record-name)
           (let ((base-name (symbol->string (syntax->datum (syntax record-name)))))
             (with-syntax ((constructor-name (datum->syntax #'record-name (string->symbol (string-append "make-" base-name))))
                           (predicate-name (datum->syntax #'record-name (string->symbol (string-append base-name "?")))))
               (syntax (define-record-type (record-name constructor-name predicate-name) record-clauses ...)))))))))

  (define-syntax record-type-descriptor
    (lambda (x)
      (syntax-case x ()
        ((_ name) (identifier? #'name) (syntax (record-type-rtd name))))))

  (define-syntax record-constructor-descriptor
    (lambda (x)
      (syntax-case x ()
        ((_ name) (identifier? #'name) (syntax (record-type-rcd name))))))

  (define-syntax fields
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax mutable
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax immutable
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax parent
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax protocol
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax sealed
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax opaque
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax nongenerative
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax parent-rtd
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  ) ;[end]
