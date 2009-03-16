#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango attribute)

  (export pango_attribute_copy
          pango_attribute_destroy
          pango_attribute_equal
          pango_attribute_init)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Pango.framework/Pango")
          (on-linux   "libpango-1.0.so.0")
          (on-freebsd "libpango-1.0.so.0")
          (on-openbsd "libpango-1.0.so.0")
          (on-windows "libpango-1.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; PangoAttribute* pango_attribute_copy (const PangoAttribute* attr)
  (define-function void* pango_attribute_copy (void*))

  ;; void pango_attribute_destroy (PangoAttribute* attr)
  (define-function void pango_attribute_destroy (void*))

  ;; gboolean pango_attribute_equal (const PangoAttribute* attr1, const PangoAttribute* attr2)
  (define-function int pango_attribute_equal (void* void*))

  ;; void pango_attribute_init (PangoAttribute* attr, const PangoAttrClass* klass)
  (define-function void pango_attribute_init (void* void*))

  ) ;[end]
