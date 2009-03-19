#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango coverage)

  (export pango_coverage_copy
          pango_coverage_from_bytes
          pango_coverage_get
          pango_coverage_level_get_type
          pango_coverage_max
          pango_coverage_new
          pango_coverage_ref
          pango_coverage_set
          pango_coverage_to_bytes
          pango_coverage_unref)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libpango-1.0.so.0")
          (on-freebsd "libpango-1.0.so.0")
          (on-openbsd "libpango-1.0.so.0")
          (on-windows "libpango-1.0-0.dll")
          (else
           (assertion-violation #f "can not locate Pango library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; PangoCoverage* pango_coverage_copy (PangoCoverage* coverage)
  (define-function void* pango_coverage_copy (void*))

  ;; PangoCoverage* pango_coverage_from_bytes (guchar* bytes, int n_bytes)
  (define-function void* pango_coverage_from_bytes (void* int))

  ;; PangoCoverageLevel pango_coverage_get (PangoCoverage* coverage, int index_)
  (define-function int pango_coverage_get (void* int))

  ;; GType pango_coverage_level_get_type (void)
  (define-function unsigned-long pango_coverage_level_get_type ())

  ;; void pango_coverage_max (PangoCoverage* coverage, PangoCoverage* other)
  (define-function void pango_coverage_max (void* void*))

  ;; PangoCoverage* pango_coverage_new (void)
  (define-function void* pango_coverage_new ())

  ;; PangoCoverage* pango_coverage_ref (PangoCoverage* coverage)
  (define-function void* pango_coverage_ref (void*))

  ;; void pango_coverage_set (PangoCoverage* coverage, int index_, PangoCoverageLevel level)
  (define-function void pango_coverage_set (void* int int))

  ;; void pango_coverage_to_bytes (PangoCoverage* coverage, guchar** bytes, int* n_bytes)
  (define-function void pango_coverage_to_bytes (void* void* void*))

  ;; void pango_coverage_unref (PangoCoverage* coverage)
  (define-function void pango_coverage_unref (void*))

  ) ;[end]
