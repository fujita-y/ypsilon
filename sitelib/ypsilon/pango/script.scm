#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango script)

  (export pango_script_for_unichar
          pango_script_get_sample_language
          pango_script_get_type
          pango_script_iter_free
          pango_script_iter_get_range
          pango_script_iter_new
          pango_script_iter_next)

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

  ;; PangoScript pango_script_for_unichar (gunichar ch)
  (define-function int pango_script_for_unichar (uint32_t))

  ;; PangoLanguage* pango_script_get_sample_language (PangoScript script)
  (define-function void* pango_script_get_sample_language (int))

  ;; GType pango_script_get_type (void)
  (define-function unsigned-long pango_script_get_type ())

  ;; void pango_script_iter_free (PangoScriptIter* iter)
  (define-function void pango_script_iter_free (void*))

  ;; void pango_script_iter_get_range (PangoScriptIter* iter, const char** start, const char** end, PangoScript* script)
  (define-function void pango_script_iter_get_range (void* void* void* void*))

  ;; PangoScriptIter* pango_script_iter_new (const char* text, int length)
  (define-function void* pango_script_iter_new (char* int))

  ;; gboolean pango_script_iter_next (PangoScriptIter* iter)
  (define-function int pango_script_iter_next (void*))

  ) ;[end]
