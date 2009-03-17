#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango language)

  (export pango_language_from_string
          pango_language_get_default
          pango_language_get_sample_string
          pango_language_get_scripts
          pango_language_get_type
          pango_language_includes_script
          pango_language_matches
          pango_language_to_string)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
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

  ;; PangoLanguage* pango_language_from_string (const char* language)
  (define-function void* pango_language_from_string (char*))

  ;; PangoLanguage* pango_language_get_default (void)
  (define-function void* pango_language_get_default ())

  ;; const char* pango_language_get_sample_string (PangoLanguage* language)
  (define-function char* pango_language_get_sample_string (void*))

  ;; const PangoScript* pango_language_get_scripts (PangoLanguage* language, int* num_scripts)
  (define-function void* pango_language_get_scripts (void* void*))

  ;; GType pango_language_get_type (void)
  (define-function unsigned-long pango_language_get_type ())

  ;; gboolean pango_language_includes_script (PangoLanguage* language, PangoScript script)
  (define-function int pango_language_includes_script (void* int))

  ;; gboolean pango_language_matches (PangoLanguage* language, const char* range_list)
  (define-function int pango_language_matches (void* char*))

  ;; const char* pango_language_to_string (PangoLanguage* language)
  (define-function char* pango_language_to_string (void*))

  ) ;[end]
