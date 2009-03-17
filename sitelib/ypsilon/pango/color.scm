#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango color)

  (export pango_color_copy
          pango_color_free
          pango_color_get_type
          pango_color_parse
          pango_color_to_string)

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

  ;; PangoColor* pango_color_copy (const PangoColor* src)
  (define-function void* pango_color_copy (void*))

  ;; void pango_color_free (PangoColor* color)
  (define-function void pango_color_free (void*))

  ;; GType pango_color_get_type (void)
  (define-function unsigned-long pango_color_get_type ())

  ;; gboolean pango_color_parse (PangoColor* color, const char* spec)
  (define-function int pango_color_parse (void* char*))

  ;; gchar* pango_color_to_string(const PangoColor* color)
  (define-function char* pango_color_to_string (void*))

  ) ;[end]
