#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango fontset)

  (export pango_fontset_foreach
          pango_fontset_get_font
          pango_fontset_get_metrics
          pango_fontset_get_type)

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

  ;; void pango_fontset_foreach (PangoFontset* fontset, PangoFontsetForeachFunc func, gpointer data)
  (define-function void pango_fontset_foreach (void* (c-callback int (void* void* void*)) void*))

  ;; PangoFont* pango_fontset_get_font (PangoFontset* fontset, guint wc)
  (define-function void* pango_fontset_get_font (void* unsigned-int))

  ;; PangoFontMetrics* pango_fontset_get_metrics (PangoFontset* fontset)
  (define-function void* pango_fontset_get_metrics (void*))

  ;; GType pango_fontset_get_type (void)
  (define-function unsigned-long pango_fontset_get_type ())

  ) ;[end]
