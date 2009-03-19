#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango find)

  (export pango_find_base_dir
          pango_find_paragraph_boundary)

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

  ;; PangoDirection pango_find_base_dir (const gchar* text, gint length)
  (define-function int pango_find_base_dir (char* int))

  ;; void pango_find_paragraph_boundary (const gchar* text, gint length, gint* paragraph_delimiter_index, gint* next_paragraph_start)
  (define-function void pango_find_paragraph_boundary (char* int void* void*))

  ) ;[end]
