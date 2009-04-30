#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango scan)

  (export pango_scan_int
          pango_scan_string
          pango_scan_word)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libpango-1.0.so.0")
          (on-sunos   "libpango-1.0.so.0")
          (on-freebsd "libpango-1.0.so.0")
          (on-openbsd "libpango-1.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libpango-1.0-0.dll")
          (else
           (assertion-violation #f "can not locate Pango library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; gboolean pango_scan_int (const char** pos, int* out)
  (define-function int pango_scan_int (void* void*))

  ;; gboolean pango_scan_string (const char** pos, GString* out)
  (define-function int pango_scan_string (void* void*))

  ;; gboolean pango_scan_word (const char** pos, GString* out)
  (define-function int pango_scan_word (void* void*))

  ) ;[end]
