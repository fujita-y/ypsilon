#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango version)

  (export pango_version
          pango_version_check
          pango_version_string)

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

  ;; int pango_version (void)
  (define-function int pango_version ())

  ;; const char* pango_version_check (int required_major, int required_minor, int required_micro)
  (define-function char* pango_version_check (int int int))

  ;; const char* pango_version_string (void)
  (define-function char* pango_version_string ())

  ) ;[end]
