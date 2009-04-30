#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk fill)

  (export gdk_fill_get_type
          gdk_fill_rule_get_type)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgdk-x11-2.0.so.0")
          (on-sunos   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libgdk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; GType gdk_fill_get_type (void)
  (define-function unsigned-long gdk_fill_get_type ())

  ;; GType gdk_fill_rule_get_type (void)
  (define-function unsigned-long gdk_fill_rule_get_type ())

  ) ;[end]
