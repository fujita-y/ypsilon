#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk notify)

  (export gdk_notify_startup_complete
          gdk_notify_startup_complete_with_id
          gdk_notify_type_get_type)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-windows "libgdk-win32-2.0-0.dll")
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

  ;; void gdk_notify_startup_complete (void)
  (define-function void gdk_notify_startup_complete ())

  ;; void gdk_notify_startup_complete_with_id (const gchar* startup_id)
  (define-function void gdk_notify_startup_complete_with_id (char*))

  ;; GType gdk_notify_type_get_type (void)
  (define-function unsigned-long gdk_notify_type_get_type ())

  ) ;[end]
