#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon glib timeout)

  (export g_timeout_add
          g_timeout_add_full
          g_timeout_add_seconds
          g_timeout_add_seconds_full)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libglib-2.0.so.0")
          (on-freebsd "libglib-2.0.so")
          (on-openbsd "libglib-2.0.so")
          (on-windows "libglib-2.0-0.dll")
          (on-darwin  "GLib.framework/GLib")
          (else
           (assertion-violation #f "can not locate GObject library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

    ;; guint g_timeout_add (guint interval, GSourceFunc function, gpointer data)
    (define-function unsigned-int g_timeout_add (unsigned-int (c-callback void (void*)) void*))

    ;; guint g_timeout_add_full (gint priority, guint interval, GSourceFunc function, gpointer data, GDestroyNotify notify)
    (define-function unsigned-int g_timeout_add_full (int unsigned-int (c-callback void (void*)) void* (c-callback void (void*))))

    ;; guint g_timeout_add_seconds (guint interval, GSourceFunc function, gpointer data)
    (define-function unsigned-int g_timeout_add_seconds (unsigned-int (c-callback void (void*)) void*))

    ;; guint g_timeout_add_seconds_full (gint priority, guint interval, GSourceFunc function, gpointer data, GDestroyNotify notify)
    (define-function unsigned-int g_timeout_add_seconds_full (int unsigned-int (c-callback void (void*)) void* (c-callback void (void*))))

  ) ;[end]
