#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk threads)

  (export gdk_threads_add_idle
          gdk_threads_add_idle_full
          gdk_threads_add_timeout
          gdk_threads_add_timeout_full
          gdk_threads_add_timeout_seconds
          gdk_threads_add_timeout_seconds_full
          gdk_threads_enter
          gdk_threads_init
          gdk_threads_leave
          gdk_threads_set_lock_functions)

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

  ;; guint gdk_threads_add_idle (GSourceFunc function, gpointer data)
  (define-function unsigned-int gdk_threads_add_idle ((c-callback int (void*)) void*))

  ;; guint gdk_threads_add_idle_full (gint priority, GSourceFunc function, gpointer data, GDestroyNotify notify)
  (define-function unsigned-int gdk_threads_add_idle_full (int (c-callback int (void*)) void* (c-callback void (void*))))

  ;; guint gdk_threads_add_timeout (guint interval, GSourceFunc function, gpointer data)
  (define-function unsigned-int gdk_threads_add_timeout (unsigned-int (c-callback int (void*)) void*))

  ;; guint gdk_threads_add_timeout_full (gint priority, guint interval, GSourceFunc function, gpointer data, GDestroyNotify notify)
  (define-function unsigned-int gdk_threads_add_timeout_full (int unsigned-int (c-callback int (void*)) void* (c-callback void (void*))))

  ;; guint gdk_threads_add_timeout_seconds (guint interval, GSourceFunc function, gpointer data)
  (define-function unsigned-int gdk_threads_add_timeout_seconds (unsigned-int (c-callback int (void*)) void*))

  ;; guint gdk_threads_add_timeout_seconds_full (gint priority, guint interval, GSourceFunc function, gpointer data, GDestroyNotify notify)
  (define-function unsigned-int gdk_threads_add_timeout_seconds_full (int unsigned-int (c-callback int (void*)) void* (c-callback void (void*))))

  ;; void gdk_threads_enter (void)
  (define-function void gdk_threads_enter ())

  ;; void gdk_threads_init (void)
  (define-function void gdk_threads_init ())

  ;; void gdk_threads_leave (void)
  (define-function void gdk_threads_leave ())

  ;; void gdk_threads_set_lock_functions (GCallback enter_fn, GCallback leave_fn)
  (define-function void gdk_threads_set_lock_functions ((c-callback void ()) (c-callback void ())))

  ) ;[end]
