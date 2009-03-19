#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk set)

  (export gdk_set_double_click_time
          gdk_set_locale
          gdk_set_pointer_hooks
          gdk_set_program_class
          gdk_set_show_events
          gdk_set_sm_client_id)

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

  ;; void gdk_set_double_click_time (guint msec)
  (define-function void gdk_set_double_click_time (unsigned-int))

  ;; gchar* gdk_set_locale (void)
  (define-function char* gdk_set_locale ())

  ;; GdkPointerHooks* gdk_set_pointer_hooks (const GdkPointerHooks* new_hooks)
  (define-function void* gdk_set_pointer_hooks (void*))

  ;; void gdk_set_program_class (const char* program_class)
  (define-function void gdk_set_program_class (char*))

  ;; void gdk_set_show_events (gboolean show_events)
  (define-function void gdk_set_show_events (int))

  ;; void gdk_set_sm_client_id (const gchar* sm_client_id)
  (define-function void gdk_set_sm_client_id (char*))

  ) ;[end]
