#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk keyboard)

  (export gdk_keyboard_grab
          gdk_keyboard_grab_info_libgtk_only
          gdk_keyboard_ungrab)

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

  ;; GdkGrabStatus gdk_keyboard_grab (GdkWindow* window, gboolean owner_events, guint32 time_)
  (define-function int gdk_keyboard_grab (void* int uint32_t))

  ;; gboolean gdk_keyboard_grab_info_libgtk_only (GdkDisplay* display, GdkWindow** grab_window, gboolean* owner_events)
  (define-function int gdk_keyboard_grab_info_libgtk_only (void* void* void*))

  ;; void gdk_keyboard_ungrab (guint32 time_)
  (define-function void gdk_keyboard_ungrab (uint32_t))

  ) ;[end]
