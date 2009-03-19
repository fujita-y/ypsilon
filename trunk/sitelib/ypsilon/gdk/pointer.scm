#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk pointer)

  (export gdk_pointer_grab
          gdk_pointer_grab_info_libgtk_only
          gdk_pointer_is_grabbed
          gdk_pointer_ungrab)

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

  ;; GdkGrabStatus gdk_pointer_grab (GdkWindow* window, gboolean owner_events, GdkEventMask event_mask, GdkWindow* confine_to, GdkCursor* cursor, guint32 time_)
  (define-function int gdk_pointer_grab (void* int int void* void* uint32_t))

  ;; gboolean gdk_pointer_grab_info_libgtk_only (GdkDisplay* display, GdkWindow** grab_window, gboolean* owner_events)
  (define-function int gdk_pointer_grab_info_libgtk_only (void* void* void*))

  ;; gboolean gdk_pointer_is_grabbed (void)
  (define-function int gdk_pointer_is_grabbed ())

  ;; void gdk_pointer_ungrab (guint32 time_)
  (define-function void gdk_pointer_ungrab (uint32_t))

  ) ;[end]
