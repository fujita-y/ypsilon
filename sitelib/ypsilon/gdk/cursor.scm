#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk cursor)

  (export gdk_cursor_get_display
          gdk_cursor_get_image
          gdk_cursor_get_type
          gdk_cursor_new
          gdk_cursor_new_for_display
          gdk_cursor_new_from_name
          gdk_cursor_new_from_pixbuf
          gdk_cursor_new_from_pixmap
          gdk_cursor_ref
          gdk_cursor_type_get_type
          gdk_cursor_unref)

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

  ;; GdkDisplay* gdk_cursor_get_display (GdkCursor* cursor)
  (define-function void* gdk_cursor_get_display (void*))

  ;; GdkPixbuf* gdk_cursor_get_image (GdkCursor* cursor)
  (define-function void* gdk_cursor_get_image (void*))

  ;; GType gdk_cursor_get_type (void)
  (define-function unsigned-long gdk_cursor_get_type ())

  ;; GdkCursor* gdk_cursor_new (GdkCursorType cursor_type)
  (define-function void* gdk_cursor_new (int))

  ;; GdkCursor* gdk_cursor_new_for_display (GdkDisplay* display, GdkCursorType cursor_type)
  (define-function void* gdk_cursor_new_for_display (void* int))

  ;; GdkCursor* gdk_cursor_new_from_name (GdkDisplay* display, const gchar* name)
  (define-function void* gdk_cursor_new_from_name (void* char*))

  ;; GdkCursor* gdk_cursor_new_from_pixbuf (GdkDisplay* display, GdkPixbuf* pixbuf, gint x, gint y)
  (define-function void* gdk_cursor_new_from_pixbuf (void* void* int int))

  ;; GdkCursor* gdk_cursor_new_from_pixmap (GdkPixmap* source, GdkPixmap* mask, const GdkColor* fg, const GdkColor* bg, gint x, gint y)
  (define-function void* gdk_cursor_new_from_pixmap (void* void* void* void* int int))

  ;; GdkCursor* gdk_cursor_ref (GdkCursor* cursor)
  (define-function void* gdk_cursor_ref (void*))

  ;; GType gdk_cursor_type_get_type (void)
  (define-function unsigned-long gdk_cursor_type_get_type ())

  ;; void gdk_cursor_unref (GdkCursor* cursor)
  (define-function void gdk_cursor_unref (void*))

  ) ;[end]
