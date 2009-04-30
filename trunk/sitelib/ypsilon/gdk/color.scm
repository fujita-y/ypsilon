#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk color)

  (export gdk_color_copy
          gdk_color_equal
          gdk_color_free
          gdk_color_get_type
          gdk_color_hash
          gdk_color_parse
          gdk_color_to_string)

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

  ;; GdkColor* gdk_color_copy (const GdkColor* color)
  (define-function void* gdk_color_copy (void*))

  ;; gboolean gdk_color_equal (const GdkColor* colora, const GdkColor* colorb)
  (define-function int gdk_color_equal (void* void*))

  ;; void gdk_color_free (GdkColor* color)
  (define-function void gdk_color_free (void*))

  ;; GType gdk_color_get_type (void)
  (define-function unsigned-long gdk_color_get_type ())

  ;; guint gdk_color_hash (const GdkColor* colora)
  (define-function unsigned-int gdk_color_hash (void*))

  ;; gboolean gdk_color_parse (const gchar* spec, GdkColor* color)
  (define-function int gdk_color_parse (char* void*))

  ;; gchar* gdk_color_to_string (const GdkColor* color)
  (define-function char* gdk_color_to_string (void*))

  ) ;[end]
