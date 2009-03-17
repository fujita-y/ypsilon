#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk pixmap)

  (export gdk_pixmap_colormap_create_from_xpm
          gdk_pixmap_colormap_create_from_xpm_d
          gdk_pixmap_create_from_data
          gdk_pixmap_create_from_xpm
          gdk_pixmap_create_from_xpm_d
          gdk_pixmap_foreign_new
          gdk_pixmap_foreign_new_for_display
          gdk_pixmap_foreign_new_for_screen
          gdk_pixmap_get_type
          gdk_pixmap_lookup
          gdk_pixmap_lookup_for_display
          gdk_pixmap_new)

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

  ;; GdkPixmap* gdk_pixmap_colormap_create_from_xpm (GdkDrawable* drawable, GdkColormap* colormap, GdkBitmap** mask, const GdkColor* transparent_color, const gchar* filename)
  (define-function void* gdk_pixmap_colormap_create_from_xpm (void* void* void* void* char*))

  ;; GdkPixmap* gdk_pixmap_colormap_create_from_xpm_d (GdkDrawable* drawable, GdkColormap* colormap, GdkBitmap** mask, const GdkColor* transparent_color, gchar** data)
  (define-function void* gdk_pixmap_colormap_create_from_xpm_d (void* void* void* void* void*))

  ;; GdkPixmap* gdk_pixmap_create_from_data (GdkDrawable* drawable, const gchar* data, gint width, gint height, gint depth, const GdkColor* fg, const GdkColor* bg)
  (define-function void* gdk_pixmap_create_from_data (void* char* int int int void* void*))

  ;; GdkPixmap* gdk_pixmap_create_from_xpm (GdkDrawable* drawable, GdkBitmap** mask, const GdkColor* transparent_color, const gchar* filename)
  (define-function void* gdk_pixmap_create_from_xpm (void* void* void* char*))

  ;; GdkPixmap* gdk_pixmap_create_from_xpm_d (GdkDrawable* drawable, GdkBitmap** mask, const GdkColor* transparent_color, gchar** data)
  (define-function void* gdk_pixmap_create_from_xpm_d (void* void* void* void*))

  ;; GdkPixmap* gdk_pixmap_foreign_new (GdkNativeWindow anid)
  (define-function void* gdk_pixmap_foreign_new (uint32_t))

  ;; GdkPixmap* gdk_pixmap_foreign_new_for_display (GdkDisplay* display, GdkNativeWindow anid)
  (define-function void* gdk_pixmap_foreign_new_for_display (void* uint32_t))

  ;; GdkPixmap* gdk_pixmap_foreign_new_for_screen (GdkScreen* screen, GdkNativeWindow anid, gint width, gint height, gint depth)
  (define-function void* gdk_pixmap_foreign_new_for_screen (void* uint32_t int int int))

  ;; GType gdk_pixmap_get_type (void)
  (define-function unsigned-long gdk_pixmap_get_type ())

  ;; GdkPixmap* gdk_pixmap_lookup (GdkNativeWindow anid)
  (define-function void* gdk_pixmap_lookup (uint32_t))

  ;; GdkPixmap* gdk_pixmap_lookup_for_display (GdkDisplay* display, GdkNativeWindow anid)
  (define-function void* gdk_pixmap_lookup_for_display (void* uint32_t))

  ;; GdkPixmap* gdk_pixmap_new (GdkDrawable* drawable, gint width, gint height, gint depth)
  (define-function void* gdk_pixmap_new (void* int int int))

  ) ;[end]
