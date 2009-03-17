#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk cairo)

  (export gdk_cairo_create
          gdk_cairo_rectangle
          gdk_cairo_region
          gdk_cairo_set_source_color
          gdk_cairo_set_source_pixbuf
          gdk_cairo_set_source_pixmap)

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

  ;; cairo_t* gdk_cairo_create (GdkDrawable* drawable)
  (define-function void* gdk_cairo_create (void*))

  ;; void gdk_cairo_rectangle (cairo_t* cr, const GdkRectangle* rectangle)
  (define-function void gdk_cairo_rectangle (void* void*))

  ;; void gdk_cairo_region (cairo_t* cr, const GdkRegion* region)
  (define-function void gdk_cairo_region (void* void*))

  ;; void gdk_cairo_set_source_color (cairo_t* cr, const GdkColor* color)
  (define-function void gdk_cairo_set_source_color (void* void*))

  ;; void gdk_cairo_set_source_pixbuf (cairo_t* cr, const GdkPixbuf* pixbuf, double pixbuf_x, double pixbuf_y)
  (define-function void gdk_cairo_set_source_pixbuf (void* void* double double))

  ;; void gdk_cairo_set_source_pixmap (cairo_t* cr, GdkPixmap* pixmap, double pixmap_x, double pixmap_y)
  (define-function void gdk_cairo_set_source_pixmap (void* void* double double))

  ) ;[end]
