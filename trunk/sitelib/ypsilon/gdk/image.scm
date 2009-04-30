#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk image)

  (export gdk_image_get_colormap
          gdk_image_get_pixel
          gdk_image_get_type
          gdk_image_new
          gdk_image_put_pixel
          gdk_image_set_colormap
          gdk_image_type_get_type)

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

  ;; GdkColormap* gdk_image_get_colormap (GdkImage* image)
  (define-function void* gdk_image_get_colormap (void*))

  ;; guint32 gdk_image_get_pixel (GdkImage* image, gint x, gint y)
  (define-function uint32_t gdk_image_get_pixel (void* int int))

  ;; GType gdk_image_get_type (void)
  (define-function unsigned-long gdk_image_get_type ())

  ;; GdkImage* gdk_image_new (GdkImageType type, GdkVisual* visual, gint width, gint height)
  (define-function void* gdk_image_new (int void* int int))

  ;; void gdk_image_put_pixel (GdkImage* image, gint x, gint y, guint32 pixel)
  (define-function void gdk_image_put_pixel (void* int int uint32_t))

  ;; void gdk_image_set_colormap (GdkImage* image, GdkColormap* colormap)
  (define-function void gdk_image_set_colormap (void* void*))

  ;; GType gdk_image_type_get_type (void)
  (define-function unsigned-long gdk_image_type_get_type ())

  ) ;[end]
