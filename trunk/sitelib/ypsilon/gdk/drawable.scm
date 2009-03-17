#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk drawable)

  (export gdk_drawable_copy_to_image
          gdk_drawable_get_clip_region
          gdk_drawable_get_colormap
          gdk_drawable_get_depth
          gdk_drawable_get_display
          gdk_drawable_get_image
          gdk_drawable_get_screen
          gdk_drawable_get_size
          gdk_drawable_get_type
          gdk_drawable_get_visible_region
          gdk_drawable_get_visual
          gdk_drawable_set_colormap)

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

  ;; GdkImage* gdk_drawable_copy_to_image (GdkDrawable* drawable, GdkImage* image, gint src_x, gint src_y, gint dest_x, gint dest_y, gint width, gint height)
  (define-function void* gdk_drawable_copy_to_image (void* void* int int int int int int))

  ;; GdkRegion* gdk_drawable_get_clip_region (GdkDrawable* drawable)
  (define-function void* gdk_drawable_get_clip_region (void*))

  ;; GdkColormap* gdk_drawable_get_colormap (GdkDrawable* drawable)
  (define-function void* gdk_drawable_get_colormap (void*))

  ;; gint gdk_drawable_get_depth (GdkDrawable* drawable)
  (define-function int gdk_drawable_get_depth (void*))

  ;; GdkDisplay* gdk_drawable_get_display (GdkDrawable* drawable)
  (define-function void* gdk_drawable_get_display (void*))

  ;; GdkImage* gdk_drawable_get_image (GdkDrawable* drawable, gint x, gint y, gint width, gint height)
  (define-function void* gdk_drawable_get_image (void* int int int int))

  ;; GdkScreen* gdk_drawable_get_screen (GdkDrawable* drawable)
  (define-function void* gdk_drawable_get_screen (void*))

  ;; void gdk_drawable_get_size (GdkDrawable* drawable, gint* width, gint* height)
  (define-function void gdk_drawable_get_size (void* void* void*))

  ;; GType gdk_drawable_get_type (void)
  (define-function unsigned-long gdk_drawable_get_type ())

  ;; GdkRegion* gdk_drawable_get_visible_region (GdkDrawable* drawable)
  (define-function void* gdk_drawable_get_visible_region (void*))

  ;; GdkVisual* gdk_drawable_get_visual (GdkDrawable* drawable)
  (define-function void* gdk_drawable_get_visual (void*))

  ;; void gdk_drawable_set_colormap (GdkDrawable* drawable, GdkColormap* colormap)
  (define-function void gdk_drawable_set_colormap (void* void*))

  ) ;[end]
