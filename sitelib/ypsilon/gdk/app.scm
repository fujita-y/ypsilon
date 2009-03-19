#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk app)

  (export gdk_app_launch_context_get_type
          gdk_app_launch_context_new
          gdk_app_launch_context_set_desktop
          gdk_app_launch_context_set_display
          gdk_app_launch_context_set_icon
          gdk_app_launch_context_set_icon_name
          gdk_app_launch_context_set_screen
          gdk_app_launch_context_set_timestamp)

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

  ;; GType gdk_app_launch_context_get_type (void)
  (define-function unsigned-long gdk_app_launch_context_get_type ())

  ;; GdkAppLaunchContext* gdk_app_launch_context_new (void)
  (define-function void* gdk_app_launch_context_new ())

  ;; void gdk_app_launch_context_set_desktop (GdkAppLaunchContext* context, gint desktop)
  (define-function void gdk_app_launch_context_set_desktop (void* int))

  ;; void gdk_app_launch_context_set_display (GdkAppLaunchContext* context, GdkDisplay* display)
  (define-function void gdk_app_launch_context_set_display (void* void*))

  ;; void gdk_app_launch_context_set_icon (GdkAppLaunchContext* context, GIcon* icon)
  (define-function void gdk_app_launch_context_set_icon (void* void*))

  ;; void gdk_app_launch_context_set_icon_name (GdkAppLaunchContext* context, const char* icon_name)
  (define-function void gdk_app_launch_context_set_icon_name (void* char*))

  ;; void gdk_app_launch_context_set_screen (GdkAppLaunchContext* context, GdkScreen* screen)
  (define-function void gdk_app_launch_context_set_screen (void* void*))

  ;; void gdk_app_launch_context_set_timestamp (GdkAppLaunchContext* context, guint32 timestamp)
  (define-function void gdk_app_launch_context_set_timestamp (void* uint32_t))

  ) ;[end]
