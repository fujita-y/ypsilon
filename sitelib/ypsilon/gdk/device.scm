#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk device)

  (export gdk_device_free_history
          gdk_device_get_axis
          gdk_device_get_core_pointer
          gdk_device_get_history
          gdk_device_get_state
          gdk_device_get_type
          gdk_device_set_axis_use
          gdk_device_set_key
          gdk_device_set_mode
          gdk_device_set_source)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gdk.framework/Gdk")
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

  ;; void gdk_device_free_history (GdkTimeCoord** events, gint n_events)
  (define-function void gdk_device_free_history (void* int))

  ;; gboolean gdk_device_get_axis (GdkDevice* device, gdouble* axes, GdkAxisUse use, gdouble* value)
  (define-function int gdk_device_get_axis (void* void* int void*))

  ;; GdkDevice* gdk_device_get_core_pointer (void)
  (define-function void* gdk_device_get_core_pointer ())

  ;; gboolean gdk_device_get_history (GdkDevice* device, GdkWindow* window, guint32 start, guint32 stop, GdkTimeCoord** *events, gint* n_events)
  (define-function int gdk_device_get_history (void* void* uint32_t uint32_t void* void*))

  ;; void gdk_device_get_state (GdkDevice* device, GdkWindow* window, gdouble* axes, GdkModifierType* mask)
  (define-function void gdk_device_get_state (void* void* void* void*))

  ;; GType gdk_device_get_type (void)
  (define-function unsigned-long gdk_device_get_type ())

  ;; void gdk_device_set_axis_use (GdkDevice* device, guint index_, GdkAxisUse use)
  (define-function void gdk_device_set_axis_use (void* unsigned-int int))

  ;; void gdk_device_set_key (GdkDevice* device, guint index_, guint keyval, GdkModifierType modifiers)
  (define-function void gdk_device_set_key (void* unsigned-int unsigned-int int))

  ;; gboolean gdk_device_set_mode (GdkDevice* device, GdkInputMode mode)
  (define-function int gdk_device_set_mode (void* int))

  ;; void gdk_device_set_source (GdkDevice* device, GdkInputSource source)
  (define-function void gdk_device_set_source (void* int))

  ) ;[end]
