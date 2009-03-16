#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk selection)

  (export gdk_selection_convert
          gdk_selection_owner_get
          gdk_selection_owner_get_for_display
          gdk_selection_owner_set
          gdk_selection_owner_set_for_display
          gdk_selection_property_get
          gdk_selection_send_notify
          gdk_selection_send_notify_for_display)

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

  ;; void gdk_selection_convert (GdkWindow* requestor, GdkAtom selection, GdkAtom target, guint32 time_)
  (define-function void gdk_selection_convert (void* void* void* uint32_t))

  ;; GdkWindow* gdk_selection_owner_get (GdkAtom selection)
  (define-function void* gdk_selection_owner_get (void*))

  ;; GdkWindow* gdk_selection_owner_get_for_display (GdkDisplay* display, GdkAtom selection)
  (define-function void* gdk_selection_owner_get_for_display (void* void*))

  ;; gboolean gdk_selection_owner_set (GdkWindow* owner, GdkAtom selection, guint32 time_, gboolean send_event)
  (define-function int gdk_selection_owner_set (void* void* uint32_t int))

  ;; gboolean gdk_selection_owner_set_for_display (GdkDisplay* display, GdkWindow* owner, GdkAtom selection, guint32 time_, gboolean send_event)
  (define-function int gdk_selection_owner_set_for_display (void* void* void* uint32_t int))

  ;; gboolean gdk_selection_property_get (GdkWindow* requestor, guchar** data, GdkAtom* prop_type, gint* prop_format)
  (define-function int gdk_selection_property_get (void* void* void* void*))

  ;; void gdk_selection_send_notify (GdkNativeWindow requestor, GdkAtom selection, GdkAtom target, GdkAtom property, guint32 time_)
  (define-function void gdk_selection_send_notify (uint32_t void* void* void* uint32_t))

  ;; void gdk_selection_send_notify_for_display (GdkDisplay* display, GdkNativeWindow requestor, GdkAtom selection, GdkAtom target, GdkAtom property, guint32 time_)
  (define-function void gdk_selection_send_notify_for_display (void* uint32_t void* void* void* uint32_t))

  ) ;[end]
