#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk property)

  (export gdk_property_change
          gdk_property_delete
          gdk_property_get
          gdk_property_state_get_type)

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

  ;; void gdk_property_change (GdkWindow* window, GdkAtom property, GdkAtom type, gint format, GdkPropMode mode, const guchar* data, gint nelements)
  (define-function void gdk_property_change (void* void* void* int int void* int))

  ;; void gdk_property_delete (GdkWindow* window, GdkAtom property)
  (define-function void gdk_property_delete (void* void*))

  ;; gboolean gdk_property_get (GdkWindow* window, GdkAtom property, GdkAtom type, gulong offset, gulong length, gint pdelete, GdkAtom* actual_property_type, gint* actual_format, gint* actual_length, guchar** data)
  (define-function int gdk_property_get (void* void* void* unsigned-long unsigned-long int void* void* void* void*))

  ;; GType gdk_property_state_get_type (void)
  (define-function unsigned-long gdk_property_state_get_type ())

  ) ;[end]
