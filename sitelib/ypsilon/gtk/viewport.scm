#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk viewport)

  (export gtk_viewport_get_hadjustment
          gtk_viewport_get_shadow_type
          gtk_viewport_get_type
          gtk_viewport_get_vadjustment
          gtk_viewport_new
          gtk_viewport_set_hadjustment
          gtk_viewport_set_shadow_type
          gtk_viewport_set_vadjustment)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgtk-x11-2.0.so.0")
          (on-sunos   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libgtk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GTK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-function/va_list
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "va_list argument not supported"))))))

  ;; GtkAdjustment* gtk_viewport_get_hadjustment (GtkViewport* viewport)
  (define-function void* gtk_viewport_get_hadjustment (void*))

  ;; GtkShadowType gtk_viewport_get_shadow_type (GtkViewport* viewport)
  (define-function int gtk_viewport_get_shadow_type (void*))

  ;; GType gtk_viewport_get_type (void)
  (define-function unsigned-long gtk_viewport_get_type ())

  ;; GtkAdjustment* gtk_viewport_get_vadjustment (GtkViewport* viewport)
  (define-function void* gtk_viewport_get_vadjustment (void*))

  ;; GtkWidget* gtk_viewport_new (GtkAdjustment* hadjustment, GtkAdjustment* vadjustment)
  (define-function void* gtk_viewport_new (void* void*))

  ;; void gtk_viewport_set_hadjustment (GtkViewport* viewport, GtkAdjustment* adjustment)
  (define-function void gtk_viewport_set_hadjustment (void* void*))

  ;; void gtk_viewport_set_shadow_type (GtkViewport* viewport, GtkShadowType type)
  (define-function void gtk_viewport_set_shadow_type (void* int))

  ;; void gtk_viewport_set_vadjustment (GtkViewport* viewport, GtkAdjustment* adjustment)
  (define-function void gtk_viewport_set_vadjustment (void* void*))

  ) ;[end]
