#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk scrolled)

  (export gtk_scrolled_window_add_with_viewport
          gtk_scrolled_window_get_hadjustment
          gtk_scrolled_window_get_hscrollbar
          gtk_scrolled_window_get_placement
          gtk_scrolled_window_get_policy
          gtk_scrolled_window_get_shadow_type
          gtk_scrolled_window_get_type
          gtk_scrolled_window_get_vadjustment
          gtk_scrolled_window_get_vscrollbar
          gtk_scrolled_window_new
          gtk_scrolled_window_set_hadjustment
          gtk_scrolled_window_set_placement
          gtk_scrolled_window_set_policy
          gtk_scrolled_window_set_shadow_type
          gtk_scrolled_window_set_vadjustment
          gtk_scrolled_window_unset_placement)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
          (on-windows "libgtk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GTK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; void gtk_scrolled_window_add_with_viewport (GtkScrolledWindow* scrolled_window, GtkWidget* child)
  (define-function void gtk_scrolled_window_add_with_viewport (void* void*))

  ;; GtkAdjustment* gtk_scrolled_window_get_hadjustment (GtkScrolledWindow* scrolled_window)
  (define-function void* gtk_scrolled_window_get_hadjustment (void*))

  ;; GtkWidget* gtk_scrolled_window_get_hscrollbar (GtkScrolledWindow* scrolled_window)
  (define-function void* gtk_scrolled_window_get_hscrollbar (void*))

  ;; GtkCornerType gtk_scrolled_window_get_placement (GtkScrolledWindow* scrolled_window)
  (define-function int gtk_scrolled_window_get_placement (void*))

  ;; void gtk_scrolled_window_get_policy (GtkScrolledWindow* scrolled_window, GtkPolicyType* hscrollbar_policy, GtkPolicyType* vscrollbar_policy)
  (define-function void gtk_scrolled_window_get_policy (void* void* void*))

  ;; GtkShadowType gtk_scrolled_window_get_shadow_type (GtkScrolledWindow* scrolled_window)
  (define-function int gtk_scrolled_window_get_shadow_type (void*))

  ;; GType gtk_scrolled_window_get_type (void)
  (define-function unsigned-long gtk_scrolled_window_get_type ())

  ;; GtkAdjustment* gtk_scrolled_window_get_vadjustment (GtkScrolledWindow* scrolled_window)
  (define-function void* gtk_scrolled_window_get_vadjustment (void*))

  ;; GtkWidget* gtk_scrolled_window_get_vscrollbar (GtkScrolledWindow* scrolled_window)
  (define-function void* gtk_scrolled_window_get_vscrollbar (void*))

  ;; GtkWidget* gtk_scrolled_window_new (GtkAdjustment* hadjustment, GtkAdjustment* vadjustment)
  (define-function void* gtk_scrolled_window_new (void* void*))

  ;; void gtk_scrolled_window_set_hadjustment (GtkScrolledWindow* scrolled_window, GtkAdjustment* hadjustment)
  (define-function void gtk_scrolled_window_set_hadjustment (void* void*))

  ;; void gtk_scrolled_window_set_placement (GtkScrolledWindow* scrolled_window, GtkCornerType window_placement)
  (define-function void gtk_scrolled_window_set_placement (void* int))

  ;; void gtk_scrolled_window_set_policy (GtkScrolledWindow* scrolled_window, GtkPolicyType hscrollbar_policy, GtkPolicyType vscrollbar_policy)
  (define-function void gtk_scrolled_window_set_policy (void* int int))

  ;; void gtk_scrolled_window_set_shadow_type (GtkScrolledWindow* scrolled_window, GtkShadowType type)
  (define-function void gtk_scrolled_window_set_shadow_type (void* int))

  ;; void gtk_scrolled_window_set_vadjustment (GtkScrolledWindow* scrolled_window, GtkAdjustment* vadjustment)
  (define-function void gtk_scrolled_window_set_vadjustment (void* void*))

  ;; void gtk_scrolled_window_unset_placement (GtkScrolledWindow* scrolled_window)
  (define-function void gtk_scrolled_window_unset_placement (void*))

  ) ;[end]
