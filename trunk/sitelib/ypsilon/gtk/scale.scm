#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk scale)

  (export gtk_scale_button_get_adjustment
          gtk_scale_button_get_minus_button
          gtk_scale_button_get_orientation
          gtk_scale_button_get_plus_button
          gtk_scale_button_get_popup
          gtk_scale_button_get_type
          gtk_scale_button_get_value
          gtk_scale_button_new
          gtk_scale_button_set_adjustment
          gtk_scale_button_set_icons
          gtk_scale_button_set_orientation
          gtk_scale_button_set_value
          gtk_scale_get_digits
          gtk_scale_get_draw_value
          gtk_scale_get_layout
          gtk_scale_get_layout_offsets
          gtk_scale_get_type
          gtk_scale_get_value_pos
          gtk_scale_set_digits
          gtk_scale_set_draw_value
          gtk_scale_set_value_pos)

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

  ;; GtkAdjustment* gtk_scale_button_get_adjustment (GtkScaleButton* button)
  (define-function void* gtk_scale_button_get_adjustment (void*))

  ;; GtkWidget* gtk_scale_button_get_minus_button (GtkScaleButton* button)
  (define-function void* gtk_scale_button_get_minus_button (void*))

  ;; GtkOrientation gtk_scale_button_get_orientation (GtkScaleButton* button)
  (define-function int gtk_scale_button_get_orientation (void*))

  ;; GtkWidget* gtk_scale_button_get_plus_button (GtkScaleButton* button)
  (define-function void* gtk_scale_button_get_plus_button (void*))

  ;; GtkWidget* gtk_scale_button_get_popup (GtkScaleButton* button)
  (define-function void* gtk_scale_button_get_popup (void*))

  ;; GType gtk_scale_button_get_type (void)
  (define-function unsigned-long gtk_scale_button_get_type ())

  ;; gdouble gtk_scale_button_get_value (GtkScaleButton* button)
  (define-function double gtk_scale_button_get_value (void*))

  ;; GtkWidget* gtk_scale_button_new (GtkIconSize size, gdouble min, gdouble max, gdouble step, const gchar** icons)
  (define-function void* gtk_scale_button_new (int double double double void*))

  ;; void gtk_scale_button_set_adjustment (GtkScaleButton* button, GtkAdjustment* adjustment)
  (define-function void gtk_scale_button_set_adjustment (void* void*))

  ;; void gtk_scale_button_set_icons (GtkScaleButton* button, const gchar** icons)
  (define-function void gtk_scale_button_set_icons (void* void*))

  ;; void gtk_scale_button_set_orientation (GtkScaleButton* button, GtkOrientation orientation)
  (define-function void gtk_scale_button_set_orientation (void* int))

  ;; void gtk_scale_button_set_value (GtkScaleButton* button, gdouble value)
  (define-function void gtk_scale_button_set_value (void* double))

  ;; gint gtk_scale_get_digits (GtkScale* scale)
  (define-function int gtk_scale_get_digits (void*))

  ;; gboolean gtk_scale_get_draw_value (GtkScale* scale)
  (define-function int gtk_scale_get_draw_value (void*))

  ;; PangoLayout* gtk_scale_get_layout (GtkScale* scale)
  (define-function void* gtk_scale_get_layout (void*))

  ;; void gtk_scale_get_layout_offsets (GtkScale* scale, gint* x, gint* y)
  (define-function void gtk_scale_get_layout_offsets (void* void* void*))

  ;; GType gtk_scale_get_type (void)
  (define-function unsigned-long gtk_scale_get_type ())

  ;; GtkPositionType gtk_scale_get_value_pos (GtkScale* scale)
  (define-function int gtk_scale_get_value_pos (void*))

  ;; void gtk_scale_set_digits (GtkScale* scale, gint digits)
  (define-function void gtk_scale_set_digits (void* int))

  ;; void gtk_scale_set_draw_value (GtkScale* scale, gboolean draw_value)
  (define-function void gtk_scale_set_draw_value (void* int))

  ;; void gtk_scale_set_value_pos (GtkScale* scale, GtkPositionType pos)
  (define-function void gtk_scale_set_value_pos (void* int))

  ) ;[end]
