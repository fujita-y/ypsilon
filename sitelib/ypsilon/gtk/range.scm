#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk range)

  (export gtk_range_get_adjustment
          gtk_range_get_fill_level
          gtk_range_get_inverted
          gtk_range_get_lower_stepper_sensitivity
          gtk_range_get_restrict_to_fill_level
          gtk_range_get_show_fill_level
          gtk_range_get_type
          gtk_range_get_update_policy
          gtk_range_get_upper_stepper_sensitivity
          gtk_range_get_value
          gtk_range_set_adjustment
          gtk_range_set_fill_level
          gtk_range_set_increments
          gtk_range_set_inverted
          gtk_range_set_lower_stepper_sensitivity
          gtk_range_set_range
          gtk_range_set_restrict_to_fill_level
          gtk_range_set_show_fill_level
          gtk_range_set_update_policy
          gtk_range_set_upper_stepper_sensitivity
          gtk_range_set_value)

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

  (define-syntax define-function/va_list
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "va_list argument not supported"))))))

  ;; GtkAdjustment* gtk_range_get_adjustment (GtkRange* range)
  (define-function void* gtk_range_get_adjustment (void*))

  ;; gdouble gtk_range_get_fill_level (GtkRange* range)
  (define-function double gtk_range_get_fill_level (void*))

  ;; gboolean gtk_range_get_inverted (GtkRange* range)
  (define-function int gtk_range_get_inverted (void*))

  ;; GtkSensitivityType gtk_range_get_lower_stepper_sensitivity (GtkRange* range)
  (define-function int gtk_range_get_lower_stepper_sensitivity (void*))

  ;; gboolean gtk_range_get_restrict_to_fill_level (GtkRange* range)
  (define-function int gtk_range_get_restrict_to_fill_level (void*))

  ;; gboolean gtk_range_get_show_fill_level (GtkRange* range)
  (define-function int gtk_range_get_show_fill_level (void*))

  ;; GType gtk_range_get_type (void)
  (define-function unsigned-long gtk_range_get_type ())

  ;; GtkUpdateType gtk_range_get_update_policy (GtkRange* range)
  (define-function int gtk_range_get_update_policy (void*))

  ;; GtkSensitivityType gtk_range_get_upper_stepper_sensitivity (GtkRange* range)
  (define-function int gtk_range_get_upper_stepper_sensitivity (void*))

  ;; gdouble gtk_range_get_value (GtkRange* range)
  (define-function double gtk_range_get_value (void*))

  ;; void gtk_range_set_adjustment (GtkRange* range, GtkAdjustment* adjustment)
  (define-function void gtk_range_set_adjustment (void* void*))

  ;; void gtk_range_set_fill_level (GtkRange* range, gdouble fill_level)
  (define-function void gtk_range_set_fill_level (void* double))

  ;; void gtk_range_set_increments (GtkRange* range, gdouble step, gdouble page)
  (define-function void gtk_range_set_increments (void* double double))

  ;; void gtk_range_set_inverted (GtkRange* range, gboolean setting)
  (define-function void gtk_range_set_inverted (void* int))

  ;; void gtk_range_set_lower_stepper_sensitivity (GtkRange* range, GtkSensitivityType sensitivity)
  (define-function void gtk_range_set_lower_stepper_sensitivity (void* int))

  ;; void gtk_range_set_range (GtkRange* range, gdouble min, gdouble max)
  (define-function void gtk_range_set_range (void* double double))

  ;; void gtk_range_set_restrict_to_fill_level (GtkRange* range, gboolean restrict_to_fill_level)
  (define-function void gtk_range_set_restrict_to_fill_level (void* int))

  ;; void gtk_range_set_show_fill_level (GtkRange* range, gboolean show_fill_level)
  (define-function void gtk_range_set_show_fill_level (void* int))

  ;; void gtk_range_set_update_policy (GtkRange* range, GtkUpdateType policy)
  (define-function void gtk_range_set_update_policy (void* int))

  ;; void gtk_range_set_upper_stepper_sensitivity (GtkRange* range, GtkSensitivityType sensitivity)
  (define-function void gtk_range_set_upper_stepper_sensitivity (void* int))

  ;; void gtk_range_set_value (GtkRange* range, gdouble value)
  (define-function void gtk_range_set_value (void* double))

  ) ;[end]
