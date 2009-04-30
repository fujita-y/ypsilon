#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk spin)

  (export gtk_spin_button_configure
          gtk_spin_button_get_adjustment
          gtk_spin_button_get_digits
          gtk_spin_button_get_increments
          gtk_spin_button_get_numeric
          gtk_spin_button_get_range
          gtk_spin_button_get_snap_to_ticks
          gtk_spin_button_get_type
          gtk_spin_button_get_update_policy
          gtk_spin_button_get_value
          gtk_spin_button_get_value_as_int
          gtk_spin_button_get_wrap
          gtk_spin_button_new
          gtk_spin_button_new_with_range
          gtk_spin_button_set_adjustment
          gtk_spin_button_set_digits
          gtk_spin_button_set_increments
          gtk_spin_button_set_numeric
          gtk_spin_button_set_range
          gtk_spin_button_set_snap_to_ticks
          gtk_spin_button_set_update_policy
          gtk_spin_button_set_value
          gtk_spin_button_set_wrap
          gtk_spin_button_spin
          gtk_spin_button_update
          gtk_spin_button_update_policy_get_type
          gtk_spin_type_get_type)

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

  ;; void gtk_spin_button_configure (GtkSpinButton* spin_button, GtkAdjustment* adjustment, gdouble climb_rate, guint digits)
  (define-function void gtk_spin_button_configure (void* void* double unsigned-int))

  ;; GtkAdjustment* gtk_spin_button_get_adjustment (GtkSpinButton* spin_button)
  (define-function void* gtk_spin_button_get_adjustment (void*))

  ;; guint gtk_spin_button_get_digits (GtkSpinButton* spin_button)
  (define-function unsigned-int gtk_spin_button_get_digits (void*))

  ;; void gtk_spin_button_get_increments (GtkSpinButton* spin_button, gdouble* step, gdouble* page)
  (define-function void gtk_spin_button_get_increments (void* void* void*))

  ;; gboolean gtk_spin_button_get_numeric (GtkSpinButton* spin_button)
  (define-function int gtk_spin_button_get_numeric (void*))

  ;; void gtk_spin_button_get_range (GtkSpinButton* spin_button, gdouble* min, gdouble* max)
  (define-function void gtk_spin_button_get_range (void* void* void*))

  ;; gboolean gtk_spin_button_get_snap_to_ticks (GtkSpinButton* spin_button)
  (define-function int gtk_spin_button_get_snap_to_ticks (void*))

  ;; GType gtk_spin_button_get_type (void)
  (define-function unsigned-long gtk_spin_button_get_type ())

  ;; GtkSpinButtonUpdatePolicy gtk_spin_button_get_update_policy (GtkSpinButton* spin_button)
  (define-function int gtk_spin_button_get_update_policy (void*))

  ;; gdouble gtk_spin_button_get_value (GtkSpinButton* spin_button)
  (define-function double gtk_spin_button_get_value (void*))

  ;; gint gtk_spin_button_get_value_as_int (GtkSpinButton* spin_button)
  (define-function int gtk_spin_button_get_value_as_int (void*))

  ;; gboolean gtk_spin_button_get_wrap (GtkSpinButton* spin_button)
  (define-function int gtk_spin_button_get_wrap (void*))

  ;; GtkWidget* gtk_spin_button_new (GtkAdjustment* adjustment, gdouble climb_rate, guint digits)
  (define-function void* gtk_spin_button_new (void* double unsigned-int))

  ;; GtkWidget* gtk_spin_button_new_with_range (gdouble min, gdouble max, gdouble step)
  (define-function void* gtk_spin_button_new_with_range (double double double))

  ;; void gtk_spin_button_set_adjustment (GtkSpinButton* spin_button, GtkAdjustment* adjustment)
  (define-function void gtk_spin_button_set_adjustment (void* void*))

  ;; void gtk_spin_button_set_digits (GtkSpinButton* spin_button, guint digits)
  (define-function void gtk_spin_button_set_digits (void* unsigned-int))

  ;; void gtk_spin_button_set_increments (GtkSpinButton* spin_button, gdouble step, gdouble page)
  (define-function void gtk_spin_button_set_increments (void* double double))

  ;; void gtk_spin_button_set_numeric (GtkSpinButton* spin_button, gboolean numeric)
  (define-function void gtk_spin_button_set_numeric (void* int))

  ;; void gtk_spin_button_set_range (GtkSpinButton* spin_button, gdouble min, gdouble max)
  (define-function void gtk_spin_button_set_range (void* double double))

  ;; void gtk_spin_button_set_snap_to_ticks (GtkSpinButton* spin_button, gboolean snap_to_ticks)
  (define-function void gtk_spin_button_set_snap_to_ticks (void* int))

  ;; void gtk_spin_button_set_update_policy (GtkSpinButton* spin_button, GtkSpinButtonUpdatePolicy policy)
  (define-function void gtk_spin_button_set_update_policy (void* int))

  ;; void gtk_spin_button_set_value (GtkSpinButton* spin_button, gdouble value)
  (define-function void gtk_spin_button_set_value (void* double))

  ;; void gtk_spin_button_set_wrap (GtkSpinButton* spin_button, gboolean wrap)
  (define-function void gtk_spin_button_set_wrap (void* int))

  ;; void gtk_spin_button_spin (GtkSpinButton* spin_button, GtkSpinType direction, gdouble increment)
  (define-function void gtk_spin_button_spin (void* int double))

  ;; void gtk_spin_button_update (GtkSpinButton* spin_button)
  (define-function void gtk_spin_button_update (void*))

  ;; GType gtk_spin_button_update_policy_get_type (void)
  (define-function unsigned-long gtk_spin_button_update_policy_get_type ())

  ;; GType gtk_spin_type_get_type (void)
  (define-function unsigned-long gtk_spin_type_get_type ())

  ) ;[end]
