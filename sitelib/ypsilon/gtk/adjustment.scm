#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk adjustment)

  (export gtk_adjustment_changed
          gtk_adjustment_clamp_page
          gtk_adjustment_configure
          gtk_adjustment_get_lower
          gtk_adjustment_get_page_increment
          gtk_adjustment_get_page_size
          gtk_adjustment_get_step_increment
          gtk_adjustment_get_type
          gtk_adjustment_get_upper
          gtk_adjustment_get_value
          gtk_adjustment_new
          gtk_adjustment_set_lower
          gtk_adjustment_set_page_increment
          gtk_adjustment_set_page_size
          gtk_adjustment_set_step_increment
          gtk_adjustment_set_upper
          gtk_adjustment_set_value
          gtk_adjustment_value_changed)

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

  ;; void gtk_adjustment_changed (GtkAdjustment* adjustment)
  (define-function void gtk_adjustment_changed (void*))

  ;; void gtk_adjustment_clamp_page (GtkAdjustment* adjustment, gdouble lower, gdouble upper)
  (define-function void gtk_adjustment_clamp_page (void* double double))

  ;; void gtk_adjustment_configure (GtkAdjustment* adjustment, gdouble value, gdouble lower, gdouble upper, gdouble step_increment, gdouble page_increment, gdouble page_size)
  (define-function void gtk_adjustment_configure (void* double double double double double double))

  ;; gdouble gtk_adjustment_get_lower (GtkAdjustment* adjustment)
  (define-function double gtk_adjustment_get_lower (void*))

  ;; gdouble gtk_adjustment_get_page_increment (GtkAdjustment* adjustment)
  (define-function double gtk_adjustment_get_page_increment (void*))

  ;; gdouble gtk_adjustment_get_page_size (GtkAdjustment* adjustment)
  (define-function double gtk_adjustment_get_page_size (void*))

  ;; gdouble gtk_adjustment_get_step_increment (GtkAdjustment* adjustment)
  (define-function double gtk_adjustment_get_step_increment (void*))

  ;; GType gtk_adjustment_get_type (void)
  (define-function unsigned-long gtk_adjustment_get_type ())

  ;; gdouble gtk_adjustment_get_upper (GtkAdjustment* adjustment)
  (define-function double gtk_adjustment_get_upper (void*))

  ;; gdouble gtk_adjustment_get_value (GtkAdjustment* adjustment)
  (define-function double gtk_adjustment_get_value (void*))

  ;; GtkObject* gtk_adjustment_new (gdouble value, gdouble lower, gdouble upper, gdouble step_increment, gdouble page_increment, gdouble page_size)
  (define-function void* gtk_adjustment_new (double double double double double double))

  ;; void gtk_adjustment_set_lower (GtkAdjustment* adjustment, gdouble lower)
  (define-function void gtk_adjustment_set_lower (void* double))

  ;; void gtk_adjustment_set_page_increment (GtkAdjustment* adjustment, gdouble page_increment)
  (define-function void gtk_adjustment_set_page_increment (void* double))

  ;; void gtk_adjustment_set_page_size (GtkAdjustment* adjustment, gdouble page_size)
  (define-function void gtk_adjustment_set_page_size (void* double))

  ;; void gtk_adjustment_set_step_increment (GtkAdjustment* adjustment, gdouble step_increment)
  (define-function void gtk_adjustment_set_step_increment (void* double))

  ;; void gtk_adjustment_set_upper (GtkAdjustment* adjustment, gdouble upper)
  (define-function void gtk_adjustment_set_upper (void* double))

  ;; void gtk_adjustment_set_value (GtkAdjustment* adjustment, gdouble value)
  (define-function void gtk_adjustment_set_value (void* double))

  ;; void gtk_adjustment_value_changed (GtkAdjustment* adjustment)
  (define-function void gtk_adjustment_value_changed (void*))

  ) ;[end]
