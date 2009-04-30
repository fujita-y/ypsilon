#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk progress)

  (export gtk_progress_bar_get_ellipsize
          gtk_progress_bar_get_fraction
          gtk_progress_bar_get_orientation
          gtk_progress_bar_get_pulse_step
          gtk_progress_bar_get_text
          gtk_progress_bar_get_type
          gtk_progress_bar_new
          gtk_progress_bar_orientation_get_type
          gtk_progress_bar_pulse
          gtk_progress_bar_set_ellipsize
          gtk_progress_bar_set_fraction
          gtk_progress_bar_set_orientation
          gtk_progress_bar_set_pulse_step
          gtk_progress_bar_set_text
          gtk_progress_bar_style_get_type)

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

  ;; PangoEllipsizeMode gtk_progress_bar_get_ellipsize (GtkProgressBar* pbar)
  (define-function int gtk_progress_bar_get_ellipsize (void*))

  ;; gdouble gtk_progress_bar_get_fraction (GtkProgressBar* pbar)
  (define-function double gtk_progress_bar_get_fraction (void*))

  ;; GtkProgressBarOrientation gtk_progress_bar_get_orientation (GtkProgressBar* pbar)
  (define-function int gtk_progress_bar_get_orientation (void*))

  ;; gdouble gtk_progress_bar_get_pulse_step (GtkProgressBar* pbar)
  (define-function double gtk_progress_bar_get_pulse_step (void*))

  ;; const gchar* gtk_progress_bar_get_text (GtkProgressBar* pbar)
  (define-function char* gtk_progress_bar_get_text (void*))

  ;; GType gtk_progress_bar_get_type (void)
  (define-function unsigned-long gtk_progress_bar_get_type ())

  ;; GtkWidget* gtk_progress_bar_new (void)
  (define-function void* gtk_progress_bar_new ())

  ;; GType gtk_progress_bar_orientation_get_type (void)
  (define-function unsigned-long gtk_progress_bar_orientation_get_type ())

  ;; void gtk_progress_bar_pulse (GtkProgressBar* pbar)
  (define-function void gtk_progress_bar_pulse (void*))

  ;; void gtk_progress_bar_set_ellipsize (GtkProgressBar* pbar, PangoEllipsizeMode mode)
  (define-function void gtk_progress_bar_set_ellipsize (void* int))

  ;; void gtk_progress_bar_set_fraction (GtkProgressBar* pbar, gdouble fraction)
  (define-function void gtk_progress_bar_set_fraction (void* double))

  ;; void gtk_progress_bar_set_orientation (GtkProgressBar* pbar, GtkProgressBarOrientation orientation)
  (define-function void gtk_progress_bar_set_orientation (void* int))

  ;; void gtk_progress_bar_set_pulse_step (GtkProgressBar* pbar, gdouble fraction)
  (define-function void gtk_progress_bar_set_pulse_step (void* double))

  ;; void gtk_progress_bar_set_text (GtkProgressBar* pbar, const gchar* text)
  (define-function void gtk_progress_bar_set_text (void* char*))

  ;; GType gtk_progress_bar_style_get_type (void)
  (define-function unsigned-long gtk_progress_bar_style_get_type ())

  ) ;[end]
