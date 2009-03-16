#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk expander)

  (export gtk_expander_get_expanded
          gtk_expander_get_label
          gtk_expander_get_label_widget
          gtk_expander_get_spacing
          gtk_expander_get_type
          gtk_expander_get_use_markup
          gtk_expander_get_use_underline
          gtk_expander_new
          gtk_expander_new_with_mnemonic
          gtk_expander_set_expanded
          gtk_expander_set_label
          gtk_expander_set_label_widget
          gtk_expander_set_spacing
          gtk_expander_set_use_markup
          gtk_expander_set_use_underline
          gtk_expander_style_get_type)

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

  ;; gboolean gtk_expander_get_expanded (GtkExpander* expander)
  (define-function int gtk_expander_get_expanded (void*))

  ;; const gchar* gtk_expander_get_label (GtkExpander* expander)
  (define-function char* gtk_expander_get_label (void*))

  ;; GtkWidget* gtk_expander_get_label_widget (GtkExpander* expander)
  (define-function void* gtk_expander_get_label_widget (void*))

  ;; gint gtk_expander_get_spacing (GtkExpander* expander)
  (define-function int gtk_expander_get_spacing (void*))

  ;; GType gtk_expander_get_type (void)
  (define-function unsigned-long gtk_expander_get_type ())

  ;; gboolean gtk_expander_get_use_markup (GtkExpander* expander)
  (define-function int gtk_expander_get_use_markup (void*))

  ;; gboolean gtk_expander_get_use_underline (GtkExpander* expander)
  (define-function int gtk_expander_get_use_underline (void*))

  ;; GtkWidget* gtk_expander_new (const gchar* label)
  (define-function void* gtk_expander_new (char*))

  ;; GtkWidget* gtk_expander_new_with_mnemonic (const gchar* label)
  (define-function void* gtk_expander_new_with_mnemonic (char*))

  ;; void gtk_expander_set_expanded (GtkExpander* expander, gboolean expanded)
  (define-function void gtk_expander_set_expanded (void* int))

  ;; void gtk_expander_set_label (GtkExpander* expander, const gchar* label)
  (define-function void gtk_expander_set_label (void* char*))

  ;; void gtk_expander_set_label_widget (GtkExpander* expander, GtkWidget* label_widget)
  (define-function void gtk_expander_set_label_widget (void* void*))

  ;; void gtk_expander_set_spacing (GtkExpander* expander, gint spacing)
  (define-function void gtk_expander_set_spacing (void* int))

  ;; void gtk_expander_set_use_markup (GtkExpander* expander, gboolean use_markup)
  (define-function void gtk_expander_set_use_markup (void* int))

  ;; void gtk_expander_set_use_underline (GtkExpander* expander, gboolean use_underline)
  (define-function void gtk_expander_set_use_underline (void* int))

  ;; GType gtk_expander_style_get_type (void)
  (define-function unsigned-long gtk_expander_style_get_type ())

  ) ;[end]
