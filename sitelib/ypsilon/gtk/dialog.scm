#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk dialog)

  (export gtk_dialog_add_action_widget
          gtk_dialog_add_button
          gtk_dialog_add_buttons
          gtk_dialog_flags_get_type
          gtk_dialog_get_action_area
          gtk_dialog_get_content_area
          gtk_dialog_get_has_separator
          gtk_dialog_get_response_for_widget
          gtk_dialog_get_type
          gtk_dialog_new
          gtk_dialog_new_with_buttons
          gtk_dialog_response
          gtk_dialog_run
          gtk_dialog_set_alternative_button_order
          gtk_dialog_set_alternative_button_order_from_array
          gtk_dialog_set_default_response
          gtk_dialog_set_has_separator
          gtk_dialog_set_response_sensitive)

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

  ;; void gtk_dialog_add_action_widget (GtkDialog* dialog, GtkWidget* child, gint response_id)
  (define-function void gtk_dialog_add_action_widget (void* void* int))

  ;; GtkWidget* gtk_dialog_add_button (GtkDialog* dialog, const gchar* button_text, gint response_id)
  (define-function void* gtk_dialog_add_button (void* char* int))

  ;; void gtk_dialog_add_buttons (GtkDialog* dialog, const gchar* first_button_text, ...)
  (define-variadic-function void gtk_dialog_add_buttons (void* char* ...))

  ;; GType gtk_dialog_flags_get_type (void)
  (define-function unsigned-long gtk_dialog_flags_get_type ())

  ;; GtkWidget* gtk_dialog_get_action_area (GtkDialog* dialog)
  (define-function void* gtk_dialog_get_action_area (void*))

  ;; GtkWidget* gtk_dialog_get_content_area (GtkDialog* dialog)
  (define-function void* gtk_dialog_get_content_area (void*))

  ;; gboolean gtk_dialog_get_has_separator (GtkDialog* dialog)
  (define-function int gtk_dialog_get_has_separator (void*))

  ;; gint gtk_dialog_get_response_for_widget (GtkDialog* dialog, GtkWidget* widget)
  (define-function int gtk_dialog_get_response_for_widget (void* void*))

  ;; GType gtk_dialog_get_type (void)
  (define-function unsigned-long gtk_dialog_get_type ())

  ;; GtkWidget* gtk_dialog_new (void)
  (define-function void* gtk_dialog_new ())

  ;; GtkWidget* gtk_dialog_new_with_buttons (const gchar* title, GtkWindow* parent, GtkDialogFlags flags, const gchar* first_button_text, ...)
  (define-variadic-function void* gtk_dialog_new_with_buttons (char* void* int char* ...))

  ;; void gtk_dialog_response (GtkDialog* dialog, gint response_id)
  (define-function void gtk_dialog_response (void* int))

  ;; gint gtk_dialog_run (GtkDialog* dialog)
  (define-function int gtk_dialog_run (void*))

  ;; void gtk_dialog_set_alternative_button_order (GtkDialog* dialog, gint first_response_id, ...)
  (define-variadic-function void gtk_dialog_set_alternative_button_order (void* int ...))

  ;; void gtk_dialog_set_alternative_button_order_from_array (GtkDialog* dialog, gint n_params, gint* new_order)
  (define-function void gtk_dialog_set_alternative_button_order_from_array (void* int void*))

  ;; void gtk_dialog_set_default_response (GtkDialog* dialog, gint response_id)
  (define-function void gtk_dialog_set_default_response (void* int))

  ;; void gtk_dialog_set_has_separator (GtkDialog* dialog, gboolean setting)
  (define-function void gtk_dialog_set_has_separator (void* int))

  ;; void gtk_dialog_set_response_sensitive (GtkDialog* dialog, gint response_id, gboolean setting)
  (define-function void gtk_dialog_set_response_sensitive (void* int int))

  ) ;[end]
