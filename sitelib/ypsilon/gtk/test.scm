#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk test)

  (export gtk_test_create_simple_window
          gtk_test_create_widget
          gtk_test_display_button_window
          gtk_test_find_label
          gtk_test_find_sibling
          gtk_test_find_widget
          gtk_test_init
          gtk_test_list_all_types
          gtk_test_register_all_types
          gtk_test_slider_get_value
          gtk_test_slider_set_perc
          gtk_test_spin_button_click
          gtk_test_text_get
          gtk_test_text_set
          gtk_test_widget_click
          gtk_test_widget_send_key)

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

  ;; GtkWidget* gtk_test_create_simple_window (const gchar* window_title, const gchar* dialog_text)
  (define-function void* gtk_test_create_simple_window (char* char*))

  ;; GtkWidget* gtk_test_create_widget (GType widget_type, const gchar* first_property_name, ...)
  (define-function void* gtk_test_create_widget (unsigned-long char* ...))

  ;; GtkWidget* gtk_test_display_button_window (const gchar* window_title, const gchar* dialog_text, ...)
  (define-function void* gtk_test_display_button_window (char* char* ...))

  ;; GtkWidget* gtk_test_find_label (GtkWidget* widget, const gchar* label_pattern)
  (define-function void* gtk_test_find_label (void* char*))

  ;; GtkWidget* gtk_test_find_sibling (GtkWidget* base_widget, GType widget_type)
  (define-function void* gtk_test_find_sibling (void* unsigned-long))

  ;; GtkWidget* gtk_test_find_widget (GtkWidget* widget, const gchar* label_pattern, GType widget_type)
  (define-function void* gtk_test_find_widget (void* char* unsigned-long))

  ;; void gtk_test_init (int* argcp, char** *argvp, ...)
  (define-function void gtk_test_init (void* void* ...))

  ;; const GType* gtk_test_list_all_types (guint* n_types)
  (define-function void* gtk_test_list_all_types (void*))

  ;; void gtk_test_register_all_types (void)
  (define-function void gtk_test_register_all_types ())

  ;; double gtk_test_slider_get_value (GtkWidget* widget)
  (define-function double gtk_test_slider_get_value (void*))

  ;; void gtk_test_slider_set_perc (GtkWidget* widget, double percentage)
  (define-function void gtk_test_slider_set_perc (void* double))

  ;; gboolean gtk_test_spin_button_click (GtkSpinButton* spinner, guint button, gboolean upwards)
  (define-function int gtk_test_spin_button_click (void* unsigned-int int))

  ;; gchar* gtk_test_text_get (GtkWidget* widget)
  (define-function char* gtk_test_text_get (void*))

  ;; void gtk_test_text_set (GtkWidget* widget, const gchar* string)
  (define-function void gtk_test_text_set (void* char*))

  ;; gboolean gtk_test_widget_click (GtkWidget* widget, guint button, GdkModifierType modifiers)
  (define-function int gtk_test_widget_click (void* unsigned-int int))

  ;; gboolean gtk_test_widget_send_key (GtkWidget* widget, guint keyval, GdkModifierType modifiers)
  (define-function int gtk_test_widget_send_key (void* unsigned-int int))

  ) ;[end]
