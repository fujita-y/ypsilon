#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk builder)

  (export gtk_builder_add_from_file
          gtk_builder_add_from_string
          gtk_builder_add_objects_from_file
          gtk_builder_add_objects_from_string
          gtk_builder_connect_signals
          gtk_builder_connect_signals_full
          gtk_builder_error_get_type
          gtk_builder_error_quark
          gtk_builder_get_object
          gtk_builder_get_objects
          gtk_builder_get_translation_domain
          gtk_builder_get_type
          gtk_builder_get_type_from_name
          gtk_builder_new
          gtk_builder_set_translation_domain
          gtk_builder_value_from_string
          gtk_builder_value_from_string_type)

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

  ;; guint gtk_builder_add_from_file (GtkBuilder* builder, const gchar* filename, GError** error)
  (define-function unsigned-int gtk_builder_add_from_file (void* char* void*))

  ;; guint gtk_builder_add_from_string (GtkBuilder* builder, const gchar* buffer, gsize length, GError** error)
  (define-function unsigned-int gtk_builder_add_from_string (void* char* unsigned-long void*))

  ;; guint gtk_builder_add_objects_from_file (GtkBuilder* builder, const gchar* filename, gchar** object_ids, GError** error)
  (define-function unsigned-int gtk_builder_add_objects_from_file (void* char* void* void*))

  ;; guint gtk_builder_add_objects_from_string (GtkBuilder* builder, const gchar* buffer, gsize length, gchar** object_ids, GError** error)
  (define-function unsigned-int gtk_builder_add_objects_from_string (void* char* unsigned-long void* void*))

  ;; void gtk_builder_connect_signals (GtkBuilder* builder, gpointer user_data)
  (define-function void gtk_builder_connect_signals (void* void*))

  ;; void gtk_builder_connect_signals_full (GtkBuilder* builder, GtkBuilderConnectFunc func, gpointer user_data)
  (define-function void gtk_builder_connect_signals_full (void* (c-callback void (void* void* void* void* void* int void*)) void*))

  ;; GType gtk_builder_error_get_type (void)
  (define-function unsigned-long gtk_builder_error_get_type ())

  ;; GQuark gtk_builder_error_quark (void)
  (define-function uint32_t gtk_builder_error_quark ())

  ;; GObject* gtk_builder_get_object (GtkBuilder* builder, const gchar* name)
  (define-function void* gtk_builder_get_object (void* char*))

  ;; GSList* gtk_builder_get_objects (GtkBuilder* builder)
  (define-function void* gtk_builder_get_objects (void*))

  ;; const gchar* gtk_builder_get_translation_domain (GtkBuilder* builder)
  (define-function char* gtk_builder_get_translation_domain (void*))

  ;; GType gtk_builder_get_type (void)
  (define-function unsigned-long gtk_builder_get_type ())

  ;; GType gtk_builder_get_type_from_name (GtkBuilder* builder, const char* type_name)
  (define-function unsigned-long gtk_builder_get_type_from_name (void* char*))

  ;; GtkBuilder* gtk_builder_new (void)
  (define-function void* gtk_builder_new ())

  ;; void gtk_builder_set_translation_domain (GtkBuilder* builder, const gchar* domain)
  (define-function void gtk_builder_set_translation_domain (void* char*))

  ;; gboolean gtk_builder_value_from_string (GtkBuilder* builder, GParamSpec* pspec, const gchar* string, GValue* value, GError** error)
  (define-function int gtk_builder_value_from_string (void* void* char* void* void*))

  ;; gboolean gtk_builder_value_from_string_type (GtkBuilder* builder, GType type, const gchar* string, GValue* value, GError** error)
  (define-function int gtk_builder_value_from_string_type (void* unsigned-long char* void* void*))

  ) ;[end]
