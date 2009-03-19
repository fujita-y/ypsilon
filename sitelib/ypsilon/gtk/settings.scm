#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk settings)

  (export gtk_settings_get_default
          gtk_settings_get_for_screen
          gtk_settings_get_type
          gtk_settings_install_property
          gtk_settings_install_property_parser
          gtk_settings_set_double_property
          gtk_settings_set_long_property
          gtk_settings_set_property_value
          gtk_settings_set_string_property)

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

  ;; GtkSettings* gtk_settings_get_default (void)
  (define-function void* gtk_settings_get_default ())

  ;; GtkSettings* gtk_settings_get_for_screen (GdkScreen* screen)
  (define-function void* gtk_settings_get_for_screen (void*))

  ;; GType gtk_settings_get_type (void)
  (define-function unsigned-long gtk_settings_get_type ())

  ;; void gtk_settings_install_property (GParamSpec* pspec)
  (define-function void gtk_settings_install_property (void*))

  ;; void gtk_settings_install_property_parser (GParamSpec* pspec, GtkRcPropertyParser parser)
  (define-function void gtk_settings_install_property_parser (void* (c-callback int (void* void* void*))))

  ;; void gtk_settings_set_double_property (GtkSettings* settings, const gchar* name, gdouble v_double, const gchar* origin)
  (define-function void gtk_settings_set_double_property (void* char* double char*))

  ;; void gtk_settings_set_long_property (GtkSettings* settings, const gchar* name, glong v_long, const gchar* origin)
  (define-function void gtk_settings_set_long_property (void* char* long char*))

  ;; void gtk_settings_set_property_value (GtkSettings* settings, const gchar* name, const GtkSettingsValue* svalue)
  (define-function void gtk_settings_set_property_value (void* char* void*))

  ;; void gtk_settings_set_string_property (GtkSettings* settings, const gchar* name, const gchar* v_string, const gchar* origin)
  (define-function void gtk_settings_set_string_property (void* char* char* char*))

  ) ;[end]
