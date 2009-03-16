#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk rc)

  (export gtk_rc_add_default_file
          gtk_rc_find_module_in_path
          gtk_rc_find_pixmap_in_path
          gtk_rc_flags_get_type
          gtk_rc_get_default_files
          gtk_rc_get_im_module_file
          gtk_rc_get_im_module_path
          gtk_rc_get_module_dir
          gtk_rc_get_style
          gtk_rc_get_style_by_paths
          gtk_rc_get_theme_dir
          gtk_rc_parse
          gtk_rc_parse_color
          gtk_rc_parse_color_full
          gtk_rc_parse_priority
          gtk_rc_parse_state
          gtk_rc_parse_string
          gtk_rc_property_parse_border
          gtk_rc_property_parse_color
          gtk_rc_property_parse_enum
          gtk_rc_property_parse_flags
          gtk_rc_property_parse_requisition
          gtk_rc_reparse_all
          gtk_rc_reparse_all_for_settings
          gtk_rc_reset_styles
          gtk_rc_scanner_new
          gtk_rc_set_default_files
          gtk_rc_style_copy
          gtk_rc_style_get_type
          gtk_rc_style_new
          gtk_rc_token_type_get_type)

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

  ;; void gtk_rc_add_default_file (const gchar* filename)
  (define-function void gtk_rc_add_default_file (char*))

  ;; gchar* gtk_rc_find_module_in_path (const gchar* module_file)
  (define-function char* gtk_rc_find_module_in_path (char*))

  ;; gchar* gtk_rc_find_pixmap_in_path (GtkSettings* settings, GScanner* scanner, const gchar* pixmap_file)
  (define-function char* gtk_rc_find_pixmap_in_path (void* void* char*))

  ;; GType gtk_rc_flags_get_type (void)
  (define-function unsigned-long gtk_rc_flags_get_type ())

  ;; gchar** gtk_rc_get_default_files (void)
  (define-function void* gtk_rc_get_default_files ())

  ;; gchar* gtk_rc_get_im_module_file (void)
  (define-function char* gtk_rc_get_im_module_file ())

  ;; gchar* gtk_rc_get_im_module_path (void)
  (define-function char* gtk_rc_get_im_module_path ())

  ;; gchar* gtk_rc_get_module_dir (void)
  (define-function char* gtk_rc_get_module_dir ())

  ;; GtkStyle* gtk_rc_get_style (GtkWidget* widget)
  (define-function void* gtk_rc_get_style (void*))

  ;; GtkStyle* gtk_rc_get_style_by_paths (GtkSettings* settings, const char* widget_path, const char* class_path, GType type)
  (define-function void* gtk_rc_get_style_by_paths (void* char* char* unsigned-long))

  ;; gchar* gtk_rc_get_theme_dir (void)
  (define-function char* gtk_rc_get_theme_dir ())

  ;; void gtk_rc_parse (const gchar* filename)
  (define-function void gtk_rc_parse (char*))

  ;; guint gtk_rc_parse_color (GScanner* scanner, GdkColor* color)
  (define-function unsigned-int gtk_rc_parse_color (void* void*))

  ;; guint gtk_rc_parse_color_full (GScanner* scanner, GtkRcStyle* style, GdkColor* color)
  (define-function unsigned-int gtk_rc_parse_color_full (void* void* void*))

  ;; guint gtk_rc_parse_priority (GScanner* scanner, GtkPathPriorityType* priority)
  (define-function unsigned-int gtk_rc_parse_priority (void* void*))

  ;; guint gtk_rc_parse_state (GScanner* scanner, GtkStateType* state)
  (define-function unsigned-int gtk_rc_parse_state (void* void*))

  ;; void gtk_rc_parse_string (const gchar* rc_string)
  (define-function void gtk_rc_parse_string (char*))

  ;; gboolean gtk_rc_property_parse_border (const GParamSpec* pspec, const GString* gstring, GValue* property_value)
  (define-function int gtk_rc_property_parse_border (void* void* void*))

  ;; gboolean gtk_rc_property_parse_color (const GParamSpec* pspec, const GString* gstring, GValue* property_value)
  (define-function int gtk_rc_property_parse_color (void* void* void*))

  ;; gboolean gtk_rc_property_parse_enum (const GParamSpec* pspec, const GString* gstring, GValue* property_value)
  (define-function int gtk_rc_property_parse_enum (void* void* void*))

  ;; gboolean gtk_rc_property_parse_flags (const GParamSpec* pspec, const GString* gstring, GValue* property_value)
  (define-function int gtk_rc_property_parse_flags (void* void* void*))

  ;; gboolean gtk_rc_property_parse_requisition (const GParamSpec* pspec, const GString* gstring, GValue* property_value)
  (define-function int gtk_rc_property_parse_requisition (void* void* void*))

  ;; gboolean gtk_rc_reparse_all (void)
  (define-function int gtk_rc_reparse_all ())

  ;; gboolean gtk_rc_reparse_all_for_settings (GtkSettings* settings, gboolean force_load)
  (define-function int gtk_rc_reparse_all_for_settings (void* int))

  ;; void gtk_rc_reset_styles (GtkSettings* settings)
  (define-function void gtk_rc_reset_styles (void*))

  ;; GScanner* gtk_rc_scanner_new (void)
  (define-function void* gtk_rc_scanner_new ())

  ;; void gtk_rc_set_default_files (gchar** filenames)
  (define-function void gtk_rc_set_default_files (void*))

  ;; GtkRcStyle* gtk_rc_style_copy (GtkRcStyle* orig)
  (define-function void* gtk_rc_style_copy (void*))

  ;; GType gtk_rc_style_get_type (void)
  (define-function unsigned-long gtk_rc_style_get_type ())

  ;; GtkRcStyle* gtk_rc_style_new (void)
  (define-function void* gtk_rc_style_new ())

  ;; GType gtk_rc_token_type_get_type (void)
  (define-function unsigned-long gtk_rc_token_type_get_type ())

  ) ;[end]
