#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk label)

  (export gtk_label_get_angle
          gtk_label_get_attributes
          gtk_label_get_ellipsize
          gtk_label_get_justify
          gtk_label_get_label
          gtk_label_get_layout
          gtk_label_get_layout_offsets
          gtk_label_get_line_wrap
          gtk_label_get_line_wrap_mode
          gtk_label_get_max_width_chars
          gtk_label_get_mnemonic_keyval
          gtk_label_get_mnemonic_widget
          gtk_label_get_selectable
          gtk_label_get_selection_bounds
          gtk_label_get_single_line_mode
          gtk_label_get_text
          gtk_label_get_type
          gtk_label_get_use_markup
          gtk_label_get_use_underline
          gtk_label_get_width_chars
          gtk_label_new
          gtk_label_new_with_mnemonic
          gtk_label_select_region
          gtk_label_set_angle
          gtk_label_set_attributes
          gtk_label_set_ellipsize
          gtk_label_set_justify
          gtk_label_set_label
          gtk_label_set_line_wrap
          gtk_label_set_line_wrap_mode
          gtk_label_set_markup
          gtk_label_set_markup_with_mnemonic
          gtk_label_set_max_width_chars
          gtk_label_set_mnemonic_widget
          gtk_label_set_pattern
          gtk_label_set_selectable
          gtk_label_set_single_line_mode
          gtk_label_set_text
          gtk_label_set_text_with_mnemonic
          gtk_label_set_use_markup
          gtk_label_set_use_underline
          gtk_label_set_width_chars)

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

  ;; gdouble gtk_label_get_angle (GtkLabel* label)
  (define-function double gtk_label_get_angle (void*))

  ;; PangoAttrList* gtk_label_get_attributes (GtkLabel* label)
  (define-function void* gtk_label_get_attributes (void*))

  ;; PangoEllipsizeMode gtk_label_get_ellipsize (GtkLabel* label)
  (define-function int gtk_label_get_ellipsize (void*))

  ;; GtkJustification gtk_label_get_justify (GtkLabel* label)
  (define-function int gtk_label_get_justify (void*))

  ;; const gchar* gtk_label_get_label (GtkLabel* label)
  (define-function char* gtk_label_get_label (void*))

  ;; PangoLayout* gtk_label_get_layout (GtkLabel* label)
  (define-function void* gtk_label_get_layout (void*))

  ;; void gtk_label_get_layout_offsets (GtkLabel* label, gint* x, gint* y)
  (define-function void gtk_label_get_layout_offsets (void* void* void*))

  ;; gboolean gtk_label_get_line_wrap (GtkLabel* label)
  (define-function int gtk_label_get_line_wrap (void*))

  ;; PangoWrapMode gtk_label_get_line_wrap_mode (GtkLabel* label)
  (define-function int gtk_label_get_line_wrap_mode (void*))

  ;; gint gtk_label_get_max_width_chars (GtkLabel* label)
  (define-function int gtk_label_get_max_width_chars (void*))

  ;; guint gtk_label_get_mnemonic_keyval (GtkLabel* label)
  (define-function unsigned-int gtk_label_get_mnemonic_keyval (void*))

  ;; GtkWidget* gtk_label_get_mnemonic_widget (GtkLabel* label)
  (define-function void* gtk_label_get_mnemonic_widget (void*))

  ;; gboolean gtk_label_get_selectable (GtkLabel* label)
  (define-function int gtk_label_get_selectable (void*))

  ;; gboolean gtk_label_get_selection_bounds (GtkLabel* label, gint* start, gint* end)
  (define-function int gtk_label_get_selection_bounds (void* void* void*))

  ;; gboolean gtk_label_get_single_line_mode (GtkLabel* label)
  (define-function int gtk_label_get_single_line_mode (void*))

  ;; const gchar* gtk_label_get_text (GtkLabel* label)
  (define-function char* gtk_label_get_text (void*))

  ;; GType gtk_label_get_type (void)
  (define-function unsigned-long gtk_label_get_type ())

  ;; gboolean gtk_label_get_use_markup (GtkLabel* label)
  (define-function int gtk_label_get_use_markup (void*))

  ;; gboolean gtk_label_get_use_underline (GtkLabel* label)
  (define-function int gtk_label_get_use_underline (void*))

  ;; gint gtk_label_get_width_chars (GtkLabel* label)
  (define-function int gtk_label_get_width_chars (void*))

  ;; GtkWidget* gtk_label_new (const gchar* str)
  (define-function void* gtk_label_new (char*))

  ;; GtkWidget* gtk_label_new_with_mnemonic (const gchar* str)
  (define-function void* gtk_label_new_with_mnemonic (char*))

  ;; void gtk_label_select_region (GtkLabel* label, gint start_offset, gint end_offset)
  (define-function void gtk_label_select_region (void* int int))

  ;; void gtk_label_set_angle (GtkLabel* label, gdouble angle)
  (define-function void gtk_label_set_angle (void* double))

  ;; void gtk_label_set_attributes (GtkLabel* label, PangoAttrList* attrs)
  (define-function void gtk_label_set_attributes (void* void*))

  ;; void gtk_label_set_ellipsize (GtkLabel* label, PangoEllipsizeMode mode)
  (define-function void gtk_label_set_ellipsize (void* int))

  ;; void gtk_label_set_justify (GtkLabel* label, GtkJustification jtype)
  (define-function void gtk_label_set_justify (void* int))

  ;; void gtk_label_set_label (GtkLabel* label, const gchar* str)
  (define-function void gtk_label_set_label (void* char*))

  ;; void gtk_label_set_line_wrap (GtkLabel* label, gboolean wrap)
  (define-function void gtk_label_set_line_wrap (void* int))

  ;; void gtk_label_set_line_wrap_mode (GtkLabel* label, PangoWrapMode wrap_mode)
  (define-function void gtk_label_set_line_wrap_mode (void* int))

  ;; void gtk_label_set_markup (GtkLabel* label, const gchar* str)
  (define-function void gtk_label_set_markup (void* char*))

  ;; void gtk_label_set_markup_with_mnemonic (GtkLabel* label, const gchar* str)
  (define-function void gtk_label_set_markup_with_mnemonic (void* char*))

  ;; void gtk_label_set_max_width_chars (GtkLabel* label, gint n_chars)
  (define-function void gtk_label_set_max_width_chars (void* int))

  ;; void gtk_label_set_mnemonic_widget (GtkLabel* label, GtkWidget* widget)
  (define-function void gtk_label_set_mnemonic_widget (void* void*))

  ;; void gtk_label_set_pattern (GtkLabel* label, const gchar* pattern)
  (define-function void gtk_label_set_pattern (void* char*))

  ;; void gtk_label_set_selectable (GtkLabel* label, gboolean setting)
  (define-function void gtk_label_set_selectable (void* int))

  ;; void gtk_label_set_single_line_mode (GtkLabel* label, gboolean single_line_mode)
  (define-function void gtk_label_set_single_line_mode (void* int))

  ;; void gtk_label_set_text (GtkLabel* label, const gchar* str)
  (define-function void gtk_label_set_text (void* char*))

  ;; void gtk_label_set_text_with_mnemonic (GtkLabel* label, const gchar* str)
  (define-function void gtk_label_set_text_with_mnemonic (void* char*))

  ;; void gtk_label_set_use_markup (GtkLabel* label, gboolean setting)
  (define-function void gtk_label_set_use_markup (void* int))

  ;; void gtk_label_set_use_underline (GtkLabel* label, gboolean setting)
  (define-function void gtk_label_set_use_underline (void* int))

  ;; void gtk_label_set_width_chars (GtkLabel* label, gint n_chars)
  (define-function void gtk_label_set_width_chars (void* int))

  ) ;[end]
