#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk font)

  (export gtk_font_button_get_font_name
          gtk_font_button_get_show_size
          gtk_font_button_get_show_style
          gtk_font_button_get_title
          gtk_font_button_get_type
          gtk_font_button_get_use_font
          gtk_font_button_get_use_size
          gtk_font_button_new
          gtk_font_button_new_with_font
          gtk_font_button_set_font_name
          gtk_font_button_set_show_size
          gtk_font_button_set_show_style
          gtk_font_button_set_title
          gtk_font_button_set_use_font
          gtk_font_button_set_use_size
          gtk_font_selection_dialog_get_apply_button
          gtk_font_selection_dialog_get_cancel_button
          gtk_font_selection_dialog_get_font_name
          gtk_font_selection_dialog_get_ok_button
          gtk_font_selection_dialog_get_preview_text
          gtk_font_selection_dialog_get_type
          gtk_font_selection_dialog_new
          gtk_font_selection_dialog_set_font_name
          gtk_font_selection_dialog_set_preview_text
          gtk_font_selection_get_face
          gtk_font_selection_get_face_list
          gtk_font_selection_get_family
          gtk_font_selection_get_family_list
          gtk_font_selection_get_font_name
          gtk_font_selection_get_preview_entry
          gtk_font_selection_get_preview_text
          gtk_font_selection_get_size
          gtk_font_selection_get_size_entry
          gtk_font_selection_get_size_list
          gtk_font_selection_get_type
          gtk_font_selection_new
          gtk_font_selection_set_font_name
          gtk_font_selection_set_preview_text)

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

  ;; const gchar* gtk_font_button_get_font_name (GtkFontButton* font_button)
  (define-function char* gtk_font_button_get_font_name (void*))

  ;; gboolean gtk_font_button_get_show_size (GtkFontButton* font_button)
  (define-function int gtk_font_button_get_show_size (void*))

  ;; gboolean gtk_font_button_get_show_style (GtkFontButton* font_button)
  (define-function int gtk_font_button_get_show_style (void*))

  ;; const gchar* gtk_font_button_get_title (GtkFontButton* font_button)
  (define-function char* gtk_font_button_get_title (void*))

  ;; GType gtk_font_button_get_type (void)
  (define-function unsigned-long gtk_font_button_get_type ())

  ;; gboolean gtk_font_button_get_use_font (GtkFontButton* font_button)
  (define-function int gtk_font_button_get_use_font (void*))

  ;; gboolean gtk_font_button_get_use_size (GtkFontButton* font_button)
  (define-function int gtk_font_button_get_use_size (void*))

  ;; GtkWidget* gtk_font_button_new (void)
  (define-function void* gtk_font_button_new ())

  ;; GtkWidget* gtk_font_button_new_with_font (const gchar* fontname)
  (define-function void* gtk_font_button_new_with_font (char*))

  ;; gboolean gtk_font_button_set_font_name (GtkFontButton* font_button, const gchar* fontname)
  (define-function int gtk_font_button_set_font_name (void* char*))

  ;; void gtk_font_button_set_show_size (GtkFontButton* font_button, gboolean show_size)
  (define-function void gtk_font_button_set_show_size (void* int))

  ;; void gtk_font_button_set_show_style (GtkFontButton* font_button, gboolean show_style)
  (define-function void gtk_font_button_set_show_style (void* int))

  ;; void gtk_font_button_set_title (GtkFontButton* font_button, const gchar* title)
  (define-function void gtk_font_button_set_title (void* char*))

  ;; void gtk_font_button_set_use_font (GtkFontButton* font_button, gboolean use_font)
  (define-function void gtk_font_button_set_use_font (void* int))

  ;; void gtk_font_button_set_use_size (GtkFontButton* font_button, gboolean use_size)
  (define-function void gtk_font_button_set_use_size (void* int))

  ;; GtkWidget* gtk_font_selection_dialog_get_apply_button (GtkFontSelectionDialog* fsd)
  (define-function void* gtk_font_selection_dialog_get_apply_button (void*))

  ;; GtkWidget* gtk_font_selection_dialog_get_cancel_button (GtkFontSelectionDialog* fsd)
  (define-function void* gtk_font_selection_dialog_get_cancel_button (void*))

  ;; gchar* gtk_font_selection_dialog_get_font_name (GtkFontSelectionDialog* fsd)
  (define-function char* gtk_font_selection_dialog_get_font_name (void*))

  ;; GtkWidget* gtk_font_selection_dialog_get_ok_button (GtkFontSelectionDialog* fsd)
  (define-function void* gtk_font_selection_dialog_get_ok_button (void*))

  ;; const gchar* gtk_font_selection_dialog_get_preview_text (GtkFontSelectionDialog* fsd)
  (define-function char* gtk_font_selection_dialog_get_preview_text (void*))

  ;; GType gtk_font_selection_dialog_get_type (void)
  (define-function unsigned-long gtk_font_selection_dialog_get_type ())

  ;; GtkWidget* gtk_font_selection_dialog_new (const gchar* title)
  (define-function void* gtk_font_selection_dialog_new (char*))

  ;; gboolean gtk_font_selection_dialog_set_font_name (GtkFontSelectionDialog* fsd, const gchar* fontname)
  (define-function int gtk_font_selection_dialog_set_font_name (void* char*))

  ;; void gtk_font_selection_dialog_set_preview_text (GtkFontSelectionDialog* fsd, const gchar* text)
  (define-function void gtk_font_selection_dialog_set_preview_text (void* char*))

  ;; PangoFontFace* gtk_font_selection_get_face (GtkFontSelection* fontsel)
  (define-function void* gtk_font_selection_get_face (void*))

  ;; GtkWidget* gtk_font_selection_get_face_list (GtkFontSelection* fontsel)
  (define-function void* gtk_font_selection_get_face_list (void*))

  ;; PangoFontFamily* gtk_font_selection_get_family (GtkFontSelection* fontsel)
  (define-function void* gtk_font_selection_get_family (void*))

  ;; GtkWidget* gtk_font_selection_get_family_list (GtkFontSelection* fontsel)
  (define-function void* gtk_font_selection_get_family_list (void*))

  ;; gchar* gtk_font_selection_get_font_name (GtkFontSelection* fontsel)
  (define-function char* gtk_font_selection_get_font_name (void*))

  ;; GtkWidget* gtk_font_selection_get_preview_entry (GtkFontSelection* fontsel)
  (define-function void* gtk_font_selection_get_preview_entry (void*))

  ;; const gchar* gtk_font_selection_get_preview_text (GtkFontSelection* fontsel)
  (define-function char* gtk_font_selection_get_preview_text (void*))

  ;; gint gtk_font_selection_get_size (GtkFontSelection* fontsel)
  (define-function int gtk_font_selection_get_size (void*))

  ;; GtkWidget* gtk_font_selection_get_size_entry (GtkFontSelection* fontsel)
  (define-function void* gtk_font_selection_get_size_entry (void*))

  ;; GtkWidget* gtk_font_selection_get_size_list (GtkFontSelection* fontsel)
  (define-function void* gtk_font_selection_get_size_list (void*))

  ;; GType gtk_font_selection_get_type (void)
  (define-function unsigned-long gtk_font_selection_get_type ())

  ;; GtkWidget* gtk_font_selection_new (void)
  (define-function void* gtk_font_selection_new ())

  ;; gboolean gtk_font_selection_set_font_name (GtkFontSelection* fontsel, const gchar* fontname)
  (define-function int gtk_font_selection_set_font_name (void* char*))

  ;; void gtk_font_selection_set_preview_text (GtkFontSelection* fontsel, const gchar* text)
  (define-function void gtk_font_selection_set_preview_text (void* char*))

  ) ;[end]
