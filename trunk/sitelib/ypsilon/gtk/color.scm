#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk color)

  (export gtk_color_button_get_alpha
          gtk_color_button_get_color
          gtk_color_button_get_title
          gtk_color_button_get_type
          gtk_color_button_get_use_alpha
          gtk_color_button_new
          gtk_color_button_new_with_color
          gtk_color_button_set_alpha
          gtk_color_button_set_color
          gtk_color_button_set_title
          gtk_color_button_set_use_alpha
          gtk_color_selection_dialog_get_color_selection
          gtk_color_selection_dialog_get_type
          gtk_color_selection_dialog_new
          gtk_color_selection_get_current_alpha
          gtk_color_selection_get_current_color
          gtk_color_selection_get_has_opacity_control
          gtk_color_selection_get_has_palette
          gtk_color_selection_get_previous_alpha
          gtk_color_selection_get_previous_color
          gtk_color_selection_get_type
          gtk_color_selection_is_adjusting
          gtk_color_selection_new
          gtk_color_selection_palette_from_string
          gtk_color_selection_palette_to_string
          gtk_color_selection_set_change_palette_with_screen_hook
          gtk_color_selection_set_current_alpha
          gtk_color_selection_set_current_color
          gtk_color_selection_set_has_opacity_control
          gtk_color_selection_set_has_palette
          gtk_color_selection_set_previous_alpha
          gtk_color_selection_set_previous_color)

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

  ;; guint16 gtk_color_button_get_alpha (GtkColorButton* color_button)
  (define-function uint16_t gtk_color_button_get_alpha (void*))

  ;; void gtk_color_button_get_color (GtkColorButton* color_button, GdkColor* color)
  (define-function void gtk_color_button_get_color (void* void*))

  ;; const gchar* gtk_color_button_get_title (GtkColorButton* color_button)
  (define-function char* gtk_color_button_get_title (void*))

  ;; GType gtk_color_button_get_type (void)
  (define-function unsigned-long gtk_color_button_get_type ())

  ;; gboolean gtk_color_button_get_use_alpha (GtkColorButton* color_button)
  (define-function int gtk_color_button_get_use_alpha (void*))

  ;; GtkWidget* gtk_color_button_new (void)
  (define-function void* gtk_color_button_new ())

  ;; GtkWidget* gtk_color_button_new_with_color (const GdkColor* color)
  (define-function void* gtk_color_button_new_with_color (void*))

  ;; void gtk_color_button_set_alpha (GtkColorButton* color_button, guint16 alpha)
  (define-function void gtk_color_button_set_alpha (void* uint16_t))

  ;; void gtk_color_button_set_color (GtkColorButton* color_button, const GdkColor* color)
  (define-function void gtk_color_button_set_color (void* void*))

  ;; void gtk_color_button_set_title (GtkColorButton* color_button, const gchar* title)
  (define-function void gtk_color_button_set_title (void* char*))

  ;; void gtk_color_button_set_use_alpha (GtkColorButton* color_button, gboolean use_alpha)
  (define-function void gtk_color_button_set_use_alpha (void* int))

  ;; GtkWidget* gtk_color_selection_dialog_get_color_selection (GtkColorSelectionDialog* colorsel)
  (define-function void* gtk_color_selection_dialog_get_color_selection (void*))

  ;; GType gtk_color_selection_dialog_get_type (void)
  (define-function unsigned-long gtk_color_selection_dialog_get_type ())

  ;; GtkWidget* gtk_color_selection_dialog_new (const gchar* title)
  (define-function void* gtk_color_selection_dialog_new (char*))

  ;; guint16 gtk_color_selection_get_current_alpha (GtkColorSelection* colorsel)
  (define-function uint16_t gtk_color_selection_get_current_alpha (void*))

  ;; void gtk_color_selection_get_current_color (GtkColorSelection* colorsel, GdkColor* color)
  (define-function void gtk_color_selection_get_current_color (void* void*))

  ;; gboolean gtk_color_selection_get_has_opacity_control (GtkColorSelection* colorsel)
  (define-function int gtk_color_selection_get_has_opacity_control (void*))

  ;; gboolean gtk_color_selection_get_has_palette (GtkColorSelection* colorsel)
  (define-function int gtk_color_selection_get_has_palette (void*))

  ;; guint16 gtk_color_selection_get_previous_alpha (GtkColorSelection* colorsel)
  (define-function uint16_t gtk_color_selection_get_previous_alpha (void*))

  ;; void gtk_color_selection_get_previous_color (GtkColorSelection* colorsel, GdkColor* color)
  (define-function void gtk_color_selection_get_previous_color (void* void*))

  ;; GType gtk_color_selection_get_type (void)
  (define-function unsigned-long gtk_color_selection_get_type ())

  ;; gboolean gtk_color_selection_is_adjusting (GtkColorSelection* colorsel)
  (define-function int gtk_color_selection_is_adjusting (void*))

  ;; GtkWidget* gtk_color_selection_new (void)
  (define-function void* gtk_color_selection_new ())

  ;; gboolean gtk_color_selection_palette_from_string (const gchar* str, GdkColor** colors, gint* n_colors)
  (define-function int gtk_color_selection_palette_from_string (char* void* void*))

  ;; gchar* gtk_color_selection_palette_to_string (const GdkColor* colors, gint n_colors)
  (define-function char* gtk_color_selection_palette_to_string (void* int))

  ;; GtkColorSelectionChangePaletteWithScreenFunc gtk_color_selection_set_change_palette_with_screen_hook (GtkColorSelectionChangePaletteWithScreenFunc func)
  (define-function void* gtk_color_selection_set_change_palette_with_screen_hook (void*))

  ;; void gtk_color_selection_set_current_alpha (GtkColorSelection* colorsel, guint16 alpha)
  (define-function void gtk_color_selection_set_current_alpha (void* uint16_t))

  ;; void gtk_color_selection_set_current_color (GtkColorSelection* colorsel, const GdkColor* color)
  (define-function void gtk_color_selection_set_current_color (void* void*))

  ;; void gtk_color_selection_set_has_opacity_control (GtkColorSelection* colorsel, gboolean has_opacity)
  (define-function void gtk_color_selection_set_has_opacity_control (void* int))

  ;; void gtk_color_selection_set_has_palette (GtkColorSelection* colorsel, gboolean has_palette)
  (define-function void gtk_color_selection_set_has_palette (void* int))

  ;; void gtk_color_selection_set_previous_alpha (GtkColorSelection* colorsel, guint16 alpha)
  (define-function void gtk_color_selection_set_previous_alpha (void* uint16_t))

  ;; void gtk_color_selection_set_previous_color (GtkColorSelection* colorsel, const GdkColor* color)
  (define-function void gtk_color_selection_set_previous_color (void* void*))

  ) ;[end]
