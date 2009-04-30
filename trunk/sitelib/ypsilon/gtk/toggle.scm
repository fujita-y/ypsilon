#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk toggle)

  (export gtk_toggle_action_get_active
          gtk_toggle_action_get_draw_as_radio
          gtk_toggle_action_get_type
          gtk_toggle_action_new
          gtk_toggle_action_set_active
          gtk_toggle_action_set_draw_as_radio
          gtk_toggle_action_toggled
          gtk_toggle_button_get_active
          gtk_toggle_button_get_inconsistent
          gtk_toggle_button_get_mode
          gtk_toggle_button_get_type
          gtk_toggle_button_new
          gtk_toggle_button_new_with_label
          gtk_toggle_button_new_with_mnemonic
          gtk_toggle_button_set_active
          gtk_toggle_button_set_inconsistent
          gtk_toggle_button_set_mode
          gtk_toggle_button_toggled
          gtk_toggle_tool_button_get_active
          gtk_toggle_tool_button_get_type
          gtk_toggle_tool_button_new
          gtk_toggle_tool_button_new_from_stock
          gtk_toggle_tool_button_set_active)

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

  ;; gboolean gtk_toggle_action_get_active (GtkToggleAction* action)
  (define-function int gtk_toggle_action_get_active (void*))

  ;; gboolean gtk_toggle_action_get_draw_as_radio (GtkToggleAction* action)
  (define-function int gtk_toggle_action_get_draw_as_radio (void*))

  ;; GType gtk_toggle_action_get_type (void)
  (define-function unsigned-long gtk_toggle_action_get_type ())

  ;; GtkToggleAction* gtk_toggle_action_new (const gchar* name, const gchar* label, const gchar* tooltip, const gchar* stock_id)
  (define-function void* gtk_toggle_action_new (char* char* char* char*))

  ;; void gtk_toggle_action_set_active (GtkToggleAction* action, gboolean is_active)
  (define-function void gtk_toggle_action_set_active (void* int))

  ;; void gtk_toggle_action_set_draw_as_radio (GtkToggleAction* action, gboolean draw_as_radio)
  (define-function void gtk_toggle_action_set_draw_as_radio (void* int))

  ;; void gtk_toggle_action_toggled (GtkToggleAction* action)
  (define-function void gtk_toggle_action_toggled (void*))

  ;; gboolean gtk_toggle_button_get_active (GtkToggleButton* toggle_button)
  (define-function int gtk_toggle_button_get_active (void*))

  ;; gboolean gtk_toggle_button_get_inconsistent (GtkToggleButton* toggle_button)
  (define-function int gtk_toggle_button_get_inconsistent (void*))

  ;; gboolean gtk_toggle_button_get_mode (GtkToggleButton* toggle_button)
  (define-function int gtk_toggle_button_get_mode (void*))

  ;; GType gtk_toggle_button_get_type (void)
  (define-function unsigned-long gtk_toggle_button_get_type ())

  ;; GtkWidget* gtk_toggle_button_new (void)
  (define-function void* gtk_toggle_button_new ())

  ;; GtkWidget* gtk_toggle_button_new_with_label (const gchar* label)
  (define-function void* gtk_toggle_button_new_with_label (char*))

  ;; GtkWidget* gtk_toggle_button_new_with_mnemonic (const gchar* label)
  (define-function void* gtk_toggle_button_new_with_mnemonic (char*))

  ;; void gtk_toggle_button_set_active (GtkToggleButton* toggle_button, gboolean is_active)
  (define-function void gtk_toggle_button_set_active (void* int))

  ;; void gtk_toggle_button_set_inconsistent (GtkToggleButton* toggle_button, gboolean setting)
  (define-function void gtk_toggle_button_set_inconsistent (void* int))

  ;; void gtk_toggle_button_set_mode (GtkToggleButton* toggle_button, gboolean draw_indicator)
  (define-function void gtk_toggle_button_set_mode (void* int))

  ;; void gtk_toggle_button_toggled (GtkToggleButton* toggle_button)
  (define-function void gtk_toggle_button_toggled (void*))

  ;; gboolean gtk_toggle_tool_button_get_active (GtkToggleToolButton* button)
  (define-function int gtk_toggle_tool_button_get_active (void*))

  ;; GType gtk_toggle_tool_button_get_type (void)
  (define-function unsigned-long gtk_toggle_tool_button_get_type ())

  ;; GtkToolItem* gtk_toggle_tool_button_new (void)
  (define-function void* gtk_toggle_tool_button_new ())

  ;; GtkToolItem* gtk_toggle_tool_button_new_from_stock (const gchar* stock_id)
  (define-function void* gtk_toggle_tool_button_new_from_stock (char*))

  ;; void gtk_toggle_tool_button_set_active (GtkToggleToolButton* button, gboolean is_active)
  (define-function void gtk_toggle_tool_button_set_active (void* int))

  ) ;[end]
