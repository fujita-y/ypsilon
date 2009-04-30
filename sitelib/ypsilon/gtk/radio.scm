#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk radio)

  (export gtk_radio_action_get_current_value
          gtk_radio_action_get_group
          gtk_radio_action_get_type
          gtk_radio_action_new
          gtk_radio_action_set_current_value
          gtk_radio_action_set_group
          gtk_radio_button_get_group
          gtk_radio_button_get_type
          gtk_radio_button_new
          gtk_radio_button_new_from_widget
          gtk_radio_button_new_with_label
          gtk_radio_button_new_with_label_from_widget
          gtk_radio_button_new_with_mnemonic
          gtk_radio_button_new_with_mnemonic_from_widget
          gtk_radio_button_set_group
          gtk_radio_menu_item_get_group
          gtk_radio_menu_item_get_type
          gtk_radio_menu_item_new
          gtk_radio_menu_item_new_from_widget
          gtk_radio_menu_item_new_with_label
          gtk_radio_menu_item_new_with_label_from_widget
          gtk_radio_menu_item_new_with_mnemonic
          gtk_radio_menu_item_new_with_mnemonic_from_widget
          gtk_radio_menu_item_set_group
          gtk_radio_tool_button_get_group
          gtk_radio_tool_button_get_type
          gtk_radio_tool_button_new
          gtk_radio_tool_button_new_from_stock
          gtk_radio_tool_button_new_from_widget
          gtk_radio_tool_button_new_with_stock_from_widget
          gtk_radio_tool_button_set_group)

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

  ;; gint gtk_radio_action_get_current_value (GtkRadioAction* action)
  (define-function int gtk_radio_action_get_current_value (void*))

  ;; GSList* gtk_radio_action_get_group (GtkRadioAction* action)
  (define-function void* gtk_radio_action_get_group (void*))

  ;; GType gtk_radio_action_get_type (void)
  (define-function unsigned-long gtk_radio_action_get_type ())

  ;; GtkRadioAction* gtk_radio_action_new (const gchar* name, const gchar* label, const gchar* tooltip, const gchar* stock_id, gint value)
  (define-function void* gtk_radio_action_new (char* char* char* char* int))

  ;; void gtk_radio_action_set_current_value (GtkRadioAction* action, gint current_value)
  (define-function void gtk_radio_action_set_current_value (void* int))

  ;; void gtk_radio_action_set_group (GtkRadioAction* action, GSList* group)
  (define-function void gtk_radio_action_set_group (void* void*))

  ;; GSList* gtk_radio_button_get_group (GtkRadioButton* radio_button)
  (define-function void* gtk_radio_button_get_group (void*))

  ;; GType gtk_radio_button_get_type (void)
  (define-function unsigned-long gtk_radio_button_get_type ())

  ;; GtkWidget* gtk_radio_button_new (GSList* group)
  (define-function void* gtk_radio_button_new (void*))

  ;; GtkWidget* gtk_radio_button_new_from_widget (GtkRadioButton* radio_group_member)
  (define-function void* gtk_radio_button_new_from_widget (void*))

  ;; GtkWidget* gtk_radio_button_new_with_label (GSList* group, const gchar* label)
  (define-function void* gtk_radio_button_new_with_label (void* char*))

  ;; GtkWidget* gtk_radio_button_new_with_label_from_widget (GtkRadioButton* radio_group_member, const gchar* label)
  (define-function void* gtk_radio_button_new_with_label_from_widget (void* char*))

  ;; GtkWidget* gtk_radio_button_new_with_mnemonic (GSList* group, const gchar* label)
  (define-function void* gtk_radio_button_new_with_mnemonic (void* char*))

  ;; GtkWidget* gtk_radio_button_new_with_mnemonic_from_widget (GtkRadioButton* radio_group_member, const gchar* label)
  (define-function void* gtk_radio_button_new_with_mnemonic_from_widget (void* char*))

  ;; void gtk_radio_button_set_group (GtkRadioButton* radio_button, GSList* group)
  (define-function void gtk_radio_button_set_group (void* void*))

  ;; GSList* gtk_radio_menu_item_get_group (GtkRadioMenuItem* radio_menu_item)
  (define-function void* gtk_radio_menu_item_get_group (void*))

  ;; GType gtk_radio_menu_item_get_type (void)
  (define-function unsigned-long gtk_radio_menu_item_get_type ())

  ;; GtkWidget* gtk_radio_menu_item_new (GSList* group)
  (define-function void* gtk_radio_menu_item_new (void*))

  ;; GtkWidget* gtk_radio_menu_item_new_from_widget (GtkRadioMenuItem* group)
  (define-function void* gtk_radio_menu_item_new_from_widget (void*))

  ;; GtkWidget* gtk_radio_menu_item_new_with_label (GSList* group, const gchar* label)
  (define-function void* gtk_radio_menu_item_new_with_label (void* char*))

  ;; GtkWidget* gtk_radio_menu_item_new_with_label_from_widget (GtkRadioMenuItem* group, const gchar* label)
  (define-function void* gtk_radio_menu_item_new_with_label_from_widget (void* char*))

  ;; GtkWidget* gtk_radio_menu_item_new_with_mnemonic (GSList* group, const gchar* label)
  (define-function void* gtk_radio_menu_item_new_with_mnemonic (void* char*))

  ;; GtkWidget* gtk_radio_menu_item_new_with_mnemonic_from_widget (GtkRadioMenuItem* group, const gchar* label)
  (define-function void* gtk_radio_menu_item_new_with_mnemonic_from_widget (void* char*))

  ;; void gtk_radio_menu_item_set_group (GtkRadioMenuItem* radio_menu_item, GSList* group)
  (define-function void gtk_radio_menu_item_set_group (void* void*))

  ;; GSList* gtk_radio_tool_button_get_group (GtkRadioToolButton* button)
  (define-function void* gtk_radio_tool_button_get_group (void*))

  ;; GType gtk_radio_tool_button_get_type (void)
  (define-function unsigned-long gtk_radio_tool_button_get_type ())

  ;; GtkToolItem* gtk_radio_tool_button_new (GSList* group)
  (define-function void* gtk_radio_tool_button_new (void*))

  ;; GtkToolItem* gtk_radio_tool_button_new_from_stock (GSList* group, const gchar* stock_id)
  (define-function void* gtk_radio_tool_button_new_from_stock (void* char*))

  ;; GtkToolItem* gtk_radio_tool_button_new_from_widget (GtkRadioToolButton* group)
  (define-function void* gtk_radio_tool_button_new_from_widget (void*))

  ;; GtkToolItem* gtk_radio_tool_button_new_with_stock_from_widget (GtkRadioToolButton* group, const gchar* stock_id)
  (define-function void* gtk_radio_tool_button_new_with_stock_from_widget (void* char*))

  ;; void gtk_radio_tool_button_set_group (GtkRadioToolButton* button, GSList* group)
  (define-function void gtk_radio_tool_button_set_group (void* void*))

  ) ;[end]
