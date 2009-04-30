#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk menu)

  (export gtk_menu_attach
          gtk_menu_attach_to_widget
          gtk_menu_bar_get_child_pack_direction
          gtk_menu_bar_get_pack_direction
          gtk_menu_bar_get_type
          gtk_menu_bar_new
          gtk_menu_bar_set_child_pack_direction
          gtk_menu_bar_set_pack_direction
          gtk_menu_detach
          gtk_menu_direction_type_get_type
          gtk_menu_get_accel_group
          gtk_menu_get_accel_path
          gtk_menu_get_active
          gtk_menu_get_attach_widget
          gtk_menu_get_for_attach_widget
          gtk_menu_get_monitor
          gtk_menu_get_tearoff_state
          gtk_menu_get_title
          gtk_menu_get_type
          gtk_menu_item_activate
          gtk_menu_item_deselect
          gtk_menu_item_get_accel_path
          gtk_menu_item_get_label
          gtk_menu_item_get_right_justified
          gtk_menu_item_get_submenu
          gtk_menu_item_get_type
          gtk_menu_item_get_use_underline
          gtk_menu_item_new
          gtk_menu_item_new_with_label
          gtk_menu_item_new_with_mnemonic
          gtk_menu_item_select
          gtk_menu_item_set_accel_path
          gtk_menu_item_set_label
          gtk_menu_item_set_right_justified
          gtk_menu_item_set_submenu
          gtk_menu_item_set_use_underline
          gtk_menu_item_toggle_size_allocate
          gtk_menu_item_toggle_size_request
          gtk_menu_new
          gtk_menu_popdown
          gtk_menu_popup
          gtk_menu_reorder_child
          gtk_menu_reposition
          gtk_menu_set_accel_group
          gtk_menu_set_accel_path
          gtk_menu_set_active
          gtk_menu_set_monitor
          gtk_menu_set_screen
          gtk_menu_set_tearoff_state
          gtk_menu_set_title
          gtk_menu_shell_activate_item
          gtk_menu_shell_append
          gtk_menu_shell_cancel
          gtk_menu_shell_deactivate
          gtk_menu_shell_deselect
          gtk_menu_shell_get_take_focus
          gtk_menu_shell_get_type
          gtk_menu_shell_insert
          gtk_menu_shell_prepend
          gtk_menu_shell_select_first
          gtk_menu_shell_select_item
          gtk_menu_shell_set_take_focus
          gtk_menu_tool_button_get_menu
          gtk_menu_tool_button_get_type
          gtk_menu_tool_button_new
          gtk_menu_tool_button_new_from_stock
          gtk_menu_tool_button_set_arrow_tooltip_markup
          gtk_menu_tool_button_set_arrow_tooltip_text
          gtk_menu_tool_button_set_menu)

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

  ;; void gtk_menu_attach (GtkMenu* menu, GtkWidget* child, guint left_attach, guint right_attach, guint top_attach, guint bottom_attach)
  (define-function void gtk_menu_attach (void* void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ;; void gtk_menu_attach_to_widget (GtkMenu* menu, GtkWidget* attach_widget, GtkMenuDetachFunc detacher)
  (define-function void gtk_menu_attach_to_widget (void* void* (c-callback void (void* void*))))

  ;; GtkPackDirection gtk_menu_bar_get_child_pack_direction (GtkMenuBar* menubar)
  (define-function int gtk_menu_bar_get_child_pack_direction (void*))

  ;; GtkPackDirection gtk_menu_bar_get_pack_direction (GtkMenuBar* menubar)
  (define-function int gtk_menu_bar_get_pack_direction (void*))

  ;; GType gtk_menu_bar_get_type (void)
  (define-function unsigned-long gtk_menu_bar_get_type ())

  ;; GtkWidget* gtk_menu_bar_new (void)
  (define-function void* gtk_menu_bar_new ())

  ;; void gtk_menu_bar_set_child_pack_direction (GtkMenuBar* menubar, GtkPackDirection child_pack_dir)
  (define-function void gtk_menu_bar_set_child_pack_direction (void* int))

  ;; void gtk_menu_bar_set_pack_direction (GtkMenuBar* menubar, GtkPackDirection pack_dir)
  (define-function void gtk_menu_bar_set_pack_direction (void* int))

  ;; void gtk_menu_detach (GtkMenu* menu)
  (define-function void gtk_menu_detach (void*))

  ;; GType gtk_menu_direction_type_get_type (void)
  (define-function unsigned-long gtk_menu_direction_type_get_type ())

  ;; GtkAccelGroup* gtk_menu_get_accel_group (GtkMenu* menu)
  (define-function void* gtk_menu_get_accel_group (void*))

  ;; const gchar* gtk_menu_get_accel_path (GtkMenu* menu)
  (define-function char* gtk_menu_get_accel_path (void*))

  ;; GtkWidget* gtk_menu_get_active (GtkMenu* menu)
  (define-function void* gtk_menu_get_active (void*))

  ;; GtkWidget* gtk_menu_get_attach_widget (GtkMenu* menu)
  (define-function void* gtk_menu_get_attach_widget (void*))

  ;; GList* gtk_menu_get_for_attach_widget (GtkWidget* widget)
  (define-function void* gtk_menu_get_for_attach_widget (void*))

  ;; gint gtk_menu_get_monitor (GtkMenu* menu)
  (define-function int gtk_menu_get_monitor (void*))

  ;; gboolean gtk_menu_get_tearoff_state (GtkMenu* menu)
  (define-function int gtk_menu_get_tearoff_state (void*))

  ;; const gchar* gtk_menu_get_title (GtkMenu* menu)
  (define-function char* gtk_menu_get_title (void*))

  ;; GType gtk_menu_get_type (void)
  (define-function unsigned-long gtk_menu_get_type ())

  ;; void gtk_menu_item_activate (GtkMenuItem* menu_item)
  (define-function void gtk_menu_item_activate (void*))

  ;; void gtk_menu_item_deselect (GtkMenuItem* menu_item)
  (define-function void gtk_menu_item_deselect (void*))

  ;; const gchar* gtk_menu_item_get_accel_path (GtkMenuItem* menu_item)
  (define-function char* gtk_menu_item_get_accel_path (void*))

  ;; const gchar* gtk_menu_item_get_label (GtkMenuItem* menu_item)
  (define-function char* gtk_menu_item_get_label (void*))

  ;; gboolean gtk_menu_item_get_right_justified (GtkMenuItem* menu_item)
  (define-function int gtk_menu_item_get_right_justified (void*))

  ;; GtkWidget* gtk_menu_item_get_submenu (GtkMenuItem* menu_item)
  (define-function void* gtk_menu_item_get_submenu (void*))

  ;; GType gtk_menu_item_get_type (void)
  (define-function unsigned-long gtk_menu_item_get_type ())

  ;; gboolean gtk_menu_item_get_use_underline (GtkMenuItem* menu_item)
  (define-function int gtk_menu_item_get_use_underline (void*))

  ;; GtkWidget* gtk_menu_item_new (void)
  (define-function void* gtk_menu_item_new ())

  ;; GtkWidget* gtk_menu_item_new_with_label (const gchar* label)
  (define-function void* gtk_menu_item_new_with_label (char*))

  ;; GtkWidget* gtk_menu_item_new_with_mnemonic (const gchar* label)
  (define-function void* gtk_menu_item_new_with_mnemonic (char*))

  ;; void gtk_menu_item_select (GtkMenuItem* menu_item)
  (define-function void gtk_menu_item_select (void*))

  ;; void gtk_menu_item_set_accel_path (GtkMenuItem* menu_item, const gchar* accel_path)
  (define-function void gtk_menu_item_set_accel_path (void* char*))

  ;; void gtk_menu_item_set_label (GtkMenuItem* menu_item, const gchar* label)
  (define-function void gtk_menu_item_set_label (void* char*))

  ;; void gtk_menu_item_set_right_justified (GtkMenuItem* menu_item, gboolean right_justified)
  (define-function void gtk_menu_item_set_right_justified (void* int))

  ;; void gtk_menu_item_set_submenu (GtkMenuItem* menu_item, GtkWidget* submenu)
  (define-function void gtk_menu_item_set_submenu (void* void*))

  ;; void gtk_menu_item_set_use_underline (GtkMenuItem* menu_item, gboolean setting)
  (define-function void gtk_menu_item_set_use_underline (void* int))

  ;; void gtk_menu_item_toggle_size_allocate (GtkMenuItem* menu_item, gint allocation)
  (define-function void gtk_menu_item_toggle_size_allocate (void* int))

  ;; void gtk_menu_item_toggle_size_request (GtkMenuItem* menu_item, gint* requisition)
  (define-function void gtk_menu_item_toggle_size_request (void* void*))

  ;; GtkWidget* gtk_menu_new (void)
  (define-function void* gtk_menu_new ())

  ;; void gtk_menu_popdown (GtkMenu* menu)
  (define-function void gtk_menu_popdown (void*))

  ;; void gtk_menu_popup (GtkMenu* menu, GtkWidget* parent_menu_shell, GtkWidget* parent_menu_item, GtkMenuPositionFunc func, gpointer data, guint button, guint32 activate_time)
  (define-function void gtk_menu_popup (void* void* void* (c-callback void (void* void* void* void* void*)) void* unsigned-int uint32_t))

  ;; void gtk_menu_reorder_child (GtkMenu* menu, GtkWidget* child, gint position)
  (define-function void gtk_menu_reorder_child (void* void* int))

  ;; void gtk_menu_reposition (GtkMenu* menu)
  (define-function void gtk_menu_reposition (void*))

  ;; void gtk_menu_set_accel_group (GtkMenu* menu, GtkAccelGroup* accel_group)
  (define-function void gtk_menu_set_accel_group (void* void*))

  ;; void gtk_menu_set_accel_path (GtkMenu* menu, const gchar* accel_path)
  (define-function void gtk_menu_set_accel_path (void* char*))

  ;; void gtk_menu_set_active (GtkMenu* menu, guint index_)
  (define-function void gtk_menu_set_active (void* unsigned-int))

  ;; void gtk_menu_set_monitor (GtkMenu* menu, gint monitor_num)
  (define-function void gtk_menu_set_monitor (void* int))

  ;; void gtk_menu_set_screen (GtkMenu* menu, GdkScreen* screen)
  (define-function void gtk_menu_set_screen (void* void*))

  ;; void gtk_menu_set_tearoff_state (GtkMenu* menu, gboolean torn_off)
  (define-function void gtk_menu_set_tearoff_state (void* int))

  ;; void gtk_menu_set_title (GtkMenu* menu, const gchar* title)
  (define-function void gtk_menu_set_title (void* char*))

  ;; void gtk_menu_shell_activate_item (GtkMenuShell* menu_shell, GtkWidget* menu_item, gboolean force_deactivate)
  (define-function void gtk_menu_shell_activate_item (void* void* int))

  ;; void gtk_menu_shell_append (GtkMenuShell* menu_shell, GtkWidget* child)
  (define-function void gtk_menu_shell_append (void* void*))

  ;; void gtk_menu_shell_cancel (GtkMenuShell* menu_shell)
  (define-function void gtk_menu_shell_cancel (void*))

  ;; void gtk_menu_shell_deactivate (GtkMenuShell* menu_shell)
  (define-function void gtk_menu_shell_deactivate (void*))

  ;; void gtk_menu_shell_deselect (GtkMenuShell* menu_shell)
  (define-function void gtk_menu_shell_deselect (void*))

  ;; gboolean gtk_menu_shell_get_take_focus (GtkMenuShell* menu_shell)
  (define-function int gtk_menu_shell_get_take_focus (void*))

  ;; GType gtk_menu_shell_get_type (void)
  (define-function unsigned-long gtk_menu_shell_get_type ())

  ;; void gtk_menu_shell_insert (GtkMenuShell* menu_shell, GtkWidget* child, gint position)
  (define-function void gtk_menu_shell_insert (void* void* int))

  ;; void gtk_menu_shell_prepend (GtkMenuShell* menu_shell, GtkWidget* child)
  (define-function void gtk_menu_shell_prepend (void* void*))

  ;; void gtk_menu_shell_select_first (GtkMenuShell* menu_shell, gboolean search_sensitive)
  (define-function void gtk_menu_shell_select_first (void* int))

  ;; void gtk_menu_shell_select_item (GtkMenuShell* menu_shell, GtkWidget* menu_item)
  (define-function void gtk_menu_shell_select_item (void* void*))

  ;; void gtk_menu_shell_set_take_focus (GtkMenuShell* menu_shell, gboolean take_focus)
  (define-function void gtk_menu_shell_set_take_focus (void* int))

  ;; GtkWidget* gtk_menu_tool_button_get_menu (GtkMenuToolButton* button)
  (define-function void* gtk_menu_tool_button_get_menu (void*))

  ;; GType gtk_menu_tool_button_get_type (void)
  (define-function unsigned-long gtk_menu_tool_button_get_type ())

  ;; GtkToolItem* gtk_menu_tool_button_new (GtkWidget* icon_widget, const gchar* label)
  (define-function void* gtk_menu_tool_button_new (void* char*))

  ;; GtkToolItem* gtk_menu_tool_button_new_from_stock (const gchar* stock_id)
  (define-function void* gtk_menu_tool_button_new_from_stock (char*))

  ;; void gtk_menu_tool_button_set_arrow_tooltip_markup (GtkMenuToolButton* button, const gchar* markup)
  (define-function void gtk_menu_tool_button_set_arrow_tooltip_markup (void* char*))

  ;; void gtk_menu_tool_button_set_arrow_tooltip_text (GtkMenuToolButton* button, const gchar* text)
  (define-function void gtk_menu_tool_button_set_arrow_tooltip_text (void* char*))

  ;; void gtk_menu_tool_button_set_menu (GtkMenuToolButton* button, GtkWidget* menu)
  (define-function void gtk_menu_tool_button_set_menu (void* void*))

  ) ;[end]
