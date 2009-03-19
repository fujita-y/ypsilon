#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk tool)

  (export gtk_tool_button_get_icon_name
          gtk_tool_button_get_icon_widget
          gtk_tool_button_get_label
          gtk_tool_button_get_label_widget
          gtk_tool_button_get_stock_id
          gtk_tool_button_get_type
          gtk_tool_button_get_use_underline
          gtk_tool_button_new
          gtk_tool_button_new_from_stock
          gtk_tool_button_set_icon_name
          gtk_tool_button_set_icon_widget
          gtk_tool_button_set_label
          gtk_tool_button_set_label_widget
          gtk_tool_button_set_stock_id
          gtk_tool_button_set_use_underline
          gtk_tool_item_get_expand
          gtk_tool_item_get_homogeneous
          gtk_tool_item_get_icon_size
          gtk_tool_item_get_is_important
          gtk_tool_item_get_orientation
          gtk_tool_item_get_proxy_menu_item
          gtk_tool_item_get_relief_style
          gtk_tool_item_get_toolbar_style
          gtk_tool_item_get_type
          gtk_tool_item_get_use_drag_window
          gtk_tool_item_get_visible_horizontal
          gtk_tool_item_get_visible_vertical
          gtk_tool_item_new
          gtk_tool_item_rebuild_menu
          gtk_tool_item_retrieve_proxy_menu_item
          gtk_tool_item_set_expand
          gtk_tool_item_set_homogeneous
          gtk_tool_item_set_is_important
          gtk_tool_item_set_proxy_menu_item
          gtk_tool_item_set_tooltip_markup
          gtk_tool_item_set_tooltip_text
          gtk_tool_item_set_use_drag_window
          gtk_tool_item_set_visible_horizontal
          gtk_tool_item_set_visible_vertical
          gtk_tool_item_toolbar_reconfigured
          gtk_tool_shell_get_icon_size
          gtk_tool_shell_get_orientation
          gtk_tool_shell_get_relief_style
          gtk_tool_shell_get_style
          gtk_tool_shell_get_type
          gtk_tool_shell_rebuild_menu)

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

  ;; const gchar* gtk_tool_button_get_icon_name (GtkToolButton* button)
  (define-function char* gtk_tool_button_get_icon_name (void*))

  ;; GtkWidget* gtk_tool_button_get_icon_widget (GtkToolButton* button)
  (define-function void* gtk_tool_button_get_icon_widget (void*))

  ;; const gchar* gtk_tool_button_get_label (GtkToolButton* button)
  (define-function char* gtk_tool_button_get_label (void*))

  ;; GtkWidget* gtk_tool_button_get_label_widget (GtkToolButton* button)
  (define-function void* gtk_tool_button_get_label_widget (void*))

  ;; const gchar* gtk_tool_button_get_stock_id (GtkToolButton* button)
  (define-function char* gtk_tool_button_get_stock_id (void*))

  ;; GType gtk_tool_button_get_type (void)
  (define-function unsigned-long gtk_tool_button_get_type ())

  ;; gboolean gtk_tool_button_get_use_underline (GtkToolButton* button)
  (define-function int gtk_tool_button_get_use_underline (void*))

  ;; GtkToolItem* gtk_tool_button_new (GtkWidget* icon_widget, const gchar* label)
  (define-function void* gtk_tool_button_new (void* char*))

  ;; GtkToolItem* gtk_tool_button_new_from_stock (const gchar* stock_id)
  (define-function void* gtk_tool_button_new_from_stock (char*))

  ;; void gtk_tool_button_set_icon_name (GtkToolButton* button, const gchar* icon_name)
  (define-function void gtk_tool_button_set_icon_name (void* char*))

  ;; void gtk_tool_button_set_icon_widget (GtkToolButton* button, GtkWidget* icon_widget)
  (define-function void gtk_tool_button_set_icon_widget (void* void*))

  ;; void gtk_tool_button_set_label (GtkToolButton* button, const gchar* label)
  (define-function void gtk_tool_button_set_label (void* char*))

  ;; void gtk_tool_button_set_label_widget (GtkToolButton* button, GtkWidget* label_widget)
  (define-function void gtk_tool_button_set_label_widget (void* void*))

  ;; void gtk_tool_button_set_stock_id (GtkToolButton* button, const gchar* stock_id)
  (define-function void gtk_tool_button_set_stock_id (void* char*))

  ;; void gtk_tool_button_set_use_underline (GtkToolButton* button, gboolean use_underline)
  (define-function void gtk_tool_button_set_use_underline (void* int))

  ;; gboolean gtk_tool_item_get_expand (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_expand (void*))

  ;; gboolean gtk_tool_item_get_homogeneous (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_homogeneous (void*))

  ;; GtkIconSize gtk_tool_item_get_icon_size (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_icon_size (void*))

  ;; gboolean gtk_tool_item_get_is_important (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_is_important (void*))

  ;; GtkOrientation gtk_tool_item_get_orientation (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_orientation (void*))

  ;; GtkWidget* gtk_tool_item_get_proxy_menu_item (GtkToolItem* tool_item, const gchar* menu_item_id)
  (define-function void* gtk_tool_item_get_proxy_menu_item (void* char*))

  ;; GtkReliefStyle gtk_tool_item_get_relief_style (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_relief_style (void*))

  ;; GtkToolbarStyle gtk_tool_item_get_toolbar_style (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_toolbar_style (void*))

  ;; GType gtk_tool_item_get_type (void)
  (define-function unsigned-long gtk_tool_item_get_type ())

  ;; gboolean gtk_tool_item_get_use_drag_window (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_use_drag_window (void*))

  ;; gboolean gtk_tool_item_get_visible_horizontal (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_visible_horizontal (void*))

  ;; gboolean gtk_tool_item_get_visible_vertical (GtkToolItem* tool_item)
  (define-function int gtk_tool_item_get_visible_vertical (void*))

  ;; GtkToolItem* gtk_tool_item_new (void)
  (define-function void* gtk_tool_item_new ())

  ;; void gtk_tool_item_rebuild_menu (GtkToolItem* tool_item)
  (define-function void gtk_tool_item_rebuild_menu (void*))

  ;; GtkWidget* gtk_tool_item_retrieve_proxy_menu_item (GtkToolItem* tool_item)
  (define-function void* gtk_tool_item_retrieve_proxy_menu_item (void*))

  ;; void gtk_tool_item_set_expand (GtkToolItem* tool_item, gboolean expand)
  (define-function void gtk_tool_item_set_expand (void* int))

  ;; void gtk_tool_item_set_homogeneous (GtkToolItem* tool_item, gboolean homogeneous)
  (define-function void gtk_tool_item_set_homogeneous (void* int))

  ;; void gtk_tool_item_set_is_important (GtkToolItem* tool_item, gboolean is_important)
  (define-function void gtk_tool_item_set_is_important (void* int))

  ;; void gtk_tool_item_set_proxy_menu_item (GtkToolItem* tool_item, const gchar* menu_item_id, GtkWidget* menu_item)
  (define-function void gtk_tool_item_set_proxy_menu_item (void* char* void*))

  ;; void gtk_tool_item_set_tooltip_markup (GtkToolItem* tool_item, const gchar* markup)
  (define-function void gtk_tool_item_set_tooltip_markup (void* char*))

  ;; void gtk_tool_item_set_tooltip_text (GtkToolItem* tool_item, const gchar* text)
  (define-function void gtk_tool_item_set_tooltip_text (void* char*))

  ;; void gtk_tool_item_set_use_drag_window (GtkToolItem* tool_item, gboolean use_drag_window)
  (define-function void gtk_tool_item_set_use_drag_window (void* int))

  ;; void gtk_tool_item_set_visible_horizontal (GtkToolItem* tool_item, gboolean visible_horizontal)
  (define-function void gtk_tool_item_set_visible_horizontal (void* int))

  ;; void gtk_tool_item_set_visible_vertical (GtkToolItem* tool_item, gboolean visible_vertical)
  (define-function void gtk_tool_item_set_visible_vertical (void* int))

  ;; void gtk_tool_item_toolbar_reconfigured (GtkToolItem* tool_item)
  (define-function void gtk_tool_item_toolbar_reconfigured (void*))

  ;; GtkIconSize gtk_tool_shell_get_icon_size (GtkToolShell* shell)
  (define-function int gtk_tool_shell_get_icon_size (void*))

  ;; GtkOrientation gtk_tool_shell_get_orientation (GtkToolShell* shell)
  (define-function int gtk_tool_shell_get_orientation (void*))

  ;; GtkReliefStyle gtk_tool_shell_get_relief_style (GtkToolShell* shell)
  (define-function int gtk_tool_shell_get_relief_style (void*))

  ;; GtkToolbarStyle gtk_tool_shell_get_style (GtkToolShell* shell)
  (define-function int gtk_tool_shell_get_style (void*))

  ;; GType gtk_tool_shell_get_type (void)
  (define-function unsigned-long gtk_tool_shell_get_type ())

  ;; void gtk_tool_shell_rebuild_menu (GtkToolShell* shell)
  (define-function void gtk_tool_shell_rebuild_menu (void*))

  ) ;[end]
