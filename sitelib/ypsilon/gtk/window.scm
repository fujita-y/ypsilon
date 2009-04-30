#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk window)

  (export gtk_window_activate_default
          gtk_window_activate_focus
          gtk_window_activate_key
          gtk_window_add_accel_group
          gtk_window_add_embedded_xid
          gtk_window_add_mnemonic
          gtk_window_begin_move_drag
          gtk_window_begin_resize_drag
          gtk_window_deiconify
          gtk_window_fullscreen
          gtk_window_get_accept_focus
          gtk_window_get_decorated
          gtk_window_get_default_icon_list
          gtk_window_get_default_icon_name
          gtk_window_get_default_size
          gtk_window_get_default_widget
          gtk_window_get_deletable
          gtk_window_get_destroy_with_parent
          gtk_window_get_focus
          gtk_window_get_focus_on_map
          gtk_window_get_frame_dimensions
          gtk_window_get_gravity
          gtk_window_get_group
          gtk_window_get_has_frame
          gtk_window_get_icon
          gtk_window_get_icon_list
          gtk_window_get_icon_name
          gtk_window_get_mnemonic_modifier
          gtk_window_get_modal
          gtk_window_get_opacity
          gtk_window_get_position
          gtk_window_get_resizable
          gtk_window_get_role
          gtk_window_get_screen
          gtk_window_get_size
          gtk_window_get_skip_pager_hint
          gtk_window_get_skip_taskbar_hint
          gtk_window_get_title
          gtk_window_get_transient_for
          gtk_window_get_type
          gtk_window_get_type_hint
          gtk_window_get_urgency_hint
          gtk_window_group_add_window
          gtk_window_group_get_type
          gtk_window_group_list_windows
          gtk_window_group_new
          gtk_window_group_remove_window
          gtk_window_has_toplevel_focus
          gtk_window_iconify
          gtk_window_is_active
          gtk_window_list_toplevels
          gtk_window_maximize
          gtk_window_mnemonic_activate
          gtk_window_move
          gtk_window_new
          gtk_window_parse_geometry
          gtk_window_position_get_type
          gtk_window_present
          gtk_window_present_with_time
          gtk_window_propagate_key_event
          gtk_window_remove_accel_group
          gtk_window_remove_embedded_xid
          gtk_window_remove_mnemonic
          gtk_window_reshow_with_initial_size
          gtk_window_resize
          gtk_window_set_accept_focus
          gtk_window_set_auto_startup_notification
          gtk_window_set_decorated
          gtk_window_set_default
          gtk_window_set_default_icon
          gtk_window_set_default_icon_from_file
          gtk_window_set_default_icon_list
          gtk_window_set_default_icon_name
          gtk_window_set_default_size
          gtk_window_set_deletable
          gtk_window_set_destroy_with_parent
          gtk_window_set_focus
          gtk_window_set_focus_on_map
          gtk_window_set_frame_dimensions
          gtk_window_set_geometry_hints
          gtk_window_set_gravity
          gtk_window_set_has_frame
          gtk_window_set_icon
          gtk_window_set_icon_from_file
          gtk_window_set_icon_list
          gtk_window_set_icon_name
          gtk_window_set_keep_above
          gtk_window_set_keep_below
          gtk_window_set_mnemonic_modifier
          gtk_window_set_modal
          gtk_window_set_opacity
          gtk_window_set_position
          gtk_window_set_resizable
          gtk_window_set_role
          gtk_window_set_screen
          gtk_window_set_skip_pager_hint
          gtk_window_set_skip_taskbar_hint
          gtk_window_set_startup_id
          gtk_window_set_title
          gtk_window_set_transient_for
          gtk_window_set_type_hint
          gtk_window_set_urgency_hint
          gtk_window_set_wmclass
          gtk_window_stick
          gtk_window_type_get_type
          gtk_window_unfullscreen
          gtk_window_unmaximize
          gtk_window_unstick)

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

  ;; gboolean gtk_window_activate_default (GtkWindow* window)
  (define-function int gtk_window_activate_default (void*))

  ;; gboolean gtk_window_activate_focus (GtkWindow* window)
  (define-function int gtk_window_activate_focus (void*))

  ;; gboolean gtk_window_activate_key (GtkWindow* window, GdkEventKey* event)
  (define-function int gtk_window_activate_key (void* void*))

  ;; void gtk_window_add_accel_group (GtkWindow* window, GtkAccelGroup* accel_group)
  (define-function void gtk_window_add_accel_group (void* void*))

  ;; void gtk_window_add_embedded_xid (GtkWindow* window, GdkNativeWindow xid)
  (define-function void gtk_window_add_embedded_xid (void* uint32_t))

  ;; void gtk_window_add_mnemonic (GtkWindow* window, guint keyval, GtkWidget* target)
  (define-function void gtk_window_add_mnemonic (void* unsigned-int void*))

  ;; void gtk_window_begin_move_drag (GtkWindow* window, gint button, gint root_x, gint root_y, guint32 timestamp)
  (define-function void gtk_window_begin_move_drag (void* int int int uint32_t))

  ;; void gtk_window_begin_resize_drag (GtkWindow* window, GdkWindowEdge edge, gint button, gint root_x, gint root_y, guint32 timestamp)
  (define-function void gtk_window_begin_resize_drag (void* int int int int uint32_t))

  ;; void gtk_window_deiconify (GtkWindow* window)
  (define-function void gtk_window_deiconify (void*))

  ;; void gtk_window_fullscreen (GtkWindow* window)
  (define-function void gtk_window_fullscreen (void*))

  ;; gboolean gtk_window_get_accept_focus (GtkWindow* window)
  (define-function int gtk_window_get_accept_focus (void*))

  ;; gboolean gtk_window_get_decorated (GtkWindow* window)
  (define-function int gtk_window_get_decorated (void*))

  ;; GList* gtk_window_get_default_icon_list (void)
  (define-function void* gtk_window_get_default_icon_list ())

  ;; constgchar* gtk_window_get_default_icon_name (void)
  (define-function char* gtk_window_get_default_icon_name ())

  ;; void gtk_window_get_default_size (GtkWindow* window, gint* width, gint* height)
  (define-function void gtk_window_get_default_size (void* void* void*))

  ;; GtkWidget* gtk_window_get_default_widget (GtkWindow* window)
  (define-function void* gtk_window_get_default_widget (void*))

  ;; gboolean gtk_window_get_deletable (GtkWindow* window)
  (define-function int gtk_window_get_deletable (void*))

  ;; gboolean gtk_window_get_destroy_with_parent (GtkWindow* window)
  (define-function int gtk_window_get_destroy_with_parent (void*))

  ;; GtkWidget* gtk_window_get_focus (GtkWindow* window)
  (define-function void* gtk_window_get_focus (void*))

  ;; gboolean gtk_window_get_focus_on_map (GtkWindow* window)
  (define-function int gtk_window_get_focus_on_map (void*))

  ;; void gtk_window_get_frame_dimensions (GtkWindow* window, gint* left, gint* top, gint* right, gint* bottom)
  (define-function void gtk_window_get_frame_dimensions (void* void* void* void* void*))

  ;; GdkGravity gtk_window_get_gravity (GtkWindow* window)
  (define-function int gtk_window_get_gravity (void*))

  ;; GtkWindowGroup* gtk_window_get_group (GtkWindow* window)
  (define-function void* gtk_window_get_group (void*))

  ;; gboolean gtk_window_get_has_frame (GtkWindow* window)
  (define-function int gtk_window_get_has_frame (void*))

  ;; GdkPixbuf* gtk_window_get_icon (GtkWindow* window)
  (define-function void* gtk_window_get_icon (void*))

  ;; GList* gtk_window_get_icon_list (GtkWindow* window)
  (define-function void* gtk_window_get_icon_list (void*))

  ;; constgchar* gtk_window_get_icon_name (GtkWindow* window)
  (define-function char* gtk_window_get_icon_name (void*))

  ;; GdkModifierType gtk_window_get_mnemonic_modifier (GtkWindow* window)
  (define-function int gtk_window_get_mnemonic_modifier (void*))

  ;; gboolean gtk_window_get_modal (GtkWindow* window)
  (define-function int gtk_window_get_modal (void*))

  ;; gdouble gtk_window_get_opacity (GtkWindow* window)
  (define-function double gtk_window_get_opacity (void*))

  ;; void gtk_window_get_position (GtkWindow* window, gint* root_x, gint* root_y)
  (define-function void gtk_window_get_position (void* void* void*))

  ;; gboolean gtk_window_get_resizable (GtkWindow* window)
  (define-function int gtk_window_get_resizable (void*))

  ;; const gchar* gtk_window_get_role (GtkWindow* window)
  (define-function char* gtk_window_get_role (void*))

  ;; GdkScreen* gtk_window_get_screen (GtkWindow* window)
  (define-function void* gtk_window_get_screen (void*))

  ;; void gtk_window_get_size (GtkWindow* window, gint* width, gint* height)
  (define-function void gtk_window_get_size (void* void* void*))

  ;; gboolean gtk_window_get_skip_pager_hint (GtkWindow* window)
  (define-function int gtk_window_get_skip_pager_hint (void*))

  ;; gboolean gtk_window_get_skip_taskbar_hint (GtkWindow* window)
  (define-function int gtk_window_get_skip_taskbar_hint (void*))

  ;; const gchar* gtk_window_get_title (GtkWindow* window)
  (define-function char* gtk_window_get_title (void*))

  ;; GtkWindow* gtk_window_get_transient_for (GtkWindow* window)
  (define-function void* gtk_window_get_transient_for (void*))

  ;; GType gtk_window_get_type (void)
  (define-function unsigned-long gtk_window_get_type ())

  ;; GdkWindowTypeHint gtk_window_get_type_hint (GtkWindow* window)
  (define-function int gtk_window_get_type_hint (void*))

  ;; gboolean gtk_window_get_urgency_hint (GtkWindow* window)
  (define-function int gtk_window_get_urgency_hint (void*))

  ;; void gtk_window_group_add_window (GtkWindowGroup* window_group, GtkWindow* window)
  (define-function void gtk_window_group_add_window (void* void*))

  ;; GType gtk_window_group_get_type (void)
  (define-function unsigned-long gtk_window_group_get_type ())

  ;; GList* gtk_window_group_list_windows (GtkWindowGroup* window_group)
  (define-function void* gtk_window_group_list_windows (void*))

  ;; GtkWindowGroup* gtk_window_group_new (void)
  (define-function void* gtk_window_group_new ())

  ;; void gtk_window_group_remove_window (GtkWindowGroup* window_group, GtkWindow* window)
  (define-function void gtk_window_group_remove_window (void* void*))

  ;; gboolean gtk_window_has_toplevel_focus (GtkWindow* window)
  (define-function int gtk_window_has_toplevel_focus (void*))

  ;; void gtk_window_iconify (GtkWindow* window)
  (define-function void gtk_window_iconify (void*))

  ;; gboolean gtk_window_is_active (GtkWindow* window)
  (define-function int gtk_window_is_active (void*))

  ;; GList* gtk_window_list_toplevels (void)
  (define-function void* gtk_window_list_toplevels ())

  ;; void gtk_window_maximize (GtkWindow* window)
  (define-function void gtk_window_maximize (void*))

  ;; gboolean gtk_window_mnemonic_activate (GtkWindow* window, guint keyval, GdkModifierType modifier)
  (define-function int gtk_window_mnemonic_activate (void* unsigned-int int))

  ;; void gtk_window_move (GtkWindow* window, gint x, gint y)
  (define-function void gtk_window_move (void* int int))

  ;; GtkWidget* gtk_window_new (GtkWindowType type)
  (define-function void* gtk_window_new (int))

  ;; gboolean gtk_window_parse_geometry (GtkWindow* window, const gchar* geometry)
  (define-function int gtk_window_parse_geometry (void* char*))

  ;; GType gtk_window_position_get_type (void)
  (define-function unsigned-long gtk_window_position_get_type ())

  ;; void gtk_window_present (GtkWindow* window)
  (define-function void gtk_window_present (void*))

  ;; void gtk_window_present_with_time (GtkWindow* window, guint32 timestamp)
  (define-function void gtk_window_present_with_time (void* uint32_t))

  ;; gboolean gtk_window_propagate_key_event (GtkWindow* window, GdkEventKey* event)
  (define-function int gtk_window_propagate_key_event (void* void*))

  ;; void gtk_window_remove_accel_group (GtkWindow* window, GtkAccelGroup* accel_group)
  (define-function void gtk_window_remove_accel_group (void* void*))

  ;; void gtk_window_remove_embedded_xid (GtkWindow* window, GdkNativeWindow xid)
  (define-function void gtk_window_remove_embedded_xid (void* uint32_t))

  ;; void gtk_window_remove_mnemonic (GtkWindow* window, guint keyval, GtkWidget* target)
  (define-function void gtk_window_remove_mnemonic (void* unsigned-int void*))

  ;; void gtk_window_reshow_with_initial_size (GtkWindow* window)
  (define-function void gtk_window_reshow_with_initial_size (void*))

  ;; void gtk_window_resize (GtkWindow* window, gint width, gint height)
  (define-function void gtk_window_resize (void* int int))

  ;; void gtk_window_set_accept_focus (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_accept_focus (void* int))

  ;; void gtk_window_set_auto_startup_notification (gboolean setting)
  (define-function void gtk_window_set_auto_startup_notification (int))

  ;; void gtk_window_set_decorated (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_decorated (void* int))

  ;; void gtk_window_set_default (GtkWindow* window, GtkWidget* default_widget)
  (define-function void gtk_window_set_default (void* void*))

  ;; void gtk_window_set_default_icon (GdkPixbuf* icon)
  (define-function void gtk_window_set_default_icon (void*))

  ;; gboolean gtk_window_set_default_icon_from_file (const gchar* filename, GError** err)
  (define-function int gtk_window_set_default_icon_from_file (char* void*))

  ;; void gtk_window_set_default_icon_list (GList* list)
  (define-function void gtk_window_set_default_icon_list (void*))

  ;; void gtk_window_set_default_icon_name (const gchar* name)
  (define-function void gtk_window_set_default_icon_name (char*))

  ;; void gtk_window_set_default_size (GtkWindow* window, gint width, gint height)
  (define-function void gtk_window_set_default_size (void* int int))

  ;; void gtk_window_set_deletable (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_deletable (void* int))

  ;; void gtk_window_set_destroy_with_parent (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_destroy_with_parent (void* int))

  ;; void gtk_window_set_focus (GtkWindow* window, GtkWidget* focus)
  (define-function void gtk_window_set_focus (void* void*))

  ;; void gtk_window_set_focus_on_map (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_focus_on_map (void* int))

  ;; void gtk_window_set_frame_dimensions (GtkWindow* window, gint left, gint top, gint right, gint bottom)
  (define-function void gtk_window_set_frame_dimensions (void* int int int int))

  ;; void gtk_window_set_geometry_hints (GtkWindow* window, GtkWidget* geometry_widget, GdkGeometry* geometry, GdkWindowHints geom_mask)
  (define-function void gtk_window_set_geometry_hints (void* void* void* int))

  ;; void gtk_window_set_gravity (GtkWindow* window, GdkGravity gravity)
  (define-function void gtk_window_set_gravity (void* int))

  ;; void gtk_window_set_has_frame (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_has_frame (void* int))

  ;; void gtk_window_set_icon (GtkWindow* window, GdkPixbuf* icon)
  (define-function void gtk_window_set_icon (void* void*))

  ;; gboolean gtk_window_set_icon_from_file (GtkWindow* window, const gchar* filename, GError** err)
  (define-function int gtk_window_set_icon_from_file (void* char* void*))

  ;; void gtk_window_set_icon_list (GtkWindow* window, GList* list)
  (define-function void gtk_window_set_icon_list (void* void*))

  ;; void gtk_window_set_icon_name (GtkWindow* window, const gchar* name)
  (define-function void gtk_window_set_icon_name (void* char*))

  ;; void gtk_window_set_keep_above (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_keep_above (void* int))

  ;; void gtk_window_set_keep_below (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_keep_below (void* int))

  ;; void gtk_window_set_mnemonic_modifier (GtkWindow* window, GdkModifierType modifier)
  (define-function void gtk_window_set_mnemonic_modifier (void* int))

  ;; void gtk_window_set_modal (GtkWindow* window, gboolean modal)
  (define-function void gtk_window_set_modal (void* int))

  ;; void gtk_window_set_opacity (GtkWindow* window, gdouble opacity)
  (define-function void gtk_window_set_opacity (void* double))

  ;; void gtk_window_set_position (GtkWindow* window, GtkWindowPosition position)
  (define-function void gtk_window_set_position (void* int))

  ;; void gtk_window_set_resizable (GtkWindow* window, gboolean resizable)
  (define-function void gtk_window_set_resizable (void* int))

  ;; void gtk_window_set_role (GtkWindow* window, const gchar* role)
  (define-function void gtk_window_set_role (void* char*))

  ;; void gtk_window_set_screen (GtkWindow* window, GdkScreen* screen)
  (define-function void gtk_window_set_screen (void* void*))

  ;; void gtk_window_set_skip_pager_hint (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_skip_pager_hint (void* int))

  ;; void gtk_window_set_skip_taskbar_hint (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_skip_taskbar_hint (void* int))

  ;; void gtk_window_set_startup_id (GtkWindow* window, const gchar* startup_id)
  (define-function void gtk_window_set_startup_id (void* char*))

  ;; void gtk_window_set_title (GtkWindow* window, const gchar* title)
  (define-function void gtk_window_set_title (void* char*))

  ;; void gtk_window_set_transient_for (GtkWindow* window, GtkWindow* parent)
  (define-function void gtk_window_set_transient_for (void* void*))

  ;; void gtk_window_set_type_hint (GtkWindow* window, GdkWindowTypeHint hint)
  (define-function void gtk_window_set_type_hint (void* int))

  ;; void gtk_window_set_urgency_hint (GtkWindow* window, gboolean setting)
  (define-function void gtk_window_set_urgency_hint (void* int))

  ;; void gtk_window_set_wmclass (GtkWindow* window, const gchar* wmclass_name, const gchar* wmclass_class)
  (define-function void gtk_window_set_wmclass (void* char* char*))

  ;; void gtk_window_stick (GtkWindow* window)
  (define-function void gtk_window_stick (void*))

  ;; GType gtk_window_type_get_type (void)
  (define-function unsigned-long gtk_window_type_get_type ())

  ;; void gtk_window_unfullscreen (GtkWindow* window)
  (define-function void gtk_window_unfullscreen (void*))

  ;; void gtk_window_unmaximize (GtkWindow* window)
  (define-function void gtk_window_unmaximize (void*))

  ;; void gtk_window_unstick (GtkWindow* window)
  (define-function void gtk_window_unstick (void*))

  ) ;[end]
