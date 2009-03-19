#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk window)

  (export gdk_window_add_filter
          gdk_window_at_pointer
          gdk_window_attributes_type_get_type
          gdk_window_beep
          gdk_window_begin_move_drag
          gdk_window_begin_paint_rect
          gdk_window_begin_paint_region
          gdk_window_begin_resize_drag
          gdk_window_class_get_type
          gdk_window_clear
          gdk_window_clear_area
          gdk_window_clear_area_e
          gdk_window_configure_finished
          gdk_window_constrain_size
          gdk_window_deiconify
          gdk_window_destroy
          gdk_window_edge_get_type
          gdk_window_enable_synchronized_configure
          gdk_window_end_paint
          gdk_window_focus
          gdk_window_foreign_new
          gdk_window_foreign_new_for_display
          gdk_window_freeze_toplevel_updates_libgtk_only
          gdk_window_freeze_updates
          gdk_window_fullscreen
          gdk_window_get_children
          gdk_window_get_decorations
          gdk_window_get_events
          gdk_window_get_frame_extents
          gdk_window_get_geometry
          gdk_window_get_group
          gdk_window_get_internal_paint_info
          gdk_window_get_origin
          gdk_window_get_parent
          gdk_window_get_pointer
          gdk_window_get_position
          gdk_window_get_root_origin
          gdk_window_get_state
          gdk_window_get_toplevel
          gdk_window_get_toplevels
          gdk_window_get_type_hint
          gdk_window_get_update_area
          gdk_window_get_user_data
          gdk_window_get_window_type
          gdk_window_hide
          gdk_window_hints_get_type
          gdk_window_iconify
          gdk_window_input_shape_combine_mask
          gdk_window_input_shape_combine_region
          gdk_window_invalidate_maybe_recurse
          gdk_window_invalidate_rect
          gdk_window_invalidate_region
          gdk_window_is_viewable
          gdk_window_is_visible
          gdk_window_lookup
          gdk_window_lookup_for_display
          gdk_window_lower
          gdk_window_maximize
          gdk_window_merge_child_input_shapes
          gdk_window_merge_child_shapes
          gdk_window_move
          gdk_window_move_region
          gdk_window_move_resize
          gdk_window_new
          gdk_window_object_get_type
          gdk_window_peek_children
          gdk_window_process_all_updates
          gdk_window_process_updates
          gdk_window_raise
          gdk_window_redirect_to_drawable
          gdk_window_register_dnd
          gdk_window_remove_filter
          gdk_window_remove_redirection
          gdk_window_reparent
          gdk_window_resize
          gdk_window_scroll
          gdk_window_set_accept_focus
          gdk_window_set_back_pixmap
          gdk_window_set_background
          gdk_window_set_child_input_shapes
          gdk_window_set_child_shapes
          gdk_window_set_composited
          gdk_window_set_cursor
          gdk_window_set_debug_updates
          gdk_window_set_decorations
          gdk_window_set_events
          gdk_window_set_focus_on_map
          gdk_window_set_functions
          gdk_window_set_geometry_hints
          gdk_window_set_group
          gdk_window_set_icon
          gdk_window_set_icon_list
          gdk_window_set_icon_name
          gdk_window_set_keep_above
          gdk_window_set_keep_below
          gdk_window_set_modal_hint
          gdk_window_set_opacity
          gdk_window_set_override_redirect
          gdk_window_set_role
          gdk_window_set_skip_pager_hint
          gdk_window_set_skip_taskbar_hint
          gdk_window_set_startup_id
          gdk_window_set_static_gravities
          gdk_window_set_title
          gdk_window_set_transient_for
          gdk_window_set_type_hint
          gdk_window_set_urgency_hint
          gdk_window_set_user_data
          gdk_window_shape_combine_mask
          gdk_window_shape_combine_region
          gdk_window_show
          gdk_window_show_unraised
          gdk_window_state_get_type
          gdk_window_stick
          gdk_window_thaw_toplevel_updates_libgtk_only
          gdk_window_thaw_updates
          gdk_window_type_get_type
          gdk_window_type_hint_get_type
          gdk_window_unfullscreen
          gdk_window_unmaximize
          gdk_window_unstick
          gdk_window_withdraw)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-windows "libgdk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; void gdk_window_add_filter (GdkWindow* window, GdkFilterFunc function, gpointer data)
  (define-function void gdk_window_add_filter (void* (c-callback int (void* void* void*)) void*))

  ;; GdkWindow* gdk_window_at_pointer (gint* win_x, gint* win_y)
  (define-function void* gdk_window_at_pointer (void* void*))

  ;; GType gdk_window_attributes_type_get_type (void)
  (define-function unsigned-long gdk_window_attributes_type_get_type ())

  ;; void gdk_window_beep (GdkWindow* window)
  (define-function void gdk_window_beep (void*))

  ;; void gdk_window_begin_move_drag (GdkWindow* window, gint button, gint root_x, gint root_y, guint32 timestamp)
  (define-function void gdk_window_begin_move_drag (void* int int int uint32_t))

  ;; void gdk_window_begin_paint_rect (GdkWindow* window, const GdkRectangle* rectangle)
  (define-function void gdk_window_begin_paint_rect (void* void*))

  ;; void gdk_window_begin_paint_region (GdkWindow* window, const GdkRegion* region)
  (define-function void gdk_window_begin_paint_region (void* void*))

  ;; void gdk_window_begin_resize_drag (GdkWindow* window, GdkWindowEdge edge, gint button, gint root_x, gint root_y, guint32 timestamp)
  (define-function void gdk_window_begin_resize_drag (void* int int int int uint32_t))

  ;; GType gdk_window_class_get_type (void)
  (define-function unsigned-long gdk_window_class_get_type ())

  ;; void gdk_window_clear (GdkWindow* window)
  (define-function void gdk_window_clear (void*))

  ;; void gdk_window_clear_area (GdkWindow* window, gint x, gint y, gint width, gint height)
  (define-function void gdk_window_clear_area (void* int int int int))

  ;; void gdk_window_clear_area_e (GdkWindow* window, gint x, gint y, gint width, gint height)
  (define-function void gdk_window_clear_area_e (void* int int int int))

  ;; void gdk_window_configure_finished (GdkWindow* window)
  (define-function void gdk_window_configure_finished (void*))

  ;; void gdk_window_constrain_size (GdkGeometry* geometry, guint flags, gint width, gint height, gint* new_width, gint* new_height)
  (define-function void gdk_window_constrain_size (void* unsigned-int int int void* void*))

  ;; void gdk_window_deiconify (GdkWindow* window)
  (define-function void gdk_window_deiconify (void*))

  ;; void gdk_window_destroy (GdkWindow* window)
  (define-function void gdk_window_destroy (void*))

  ;; GType gdk_window_edge_get_type (void)
  (define-function unsigned-long gdk_window_edge_get_type ())

  ;; void gdk_window_enable_synchronized_configure (GdkWindow* window)
  (define-function void gdk_window_enable_synchronized_configure (void*))

  ;; void gdk_window_end_paint (GdkWindow* window)
  (define-function void gdk_window_end_paint (void*))

  ;; void gdk_window_focus (GdkWindow* window, guint32 timestamp)
  (define-function void gdk_window_focus (void* uint32_t))

  ;; GdkWindow* gdk_window_foreign_new (GdkNativeWindow anid)
  (define-function void* gdk_window_foreign_new (uint32_t))

  ;; GdkWindow* gdk_window_foreign_new_for_display (GdkDisplay* display, GdkNativeWindow anid)
  (define-function void* gdk_window_foreign_new_for_display (void* uint32_t))

  ;; void gdk_window_freeze_toplevel_updates_libgtk_only (GdkWindow* window)
  (define-function void gdk_window_freeze_toplevel_updates_libgtk_only (void*))

  ;; void gdk_window_freeze_updates (GdkWindow* window)
  (define-function void gdk_window_freeze_updates (void*))

  ;; void gdk_window_fullscreen (GdkWindow* window)
  (define-function void gdk_window_fullscreen (void*))

  ;; GList* gdk_window_get_children (GdkWindow* window)
  (define-function void* gdk_window_get_children (void*))

  ;; gboolean gdk_window_get_decorations (GdkWindow* window, GdkWMDecoration* decorations)
  (define-function int gdk_window_get_decorations (void* void*))

  ;; GdkEventMask gdk_window_get_events (GdkWindow* window)
  (define-function int gdk_window_get_events (void*))

  ;; void gdk_window_get_frame_extents (GdkWindow* window, GdkRectangle* rect)
  (define-function void gdk_window_get_frame_extents (void* void*))

  ;; void gdk_window_get_geometry (GdkWindow* window, gint* x, gint* y, gint* width, gint* height, gint* depth)
  (define-function void gdk_window_get_geometry (void* void* void* void* void* void*))

  ;; GdkWindow* gdk_window_get_group (GdkWindow* window)
  (define-function void* gdk_window_get_group (void*))

  ;; void gdk_window_get_internal_paint_info (GdkWindow* window, GdkDrawable** real_drawable, gint* x_offset, gint* y_offset)
  (define-function void gdk_window_get_internal_paint_info (void* void* void* void*))

  ;; gint gdk_window_get_origin (GdkWindow* window, gint* x, gint* y)
  (define-function int gdk_window_get_origin (void* void* void*))

  ;; GdkWindow* gdk_window_get_parent (GdkWindow* window)
  (define-function void* gdk_window_get_parent (void*))

  ;; GdkWindow* gdk_window_get_pointer (GdkWindow* window, gint* x, gint* y, GdkModifierType* mask)
  (define-function void* gdk_window_get_pointer (void* void* void* void*))

  ;; void gdk_window_get_position (GdkWindow* window, gint* x, gint* y)
  (define-function void gdk_window_get_position (void* void* void*))

  ;; void gdk_window_get_root_origin (GdkWindow* window, gint* x, gint* y)
  (define-function void gdk_window_get_root_origin (void* void* void*))

  ;; GdkWindowState gdk_window_get_state (GdkWindow* window)
  (define-function int gdk_window_get_state (void*))

  ;; GdkWindow* gdk_window_get_toplevel (GdkWindow* window)
  (define-function void* gdk_window_get_toplevel (void*))

  ;; GList* gdk_window_get_toplevels (void)
  (define-function void* gdk_window_get_toplevels ())

  ;; GdkWindowTypeHint gdk_window_get_type_hint (GdkWindow* window)
  (define-function int gdk_window_get_type_hint (void*))

  ;; GdkRegion* gdk_window_get_update_area (GdkWindow* window)
  (define-function void* gdk_window_get_update_area (void*))

  ;; void gdk_window_get_user_data (GdkWindow* window, gpointer* data)
  (define-function void gdk_window_get_user_data (void* void*))

  ;; GdkWindowType gdk_window_get_window_type (GdkWindow* window)
  (define-function int gdk_window_get_window_type (void*))

  ;; void gdk_window_hide (GdkWindow* window)
  (define-function void gdk_window_hide (void*))

  ;; GType gdk_window_hints_get_type (void)
  (define-function unsigned-long gdk_window_hints_get_type ())

  ;; void gdk_window_iconify (GdkWindow* window)
  (define-function void gdk_window_iconify (void*))

  ;; void gdk_window_input_shape_combine_mask (GdkWindow* window, GdkBitmap* mask, gint x, gint y)
  (define-function void gdk_window_input_shape_combine_mask (void* void* int int))

  ;; void gdk_window_input_shape_combine_region (GdkWindow* window, const GdkRegion* shape_region, gint offset_x, gint offset_y)
  (define-function void gdk_window_input_shape_combine_region (void* void* int int))

  ;; void gdk_window_invalidate_maybe_recurse (GdkWindow* window, const GdkRegion* region, gboolean (*child_func) (GdkWindow* , gpointer), gpointer user_data)
  (define-function void gdk_window_invalidate_maybe_recurse (void* void* (c-callback int (void* void*)) void*))

  ;; void gdk_window_invalidate_rect (GdkWindow* window, const GdkRectangle* rect, gboolean invalidate_children)
  (define-function void gdk_window_invalidate_rect (void* void* int))

  ;; void gdk_window_invalidate_region (GdkWindow* window, const GdkRegion* region, gboolean invalidate_children)
  (define-function void gdk_window_invalidate_region (void* void* int))

  ;; gboolean gdk_window_is_viewable (GdkWindow* window)
  (define-function int gdk_window_is_viewable (void*))

  ;; gboolean gdk_window_is_visible (GdkWindow* window)
  (define-function int gdk_window_is_visible (void*))

  ;; GdkWindow* gdk_window_lookup (GdkNativeWindow anid)
  (define-function void* gdk_window_lookup (uint32_t))

  ;; GdkWindow* gdk_window_lookup_for_display (GdkDisplay* display, GdkNativeWindow anid)
  (define-function void* gdk_window_lookup_for_display (void* uint32_t))

  ;; void gdk_window_lower (GdkWindow* window)
  (define-function void gdk_window_lower (void*))

  ;; void gdk_window_maximize (GdkWindow* window)
  (define-function void gdk_window_maximize (void*))

  ;; void gdk_window_merge_child_input_shapes (GdkWindow* window)
  (define-function void gdk_window_merge_child_input_shapes (void*))

  ;; void gdk_window_merge_child_shapes (GdkWindow* window)
  (define-function void gdk_window_merge_child_shapes (void*))

  ;; void gdk_window_move (GdkWindow* window, gint x, gint y)
  (define-function void gdk_window_move (void* int int))

  ;; void gdk_window_move_region (GdkWindow* window, const GdkRegion* region, gint dx, gint dy)
  (define-function void gdk_window_move_region (void* void* int int))

  ;; void gdk_window_move_resize (GdkWindow* window, gint x, gint y, gint width, gint height)
  (define-function void gdk_window_move_resize (void* int int int int))

  ;; GdkWindow* gdk_window_new (GdkWindow* parent, GdkWindowAttr* attributes, gint attributes_mask)
  (define-function void* gdk_window_new (void* void* int))

  ;; GType gdk_window_object_get_type (void)
  (define-function unsigned-long gdk_window_object_get_type ())

  ;; GList* gdk_window_peek_children (GdkWindow* window)
  (define-function void* gdk_window_peek_children (void*))

  ;; void gdk_window_process_all_updates (void)
  (define-function void gdk_window_process_all_updates ())

  ;; void gdk_window_process_updates (GdkWindow* window, gboolean update_children)
  (define-function void gdk_window_process_updates (void* int))

  ;; void gdk_window_raise (GdkWindow* window)
  (define-function void gdk_window_raise (void*))

  ;; void gdk_window_redirect_to_drawable (GdkWindow* window, GdkDrawable* drawable, gint src_x, gint src_y, gint dest_x, gint dest_y, gint width, gint height)
  (define-function void gdk_window_redirect_to_drawable (void* void* int int int int int int))

  ;; void gdk_window_register_dnd (GdkWindow* window)
  (define-function void gdk_window_register_dnd (void*))

  ;; void gdk_window_remove_filter (GdkWindow* window, GdkFilterFunc function, gpointer data)
  (define-function void gdk_window_remove_filter (void* (c-callback int (void* void* void*)) void*))

  ;; void gdk_window_remove_redirection (GdkWindow* window)
  (define-function void gdk_window_remove_redirection (void*))

  ;; void gdk_window_reparent (GdkWindow* window, GdkWindow* new_parent, gint x, gint y)
  (define-function void gdk_window_reparent (void* void* int int))

  ;; void gdk_window_resize (GdkWindow* window, gint width, gint height)
  (define-function void gdk_window_resize (void* int int))

  ;; void gdk_window_scroll (GdkWindow* window, gint dx, gint dy)
  (define-function void gdk_window_scroll (void* int int))

  ;; void gdk_window_set_accept_focus (GdkWindow* window, gboolean accept_focus)
  (define-function void gdk_window_set_accept_focus (void* int))

  ;; void gdk_window_set_back_pixmap (GdkWindow* window, GdkPixmap* pixmap, gboolean parent_relative)
  (define-function void gdk_window_set_back_pixmap (void* void* int))

  ;; void gdk_window_set_background (GdkWindow* window, const GdkColor* color)
  (define-function void gdk_window_set_background (void* void*))

  ;; void gdk_window_set_child_input_shapes (GdkWindow* window)
  (define-function void gdk_window_set_child_input_shapes (void*))

  ;; void gdk_window_set_child_shapes (GdkWindow* window)
  (define-function void gdk_window_set_child_shapes (void*))

  ;; void gdk_window_set_composited (GdkWindow* window, gboolean composited)
  (define-function void gdk_window_set_composited (void* int))

  ;; void gdk_window_set_cursor (GdkWindow* window, GdkCursor* cursor)
  (define-function void gdk_window_set_cursor (void* void*))

  ;; void gdk_window_set_debug_updates (gboolean setting)
  (define-function void gdk_window_set_debug_updates (int))

  ;; void gdk_window_set_decorations (GdkWindow* window, GdkWMDecoration decorations)
  (define-function void gdk_window_set_decorations (void* int))

  ;; void gdk_window_set_events (GdkWindow* window, GdkEventMask event_mask)
  (define-function void gdk_window_set_events (void* int))

  ;; void gdk_window_set_focus_on_map (GdkWindow* window, gboolean focus_on_map)
  (define-function void gdk_window_set_focus_on_map (void* int))

  ;; void gdk_window_set_functions (GdkWindow* window, GdkWMFunction functions)
  (define-function void gdk_window_set_functions (void* int))

  ;; void gdk_window_set_geometry_hints (GdkWindow* window, const GdkGeometry* geometry, GdkWindowHints geom_mask)
  (define-function void gdk_window_set_geometry_hints (void* void* int))

  ;; void gdk_window_set_group (GdkWindow* window, GdkWindow* leader)
  (define-function void gdk_window_set_group (void* void*))

  ;; void gdk_window_set_icon (GdkWindow* window, GdkWindow* icon_window, GdkPixmap* pixmap, GdkBitmap* mask)
  (define-function void gdk_window_set_icon (void* void* void* void*))

  ;; void gdk_window_set_icon_list (GdkWindow* window, GList* pixbufs)
  (define-function void gdk_window_set_icon_list (void* void*))

  ;; void gdk_window_set_icon_name (GdkWindow* window, const gchar* name)
  (define-function void gdk_window_set_icon_name (void* char*))

  ;; void gdk_window_set_keep_above (GdkWindow* window, gboolean setting)
  (define-function void gdk_window_set_keep_above (void* int))

  ;; void gdk_window_set_keep_below (GdkWindow* window, gboolean setting)
  (define-function void gdk_window_set_keep_below (void* int))

  ;; void gdk_window_set_modal_hint (GdkWindow* window, gboolean modal)
  (define-function void gdk_window_set_modal_hint (void* int))

  ;; void gdk_window_set_opacity (GdkWindow* window, gdouble opacity)
  (define-function void gdk_window_set_opacity (void* double))

  ;; void gdk_window_set_override_redirect (GdkWindow* window, gboolean override_redirect)
  (define-function void gdk_window_set_override_redirect (void* int))

  ;; void gdk_window_set_role (GdkWindow* window, const gchar* role)
  (define-function void gdk_window_set_role (void* char*))

  ;; void gdk_window_set_skip_pager_hint (GdkWindow* window, gboolean skips_pager)
  (define-function void gdk_window_set_skip_pager_hint (void* int))

  ;; void gdk_window_set_skip_taskbar_hint (GdkWindow* window, gboolean skips_taskbar)
  (define-function void gdk_window_set_skip_taskbar_hint (void* int))

  ;; void gdk_window_set_startup_id (GdkWindow* window, const gchar* startup_id)
  (define-function void gdk_window_set_startup_id (void* char*))

  ;; gboolean gdk_window_set_static_gravities (GdkWindow* window, gboolean use_static)
  (define-function int gdk_window_set_static_gravities (void* int))

  ;; void gdk_window_set_title (GdkWindow* window, const gchar* title)
  (define-function void gdk_window_set_title (void* char*))

  ;; void gdk_window_set_transient_for (GdkWindow* window, GdkWindow* parent)
  (define-function void gdk_window_set_transient_for (void* void*))

  ;; void gdk_window_set_type_hint (GdkWindow* window, GdkWindowTypeHint hint)
  (define-function void gdk_window_set_type_hint (void* int))

  ;; void gdk_window_set_urgency_hint (GdkWindow* window, gboolean urgent)
  (define-function void gdk_window_set_urgency_hint (void* int))

  ;; void gdk_window_set_user_data (GdkWindow* window, gpointer user_data)
  (define-function void gdk_window_set_user_data (void* void*))

  ;; void gdk_window_shape_combine_mask (GdkWindow* window, GdkBitmap* mask, gint x, gint y)
  (define-function void gdk_window_shape_combine_mask (void* void* int int))

  ;; void gdk_window_shape_combine_region (GdkWindow* window, const GdkRegion* shape_region, gint offset_x, gint offset_y)
  (define-function void gdk_window_shape_combine_region (void* void* int int))

  ;; void gdk_window_show (GdkWindow* window)
  (define-function void gdk_window_show (void*))

  ;; void gdk_window_show_unraised (GdkWindow* window)
  (define-function void gdk_window_show_unraised (void*))

  ;; GType gdk_window_state_get_type (void)
  (define-function unsigned-long gdk_window_state_get_type ())

  ;; void gdk_window_stick (GdkWindow* window)
  (define-function void gdk_window_stick (void*))

  ;; void gdk_window_thaw_toplevel_updates_libgtk_only (GdkWindow* window)
  (define-function void gdk_window_thaw_toplevel_updates_libgtk_only (void*))

  ;; void gdk_window_thaw_updates (GdkWindow* window)
  (define-function void gdk_window_thaw_updates (void*))

  ;; GType gdk_window_type_get_type (void)
  (define-function unsigned-long gdk_window_type_get_type ())

  ;; GType gdk_window_type_hint_get_type (void)
  (define-function unsigned-long gdk_window_type_hint_get_type ())

  ;; void gdk_window_unfullscreen (GdkWindow* window)
  (define-function void gdk_window_unfullscreen (void*))

  ;; void gdk_window_unmaximize (GdkWindow* window)
  (define-function void gdk_window_unmaximize (void*))

  ;; void gdk_window_unstick (GdkWindow* window)
  (define-function void gdk_window_unstick (void*))

  ;; void gdk_window_withdraw (GdkWindow* window)
  (define-function void gdk_window_withdraw (void*))

  ) ;[end]
