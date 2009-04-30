#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk widget)

  (export gtk_widget_activate
          gtk_widget_add_accelerator
          gtk_widget_add_events
          gtk_widget_add_mnemonic_label
          gtk_widget_can_activate_accel
          gtk_widget_child_focus
          gtk_widget_child_notify
          gtk_widget_class_find_style_property
          gtk_widget_class_install_style_property
          gtk_widget_class_install_style_property_parser
          gtk_widget_class_list_style_properties
          gtk_widget_class_path
          gtk_widget_create_pango_context
          gtk_widget_create_pango_layout
          gtk_widget_destroy
          gtk_widget_destroyed
          gtk_widget_ensure_style
          gtk_widget_error_bell
          gtk_widget_event
          gtk_widget_flags_get_type
          gtk_widget_freeze_child_notify
          gtk_widget_get_accessible
          gtk_widget_get_ancestor
          gtk_widget_get_child_requisition
          gtk_widget_get_child_visible
          gtk_widget_get_clipboard
          gtk_widget_get_colormap
          gtk_widget_get_composite_name
          gtk_widget_get_default_colormap
          gtk_widget_get_default_direction
          gtk_widget_get_default_style
          gtk_widget_get_default_visual
          gtk_widget_get_direction
          gtk_widget_get_display
          gtk_widget_get_events
          gtk_widget_get_extension_events
          gtk_widget_get_has_tooltip
          gtk_widget_get_modifier_style
          gtk_widget_get_name
          gtk_widget_get_no_show_all
          gtk_widget_get_pango_context
          gtk_widget_get_parent
          gtk_widget_get_parent_window
          gtk_widget_get_pointer
          gtk_widget_get_root_window
          gtk_widget_get_screen
          gtk_widget_get_settings
          gtk_widget_get_size_request
          gtk_widget_get_snapshot
          gtk_widget_get_style
          gtk_widget_get_tooltip_markup
          gtk_widget_get_tooltip_text
          gtk_widget_get_tooltip_window
          gtk_widget_get_toplevel
          gtk_widget_get_type
          gtk_widget_get_visual
          gtk_widget_get_window
          gtk_widget_grab_default
          gtk_widget_grab_focus
          gtk_widget_has_screen
          gtk_widget_help_type_get_type
          gtk_widget_hide
          gtk_widget_hide_all
          gtk_widget_hide_on_delete
          gtk_widget_input_shape_combine_mask
          gtk_widget_intersect
          gtk_widget_is_ancestor
          gtk_widget_is_composited
          gtk_widget_is_focus
          gtk_widget_keynav_failed
          gtk_widget_list_accel_closures
          gtk_widget_list_mnemonic_labels
          gtk_widget_map
          gtk_widget_mnemonic_activate
          gtk_widget_modify_base
          gtk_widget_modify_bg
          gtk_widget_modify_cursor
          gtk_widget_modify_fg
          gtk_widget_modify_font
          gtk_widget_modify_style
          gtk_widget_modify_text
          gtk_widget_new
          gtk_widget_path
          gtk_widget_pop_colormap
          gtk_widget_pop_composite_child
          gtk_widget_push_colormap
          gtk_widget_push_composite_child
          gtk_widget_queue_draw
          gtk_widget_queue_draw_area
          gtk_widget_queue_resize
          gtk_widget_queue_resize_no_redraw
          gtk_widget_realize
          gtk_widget_region_intersect
          gtk_widget_remove_accelerator
          gtk_widget_remove_mnemonic_label
          gtk_widget_render_icon
          gtk_widget_reparent
          gtk_widget_reset_rc_styles
          gtk_widget_reset_shapes
          gtk_widget_send_expose
          gtk_widget_set_accel_path
          gtk_widget_set_app_paintable
          gtk_widget_set_child_visible
          gtk_widget_set_colormap
          gtk_widget_set_composite_name
          gtk_widget_set_default_colormap
          gtk_widget_set_default_direction
          gtk_widget_set_direction
          gtk_widget_set_double_buffered
          gtk_widget_set_events
          gtk_widget_set_extension_events
          gtk_widget_set_has_tooltip
          gtk_widget_set_name
          gtk_widget_set_no_show_all
          gtk_widget_set_parent
          gtk_widget_set_parent_window
          gtk_widget_set_redraw_on_allocate
          gtk_widget_set_scroll_adjustments
          gtk_widget_set_sensitive
          gtk_widget_set_size_request
          gtk_widget_set_state
          gtk_widget_set_style
          gtk_widget_set_tooltip_markup
          gtk_widget_set_tooltip_text
          gtk_widget_set_tooltip_window
          gtk_widget_shape_combine_mask
          gtk_widget_show
          gtk_widget_show_all
          gtk_widget_show_now
          gtk_widget_size_allocate
          gtk_widget_size_request
          gtk_widget_style_get
          gtk_widget_style_get_property
          gtk_widget_style_get_valist
          gtk_widget_thaw_child_notify
          gtk_widget_translate_coordinates
          gtk_widget_trigger_tooltip_query
          gtk_widget_unmap
          gtk_widget_unparent
          gtk_widget_unrealize)

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

  ;; gboolean gtk_widget_activate (GtkWidget* widget)
  (define-function int gtk_widget_activate (void*))

  ;; void gtk_widget_add_accelerator (GtkWidget* widget, const gchar* accel_signal, GtkAccelGroup* accel_group, guint accel_key, GdkModifierType accel_mods, GtkAccelFlags accel_flags)
  (define-function void gtk_widget_add_accelerator (void* char* void* unsigned-int int int))

  ;; void gtk_widget_add_events (GtkWidget* widget, gint events)
  (define-function void gtk_widget_add_events (void* int))

  ;; void gtk_widget_add_mnemonic_label (GtkWidget* widget, GtkWidget* label)
  (define-function void gtk_widget_add_mnemonic_label (void* void*))

  ;; gboolean gtk_widget_can_activate_accel (GtkWidget* widget, guint signal_id)
  (define-function int gtk_widget_can_activate_accel (void* unsigned-int))

  ;; gboolean gtk_widget_child_focus (GtkWidget* widget, GtkDirectionType direction)
  (define-function int gtk_widget_child_focus (void* int))

  ;; void gtk_widget_child_notify (GtkWidget* widget, const gchar* child_property)
  (define-function void gtk_widget_child_notify (void* char*))

  ;; GParamSpec* gtk_widget_class_find_style_property (GtkWidgetClass* klass, const gchar* property_name)
  (define-function void* gtk_widget_class_find_style_property (void* char*))

  ;; void gtk_widget_class_install_style_property (GtkWidgetClass* klass, GParamSpec* pspec)
  (define-function void gtk_widget_class_install_style_property (void* void*))

  ;; void gtk_widget_class_install_style_property_parser (GtkWidgetClass* klass, GParamSpec* pspec, GtkRcPropertyParser parser)
  (define-function void gtk_widget_class_install_style_property_parser (void* void* (c-callback int (void* void* void*))))

  ;; GParamSpec** gtk_widget_class_list_style_properties (GtkWidgetClass* klass, guint* n_properties)
  (define-function void* gtk_widget_class_list_style_properties (void* void*))

  ;; void gtk_widget_class_path (GtkWidget* widget, guint* path_length, gchar** path, gchar** path_reversed)
  (define-function void gtk_widget_class_path (void* void* void* void*))

  ;; PangoContext* gtk_widget_create_pango_context (GtkWidget* widget)
  (define-function void* gtk_widget_create_pango_context (void*))

  ;; PangoLayout* gtk_widget_create_pango_layout (GtkWidget* widget, const gchar* text)
  (define-function void* gtk_widget_create_pango_layout (void* char*))

  ;; void gtk_widget_destroy (GtkWidget* widget)
  (define-function void gtk_widget_destroy (void*))

  ;; void gtk_widget_destroyed (GtkWidget* widget, GtkWidget** widget_pointer)
  (define-function void gtk_widget_destroyed (void* void*))

  ;; void gtk_widget_ensure_style (GtkWidget* widget)
  (define-function void gtk_widget_ensure_style (void*))

  ;; void gtk_widget_error_bell (GtkWidget* widget)
  (define-function void gtk_widget_error_bell (void*))

  ;; gboolean gtk_widget_event (GtkWidget* widget, GdkEvent* event)
  (define-function int gtk_widget_event (void* void*))

  ;; GType gtk_widget_flags_get_type (void)
  (define-function unsigned-long gtk_widget_flags_get_type ())

  ;; void gtk_widget_freeze_child_notify (GtkWidget* widget)
  (define-function void gtk_widget_freeze_child_notify (void*))

  ;; AtkObject* gtk_widget_get_accessible (GtkWidget* widget)
  (define-function void* gtk_widget_get_accessible (void*))

  ;; GtkWidget* gtk_widget_get_ancestor (GtkWidget* widget, GType widget_type)
  (define-function void* gtk_widget_get_ancestor (void* unsigned-long))

  ;; void gtk_widget_get_child_requisition (GtkWidget* widget, GtkRequisition* requisition)
  (define-function void gtk_widget_get_child_requisition (void* void*))

  ;; gboolean gtk_widget_get_child_visible (GtkWidget* widget)
  (define-function int gtk_widget_get_child_visible (void*))

  ;; GtkClipboard* gtk_widget_get_clipboard (GtkWidget* widget, GdkAtom selection)
  (define-function void* gtk_widget_get_clipboard (void* void*))

  ;; GdkColormap* gtk_widget_get_colormap (GtkWidget* widget)
  (define-function void* gtk_widget_get_colormap (void*))

  ;; gchar* gtk_widget_get_composite_name (GtkWidget* widget)
  (define-function char* gtk_widget_get_composite_name (void*))

  ;; GdkColormap* gtk_widget_get_default_colormap (void)
  (define-function void* gtk_widget_get_default_colormap ())

  ;; GtkTextDirection gtk_widget_get_default_direction (void)
  (define-function int gtk_widget_get_default_direction ())

  ;; GtkStyle* gtk_widget_get_default_style (void)
  (define-function void* gtk_widget_get_default_style ())

  ;; GdkVisual* gtk_widget_get_default_visual (void)
  (define-function void* gtk_widget_get_default_visual ())

  ;; GtkTextDirection gtk_widget_get_direction (GtkWidget* widget)
  (define-function int gtk_widget_get_direction (void*))

  ;; GdkDisplay* gtk_widget_get_display (GtkWidget* widget)
  (define-function void* gtk_widget_get_display (void*))

  ;; gint gtk_widget_get_events (GtkWidget* widget)
  (define-function int gtk_widget_get_events (void*))

  ;; GdkExtensionMode gtk_widget_get_extension_events (GtkWidget* widget)
  (define-function int gtk_widget_get_extension_events (void*))

  ;; gboolean gtk_widget_get_has_tooltip (GtkWidget* widget)
  (define-function int gtk_widget_get_has_tooltip (void*))

  ;; GtkRcStyle* gtk_widget_get_modifier_style (GtkWidget* widget)
  (define-function void* gtk_widget_get_modifier_style (void*))

  ;; const gchar* gtk_widget_get_name (GtkWidget* widget)
  (define-function char* gtk_widget_get_name (void*))

  ;; gboolean gtk_widget_get_no_show_all (GtkWidget* widget)
  (define-function int gtk_widget_get_no_show_all (void*))

  ;; PangoContext* gtk_widget_get_pango_context (GtkWidget* widget)
  (define-function void* gtk_widget_get_pango_context (void*))

  ;; GtkWidget* gtk_widget_get_parent (GtkWidget* widget)
  (define-function void* gtk_widget_get_parent (void*))

  ;; GdkWindow* gtk_widget_get_parent_window (GtkWidget* widget)
  (define-function void* gtk_widget_get_parent_window (void*))

  ;; void gtk_widget_get_pointer (GtkWidget* widget, gint* x, gint* y)
  (define-function void gtk_widget_get_pointer (void* void* void*))

  ;; GdkWindow* gtk_widget_get_root_window (GtkWidget* widget)
  (define-function void* gtk_widget_get_root_window (void*))

  ;; GdkScreen* gtk_widget_get_screen (GtkWidget* widget)
  (define-function void* gtk_widget_get_screen (void*))

  ;; GtkSettings* gtk_widget_get_settings (GtkWidget* widget)
  (define-function void* gtk_widget_get_settings (void*))

  ;; void gtk_widget_get_size_request (GtkWidget* widget, gint* width, gint* height)
  (define-function void gtk_widget_get_size_request (void* void* void*))

  ;; GdkPixmap* gtk_widget_get_snapshot (GtkWidget* widget, GdkRectangle* clip_rect)
  (define-function void* gtk_widget_get_snapshot (void* void*))

  ;; GtkStyle* gtk_widget_get_style (GtkWidget* widget)
  (define-function void* gtk_widget_get_style (void*))

  ;; gchar* gtk_widget_get_tooltip_markup (GtkWidget* widget)
  (define-function char* gtk_widget_get_tooltip_markup (void*))

  ;; gchar* gtk_widget_get_tooltip_text (GtkWidget* widget)
  (define-function char* gtk_widget_get_tooltip_text (void*))

  ;; GtkWindow* gtk_widget_get_tooltip_window (GtkWidget* widget)
  (define-function void* gtk_widget_get_tooltip_window (void*))

  ;; GtkWidget* gtk_widget_get_toplevel (GtkWidget* widget)
  (define-function void* gtk_widget_get_toplevel (void*))

  ;; GType gtk_widget_get_type (void)
  (define-function unsigned-long gtk_widget_get_type ())

  ;; GdkVisual* gtk_widget_get_visual (GtkWidget* widget)
  (define-function void* gtk_widget_get_visual (void*))

  ;; GdkWindow* gtk_widget_get_window (GtkWidget* widget)
  (define-function void* gtk_widget_get_window (void*))

  ;; void gtk_widget_grab_default (GtkWidget* widget)
  (define-function void gtk_widget_grab_default (void*))

  ;; void gtk_widget_grab_focus (GtkWidget* widget)
  (define-function void gtk_widget_grab_focus (void*))

  ;; gboolean gtk_widget_has_screen (GtkWidget* widget)
  (define-function int gtk_widget_has_screen (void*))

  ;; GType gtk_widget_help_type_get_type (void)
  (define-function unsigned-long gtk_widget_help_type_get_type ())

  ;; void gtk_widget_hide (GtkWidget* widget)
  (define-function void gtk_widget_hide (void*))

  ;; void gtk_widget_hide_all (GtkWidget* widget)
  (define-function void gtk_widget_hide_all (void*))

  ;; gboolean gtk_widget_hide_on_delete (GtkWidget* widget)
  (define-function int gtk_widget_hide_on_delete (void*))

  ;; void gtk_widget_input_shape_combine_mask (GtkWidget* widget, GdkBitmap* shape_mask, gint offset_x, gint offset_y)
  (define-function void gtk_widget_input_shape_combine_mask (void* void* int int))

  ;; gboolean gtk_widget_intersect (GtkWidget* widget, const GdkRectangle* area, GdkRectangle* intersection)
  (define-function int gtk_widget_intersect (void* void* void*))

  ;; gboolean gtk_widget_is_ancestor (GtkWidget* widget, GtkWidget* ancestor)
  (define-function int gtk_widget_is_ancestor (void* void*))

  ;; gboolean gtk_widget_is_composited (GtkWidget* widget)
  (define-function int gtk_widget_is_composited (void*))

  ;; gboolean gtk_widget_is_focus (GtkWidget* widget)
  (define-function int gtk_widget_is_focus (void*))

  ;; gboolean gtk_widget_keynav_failed (GtkWidget* widget, GtkDirectionType direction)
  (define-function int gtk_widget_keynav_failed (void* int))

  ;; GList* gtk_widget_list_accel_closures (GtkWidget* widget)
  (define-function void* gtk_widget_list_accel_closures (void*))

  ;; GList* gtk_widget_list_mnemonic_labels (GtkWidget* widget)
  (define-function void* gtk_widget_list_mnemonic_labels (void*))

  ;; void gtk_widget_map (GtkWidget* widget)
  (define-function void gtk_widget_map (void*))

  ;; gboolean gtk_widget_mnemonic_activate (GtkWidget* widget, gboolean group_cycling)
  (define-function int gtk_widget_mnemonic_activate (void* int))

  ;; void gtk_widget_modify_base (GtkWidget* widget, GtkStateType state, const GdkColor* color)
  (define-function void gtk_widget_modify_base (void* int void*))

  ;; void gtk_widget_modify_bg (GtkWidget* widget, GtkStateType state, const GdkColor* color)
  (define-function void gtk_widget_modify_bg (void* int void*))

  ;; void gtk_widget_modify_cursor (GtkWidget* widget, const GdkColor* primary, const GdkColor* secondary)
  (define-function void gtk_widget_modify_cursor (void* void* void*))

  ;; void gtk_widget_modify_fg (GtkWidget* widget, GtkStateType state, const GdkColor* color)
  (define-function void gtk_widget_modify_fg (void* int void*))

  ;; void gtk_widget_modify_font (GtkWidget* widget, PangoFontDescription* font_desc)
  (define-function void gtk_widget_modify_font (void* void*))

  ;; void gtk_widget_modify_style (GtkWidget* widget, GtkRcStyle* style)
  (define-function void gtk_widget_modify_style (void* void*))

  ;; void gtk_widget_modify_text (GtkWidget* widget, GtkStateType state, const GdkColor* color)
  (define-function void gtk_widget_modify_text (void* int void*))

  ;; GtkWidget* gtk_widget_new (GType type, const gchar* first_property_name, ...)
  (define-function void* gtk_widget_new (unsigned-long char* ...))

  ;; void gtk_widget_path (GtkWidget* widget, guint* path_length, gchar** path, gchar** path_reversed)
  (define-function void gtk_widget_path (void* void* void* void*))

  ;; void gtk_widget_pop_colormap (void)
  (define-function void gtk_widget_pop_colormap ())

  ;; void gtk_widget_pop_composite_child (void)
  (define-function void gtk_widget_pop_composite_child ())

  ;; void gtk_widget_push_colormap (GdkColormap* cmap)
  (define-function void gtk_widget_push_colormap (void*))

  ;; void gtk_widget_push_composite_child (void)
  (define-function void gtk_widget_push_composite_child ())

  ;; void gtk_widget_queue_draw (GtkWidget* widget)
  (define-function void gtk_widget_queue_draw (void*))

  ;; void gtk_widget_queue_draw_area (GtkWidget* widget, gint x, gint y, gint width, gint height)
  (define-function void gtk_widget_queue_draw_area (void* int int int int))

  ;; void gtk_widget_queue_resize (GtkWidget* widget)
  (define-function void gtk_widget_queue_resize (void*))

  ;; void gtk_widget_queue_resize_no_redraw (GtkWidget* widget)
  (define-function void gtk_widget_queue_resize_no_redraw (void*))

  ;; void gtk_widget_realize (GtkWidget* widget)
  (define-function void gtk_widget_realize (void*))

  ;; GdkRegion* gtk_widget_region_intersect (GtkWidget* widget, const GdkRegion* region)
  (define-function void* gtk_widget_region_intersect (void* void*))

  ;; gboolean gtk_widget_remove_accelerator (GtkWidget* widget, GtkAccelGroup* accel_group, guint accel_key, GdkModifierType accel_mods)
  (define-function int gtk_widget_remove_accelerator (void* void* unsigned-int int))

  ;; void gtk_widget_remove_mnemonic_label (GtkWidget* widget, GtkWidget* label)
  (define-function void gtk_widget_remove_mnemonic_label (void* void*))

  ;; GdkPixbuf* gtk_widget_render_icon (GtkWidget* widget, const gchar* stock_id, GtkIconSize size, const gchar* detail)
  (define-function void* gtk_widget_render_icon (void* char* int char*))

  ;; void gtk_widget_reparent (GtkWidget* widget, GtkWidget* new_parent)
  (define-function void gtk_widget_reparent (void* void*))

  ;; void gtk_widget_reset_rc_styles (GtkWidget* widget)
  (define-function void gtk_widget_reset_rc_styles (void*))

  ;; void gtk_widget_reset_shapes (GtkWidget* widget)
  (define-function void gtk_widget_reset_shapes (void*))

  ;; gint gtk_widget_send_expose (GtkWidget* widget, GdkEvent* event)
  (define-function int gtk_widget_send_expose (void* void*))

  ;; void gtk_widget_set_accel_path (GtkWidget* widget, const gchar* accel_path, GtkAccelGroup* accel_group)
  (define-function void gtk_widget_set_accel_path (void* char* void*))

  ;; void gtk_widget_set_app_paintable (GtkWidget* widget, gboolean app_paintable)
  (define-function void gtk_widget_set_app_paintable (void* int))

  ;; void gtk_widget_set_child_visible (GtkWidget* widget, gboolean is_visible)
  (define-function void gtk_widget_set_child_visible (void* int))

  ;; void gtk_widget_set_colormap (GtkWidget* widget, GdkColormap* colormap)
  (define-function void gtk_widget_set_colormap (void* void*))

  ;; void gtk_widget_set_composite_name (GtkWidget* widget, const gchar* name)
  (define-function void gtk_widget_set_composite_name (void* char*))

  ;; void gtk_widget_set_default_colormap (GdkColormap* colormap)
  (define-function void gtk_widget_set_default_colormap (void*))

  ;; void gtk_widget_set_default_direction (GtkTextDirection dir)
  (define-function void gtk_widget_set_default_direction (int))

  ;; void gtk_widget_set_direction (GtkWidget* widget, GtkTextDirection dir)
  (define-function void gtk_widget_set_direction (void* int))

  ;; void gtk_widget_set_double_buffered (GtkWidget* widget, gboolean double_buffered)
  (define-function void gtk_widget_set_double_buffered (void* int))

  ;; void gtk_widget_set_events (GtkWidget* widget, gint events)
  (define-function void gtk_widget_set_events (void* int))

  ;; void gtk_widget_set_extension_events (GtkWidget* widget, GdkExtensionMode mode)
  (define-function void gtk_widget_set_extension_events (void* int))

  ;; void gtk_widget_set_has_tooltip (GtkWidget* widget, gboolean has_tooltip)
  (define-function void gtk_widget_set_has_tooltip (void* int))

  ;; void gtk_widget_set_name (GtkWidget* widget, const gchar* name)
  (define-function void gtk_widget_set_name (void* char*))

  ;; void gtk_widget_set_no_show_all (GtkWidget* widget, gboolean no_show_all)
  (define-function void gtk_widget_set_no_show_all (void* int))

  ;; void gtk_widget_set_parent (GtkWidget* widget, GtkWidget* parent)
  (define-function void gtk_widget_set_parent (void* void*))

  ;; void gtk_widget_set_parent_window (GtkWidget* widget, GdkWindow* parent_window)
  (define-function void gtk_widget_set_parent_window (void* void*))

  ;; void gtk_widget_set_redraw_on_allocate (GtkWidget* widget, gboolean redraw_on_allocate)
  (define-function void gtk_widget_set_redraw_on_allocate (void* int))

  ;; gboolean gtk_widget_set_scroll_adjustments (GtkWidget* widget, GtkAdjustment* hadjustment, GtkAdjustment* vadjustment)
  (define-function int gtk_widget_set_scroll_adjustments (void* void* void*))

  ;; void gtk_widget_set_sensitive (GtkWidget* widget, gboolean sensitive)
  (define-function void gtk_widget_set_sensitive (void* int))

  ;; void gtk_widget_set_size_request (GtkWidget* widget, gint width, gint height)
  (define-function void gtk_widget_set_size_request (void* int int))

  ;; void gtk_widget_set_state (GtkWidget* widget, GtkStateType state)
  (define-function void gtk_widget_set_state (void* int))

  ;; void gtk_widget_set_style (GtkWidget* widget, GtkStyle* style)
  (define-function void gtk_widget_set_style (void* void*))

  ;; void gtk_widget_set_tooltip_markup (GtkWidget* widget, const gchar* markup)
  (define-function void gtk_widget_set_tooltip_markup (void* char*))

  ;; void gtk_widget_set_tooltip_text (GtkWidget* widget, const gchar* text)
  (define-function void gtk_widget_set_tooltip_text (void* char*))

  ;; void gtk_widget_set_tooltip_window (GtkWidget* widget, GtkWindow* custom_window)
  (define-function void gtk_widget_set_tooltip_window (void* void*))

  ;; void gtk_widget_shape_combine_mask (GtkWidget* widget, GdkBitmap* shape_mask, gint offset_x, gint offset_y)
  (define-function void gtk_widget_shape_combine_mask (void* void* int int))

  ;; void gtk_widget_show (GtkWidget* widget)
  (define-function void gtk_widget_show (void*))

  ;; void gtk_widget_show_all (GtkWidget* widget)
  (define-function void gtk_widget_show_all (void*))

  ;; void gtk_widget_show_now (GtkWidget* widget)
  (define-function void gtk_widget_show_now (void*))

  ;; void gtk_widget_size_allocate (GtkWidget* widget, GtkAllocation* allocation)
  (define-function void gtk_widget_size_allocate (void* void*))

  ;; void gtk_widget_size_request (GtkWidget* widget, GtkRequisition* requisition)
  (define-function void gtk_widget_size_request (void* void*))

  ;; void gtk_widget_style_get (GtkWidget* widget, const gchar* first_property_name, ...)
  (define-function void gtk_widget_style_get (void* char* ...))

  ;; void gtk_widget_style_get_property (GtkWidget* widget, const gchar* property_name, GValue* value)
  (define-function void gtk_widget_style_get_property (void* char* void*))

  ;; void gtk_widget_style_get_valist (GtkWidget* widget, const gchar* first_property_name, va_list var_args)
  (define-function/va_list void gtk_widget_style_get_valist (void* char* va_list))

  ;; void gtk_widget_thaw_child_notify (GtkWidget* widget)
  (define-function void gtk_widget_thaw_child_notify (void*))

  ;; gboolean gtk_widget_translate_coordinates (GtkWidget* src_widget, GtkWidget* dest_widget, gint src_x, gint src_y, gint* dest_x, gint* dest_y)
  (define-function int gtk_widget_translate_coordinates (void* void* int int void* void*))

  ;; void gtk_widget_trigger_tooltip_query (GtkWidget* widget)
  (define-function void gtk_widget_trigger_tooltip_query (void*))

  ;; void gtk_widget_unmap (GtkWidget* widget)
  (define-function void gtk_widget_unmap (void*))

  ;; void gtk_widget_unparent (GtkWidget* widget)
  (define-function void gtk_widget_unparent (void*))

  ;; void gtk_widget_unrealize (GtkWidget* widget)
  (define-function void gtk_widget_unrealize (void*))

  ) ;[end]
