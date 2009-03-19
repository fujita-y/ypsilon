#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk drag)

  (export gtk_drag_begin
          gtk_drag_check_threshold
          gtk_drag_dest_add_image_targets
          gtk_drag_dest_add_text_targets
          gtk_drag_dest_add_uri_targets
          gtk_drag_dest_find_target
          gtk_drag_dest_get_target_list
          gtk_drag_dest_get_track_motion
          gtk_drag_dest_set
          gtk_drag_dest_set_proxy
          gtk_drag_dest_set_target_list
          gtk_drag_dest_set_track_motion
          gtk_drag_dest_unset
          gtk_drag_finish
          gtk_drag_get_data
          gtk_drag_get_source_widget
          gtk_drag_highlight
          gtk_drag_result_get_type
          gtk_drag_set_icon_default
          gtk_drag_set_icon_name
          gtk_drag_set_icon_pixbuf
          gtk_drag_set_icon_pixmap
          gtk_drag_set_icon_stock
          gtk_drag_set_icon_widget
          gtk_drag_source_add_image_targets
          gtk_drag_source_add_text_targets
          gtk_drag_source_add_uri_targets
          gtk_drag_source_get_target_list
          gtk_drag_source_set
          gtk_drag_source_set_icon
          gtk_drag_source_set_icon_name
          gtk_drag_source_set_icon_pixbuf
          gtk_drag_source_set_icon_stock
          gtk_drag_source_set_target_list
          gtk_drag_source_unset
          gtk_drag_unhighlight)

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

  ;; GdkDragContext* gtk_drag_begin (GtkWidget* widget, GtkTargetList* targets, GdkDragAction actions, gint button, GdkEvent* event)
  (define-function void* gtk_drag_begin (void* void* int int void*))

  ;; gboolean gtk_drag_check_threshold (GtkWidget* widget, gint start_x, gint start_y, gint current_x, gint current_y)
  (define-function int gtk_drag_check_threshold (void* int int int int))

  ;; void gtk_drag_dest_add_image_targets (GtkWidget* widget)
  (define-function void gtk_drag_dest_add_image_targets (void*))

  ;; void gtk_drag_dest_add_text_targets (GtkWidget* widget)
  (define-function void gtk_drag_dest_add_text_targets (void*))

  ;; void gtk_drag_dest_add_uri_targets (GtkWidget* widget)
  (define-function void gtk_drag_dest_add_uri_targets (void*))

  ;; GdkAtom gtk_drag_dest_find_target (GtkWidget* widget, GdkDragContext* context, GtkTargetList* target_list)
  (define-function void* gtk_drag_dest_find_target (void* void* void*))

  ;; GtkTargetList* gtk_drag_dest_get_target_list (GtkWidget* widget)
  (define-function void* gtk_drag_dest_get_target_list (void*))

  ;; gboolean gtk_drag_dest_get_track_motion (GtkWidget* widget)
  (define-function int gtk_drag_dest_get_track_motion (void*))

  ;; void gtk_drag_dest_set (GtkWidget* widget, GtkDestDefaults flags, const GtkTargetEntry* targets, gint n_targets, GdkDragAction actions)
  (define-function void gtk_drag_dest_set (void* int void* int int))

  ;; void gtk_drag_dest_set_proxy (GtkWidget* widget, GdkWindow* proxy_window, GdkDragProtocol protocol, gboolean use_coordinates)
  (define-function void gtk_drag_dest_set_proxy (void* void* int int))

  ;; void gtk_drag_dest_set_target_list (GtkWidget* widget, GtkTargetList* target_list)
  (define-function void gtk_drag_dest_set_target_list (void* void*))

  ;; void gtk_drag_dest_set_track_motion (GtkWidget* widget, gboolean track_motion)
  (define-function void gtk_drag_dest_set_track_motion (void* int))

  ;; void gtk_drag_dest_unset (GtkWidget* widget)
  (define-function void gtk_drag_dest_unset (void*))

  ;; void gtk_drag_finish (GdkDragContext* context, gboolean success, gboolean del, guint32 time_)
  (define-function void gtk_drag_finish (void* int int uint32_t))

  ;; void gtk_drag_get_data (GtkWidget* widget, GdkDragContext* context, GdkAtom target, guint32 time_)
  (define-function void gtk_drag_get_data (void* void* void* uint32_t))

  ;; GtkWidget* gtk_drag_get_source_widget (GdkDragContext* context)
  (define-function void* gtk_drag_get_source_widget (void*))

  ;; void gtk_drag_highlight (GtkWidget* widget)
  (define-function void gtk_drag_highlight (void*))

  ;; GType gtk_drag_result_get_type (void)
  (define-function unsigned-long gtk_drag_result_get_type ())

  ;; void gtk_drag_set_icon_default (GdkDragContext* context)
  (define-function void gtk_drag_set_icon_default (void*))

  ;; void gtk_drag_set_icon_name (GdkDragContext* context, const gchar* icon_name, gint hot_x, gint hot_y)
  (define-function void gtk_drag_set_icon_name (void* char* int int))

  ;; void gtk_drag_set_icon_pixbuf (GdkDragContext* context, GdkPixbuf* pixbuf, gint hot_x, gint hot_y)
  (define-function void gtk_drag_set_icon_pixbuf (void* void* int int))

  ;; void gtk_drag_set_icon_pixmap (GdkDragContext* context, GdkColormap* colormap, GdkPixmap* pixmap, GdkBitmap* mask, gint hot_x, gint hot_y)
  (define-function void gtk_drag_set_icon_pixmap (void* void* void* void* int int))

  ;; void gtk_drag_set_icon_stock (GdkDragContext* context, const gchar* stock_id, gint hot_x, gint hot_y)
  (define-function void gtk_drag_set_icon_stock (void* char* int int))

  ;; void gtk_drag_set_icon_widget (GdkDragContext* context, GtkWidget* widget, gint hot_x, gint hot_y)
  (define-function void gtk_drag_set_icon_widget (void* void* int int))

  ;; void gtk_drag_source_add_image_targets (GtkWidget* widget)
  (define-function void gtk_drag_source_add_image_targets (void*))

  ;; void gtk_drag_source_add_text_targets (GtkWidget* widget)
  (define-function void gtk_drag_source_add_text_targets (void*))

  ;; void gtk_drag_source_add_uri_targets (GtkWidget* widget)
  (define-function void gtk_drag_source_add_uri_targets (void*))

  ;; GtkTargetList* gtk_drag_source_get_target_list (GtkWidget* widget)
  (define-function void* gtk_drag_source_get_target_list (void*))

  ;; void gtk_drag_source_set (GtkWidget* widget, GdkModifierType start_button_mask, const GtkTargetEntry* targets, gint n_targets, GdkDragAction actions)
  (define-function void gtk_drag_source_set (void* int void* int int))

  ;; void gtk_drag_source_set_icon (GtkWidget* widget, GdkColormap* colormap, GdkPixmap* pixmap, GdkBitmap* mask)
  (define-function void gtk_drag_source_set_icon (void* void* void* void*))

  ;; void gtk_drag_source_set_icon_name (GtkWidget* widget, const gchar* icon_name)
  (define-function void gtk_drag_source_set_icon_name (void* char*))

  ;; void gtk_drag_source_set_icon_pixbuf (GtkWidget* widget, GdkPixbuf* pixbuf)
  (define-function void gtk_drag_source_set_icon_pixbuf (void* void*))

  ;; void gtk_drag_source_set_icon_stock (GtkWidget* widget, const gchar* stock_id)
  (define-function void gtk_drag_source_set_icon_stock (void* char*))

  ;; void gtk_drag_source_set_target_list (GtkWidget* widget, GtkTargetList* target_list)
  (define-function void gtk_drag_source_set_target_list (void* void*))

  ;; void gtk_drag_source_unset (GtkWidget* widget)
  (define-function void gtk_drag_source_unset (void*))

  ;; void gtk_drag_unhighlight (GtkWidget* widget)
  (define-function void gtk_drag_unhighlight (void*))

  ) ;[end]
