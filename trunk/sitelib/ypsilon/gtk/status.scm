#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk status)

  (export gtk_status_icon_get_blinking
          gtk_status_icon_get_geometry
          gtk_status_icon_get_gicon
          gtk_status_icon_get_icon_name
          gtk_status_icon_get_pixbuf
          gtk_status_icon_get_screen
          gtk_status_icon_get_size
          gtk_status_icon_get_stock
          gtk_status_icon_get_storage_type
          gtk_status_icon_get_type
          gtk_status_icon_get_visible
          gtk_status_icon_get_x11_window_id
          gtk_status_icon_is_embedded
          gtk_status_icon_new
          gtk_status_icon_new_from_file
          gtk_status_icon_new_from_gicon
          gtk_status_icon_new_from_icon_name
          gtk_status_icon_new_from_pixbuf
          gtk_status_icon_new_from_stock
          gtk_status_icon_position_menu
          gtk_status_icon_set_blinking
          gtk_status_icon_set_from_file
          gtk_status_icon_set_from_gicon
          gtk_status_icon_set_from_icon_name
          gtk_status_icon_set_from_pixbuf
          gtk_status_icon_set_from_stock
          gtk_status_icon_set_screen
          gtk_status_icon_set_tooltip
          gtk_status_icon_set_visible)

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

  ;; gboolean gtk_status_icon_get_blinking (GtkStatusIcon* status_icon)
  (define-function int gtk_status_icon_get_blinking (void*))

  ;; gboolean gtk_status_icon_get_geometry (GtkStatusIcon* status_icon, GdkScreen** screen, GdkRectangle* area, GtkOrientation* orientation)
  (define-function int gtk_status_icon_get_geometry (void* void* void* void*))

  ;; GIcon* gtk_status_icon_get_gicon (GtkStatusIcon* status_icon)
  (define-function void* gtk_status_icon_get_gicon (void*))

  ;; const gchar* gtk_status_icon_get_icon_name (GtkStatusIcon* status_icon)
  (define-function char* gtk_status_icon_get_icon_name (void*))

  ;; GdkPixbuf* gtk_status_icon_get_pixbuf (GtkStatusIcon* status_icon)
  (define-function void* gtk_status_icon_get_pixbuf (void*))

  ;; GdkScreen* gtk_status_icon_get_screen (GtkStatusIcon* status_icon)
  (define-function void* gtk_status_icon_get_screen (void*))

  ;; gint gtk_status_icon_get_size (GtkStatusIcon* status_icon)
  (define-function int gtk_status_icon_get_size (void*))

  ;; const gchar* gtk_status_icon_get_stock (GtkStatusIcon* status_icon)
  (define-function char* gtk_status_icon_get_stock (void*))

  ;; GtkImageType gtk_status_icon_get_storage_type (GtkStatusIcon* status_icon)
  (define-function int gtk_status_icon_get_storage_type (void*))

  ;; GType gtk_status_icon_get_type (void)
  (define-function unsigned-long gtk_status_icon_get_type ())

  ;; gboolean gtk_status_icon_get_visible (GtkStatusIcon* status_icon)
  (define-function int gtk_status_icon_get_visible (void*))

  ;; guint32 gtk_status_icon_get_x11_window_id (GtkStatusIcon* status_icon)
  (define-function uint32_t gtk_status_icon_get_x11_window_id (void*))

  ;; gboolean gtk_status_icon_is_embedded (GtkStatusIcon* status_icon)
  (define-function int gtk_status_icon_is_embedded (void*))

  ;; GtkStatusIcon* gtk_status_icon_new (void)
  (define-function void* gtk_status_icon_new ())

  ;; GtkStatusIcon* gtk_status_icon_new_from_file (const gchar* filename)
  (define-function void* gtk_status_icon_new_from_file (char*))

  ;; GtkStatusIcon* gtk_status_icon_new_from_gicon (GIcon* icon)
  (define-function void* gtk_status_icon_new_from_gicon (void*))

  ;; GtkStatusIcon* gtk_status_icon_new_from_icon_name (const gchar* icon_name)
  (define-function void* gtk_status_icon_new_from_icon_name (char*))

  ;; GtkStatusIcon* gtk_status_icon_new_from_pixbuf (GdkPixbuf* pixbuf)
  (define-function void* gtk_status_icon_new_from_pixbuf (void*))

  ;; GtkStatusIcon* gtk_status_icon_new_from_stock (const gchar* stock_id)
  (define-function void* gtk_status_icon_new_from_stock (char*))

  ;; void gtk_status_icon_position_menu (GtkMenu* menu, gint* x, gint* y, gboolean* push_in, gpointer user_data)
  (define-function void gtk_status_icon_position_menu (void* void* void* void* void*))

  ;; void gtk_status_icon_set_blinking (GtkStatusIcon* status_icon, gboolean blinking)
  (define-function void gtk_status_icon_set_blinking (void* int))

  ;; void gtk_status_icon_set_from_file (GtkStatusIcon* status_icon, const gchar* filename)
  (define-function void gtk_status_icon_set_from_file (void* char*))

  ;; void gtk_status_icon_set_from_gicon (GtkStatusIcon* status_icon, GIcon* icon)
  (define-function void gtk_status_icon_set_from_gicon (void* void*))

  ;; void gtk_status_icon_set_from_icon_name (GtkStatusIcon* status_icon, const gchar* icon_name)
  (define-function void gtk_status_icon_set_from_icon_name (void* char*))

  ;; void gtk_status_icon_set_from_pixbuf (GtkStatusIcon* status_icon, GdkPixbuf* pixbuf)
  (define-function void gtk_status_icon_set_from_pixbuf (void* void*))

  ;; void gtk_status_icon_set_from_stock (GtkStatusIcon* status_icon, const gchar* stock_id)
  (define-function void gtk_status_icon_set_from_stock (void* char*))

  ;; void gtk_status_icon_set_screen (GtkStatusIcon* status_icon, GdkScreen* screen)
  (define-function void gtk_status_icon_set_screen (void* void*))

  ;; void gtk_status_icon_set_tooltip (GtkStatusIcon* status_icon, const gchar* tooltip_text)
  (define-function void gtk_status_icon_set_tooltip (void* char*))

  ;; void gtk_status_icon_set_visible (GtkStatusIcon* status_icon, gboolean visible)
  (define-function void gtk_status_icon_set_visible (void* int))

  ) ;[end]
