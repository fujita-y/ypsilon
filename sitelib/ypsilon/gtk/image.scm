#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk image)

  (export gtk_image_clear
          gtk_image_get_animation
          gtk_image_get_gicon
          gtk_image_get_icon_name
          gtk_image_get_icon_set
          gtk_image_get_pixbuf
          gtk_image_get_pixel_size
          gtk_image_get_pixmap
          gtk_image_get_stock
          gtk_image_get_storage_type
          gtk_image_get_type
          gtk_image_menu_item_get_image
          gtk_image_menu_item_get_type
          gtk_image_menu_item_new
          gtk_image_menu_item_new_from_stock
          gtk_image_menu_item_new_with_label
          gtk_image_menu_item_new_with_mnemonic
          gtk_image_menu_item_set_image
          gtk_image_new
          gtk_image_new_from_animation
          gtk_image_new_from_file
          gtk_image_new_from_gicon
          gtk_image_new_from_icon_name
          gtk_image_new_from_icon_set
          gtk_image_new_from_image
          gtk_image_new_from_pixbuf
          gtk_image_new_from_pixmap
          gtk_image_new_from_stock
          gtk_image_set_from_animation
          gtk_image_set_from_file
          gtk_image_set_from_gicon
          gtk_image_set_from_icon_name
          gtk_image_set_from_icon_set
          gtk_image_set_from_pixbuf
          gtk_image_set_from_pixmap
          gtk_image_set_from_stock
          gtk_image_set_pixel_size
          gtk_image_type_get_type)

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

  ;; void gtk_image_clear (GtkImage* image)
  (define-function void gtk_image_clear (void*))

  ;; GdkPixbufAnimation* gtk_image_get_animation (GtkImage* image)
  (define-function void* gtk_image_get_animation (void*))

  ;; void gtk_image_get_gicon (GtkImage* image, GIcon** gicon, GtkIconSize* size)
  (define-function void gtk_image_get_gicon (void* void* void*))

  ;; void gtk_image_get_icon_name (GtkImage* image, const gchar** icon_name, GtkIconSize* size)
  (define-function void gtk_image_get_icon_name (void* void* void*))

  ;; void gtk_image_get_icon_set (GtkImage* image, GtkIconSet** icon_set, GtkIconSize* size)
  (define-function void gtk_image_get_icon_set (void* void* void*))

  ;; GdkPixbuf* gtk_image_get_pixbuf (GtkImage* image)
  (define-function void* gtk_image_get_pixbuf (void*))

  ;; gint gtk_image_get_pixel_size (GtkImage* image)
  (define-function int gtk_image_get_pixel_size (void*))

  ;; void gtk_image_get_pixmap (GtkImage* image, GdkPixmap** pixmap, GdkBitmap** mask)
  (define-function void gtk_image_get_pixmap (void* void* void*))

  ;; void gtk_image_get_stock (GtkImage* image, gchar** stock_id, GtkIconSize* size)
  (define-function void gtk_image_get_stock (void* void* void*))

  ;; GtkImageType gtk_image_get_storage_type (GtkImage* image)
  (define-function int gtk_image_get_storage_type (void*))

  ;; GType gtk_image_get_type (void)
  (define-function unsigned-long gtk_image_get_type ())

  ;; GtkWidget* gtk_image_menu_item_get_image (GtkImageMenuItem* image_menu_item)
  (define-function void* gtk_image_menu_item_get_image (void*))

  ;; GType gtk_image_menu_item_get_type (void)
  (define-function unsigned-long gtk_image_menu_item_get_type ())

  ;; GtkWidget* gtk_image_menu_item_new (void)
  (define-function void* gtk_image_menu_item_new ())

  ;; GtkWidget* gtk_image_menu_item_new_from_stock (const gchar* stock_id, GtkAccelGroup* accel_group)
  (define-function void* gtk_image_menu_item_new_from_stock (char* void*))

  ;; GtkWidget* gtk_image_menu_item_new_with_label (const gchar* label)
  (define-function void* gtk_image_menu_item_new_with_label (char*))

  ;; GtkWidget* gtk_image_menu_item_new_with_mnemonic (const gchar* label)
  (define-function void* gtk_image_menu_item_new_with_mnemonic (char*))

  ;; void gtk_image_menu_item_set_image (GtkImageMenuItem* image_menu_item, GtkWidget* image)
  (define-function void gtk_image_menu_item_set_image (void* void*))

  ;; GtkWidget* gtk_image_new (void)
  (define-function void* gtk_image_new ())

  ;; GtkWidget* gtk_image_new_from_animation (GdkPixbufAnimation* animation)
  (define-function void* gtk_image_new_from_animation (void*))

  ;; GtkWidget* gtk_image_new_from_file (const gchar* filename)
  (define-function void* gtk_image_new_from_file (char*))

  ;; GtkWidget* gtk_image_new_from_gicon (GIcon* icon, GtkIconSize size)
  (define-function void* gtk_image_new_from_gicon (void* int))

  ;; GtkWidget* gtk_image_new_from_icon_name (const gchar* icon_name, GtkIconSize size)
  (define-function void* gtk_image_new_from_icon_name (char* int))

  ;; GtkWidget* gtk_image_new_from_icon_set (GtkIconSet* icon_set, GtkIconSize size)
  (define-function void* gtk_image_new_from_icon_set (void* int))

  ;; GtkWidget* gtk_image_new_from_image (GdkImage* image, GdkBitmap* mask)
  (define-function void* gtk_image_new_from_image (void* void*))

  ;; GtkWidget* gtk_image_new_from_pixbuf (GdkPixbuf* pixbuf)
  (define-function void* gtk_image_new_from_pixbuf (void*))

  ;; GtkWidget* gtk_image_new_from_pixmap (GdkPixmap* pixmap, GdkBitmap* mask)
  (define-function void* gtk_image_new_from_pixmap (void* void*))

  ;; GtkWidget* gtk_image_new_from_stock (const gchar* stock_id, GtkIconSize size)
  (define-function void* gtk_image_new_from_stock (char* int))

  ;; void gtk_image_set_from_animation (GtkImage* image, GdkPixbufAnimation* animation)
  (define-function void gtk_image_set_from_animation (void* void*))

  ;; void gtk_image_set_from_file (GtkImage* image, const gchar* filename)
  (define-function void gtk_image_set_from_file (void* char*))

  ;; void gtk_image_set_from_gicon (GtkImage* image, GIcon* icon, GtkIconSize size)
  (define-function void gtk_image_set_from_gicon (void* void* int))

  ;; void gtk_image_set_from_icon_name (GtkImage* image, const gchar* icon_name, GtkIconSize size)
  (define-function void gtk_image_set_from_icon_name (void* char* int))

  ;; void gtk_image_set_from_icon_set (GtkImage* image, GtkIconSet* icon_set, GtkIconSize size)
  (define-function void gtk_image_set_from_icon_set (void* void* int))

  ;; void gtk_image_set_from_pixbuf (GtkImage* image, GdkPixbuf* pixbuf)
  (define-function void gtk_image_set_from_pixbuf (void* void*))

  ;; void gtk_image_set_from_pixmap (GtkImage* image, GdkPixmap* pixmap, GdkBitmap* mask)
  (define-function void gtk_image_set_from_pixmap (void* void* void*))

  ;; void gtk_image_set_from_stock (GtkImage* image, const gchar* stock_id, GtkIconSize size)
  (define-function void gtk_image_set_from_stock (void* char* int))

  ;; void gtk_image_set_pixel_size (GtkImage* image, gint pixel_size)
  (define-function void gtk_image_set_pixel_size (void* int))

  ;; GType gtk_image_type_get_type (void)
  (define-function unsigned-long gtk_image_type_get_type ())

  ) ;[end]
