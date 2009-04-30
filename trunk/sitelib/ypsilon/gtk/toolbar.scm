#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk toolbar)

  (export gtk_toolbar_child_type_get_type
          gtk_toolbar_get_drop_index
          gtk_toolbar_get_icon_size
          gtk_toolbar_get_item_index
          gtk_toolbar_get_n_items
          gtk_toolbar_get_nth_item
          gtk_toolbar_get_relief_style
          gtk_toolbar_get_show_arrow
          gtk_toolbar_get_style
          gtk_toolbar_get_type
          gtk_toolbar_insert
          gtk_toolbar_new
          gtk_toolbar_set_drop_highlight_item
          gtk_toolbar_set_icon_size
          gtk_toolbar_set_show_arrow
          gtk_toolbar_set_style
          gtk_toolbar_space_style_get_type
          gtk_toolbar_style_get_type
          gtk_toolbar_unset_icon_size
          gtk_toolbar_unset_style)

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

  ;; GType gtk_toolbar_child_type_get_type (void)
  (define-function unsigned-long gtk_toolbar_child_type_get_type ())

  ;; gint gtk_toolbar_get_drop_index (GtkToolbar* toolbar, gint x, gint y)
  (define-function int gtk_toolbar_get_drop_index (void* int int))

  ;; GtkIconSize gtk_toolbar_get_icon_size (GtkToolbar* toolbar)
  (define-function int gtk_toolbar_get_icon_size (void*))

  ;; gint gtk_toolbar_get_item_index (GtkToolbar* toolbar, GtkToolItem* item)
  (define-function int gtk_toolbar_get_item_index (void* void*))

  ;; gint gtk_toolbar_get_n_items (GtkToolbar* toolbar)
  (define-function int gtk_toolbar_get_n_items (void*))

  ;; GtkToolItem* gtk_toolbar_get_nth_item (GtkToolbar* toolbar, gint n)
  (define-function void* gtk_toolbar_get_nth_item (void* int))

  ;; GtkReliefStyle gtk_toolbar_get_relief_style (GtkToolbar* toolbar)
  (define-function int gtk_toolbar_get_relief_style (void*))

  ;; gboolean gtk_toolbar_get_show_arrow (GtkToolbar* toolbar)
  (define-function int gtk_toolbar_get_show_arrow (void*))

  ;; GtkToolbarStyle gtk_toolbar_get_style (GtkToolbar* toolbar)
  (define-function int gtk_toolbar_get_style (void*))

  ;; GType gtk_toolbar_get_type (void)
  (define-function unsigned-long gtk_toolbar_get_type ())

  ;; void gtk_toolbar_insert (GtkToolbar* toolbar, GtkToolItem* item, gint pos)
  (define-function void gtk_toolbar_insert (void* void* int))

  ;; GtkWidget* gtk_toolbar_new (void)
  (define-function void* gtk_toolbar_new ())

  ;; void gtk_toolbar_set_drop_highlight_item (GtkToolbar* toolbar, GtkToolItem* tool_item, gint index_)
  (define-function void gtk_toolbar_set_drop_highlight_item (void* void* int))

  ;; void gtk_toolbar_set_icon_size (GtkToolbar* toolbar, GtkIconSize icon_size)
  (define-function void gtk_toolbar_set_icon_size (void* int))

  ;; void gtk_toolbar_set_show_arrow (GtkToolbar* toolbar, gboolean show_arrow)
  (define-function void gtk_toolbar_set_show_arrow (void* int))

  ;; void gtk_toolbar_set_style (GtkToolbar* toolbar, GtkToolbarStyle style)
  (define-function void gtk_toolbar_set_style (void* int))

  ;; GType gtk_toolbar_space_style_get_type (void)
  (define-function unsigned-long gtk_toolbar_space_style_get_type ())

  ;; GType gtk_toolbar_style_get_type (void)
  (define-function unsigned-long gtk_toolbar_style_get_type ())

  ;; void gtk_toolbar_unset_icon_size (GtkToolbar* toolbar)
  (define-function void gtk_toolbar_unset_icon_size (void*))

  ;; void gtk_toolbar_unset_style (GtkToolbar* toolbar)
  (define-function void gtk_toolbar_unset_style (void*))

  ) ;[end]
