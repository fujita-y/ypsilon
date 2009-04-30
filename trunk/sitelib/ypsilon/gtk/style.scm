#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk style)

  (export gtk_style_apply_default_background
          gtk_style_attach
          gtk_style_copy
          gtk_style_detach
          gtk_style_get
          gtk_style_get_style_property
          gtk_style_get_type
          gtk_style_get_valist
          gtk_style_lookup_color
          gtk_style_lookup_icon_set
          gtk_style_new
          gtk_style_render_icon
          gtk_style_set_background)

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

  ;; void gtk_style_apply_default_background (GtkStyle* style, GdkWindow* window, gboolean set_bg, GtkStateType state_type, const GdkRectangle* area, gint x, gint y, gint width, gint height)
  (define-function void gtk_style_apply_default_background (void* void* int int void* int int int int))

  ;; GtkStyle* gtk_style_attach (GtkStyle* style, GdkWindow* window)
  (define-function void* gtk_style_attach (void* void*))

  ;; GtkStyle* gtk_style_copy (GtkStyle* style)
  (define-function void* gtk_style_copy (void*))

  ;; void gtk_style_detach (GtkStyle* style)
  (define-function void gtk_style_detach (void*))

  ;; void gtk_style_get (GtkStyle* style, GType widget_type, const gchar* first_property_name, ...)
  (define-function void gtk_style_get (void* unsigned-long char* ...))

  ;; void gtk_style_get_style_property (GtkStyle* style, GType widget_type, const gchar* property_name, GValue* value)
  (define-function void gtk_style_get_style_property (void* unsigned-long char* void*))

  ;; GType gtk_style_get_type (void)
  (define-function unsigned-long gtk_style_get_type ())

  ;; void gtk_style_get_valist (GtkStyle* style, GType widget_type, const gchar* first_property_name, va_list var_args)
  (define-function/va_list void gtk_style_get_valist (void* unsigned-long char* va_list))

  ;; gboolean gtk_style_lookup_color (GtkStyle* style, const gchar* color_name, GdkColor* color)
  (define-function int gtk_style_lookup_color (void* char* void*))

  ;; GtkIconSet* gtk_style_lookup_icon_set (GtkStyle* style, const gchar* stock_id)
  (define-function void* gtk_style_lookup_icon_set (void* char*))

  ;; GtkStyle* gtk_style_new (void)
  (define-function void* gtk_style_new ())

  ;; GdkPixbuf* gtk_style_render_icon (GtkStyle* style, const GtkIconSource* source, GtkTextDirection direction, GtkStateType state, GtkIconSize size, GtkWidget* widget, const gchar* detail)
  (define-function void* gtk_style_render_icon (void* void* int int int void* char*))

  ;; void gtk_style_set_background (GtkStyle* style, GdkWindow* window, GtkStateType state_type)
  (define-function void gtk_style_set_background (void* void* int))

  ) ;[end]
