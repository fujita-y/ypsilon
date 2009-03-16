#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk tooltip)

  (export gtk_tooltip_get_type
          gtk_tooltip_set_custom
          gtk_tooltip_set_icon
          gtk_tooltip_set_icon_from_icon_name
          gtk_tooltip_set_icon_from_stock
          gtk_tooltip_set_markup
          gtk_tooltip_set_text
          gtk_tooltip_set_tip_area
          gtk_tooltip_trigger_tooltip_query)

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

  ;; GType gtk_tooltip_get_type (void)
  (define-function unsigned-long gtk_tooltip_get_type ())

  ;; void gtk_tooltip_set_custom (GtkTooltip* tooltip, GtkWidget* custom_widget)
  (define-function void gtk_tooltip_set_custom (void* void*))

  ;; void gtk_tooltip_set_icon (GtkTooltip* tooltip, GdkPixbuf* pixbuf)
  (define-function void gtk_tooltip_set_icon (void* void*))

  ;; void gtk_tooltip_set_icon_from_icon_name (GtkTooltip* tooltip, const gchar* icon_name, GtkIconSize size)
  (define-function void gtk_tooltip_set_icon_from_icon_name (void* char* int))

  ;; void gtk_tooltip_set_icon_from_stock (GtkTooltip* tooltip, const gchar* stock_id, GtkIconSize size)
  (define-function void gtk_tooltip_set_icon_from_stock (void* char* int))

  ;; void gtk_tooltip_set_markup (GtkTooltip* tooltip, const gchar* markup)
  (define-function void gtk_tooltip_set_markup (void* char*))

  ;; void gtk_tooltip_set_text (GtkTooltip* tooltip, const gchar* text)
  (define-function void gtk_tooltip_set_text (void* char*))

  ;; void gtk_tooltip_set_tip_area (GtkTooltip* tooltip, const GdkRectangle* rect)
  (define-function void gtk_tooltip_set_tip_area (void* void*))

  ;; void gtk_tooltip_trigger_tooltip_query (GdkDisplay* display)
  (define-function void gtk_tooltip_trigger_tooltip_query (void*))

  ) ;[end]
