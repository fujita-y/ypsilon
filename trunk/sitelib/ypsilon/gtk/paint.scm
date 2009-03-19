#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk paint)

  (export gtk_paint_arrow
          gtk_paint_box
          gtk_paint_box_gap
          gtk_paint_check
          gtk_paint_diamond
          gtk_paint_expander
          gtk_paint_extension
          gtk_paint_flat_box
          gtk_paint_focus
          gtk_paint_handle
          gtk_paint_hline
          gtk_paint_layout
          gtk_paint_option
          gtk_paint_polygon
          gtk_paint_resize_grip
          gtk_paint_shadow
          gtk_paint_shadow_gap
          gtk_paint_slider
          gtk_paint_tab
          gtk_paint_vline)

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

  ;; void gtk_paint_arrow (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, GtkArrowType arrow_type, gboolean fill, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_arrow (void* void* int int void* void* char* int int int int int int))

  ;; void gtk_paint_box (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_box (void* void* int int void* void* char* int int int int))

  ;; void gtk_paint_box_gap (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height, GtkPositionType gap_side, gint gap_x, gint gap_width)
  (define-function void gtk_paint_box_gap (void* void* int int void* void* char* int int int int int int int))

  ;; void gtk_paint_check (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_check (void* void* int int void* void* char* int int int int))

  ;; void gtk_paint_diamond (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_diamond (void* void* int int void* void* char* int int int int))

  ;; void gtk_paint_expander (GtkStyle* style, GdkWindow* window, GtkStateType state_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, GtkExpanderStyle expander_style)
  (define-function void gtk_paint_expander (void* void* int void* void* char* int int int))

  ;; void gtk_paint_extension (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height, GtkPositionType gap_side)
  (define-function void gtk_paint_extension (void* void* int int void* void* char* int int int int int))

  ;; void gtk_paint_flat_box (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_flat_box (void* void* int int void* void* char* int int int int))

  ;; void gtk_paint_focus (GtkStyle* style, GdkWindow* window, GtkStateType state_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_focus (void* void* int void* void* char* int int int int))

  ;; void gtk_paint_handle (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height, GtkOrientation orientation)
  (define-function void gtk_paint_handle (void* void* int int void* void* char* int int int int int))

  ;; void gtk_paint_hline (GtkStyle* style, GdkWindow* window, GtkStateType state_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x1, gint x2, gint y)
  (define-function void gtk_paint_hline (void* void* int void* void* char* int int int))

  ;; void gtk_paint_layout (GtkStyle* style, GdkWindow* window, GtkStateType state_type, gboolean use_text, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, PangoLayout* layout)
  (define-function void gtk_paint_layout (void* void* int int void* void* char* int int void*))

  ;; void gtk_paint_option (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_option (void* void* int int void* void* char* int int int int))

  ;; void gtk_paint_polygon (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, const GdkPoint* points, gint n_points, gboolean fill)
  (define-function void gtk_paint_polygon (void* void* int int void* void* char* void* int int))

  ;; void gtk_paint_resize_grip (GtkStyle* style, GdkWindow* window, GtkStateType state_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, GdkWindowEdge edge, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_resize_grip (void* void* int void* void* char* int int int int int))

  ;; void gtk_paint_shadow (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_shadow (void* void* int int void* void* char* int int int int))

  ;; void gtk_paint_shadow_gap (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height, GtkPositionType gap_side, gint gap_x, gint gap_width)
  (define-function void gtk_paint_shadow_gap (void* void* int int void* void* char* int int int int int int int))

  ;; void gtk_paint_slider (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height, GtkOrientation orientation)
  (define-function void gtk_paint_slider (void* void* int int void* void* char* int int int int int))

  ;; void gtk_paint_tab (GtkStyle* style, GdkWindow* window, GtkStateType state_type, GtkShadowType shadow_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint x, gint y, gint width, gint height)
  (define-function void gtk_paint_tab (void* void* int int void* void* char* int int int int))

  ;; void gtk_paint_vline (GtkStyle* style, GdkWindow* window, GtkStateType state_type, const GdkRectangle* area, GtkWidget* widget, const gchar* detail, gint y1_, gint y2_, gint x)
  (define-function void gtk_paint_vline (void* void* int void* void* char* int int int))

  ) ;[end]
