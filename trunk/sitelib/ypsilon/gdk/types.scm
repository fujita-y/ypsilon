#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk types)
  (export
   GdkColor
   GdkColormap
   GdkCursor
   GdkGCValues
   GdkGeometry
   GdkKeymapKey
   GdkPoint
   GdkRectangle
   GdkRgbCmap
   GdkSegment
   GdkTrapezoid
   define-c-struct-methods
   make-bytevector-mapping
   c-coerce-void*)

  (import (rnrs) (ypsilon c-types))

  ;; typedef struct {
  ;;   guint32 pixel;
  ;;   guint16 red;
  ;;   guint16 green;
  ;;   guint16 blue;
  ;; } GdkColor;
  ;;
  (define-c-typedef GdkColor
    (struct (uint32_t pixel)
            (uint16_t red)
            (uint16_t green)
            (uint16_t blue)))

  ;; typedef struct {
  ;;   gint      size;
  ;;   GdkColor *colors;
  ;; } GdkColormap;
  ;;
  (define-c-typedef GdkColormap
    (struct (int size)
            (void* colors)))

  ;; typedef struct {
  ;;   GdkCursorType type;
  ;; } GdkCursor;
  ;;
  (define-c-typedef GdkCursor
    (struct (int type)))

  ;; typedef struct {
  ;;   GdkColor foreground;
  ;;   GdkColor background;
  ;;   GdkFont *font;
  ;;   GdkFunction function;
  ;;   GdkFill fill;
  ;;   GdkPixmap *tile;
  ;;   GdkPixmap *stipple;
  ;;   GdkPixmap *clip_mask;
  ;;   GdkSubwindowMode subwindow_mode;
  ;;   gint ts_x_origin;
  ;;   gint ts_y_origin;
  ;;   gint clip_x_origin;
  ;;   gint clip_y_origin;
  ;;   gint graphics_exposures;
  ;;   gint line_width;
  ;;   GdkLineStyle line_style;
  ;;   GdkCapStyle cap_style;
  ;;   GdkJoinStyle join_style;
  ;; } GdkGCValues;
  ;;
  (define-c-typedef GdkGCValues
    (struct (GdkColor foreground)
            (GdkColor background)
            (void* font)
            (int function)
            (int fill)
            (void* tile)
            (void* stipple)
            (void* clip_mask)
            (int subwindow_mode)
            (int ts_x_origin)
            (int ts_y_origin)
            (int clip_x_origin)
            (int clip_y_origin)
            (int graphics_exposures)
            (int line_width)
            (int line_style)
            (int cap_style)
            (int join_style)))

  ;; typedef struct {
  ;;   gint min_width;
  ;;   gint min_height;
  ;;   gint max_width;
  ;;   gint max_height;
  ;;   gint base_width;
  ;;   gint base_height;
  ;;   gint width_inc;
  ;;   gint height_inc;
  ;;   gdouble min_aspect;
  ;;   gdouble max_aspect;
  ;;   GdkGravity win_gravity;
  ;; } GdkGeometry;
  ;;
  (define-c-typedef GdkGeometry
    (struct (int min_width)
            (int min_height)
            (int max_width)
            (int max_height)
            (int base_width)
            (int base_height)
            (int width_inc)
            (int height_inc)
            (double min_aspect)
            (double max_aspect)
            (int win_gravity)))

  ;; typedef struct {
  ;;   guint keycode;
  ;;   gint group;
  ;;   gint level;
  ;; } GdkKeymapKey;
  ;;
  (define-c-typedef GdkKeymapKey
    (struct (unsigned-int keycode)
            (int group)
            (int level)))

  ;; typedef struct {
  ;;   gint x;
  ;;   gint y;
  ;; } GdkPoint;
  ;;
  (define-c-typedef GdkPoint
    (struct (int x)
            (int y)))

  ;; typedef struct {
  ;;   gint x;
  ;;   gint y;
  ;;   gint width;
  ;;   gint height;
  ;; } GdkRectangle;
  ;;
  (define-c-typedef GdkRectangle
    (struct (int x)
            (int y)
            (int width)
            (int height)))

  ;; typedef struct {
  ;;   guint32 colors[256];
  ;;   gint n_colors;
  ;; } GdkRgbCmap;
  ;;
  (define-c-typedef GdkRgbCmap
    (struct (uint32_t colors [256])
            (int n_colors)))

  ;; typedef struct {
  ;;   gint x1;
  ;;   gint y1;
  ;;   gint x2;
  ;;   gint y2;
  ;; } GdkSegment;
  ;;
  (define-c-typedef GdkSegment
    (struct (int x1)
            (int y1)
            (int x2)
            (int y2)))

  ;; typedef struct {
  ;;   double y1, x11, x21, y2, x12, x22;
  ;; } GdkTrapezoid;
  ;;
  (define-c-typedef GdkTrapezoid
    (struct (double y1)
            (double x11)
            (double x21)
            (double y2)
            (double x12)
            (double x22)))

  ;; typedef struct {
  ;;   gchar *title;
  ;;   gint event_mask;
  ;;   gint x, y;
  ;;   gint width;
  ;;   gint height;
  ;;   GdkWindowClass wclass;
  ;;   GdkVisual *visual;
  ;;   GdkColormap *colormap;
  ;;   GdkWindowType window_type;
  ;;   GdkCursor *cursor;
  ;;   gchar *wmclass_name;
  ;;   gchar *wmclass_class;
  ;;   gboolean override_redirect;
  ;;   GdkWindowTypeHint type_hint;
  ;; } GdkWindowAttr;

  (define-c-typedef GdkWindowAttr
    (struct (void* title)
            (int event_mask)
            (int x)
            (int y)
            (int width)
            (int height)
            (int wclass)
            (void* visual)
            (void* colormap)
            (int window_type)
            (void* cursor)
            (void* wmclass_name)
            (void* wmclass_class)
            (int override_redirect)
            (int type_hint)))

  ) ;[end]
