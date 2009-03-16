#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk types)
  (export GdkPoint
          GdkRectangle
          GdkColor
          GdkGeometry
          define-c-struct-methods
          make-bytevector-mapping
          c-coerce-void*)

  (import (rnrs) (ypsilon ffi))

  ;; typedef struct {
  ;;  gint x;
  ;;  gint y;
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

  ) ;[end]
