#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango types)

  (export PangoColor
          PangoMatrix
          PangoRectangle
          define-c-struct-methods
          make-bytevector-mapping
          c-coerce-void*)

  (import (rnrs) (ypsilon c-types))

  ;; typedef struct {
  ;;   guint16 red;
  ;;   guint16 green;
  ;;   guint16 blue;
  ;; } PangoColor;
  ;;
  (define-c-typedef PangoColor
    (struct (uint16_t red)
            (uint16_t green)
            (uint16_t blue)))

  ;; typedef struct {
  ;;   double xx;
  ;;   double xy;
  ;;   double yx;
  ;;   double yy;
  ;;   double x0;
  ;;   double y0;
  ;; } PangoMatrix;
  ;;
  (define-c-typedef PangoMatrix
    (struct (double xx)
            (double xy)
            (double yx)
            (double yy)
            (double x0)
            (double y0)))

  ;; typedef struct {
  ;;   int x;
  ;;   int y;
  ;;   int width;
  ;;   int height;
  ;; } PangoRectangle;
  ;;
  (define-c-typedef PangoRectangle
    (struct (int x)
            (int y)
            (int width)
            (int height)))


  ) ;[end]
