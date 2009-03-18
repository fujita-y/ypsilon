#!/usr/bin/env ypsilon

;; gtkglext-demo.scm:
;;   GTK, GDK, GLIB, OpenGL, FFI demo program (experimental)
;; requirements:
;;   GTK 2.14 or later.

(import (only (core) auto-compile-verbose scheme-load-verbose))

(auto-compile-verbose #t)
(scheme-load-verbose #t)

(import (rnrs)
        (rnrs programs)
        (ypsilon c-types)
        (ypsilon gobject signal)
        (ypsilon glib timeout)
        (ypsilon gdk types)
        (ypsilon gdk constants)
        (ypsilon gdk window)
        (ypsilon gdk drawable)
        (ypsilon gtk constants)
        (ypsilon gtk init)
        (ypsilon gtk main)
        (ypsilon gtk window)
        (ypsilon gtk widget)
        (ypsilon gtk container)
        (ypsilon gtk drawing)
        (ypsilon gtkglext)
        (ypsilon gl))

(define-c-struct-methods GdkGeometry)

(define current-angle 0.0)
(define terminating #f)

(define TRUE 1)
(define FALSE 0)
(define NULL 0)

(define-syntax make-gint (syntax-rules () ((_) (make-bytevector (c-sizeof int)))))
(define-syntax gint-ref (syntax-rules () ((_ obj) (bytevector-c-int-ref obj 0))))

(define f32vector
  (lambda lst
    (let ((bv (make-bytevector (* (length lst) 4))))
      (let loop ((i 0) (lst lst))
        (cond ((null? lst) bv)
              (else
               (bytevector-c-float-set! bv (* i 4) (car lst))
               (loop (+ i 1) (cdr lst))))))))

(define render-one
  (lambda (x y ambr ambg ambb difr difg difb specr specg specb shine)
    (glPushMatrix)
    (glTranslatef x y 0.0)
    (glRotatef current-angle -0.3 1.0 -0.5)
    (glMaterialfv GL_FRONT GL_AMBIENT (f32vector ambr ambg ambb 1.0))
    (glMaterialfv GL_FRONT GL_DIFFUSE (f32vector difr difg difb 1.0))
    (glMaterialfv GL_FRONT GL_SPECULAR (f32vector specr specg specb 1.0))
    (glMaterialf GL_FRONT GL_SHININESS (* shine 128.0))
    (gdk_gl_draw_icosahedron TRUE)
    (glPopMatrix)))

(define rendering
  (lambda ()
    (do ((y 2.0 (+ y 3.0)))
      ((> y 14.0))
      (do ((x 2.0 (+ x 3.0)))
        ((> x 14.0))
        (render-one x y
                    0.4 (/ x 40.0) (/ y 40.0)
                    (/ x 20.0) (/ y 20.0) 0.4
                    (/ x 20.0) (/ y 20.0) 0.2
                    (/ (+ x y) 20.0 100.0))))))

(define expose-event
  (signal-callback gboolean (GtkWidget* GdkEvent* gpointer)
    (lambda (drawing-area event data)
      (let ((context (gtk_widget_get_gl_context drawing-area))
            (drawable (gtk_widget_get_gl_window drawing-area)))
        (unless (= (gdk_gl_drawable_gl_begin drawable context) FALSE)
          (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
          (rendering)
          (if (= (gdk_gl_drawable_is_double_buffered drawable) TRUE)
              (gdk_gl_drawable_swap_buffers drawable)
              (glFlush))
          (gdk_gl_drawable_gl_end drawable))))))

(define realize
  (signal-callback gboolean (GtkWidget* gpointer)
    (lambda (drawing-area data)
      (let ((context (gtk_widget_get_gl_context drawing-area))
            (drawable (gtk_widget_get_gl_window drawing-area)))
        (unless (= (gdk_gl_drawable_gl_begin drawable context) FALSE)
          (glLightfv GL_LIGHT0 GL_AMBIENT (f32vector 0.0 0.0 0.0 1.0))
          (glLightfv GL_LIGHT0 GL_DIFFUSE (f32vector 1.0 1.0 1.0 1.0))
          (glLightfv GL_LIGHT0 GL_POSITION (f32vector 0.0 3.0 3.0 0.0))
          (glLightModelfv GL_LIGHT_MODEL_AMBIENT (f32vector 0.2 0.2 0.2 1.0))
          (glLightModelfv GL_LIGHT_MODEL_LOCAL_VIEWER (f32vector 0.0))
          (glShadeModel GL_FLAT)
          (glFrontFace GL_CW)
          (glEnable GL_LIGHTING)
          (glEnable GL_LIGHT0)
          (glEnable GL_AUTO_NORMAL)
          (glEnable GL_NORMALIZE)
          (glEnable GL_DEPTH_TEST)
          (glDepthFunc GL_LESS)
          (gdk_gl_drawable_gl_end drawable))))))

(define get-drawable-size
  (lambda (drawable)
    (let ((w (make-gint)) (h (make-gint)))
      (gdk_drawable_get_size drawable w h)
      (values (gint-ref w) (gint-ref h)))))

(define configure-event
  (signal-callback gboolean (GtkWidget* GdkEvent* gpointer)
    (lambda (drawing-area event data)
      (let ((context (gtk_widget_get_gl_context drawing-area))
            (drawable (gtk_widget_get_gl_window drawing-area)))
        (unless (= (gdk_gl_drawable_gl_begin drawable context) FALSE)
          (let-values (((w h) (get-drawable-size drawable)))
            (when (and (> w 0) (> h 0))
              (glViewport 0 0 w h)
              (glMatrixMode GL_PROJECTION)
              (glLoadIdentity)
              (if (<= w h)
                  (glOrtho 0.0 16.0 0.0 (/ (* 16.0 h) w) -10.0 10.0)
                  (glOrtho 0.0 (/ (* 16.0 w) h) 0.0 16.0 -10.0 10.0))
              (glMatrixMode GL_MODELVIEW)))
          (gdk_gl_drawable_gl_end drawable))))))

(define destroy
  (signal-callback gboolean (GtkObject* gpointer)
    (lambda x
      (set! terminating #t)
      (gtk_main_quit))))

(define timeout
  (lambda (widget)
    (unless terminating
      (let ((new-current-angle (+ current-angle 2.0)))
        (if (>= new-current-angle 360.0)
            (set! current-angle (- new-current-angle 360.0))
            (set! current-angle new-current-angle)))
      (gdk_window_invalidate_rect (gtk_widget_get_window widget) NULL FALSE)
      (gdk_window_process_updates (gtk_widget_get_window widget) FALSE))
    (if terminating FALSE TRUE)))

(define get-gl-config
  (lambda ()
    (let ((double-mode
           (gdk_gl_config_new_by_mode
            (bitwise-ior GDK_GL_MODE_RGB
                         GDK_GL_MODE_DEPTH
                         GDK_GL_MODE_DOUBLE))))
      (cond ((= double-mode NULL)
             (let ((single-mode
                    (gdk_gl_config_new_by_mode
                     (bitwise-ior GDK_GL_MODE_RGB
                                  GDK_GL_MODE_DEPTH))))
               (cond ((= single-mode NULL)
                      (error 'gdk_gl_config_new_by_mode "initialization failed"))
                     (else single-mode))))
            (else double-mode)))))

(define run
  (lambda ()
    (gtk_init (vector (length (command-line))) (apply vector (command-line)))
    (gdk_gl_init (vector (length (command-line))) (apply vector (command-line)))
    (let ((drawing-area (gtk_drawing_area_new)))
      (gtk_widget_set_gl_capability drawing-area (get-gl-config) NULL TRUE GDK_GL_RGBA_TYPE)
      (gtk_widget_set_size_request drawing-area 512 512)
      (g_signal_connect drawing-area "configure-event" configure-event NULL)
      (g_signal_connect drawing-area "expose-event" expose-event NULL)
      (g_signal_connect_after drawing-area "realize" realize NULL)
      (g_timeout_add 16 timeout drawing-area)
      (let ((window (gtk_window_new GTK_WINDOW_TOPLEVEL)))
        (let ((hint (make-GdkGeometry)))
          (GdkGeometry-min_aspect-set! hint 1.0)
          (GdkGeometry-max_aspect-set! hint 1.0)
          (gtk_window_set_geometry_hints window window hint GDK_HINT_ASPECT))
        (gtk_window_set_title window "GtkGLExt - Hello World")
        (g_signal_connect window "destroy" destroy NULL)
        (gtk_container_add window drawing-area)
        (gtk_widget_show drawing-area)
        (gtk_widget_show window)
        (gtk_main)))))

(run)
