#|
    GLUT, OpenGL, FFI, and Concurrent GC Stress Test

    Requirements:
      Darwin: OpenGL.framework GLUT.framework
      Linux: libGL.so.1 libglut.so.3

    Run:
      ./ypsilon --r6rs --top-level-program demo/glut-demo.scm

    Operation:
      Left button change direction
      Right button show menu
|#

(import (rnrs)
        (rnrs programs)
        (ypsilon gl)
        (ypsilon glut)
        (ypsilon time)
        (ypsilon c-ffi)
        (ypsilon c-types)
        (only (core) collect-notify format))

(define report (collect-notify #t))

(define object glutSolidIcosahedron)
(define current-direction #t)
(define current-angle 0.0)
(define last-update 0)

(define display-func
  (lambda ()
    (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (do ((y 2.0 (+ y 3.0)))
      ((> y 14.0))
      (do ((x 2.0 (+ x 3.0)))
        ((> x 14.0))
        (if current-direction
            (render-one x y
                        0.4 (/ x 40.0) (/ y 40.0)
                        (/ x 20.0) (/ y 20.0) 0.4
                        (/ x 20.0) 0.2 (/ y 20.0)
                        (/ (+ x y) 20.0 100.0))
            (render-one x y
                        (/ y 40.0) (/ x 40.0) 0.4
                        (/ x 20.0) 0.4 (/ y 20.0)
                        0.2 (/ x 20.0) (/ y 20.0)
                        (/ (+ x y) 20.0 100.0)))))
    (glutSwapBuffers)))

(define rotate-func
  (lambda ()
    (cond ((< (+ last-update 16000) (microsecond))
           (if (= (glutGetWindow) 0) (exit 0))
           (if current-direction
               (let ((new-current-angle (+ current-angle 2.0)))
                 (if (>= new-current-angle 360.0)
                     (set! current-angle (- new-current-angle 360.0))
                     (set! current-angle new-current-angle)))
               (let ((new-current-angle (- current-angle 2.0)))
                 (if (< new-current-angle 360.0)
                     (set! current-angle (+ new-current-angle 360.0))
                     (set! current-angle new-current-angle))))
           (set! last-update (microsecond))
           (glutPostRedisplay)))))

(define mouse-func
  (lambda (button state x y)
    (and (= state 0) (set! current-direction (not current-direction)))
    (format #t ";; callback: (mouse-func ~s ~s ~s ~s) ~%" button state x y)))

(define show-dodecahedron (lambda () (glScalef 0.6 0.6 0.6) (glutSolidDodecahedron)))
(define show-sphere       (lambda () (glutSolidSphere 1.0 32 16)))
(define show-cone         (lambda () (glutSolidCone 1.0 2.0 32 1)))
(define show-cube         (lambda () (glutSolidCube 1.5)))
(define show-torus        (lambda () (glutSolidTorus 0.5 1.0 16 32)))

(define menu
  (lambda (m)
    (format #t ";; callback: (menu ~s) ~%" m)
    (case m
      ((1) (set! object glutSolidIcosahedron))
      ((2) (set! object glutSolidOctahedron))
      ((3) (set! object glutSolidTetrahedron))
      ((4) (set! object show-dodecahedron))
      ((5) (set! object show-sphere))
      ((6) (set! object show-cone))
      ((7) (set! object show-cube))
      ((8) (set! object show-torus))
      ((9) (glShadeModel GL_SMOOTH))
      ((10) (glShadeModel GL_FLAT))
      ((11) (exit)))))

(define reshape-func
  (lambda (w h)
    (format #t ";; callback: (reshape-func ~s ~s) ~%" w h)
    (and (> w 0)
         (> h 0)
         (begin
           (glViewport 0 0 w h)
           (glMatrixMode GL_PROJECTION)
           (glLoadIdentity)
           (if (<= w h)
               (glOrtho 0.0 16.0 0.0 (/ (* 16.0 h) w) -10.0 10.0)
               (glOrtho 0.0 (/ (* 16.0 w) h) 0.0 16.0 -10.0 10.0))
           (glMatrixMode GL_MODELVIEW)))))

(define visibility-func
  (lambda (state)
    (format #t ";; callback: (visibility-func ~s) ~%" state)))

(define f32vector
  (lambda lst
    (define-syntax f32set!
      (syntax-rules ()
        ((_ bv n value)
         (bytevector-ieee-single-native-set! bv (* n 4) value))))
    (let ((bv (make-bytevector (* (length lst) 4))))
      (let loop ((i 0) (lst lst))
        (cond ((null? lst) bv)
              (else
               (f32set! bv i (car lst))
               (loop (+ i 1) (cdr lst))))))))

(define render-one
  (lambda (x y ambr ambg ambb difr difg difb specr specg specb shine)
    (glPushMatrix)
    (glTranslatef x y 0.0)
    (cond ((eq? object show-sphere)
           (glRotatef 90.0 0.0 1.0 0.0)
           (glRotatef current-angle 0.0 0.0 1.0))
          (else
           (glRotatef current-angle -0.3 1.0 -0.5)))
    (glMaterialfv GL_FRONT GL_AMBIENT (f32vector ambr ambg ambb 1.0))
    (glMaterialfv GL_FRONT GL_DIFFUSE (f32vector difr difg difb 1.0))
    (glMaterialfv GL_FRONT GL_SPECULAR (f32vector specr specg specb 1.0))
    (glMaterialf GL_FRONT GL_SHININESS (* shine 128.0))
    (object)
    (glPopMatrix)))

(define run
  (lambda ()
    (glutInit (make-c-int (c-main-argc)) (c-main-argv))
    (glutCreateWindow (string->utf8/nul "Ypsilon"))
    (glLightfv GL_LIGHT0 GL_AMBIENT (f32vector 0.2 0.2 0.2 1.0))
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
    (glutDisplayFunc (c-callback void () display-func))
    (glutReshapeFunc (c-callback void (int int) reshape-func))
    (glutVisibilityFunc (c-callback void (void*) visibility-func))
    (glutMouseFunc (c-callback void (int int int int) mouse-func))
    (glutIdleFunc (c-callback void () rotate-func))
    (glutCreateMenu (c-callback void (int) menu))
    (glutAddMenuEntry (string->utf8/nul "Icosahedron") 1)
    (glutAddMenuEntry (string->utf8/nul "Octahedron") 2)
    (glutAddMenuEntry (string->utf8/nul "Tetrahedron") 3)
    (glutAddMenuEntry (string->utf8/nul "Dodecahedron") 4)
    (glutAddMenuEntry (string->utf8/nul "Sphere") 5)
    (glutAddMenuEntry (string->utf8/nul "Cone") 6)
    (glutAddMenuEntry (string->utf8/nul "Cube") 7)
    (glutAddMenuEntry (string->utf8/nul "Torus") 8)
    (glutAddMenuEntry (string->utf8/nul "[smooth]") 9)
    (glutAddMenuEntry (string->utf8/nul "[flat]") 10)
    (glutAddMenuEntry (string->utf8/nul "Exit") 11)
    (glutAttachMenu GLUT_RIGHT_BUTTON)
	  (glutReshapeWindow 800 800)
    (glutMainLoop)))

(run)
