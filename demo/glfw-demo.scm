;; Translated from C version at https://www.glfw.org/
;; ./ypsilon --r6rs --top-level-program demo/glfw-demo.scm

(import (rnrs)
        (rnrs programs)
        (ypsilon gl)
        (ypsilon glfw)
        (ypsilon c-ffi)
        (ypsilon c-types)
        (only (core) format))

(define char*->string
  (lambda (p)
    (utf8->string (make-bytevector-mapping p 1024))))

(define error-callback
  (lambda (error description)
    (format #t "Error: ~a~%~!" (char*->string description))))

(define key-callback
  (lambda (window key scancode action mods)
    (and (= key GLFW_KEY_ESCAPE) (= action GLFW_PRESS)
         (glfwSetWindowShouldClose window GLFW_TRUE))))

(define run
  (lambda ()
    (glfwSetErrorCallback (c-callback void (int void*) error-callback))

    (if (= (glfwInit) 0) (exit 1))

    (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 2)
    (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 0)

    (let ((window (glfwCreateWindow 640 480 (make-c-string "Simple example") 0 0)))
      (if (= window 0) (begin (glfwTerminate) (exit 1)))

      (glfwMakeContextCurrent window)
      (glfwSetKeyCallback window (c-callback void (void* int int int int) key-callback))
      (glfwSwapInterval 1)

      (let loop ()
        (cond ((= (glfwWindowShouldClose window) 0)
               (let ((width (make-c-int 0)) (height (make-c-int 0)))
                 (glfwGetFramebufferSize window width height)
                 (let ((ratio (/ (inexact (c-int-ref width)) (inexact (c-int-ref height)))))
                   (glViewport 0 0 (c-int-ref width) (c-int-ref height))
                   (glClear GL_COLOR_BUFFER_BIT)
                   (glMatrixMode GL_PROJECTION)
                   (glLoadIdentity)
                   (glOrtho (- ratio) ratio -1.0 1.0 1.0 -1.0)
                   (glMatrixMode GL_MODELVIEW)
                   (glLoadIdentity)
                   (glRotatef (* (glfwGetTime) 50.0) 0.0 0.0 1.0)
                   (glBegin GL_TRIANGLES)
                   (glColor3f 1.0 0.0 0.0)
                   (glVertex3f -0.6 -0.4 0.0)
                   (glColor3f 0.0 1.0 0.0)
                   (glVertex3f 0.6 -0.4 0.0)
                   (glColor3f 0.0 0.0 1.0)
                   (glVertex3f 0.0 0.6 0.0)
                   (glEnd)
                   (glfwSwapBuffers window)
                   (glfwPollEvents)
                   (loop))))
              (else
                (glfwDestroyWindow window)
                (glfwTerminate)
                (exit 0)))))))

(run)
