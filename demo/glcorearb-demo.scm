;; Translated from C version at https://www.glfw.org/
;; ./ypsilon --r6rs --top-level-program demo/glcorearb-demo.scm

(import (rnrs)
        (rnrs programs)
        (ypsilon glcorearb)
        (ypsilon glfw)
        (ypsilon c-ffi)
        (ypsilon c-types)
        (only (core) format mat4x4-identity mat4x4-rotate mat4x4-ortho mat4x4-mul))

(define char*->string
  (lambda (p)
    (utf8->string (make-bytevector-mapping p 1024))))

(define error-callback
  (lambda (error description)
    (format #t "Error: ~a~%~!" (char*->string description))))

(define key-callback
  (lambda (window key scancode action mods)
    (and (= key GLFW_KEY_ESCAPE)
         (= action GLFW_PRESS)
         (glfwSetWindowShouldClose window GLFW_TRUE))))

(define vertices
  (let ((bv (make-bytevector (* 4 15))))
    (let loop ((i 0) (vals '(-0.6 -0.4 1.0 0.0 0.0 0.6 -0.4 0.0 1.0 0.0 0.0 0.6 0.0 0.0 1.0)))
      (cond ((null? vals) bv)
            (else
              (bytevector-ieee-single-native-set! bv i (car vals))
              (loop (+ i 4) (cdr vals)))))))

(define vertex_shader_text (make-c-string "
  #version 110
  uniform mat4 MVP;
  attribute vec3 vCol;
  attribute vec2 vPos;
  varying vec3 color;
  void main()
  {
      gl_Position = MVP * vec4(vPos, 0.0, 1.0);
      color = vCol;
  }
"))

(define fragment_shader_text (make-c-string "
  #version 110
  varying vec3 color;
  void main()
  {
      gl_FragColor = vec4(color, 1.0);
  }
"))

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
      (let ((vertex_buffer (make-c-int 0)))
        (glGenBuffers 1 vertex_buffer)
        (glBindBuffer GL_ARRAY_BUFFER (c-int-ref vertex_buffer))
        (glBufferData GL_ARRAY_BUFFER (bytevector-length vertices) vertices GL_STATIC_DRAW)
        (let ((vertex_shader (glCreateShader GL_VERTEX_SHADER))
              (fragment_shader (glCreateShader GL_FRAGMENT_SHADER)))
          (glShaderSource vertex_shader 1 (make-c-void* (bytevector->pinned-c-void* vertex_shader_text)) 0)
          (glCompileShader vertex_shader)
          (glShaderSource fragment_shader 1 (make-c-void* (bytevector->pinned-c-void* fragment_shader_text)) 0)
          (glCompileShader fragment_shader)
          (let ((program (glCreateProgram)))
            (glAttachShader program vertex_shader)
            (glAttachShader program fragment_shader)
            (glLinkProgram program)
            (let ((mvp_location (glGetUniformLocation program (make-c-string "MVP")))
                  (vpos_location (glGetAttribLocation program (make-c-string "vPos")))
                  (vcol_location (glGetAttribLocation program (make-c-string "vCol"))))
              (glEnableVertexAttribArray vpos_location)
              (glVertexAttribPointer vpos_location 2 GL_FLOAT GL_FALSE 20 0)
              (glEnableVertexAttribArray vcol_location)
              (glVertexAttribPointer vcol_location 3 GL_FLOAT GL_FALSE 20 8)
              (let loop ()
                (cond ((= (glfwWindowShouldClose window) 0)
                       (let ((width (make-c-int 0)) (height (make-c-int 0)))
                         (glfwGetFramebufferSize window width height)
                         (let ((ratio (/ (inexact (c-int-ref width)) (inexact (c-int-ref height)))))
                           (glViewport 0 0 (c-int-ref width) (c-int-ref height))
                           (glClear GL_COLOR_BUFFER_BIT)
                           (let ((m (make-bytevector 64))
                                 (p (make-bytevector 64))
                                 (mvp (make-bytevector 64)))
                             (mat4x4-identity m)
                             (mat4x4-rotate m m 0 0 1 (glfwGetTime))
                             (mat4x4-ortho p (- ratio) ratio -1 1 1 -1)
                             (mat4x4-mul mvp p m)
                             (glUseProgram program)
                             (glUniformMatrix4fv mvp_location 1 GL_FALSE mvp)
                             (glDrawArrays GL_TRIANGLES 0 3)
                             (glfwSwapBuffers window)
                             (glfwPollEvents)
                             (loop)))))
                      (else (glfwDestroyWindow window) (glfwTerminate) (exit 0)))))))))))

(run)
