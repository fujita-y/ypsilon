;; ./ypsilon --r6rs --top-level-program demo/freetype-demo.scm

(import (rnrs)
        (rnrs programs)
        (ypsilon glcorearb)
        (ypsilon glfw)
        (ypsilon view)
        (ypsilon c-ffi)
        (ypsilon c-types)
        (ypsilon freetype)
        (only (core) format mat4x4-identity mat4x4-rotate mat4x4-ortho mat4x4-mul))

(define-record-type Renderable (fields program vao vbo renderer))

(define-record-type Glyph (fields ch texture size.x size.y bearing.x bearing.y advance))

(define make-string-pane
  (lambda ()
    (define vs-source "
      #version 110
      uniform mat4 u_mvp;
      uniform vec2 u_scale;
      uniform vec2 u_offset;
      attribute vec2 a_pos;
      attribute vec2 a_texcoord0;
      varying vec2 v_texcoord0;
      void main()
      {
        gl_Position = u_mvp * vec4(a_pos * u_scale + u_offset, 0.0, 1.0);
        v_texcoord0 = a_texcoord0;
      }
    ")
    (define fs-source "
      #version 110
      varying vec2 v_texcoord0;
      uniform sampler2D text;
      void main()
      {
        gl_FragColor = vec4(0.0, texture2D(text, v_texcoord0).r, 0.0, 1.0);

      }
    ")
    (define vertices
        (let ((verts
          `(0.0 1.0 0.0 0.0
            1.0 0.0 1.0 1.0
            0.0 0.0 0.0 1.0
            0.0 1.0 0.0 0.0
            1.0 1.0 1.0 0.0
            1.0 0.0 1.0 1.0)))
          (let ((bv (make-bytevector (* 24 4))))
            (let loop ((i 0) (verts verts))
              (cond ((null? verts) bv)
                    (else
                     (bytevector-ieee-single-native-set! bv i (car verts))
                     (loop (+ i 4) (cdr verts))))))))
    (define program (init-program vs-source fs-source))
    (define vao (make-vao))
    (define vbo (make-vbo vao vertices GL_ARRAY_BUFFER GL_STATIC_DRAW))
    (define fonts
      (let ((ht (make-eq-hashtable)))
        (let-values (((face glyphs) (load-font-textures "demo/Roboto-Regular.ttf" 300)))
          (begin0
            ht
            (for-each (lambda (glyph) (hashtable-set! ht (car glyph) (apply make-Glyph glyph))) glyphs)))))
    (define renderer
      (lambda (mvp scale x y s)
        (glUseProgram program)
        (glBindVertexArray vao)
        (let loop ((s (string->list s)) (x x) (y y))
          (or (null? s)
              (let* ((ch (car s)) (glyph (hashtable-ref fonts ch #f)))
                (if glyph
                    (let ((texture (Glyph-texture glyph)))
                      (glBindTexture GL_TEXTURE_2D texture)
                      (program-uniform-max4x4-set! program "u_mvp" mvp)
                      (program-uniform-vec2-set! program "u_scale" (* scale (Glyph-size.x glyph)) (* scale (Glyph-size.y glyph)))
                      (program-uniform-vec2-set!
                        program
                        "u_offset"
                        (+ x (* scale (Glyph-bearing.x glyph)))
                        (- y (* scale (- (Glyph-size.y glyph) (Glyph-bearing.y glyph)))))
                      (glDrawArrays GL_TRIANGLES 0 6)
                      (loop (cdr s) (+ x (* scale (Glyph-advance glyph))) y))
                    (loop (cdr s) x y)))))))
    (enable-vertex-attribute program vao vbo GL_ARRAY_BUFFER "a_pos" 2 GL_FLOAT GL_FALSE 16 0)
    (enable-vertex-attribute program vao vbo GL_ARRAY_BUFFER "a_texcoord0" 2 GL_FLOAT GL_FALSE 16 8)
    (make-Renderable program vao vbo renderer)))

(define main
  (lambda ()
    (define error-callback
      (lambda (error description)
        (format #t "error: ~a~%~!" (utf8->string (make-bytevector-mapping description 1024)))))
    (define key-callback
      (lambda (window key scancode action mods)
        (and (= key GLFW_KEY_ESCAPE)
             (= action GLFW_PRESS)
             (glfwSetWindowShouldClose window GLFW_TRUE))))
    (glfwSetErrorCallback (c-callback void (int void*) error-callback))
    (let ((window (init-window 256 192 "Ypsilon")))
      (glfwSetKeyCallback window (c-callback void (void* int int int int) key-callback))
      (glfwMakeContextCurrent window)
      (glfwSwapInterval 1)
      (let ((m (make-bytevector 64))
            (p (make-bytevector 64))
            (mvp (make-bytevector 64))
            (width (make-c-int 0))
            (height (make-c-int 0))
            (model (make-string-pane)))
          (let loop ()
            (cond ((= (glfwWindowShouldClose window) 0)
                   (glfwGetFramebufferSize window width height)
                   (glViewport 0 0 (c-int-ref width) (c-int-ref height))
                   (glClear GL_COLOR_BUFFER_BIT)
                   (mat4x4-identity m)
                   (mat4x4-rotate m m 0 0 -1 (glfwGetTime))
                   (let ((ratio (/ (inexact (c-int-ref width)) (inexact (c-int-ref height)))))
                     (mat4x4-ortho p (- ratio) ratio -1 1 1 -1))
                   (mat4x4-mul mvp p m)
                   ((Renderable-renderer model) mvp 0.3 -1.0 -0.1 "Hello Ypsilon")
                   (glfwSwapBuffers window)
                   (glfwPollEvents)
                   (loop))
                  (else
                    (glfwDestroyWindow window) (glfwTerminate) (exit 0))))))))

(main)
