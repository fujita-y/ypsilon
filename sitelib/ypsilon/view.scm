#!nobacktrace
;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon view)
  (export begin0 init-window init-program make-vao make-vbo enable-vertex-attribute
          program-uniform-max4x4-set!
          program-uniform-vec4-set!
          program-uniform-vec2-set!
          program-uniform-integer-set!
          load-font-textures)
  (import (rnrs)
          (ypsilon glcorearb)
          (ypsilon glfw)
          (ypsilon freetype)
          (ypsilon c-ffi)
          (ypsilon c-types)
          (only (core) format))

  (define-syntax begin0
    (syntax-rules ()
      ((_ e0 e1 ...) (begin e1 ... e0))))

  (define init-window
    (lambda (width height title)
      (if (= (glfwInit) 0) (exit 1))
      (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 2)
      (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 0)
      (let ((window (glfwCreateWindow width height (make-c-string title) 0 0)))
        (begin0
          window
          (if (= window 0)
              (begin (glfwTerminate) (exit 1)))))))

  (define init-program
    (lambda (vertex-shader fragment-shader)
      (define compile-shader
        (lambda (source type)
          (let ((shader (glCreateShader type)) (is-compiled (make-c-int 0)))
            (begin0 shader
              (glShaderSource shader 1 (make-c-void* (bytevector->pinned-c-void* (make-c-string source))) 0)
              (glCompileShader shader)
              (glGetShaderiv shader GL_COMPILE_STATUS is-compiled)
              (if (= (c-int-ref is-compiled) 0)
                  (let ((max-length (make-c-int 0)))
                    (format #t "error in glCompileShader:~%~a~%~!" source)
                    (glGetShaderiv shader GL_INFO_LOG_LENGTH max-length)
                    (let ((message (make-bytevector (c-int-ref max-length))))
                      (glGetShaderInfoLog shader (c-int-ref max-length) max-length message)
                      (format #t "~a~%~!" (utf8->string message))
                      (exit 1))))))))
      (let ((program (glCreateProgram)) (is-linked (make-c-int 0)))
        (begin0
          program
          (glAttachShader program (compile-shader vertex-shader GL_VERTEX_SHADER))
          (glAttachShader program (compile-shader fragment-shader GL_FRAGMENT_SHADER))
          (glLinkProgram program)
          (glGetProgramiv program GL_LINK_STATUS is-linked)
          (if (= (c-int-ref is-linked) 0)
              (let ((max-length (make-c-int 0)))
                (format #t "error in glLinkProgram:~%~!")
                (glGetProgramiv program GL_INFO_LOG_LENGTH max-length)
                (let ((message (make-bytevector (c-int-ref max-length))))
                  (glGetProgramInfoLog program (c-int-ref max-length) max-length message)
                  (format #t "~a~%~!" (utf8->string message))
                  (exit 1))))))))

  (define program-uniform-max4x4-set!
    (lambda (program name mat4x4)
      (let ((loc (glGetUniformLocation program (make-c-string name))))
        (glUniformMatrix4fv loc 1 GL_FALSE mat4x4))))

  (define program-uniform-integer-set!
    (lambda (program name i)
      (let ((loc (glGetUniformLocation program (make-c-string name))))
        (glUniform1i loc i))))

  (define program-uniform-vec2-set!
    (lambda (program name f1 f2)
      (let ((loc (glGetUniformLocation program (make-c-string name))))
        (glUniform2f loc f1 f2))))

  (define program-uniform-vec4-set!
    (lambda (program name f1 f2 f3 f4)
      (let ((loc (glGetUniformLocation program (make-c-string name))))
        (glUniform4f loc f1 f2 f3 f4))))

  (define make-vao
    (lambda ()
      (let ((vao (make-c-int 0)))
        (glGenVertexArrays 1 vao)
        (c-int-ref vao))))

  (define make-vbo
    (lambda (vao vertices target usage)
      (let ((vbo (make-c-int 0)))
        (begin0
          (c-int-ref vbo)
          (glBindVertexArray vao)
          (glGenBuffers 1 vbo)
          (glBindBuffer target (c-int-ref vbo))
          (glBufferData target (bytevector-length vertices) vertices usage)))))

  (define enable-vertex-attribute
    (lambda (program vao vbo target name size type normalized stride pointer)
      (glBindVertexArray vao)
      (glBindBuffer target vbo)
      (let ((loc (glGetAttribLocation program (make-c-string name))))
        (glEnableVertexAttribArray loc)
        (glVertexAttribPointer loc size type normalized stride pointer))))

  (define load-freetype
    (let ((memo #f))
      (lambda ()
        (or memo
            (let ((ft (make-c-void* 0)))
              (FT_Init_FreeType ft)
              (begin0 memo (set! memo (c-void*-ref ft))))))))

  (define load-font-textures
    (lambda (font pixel-size)
      (let ((ft (load-freetype)) (face (make-c-void* 0)))
        (FT_New_Face ft (make-c-string font) 0 face)
        (let ((face (c-void*-ref face)))
          (FT_Set_Pixel_Sizes face 0 pixel-size)
          (let loop ((codepoint 1) (acc '()))
            (cond ((> codepoint 127)
                  (glBindTexture GL_TEXTURE_2D 0)
                  (values face (reverse acc)))
                  (else
                    (FT_Load_Char face codepoint FT_LOAD_RENDER)
                    (let ((face (make-bytevector-mapping face (c-sizeof FT_FaceRec))))
                      (define-c-struct-methods FT_FaceRec FT_GlyphSlotRec FT_Bitmap FT_Vector)
                      (let ((glyph
                              (make-bytevector-mapping
                                (FT_FaceRec-glyph face)
                                (c-sizeof FT_GlyphSlotRec))))
                        (let ((bitmap (FT_GlyphSlotRec-bitmap glyph))
                              (advance (FT_GlyphSlotRec-advance glyph)))
                          (let ((buffer (FT_Bitmap-buffer bitmap))
                                (size.x (FT_Bitmap-width bitmap))
                                (size.y (FT_Bitmap-rows bitmap))
                                (bearing.x (FT_GlyphSlotRec-bitmap_left glyph))
                                (bearing.y (FT_GlyphSlotRec-bitmap_top glyph))
                                (advance (/ (FT_Vector-x advance) 64.0)))
                            (let ((texture (make-c-int 0)))
                              (glPixelStorei GL_UNPACK_ALIGNMENT 1)
                              (glGenTextures 1 texture)
                              (let ((texture (c-int-ref texture)))
                                (glBindTexture GL_TEXTURE_2D texture)
                                (glTexImage2D GL_TEXTURE_2D 0 GL_RED size.x size.y 0 GL_RED GL_UNSIGNED_BYTE buffer)
                                (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
                                (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
                                (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                                (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                                (let ((deno (inexact pixel-size)))
                                  (loop
                                    (+ codepoint 1)
                                    (cons (list
                                            (integer->char codepoint)
                                            texture
                                            (/ size.x deno)
                                            (/ size.y deno)
                                            (/ bearing.x deno)
                                            (/ bearing.y deno)
                                            (/ advance deno))
                                          acc))))))))))))))))

  ) ;[end]
