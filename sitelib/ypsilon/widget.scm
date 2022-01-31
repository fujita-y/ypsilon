#!nobacktrace
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon widget)
  (export make-text-widget)
  (import (rnrs)
          (only (core) format)
          (ypsilon c-types)
          (ypsilon freetype)
          (ypsilon glcorearb)
          (ypsilon view))

(define-record-type Glyph (fields ch texture size.x size.y bearing.x bearing.y advance))

(define make-text-widget
  (lambda (font size)
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
      uniform vec4 u_color;
      uniform sampler2D text;
      varying vec2 v_texcoord0;
      void main()
      {
        gl_FragColor = vec4(u_color.rgb, u_color.a * texture2D(text, v_texcoord0).r);
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
    (define font-face #f)
    (define font-texture
      (let ((ht (make-eq-hashtable)))
        (let-values (((face glyphs) (load-font-textures font size)))
        (set! font-face face)
        (begin0
          ht
          (for-each
            (lambda (glyph)
              (hashtable-set! ht (car glyph) (apply make-Glyph glyph)))
            glyphs)))))
    (define lookup-kern-table
      (lambda (prev ch)
        (define-c-struct-methods FT_Vector)
        (if prev
            (let ((n0 (FT_Get_Char_Index font-face (char->integer prev)))
                  (n1 (FT_Get_Char_Index font-face (char->integer ch)))
                  (delta (make-FT_Vector)))
                (FT_Get_Kerning font-face n0 n1 FT_KERNING_DEFAULT delta)
                (/ (FT_Vector-x delta) 64.0))
            0.0)))
    (enable-vertex-attribute program vao vbo GL_ARRAY_BUFFER "a_pos" 2 GL_FLOAT GL_FALSE 16 0)
    (enable-vertex-attribute program vao vbo GL_ARRAY_BUFFER "a_texcoord0" 2 GL_FLOAT GL_FALSE 16 8)
    (lambda (mvp r g b a x y scale text)
      (glUseProgram program)
      (glBindVertexArray vao)
      (program-uniform-max4x4-set! program "u_mvp" mvp)
      (program-uniform-vec4-set! program "u_color" r g b a)
      (let loop ((s (string->list text)) (x x) (y y) (prev #f))
        (or (null? s)
            (let* ((ch (car s)) (glyph (hashtable-ref font-texture ch #f)))
              (if glyph
                  (let ((texture (Glyph-texture glyph)) (kern (/ (lookup-kern-table prev ch) size)))
                    (glBindTexture GL_TEXTURE_2D texture)
                    (program-uniform-vec2-set! program "u_scale" (* scale (Glyph-size.x glyph)) (* scale (Glyph-size.y glyph)))
                    (program-uniform-vec2-set!
                      program
                      "u_offset"
                      (+ x (* scale (+ kern (Glyph-bearing.x glyph))))
                      (- y (* scale (- (Glyph-size.y glyph) (Glyph-bearing.y glyph)))))
                    (glDrawArrays GL_TRIANGLES 0 6)
                    (loop (cdr s) (+ x (* scale (Glyph-advance glyph))) y ch))
                  (loop (cdr s) x y ch))))))))
  ) ;[end]
