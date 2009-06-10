#!/usr/bin/env ypsilon

;;; JelloBench - A practical garbage collection benchmark program.
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

;;; Supports Linux, Mac OS X, and Windows XP.
;;; Requires Ypsilon(svn trunk), SDL, SDL_image, and SDL_mixer.

(library (jello config)

  (export config-auto-on
          config-demo-on
          config-show-fps-on
          config-show-collect-on)

  (import (rnrs))

  (define config-auto-on #f)         ; hit 'a' key to toggle - autoplay
  (define config-demo-on #t)         ; hit 'd' key to toggle - shows banner
  (define config-show-fps-on #f)     ; hit 'f' key to toggle - prints FPS
  (define config-show-collect-on #f) ; hit 'c' key to toggle - prints GC activity

  );[end]

(library (jello loader)

  (export load-image
          load-music
          load-sound)

  (import (rnrs)
          (srfi :48)
          (ypsilon sdl)
          (only (core) current-directory))

  (define locate-data-file
    (lambda (filename)
      (format "~a/jello-data/~a" (current-directory) filename)))

  (define load-image
    (lambda (filename)
      (let ((path (locate-data-file filename)))
        (let ((file-image (IMG_Load path)))
          (when (= file-image 0) (assertion-violation 'IMG_Load (SDL_GetError)))
          (let ((device-image (SDL_DisplayFormatAlpha file-image)))
            (SDL_FreeSurface file-image)
            (when (= device-image 0) (assertion-violation 'SDL_DisplayFormat (SDL_GetError)))
            device-image)))))

  (define load-music
    (lambda (filename)
      (let ((path (locate-data-file filename)))
        (let ((music (and (file-exists? path) (Mix_LoadMUS path))))
          (when (and music (= music 0))
            (assertion-violation 'load-music (SDL_GetError)))
          music))))

  (define load-wav-from-file
    (lambda (file)
      (Mix_LoadWAV_RW (SDL_RWFromFile file "rb") 1)))

  (define load-sound
    (lambda (filename)
      (let ((path (locate-data-file filename)))
        (let ((sound (and (file-exists? path) (load-wav-from-file path))))
          (when (and sound (= sound 0))
            (assertion-violation 'load-sound (SDL_GetError)))
          sound))))

  ) ;[end]

(library (jello sprite)

  (export init-opengl-attribute
          opengl-enter-2d-mode
          opengl-leave-2d-mode
          make-opengl-sprite
          draw-opengl-sprite
          draw-opengl-sprite-h-wipe
          draw-opengl-sprite-h-reverse-wipe)
  (import (rnrs)
          (ypsilon gl)
          (ypsilon sdl)
          (ypsilon ffi)
          (only (core) define-struct))

  (define-struct sprite (width height id x1y0 x0y1 x1y1))

  (define make-float2d
    (lambda (x y)
      (let ((float2d (make-bytevector 8)))
        (bytevector-ieee-single-native-set! float2d 0 x)
        (bytevector-ieee-single-native-set! float2d 4 y)
        float2d)))

  (define x0y0 (make-float2d 0.0 0.0))

  (define init-opengl-attribute
    (lambda ()
      (SDL_GL_SetAttribute SDL_GL_RED_SIZE 8)
      (SDL_GL_SetAttribute SDL_GL_GREEN_SIZE 8)
      (SDL_GL_SetAttribute SDL_GL_BLUE_SIZE 8)
      (SDL_GL_SetAttribute SDL_GL_DEPTH_SIZE 16)
      (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)))

  (define opengl-enter-2d-mode
    (lambda ()
      (define-c-struct-methods SDL_Surface)
      (let ((screen (c-coerce-void* (SDL_GetVideoSurface) SDL_Surface)))
        (glPushAttrib GL_ENABLE_BIT)
        (glDisable GL_DEPTH_TEST)
        (glDisable GL_CULL_FACE)
        (glEnable GL_TEXTURE_2D)
        (glEnable GL_BLEND)
        (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
        (glViewport 0 0 (SDL_Surface-w screen) (SDL_Surface-h screen))
        (glMatrixMode GL_PROJECTION)
        (glPushMatrix)
        (glLoadIdentity)
        (glOrtho 0.0 (inexact (SDL_Surface-w screen)) (inexact (SDL_Surface-h screen)) 0.0 0.0 1.0)
        (glMatrixMode GL_MODELVIEW)
        (glPushMatrix)
        (glLoadIdentity)
        (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE))))

  (define opengl-leave-2d-mode
    (lambda ()
      (glMatrixMode GL_MODELVIEW)
      (glPopMatrix)
      (glMatrixMode GL_PROJECTION)
      (glPopMatrix)
      (glPopAttrib)))

  (define make-opengl-sprite
    (lambda (surface)
      (define-c-struct-methods SDL_Rect SDL_Surface SDL_PixelFormat)
      (define n^2 (lambda (size) (let loop ((n 2)) (if (>= n size) n (loop (* n 2))))))
      (let ((surface (c-coerce-void* surface SDL_Surface)))
        (let* ((w (SDL_Surface-w surface))
               (h (SDL_Surface-h surface))
               (w2 (n^2 w))
               (h2 (n^2 h))
               (texcoord2 (/ (inexact w) w2))
               (texcoord3 (/ (inexact h) h2))
               (image (SDL_CreateRGBSurface SDL_SWSURFACE w2 h2 32 #x000000ff #x0000ff00 #x00ff0000 #xff000000)))
          (when (= image 0) (assertion-violation 'SDL_CreateRGBSurface (SDL_GetError)))
          (let ((saved-flags (bitwise-and (SDL_Surface-flags surface) (bitwise-ior SDL_SRCALPHA SDL_RLEACCELOK)))
                (saved-alpha (SDL_PixelFormat-alpha (c-coerce-void* (SDL_Surface-format surface) SDL_PixelFormat))))
            (when (= (bitwise-and saved-flags SDL_SRCALPHA) SDL_SRCALPHA)
              (SDL_SetAlpha surface 0 0))
            (let ((area (make-SDL_Rect)))
              (SDL_Rect-x-set! area 0)
              (SDL_Rect-y-set! area 0)
              (SDL_Rect-w-set! area w)
              (SDL_Rect-h-set! area h)
              (SDL_BlitSurface surface area image area)
              (when (= (bitwise-and saved-flags SDL_SRCALPHA) SDL_SRCALPHA)
                (SDL_SetAlpha surface saved-flags saved-alpha))
              (let ((texture (make-c-int 0)))
                (glGenTextures 1 texture)
                (let ((texture (c-unsigned-int-ref texture)))
                  (glBindTexture GL_TEXTURE_2D texture)
                  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
                  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
                  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w2 h2 0 GL_RGBA GL_UNSIGNED_BYTE
                                (make-bytevector-mapping (SDL_Surface-pixels (c-coerce-void* image SDL_Surface)) (* w2 h2 4)))
                  (SDL_FreeSurface image)
                  (make-sprite w h texture (make-float2d texcoord2 0.0) (make-float2d 0.0 texcoord3) (make-float2d texcoord2 texcoord3))))))))))

  (define draw-opengl-sprite
    (lambda (sprite x y)
      (glBindTexture GL_TEXTURE_2D (sprite-id sprite))
      (glBegin GL_TRIANGLE_STRIP)
      (let ((w (sprite-width sprite)) (h (sprite-height sprite)))
        (glTexCoord2fv x0y0)
        (glVertex2i x y)
        (glTexCoord2fv (sprite-x1y0 sprite))
        (glVertex2i (+ x w) y)
        (glTexCoord2fv (sprite-x0y1 sprite))
        (glVertex2i x (+ y h))
        (glTexCoord2fv (sprite-x1y1 sprite))
        (glVertex2i (+ x w) (+ y h))
        (glEnd))))

  (define draw-opengl-sprite-h-wipe
    (lambda (sprite x y wipe)
      (glBindTexture GL_TEXTURE_2D (sprite-id sprite))
      (glBegin GL_TRIANGLE_STRIP)
      (let ((w (sprite-width sprite)) (h (sprite-height sprite)))
        (let* ((c (/ w 2.0)))
          (let ((left (- c (* c wipe)))
                (right (+ c (* c wipe))))
            (glTexCoord2fv x0y0)
            (glVertex2f (+ x left) y)
            (glTexCoord2fv (sprite-x1y0 sprite))
            (glVertex2f (+ x right) y)
            (glTexCoord2fv (sprite-x0y1 sprite))
            (glVertex2f (+ x left) (+ y h))
            (glTexCoord2fv (sprite-x1y1 sprite))
            (glVertex2f (+ x right) (+ y h))
            (glEnd))))))

  (define draw-opengl-sprite-h-reverse-wipe
    (lambda (sprite x y wipe)
      (glBindTexture GL_TEXTURE_2D (sprite-id sprite))
      (glBegin GL_TRIANGLE_STRIP)
      (let ((w (sprite-width sprite)) (h (sprite-height sprite)))
        (let* ((right (bytevector-ieee-single-native-ref (sprite-x1y1 sprite) 0))
               (bottom (bytevector-ieee-single-native-ref (sprite-x1y1 sprite) 4))
               (center (/ right 2.0))
               (left (- center (* center wipe)))
               (right (+ center (* center wipe))))
          (glTexCoord2f left 0)
          (glVertex2i x y)
          (glTexCoord2f right 0)
          (glVertex2i (+ x w) y)
          (glTexCoord2f left bottom)
          (glVertex2i x (+ y h))
          (glTexCoord2f right bottom)
          (glVertex2i (+ x w) (+ y h))
          (glEnd)))))

  ) ;[end]

(library (jello bench)

  (export program-main)

  (import (rnrs)
          (only (srfi :1) iota)
          (srfi :27)
          (srfi :39)
          (srfi :48)
          (ypsilon gl)
          (ypsilon sdl)
          (jello config)
          (jello loader)
          (jello sprite)
          (only (core)
                usleep
                list-head
                microsecond
                collect-notify
                destructuring-bind
                make-cmwc-random-state
                cmwc-random-u32))

  (define random-u32
    (let ((state (make-cmwc-random-state (microsecond))))
      (lambda ()
        (cmwc-random-u32 state))))

  (define-c-struct-methods SDL_Rect SDL_Surface SDL_MouseMotionEvent SDL_keysym SDL_KeyboardEvent)

  (define auto-on config-auto-on)
  (define demo-on config-demo-on)
  (define fps-on config-show-fps-on)
  (define collect-on config-show-collect-on)

  ;;; Sprite data

  (define title-sprite)
  (define logo-sprite)
  (define ball-sprite)
  (define red-sprite)
  (define green-sprite)
  (define blue-sprite)
  (define blue-frag-sprite)
  (define red-frag-sprite)
  (define green-frag-sprite)
  (define big-blue-frag-sprite)
  (define big-red-frag-sprite)
  (define big-green-frag-sprite)
  (define mini-yellow-frag-sprite)
  (define mini-blue-frag-sprite)
  (define small-red-frag-sprite)
  (define small-green-frag-sprite)
  (define small-blue-frag-sprite)
  (define init-sprites
    (lambda ()
      (set! title-sprite (make-opengl-sprite (load-image "title.png")))
      (set! logo-sprite (make-opengl-sprite (load-image "ypsilon-logo.png")))
      (set! paddle-sprite (make-opengl-sprite (load-image "paddle.png")))
      (set! blue-sprite (make-opengl-sprite (load-image "blue.png")))
      (set! red-sprite (make-opengl-sprite (load-image "red.png")))
      (set! green-sprite (make-opengl-sprite (load-image "green.png")))
      (set! ball-sprite (make-opengl-sprite (load-image "ball16.png")))
      (set! blue-frag-sprite (make-opengl-sprite (load-image "blue-frag.png")))
      (set! red-frag-sprite (make-opengl-sprite (load-image "red-frag.png")))
      (set! green-frag-sprite (make-opengl-sprite (load-image "green-frag.png")))
      (set! big-blue-frag-sprite (make-opengl-sprite (load-image "big-blue-frag.png")))
      (set! big-red-frag-sprite (make-opengl-sprite (load-image "big-red-frag.png")))
      (set! big-green-frag-sprite (make-opengl-sprite (load-image "big-green-frag.png")))
      (set! mini-yellow-frag-sprite (make-opengl-sprite (load-image "mini-frag-yellow.png")))
      (set! mini-blue-frag-sprite (make-opengl-sprite (load-image "mini-frag-blue.png")))
      (set! small-green-frag-sprite (make-opengl-sprite (load-image "small-green-frag.png")))
      (set! small-red-frag-sprite (make-opengl-sprite (load-image "small-red-frag.png")))
      (set! small-blue-frag-sprite (make-opengl-sprite (load-image "small-blue-frag.png")))))

  ;;; Sound data

  (define music)
  (define SE-wall)
  (define SE-paddle)
  (define SE-block)
  (define SE-setup-block)
  (define SE-ball-dead)
  (define SE-ball-start)
  (define SE-complete)
  (define init-music-and-sound
    (lambda ()
      (Mix_VolumeMusic 64)
      (set! music (load-music "music.wav"))
      (set! SE-wall (load-sound "wall.wav"))
      (set! SE-paddle (load-sound "paddle.wav"))
      (set! SE-block (load-sound "block.wav"))
      (set! SE-setup-block (load-sound "setup-block.wav"))
      (set! SE-ball-dead (load-sound "ball-dead.wav"))
      (set! SE-ball-start (load-sound "ball-start.wav"))
      (set! SE-complete (load-sound "complete.wav"))))

  ;;; Game parameter

  (define paddle-level 0)
  (define paddle-sprite)
  (define paddle-x (+ (mod (random-u32) 500) 10))
  (define paddle-y 440)
  (define paddle-width 64)
  (define paddle-height 16)
  (define ball-x 253)
  (define ball-y 122)
  (define ball-dx 180)
  (define ball-dy 360)
  (define ball-tail-max 20)
  (define ball-tail 0)
  (define ball-level 255)
  (define ball-last-move (microsecond))
  (define limit-x (* (- 640 16) 128))
  (define limit-2x (* limit-x 2))
  (define limit-y (* (- 480 16) 128))
  (define limit-2y (* limit-y 2))
  (define dead-y (* (+ 480 16) 128))
  (define blocks (make-bytevector (* 7 3)))
  (define blocks-org-x 96)
  (define blocks-org-y 40)
  (define block-width 64)
  (define block-height 27)
  (define block-alive 0)

  ;;; Misc utils

  (define opacity-table
    (let ((table (make-bytevector (* 256 8))))
      (let loop ((n 0))
        (cond ((= n 256) table)
              (else
               (bytevector-ieee-double-native-set! table (* n 8) (* 1.4 (/ n 255.0)))
               (loop (+ n 1)))))))

  (define octet->opacity
    (lambda (n)
      (bytevector-ieee-double-native-ref opacity-table (* n 8))))

  (define show-fps
    (let ((memo (microsecond)) (frame 0))
      (lambda ()
        (set! frame (+ frame 1))
        (when (> (- (microsecond) memo) 1000000)
          (format #t ";; FPS: ~a\n" frame)
          (set! frame 0)
          (set! memo (microsecond))))))

  ;;; Screen animation

  (define draw-paddle
    (lambda ()
      (glColor4d 1.0 1.0 1.0 (octet->opacity paddle-level))
      (draw-opengl-sprite paddle-sprite paddle-x paddle-y)
      (glColor4d 1.0 1.0 1.0 (octet->opacity paddle-level))))

  (define increase-ball-speed
    (lambda ()
      (let ((p1 (sqrt (inexact (+ (* ball-dx ball-dx) (* ball-dy ball-dy))))))
        (let ((p1 (* p1 1.01)))
          (let ((p1 (if (> p1 800.0) 800.0 p1)))
            (let ((p2 (sqrt (inexact (+ (* ball-dx ball-dx) (* ball-dy ball-dy))))))
              (let ((dx (/ ball-dx p2)) (dy (/ ball-dy p2)))
                (set! ball-dx (exact (round (* dx p1))))
                (set! ball-dy (exact (round (* dy p1)))))))))))

  (define update-ball-position
    (lambda ()
      (when (< ball-tail ball-tail-max)
        (set! ball-tail (+ ball-tail 1)))
      (cond ((= block-alive 0)
             (cond ((= ball-level 0))
                   ((< ball-level 2)
                    (set! ball-level 0))
                   (else
                    (set! ball-level (- ball-level 2)))))
            (else
             (unless (= ball-level 255)
               (set! ball-level (+ ball-level 2)))))
      (set! ball-x (+ ball-x ball-dx))
      (set! ball-y (+ ball-y ball-dy))
      (cond ((< ball-x 0)
             (set! ball-x (- ball-x))
             (set! ball-dx (- ball-dx))
             (when (= ball-level 255)
               (and SE-wall (Mix_PlayChannelTimed -1 SE-wall 0 -1))
               (add-mini-fragments mini-yellow-frag-sprite ball-x ball-y 3)))
            ((< ball-y 0)
             (set! ball-y (- ball-y))
             (set! ball-dy (- ball-dy))
             (when (= ball-level 255)
               (and SE-wall (Mix_PlayChannelTimed -1 SE-wall 0 -1))
               (add-mini-fragments mini-yellow-frag-sprite ball-x ball-y 3)))
            ((> ball-x limit-x)
             (set! ball-x (- limit-2x ball-x))
             (set! ball-dx (- ball-dx))
             (when (= ball-level 255)
               (and SE-wall (Mix_PlayChannelTimed -1 SE-wall 0 -1))
               (add-mini-fragments mini-yellow-frag-sprite ball-x ball-y 3)))
            (else
             (let ((x (div ball-x 128)) (y (div ball-y 128)))
               (when (and (> y (- paddle-y 16))
                          (< y (+ paddle-y 16))
                          (> x (- paddle-x 16))
                          (< x (+ paddle-x paddle-width)))
                 (when (> ball-dy 0)
                   (when (= ball-level 255) (and SE-paddle (Mix_PlayChannelTimed -1 SE-paddle 0 -1)))
                   (set! ball-tail 0)
                   (let ((p1 (sqrt (inexact (+ (* ball-dx ball-dx) (* ball-dy ball-dy)))))
                         (delta (+ (- x paddle-x (div paddle-width 2)) 8)))
                     (cond ((> delta 0)
                            (set! ball-dx (+ ball-dx (* delta delta))))
                           (else
                            (set! ball-dx (- ball-dx (* delta delta)))))
                     (let ((p1 (* p1 1.04)))
                       (let ((p1 (if (> p1 800.0) 800.0 p1)))
                         (let ((p2 (sqrt (inexact (+ (* ball-dx ball-dx) (* ball-dy ball-dy))))))
                           (let ((dx (/ ball-dx p2)) (dy (- (/ ball-dy p2))))
                             (let ((dy (if (> dy -0.3) -0.3 dy)))
                               (set! ball-dx (exact (round (* dx p1))))
                               (set! ball-dy (exact (round (* dy p1)))))))))))))))))

  (define draw-ball
    (lambda ()
      (unless (= ball-level 0)
        (let loop ((n 0) (m 0.4) (x ball-x) (y ball-y))
          (cond ((>= n ball-tail))
                (else
                 (glColor4d (octet->opacity ball-level) (octet->opacity ball-level) (octet->opacity ball-level) m)
                 (draw-opengl-sprite ball-sprite (div x 128) (div y 128))
                 (loop (+ n 1) (* m 0.8) (- x ball-dx) (- y ball-dy)))))
        (glColor4d (octet->opacity ball-level) (octet->opacity ball-level) (octet->opacity ball-level) 1.0)
        (draw-opengl-sprite ball-sprite (div ball-x 128) (div ball-y 128)))))

  (define draw-blocks
    (lambda ()
      (let loop-y ((i 0) (y blocks-org-y))
        (cond ((> y 120))
              (else
               (let loop-x ((i i) (x blocks-org-x))
                 (cond ((>= x 544) (loop-y i (+ y block-height)))
                       (else
                        (let ((state (bytevector-u8-ref blocks i)))
                          (cond ((= state 0))
                                (else
                                 (glColor4d 1.0 1.0 1.0 (octet->opacity state))
                                 (case (mod x 3)
                                   ((0) (draw-opengl-sprite red-sprite x y))
                                   ((1) (draw-opengl-sprite green-sprite x y))
                                   ((2) (draw-opengl-sprite blue-sprite x y))))))
                        (loop-x (+ i 1) (+ x block-width))))))))
      (glColor4d 1.0 1.0 1.0 1.0)))

  (define update-blocks
    (lambda ()
      (let loop ((i 0))
        (cond ((>= i 21))
              (else
               (let ((state (bytevector-u8-ref blocks i)))
                 (cond ((= state 0))
                       ((= state 255))
                       ((< state 6)
                        (bytevector-u8-set! blocks i 0))
                       (else
                        (bytevector-u8-set! blocks i (- state 6)))))
               (loop (+ i 1)))))))

  (define fragment-display-list '())

  (define mini-fragment-display-list '())

  (define fragments-motion-list
    (begin
      (for-each random-integer (iota (mod (random-u32) 1234) 1))
      (map (lambda (n)
             (list (- (exact (round (sqrt (* (random-integer 20) (random-integer 20))))) 10)
                   (- (exact (round (sqrt (* (random-integer 20) (random-integer 20))))) 10)
                   (- (exact (round (sqrt (* (random-integer 600) (random-integer 600))))) 300)
                   (- (exact (round (sqrt (* (random-integer 400) (random-integer 400))))) 200)))
           (iota 512))))

  (define add-fragments
    (lambda (sprite org-x org-y count)
      (let ((n (mod (* (random-u32) count org-x org-y) 256)))
        (let ((lst (list-head (list-tail fragments-motion-list n) count)))
          (let ((lst (map (lambda (dot)
                            (destructuring-bind (x y dx dy) dot
                              (list (+ x org-x) (+ y org-y) dx dy)))
                          lst)))
            (set! fragment-display-list (cons `(255 ,sprite ,@lst) fragment-display-list)))))))

  (define add-mini-fragments
    (lambda (sprite org-x org-y count)
      (let ((n (mod (random-u32) 256)))
        (let ((lst (list-head (list-tail fragments-motion-list n) count)))
          (let ((lst (map (lambda (dot)
                            (destructuring-bind (x y dx dy) dot
                              (list (+ x org-x) (+ y org-y) dx dy)))
                          lst)))
            (set! mini-fragment-display-list (cons `(255 ,sprite ,@lst) fragment-display-list)))))))

  (define update-fragments
    (lambda ()
      (set! fragment-display-list
            (filter values
                    (map (lambda (frag)
                           (destructuring-bind (level sprite . lst) frag
                             (cond ((<= level 2) #f)
                                   (else
                                    (cons* (- level 2)
                                           sprite
                                           (map (lambda (dot)
                                                  (destructuring-bind (x y dx dy) dot
                                                    (list (+ x dx) (+ y dy) dx (+ dy 3))))
                                                lst))))))
                         fragment-display-list)))))

  (define update-mini-fragments
    (lambda ()
      (set! mini-fragment-display-list
            (filter values
                    (map (lambda (frag)
                           (destructuring-bind (level sprite . lst) frag
                             (cond ((<= level 4) #f)
                                   (else
                                    (cons* (- level 4)
                                           sprite
                                           (map (lambda (dot)
                                                  (destructuring-bind (x y dx dy) dot
                                                    (list (+ x dx) (+ y dy) dx (+ dy 2))))
                                                lst))))))
                         mini-fragment-display-list)))))

  (define update-mini-fragments-slow
    (lambda ()
      (set! mini-fragment-display-list
            (filter values
                    (map (lambda (frag)
                           (destructuring-bind (level sprite . lst) frag
                             (cond ((<= level 1) #f)
                                   (else
                                    (cons* (- level 1)
                                           sprite
                                           (map (lambda (dot)
                                                  (destructuring-bind (x y dx dy) dot
                                                    (list (+ x dx) (+ y dy) dx (+ dy 2))))
                                                lst))))))
                         mini-fragment-display-list)))))

  (define draw-fragments
    (lambda ()
      (for-each
       (lambda (frag)
         (destructuring-bind (level sprite . lst) frag
           (glColor4d 1.0 1.0 1.0 (octet->opacity level))
           (for-each (lambda (dot)
                       (destructuring-bind (x y dx dy) dot
                         (draw-opengl-sprite sprite (div x 128) (div y 128))))
                     lst)))
       fragment-display-list)))

  (define draw-mini-fragments
    (lambda ()
      (for-each
       (lambda (frag)
         (destructuring-bind (level sprite . lst) frag
           (glColor4d 1.0 1.0 1.0 (octet->opacity level))
           (for-each (lambda (dot)
                       (destructuring-bind (x y dx dy) dot
                         (draw-opengl-sprite sprite (div x 128) (div y 128))))
                     lst)))
       mini-fragment-display-list)))

  ;;; Game

  (define block-reset-time)

  (define init-blocks
    (lambda ()

      (define last-update)

      (define update
        (lambda ()
          (let ((now (microsecond)))
            (when (> now (+ last-update 2000))
              (let ((delta (div (- now last-update) 2000)))
                (set! last-update now)
                (let loop ((i 0))
                  (cond ((>= i 21))
                        (else
                         (let ((state (bytevector-u8-ref blocks i)))
                           (unless (= state 0)
                             (bytevector-u8-set! blocks i (min 255 (+ state delta)))))
                         (loop (+ i 1))))))))))

      (set! last-update (microsecond))
      (collect-notify #f)
      (when demo-on
        (let ((until (+ last-update 5000000)))
          (let loop ((last last-update) (level 0.0))
            (let ((now (microsecond)))
              (when (< now until)
                (cond ((> now (+ last 2000))
                       (handle-event)
                       (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                       (if (> level 1.0)
                           (glColor4d 1.0 1.0 1.0 1.0)
                           (glColor4d 1.0 1.0 1.0 (* level 0.9)))
                       (if (> level 0.5)
                           (draw-opengl-sprite title-sprite 66 120)
                           (draw-opengl-sprite-h-reverse-wipe title-sprite 65 120 (* level 2.0)))
                       (SDL_GL_SwapBuffers)
                       (loop now (+ level (/ (- now last) 200000.0))))
                      (else
                       (usleep 1000)
                       (loop last level)))))))
        (set! last-update (microsecond))
        (let ((until (+ last-update 1200000)))
          (let loop ((last last-update) (level 1.0))
            (let ((now (microsecond)))
              (when (< now until)
                (cond ((> now (+ last 7000))
                       (handle-event)
                       (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                       (glColor4d 1.0 1.0 1.0 1.0)
                       (draw-opengl-sprite logo-sprite 254 180)
                       (if (< level 0.0)
                           (glColor4d 1.0 1.0 1.0 0.0)
                           (glColor4d 1.0 1.0 1.0 level))
                       (draw-opengl-sprite title-sprite 66 120)
                       (SDL_GL_SwapBuffers)
                       (loop now (- level (/ (- now last) 700000.0))))
                      (else
                       (usleep 1000)
                       (loop last level)))))))
        (set! last-update (microsecond))
        (collect-notify collect-on)
        (glColor4d 1.0 1.0 1.0 1.0)
        (set! block-alive 21)
        (bytevector-fill! blocks 0)
        (let loop ((n 0) (last last-update))
          (let ((now (microsecond)))
            (handle-event)
            (update)
            (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
            (draw-blocks)
            (draw-opengl-sprite logo-sprite 254 180)
            (draw-paddle)
            (SDL_GL_SwapBuffers)
            (cond ((= n 7)
                   (cond ((> now (+ last 480000))
                          (bytevector-fill! blocks 255))
                         (else
                          (usleep 1000)
                          (loop n last))))
                  (else
                   (cond ((> now (+ last 200000))
                          (for-each (lambda (i) (bytevector-u8-set! blocks i 1))
                                    (map (lambda (i) (+ i n)) '(0 7 14)))
                          (and SE-setup-block (Mix_PlayChannelTimed -1 SE-setup-block 0 -1))
                          (loop (+ n 1) (+ last 200000)))
                         (else
                          (usleep 1000)
                          (loop n last))))))))))

  (define ball-hit-test
    (lambda ()
      (let ((x (+ (div ball-x 128) 8)) (y (+ (div ball-y 128) 8)))
        (let ((hit-x (div (- x blocks-org-x) block-width))
              (hit-y (div (- y blocks-org-y) block-height)))
          (if (and (<= 0 hit-x 6) (<= 0 hit-y 2))
              (let ((i (+ (* hit-y 7) hit-x)))
                (when (= (bytevector-u8-ref blocks i) 255)
                  (set! ball-dy (- ball-dy))
                  (set! ball-tail 0)
                  (set! block-alive (- block-alive 1))
                  (increase-ball-speed)
                  (cond ((= block-alive 0)
                         (Mix_HaltMusic)
                         (and SE-complete (Mix_PlayChannelTimed -1 SE-complete 0 -1))
                         (set! block-reset-time (+ (microsecond) 2000000)))
                        (else
                         (and SE-block (Mix_PlayChannelTimed -1 SE-block 0 -1))))
                  (let ((color (mod hit-x 3)))
                    (add-fragments (case color
                                     ((0) small-red-frag-sprite)
                                     ((1) small-green-frag-sprite)
                                     ((2) small-blue-frag-sprite))
                                   ball-x
                                   ball-y
                                   16)
                    (add-fragments (case color
                                     ((0) red-frag-sprite)
                                     ((1) green-frag-sprite)
                                     ((2) blue-frag-sprite))
                                   ball-x
                                   ball-y
                                   8)
                    (add-fragments (case color
                                     ((0) big-red-frag-sprite)
                                     ((1) big-green-frag-sprite)
                                     ((2) big-blue-frag-sprite))
                                   ball-x
                                   ball-y
                                   2))
                  (bytevector-u8-set! blocks i 254))))))))

  (define new-ball
    (lambda (wait fade-paddle)
      (and fade-paddle (set! paddle-level 1))
      (let ((until (+ (microsecond) wait)))
        (let loop ()
          (let ((now (microsecond)))
            (cond ((> now until))
                  (else
                   (when (> now (+ ball-last-move 10000))
                     (set! ball-last-move now)
                     (update-paddle)
                     (handle-event))
                   (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                   (draw-blocks)
                   (draw-opengl-sprite logo-sprite 254 180)
                   (draw-paddle)
                   (SDL_GL_SwapBuffers)
                   (loop))))))

      (and music
           (unless (= (Mix_PlayMusic music -1) 0)
             (assertion-violation 'Mix_PlayMusic (SDL_GetError))))
      (set! ball-x 253)
      (set! ball-y 122)
      (set! ball-dx (+ 180 (random-integer 10)))
      (set! ball-dy (- 360 (random-integer 10)))
      (set! ball-tail 0)
      (set! ball-last-move (microsecond))
      (set! ball-level 1)))

  (define ball-dead
    (lambda ()
      (Mix_HaltMusic)
      (set! ball-level 0)
      (set! paddle-level 0)
      (and SE-ball-dead (Mix_PlayChannelTimed -1 SE-ball-dead 0 -1))
      (add-mini-fragments mini-blue-frag-sprite (* (+ paddle-x 32) 128) (* paddle-y 128) 64)
      (let ((until (+ (microsecond) 2500000)))
        (let loop ()
          (let ((now (microsecond)))
            (cond ((> now until))
                  (else
                   (when (> now (+ ball-last-move 10000))
                     (set! ball-last-move now)
                     (handle-event)
                     (update-fragments)
                     (update-mini-fragments-slow))
                   (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                   (draw-blocks)
                   (draw-opengl-sprite logo-sprite 254 180)
                   (draw-fragments)
                   (draw-mini-fragments)
                   (SDL_GL_SwapBuffers)
                   (loop))))))))

  (define update-paddle
    (lambda ()
      (when (and auto-on (> ball-level 0))
        (let ((delta (- (+ (div (+ ball-x ball-dx) 128) 8) (+ paddle-x 32))))
          (cond ((or (< ball-dy 0) (< ball-y 26000))
                 (when (and (< 12800 ball-x 69120) (not (= 0 (bitwise-and (random-u32) 15))))
                   (cond ((> delta 0)
                          (set! paddle-x (+ paddle-x 1)))
                         ((< delta 0)
                          (set! paddle-x (- paddle-x 1))))))
                (else
                 (cond ((and (< ball-y 42000) (not (< 6400 ball-x 75520)))
                        (cond ((> delta 0)
                               (set! paddle-x (+ paddle-x 1)))
                              ((< delta 0)
                               (set! paddle-x (- paddle-x 1)))))
                       ((> delta 6)
                        (if (and (> ball-dy 1024) (> ball-dx 2048) (< paddle-x 320))
                            (set! paddle-x (+ paddle-x 6))
                            (set! paddle-x (+ paddle-x 4))))
                       ((> delta 0)
                        (set! paddle-x (+ paddle-x 1)))
                       ((< delta 6)
                        (if (and (> ball-dy 1024) (< ball-dx -2048) (> paddle-x 320))
                            (set! paddle-x (- paddle-x 6))
                            (set! paddle-x (- paddle-x 4))))
                       ((< delta 0)
                        (set! paddle-x (- paddle-x 1))))))))
      (cond ((= paddle-level 0))
            ((= paddle-level 255))
            (else
             (set! paddle-level (+ paddle-level 1))))))

  ;;; Event

  (define exit-game)

  (define event (make-SDL_Event))

  (define handle-event
    (lambda ()
      (when (> (SDL_PollEvent event) 0)
        (let ((type (SDL_Event-type event)))
          (cond ((= type SDL_MOUSEBUTTONDOWN) (exit-game 0))
                ((= type SDL_KEYDOWN)
                 (let ((key (SDL_keysym-sym (SDL_KeyboardEvent-keysym event))))
                   (case key
                     ((97)  ; a
                      (set! auto-on (not auto-on))
                      (format #t "autoplay: ~a\n" (if auto-on "on" "off")))
                     ((99)  ; c
                      (set! collect-on (not collect-on))
                      (collect-notify collect-on)
                      (format #t "prints gc activity: ~a\n" (if collect-on "on" "off")))
                     ((100) ; d
                      (set! demo-on (not demo-on))
                      (format #t "shows banner: ~a\n" (if demo-on "on" "off")))
                     ((102) ; f
                      (set! fps-on (not fps-on))
                      (format #t "prints fps: ~a\n" (if fps-on "on" "off"))))))
                ((= type SDL_MOUSEMOTION)
                 (let ((x (- (SDL_MouseMotionEvent-x event) (div paddle-width 2))))
                   (cond ((< x 0)
                          (set! paddle-x 0))
                         ((< x (- 640 paddle-width))
                          (set! paddle-x x))
                         (else
                          (set! paddle-x (- 640 paddle-width)))))))))))

  (define game-main
    (lambda ()

      (define update-objects
        (lambda ()
          (update-paddle)
          (update-ball-position)
          (ball-hit-test)
          (update-fragments)
          (update-mini-fragments)
          (update-blocks)))

      (define update-display
        (lambda ()
          (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
          (draw-blocks)
          (draw-opengl-sprite logo-sprite 254 180)
          (draw-paddle)
          (draw-ball)
          (draw-fragments)
          (draw-mini-fragments)
          (SDL_GL_SwapBuffers)))

      (when (> (SDL_Init SDL_INIT_EVERYTHING) 0) (assertion-violation 'SDL_Init (SDL_GetError)))
      (unless (= (Mix_OpenAudio 44100 AUDIO_S16SYS 1 4096) 0)
        (assertion-violation 'Mix_OpenAudio (SDL_GetError)))
      (Mix_AllocateChannels 16)
      (init-opengl-attribute)
      (let ((screen (SDL_SetVideoMode 640 480 32 (+ SDL_SWSURFACE SDL_OPENGL))))
        (when (zero? screen) (assertion-violation 'SDL_SetVideoMode (SDL_GetError)))
        (SDL_WM_SetCaption "JelloBench - Ypsilon/SDL/GL" 0)
        (opengl-enter-2d-mode)
        (init-sprites)
        (init-music-and-sound)
        (set! block-alive 0)
        (init-blocks)
        (and SE-ball-start (Mix_PlayChannelTimed -1 SE-ball-start 0 -1))
        (new-ball 2500000 #t)
        (let loop ()
          (let ((now (microsecond)))
            (and fps-on (show-fps))
            (when (> ball-y dead-y)
              (set! ball-y dead-y)
              (set! ball-x 0)
              (unless (= 0 block-alive)
                (ball-dead)
                (and SE-ball-start (Mix_PlayChannelTimed -1 SE-ball-start 0 -1))
                (new-ball 2500000 #t)))
            (when (and (= 0 block-alive) (> now block-reset-time))
              (init-blocks)
              (and SE-ball-start (Mix_PlayChannelTimed -1 SE-ball-start 0 -1))
              (new-ball 0 #f))
            (when (> now (+ ball-last-move 10000))
              (handle-event)
              (when (> now (+ ball-last-move 20000)) (update-objects))
              (update-objects)
              (set! ball-last-move now))
            (update-display)
            (loop))))))

  (define program-main
    (lambda ()
      (format #t "\nUse mouse to move paddle. Click window to exit program.\n\n")
      (for-each (lambda (a) (format #t "~a[~a]: ~a~%" (car a) (cadr a) (cddr a)))
                `((config-auto-on "a" . ,config-auto-on)
                  (config-demo-on "d" . ,config-demo-on)
                  (config-show-fps-on "f" . ,config-show-fps-on)
                  (config-show-collect-on "c" . ,config-show-collect-on)))
      (newline)
      (collect-notify collect-on)
      (call/cc
       (lambda (cont)
         (set! exit-game cont)
         (with-exception-handler
          (lambda (c)
            (Mix_HaltChannel -1)
            (Mix_HaltMusic)
            (Mix_CloseAudio)
            (SDL_Quit)
            (raise c))
          (lambda () (game-main)))))
      (Mix_HaltChannel -1)
      (Mix_HaltMusic)
      (Mix_CloseAudio)
      (SDL_Quit))))

;;; Start

(import (jello bench))
(program-main)
