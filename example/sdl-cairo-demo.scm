#!/usr/bin/env ypsilon
#!r6rs
   
;; sdl-cairo-demo.scm:
;;   SDL/Cairo hello world
;;
;; Requirements:
;;   Darwin:  SDL.framework Cairo.framework
;;   Linux:   libSDL.so libcairo.so.2
;;   FreeBSD: libSDL.so libcairo.so
;;   OpenBSD: libSDL.so libcairo.so
;;   Windows: SDL.dll libcairo-2.dll
   
(import (rnrs) 
        (ypsilon sdl base)
        (ypsilon sdl constants)
        (ypsilon sdl types)
        (ypsilon cairo) 
        (ypsilon time))

;; init SDL
(when (> (SDL_Init SDL_INIT_EVERYTHING) 0) (assertion-violation 'SDL_Init (SDL_GetError)))

;; init video-surface
(define video-surface (SDL_SetVideoMode 320 320 32 SDL_SWSURFACE))
(when (zero? video-surface) (assertion-violation 'SDL_SetVideoMode (SDL_GetError)))
(SDL_WM_SetCaption "SDL/Cairo HelloWorld" 0)

;; init cairo-surface
(define cairo-surface
  (let ((obj (c-coerce-void* video-surface SDL_Surface)))
    (define-c-struct-methods SDL_Surface)
    (cairo_image_surface_create_for_data (SDL_Surface-pixels obj)
                                         CAIRO_FORMAT_ARGB32
                                         (SDL_Surface-w obj)
                                         (SDL_Surface-h obj)
                                         (SDL_Surface-pitch obj))))

;; init cairo-context (cr)
(define cairo-context (cairo_create cairo-surface))

;; define draw
(define draw
  (lambda (delta)
    (let ((zero-to-one (sqrt (/ (+ (abs delta) 1.0) 2.0))))
      (SDL_FillRect video-surface 0 #xffffffff)
      (cairo_move_to cairo-context 120.0 80.0)
      (cairo_rotate cairo-context delta)
      (cairo_set_source_rgba cairo-context 0.7 zero-to-one 1.0 zero-to-one)
      (cairo_select_font_face cairo-context "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
      (cairo_set_font_size cairo-context 32.0)
      (cairo_show_text cairo-context "Hello World")
      (cairo_surface_flush cairo-surface)
      (SDL_Flip video-surface))))

(define FRAME-INTERVAL 16000)
(define SLEEP-TIME 1000)
(define STEP (/ FRAME-INTERVAL 3200000.0)) ;; 3200000.0

(define event-loop
  (lambda ()
    (define event (make-SDL_Event))
    (let loop ((next-update (microsecond)) (delta 0.0))
      (when (> (SDL_PollEvent event) 0)
        (when (= (SDL_Event-type event) SDL_MOUSEBUTTONDOWN)
          ;; cleanup and exit
          (cairo_destroy cairo-context)
          (cairo_surface_destroy cairo-surface)
          (SDL_Quit)
          (exit 0)))
      (cond ((> (microsecond) next-update)
             (draw (- delta 1.0))
             (let ((new-delta (+ delta STEP)))
               (loop (+ next-update FRAME-INTERVAL)
                     (if (> new-delta 2.0) 0.0 new-delta))))
            (else
             (usleep SLEEP-TIME)
             (loop next-update delta))))))

(event-loop)

