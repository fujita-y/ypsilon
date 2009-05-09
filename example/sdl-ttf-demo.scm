#!/usr/bin/env ypsilon
#!r6rs

;; sdl-ttf-demo.scm:
;;   SDL/TTF hello world
;;
;; Requirements:
;;   Linux:   libSDL_ttf.so
;;   SunOS:   libSDL_ttf.so
;;   FreeBSD: libSDL_ttf.so
;;   OpenBSD: libSDL_ttf.so
;;   Darwin:  SDL_ttf.framework
;;   Windows: SDL_ttf.dll

(import (rnrs)
        (ypsilon sdl base)
        (ypsilon sdl constants)
        (ypsilon sdl types)
        (ypsilon sdl ttf)
        (ypsilon ffi)
        (ypsilon time))

(define-c-struct-methods SDL_Rect)
(define NULL 0)
(define font-path "./VeraMono.ttf")

(unless (file-exists? font-path)
  (error 'sdl-ttf-demo.scm "this demo program expect './VeraMono.ttf' file in current directory."))

;; init SDL
(when (> (SDL_Init SDL_INIT_EVERYTHING) 0) (assertion-violation 'SDL_Init (SDL_GetError)))
(when (< (TTF_Init) 0) (assertion-violation 'TTF_Init (TTF_GetError)))

;; init video-surface
(define video-surface (SDL_SetVideoMode 320 320 32 SDL_SWSURFACE))
(when (zero? video-surface) (assertion-violation 'SDL_SetVideoMode (SDL_GetError)))
(SDL_WM_SetCaption "SDL/TTF HelloWorld" 0)

;; init font
(define font (TTF_OpenFont font-path 32))
(when (= font NULL) (assertion-violation 'TTF_OpenFont (TTF_GetError)))

;; define draw
(define draw
  (lambda (delta msg)
    (define color (TTF_Color (+ 128 (div delta 4))
                             (+ 128 (div delta 2))
                             (- 255 (div delta 4))))
    (let ((text-surf (TTF_RenderUTF8_Blended font msg color)))
      (let ((w (make-c-int 0)) (h (make-c-int 0)))
        (define rect (make-SDL_Rect))
        (TTF_SizeUTF8 font msg w h)
        (SDL_Rect-x-set! rect (div (- 320 (c-int-ref w)) 2))
        (SDL_Rect-y-set! rect (+ 16 delta))
        (SDL_Rect-w-set! rect 320)
        (SDL_Rect-h-set! rect 320)
        (SDL_FillRect video-surface 0 #xffffffff)
        (SDL_BlitSurface text-surf NULL video-surface rect)
        (SDL_FreeSurface text-surf)
        (SDL_Flip video-surface)))))

(define FRAME-INTERVAL 16000)
(define SLEEP-TIME 100)

(define event-loop
  (lambda ()
    (define event (make-SDL_Event))
    (let loop ((next-update (microsecond)) (delta 0))
      (when (> (SDL_PollEvent event) 0)
        (when (= (SDL_Event-type event) SDL_MOUSEBUTTONDOWN)
          ;; cleanup and exit
          (TTF_Quit)
          (SDL_Quit)
          (exit 0)))
      (cond ((> (microsecond) next-update)
             (draw (if (< delta 256) delta (- 511 delta)) "Hello World")
             (let ((new-delta (+ delta 1)))
               (loop (+ next-update FRAME-INTERVAL)
                     (if (> new-delta 511) 0 new-delta))))
            (else
             (usleep SLEEP-TIME)
             (loop next-update delta))))))

(event-loop)

