#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon sdl image)
  (export IMG_InvertAlpha
          IMG_Linked_Version
          IMG_Load
          IMG_LoadBMP_RW
          IMG_LoadGIF_RW
          IMG_LoadJPG_RW
          IMG_LoadLBM_RW
          IMG_LoadPCX_RW
          IMG_LoadPNG_RW
          IMG_LoadPNM_RW
          IMG_LoadTGA_RW
          IMG_LoadTIF_RW
          IMG_LoadTyped_RW
          IMG_LoadXCF_RW
          IMG_LoadXPM_RW
          IMG_LoadXV_RW
          IMG_Load_RW
          IMG_ReadXPMFromArray
          IMG_isBMP
          IMG_isGIF
          IMG_isJPG
          IMG_isLBM
          IMG_isPCX
          IMG_isPNG
          IMG_isPNM
          IMG_isTIF
          IMG_isXCF
          IMG_isXPM
          IMG_isXV)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libSDL_image.so")
          (on-sunos   "libSDL_image.so")
          (on-freebsd "libSDL_image.so")
          (on-openbsd "libSDL_image.so")
          (on-darwin  "SDL_image.framework/SDL_image")
          (on-windows "SDL_image.dll")
          (else
           (assertion-violation #f "can not locate SDL Image library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; int IMG_InvertAlpha(int on)
  (define-function int IMG_InvertAlpha (int))

  ;; const SDL_version* IMG_Linked_Version(void)
  (define-function void* IMG_Linked_Version ())

  ;; SDL_Surface* IMG_Load(const char* file)
  (define-function void* IMG_Load (char*))

  ;; SDL_Surface* IMG_LoadBMP_RW(SDL_RWops* src)
  (define-function void* IMG_LoadBMP_RW (void*))

  ;; SDL_Surface* IMG_LoadGIF_RW(SDL_RWops* src)
  (define-function void* IMG_LoadGIF_RW (void*))

  ;; SDL_Surface* IMG_LoadJPG_RW(SDL_RWops* src)
  (define-function void* IMG_LoadJPG_RW (void*))

  ;; SDL_Surface* IMG_LoadLBM_RW(SDL_RWops* src)
  (define-function void* IMG_LoadLBM_RW (void*))

  ;; SDL_Surface* IMG_LoadPCX_RW(SDL_RWops* src)
  (define-function void* IMG_LoadPCX_RW (void*))

  ;; SDL_Surface* IMG_LoadPNG_RW(SDL_RWops* src)
  (define-function void* IMG_LoadPNG_RW (void*))

  ;; SDL_Surface* IMG_LoadPNM_RW(SDL_RWops* src)
  (define-function void* IMG_LoadPNM_RW (void*))

  ;; SDL_Surface* IMG_LoadTGA_RW(SDL_RWops* src)
  (define-function void* IMG_LoadTGA_RW (void*))

  ;; SDL_Surface* IMG_LoadTIF_RW(SDL_RWops* src)
  (define-function void* IMG_LoadTIF_RW (void*))

  ;; SDL_Surface* IMG_LoadTyped_RW(SDL_RWops* src, int freesrc, char* type)
  (define-function void* IMG_LoadTyped_RW (void* int char*))

  ;; SDL_Surface* IMG_LoadXCF_RW(SDL_RWops* src)
  (define-function void* IMG_LoadXCF_RW (void*))

  ;; SDL_Surface* IMG_LoadXPM_RW(SDL_RWops* src)
  (define-function void* IMG_LoadXPM_RW (void*))

  ;; SDL_Surface* IMG_LoadXV_RW(SDL_RWops* src)
  (define-function void* IMG_LoadXV_RW (void*))

  ;; SDL_Surface* IMG_Load_RW(SDL_RWops* src, int freesrc)
  (define-function void* IMG_Load_RW (void* int))

  ;; SDL_Surface* IMG_ReadXPMFromArray(char** xpm)
  (define-function void* IMG_ReadXPMFromArray (void*))

  ;; int IMG_isBMP(SDL_RWops* src)
  (define-function int IMG_isBMP (void*))

  ;; int IMG_isGIF(SDL_RWops* src)
  (define-function int IMG_isGIF (void*))

  ;; int IMG_isJPG(SDL_RWops* src)
  (define-function int IMG_isJPG (void*))

  ;; int IMG_isLBM(SDL_RWops* src)
  (define-function int IMG_isLBM (void*))

  ;; int IMG_isPCX(SDL_RWops* src)
  (define-function int IMG_isPCX (void*))

  ;; int IMG_isPNG(SDL_RWops* src)
  (define-function int IMG_isPNG (void*))

  ;; int IMG_isPNM(SDL_RWops* src)
  (define-function int IMG_isPNM (void*))

  ;; int IMG_isTIF(SDL_RWops* src)
  (define-function int IMG_isTIF (void*))

  ;; int IMG_isXCF(SDL_RWops* src)
  (define-function int IMG_isXCF (void*))

  ;; int IMG_isXPM(SDL_RWops* src)
  (define-function int IMG_isXPM (void*))

  ;; int IMG_isXV(SDL_RWops* src)
  (define-function int IMG_isXV (void*))

  ) ;[end]
