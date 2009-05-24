#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon sdl ttf)

  (export TTF_ByteSwappedUNICODE
          TTF_Color
          TTF_CloseFont
          TTF_FontAscent
          TTF_FontDescent
          TTF_FontFaceFamilyName
          TTF_FontFaceIsFixedWidth
          TTF_FontFaceStyleName
          TTF_FontFaces
          TTF_FontHeight
          TTF_FontLineSkip
          TTF_GetError
          TTF_GetFontStyle
          TTF_GlyphMetrics
          TTF_Init
          TTF_Linked_Version
          TTF_OpenFont
          TTF_OpenFontIndex
          TTF_OpenFontIndexRW
          TTF_OpenFontRW
          TTF_Quit
          TTF_RenderGlyph_Blended
          TTF_RenderGlyph_Shaded
          TTF_RenderGlyph_Solid
          TTF_RenderText_Blended
          TTF_RenderText_Shaded
          TTF_RenderText_Solid
          TTF_RenderUNICODE_Blended
          TTF_RenderUNICODE_Shaded
          TTF_RenderUNICODE_Solid
          TTF_RenderUTF8_Blended
          TTF_RenderUTF8_Shaded
          TTF_RenderUTF8_Solid
          TTF_SetError
          TTF_SetFontStyle
          TTF_SizeText
          TTF_SizeUNICODE
          TTF_SizeUTF8
          TTF_WasInit
          TTF_STYLE_BOLD
          TTF_STYLE_ITALIC
          TTF_STYLE_NORMAL
          TTF_STYLE_UNDERLINE
          UNICODE_BOM_NATIVE
          UNICODE_BOM_SWAPPED)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libSDL_ttf.so")
          (on-sunos   "libSDL_ttf.so")
          (on-freebsd "libSDL_ttf.so")
          (on-openbsd "libSDL_ttf.so")
          (on-darwin  "SDL_ttf.framework/SDL_ttf")
          (on-windows "SDL_ttf.dll")
          (else
           (assertion-violation #f "can not locate SDL TTF library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define UNICODE_BOM_NATIVE #xFEFF)
  (define UNICODE_BOM_SWAPPED #xFFFE)
  (define TTF_STYLE_NORMAL #x00)
  (define TTF_STYLE_BOLD #x01)
  (define TTF_STYLE_ITALIC #x02)
  (define TTF_STYLE_UNDERLINE #x04)

  (define TTF_Color
    (cond (on-ppc32
           (lambda (r g b)
             (u8-list->bytevector (list r g b 0))))
          (on-ppc64
           (lambda (r g b)
             (+ (bitwise-arithmetic-shift-left r 56)
                (bitwise-arithmetic-shift-left g 48)
                (bitwise-arithmetic-shift-left r 42))))
          (else
           (lambda (r g b)
             (+ (bitwise-arithmetic-shift-left b 16) (bitwise-arithmetic-shift-left g 8) r)))))

  ;; void TTF_ByteSwappedUNICODE(int swapped) 
  (define-function void TTF_ByteSwappedUNICODE (int))

  ;; void TTF_CloseFont(TTF_Font* font) 
  (define-function void TTF_CloseFont (void*))

  ;; int TTF_FontAscent(TTF_Font* font) 
  (define-function int TTF_FontAscent (void*))

  ;; int TTF_FontDescent(TTF_Font* font) 
  (define-function int TTF_FontDescent (void*))

  ;; char* TTF_FontFaceFamilyName(TTF_Font* font) 
  (define-function void* TTF_FontFaceFamilyName (void*))

  ;; int TTF_FontFaceIsFixedWidth(TTF_Font* font) 
  (define-function int TTF_FontFaceIsFixedWidth (void*))

  ;; char* TTF_FontFaceStyleName(TTF_Font* font) 
  (define-function char* TTF_FontFaceStyleName (void*))

  ;; long TTF_FontFaces(TTF_Font* font) 
  (define-function long TTF_FontFaces (void*))

  ;; int TTF_FontHeight(TTF_Font* font) 
  (define-function int TTF_FontHeight (void*))

  ;; int TTF_FontLineSkip(TTF_Font* font) 
  (define-function int TTF_FontLineSkip (void*))

  ;; char* TTF_GetError() 
  (define-function char* TTF_GetError ())

  ;; int TTF_GetFontStyle(TTF_Font* font) 
  (define-function int TTF_GetFontStyle (void*))

  ;; int TTF_GlyphMetrics(TTF_Font* font, Uint16 ch, int* minx, int* maxx, int* miny, int* maxy, int* advance) 
  (define-function int TTF_GlyphMetrics (void* uint16_t void* void* void* void* void*))

  ;; int TTF_Init()
  (define-function int TTF_Init ())

  ;; const SDL_version* TTF_Linked_Version()
  (define-function void* TTF_Linked_Version ())

  ;; TTF_Font* TTF_OpenFont(const char* file, int ptsize) 
  (define-function void* TTF_OpenFont (char* int))

  ;; TTF_Font* TTF_OpenFontIndex(const char* file, int ptsize, long index) 
  (define-function void* TTF_OpenFontIndex (char* int long))

  ;; TTF_Font* TTF_OpenFontIndexRW(SDL_RWops* src, int freesrc, int ptsize, long index) 
  (define-function void* TTF_OpenFontIndexRW (void* int int long))

  ;; TTF_Font* TTF_OpenFontRW(SDL_RWops* src, int freesrc, int ptsize) 
  (define-function void* TTF_OpenFontRW (void* int int))

  ;; void TTF_Quit() 
  (define-function void TTF_Quit ())

  ;; SDL_Surface* TTF_RenderGlyph_Blended(TTF_Font* font, Uint16 ch, SDL_Color fg) 
  (define-function void* TTF_RenderGlyph_Blended (void* uint16_t void*))

  ;; SDL_Surface* TTF_RenderGlyph_Shaded(TTF_Font* font, Uint16 ch, SDL_Color fg, SDL_Color bg) 
  (define-function void* TTF_RenderGlyph_Shaded (void* uint16_t void* void*))

  ;; SDL_Surface* TTF_RenderGlyph_Solid(TTF_Font* font, Uint16 ch, SDL_Color fg) 
  (define-function void* TTF_RenderGlyph_Solid (void* uint16_t void*))

  ;; SDL_Surface* TTF_RenderText_Blended(TTF_Font* font, const char* text, SDL_Color fg) 
  (define-function void* TTF_RenderText_Blended (void* char* void*))

  ;; SDL_Surface* TTF_RenderText_Shaded(TTF_Font* font, const char* text, SDL_Color fg, SDL_Color bg) 
  (define-function void* TTF_RenderText_Shaded (void* char* void* void*))

  ;; SDL_Surface* TTF_RenderText_Solid(TTF_Font* font, const char* text, SDL_Color fg) 
  (define-function void* TTF_RenderText_Solid (void* char* void*))

  ;; SDL_Surface* TTF_RenderUNICODE_Blended(TTF_Font* font, const Uint16* text, SDL_Color fg) 
  (define-function void* TTF_RenderUNICODE_Blended (void* void* void*))

  ;; SDL_Surface* TTF_RenderUNICODE_Shaded(TTF_Font* font, const Uint16* text, SDL_Color fg, SDL_Color bg) 
  (define-function void* TTF_RenderUNICODE_Shaded (void* void* void* void*))

  ;; SDL_Surface* TTF_RenderUNICODE_Solid(TTF_Font* font, const Uint16* text, SDL_Color fg) 
  (define-function void* TTF_RenderUNICODE_Solid (void* void* void*))

  ;; SDL_Surface* TTF_RenderUTF8_Blended(TTF_Font* font, const char* text, SDL_Color fg) 
  (define-function void* TTF_RenderUTF8_Blended (void* char* void*))

  ;; SDL_Surface* TTF_RenderUTF8_Shaded(TTF_Font* font, const char* text, SDL_Color fg, SDL_Color bg) 
  (define-function void* TTF_RenderUTF8_Shaded (void* char* void* void*))

  ;; SDL_Surface* TTF_RenderUTF8_Solid(TTF_Font* font, const char* text, SDL_Color fg) 
  (define-function void* TTF_RenderUTF8_Solid (void* char* void*))

  ;; void TTF_SetError(const char* fmt, ...) 
  (define-function void TTF_SetError (char* ...))

  ;; void TTF_SetFontStyle(TTF_Font* font, int style) 
  (define-function void TTF_SetFontStyle (void* int))

  ;; int TTF_SizeText(TTF_Font* font, const char* text, int* w, int* h) 
  (define-function int TTF_SizeText (void* char* void* void*))

  ;; int TTF_SizeUNICODE(TTF_Font* font, const Uint16* text, int* w, int* h) 
  (define-function int TTF_SizeUNICODE (void* void* void* void*))

  ;; int TTF_SizeUTF8(TTF_Font* font, const char* text, int* w, int* h) 
  (define-function int TTF_SizeUTF8 (void* char* void* void*))

  ;; int TTF_WasInit() 
  (define-function int TTF_WasInit ())

  ) ;[end]
