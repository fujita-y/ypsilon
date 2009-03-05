#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon sdl types)
  (export SDLKey
          SDLMod
          SDL_Rect
          SDL_PixelFormat
          SDL_Surface
          SDL_keysym
          SDL_ActiveEvent
          SDL_KeyboardEvent
          SDL_MouseMotionEvent
          SDL_MouseButtonEvent
          SDL_JoyAxisEvent
          SDL_JoyBallEvent
          SDL_JoyHatEvent
          SDL_JoyButtonEvent
          SDL_ResizeEvent
          SDL_ExposeEvent
          SDL_QuitEvent
          SDL_UserEvent
          SDL_SysWMEvent
          make-SDL_Event
          SDL_Event-type
          SDL_Event-type-set!
          c-coerce-void*
          define-c-struct-methods)

  (import (rnrs) (ypsilon ffi))

  ;; typdef enum { ... } SDLKey;
  ;;
  (define-c-typedef SDLKey int)

  ;; typdef enum { ... } SDLMod;
  ;;
  (define-c-typedef SDLMod int)

  ;; typedef struct SDL_Rect {
  ;;     Sint16 x, y;
  ;;     Uint16 w, h;
  ;; } SDL_Rect;
  ;;
  (define-c-typedef SDL_Rect
    (struct (int16_t x)
            (int16_t y)
            (uint16_t w)
            (uint16_t h)))

  ;; typedef struct SDL_PixelFormat {
  ;;     SDL_Palette *palette;
  ;;     Uint8 BitsPerPixel;
  ;;     Uint8 BytesPerPixel;
  ;;     Uint8 Rloss;
  ;;     Uint8 Gloss;
  ;;     Uint8 Bloss;
  ;;     Uint8 Aloss;
  ;;     Uint8 Rshift;
  ;;     Uint8 Gshift;
  ;;     Uint8 Bshift;
  ;;     Uint8 Ashift;
  ;;     Uint32 Rmask;
  ;;     Uint32 Gmask;
  ;;     Uint32 Bmask;
  ;;     Uint32 Amask;
  ;;     Uint32 colorkey;
  ;;     Uint8 alpha;
  ;; } SDL_PixelFormat;
  ;;
  (define-c-typedef SDL_PixelFormat
    (struct (void* palette)
            (uint8_t BitsPerPixel)
            (uint8_t BytesPerPixel)
            (uint8_t Rloss)
            (uint8_t Gloss)
            (uint8_t Bloss)
            (uint8_t Aloss)
            (uint8_t Rshift)
            (uint8_t Gshift)
            (uint8_t Bshift)
            (uint8_t Ashift)
            (uint32_t Rmask)
            (uint32_t Gmask)
            (uint32_t Bmask)
            (uint32_t Amask)
            (uint32_t colorkey)
            (uint8_t alpha)))

  ;; typedef struct SDL_Surface {
  ;;     Uint32 flags;
  ;;     SDL_PixelFormat *format;
  ;;     int w, h;
  ;;     Uint16 pitch;
  ;;     void *pixels;
  ;;     int offset;
  ;;     struct private_hwdata *hwdata;
  ;;     SDL_Rect clip_rect;
  ;;     Uint32 unused1;
  ;;     Uint32 locked;
  ;;     struct SDL_BlitMap *map;
  ;;     unsigned int format_version;
  ;;     int refcount;
  ;; } SDL_Surface;
  ;;
  (define-c-typedef SDL_Surface
    (struct (uint32_t flags)
            (void* format)
            (int w)
            (int h)
            (uint16_t pitch)
            (void* pixels)
            (int offset)
            (void* hwdata)
            (SDL_Rect clip_rect)
            (uint32_t unused1)
            (uint32_t locked)
            (void* map)
            (unsigned-int format-version)
            (int refcount)))

  ;; typedef struct SDL_keysym {
  ;;     Uint8 scancode;
  ;;     SDLKey sym;
  ;;     SDLMod mod;
  ;;     Uint16 unicode;
  ;; } SDL_keysym;
  ;;
  (define-c-typedef SDL_keysym
    (struct (uint8_t scancode)
            (SDLKey sym)
            (SDLMod mod)
            (uint16_t unicode)))

  ;; typedef struct SDL_ActiveEvent {
  ;;     Uint8 type;
  ;;     Uint8 gain;
  ;;     Uint8 state;
  ;; } SDL_ActiveEvent;
  ;;
  (define-c-typedef SDL_ActiveEvent
    (struct (uint8_t type)
            (uint8_t gain)
            (uint8_t state)))

  ;; typedef struct SDL_KeyboardEvent {
  ;;     Uint8 type;
  ;;     Uint8 which;
  ;;     Uint8 state;
  ;;     SDL_keysym keysym;
  ;; } SDL_KeyboardEvent;
  ;;
  (define-c-typedef SDL_KeyboardEvent
    (struct (uint8_t type)
            (uint8_t which)
            (uint8_t state)
            (SDL_keysym keysym)))

  ;; typedef struct SDL_MouseMotionEvent {
  ;;     Uint8 type;
  ;;     Uint8 which;
  ;;     Uint8 state;
  ;;     Uint16 x, y;
  ;;     Sint16 xrel;
  ;;     Sint16 yrel;
  ;; } SDL_MouseMotionEvent;
  ;;
  (define-c-typedef SDL_MouseMotionEvent
    (struct (uint8_t type)
            (uint8_t which)
            (uint8_t state)
            (uint16_t x)
            (uint16_t y)
            (int16_t xrel)
            (int16_t yrel)))

  ;; typedef struct SDL_MouseButtonEvent {
  ;;     Uint8 type;
  ;;     Uint8 which;
  ;;     Uint8 button;
  ;;     Uint8 state;
  ;;     Uint16 x, y;
  ;; } SDL_MouseButtonEvent;
  ;;
  (define-c-typedef SDL_MouseButtonEvent
    (struct (uint8_t type)
            (uint8_t which)
            (uint8_t state)
            (uint16_t x)
            (uint16_t y)))

  ;; typedef struct SDL_JoyAxisEvent {
  ;;     Uint8 type;
  ;;     Uint8 which;
  ;;     Uint8 axis;
  ;;     Sint16 value;
  ;; } SDL_JoyAxisEvent;
  ;;
  (define-c-typedef SDL_JoyAxisEvent
    (struct (uint8_t type)
            (uint8_t which)
            (uint8_t axis)
            (int16_t value)))

  ;; typedef struct SDL_JoyBallEvent {
  ;;     Uint8 type;
  ;;     Uint8 which;
  ;;     Uint8 ball;
  ;;     Sint16 xrel;
  ;;     Sint16 yrel;
  ;; } SDL_JoyBallEvent;
  ;;
  (define-c-typedef SDL_JoyBallEvent
    (struct (uint8_t type)
            (uint8_t which)
            (uint8_t ball)
            (int16_t xrel)
            (int16_t yrel)))

  ;; typedef struct SDL_JoyHatEvent {
  ;;     Uint8 type;
  ;;     Uint8 which;
  ;;     Uint8 hat;
  ;;     Uint8 value;
  ;; } SDL_JoyHatEvent;
  ;;
  (define-c-typedef SDL_JoyHatEvent
    (struct (uint8_t type)
            (uint8_t which)
            (uint8_t hat)
            (uint8_t value)))

  ;; typedef struct SDL_JoyButtonEvent {
  ;;     Uint8 type;
  ;;     Uint8 which;
  ;;     Uint8 button;
  ;;     Uint8 state;
  ;; } SDL_JoyButtonEvent;
  ;;
  (define-c-typedef SDL_JoyButtonEvent
    (struct (uint8_t type)
            (uint8_t which)
            (uint8_t button)
            (uint8_t state)))

  ;; typedef struct SDL_ResizeEvent {
  ;;     Uint8 type;
  ;;     int w;
  ;;     int h;
  ;; } SDL_ResizeEvent;
  ;;
  (define-c-typedef SDL_ResizeEvent
    (struct (uint8_t type)
            (int w)
            (int h)))

  ;; typedef struct SDL_ExposeEvent {
  ;;     Uint8 type;
  ;; } SDL_ExposeEvent;
  ;;
  (define-c-typedef SDL_ExposeEvent
    (struct (uint8_t type)))

  ;; typedef struct SDL_QuitEvent {
  ;;     Uint8 type;
  ;; } SDL_QuitEvent;
  ;;
  (define-c-typedef SDL_QuitEvent
    (struct (uint8_t type)))

  ;; typedef struct SDL_UserEvent {
  ;;     Uint8 type;
  ;;     int code;
  ;;     void *data1;
  ;;     void *data2;
  ;; } SDL_UserEvent;
  ;;
  (define-c-typedef SDL_UserEvent
    (struct (uint8_t type)
            (int code)
            (void* data1)
            (void* data2)))

  ;; typedef struct SDL_SysWMEvent {
  ;;     Uint8 type;
  ;;     SDL_SysWMmsg *msg;
  ;; } SDL_SysWMEvent;
  ;;
  (define-c-typedef SDL_SysWMEvent
    (struct (uint8_t type)
            (void* msg)))

  (define sizeof:SDL_Event
    (max (c-sizeof SDL_ActiveEvent)
         (c-sizeof SDL_KeyboardEvent)
         (c-sizeof SDL_MouseMotionEvent)
         (c-sizeof SDL_MouseButtonEvent)
         (c-sizeof SDL_JoyAxisEvent)
         (c-sizeof SDL_JoyBallEvent)
         (c-sizeof SDL_JoyHatEvent)
         (c-sizeof SDL_JoyButtonEvent)
         (c-sizeof SDL_ResizeEvent)
         (c-sizeof SDL_ExposeEvent)
         (c-sizeof SDL_QuitEvent)
         (c-sizeof SDL_UserEvent)
         (c-sizeof SDL_SysWMEvent)))

  (define make-SDL_Event
    (lambda ()
      (make-bytevector sizeof:SDL_Event)))

  (define SDL_Event-type
    (lambda (bv)
      (bytevector-c-uint8-ref bv 0)))

  (define SDL_Event-type-set!
    (lambda (bv u8)
      (bytevector-c-int8-set! bv 0 u8)))

  ) ;[end]
