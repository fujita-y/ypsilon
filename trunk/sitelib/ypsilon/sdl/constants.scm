#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon sdl constants)
  (export SDL_INIT_AUDIO
          SDL_INIT_VIDEO
          SDL_INIT_CDROM
          SDL_INIT_TIMER
          SDL_INIT_JOYSTICK
          SDL_INIT_NOPARACHUTE
          SDL_INIT_EVENTTHREAD
          SDL_INIT_EVERYTHING

          SDL_SWSURFACE
          SDL_HWSURFACE
          SDL_ASYNCBLIT
          SDL_ANYFORMAT
          SDL_HWPALETTE
          SDL_DOUBLEBUF
          SDL_FULLSCREEN
          SDL_OPENGL
          SDL_OPENGLBLIT
          SDL_RESIZABLE
          SDL_NOFRAME
          SDL_HWACCEL
          SDL_SRCCOLORKEY
          SDL_RLEACCELOK
          SDL_RLEACCEL
          SDL_SRCALPHA
          SDL_PREALLOC

          SDL_FALSE
          SDL_TRUE

          SDL_ENOMEM
          SDL_EFREAD
          SDL_EFWRITE
          SDL_EFSEEK
          SDL_UNSUPPORTED
          SDL_LASTERROR

          SDLK_UNKNOWN
          SDLK_FIRST
          SDLK_BACKSPACE
          SDLK_TAB
          SDLK_CLEAR
          SDLK_RETURN
          SDLK_PAUSE
          SDLK_ESCAPE
          SDLK_SPACE
          SDLK_EXCLAIM
          SDLK_QUOTEDBL
          SDLK_HASH
          SDLK_DOLLAR
          SDLK_AMPERSAND
          SDLK_QUOTE
          SDLK_LEFTPAREN
          SDLK_RIGHTPAREN
          SDLK_ASTERISK
          SDLK_PLUS
          SDLK_COMMA
          SDLK_MINUS
          SDLK_PERIOD
          SDLK_SLASH
          SDLK_0
          SDLK_1
          SDLK_2
          SDLK_3
          SDLK_4
          SDLK_5
          SDLK_6
          SDLK_7
          SDLK_8
          SDLK_9
          SDLK_COLON
          SDLK_SEMICOLON
          SDLK_LESS
          SDLK_EQUALS
          SDLK_GREATER
          SDLK_QUESTION
          SDLK_AT
          SDLK_LEFTBRACKET
          SDLK_BACKSLASH
          SDLK_RIGHTBRACKET
          SDLK_CARET
          SDLK_UNDERSCORE
          SDLK_BACKQUOTE
          SDLK_a
          SDLK_b
          SDLK_c
          SDLK_d
          SDLK_e
          SDLK_f
          SDLK_g
          SDLK_h
          SDLK_i
          SDLK_j
          SDLK_k
          SDLK_l
          SDLK_m
          SDLK_n
          SDLK_o
          SDLK_p
          SDLK_q
          SDLK_r
          SDLK_s
          SDLK_t
          SDLK_u
          SDLK_v
          SDLK_w
          SDLK_x
          SDLK_y
          SDLK_z
          SDLK_DELETE
          SDLK_WORLD_0
          SDLK_WORLD_1
          SDLK_WORLD_2
          SDLK_WORLD_3
          SDLK_WORLD_4
          SDLK_WORLD_5
          SDLK_WORLD_6
          SDLK_WORLD_7
          SDLK_WORLD_8
          SDLK_WORLD_9
          SDLK_WORLD_10
          SDLK_WORLD_11
          SDLK_WORLD_12
          SDLK_WORLD_13
          SDLK_WORLD_14
          SDLK_WORLD_15
          SDLK_WORLD_16
          SDLK_WORLD_17
          SDLK_WORLD_18
          SDLK_WORLD_19
          SDLK_WORLD_20
          SDLK_WORLD_21
          SDLK_WORLD_22
          SDLK_WORLD_23
          SDLK_WORLD_24
          SDLK_WORLD_25
          SDLK_WORLD_26
          SDLK_WORLD_27
          SDLK_WORLD_28
          SDLK_WORLD_29
          SDLK_WORLD_30
          SDLK_WORLD_31
          SDLK_WORLD_32
          SDLK_WORLD_33
          SDLK_WORLD_34
          SDLK_WORLD_35
          SDLK_WORLD_36
          SDLK_WORLD_37
          SDLK_WORLD_38
          SDLK_WORLD_39
          SDLK_WORLD_40
          SDLK_WORLD_41
          SDLK_WORLD_42
          SDLK_WORLD_43
          SDLK_WORLD_44
          SDLK_WORLD_45
          SDLK_WORLD_46
          SDLK_WORLD_47
          SDLK_WORLD_48
          SDLK_WORLD_49
          SDLK_WORLD_50
          SDLK_WORLD_51
          SDLK_WORLD_52
          SDLK_WORLD_53
          SDLK_WORLD_54
          SDLK_WORLD_55
          SDLK_WORLD_56
          SDLK_WORLD_57
          SDLK_WORLD_58
          SDLK_WORLD_59
          SDLK_WORLD_60
          SDLK_WORLD_61
          SDLK_WORLD_62
          SDLK_WORLD_63
          SDLK_WORLD_64
          SDLK_WORLD_65
          SDLK_WORLD_66
          SDLK_WORLD_67
          SDLK_WORLD_68
          SDLK_WORLD_69
          SDLK_WORLD_70
          SDLK_WORLD_71
          SDLK_WORLD_72
          SDLK_WORLD_73
          SDLK_WORLD_74
          SDLK_WORLD_75
          SDLK_WORLD_76
          SDLK_WORLD_77
          SDLK_WORLD_78
          SDLK_WORLD_79
          SDLK_WORLD_80
          SDLK_WORLD_81
          SDLK_WORLD_82
          SDLK_WORLD_83
          SDLK_WORLD_84
          SDLK_WORLD_85
          SDLK_WORLD_86
          SDLK_WORLD_87
          SDLK_WORLD_88
          SDLK_WORLD_89
          SDLK_WORLD_90
          SDLK_WORLD_91
          SDLK_WORLD_92
          SDLK_WORLD_93
          SDLK_WORLD_94
          SDLK_WORLD_95
          SDLK_KP0
          SDLK_KP1
          SDLK_KP2
          SDLK_KP3
          SDLK_KP4
          SDLK_KP5
          SDLK_KP6
          SDLK_KP7
          SDLK_KP8
          SDLK_KP9
          SDLK_KP_PERIOD
          SDLK_KP_DIVIDE
          SDLK_KP_MULTIPLY
          SDLK_KP_MINUS
          SDLK_KP_PLUS
          SDLK_KP_ENTER
          SDLK_KP_EQUALS
          SDLK_UP
          SDLK_DOWN
          SDLK_RIGHT
          SDLK_LEFT
          SDLK_INSERT
          SDLK_HOME
          SDLK_END
          SDLK_PAGEUP
          SDLK_PAGEDOWN
          SDLK_F1
          SDLK_F2
          SDLK_F3
          SDLK_F4
          SDLK_F5
          SDLK_F6
          SDLK_F7
          SDLK_F8
          SDLK_F9
          SDLK_F10
          SDLK_F11
          SDLK_F12
          SDLK_F13
          SDLK_F14
          SDLK_F15
          SDLK_NUMLOCK
          SDLK_CAPSLOCK
          SDLK_SCROLLOCK
          SDLK_RSHIFT
          SDLK_LSHIFT
          SDLK_RCTRL
          SDLK_LCTRL
          SDLK_RALT
          SDLK_LALT
          SDLK_RMETA
          SDLK_LMETA
          SDLK_LSUPER
          SDLK_RSUPER
          SDLK_MODE
          SDLK_COMPOSE
          SDLK_HELP
          SDLK_PRINT
          SDLK_SYSREQ
          SDLK_BREAK
          SDLK_MENU
          SDLK_POWER
          SDLK_EURO
          SDLK_UNDO
          SDLK_LAST

          SDL_GL_RED_SIZE
          SDL_GL_GREEN_SIZE
          SDL_GL_BLUE_SIZE
          SDL_GL_ALPHA_SIZE
          SDL_GL_BUFFER_SIZE
          SDL_GL_DOUBLEBUFFER
          SDL_GL_DEPTH_SIZE
          SDL_GL_STENCIL_SIZE
          SDL_GL_ACCUM_RED_SIZE
          SDL_GL_ACCUM_GREEN_SIZE
          SDL_GL_ACCUM_BLUE_SIZE
          SDL_GL_ACCUM_ALPHA_SIZE
          SDL_GL_STEREO
          SDL_GL_MULTISAMPLEBUFFERS
          SDL_GL_MULTISAMPLESAMPLES
          SDL_GL_ACCELERATED_VISUAL
          SDL_GL_SWAP_CONTROL

          SDL_GRAB_QUERY
          SDL_GRAB_OFF
          SDL_GRAB_ON
          SDL_GRAB_FULLSCREEN

          SDL_NOEVENT
          SDL_ACTIVEEVENT
          SDL_KEYDOWN
          SDL_KEYUP
          SDL_MOUSEMOTION
          SDL_MOUSEBUTTONDOWN
          SDL_MOUSEBUTTONUP
          SDL_JOYAXISMOTION
          SDL_JOYBALLMOTION
          SDL_JOYHATMOTION
          SDL_JOYBUTTONDOWN
          SDL_JOYBUTTONUP
          SDL_QUIT
          SDL_SYSWMEVENT
          SDL_EVENT_RESERVEDA
          SDL_EVENT_RESERVEDB
          SDL_VIDEORESIZE
          SDL_VIDEOEXPOSE
          SDL_EVENT_RESERVED2
          SDL_EVENT_RESERVED3
          SDL_EVENT_RESERVED4
          SDL_EVENT_RESERVED5
          SDL_EVENT_RESERVED6
          SDL_EVENT_RESERVED7
          SDL_USEREVENT
          SDL_NUMEVENTS

          SDL_ACTIVEEVENTMASK
          SDL_KEYDOWNMASK
          SDL_KEYUPMASK
          SDL_KEYEVENTMASK
          SDL_MOUSEMOTIONMASK
          SDL_MOUSEBUTTONDOWNMASK
          SDL_MOUSEBUTTONUPMASK
          SDL_MOUSEEVENTMASK
          SDL_JOYAXISMOTIONMASK
          SDL_JOYBALLMOTIONMASK
          SDL_JOYHATMOTIONMASK
          SDL_JOYBUTTONDOWNMASK
          SDL_JOYBUTTONUPMASK
          SDL_JOYEVENTMASK
          SDL_VIDEORESIZEMASK
          SDL_VIDEOEXPOSEMASK
          SDL_QUITMASK
          SDL_SYSWMEVENTMASK

          SDL_ADDEVENT
          SDL_PEEKEVENT
          SDL_GETEVENT

          AUDIO_U8
          AUDIO_S8
          AUDIO_U16LSB
          AUDIO_S16LSB
          AUDIO_U16MSB
          AUDIO_S16MSB
          AUDIO_U16
          AUDIO_S16
          AUDIO_U16SYS
          AUDIO_S16SYS

          SDL_AUDIO_STOPPED
          SDL_AUDIO_PLAYING
          SDL_AUDIO_PAUSED

          CD_TRAYEMPTY
          CD_STOPPED
          CD_PLAYING
          CD_PAUSED
          CD_ERROR

          MIX_NO_FADING
          MIX_FADING_OUT
          MIX_FADING_IN

          MUS_NONE
          MUS_CMD
          MUS_WAV
          MUS_MOD
          MUS_MID
          MUS_OGG
          MUS_MP3
          MUS_MP3_MAD

          SDL_APPMOUSEFOCUS
          SDL_APPINPUTFOCUS
          SDL_APPACTIVE

          SDL_RELEASED
          SDL_PRESSED

          SDL_BUTTON_LEFT
          SDL_BUTTON_MIDDLE
          SDL_BUTTON_RIGHT
          SDL_BUTTON_WHEELUP
          SDL_BUTTON_WHEELDOWN
          SDL_BUTTON_X1
          SDL_BUTTON_X2

          SDL_BUTTON_LMASK
          SDL_BUTTON_MMASK
          SDL_BUTTON_RMASK
          SDL_BUTTON_X1MASK
          SDL_BUTTON_X2MASK

          SDL_HAT_CENTERED
          SDL_HAT_UP
          SDL_HAT_RIGHT
          SDL_HAT_DOWN
          SDL_HAT_LEFT

          SDL_HAT_RIGHTUP
          SDL_HAT_RIGHTDOWN
          SDL_HAT_LEFTUP
          SDL_HAT_LEFTDOWN

          KMOD_CTRL
          KMOD_SHIFT
          KMOD_ALT
          KMOD_META)

  (import (core) (rnrs) (ypsilon c-types))

  (define SDL_INIT_TIMER       #x00000001)
  (define SDL_INIT_AUDIO       #x00000010)
  (define SDL_INIT_VIDEO       #x00000020)
  (define SDL_INIT_CDROM       #x00000100)
  (define SDL_INIT_JOYSTICK    #x00000200)
  (define SDL_INIT_NOPARACHUTE #x00100000)
  (define SDL_INIT_EVENTTHREAD #x01000000)
  (define SDL_INIT_EVERYTHING  #x0000FFFF)

  (define SDL_SWSURFACE   #x00000000)
  (define SDL_HWSURFACE   #x00000001)
  (define SDL_ASYNCBLIT   #x00000004)
  (define SDL_ANYFORMAT   #x10000000)
  (define SDL_HWPALETTE   #x20000000)
  (define SDL_DOUBLEBUF   #x40000000)
  (define SDL_FULLSCREEN  #x80000000)
  (define SDL_OPENGL      #x00000002)
  (define SDL_OPENGLBLIT  #x0000000A)
  (define SDL_RESIZABLE   #x00000010)
  (define SDL_NOFRAME     #x00000020)
  (define SDL_HWACCEL     #x00000100)
  (define SDL_SRCCOLORKEY #x00001000)
  (define SDL_RLEACCELOK  #x00002000)
  (define SDL_RLEACCEL    #x00004000)
  (define SDL_SRCALPHA    #x00010000)
  (define SDL_PREALLOC    #x01000000)

  ;; enum SDL_bool
  (define-c-enum SDL_FALSE SDL_TRUE)

  ;; enum SDL_errorcode
  (define-c-enum SDL_ENOMEM SDL_EFREAD SDL_EFWRITE SDL_EFSEEK SDL_UNSUPPORTED SDL_LASTERROR)

  ;; enum SDLKey
  (define-c-enum (SDLK_UNKNOWN . 0)
                 (SDLK_FIRST . 0)
                 (SDLK_BACKSPACE . 8)
                 (SDLK_TAB . 9)
                 (SDLK_CLEAR . 12)
                 (SDLK_RETURN . 13)
                 (SDLK_PAUSE . 19)
                 (SDLK_ESCAPE . 27)
                 (SDLK_SPACE . 32)
                 SDLK_EXCLAIM SDLK_QUOTEDBL SDLK_HASH SDLK_DOLLAR
                 (SDLK_AMPERSAND . 38)
                 SDLK_QUOTE SDLK_LEFTPAREN SDLK_RIGHTPAREN SDLK_ASTERISK SDLK_PLUS SDLK_COMMA SDLK_MINUS SDLK_PERIOD SDLK_SLASH
                 SDLK_0 SDLK_1 SDLK_2 SDLK_3 SDLK_4 SDLK_5 SDLK_6 SDLK_7 SDLK_8 SDLK_9
                 SDLK_COLON SDLK_SEMICOLON SDLK_LESS SDLK_EQUALS SDLK_GREATER SDLK_QUESTION SDLK_AT
                 (SDLK_LEFTBRACKET . 91)
                 SDLK_BACKSLASH SDLK_RIGHTBRACKET SDLK_CARET SDLK_UNDERSCORE SDLK_BACKQUOTE
                 (SDLK_DELETE . 127))

  ;; SDLK_a ... SDLK_z
  (define-syntax def-key-alphabet
    (lambda (x)
      (syntax-case x ()
        ((stx from to)
         (let ((from (char->integer (syntax->datum #'from))) (to (char->integer (syntax->datum #'to))))
           (let loop ((n from) (def '()))
             (cond ((<= n to)
                    (loop (+ n 1) (cons `(define ,(string->symbol (format "SDLK_~a" (integer->char n))) ,n) def)))
                   (else
                    (datum->syntax #'stx (cons 'begin (reverse def)))))))))))
  (def-key-alphabet #\a #\z)

  ;; SDLK_WORLD_0 ... SDLK_WORLD_95
  (define-syntax def-key-world
    (lambda (x)
      (syntax-case x ()
        ((stx from to)
         (let ((from (syntax->datum #'from)) (to (syntax->datum #'to)))
           (let loop ((n from) (def '()))
             (cond ((<= n to)
                    (loop (+ n 1) (cons `(define ,(string->symbol (format "SDLK_WORLD_~a" (- n from))) ,n) def)))
                   (else
                    (datum->syntax #'stx (cons 'begin (reverse def)))))))))))
  (def-key-world 160 255)

  (define-c-enum (SDLK_KP0 . 256)
                 SDLK_KP1 SDLK_KP2 SDLK_KP3 SDLK_KP4 SDLK_KP5 SDLK_KP6 SDLK_KP7 SDLK_KP8 SDLK_KP9
                 SDLK_KP_PERIOD SDLK_KP_DIVIDE SDLK_KP_MULTIPLY SDLK_KP_MINUS SDLK_KP_PLUS
                 SDLK_KP_ENTER SDLK_KP_EQUALS SDLK_UP SDLK_DOWN SDLK_RIGHT SDLK_LEFT SDLK_INSERT SDLK_HOME SDLK_END SDLK_PAGEUP SDLK_PAGEDOWN
                 SDLK_F1 SDLK_F2 SDLK_F3 SDLK_F4 SDLK_F5 SDLK_F6 SDLK_F7 SDLK_F8 SDLK_F9 SDLK_F10 SDLK_F11 SDLK_F12 SDLK_F13 SDLK_F14
                 SDLK_F15)

  (define-c-enum (SDLK_NUMLOCK . 300)
                 SDLK_CAPSLOCK SDLK_SCROLLOCK SDLK_RSHIFT SDLK_LSHIFT SDLK_RCTRL SDLK_LCTRL SDLK_RALT SDLK_LALT SDLK_RMETA SDLK_LMETA
                 SDLK_LSUPER SDLK_RSUPER SDLK_MODE SDLK_COMPOSE SDLK_HELP SDLK_PRINT SDLK_SYSREQ SDLK_BREAK SDLK_MENU SDLK_POWER SDLK_EURO SDLK_UNDO
                 SDLK_LAST)

  ;; enum SDLMod
  (define-c-enum (KMOD_NONE . #x0000)
                 (KMOD_LSHIFT . #x0001)
                 (KMOD_RSHIFT . #x0002)
                 (KMOD_LCTRL . #x0040)
                 (KMOD_RCTRL . #x0080)
                 (KMOD_LALT . #x0100)
                 (KMOD_RALT . #x0200)
                 (KMOD_LMETA . #x0400)
                 (KMOD_RMETA . #x0800)
                 (KMOD_NUM . #x1000)
                 (KMOD_CAPS . #x2000)
                 (KMOD_MODE . #x4000)
                 (KMOD_RESERVED . #x8000))

  ;; enum SDL_GLattr
  (define-c-enum SDL_GL_RED_SIZE
                 SDL_GL_GREEN_SIZE
                 SDL_GL_BLUE_SIZE
                 SDL_GL_ALPHA_SIZE
                 SDL_GL_BUFFER_SIZE
                 SDL_GL_DOUBLEBUFFER
                 SDL_GL_DEPTH_SIZE
                 SDL_GL_STENCIL_SIZE
                 SDL_GL_ACCUM_RED_SIZE
                 SDL_GL_ACCUM_GREEN_SIZE
                 SDL_GL_ACCUM_BLUE_SIZE
                 SDL_GL_ACCUM_ALPHA_SIZE
                 SDL_GL_STEREO
                 SDL_GL_MULTISAMPLEBUFFERS
                 SDL_GL_MULTISAMPLESAMPLES
                 SDL_GL_ACCELERATED_VISUAL
                 SDL_GL_SWAP_CONTROL)

  ;; enum SDL_GrabMode
  (define-c-enum (SDL_GRAB_QUERY . -1) SDL_GRAB_OFF SDL_GRAB_ON SDL_GRAB_FULLSCREEN)

  ;; enum SDL_Events
  (define-c-enum SDL_NOEVENT
                 SDL_ACTIVEEVENT
                 SDL_KEYDOWN
                 SDL_KEYUP
                 SDL_MOUSEMOTION
                 SDL_MOUSEBUTTONDOWN
                 SDL_MOUSEBUTTONUP
                 SDL_JOYAXISMOTION
                 SDL_JOYBALLMOTION
                 SDL_JOYHATMOTION
                 SDL_JOYBUTTONDOWN
                 SDL_JOYBUTTONUP
                 SDL_QUIT
                 SDL_SYSWMEVENT
                 SDL_EVENT_RESERVEDA
                 SDL_EVENT_RESERVEDB
                 SDL_VIDEORESIZE
                 SDL_VIDEOEXPOSE
                 SDL_EVENT_RESERVED2
                 SDL_EVENT_RESERVED3
                 SDL_EVENT_RESERVED4
                 SDL_EVENT_RESERVED5
                 SDL_EVENT_RESERVED6
                 SDL_EVENT_RESERVED7
                 (SDL_USEREVENT . 24)
                 (SDL_NUMEVENTS . 32))

  ;; enum SDL_EventMasks
  (define SDL_ACTIVEEVENTMASK (bitwise-arithmetic-shift-left 1 SDL_ACTIVEEVENT))
  (define SDL_KEYDOWNMASK (bitwise-arithmetic-shift-left 1 SDL_KEYDOWN))
  (define SDL_KEYUPMASK (bitwise-arithmetic-shift-left 1 SDL_KEYUP))
  (define SDL_KEYEVENTMASK (bitwise-ior SDL_KEYDOWNMASK SDL_KEYUPMASK))
  (define SDL_MOUSEMOTIONMASK (bitwise-arithmetic-shift-left 1 SDL_MOUSEMOTION))
  (define SDL_MOUSEBUTTONDOWNMASK (bitwise-arithmetic-shift-left 1 SDL_MOUSEBUTTONDOWN))
  (define SDL_MOUSEBUTTONUPMASK (bitwise-arithmetic-shift-left 1 SDL_MOUSEBUTTONUP))
  (define SDL_MOUSEEVENTMASK (bitwise-ior SDL_MOUSEMOTIONMASK SDL_MOUSEBUTTONDOWNMASK SDL_MOUSEBUTTONUPMASK))
  (define SDL_JOYAXISMOTIONMASK (bitwise-arithmetic-shift-left 1 SDL_JOYAXISMOTION))
  (define SDL_JOYBALLMOTIONMASK (bitwise-arithmetic-shift-left 1 SDL_JOYBALLMOTION))
  (define SDL_JOYHATMOTIONMASK (bitwise-arithmetic-shift-left 1 SDL_JOYHATMOTION))
  (define SDL_JOYBUTTONDOWNMASK (bitwise-arithmetic-shift-left 1 SDL_JOYBUTTONDOWN))
  (define SDL_JOYBUTTONUPMASK (bitwise-arithmetic-shift-left 1 SDL_JOYBUTTONUP))
  (define SDL_JOYEVENTMASK (bitwise-ior SDL_JOYAXISMOTIONMASK SDL_JOYBALLMOTIONMASK SDL_JOYHATMOTIONMASK SDL_JOYBUTTONDOWNMASK SDL_JOYBUTTONUPMASK))
  (define SDL_VIDEORESIZEMASK (bitwise-arithmetic-shift-left 1 SDL_VIDEORESIZE))
  (define SDL_VIDEOEXPOSEMASK (bitwise-arithmetic-shift-left 1 SDL_VIDEOEXPOSE))
  (define SDL_QUITMASK (bitwise-arithmetic-shift-left 1 SDL_QUIT))
  (define SDL_SYSWMEVENTMASK (bitwise-arithmetic-shift-left 1 SDL_SYSWMEVENT))

  ;; enum SDL_eventaction
  (define-c-enum SDL_ADDEVENT SDL_PEEKEVENT SDL_GETEVENT)

  (define AUDIO_U8     #x0008)
  (define AUDIO_S8     #x8008)
  (define AUDIO_U16LSB #x0010)
  (define AUDIO_S16LSB #x8010)
  (define AUDIO_U16MSB #x1010)
  (define AUDIO_S16MSB #x9010)
  (define AUDIO_U16    AUDIO_U16LSB)
  (define AUDIO_S16    AUDIO_S16LSB)
  (define AUDIO_U16SYS (if (eq? (native-endianness) (endianness little)) AUDIO_U16LSB AUDIO_U16MSB))
  (define AUDIO_S16SYS (if (eq? (native-endianness) (endianness little)) AUDIO_S16LSB AUDIO_S16MSB))

  ;; enum SDL_audiostatus
  (define-c-enum SDL_AUDIO_STOPPED SDL_AUDIO_PLAYING SDL_AUDIO_PAUSED)

  ;; enum CDstatus
  (define-c-enum CD_TRAYEMPTY CD_STOPPED CD_PLAYING CD_PAUSED (CD_ERROR . -1))

  ;; enum Mix_Fading
  (define-c-enum MIX_NO_FADING MIX_FADING_OUT MIX_FADING_IN)

  ;; enum Mix_MusicType
  (define-c-enum MUS_NONE MUS_CMD MUS_WAV MUS_MOD MUS_MID MUS_OGG MUS_MP3 MUS_MP3_MAD)

  ;; struct SDL_ActiveEvent.state
  (define SDL_APPMOUSEFOCUS     1)
  (define SDL_APPINPUTFOCUS     2)
  (define SDL_APPACTIVE         4)

  ;; struct SDL_{Keyboard, MouseButton, JoyButton}Event.state
  (define SDL_RELEASED    0)
  (define SDL_PRESSED     1)

  ;; struct SDL_MouseButtonEvent.button
  (define SDL_BUTTON_LEFT       1)
  (define SDL_BUTTON_MIDDLE     2)
  (define SDL_BUTTON_RIGHT      3)
  (define SDL_BUTTON_WHEELUP    4)
  (define SDL_BUTTON_WHEELDOWN  5)
  (define SDL_BUTTON_X1         6)
  (define SDL_BUTTON_X2         7)

  ;; struct SDL_MouseMotionEvent.state masks
  (define SDL_BUTTON_LMASK      (bitwise-arithmetic-shift-left 1 (- SDL_BUTTON_LEFT 1)))
  (define SDL_BUTTON_MMASK      (bitwise-arithmetic-shift-left 1 (- SDL_BUTTON_MIDDLE 1)))
  (define SDL_BUTTON_RMASK      (bitwise-arithmetic-shift-left 1 (- SDL_BUTTON_RIGHT 1)))
  (define SDL_BUTTON_X1MASK     (bitwise-arithmetic-shift-left 1 (- SDL_BUTTON_X1 1)))
  (define SDL_BUTTON_X2MASK     (bitwise-arithmetic-shift-left 1 (- SDL_BUTTON_X2 1)))

  ;; struct SDL_JoyHatEvent.value
  (define SDL_HAT_CENTERED      0)
  (define SDL_HAT_UP            1)
  (define SDL_HAT_RIGHT         2)
  (define SDL_HAT_DOWN          4)
  (define SDL_HAT_LEFT          8)

  (define SDL_HAT_RIGHTUP       (+ SDL_HAT_RIGHT SDL_HAT_UP))
  (define SDL_HAT_RIGHTDOWN     (+ SDL_HAT_RIGHT SDL_HAT_DOWN))
  (define SDL_HAT_LEFTUP        (+ SDL_HAT_LEFT SDL_HAT_UP))
  (define SDL_HAT_LEFTDOWN      (+ SDL_HAT_LEFT SDL_HAT_DOWN))

  (define KMOD_CTRL (bitwise-ior KMOD_LCTRL KMOD_RCTRL))
  (define KMOD_SHIFT (bitwise-ior KMOD_LSHIFT KMOD_RSHIFT))
  (define KMOD_ALT (bitwise-ior KMOD_LALT KMOD_RALT))
  (define KMOD_META (bitwise-ior KMOD_LMETA KMOD_RMETA))

  ) ;[end]
