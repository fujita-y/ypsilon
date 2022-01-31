#!nobacktrace
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon glfw)
  (export glfwInit
          glfwTerminate
          glfwInitHint
          glfwGetVersion
          glfwGetVersionString
          glfwGetError
          glfwSetErrorCallback
          glfwGetMonitors
          glfwGetPrimaryMonitor
          glfwGetMonitorPos
          glfwGetMonitorWorkarea
          glfwGetMonitorPhysicalSize
          glfwGetMonitorContentScale
          glfwGetMonitorName
          glfwSetMonitorUserPointer
          glfwGetMonitorUserPointer
          glfwSetMonitorCallback
          glfwGetVideoModes
          glfwGetVideoMode
          glfwSetGamma
          glfwGetGammaRamp
          glfwSetGammaRamp
          glfwDefaultWindowHints
          glfwWindowHint
          glfwWindowHintString
          glfwCreateWindow
          glfwDestroyWindow
          glfwWindowShouldClose
          glfwSetWindowShouldClose
          glfwSetWindowTitle
          glfwSetWindowIcon
          glfwGetWindowPos
          glfwSetWindowPos
          glfwGetWindowSize
          glfwSetWindowSizeLimits
          glfwSetWindowAspectRatio
          glfwSetWindowSize
          glfwGetFramebufferSize
          glfwGetWindowFrameSize
          glfwGetWindowContentScale
          glfwGetWindowOpacity
          glfwSetWindowOpacity
          glfwIconifyWindow
          glfwRestoreWindow
          glfwMaximizeWindow
          glfwShowWindow
          glfwHideWindow
          glfwFocusWindow
          glfwRequestWindowAttention
          glfwGetWindowMonitor
          glfwSetWindowMonitor
          glfwGetWindowAttrib
          glfwSetWindowAttrib
          glfwSetWindowUserPointer
          glfwGetWindowUserPointer
          glfwSetWindowPosCallback
          glfwSetWindowSizeCallback
          glfwSetWindowCloseCallback
          glfwSetWindowRefreshCallback
          glfwSetWindowFocusCallback
          glfwSetWindowIconifyCallback
          glfwSetWindowMaximizeCallback
          glfwSetFramebufferSizeCallback
          glfwSetWindowContentScaleCallback
          glfwPollEvents
          glfwWaitEvents
          glfwWaitEventsTimeout
          glfwPostEmptyEvent
          glfwGetInputMode
          glfwSetInputMode
          glfwRawMouseMotionSupported
          glfwGetKeyName
          glfwGetKeyScancode
          glfwGetKey
          glfwGetMouseButton
          glfwGetCursorPos
          glfwSetCursorPos
          glfwCreateCursor
          glfwCreateStandardCursor
          glfwDestroyCursor
          glfwSetCursor
          glfwSetKeyCallback
          glfwSetCharCallback
          glfwSetCharModsCallback
          glfwSetMouseButtonCallback
          glfwSetCursorPosCallback
          glfwSetCursorEnterCallback
          glfwSetScrollCallback
          glfwSetDropCallback
          glfwJoystickPresent
          glfwGetJoystickAxes
          glfwGetJoystickButtons
          glfwGetJoystickHats
          glfwGetJoystickName
          glfwGetJoystickGUID
          glfwSetJoystickUserPointer
          glfwGetJoystickUserPointer
          glfwJoystickIsGamepad
          glfwSetJoystickCallback
          glfwUpdateGamepadMappings
          glfwGetGamepadName
          glfwGetGamepadState
          glfwSetClipboardString
          glfwGetClipboardString
          glfwGetTime
          glfwSetTime
          glfwMakeContextCurrent
          glfwGetCurrentContext
          glfwSwapBuffers
          glfwSwapInterval
          glfwExtensionSupported
          glfwGetProcAddress
          glfwVulkanSupported
          glfwGetRequiredInstanceExtensions
          glfwGetInstanceProcAddress
          glfwGetPhysicalDevicePresentationSupport
          glfwCreateWindowSurface
          GLFW_VERSION_MAJOR
          GLFW_VERSION_MINOR
          GLFW_VERSION_REVISION
          GLFW_TRUE
          GLFW_FALSE
          GLFW_RELEASE
          GLFW_PRESS
          GLFW_REPEAT
          GLFW_HAT_CENTERED
          GLFW_HAT_UP
          GLFW_HAT_RIGHT
          GLFW_HAT_DOWN
          GLFW_HAT_LEFT
          GLFW_HAT_RIGHT_UP
          GLFW_HAT_RIGHT_DOWN
          GLFW_HAT_LEFT_UP
          GLFW_HAT_LEFT_DOWN
          GLFW_KEY_UNKNOWN
          GLFW_KEY_SPACE
          GLFW_KEY_0
          GLFW_KEY_1
          GLFW_KEY_2
          GLFW_KEY_3
          GLFW_KEY_4
          GLFW_KEY_5
          GLFW_KEY_6
          GLFW_KEY_7
          GLFW_KEY_8
          GLFW_KEY_9
          GLFW_KEY_A
          GLFW_KEY_B
          GLFW_KEY_C
          GLFW_KEY_D
          GLFW_KEY_E
          GLFW_KEY_F
          GLFW_KEY_G
          GLFW_KEY_H
          GLFW_KEY_I
          GLFW_KEY_J
          GLFW_KEY_K
          GLFW_KEY_L
          GLFW_KEY_M
          GLFW_KEY_N
          GLFW_KEY_O
          GLFW_KEY_P
          GLFW_KEY_Q
          GLFW_KEY_R
          GLFW_KEY_S
          GLFW_KEY_T
          GLFW_KEY_U
          GLFW_KEY_V
          GLFW_KEY_W
          GLFW_KEY_X
          GLFW_KEY_Y
          GLFW_KEY_Z
          GLFW_KEY_ESCAPE
          GLFW_KEY_ENTER
          GLFW_KEY_TAB
          GLFW_KEY_BACKSPACE
          GLFW_KEY_INSERT
          GLFW_KEY_DELETE
          GLFW_KEY_RIGHT
          GLFW_KEY_LEFT
          GLFW_KEY_DOWN
          GLFW_KEY_UP
          GLFW_KEY_PAGE_UP
          GLFW_KEY_PAGE_DOWN
          GLFW_KEY_HOME
          GLFW_KEY_END
          GLFW_KEY_CAPS_LOCK
          GLFW_KEY_SCROLL_LOCK
          GLFW_KEY_NUM_LOCK
          GLFW_KEY_PRINT_SCREEN
          GLFW_KEY_PAUSE
          GLFW_KEY_F1
          GLFW_KEY_F2
          GLFW_KEY_F3
          GLFW_KEY_F4
          GLFW_KEY_F5
          GLFW_KEY_F6
          GLFW_KEY_F7
          GLFW_KEY_F8
          GLFW_KEY_F9
          GLFW_KEY_F10
          GLFW_KEY_F11
          GLFW_KEY_F12
          GLFW_KEY_F13
          GLFW_KEY_F14
          GLFW_KEY_F15
          GLFW_KEY_F16
          GLFW_KEY_F17
          GLFW_KEY_F18
          GLFW_KEY_F19
          GLFW_KEY_F20
          GLFW_KEY_F21
          GLFW_KEY_F22
          GLFW_KEY_F23
          GLFW_KEY_F24
          GLFW_KEY_F25
          GLFW_KEY_KP_0
          GLFW_KEY_KP_1
          GLFW_KEY_KP_2
          GLFW_KEY_KP_3
          GLFW_KEY_KP_4
          GLFW_KEY_KP_5
          GLFW_KEY_KP_6
          GLFW_KEY_KP_7
          GLFW_KEY_KP_8
          GLFW_KEY_KP_9
          GLFW_KEY_KP_DECIMAL
          GLFW_KEY_KP_DIVIDE
          GLFW_KEY_KP_MULTIPLY
          GLFW_KEY_KP_SUBTRACT
          GLFW_KEY_KP_ADD
          GLFW_KEY_KP_ENTER
          GLFW_KEY_KP_EQUAL
          GLFW_KEY_LEFT_SHIFT
          GLFW_KEY_LEFT_CONTROL
          GLFW_KEY_LEFT_ALT
          GLFW_KEY_LEFT_SUPER
          GLFW_KEY_RIGHT_SHIFT
          GLFW_KEY_RIGHT_CONTROL
          GLFW_KEY_RIGHT_ALT
          GLFW_KEY_RIGHT_SUPER
          GLFW_KEY_MENU
          GLFW_KEY_LAST
          GLFW_MOD_SHIFT
          GLFW_MOD_CONTROL
          GLFW_MOD_ALT
          GLFW_MOD_SUPER
          GLFW_MOD_CAPS_LOCK
          GLFW_MOD_NUM_LOCK
          GLFW_MOUSE_BUTTON_1
          GLFW_MOUSE_BUTTON_2
          GLFW_MOUSE_BUTTON_3
          GLFW_MOUSE_BUTTON_4
          GLFW_MOUSE_BUTTON_5
          GLFW_MOUSE_BUTTON_6
          GLFW_MOUSE_BUTTON_7
          GLFW_MOUSE_BUTTON_8
          GLFW_MOUSE_BUTTON_LAST
          GLFW_MOUSE_BUTTON_LEFT
          GLFW_MOUSE_BUTTON_RIGHT
          GLFW_MOUSE_BUTTON_MIDDLE
          GLFW_JOYSTICK_1
          GLFW_JOYSTICK_2
          GLFW_JOYSTICK_3
          GLFW_JOYSTICK_4
          GLFW_JOYSTICK_5
          GLFW_JOYSTICK_6
          GLFW_JOYSTICK_7
          GLFW_JOYSTICK_8
          GLFW_JOYSTICK_9
          GLFW_JOYSTICK_10
          GLFW_JOYSTICK_11
          GLFW_JOYSTICK_12
          GLFW_JOYSTICK_13
          GLFW_JOYSTICK_14
          GLFW_JOYSTICK_15
          GLFW_JOYSTICK_16
          GLFW_JOYSTICK_LAST
          GLFW_GAMEPAD_BUTTON_A
          GLFW_GAMEPAD_BUTTON_B
          GLFW_GAMEPAD_BUTTON_X
          GLFW_GAMEPAD_BUTTON_Y
          GLFW_GAMEPAD_BUTTON_LEFT_BUMPER
          GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER
          GLFW_GAMEPAD_BUTTON_BACK
          GLFW_GAMEPAD_BUTTON_START
          GLFW_GAMEPAD_BUTTON_GUIDE
          GLFW_GAMEPAD_BUTTON_LEFT_THUMB
          GLFW_GAMEPAD_BUTTON_RIGHT_THUMB
          GLFW_GAMEPAD_BUTTON_DPAD_UP
          GLFW_GAMEPAD_BUTTON_DPAD_RIGHT
          GLFW_GAMEPAD_BUTTON_DPAD_DOWN
          GLFW_GAMEPAD_BUTTON_DPAD_LEFT
          GLFW_GAMEPAD_BUTTON_LAST
          GLFW_GAMEPAD_BUTTON_CROSS
          GLFW_GAMEPAD_BUTTON_CIRCLE
          GLFW_GAMEPAD_BUTTON_SQUARE
          GLFW_GAMEPAD_BUTTON_TRIANGLE
          GLFW_GAMEPAD_AXIS_LEFT_X
          GLFW_GAMEPAD_AXIS_LEFT_Y
          GLFW_GAMEPAD_AXIS_RIGHT_X
          GLFW_GAMEPAD_AXIS_RIGHT_Y
          GLFW_GAMEPAD_AXIS_LEFT_TRIGGER
          GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER
          GLFW_GAMEPAD_AXIS_LAST
          GLFW_NO_ERROR
          GLFW_NOT_INITIALIZED
          GLFW_NO_CURRENT_CONTEXT
          GLFW_INVALID_ENUM
          GLFW_INVALID_VALUE
          GLFW_OUT_OF_MEMORY
          GLFW_API_UNAVAILABLE
          GLFW_VERSION_UNAVAILABLE
          GLFW_PLATFORM_ERROR
          GLFW_FORMAT_UNAVAILABLE
          GLFW_NO_WINDOW_CONTEXT
          GLFW_CURSOR_UNAVAILABLE
          GLFW_FOCUSED
          GLFW_ICONIFIED
          GLFW_RESIZABLE
          GLFW_VISIBLE
          GLFW_DECORATED
          GLFW_AUTO_ICONIFY
          GLFW_FLOATING
          GLFW_MAXIMIZED
          GLFW_CENTER_CURSOR
          GLFW_TRANSPARENT_FRAMEBUFFER
          GLFW_HOVERED
          GLFW_FOCUS_ON_SHOW
          GLFW_RED_BITS
          GLFW_GREEN_BITS
          GLFW_BLUE_BITS
          GLFW_ALPHA_BITS
          GLFW_DEPTH_BITS
          GLFW_STENCIL_BITS
          GLFW_ACCUM_RED_BITS
          GLFW_ACCUM_GREEN_BITS
          GLFW_ACCUM_BLUE_BITS
          GLFW_ACCUM_ALPHA_BITS
          GLFW_AUX_BUFFERS
          GLFW_STEREO
          GLFW_SAMPLES
          GLFW_SRGB_CAPABLE
          GLFW_REFRESH_RATE
          GLFW_DOUBLEBUFFER
          GLFW_CLIENT_API
          GLFW_CONTEXT_VERSION_MAJOR
          GLFW_CONTEXT_VERSION_MINOR
          GLFW_CONTEXT_REVISION
          GLFW_CONTEXT_ROBUSTNESS
          GLFW_OPENGL_FORWARD_COMPAT
          GLFW_OPENGL_DEBUG_CONTEXT
          GLFW_OPENGL_PROFILE
          GLFW_CONTEXT_RELEASE_BEHAVIOR
          GLFW_CONTEXT_NO_ERROR
          GLFW_CONTEXT_CREATION_API
          GLFW_SCALE_TO_MONITOR
          GLFW_COCOA_RETINA_FRAMEBUFFER
          GLFW_COCOA_FRAME_NAME
          GLFW_COCOA_GRAPHICS_SWITCHING
          GLFW_X11_CLASS_NAME
          GLFW_X11_INSTANCE_NAME
          GLFW_WIN32_KEYBOARD_MENU
          GLFW_NO_API
          GLFW_OPENGL_API
          GLFW_OPENGL_ES_API
          GLFW_NO_ROBUSTNESS
          GLFW_NO_RESET_NOTIFICATION
          GLFW_LOSE_CONTEXT_ON_RESET
          GLFW_OPENGL_ANY_PROFILE
          GLFW_OPENGL_CORE_PROFILE
          GLFW_OPENGL_COMPAT_PROFILE
          GLFW_CURSOR
          GLFW_STICKY_KEYS
          GLFW_STICKY_MOUSE_BUTTONS
          GLFW_LOCK_KEY_MODS
          GLFW_RAW_MOUSE_MOTION
          GLFW_CURSOR_NORMAL
          GLFW_CURSOR_HIDDEN
          GLFW_CURSOR_DISABLED
          GLFW_ANY_RELEASE_BEHAVIOR
          GLFW_RELEASE_BEHAVIOR_FLUSH
          GLFW_RELEASE_BEHAVIOR_NONE
          GLFW_NATIVE_CONTEXT_API
          GLFW_EGL_CONTEXT_API
          GLFW_OSMESA_CONTEXT_API
          GLFW_ARROW_CURSOR
          GLFW_IBEAM_CURSOR
          GLFW_CROSSHAIR_CURSOR
          GLFW_POINTING_HAND_CURSOR
          GLFW_RESIZE_EW_CURSOR
          GLFW_RESIZE_NS_CURSOR
          GLFW_RESIZE_NWSE_CURSOR
          GLFW_RESIZE_NESW_CURSOR
          GLFW_RESIZE_ALL_CURSOR
          GLFW_NOT_ALLOWED_CURSOR
          GLFW_HRESIZE_CURSOR
          GLFW_VRESIZE_CURSOR
          GLFW_HAND_CURSOR
          GLFW_CONNECTED
          GLFW_DISCONNECTED
          GLFW_JOYSTICK_HAT_BUTTONS
          GLFW_COCOA_CHDIR_RESOURCES
          GLFW_COCOA_MENUBAR
          GLFW_DONT_CARE)
  (import (core) (ypsilon c-ffi))
  (define libGLFW
    (let ((sysname (architecture-feature 'sysname)))
      (cond ((string-contains sysname "darwin")
             (load-shared-object "libglfw.dylib"))
            ((string-contains sysname "linux")
             (load-shared-object "libglfw.so.3"))
            (else
              (assertion-violation 'load-shared-object "can not load GLFW library, unknown operating system")))))
  (define-syntax define-cdecl
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function/weak ret name args)))))
  (define GLFW_VERSION_MAJOR 3)
  (define GLFW_VERSION_MINOR 4)
  (define GLFW_VERSION_REVISION 0)
  (define GLFW_TRUE 1)
  (define GLFW_FALSE 0)
  (define GLFW_RELEASE 0)
  (define GLFW_PRESS 1)
  (define GLFW_REPEAT 2)
  (define GLFW_HAT_CENTERED 0)
  (define GLFW_HAT_UP 1)
  (define GLFW_HAT_RIGHT 2)
  (define GLFW_HAT_DOWN 4)
  (define GLFW_HAT_LEFT 8)
  (define GLFW_HAT_RIGHT_UP (+ GLFW_HAT_RIGHT GLFW_HAT_UP))
  (define GLFW_HAT_RIGHT_DOWN (+ GLFW_HAT_RIGHT GLFW_HAT_DOWN))
  (define GLFW_HAT_LEFT_UP (+ GLFW_HAT_LEFT  GLFW_HAT_UP))
  (define GLFW_HAT_LEFT_DOWN (+ GLFW_HAT_LEFT  GLFW_HAT_DOWN))
  (define GLFW_KEY_UNKNOWN -1)
  (define GLFW_KEY_SPACE 32)
  (define GLFW_KEY_0 48)
  (define GLFW_KEY_1 49)
  (define GLFW_KEY_2 50)
  (define GLFW_KEY_3 51)
  (define GLFW_KEY_4 52)
  (define GLFW_KEY_5 53)
  (define GLFW_KEY_6 54)
  (define GLFW_KEY_7 55)
  (define GLFW_KEY_8 56)
  (define GLFW_KEY_9 57)
  (define GLFW_KEY_A 65)
  (define GLFW_KEY_B 66)
  (define GLFW_KEY_C 67)
  (define GLFW_KEY_D 68)
  (define GLFW_KEY_E 69)
  (define GLFW_KEY_F 70)
  (define GLFW_KEY_G 71)
  (define GLFW_KEY_H 72)
  (define GLFW_KEY_I 73)
  (define GLFW_KEY_J 74)
  (define GLFW_KEY_K 75)
  (define GLFW_KEY_L 76)
  (define GLFW_KEY_M 77)
  (define GLFW_KEY_N 78)
  (define GLFW_KEY_O 79)
  (define GLFW_KEY_P 80)
  (define GLFW_KEY_Q 81)
  (define GLFW_KEY_R 82)
  (define GLFW_KEY_S 83)
  (define GLFW_KEY_T 84)
  (define GLFW_KEY_U 85)
  (define GLFW_KEY_V 86)
  (define GLFW_KEY_W 87)
  (define GLFW_KEY_X 88)
  (define GLFW_KEY_Y 89)
  (define GLFW_KEY_Z 90)
  (define GLFW_KEY_ESCAPE 256)
  (define GLFW_KEY_ENTER 257)
  (define GLFW_KEY_TAB 258)
  (define GLFW_KEY_BACKSPACE 259)
  (define GLFW_KEY_INSERT 260)
  (define GLFW_KEY_DELETE 261)
  (define GLFW_KEY_RIGHT 262)
  (define GLFW_KEY_LEFT 263)
  (define GLFW_KEY_DOWN 264)
  (define GLFW_KEY_UP 265)
  (define GLFW_KEY_PAGE_UP 266)
  (define GLFW_KEY_PAGE_DOWN 267)
  (define GLFW_KEY_HOME 268)
  (define GLFW_KEY_END 269)
  (define GLFW_KEY_CAPS_LOCK 280)
  (define GLFW_KEY_SCROLL_LOCK 281)
  (define GLFW_KEY_NUM_LOCK 282)
  (define GLFW_KEY_PRINT_SCREEN 283)
  (define GLFW_KEY_PAUSE 284)
  (define GLFW_KEY_F1 290)
  (define GLFW_KEY_F2 291)
  (define GLFW_KEY_F3 292)
  (define GLFW_KEY_F4 293)
  (define GLFW_KEY_F5 294)
  (define GLFW_KEY_F6 295)
  (define GLFW_KEY_F7 296)
  (define GLFW_KEY_F8 297)
  (define GLFW_KEY_F9 298)
  (define GLFW_KEY_F10 299)
  (define GLFW_KEY_F11 300)
  (define GLFW_KEY_F12 301)
  (define GLFW_KEY_F13 302)
  (define GLFW_KEY_F14 303)
  (define GLFW_KEY_F15 304)
  (define GLFW_KEY_F16 305)
  (define GLFW_KEY_F17 306)
  (define GLFW_KEY_F18 307)
  (define GLFW_KEY_F19 308)
  (define GLFW_KEY_F20 309)
  (define GLFW_KEY_F21 310)
  (define GLFW_KEY_F22 311)
  (define GLFW_KEY_F23 312)
  (define GLFW_KEY_F24 313)
  (define GLFW_KEY_F25 314)
  (define GLFW_KEY_KP_0 320)
  (define GLFW_KEY_KP_1 321)
  (define GLFW_KEY_KP_2 322)
  (define GLFW_KEY_KP_3 323)
  (define GLFW_KEY_KP_4 324)
  (define GLFW_KEY_KP_5 325)
  (define GLFW_KEY_KP_6 326)
  (define GLFW_KEY_KP_7 327)
  (define GLFW_KEY_KP_8 328)
  (define GLFW_KEY_KP_9 329)
  (define GLFW_KEY_KP_DECIMAL 330)
  (define GLFW_KEY_KP_DIVIDE 331)
  (define GLFW_KEY_KP_MULTIPLY 332)
  (define GLFW_KEY_KP_SUBTRACT 333)
  (define GLFW_KEY_KP_ADD 334)
  (define GLFW_KEY_KP_ENTER 335)
  (define GLFW_KEY_KP_EQUAL 336)
  (define GLFW_KEY_LEFT_SHIFT 340)
  (define GLFW_KEY_LEFT_CONTROL 341)
  (define GLFW_KEY_LEFT_ALT 342)
  (define GLFW_KEY_LEFT_SUPER 343)
  (define GLFW_KEY_RIGHT_SHIFT 344)
  (define GLFW_KEY_RIGHT_CONTROL 345)
  (define GLFW_KEY_RIGHT_ALT 346)
  (define GLFW_KEY_RIGHT_SUPER 347)
  (define GLFW_KEY_MENU 348)
  (define GLFW_KEY_LAST GLFW_KEY_MENU)
  (define GLFW_MOD_SHIFT #x0001)
  (define GLFW_MOD_CONTROL #x0002)
  (define GLFW_MOD_ALT #x0004)
  (define GLFW_MOD_SUPER #x0008)
  (define GLFW_MOD_CAPS_LOCK #x0010)
  (define GLFW_MOD_NUM_LOCK #x0020)
  (define GLFW_MOUSE_BUTTON_1 0)
  (define GLFW_MOUSE_BUTTON_2 1)
  (define GLFW_MOUSE_BUTTON_3 2)
  (define GLFW_MOUSE_BUTTON_4 3)
  (define GLFW_MOUSE_BUTTON_5 4)
  (define GLFW_MOUSE_BUTTON_6 5)
  (define GLFW_MOUSE_BUTTON_7 6)
  (define GLFW_MOUSE_BUTTON_8 7)
  (define GLFW_MOUSE_BUTTON_LAST GLFW_MOUSE_BUTTON_8)
  (define GLFW_MOUSE_BUTTON_LEFT GLFW_MOUSE_BUTTON_1)
  (define GLFW_MOUSE_BUTTON_RIGHT GLFW_MOUSE_BUTTON_2)
  (define GLFW_MOUSE_BUTTON_MIDDLE GLFW_MOUSE_BUTTON_3)
  (define GLFW_JOYSTICK_1 0)
  (define GLFW_JOYSTICK_2 1)
  (define GLFW_JOYSTICK_3 2)
  (define GLFW_JOYSTICK_4 3)
  (define GLFW_JOYSTICK_5 4)
  (define GLFW_JOYSTICK_6 5)
  (define GLFW_JOYSTICK_7 6)
  (define GLFW_JOYSTICK_8 7)
  (define GLFW_JOYSTICK_9 8)
  (define GLFW_JOYSTICK_10 9)
  (define GLFW_JOYSTICK_11 10)
  (define GLFW_JOYSTICK_12 11)
  (define GLFW_JOYSTICK_13 12)
  (define GLFW_JOYSTICK_14 13)
  (define GLFW_JOYSTICK_15 14)
  (define GLFW_JOYSTICK_16 15)
  (define GLFW_JOYSTICK_LAST GLFW_JOYSTICK_16)
  (define GLFW_GAMEPAD_BUTTON_A 0)
  (define GLFW_GAMEPAD_BUTTON_B 1)
  (define GLFW_GAMEPAD_BUTTON_X 2)
  (define GLFW_GAMEPAD_BUTTON_Y 3)
  (define GLFW_GAMEPAD_BUTTON_LEFT_BUMPER 4)
  (define GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER 5)
  (define GLFW_GAMEPAD_BUTTON_BACK 6)
  (define GLFW_GAMEPAD_BUTTON_START 7)
  (define GLFW_GAMEPAD_BUTTON_GUIDE 8)
  (define GLFW_GAMEPAD_BUTTON_LEFT_THUMB 9)
  (define GLFW_GAMEPAD_BUTTON_RIGHT_THUMB 10)
  (define GLFW_GAMEPAD_BUTTON_DPAD_UP 11)
  (define GLFW_GAMEPAD_BUTTON_DPAD_RIGHT 12)
  (define GLFW_GAMEPAD_BUTTON_DPAD_DOWN 13)
  (define GLFW_GAMEPAD_BUTTON_DPAD_LEFT 14)
  (define GLFW_GAMEPAD_BUTTON_LAST GLFW_GAMEPAD_BUTTON_DPAD_LEFT)
  (define GLFW_GAMEPAD_BUTTON_CROSS GLFW_GAMEPAD_BUTTON_A)
  (define GLFW_GAMEPAD_BUTTON_CIRCLE GLFW_GAMEPAD_BUTTON_B)
  (define GLFW_GAMEPAD_BUTTON_SQUARE GLFW_GAMEPAD_BUTTON_X)
  (define GLFW_GAMEPAD_BUTTON_TRIANGLE GLFW_GAMEPAD_BUTTON_Y)
  (define GLFW_GAMEPAD_AXIS_LEFT_X 0)
  (define GLFW_GAMEPAD_AXIS_LEFT_Y 1)
  (define GLFW_GAMEPAD_AXIS_RIGHT_X 2)
  (define GLFW_GAMEPAD_AXIS_RIGHT_Y 3)
  (define GLFW_GAMEPAD_AXIS_LEFT_TRIGGER 4)
  (define GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER 5)
  (define GLFW_GAMEPAD_AXIS_LAST GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER)
  (define GLFW_NO_ERROR 0)
  (define GLFW_NOT_INITIALIZED #x00010001)
  (define GLFW_NO_CURRENT_CONTEXT #x00010002)
  (define GLFW_INVALID_ENUM #x00010003)
  (define GLFW_INVALID_VALUE #x00010004)
  (define GLFW_OUT_OF_MEMORY #x00010005)
  (define GLFW_API_UNAVAILABLE #x00010006)
  (define GLFW_VERSION_UNAVAILABLE #x00010007)
  (define GLFW_PLATFORM_ERROR #x00010008)
  (define GLFW_FORMAT_UNAVAILABLE #x00010009)
  (define GLFW_NO_WINDOW_CONTEXT #x0001000A)
  (define GLFW_CURSOR_UNAVAILABLE #x0001000B)
  (define GLFW_FOCUSED #x00020001)
  (define GLFW_ICONIFIED #x00020002)
  (define GLFW_RESIZABLE #x00020003)
  (define GLFW_VISIBLE #x00020004)
  (define GLFW_DECORATED #x00020005)
  (define GLFW_AUTO_ICONIFY #x00020006)
  (define GLFW_FLOATING #x00020007)
  (define GLFW_MAXIMIZED #x00020008)
  (define GLFW_CENTER_CURSOR #x00020009)
  (define GLFW_TRANSPARENT_FRAMEBUFFER #x0002000A)
  (define GLFW_HOVERED #x0002000B)
  (define GLFW_FOCUS_ON_SHOW #x0002000C)
  (define GLFW_RED_BITS #x00021001)
  (define GLFW_GREEN_BITS #x00021002)
  (define GLFW_BLUE_BITS #x00021003)
  (define GLFW_ALPHA_BITS #x00021004)
  (define GLFW_DEPTH_BITS #x00021005)
  (define GLFW_STENCIL_BITS #x00021006)
  (define GLFW_ACCUM_RED_BITS #x00021007)
  (define GLFW_ACCUM_GREEN_BITS #x00021008)
  (define GLFW_ACCUM_BLUE_BITS #x00021009)
  (define GLFW_ACCUM_ALPHA_BITS #x0002100A)
  (define GLFW_AUX_BUFFERS #x0002100B)
  (define GLFW_STEREO #x0002100C)
  (define GLFW_SAMPLES #x0002100D)
  (define GLFW_SRGB_CAPABLE #x0002100E)
  (define GLFW_REFRESH_RATE #x0002100F)
  (define GLFW_DOUBLEBUFFER #x00021010)
  (define GLFW_CLIENT_API #x00022001)
  (define GLFW_CONTEXT_VERSION_MAJOR #x00022002)
  (define GLFW_CONTEXT_VERSION_MINOR #x00022003)
  (define GLFW_CONTEXT_REVISION #x00022004)
  (define GLFW_CONTEXT_ROBUSTNESS #x00022005)
  (define GLFW_OPENGL_FORWARD_COMPAT #x00022006)
  (define GLFW_OPENGL_DEBUG_CONTEXT #x00022007)
  (define GLFW_OPENGL_PROFILE #x00022008)
  (define GLFW_CONTEXT_RELEASE_BEHAVIOR #x00022009)
  (define GLFW_CONTEXT_NO_ERROR #x0002200A)
  (define GLFW_CONTEXT_CREATION_API #x0002200B)
  (define GLFW_SCALE_TO_MONITOR #x0002200C)
  (define GLFW_COCOA_RETINA_FRAMEBUFFER #x00023001)
  (define GLFW_COCOA_FRAME_NAME #x00023002)
  (define GLFW_COCOA_GRAPHICS_SWITCHING #x00023003)
  (define GLFW_X11_CLASS_NAME #x00024001)
  (define GLFW_X11_INSTANCE_NAME #x00024002)
  (define GLFW_WIN32_KEYBOARD_MENU #x00025001)
  (define GLFW_NO_API 0)
  (define GLFW_OPENGL_API #x00030001)
  (define GLFW_OPENGL_ES_API #x00030002)
  (define GLFW_NO_ROBUSTNESS 0)
  (define GLFW_NO_RESET_NOTIFICATION #x00031001)
  (define GLFW_LOSE_CONTEXT_ON_RESET #x00031002)
  (define GLFW_OPENGL_ANY_PROFILE 0)
  (define GLFW_OPENGL_CORE_PROFILE #x00032001)
  (define GLFW_OPENGL_COMPAT_PROFILE #x00032002)
  (define GLFW_CURSOR #x00033001)
  (define GLFW_STICKY_KEYS #x00033002)
  (define GLFW_STICKY_MOUSE_BUTTONS #x00033003)
  (define GLFW_LOCK_KEY_MODS #x00033004)
  (define GLFW_RAW_MOUSE_MOTION #x00033005)
  (define GLFW_CURSOR_NORMAL #x00034001)
  (define GLFW_CURSOR_HIDDEN #x00034002)
  (define GLFW_CURSOR_DISABLED #x00034003)
  (define GLFW_ANY_RELEASE_BEHAVIOR 0)
  (define GLFW_RELEASE_BEHAVIOR_FLUSH #x00035001)
  (define GLFW_RELEASE_BEHAVIOR_NONE #x00035002)
  (define GLFW_NATIVE_CONTEXT_API #x00036001)
  (define GLFW_EGL_CONTEXT_API #x00036002)
  (define GLFW_OSMESA_CONTEXT_API #x00036003)
  (define GLFW_ARROW_CURSOR #x00036001)
  (define GLFW_IBEAM_CURSOR #x00036002)
  (define GLFW_CROSSHAIR_CURSOR #x00036003)
  (define GLFW_POINTING_HAND_CURSOR #x00036004)
  (define GLFW_RESIZE_EW_CURSOR #x00036005)
  (define GLFW_RESIZE_NS_CURSOR #x00036006)
  (define GLFW_RESIZE_NWSE_CURSOR #x00036007)
  (define GLFW_RESIZE_NESW_CURSOR #x00036008)
  (define GLFW_RESIZE_ALL_CURSOR #x00036009)
  (define GLFW_NOT_ALLOWED_CURSOR #x0003600A)
  (define GLFW_HRESIZE_CURSOR GLFW_RESIZE_EW_CURSOR)
  (define GLFW_VRESIZE_CURSOR GLFW_RESIZE_NS_CURSOR)
  (define GLFW_HAND_CURSOR GLFW_POINTING_HAND_CURSOR)
  (define GLFW_CONNECTED #x00040001)
  (define GLFW_DISCONNECTED #x00040002)
  (define GLFW_JOYSTICK_HAT_BUTTONS #x00050001)
  (define GLFW_COCOA_CHDIR_RESOURCES #x00051001)
  (define GLFW_COCOA_MENUBAR #x00051002)
  (define GLFW_DONT_CARE -1)
  ;; int glfwInit(void)
  (define-cdecl int glfwInit ())
  ;; void glfwTerminate(void)
  (define-cdecl void glfwTerminate ())
  ;; void glfwInitHint(int hint, int value)
  (define-cdecl void glfwInitHint (int int))
  ;; void glfwGetVersion(int* major, int* minor, int* rev)
  (define-cdecl void glfwGetVersion (void* void* void*))
  ;; const char* glfwGetVersionString(void)
  (define-cdecl void* glfwGetVersionString ())
  ;; int glfwGetError(const char** description)
  (define-cdecl int glfwGetError (void*))
  ;; GLFWerrorfun glfwSetErrorCallback(GLFWerrorfun callback)
  (define-cdecl void* glfwSetErrorCallback (void*))
  ;; GLFWmonitor** glfwGetMonitors(int* count)
  (define-cdecl void* glfwGetMonitors (void*))
  ;; GLFWmonitor* glfwGetPrimaryMonitor(void)
  (define-cdecl void* glfwGetPrimaryMonitor ())
  ;; void glfwGetMonitorPos(GLFWmonitor* monitor, int* xpos, int* ypos)
  (define-cdecl void glfwGetMonitorPos (void* void* void*))
  ;; void glfwGetMonitorWorkarea(GLFWmonitor* monitor, int* xpos, int* ypos, int* width, int* height)
  (define-cdecl void glfwGetMonitorWorkarea (void* void* void* void* void*))
  ;; void glfwGetMonitorPhysicalSize(GLFWmonitor* monitor, int* widthMM, int* heightMM)
  (define-cdecl void glfwGetMonitorPhysicalSize (void* void* void*))
  ;; void glfwGetMonitorContentScale(GLFWmonitor* monitor, float* xscale, float* yscale)
  (define-cdecl void glfwGetMonitorContentScale (void* void* void*))
  ;; const char* glfwGetMonitorName(GLFWmonitor* monitor)
  (define-cdecl void* glfwGetMonitorName (void*))
  ;; void glfwSetMonitorUserPointer(GLFWmonitor* monitor, void* pointer)
  (define-cdecl void glfwSetMonitorUserPointer (void* void*))
  ;; void* glfwGetMonitorUserPointer(GLFWmonitor* monitor)
  (define-cdecl void* glfwGetMonitorUserPointer (void*))
  ;; GLFWmonitorfun glfwSetMonitorCallback(GLFWmonitorfun callback)
  (define-cdecl void* glfwSetMonitorCallback (void*))
  ;; const GLFWvidmode* glfwGetVideoModes(GLFWmonitor* monitor, int* count)
  (define-cdecl void* glfwGetVideoModes (void* void*))
  ;; const GLFWvidmode* glfwGetVideoMode(GLFWmonitor* monitor)
  (define-cdecl void* glfwGetVideoMode (void*))
  ;; void glfwSetGamma(GLFWmonitor* monitor, float gamma)
  (define-cdecl void glfwSetGamma (void* float))
  ;; const GLFWgammaramp* glfwGetGammaRamp(GLFWmonitor* monitor)
  (define-cdecl void* glfwGetGammaRamp (void*))
  ;; void glfwSetGammaRamp(GLFWmonitor* monitor, const GLFWgammaramp* ramp)
  (define-cdecl void glfwSetGammaRamp (void* void*))
  ;; void glfwDefaultWindowHints(void)
  (define-cdecl void glfwDefaultWindowHints ())
  ;; void glfwWindowHint(int hint, int value)
  (define-cdecl void glfwWindowHint (int int))
  ;; void glfwWindowHintString(int hint, const char* value)
  (define-cdecl void glfwWindowHintString (int void*))
  ;; GLFWwindow* glfwCreateWindow(int width, int height, const char* title, GLFWmonitor* monitor, GLFWwindow* share)
  (define-cdecl void* glfwCreateWindow (int int void* void* void*))
  ;; void glfwDestroyWindow(GLFWwindow* window)
  (define-cdecl void glfwDestroyWindow (void*))
  ;; int glfwWindowShouldClose(GLFWwindow* window)
  (define-cdecl int glfwWindowShouldClose (void*))
  ;; void glfwSetWindowShouldClose(GLFWwindow* window, int value)
  (define-cdecl void glfwSetWindowShouldClose (void* int))
  ;; void glfwSetWindowTitle(GLFWwindow* window, const char* title)
  (define-cdecl void glfwSetWindowTitle (void* void*))
  ;; void glfwSetWindowIcon(GLFWwindow* window, int count, const GLFWimage* images)
  (define-cdecl void glfwSetWindowIcon (void* int void*))
  ;; void glfwGetWindowPos(GLFWwindow* window, int* xpos, int* ypos)
  (define-cdecl void glfwGetWindowPos (void* void* void*))
  ;; void glfwSetWindowPos(GLFWwindow* window, int xpos, int ypos)
  (define-cdecl void glfwSetWindowPos (void* int int))
  ;; void glfwGetWindowSize(GLFWwindow* window, int* width, int* height)
  (define-cdecl void glfwGetWindowSize (void* void* void*))
  ;; void glfwSetWindowSizeLimits(GLFWwindow* window, int minwidth, int minheight, int maxwidth, int maxheight)
  (define-cdecl void glfwSetWindowSizeLimits (void* int int int int))
  ;; void glfwSetWindowAspectRatio(GLFWwindow* window, int numer, int denom)
  (define-cdecl void glfwSetWindowAspectRatio (void* int int))
  ;; void glfwSetWindowSize(GLFWwindow* window, int width, int height)
  (define-cdecl void glfwSetWindowSize (void* int int))
  ;; void glfwGetFramebufferSize(GLFWwindow* window, int* width, int* height)
  (define-cdecl void glfwGetFramebufferSize (void* void* void*))
  ;; void glfwGetWindowFrameSize(GLFWwindow* window, int* left, int* top, int* right, int* bottom)
  (define-cdecl void glfwGetWindowFrameSize (void* void* void* void* void*))
  ;; void glfwGetWindowContentScale(GLFWwindow* window, float* xscale, float* yscale)
  (define-cdecl void glfwGetWindowContentScale (void* void* void*))
  ;; float glfwGetWindowOpacity(GLFWwindow* window)
  (define-cdecl float glfwGetWindowOpacity (void*))
  ;; void glfwSetWindowOpacity(GLFWwindow* window, float opacity)
  (define-cdecl void glfwSetWindowOpacity (void* float))
  ;; void glfwIconifyWindow(GLFWwindow* window)
  (define-cdecl void glfwIconifyWindow (void*))
  ;; void glfwRestoreWindow(GLFWwindow* window)
  (define-cdecl void glfwRestoreWindow (void*))
  ;; void glfwMaximizeWindow(GLFWwindow* window)
  (define-cdecl void glfwMaximizeWindow (void*))
  ;; void glfwShowWindow(GLFWwindow* window)
  (define-cdecl void glfwShowWindow (void*))
  ;; void glfwHideWindow(GLFWwindow* window)
  (define-cdecl void glfwHideWindow (void*))
  ;; void glfwFocusWindow(GLFWwindow* window)
  (define-cdecl void glfwFocusWindow (void*))
  ;; void glfwRequestWindowAttention(GLFWwindow* window)
  (define-cdecl void glfwRequestWindowAttention (void*))
  ;; GLFWmonitor* glfwGetWindowMonitor(GLFWwindow* window)
  (define-cdecl void* glfwGetWindowMonitor (void*))
  ;; void glfwSetWindowMonitor(GLFWwindow* window, GLFWmonitor* monitor, int xpos, int ypos, int width, int height, int refreshRate)
  (define-cdecl void glfwSetWindowMonitor (void* void* int int int int int))
  ;; int glfwGetWindowAttrib(GLFWwindow* window, int attrib)
  (define-cdecl int glfwGetWindowAttrib (void* int))
  ;; void glfwSetWindowAttrib(GLFWwindow* window, int attrib, int value)
  (define-cdecl void glfwSetWindowAttrib (void* int int))
  ;; void glfwSetWindowUserPointer(GLFWwindow* window, void* pointer)
  (define-cdecl void glfwSetWindowUserPointer (void* void*))
  ;; void* glfwGetWindowUserPointer(GLFWwindow* window)
  (define-cdecl void* glfwGetWindowUserPointer (void*))
  ;; GLFWwindowposfun glfwSetWindowPosCallback(GLFWwindow* window, GLFWwindowposfun callback)
  (define-cdecl void* glfwSetWindowPosCallback (void* void*))
  ;; GLFWwindowsizefun glfwSetWindowSizeCallback(GLFWwindow* window, GLFWwindowsizefun callback)
  (define-cdecl void* glfwSetWindowSizeCallback (void* void*))
  ;; GLFWwindowclosefun glfwSetWindowCloseCallback(GLFWwindow* window, GLFWwindowclosefun callback)
  (define-cdecl void* glfwSetWindowCloseCallback (void* void*))
  ;; GLFWwindowrefreshfun glfwSetWindowRefreshCallback(GLFWwindow* window, GLFWwindowrefreshfun callback)
  (define-cdecl void* glfwSetWindowRefreshCallback (void* void*))
  ;; GLFWwindowfocusfun glfwSetWindowFocusCallback(GLFWwindow* window, GLFWwindowfocusfun callback)
  (define-cdecl void* glfwSetWindowFocusCallback (void* void*))
  ;; GLFWwindowiconifyfun glfwSetWindowIconifyCallback(GLFWwindow* window, GLFWwindowiconifyfun callback)
  (define-cdecl void* glfwSetWindowIconifyCallback (void* void*))
  ;; GLFWwindowmaximizefun glfwSetWindowMaximizeCallback(GLFWwindow* window, GLFWwindowmaximizefun callback)
  (define-cdecl void* glfwSetWindowMaximizeCallback (void* void*))
  ;; GLFWframebuffersizefun glfwSetFramebufferSizeCallback(GLFWwindow* window, GLFWframebuffersizefun callback)
  (define-cdecl void* glfwSetFramebufferSizeCallback (void* void*))
  ;; GLFWwindowcontentscalefun glfwSetWindowContentScaleCallback(GLFWwindow* window, GLFWwindowcontentscalefun callback)
  (define-cdecl void* glfwSetWindowContentScaleCallback (void* void*))
  ;; void glfwPollEvents(void)
  (define-cdecl void glfwPollEvents ())
  ;; void glfwWaitEvents(void)
  (define-cdecl void glfwWaitEvents ())
  ;; void glfwWaitEventsTimeout(double timeout)
  (define-cdecl void glfwWaitEventsTimeout (double))
  ;; void glfwPostEmptyEvent(void)
  (define-cdecl void glfwPostEmptyEvent ())
  ;; int glfwGetInputMode(GLFWwindow* window, int mode)
  (define-cdecl int glfwGetInputMode (void* int))
  ;; void glfwSetInputMode(GLFWwindow* window, int mode, int value)
  (define-cdecl void glfwSetInputMode (void* int int))
  ;; int glfwRawMouseMotionSupported(void)
  (define-cdecl int glfwRawMouseMotionSupported ())
  ;; const char* glfwGetKeyName(int key, int scancode)
  (define-cdecl void* glfwGetKeyName (int int))
  ;; int glfwGetKeyScancode(int key)
  (define-cdecl int glfwGetKeyScancode (int))
  ;; int glfwGetKey(GLFWwindow* window, int key)
  (define-cdecl int glfwGetKey (void* int))
  ;; int glfwGetMouseButton(GLFWwindow* window, int button)
  (define-cdecl int glfwGetMouseButton (void* int))
  ;; void glfwGetCursorPos(GLFWwindow* window, double* xpos, double* ypos)
  (define-cdecl void glfwGetCursorPos (void* void* void*))
  ;; void glfwSetCursorPos(GLFWwindow* window, double xpos, double ypos)
  (define-cdecl void glfwSetCursorPos (void* double double))
  ;; GLFWcursor* glfwCreateCursor(const GLFWimage* image, int xhot, int yhot)
  (define-cdecl void* glfwCreateCursor (void* int int))
  ;; GLFWcursor* glfwCreateStandardCursor(int shape)
  (define-cdecl void* glfwCreateStandardCursor (int))
  ;; void glfwDestroyCursor(GLFWcursor* cursor)
  (define-cdecl void glfwDestroyCursor (void*))
  ;; void glfwSetCursor(GLFWwindow* window, GLFWcursor* cursor)
  (define-cdecl void glfwSetCursor (void* void*))
  ;; GLFWkeyfun glfwSetKeyCallback(GLFWwindow* window, GLFWkeyfun callback)
  (define-cdecl void* glfwSetKeyCallback (void* void*))
  ;; GLFWcharfun glfwSetCharCallback(GLFWwindow* window, GLFWcharfun callback)
  (define-cdecl void* glfwSetCharCallback (void* void*))
  ;; GLFWcharmodsfun glfwSetCharModsCallback(GLFWwindow* window, GLFWcharmodsfun callback)
  (define-cdecl void* glfwSetCharModsCallback (void* void*))
  ;; GLFWmousebuttonfun glfwSetMouseButtonCallback(GLFWwindow* window, GLFWmousebuttonfun callback)
  (define-cdecl void* glfwSetMouseButtonCallback (void* void*))
  ;; GLFWcursorposfun glfwSetCursorPosCallback(GLFWwindow* window, GLFWcursorposfun callback)
  (define-cdecl void* glfwSetCursorPosCallback (void* void*))
  ;; GLFWcursorenterfun glfwSetCursorEnterCallback(GLFWwindow* window, GLFWcursorenterfun callback)
  (define-cdecl void* glfwSetCursorEnterCallback (void* void*))
  ;; GLFWscrollfun glfwSetScrollCallback(GLFWwindow* window, GLFWscrollfun callback)
  (define-cdecl void* glfwSetScrollCallback (void* void*))
  ;; GLFWdropfun glfwSetDropCallback(GLFWwindow* window, GLFWdropfun callback)
  (define-cdecl void* glfwSetDropCallback (void* void*))
  ;; int glfwJoystickPresent(int jid)
  (define-cdecl int glfwJoystickPresent (int))
  ;; const float* glfwGetJoystickAxes(int jid, int* count)
  (define-cdecl void* glfwGetJoystickAxes (int void*))
  ;; const unsigned char* glfwGetJoystickButtons(int jid, int* count)
  (define-cdecl void* glfwGetJoystickButtons (int void*))
  ;; const unsigned char* glfwGetJoystickHats(int jid, int* count)
  (define-cdecl void* glfwGetJoystickHats (int void*))
  ;; const char* glfwGetJoystickName(int jid)
  (define-cdecl void* glfwGetJoystickName (int))
  ;; const char* glfwGetJoystickGUID(int jid)
  (define-cdecl void* glfwGetJoystickGUID (int))
  ;; void glfwSetJoystickUserPointer(int jid, void* pointer)
  (define-cdecl void glfwSetJoystickUserPointer (int void*))
  ;; void* glfwGetJoystickUserPointer(int jid)
  (define-cdecl void* glfwGetJoystickUserPointer (int))
  ;; int glfwJoystickIsGamepad(int jid)
  (define-cdecl int glfwJoystickIsGamepad (int))
  ;; GLFWjoystickfun glfwSetJoystickCallback(GLFWjoystickfun callback)
  (define-cdecl void* glfwSetJoystickCallback (void*))
  ;; int glfwUpdateGamepadMappings(const char* string)
  (define-cdecl int glfwUpdateGamepadMappings (void*))
  ;; const char* glfwGetGamepadName(int jid)
  (define-cdecl void* glfwGetGamepadName (int))
  ;; int glfwGetGamepadState(int jid, GLFWgamepadstate* state)
  (define-cdecl int glfwGetGamepadState (int void*))
  ;; void glfwSetClipboardString(GLFWwindow* window, const char* string)
  (define-cdecl void glfwSetClipboardString (void* void*))
  ;; const char* glfwGetClipboardString(GLFWwindow* window)
  (define-cdecl void* glfwGetClipboardString (void*))
  ;; double glfwGetTime(void)
  (define-cdecl double glfwGetTime ())
  ;; void glfwSetTime(double time)
  (define-cdecl void glfwSetTime (double))
  ;; void glfwMakeContextCurrent(GLFWwindow* window)
  (define-cdecl void glfwMakeContextCurrent (void*))
  ;; GLFWwindow* glfwGetCurrentContext(void)
  (define-cdecl void* glfwGetCurrentContext ())
  ;; void glfwSwapBuffers(GLFWwindow* window)
  (define-cdecl void glfwSwapBuffers (void*))
  ;; void glfwSwapInterval(int interval)
  (define-cdecl void glfwSwapInterval (int))
  ;; int glfwExtensionSupported(const char* extension)
  (define-cdecl int glfwExtensionSupported (void*))
  ;; GLFWglproc glfwGetProcAddress(const char* procname)
  (define-cdecl void* glfwGetProcAddress (void*))
  ;; int glfwVulkanSupported(void)
  (define-cdecl int glfwVulkanSupported ())
  ;; const char** glfwGetRequiredInstanceExtensions(uint32_t* count)
  (define-cdecl void* glfwGetRequiredInstanceExtensions (void*))
  ;; GLFWvkproc glfwGetInstanceProcAddress(VkInstance instance, const char* procname)
  (define-cdecl void* glfwGetInstanceProcAddress (void* void*))
  ;; int glfwGetPhysicalDevicePresentationSupport(VkInstance instance, VkPhysicalDevice device, uint32_t queuefamily)
  (define-cdecl int glfwGetPhysicalDevicePresentationSupport (void* void* int))
  ;; VkResult glfwCreateWindowSurface(VkInstance instance, GLFWwindow* window, const VkAllocationCallbacks* allocator, VkSurfaceKHR* surface)
  (define-cdecl int glfwCreateWindowSurface (void* void* void* void*))
) ;[end]
