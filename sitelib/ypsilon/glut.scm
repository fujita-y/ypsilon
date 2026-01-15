#!nobacktrace
;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon glut)
  (export glutInit
          glutInitWindowPosition
          glutInitWindowSize
          glutInitDisplayMode
          glutInitDisplayString
          glutMainLoop
          glutCreateWindow
          glutCreateSubWindow
          glutDestroyWindow
          glutSetWindow
          glutGetWindow
          glutSetWindowTitle
          glutSetIconTitle
          glutReshapeWindow
          glutPositionWindow
          glutShowWindow
          glutHideWindow
          glutIconifyWindow
          glutPushWindow
          glutPopWindow
          glutFullScreen
          glutPostWindowRedisplay
          glutPostRedisplay
          glutSwapBuffers
          glutWarpPointer
          glutSetCursor
          glutEstablishOverlay
          glutRemoveOverlay
          glutUseLayer
          glutPostOverlayRedisplay
          glutPostWindowOverlayRedisplay
          glutShowOverlay
          glutHideOverlay
          glutCreateMenu
          glutDestroyMenu
          glutGetMenu
          glutSetMenu
          glutAddMenuEntry
          glutAddSubMenu
          glutChangeToMenuEntry
          glutChangeToSubMenu
          glutRemoveMenuItem
          glutAttachMenu
          glutDetachMenu
          glutTimerFunc
          glutIdleFunc
          glutKeyboardFunc
          glutSpecialFunc
          glutReshapeFunc
          glutVisibilityFunc
          glutDisplayFunc
          glutMouseFunc
          glutMotionFunc
          glutPassiveMotionFunc
          glutEntryFunc
          glutKeyboardUpFunc
          glutSpecialUpFunc
          glutJoystickFunc
          glutMenuStateFunc
          glutMenuStatusFunc
          glutOverlayDisplayFunc
          glutWindowStatusFunc
          glutSpaceballMotionFunc
          glutSpaceballRotateFunc
          glutSpaceballButtonFunc
          glutButtonBoxFunc
          glutDialsFunc
          glutTabletMotionFunc
          glutTabletButtonFunc
          glutGet
          glutDeviceGet
          glutGetModifiers
          glutLayerGet
          glutBitmapCharacter
          glutBitmapWidth
          glutStrokeCharacter
          glutStrokeWidth
          glutBitmapLength
          glutStrokeLength
          glutWireCube
          glutSolidCube
          glutWireSphere
          glutSolidSphere
          glutWireCone
          glutSolidCone
          glutWireTorus
          glutSolidTorus
          glutWireDodecahedron
          glutSolidDodecahedron
          glutWireOctahedron
          glutSolidOctahedron
          glutWireTetrahedron
          glutSolidTetrahedron
          glutWireIcosahedron
          glutSolidIcosahedron
          glutWireTeapot
          glutSolidTeapot
          glutGameModeString
          glutEnterGameMode
          glutLeaveGameMode
          glutGameModeGet
          glutVideoResizeGet
          glutSetupVideoResizing
          glutStopVideoResizing
          glutVideoResize
          glutVideoPan
          glutSetColor
          glutGetColor
          glutCopyColormap
          glutIgnoreKeyRepeat
          glutSetKeyRepeat
          glutForceJoystickFunc
          glutExtensionSupported
          glutReportErrors
          GLUT_API_VERSION
          GLUT_XLIB_IMPLEMENTATION
          GLUT_KEY_F1
          GLUT_KEY_F2
          GLUT_KEY_F3
          GLUT_KEY_F4
          GLUT_KEY_F5
          GLUT_KEY_F6
          GLUT_KEY_F7
          GLUT_KEY_F8
          GLUT_KEY_F9
          GLUT_KEY_F10
          GLUT_KEY_F11
          GLUT_KEY_F12
          GLUT_KEY_LEFT
          GLUT_KEY_UP
          GLUT_KEY_RIGHT
          GLUT_KEY_DOWN
          GLUT_KEY_PAGE_UP
          GLUT_KEY_PAGE_DOWN
          GLUT_KEY_HOME
          GLUT_KEY_END
          GLUT_KEY_INSERT
          GLUT_LEFT_BUTTON
          GLUT_MIDDLE_BUTTON
          GLUT_RIGHT_BUTTON
          GLUT_DOWN
          GLUT_UP
          GLUT_LEFT
          GLUT_ENTERED
          GLUT_RGB
          GLUT_RGBA
          GLUT_INDEX
          GLUT_SINGLE
          GLUT_DOUBLE
          GLUT_ACCUM
          GLUT_ALPHA
          GLUT_DEPTH
          GLUT_STENCIL
          GLUT_MULTISAMPLE
          GLUT_STEREO
          GLUT_LUMINANCE
          GLUT_MENU_NOT_IN_USE
          GLUT_MENU_IN_USE
          GLUT_NOT_VISIBLE
          GLUT_VISIBLE
          GLUT_HIDDEN
          GLUT_FULLY_RETAINED
          GLUT_PARTIALLY_RETAINED
          GLUT_FULLY_COVERED
          GLUT_WINDOW_X
          GLUT_WINDOW_Y
          GLUT_WINDOW_WIDTH
          GLUT_WINDOW_HEIGHT
          GLUT_WINDOW_BUFFER_SIZE
          GLUT_WINDOW_STENCIL_SIZE
          GLUT_WINDOW_DEPTH_SIZE
          GLUT_WINDOW_RED_SIZE
          GLUT_WINDOW_GREEN_SIZE
          GLUT_WINDOW_BLUE_SIZE
          GLUT_WINDOW_ALPHA_SIZE
          GLUT_WINDOW_ACCUM_RED_SIZE
          GLUT_WINDOW_ACCUM_GREEN_SIZE
          GLUT_WINDOW_ACCUM_BLUE_SIZE
          GLUT_WINDOW_ACCUM_ALPHA_SIZE
          GLUT_WINDOW_DOUBLEBUFFER
          GLUT_WINDOW_RGBA
          GLUT_WINDOW_PARENT
          GLUT_WINDOW_NUM_CHILDREN
          GLUT_WINDOW_COLORMAP_SIZE
          GLUT_WINDOW_NUM_SAMPLES
          GLUT_WINDOW_STEREO
          GLUT_WINDOW_CURSOR
          GLUT_SCREEN_WIDTH
          GLUT_SCREEN_HEIGHT
          GLUT_SCREEN_WIDTH_MM
          GLUT_SCREEN_HEIGHT_MM
          GLUT_MENU_NUM_ITEMS
          GLUT_DISPLAY_MODE_POSSIBLE
          GLUT_INIT_WINDOW_X
          GLUT_INIT_WINDOW_Y
          GLUT_INIT_WINDOW_WIDTH
          GLUT_INIT_WINDOW_HEIGHT
          GLUT_INIT_DISPLAY_MODE
          GLUT_ELAPSED_TIME
          GLUT_WINDOW_FORMAT_ID
          GLUT_HAS_KEYBOARD
          GLUT_HAS_MOUSE
          GLUT_HAS_SPACEBALL
          GLUT_HAS_DIAL_AND_BUTTON_BOX
          GLUT_HAS_TABLET
          GLUT_NUM_MOUSE_BUTTONS
          GLUT_NUM_SPACEBALL_BUTTONS
          GLUT_NUM_BUTTON_BOX_BUTTONS
          GLUT_NUM_DIALS
          GLUT_NUM_TABLET_BUTTONS
          GLUT_DEVICE_IGNORE_KEY_REPEAT
          GLUT_DEVICE_KEY_REPEAT
          GLUT_HAS_JOYSTICK
          GLUT_OWNS_JOYSTICK
          GLUT_JOYSTICK_BUTTONS
          GLUT_JOYSTICK_AXES
          GLUT_JOYSTICK_POLL_RATE
          GLUT_OVERLAY_POSSIBLE
          GLUT_LAYER_IN_USE
          GLUT_HAS_OVERLAY
          GLUT_TRANSPARENT_INDEX
          GLUT_NORMAL_DAMAGED
          GLUT_OVERLAY_DAMAGED
          GLUT_VIDEO_RESIZE_POSSIBLE
          GLUT_VIDEO_RESIZE_IN_USE
          GLUT_VIDEO_RESIZE_X_DELTA
          GLUT_VIDEO_RESIZE_Y_DELTA
          GLUT_VIDEO_RESIZE_WIDTH_DELTA
          GLUT_VIDEO_RESIZE_HEIGHT_DELTA
          GLUT_VIDEO_RESIZE_X
          GLUT_VIDEO_RESIZE_Y
          GLUT_VIDEO_RESIZE_WIDTH
          GLUT_VIDEO_RESIZE_HEIGHT
          GLUT_NORMAL
          GLUT_OVERLAY
          GLUT_ACTIVE_SHIFT
          GLUT_ACTIVE_CTRL
          GLUT_ACTIVE_ALT
          GLUT_CURSOR_RIGHT_ARROW
          GLUT_CURSOR_LEFT_ARROW
          GLUT_CURSOR_INFO
          GLUT_CURSOR_DESTROY
          GLUT_CURSOR_HELP
          GLUT_CURSOR_CYCLE
          GLUT_CURSOR_SPRAY
          GLUT_CURSOR_WAIT
          GLUT_CURSOR_TEXT
          GLUT_CURSOR_CROSSHAIR
          GLUT_CURSOR_UP_DOWN
          GLUT_CURSOR_LEFT_RIGHT
          GLUT_CURSOR_TOP_SIDE
          GLUT_CURSOR_BOTTOM_SIDE
          GLUT_CURSOR_LEFT_SIDE
          GLUT_CURSOR_RIGHT_SIDE
          GLUT_CURSOR_TOP_LEFT_CORNER
          GLUT_CURSOR_TOP_RIGHT_CORNER
          GLUT_CURSOR_BOTTOM_RIGHT_CORNER
          GLUT_CURSOR_BOTTOM_LEFT_CORNER
          GLUT_CURSOR_INHERIT
          GLUT_CURSOR_NONE
          GLUT_CURSOR_FULL_CROSSHAIR
          GLUT_RED
          GLUT_GREEN
          GLUT_BLUE
          GLUT_KEY_REPEAT_OFF
          GLUT_KEY_REPEAT_ON
          GLUT_KEY_REPEAT_DEFAULT
          GLUT_JOYSTICK_BUTTON_A
          GLUT_JOYSTICK_BUTTON_B
          GLUT_JOYSTICK_BUTTON_C
          GLUT_JOYSTICK_BUTTON_D
          GLUT_GAME_MODE_ACTIVE
          GLUT_GAME_MODE_POSSIBLE
          GLUT_GAME_MODE_WIDTH
          GLUT_GAME_MODE_HEIGHT
          GLUT_GAME_MODE_PIXEL_DEPTH
          GLUT_GAME_MODE_REFRESH_RATE
          GLUT_GAME_MODE_DISPLAY_CHANGED)
  (import (core) (ypsilon c-ffi))
  (define libGLUT
    (let ((sysname (architecture-feature 'sysname)))
      (cond ((string-contains sysname "darwin")
             (load-shared-object "GLUT.framework/GLUT"))
            ((string-contains sysname "linux")
             (load-shared-object "libglut.so.3"))
            (else
              (assertion-violation 'load-shared-object "can not load GLUT library, unknown operating system")))))
  (define-syntax define-cdecl
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function/weak ret name args)))))
  (define GLUT_API_VERSION 4)
  (define GLUT_XLIB_IMPLEMENTATION 13)
  (define GLUT_KEY_F1 #x0001)
  (define GLUT_KEY_F2 #x0002)
  (define GLUT_KEY_F3 #x0003)
  (define GLUT_KEY_F4 #x0004)
  (define GLUT_KEY_F5 #x0005)
  (define GLUT_KEY_F6 #x0006)
  (define GLUT_KEY_F7 #x0007)
  (define GLUT_KEY_F8 #x0008)
  (define GLUT_KEY_F9 #x0009)
  (define GLUT_KEY_F10 #x000A)
  (define GLUT_KEY_F11 #x000B)
  (define GLUT_KEY_F12 #x000C)
  (define GLUT_KEY_LEFT #x0064)
  (define GLUT_KEY_UP #x0065)
  (define GLUT_KEY_RIGHT #x0066)
  (define GLUT_KEY_DOWN #x0067)
  (define GLUT_KEY_PAGE_UP #x0068)
  (define GLUT_KEY_PAGE_DOWN #x0069)
  (define GLUT_KEY_HOME #x006A)
  (define GLUT_KEY_END #x006B)
  (define GLUT_KEY_INSERT #x006C)
  (define GLUT_LEFT_BUTTON #x0000)
  (define GLUT_MIDDLE_BUTTON #x0001)
  (define GLUT_RIGHT_BUTTON #x0002)
  (define GLUT_DOWN #x0000)
  (define GLUT_UP #x0001)
  (define GLUT_LEFT #x0000)
  (define GLUT_ENTERED #x0001)
  (define GLUT_RGB #x0000)
  (define GLUT_RGBA #x0000)
  (define GLUT_INDEX #x0001)
  (define GLUT_SINGLE #x0000)
  (define GLUT_DOUBLE #x0002)
  (define GLUT_ACCUM #x0004)
  (define GLUT_ALPHA #x0008)
  (define GLUT_DEPTH #x0010)
  (define GLUT_STENCIL #x0020)
  (define GLUT_MULTISAMPLE #x0080)
  (define GLUT_STEREO #x0100)
  (define GLUT_LUMINANCE #x0200)
  (define GLUT_MENU_NOT_IN_USE #x0000)
  (define GLUT_MENU_IN_USE #x0001)
  (define GLUT_NOT_VISIBLE #x0000)
  (define GLUT_VISIBLE #x0001)
  (define GLUT_HIDDEN #x0000)
  (define GLUT_FULLY_RETAINED #x0001)
  (define GLUT_PARTIALLY_RETAINED #x0002)
  (define GLUT_FULLY_COVERED #x0003)
  (define GLUT_WINDOW_X #x0064)
  (define GLUT_WINDOW_Y #x0065)
  (define GLUT_WINDOW_WIDTH #x0066)
  (define GLUT_WINDOW_HEIGHT #x0067)
  (define GLUT_WINDOW_BUFFER_SIZE #x0068)
  (define GLUT_WINDOW_STENCIL_SIZE #x0069)
  (define GLUT_WINDOW_DEPTH_SIZE #x006A)
  (define GLUT_WINDOW_RED_SIZE #x006B)
  (define GLUT_WINDOW_GREEN_SIZE #x006C)
  (define GLUT_WINDOW_BLUE_SIZE #x006D)
  (define GLUT_WINDOW_ALPHA_SIZE #x006E)
  (define GLUT_WINDOW_ACCUM_RED_SIZE #x006F)
  (define GLUT_WINDOW_ACCUM_GREEN_SIZE #x0070)
  (define GLUT_WINDOW_ACCUM_BLUE_SIZE #x0071)
  (define GLUT_WINDOW_ACCUM_ALPHA_SIZE #x0072)
  (define GLUT_WINDOW_DOUBLEBUFFER #x0073)
  (define GLUT_WINDOW_RGBA #x0074)
  (define GLUT_WINDOW_PARENT #x0075)
  (define GLUT_WINDOW_NUM_CHILDREN #x0076)
  (define GLUT_WINDOW_COLORMAP_SIZE #x0077)
  (define GLUT_WINDOW_NUM_SAMPLES #x0078)
  (define GLUT_WINDOW_STEREO #x0079)
  (define GLUT_WINDOW_CURSOR #x007A)
  (define GLUT_SCREEN_WIDTH #x00C8)
  (define GLUT_SCREEN_HEIGHT #x00C9)
  (define GLUT_SCREEN_WIDTH_MM #x00CA)
  (define GLUT_SCREEN_HEIGHT_MM #x00CB)
  (define GLUT_MENU_NUM_ITEMS #x012C)
  (define GLUT_DISPLAY_MODE_POSSIBLE #x0190)
  (define GLUT_INIT_WINDOW_X #x01F4)
  (define GLUT_INIT_WINDOW_Y #x01F5)
  (define GLUT_INIT_WINDOW_WIDTH #x01F6)
  (define GLUT_INIT_WINDOW_HEIGHT #x01F7)
  (define GLUT_INIT_DISPLAY_MODE #x01F8)
  (define GLUT_ELAPSED_TIME #x02BC)
  (define GLUT_WINDOW_FORMAT_ID #x007B)
  (define GLUT_HAS_KEYBOARD #x0258)
  (define GLUT_HAS_MOUSE #x0259)
  (define GLUT_HAS_SPACEBALL #x025A)
  (define GLUT_HAS_DIAL_AND_BUTTON_BOX #x025B)
  (define GLUT_HAS_TABLET #x025C)
  (define GLUT_NUM_MOUSE_BUTTONS #x025D)
  (define GLUT_NUM_SPACEBALL_BUTTONS #x025E)
  (define GLUT_NUM_BUTTON_BOX_BUTTONS #x025F)
  (define GLUT_NUM_DIALS #x0260)
  (define GLUT_NUM_TABLET_BUTTONS #x0261)
  (define GLUT_DEVICE_IGNORE_KEY_REPEAT #x0262)
  (define GLUT_DEVICE_KEY_REPEAT #x0263)
  (define GLUT_HAS_JOYSTICK #x0264)
  (define GLUT_OWNS_JOYSTICK #x0265)
  (define GLUT_JOYSTICK_BUTTONS #x0266)
  (define GLUT_JOYSTICK_AXES #x0267)
  (define GLUT_JOYSTICK_POLL_RATE #x0268)
  (define GLUT_OVERLAY_POSSIBLE #x0320)
  (define GLUT_LAYER_IN_USE #x0321)
  (define GLUT_HAS_OVERLAY #x0322)
  (define GLUT_TRANSPARENT_INDEX #x0323)
  (define GLUT_NORMAL_DAMAGED #x0324)
  (define GLUT_OVERLAY_DAMAGED #x0325)
  (define GLUT_VIDEO_RESIZE_POSSIBLE #x0384)
  (define GLUT_VIDEO_RESIZE_IN_USE #x0385)
  (define GLUT_VIDEO_RESIZE_X_DELTA #x0386)
  (define GLUT_VIDEO_RESIZE_Y_DELTA #x0387)
  (define GLUT_VIDEO_RESIZE_WIDTH_DELTA #x0388)
  (define GLUT_VIDEO_RESIZE_HEIGHT_DELTA #x0389)
  (define GLUT_VIDEO_RESIZE_X #x038A)
  (define GLUT_VIDEO_RESIZE_Y #x038B)
  (define GLUT_VIDEO_RESIZE_WIDTH #x038C)
  (define GLUT_VIDEO_RESIZE_HEIGHT #x038D)
  (define GLUT_NORMAL #x0000)
  (define GLUT_OVERLAY #x0001)
  (define GLUT_ACTIVE_SHIFT #x0001)
  (define GLUT_ACTIVE_CTRL #x0002)
  (define GLUT_ACTIVE_ALT #x0004)
  (define GLUT_CURSOR_RIGHT_ARROW #x0000)
  (define GLUT_CURSOR_LEFT_ARROW #x0001)
  (define GLUT_CURSOR_INFO #x0002)
  (define GLUT_CURSOR_DESTROY #x0003)
  (define GLUT_CURSOR_HELP #x0004)
  (define GLUT_CURSOR_CYCLE #x0005)
  (define GLUT_CURSOR_SPRAY #x0006)
  (define GLUT_CURSOR_WAIT #x0007)
  (define GLUT_CURSOR_TEXT #x0008)
  (define GLUT_CURSOR_CROSSHAIR #x0009)
  (define GLUT_CURSOR_UP_DOWN #x000A)
  (define GLUT_CURSOR_LEFT_RIGHT #x000B)
  (define GLUT_CURSOR_TOP_SIDE #x000C)
  (define GLUT_CURSOR_BOTTOM_SIDE #x000D)
  (define GLUT_CURSOR_LEFT_SIDE #x000E)
  (define GLUT_CURSOR_RIGHT_SIDE #x000F)
  (define GLUT_CURSOR_TOP_LEFT_CORNER #x0010)
  (define GLUT_CURSOR_TOP_RIGHT_CORNER #x0011)
  (define GLUT_CURSOR_BOTTOM_RIGHT_CORNER #x0012)
  (define GLUT_CURSOR_BOTTOM_LEFT_CORNER #x0013)
  (define GLUT_CURSOR_INHERIT #x0064)
  (define GLUT_CURSOR_NONE #x0065)
  (define GLUT_CURSOR_FULL_CROSSHAIR #x0066)
  (define GLUT_RED #x0000)
  (define GLUT_GREEN #x0001)
  (define GLUT_BLUE #x0002)
  (define GLUT_KEY_REPEAT_OFF #x0000)
  (define GLUT_KEY_REPEAT_ON #x0001)
  (define GLUT_KEY_REPEAT_DEFAULT #x0002)
  (define GLUT_JOYSTICK_BUTTON_A #x0001)
  (define GLUT_JOYSTICK_BUTTON_B #x0002)
  (define GLUT_JOYSTICK_BUTTON_C #x0004)
  (define GLUT_JOYSTICK_BUTTON_D #x0008)
  (define GLUT_GAME_MODE_ACTIVE #x0000)
  (define GLUT_GAME_MODE_POSSIBLE #x0001)
  (define GLUT_GAME_MODE_WIDTH #x0002)
  (define GLUT_GAME_MODE_HEIGHT #x0003)
  (define GLUT_GAME_MODE_PIXEL_DEPTH #x0004)
  (define GLUT_GAME_MODE_REFRESH_RATE #x0005)
  (define GLUT_GAME_MODE_DISPLAY_CHANGED #x0006)
  ;; void glutInit(int* pargc, char** argv)
  (define-cdecl void glutInit (void* void*))
  ;; void glutInitWindowPosition(int x, int y)
  (define-cdecl void glutInitWindowPosition (int int))
  ;; void glutInitWindowSize(int width, int height)
  (define-cdecl void glutInitWindowSize (int int))
  ;; void glutInitDisplayMode(unsigned int displayMode)
  (define-cdecl void glutInitDisplayMode (unsigned-int))
  ;; void glutInitDisplayString(const char* displayMode)
  (define-cdecl void glutInitDisplayString (void*))
  ;; void glutMainLoop(void)
  (define-cdecl void glutMainLoop ())
  ;; int glutCreateWindow(const char* title)
  (define-cdecl int glutCreateWindow (void*))
  ;; int glutCreateSubWindow(int window, int x, int y, int width, int height)
  (define-cdecl int glutCreateSubWindow (int int int int int))
  ;; void glutDestroyWindow(int window)
  (define-cdecl void glutDestroyWindow (int))
  ;; void glutSetWindow(int window)
  (define-cdecl void glutSetWindow (int))
  ;; int glutGetWindow(void)
  (define-cdecl int glutGetWindow ())
  ;; void glutSetWindowTitle(const char* title)
  (define-cdecl void glutSetWindowTitle (void*))
  ;; void glutSetIconTitle(const char* title)
  (define-cdecl void glutSetIconTitle (void*))
  ;; void glutReshapeWindow(int width, int height)
  (define-cdecl void glutReshapeWindow (int int))
  ;; void glutPositionWindow(int x, int y)
  (define-cdecl void glutPositionWindow (int int))
  ;; void glutShowWindow(void)
  (define-cdecl void glutShowWindow ())
  ;; void glutHideWindow(void)
  (define-cdecl void glutHideWindow ())
  ;; void glutIconifyWindow(void)
  (define-cdecl void glutIconifyWindow ())
  ;; void glutPushWindow(void)
  (define-cdecl void glutPushWindow ())
  ;; void glutPopWindow(void)
  (define-cdecl void glutPopWindow ())
  ;; void glutFullScreen(void)
  (define-cdecl void glutFullScreen ())
  ;; void glutPostWindowRedisplay(int window)
  (define-cdecl void glutPostWindowRedisplay (int))
  ;; void glutPostRedisplay(void)
  (define-cdecl void glutPostRedisplay ())
  ;; void glutSwapBuffers(void)
  (define-cdecl void glutSwapBuffers ())
  ;; void glutWarpPointer(int x, int y)
  (define-cdecl void glutWarpPointer (int int))
  ;; void glutSetCursor(int cursor)
  (define-cdecl void glutSetCursor (int))
  ;; void glutEstablishOverlay(void)
  (define-cdecl void glutEstablishOverlay ())
  ;; void glutRemoveOverlay(void)
  (define-cdecl void glutRemoveOverlay ())
  ;; void glutUseLayer(GLenum layer)
  (define-cdecl void glutUseLayer (unsigned-int))
  ;; void glutPostOverlayRedisplay(void)
  (define-cdecl void glutPostOverlayRedisplay ())
  ;; void glutPostWindowOverlayRedisplay(int window)
  (define-cdecl void glutPostWindowOverlayRedisplay (int))
  ;; void glutShowOverlay(void)
  (define-cdecl void glutShowOverlay ())
  ;; void glutHideOverlay(void)
  (define-cdecl void glutHideOverlay ())
  ;; int glutCreateMenu(void (* callback)( int menu ))
  (define-cdecl int glutCreateMenu (void*))
  ;; void glutDestroyMenu(int menu)
  (define-cdecl void glutDestroyMenu (int))
  ;; int glutGetMenu(void)
  (define-cdecl int glutGetMenu ())
  ;; void glutSetMenu(int menu)
  (define-cdecl void glutSetMenu (int))
  ;; void glutAddMenuEntry(const char* label, int value)
  (define-cdecl void glutAddMenuEntry (void* int))
  ;; void glutAddSubMenu(const char* label, int subMenu)
  (define-cdecl void glutAddSubMenu (void* int))
  ;; void glutChangeToMenuEntry(int item, const char* label, int value)
  (define-cdecl void glutChangeToMenuEntry (int void* int))
  ;; void glutChangeToSubMenu(int item, const char* label, int value)
  (define-cdecl void glutChangeToSubMenu (int void* int))
  ;; void glutRemoveMenuItem(int item)
  (define-cdecl void glutRemoveMenuItem (int))
  ;; void glutAttachMenu(int button)
  (define-cdecl void glutAttachMenu (int))
  ;; void glutDetachMenu(int button)
  (define-cdecl void glutDetachMenu (int))
  ;; void glutTimerFunc(unsigned int time, void (* callback)( int ), int value)
  (define-cdecl void glutTimerFunc (unsigned-int void* int))
  ;; void glutIdleFunc(void (* callback)( void ))
  (define-cdecl void glutIdleFunc (void*))
  ;; void glutKeyboardFunc(void (* callback)( unsigned char, int, int ))
  (define-cdecl void glutKeyboardFunc (void*))
  ;; void glutSpecialFunc(void (* callback)( int, int, int ))
  (define-cdecl void glutSpecialFunc (void*))
  ;; void glutReshapeFunc(void (* callback)( int, int ))
  (define-cdecl void glutReshapeFunc (void*))
  ;; void glutVisibilityFunc(void (* callback)( int ))
  (define-cdecl void glutVisibilityFunc (void*))
  ;; void glutDisplayFunc(void (* callback)( void ))
  (define-cdecl void glutDisplayFunc (void*))
  ;; void glutMouseFunc(void (* callback)( int, int, int, int ))
  (define-cdecl void glutMouseFunc (void*))
  ;; void glutMotionFunc(void (* callback)( int, int ))
  (define-cdecl void glutMotionFunc (void*))
  ;; void glutPassiveMotionFunc(void (* callback)( int, int ))
  (define-cdecl void glutPassiveMotionFunc (void*))
  ;; void glutEntryFunc(void (* callback)( int ))
  (define-cdecl void glutEntryFunc (void*))
  ;; void glutKeyboardUpFunc(void (* callback)( unsigned char, int, int ))
  (define-cdecl void glutKeyboardUpFunc (void*))
  ;; void glutSpecialUpFunc(void (* callback)( int, int, int ))
  (define-cdecl void glutSpecialUpFunc (void*))
  ;; void glutJoystickFunc(void (* callback)( unsigned int, int, int, int ), int pollInterval)
  (define-cdecl void glutJoystickFunc (void* int))
  ;; void glutMenuStateFunc(void (* callback)( int ))
  (define-cdecl void glutMenuStateFunc (void*))
  ;; void glutMenuStatusFunc(void (* callback)( int, int, int ))
  (define-cdecl void glutMenuStatusFunc (void*))
  ;; void glutOverlayDisplayFunc(void (* callback)( void ))
  (define-cdecl void glutOverlayDisplayFunc (void*))
  ;; void glutWindowStatusFunc(void (* callback)( int ))
  (define-cdecl void glutWindowStatusFunc (void*))
  ;; void glutSpaceballMotionFunc(void (* callback)( int, int, int ))
  (define-cdecl void glutSpaceballMotionFunc (void*))
  ;; void glutSpaceballRotateFunc(void (* callback)( int, int, int ))
  (define-cdecl void glutSpaceballRotateFunc (void*))
  ;; void glutSpaceballButtonFunc(void (* callback)( int, int ))
  (define-cdecl void glutSpaceballButtonFunc (void*))
  ;; void glutButtonBoxFunc(void (* callback)( int, int ))
  (define-cdecl void glutButtonBoxFunc (void*))
  ;; void glutDialsFunc(void (* callback)( int, int ))
  (define-cdecl void glutDialsFunc (void*))
  ;; void glutTabletMotionFunc(void (* callback)( int, int ))
  (define-cdecl void glutTabletMotionFunc (void*))
  ;; void glutTabletButtonFunc(void (* callback)( int, int, int, int ))
  (define-cdecl void glutTabletButtonFunc (void*))
  ;; int glutGet(GLenum query)
  (define-cdecl int glutGet (unsigned-int))
  ;; int glutDeviceGet(GLenum query)
  (define-cdecl int glutDeviceGet (unsigned-int))
  ;; int glutGetModifiers(void)
  (define-cdecl int glutGetModifiers ())
  ;; int glutLayerGet(GLenum query)
  (define-cdecl int glutLayerGet (unsigned-int))
  ;; void glutBitmapCharacter(void* font, int character)
  (define-cdecl void glutBitmapCharacter (void* int))
  ;; int glutBitmapWidth(void* font, int character)
  (define-cdecl int glutBitmapWidth (void* int))
  ;; void glutStrokeCharacter(void* font, int character)
  (define-cdecl void glutStrokeCharacter (void* int))
  ;; int glutStrokeWidth(void* font, int character)
  (define-cdecl int glutStrokeWidth (void* int))
  ;; int glutBitmapLength(void* font, const unsigned char* string)
  (define-cdecl int glutBitmapLength (void* void*))
  ;; int glutStrokeLength(void* font, const unsigned char* string)
  (define-cdecl int glutStrokeLength (void* void*))
  ;; void glutWireCube(GLdouble size)
  (define-cdecl void glutWireCube (double))
  ;; void glutSolidCube(GLdouble size)
  (define-cdecl void glutSolidCube (double))
  ;; void glutWireSphere(GLdouble radius, GLint slices, GLint stacks)
  (define-cdecl void glutWireSphere (double int int))
  ;; void glutSolidSphere(GLdouble radius, GLint slices, GLint stacks)
  (define-cdecl void glutSolidSphere (double int int))
  ;; void glutWireCone(GLdouble base, GLdouble height, GLint slices, GLint stacks)
  (define-cdecl void glutWireCone (double double int int))
  ;; void glutSolidCone(GLdouble base, GLdouble height, GLint slices, GLint stacks)
  (define-cdecl void glutSolidCone (double double int int))
  ;; void glutWireTorus(GLdouble innerRadius, GLdouble outerRadius, GLint sides, GLint rings)
  (define-cdecl void glutWireTorus (double double int int))
  ;; void glutSolidTorus(GLdouble innerRadius, GLdouble outerRadius, GLint sides, GLint rings)
  (define-cdecl void glutSolidTorus (double double int int))
  ;; void glutWireDodecahedron(void)
  (define-cdecl void glutWireDodecahedron ())
  ;; void glutSolidDodecahedron(void)
  (define-cdecl void glutSolidDodecahedron ())
  ;; void glutWireOctahedron(void)
  (define-cdecl void glutWireOctahedron ())
  ;; void glutSolidOctahedron(void)
  (define-cdecl void glutSolidOctahedron ())
  ;; void glutWireTetrahedron(void)
  (define-cdecl void glutWireTetrahedron ())
  ;; void glutSolidTetrahedron(void)
  (define-cdecl void glutSolidTetrahedron ())
  ;; void glutWireIcosahedron(void)
  (define-cdecl void glutWireIcosahedron ())
  ;; void glutSolidIcosahedron(void)
  (define-cdecl void glutSolidIcosahedron ())
  ;; void glutWireTeapot(GLdouble size)
  (define-cdecl void glutWireTeapot (double))
  ;; void glutSolidTeapot(GLdouble size)
  (define-cdecl void glutSolidTeapot (double))
  ;; void glutGameModeString(const char* string)
  (define-cdecl void glutGameModeString (void*))
  ;; int glutEnterGameMode(void)
  (define-cdecl int glutEnterGameMode ())
  ;; void glutLeaveGameMode(void)
  (define-cdecl void glutLeaveGameMode ())
  ;; int glutGameModeGet(GLenum query)
  (define-cdecl int glutGameModeGet (unsigned-int))
  ;; int glutVideoResizeGet(GLenum query)
  (define-cdecl int glutVideoResizeGet (unsigned-int))
  ;; void glutSetupVideoResizing(void)
  (define-cdecl void glutSetupVideoResizing ())
  ;; void glutStopVideoResizing(void)
  (define-cdecl void glutStopVideoResizing ())
  ;; void glutVideoResize(int x, int y, int width, int height)
  (define-cdecl void glutVideoResize (int int int int))
  ;; void glutVideoPan(int x, int y, int width, int height)
  (define-cdecl void glutVideoPan (int int int int))
  ;; void glutSetColor(int color, GLfloat red, GLfloat green, GLfloat blue)
  (define-cdecl void glutSetColor (int float float float))
  ;; GLfloat glutGetColor(int color, int component)
  (define-cdecl float glutGetColor (int int))
  ;; void glutCopyColormap(int window)
  (define-cdecl void glutCopyColormap (int))
  ;; void glutIgnoreKeyRepeat(int ignore)
  (define-cdecl void glutIgnoreKeyRepeat (int))
  ;; void glutSetKeyRepeat(int repeatMode)
  (define-cdecl void glutSetKeyRepeat (int))
  ;; void glutForceJoystickFunc(void)
  (define-cdecl void glutForceJoystickFunc ())
  ;; int glutExtensionSupported(const char* extension)
  (define-cdecl int glutExtensionSupported (void*))
  ;; void glutReportErrors(void)
  (define-cdecl void glutReportErrors ())
) ;[end]
