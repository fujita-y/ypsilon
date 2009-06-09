#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon sdl base)
  (export SDL_AllocRW
          SDL_AudioDriverName
          SDL_AudioInit
          SDL_AudioQuit
          SDL_BuildAudioCVT
          SDL_BlitSurface
          SDL_CDClose
          SDL_CDEject
          SDL_CDName
          SDL_CDNumDrives
          SDL_CDOpen
          SDL_CDPause
          SDL_CDPlay
          SDL_CDPlayTracks
          SDL_CDResume
          SDL_CDStatus
          SDL_CDStop
          SDL_ClearError
          SDL_CloseAudio
          SDL_CondBroadcast
          SDL_CondSignal
          SDL_CondWait
          SDL_CondWaitTimeout
          SDL_ConvertAudio
          SDL_ConvertSurface
          SDL_CreateCond
          SDL_CreateCursor
          SDL_CreateMutex
          SDL_CreateRGBSurface
          SDL_CreateRGBSurfaceFrom
          SDL_CreateSemaphore
          SDL_CreateThread
          SDL_CreateYUVOverlay
          SDL_Delay
          SDL_DestroyCond
          SDL_DestroyMutex
          SDL_DestroySemaphore
          SDL_DisplayFormat
          SDL_DisplayFormatAlpha
          SDL_DisplayYUVOverlay
          SDL_EnableKeyRepeat
          SDL_EnableUNICODE
          SDL_Error
          SDL_EventState
          SDL_FillRect
          SDL_Flip
          SDL_FreeCursor
          SDL_FreeRW
          SDL_FreeSurface
          SDL_FreeWAV
          SDL_FreeYUVOverlay
          SDL_GL_GetAttribute
          SDL_GL_GetProcAddress
          SDL_GL_LoadLibrary
          SDL_GL_Lock
          SDL_GL_SetAttribute
          SDL_GL_SwapBuffers
          SDL_GL_Unlock
          SDL_GL_UpdateRects
          SDL_GetAppState
          SDL_GetAudioStatus
          SDL_GetClipRect
          SDL_GetCursor
          SDL_GetError
          SDL_GetEventFilter
          SDL_GetGammaRamp
          SDL_GetKeyName
          SDL_GetKeyRepeat
          SDL_GetKeyState
          SDL_GetModState
          SDL_GetMouseState
          SDL_GetRGB
          SDL_GetRGBA
          SDL_GetRelativeMouseState
          SDL_GetThreadID
          SDL_GetTicks
          SDL_GetVideoInfo
          SDL_GetVideoSurface
          SDL_Has3DNow
          SDL_Has3DNowExt
          SDL_HasAltiVec
          SDL_HasMMX
          SDL_HasMMXExt
          SDL_HasRDTSC
          SDL_HasSSE
          SDL_HasSSE2
          SDL_Init
          SDL_InitSubSystem
          SDL_JoystickClose
          SDL_JoystickEventState
          SDL_JoystickGetAxis
          SDL_JoystickGetBall
          SDL_JoystickGetButton
          SDL_JoystickGetHat
          SDL_JoystickIndex
          SDL_JoystickName
          SDL_JoystickNumAxes
          SDL_JoystickNumBalls
          SDL_JoystickNumButtons
          SDL_JoystickNumHats
          SDL_JoystickOpen
          SDL_JoystickOpened
          SDL_JoystickUpdate
          SDL_KillThread
          SDL_Linked_Version
          SDL_ListModes
          SDL_LoadBMP_RW
          SDL_LoadFunction
          SDL_LoadObject
          SDL_LoadWAV_RW
          SDL_LockAudio
          SDL_LockSurface
          SDL_LockYUVOverlay
          SDL_LowerBlit
          SDL_MapRGB
          SDL_MapRGBA
          SDL_MixAudio
          SDL_NumJoysticks
          SDL_OpenAudio
          SDL_PauseAudio
          SDL_PeepEvents
          SDL_PollEvent
          SDL_PumpEvents
          SDL_PushEvent
          SDL_Quit
          SDL_QuitSubSystem
          SDL_RWFromConstMem
          SDL_RWFromFP
          SDL_RWFromFile
          SDL_RWFromMem
          SDL_ReadBE16
          SDL_ReadBE32
          SDL_ReadBE64
          SDL_ReadLE16
          SDL_ReadLE32
          SDL_ReadLE64
          SDL_RemoveTimer
          SDL_SaveBMP_RW
          SDL_SemPost
          SDL_SemTryWait
          SDL_SemValue
          SDL_SemWait
          SDL_SemWaitTimeout
          SDL_SetAlpha
          SDL_SetClipRect
          SDL_SetColorKey
          SDL_SetColors
          SDL_SetCursor
          SDL_SetError
          SDL_SetEventFilter
          SDL_SetGamma
          SDL_SetGammaRamp
          SDL_SetModState
          SDL_SetPalette
          SDL_SetVideoMode
          SDL_ShowCursor
          SDL_SoftStretch
          SDL_ThreadID
          SDL_UnloadObject
          SDL_UnlockAudio
          SDL_UnlockSurface
          SDL_UnlockYUVOverlay
          SDL_UpdateRect
          SDL_UpdateRects
          SDL_UpperBlit
          SDL_VideoDriverName
          SDL_VideoInit
          SDL_VideoModeOK
          SDL_VideoQuit
          SDL_WM_GetCaption
          SDL_WM_GrabInput
          SDL_WM_IconifyWindow
          SDL_WM_SetCaption
          SDL_WM_SetIcon
          SDL_WM_ToggleFullScreen
          SDL_WaitEvent
          SDL_WaitThread
          SDL_WarpMouse
          SDL_WasInit
          SDL_WriteBE16
          SDL_WriteBE32
          SDL_WriteBE64
          SDL_WriteLE16
          SDL_WriteLE32
          SDL_WriteLE64
          SDL_iconv
          SDL_iconv_string
          SDL_lltoa
          SDL_ltoa
          SDL_mutexP
          SDL_mutexV
          SDL_strlcat
          SDL_strlcpy
          SDL_strlwr
          SDL_strrev
          SDL_strupr
          SDL_ulltoa
          SDL_ultoa)

  ;; SDL_AddTimer -- incompatible thread callback
  ;; SDL_SetTimer -- incompatible thread callback

  (import (rnrs)
          (ypsilon ffi)
          (only (core) system-extension-path))

  (define lib-name
    (cond (on-linux   "libSDL.so")
          (on-sunos   "libSDL.so")
          (on-freebsd "libSDL.so")
          (on-openbsd "libSDL.so")
          (on-darwin  "SDL.framework/SDL")
          (on-windows "SDL.dll")
          (else
           (assertion-violation #f "can not locate SDL library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; int SDL_Init(Uint32 flags)
  ;;(define-function int SDL_Init (uint32_t))
  (define SDL_Init
    (lambda (arg)
      (if on-darwin
          (let ((libmain (load-shared-object (string-append (system-extension-path) "/SDLMain.dylib"))))
            ((c-function libmain "SDLMain.dylib" int main ([int] [*(char*)]))
             (vector (length (command-line)))
             (apply vector (command-line)))
            ((c-function lib lib-name int SDL_Init (uint32_t)) arg))
          ((c-function lib lib-name int SDL_Init (uint32_t)) arg))))

  ;; SDL_TimerID SDL_AddTimer(Uint32 interval, SDL_NewTimerCallback callback, void* param)
  ;; Uint32 (*SDL_NewTimerCallback)(Uint32 interval, void* param)
  (define-function void* SDL_AddTimer (uint32_t [c-callback uint32_t (uint32_t void*)] void*))

  ;; SDL_RWops* SDL_AllocRW(void)
  (define-function void* SDL_AllocRW ())

  ;; char* SDL_AudioDriverName(char* namebuf, int maxlen)
  (define-function char* SDL_AudioDriverName (char* int))

  ;; int SDL_AudioInit(const char* driver_name)
  (define-function int SDL_AudioInit (char*))

  ;; void SDL_AudioQuit(void)
  (define-function void SDL_AudioQuit ())

  ;; int SDL_BuildAudioCVT(SDL_AudioCVT* cvt, Uint16 src_format, Uint8 src_channels, int src_rate, Uint16 dst_format, Uint8 dst_channels, int dst_rate)
  (define-function int SDL_BuildAudioCVT (void* uint16_t uint8_t int uint16_t uint8_t int))

  ;; void SDL_CDClose(SDL_CD* cdrom)
  (define-function void SDL_CDClose (void*))

  ;; int SDL_CDEject(SDL_CD* cdrom)
  (define-function int SDL_CDEject (void*))

  ;; const char* SDL_CDName(int drive)
  (define-function char* SDL_CDName (int))

  ;; int SDL_CDNumDrives(void)
  (define-function int SDL_CDNumDrives ())

  ;; SDL_CD* SDL_CDOpen(int drive)
  (define-function void* SDL_CDOpen (int))

  ;; int SDL_CDPause(SDL_CD* cdrom)
  (define-function int SDL_CDPause (void*))

  ;; int SDL_CDPlay(SDL_CD* cdrom, int start, int length)
  (define-function int SDL_CDPlay (void* int int))

  ;; int SDL_CDPlayTracks(SDL_CD* cdrom, int start_track, int start_frame, int ntracks, int nframes)
  (define-function int SDL_CDPlayTracks (void* int int int int))

  ;; int SDL_CDResume(SDL_CD* cdrom)
  (define-function int SDL_CDResume (void*))

  ;; CDstatus SDL_CDStatus(SDL_CD* cdrom)
  (define-function int SDL_CDStatus (void*))

  ;; int SDL_CDStop(SDL_CD* cdrom)
  (define-function int SDL_CDStop (void*))

  ;; void SDL_ClearError(void)
  (define-function void SDL_ClearError ())

  ;; void SDL_CloseAudio(void)
  (define-function void SDL_CloseAudio ())

  ;; int SDL_CondBroadcast(SDL_cond* cond)
  (define-function int SDL_CondBroadcast (void*))

  ;; int SDL_CondSignal(SDL_cond* cond)
  (define-function int SDL_CondSignal (void*))

  ;; int SDL_CondWait(SDL_cond* cond, SDL_mutex* mut)
  (define-function int SDL_CondWait (void* void*))

  ;; int SDL_CondWaitTimeout(SDL_cond* cond, SDL_mutex* mutex, Uint32 ms)
  (define-function int SDL_CondWaitTimeout (void* void* uint32_t))

  ;; int SDL_ConvertAudio(SDL_AudioCVT* cvt)
  (define-function int SDL_ConvertAudio (void*))

  ;; SDL_Surface* SDL_ConvertSurface (SDL_Surface* src, SDL_PixelFormat* fmt, Uint32 flags)
  (define-function void* SDL_ConvertSurface (void* void* uint32_t))

  ;; SDL_cond* SDL_CreateCond(void)
  (define-function void* SDL_CreateCond ())

  ;; SDL_Cursor* SDL_CreateCursor (Uint8* data, Uint8* mask, int w, int h, int hot_x, int hot_y)
  (define-function void* SDL_CreateCursor (void* void* int int int int))

  ;; SDL_mutex* SDL_CreateMutex(void)
  (define-function void* SDL_CreateMutex ())

  ;; SDL_Surface* SDL_CreateRGBSurface (Uint32 flags, int width, int height, int depth, Uint32 Rmask, Uint32 Gmask, Uint32 Bmask, Uint32 Amask)
  (define-function void* SDL_CreateRGBSurface (uint32_t int int int uint32_t uint32_t uint32_t uint32_t))

  ;; SDL_Surface* SDL_CreateRGBSurfaceFrom(void* pixels, int width, int height, int depth, int pitch, Uint32 Rmask, Uint32 Gmask, Uint32 Bmask, Uint32 Amask)
  (define-function void* SDL_CreateRGBSurfaceFrom (void* int int int int uint32_t uint32_t uint32_t uint32_t))

  ;; SDL_sem* SDL_CreateSemaphore(Uint32 initial_value)
  (define-function void* SDL_CreateSemaphore (uint32_t))

  ;; SDL_Thread* SDL_CreateThread(int (* fn)(void* ), void* data)
  (define-function void* SDL_CreateThread ([c-callback int (void*)] void*))

  ;; SDL_Overlay* SDL_CreateYUVOverlay(int width, int height, Uint32 format, SDL_Surface* display)
  (define-function void* SDL_CreateYUVOverlay (int int uint32_t void*))

  ;; void SDL_Delay(Uint32 ms)
  (define-function void SDL_Delay (uint32_t))

  ;; void SDL_DestroyCond(SDL_cond* cond)
  (define-function void SDL_DestroyCond (void*))

  ;; void SDL_DestroyMutex(SDL_mutex* mutex)
  (define-function void SDL_DestroyMutex (void*))

  ;; void SDL_DestroySemaphore(SDL_sem* sem)
  (define-function void SDL_DestroySemaphore (void*))

  ;; SDL_Surface* SDL_DisplayFormat(SDL_Surface* surface)
  (define-function void* SDL_DisplayFormat (void*))

  ;; SDL_Surface* SDL_DisplayFormatAlpha(SDL_Surface* surface)
  (define-function void* SDL_DisplayFormatAlpha (void*))

  ;; int SDL_DisplayYUVOverlay(SDL_Overlay* overlay, SDL_Rect* dstrect)
  (define-function int SDL_DisplayYUVOverlay (void* void*))

  ;; int SDL_EnableKeyRepeat(int delay, int interval)
  (define-function int SDL_EnableKeyRepeat (int int))

  ;; int SDL_EnableUNICODE(int enable)
  (define-function int SDL_EnableUNICODE (int))

  ;; void SDL_Error(SDL_errorcode code)
  (define-function void SDL_Error (int))

  ;; Uint8 SDL_EventState(Uint8 type, int state)
  (define-function uint8_t SDL_EventState (uint8_t int))

  ;; int SDL_FillRect (SDL_Surface* dst, SDL_Rect* dstrect, Uint32 color)
  (define-function int SDL_FillRect (void* void* uint32_t))

  ;; int SDL_Flip(SDL_Surface* screen)
  (define-function int SDL_Flip (void*))

  ;; void SDL_FreeCursor(SDL_Cursor* cursor)
  (define-function void SDL_FreeCursor (void*))

  ;; void SDL_FreeRW(SDL_RWops* area)
  (define-function void SDL_FreeRW (void*))

  ;; void SDL_FreeSurface(SDL_Surface* surface)
  (define-function void SDL_FreeSurface (void*))

  ;; void SDL_FreeWAV(Uint8* audio_buf)
  (define-function void SDL_FreeWAV (void*))

  ;; void SDL_FreeYUVOverlay(SDL_Overlay* overlay)
  (define-function void SDL_FreeYUVOverlay (void*))

  ;; int SDL_GL_GetAttribute(SDL_GLattr attr, int* value)
  (define-function int SDL_GL_GetAttribute (int void*))

  ;; void* SDL_GL_GetProcAddress(const char* proc)
  (define-function void* SDL_GL_GetProcAddress (char*))

  ;; int SDL_GL_LoadLibrary(const char* path)
  (define-function int SDL_GL_LoadLibrary (char*))

  ;; void SDL_GL_Lock(void)
  (define-function void SDL_GL_Lock ())

  ;; int SDL_GL_SetAttribute(SDL_GLattr attr, int value)
  (define-function int SDL_GL_SetAttribute (int int))

  ;; void SDL_GL_SwapBuffers(void)
  (define-function void SDL_GL_SwapBuffers ())

  ;; void SDL_GL_Unlock(void)
  (define-function void SDL_GL_Unlock ())

  ;; void SDL_GL_UpdateRects(int numrects, SDL_Rect* rects)
  (define-function void SDL_GL_UpdateRects (int void*))

  ;; Uint8 SDL_GetAppState(void)
  (define-function uint8_t SDL_GetAppState ())

  ;; SDL_audiostatus SDL_GetAudioStatus(void)
  (define-function int SDL_GetAudioStatus ())

  ;; void SDL_GetClipRect(SDL_Surface* surface, SDL_Rect* rect)
  (define-function void SDL_GetClipRect (void* void*))

  ;; SDL_Cursor* SDL_GetCursor(void)
  (define-function void* SDL_GetCursor ())

  ;; char* SDL_GetError(void)
  (define-function char* SDL_GetError ())

  ;; SDL_EventFilter SDL_GetEventFilter(void)
  (define-function void* SDL_GetEventFilter ())

  ;; int SDL_GetGammaRamp(Uint16* red, Uint16* green, Uint16* blue)
  (define-function int SDL_GetGammaRamp (void* void* void*))

  ;; char* SDL_GetKeyName(SDLKey key)
  (define-function char* SDL_GetKeyName (int))

  ;; void SDL_GetKeyRepeat(int* delay, int* interval)
  (define-function void SDL_GetKeyRepeat (void* void*))

  ;; Uint8* SDL_GetKeyState(int* numkeys)
  (define-function void* SDL_GetKeyState (void*))

  ;; SDLMod SDL_GetModState(void)
  (define-function int SDL_GetModState ())

  ;; Uint8 SDL_GetMouseState(int* x, int* y)
  (define-function uint8_t SDL_GetMouseState (void* void*))

  ;; void SDL_GetRGB(Uint32 pixel, SDL_PixelFormat* fmt, Uint8* r, Uint8* g, Uint8* b)
  (define-function void SDL_GetRGB (uint32_t void* void* void* void*))

  ;; void SDL_GetRGBA(Uint32 pixel, SDL_PixelFormat* fmt, Uint8* r, Uint8* g, Uint8* b, Uint8* a)
  (define-function void SDL_GetRGBA (uint32_t void* void* void* void* void*))

  ;; Uint8 SDL_GetRelativeMouseState(int* x, int* y)
  (define-function uint8_t SDL_GetRelativeMouseState (void* void*))

  ;; Uint32 SDL_GetThreadID(SDL_Thread* thread)
  (define-function uint32_t SDL_GetThreadID (void*))

  ;; Uint32 SDL_GetTicks(void)
  (define-function uint32_t SDL_GetTicks ())

  ;; const SDL_VideoInfo* SDL_GetVideoInfo(void)
  (define-function void* SDL_GetVideoInfo ())

  ;; SDL_Surface* SDL_GetVideoSurface(void)
  (define-function void* SDL_GetVideoSurface ())

  ;; SDL_bool SDL_Has3DNow(void)
  (define-function int SDL_Has3DNow ())

  ;; SDL_bool SDL_Has3DNowExt(void)
  (define-function int SDL_Has3DNowExt ())

  ;; SDL_bool SDL_HasAltiVec(void)
  (define-function int SDL_HasAltiVec ())

  ;; SDL_bool SDL_HasMMX(void)
  (define-function int SDL_HasMMX ())

  ;; SDL_bool SDL_HasMMXExt(void)
  (define-function int SDL_HasMMXExt ())

  ;; SDL_bool SDL_HasRDTSC(void)
  (define-function int SDL_HasRDTSC ())

  ;; SDL_bool SDL_HasSSE(void)
  (define-function int SDL_HasSSE ())

  ;; SDL_bool SDL_HasSSE2(void)
  (define-function int SDL_HasSSE2 ())

  ;; int SDL_InitSubSystem(Uint32 flags)
  (define-function int SDL_InitSubSystem (uint32_t))

  ;; void SDL_JoystickClose(SDL_Joystick* joystick)
  (define-function void SDL_JoystickClose (void*))

  ;; int SDL_JoystickEventState(int state)
  (define-function int SDL_JoystickEventState (int))

  ;; Sint16 SDL_JoystickGetAxis(SDL_Joystick* joystick, int axis)
  (define-function int16_t SDL_JoystickGetAxis (void* int))

  ;; int SDL_JoystickGetBall(SDL_Joystick* joystick, int ball, int* dx, int* dy)
  (define-function int SDL_JoystickGetBall (void* int void* void*))

  ;; Uint8 SDL_JoystickGetButton(SDL_Joystick* joystick, int button)
  (define-function uint8_t SDL_JoystickGetButton (void* int))

  ;; Uint8 SDL_JoystickGetHat(SDL_Joystick* joystick, int hat)
  (define-function uint8_t SDL_JoystickGetHat (void* int))

  ;; int SDL_JoystickIndex(SDL_Joystick* joystick)
  (define-function int SDL_JoystickIndex (void*))

  ;; const char* SDL_JoystickName(int device_index)
  (define-function char* SDL_JoystickName (int))

  ;; int SDL_JoystickNumAxes(SDL_Joystick* joystick)
  (define-function int SDL_JoystickNumAxes (void*))

  ;; int SDL_JoystickNumBalls(SDL_Joystick* joystick)
  (define-function int SDL_JoystickNumBalls (void*))

  ;; int SDL_JoystickNumButtons(SDL_Joystick* joystick)
  (define-function int SDL_JoystickNumButtons (void*))

  ;; int SDL_JoystickNumHats(SDL_Joystick* joystick)
  (define-function int SDL_JoystickNumHats (void*))

  ;; SDL_Joystick* SDL_JoystickOpen(int device_index)
  (define-function void* SDL_JoystickOpen (int))

  ;; int SDL_JoystickOpened(int device_index)
  (define-function int SDL_JoystickOpened (int))

  ;; void SDL_JoystickUpdate(void)
  (define-function void SDL_JoystickUpdate ())

  ;; void SDL_KillThread(SDL_Thread* thread)
  (define-function void SDL_KillThread (void*))

  ;; const SDL_version* SDL_Linked_Version(void)
  (define-function void* SDL_Linked_Version ())

  ;; SDL_Rect** SDL_ListModes(SDL_PixelFormat* format, Uint32 flags)
  (define-function void* SDL_ListModes (void* uint32_t))

  ;; SDL_Surface* SDL_LoadBMP_RW(SDL_RWops* src, int freesrc)
  (define-function void* SDL_LoadBMP_RW (void* int))

  ;; void* SDL_LoadFunction(void* handle, const char* name)
  (define-function void* SDL_LoadFunction (void* char*))

  ;; void* SDL_LoadObject(const char* sofile)
  (define-function void* SDL_LoadObject (char*))

  ;; SDL_AudioSpec* SDL_LoadWAV_RW(SDL_RWops* src, int freesrc, SDL_AudioSpec* spec, Uint8** audio_buf, Uint32* audio_len)
  (define-function void* SDL_LoadWAV_RW (void* int void* void* void*))

  ;; void SDL_LockAudio(void)
  (define-function void SDL_LockAudio ())

  ;; int SDL_LockSurface(SDL_Surface* surface)
  (define-function int SDL_LockSurface (void*))

  ;; int SDL_LockYUVOverlay(SDL_Overlay* overlay)
  (define-function int SDL_LockYUVOverlay (void*))

  ;; int SDL_LowerBlit (SDL_Surface* src, SDL_Rect* srcrect, SDL_Surface* dst, SDL_Rect* dstrect)
  (define-function int SDL_LowerBlit (void* void* void* void*))

  ;; Uint32 SDL_MapRGB(const SDL_PixelFormat* const format, const Uint8 r, const Uint8 g, const Uint8 b)
  (define-function uint32_t SDL_MapRGB (void* uint8_t uint8_t uint8_t))

  ;; Uint32 SDL_MapRGBA(const SDL_PixelFormat* const format, const Uint8 r, const Uint8 g, const Uint8 b, const Uint8 a)
  (define-function uint32_t SDL_MapRGBA (void* uint8_t uint8_t uint8_t uint8_t))

  ;; void SDL_MixAudio(Uint8* dst, const Uint8* src, Uint32 len, int volume)
  (define-function void SDL_MixAudio (void* void* uint32_t int))

  ;; int SDL_NumJoysticks(void)
  (define-function int SDL_NumJoysticks ())

  ;; int SDL_OpenAudio(SDL_AudioSpec* desired, SDL_AudioSpec* obtained)
  (define-function int SDL_OpenAudio (void* void*))

  ;; void SDL_PauseAudio(int pause_on)
  (define-function void SDL_PauseAudio (int))

  ;; int SDL_PeepEvents(SDL_Event* events, int numevents, SDL_eventaction action, Uint32 mask)
  (define-function int SDL_PeepEvents (void* int int uint32_t))

  ;; int SDL_PollEvent(SDL_Event* event)
  (define-function int SDL_PollEvent (void*))

  ;; void SDL_PumpEvents(void)
  (define-function void SDL_PumpEvents ())

  ;; int SDL_PushEvent(SDL_Event* event)
  (define-function int SDL_PushEvent (void*))

  ;; void SDL_Quit(void)
  (define-function void SDL_Quit ())

  ;; void SDL_QuitSubSystem(Uint32 flags)
  (define-function void SDL_QuitSubSystem (uint32_t))

  ;; SDL_RWops* SDL_RWFromConstMem(const void* mem, int size)
  (define-function void* SDL_RWFromConstMem (void* int))

  ;; SDL_RWops* SDL_RWFromFP(FILE* fp, int autoclose)
  (define-function void* SDL_RWFromFP (void* int))

  ;; SDL_RWops* SDL_RWFromFile(const char* file, const char* mode)
  (define-function void* SDL_RWFromFile (char* char*))

  ;; SDL_RWops* SDL_RWFromMem(void* mem, int size)
  (define-function void* SDL_RWFromMem (void* int))

  ;; Uint16 SDL_ReadBE16(SDL_RWops* src)
  (define-function uint16_t SDL_ReadBE16 (void*))

  ;; Uint32 SDL_ReadBE32(SDL_RWops* src)
  (define-function uint32_t SDL_ReadBE32 (void*))

  ;; Uint64 SDL_ReadBE64(SDL_RWops* src)
  (define-function uint64_t SDL_ReadBE64 (void*))

  ;; Uint16 SDL_ReadLE16(SDL_RWops* src)
  (define-function uint16_t SDL_ReadLE16 (void*))

  ;; Uint32 SDL_ReadLE32(SDL_RWops* src)
  (define-function uint32_t SDL_ReadLE32 (void*))

  ;; Uint64 SDL_ReadLE64(SDL_RWops* src)
  (define-function uint64_t SDL_ReadLE64 (void*))

  ;; SDL_bool SDL_RemoveTimer(SDL_TimerID t)
  (define-function int SDL_RemoveTimer (void*))

  ;; int SDL_SaveBMP_RW (SDL_Surface* surface, SDL_RWops* dst, int freedst)
  (define-function int SDL_SaveBMP_RW (void* void* int))

  ;; int SDL_SemPost(SDL_sem* sem)
  (define-function int SDL_SemPost (void*))

  ;; int SDL_SemTryWait(SDL_sem* sem)
  (define-function int SDL_SemTryWait (void*))

  ;; Uint32 SDL_SemValue(SDL_sem* sem)
  (define-function uint32_t SDL_SemValue (void*))

  ;; int SDL_SemWait(SDL_sem* sem)
  (define-function int SDL_SemWait (void*))

  ;; int SDL_SemWaitTimeout(SDL_sem* sem, Uint32 ms)
  (define-function int SDL_SemWaitTimeout (void* uint32_t))

  ;; int SDL_SetAlpha(SDL_Surface* surface, Uint32 flag, Uint8 alpha)
  (define-function int SDL_SetAlpha (void* uint32_t uint8_t))

  ;; SDL_bool SDL_SetClipRect(SDL_Surface* surface, const SDL_Rect* rect)
  (define-function int SDL_SetClipRect (void* void*))

  ;; int SDL_SetColorKey (SDL_Surface* surface, Uint32 flag, Uint32 key)
  (define-function int SDL_SetColorKey (void* uint32_t uint32_t))

  ;; int SDL_SetColors(SDL_Surface* surface, SDL_Color* colors, int firstcolor, int ncolors)
  (define-function int SDL_SetColors (void* void* int int))

  ;; void SDL_SetCursor(SDL_Cursor* cursor)
  (define-function void SDL_SetCursor (void*))

  ;; void SDL_SetError(const char* fmt, ...)
  (define-function void SDL_SetError (char* ...))

  ;; void SDL_SetEventFilter(SDL_EventFilter filter)
  (define-function void SDL_SetEventFilter (void*))

  ;; int SDL_SetGamma(float red, float green, float blue)
  (define-function int SDL_SetGamma (float float float))

  ;; int SDL_SetGammaRamp(const Uint16* red, const Uint16* green, const Uint16* blue)
  (define-function int SDL_SetGammaRamp (void* void* void*))

  ;; void SDL_SetModState(SDLMod modstate)
  (define-function void SDL_SetModState (int))

  ;; int SDL_SetPalette(SDL_Surface* surface, int flags, SDL_Color* colors, int firstcolor, int ncolors)
  (define-function int SDL_SetPalette (void* int void* int int))

  ;; int SDL_SetTimer(Uint32 interval, SDL_TimerCallback callback)
  ;; Uint32 ( *SDL_TimerCallback)(Uint32 interval);
  (define-function int SDL_SetTimer (uint32_t [c-callback uint32_t (uint32_t)]))

  ;; SDL_Surface* SDL_SetVideoMode (int width, int height, int bpp, Uint32 flags)
  (define-function void* SDL_SetVideoMode (int int int uint32_t))

  ;; int SDL_ShowCursor(int toggle)
  (define-function int SDL_ShowCursor (int))

  ;; int SDL_SoftStretch(SDL_Surface* src, SDL_Rect* srcrect, SDL_Surface* dst, SDL_Rect* dstrect)
  (define-function int SDL_SoftStretch (void* void* void* void*))

  ;; Uint32 SDL_ThreadID(void)
  (define-function uint32_t SDL_ThreadID ())

  ;; void SDL_UnloadObject(void* handle)
  (define-function void SDL_UnloadObject (void*))

  ;; void SDL_UnlockAudio(void)
  (define-function void SDL_UnlockAudio ())

  ;; void SDL_UnlockSurface(SDL_Surface* surface)
  (define-function void SDL_UnlockSurface (void*))

  ;; void SDL_UnlockYUVOverlay(SDL_Overlay* overlay)
  (define-function void SDL_UnlockYUVOverlay (void*))

  ;; void SDL_UpdateRect (SDL_Surface* screen, Sint32 x, Sint32 y, Uint32 w, Uint32 h)
  (define-function void SDL_UpdateRect (void* int32_t int32_t uint32_t uint32_t))

  ;; void SDL_UpdateRects (SDL_Surface* screen, int numrects, SDL_Rect* rects)
  (define-function void SDL_UpdateRects (void* int void*))

  ;; int SDL_UpperBlit (SDL_Surface* src, SDL_Rect* srcrect, SDL_Surface* dst, SDL_Rect* dstrect)
  (define-function int SDL_UpperBlit (void* void* void* void*))

  ;; char* SDL_VideoDriverName(char* namebuf, int maxlen)
  (define-function char* SDL_VideoDriverName (char* int))

  ;; int SDL_VideoInit(const char* driver_name, Uint32 flags)
  (define-function int SDL_VideoInit (char* uint32_t))

  ;; int SDL_VideoModeOK(int width, int height, int bpp, Uint32 flags)
  (define-function int SDL_VideoModeOK (int int int uint32_t))

  ;; void SDL_VideoQuit(void)
  (define-function void SDL_VideoQuit ())

  ;; void SDL_WM_GetCaption(char** title, char** icon)
  (define-function void SDL_WM_GetCaption (void* void*))

  ;; SDL_GrabMode SDL_WM_GrabInput(SDL_GrabMode mode)
  (define-function int SDL_WM_GrabInput (int))

  ;; int SDL_WM_IconifyWindow(void)
  (define-function int SDL_WM_IconifyWindow ())

  ;; void SDL_WM_SetCaption(const char* title, const char* icon)
  (define-function void SDL_WM_SetCaption (char* char*))

  ;; void SDL_WM_SetIcon(SDL_Surface* icon, Uint8* mask)
  (define-function void SDL_WM_SetIcon (void* void*))

  ;; int SDL_WM_ToggleFullScreen(SDL_Surface* surface)
  (define-function int SDL_WM_ToggleFullScreen (void*))

  ;; int SDL_WaitEvent(SDL_Event* event)
  (define-function int SDL_WaitEvent (void*))

  ;; void SDL_WaitThread(SDL_Thread* thread, int* status)
  (define-function void SDL_WaitThread (void* void*))

  ;; void SDL_WarpMouse(Uint16 x, Uint16 y)
  (define-function void SDL_WarpMouse (uint16_t uint16_t))

  ;; Uint32 SDL_WasInit(Uint32 flags)
  (define-function uint32_t SDL_WasInit (uint32_t))

  ;; int SDL_WriteBE16(SDL_RWops* dst, Uint16 value)
  (define-function int SDL_WriteBE16 (void* uint16_t))

  ;; int SDL_WriteBE32(SDL_RWops* dst, Uint32 value)
  (define-function int SDL_WriteBE32 (void* uint32_t))

  ;; int SDL_WriteBE64(SDL_RWops* dst, Uint64 value)
  (define-function int SDL_WriteBE64 (void* uint64_t))

  ;; int SDL_WriteLE16(SDL_RWops* dst, Uint16 value)
  (define-function int SDL_WriteLE16 (void* uint16_t))

  ;; int SDL_WriteLE32(SDL_RWops* dst, Uint32 value)
  (define-function int SDL_WriteLE32 (void* uint32_t))

  ;; int SDL_WriteLE64(SDL_RWops* dst, Uint64 value)
  (define-function int SDL_WriteLE64 (void* uint64_t))

  ;; size_t SDL_iconv(iconv_t cd, const char** inbuf, size_t* inbytesleft, char** outbuf, size_t* outbytesleft)
  (define-function size_t SDL_iconv (void* void* void* void* void*))

  ;; char* SDL_iconv_string(const char* tocode, const char* fromcode, const char* inbuf, size_t inbytesleft)
  (define-function char* SDL_iconv_string (char* char* char* size_t))

  ;; char* SDL_lltoa(Sint64 value, char* string, int radix)
  (define-function char* SDL_lltoa (int64_t char* int))

  ;; char* SDL_ltoa(long value, char* string, int radix)
  (define-function char* SDL_ltoa (long char* int))

  ;; int SDL_mutexP(SDL_mutex* mutex)
  (define-function int SDL_mutexP (void*))

  ;; int SDL_mutexV(SDL_mutex* mutex)
  (define-function int SDL_mutexV (void*))

  ;; size_t SDL_strlcat(char* dst, const char* src, size_t maxlen)
  (define-function size_t SDL_strlcat (char* char* size_t))

  ;; size_t SDL_strlcpy(char* dst, const char* src, size_t maxlen)
  (define-function size_t SDL_strlcpy (char* char* size_t))

  ;; char* SDL_strlwr(char* string)
  (define-function char* SDL_strlwr (char*))

  ;; char* SDL_strrev(char* string)
  (define-function char* SDL_strrev (char*))

  ;; char* SDL_strupr(char* string)
  (define-function char* SDL_strupr (char*))

  ;; char* SDL_ulltoa(Uint64 value, char* string, int radix)
  (define-function char* SDL_ulltoa (uint64_t char* int))

  ;; char* SDL_ultoa(unsigned long value, char* string, int radix)
  (define-function char* SDL_ultoa (unsigned-long char* int))

  (define SDL_BlitSurface SDL_UpperBlit)

  ) ;[end]
