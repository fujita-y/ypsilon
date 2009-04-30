#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon sdl mixer)
  (export Mix_AllocateChannels
          Mix_CloseAudio
          Mix_ExpireChannel
          Mix_FadeInChannelTimed
          Mix_FadeInMusic
          Mix_FadeInMusicPos
          Mix_FadeOutChannel
          Mix_FadeOutGroup
          Mix_FadeOutMusic
          Mix_FadingChannel
          Mix_FadingMusic
          Mix_FreeChunk
          Mix_FreeMusic
          Mix_GetChunk
          Mix_GetMusicHookData
          Mix_GetMusicType
          Mix_GetSynchroValue
          Mix_GroupAvailable
          Mix_GroupChannel
          Mix_GroupChannels
          Mix_GroupCount
          Mix_GroupNewer
          Mix_GroupOldest
          Mix_HaltChannel
          Mix_HaltGroup
          Mix_HaltMusic
          Mix_Linked_Version
          Mix_LoadMUS
          Mix_LoadMUS_RW
          Mix_LoadWAV_RW
          Mix_OpenAudio
          Mix_Pause
          Mix_PauseMusic
          Mix_Paused
          Mix_PausedMusic
          Mix_PlayChannelTimed
          Mix_PlayMusic
          Mix_Playing
          Mix_PlayingMusic
          Mix_QuerySpec
          Mix_QuickLoad_RAW
          Mix_QuickLoad_WAV
          Mix_RegisterEffect
          Mix_ReserveChannels
          Mix_Resume
          Mix_ResumeMusic
          Mix_RewindMusic
          Mix_SetDistance
          Mix_SetMusicCMD
          Mix_SetMusicPosition
          Mix_SetPanning
          Mix_SetPosition
          Mix_SetReverseStereo
          Mix_SetSynchroValue
          Mix_UnregisterAllEffects
          Mix_UnregisterEffect
          Mix_Volume
          Mix_VolumeChunk
          Mix_VolumeMusic)

  ;; Mix_ChannelFinished   -- incompatible thread callback
  ;; Mix_HookMusic         -- incompatible thread callback
  ;; Mix_HookMusicFinished -- incompatible thread callback
  ;; Mix_SetPostMix        -- incompatible thread callback

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libSDL_mixer.so")
          (on-sunos   "libSDL_mixer.so")
          (on-freebsd "libSDL_mixer.so")
          (on-openbsd "libSDL_mixer.so")
          (on-darwin  "SDL_mixer.framework/SDL_mixer")
          (on-windows "SDL_mixer.dll")
          (else
           (assertion-violation #f "can not locate SDL Mixer library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; int Mix_AllocateChannels(int numchans)
  (define-function int Mix_AllocateChannels (int))

  ;; void Mix_ChannelFinished(void (*channel_finished)(int channel))
  (define-function void Mix_ChannelFinished ([c-callback void (int)]))

  ;; void Mix_CloseAudio(void)
  (define-function void Mix_CloseAudio ())

  ;; int Mix_ExpireChannel(int channel, int ticks)
  (define-function int Mix_ExpireChannel (int int))

  ;; int Mix_FadeInChannelTimed(int channel, Mix_Chunk* chunk, int loops, int ms, int ticks)
  (define-function int Mix_FadeInChannelTimed (int void* int int int))

  ;; int Mix_FadeInMusic(Mix_Music* music, int loops, int ms)
  (define-function int Mix_FadeInMusic (void* int int))

  ;; int Mix_FadeInMusicPos(Mix_Music* music, int loops, int ms, double position)
  (define-function int Mix_FadeInMusicPos (void* int int double))

  ;; int Mix_FadeOutChannel(int which, int ms)
  (define-function int Mix_FadeOutChannel (int int))

  ;; int Mix_FadeOutGroup(int tag, int ms)
  (define-function int Mix_FadeOutGroup (int int))

  ;; int Mix_FadeOutMusic(int ms)
  (define-function int Mix_FadeOutMusic (int))

  ;; Mix_Fading Mix_FadingChannel(int which)
  (define-function int Mix_FadingChannel (int))

  ;; Mix_Fading Mix_FadingMusic(void)
  (define-function int Mix_FadingMusic ())

  ;; void Mix_FreeChunk(Mix_Chunk* chunk)
  (define-function void Mix_FreeChunk (void*))

  ;; void Mix_FreeMusic(Mix_Music* music)
  (define-function void Mix_FreeMusic (void*))

  ;; Mix_Chunk* Mix_GetChunk(int channel)
  (define-function void* Mix_GetChunk (int))

  ;; void* Mix_GetMusicHookData(void)
  (define-function void* Mix_GetMusicHookData ())

  ;; Mix_MusicType Mix_GetMusicType(const Mix_Music* music)
  (define-function int Mix_GetMusicType (void*))

  ;; int Mix_GetSynchroValue(void)
  (define-function int Mix_GetSynchroValue ())

  ;; int Mix_GroupAvailable(int tag)
  (define-function int Mix_GroupAvailable (int))

  ;; int Mix_GroupChannel(int which, int tag)
  (define-function int Mix_GroupChannel (int int))

  ;; int Mix_GroupChannels(int from, int to, int tag)
  (define-function int Mix_GroupChannels (int int int))

  ;; int Mix_GroupCount(int tag)
  (define-function int Mix_GroupCount (int))

  ;; int Mix_GroupNewer(int tag)
  (define-function int Mix_GroupNewer (int))

  ;; int Mix_GroupOldest(int tag)
  (define-function int Mix_GroupOldest (int))

  ;; int Mix_HaltChannel(int channel)
  (define-function int Mix_HaltChannel (int))

  ;; int Mix_HaltGroup(int tag)
  (define-function int Mix_HaltGroup (int))

  ;; int Mix_HaltMusic(void)
  (define-function int Mix_HaltMusic ())

  ;; void Mix_HookMusic(void (*mix_func) (void* udata, Uint8* stream, int len), void* arg)
  (define-function void Mix_HookMusic ([c-callback void (void* void* int)] void*))

  ;; void Mix_HookMusicFinished(void (*music_finished)(void))
  (define-function void Mix_HookMusicFinished ([c-callback void ()]))

  ;; const SDL_version* Mix_Linked_Version(void)
  (define-function void* Mix_Linked_Version ())

  ;; Mix_Music* Mix_LoadMUS(const char* file)
  (define-function void* Mix_LoadMUS (char*))

  ;; Mix_Music* Mix_LoadMUS_RW(SDL_RWops* rw)
  (define-function void* Mix_LoadMUS_RW (void*))

  ;; Mix_Chunk* Mix_LoadWAV_RW(SDL_RWops* src, int freesrc)
  (define-function void* Mix_LoadWAV_RW (void* int))

  ;; int Mix_OpenAudio(int frequency, Uint16 format, int channels, int chunksize)
  (define-function int Mix_OpenAudio (int uint16_t int int))

  ;; void Mix_Pause(int channel)
  (define-function void Mix_Pause (int))

  ;; void Mix_PauseMusic(void)
  (define-function void Mix_PauseMusic ())

  ;; int Mix_Paused(int channel)
  (define-function int Mix_Paused (int))

  ;; int Mix_PausedMusic(void)
  (define-function int Mix_PausedMusic ())

  ;; int Mix_PlayChannelTimed(int channel, Mix_Chunk* chunk, int loops, int ticks)
  (define-function int Mix_PlayChannelTimed (int void* int int))

  ;; int Mix_PlayMusic(Mix_Music* music, int loops)
  (define-function int Mix_PlayMusic (void* int))

  ;; int Mix_Playing(int channel)
  (define-function int Mix_Playing (int))

  ;; int Mix_PlayingMusic(void)
  (define-function int Mix_PlayingMusic ())

  ;; int Mix_QuerySpec(int* frequency, Uint16* format, int* channels)
  (define-function int Mix_QuerySpec (void* void* void*))

  ;; Mix_Chunk* Mix_QuickLoad_RAW(Uint8* mem, Uint32 len)
  (define-function void* Mix_QuickLoad_RAW (void* uint32_t))

  ;; Mix_Chunk* Mix_QuickLoad_WAV(Uint8* mem)
  (define-function void* Mix_QuickLoad_WAV (void*))

  ;; int Mix_RegisterEffect(int chan, Mix_EffectFunc_t f, Mix_EffectDone_t d, void* arg)
  (define-function int Mix_RegisterEffect (int void* void* void*))

  ;; int Mix_ReserveChannels(int num)
  (define-function int Mix_ReserveChannels (int))

  ;; void Mix_Resume(int channel)
  (define-function void Mix_Resume (int))

  ;; void Mix_ResumeMusic(void)
  (define-function void Mix_ResumeMusic ())

  ;; void Mix_RewindMusic(void)
  (define-function void Mix_RewindMusic ())

  ;; int Mix_SetDistance(int channel, Uint8 distance)
  (define-function int Mix_SetDistance (int uint8_t))

  ;; int Mix_SetMusicCMD(const char* command)
  (define-function int Mix_SetMusicCMD (char*))

  ;; int Mix_SetMusicPosition(double position)
  (define-function int Mix_SetMusicPosition (double))

  ;; int Mix_SetPanning(int channel, Uint8 left, Uint8 right)
  (define-function int Mix_SetPanning (int uint8_t uint8_t))

  ;; int Mix_SetPosition(int channel, Sint16 angle, Uint8 distance)
  (define-function int Mix_SetPosition (int int16_t uint8_t))

  ;; void Mix_SetPostMix(void (*mix_func) (void* udata, Uint8* stream, int len), void* arg)
  (define-function void Mix_SetPostMix ([c-callback void (void* void* int)] void*))

  ;; int Mix_SetReverseStereo(int channel, int flip)
  (define-function int Mix_SetReverseStereo (int int))

  ;; int Mix_SetSynchroValue(int value)
  (define-function int Mix_SetSynchroValue (int))

  ;; int Mix_UnregisterAllEffects(int channel)
  (define-function int Mix_UnregisterAllEffects (int))

  ;; int Mix_UnregisterEffect(int channel, Mix_EffectFunc_t f)
  (define-function int Mix_UnregisterEffect (int void*))

  ;; int Mix_Volume(int channel, int volume)
  (define-function int Mix_Volume (int int))

  ;; int Mix_VolumeChunk(Mix_Chunk* chunk, int volume)
  (define-function int Mix_VolumeChunk (void* int))

  ;; int Mix_VolumeMusic(int volume)
  (define-function int Mix_VolumeMusic (int))

  ) ;[end]
