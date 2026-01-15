#!nobacktrace
;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon freetype)
  (export FT_Init_FreeType
          FT_New_Face
          FT_Set_Pixel_Sizes
          FT_Load_Char
          FT_Get_Char_Index
          FT_Get_Kerning
          FT_Done_Face
          FT_Done_FreeType
          FT_FaceRec
          FT_GlyphSlotRec
          FT_Glyph_Metrics
          FT_Outline
          FT_Bitmap
          FT_ListRec
          FT_BBox
          FT_Vector
          FT_Generic
          FT_LOAD_RENDER
          FT_KERNING_DEFAULT)
  (import (core) (ypsilon c-ffi) (ypsilon c-types))
  (define libFT
    (let ((sysname (architecture-feature 'sysname)))
      (cond ((string-contains sysname "darwin")
             (load-shared-object "libfreetype.dylib"))
            ((string-contains sysname "linux")
             (load-shared-object "libfreetype.so.6"))
            (else
              (assertion-violation 'load-shared-object "can not load freetype library, unknown operating system")))))

  (define FT_LOAD_RENDER 4)
  (define FT_KERNING_DEFAULT 0)

  (define-syntax define-cdecl
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function/weak ret name args)))))

  (define-c-typedef FT_Generic
    (struct (void* data)
            (void* finalizer)))

  (define-c-typedef FT_Vector
    (struct (long x)
            (long y)))

  (define-c-typedef FT_BBox
    (struct (long xMin)
            (long yMin)
            (long xMax)
            (long yMax)))

  (define-c-typedef FT_ListRec
    (struct (void*  head)
            (void*  tail)))

  (define-c-typedef FT_Bitmap
    (struct (unsigned-int rows)
            (unsigned-int width)
            (int pitch)
            (void* buffer)
            (unsigned-short num_grays)
            (uint8_t pixel_mode)
            (uint8_t palette_mode)
            (void* palette)))

  (define-c-typedef FT_Outline
    (struct (short n_contours)
            (short n_points)
            (void* points)
            (void* tags)
            (void* contours)
            (int flags)))

  (define-c-typedef FT_Glyph_Metrics
    (struct (long width)
            (long height)
            (long horiBearingX)
            (long horiBearingY)
            (long horiAdvance)
            (long vertBearingX)
            (long vertBearingY)
            (long vertAdvance)))

  (define-c-typedef FT_GlyphSlotRec
    (struct (void* library)
            (void* face)
            (void* next)
            (unsigned-int glyph_index)
            (FT_Generic generic)
            (FT_Glyph_Metrics metrics)
            (long linearHoriAdvance)
            (long linearVertAdvance)
            (FT_Vector advance)
            (int format)
            (FT_Bitmap bitmap)
            (int bitmap_left)
            (int bitmap_top)
            (FT_Outline outline)
            (unsigned-int num_subglyphs)
            (void* subglyphs)
            (void* control_data)
            (long control_len)
            (long lsb_delta)
            (long rsb_delta)
            (void* other)
            (void* internal)))

  (define-c-typedef FT_FaceRec
    (struct (long num_faces)
            (long face_index)
            (long face_flags)
            (long style_flags)
            (long num_glyphs)
            (void* family_name)
            (void* style_name)
            (int num_fixed_sizes)
            (void* available_sizes)
            (int num_charmaps)
            (void* charmaps)
            (FT_Generic generic)
            (FT_BBox bbox)
            (unsigned-short units_per_EM)
            (short ascender)
            (short descender)
            (short height)
            (short max_advance_width)
            (short max_advance_height)
            (short underline_position)
            (short underline_thickness)
            (void* glyph)
            (void* size)
            (void* charmap)
            (void* driver)
            (void* memory)
            (void* stream)
            (FT_ListRec sizes_list)
            (FT_Generic autohint)
            (void* extensions)
            (void* internal)))

  ;; FT_Error FT_Init_FreeType(FT_Library *alibrary)
  (define-cdecl int FT_Init_FreeType (void*))
  ;; FT_Error FT_New_Face(FT_Library library, const char* filepathname, FT_Long face_index, FT_Face *aface)
  (define-cdecl int FT_New_Face (void* void* long void*))
  ;; FT_Error FT_Set_Pixel_Sizes(FT_Face face, FT_UInt pixel_width, FT_UInt pixel_height)
  (define-cdecl int FT_Set_Pixel_Sizes (void* unsigned-int unsigned-int))
  ;; FT_Error FT_Load_Char(FT_Face face, FT_ULong char_code, FT_Int32 load_flags)
  (define-cdecl int FT_Load_Char (void* unsigned-long int32_t))
  ;; FT_Error FT_Done_Face(FT_Face face)
  (define-cdecl int FT_Done_Face (void*))
  ;; FT_Error FT_Done_FreeType(FT_Library library)
  (define-cdecl int FT_Done_FreeType (void*))
  ;; FT_Error FT_Get_Char_Index(FT_Face, FT_ULong charcode)
  (define-cdecl int FT_Get_Char_Index (void* unsigned-long))
  ;; FT_Error FT_Get_Kerning(FT_Face face, FT_UInt left_glyph, FT_UInt right_glyph, FT_UInt kern_mode, FT_Vector *akerning)
  (define-cdecl int FT_Get_Kerning (void* unsigned-int unsigned-int unsigned-int void*))
) ;[end]

;(define rec (make-bytevector (c-sizeof FT_GlyphSlotRec)))
;(define-c-struct-methods FT_GlyphSlotRec)
;(define rec (make-FT_GlyphSlotRec))
;(FT_GlyphSlotRec-linearHoriAdvance-set! rec 10)
#|

(import (ypsilon c-types) (ypsilon freetype))
(define ft (make-c-void* 0))
(FT_Init_FreeType ft)
(define face (make-c-void* 0))
(FT_New_Face (c-void*-ref ft) (make-c-string "demo/VeraMono.ttf") 0 face)
(FT_Set_Pixel_Sizes (c-void*-ref face) 0 24)

|#