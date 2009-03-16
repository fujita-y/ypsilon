#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk pixbuf)

  (export gdk_pixbuf_add_alpha
          gdk_pixbuf_alpha_mode_get_type
          gdk_pixbuf_animation_get_height
          gdk_pixbuf_animation_get_iter
          gdk_pixbuf_animation_get_static_image
          gdk_pixbuf_animation_get_type
          gdk_pixbuf_animation_get_width
          gdk_pixbuf_animation_is_static_image
          gdk_pixbuf_animation_iter_advance
          gdk_pixbuf_animation_iter_get_delay_time
          gdk_pixbuf_animation_iter_get_pixbuf
          gdk_pixbuf_animation_iter_get_type
          gdk_pixbuf_animation_iter_on_currently_loading_frame
          gdk_pixbuf_animation_new_from_file
          gdk_pixbuf_animation_ref
          gdk_pixbuf_animation_unref
          gdk_pixbuf_apply_embedded_orientation
          gdk_pixbuf_composite
          gdk_pixbuf_composite_color
          gdk_pixbuf_composite_color_simple
          gdk_pixbuf_copy
          gdk_pixbuf_copy_area
          gdk_pixbuf_error_get_type
          gdk_pixbuf_error_quark
          gdk_pixbuf_fill
          gdk_pixbuf_flip
          gdk_pixbuf_format_get_description
          gdk_pixbuf_format_get_extensions
          gdk_pixbuf_format_get_license
          gdk_pixbuf_format_get_mime_types
          gdk_pixbuf_format_get_name
          gdk_pixbuf_format_is_disabled
          gdk_pixbuf_format_is_scalable
          gdk_pixbuf_format_is_writable
          gdk_pixbuf_format_set_disabled
          gdk_pixbuf_get_bits_per_sample
          gdk_pixbuf_get_colorspace
          gdk_pixbuf_get_file_info
          gdk_pixbuf_get_formats
          gdk_pixbuf_get_from_drawable
          gdk_pixbuf_get_from_image
          gdk_pixbuf_get_has_alpha
          gdk_pixbuf_get_height
          gdk_pixbuf_get_n_channels
          gdk_pixbuf_get_option
          gdk_pixbuf_get_pixels
          gdk_pixbuf_get_rowstride
          gdk_pixbuf_get_type
          gdk_pixbuf_get_width
          gdk_pixbuf_loader_close
          gdk_pixbuf_loader_get_animation
          gdk_pixbuf_loader_get_format
          gdk_pixbuf_loader_get_pixbuf
          gdk_pixbuf_loader_get_type
          gdk_pixbuf_loader_new
          gdk_pixbuf_loader_new_with_mime_type
          gdk_pixbuf_loader_new_with_type
          gdk_pixbuf_loader_set_size
          gdk_pixbuf_loader_write
          gdk_pixbuf_new
          gdk_pixbuf_new_from_data
          gdk_pixbuf_new_from_file
          gdk_pixbuf_new_from_file_at_scale
          gdk_pixbuf_new_from_file_at_size
          gdk_pixbuf_new_from_inline
          gdk_pixbuf_new_from_stream
          gdk_pixbuf_new_from_stream_at_scale
          gdk_pixbuf_new_from_xpm_data
          gdk_pixbuf_new_subpixbuf
          gdk_pixbuf_ref
          gdk_pixbuf_render_pixmap_and_mask
          gdk_pixbuf_render_pixmap_and_mask_for_colormap
          gdk_pixbuf_render_threshold_alpha
          gdk_pixbuf_rotate_simple
          gdk_pixbuf_rotation_get_type
          gdk_pixbuf_saturate_and_pixelate
          gdk_pixbuf_save
          gdk_pixbuf_save_to_buffer
          gdk_pixbuf_save_to_bufferv
          gdk_pixbuf_save_to_callback
          gdk_pixbuf_save_to_callbackv
          gdk_pixbuf_save_to_stream
          gdk_pixbuf_savev
          gdk_pixbuf_scale
          gdk_pixbuf_scale_simple
          gdk_pixbuf_simple_anim_add_frame
          gdk_pixbuf_simple_anim_get_type
          gdk_pixbuf_simple_anim_iter_get_type
          gdk_pixbuf_simple_anim_new
          gdk_pixbuf_unref)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gdk.framework/Gdk")
          (on-linux   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-windows "libgdk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; GdkPixbuf* gdk_pixbuf_add_alpha (const GdkPixbuf* pixbuf, gboolean substitute_color, guchar r, guchar g, guchar b)
  (define-function void* gdk_pixbuf_add_alpha (void* int uint8_t uint8_t uint8_t))

  ;; GType gdk_pixbuf_alpha_mode_get_type (void)
  (define-function unsigned-long gdk_pixbuf_alpha_mode_get_type ())

  ;; int gdk_pixbuf_animation_get_height (GdkPixbufAnimation* animation)
  (define-function int gdk_pixbuf_animation_get_height (void*))

  ;; GdkPixbufAnimationIter* gdk_pixbuf_animation_get_iter (GdkPixbufAnimation* animation, const GTimeVal* start_time)
  (define-function void* gdk_pixbuf_animation_get_iter (void* void*))

  ;; GdkPixbuf* gdk_pixbuf_animation_get_static_image (GdkPixbufAnimation* animation)
  (define-function void* gdk_pixbuf_animation_get_static_image (void*))

  ;; GType gdk_pixbuf_animation_get_type (void)
  (define-function unsigned-long gdk_pixbuf_animation_get_type ())

  ;; int gdk_pixbuf_animation_get_width (GdkPixbufAnimation* animation)
  (define-function int gdk_pixbuf_animation_get_width (void*))

  ;; gboolean gdk_pixbuf_animation_is_static_image (GdkPixbufAnimation* animation)
  (define-function int gdk_pixbuf_animation_is_static_image (void*))

  ;; gboolean gdk_pixbuf_animation_iter_advance (GdkPixbufAnimationIter* iter, const GTimeVal* current_time)
  (define-function int gdk_pixbuf_animation_iter_advance (void* void*))

  ;; int gdk_pixbuf_animation_iter_get_delay_time (GdkPixbufAnimationIter* iter)
  (define-function int gdk_pixbuf_animation_iter_get_delay_time (void*))

  ;; GdkPixbuf* gdk_pixbuf_animation_iter_get_pixbuf (GdkPixbufAnimationIter* iter)
  (define-function void* gdk_pixbuf_animation_iter_get_pixbuf (void*))

  ;; GType gdk_pixbuf_animation_iter_get_type (void)
  (define-function unsigned-long gdk_pixbuf_animation_iter_get_type ())

  ;; gboolean gdk_pixbuf_animation_iter_on_currently_loading_frame (GdkPixbufAnimationIter* iter)
  (define-function int gdk_pixbuf_animation_iter_on_currently_loading_frame (void*))

  ;; GdkPixbufAnimation* gdk_pixbuf_animation_new_from_file (const char* filename, GError** error)
  (define-function void* gdk_pixbuf_animation_new_from_file (char* void*))

  ;; GdkPixbufAnimation* gdk_pixbuf_animation_ref (GdkPixbufAnimation* animation)
  (define-function void* gdk_pixbuf_animation_ref (void*))

  ;; void gdk_pixbuf_animation_unref (GdkPixbufAnimation* animation)
  (define-function void gdk_pixbuf_animation_unref (void*))

  ;; GdkPixbuf* gdk_pixbuf_apply_embedded_orientation (GdkPixbuf* src)
  (define-function void* gdk_pixbuf_apply_embedded_orientation (void*))

  ;; void gdk_pixbuf_composite (const GdkPixbuf* src, GdkPixbuf* dest, int dest_x, int dest_y, int dest_width, int dest_height, double offset_x, double offset_y, double scale_x, double scale_y, GdkInterpType interp_type, int overall_alpha)
  (define-function void gdk_pixbuf_composite (void* void* int int int int double double double double int int))

  ;; void gdk_pixbuf_composite_color (const GdkPixbuf* src, GdkPixbuf* dest, int dest_x, int dest_y, int dest_width, int dest_height, double offset_x, double offset_y, double scale_x, double scale_y, GdkInterpType interp_type, int overall_alpha, int check_x, int check_y, int check_size, guint32 color1, guint32 color2)
  (define-function void gdk_pixbuf_composite_color (void* void* int int int int double double double double int int int int int uint32_t uint32_t))

  ;; GdkPixbuf* gdk_pixbuf_composite_color_simple (const GdkPixbuf* src, int dest_width, int dest_height, GdkInterpType interp_type, int overall_alpha, int check_size, guint32 color1, guint32 color2)
  (define-function void* gdk_pixbuf_composite_color_simple (void* int int int int int uint32_t uint32_t))

  ;; GdkPixbuf* gdk_pixbuf_copy (const GdkPixbuf* pixbuf)
  (define-function void* gdk_pixbuf_copy (void*))

  ;; void gdk_pixbuf_copy_area (const GdkPixbuf* src_pixbuf, int src_x, int src_y, int width, int height, GdkPixbuf* dest_pixbuf, int dest_x, int dest_y)
  (define-function void gdk_pixbuf_copy_area (void* int int int int void* int int))

  ;; GType gdk_pixbuf_error_get_type (void)
  (define-function unsigned-long gdk_pixbuf_error_get_type ())

  ;; GQuark gdk_pixbuf_error_quark (void)
  (define-function uint32_t gdk_pixbuf_error_quark ())

  ;; void gdk_pixbuf_fill (GdkPixbuf* pixbuf, guint32 pixel)
  (define-function void gdk_pixbuf_fill (void* uint32_t))

  ;; GdkPixbuf* gdk_pixbuf_flip (const GdkPixbuf* src, gboolean horizontal)
  (define-function void* gdk_pixbuf_flip (void* int))

  ;; gchar* gdk_pixbuf_format_get_description (GdkPixbufFormat* format)
  (define-function char* gdk_pixbuf_format_get_description (void*))

  ;; gchar** gdk_pixbuf_format_get_extensions (GdkPixbufFormat* format)
  (define-function void* gdk_pixbuf_format_get_extensions (void*))

  ;; gchar* gdk_pixbuf_format_get_license (GdkPixbufFormat* format)
  (define-function char* gdk_pixbuf_format_get_license (void*))

  ;; gchar** gdk_pixbuf_format_get_mime_types (GdkPixbufFormat* format)
  (define-function void* gdk_pixbuf_format_get_mime_types (void*))

  ;; gchar* gdk_pixbuf_format_get_name (GdkPixbufFormat* format)
  (define-function char* gdk_pixbuf_format_get_name (void*))

  ;; gboolean gdk_pixbuf_format_is_disabled (GdkPixbufFormat* format)
  (define-function int gdk_pixbuf_format_is_disabled (void*))

  ;; gboolean gdk_pixbuf_format_is_scalable (GdkPixbufFormat* format)
  (define-function int gdk_pixbuf_format_is_scalable (void*))

  ;; gboolean gdk_pixbuf_format_is_writable (GdkPixbufFormat* format)
  (define-function int gdk_pixbuf_format_is_writable (void*))

  ;; void gdk_pixbuf_format_set_disabled (GdkPixbufFormat* format, gboolean disabled)
  (define-function void gdk_pixbuf_format_set_disabled (void* int))

  ;; int gdk_pixbuf_get_bits_per_sample (const GdkPixbuf* pixbuf)
  (define-function int gdk_pixbuf_get_bits_per_sample (void*))

  ;; GdkColorspace gdk_pixbuf_get_colorspace (const GdkPixbuf* pixbuf)
  (define-function int gdk_pixbuf_get_colorspace (void*))

  ;; GdkPixbufFormat* gdk_pixbuf_get_file_info (const gchar* filename, gint* width, gint* height)
  (define-function void* gdk_pixbuf_get_file_info (char* void* void*))

  ;; GSList* gdk_pixbuf_get_formats (void)
  (define-function void* gdk_pixbuf_get_formats ())

  ;; GdkPixbuf* gdk_pixbuf_get_from_drawable (GdkPixbuf* dest, GdkDrawable* src, GdkColormap* cmap, int src_x, int src_y, int dest_x, int dest_y, int width, int height)
  (define-function void* gdk_pixbuf_get_from_drawable (void* void* void* int int int int int int))

  ;; GdkPixbuf* gdk_pixbuf_get_from_image (GdkPixbuf* dest, GdkImage* src, GdkColormap* cmap, int src_x, int src_y, int dest_x, int dest_y, int width, int height)
  (define-function void* gdk_pixbuf_get_from_image (void* void* void* int int int int int int))

  ;; gboolean gdk_pixbuf_get_has_alpha (const GdkPixbuf* pixbuf)
  (define-function int gdk_pixbuf_get_has_alpha (void*))

  ;; int gdk_pixbuf_get_height (const GdkPixbuf* pixbuf)
  (define-function int gdk_pixbuf_get_height (void*))

  ;; int gdk_pixbuf_get_n_channels (const GdkPixbuf* pixbuf)
  (define-function int gdk_pixbuf_get_n_channels (void*))

  ;; const gchar* gdk_pixbuf_get_option (GdkPixbuf* pixbuf, const gchar* key)
  (define-function char* gdk_pixbuf_get_option (void* char*))

  ;; guchar* gdk_pixbuf_get_pixels (const GdkPixbuf* pixbuf)
  (define-function void* gdk_pixbuf_get_pixels (void*))

  ;; int gdk_pixbuf_get_rowstride (const GdkPixbuf* pixbuf)
  (define-function int gdk_pixbuf_get_rowstride (void*))

  ;; GType gdk_pixbuf_get_type (void)
  (define-function unsigned-long gdk_pixbuf_get_type ())

  ;; int gdk_pixbuf_get_width (const GdkPixbuf* pixbuf)
  (define-function int gdk_pixbuf_get_width (void*))

  ;; gboolean gdk_pixbuf_loader_close (GdkPixbufLoader* loader, GError** error)
  (define-function int gdk_pixbuf_loader_close (void* void*))

  ;; GdkPixbufAnimation* gdk_pixbuf_loader_get_animation (GdkPixbufLoader* loader)
  (define-function void* gdk_pixbuf_loader_get_animation (void*))

  ;; GdkPixbufFormat* gdk_pixbuf_loader_get_format (GdkPixbufLoader* loader)
  (define-function void* gdk_pixbuf_loader_get_format (void*))

  ;; GdkPixbuf* gdk_pixbuf_loader_get_pixbuf (GdkPixbufLoader* loader)
  (define-function void* gdk_pixbuf_loader_get_pixbuf (void*))

  ;; GType gdk_pixbuf_loader_get_type (void)
  (define-function unsigned-long gdk_pixbuf_loader_get_type ())

  ;; GdkPixbufLoader* gdk_pixbuf_loader_new (void)
  (define-function void* gdk_pixbuf_loader_new ())

  ;; GdkPixbufLoader* gdk_pixbuf_loader_new_with_mime_type (const char* mime_type, GError** error)
  (define-function void* gdk_pixbuf_loader_new_with_mime_type (char* void*))

  ;; GdkPixbufLoader* gdk_pixbuf_loader_new_with_type (const char* image_type, GError** error)
  (define-function void* gdk_pixbuf_loader_new_with_type (char* void*))

  ;; void gdk_pixbuf_loader_set_size (GdkPixbufLoader* loader, int width, int height)
  (define-function void gdk_pixbuf_loader_set_size (void* int int))

  ;; gboolean gdk_pixbuf_loader_write (GdkPixbufLoader* loader, const guchar* buf, gsize count, GError** error)
  (define-function int gdk_pixbuf_loader_write (void* void* unsigned-long void*))

  ;; GdkPixbuf* gdk_pixbuf_new (GdkColorspace colorspace, gboolean has_alpha, int bits_per_sample, int width, int height)
  (define-function void* gdk_pixbuf_new (int int int int int))

  ;; GdkPixbuf* gdk_pixbuf_new_from_data (const guchar* data, GdkColorspace colorspace, gboolean has_alpha, int bits_per_sample, int width, int height, int rowstride, GdkPixbufDestroyNotify destroy_fn, gpointer destroy_fn_data)
  (define-function void* gdk_pixbuf_new_from_data (void* int int int int int int (c-callback void (void* void*)) void*))

  ;; GdkPixbuf* gdk_pixbuf_new_from_file (const char* filename, GError** error)
  (define-function void* gdk_pixbuf_new_from_file (char* void*))

  ;; GdkPixbuf* gdk_pixbuf_new_from_file_at_scale (const char* filename, int width, int height, gboolean preserve_aspect_ratio, GError** error)
  (define-function void* gdk_pixbuf_new_from_file_at_scale (char* int int int void*))

  ;; GdkPixbuf* gdk_pixbuf_new_from_file_at_size (const char* filename, int width, int height, GError** error)
  (define-function void* gdk_pixbuf_new_from_file_at_size (char* int int void*))

  ;; GdkPixbuf* gdk_pixbuf_new_from_inline (gint data_length, const guint8* data, gboolean copy_pixels, GError** error)
  (define-function void* gdk_pixbuf_new_from_inline (int void* int void*))

  ;; GdkPixbuf* gdk_pixbuf_new_from_stream (GInputStream* stream, GCancellable* cancellable, GError** error)
  (define-function void* gdk_pixbuf_new_from_stream (void* void* void*))

  ;; GdkPixbuf* gdk_pixbuf_new_from_stream_at_scale (GInputStream* stream, gint width, gint height, gboolean preserve_aspect_ratio, GCancellable* cancellable, GError** error)
  (define-function void* gdk_pixbuf_new_from_stream_at_scale (void* int int int void* void*))

  ;; GdkPixbuf* gdk_pixbuf_new_from_xpm_data (const char** data)
  (define-function void* gdk_pixbuf_new_from_xpm_data (void*))

  ;; GdkPixbuf* gdk_pixbuf_new_subpixbuf (GdkPixbuf* src_pixbuf, int src_x, int src_y, int width, int height)
  (define-function void* gdk_pixbuf_new_subpixbuf (void* int int int int))

  ;; GdkPixbuf* gdk_pixbuf_ref (GdkPixbuf* pixbuf)
  (define-function void* gdk_pixbuf_ref (void*))

  ;; void gdk_pixbuf_render_pixmap_and_mask (GdkPixbuf* pixbuf, GdkPixmap** pixmap_return, GdkBitmap** mask_return, int alpha_threshold)
  (define-function void gdk_pixbuf_render_pixmap_and_mask (void* void* void* int))

  ;; void gdk_pixbuf_render_pixmap_and_mask_for_colormap (GdkPixbuf* pixbuf, GdkColormap* colormap, GdkPixmap** pixmap_return, GdkBitmap** mask_return, int alpha_threshold)
  (define-function void gdk_pixbuf_render_pixmap_and_mask_for_colormap (void* void* void* void* int))

  ;; void gdk_pixbuf_render_threshold_alpha (GdkPixbuf* pixbuf, GdkBitmap* bitmap, int src_x, int src_y, int dest_x, int dest_y, int width, int height, int alpha_threshold)
  (define-function void gdk_pixbuf_render_threshold_alpha (void* void* int int int int int int int))

  ;; GdkPixbuf* gdk_pixbuf_rotate_simple (const GdkPixbuf* src, GdkPixbufRotation angle)
  (define-function void* gdk_pixbuf_rotate_simple (void* int))

  ;; GType gdk_pixbuf_rotation_get_type (void)
  (define-function unsigned-long gdk_pixbuf_rotation_get_type ())

  ;; void gdk_pixbuf_saturate_and_pixelate (const GdkPixbuf* src, GdkPixbuf* dest, gfloat saturation, gboolean pixelate)
  (define-function void gdk_pixbuf_saturate_and_pixelate (void* void* float int))

  ;; gboolean gdk_pixbuf_save (GdkPixbuf* pixbuf, const char* filename, const char* type, GError** error, ...)
  (define-variadic-function int gdk_pixbuf_save (void* char* char* void* ...))

  ;; gboolean gdk_pixbuf_save_to_buffer (GdkPixbuf* pixbuf, gchar** buffer, gsize* buffer_size, const char* type, GError** error, ...)
  (define-variadic-function int gdk_pixbuf_save_to_buffer (void* void* void* char* void* ...))

  ;; gboolean gdk_pixbuf_save_to_bufferv (GdkPixbuf* pixbuf, gchar** buffer, gsize* buffer_size, const char* type, char** option_keys, char** option_values, GError** error)
  (define-function int gdk_pixbuf_save_to_bufferv (void* void* void* char* void* void* void*))

  ;; gboolean gdk_pixbuf_save_to_callback (GdkPixbuf* pixbuf, GdkPixbufSaveFunc save_func, gpointer user_data, const char* type, GError** error, ...)
  (define-variadic-function int gdk_pixbuf_save_to_callback (void* (c-callback int (void* unsigned-long void* void*)) void* char* void* ...))

  ;; gboolean gdk_pixbuf_save_to_callbackv (GdkPixbuf* pixbuf, GdkPixbufSaveFunc save_func, gpointer user_data, const char* type, char** option_keys, char** option_values, GError** error)
  (define-function int gdk_pixbuf_save_to_callbackv (void* (c-callback int (void* unsigned-long void* void*)) void* char* void* void* void*))

  ;; gboolean gdk_pixbuf_save_to_stream (GdkPixbuf* pixbuf, GOutputStream* stream, const char* type, GCancellable* cancellable, GError** error, ...)
  (define-variadic-function int gdk_pixbuf_save_to_stream (void* void* char* void* void* ...))

  ;; gboolean gdk_pixbuf_savev (GdkPixbuf* pixbuf, const char* filename, const char* type, char** option_keys, char** option_values, GError** error)
  (define-function int gdk_pixbuf_savev (void* char* char* void* void* void*))

  ;; void gdk_pixbuf_scale (const GdkPixbuf* src, GdkPixbuf* dest, int dest_x, int dest_y, int dest_width, int dest_height, double offset_x, double offset_y, double scale_x, double scale_y, GdkInterpType interp_type)
  (define-function void gdk_pixbuf_scale (void* void* int int int int double double double double int))

  ;; GdkPixbuf* gdk_pixbuf_scale_simple (const GdkPixbuf* src, int dest_width, int dest_height, GdkInterpType interp_type)
  (define-function void* gdk_pixbuf_scale_simple (void* int int int))

  ;; void gdk_pixbuf_simple_anim_add_frame (GdkPixbufSimpleAnim* animation, GdkPixbuf* pixbuf)
  (define-function void gdk_pixbuf_simple_anim_add_frame (void* void*))

  ;; GType gdk_pixbuf_simple_anim_get_type (void)
  (define-function unsigned-long gdk_pixbuf_simple_anim_get_type ())

  ;; GType gdk_pixbuf_simple_anim_iter_get_type (void)
  (define-function unsigned-long gdk_pixbuf_simple_anim_iter_get_type ())

  ;; GdkPixbufSimpleAnim* gdk_pixbuf_simple_anim_new (gint width, gint height, gfloat rate)
  (define-function void* gdk_pixbuf_simple_anim_new (int int float))

  ;; void gdk_pixbuf_unref (GdkPixbuf* pixbuf)
  (define-function void gdk_pixbuf_unref (void*))

  ) ;[end]
