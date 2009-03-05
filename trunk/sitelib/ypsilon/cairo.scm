#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon cairo)
  (export cairo_append_path
          cairo_arc
          cairo_arc_negative
          cairo_clip
          cairo_clip_extents
          cairo_clip_preserve
          cairo_close_path
          cairo_copy_clip_rectangle_list
          cairo_copy_page
          cairo_copy_path
          cairo_copy_path_flat
          cairo_create
          cairo_curve_to
          cairo_debug_reset_static_data
          cairo_destroy
          cairo_device_to_user
          cairo_device_to_user_distance
          cairo_fill
          cairo_fill_extents
          cairo_fill_preserve
          cairo_font_extents
          cairo_font_face_destroy
          cairo_font_face_get_reference_count
          cairo_font_face_get_type
          cairo_font_face_get_user_data
          cairo_font_face_reference
          cairo_font_face_set_user_data
          cairo_font_face_status
          cairo_font_options_copy
          cairo_font_options_create
          cairo_font_options_destroy
          cairo_font_options_equal
          cairo_font_options_get_antialias
          cairo_font_options_get_hint_metrics
          cairo_font_options_get_hint_style
          cairo_font_options_get_subpixel_order
          cairo_font_options_hash
          cairo_font_options_merge
          cairo_font_options_set_antialias
          cairo_font_options_set_hint_metrics
          cairo_font_options_set_hint_style
          cairo_font_options_set_subpixel_order
          cairo_font_options_status
          cairo_format_stride_for_width
          cairo_get_antialias
          cairo_get_current_point
          cairo_get_dash
          cairo_get_dash_count
          cairo_get_fill_rule
          cairo_get_font_face
          cairo_get_font_matrix
          cairo_get_font_options
          cairo_get_group_target
          cairo_get_line_cap
          cairo_get_line_join
          cairo_get_line_width
          cairo_get_matrix
          cairo_get_miter_limit
          cairo_get_operator
          cairo_get_reference_count
          cairo_get_scaled_font
          cairo_get_source
          cairo_get_target
          cairo_get_tolerance
          cairo_get_user_data
          cairo_glyph_allocate
          cairo_glyph_extents
          cairo_glyph_free
          cairo_glyph_path
          cairo_has_current_point
          cairo_identity_matrix
          cairo_image_surface_create
          cairo_image_surface_create_for_data
          cairo_image_surface_create_from_png
          cairo_image_surface_create_from_png_stream
          cairo_image_surface_get_data
          cairo_image_surface_get_format
          cairo_image_surface_get_height
          cairo_image_surface_get_stride
          cairo_image_surface_get_width
          cairo_in_fill
          cairo_in_stroke
          cairo_line_to
          cairo_mask
          cairo_mask_surface
          cairo_matrix_init
          cairo_matrix_init_identity
          cairo_matrix_init_rotate
          cairo_matrix_init_scale
          cairo_matrix_init_translate
          cairo_matrix_invert
          cairo_matrix_multiply
          cairo_matrix_rotate
          cairo_matrix_scale
          cairo_matrix_transform_distance
          cairo_matrix_transform_point
          cairo_matrix_translate
          cairo_move_to
          cairo_new_path
          cairo_new_sub_path
          cairo_paint
          cairo_paint_with_alpha
          cairo_path_destroy
          cairo_path_extents
          cairo_pattern_add_color_stop_rgb
          cairo_pattern_add_color_stop_rgba
          cairo_pattern_create_for_surface
          cairo_pattern_create_linear
          cairo_pattern_create_radial
          cairo_pattern_create_rgb
          cairo_pattern_create_rgba
          cairo_pattern_destroy
          cairo_pattern_get_color_stop_count
          cairo_pattern_get_color_stop_rgba
          cairo_pattern_get_extend
          cairo_pattern_get_filter
          cairo_pattern_get_linear_points
          cairo_pattern_get_matrix
          cairo_pattern_get_radial_circles
          cairo_pattern_get_reference_count
          cairo_pattern_get_rgba
          cairo_pattern_get_surface
          cairo_pattern_get_type
          cairo_pattern_get_user_data
          cairo_pattern_reference
          cairo_pattern_set_extend
          cairo_pattern_set_filter
          cairo_pattern_set_matrix
          cairo_pattern_set_user_data
          cairo_pattern_status
          cairo_pop_group
          cairo_pop_group_to_source
          cairo_push_group
          cairo_push_group_with_content
          cairo_rectangle
          cairo_rectangle_list_destroy
          cairo_reference
          cairo_rel_curve_to
          cairo_rel_line_to
          cairo_rel_move_to
          cairo_reset_clip
          cairo_restore
          cairo_rotate
          cairo_save
          cairo_scale
          cairo_scaled_font_create
          cairo_scaled_font_destroy
          cairo_scaled_font_extents
          cairo_scaled_font_get_ctm
          cairo_scaled_font_get_font_face
          cairo_scaled_font_get_font_matrix
          cairo_scaled_font_get_font_options
          cairo_scaled_font_get_reference_count
          cairo_scaled_font_get_scale_matrix
          cairo_scaled_font_get_type
          cairo_scaled_font_get_user_data
          cairo_scaled_font_glyph_extents
          cairo_scaled_font_reference
          cairo_scaled_font_set_user_data
          cairo_scaled_font_status
          cairo_scaled_font_text_extents
          cairo_scaled_font_text_to_glyphs
          cairo_select_font_face
          cairo_set_antialias
          cairo_set_dash
          cairo_set_fill_rule
          cairo_set_font_face
          cairo_set_font_matrix
          cairo_set_font_options
          cairo_set_font_size
          cairo_set_line_cap
          cairo_set_line_join
          cairo_set_line_width
          cairo_set_matrix
          cairo_set_miter_limit
          cairo_set_operator
          cairo_set_scaled_font
          cairo_set_source
          cairo_set_source_rgb
          cairo_set_source_rgba
          cairo_set_source_surface
          cairo_set_tolerance
          cairo_set_user_data
          cairo_show_glyphs
          cairo_show_page
          cairo_show_text
          cairo_show_text_glyphs
          cairo_status
          cairo_status_to_string
          cairo_stroke
          cairo_stroke_extents
          cairo_stroke_preserve
          cairo_surface_copy_page
          cairo_surface_create_similar
          cairo_surface_destroy
          cairo_surface_finish
          cairo_surface_flush
          cairo_surface_get_content
          cairo_surface_get_device_offset
          cairo_surface_get_fallback_resolution
          cairo_surface_get_font_options
          cairo_surface_get_reference_count
          cairo_surface_get_type
          cairo_surface_get_user_data
          cairo_surface_has_show_text_glyphs
          cairo_surface_mark_dirty
          cairo_surface_mark_dirty_rectangle
          cairo_surface_reference
          cairo_surface_set_device_offset
          cairo_surface_set_fallback_resolution
          cairo_surface_set_user_data
          cairo_surface_show_page
          cairo_surface_status
          cairo_surface_write_to_png
          cairo_surface_write_to_png_stream
          cairo_text_cluster_allocate
          cairo_text_cluster_free
          cairo_text_extents
          cairo_text_path
          cairo_toy_font_face_create
          cairo_toy_font_face_get_family
          cairo_toy_font_face_get_slant
          cairo_toy_font_face_get_weight
          cairo_transform
          cairo_translate
          cairo_user_font_face_create
          cairo_user_font_face_get_init_func
          cairo_user_font_face_get_render_glyph_func
          cairo_user_font_face_get_text_to_glyphs_func
          cairo_user_font_face_get_unicode_to_glyph_func
          cairo_user_font_face_set_init_func
          cairo_user_font_face_set_render_glyph_func
          cairo_user_font_face_set_text_to_glyphs_func
          cairo_user_font_face_set_unicode_to_glyph_func
          cairo_user_to_device
          cairo_user_to_device_distance
          cairo_version
          cairo_version_string
          ;; cairo-ft.h
          cairo_ft_font_face_create_for_ft_face
          cairo_ft_font_face_create_for_pattern
          cairo_ft_font_options_substitute
          cairo_ft_scaled_font_lock_face
          cairo_ft_scaled_font_unlock_face
          ;; cairo-pdf.h
          cairo_pdf_surface_create
          cairo_pdf_surface_create_for_stream
          cairo_pdf_surface_set_size
          ;; cairo-ps.h
          cairo_ps_get_levels
          cairo_ps_level_to_string
          cairo_ps_surface_create
          cairo_ps_surface_create_for_stream
          cairo_ps_surface_dsc_begin_page_setup
          cairo_ps_surface_dsc_begin_setup
          cairo_ps_surface_dsc_comment
          cairo_ps_surface_get_eps
          cairo_ps_surface_restrict_to_level
          cairo_ps_surface_set_eps
          cairo_ps_surface_set_size
          ;; cairo-svg.h
          cairo_svg_get_versions
          cairo_svg_surface_create
          cairo_svg_surface_create_for_stream
          cairo_svg_surface_restrict_to_version
          cairo_svg_version_to_string
          ;; cairo-xcb.h
          cairo_xcb_surface_create
          cairo_xcb_surface_create_for_bitmap
          cairo_xcb_surface_set_size
          ;; cairo-xcb-xrender.h
          cairo_xcb_surface_create_with_xrender_format
          ;; cairo-xlib.h
          cairo_xlib_surface_create
          cairo_xlib_surface_create_for_bitmap
          cairo_xlib_surface_get_depth
          cairo_xlib_surface_get_display
          cairo_xlib_surface_get_drawable
          cairo_xlib_surface_get_height
          cairo_xlib_surface_get_screen
          cairo_xlib_surface_get_visual
          cairo_xlib_surface_get_width
          cairo_xlib_surface_get_xrender_format
          cairo_xlib_surface_set_drawable
          cairo_xlib_surface_set_size
          ;; cairo-xlib-xrender.h
          cairo_xlib_surface_create_with_xrender_format
          ;; cairo_antialias_t
          CAIRO_ANTIALIAS_DEFAULT
          CAIRO_ANTIALIAS_NONE
          CAIRO_ANTIALIAS_GRAY
          CAIRO_ANTIALIAS_SUBPIXEL
          ;; cairo_content_t
          CAIRO_CONTENT_COLOR
          CAIRO_CONTENT_ALPHA
          CAIRO_CONTENT_COLOR_ALPHA
          ;; cairo_extend_t
          CAIRO_EXTEND_NONE
          CAIRO_EXTEND_REPEAT
          CAIRO_EXTEND_REFLECT
          CAIRO_EXTEND_PAD
          ;; cairo_fill_rule_t
          CAIRO_FILL_RULE_WINDING
          CAIRO_FILL_RULE_EVEN_ODD
          ;; cairo_filter_t
          CAIRO_FILTER_FAST
          CAIRO_FILTER_GOOD
          CAIRO_FILTER_BEST
          CAIRO_FILTER_NEAREST
          CAIRO_FILTER_BILINEAR
          CAIRO_FILTER_GAUSSIAN
          ;; cairo_font_slant_t
          CAIRO_FONT_SLANT_NORMAL
          CAIRO_FONT_SLANT_ITALIC
          CAIRO_FONT_SLANT_OBLIQUE
          ;; cairo_font_type_t
          CAIRO_FONT_TYPE_TOY
          CAIRO_FONT_TYPE_FT
          CAIRO_FONT_TYPE_WIN32
          CAIRO_FONT_TYPE_QUARTZ
          CAIRO_FONT_TYPE_USER
          ;; cairo_font_weight_t
          CAIRO_FONT_WEIGHT_NORMAL
          CAIRO_FONT_WEIGHT_BOLD
          ;; cairo_format_t
          CAIRO_FORMAT_ARGB32
          CAIRO_FORMAT_RGB24
          CAIRO_FORMAT_A8
          CAIRO_FORMAT_A1
          ;; cairo_hint_metrics_t
          CAIRO_HINT_METRICS_DEFAULT
          CAIRO_HINT_METRICS_OFF
          CAIRO_HINT_METRICS_ON
          ;; cairo_hint_style_t
          CAIRO_HINT_STYLE_DEFAULT
          CAIRO_HINT_STYLE_NONE
          CAIRO_HINT_STYLE_SLIGHT
          CAIRO_HINT_STYLE_MEDIUM
          CAIRO_HINT_STYLE_FULL
          ;; cairo_line_cap_t
          CAIRO_LINE_CAP_BUTT
          CAIRO_LINE_CAP_ROUND
          CAIRO_LINE_CAP_SQUARE
          ;; cairo_line_join_t
          CAIRO_LINE_JOIN_MITER
          CAIRO_LINE_JOIN_ROUND
          CAIRO_LINE_JOIN_BEVEL
          ;; cairo_operator_t
          CAIRO_OPERATOR_CLEAR
          CAIRO_OPERATOR_SOURCE
          CAIRO_OPERATOR_OVER
          CAIRO_OPERATOR_IN
          CAIRO_OPERATOR_OUT
          CAIRO_OPERATOR_ATOP
          CAIRO_OPERATOR_DEST
          CAIRO_OPERATOR_DEST_OVER
          CAIRO_OPERATOR_DEST_IN
          CAIRO_OPERATOR_DEST_OUT
          CAIRO_OPERATOR_DEST_ATOP
          CAIRO_OPERATOR_XOR
          CAIRO_OPERATOR_ADD
          CAIRO_OPERATOR_SATURATE
          ;; cairo_path_data_type_t
          CAIRO_PATH_MOVE_TO
          CAIRO_PATH_LINE_TO
          CAIRO_PATH_CURVE_TO
          CAIRO_PATH_CLOSE_PATH
          ;; cairo_pattern_type_t
          CAIRO_PATTERN_TYPE_SOLID
          CAIRO_PATTERN_TYPE_SURFACE
          CAIRO_PATTERN_TYPE_LINEAR
          CAIRO_PATTERN_TYPE_RADIAL
          ;; cairo_status_t
          CAIRO_STATUS_SUCCESS
          CAIRO_STATUS_NO_MEMORY
          CAIRO_STATUS_INVALID_RESTORE
          CAIRO_STATUS_INVALID_POP_GROUP
          CAIRO_STATUS_NO_CURRENT_POINT
          CAIRO_STATUS_INVALID_MATRIX
          CAIRO_STATUS_INVALID_STATUS
          CAIRO_STATUS_NULL_POINTER
          CAIRO_STATUS_INVALID_STRING
          CAIRO_STATUS_INVALID_PATH_DATA
          CAIRO_STATUS_READ_ERROR
          CAIRO_STATUS_WRITE_ERROR
          CAIRO_STATUS_SURFACE_FINISHED
          CAIRO_STATUS_SURFACE_TYPE_MISMATCH
          CAIRO_STATUS_PATTERN_TYPE_MISMATCH
          CAIRO_STATUS_INVALID_CONTENT
          CAIRO_STATUS_INVALID_FORMAT
          CAIRO_STATUS_INVALID_VISUAL
          CAIRO_STATUS_FILE_NOT_FOUND
          CAIRO_STATUS_INVALID_DASH
          CAIRO_STATUS_INVALID_DSC_COMMENT
          CAIRO_STATUS_INVALID_INDEX
          CAIRO_STATUS_CLIP_NOT_REPRESENTABLE
          CAIRO_STATUS_TEMP_FILE_ERROR
          CAIRO_STATUS_INVALID_STRIDE
          CAIRO_STATUS_FONT_TYPE_MISMATCH
          CAIRO_STATUS_USER_FONT_IMMUTABLE
          CAIRO_STATUS_USER_FONT_ERROR
          CAIRO_STATUS_NEGATIVE_COUNT
          CAIRO_STATUS_INVALID_CLUSTERS
          CAIRO_STATUS_INVALID_SLANT
          CAIRO_STATUS_INVALID_WEIGHT
          ;; cairo_subpixel_order_t
          CAIRO_SUBPIXEL_ORDER_DEFAULT
          CAIRO_SUBPIXEL_ORDER_RGB
          CAIRO_SUBPIXEL_ORDER_BGR
          CAIRO_SUBPIXEL_ORDER_VRGB
          CAIRO_SUBPIXEL_ORDER_VBGR
          ;; cairo_surface_type_t
          CAIRO_SURFACE_TYPE_IMAGE
          CAIRO_SURFACE_TYPE_PDF
          CAIRO_SURFACE_TYPE_PS
          CAIRO_SURFACE_TYPE_XLIB
          CAIRO_SURFACE_TYPE_XCB
          CAIRO_SURFACE_TYPE_GLITZ
          CAIRO_SURFACE_TYPE_QUARTZ
          CAIRO_SURFACE_TYPE_WIN32
          CAIRO_SURFACE_TYPE_BEOS
          CAIRO_SURFACE_TYPE_DIRECTFB
          CAIRO_SURFACE_TYPE_SVG
          CAIRO_SURFACE_TYPE_OS2
          CAIRO_SURFACE_TYPE_WIN32_PRINTING
          CAIRO_SURFACE_TYPE_QUARTZ_IMAGE
          ;; cairo_text_cluster_flags_t
          CAIRO_TEXT_CLUSTER_FLAG_BACKWARD
          ;; cairo_ps_level_t
          CAIRO_PS_LEVEL_2
          CAIRO_PS_LEVEL_3
          ;; cairo_svg_version_t
          CAIRO_SVG_VERSION_1_1
          CAIRO_SVG_VERSION_1_2
          cairo_font_extents_t
          cairo_glyph_t
          cairo_matrix_t
          cairo_path_t
          cairo_rectangle_list_t
          cairo_rectangle_t
          cairo_text_cluster_t
          cairo_text_extents_t
          cairo_user_data_key_t)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Cairo.framework/Cairo")
          (on-linux   "libcairo.so.2")
          (on-freebsd "libcairo.so")
          (on-openbsd "libcairo.so")
          (on-windows "libcairo-2.dll")
          (else
           (assertion-violation #f "can not locate Cairo library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-c-typedef cairo_matrix_t
    (struct (double xx)
            (double yx)
            (double xy)
            (double yy)
            (double x0)
            (double y0)))

  (define-c-typedef cairo_user_data_key_t
    (struct (int unused)))

  (define-c-typedef cairo_rectangle_t
    (struct (double x)
            (double y)
            (double width)
            (double height)))

  (define-c-typedef cairo_rectangle_list_t
    (struct (int status)
            (void* rectangles)
            (int num_rectangles)))

  (define-c-typedef cairo_glyph_t
    (struct (unsigned-long index)
            (double x)
            (double y)))

  (define-c-typedef cairo_text_cluster_t
    (struct (int num_bytes)
            (int num_glyphs)))

  (define-c-typedef cairo_text_extents_t
    (struct (double x_bearing)
            (double y_bearing)
            (double width)
            (double height)
            (double x_advance)
            (double y_advance)))

  (define-c-typedef cairo_font_extents_t
    (struct (double ascent)
            (double descent)
            (double height)
            (double max_x_advance)
            (double max_y_advance)))

  (define-c-typedef cairo_path_t
    (struct (int status)
            (void* data)
            (int num_data)))

  ;;  void cairo_append_path (cairo_t* cr, const cairo_path_t* path)
  (define-function void cairo_append_path (void* void*))

  ;;  void cairo_arc (cairo_t* cr, double xc, double yc, double radius, double angle1, double angle2)
  (define-function void cairo_arc (void* double double double double double))

  ;;  void cairo_arc_negative (cairo_t* cr, double xc, double yc, double radius, double angle1, double angle2)
  (define-function void cairo_arc_negative (void* double double double double double))

  ;;  void cairo_clip (cairo_t* cr)
  (define-function void cairo_clip (void*))

  ;;  void cairo_clip_extents (cairo_t* cr, double* x1, double* y1, double* x2, double* y2)
  (define-function void cairo_clip_extents (void* void* void* void* void*))

  ;;  void cairo_clip_preserve (cairo_t* cr)
  (define-function void cairo_clip_preserve (void*))

  ;;  void cairo_close_path (cairo_t* cr)
  (define-function void cairo_close_path (void*))

  ;;  cairo_rectangle_list_t* cairo_copy_clip_rectangle_list (cairo_t* cr)
  (define-function void* cairo_copy_clip_rectangle_list (void*))

  ;;  void cairo_copy_page (cairo_t* cr)
  (define-function void cairo_copy_page (void*))

  ;;  cairo_path_t* cairo_copy_path (cairo_t* cr)
  (define-function void* cairo_copy_path (void*))

  ;;  cairo_path_t* cairo_copy_path_flat (cairo_t* cr)
  (define-function void* cairo_copy_path_flat (void*))

  ;;  cairo_t* cairo_create (cairo_surface_t* target)
  (define-function void* cairo_create (void*))

  ;;  void cairo_curve_to (cairo_t* cr, double x1, double y1, double x2, double y2, double x3, double y3)
  (define-function void cairo_curve_to (void* double double double double double double))

  ;;  void cairo_debug_reset_static_data (void)
  (define-function void cairo_debug_reset_static_data ())

  ;;  void cairo_destroy (cairo_t* cr)
  (define-function void cairo_destroy (void*))

  ;;  void cairo_device_to_user (cairo_t* cr, double* x, double* y)
  (define-function void cairo_device_to_user (void* void* void*))

  ;;  void cairo_device_to_user_distance (cairo_t* cr, double* dx, double* dy)
  (define-function void cairo_device_to_user_distance (void* void* void*))

  ;;  void cairo_fill (cairo_t* cr)
  (define-function void cairo_fill (void*))

  ;;  void cairo_fill_extents (cairo_t* cr, double* x1, double* y1, double* x2, double* y2)
  (define-function void cairo_fill_extents (void* void* void* void* void*))

  ;;  void cairo_fill_preserve (cairo_t* cr)
  (define-function void cairo_fill_preserve (void*))

  ;;  void cairo_font_extents (cairo_t* cr, cairo_font_extents_t* extents)
  (define-function void cairo_font_extents (void* void*))

  ;;  void cairo_font_face_destroy (cairo_font_face_t* font_face)
  (define-function void cairo_font_face_destroy (void*))

  ;;  unsigned int cairo_font_face_get_reference_count (cairo_font_face_t* font_face)
  (define-function unsigned-int cairo_font_face_get_reference_count (void*))

  ;;  cairo_font_type_t cairo_font_face_get_type (cairo_font_face_t* font_face)
  (define-function int cairo_font_face_get_type (void*))

  ;;  void* cairo_font_face_get_user_data (cairo_font_face_t* font_face, const cairo_user_data_key_t* key)
  (define-function void* cairo_font_face_get_user_data (void* void*))

  ;;  cairo_font_face_t* cairo_font_face_reference (cairo_font_face_t* font_face)
  (define-function void* cairo_font_face_reference (void*))

  ;;  cairo_status_t cairo_font_face_set_user_data (cairo_font_face_t* font_face, const cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy)
  (define-function int cairo_font_face_set_user_data (void* void* void* (c-callback void (void*))))

  ;;  cairo_status_t cairo_font_face_status (cairo_font_face_t* font_face)
  (define-function int cairo_font_face_status (void*))

  ;;  cairo_font_options_t* cairo_font_options_copy (const cairo_font_options_t* original)
  (define-function void* cairo_font_options_copy (void*))

  ;;  cairo_font_options_t* cairo_font_options_create (void)
  (define-function void* cairo_font_options_create ())

  ;;  void cairo_font_options_destroy (cairo_font_options_t* options)
  (define-function void cairo_font_options_destroy (void*))

  ;;  cairo_bool_t cairo_font_options_equal (const cairo_font_options_t* options, const cairo_font_options_t* other)
  (define-function int cairo_font_options_equal (void* void*))

  ;;  cairo_antialias_t cairo_font_options_get_antialias (const cairo_font_options_t* options)
  (define-function int cairo_font_options_get_antialias (void*))

  ;;  cairo_hint_metrics_t cairo_font_options_get_hint_metrics (const cairo_font_options_t* options)
  (define-function int cairo_font_options_get_hint_metrics (void*))

  ;;  cairo_hint_style_t cairo_font_options_get_hint_style (const cairo_font_options_t* options)
  (define-function int cairo_font_options_get_hint_style (void*))

  ;;  cairo_subpixel_order_t cairo_font_options_get_subpixel_order (const cairo_font_options_t* options)
  (define-function int cairo_font_options_get_subpixel_order (void*))

  ;;  unsigned long cairo_font_options_hash (const cairo_font_options_t* options)
  (define-function unsigned-long cairo_font_options_hash (void*))

  ;;  void cairo_font_options_merge (cairo_font_options_t* options, const cairo_font_options_t* other)
  (define-function void cairo_font_options_merge (void* void*))

  ;;  void cairo_font_options_set_antialias (cairo_font_options_t* options, cairo_antialias_t antialias)
  (define-function void cairo_font_options_set_antialias (void* int))

  ;;  void cairo_font_options_set_hint_metrics (cairo_font_options_t* options, cairo_hint_metrics_t hint_metrics)
  (define-function void cairo_font_options_set_hint_metrics (void* int))

  ;;  void cairo_font_options_set_hint_style (cairo_font_options_t* options, cairo_hint_style_t hint_style)
  (define-function void cairo_font_options_set_hint_style (void* int))

  ;;  void cairo_font_options_set_subpixel_order (cairo_font_options_t* options, cairo_subpixel_order_t subpixel_order)
  (define-function void cairo_font_options_set_subpixel_order (void* int))

  ;;  cairo_status_t cairo_font_options_status (cairo_font_options_t* options)
  (define-function int cairo_font_options_status (void*))

  ;;  int cairo_format_stride_for_width (cairo_format_t format, int width)
  (define-function int cairo_format_stride_for_width (int int))

  ;;  cairo_antialias_t cairo_get_antialias (cairo_t* cr)
  (define-function int cairo_get_antialias (void*))

  ;;  void cairo_get_current_point (cairo_t* cr, double* x, double* y)
  (define-function void cairo_get_current_point (void* void* void*))

  ;;  void cairo_get_dash (cairo_t* cr, double* dashes, double* offset)
  (define-function void cairo_get_dash (void* void* void*))

  ;;  int cairo_get_dash_count (cairo_t* cr)
  (define-function int cairo_get_dash_count (void*))

  ;;  cairo_fill_rule_t cairo_get_fill_rule (cairo_t* cr)
  (define-function int cairo_get_fill_rule (void*))

  ;;  cairo_font_face_t* cairo_get_font_face (cairo_t* cr)
  (define-function void* cairo_get_font_face (void*))

  ;;  void cairo_get_font_matrix (cairo_t* cr, cairo_matrix_t* matrix)
  (define-function void cairo_get_font_matrix (void* void*))

  ;;  void cairo_get_font_options (cairo_t* cr, cairo_font_options_t* options)
  (define-function void cairo_get_font_options (void* void*))

  ;;  cairo_surface_t* cairo_get_group_target (cairo_t* cr)
  (define-function void* cairo_get_group_target (void*))

  ;;  cairo_line_cap_t cairo_get_line_cap (cairo_t* cr)
  (define-function int cairo_get_line_cap (void*))

  ;;  cairo_line_join_t cairo_get_line_join (cairo_t* cr)
  (define-function int cairo_get_line_join (void*))

  ;;  double cairo_get_line_width (cairo_t* cr)
  (define-function double cairo_get_line_width (void*))

  ;;  void cairo_get_matrix (cairo_t* cr, cairo_matrix_t* matrix)
  (define-function void cairo_get_matrix (void* void*))

  ;;  double cairo_get_miter_limit (cairo_t* cr)
  (define-function double cairo_get_miter_limit (void*))

  ;;  cairo_operator_t cairo_get_operator (cairo_t* cr)
  (define-function int cairo_get_operator (void*))

  ;;  unsigned int cairo_get_reference_count (cairo_t* cr)
  (define-function unsigned-int cairo_get_reference_count (void*))

  ;;  cairo_scaled_font_t* cairo_get_scaled_font (cairo_t* cr)
  (define-function void* cairo_get_scaled_font (void*))

  ;;  cairo_pattern_t* cairo_get_source (cairo_t* cr)
  (define-function void* cairo_get_source (void*))

  ;;  cairo_surface_t* cairo_get_target (cairo_t* cr)
  (define-function void* cairo_get_target (void*))

  ;;  double cairo_get_tolerance (cairo_t* cr)
  (define-function double cairo_get_tolerance (void*))

  ;;  void* cairo_get_user_data (cairo_t* cr, const cairo_user_data_key_t* key)
  (define-function void* cairo_get_user_data (void* void*))

  ;;  cairo_glyph_t* cairo_glyph_allocate (int num_glyphs)
  (define-function void* cairo_glyph_allocate (int))

  ;;  void cairo_glyph_extents (cairo_t* cr, const cairo_glyph_t* glyphs, int num_glyphs, cairo_text_extents_t* extents)
  (define-function void cairo_glyph_extents (void* void* int void*))

  ;;  void cairo_glyph_free (cairo_glyph_t* glyphs)
  (define-function void cairo_glyph_free (void*))

  ;;  void cairo_glyph_path (cairo_t* cr, const cairo_glyph_t* glyphs, int num_glyphs)
  (define-function void cairo_glyph_path (void* void* int))

  ;;  cairo_bool_t cairo_has_current_point (cairo_t* cr)
  (define-function int cairo_has_current_point (void*))

  ;;  void cairo_identity_matrix (cairo_t* cr)
  (define-function void cairo_identity_matrix (void*))

  ;;  cairo_surface_t* cairo_image_surface_create (cairo_format_t format, int width, int height)
  (define-function void* cairo_image_surface_create (int int int))

  ;;  cairo_surface_t* cairo_image_surface_create_for_data (unsigned char* data, cairo_format_t format, int width, int height, int stride)
  (define-function void* cairo_image_surface_create_for_data (void* int int int int))

  ;;  cairo_surface_t* cairo_image_surface_create_from_png (const char* filename)
  (define-function void* cairo_image_surface_create_from_png (char*))

  ;;  cairo_surface_t* cairo_image_surface_create_from_png_stream (cairo_read_func_t read_func, void* closure)
  (define-function void* cairo_image_surface_create_from_png_stream ((c-callback int (void* void* unsigned-int)) void*))

  ;;  unsigned char* cairo_image_surface_get_data (cairo_surface_t* surface)
  (define-function void* cairo_image_surface_get_data (void*))

  ;;  cairo_format_t cairo_image_surface_get_format (cairo_surface_t* surface)
  (define-function int cairo_image_surface_get_format (void*))

  ;;  int cairo_image_surface_get_height (cairo_surface_t* surface)
  (define-function int cairo_image_surface_get_height (void*))

  ;;  int cairo_image_surface_get_stride (cairo_surface_t* surface)
  (define-function int cairo_image_surface_get_stride (void*))

  ;;  int cairo_image_surface_get_width (cairo_surface_t* surface)
  (define-function int cairo_image_surface_get_width (void*))

  ;;  cairo_bool_t cairo_in_fill (cairo_t* cr, double x, double y)
  (define-function int cairo_in_fill (void* double double))

  ;;  cairo_bool_t cairo_in_stroke (cairo_t* cr, double x, double y)
  (define-function int cairo_in_stroke (void* double double))

  ;;  void cairo_line_to (cairo_t* cr, double x, double y)
  (define-function void cairo_line_to (void* double double))

  ;;  void cairo_mask (cairo_t* cr, cairo_pattern_t* pattern)
  (define-function void cairo_mask (void* void*))

  ;;  void cairo_mask_surface (cairo_t* cr, cairo_surface_t* surface, double surface_x, double surface_y)
  (define-function void cairo_mask_surface (void* void* double double))

  ;;  void cairo_matrix_init (cairo_matrix_t* matrix, double xx, double yx, double xy, double yy, double x0, double y0)
  (define-function void cairo_matrix_init (void* double double double double double double))

  ;;  void cairo_matrix_init_identity (cairo_matrix_t* matrix)
  (define-function void cairo_matrix_init_identity (void*))

  ;;  void cairo_matrix_init_rotate (cairo_matrix_t* matrix, double radians)
  (define-function void cairo_matrix_init_rotate (void* double))

  ;;  void cairo_matrix_init_scale (cairo_matrix_t* matrix, double sx, double sy)
  (define-function void cairo_matrix_init_scale (void* double double))

  ;;  void cairo_matrix_init_translate (cairo_matrix_t* matrix, double tx, double ty)
  (define-function void cairo_matrix_init_translate (void* double double))

  ;;  cairo_status_t cairo_matrix_invert (cairo_matrix_t* matrix)
  (define-function int cairo_matrix_invert (void*))

  ;;  void cairo_matrix_multiply (cairo_matrix_t* result, const cairo_matrix_t* a, const cairo_matrix_t* b)
  (define-function void cairo_matrix_multiply (void* void* void*))

  ;;  void cairo_matrix_rotate (cairo_matrix_t* matrix, double radians)
  (define-function void cairo_matrix_rotate (void* double))

  ;;  void cairo_matrix_scale (cairo_matrix_t* matrix, double sx, double sy)
  (define-function void cairo_matrix_scale (void* double double))

  ;;  void cairo_matrix_transform_distance (const cairo_matrix_t* matrix, double* dx, double* dy)
  (define-function void cairo_matrix_transform_distance (void* void* void*))

  ;;  void cairo_matrix_transform_point (const cairo_matrix_t* matrix, double* x, double* y)
  (define-function void cairo_matrix_transform_point (void* void* void*))

  ;;  void cairo_matrix_translate (cairo_matrix_t* matrix, double tx, double ty)
  (define-function void cairo_matrix_translate (void* double double))

  ;;  void cairo_move_to (cairo_t* cr, double x, double y)
  (define-function void cairo_move_to (void* double double))

  ;;  void cairo_new_path (cairo_t* cr)
  (define-function void cairo_new_path (void*))

  ;;  void cairo_new_sub_path (cairo_t* cr)
  (define-function void cairo_new_sub_path (void*))

  ;;  void cairo_paint (cairo_t* cr)
  (define-function void cairo_paint (void*))

  ;;  void cairo_paint_with_alpha (cairo_t* cr, double alpha)
  (define-function void cairo_paint_with_alpha (void* double))

  ;;  void cairo_path_destroy (cairo_path_t* path)
  (define-function void cairo_path_destroy (void*))

  ;;  void cairo_path_extents (cairo_t* cr, double* x1, double* y1, double* x2, double* y2)
  (define-function void cairo_path_extents (void* void* void* void* void*))

  ;;  void cairo_pattern_add_color_stop_rgb (cairo_pattern_t* pattern, double offset, double red, double green, double blue)
  (define-function void cairo_pattern_add_color_stop_rgb (void* double double double double))

  ;;  void cairo_pattern_add_color_stop_rgba (cairo_pattern_t* pattern, double offset, double red, double green, double blue, double alpha)
  (define-function void cairo_pattern_add_color_stop_rgba (void* double double double double double))

  ;;  cairo_pattern_t* cairo_pattern_create_for_surface (cairo_surface_t* surface)
  (define-function void* cairo_pattern_create_for_surface (void*))

  ;;  cairo_pattern_t* cairo_pattern_create_linear (double x0, double y0, double x1, double y1)
  (define-function void* cairo_pattern_create_linear (double double double double))

  ;;  cairo_pattern_t* cairo_pattern_create_radial (double cx0, double cy0, double radius0, double cx1, double cy1, double radius1)
  (define-function void* cairo_pattern_create_radial (double double double double double double))

  ;;  cairo_pattern_t* cairo_pattern_create_rgb (double red, double green, double blue)
  (define-function void* cairo_pattern_create_rgb (double double double))

  ;;  cairo_pattern_t* cairo_pattern_create_rgba (double red, double green, double blue, double alpha)
  (define-function void* cairo_pattern_create_rgba (double double double double))

  ;;  void cairo_pattern_destroy (cairo_pattern_t* pattern)
  (define-function void cairo_pattern_destroy (void*))

  ;;  cairo_status_t cairo_pattern_get_color_stop_count (cairo_pattern_t* pattern, int* count)
  (define-function int cairo_pattern_get_color_stop_count (void* void*))

  ;;  cairo_status_t cairo_pattern_get_color_stop_rgba (cairo_pattern_t* pattern, int index, double* offset, double* red, double* green, double* blue, double* alpha)
  (define-function int cairo_pattern_get_color_stop_rgba (void* int void* void* void* void* void*))

  ;;  cairo_extend_t cairo_pattern_get_extend (cairo_pattern_t* pattern)
  (define-function int cairo_pattern_get_extend (void*))

  ;;  cairo_filter_t cairo_pattern_get_filter (cairo_pattern_t* pattern)
  (define-function int cairo_pattern_get_filter (void*))

  ;;  cairo_status_t cairo_pattern_get_linear_points (cairo_pattern_t* pattern, double* x0, double* y0, double* x1, double* y1)
  (define-function int cairo_pattern_get_linear_points (void* void* void* void* void*))

  ;;  void cairo_pattern_get_matrix (cairo_pattern_t* pattern, cairo_matrix_t* matrix)
  (define-function void cairo_pattern_get_matrix (void* void*))

  ;;  cairo_status_t cairo_pattern_get_radial_circles (cairo_pattern_t* pattern, double* x0, double* y0, double* r0, double* x1, double* y1, double* r1)
  (define-function int cairo_pattern_get_radial_circles (void* void* void* void* void* void* void*))

  ;;  unsigned int cairo_pattern_get_reference_count (cairo_pattern_t* pattern)
  (define-function unsigned-int cairo_pattern_get_reference_count (void*))

  ;;  cairo_status_t cairo_pattern_get_rgba (cairo_pattern_t* pattern, double* red, double* green, double* blue, double* alpha)
  (define-function int cairo_pattern_get_rgba (void* void* void* void* void*))

  ;;  cairo_status_t cairo_pattern_get_surface (cairo_pattern_t* pattern, cairo_surface_t** surface)
  (define-function int cairo_pattern_get_surface (void* void*))

  ;;  cairo_pattern_type_t cairo_pattern_get_type (cairo_pattern_t* pattern)
  (define-function int cairo_pattern_get_type (void*))

  ;;  void* cairo_pattern_get_user_data (cairo_pattern_t* pattern, const cairo_user_data_key_t* key)
  (define-function void* cairo_pattern_get_user_data (void* void*))

  ;;  cairo_pattern_t* cairo_pattern_reference (cairo_pattern_t* pattern)
  (define-function void* cairo_pattern_reference (void*))

  ;;  void cairo_pattern_set_extend (cairo_pattern_t* pattern, cairo_extend_t extend)
  (define-function void cairo_pattern_set_extend (void* int))

  ;;  void cairo_pattern_set_filter (cairo_pattern_t* pattern, cairo_filter_t filter)
  (define-function void cairo_pattern_set_filter (void* int))

  ;;  void cairo_pattern_set_matrix (cairo_pattern_t* pattern, const cairo_matrix_t* matrix)
  (define-function void cairo_pattern_set_matrix (void* void*))

  ;;  cairo_status_t cairo_pattern_set_user_data (cairo_pattern_t* pattern, const cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy)
  (define-function int cairo_pattern_set_user_data (void* void* void* (c-callback void (void*))))

  ;;  cairo_status_t cairo_pattern_status (cairo_pattern_t* pattern)
  (define-function int cairo_pattern_status (void*))

  ;;  cairo_pattern_t* cairo_pop_group (cairo_t* cr)
  (define-function void* cairo_pop_group (void*))

  ;;  void cairo_pop_group_to_source (cairo_t* cr)
  (define-function void cairo_pop_group_to_source (void*))

  ;;  void cairo_push_group (cairo_t* cr)
  (define-function void cairo_push_group (void*))

  ;;  void cairo_push_group_with_content (cairo_t* cr, cairo_content_t content)
  (define-function void cairo_push_group_with_content (void* int))

  ;;  void cairo_rectangle (cairo_t* cr, double x, double y, double width, double height)
  (define-function void cairo_rectangle (void* double double double double))

  ;;  void cairo_rectangle_list_destroy (cairo_rectangle_list_t* rectangle_list)
  (define-function void cairo_rectangle_list_destroy (void*))

  ;;  cairo_t* cairo_reference (cairo_t* cr)
  (define-function void* cairo_reference (void*))

  ;;  void cairo_rel_curve_to (cairo_t* cr, double dx1, double dy1, double dx2, double dy2, double dx3, double dy3)
  (define-function void cairo_rel_curve_to (void* double double double double double double))

  ;;  void cairo_rel_line_to (cairo_t* cr, double dx, double dy)
  (define-function void cairo_rel_line_to (void* double double))

  ;;  void cairo_rel_move_to (cairo_t* cr, double dx, double dy)
  (define-function void cairo_rel_move_to (void* double double))

  ;;  void cairo_reset_clip (cairo_t* cr)
  (define-function void cairo_reset_clip (void*))

  ;;  void cairo_restore (cairo_t* cr)
  (define-function void cairo_restore (void*))

  ;;  void cairo_rotate (cairo_t* cr, double angle)
  (define-function void cairo_rotate (void* double))

  ;;  void cairo_save (cairo_t* cr)
  (define-function void cairo_save (void*))

  ;;  void cairo_scale (cairo_t* cr, double sx, double sy)
  (define-function void cairo_scale (void* double double))

  ;;  cairo_scaled_font_t* cairo_scaled_font_create (cairo_font_face_t* font_face, const cairo_matrix_t* font_matrix, const cairo_matrix_t* ctm, const cairo_font_options_t* options)
  (define-function void* cairo_scaled_font_create (void* void* void* void*))

  ;;  void cairo_scaled_font_destroy (cairo_scaled_font_t* scaled_font)
  (define-function void cairo_scaled_font_destroy (void*))

  ;;  void cairo_scaled_font_extents (cairo_scaled_font_t* scaled_font, cairo_font_extents_t* extents)
  (define-function void cairo_scaled_font_extents (void* void*))

  ;;  void cairo_scaled_font_get_ctm (cairo_scaled_font_t* scaled_font, cairo_matrix_t* ctm)
  (define-function void cairo_scaled_font_get_ctm (void* void*))

  ;;  cairo_font_face_t* cairo_scaled_font_get_font_face (cairo_scaled_font_t* scaled_font)
  (define-function void* cairo_scaled_font_get_font_face (void*))

  ;;  void cairo_scaled_font_get_font_matrix (cairo_scaled_font_t* scaled_font, cairo_matrix_t* font_matrix)
  (define-function void cairo_scaled_font_get_font_matrix (void* void*))

  ;;  void cairo_scaled_font_get_font_options (cairo_scaled_font_t* scaled_font, cairo_font_options_t* options)
  (define-function void cairo_scaled_font_get_font_options (void* void*))

  ;;  unsigned int cairo_scaled_font_get_reference_count (cairo_scaled_font_t* scaled_font)
  (define-function unsigned-int cairo_scaled_font_get_reference_count (void*))

  ;;  void cairo_scaled_font_get_scale_matrix (cairo_scaled_font_t* scaled_font, cairo_matrix_t* scale_matrix)
  (define-function void cairo_scaled_font_get_scale_matrix (void* void*))

  ;;  cairo_font_type_t cairo_scaled_font_get_type (cairo_scaled_font_t* scaled_font)
  (define-function int cairo_scaled_font_get_type (void*))

  ;;  void* cairo_scaled_font_get_user_data (cairo_scaled_font_t* scaled_font, const cairo_user_data_key_t* key)
  (define-function void* cairo_scaled_font_get_user_data (void* void*))

  ;;  void cairo_scaled_font_glyph_extents (cairo_scaled_font_t* scaled_font, const cairo_glyph_t* glyphs, int num_glyphs, cairo_text_extents_t* extents)
  (define-function void cairo_scaled_font_glyph_extents (void* void* int void*))

  ;;  cairo_scaled_font_t* cairo_scaled_font_reference (cairo_scaled_font_t* scaled_font)
  (define-function void* cairo_scaled_font_reference (void*))

  ;;  cairo_status_t cairo_scaled_font_set_user_data (cairo_scaled_font_t* scaled_font, const cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy)
  (define-function int cairo_scaled_font_set_user_data (void* void* void* (c-callback void (void*))))

  ;;  cairo_status_t cairo_scaled_font_status (cairo_scaled_font_t* scaled_font)
  (define-function int cairo_scaled_font_status (void*))

  ;;  void cairo_scaled_font_text_extents (cairo_scaled_font_t* scaled_font, const char* utf8, cairo_text_extents_t* extents)
  (define-function void cairo_scaled_font_text_extents (void* char* void*))

  ;;  cairo_status_t cairo_scaled_font_text_to_glyphs (cairo_scaled_font_t* scaled_font, double x, double y, const char* utf8, int utf8_len, cairo_glyph_t** glyphs, int* num_glyphs, cairo_text_cluster_t** clusters, int* num_clusters, cairo_text_cluster_flags_t* cluster_flags)
  (define-function int cairo_scaled_font_text_to_glyphs (void* double double char* int void* void* void* void* void*))

  ;;  void cairo_select_font_face (cairo_t* cr, const char* family, cairo_font_slant_t slant, cairo_font_weight_t weight)
  (define-function void cairo_select_font_face (void* char* int int))

  ;;  void cairo_set_antialias (cairo_t* cr, cairo_antialias_t antialias)
  (define-function void cairo_set_antialias (void* int))

  ;;  void cairo_set_dash (cairo_t* cr, const double* dashes, int num_dashes, double offset)
  (define-function void cairo_set_dash (void* void* int double))

  ;;  void cairo_set_fill_rule (cairo_t* cr, cairo_fill_rule_t fill_rule)
  (define-function void cairo_set_fill_rule (void* int))

  ;;  void cairo_set_font_face (cairo_t* cr, cairo_font_face_t* font_face)
  (define-function void cairo_set_font_face (void* void*))

  ;;  void cairo_set_font_matrix (cairo_t* cr, const cairo_matrix_t* matrix)
  (define-function void cairo_set_font_matrix (void* void*))

  ;;  void cairo_set_font_options (cairo_t* cr, const cairo_font_options_t* options)
  (define-function void cairo_set_font_options (void* void*))

  ;;  void cairo_set_font_size (cairo_t* cr, double size)
  (define-function void cairo_set_font_size (void* double))

  ;;  void cairo_set_line_cap (cairo_t* cr, cairo_line_cap_t line_cap)
  (define-function void cairo_set_line_cap (void* int))

  ;;  void cairo_set_line_join (cairo_t* cr, cairo_line_join_t line_join)
  (define-function void cairo_set_line_join (void* int))

  ;;  void cairo_set_line_width (cairo_t* cr, double width)
  (define-function void cairo_set_line_width (void* double))

  ;;  void cairo_set_matrix (cairo_t* cr, const cairo_matrix_t* matrix)
  (define-function void cairo_set_matrix (void* void*))

  ;;  void cairo_set_miter_limit (cairo_t* cr, double limit)
  (define-function void cairo_set_miter_limit (void* double))

  ;;  void cairo_set_operator (cairo_t* cr, cairo_operator_t op)
  (define-function void cairo_set_operator (void* int))

  ;;  void cairo_set_scaled_font (cairo_t* cr, const cairo_scaled_font_t* scaled_font)
  (define-function void cairo_set_scaled_font (void* void*))

  ;;  void cairo_set_source (cairo_t* cr, cairo_pattern_t* source)
  (define-function void cairo_set_source (void* void*))

  ;;  void cairo_set_source_rgb (cairo_t* cr, double red, double green, double blue)
  (define-function void cairo_set_source_rgb (void* double double double))

  ;;  void cairo_set_source_rgba (cairo_t* cr, double red, double green, double blue, double alpha)
  (define-function void cairo_set_source_rgba (void* double double double double))

  ;;  void cairo_set_source_surface (cairo_t* cr, cairo_surface_t* surface, double x, double y)
  (define-function void cairo_set_source_surface (void* void* double double))

  ;;  void cairo_set_tolerance (cairo_t* cr, double tolerance)
  (define-function void cairo_set_tolerance (void* double))

  ;;  cairo_status_t cairo_set_user_data (cairo_t* cr, const cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy)
  (define-function int cairo_set_user_data (void* void* void* (c-callback void (void*))))

  ;;  void cairo_show_glyphs (cairo_t* cr, const cairo_glyph_t* glyphs, int num_glyphs)
  (define-function void cairo_show_glyphs (void* void* int))

  ;;  void cairo_show_page (cairo_t* cr)
  (define-function void cairo_show_page (void*))

  ;;  void cairo_show_text (cairo_t* cr, const char* utf8)
  (define-function void cairo_show_text (void* char*))

  ;;  void cairo_show_text_glyphs (cairo_t* cr, const char* utf8, int utf8_len, const cairo_glyph_t* glyphs, int num_glyphs, const cairo_text_cluster_t* clusters, int num_clusters, cairo_text_cluster_flags_t cluster_flags)
  (define-function void cairo_show_text_glyphs (void* char* int void* int void* int int))

  ;;  cairo_status_t cairo_status (cairo_t* cr)
  (define-function int cairo_status (void*))

  ;;  const char* cairo_status_to_string (cairo_status_t status)
  (define-function char* cairo_status_to_string (int))

  ;;  void cairo_stroke (cairo_t* cr)
  (define-function void cairo_stroke (void*))

  ;;  void cairo_stroke_extents (cairo_t* cr, double* x1, double* y1, double* x2, double* y2)
  (define-function void cairo_stroke_extents (void* void* void* void* void*))

  ;;  void cairo_stroke_preserve (cairo_t* cr)
  (define-function void cairo_stroke_preserve (void*))

  ;;  void cairo_surface_copy_page (cairo_surface_t* surface)
  (define-function void cairo_surface_copy_page (void*))

  ;;  cairo_surface_t* cairo_surface_create_similar (cairo_surface_t* other, cairo_content_t content, int width, int height)
  (define-function void* cairo_surface_create_similar (void* int int int))

  ;;  void cairo_surface_destroy (cairo_surface_t* surface)
  (define-function void cairo_surface_destroy (void*))

  ;;  void cairo_surface_finish (cairo_surface_t* surface)
  (define-function void cairo_surface_finish (void*))

  ;;  void cairo_surface_flush (cairo_surface_t* surface)
  (define-function void cairo_surface_flush (void*))

  ;;  cairo_content_t cairo_surface_get_content (cairo_surface_t* surface)
  (define-function int cairo_surface_get_content (void*))

  ;;  void cairo_surface_get_device_offset (cairo_surface_t* surface, double* x_offset, double* y_offset)
  (define-function void cairo_surface_get_device_offset (void* void* void*))

  ;;  void cairo_surface_get_fallback_resolution (cairo_surface_t* surface, double* x_pixels_per_inch, double* y_pixels_per_inch)
  (define-function void cairo_surface_get_fallback_resolution (void* void* void*))

  ;;  void cairo_surface_get_font_options (cairo_surface_t* surface, cairo_font_options_t* options)
  (define-function void cairo_surface_get_font_options (void* void*))

  ;;  unsigned int cairo_surface_get_reference_count (cairo_surface_t* surface)
  (define-function unsigned-int cairo_surface_get_reference_count (void*))

  ;;  cairo_surface_type_t cairo_surface_get_type (cairo_surface_t* surface)
  (define-function int cairo_surface_get_type (void*))

  ;;  void* cairo_surface_get_user_data (cairo_surface_t* surface, const cairo_user_data_key_t* key)
  (define-function void* cairo_surface_get_user_data (void* void*))

  ;;  cairo_bool_t cairo_surface_has_show_text_glyphs (cairo_surface_t* surface)
  (define-function int cairo_surface_has_show_text_glyphs (void*))

  ;;  void cairo_surface_mark_dirty (cairo_surface_t* surface)
  (define-function void cairo_surface_mark_dirty (void*))

  ;;  void cairo_surface_mark_dirty_rectangle (cairo_surface_t* surface, int x, int y, int width, int height)
  (define-function void cairo_surface_mark_dirty_rectangle (void* int int int int))

  ;;  cairo_surface_t* cairo_surface_reference (cairo_surface_t* surface)
  (define-function void* cairo_surface_reference (void*))

  ;;  void cairo_surface_set_device_offset (cairo_surface_t* surface, double x_offset, double y_offset)
  (define-function void cairo_surface_set_device_offset (void* double double))

  ;;  void cairo_surface_set_fallback_resolution (cairo_surface_t* surface, double x_pixels_per_inch, double y_pixels_per_inch)
  (define-function void cairo_surface_set_fallback_resolution (void* double double))

  ;;  cairo_status_t cairo_surface_set_user_data (cairo_surface_t* surface, const cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy)
  (define-function int cairo_surface_set_user_data (void* void* void* (c-callback void (void*))))

  ;;  void cairo_surface_show_page (cairo_surface_t* surface)
  (define-function void cairo_surface_show_page (void*))

  ;;  cairo_status_t cairo_surface_status (cairo_surface_t* surface)
  (define-function int cairo_surface_status (void*))

  ;;  cairo_status_t cairo_surface_write_to_png (cairo_surface_t* surface, const char* filename)
  (define-function int cairo_surface_write_to_png (void* char*))

  ;;  cairo_status_t cairo_surface_write_to_png_stream (cairo_surface_t* surface, cairo_write_func_t write_func, void* closure)
  (define-function int cairo_surface_write_to_png_stream (void* (c-callback int (void* void* unsigned-int)) void*))

  ;;  cairo_text_cluster_t* cairo_text_cluster_allocate (int num_clusters)
  (define-function void* cairo_text_cluster_allocate (int))

  ;;  void cairo_text_cluster_free (cairo_text_cluster_t* clusters)
  (define-function void cairo_text_cluster_free (void*))

  ;;  void cairo_text_extents (cairo_t* cr, const char* utf8, cairo_text_extents_t* extents)
  (define-function void cairo_text_extents (void* char* void*))

  ;;  void cairo_text_path (cairo_t* cr, const char* utf8)
  (define-function void cairo_text_path (void* char*))

  ;;  cairo_font_face_t* cairo_toy_font_face_create (const char* family, cairo_font_slant_t slant, cairo_font_weight_t weight)
  (define-function void* cairo_toy_font_face_create (char* int int))

  ;;  const char* cairo_toy_font_face_get_family (cairo_font_face_t* font_face)
  (define-function char* cairo_toy_font_face_get_family (void*))

  ;;  cairo_font_slant_t cairo_toy_font_face_get_slant (cairo_font_face_t* font_face)
  (define-function int cairo_toy_font_face_get_slant (void*))

  ;;  cairo_font_weight_t cairo_toy_font_face_get_weight (cairo_font_face_t* font_face)
  (define-function int cairo_toy_font_face_get_weight (void*))

  ;;  void cairo_transform (cairo_t* cr, const cairo_matrix_t* matrix)
  (define-function void cairo_transform (void* void*))

  ;;  void cairo_translate (cairo_t* cr, double tx, double ty)
  (define-function void cairo_translate (void* double double))

  ;;  cairo_font_face_t* cairo_user_font_face_create (void)
  (define-function void* cairo_user_font_face_create ())

  ;;  cairo_user_scaled_font_init_func_t cairo_user_font_face_get_init_func (cairo_font_face_t* font_face)
  (define-function void* cairo_user_font_face_get_init_func (void*))

  ;;  cairo_user_scaled_font_render_glyph_func_t cairo_user_font_face_get_render_glyph_func (cairo_font_face_t* font_face)
  (define-function void* cairo_user_font_face_get_render_glyph_func (void*))

  ;;  cairo_user_scaled_font_text_to_glyphs_func_t cairo_user_font_face_get_text_to_glyphs_func (cairo_font_face_t* font_face)
  (define-function void* cairo_user_font_face_get_text_to_glyphs_func (void*))

  ;;  cairo_user_scaled_font_unicode_to_glyph_func_t cairo_user_font_face_get_unicode_to_glyph_func (cairo_font_face_t* font_face)
  (define-function void* cairo_user_font_face_get_unicode_to_glyph_func (void*))

  ;;  void cairo_user_font_face_set_init_func (cairo_font_face_t* font_face, cairo_user_scaled_font_init_func_t init_func)
  (define-function void cairo_user_font_face_set_init_func (void* void*))

  ;;  void cairo_user_font_face_set_render_glyph_func (cairo_font_face_t* font_face, cairo_user_scaled_font_render_glyph_func_t render_glyph_func)
  (define-function void cairo_user_font_face_set_render_glyph_func (void* void*))

  ;;  void cairo_user_font_face_set_text_to_glyphs_func (cairo_font_face_t* font_face, cairo_user_scaled_font_text_to_glyphs_func_t text_to_glyphs_func)
  (define-function void cairo_user_font_face_set_text_to_glyphs_func (void* void*))

  ;;  void cairo_user_font_face_set_unicode_to_glyph_func (cairo_font_face_t* font_face, cairo_user_scaled_font_unicode_to_glyph_func_t unicode_to_glyph_func)
  (define-function void cairo_user_font_face_set_unicode_to_glyph_func (void* void*))

  ;;  void cairo_user_to_device (cairo_t* cr, double* x, double* y)
  (define-function void cairo_user_to_device (void* void* void*))

  ;;  void cairo_user_to_device_distance (cairo_t* cr, double* dx, double* dy)
  (define-function void cairo_user_to_device_distance (void* void* void*))

  ;;  int cairo_version (void)
  (define-function int cairo_version ())

  ;;  const char* cairo_version_string (void)
  (define-function char* cairo_version_string ())

  ;;; cairo-ft.h

  ;;  cairo_font_face_t* cairo_ft_font_face_create_for_ft_face (FT_Face face, int load_flags)
  (define-function void* cairo_ft_font_face_create_for_ft_face (void* int))

  ;;  cairo_font_face_t* cairo_ft_font_face_create_for_pattern (FcPattern* pattern)
  (define-function void* cairo_ft_font_face_create_for_pattern (void*))

  ;;  void cairo_ft_font_options_substitute (const cairo_font_options_t* options, FcPattern* pattern)
  (define-function void cairo_ft_font_options_substitute (void* void*))

  ;;  FT_Face cairo_ft_scaled_font_lock_face (cairo_scaled_font_t* scaled_font)
  (define-function void* cairo_ft_scaled_font_lock_face (void*))

  ;;  void cairo_ft_scaled_font_unlock_face (cairo_scaled_font_t* scaled_font)
  (define-function void cairo_ft_scaled_font_unlock_face (void*))

  ;;; cairo-pdf.h

  ;;  cairo_surface_t* cairo_pdf_surface_create (const char* filename, double width_in_points, double height_in_points)
  (define-function void* cairo_pdf_surface_create (char* double double))

  ;;  cairo_surface_t* cairo_pdf_surface_create_for_stream (cairo_write_func_t write_func, void* closure, double width_in_points, double height_in_points)
  (define-function void* cairo_pdf_surface_create_for_stream ((c-callback int (void* void* unsigned-int)) void* double double))

  ;;  void cairo_pdf_surface_set_size (cairo_surface_t* surface, double width_in_points, double height_in_points)
  (define-function void cairo_pdf_surface_set_size (void* double double))

  ;;; cairo-ps.h  

  ;;  void cairo_ps_get_levels (cairo_ps_level_t const** levels, int* num_levels)
  (define-function void cairo_ps_get_levels (void* void*))

  ;;  const char* cairo_ps_level_to_string (cairo_ps_level_t level)
  (define-function char* cairo_ps_level_to_string (int))

  ;;  cairo_surface_t* cairo_ps_surface_create (const char* filename, double width_in_points, double height_in_points)
  (define-function void* cairo_ps_surface_create (char* double double))

  ;;  cairo_surface_t* cairo_ps_surface_create_for_stream (cairo_write_func_t write_func, void* closure, double width_in_points, double height_in_points)
  (define-function void* cairo_ps_surface_create_for_stream ((c-callback int (void* void* unsigned-int)) void* double double))

  ;;  void cairo_ps_surface_dsc_begin_page_setup (cairo_surface_t* surface)
  (define-function void cairo_ps_surface_dsc_begin_page_setup (void*))

  ;;  void cairo_ps_surface_dsc_begin_setup (cairo_surface_t* surface)
  (define-function void cairo_ps_surface_dsc_begin_setup (void*))

  ;;  void cairo_ps_surface_dsc_comment (cairo_surface_t* surface, const char* comment)
  (define-function void cairo_ps_surface_dsc_comment (void* char*))

  ;;  cairo_bool_t cairo_ps_surface_get_eps (cairo_surface_t* surface)
  (define-function int cairo_ps_surface_get_eps (void*))

  ;;  void cairo_ps_surface_restrict_to_level (cairo_surface_t* surface, cairo_ps_level_t level)
  (define-function void cairo_ps_surface_restrict_to_level (void* int))

  ;;  void cairo_ps_surface_set_eps (cairo_surface_t* surface, cairo_bool_t eps)
  (define-function void cairo_ps_surface_set_eps (void* int))

  ;;  void cairo_ps_surface_set_size (cairo_surface_t* surface, double width_in_points, double height_in_points)
  (define-function void cairo_ps_surface_set_size (void* double double))

  ;;; cairo-svg.h

  ;;  void cairo_svg_get_versions (cairo_svg_version_t const** versions, int* num_versions)
  (define-function void cairo_svg_get_versions (void* void*))

  ;;  cairo_surface_t* cairo_svg_surface_create (const char* filename, double width_in_points, double height_in_points)
  (define-function void* cairo_svg_surface_create (char* double double))

  ;;  cairo_surface_t* cairo_svg_surface_create_for_stream (cairo_write_func_t write_func, void* closure, double width_in_points, double height_in_points)
  (define-function void* cairo_svg_surface_create_for_stream ((c-callback int (void* void* unsigned-int)) void* double double))

  ;;  void cairo_svg_surface_restrict_to_version (cairo_surface_t* surface, cairo_svg_version_t version)
  (define-function void cairo_svg_surface_restrict_to_version (void* int))

  ;;  const char* cairo_svg_version_to_string (cairo_svg_version_t version)
  (define-function char* cairo_svg_version_to_string (int))

  ;;; cairo-xcb.h

  ;;  cairo_surface_t* cairo_xcb_surface_create (xcb_connection_t* c, xcb_drawable_t drawable, xcb_visualtype_t* visual, int width, int height)
  (define-function void* cairo_xcb_surface_create (void* uint32_t void* int int))

  ;;  cairo_surface_t* cairo_xcb_surface_create_for_bitmap (xcb_connection_t* c, xcb_pixmap_t bitmap, xcb_screen_t* screen, int width, int height)
  (define-function void* cairo_xcb_surface_create_for_bitmap (void* uint32_t void* int int))

  ;;  void cairo_xcb_surface_set_size (cairo_surface_t* surface, int width, int height)
  (define-function void cairo_xcb_surface_set_size (void* int int))

  ;;; cairo-xcb-xrender.h

  ;;  cairo_surface_t* cairo_xcb_surface_create_with_xrender_format (xcb_connection_t* c, xcb_drawable_t drawable, xcb_screen_t* screen, xcb_render_pictforminfo_t* format, int width, int height)
  (define-function void* cairo_xcb_surface_create_with_xrender_format (void* uint32_t void* void* int int))

  ;;; cairo-xlib.h

  ;;  cairo_surface_t* cairo_xlib_surface_create (Display* dpy, Drawable drawable, Visual* visual, int width, int height)
  (define-function void* cairo_xlib_surface_create (void* unsigned-long void* int int))

  ;;  cairo_surface_t* cairo_xlib_surface_create_for_bitmap (Display* dpy, Pixmap bitmap, Screen* screen, int width, int height)
  (define-function void* cairo_xlib_surface_create_for_bitmap (void* unsigned-long void* int int))

  ;;  cairo_surface_t* cairo_xlib_surface_create_with_xrender_format (Display* dpy, Drawable drawable, Screen* screen, XRenderPictFormat* format, int width, int height)
  (define-function void* cairo_xlib_surface_create_with_xrender_format (void* unsigned-long void* void* int int))

  ;;  int cairo_xlib_surface_get_depth (cairo_surface_t* surface)
  (define-function int cairo_xlib_surface_get_depth (void*))

  ;;  Display* cairo_xlib_surface_get_display (cairo_surface_t* surface)
  (define-function void* cairo_xlib_surface_get_display (void*))

  ;;  Drawable cairo_xlib_surface_get_drawable (cairo_surface_t* surface)
  (define-function unsigned-long cairo_xlib_surface_get_drawable (void*))

  ;;  int cairo_xlib_surface_get_height (cairo_surface_t* surface)
  (define-function int cairo_xlib_surface_get_height (void*))

  ;;  Screen* cairo_xlib_surface_get_screen (cairo_surface_t* surface)
  (define-function void* cairo_xlib_surface_get_screen (void*))

  ;;  Visual* cairo_xlib_surface_get_visual (cairo_surface_t* surface)
  (define-function void* cairo_xlib_surface_get_visual (void*))

  ;;  int cairo_xlib_surface_get_width (cairo_surface_t* surface)
  (define-function int cairo_xlib_surface_get_width (void*))

  ;;  void cairo_xlib_surface_set_drawable (cairo_surface_t* surface, Drawable drawable, int width, int height)
  (define-function void cairo_xlib_surface_set_drawable (void* unsigned-long int int))

  ;;  void cairo_xlib_surface_set_size (cairo_surface_t* surface, int width, int height)
  (define-function void cairo_xlib_surface_set_size (void* int int))

  ;;; cairo-xlib-xrender.h

  ;;  XRenderPictFormat* cairo_xlib_surface_get_xrender_format (cairo_surface_t* surface)
  (define-function void* cairo_xlib_surface_get_xrender_format (void*))

  ;; cairo_antialias_t
  (define-c-enum CAIRO_ANTIALIAS_DEFAULT
                 CAIRO_ANTIALIAS_NONE
                 CAIRO_ANTIALIAS_GRAY
                 CAIRO_ANTIALIAS_SUBPIXEL)

  ;; cairo_content_t
  (define-c-enum (CAIRO_CONTENT_COLOR . #x1000)
                 (CAIRO_CONTENT_ALPHA . #x2000)
                 (CAIRO_CONTENT_COLOR_ALPHA . #x3000))

  ;; cairo_extend_t
  (define-c-enum CAIRO_EXTEND_NONE
                 CAIRO_EXTEND_REPEAT
                 CAIRO_EXTEND_REFLECT
                 CAIRO_EXTEND_PAD)

  ;; cairo_fill_rule_t
  (define-c-enum CAIRO_FILL_RULE_WINDING
                 CAIRO_FILL_RULE_EVEN_ODD)

  ;; cairo_filter_t
  (define-c-enum CAIRO_FILTER_FAST
                 CAIRO_FILTER_GOOD
                 CAIRO_FILTER_BEST
                 CAIRO_FILTER_NEAREST
                 CAIRO_FILTER_BILINEAR
                 CAIRO_FILTER_GAUSSIAN)

  ;; cairo_font_slant_t
  (define-c-enum CAIRO_FONT_SLANT_NORMAL
                 CAIRO_FONT_SLANT_ITALIC
                 CAIRO_FONT_SLANT_OBLIQUE)

  ;; cairo_font_type_t
  (define-c-enum CAIRO_FONT_TYPE_TOY
                 CAIRO_FONT_TYPE_FT
                 CAIRO_FONT_TYPE_WIN32
                 CAIRO_FONT_TYPE_QUARTZ
                 CAIRO_FONT_TYPE_USER)

  ;; cairo_font_weight_t
  (define-c-enum CAIRO_FONT_WEIGHT_NORMAL
                 CAIRO_FONT_WEIGHT_BOLD)

  ;; cairo_format_t
  (define-c-enum CAIRO_FORMAT_ARGB32
                 CAIRO_FORMAT_RGB24
                 CAIRO_FORMAT_A8
                 CAIRO_FORMAT_A1)

  ;; cairo_hint_metrics_t
  (define-c-enum CAIRO_HINT_METRICS_DEFAULT
                 CAIRO_HINT_METRICS_OFF
                 CAIRO_HINT_METRICS_ON)

  ;; cairo_hint_style_t
  (define-c-enum CAIRO_HINT_STYLE_DEFAULT
                 CAIRO_HINT_STYLE_NONE
                 CAIRO_HINT_STYLE_SLIGHT
                 CAIRO_HINT_STYLE_MEDIUM
                 CAIRO_HINT_STYLE_FULL)

  ;; cairo_line_cap_t
  (define-c-enum CAIRO_LINE_CAP_BUTT
                 CAIRO_LINE_CAP_ROUND
                 CAIRO_LINE_CAP_SQUARE)

  ;; cairo_line_join_t
  (define-c-enum CAIRO_LINE_JOIN_MITER
                 CAIRO_LINE_JOIN_ROUND
                 CAIRO_LINE_JOIN_BEVEL)

  ;; cairo_operator_t
  (define-c-enum CAIRO_OPERATOR_CLEAR
                 CAIRO_OPERATOR_SOURCE
                 CAIRO_OPERATOR_OVER
                 CAIRO_OPERATOR_IN
                 CAIRO_OPERATOR_OUT
                 CAIRO_OPERATOR_ATOP
                 CAIRO_OPERATOR_DEST
                 CAIRO_OPERATOR_DEST_OVER
                 CAIRO_OPERATOR_DEST_IN
                 CAIRO_OPERATOR_DEST_OUT
                 CAIRO_OPERATOR_DEST_ATOP
                 CAIRO_OPERATOR_XOR
                 CAIRO_OPERATOR_ADD
                 CAIRO_OPERATOR_SATURATE)

  ;; cairo_path_data_type_t
  (define-c-enum CAIRO_PATH_MOVE_TO
                 CAIRO_PATH_LINE_TO
                 CAIRO_PATH_CURVE_TO
                 CAIRO_PATH_CLOSE_PATH)

  ;; cairo_pattern_type_t
  (define-c-enum CAIRO_PATTERN_TYPE_SOLID
                 CAIRO_PATTERN_TYPE_SURFACE
                 CAIRO_PATTERN_TYPE_LINEAR
                 CAIRO_PATTERN_TYPE_RADIAL)

  ;; cairo_status_t
  (define-c-enum CAIRO_STATUS_SUCCESS
                 CAIRO_STATUS_NO_MEMORY
                 CAIRO_STATUS_INVALID_RESTORE
                 CAIRO_STATUS_INVALID_POP_GROUP
                 CAIRO_STATUS_NO_CURRENT_POINT
                 CAIRO_STATUS_INVALID_MATRIX
                 CAIRO_STATUS_INVALID_STATUS
                 CAIRO_STATUS_NULL_POINTER
                 CAIRO_STATUS_INVALID_STRING
                 CAIRO_STATUS_INVALID_PATH_DATA
                 CAIRO_STATUS_READ_ERROR
                 CAIRO_STATUS_WRITE_ERROR
                 CAIRO_STATUS_SURFACE_FINISHED
                 CAIRO_STATUS_SURFACE_TYPE_MISMATCH
                 CAIRO_STATUS_PATTERN_TYPE_MISMATCH
                 CAIRO_STATUS_INVALID_CONTENT
                 CAIRO_STATUS_INVALID_FORMAT
                 CAIRO_STATUS_INVALID_VISUAL
                 CAIRO_STATUS_FILE_NOT_FOUND
                 CAIRO_STATUS_INVALID_DASH
                 CAIRO_STATUS_INVALID_DSC_COMMENT
                 CAIRO_STATUS_INVALID_INDEX
                 CAIRO_STATUS_CLIP_NOT_REPRESENTABLE
                 CAIRO_STATUS_TEMP_FILE_ERROR
                 CAIRO_STATUS_INVALID_STRIDE
                 CAIRO_STATUS_FONT_TYPE_MISMATCH
                 CAIRO_STATUS_USER_FONT_IMMUTABLE
                 CAIRO_STATUS_USER_FONT_ERROR
                 CAIRO_STATUS_NEGATIVE_COUNT
                 CAIRO_STATUS_INVALID_CLUSTERS
                 CAIRO_STATUS_INVALID_SLANT
                 CAIRO_STATUS_INVALID_WEIGHT)

  ;; cairo_subpixel_order_t
  (define-c-enum CAIRO_SUBPIXEL_ORDER_DEFAULT
                 CAIRO_SUBPIXEL_ORDER_RGB
                 CAIRO_SUBPIXEL_ORDER_BGR
                 CAIRO_SUBPIXEL_ORDER_VRGB
                 CAIRO_SUBPIXEL_ORDER_VBGR)

  ;; cairo_surface_type_t
  (define-c-enum CAIRO_SURFACE_TYPE_IMAGE
                 CAIRO_SURFACE_TYPE_PDF
                 CAIRO_SURFACE_TYPE_PS
                 CAIRO_SURFACE_TYPE_XLIB
                 CAIRO_SURFACE_TYPE_XCB
                 CAIRO_SURFACE_TYPE_GLITZ
                 CAIRO_SURFACE_TYPE_QUARTZ
                 CAIRO_SURFACE_TYPE_WIN32
                 CAIRO_SURFACE_TYPE_BEOS
                 CAIRO_SURFACE_TYPE_DIRECTFB
                 CAIRO_SURFACE_TYPE_SVG
                 CAIRO_SURFACE_TYPE_OS2
                 CAIRO_SURFACE_TYPE_WIN32_PRINTING
                 CAIRO_SURFACE_TYPE_QUARTZ_IMAGE)

  ;; cairo_text_cluster_flags_t
  (define-c-enum (CAIRO_TEXT_CLUSTER_FLAG_BACKWARD . #x00000001))

  ;; cairo_ps_level_t
  (define-c-enum CAIRO_PS_LEVEL_2 CAIRO_PS_LEVEL_3)

  ;; cairo_svg_version_t
  (define-c-enum  CAIRO_SVG_VERSION_1_1 CAIRO_SVG_VERSION_1_2)

  ) ;[end]
