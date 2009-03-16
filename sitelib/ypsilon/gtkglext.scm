#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtkglext)

  (export gdk_gl_buffer_mask_get_type
          gdk_gl_config_attrib_get_type
          gdk_gl_config_caveat_get_type
          gdk_gl_config_error_get_type
          gdk_gl_config_get_attrib
          gdk_gl_config_get_colormap
          gdk_gl_config_get_depth
          gdk_gl_config_get_layer_plane
          gdk_gl_config_get_n_aux_buffers
          gdk_gl_config_get_n_sample_buffers
          gdk_gl_config_get_screen
          gdk_gl_config_get_type
          gdk_gl_config_get_visual
          gdk_gl_config_has_accum_buffer
          gdk_gl_config_has_alpha
          gdk_gl_config_has_depth_buffer
          gdk_gl_config_has_stencil_buffer
          gdk_gl_config_is_double_buffered
          gdk_gl_config_is_rgba
          gdk_gl_config_is_stereo
          gdk_gl_config_mode_get_type
          gdk_gl_config_new
          gdk_gl_config_new_by_mode
          gdk_gl_config_new_by_mode_for_screen
          gdk_gl_config_new_for_screen
          gdk_gl_context_copy
          gdk_gl_context_destroy
          gdk_gl_context_get_current
          gdk_gl_context_get_gl_config
          gdk_gl_context_get_gl_drawable
          gdk_gl_context_get_render_type
          gdk_gl_context_get_share_list
          gdk_gl_context_get_type
          gdk_gl_context_is_direct
          gdk_gl_context_new
          gdk_gl_draw_cone
          gdk_gl_draw_cube
          gdk_gl_draw_dodecahedron
          gdk_gl_draw_icosahedron
          gdk_gl_draw_octahedron
          gdk_gl_draw_sphere
          gdk_gl_draw_teapot
          gdk_gl_draw_tetrahedron
          gdk_gl_draw_torus
          gdk_gl_drawable_attrib_get_type
          gdk_gl_drawable_get_current
          gdk_gl_drawable_get_gl_config
          gdk_gl_drawable_get_size
          gdk_gl_drawable_get_type
          gdk_gl_drawable_gl_begin
          gdk_gl_drawable_gl_end
          gdk_gl_drawable_is_double_buffered
          gdk_gl_drawable_make_current
          gdk_gl_drawable_swap_buffers
          gdk_gl_drawable_type_get_type
          gdk_gl_drawable_type_mask_get_type
          gdk_gl_drawable_wait_gdk
          gdk_gl_drawable_wait_gl
          gdk_gl_event_mask_get_type
          gdk_gl_event_type_get_type
          gdk_gl_font_use_pango_font
          gdk_gl_font_use_pango_font_for_display
          gdk_gl_get_proc_address
          gdk_gl_init
          gdk_gl_init_check
          gdk_gl_parse_args
          gdk_gl_pbuffer_attrib_get_type
          gdk_gl_pixmap_destroy
          gdk_gl_pixmap_get_pixmap
          gdk_gl_pixmap_get_type
          gdk_gl_pixmap_new
          gdk_gl_query_extension
          gdk_gl_query_extension_for_display
          gdk_gl_query_gl_extension
          gdk_gl_query_version
          gdk_gl_query_version_for_display
          gdk_gl_render_type_get_type
          gdk_gl_render_type_mask_get_type
          gdk_gl_transparent_type_get_type
          gdk_gl_visual_type_get_type
          gdk_gl_window_destroy
          gdk_gl_window_get_type
          gdk_gl_window_get_window
          gdk_gl_window_new
          gdk_pixmap_get_gl_pixmap
          gdk_pixmap_is_gl_capable
          gdk_pixmap_set_gl_capability
          gdk_pixmap_unset_gl_capability
          gdk_window_get_gl_window
          gdk_window_is_gl_capable
          gdk_window_set_gl_capability
          gdk_window_unset_gl_capability
          gtk_gl_init
          gtk_gl_init_check
          gtk_gl_parse_args
          gtk_widget_create_gl_context
          gtk_widget_get_gl_config
          gtk_widget_get_gl_context
          gtk_widget_get_gl_window
          gtk_widget_is_gl_capable
          gtk_widget_set_gl_capability
          GDK_GL_ACCUM_ALPHA_SIZE
          GDK_GL_ACCUM_BLUE_SIZE
          GDK_GL_ACCUM_BUFFER_BIT
          GDK_GL_ACCUM_GREEN_SIZE
          GDK_GL_ACCUM_RED_SIZE
          GDK_GL_ALPHA_SIZE
          GDK_GL_AUX_BUFFERS
          GDK_GL_AUX_BUFFERS_BIT
          GDK_GL_BACK_LEFT_BUFFER_BIT
          GDK_GL_BACK_RIGHT_BUFFER_BIT
          GDK_GL_BAD_ATTRIBUTE
          GDK_GL_BAD_CONTEXT
          GDK_GL_BAD_ENUM
          GDK_GL_BAD_SCREEN
          GDK_GL_BAD_VALUE
          GDK_GL_BAD_VISUAL
          GDK_GL_BLUE_SIZE
          GDK_GL_BUFFER_SIZE
          GDK_GL_COLOR_INDEX_BIT
          GDK_GL_COLOR_INDEX_TYPE
          GDK_GL_CONFIG_CAVEAT
          GDK_GL_CONFIG_CAVEAT_DONT_CARE
          GDK_GL_CONFIG_CAVEAT_NONE
          GDK_GL_DAMAGED
          GDK_GL_DEPTH_BUFFER_BIT
          GDK_GL_DEPTH_SIZE
          GDK_GL_DIRECT_COLOR
          GDK_GL_DOUBLEBUFFER
          GDK_GL_DRAWABLE_TYPE
          GDK_GL_EVENT_MASK
          GDK_GL_FBCONFIG_ID
          GDK_GL_FRONT_LEFT_BUFFER_BIT
          GDK_GL_FRONT_RIGHT_BUFFER_BIT
          GDK_GL_GRAY_SCALE
          GDK_GL_GREEN_SIZE
          GDK_GL_HEIGHT
          GDK_GL_LARGEST_PBUFFER
          GDK_GL_LEVEL
          GDK_GL_MAX_PBUFFER_HEIGHT
          GDK_GL_MAX_PBUFFER_PIXELS
          GDK_GL_MAX_PBUFFER_WIDTH
          GDK_GL_MODE_ACCUM
          GDK_GL_MODE_ALPHA
          GDK_GL_MODE_DEPTH
          GDK_GL_MODE_DOUBLE
          GDK_GL_MODE_INDEX
          GDK_GL_MODE_MULTISAMPLE
          GDK_GL_MODE_RGB
          GDK_GL_MODE_RGBA
          GDK_GL_MODE_SINGLE
          GDK_GL_MODE_STENCIL
          GDK_GL_MODE_STEREO
          GDK_GL_NON_CONFORMANT_CONFIG
          GDK_GL_NO_EXTENSION
          GDK_GL_PBUFFER
          GDK_GL_PBUFFER_BIT
          GDK_GL_PBUFFER_CLOBBER_MASK
          GDK_GL_PBUFFER_HEIGHT
          GDK_GL_PBUFFER_LARGEST_PBUFFER
          GDK_GL_PBUFFER_PRESERVED_CONTENTS
          GDK_GL_PBUFFER_WIDTH
          GDK_GL_PIXMAP_BIT
          GDK_GL_PRESERVED_CONTENTS
          GDK_GL_PSEUDO_COLOR
          GDK_GL_RED_SIZE
          GDK_GL_RENDER_TYPE
          GDK_GL_RGBA
          GDK_GL_RGBA_BIT
          GDK_GL_RGBA_TYPE
          GDK_GL_SAMPLES
          GDK_GL_SAMPLE_BUFFERS
          GDK_GL_SAVED
          GDK_GL_SCREEN
          GDK_GL_SLOW_CONFIG
          GDK_GL_STATIC_COLOR
          GDK_GL_STATIC_GRAY
          GDK_GL_STENCIL_BUFFER_BIT
          GDK_GL_STENCIL_SIZE
          GDK_GL_STEREO
          GDK_GL_TRANSPARENT_ALPHA_VALUE
          GDK_GL_TRANSPARENT_BLUE_VALUE
          GDK_GL_TRANSPARENT_GREEN_VALUE
          GDK_GL_TRANSPARENT_INDEX
          GDK_GL_TRANSPARENT_INDEX_VALUE
          GDK_GL_TRANSPARENT_NONE
          GDK_GL_TRANSPARENT_RED_VALUE
          GDK_GL_TRANSPARENT_RGB
          GDK_GL_TRANSPARENT_TYPE
          GDK_GL_TRUE_COLOR
          GDK_GL_USE_GL
          GDK_GL_VISUAL_ID
          GDK_GL_VISUAL_TYPE_DONT_CARE
          GDK_GL_WIDTH
          GDK_GL_WINDOW
          GDK_GL_WINDOW_BIT
          GDK_GL_X_RENDERABLE
          GDK_GL_X_VISUAL_TYPE)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "libgtkglext-x11-1.0.0.dylib")
          (on-linux   "libgtkglext-x11-1.0.so.0")
          (on-freebsd "libgtkglext-x11-1.0.so")
          (on-openbsd "libgtkglext-x11-1.0.so")
          (on-windows "libgtkglext-win32-1.0-0.dll")
          (else
           (assertion-violation #f "can not locate GtkGLExt library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; GType gdk_gl_buffer_mask_get_type (void)
  (define-function unsigned-long gdk_gl_buffer_mask_get_type ())

  ;; GType gdk_gl_config_attrib_get_type (void)
  (define-function unsigned-long gdk_gl_config_attrib_get_type ())

  ;; GType gdk_gl_config_caveat_get_type (void)
  (define-function unsigned-long gdk_gl_config_caveat_get_type ())

  ;; GType gdk_gl_config_error_get_type (void)
  (define-function unsigned-long gdk_gl_config_error_get_type ())

  ;; gboolean gdk_gl_config_get_attrib (GdkGLConfig* glconfig, int attribute, int* value)
  (define-function int gdk_gl_config_get_attrib (void* int void*))

  ;; GdkColormap* gdk_gl_config_get_colormap (GdkGLConfig* glconfig)
  (define-function void* gdk_gl_config_get_colormap (void*))

  ;; gint gdk_gl_config_get_depth (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_get_depth (void*))

  ;; gint gdk_gl_config_get_layer_plane (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_get_layer_plane (void*))

  ;; gint gdk_gl_config_get_n_aux_buffers (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_get_n_aux_buffers (void*))

  ;; gint gdk_gl_config_get_n_sample_buffers (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_get_n_sample_buffers (void*))

  ;; GdkScreen* gdk_gl_config_get_screen (GdkGLConfig* glconfig)
  (define-function void* gdk_gl_config_get_screen (void*))

  ;; GType gdk_gl_config_get_type (void)
  (define-function unsigned-long gdk_gl_config_get_type ())

  ;; GdkVisual* gdk_gl_config_get_visual (GdkGLConfig* glconfig)
  (define-function void* gdk_gl_config_get_visual (void*))

  ;; gboolean gdk_gl_config_has_accum_buffer (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_has_accum_buffer (void*))

  ;; gboolean gdk_gl_config_has_alpha (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_has_alpha (void*))

  ;; gboolean gdk_gl_config_has_depth_buffer (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_has_depth_buffer (void*))

  ;; gboolean gdk_gl_config_has_stencil_buffer (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_has_stencil_buffer (void*))

  ;; gboolean gdk_gl_config_is_double_buffered (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_is_double_buffered (void*))

  ;; gboolean gdk_gl_config_is_rgba (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_is_rgba (void*))

  ;; gboolean gdk_gl_config_is_stereo (GdkGLConfig* glconfig)
  (define-function int gdk_gl_config_is_stereo (void*))

  ;; GType gdk_gl_config_mode_get_type (void)
  (define-function unsigned-long gdk_gl_config_mode_get_type ())

  ;; GdkGLConfig* gdk_gl_config_new (const int* attrib_list)
  (define-function void* gdk_gl_config_new (void*))

  ;; GdkGLConfig* gdk_gl_config_new_by_mode (GdkGLConfigMode mode)
  (define-function void* gdk_gl_config_new_by_mode (int))

  ;; GdkGLConfig* gdk_gl_config_new_by_mode_for_screen (GdkScreen* screen, GdkGLConfigMode mode)
  (define-function void* gdk_gl_config_new_by_mode_for_screen (void* int))

  ;; GdkGLConfig* gdk_gl_config_new_for_screen (GdkScreen* screen, const int* attrib_list)
  (define-function void* gdk_gl_config_new_for_screen (void* void*))

  ;; gboolean gdk_gl_context_copy (GdkGLContext* glcontext, GdkGLContext* src, unsigned long mask)
  (define-function int gdk_gl_context_copy (void* void* unsigned-long))

  ;; void gdk_gl_context_destroy (GdkGLContext* glcontext)
  (define-function void gdk_gl_context_destroy (void*))

  ;; GdkGLContext* gdk_gl_context_get_current (void)
  (define-function void* gdk_gl_context_get_current ())

  ;; GdkGLConfig* gdk_gl_context_get_gl_config (GdkGLContext* glcontext)
  (define-function void* gdk_gl_context_get_gl_config (void*))

  ;; GdkGLDrawable* gdk_gl_context_get_gl_drawable (GdkGLContext* glcontext)
  (define-function void* gdk_gl_context_get_gl_drawable (void*))

  ;; int gdk_gl_context_get_render_type (GdkGLContext* glcontext)
  (define-function int gdk_gl_context_get_render_type (void*))

  ;; GdkGLContext* gdk_gl_context_get_share_list (GdkGLContext* glcontext)
  (define-function void* gdk_gl_context_get_share_list (void*))

  ;; GType gdk_gl_context_get_type (void)
  (define-function unsigned-long gdk_gl_context_get_type ())

  ;; gboolean gdk_gl_context_is_direct (GdkGLContext* glcontext)
  (define-function int gdk_gl_context_is_direct (void*))

  ;; GdkGLContext* gdk_gl_context_new (GdkGLDrawable* gldrawable, GdkGLContext* share_list, gboolean direct, int render_type)
  (define-function void* gdk_gl_context_new (void* void* int int))

  ;; void gdk_gl_draw_cone (gboolean solid, double base, double height, int slices, int stacks)
  (define-function void gdk_gl_draw_cone (int double double int int))

  ;; void gdk_gl_draw_cube (gboolean solid, double size)
  (define-function void gdk_gl_draw_cube (int double))

  ;; void gdk_gl_draw_dodecahedron (gboolean solid)
  (define-function void gdk_gl_draw_dodecahedron (int))

  ;; void gdk_gl_draw_icosahedron (gboolean solid)
  (define-function void gdk_gl_draw_icosahedron (int))

  ;; void gdk_gl_draw_octahedron (gboolean solid)
  (define-function void gdk_gl_draw_octahedron (int))

  ;; void gdk_gl_draw_sphere (gboolean solid, double radius, int slices, int stacks)
  (define-function void gdk_gl_draw_sphere (int double int int))

  ;; void gdk_gl_draw_teapot (gboolean solid, double scale)
  (define-function void gdk_gl_draw_teapot (int double))

  ;; void gdk_gl_draw_tetrahedron (gboolean solid)
  (define-function void gdk_gl_draw_tetrahedron (int))

  ;; void gdk_gl_draw_torus (gboolean solid, double inner_radius, double outer_radius, int nsides, int rings)
  (define-function void gdk_gl_draw_torus (int double double int int))

  ;; GType gdk_gl_drawable_attrib_get_type (void)
  (define-function unsigned-long gdk_gl_drawable_attrib_get_type ())

  ;; GdkGLDrawable* gdk_gl_drawable_get_current (void)
  (define-function void* gdk_gl_drawable_get_current ())

  ;; GdkGLConfig* gdk_gl_drawable_get_gl_config (GdkGLDrawable* gldrawable)
  (define-function void* gdk_gl_drawable_get_gl_config (void*))

  ;; void gdk_gl_drawable_get_size (GdkGLDrawable* gldrawable, gint* width, gint* height)
  (define-function void gdk_gl_drawable_get_size (void* void* void*))

  ;; GType gdk_gl_drawable_get_type (void)
  (define-function unsigned-long gdk_gl_drawable_get_type ())

  ;; gboolean gdk_gl_drawable_gl_begin (GdkGLDrawable* gldrawable, GdkGLContext* glcontext)
  (define-function int gdk_gl_drawable_gl_begin (void* void*))

  ;; void gdk_gl_drawable_gl_end (GdkGLDrawable* gldrawable)
  (define-function void gdk_gl_drawable_gl_end (void*))

  ;; gboolean gdk_gl_drawable_is_double_buffered (GdkGLDrawable* gldrawable)
  (define-function int gdk_gl_drawable_is_double_buffered (void*))

  ;; gboolean gdk_gl_drawable_make_current (GdkGLDrawable* gldrawable, GdkGLContext* glcontext)
  (define-function int gdk_gl_drawable_make_current (void* void*))

  ;; void gdk_gl_drawable_swap_buffers (GdkGLDrawable* gldrawable)
  (define-function void gdk_gl_drawable_swap_buffers (void*))

  ;; GType gdk_gl_drawable_type_get_type (void)
  (define-function unsigned-long gdk_gl_drawable_type_get_type ())

  ;; GType gdk_gl_drawable_type_mask_get_type (void)
  (define-function unsigned-long gdk_gl_drawable_type_mask_get_type ())

  ;; void gdk_gl_drawable_wait_gdk (GdkGLDrawable* gldrawable)
  (define-function void gdk_gl_drawable_wait_gdk (void*))

  ;; void gdk_gl_drawable_wait_gl (GdkGLDrawable* gldrawable)
  (define-function void gdk_gl_drawable_wait_gl (void*))

  ;; GType gdk_gl_event_mask_get_type (void)
  (define-function unsigned-long gdk_gl_event_mask_get_type ())

  ;; GType gdk_gl_event_type_get_type (void)
  (define-function unsigned-long gdk_gl_event_type_get_type ())

  ;; PangoFont* gdk_gl_font_use_pango_font (const PangoFontDescription* font_desc, int first, int count, int list_base)
  (define-function void* gdk_gl_font_use_pango_font (void* int int int))

  ;; PangoFont* gdk_gl_font_use_pango_font_for_display (GdkDisplay* display, const PangoFontDescription* font_desc, int first, int count, int list_base)
  (define-function void* gdk_gl_font_use_pango_font_for_display (void* void* int int int))

  ;; GdkGLProc gdk_gl_get_proc_address (const char* proc_name)
  (define-function void* gdk_gl_get_proc_address (char*))

  ;; void gdk_gl_init (int* argc, char** *argv)
  (define-function void gdk_gl_init ((int) (* (char*))))

  ;; gboolean gdk_gl_init_check (int* argc, char** *argv)
  (define-function int gdk_gl_init_check ((int) (* (char*))))

  ;; gboolean gdk_gl_parse_args (int* argc, char** *argv)
  (define-function int gdk_gl_parse_args ((int) (* (char*))))

  ;; GType gdk_gl_pbuffer_attrib_get_type (void)
  (define-function unsigned-long gdk_gl_pbuffer_attrib_get_type ())

  ;; void gdk_gl_pixmap_destroy (GdkGLPixmap* glpixmap)
  (define-function void gdk_gl_pixmap_destroy (void*))

  ;; GdkPixmap* gdk_gl_pixmap_get_pixmap (GdkGLPixmap* glpixmap)
  (define-function void* gdk_gl_pixmap_get_pixmap (void*))

  ;; GType gdk_gl_pixmap_get_type (void)
  (define-function unsigned-long gdk_gl_pixmap_get_type ())

  ;; GdkGLPixmap* gdk_gl_pixmap_new (GdkGLConfig* glconfig, GdkPixmap* pixmap, const int* attrib_list)
  (define-function void* gdk_gl_pixmap_new (void* void* void*))

  ;; gboolean gdk_gl_query_extension (void)
  (define-function int gdk_gl_query_extension ())

  ;; gboolean gdk_gl_query_extension_for_display (GdkDisplay* display)
  (define-function int gdk_gl_query_extension_for_display (void*))

  ;; gboolean gdk_gl_query_gl_extension (const char* extension)
  (define-function int gdk_gl_query_gl_extension (char*))

  ;; gboolean gdk_gl_query_version (int* major, int* minor)
  (define-function int gdk_gl_query_version (void* void*))

  ;; gboolean gdk_gl_query_version_for_display (GdkDisplay* display, int* major, int* minor)
  (define-function int gdk_gl_query_version_for_display (void* void* void*))

  ;; GType gdk_gl_render_type_get_type (void)
  (define-function unsigned-long gdk_gl_render_type_get_type ())

  ;; GType gdk_gl_render_type_mask_get_type (void)
  (define-function unsigned-long gdk_gl_render_type_mask_get_type ())

  ;; GType gdk_gl_transparent_type_get_type (void)
  (define-function unsigned-long gdk_gl_transparent_type_get_type ())

  ;; GType gdk_gl_visual_type_get_type (void)
  (define-function unsigned-long gdk_gl_visual_type_get_type ())

  ;; void gdk_gl_window_destroy (GdkGLWindow* glwindow)
  (define-function void gdk_gl_window_destroy (void*))

  ;; GType gdk_gl_window_get_type (void)
  (define-function unsigned-long gdk_gl_window_get_type ())

  ;; GdkWindow* gdk_gl_window_get_window (GdkGLWindow* glwindow)
  (define-function void* gdk_gl_window_get_window (void*))

  ;; GdkGLWindow* gdk_gl_window_new (GdkGLConfig* glconfig, GdkWindow* window, const int* attrib_list)
  (define-function void* gdk_gl_window_new (void* void* void*))

  ;; GdkGLPixmap* gdk_pixmap_get_gl_pixmap (GdkPixmap* pixmap)
  (define-function void* gdk_pixmap_get_gl_pixmap (void*))

  ;; gboolean gdk_pixmap_is_gl_capable (GdkPixmap* pixmap)
  (define-function int gdk_pixmap_is_gl_capable (void*))

  ;; GdkGLPixmap* gdk_pixmap_set_gl_capability (GdkPixmap* pixmap, GdkGLConfig* glconfig, const int* attrib_list)
  (define-function void* gdk_pixmap_set_gl_capability (void* void* void*))

  ;; void gdk_pixmap_unset_gl_capability (GdkPixmap* pixmap)
  (define-function void gdk_pixmap_unset_gl_capability (void*))

  ;; GdkGLWindow* gdk_window_get_gl_window (GdkWindow* window)
  (define-function void* gdk_window_get_gl_window (void*))

  ;; gboolean gdk_window_is_gl_capable (GdkWindow* window)
  (define-function int gdk_window_is_gl_capable (void*))

  ;; GdkGLWindow* gdk_window_set_gl_capability (GdkWindow* window, GdkGLConfig* glconfig, const int* attrib_list)
  (define-function void* gdk_window_set_gl_capability (void* void* void*))

  ;; void gdk_window_unset_gl_capability (GdkWindow* window)
  (define-function void gdk_window_unset_gl_capability (void*))

  ;; void gtk_gl_init (int* argc, char** *argv)
  (define-function void gtk_gl_init ((int) (* (char*))))

  ;; gboolean gtk_gl_init_check (int* argc, char** *argv)
  (define-function int gtk_gl_init_check ((int) (* (char*))))

  ;; gboolean gtk_gl_parse_args (int* argc, char** *argv)
  (define-function int gtk_gl_parse_args ((int) (* (char*))))

  ;; GdkGLContext* gtk_widget_create_gl_context (GtkWidget* widget, GdkGLContext* share_list, gboolean direct, int render_type)
  (define-function void* gtk_widget_create_gl_context (void* void* int int))

  ;; GdkGLConfig* gtk_widget_get_gl_config (GtkWidget* widget)
  (define-function void* gtk_widget_get_gl_config (void*))

  ;; GdkGLContext* gtk_widget_get_gl_context (GtkWidget* widget)
  (define-function void* gtk_widget_get_gl_context (void*))

  ;; GdkGLWindow* gtk_widget_get_gl_window (GtkWidget* widget)
  (define-function void* gtk_widget_get_gl_window (void*))

  ;; gboolean gtk_widget_is_gl_capable (GtkWidget* widget)
  (define-function int gtk_widget_is_gl_capable (void*))

  ;; gboolean gtk_widget_set_gl_capability (GtkWidget* widget, GdkGLConfig* glconfig, GdkGLContext* share_list, gboolean direct, int render_type)
  (define-function int gtk_widget_set_gl_capability (void* void* void* int int))

  (define GDK_GL_USE_GL 1)
  (define GDK_GL_BUFFER_SIZE 2)
  (define GDK_GL_LEVEL 3)
  (define GDK_GL_RGBA 4)
  (define GDK_GL_DOUBLEBUFFER 5)
  (define GDK_GL_STEREO 6)
  (define GDK_GL_AUX_BUFFERS 7)
  (define GDK_GL_RED_SIZE 8)
  (define GDK_GL_GREEN_SIZE 9)
  (define GDK_GL_BLUE_SIZE 10)
  (define GDK_GL_ALPHA_SIZE 11)
  (define GDK_GL_DEPTH_SIZE 12)
  (define GDK_GL_STENCIL_SIZE 13)
  (define GDK_GL_ACCUM_RED_SIZE 14)
  (define GDK_GL_ACCUM_GREEN_SIZE 15)
  (define GDK_GL_ACCUM_BLUE_SIZE 16)
  (define GDK_GL_ACCUM_ALPHA_SIZE 17)
  (define GDK_GL_CONFIG_CAVEAT 32)
  (define GDK_GL_X_VISUAL_TYPE 34)
  (define GDK_GL_TRANSPARENT_TYPE 35)
  (define GDK_GL_TRANSPARENT_INDEX_VALUE 36)
  (define GDK_GL_TRANSPARENT_RED_VALUE 37)
  (define GDK_GL_TRANSPARENT_GREEN_VALUE 38)
  (define GDK_GL_TRANSPARENT_BLUE_VALUE 39)
  (define GDK_GL_TRANSPARENT_ALPHA_VALUE 40)
  (define GDK_GL_DRAWABLE_TYPE 32784)
  (define GDK_GL_RENDER_TYPE 32785)
  (define GDK_GL_X_RENDERABLE 32786)
  (define GDK_GL_FBCONFIG_ID 32787)
  (define GDK_GL_MAX_PBUFFER_WIDTH 32790)
  (define GDK_GL_MAX_PBUFFER_HEIGHT 32791)
  (define GDK_GL_MAX_PBUFFER_PIXELS 32792)
  (define GDK_GL_VISUAL_ID 32779)
  (define GDK_GL_SCREEN 32780)
  (define GDK_GL_SAMPLE_BUFFERS 100000)
  (define GDK_GL_SAMPLES 100001)
  (define GDK_GL_CONFIG_CAVEAT_DONT_CARE 4294967295)
  (define GDK_GL_CONFIG_CAVEAT_NONE 32768)
  (define GDK_GL_SLOW_CONFIG 32769)
  (define GDK_GL_NON_CONFORMANT_CONFIG 32781)
  (define GDK_GL_VISUAL_TYPE_DONT_CARE 4294967295)
  (define GDK_GL_TRUE_COLOR 32770)
  (define GDK_GL_DIRECT_COLOR 32771)
  (define GDK_GL_PSEUDO_COLOR 32772)
  (define GDK_GL_STATIC_COLOR 32773)
  (define GDK_GL_GRAY_SCALE 32774)
  (define GDK_GL_STATIC_GRAY 32775)
  (define GDK_GL_TRANSPARENT_NONE 32768)
  (define GDK_GL_TRANSPARENT_RGB 32776)
  (define GDK_GL_TRANSPARENT_INDEX 32777)
  (define GDK_GL_WINDOW_BIT 1)
  (define GDK_GL_PIXMAP_BIT 2)
  (define GDK_GL_PBUFFER_BIT 4)
  (define GDK_GL_RGBA_BIT 1)
  (define GDK_GL_COLOR_INDEX_BIT 2)
  (define GDK_GL_FRONT_LEFT_BUFFER_BIT 1)
  (define GDK_GL_FRONT_RIGHT_BUFFER_BIT 2)
  (define GDK_GL_BACK_LEFT_BUFFER_BIT 4)
  (define GDK_GL_BACK_RIGHT_BUFFER_BIT 8)
  (define GDK_GL_AUX_BUFFERS_BIT 16)
  (define GDK_GL_DEPTH_BUFFER_BIT 32)
  (define GDK_GL_STENCIL_BUFFER_BIT 64)
  (define GDK_GL_ACCUM_BUFFER_BIT 128)
  (define GDK_GL_BAD_SCREEN 1)
  (define GDK_GL_BAD_ATTRIBUTE 2)
  (define GDK_GL_NO_EXTENSION 3)
  (define GDK_GL_BAD_VISUAL 4)
  (define GDK_GL_BAD_CONTEXT 5)
  (define GDK_GL_BAD_VALUE 6)
  (define GDK_GL_BAD_ENUM 7)
  (define GDK_GL_RGBA_TYPE 32788)
  (define GDK_GL_COLOR_INDEX_TYPE 32789)
  (define GDK_GL_PRESERVED_CONTENTS 32795)
  (define GDK_GL_LARGEST_PBUFFER 32796)
  (define GDK_GL_WIDTH 32797)
  (define GDK_GL_HEIGHT 32798)
  (define GDK_GL_EVENT_MASK 32799)
  (define GDK_GL_PBUFFER_PRESERVED_CONTENTS 32795)
  (define GDK_GL_PBUFFER_LARGEST_PBUFFER 32796)
  (define GDK_GL_PBUFFER_HEIGHT 32832)
  (define GDK_GL_PBUFFER_WIDTH 32833)
  (define GDK_GL_PBUFFER_CLOBBER_MASK 134217728)
  (define GDK_GL_DAMAGED 32800)
  (define GDK_GL_SAVED 32801)
  (define GDK_GL_WINDOW 32802)
  (define GDK_GL_PBUFFER 32803)
  (define GDK_GL_MODE_RGB 0)
  (define GDK_GL_MODE_RGBA 0)
  (define GDK_GL_MODE_INDEX 1)
  (define GDK_GL_MODE_SINGLE 0)
  (define GDK_GL_MODE_DOUBLE 2)
  (define GDK_GL_MODE_STEREO 4)
  (define GDK_GL_MODE_ALPHA 8)
  (define GDK_GL_MODE_DEPTH 16)
  (define GDK_GL_MODE_STENCIL 32)
  (define GDK_GL_MODE_ACCUM 64)
  (define GDK_GL_MODE_MULTISAMPLE 128)

  ) ;[end]
