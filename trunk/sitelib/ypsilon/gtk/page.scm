#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk page)

  (export gtk_page_orientation_get_type
          gtk_page_set_get_type
          gtk_page_setup_copy
          gtk_page_setup_get_bottom_margin
          gtk_page_setup_get_left_margin
          gtk_page_setup_get_orientation
          gtk_page_setup_get_page_height
          gtk_page_setup_get_page_width
          gtk_page_setup_get_paper_height
          gtk_page_setup_get_paper_size
          gtk_page_setup_get_paper_width
          gtk_page_setup_get_right_margin
          gtk_page_setup_get_top_margin
          gtk_page_setup_get_type
          gtk_page_setup_load_file
          gtk_page_setup_load_key_file
          gtk_page_setup_new
          gtk_page_setup_new_from_file
          gtk_page_setup_new_from_key_file
          gtk_page_setup_set_bottom_margin
          gtk_page_setup_set_left_margin
          gtk_page_setup_set_orientation
          gtk_page_setup_set_paper_size
          gtk_page_setup_set_paper_size_and_default_margins
          gtk_page_setup_set_right_margin
          gtk_page_setup_set_top_margin
          gtk_page_setup_to_file
          gtk_page_setup_to_key_file)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgtk-x11-2.0.so.0")
          (on-sunos   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libgtk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GTK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-function/va_list
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "va_list argument not supported"))))))

  ;; GType gtk_page_orientation_get_type (void)
  (define-function unsigned-long gtk_page_orientation_get_type ())

  ;; GType gtk_page_set_get_type (void)
  (define-function unsigned-long gtk_page_set_get_type ())

  ;; GtkPageSetup* gtk_page_setup_copy (GtkPageSetup* other)
  (define-function void* gtk_page_setup_copy (void*))

  ;; gdouble gtk_page_setup_get_bottom_margin (GtkPageSetup* setup, GtkUnit unit)
  (define-function double gtk_page_setup_get_bottom_margin (void* int))

  ;; gdouble gtk_page_setup_get_left_margin (GtkPageSetup* setup, GtkUnit unit)
  (define-function double gtk_page_setup_get_left_margin (void* int))

  ;; GtkPageOrientation gtk_page_setup_get_orientation (GtkPageSetup* setup)
  (define-function int gtk_page_setup_get_orientation (void*))

  ;; gdouble gtk_page_setup_get_page_height (GtkPageSetup* setup, GtkUnit unit)
  (define-function double gtk_page_setup_get_page_height (void* int))

  ;; gdouble gtk_page_setup_get_page_width (GtkPageSetup* setup, GtkUnit unit)
  (define-function double gtk_page_setup_get_page_width (void* int))

  ;; gdouble gtk_page_setup_get_paper_height (GtkPageSetup* setup, GtkUnit unit)
  (define-function double gtk_page_setup_get_paper_height (void* int))

  ;; GtkPaperSize* gtk_page_setup_get_paper_size (GtkPageSetup* setup)
  (define-function void* gtk_page_setup_get_paper_size (void*))

  ;; gdouble gtk_page_setup_get_paper_width (GtkPageSetup* setup, GtkUnit unit)
  (define-function double gtk_page_setup_get_paper_width (void* int))

  ;; gdouble gtk_page_setup_get_right_margin (GtkPageSetup* setup, GtkUnit unit)
  (define-function double gtk_page_setup_get_right_margin (void* int))

  ;; gdouble gtk_page_setup_get_top_margin (GtkPageSetup* setup, GtkUnit unit)
  (define-function double gtk_page_setup_get_top_margin (void* int))

  ;; GType gtk_page_setup_get_type (void)
  (define-function unsigned-long gtk_page_setup_get_type ())

  ;; gboolean gtk_page_setup_load_file (GtkPageSetup* setup, const char* file_name, GError** error)
  (define-function int gtk_page_setup_load_file (void* char* void*))

  ;; gboolean gtk_page_setup_load_key_file (GtkPageSetup* setup, GKeyFile* key_file, const gchar* group_name, GError** error)
  (define-function int gtk_page_setup_load_key_file (void* void* char* void*))

  ;; GtkPageSetup* gtk_page_setup_new (void)
  (define-function void* gtk_page_setup_new ())

  ;; GtkPageSetup* gtk_page_setup_new_from_file (const gchar* file_name, GError** error)
  (define-function void* gtk_page_setup_new_from_file (char* void*))

  ;; GtkPageSetup* gtk_page_setup_new_from_key_file (GKeyFile* key_file, const gchar* group_name, GError** error)
  (define-function void* gtk_page_setup_new_from_key_file (void* char* void*))

  ;; void gtk_page_setup_set_bottom_margin (GtkPageSetup* setup, gdouble margin, GtkUnit unit)
  (define-function void gtk_page_setup_set_bottom_margin (void* double int))

  ;; void gtk_page_setup_set_left_margin (GtkPageSetup* setup, gdouble margin, GtkUnit unit)
  (define-function void gtk_page_setup_set_left_margin (void* double int))

  ;; void gtk_page_setup_set_orientation (GtkPageSetup* setup, GtkPageOrientation orientation)
  (define-function void gtk_page_setup_set_orientation (void* int))

  ;; void gtk_page_setup_set_paper_size (GtkPageSetup* setup, GtkPaperSize* size)
  (define-function void gtk_page_setup_set_paper_size (void* void*))

  ;; void gtk_page_setup_set_paper_size_and_default_margins (GtkPageSetup* setup, GtkPaperSize* size)
  (define-function void gtk_page_setup_set_paper_size_and_default_margins (void* void*))

  ;; void gtk_page_setup_set_right_margin (GtkPageSetup* setup, gdouble margin, GtkUnit unit)
  (define-function void gtk_page_setup_set_right_margin (void* double int))

  ;; void gtk_page_setup_set_top_margin (GtkPageSetup* setup, gdouble margin, GtkUnit unit)
  (define-function void gtk_page_setup_set_top_margin (void* double int))

  ;; gboolean gtk_page_setup_to_file (GtkPageSetup* setup, const char* file_name, GError** error)
  (define-function int gtk_page_setup_to_file (void* char* void*))

  ;; void gtk_page_setup_to_key_file (GtkPageSetup* setup, GKeyFile* key_file, const gchar* group_name)
  (define-function void gtk_page_setup_to_key_file (void* void* char*))

  ) ;[end]
