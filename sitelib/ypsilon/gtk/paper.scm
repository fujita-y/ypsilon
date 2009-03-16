#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk paper)

  (export gtk_paper_size_copy
          gtk_paper_size_free
          gtk_paper_size_get_default
          gtk_paper_size_get_default_bottom_margin
          gtk_paper_size_get_default_left_margin
          gtk_paper_size_get_default_right_margin
          gtk_paper_size_get_default_top_margin
          gtk_paper_size_get_display_name
          gtk_paper_size_get_height
          gtk_paper_size_get_name
          gtk_paper_size_get_paper_sizes
          gtk_paper_size_get_ppd_name
          gtk_paper_size_get_type
          gtk_paper_size_get_width
          gtk_paper_size_is_custom
          gtk_paper_size_is_equal
          gtk_paper_size_new
          gtk_paper_size_new_custom
          gtk_paper_size_new_from_key_file
          gtk_paper_size_new_from_ppd
          gtk_paper_size_set_size
          gtk_paper_size_to_key_file)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
          (on-windows "libgtk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GTK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; GtkPaperSize* gtk_paper_size_copy (GtkPaperSize* other)
  (define-function void* gtk_paper_size_copy (void*))

  ;; void gtk_paper_size_free (GtkPaperSize* size)
  (define-function void gtk_paper_size_free (void*))

  ;; const gchar* gtk_paper_size_get_default (void)
  (define-function char* gtk_paper_size_get_default ())

  ;; gdouble gtk_paper_size_get_default_bottom_margin (GtkPaperSize* size, GtkUnit unit)
  (define-function double gtk_paper_size_get_default_bottom_margin (void* int))

  ;; gdouble gtk_paper_size_get_default_left_margin (GtkPaperSize* size, GtkUnit unit)
  (define-function double gtk_paper_size_get_default_left_margin (void* int))

  ;; gdouble gtk_paper_size_get_default_right_margin (GtkPaperSize* size, GtkUnit unit)
  (define-function double gtk_paper_size_get_default_right_margin (void* int))

  ;; gdouble gtk_paper_size_get_default_top_margin (GtkPaperSize* size, GtkUnit unit)
  (define-function double gtk_paper_size_get_default_top_margin (void* int))

  ;; const gchar* gtk_paper_size_get_display_name (GtkPaperSize* size)
  (define-function char* gtk_paper_size_get_display_name (void*))

  ;; gdouble gtk_paper_size_get_height (GtkPaperSize* size, GtkUnit unit)
  (define-function double gtk_paper_size_get_height (void* int))

  ;; const gchar* gtk_paper_size_get_name (GtkPaperSize* size)
  (define-function char* gtk_paper_size_get_name (void*))

  ;; GList* gtk_paper_size_get_paper_sizes (gboolean include_custom)
  (define-function void* gtk_paper_size_get_paper_sizes (int))

  ;; const gchar* gtk_paper_size_get_ppd_name (GtkPaperSize* size)
  (define-function char* gtk_paper_size_get_ppd_name (void*))

  ;; GType gtk_paper_size_get_type (void)
  (define-function unsigned-long gtk_paper_size_get_type ())

  ;; gdouble gtk_paper_size_get_width (GtkPaperSize* size, GtkUnit unit)
  (define-function double gtk_paper_size_get_width (void* int))

  ;; gboolean gtk_paper_size_is_custom (GtkPaperSize* size)
  (define-function int gtk_paper_size_is_custom (void*))

  ;; gboolean gtk_paper_size_is_equal (GtkPaperSize* size1, GtkPaperSize* size2)
  (define-function int gtk_paper_size_is_equal (void* void*))

  ;; GtkPaperSize* gtk_paper_size_new (const gchar* name)
  (define-function void* gtk_paper_size_new (char*))

  ;; GtkPaperSize* gtk_paper_size_new_custom (const gchar* name, const gchar* display_name, gdouble width, gdouble height, GtkUnit unit)
  (define-function void* gtk_paper_size_new_custom (char* char* double double int))

  ;; GtkPaperSize* gtk_paper_size_new_from_key_file (GKeyFile* key_file, const gchar* group_name, GError** error)
  (define-function void* gtk_paper_size_new_from_key_file (void* char* void*))

  ;; GtkPaperSize* gtk_paper_size_new_from_ppd (const gchar* ppd_name, const gchar* ppd_display_name, gdouble width, gdouble height)
  (define-function void* gtk_paper_size_new_from_ppd (char* char* double double))

  ;; void gtk_paper_size_set_size (GtkPaperSize* size, gdouble width, gdouble height, GtkUnit unit)
  (define-function void gtk_paper_size_set_size (void* double double int))

  ;; void gtk_paper_size_to_key_file (GtkPaperSize* size, GKeyFile* key_file, const gchar* group_name)
  (define-function void gtk_paper_size_to_key_file (void* void* char*))

  ) ;[end]
