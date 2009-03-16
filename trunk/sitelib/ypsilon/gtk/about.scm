#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk about)

  (export gtk_about_dialog_get_artists
          gtk_about_dialog_get_authors
          gtk_about_dialog_get_comments
          gtk_about_dialog_get_copyright
          gtk_about_dialog_get_documenters
          gtk_about_dialog_get_license
          gtk_about_dialog_get_logo
          gtk_about_dialog_get_logo_icon_name
          gtk_about_dialog_get_program_name
          gtk_about_dialog_get_translator_credits
          gtk_about_dialog_get_type
          gtk_about_dialog_get_version
          gtk_about_dialog_get_website
          gtk_about_dialog_get_website_label
          gtk_about_dialog_get_wrap_license
          gtk_about_dialog_new
          gtk_about_dialog_set_artists
          gtk_about_dialog_set_authors
          gtk_about_dialog_set_comments
          gtk_about_dialog_set_copyright
          gtk_about_dialog_set_documenters
          gtk_about_dialog_set_email_hook
          gtk_about_dialog_set_license
          gtk_about_dialog_set_logo
          gtk_about_dialog_set_logo_icon_name
          gtk_about_dialog_set_program_name
          gtk_about_dialog_set_translator_credits
          gtk_about_dialog_set_url_hook
          gtk_about_dialog_set_version
          gtk_about_dialog_set_website
          gtk_about_dialog_set_website_label
          gtk_about_dialog_set_wrap_license)

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

  ;; const gchar* const* gtk_about_dialog_get_artists (GtkAboutDialog* about)
  (define-function void* gtk_about_dialog_get_artists (void*))

  ;; const gchar* const* gtk_about_dialog_get_authors (GtkAboutDialog* about)
  (define-function void* gtk_about_dialog_get_authors (void*))

  ;; const gchar* gtk_about_dialog_get_comments (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_comments (void*))

  ;; const gchar* gtk_about_dialog_get_copyright (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_copyright (void*))

  ;; const gchar* const* gtk_about_dialog_get_documenters (GtkAboutDialog* about)
  (define-function void* gtk_about_dialog_get_documenters (void*))

  ;; const gchar* gtk_about_dialog_get_license (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_license (void*))

  ;; GdkPixbuf* gtk_about_dialog_get_logo (GtkAboutDialog* about)
  (define-function void* gtk_about_dialog_get_logo (void*))

  ;; const gchar* gtk_about_dialog_get_logo_icon_name (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_logo_icon_name (void*))

  ;; const gchar* gtk_about_dialog_get_program_name (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_program_name (void*))

  ;; const gchar* gtk_about_dialog_get_translator_credits (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_translator_credits (void*))

  ;; GType gtk_about_dialog_get_type (void)
  (define-function unsigned-long gtk_about_dialog_get_type ())

  ;; const gchar* gtk_about_dialog_get_version (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_version (void*))

  ;; const gchar* gtk_about_dialog_get_website (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_website (void*))

  ;; const gchar* gtk_about_dialog_get_website_label (GtkAboutDialog* about)
  (define-function char* gtk_about_dialog_get_website_label (void*))

  ;; gboolean gtk_about_dialog_get_wrap_license (GtkAboutDialog* about)
  (define-function int gtk_about_dialog_get_wrap_license (void*))

  ;; GtkWidget* gtk_about_dialog_new (void)
  (define-function void* gtk_about_dialog_new ())

  ;; void gtk_about_dialog_set_artists (GtkAboutDialog* about, const gchar** artists)
  (define-function void gtk_about_dialog_set_artists (void* void*))

  ;; void gtk_about_dialog_set_authors (GtkAboutDialog* about, const gchar** authors)
  (define-function void gtk_about_dialog_set_authors (void* void*))

  ;; void gtk_about_dialog_set_comments (GtkAboutDialog* about, const gchar* comments)
  (define-function void gtk_about_dialog_set_comments (void* char*))

  ;; void gtk_about_dialog_set_copyright (GtkAboutDialog* about, const gchar* copyright)
  (define-function void gtk_about_dialog_set_copyright (void* char*))

  ;; void gtk_about_dialog_set_documenters (GtkAboutDialog* about, const gchar** documenters)
  (define-function void gtk_about_dialog_set_documenters (void* void*))

  ;; GtkAboutDialogActivateLinkFunc gtk_about_dialog_set_email_hook (GtkAboutDialogActivateLinkFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void* gtk_about_dialog_set_email_hook (void* void* (c-callback void (void*))))

  ;; void gtk_about_dialog_set_license (GtkAboutDialog* about, const gchar* license)
  (define-function void gtk_about_dialog_set_license (void* char*))

  ;; void gtk_about_dialog_set_logo (GtkAboutDialog* about, GdkPixbuf* logo)
  (define-function void gtk_about_dialog_set_logo (void* void*))

  ;; void gtk_about_dialog_set_logo_icon_name (GtkAboutDialog* about, const gchar* icon_name)
  (define-function void gtk_about_dialog_set_logo_icon_name (void* char*))

  ;; void gtk_about_dialog_set_program_name (GtkAboutDialog* about, const gchar* name)
  (define-function void gtk_about_dialog_set_program_name (void* char*))

  ;; void gtk_about_dialog_set_translator_credits (GtkAboutDialog* about, const gchar* translator_credits)
  (define-function void gtk_about_dialog_set_translator_credits (void* char*))

  ;; GtkAboutDialogActivateLinkFunc gtk_about_dialog_set_url_hook (GtkAboutDialogActivateLinkFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void* gtk_about_dialog_set_url_hook (void* void* (c-callback void (void*))))

  ;; void gtk_about_dialog_set_version (GtkAboutDialog* about, const gchar* version)
  (define-function void gtk_about_dialog_set_version (void* char*))

  ;; void gtk_about_dialog_set_website (GtkAboutDialog* about, const gchar* website)
  (define-function void gtk_about_dialog_set_website (void* char*))

  ;; void gtk_about_dialog_set_website_label (GtkAboutDialog* about, const gchar* website_label)
  (define-function void gtk_about_dialog_set_website_label (void* char*))

  ;; void gtk_about_dialog_set_wrap_license (GtkAboutDialog* about, gboolean wrap_license)
  (define-function void gtk_about_dialog_set_wrap_license (void* int))

  ) ;[end]
