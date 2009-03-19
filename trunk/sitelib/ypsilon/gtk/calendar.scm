#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk calendar)

  (export gtk_calendar_clear_marks
          gtk_calendar_display_options_get_type
          gtk_calendar_get_date
          gtk_calendar_get_detail_height_rows
          gtk_calendar_get_detail_width_chars
          gtk_calendar_get_display_options
          gtk_calendar_get_type
          gtk_calendar_mark_day
          gtk_calendar_new
          gtk_calendar_select_day
          gtk_calendar_select_month
          gtk_calendar_set_detail_func
          gtk_calendar_set_detail_height_rows
          gtk_calendar_set_detail_width_chars
          gtk_calendar_set_display_options
          gtk_calendar_unmark_day)

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

  (define-syntax define-function/va_list
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "va_list argument not supported"))))))

  ;; void gtk_calendar_clear_marks (GtkCalendar* calendar)
  (define-function void gtk_calendar_clear_marks (void*))

  ;; GType gtk_calendar_display_options_get_type (void)
  (define-function unsigned-long gtk_calendar_display_options_get_type ())

  ;; void gtk_calendar_get_date (GtkCalendar* calendar, guint* year, guint* month, guint* day)
  (define-function void gtk_calendar_get_date (void* void* void* void*))

  ;; gint gtk_calendar_get_detail_height_rows (GtkCalendar* calendar)
  (define-function int gtk_calendar_get_detail_height_rows (void*))

  ;; gint gtk_calendar_get_detail_width_chars (GtkCalendar* calendar)
  (define-function int gtk_calendar_get_detail_width_chars (void*))

  ;; GtkCalendarDisplayOptions gtk_calendar_get_display_options (GtkCalendar* calendar)
  (define-function int gtk_calendar_get_display_options (void*))

  ;; GType gtk_calendar_get_type (void)
  (define-function unsigned-long gtk_calendar_get_type ())

  ;; gboolean gtk_calendar_mark_day (GtkCalendar* calendar, guint day)
  (define-function int gtk_calendar_mark_day (void* unsigned-int))

  ;; GtkWidget* gtk_calendar_new (void)
  (define-function void* gtk_calendar_new ())

  ;; void gtk_calendar_select_day (GtkCalendar* calendar, guint day)
  (define-function void gtk_calendar_select_day (void* unsigned-int))

  ;; gboolean gtk_calendar_select_month (GtkCalendar* calendar, guint month, guint year)
  (define-function int gtk_calendar_select_month (void* unsigned-int unsigned-int))

  ;; void gtk_calendar_set_detail_func (GtkCalendar* calendar, GtkCalendarDetailFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_calendar_set_detail_func (void* (c-callback void* (void* unsigned-int unsigned-int unsigned-int void*)) void* (c-callback void (void*))))

  ;; void gtk_calendar_set_detail_height_rows (GtkCalendar* calendar, gint rows)
  (define-function void gtk_calendar_set_detail_height_rows (void* int))

  ;; void gtk_calendar_set_detail_width_chars (GtkCalendar* calendar, gint chars)
  (define-function void gtk_calendar_set_detail_width_chars (void* int))

  ;; void gtk_calendar_set_display_options (GtkCalendar* calendar, GtkCalendarDisplayOptions flags)
  (define-function void gtk_calendar_set_display_options (void* int))

  ;; gboolean gtk_calendar_unmark_day (GtkCalendar* calendar, guint day)
  (define-function int gtk_calendar_unmark_day (void* unsigned-int))

  ) ;[end]
