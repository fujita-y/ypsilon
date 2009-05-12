#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon mysql)

  (export my_init
          mysql_affected_rows
          mysql_autocommit
          mysql_change_user
          mysql_character_set_name
          mysql_close
          mysql_commit
          mysql_data_seek
          mysql_debug
          mysql_dump_debug_info
          mysql_errno
          mysql_error
          mysql_fetch_field
          mysql_fetch_field_direct
          mysql_fetch_fields
          mysql_fetch_lengths
          mysql_fetch_row
          mysql_field_count
          mysql_field_seek
          mysql_field_tell
          mysql_free_result
          mysql_get_character_set_info
          mysql_get_client_info
          mysql_get_client_version
          mysql_get_host_info
          mysql_get_proto_info
          mysql_get_server_info
          mysql_get_server_version
          mysql_get_ssl_cipher
          mysql_hex_string
          mysql_info
          mysql_init
          mysql_insert_id
          mysql_kill
          mysql_list_dbs
          mysql_list_fields
          mysql_list_processes
          mysql_list_tables
          mysql_more_results
          mysql_next_result
          mysql_num_fields
          mysql_num_rows
          mysql_options
          mysql_ping
          mysql_query
          mysql_real_connect
          mysql_real_escape_string
          mysql_real_query
          mysql_refresh
          mysql_rollback
          mysql_row_seek
          mysql_row_tell
          mysql_select_db
          mysql_server_end
          mysql_server_init
          mysql_set_character_set
          mysql_set_local_infile_default
          mysql_set_local_infile_handler
          mysql_set_server_option
          mysql_shutdown
          mysql_sqlstate
          mysql_ssl_set
          mysql_stat
          mysql_stmt_affected_rows
          mysql_stmt_attr_get
          mysql_stmt_attr_set
          mysql_stmt_bind_param
          mysql_stmt_bind_result
          mysql_stmt_close
          mysql_stmt_data_seek
          mysql_stmt_errno
          mysql_stmt_error
          mysql_stmt_execute
          mysql_stmt_fetch
          mysql_stmt_fetch_column
          mysql_stmt_field_count
          mysql_stmt_free_result
          mysql_stmt_init
          mysql_stmt_insert_id
          mysql_stmt_num_rows
          mysql_stmt_param_count
          mysql_stmt_param_metadata
          mysql_stmt_prepare
          mysql_stmt_reset
          mysql_stmt_result_metadata
          mysql_stmt_row_seek
          mysql_stmt_row_tell
          mysql_stmt_send_long_data
          mysql_stmt_sqlstate
          mysql_stmt_store_result
          mysql_store_result
          mysql_thread_end
          mysql_thread_id
          mysql_thread_init
          mysql_thread_safe
          mysql_use_result
          mysql_warning_count
          (rename (mysql_server_init mysql_library_init))
          (rename (mysql_server_end mysql_library_end))
          c-coerce-void*
          define-c-struct-methods
          AUTO_INCREMENT_FLAG
          BINARY_FLAG
          CLIENT_COMPRESS
          CLIENT_FOUND_ROWS
          CLIENT_IGNORE_SPACE
          CLIENT_INTERACTIVE
          CLIENT_LOCAL_FILES
          CLIENT_MULTI_RESULTS
          CLIENT_MULTI_STATEMENTS
          CLIENT_NO_SCHEMA
          CLIENT_ODBC
          MULTIPLE_KEY_FLAG
          MYSQL_FIELD
          MYSQL_INIT_COMMAND
          MYSQL_OPT_COMPRESS
          MYSQL_OPT_CONNECT_TIMEOUT
          MYSQL_OPT_GUESS_CONNECTION
          MYSQL_OPT_LOCAL_INFILE
          MYSQL_OPT_NAMED_PIPE
          MYSQL_OPT_PROTOCOL
          MYSQL_OPT_READ_TIMEOUT
          MYSQL_OPT_RECONNECT
          MYSQL_OPT_SSL_VERIFY_SERVER_CERT
          MYSQL_OPT_USE_EMBEDDED_CONNECTION
          MYSQL_OPT_USE_REMOTE_CONNECTION
          MYSQL_OPT_USE_RESULT
          MYSQL_OPT_WRITE_TIMEOUT
          MYSQL_READ_DEFAULT_FILE
          MYSQL_READ_DEFAULT_GROUP
          MYSQL_REPORT_DATA_TRUNCATION
          MYSQL_SECURE_AUTH
          MYSQL_SET_CHARSET_DIR
          MYSQL_SET_CHARSET_NAME
          MYSQL_SET_CLIENT_IP
          MYSQL_SHARED_MEMORY_BASE_NAME
          MYSQL_TYPE_BIT
          MYSQL_TYPE_BLOB
          MYSQL_TYPE_DATE
          MYSQL_TYPE_DATETIME
          MYSQL_TYPE_DECIMAL
          MYSQL_TYPE_DOUBLE
          MYSQL_TYPE_ENUM
          MYSQL_TYPE_FLOAT
          MYSQL_TYPE_GEOMETRY
          MYSQL_TYPE_INT24
          MYSQL_TYPE_LONG
          MYSQL_TYPE_LONGLONG
          MYSQL_TYPE_LONG_BLOB
          MYSQL_TYPE_MEDIUM_BLOB
          MYSQL_TYPE_NEWDATE
          MYSQL_TYPE_NEWDECIMAL
          MYSQL_TYPE_NULL
          MYSQL_TYPE_SET
          MYSQL_TYPE_SHORT
          MYSQL_TYPE_STRING
          MYSQL_TYPE_TIME
          MYSQL_TYPE_TIMESTAMP
          MYSQL_TYPE_TINY
          MYSQL_TYPE_TINY_BLOB
          MYSQL_TYPE_VARCHAR
          MYSQL_TYPE_VAR_STRING
          MYSQL_TYPE_YEAR
          NOT_NULL_FLAG
          PRI_KEY_FLAG
          UNIQUE_KEY_FLAG
          UNSIGNED_FLAG
          ZEROFILL_FLAG)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libmysqlclient.so")
          (on-sunos   "libmysqlclient.so")
          (on-freebsd "libmysqlclient.so")
          (on-openbsd "libmysqlclient.so")
          (on-darwin  "/usr/local/mysql/libmysqlclient.dylib")
          (on-windows "libmySQL.dll")
          (else
           (assertion-violation #f "can not locate MySQL client library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret __stdcall name args)))))

  ;; based on MySQL 5.1 C API (deprecated functions are excluded)

  (define-c-typedef MYSQL_FIELD
    (struct (void* name)
            (void* org_name)
            (void* table)
            (void* org_table)
            (void* db)
            (void* catalog)
            (void* def)
            (unsigned-long length)
            (unsigned-long max_length)
            (unsigned-int name_length)
            (unsigned-int org_name_length)
            (unsigned-int table_length)
            (unsigned-int org_table_length)
            (unsigned-int db_length)
            (unsigned-int catalog_length)
            (unsigned-int def_length)
            (unsigned-int flags)
            (unsigned-int decimals)
            (unsigned-int charsetnr)
            (int type)))

  (define-c-enum MYSQL_TYPE_DECIMAL
                 MYSQL_TYPE_TINY
                 MYSQL_TYPE_SHORT
                 MYSQL_TYPE_LONG
                 MYSQL_TYPE_FLOAT
                 MYSQL_TYPE_DOUBLE
                 MYSQL_TYPE_NULL
                 MYSQL_TYPE_TIMESTAMP
                 MYSQL_TYPE_LONGLONG
                 MYSQL_TYPE_INT24
                 MYSQL_TYPE_DATE
                 MYSQL_TYPE_TIME
                 MYSQL_TYPE_DATETIME
                 MYSQL_TYPE_YEAR
                 MYSQL_TYPE_NEWDATE
                 MYSQL_TYPE_VARCHAR
                 MYSQL_TYPE_BIT
                 (MYSQL_TYPE_NEWDECIMAL . 246)
                 MYSQL_TYPE_ENUM
                 MYSQL_TYPE_SET
                 MYSQL_TYPE_TINY_BLOB
                 MYSQL_TYPE_MEDIUM_BLOB
                 MYSQL_TYPE_LONG_BLOB
                 MYSQL_TYPE_BLOB
                 MYSQL_TYPE_VAR_STRING
                 MYSQL_TYPE_STRING
                 MYSQL_TYPE_GEOMETRY)

  (define-c-enum MYSQL_OPT_CONNECT_TIMEOUT
                 MYSQL_OPT_COMPRESS
                 MYSQL_OPT_NAMED_PIPE
                 MYSQL_INIT_COMMAND
                 MYSQL_READ_DEFAULT_FILE
                 MYSQL_READ_DEFAULT_GROUP
                 MYSQL_SET_CHARSET_DIR
                 MYSQL_SET_CHARSET_NAME
                 MYSQL_OPT_LOCAL_INFILE
                 MYSQL_OPT_PROTOCOL
                 MYSQL_SHARED_MEMORY_BASE_NAME
                 MYSQL_OPT_READ_TIMEOUT
                 MYSQL_OPT_WRITE_TIMEOUT
                 MYSQL_OPT_USE_RESULT
                 MYSQL_OPT_USE_REMOTE_CONNECTION
                 MYSQL_OPT_USE_EMBEDDED_CONNECTION
                 MYSQL_OPT_GUESS_CONNECTION
                 MYSQL_SET_CLIENT_IP
                 MYSQL_SECURE_AUTH
                 MYSQL_REPORT_DATA_TRUNCATION
                 MYSQL_OPT_RECONNECT
                 MYSQL_OPT_SSL_VERIFY_SERVER_CERT)

  (define NOT_NULL_FLAG 1)
  (define PRI_KEY_FLAG 2)
  (define UNIQUE_KEY_FLAG 4)
  (define MULTIPLE_KEY_FLAG 8)
  (define UNSIGNED_FLAG 32)
  (define ZEROFILL_FLAG 64)
  (define BINARY_FLAG 128)
  (define AUTO_INCREMENT_FLAG 512)

  (define CLIENT_COMPRESS 32)
  (define CLIENT_FOUND_ROWS 2)
  (define CLIENT_IGNORE_SPACE 256)
  (define CLIENT_INTERACTIVE 1024)
  (define CLIENT_LOCAL_FILES 128)
  (define CLIENT_MULTI_RESULTS 131072)
  (define CLIENT_MULTI_STATEMENTS 65536)
  (define CLIENT_NO_SCHEMA 16)
  (define CLIENT_ODBC 64)

  ;; void my_init(void)
  (define-function void my_init ())

  ;; my_ulonglong mysql_affected_rows(MYSQL* mysql)
  (define-function uint64_t mysql_affected_rows (void*))

  ;;  my_bool mysql_autocommit(MYSQL* mysql, my_bool mode)
  (define-function int8_t mysql_autocommit (void* int8_t))

  ;;  my_bool mysql_change_user(MYSQL* mysql, const char* user, const char* password, const char* db)
  (define-function int8_t mysql_change_user (void* char* char* char*))

  ;;  const char* mysql_character_set_name(MYSQL* mysql)
  (define-function char* mysql_character_set_name (void*))

  ;;  void mysql_close(MYSQL* mysql)
  (define-function void mysql_close (void*))

  ;;  my_bool mysql_commit(MYSQL* mysql)
  (define-function int8_t mysql_commit (void*))

  ;;  void mysql_data_seek(MYSQL_RES* result, my_ulonglong offset)
  (define-function void mysql_data_seek (void* uint64_t))

  ;;  void mysql_debug(const char* debug)
  (define-function void mysql_debug (char*))

  ;;  int mysql_dump_debug_info(MYSQL* mysql)
  (define-function int mysql_dump_debug_info (void*))

  ;;  unsigned int mysql_errno(MYSQL* mysql)
  (define-function unsigned-int mysql_errno (void*))

  ;;  const char* mysql_error(MYSQL* mysql)
  (define-function char* mysql_error (void*))

  ;;  MYSQL_FIELD* mysql_fetch_field(MYSQL_RES* result)
  (define-function void* mysql_fetch_field (void*))

  ;;  MYSQL_FIELD* mysql_fetch_field_direct(MYSQL_RES* result, unsigned int fieldnr)
  (define-function void* mysql_fetch_field_direct (void* unsigned-int))

  ;;  MYSQL_FIELD* mysql_fetch_fields(MYSQL_RES* result)
  (define-function void* mysql_fetch_fields (void*))

  ;;  unsigned long* mysql_fetch_lengths(MYSQL_RES* result)
  (define-function void* mysql_fetch_lengths (void*))

  ;;  MYSQL_ROW mysql_fetch_row(MYSQL_RES* result)
  (define-function void* mysql_fetch_row (void*))

  ;;  unsigned int mysql_field_count(MYSQL* mysql)
  (define-function unsigned-int mysql_field_count (void*))

  ;;  MYSQL_FIELD_OFFSET mysql_field_seek(MYSQL_RES* result, MYSQL_FIELD_OFFSET offset)
  (define-function unsigned-int mysql_field_seek (void* unsigned-int))

  ;;  MYSQL_FIELD_OFFSET mysql_field_tell(MYSQL_RES* result)
  (define-function unsigned-int mysql_field_tell (void*))

  ;;  void mysql_free_result(MYSQL_RES* result)
  (define-function void mysql_free_result (void*))

  ;;  void mysql_get_character_set_info(MYSQL* mysql, MY_CHARSET_INFO* cs)
  (define-function void mysql_get_character_set_info (void* void*))

  ;;  const char* mysql_get_client_info(void)
  (define-function char* mysql_get_client_info ())

  ;;  unsigned long mysql_get_client_version(void)
  (define-function unsigned-long mysql_get_client_version ())

  ;;  const char* mysql_get_host_info(MYSQL* mysql)
  (define-function char* mysql_get_host_info (void*))

  ;;  unsigned int mysql_get_proto_info(MYSQL* mysql)
  (define-function unsigned-int mysql_get_proto_info (void*))

  ;;  const char* mysql_get_server_info(MYSQL* mysql)
  (define-function char* mysql_get_server_info (void*))

  ;; unsigned long mysql_get_server_version(MYSQL* mysql)
  (define-function unsigned-long mysql_get_server_version (void*))

  ;; const char* mysql_get_ssl_cipher(MYSQL* mysql)
  (define-function char* mysql_get_ssl_cipher (void*))

  ;; unsigned long mysql_hex_string(char* to, const char* from, unsigned long length)
  (define-function unsigned-long mysql_hex_string (void* char* unsigned-long))

  ;; const char* mysql_info(MYSQL* mysql)
  (define-function char* mysql_info (void*))

  ;; MYSQL* mysql_init(MYSQL* mysql)
  (define-function void* mysql_init (void*))

  ;; my_ulonglong mysql_insert_id(MYSQL* mysql)
  (define-function uint64_t mysql_insert_id (void*))

  ;; int mysql_kill(MYSQL* mysql, unsigned long pid)
  (define-function int mysql_kill (void* unsigned-long))

  ;; MYSQL_RES* mysql_list_dbs(MYSQL* mysql, const char* wild)
  (define-function void* mysql_list_dbs (void* char*))

  ;; MYSQL_RES* mysql_list_fields(MYSQL* mysql, const char* table, const char* wild)
  (define-function void* mysql_list_fields (void* char* char*))

  ;; MYSQL_RES* mysql_list_processes(MYSQL* mysql)
  (define-function void* mysql_list_processes (void*))

  ;; MYSQL_RES* mysql_list_tables(MYSQL* mysql, const char* wild)
  (define-function void* mysql_list_tables (void* char*))

  ;; my_bool mysql_more_results(MYSQL* mysql)
  (define-function int8_t mysql_more_results (void*))

  ;; int mysql_next_result(MYSQL* mysql)
  (define-function int mysql_next_result (void*))

  ;; unsigned int mysql_num_fields(MYSQL_RES* result)
  (define-function unsigned-int mysql_num_fields (void*))

  ;; my_ulonglong mysql_num_rows(MYSQL_RES* result)
  (define-function uint64_t mysql_num_rows (void*))

  ;; int mysql_options(MYSQL* mysql, enum mysql_option option, const void* arg)
  (define-function int mysql_options (void* int void*))

  ;; int mysql_ping(MYSQL* mysql)
  (define-function int mysql_ping (void*))

  ;; int mysql_query(MYSQL* mysql, const char* stmt_str)
  (define-function int mysql_query (void* char*))

  ;; MYSQL* mysql_real_connect(MYSQL* mysql, const char* host, const char* user, const char* passwd, const char* db, unsigned int port, const char* unix_socket, unsigned long client_flag)
  (define-function void* mysql_real_connect (void* char* char* char* char* unsigned-int char* unsigned-long))

  ;; unsigned long mysql_real_escape_string(MYSQL* mysql, char* to, const char* from, unsigned long length)
  (define-function unsigned-long mysql_real_escape_string (void* void* char* unsigned-long))

  ;; int mysql_real_query(MYSQL* mysql, const char* stmt_str, unsigned long length)
  (define-function int mysql_real_query (void* char* unsigned-long))

  ;; int mysql_refresh(MYSQL* mysql, unsigned int options)
  (define-function int mysql_refresh (void* unsigned-int))

  ;; my_bool mysql_rollback(MYSQL* mysql)
  (define-function int8_t mysql_rollback (void*))

  ;; MYSQL_ROW_OFFSET mysql_row_seek(MYSQL_RES* result, MYSQL_ROW_OFFSET offset)
  (define-function void* mysql_row_seek (void* void*))

  ;; MYSQL_ROW_OFFSET mysql_row_tell(MYSQL_RES* result)
  (define-function void* mysql_row_tell (void*))

  ;; int mysql_select_db(MYSQL* mysql, const char* db)
  (define-function int mysql_select_db (void* char*))

  ;; void mysql_server_end(void)
  (define-function void mysql_server_end ())

  ;; int mysql_server_init(int argc, char** argv, char** groups)
  (define-function void mysql_server_init (int (char*) (char*)))

  ;; int mysql_set_character_set(MYSQL* mysql, const char* csname)
  (define-function int mysql_set_character_set (void* char*))

  ;; void mysql_set_local_infile_default(MYSQL* mysql)
  (define-function void mysql_set_local_infile_default (void*))

  ;; void mysql_set_local_infile_handler(MYSQL* mysql, int (*local_infile_init)(void** , const char* , void* ), int (*local_infile_read)(void* , char* , unsigned int), void (*local_infile_end)(void* ), int (*local_infile_error)(void* , char*, unsigned int), void* userdata)
  (define-function void mysql_set_local_infile_handler (void* (c-callback int (void* void* void*)) (c-callback int (void* void* unsigned-int)) (c-callback void (void*)) (c-callback int (void* void* unsigned-int)) void*))

  ;; int mysql_set_server_option(MYSQL* mysql, enum enum_mysql_set_option option)
  (define-function int mysql_set_server_option (void* int))

  ;; int mysql_shutdown(MYSQL* mysql, enum mysql_enum_shutdown_level shutdown_level)
  (define-function int mysql_shutdown (void* int))

  ;; const char* mysql_sqlstate(MYSQL* mysql)
  (define-function char* mysql_sqlstate (void*))

  ;; my_bool mysql_ssl_set(MYSQL* mysql, const char* key, const char* cert, const char* ca, const char* capath, const char* cipher)
  (define-function int8_t mysql_ssl_set (void* char* char* char* char* char*))

  ;; const char* mysql_stat(MYSQL* mysql)
  (define-function char* mysql_stat (void*))

  ;; my_ulonglong mysql_stmt_affected_rows(MYSQL_STMT* stmt)
  (define-function uint64_t mysql_stmt_affected_rows (void*))

  ;; my_bool mysql_stmt_attr_get(MYSQL_STMT* stmt, enum enum_stmt_attr_type option, void* arg)
  (define-function int8_t mysql_stmt_attr_get (void* int void*))

  ;; my_bool mysql_stmt_attr_set(MYSQL_STMT* stmt, enum enum_stmt_attr_type option, const void* arg)
  (define-function int8_t mysql_stmt_attr_set (void* int void*))

  ;; my_bool mysql_stmt_bind_param(MYSQL_STMT* stmt, MYSQL_BIND* bind)
  (define-function int8_t mysql_stmt_bind_param (void* void*))

  ;; my_bool mysql_stmt_bind_result(MYSQL_STMT* stmt, MYSQL_BIND* bind)
  (define-function int8_t mysql_stmt_bind_result (void* void*))

  ;; my_bool mysql_stmt_close(MYSQL_STMT* stmt)
  (define-function int8_t mysql_stmt_close (void*))

  ;; void mysql_stmt_data_seek(MYSQL_STMT* stmt, my_ulonglong offset)
  (define-function void mysql_stmt_data_seek (void* uint64_t))

  ;; unsigned int mysql_stmt_errno(MYSQL_STMT* stmt)
  (define-function unsigned-int mysql_stmt_errno (void*))

  ;; const char* mysql_stmt_error(MYSQL_STMT* stmt)
  (define-function char* mysql_stmt_error (void*))

  ;; int mysql_stmt_execute(MYSQL_STMT* stmt)
  (define-function int mysql_stmt_execute (void*))

  ;; int mysql_stmt_fetch(MYSQL_STMT* stmt)
  (define-function int mysql_stmt_fetch (void*))

  ;; int mysql_stmt_fetch_column(MYSQL_STMT* stmt, MYSQL_BIND* bind, unsigned int column, unsigned long offset)
  (define-function int mysql_stmt_fetch_column (void* void* unsigned-int unsigned-long))

  ;; unsigned int mysql_stmt_field_count(MYSQL_STMT* stmt)
  (define-function unsigned-int mysql_stmt_field_count (void*))

  ;; my_bool mysql_stmt_free_result(MYSQL_STMT* stmt)
  (define-function int8_t mysql_stmt_free_result (void*))

  ;; MYSQL_STMT* mysql_stmt_init(MYSQL* mysql)
  (define-function void* mysql_stmt_init (void*))

  ;; my_ulonglong mysql_stmt_insert_id(MYSQL_STMT* stmt)
  (define-function uint64_t mysql_stmt_insert_id (void*))

  ;; my_ulonglong mysql_stmt_num_rows(MYSQL_STMT* stmt)
  (define-function uint64_t mysql_stmt_num_rows (void*))

  ;; unsigned long mysql_stmt_param_count(MYSQL_STMT* stmt)
  (define-function unsigned-long mysql_stmt_param_count (void*))

  ;; MYSQL_RES* mysql_stmt_param_metadata(MYSQL_STMT* stmt)
  (define-function void* mysql_stmt_param_metadata (void*))

  ;; int mysql_stmt_prepare(MYSQL_STMT* stmt, const char* stmt_str, unsigned long length)
  (define-function int mysql_stmt_prepare (void* char* unsigned-long))

  ;; my_bool mysql_stmt_reset(MYSQL_STMT* stmt)
  (define-function int8_t mysql_stmt_reset (void*))

  ;; MYSQL_RES* mysql_stmt_result_metadata(MYSQL_STMT* stmt)
  (define-function void* mysql_stmt_result_metadata (void*))

  ;; MYSQL_ROW_OFFSET mysql_stmt_row_seek(MYSQL_STMT* stmt, MYSQL_ROW_OFFSET offset)
  (define-function void* mysql_stmt_row_seek (void* void*))

  ;; MYSQL_ROW_OFFSET mysql_stmt_row_tell(MYSQL_STMT* stmt)
  (define-function void* mysql_stmt_row_tell (void*))

  ;; my_bool mysql_stmt_send_long_data(MYSQL_STMT* stmt, unsigned int parameter_number, const char* data, unsigned long length)
  (define-function int8_t mysql_stmt_send_long_data (void* unsigned-int char* unsigned-long))

  ;; const char* mysql_stmt_sqlstate(MYSQL_STMT* stmt)
  (define-function char* mysql_stmt_sqlstate (void*))

  ;; int mysql_stmt_store_result(MYSQL_STMT* stmt)
  (define-function int mysql_stmt_store_result (void*))

  ;; MYSQL_RES* mysql_store_result(MYSQL* mysql)
  (define-function void* mysql_store_result (void*))

  ;; void mysql_thread_end(void)
  (define-function void mysql_thread_end ())

  ;; unsigned long mysql_thread_id(MYSQL* mysql)
  (define-function unsigned-long mysql_thread_id (void*))

  ;; my_bool mysql_thread_init(void)
  (define-function int8_t mysql_thread_init ())

  ;; unsigned int mysql_thread_safe(void)
  (define-function unsigned-int mysql_thread_safe ())

  ;; MYSQL_RES* mysql_use_result(MYSQL* mysql)
  (define-function void* mysql_use_result (void*))

  ;; unsigned int mysql_warning_count(MYSQL* mysql)
  (define-function unsigned-int mysql_warning_count (void*))

  ) ;[end]
