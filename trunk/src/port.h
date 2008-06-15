/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef PORT_H_INCLUDED
#define PORT_H_INCLUDED

#include "core.h"
#include "object.h"

#define SCM_PORT_TYPE_NAMED_FILE                1
#define SCM_PORT_TYPE_BYTEVECTOR                2
#define SCM_PORT_TYPE_CUSTOM                    3

#define SCM_PORT_SUBTYPE_NONE                   0
#define SCM_PORT_SUBTYPE_CHAR_SPECIAL           1
#define SCM_PORT_SUBTYPE_FIFO                   2

#define SCM_PORT_DIRECTION_IN                   0x01
#define SCM_PORT_DIRECTION_OUT                  0x02
#define SCM_PORT_DIRECTION_BOTH                 0x03

#define SCM_PORT_BUFFER_MODE_NONE               1
#define SCM_PORT_BUFFER_MODE_LINE               2
#define SCM_PORT_BUFFER_MODE_BLOCK              3

#define SCM_PORT_FILE_OPTION_NONE               0
#define SCM_PORT_FILE_OPTION_NO_CREATE          0x01
#define SCM_PORT_FILE_OPTION_NO_FAIL            0x02
#define SCM_PORT_FILE_OPTION_NO_TRUNCATE        0x04

#define SCM_PORT_CODEC_LATIN1                   1
#define SCM_PORT_CODEC_UTF8                     2
#define SCM_PORT_CODEC_UTF16                    3
#if _MSC_VER
  #define SCM_PORT_CODEC_CP932                  4
#endif

#define SCM_PORT_EOL_STYLE_NONE                 1
#define SCM_PORT_EOL_STYLE_LF                   2
#define SCM_PORT_EOL_STYLE_CR                   3
#define SCM_PORT_EOL_STYLE_CRLF                 4
#define SCM_PORT_EOL_STYLE_NEL                  5
#define SCM_PORT_EOL_STYLE_CRNEL                6
#define SCM_PORT_EOL_STYLE_LS                   7

#define SCM_PORT_ERROR_HANDLING_MODE_IGNORE     1
#define SCM_PORT_ERROR_HANDLING_MODE_RAISE      2
#define SCM_PORT_ERROR_HANDLING_MODE_REPLACE    3

#define SCM_PORT_BLOCK_BUFFER_SIZE              4096
#define SCM_PORT_LINE_BUFFER_SIZE               256
#define SCM_PORT_CUSTOM_BUFFER_SIZE             SCM_PORT_BLOCK_BUFFER_SIZE

#define SCM_PORT_BUF_STATE_UNSPECIFIED          0
#define SCM_PORT_BUF_STATE_READ                 1
#define SCM_PORT_BUF_STATE_WRITE                2
#define SCM_PORT_BUF_STATE_ACCUMULATE           3

#define SCM_PORT_UCS4_BOM                       0x0feff
#define SCM_PORT_UCS4_REPLACEMENT_CHAR          0x0fffd
#define SCM_PORT_UCS4_LF                        0x0000a
#define SCM_PORT_UCS4_CR                        0x0000d
#define SCM_PORT_UCS4_NEL                       0x00085
#define SCM_PORT_UCS4_LS                        0x02028

#define SCM_PORT_BYTE_REPLACEMENT_CHAR          '?'

#define SCM_PORT_BYTEVECTOR_OUTPUT_CHUNK        128

// do not change
#define SCM_PORT_CODEC_NATIVE                   SCM_PORT_CODEC_UTF8
#define SCM_PORT_EOL_STYLE_NATIVE               SCM_PORT_EOL_STYLE_LF

#define SCM_PORT_OPERATION_OPEN                 1
#define SCM_PORT_OPERATION_READ                 2
#define SCM_PORT_OPERATION_WRITE                3
#define SCM_PORT_OPERATION_CLOSE                4
#define SCM_PORT_OPERATION_SEEK                 5
#define SCM_PORT_OPERATION_STAT                 6
#define SCM_PORT_OPERATION_SELECT               7
#define SCM_PORT_OPERATION_ENCODE               8
#define SCM_PORT_OPERATION_DECODE               9


enum {
    SCM_PORT_HANDLER_TEXTUAL = 0,
    SCM_PORT_HANDLER_READ,
    SCM_PORT_HANDLER_WRITE,
    SCM_PORT_HANDLER_GET_POS,   // expect 1 arg for textual(current byte position), no arg for binary
    SCM_PORT_HANDLER_SET_POS,
    SCM_PORT_HANDLER_CLOSE
};

void port_open_std(scm_port_t port, fd_t fd, scm_obj_t name, int direction, int file_options, int buffer_mode, scm_obj_t transcoder);
void port_open_file(scm_port_t port, scm_obj_t name, int direction, int file_options, int buffer_mode, scm_obj_t transcoder);
void port_open_bytevector(scm_port_t port, scm_obj_t name, int direction, scm_obj_t bytes, scm_obj_t transcoder);
void port_make_custom_port(scm_port_t port, scm_obj_t name, int direction, scm_obj_t handlers, scm_obj_t transcoder);
void port_make_transcoded_port(scm_obj_t name, scm_port_t binary, scm_port_t textual, scm_bvector_t transcoder);
void port_open_temp_file(scm_port_t port, scm_obj_t name, int buffer_mode, scm_obj_t transcoder);
void port_flush_output(scm_port_t port);
void port_close(scm_port_t port);
bool port_nonblock_byte_ready(scm_port_t port);
bool port_eof(scm_port_t port);
int port_lookahead_byte(scm_port_t port);
int port_get_byte(scm_port_t port);
scm_obj_t port_lookahead_utf8(scm_port_t port);
scm_obj_t port_get_utf8(scm_port_t port);
void port_put_byte(scm_port_t port, int byte);
void port_puts(scm_port_t port, const char* s);
void port_format(scm_port_t port, const char *fmt, ...);
bool port_input_pred(scm_port_t port);
bool port_output_pred(scm_port_t port);
bool port_textual_pred(scm_port_t port);
bool port_binary_pred(scm_port_t port);
bool port_open_pred(scm_port_t port);
bool port_bytevector_pred(scm_port_t port);
bool port_regular_file_pred(scm_port_t port);
int port_buffered_byte_count(scm_port_t port);
bool port_has_port_position_pred(scm_port_t port);
bool port_has_set_port_position_pred(scm_port_t port);
off64_t port_position(scm_port_t port);
void port_set_port_position(scm_port_t port, off64_t);
scm_bvector_t port_extract_bytevector(object_heap_t* heap, scm_port_t port);
scm_string_t port_extract_string(object_heap_t* heap, scm_port_t port);
scm_string_t port_get_string(object_heap_t* heap, scm_port_t port);
scm_obj_t port_lookahead_char(scm_port_t port);
scm_obj_t port_get_char(scm_port_t port);
scm_obj_t port_lookahead_u8(scm_port_t port);
scm_obj_t port_get_u8(scm_port_t port);
int port_output_buffer_mode(scm_port_t port);
void port_put_char(scm_port_t port, scm_char_t c);
void port_put_string(scm_port_t port, scm_string_t string);

#endif


