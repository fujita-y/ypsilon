/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/


#ifndef IOERROR_H_INCLUDED
#define IOERROR_H_INCLUDED

#include "core.h"
#include "object.h"

#define SCM_PORT_OPERATION_OPEN         1
#define SCM_PORT_OPERATION_READ         2
#define SCM_PORT_OPERATION_WRITE        3
#define SCM_PORT_OPERATION_CLOSE        4
#define SCM_PORT_OPERATION_SEEK         5
#define SCM_PORT_OPERATION_STAT         6
#define SCM_PORT_OPERATION_SELECT       7
#define SCM_PORT_OPERATION_ENCODE       8
#define SCM_PORT_OPERATION_DECODE       9
#define SCM_SOCKET_OPERATION_OPEN       10
#define SCM_SOCKET_OPERATION_WRITE      11
#define SCM_SOCKET_OPERATION_READ       12
#define SCM_SOCKET_OPERATION_ACCEPT     13

void raise_io_error(VM* vm, const char* who, int operation, const char* message, int err, scm_obj_t port, scm_obj_t filename);
void raise_io_codec_error(VM* vm, const char* who, int operation, const char* message, scm_obj_t port, scm_obj_t ch);
void raise_io_filesystem_error(VM* vm, const char* who, const char* message, int err, scm_obj_t old_filename, scm_obj_t new_filename);

#endif
