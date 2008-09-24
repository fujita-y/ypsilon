/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef SOCKET_H_INCLUDED
#define SOCKET_H_INCLUDED

#include "core.h"
#include "object.h"
#include "ioerror.h"

#define SCM_SOCKET_MODE_NONE    0
#define SCM_SOCKET_MODE_CLIENT  1
#define SCM_SOCKET_MODE_SERVER  2

void socket_open(scm_socket_t s, const char* node, const char* service, int family, int type, int protocol, int flags);
void socket_close(scm_socket_t s);
void socket_shutdown(scm_socket_t s, int how);
int socket_send(scm_socket_t s, uint8_t* buf, int len, int flags);
int socket_recv(scm_socket_t s, uint8_t* buf, int len, int flags, bool* again);
scm_obj_t socket_name_string(object_heap_t* heap, scm_socket_t s);
scm_obj_t socket_accept(object_heap_t* heap, scm_socket_t s);

#endif


