/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "port.h"
#include "socket.h"

#if _MSC_VER
  #define CLOSE_SOCKET closesocket
#else
  #define CLOSE_SOCKET close
#endif

static void
throw_socket_error(int operation, int code)
{
    throw io_exception_t(operation, code);
}

static void
throw_socket_error(int operation, const char* message)
{
    throw io_exception_t(operation, message);
}

void
socket_open(scm_socket_t s, const char* node, const char* service, int family, int type, int flags, int protocol)
{
    s->lock.verify_locked();
    struct addrinfo hints;
    struct addrinfo* list;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = family;
    hints.ai_socktype = type;
    hints.ai_flags = flags;
    hints.ai_protocol = protocol;
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;
    int retval = getaddrinfo(node, service, &hints, &list);
#if _MSC_VER
    if (retval) {
        char utf8[256];
        if (!WideCharToMultiByte(CP_UTF8, 0, gai_strerrorW(retval), -1, utf8, sizeof(utf8), NULL, NULL)) {
            snprintf(utf8, sizeof(utf8), "socket operation failed(%d)", retval);
        }
        throw_socket_error(SCM_SOCKET_OPERATION_OPEN, utf8);
    }
#else
    if (retval) throw_socket_error(SCM_SOCKET_OPERATION_OPEN, gai_strerror(retval));
#endif
    int first_error = 0;
    if (flags & AI_PASSIVE) {
        for (struct addrinfo* p = list; p != NULL; p = p->ai_next) {
            int fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
            if (fd == INVALID_SOCKET) continue;
            int one = 1;
            if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&one, sizeof(one)) == 0) {
                if (bind(fd, p->ai_addr, p->ai_addrlen) == 0) {
                    s->mode = SCM_SOCKET_MODE_SERVER;
                    s->fd = fd;
                    s->family = p->ai_family;
                    s->socktype = p->ai_socktype;
                    s->protocol = p->ai_protocol;
                    s->addrlen = p->ai_addrlen;
                    memcpy(&s->addr, p->ai_addr, p->ai_addrlen);
                    freeaddrinfo(list);
                    if (p->ai_socktype == SOCK_STREAM) {
                        if (listen(fd, 5) == 0) return;
                    } else {
                        return;
                    }
                }
            }
            if (first_error == 0) first_error = errno;
            CLOSE_SOCKET(fd);
        }
    } else {
        for (struct addrinfo* p = list; p != NULL; p = p->ai_next) {
            int fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
            if (fd == INVALID_SOCKET) continue;
            if (connect(fd, p->ai_addr, p->ai_addrlen) == 0) {
                s->mode = SCM_SOCKET_MODE_CLIENT;
                s->fd = fd;
                s->family = p->ai_family;
                s->socktype = p->ai_socktype;
                s->protocol = p->ai_protocol;
                s->addrlen = p->ai_addrlen;
                memcpy(&s->addr, p->ai_addr, p->ai_addrlen);
                freeaddrinfo(list);
                return;
            }
            if (first_error == 0) first_error = errno;
            CLOSE_SOCKET(fd);
        }
    }
    freeaddrinfo(list);
    throw_socket_error(SCM_SOCKET_OPERATION_OPEN, first_error);
}

void
socket_shutdown(scm_socket_t s, int how)
{
    s->lock.verify_locked();
    if (s->fd == INVALID_SOCKET) return;
    shutdown(s->fd, how);
}

void
socket_close(scm_socket_t s)
{
    s->lock.verify_locked();
    if (s->fd == INVALID_SOCKET) return;
    CLOSE_SOCKET(s->fd);
    s->fd = INVALID_SOCKET;
}

scm_obj_t
socket_name_string(object_heap_t* heap, scm_socket_t s)
{
    char hbuf[NI_MAXHOST];
    char sbuf[NI_MAXSERV];
    if (getnameinfo((const sockaddr*)&s->addr, s->addrlen, hbuf, sizeof(hbuf), sbuf, sizeof(sbuf), NI_NUMERICHOST | NI_NUMERICSERV)) {
        return scm_false;
    }
    char nbuf[NI_MAXHOST + NI_MAXSERV];
    if (s->family == AF_INET6) snprintf(nbuf, sizeof(nbuf), "[%s]:%s", hbuf, sbuf);
    else snprintf(nbuf, sizeof(nbuf), "%s:%s", hbuf, sbuf);
    return make_string_literal(heap, nbuf);
}

int
socket_send(scm_socket_t s, uint8_t* buf, int len, int flags)
{
    s->lock.verify_locked();
    assert(s->fd != INVALID_SOCKET);
    uint8_t* p = buf;
    int rest = len;
    int written = 0;
    while (rest > 0) {
        int n = send(s->fd, (const char*)p, rest, flags);
        if (n < 0) {
            if (errno == EAGAIN) return written;
            if (errno == EINTR) continue;
            throw_socket_error(SCM_SOCKET_OPERATION_WRITE, errno);
        }
        p += n;
        written += n;
        rest -= n;
    }
    return written;
}

int
socket_recv(scm_socket_t s, uint8_t* buf, int len, int flags, bool* again)
{
    s->lock.verify_locked();
    assert(s->fd != INVALID_SOCKET);

loop:
    int n = recv(s->fd, (char*)buf, len, flags);
    if (n < 0) {
        if (errno == EAGAIN) {
            if (again == NULL) goto loop;
            *again = true;
            return 0;
        }
        if (errno == EINTR) goto loop;
        throw_socket_error(SCM_SOCKET_OPERATION_READ, errno);
    }
    if (again) *again = false;
    return n;
}

scm_obj_t
socket_accept(object_heap_t* heap, scm_socket_t s)
{
    s->lock.verify_locked();
    assert(s->fd != INVALID_SOCKET);
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);

loop:
    int fd = accept(s->fd, (sockaddr*)&addr, &addrlen);
    if (fd < 0) {
        if (errno == EAGAIN) return scm_false;
#ifdef EWOULDBLOCK
        if (errno == EWOULDBLOCK) return scm_false;
#endif
        if (errno == EINTR) goto loop;
        throw_socket_error(SCM_SOCKET_OPERATION_ACCEPT, errno);
    }
    scm_socket_t client = make_socket(heap);
    client->mode = SCM_SOCKET_MODE_CLIENT;
    client->fd = fd;
    client->family = s->family;
    client->socktype = s->socktype;
    client->protocol = s->protocol;
    client->addrlen = addrlen;
    memcpy(&client->addr, &addr, addrlen);
    return client;
}
