/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/
/*

 * 'ungetc' implementation may return part of replacement character to subsequent get-u8 operation.
   If buffer-mode is 'none, this implementation do that otherwise subsequent get-u8 operation provide
   octet which triggered decode error.

 * lookahead[4] for exclusive use of non-bufferd named file

 * filr position after lookahead operation:
   non-buffered |__________________________________________|
                    ^[filepos]
                    ^[lookahead ...]
       buffered |__________________________________________|
                    ^[filepos]
                    ^[buf_head]  ........... ^[buf_tail]

 * SCM_PORT_SUBTYPE_CHAR_SPECIAL for non-positionable named file.

 * Mark is absense if port is non-positionable, however each read/write operation update it.

 */

#include "core.h"
#include "vm.h"
#include "file.h"
#include "port.h"
#include "socket.h"
#include "utf8.h"
#include "arith.h"
#include "ioerror.h"

#define SCM_CHAR_LF             MAKECHAR(SCM_PORT_UCS4_LF)
#define SCM_CHAR_OCTET_MAX      4

#define CUSTOM_PORT_FORCE_SYNC  1

static void throw_io_error(int operation, int code)
{
    throw io_exception_t(operation, code);
}

static void throw_io_error(int operation, const char* message)
{
    throw io_exception_t(operation, message);
}

#if _MSC_VER

    #define S_IRUSR         _S_IREAD
    #define S_IWUSR         _S_IWRITE

    static fd_t io_open(const char* path, int oflag, int pmode)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            int access = GENERIC_READ;
            switch (oflag & (O_RDWR | O_RDONLY | O_WRONLY)) {
                case O_RDONLY:
                    access = GENERIC_READ;
                    break;
                case O_WRONLY:
                    access = GENERIC_WRITE;
                    break;
                case O_RDWR:
                    access = GENERIC_READ | GENERIC_WRITE;
                    break;
            }
            int create = OPEN_EXISTING;
            switch (oflag & (O_CREAT | O_TRUNC | O_EXCL)) {
                case O_CREAT:
                    create = OPEN_ALWAYS;
                    break;
                case O_CREAT | O_TRUNC:
                    create = CREATE_ALWAYS;
                    break;
                case O_EXCL:
                    create = OPEN_EXISTING;
                    break;
                case O_CREAT | O_EXCL:
                case O_CREAT | O_EXCL | O_TRUNC:
                    create = CREATE_NEW;
                    break;
                case O_TRUNC:
                case O_TRUNC | O_EXCL:
                    create = TRUNCATE_EXISTING;
                    break;
            }
            HANDLE fd = CreateFileW(ucs2,
                                   access,
                                   FILE_SHARE_READ | FILE_SHARE_WRITE,
                                   NULL,
                                   create,
                                   FILE_ATTRIBUTE_NORMAL,
                                   NULL);
            if (fd == INVALID_HANDLE_VALUE) {
                _dosmaperr(GetLastError());
                return INVALID_FD;
            }
            return fd;
        }
        errno = ENOENT;
        return INVALID_FD;
    }

    static int io_close(fd_t fd)
    {
        if (CloseHandle(fd)) return 0;
        _dosmaperr(GetLastError());
        return -1;
    }

    static int io_pread(fd_t fd, void* buf, size_t nbytes, off64_t offset)
    {
        DWORD count = 0;
        OVERLAPPED ov;
        ov.Offset = offset & 0xffffffff;
        ov.OffsetHigh = offset >> 32;
        ov.hEvent = NULL;
        if (ReadFile(fd, buf, nbytes, &count, &ov) == 0) {
            DWORD err = GetLastError();
            if (err == ERROR_HANDLE_EOF) return 0;
            if (err == ERROR_BROKEN_PIPE) return 0;
            if (err == ERROR_ACCESS_DENIED) errno = EBADF;
            else _dosmaperr(err);
            return -1;
        }
        return count;
    }

    static int io_pwrite(fd_t fd, void* buf, size_t nbytes, off64_t offset)
    {
        DWORD count = 0;
        OVERLAPPED ov;
        ov.Offset = offset & 0xffffffff;
        ov.OffsetHigh = offset >> 32;
        ov.hEvent = NULL;
        if (WriteFile(fd, buf, nbytes, &count, &ov) == 0) {
            DWORD err = GetLastError();
            if (err == ERROR_ACCESS_DENIED) errno = EBADF;
            else _dosmaperr(err);
            return -1;
        }
        if (count == 0) {
            errno = ENOSPC;
            return -1;
        }
        return count;
    }

    static int io_read(fd_t fd, void* buf, size_t nbytes)
    {
        DWORD count = 0;
        if (ReadFile(fd, buf, nbytes, &count, NULL) == 0) {
            DWORD err = GetLastError();
            if (err == ERROR_HANDLE_EOF) return 0;
            if (err == ERROR_BROKEN_PIPE) return 0;
            if (err == ERROR_ACCESS_DENIED) errno = EBADF;
            else _dosmaperr(err);
            return -1;
        }
        return count;
    }

    static int io_write(fd_t fd, void* buf, size_t nbytes)
    {
        if (nbytes == 0) return 0;
        DWORD count = 0;
        if (WriteFile(fd, buf, nbytes, &count, NULL) == 0) {
            DWORD err = GetLastError();
            if (err == ERROR_ACCESS_DENIED) errno = EBADF;
            else _dosmaperr(err);
            return -1;
        }
        if (count == 0) {
            errno = ENOSPC;
            return -1;
        }
        return count;
    }

    static off64_t io_lseek64(fd_t fd, off64_t offset, int origin)
    {
        LARGE_INTEGER in;
        in.QuadPart = offset;
        LARGE_INTEGER out;
        int method;
        switch (origin) {
        case SEEK_SET: method = FILE_BEGIN; break;
        case SEEK_CUR: method = FILE_CURRENT; break;
        case SEEK_END: method = FILE_END; break;
        default:
            fatal("%s:%u wrong origin", __FILE__, __LINE__);
        }
        if (SetFilePointerEx(fd, in, &out, method)) return out.QuadPart;
        _dosmaperr(GetLastError());
        return -1;
    }

    static fd_t io_mkstemp(char *tmpl)
    {
        char pathBuf[MAX_PATH];
        int retval;
        retval = GetTempPathA(sizeof(pathBuf), pathBuf);
        if (retval == 0) {
            _dosmaperr(GetLastError());
            return INVALID_FD;
        }
        char nameBuf[MAX_PATH + 64];
        retval = GetTempFileNameA(pathBuf, "tmp", 0, nameBuf);
        if (retval == 0) {
            _dosmaperr(GetLastError());
            return INVALID_FD;
        }
        HANDLE fd = CreateFileA(nameBuf,
                            GENERIC_READ | GENERIC_WRITE,
                            FILE_SHARE_READ | FILE_SHARE_WRITE,
                            NULL,
                            CREATE_ALWAYS,
                            FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE,
                            NULL);
        if (fd == INVALID_HANDLE_VALUE) {
            _dosmaperr(GetLastError());
            return INVALID_FD;
        }
        return fd;
    }

    static int io_stat_mode(const char* path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            HANDLE fd = CreateFileW(ucs2, 0, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
            if (fd != INVALID_HANDLE_VALUE) {
                DWORD type = GetFileType(fd);
                CloseHandle(fd);
                switch (type) {
                    case FILE_TYPE_CHAR: return SCM_PORT_SUBTYPE_CHAR_SPECIAL;
                    case FILE_TYPE_PIPE: return SCM_PORT_SUBTYPE_FIFO;
                }
            }
            return SCM_PORT_SUBTYPE_NONE;
        }
        return SCM_PORT_SUBTYPE_NONE;
    }

    static int io_fstat_mode(fd_t fd)
    {
        switch (GetFileType(fd)) {
            case FILE_TYPE_CHAR: return SCM_PORT_SUBTYPE_CHAR_SPECIAL;
            case FILE_TYPE_PIPE: return SCM_PORT_SUBTYPE_FIFO;
        }
        return SCM_PORT_SUBTYPE_NONE;
    }

    static int io_fstat_size(fd_t fd, off64_t* size)
    {
        LARGE_INTEGER fileSize;
        if (GetFileSizeEx(fd, &fileSize)) {
            *size = fileSize.QuadPart;
            return 0;
        }
        _dosmaperr(GetLastError());
        return -1;
    }

#else

    static inline int io_open(const char* path, int oflag, int pmode)
    {
        return open(path, oflag, pmode);
    }

    static inline int io_close(fd_t fd)
    {
        return close(fd);
    }

    static inline int io_read(fd_t fd, void* buf, size_t nbytes)
    {
        return read(fd, buf, nbytes);
    }

    static inline int io_write(fd_t fd, void* buf, size_t nbytes)
    {
        return write(fd, buf, nbytes);
    }

    static inline int io_pread(fd_t fd, void* buf, size_t nbytes, off64_t offset)
    {
        return pread(fd, buf, nbytes, offset);
    }

    static inline int io_pwrite(fd_t fd, void* buf, size_t nbytes, off64_t offset)
    {
        return pwrite(fd, buf, nbytes, offset);
    }

    static inline off64_t io_lseek64(fd_t fd, off64_t offset, int origin)
    {
        return lseek(fd, offset, origin);
    }

    static inline fd_t io_mkstemp(char *tmpl)
    {
        return mkstemp(tmpl);
    }

    static int io_stat_mode(const char* path)
    {
        struct stat st;
        if (stat(path, &st) == 0) {
            if (S_ISCHR(st.st_mode)) return SCM_PORT_SUBTYPE_CHAR_SPECIAL;
            if (S_ISFIFO(st.st_mode)) return SCM_PORT_SUBTYPE_FIFO;
        }
        return SCM_PORT_SUBTYPE_NONE;
    }

    static int io_fstat_mode(fd_t fd)
    {
        struct stat st;
        if (fstat(fd, &st) == 0) {
            if (S_ISCHR(st.st_mode)) return SCM_PORT_SUBTYPE_CHAR_SPECIAL;
            if (S_ISFIFO(st.st_mode)) return SCM_PORT_SUBTYPE_FIFO;
        }
        return SCM_PORT_SUBTYPE_NONE;
    }

    static int io_fstat_size(fd_t fd, off64_t* size)
    {
        struct stat st;
        if (fstat(fd, &st) == 0) {
            *size = st.st_size;
            return 0;
        }
        return -1;
    }

#endif

static bool
no_input_buffered(scm_port_t port)
{
    return ((port->lookahead_size == 0) && (port->buf == NULL || port->buf_head == port->buf_tail || port->buf_state != SCM_PORT_BUF_STATE_READ));
}

static bool
no_output_buffered(scm_port_t port)
{
    return (port->buf_state != SCM_PORT_BUF_STATE_WRITE || port->buf == NULL || port->buf_head == port->buf_tail);
}

static void
throw_codec_error(int operation, const char* message, scm_obj_t ch)
{
    throw io_codec_exception_t(operation, message, ch);
}

static ssize_t
device_read(scm_port_t port, uint8_t* p, int size, off64_t mark)
{    
    if (port->type == SCM_PORT_TYPE_NAMED_FILE) {
        ssize_t n;
        while (true) {
            switch (port->subtype) {
                case SCM_PORT_SUBTYPE_NONE:
                    n = io_pread(port->fd, p, size, mark);
                    break;
                case SCM_PORT_SUBTYPE_FIFO:
                case SCM_PORT_SUBTYPE_CHAR_SPECIAL:
                    n = io_read(port->fd, p, size);
                    break;
                default:
                    fatal("%s:%u wrong port subtype", __FILE__, __LINE__);
            }
            if (n < 0) {
                if (errno == EINTR) continue;
                throw_io_error(SCM_PORT_OPERATION_READ, errno);
            }
            break;
        }
        return n;
    }

    if (port->type == SCM_PORT_TYPE_SOCKET) {
        return socket_recv((scm_socket_t)port_socket(port), p, size, 0, NULL);
    }
    
    if (port->type == SCM_PORT_TYPE_CUSTOM) {
        assert(size <= SCM_PORT_CUSTOM_BUFFER_SIZE);
        VM* vm = current_vm();
        assert(VECTORP(port->handlers));
        scm_vector_t vect = (scm_vector_t)port->handlers;
        assert(BVECTORP(port->bytes));
        scm_bvector_t bv = (scm_bvector_t)port->bytes;
        assert(CLOSUREP(vect->elts[SCM_PORT_HANDLER_READ]));
        scm_obj_t result = vm->call_scheme(vect->elts[SCM_PORT_HANDLER_READ], 3, bv, MAKEFIXNUM(0), MAKEFIXNUM(size));
        memcpy(p, bv->elts, size);
        if (FIXNUMP(result)) return FIXNUM(result);
        throw_io_error(SCM_PORT_OPERATION_READ, "custom port read! procedure return invalid value");
    }
    
    fatal("%s:%u wrong port type", __FILE__, __LINE__);
    
}

static void
device_write(scm_port_t port, uint8_t* p, int size, off64_t mark)
{
    if (port->type == SCM_PORT_TYPE_NAMED_FILE) {
        int rest = size;
        off64_t offset = mark;
        while (rest > 0) {
            int written;
            switch (port->subtype) {
                case SCM_PORT_SUBTYPE_NONE:
                    written = io_pwrite(port->fd, p, rest, offset);
                    break;
                case SCM_PORT_SUBTYPE_FIFO:
                case SCM_PORT_SUBTYPE_CHAR_SPECIAL:
                    written = io_write(port->fd, p, rest);
                    break;
                default:
                    fatal("%s:%u wrong port subtype", __FILE__, __LINE__);
            }
            if (written < 0) {
                if (errno == EINTR) continue;
                throw_io_error(SCM_PORT_OPERATION_WRITE, errno);
            }
            p += written;
            rest -= written;
            offset += written;
        }
        return;
    }

    if (port->type == SCM_PORT_TYPE_SOCKET) {
#ifdef MSG_NOSIGNAL
        socket_send((scm_socket_t)port_socket(port), p, size, MSG_NOSIGNAL);
#else
        socket_send((scm_socket_t)port_socket(port), p, size, 0);
#endif
        return;
    }
    
    if (port->type == SCM_PORT_TYPE_CUSTOM) {
        assert(size <= SCM_PORT_CUSTOM_BUFFER_SIZE);
        VM* vm = current_vm();
        assert(VECTORP(port->handlers));
        scm_vector_t vect = (scm_vector_t)port->handlers;
        assert(BVECTORP(port->bytes));
        scm_bvector_t bv = (scm_bvector_t)port->bytes;
        int rest = size;
        off64_t offset = mark;
        while (rest > 0) {
            int written;
            memcpy(bv->elts, p, rest);
            assert(CLOSUREP(vect->elts[SCM_PORT_HANDLER_WRITE]));
            written = FIXNUM(vm->call_scheme(vect->elts[SCM_PORT_HANDLER_WRITE], 3, bv, MAKEFIXNUM(0), MAKEFIXNUM(rest)));
            p += written;
            rest -= written;
            offset += written;
        }
        return;
    } 
        
    fatal("%s:%u wrong port type", __FILE__, __LINE__);
}

static void
device_set_mark(scm_port_t port, off64_t offset)
{

    if (port->type == SCM_PORT_TYPE_NAMED_FILE) {
        switch (port->subtype) {

            case SCM_PORT_SUBTYPE_NONE: {
                assert((port->buf_state == SCM_PORT_BUF_STATE_READ) || (port->buf_state == SCM_PORT_BUF_STATE_UNSPECIFIED));
                assert(port->fd >= 0);
                if (io_lseek64(port->fd, offset, SEEK_SET) < 0) throw_io_error(SCM_PORT_OPERATION_SEEK, errno);
            } break;

            case SCM_PORT_SUBTYPE_FIFO:
            case SCM_PORT_SUBTYPE_CHAR_SPECIAL: {
                assert(false);
            } break;

            default: fatal("%s:%u wrong port subtype", __FILE__, __LINE__);

        }
    } else if (port->type == SCM_PORT_TYPE_CUSTOM) {
        VM* vm = current_vm();
        assert(VECTORP(port->handlers));
        scm_vector_t vect = (scm_vector_t)port->handlers;
        scm_obj_t off = int64_to_integer(vm->m_heap, offset);
        assert(CLOSUREP(vect->elts[SCM_PORT_HANDLER_SET_POS]));
        vm->call_scheme(vect->elts[SCM_PORT_HANDLER_SET_POS], 1, off);
    } else {
        fatal("%s:%u wrong port type", __FILE__, __LINE__);
    }
    port->mark = offset;
    port->buf_head = port->buf_tail = port->buf;
    port->buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
}

static void
device_close(scm_port_t port)
{
    if (port->type == SCM_PORT_TYPE_NAMED_FILE) {
        if (port->fd != INVALID_FD) {
            while (true) {
                if (io_close(port->fd) < 0) {
                    if (errno == EINTR) continue;
                    throw_io_error(SCM_PORT_OPERATION_CLOSE, errno);
                }
                break;
            }
        }
        port->fd = INVALID_FD;
        return;
    }

    if (port->type == SCM_PORT_TYPE_CUSTOM) {
        VM* vm = current_vm();
        assert(VECTORP(port->handlers));
        scm_vector_t vect = (scm_vector_t)port->handlers;
        if (CLOSUREP(vect->elts[SCM_PORT_HANDLER_CLOSE])) vm->call_scheme(vect->elts[SCM_PORT_HANDLER_CLOSE], 0);
        return;
    }
}

static void
init_port_buffer(scm_port_t port)
{
    switch (port->buffer_mode) {

        case SCM_PORT_BUFFER_MODE_LINE: {
            port->buf = (uint8_t*)malloc(SCM_PORT_LINE_BUFFER_SIZE);
            if (port->buf == NULL) fatal("%s:%u memory overflow", __FILE__, __LINE__);
            port->buf_head = port->buf;
            port->buf_tail = port->buf_head;
            port->buf_size = SCM_PORT_LINE_BUFFER_SIZE;
        } break;

        case SCM_PORT_BUFFER_MODE_BLOCK: {
            port->buf = (uint8_t*)malloc(SCM_PORT_BLOCK_BUFFER_SIZE);
            if (port->buf == NULL) fatal("%s:%u memory overflow", __FILE__, __LINE__);
            port->buf_head = port->buf;
            port->buf_tail = port->buf_head;
            port->buf_size = SCM_PORT_BLOCK_BUFFER_SIZE;
        } break;

        case SCM_PORT_BUFFER_MODE_NONE: {
            port->buf = NULL;
            port->buf_head = NULL;
            port->buf_tail = NULL;
            port->buf_size = 0;
            port->buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
        } break;

        default:
            fatal("%s:%u wrong buffer mode", __FILE__, __LINE__);

    }
}

static void
init_port_transcoder(scm_port_t port)
{
    scm_obj_t transcoder = port->transcoder;
    if (BVECTORP(transcoder)) {
        scm_bvector_t bvector = (scm_bvector_t)transcoder;
        port->codec = bvector->elts[0];
        port->eol_style = bvector->elts[1];
        port->error_handling_mode = bvector->elts[2];
        if ((port->codec == SCM_PORT_CODEC_NATIVE)
            && (port->eol_style == SCM_PORT_EOL_STYLE_NATIVE)
            && (port->error_handling_mode == SCM_PORT_ERROR_HANDLING_MODE_REPLACE)) {
            port->transcoder = scm_true;
        }
    } else {
        port->codec = SCM_PORT_CODEC_NATIVE;
        port->eol_style = SCM_PORT_EOL_STYLE_NATIVE;
        port->error_handling_mode = SCM_PORT_ERROR_HANDLING_MODE_REPLACE;
    }
}

static void
init_port_tracking(scm_port_t port)
{
    port->mark = 0;
    port->line = 1;
    port->column = 1;
    port->track_line_column = (port->transcoder != scm_false);
    port->lookahead_size = 0;
    port->bom_le = false;
    port->bom_be = false;
}

void
port_open_std(scm_port_t port, fd_t fd, scm_obj_t name, int direction, int file_options, int buffer_mode, scm_obj_t transcoder)
{
    assert(PORTP(port));
    assert(STRINGP(name));

    port->lock.verify_locked();

    port->fd = fd;
    port->opened = true;
    port->type = SCM_PORT_TYPE_NAMED_FILE;
    port->subtype = io_fstat_mode(fd);
    port->handlers = scm_false;
    port->bytes = scm_false;
    port->name = name;
    port->direction = direction;
    port->transcoder = transcoder;
    port->buffer_mode = buffer_mode;
    port->file_options = file_options;
    port->force_sync = false;

    init_port_tracking(port);
    init_port_transcoder(port);
    init_port_buffer(port);
}

off64_t
std_port_position(fd_t fd)
{
    if (io_fstat_mode(fd) == SCM_PORT_SUBTYPE_NONE) {
        off64_t mark = io_lseek64(fd, 0, SEEK_CUR);
        if (mark < 0) throw_io_error(SCM_PORT_OPERATION_SEEK, errno);
        return mark;
    }
    return 0;
}

void
port_sync_port_position(scm_port_t port)
{
    if (port_has_set_port_position_pred(port)) {
        if (port->type == SCM_PORT_TYPE_NAMED_FILE && port->subtype == SCM_PORT_SUBTYPE_NONE) {
            if (io_lseek64(port->fd, port->mark, SEEK_CUR) >= 0) return;
            throw_io_error(SCM_PORT_OPERATION_SEEK, errno);
        }
    }
}

void
port_open_file(scm_port_t port, scm_obj_t name, int direction, int file_options, int buffer_mode, scm_obj_t transcoder)
{
    assert(PORTP(port));
    assert(STRINGP(name));

    const char* path = ((scm_string_t)name)->name;

    port->lock.verify_locked();

    port->fd = INVALID_FD;
    port->opened = false;
    port->type = SCM_PORT_TYPE_NAMED_FILE;
    port->subtype = io_stat_mode(path);
    port->handlers = scm_false;
    port->bytes = scm_false;
    port->name = name;
    port->direction = direction;
    port->transcoder = transcoder;
    port->buffer_mode = buffer_mode;
    port->file_options = file_options;
    port->force_sync = false;

    int options = 0;
    switch (port->subtype) {

        case SCM_PORT_SUBTYPE_NONE: {

            switch (port->direction) {
                case SCM_PORT_DIRECTION_IN:
                    options = O_RDONLY;
                    break;
                case SCM_PORT_DIRECTION_OUT:
                    options = O_CREAT | O_TRUNC | O_EXCL | O_WRONLY;
                    break;
                case SCM_PORT_DIRECTION_BOTH:
                    options = O_CREAT | O_TRUNC | O_EXCL | O_RDWR;
                    break;
            }

            if (SCM_PORT_FILE_OPTION_NO_CREATE & file_options) options &= (~O_CREAT);
            if (SCM_PORT_FILE_OPTION_NO_FAIL & file_options) options &= (~O_EXCL);
            if (SCM_PORT_FILE_OPTION_NO_TRUNCATE & file_options) options &= (~O_TRUNC);
        } break;

        case SCM_PORT_SUBTYPE_FIFO:
        case SCM_PORT_SUBTYPE_CHAR_SPECIAL: {

            switch (port->direction) {
                case SCM_PORT_DIRECTION_IN:
                    options = O_RDONLY;
                    break;
                case SCM_PORT_DIRECTION_OUT:
                    options = O_WRONLY;
                    break;
                case SCM_PORT_DIRECTION_BOTH:
                    options = O_RDWR;
                    break;
            }

        } break;

        default:
            fatal("%s:%u wrong port subtype", __FILE__, __LINE__);

    }
    port->fd = io_open(path, options, (S_IRUSR | S_IWUSR));
    if (port->fd == INVALID_FD) throw_io_error(SCM_PORT_OPERATION_OPEN, errno);

    init_port_tracking(port);
    init_port_transcoder(port);
    init_port_buffer(port);

    port->opened = true;
}

void
port_open_temp_file(scm_port_t port, scm_obj_t name, int buffer_mode, scm_obj_t transcoder)
{
    assert(PORTP(port));
    assert(STRINGP(name));

    port->lock.verify_locked();

    port->fd = INVALID_FD;
    port->opened = false;
    port->type = SCM_PORT_TYPE_NAMED_FILE;
    port->subtype = SCM_PORT_SUBTYPE_NONE;
    port->handlers = scm_false;
    port->bytes = scm_false;
    port->name = name;
    port->direction = SCM_PORT_DIRECTION_BOTH;
    port->transcoder = transcoder;
    port->buffer_mode = buffer_mode;
    port->file_options = SCM_PORT_FILE_OPTION_NONE;
    port->force_sync = false;

    char tmpl[256] = "/tmp/scm_temp_XXXXXX";
    port->fd = io_mkstemp(tmpl);
    if (port->fd == INVALID_FD) throw_io_error(SCM_PORT_OPERATION_OPEN, errno);

    init_port_tracking(port);
    init_port_transcoder(port);
    init_port_buffer(port);

    port->opened = true;
}

void
port_open_bytevector(scm_port_t port, scm_obj_t name, int direction, scm_obj_t bytes, scm_obj_t transcoder)
{
    assert(PORTP(port));
    assert(SYMBOLP(name));
    assert((bytes == scm_false) || BVECTORP(bytes));
    assert(direction != SCM_PORT_DIRECTION_BOTH);
    assert((direction & SCM_PORT_DIRECTION_OUT) || BVECTORP(bytes));

    port->lock.verify_locked();

    port->fd = INVALID_FD;
    port->opened = false;
    port->type = SCM_PORT_TYPE_BYTEVECTOR;
    port->subtype = SCM_PORT_SUBTYPE_NONE;
    port->handlers = scm_false;

    if (port->direction & SCM_PORT_DIRECTION_OUT) port->bytes = scm_false;
    else port->bytes = bytes;

    port->name = name;
    port->direction = direction;
    port->transcoder = transcoder;
    port->buffer_mode = SCM_PORT_BUFFER_MODE_NONE;
    port->file_options = SCM_PORT_FILE_OPTION_NONE;
    port->force_sync = false;

    init_port_tracking(port);
    init_port_transcoder(port);

    port->buf = NULL;
    port->buf_head = NULL;
    port->buf_tail = NULL;
    port->buf_size = 0;
    port->buf_state = (port->direction & SCM_PORT_DIRECTION_OUT) ? SCM_PORT_BUF_STATE_ACCUMULATE : SCM_PORT_BUF_STATE_UNSPECIFIED;

    port->opened = true;
}

void
port_make_socket_port(scm_port_t port, scm_socket_t socket, scm_obj_t transcoder)
{
    assert(PORTP(port));
    assert(SOCKETP(socket));

    port->lock.verify_locked();

    port->fd = INVALID_FD;
    port->opened = false;
    port->type = SCM_PORT_TYPE_SOCKET;
    port->subtype = SCM_PORT_SUBTYPE_NONE;
    port->handlers = scm_false;
    port->bytes = scm_false;

    port->name = socket;
    port->direction = SCM_PORT_DIRECTION_BOTH;
    port->transcoder = transcoder;
    port->buffer_mode = SCM_PORT_BUFFER_MODE_BLOCK;
    port->file_options = SCM_PORT_FILE_OPTION_NONE;
    port->force_sync = false;
    
    init_port_tracking(port);
    init_port_transcoder(port);
    init_port_buffer(port);

    port->opened = true;
}

void
port_make_custom_port(scm_port_t port, scm_obj_t name, int direction, scm_obj_t handlers, scm_obj_t transcoder)
{
    assert(PORTP(port));
    assert(STRINGP(name));
    assert(VECTORP(handlers));

    port->lock.verify_locked();

    port->fd = INVALID_FD;
    port->opened = false;
    port->type = SCM_PORT_TYPE_CUSTOM;
    port->subtype = SCM_PORT_SUBTYPE_NONE;
    port->handlers = handlers;
    port->bytes = make_bvector(current_vm()->m_heap, SCM_PORT_CUSTOM_BUFFER_SIZE);

    port->name = name;
    port->direction = direction;
    port->transcoder = transcoder;
    port->buffer_mode = SCM_PORT_BUFFER_MODE_BLOCK;
    port->file_options = SCM_PORT_FILE_OPTION_NONE;
    
#if CUSTOM_PORT_FORCE_SYNC
    port->force_sync = true;
#else
    port->force_sync = false;
#endif
    
    init_port_tracking(port);
    init_port_transcoder(port);
    init_port_buffer(port);

    port->opened = true;
}

void
port_make_transcoded_port(scm_obj_t name, scm_port_t binary, scm_port_t textual, scm_bvector_t transcoder)
{
    assert(BVECTORP(transcoder));

    binary->lock.verify_locked();
    textual->lock.verify_locked();
    textual->bytes = binary->bytes;
    memcpy(textual->lookahead, binary->lookahead, sizeof(textual->lookahead));
    textual->lookahead_size = binary->lookahead_size;
    textual->buf = binary->buf;
    textual->buf_head = binary->buf_head;
    textual->buf_tail = binary->buf_tail;
    textual->buf_size = binary->buf_size;
    textual->buf_state = binary->buf_state;
    textual->mark = binary->mark;
    textual->line = binary->line;
    textual->column = binary->column;
    textual->fd = binary->fd;
    textual->name = name;
    textual->transcoder = transcoder;
    textual->codec = transcoder->elts[0];
    textual->eol_style = transcoder->elts[1];
    textual->error_handling_mode = transcoder->elts[2];
    textual->file_options = binary->file_options;
    textual->buffer_mode = binary->buffer_mode;
    textual->type = binary->type;
    textual->subtype = binary->subtype;
    textual->handlers = binary->handlers;
    textual->direction = binary->direction;
    textual->track_line_column = binary->track_line_column;
    textual->opened = binary->opened;
    textual->force_sync = binary->force_sync;

    binary->fd = INVALID_FD;
    binary->opened = false;
    binary->buf = NULL;
    binary->buf_head = NULL;
    binary->buf_tail = NULL;
    binary->buf_size = 0;
    binary->buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
}

void
port_flush_output(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    if (port->opened) {
        if (no_output_buffered(port)) return;        
        int n = port->buf_tail - port->buf_head;
        device_write(port, port->buf_head, n, port->mark - n);
        port->buf_head = port->buf_tail = port->buf;
        port->buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
    }
}

void
port_close(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    if (port->opened) {
        if (port->buf) {
            port_flush_output(port);
            free(port->buf);
            port->buf = NULL;
            port->buf_head = NULL;
            port->buf_tail = NULL;
            port->buf_size = 0;
            port->buf_state = SCM_PORT_BUF_STATE_UNSPECIFIED;
        }
        device_close(port);
        port->opened = false;
    }
}

bool
port_nonblock_byte_ready(scm_port_t port)
{
#if _MSC_VER

    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->opened);
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {
        switch (port->type) {

        case SCM_PORT_TYPE_SOCKET: {
            if (no_input_buffered(port)) {
                scm_obj_t socket = port_socket(port);
                assert(SOCKETP(socket));
                int fd = ((scm_socket_t)socket)->fd;
                struct timeval tm = { 0, 0 };
                fd_set fds;
                FD_ZERO(&fds);
                FD_SET(fd, &fds);
                int state = select(fd + 1, &fds, NULL, NULL, &tm);
                if (state < 0) {
                     if (errno == EINTR) return false;
                     throw_io_error(SCM_PORT_OPERATION_SELECT, errno);
                }
                return (state != 0);
            }
        } break;
        
        case SCM_PORT_TYPE_NAMED_FILE: {

                switch (port->subtype) {

                    case SCM_PORT_SUBTYPE_FIFO: {
                        if (no_input_buffered(port)) {
                            DWORD bytes;
                            if (PeekNamedPipe(port->fd, NULL, 0, NULL, &bytes, NULL)) return (bytes != 0);
                            return false;
                        }
                    } break;

                    case SCM_PORT_SUBTYPE_CHAR_SPECIAL: {
                        if (no_input_buffered(port)) {
                            INPUT_RECORD inRec[32];
                            DWORD numRec;
                            if (PeekConsoleInput(port->fd, inRec, array_sizeof(inRec), &numRec)) {
                                for (int i = 0; i < numRec; i++) {
                                    if (inRec[i].EventType == KEY_EVENT) return true;
                                }
                            }
                            return false;
                        }
                    } break;

                    case SCM_PORT_SUBTYPE_NONE:
                        return false;

                    default:
                        fatal("%s:%u wrong port subtype", __FILE__, __LINE__);

                }

            } break;

            case SCM_PORT_TYPE_CUSTOM:
            case SCM_PORT_TYPE_BYTEVECTOR:
                break;

            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);

        }
    }

    return true;

#else

    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->opened);
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {
        switch (port->type) {

            case SCM_PORT_TYPE_SOCKET: {
                if (no_input_buffered(port)) {
                    scm_obj_t socket = port_socket(port);
                    assert(SOCKETP(socket));
                    int fd = ((scm_socket_t)socket)->fd;
                    struct timeval tm = { 0, 0 };
                    fd_set fds;
                    FD_ZERO(&fds);
                    FD_SET(fd, &fds);
                    int state = select(fd + 1, &fds, NULL, NULL, &tm);
                    if (state < 0) {
                         if (errno == EINTR) return false;
                         throw_io_error(SCM_PORT_OPERATION_SELECT, errno);
                     }
                    return (state != 0);
                }
            } break;
            
            case SCM_PORT_TYPE_NAMED_FILE: {

                switch (port->subtype) {

                    case SCM_PORT_SUBTYPE_FIFO:
                    case SCM_PORT_SUBTYPE_CHAR_SPECIAL: {
                        if (no_input_buffered(port)) {
                            struct timeval tm = { 0, 0 };
                            fd_set fds;
                            FD_ZERO(&fds);
                            FD_SET(port->fd, &fds);
                            int state = select(port->fd + 1, &fds, NULL, NULL, &tm);
                            if (state < 0) {
                                 if (errno == EINTR) return false;
                                 throw_io_error(SCM_PORT_OPERATION_SELECT, errno);
                             }
                            return (state != 0);
                        }
                    } break;

                    case SCM_PORT_SUBTYPE_NONE:
                        break;

                    default:
                        fatal("%s:%u wrong port subtype", __FILE__, __LINE__);

                }

            } break;

            case SCM_PORT_TYPE_CUSTOM:
            case SCM_PORT_TYPE_BYTEVECTOR:
                break;

            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);

        }
    }
    return true;

#endif

}

int
port_buffered_byte_count(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->opened);
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {
        switch (port->type) {

            case SCM_PORT_TYPE_NAMED_FILE: {
                if ((port->buf_state == SCM_PORT_BUF_STATE_READ)) return port->buf_tail - port->buf_head;
                return port->lookahead_size;
            } break;

            case SCM_PORT_TYPE_BYTEVECTOR: {
                scm_bvector_t bvector = (scm_bvector_t)port->bytes;
                assert(BVECTORP(bvector));
                return (bvector->count - port->mark);
            }

            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);

        }
    }
    return 0;
}

bool
port_eof(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {
        switch (port->type) {

            case SCM_PORT_TYPE_NAMED_FILE: {

                switch (port->subtype) {

                    case SCM_PORT_SUBTYPE_NONE: {
                        if (no_input_buffered(port)) {
                            off64_t size;
                            if (io_fstat_size(port->fd, &size) == 0) return (size <= port->mark);
                            throw_io_error(SCM_PORT_OPERATION_STAT, errno);
                        }
                    } break;

                    case SCM_PORT_SUBTYPE_FIFO: break;
                    case SCM_PORT_SUBTYPE_CHAR_SPECIAL: break;

                    default: fatal("%s:%u wrong port subtype", __FILE__, __LINE__);

                }

            } break;

            case SCM_PORT_TYPE_BYTEVECTOR: {
                scm_bvector_t bvec = ((scm_bvector_t)port->bytes);
                return (bvec->count <= port->mark);
            } break;

            case SCM_PORT_TYPE_CUSTOM: break;
            case SCM_PORT_TYPE_SOCKET: break;
            
            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);

        }
        return false;
    }
    return true;
}

int
port_lookahead_byte(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {

        switch (port->type) {

            case SCM_PORT_TYPE_BYTEVECTOR: {
                assert(port->buf == NULL);
                assert(port->lookahead_size == 0);
                scm_bvector_t bvec = ((scm_bvector_t)port->bytes);
                if (bvec->count <= port->mark) return EOF;
                return bvec->elts[port->mark];
            } break;

            case SCM_PORT_TYPE_CUSTOM:
            case SCM_PORT_TYPE_SOCKET:
            case SCM_PORT_TYPE_NAMED_FILE: {
                if (port->buf) {
                    assert(port->lookahead_size == 0);
                    if ((port->buf_state == SCM_PORT_BUF_STATE_READ) && (port->buf_head != port->buf_tail)) return *port->buf_head;
                    port_flush_output(port);
                    port->buf_state = SCM_PORT_BUF_STATE_READ;
                    port->buf_head = port->buf_tail = port->buf;
                    int n = device_read(port, port->buf, port->buf_size, port->mark);
                    if (n == 0) return EOF;
                    port->buf_tail = port->buf + n;
                    return *port->buf_head;
                } else {
                    assert(port->buf == NULL);
                    if (port->lookahead_size) {
                        return port->lookahead[0];
                    }
                    uint8_t b;
                    int n = device_read(port, &b, 1, port->mark);
                    if (n == 0) return EOF;
                    port->lookahead[0] = b;
                    port->lookahead_size = 1;
                    return b;
                }
            } break;

            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);
        }
    }
    return EOF;
}

static
void port_update_line_column(scm_port_t port, int c)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    if (c == '\n') {
        port->line++;
        port->column = 1;
        return;
    }
    port->column++;
}

int
port_get_byte(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {

        switch (port->type) {

            case SCM_PORT_TYPE_BYTEVECTOR: {
                assert(BVECTORP(port->bytes));
                assert(port->buf == NULL);
                assert(port->lookahead_size == 0);
                scm_bvector_t bvec = ((scm_bvector_t)port->bytes);
                if (bvec->count <= port->mark) return EOF;
                uint8_t b = bvec->elts[port->mark];
                if (port->track_line_column) port_update_line_column(port, b);
                port->mark++;
                return b;
            } break;

            case SCM_PORT_TYPE_CUSTOM:
            case SCM_PORT_TYPE_SOCKET:
            case SCM_PORT_TYPE_NAMED_FILE: {
                if (port->buf) {
                    assert(port->lookahead_size == 0);
                    if (port->buf_state != SCM_PORT_BUF_STATE_READ || port->buf_head == port->buf_tail) {
                        port_flush_output(port);
                        port->buf_state = SCM_PORT_BUF_STATE_READ;
                        port->buf_head = port->buf_tail = port->buf;
                        int n = device_read(port, port->buf, port->buf_size, port->mark);
                        if (n == 0) return EOF;
                        port->buf_tail = port->buf + n;
                    }
                    uint8_t b = *port->buf_head++;
                    if (port->track_line_column) port_update_line_column(port, b);
                    port->mark++;
                    return b;
                } else {
                    assert(port->buf == NULL);
                    uint8_t b;
                    if (port->lookahead_size) {
                        assert(port->buf == NULL);
                        b = port->lookahead[0];
                        port->lookahead[0] = port->lookahead[1];
                        port->lookahead[1] = port->lookahead[2];
                        port->lookahead[2] = port->lookahead[3];
                        port->lookahead_size--;
                    } else {
                        int n = device_read(port, &b, 1, port->mark);
                        if (n == 0) return EOF;
                    }
                    if (port->track_line_column) port_update_line_column(port, b);
                    port->mark++;
                    return b;
                }
            } break;

            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);

        }
    }
    return EOF;
}

scm_obj_t
port_lookahead_utf8(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {

    top:
        uint8_t utf8[4];
        int b = port_lookahead_byte(port);
        if (b == EOF) return scm_eof;
        if (b > 127) {
            int code_length = utf8_byte_count(b);
            utf8[0] = b;
            if (code_length > 1) {
                switch (port->type) {

                    case SCM_PORT_TYPE_BYTEVECTOR: {
                        assert(port->buf == NULL);
                        scm_bvector_t bvec = ((scm_bvector_t)port->bytes);
                        if (bvec->count - port->mark < code_length) goto hit_eof;
                        memcpy(utf8, bvec->elts + port->mark, code_length);
                    } break;

                    case SCM_PORT_TYPE_NAMED_FILE: {
                        if (port->buf) {
                            assert(port->lookahead_size == 0);
                            assert(port->buf_state == SCM_PORT_BUF_STATE_READ);
                            assert(port->buf_size >= PORT_LOOKAHEAD_SIZE);
                            int n1 = port->buf_tail - port->buf_head;
                            while (n1 < code_length) {
                                if (port->buf != port->buf_head) {
                                    memmove(port->buf, port->buf_head, n1);
                                    port->buf_head = port->buf;
                                }
                                int n2 = device_read(port, port->buf + n1, port->buf_size - n1, port->mark + n1);
                                n1 = n1 + n2;
                                port->buf_tail = port->buf_head + n1;
                                if (n2 == 0) goto hit_eof;
                            }
                            memcpy(utf8, port->buf_head, code_length);
                        } else {
                            assert(port->buf == NULL);
                            while (port->lookahead_size < code_length) {
                                int n = device_read(port, port->lookahead + port->lookahead_size, code_length - port->lookahead_size, port->mark + port->lookahead_size);
                                port->lookahead_size += n;
                                if (n == 0) goto hit_eof;
                            }
                            memcpy(utf8, port->lookahead, code_length);
                        }
                    } break;

                    default:
                        fatal("%s:%u wrong port type", __FILE__, __LINE__);

                }
            }

            uint32_t ucs4;
            if (cnvt_utf8_to_ucs4(utf8, &ucs4) < 1) {
                switch (port->error_handling_mode) {
                    case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
                        for (int i = 0; i < code_length; i++) port_get_byte(port);
                        goto top;
                    case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
                        if (port->lookahead_size) {
                            assert(port->buf == NULL);
                            port->lookahead_size = cnvt_ucs4_to_utf8(SCM_PORT_UCS4_REPLACEMENT_CHAR, port->lookahead);
                        }
                        ucs4 = SCM_PORT_UCS4_REPLACEMENT_CHAR;
                        break;
                    case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
                        throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-8 sequence", scm_false);
                    default:
                        fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
                }
            }
            if (port->mark == 0 && ucs4 == SCM_PORT_UCS4_BOM) {
                port_get_byte(port); port_get_byte(port); port_get_byte(port);
                goto top;
            }
            return MAKECHAR(ucs4);
        }
        return MAKECHAR(b);
    }
    return scm_eof;

hit_eof:
    switch (port->error_handling_mode) {
        case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
            return scm_eof;
        case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
            if (port->lookahead_size) {
                assert(port->buf == NULL);
                port->lookahead_size = cnvt_ucs4_to_utf8(SCM_PORT_UCS4_REPLACEMENT_CHAR, port->lookahead);
            }
            return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
        case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-8 sequence", scm_false);
        default:
            fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
    }
}

scm_obj_t
port_get_utf8(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {

    top:
        uint8_t utf8[4];
        int b = port_get_byte(port);
        if (b == EOF) return scm_eof;
        if (b > 127) {
            int code_length = utf8_byte_count(b);
            utf8[0] = b;
            for (int i = 1; i < code_length; i++) {
                int b = port_get_byte(port);
                if (b == EOF) {
                    switch (port->error_handling_mode) {
                        case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
                            return scm_eof;
                        case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
                            return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
                        case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
                            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-8 sequence", scm_false);
                        default:
                            fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
                    }
                }
                utf8[i] = b;
            }
            uint32_t ucs4;
            if (cnvt_utf8_to_ucs4(utf8, &ucs4) < 1) {
                switch (port->error_handling_mode) {
                    case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
                        goto top;
                    case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
                        ucs4 = SCM_PORT_UCS4_REPLACEMENT_CHAR;
                        break;
                    case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
                        throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-8 sequence", scm_false);
                    default:
                        fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
                }
            }
            if (port->mark == 3 && ucs4 == SCM_PORT_UCS4_BOM) goto top;
            return MAKECHAR(ucs4);
        }
        return MAKECHAR(b);
    }
    return scm_eof;
}

static bool
bad_leading_utf16(uint32_t ucs2)
{
    return (ucs2 == 0xFFFE) || (ucs2 == 0xFFFF) || ((ucs2 >= 0xDC00) & (ucs2 <= 0xDFFF));
}

static bool
bad_secondary_utf16(uint32_t ucs2)
{
    return !((ucs2 >= 0xDC00) & (ucs2 <= 0xDFFF));
}

static bool
bad_ucs4(uint32_t ucs4)
{
    return (ucs4 > 0x10FFFF) || (ucs4 == 0xFFFE) || (ucs4 == 0xFFFF) || ((ucs4 >= 0xD800) & (ucs4 <= 0xDFFF));
}

static bool
decode_surrogate_pair(uint32_t* ucs4, uint32_t right)
{
    if (bad_secondary_utf16(right)) return true;
    uint32_t n = ((*ucs4 - 0xD800) << 10) + (right - 0xDC00) + 0x10000;
    if (bad_ucs4(n)) return true;
    *ucs4 = n;
    return false;
}

static void
encode_surrogate_pair(uint32_t ucs4, uint32_t* left, uint32_t* right)
{
    uint32_t n = ucs4 - 0x10000;
    *left = (n >> 10) + 0xD800;
    *right = (n & 0x3FF) + 0xDC00;
}

static bool
test_bom(scm_port_t port, const uint8_t utf16[2])
{
    if (utf16[0] == 0xFE && utf16[1] == 0xFF) {
        port->bom_be = true;
        return true;
    } else if (utf16[0] == 0xFF && utf16[1] == 0xFE) {
        port->bom_le = true;
        return true;
    }
    return false;
}

static bool
test_bom(scm_port_t port, const int utf16[2])
{
    if (utf16[0] == 0xFE && utf16[1] == 0xFF) {
        port->bom_be = true;
        return true;
    } else if (utf16[0] == 0xFF && utf16[1] == 0xFE) {
        port->bom_le = true;
        return true;
    }
    return false;
}

static uint32_t
cnvt_utf16_to_ucs4_using_bom(scm_port_t port, const uint8_t utf16[2])
{
    if (port->bom_le) return ((uint32_t)utf16[1] << 8) + utf16[0];
    return ((uint32_t)utf16[0] << 8) + utf16[1];
}

static uint32_t
cnvt_utf16_to_ucs4_using_bom(scm_port_t port, const int utf16[2])
{
    if (port->bom_le) return (uint32_t)((utf16[1] << 8) + utf16[0]);
    return (uint32_t)((utf16[0] << 8) + utf16[1]);
}

scm_obj_t
port_lookahead_utf16(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {

    top:
        uint32_t ucs4;
        switch (port->type) {

            case SCM_PORT_TYPE_BYTEVECTOR: {
                assert(port->buf == NULL);
                scm_bvector_t bvec = ((scm_bvector_t)port->bytes);
                if (bvec->count - port->mark < 2) goto hit_eof;
                const uint8_t* utf16 = bvec->elts + port->mark;
                if (port->mark == 0 && test_bom(port, utf16)) goto hit_bom;
                ucs4 = cnvt_utf16_to_ucs4_using_bom(port, utf16);
                if (bad_leading_utf16(ucs4)) goto bad_code;
                if ((ucs4 >= 0xD800) & (ucs4 <= 0xDBFF)) {
                    if (bvec->count - port->mark < 4) goto hit_eof;
                    utf16 = bvec->elts + port->mark + 2;
                    uint32_t right = cnvt_utf16_to_ucs4_using_bom(port, utf16);
                    if ((right < 0xDC00) | (right > 0xDFFF)) goto bad_code;
                    if (decode_surrogate_pair(&ucs4, right)) goto bad_code;
                }
                return MAKECHAR(ucs4);
            } break;

            case SCM_PORT_TYPE_NAMED_FILE: {
                if (port->buf) {
                    assert(port->lookahead_size == 0);
                    assert(port->buf_state == SCM_PORT_BUF_STATE_READ);
                    assert(port->buf_size >= PORT_LOOKAHEAD_SIZE);
                    int n1 = port->buf_tail - port->buf_head;
                    while (n1 < 4) {
                        if (port->buf != port->buf_head) {
                            memmove(port->buf, port->buf_head, n1);
                            port->buf_head = port->buf;
                        }
                        int n2 = device_read(port, port->buf + n1, port->buf_size - n1, port->mark + n1);
                        n1 = n1 + n2;
                        port->buf_tail = port->buf_head + n1;
                        if (n2 == 0) return scm_eof;
                    }
                    const uint8_t* utf16 = port->buf_head;
                    if (port->mark == 0 && test_bom(port, utf16)) goto hit_bom;
                    ucs4 = cnvt_utf16_to_ucs4_using_bom(port, utf16);
                    if (bad_leading_utf16(ucs4)) goto bad_code;
                    if ((ucs4 >= 0xD800) & (ucs4 <= 0xDBFF)) {
                        if (port->buf_tail - port->buf_head < 4) goto hit_eof;
                        utf16 = port->buf_head + 2;
                        uint32_t right = cnvt_utf16_to_ucs4_using_bom(port, utf16);
                        if ((right < 0xDC00) | (right > 0xDFFF)) goto bad_code;
                        if (decode_surrogate_pair(&ucs4, right)) goto bad_code;
                        return MAKECHAR(ucs4);
                    }
                    return MAKECHAR(ucs4);
                } else {
                    assert(port->buf == NULL);
                    while (port->lookahead_size < 2) {
                        int n = device_read(port, port->lookahead + port->lookahead_size, 2 - port->lookahead_size, port->mark + port->lookahead_size);
                        port->lookahead_size += n;
                        if (n == 0) return scm_eof;
                    }
                    const uint8_t* utf16 = port->lookahead;
                    if (port->mark == 0 && test_bom(port, utf16)) goto hit_bom;
                    ucs4 = cnvt_utf16_to_ucs4_using_bom(port, utf16);
                    if (bad_leading_utf16(ucs4)) goto bad_code;
                    if ((ucs4 >= 0xD800) & (ucs4 <= 0xDBFF)) {
                        while (port->lookahead_size < 4) {
                            int n = device_read(port, port->lookahead + port->lookahead_size, 4 - port->lookahead_size, port->mark + port->lookahead_size);
                            port->lookahead_size += n;
                            if (n == 0) goto hit_eof;
                        }
                        utf16 = port->lookahead + 2;
                        uint32_t right = cnvt_utf16_to_ucs4_using_bom(port, utf16);
                        if ((right < 0xDC00) | (right > 0xDFFF)) goto bad_code;
                        if (decode_surrogate_pair(&ucs4, right)) goto bad_code;
                        return MAKECHAR(ucs4);
                    }
                    return MAKECHAR(ucs4);

                }
            } break;

            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);

        }
    }
    return scm_eof;

hit_bom:
    port_get_byte(port);
    port_get_byte(port);
    goto top;

bad_code:
    switch (port->error_handling_mode) {
        case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
            port_get_byte(port);
            port_get_byte(port);
            goto top;
        case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
            goto return_replacement;
        case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-16 sequence", scm_false);
        default:
            fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
    }

hit_eof:
    switch (port->error_handling_mode) {
        case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
            return scm_eof;
        case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
            goto return_replacement;
        case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-16 sequence", scm_false);
        default:
            fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
    }

return_replacement:
    if (port->lookahead_size) {
        assert(port->buf == NULL);
        if (port->bom_le) {
            port->lookahead[0] = SCM_PORT_UCS4_REPLACEMENT_CHAR & 0xFF;
            port->lookahead[1] = SCM_PORT_UCS4_REPLACEMENT_CHAR >> 8;
        } else {
            port->lookahead[0] = SCM_PORT_UCS4_REPLACEMENT_CHAR >> 8;
            port->lookahead[1] = SCM_PORT_UCS4_REPLACEMENT_CHAR & 0xFF;
        }
        port->lookahead_size = 2;
    }
    return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
}

scm_obj_t
port_get_utf16(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_IN);
    if (port->opened) {

    top:
        int utf16[2];
        if ((utf16[0] = port_get_byte(port)) == EOF) return scm_eof;
        if ((utf16[1] = port_get_byte(port)) == EOF) goto hit_eof;
        if (port->mark == 2 && test_bom(port, utf16)) goto top;
        uint32_t ucs4 = cnvt_utf16_to_ucs4_using_bom(port, utf16);
        if (bad_leading_utf16(ucs4)) goto bad_code;
        if ((ucs4 >= 0xD800) & (ucs4 <= 0xDBFF)) {
            if ((utf16[0] = port_get_byte(port)) == EOF) goto hit_eof;
            if ((utf16[1] = port_get_byte(port)) == EOF) goto hit_eof;
            uint32_t right = cnvt_utf16_to_ucs4_using_bom(port, utf16);
            if ((right < 0xDC00) | (right > 0xDFFF)) goto bad_code;
            if (decode_surrogate_pair(&ucs4, right)) goto bad_code;
        }
        return MAKECHAR(ucs4);
    }
    return scm_eof;

bad_code:
    switch (port->error_handling_mode) {
        case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
            goto top;
        case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
            return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
        case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-16 sequence", scm_false);
        default:
            fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
    }

hit_eof:
    switch (port->error_handling_mode) {
        case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
            return scm_eof;
        case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
            return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
        case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-16 sequence", scm_false);
        default:
            fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
    }
}

#if _MSC_VER

    scm_obj_t
    port_lookahead_cp932(scm_port_t port)
    {
        assert(PORTP(port));
        port->lock.verify_locked();
        assert(port->direction & SCM_PORT_DIRECTION_IN);
        uint8_t cp932[2];

        if (port->opened) {
        top:
            int b = port_lookahead_byte(port);
            if (b == EOF) return scm_eof;
            if (b > 127) {
                int code_length = 1;
                cp932[0] = b;
                if (((b >= 0x81) && (b <= 0x9f)) || ((b >= 0xe0) && (b <= 0xfc))) code_length = 2;
                if (code_length > 1) {
                    switch (port->type) {

                        case SCM_PORT_TYPE_BYTEVECTOR: {
                            assert(port->buf == NULL);
                            scm_bvector_t bvec = ((scm_bvector_t)port->bytes);
                            if (bvec->count - port->mark < code_length) goto hit_eof;
                            memcpy(cp932, bvec->elts + port->mark, code_length);
                        } break;

                        case SCM_PORT_TYPE_NAMED_FILE: {
                            if (port->buf) {
                                assert(port->lookahead_size == 0);
                                assert(port->buf_state == SCM_PORT_BUF_STATE_READ);
                                assert(port->buf_size >= PORT_LOOKAHEAD_SIZE);
                                int n1 = port->buf_tail - port->buf_head;
                                while (n1 < code_length) {
                                    if (port->buf != port->buf_head) {
                                        memmove(port->buf, port->buf_head, n1);
                                        port->buf_head = port->buf;
                                    }
                                    int n2 = device_read(port, port->buf + n1, port->buf_size - n1, port->mark + n1);
                                    n1 = n1 + n2;
                                    port->buf_tail = port->buf_head + n1;
                                    if (n2 == 0) goto hit_eof;
                                }
                                memcpy(cp932, port->buf_head, code_length);
                            } else {
                                assert(port->buf == NULL);
                                while (port->lookahead_size < code_length) {
                                    int n = device_read(port, port->lookahead + port->lookahead_size, code_length - port->lookahead_size, port->mark + port->lookahead_size);
                                    port->lookahead_size += n;
                                    if (n == 0) goto hit_eof;
                                }
                                memcpy(cp932, port->lookahead, code_length);
                            }
                        } break;

                        default:
                            fatal("%s:%u wrong port type", __FILE__, __LINE__);

                    }
                }
                wchar_t ucs2[2];
                if (!MultiByteToWideChar(932, MB_ERR_INVALID_CHARS, (LPCSTR)cp932, code_length, ucs2, array_sizeof(ucs2))) {
                    switch (port->error_handling_mode) {
                        case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
                            for (int i = 0; i < code_length; i++) port_get_byte(port);
                            goto top;
                        case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
                            return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
                            break;
                        case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
                            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid cp932 sequence", scm_false);
                        default:
                            fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
                    }
                }
                return MAKECHAR(ucs2[0]);
            }
            return MAKECHAR(b);
        }
        return scm_eof;

    hit_eof:
        switch (port->error_handling_mode) {
            case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
                return scm_eof;
            case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
                if (port->lookahead_size) {
                    assert(port->buf == NULL);
                    port->lookahead[0] = cp932[0];
                    port->lookahead[1] = cp932[1];
                    port->lookahead_size = 2;
                }
                return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
            case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
                throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid cp932 sequence", scm_false);
            default:
                fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
        }
    }

    scm_obj_t
    port_get_cp932(scm_port_t port)
    {
        assert(PORTP(port));
        port->lock.verify_locked();
        assert(port->direction & SCM_PORT_DIRECTION_IN);
        uint8_t cp932[2];

        if (port->opened) {

        top:
            int b = port_get_byte(port);
            if (b == EOF) return scm_eof;
            if (b > 127) {
                int code_length = 1;
                cp932[0] = b;
                if (((b >= 0x81) && (b <= 0x9f)) || ((b >= 0xe0) && (b <= 0xfc))) code_length = 2;
                for (int i = 1; i < code_length; i++) {
                    int b = port_get_byte(port);
                    if (b == EOF) {
                        switch (port->error_handling_mode) {
                            case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
                                return scm_eof;
                            case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
                                return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
                            case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
                                throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid cp932 sequence", scm_false);
                            default:
                                fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
                        }
                    }
                    cp932[i] = b;
                }
                wchar_t ucs2[2];
                if (!MultiByteToWideChar(932, MB_ERR_INVALID_CHARS, (LPCSTR)cp932, code_length, ucs2, array_sizeof(ucs2))) {
                    switch (port->error_handling_mode) {
                        case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
                            goto top;
                        case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
                            return MAKECHAR(SCM_PORT_UCS4_REPLACEMENT_CHAR);
                            break;
                        case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
                            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid cp932 sequence", scm_false);
                        default:
                            fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
                    }
                }
                return MAKECHAR(ucs2[0]);
            }
            return MAKECHAR(b);
        }
        return scm_eof;
    }

    static void
    port_put_cp932(scm_port_t port, int32_t ucs4)
    {
        assert(PORTP(port));
        wchar_t ucs2[1];
        ucs2[0] = ucs4; // TODO: sarrogates
        uint8_t buf[4];
        int n = WideCharToMultiByte(932, 0, ucs2, 1, (LPSTR)buf, 4, NULL, NULL);
        for (int i = 0; i < n; i++) port_put_byte(port, buf[i]);
    }

#endif // _MSC_VER

void
port_set_mark(scm_port_t port, off64_t offset)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    if (port->opened) {
        port_flush_output(port);
        port->lookahead_size = 0;
        switch (port->type) {

            case SCM_PORT_TYPE_BYTEVECTOR: {
                assert((port->buf_state == SCM_PORT_BUF_STATE_ACCUMULATE) || (port->buf_state == SCM_PORT_BUF_STATE_UNSPECIFIED));
                port->mark = offset;
            } break;

            case SCM_PORT_TYPE_CUSTOM:
            case SCM_PORT_TYPE_NAMED_FILE: {
                device_set_mark(port, offset);
            } break;

            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);

        }
        port->track_line_column = false;
        port->column = 0;
        port->line = 0;
    }
}

#define HUNDRED_TWENTY_FIVE_PERCENT_OF(n) ((n) + ((n) >> 2))

void
port_put_byte(scm_port_t port, int byte)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_OUT);
    if (port->opened) {
        port->lookahead_size = 0;
        if (port->track_line_column) port_update_line_column(port, byte);
        switch (port->type) {

            case SCM_PORT_TYPE_BYTEVECTOR: {
                assert(port->buf_state == SCM_PORT_BUF_STATE_ACCUMULATE);
                if (port->mark >= port->buf_size) {
                    size_t newsize = port->mark + SCM_PORT_BYTEVECTOR_OUTPUT_CHUNK;
                    size_t cursize = port->buf_tail - port->buf_head;
                    if (newsize < HUNDRED_TWENTY_FIVE_PERCENT_OF(cursize)) newsize = HUNDRED_TWENTY_FIVE_PERCENT_OF(cursize);
                    if (newsize < port->mark + SCM_PORT_BYTEVECTOR_OUTPUT_CHUNK) {
                        throw_io_error(SCM_PORT_OPERATION_WRITE, "port bytevector too large, memory allocation failed");
                    }
                    uint8_t* prev = port->buf;
                    port->buf = (uint8_t*)realloc(port->buf, newsize);
                    if (port->buf == NULL) {
                        port->buf = prev;
                        throw_io_error(SCM_PORT_OPERATION_WRITE, "port bytevector too large, memory allocation failed");
                    }
                    memset(port->buf + cursize, 0, newsize - cursize);
                    port->buf_head = port->buf;
                    port->buf_tail = port->buf_head + cursize;
                    port->buf_size = newsize;
                }
                uint8_t* p = port->buf_head + port->mark;
                *p = byte;
                port->mark++;
                if (++p > port->buf_tail) port->buf_tail = p;
            } break;

            case SCM_PORT_TYPE_CUSTOM:
            case SCM_PORT_TYPE_SOCKET:
            case SCM_PORT_TYPE_NAMED_FILE: {
                if (port->buf) {
                    assert(port->buf_state != SCM_PORT_BUF_STATE_ACCUMULATE);
                    if (port->buf_state != SCM_PORT_BUF_STATE_WRITE) {
                        port->buf_state = SCM_PORT_BUF_STATE_WRITE;
                        port->buf_head = port->buf_tail = port->buf;
                    } else {
                        if (port->buf_tail == port->buf + port->buf_size) {
                            port_flush_output(port);
                            port->buf_state = SCM_PORT_BUF_STATE_WRITE;
                        }
                    }
                    *(port->buf_tail++) = byte;
                    port->mark++;
                    if (port->buffer_mode == SCM_PORT_BUFFER_MODE_LINE) {
                        if (byte == '\n') port_flush_output(port);
                    }
                } else {
                    uint8_t b = byte;
                    device_write(port, &b, 1, port->mark);
                    port->mark++;
                }
                return;
            } break;

            default:
                fatal("%s:%u wrong port type", __FILE__, __LINE__);

        }
    }
}

static void
port_put_utf8(scm_port_t port, int32_t ucs4)
{
    assert(PORTP(port));
    uint8_t utf8[4];
    int n = cnvt_ucs4_to_utf8(ucs4, utf8);
    for (int i = 0; i < n; i++) port_put_byte(port, utf8[i]);
}

static void
port_put_utf16(scm_port_t port, int32_t ucs4)
{
    assert(PORTP(port));
    if (ucs4 >= 0x10000) {
        uint32_t left;
        uint32_t right;
        encode_surrogate_pair(ucs4, &left, &right);
        if (port->mark == 0 && port->bom_be == false && port->bom_le == false) {
            port->bom_be = true;
            port_put_byte(port, 0xFE);
            port_put_byte(port, 0xFF);
        }
        port_put_byte(port, left >> 8);
        port_put_byte(port, left & 0xFF);
        port_put_byte(port, right >> 8);
        port_put_byte(port, right & 0xFF);
        return;
    }
    if (port->mark == 0 && port->bom_be == false && port->bom_le == false) {
        port->bom_be = true;
        port_put_byte(port, 0xFE);
        port_put_byte(port, 0xFF);
    }
    port_put_byte(port, ucs4 >> 8);
    port_put_byte(port, ucs4 & 0xFF);
}

bool
port_input_pred(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    return (port->direction & SCM_PORT_DIRECTION_IN) != 0;
}

bool
port_output_pred(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    return (port->direction & SCM_PORT_DIRECTION_OUT) != 0;
}

bool
port_textual_pred(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    return port->transcoder != scm_false;
}

bool
port_binary_pred(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    return port->transcoder == scm_false;
}

bool
port_open_pred(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    return port->opened;
}

bool
port_bytevector_pred(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    return port->type == SCM_PORT_TYPE_BYTEVECTOR;
}

bool
port_regular_file_pred(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    return (port->type == SCM_PORT_TYPE_NAMED_FILE) && (port->subtype == SCM_PORT_SUBTYPE_NONE);
}

scm_bvector_t
port_extract_bytevector(object_heap_t* heap, scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_OUT);
    assert(port->type == SCM_PORT_TYPE_BYTEVECTOR);
    assert(port->buf_state == SCM_PORT_BUF_STATE_ACCUMULATE);
    int n = port->buf_tail - port->buf_head;
    scm_bvector_t bvector;
    if (port->buf == NULL) {
        bvector = make_bvector(heap, 0);
    } else {
        bvector = make_bvector(heap, n);
        memcpy(bvector->elts, port->buf_head, n);
    }
    free(port->buf);
    port->buf = NULL;
    port->buf_head = NULL;
    port->buf_tail = NULL;
    port->buf_size = 0;
    port->mark = 0;
    return bvector;
}

scm_string_t
port_extract_string(object_heap_t* heap, scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_OUT);
    assert(port->type == SCM_PORT_TYPE_BYTEVECTOR);
    assert(port->buf_state == SCM_PORT_BUF_STATE_ACCUMULATE);
    scm_string_t string;
    if (port->buf == NULL) string = make_string(heap, "");
    else string = make_string(heap, (const char*)port->buf_head, port->buf_tail - port->buf_head);
    free(port->buf);
    port->buf = NULL;
    port->buf_head = NULL;
    port->buf_tail = NULL;
    port->buf_size = 0;
    port->mark = 0;
    return string;
}

scm_string_t
port_get_string(object_heap_t* heap, scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    assert(port->direction & SCM_PORT_DIRECTION_OUT);
    assert(port->type == SCM_PORT_TYPE_BYTEVECTOR);
    assert(port->buf_state == SCM_PORT_BUF_STATE_ACCUMULATE);
    scm_string_t string;
    if (port->buf == NULL) string = make_string(heap, "");
    else string = make_string(heap, (const char*)port->buf_head, port->buf_tail - port->buf_head);
    return string;
}

static scm_obj_t
port_lookahead_ch(scm_port_t port)
{
    switch (port->codec) {
        case SCM_PORT_CODEC_UTF8:
            return port_lookahead_utf8(port);
        case SCM_PORT_CODEC_UTF16:
            return port_lookahead_utf16(port);
#if _MSC_VER
        case SCM_PORT_CODEC_CP932:
            return port_lookahead_cp932(port);
#endif
        case SCM_PORT_CODEC_LATIN1:
            int c = port_lookahead_byte(port);
            if (c == EOF) return scm_eof;
            return MAKECHAR(c);
    }
    fatal("%s:%u wrong codec", __FILE__, __LINE__);
}

scm_obj_t
port_lookahead_char(scm_port_t port)
{
    scm_obj_t ch = port_lookahead_ch(port);
    if (ch == scm_eof) return ch;
    if (port->transcoder == scm_false) return ch;
    if (port->eol_style == SCM_PORT_EOL_STYLE_NONE) return ch;
    switch (CHAR(ch)) {
        case SCM_PORT_UCS4_CR:
        case SCM_PORT_UCS4_NEL:
        case SCM_PORT_UCS4_LS:
            return SCM_CHAR_LF;
    }
    return ch;
}

static scm_obj_t
port_get_ch(scm_port_t port)
{
    switch (port->codec) {
        case SCM_PORT_CODEC_UTF8:
            return port_get_utf8(port);
        case SCM_PORT_CODEC_UTF16:
            return port_get_utf16(port);
#if _MSC_VER
        case SCM_PORT_CODEC_CP932:
            return port_get_cp932(port);
#endif
        case SCM_PORT_CODEC_LATIN1:
            int c = port_get_byte(port);
            if (c == EOF) return scm_eof;
            return MAKECHAR(c);
    }
    fatal("%s:%u wrong codec", __FILE__, __LINE__);
}

scm_obj_t
port_get_char(scm_port_t port)
{
    scm_char_t ch = port_get_ch(port);
    if (ch == scm_eof) return ch;
    if (port->transcoder == scm_false) return ch;
    if (port->eol_style == SCM_PORT_EOL_STYLE_NONE) return ch;
    switch (CHAR(ch)) {
        case SCM_PORT_UCS4_NEL:
        case SCM_PORT_UCS4_LS:
            return SCM_CHAR_LF;
        case SCM_PORT_UCS4_CR:
            scm_obj_t post = port_lookahead_ch(port);
            switch (CHAR(post)) {
                case SCM_PORT_UCS4_LF:
                case SCM_PORT_UCS4_NEL:
                    port_get_ch(port);
                    return SCM_CHAR_LF;
            }
            return SCM_CHAR_LF;
    }
    return ch;
}

scm_obj_t
port_lookahead_u8(scm_port_t port)
{
    int c = port_lookahead_byte(port);
    if (c == EOF) return scm_eof;
    return MAKEFIXNUM(c);
}

scm_obj_t
port_get_u8(scm_port_t port)
{
    int c = port_get_byte(port);
    if (c == EOF) return scm_eof;
    return MAKEFIXNUM(c);
}

bool
port_has_port_position_pred(scm_port_t port)
{
    switch (port->type) {
        case SCM_PORT_TYPE_BYTEVECTOR:
            return true;
        case SCM_PORT_TYPE_SOCKET:
            return false;
        case SCM_PORT_TYPE_CUSTOM: {
            assert(VECTORP(port->handlers));
            scm_vector_t vect = (scm_vector_t)port->handlers;
            return (CLOSUREP(vect->elts[SCM_PORT_HANDLER_GET_POS]));
        }
        case SCM_PORT_TYPE_NAMED_FILE:
            switch (port->subtype) {
                case SCM_PORT_SUBTYPE_NONE: return true;
                case SCM_PORT_SUBTYPE_FIFO: return false;
                case SCM_PORT_SUBTYPE_CHAR_SPECIAL: return false;
                default: fatal("%s:%u wrong port subtype", __FILE__, __LINE__);
            }
        default:
            fatal("%s:%u wrong port type", __FILE__, __LINE__);
    }
}

bool
port_has_set_port_position_pred(scm_port_t port)
{
    switch (port->type) {
        case SCM_PORT_TYPE_BYTEVECTOR:
            return true;
        case SCM_PORT_TYPE_SOCKET:
            return false;
        case SCM_PORT_TYPE_CUSTOM: {
            assert(VECTORP(port->handlers));
            scm_vector_t vect = (scm_vector_t)port->handlers;
            return (CLOSUREP(vect->elts[SCM_PORT_HANDLER_SET_POS]));
        }
        case SCM_PORT_TYPE_NAMED_FILE:
            switch (port->subtype) {
                case SCM_PORT_SUBTYPE_NONE: return true;
                case SCM_PORT_SUBTYPE_FIFO: return false;
                case SCM_PORT_SUBTYPE_CHAR_SPECIAL: return false;
                default:
                    fatal("%s:%u wrong port subtype", __FILE__, __LINE__);
            }
        default:
            fatal("%s:%u wrong port type", __FILE__, __LINE__);
    }
}

off64_t
port_position(scm_port_t port)
{
    if (port->type == SCM_PORT_TYPE_CUSTOM) {
        assert(VECTORP(port->handlers));
        scm_vector_t vect = (scm_vector_t)port->handlers;
        if (vect->elts[SCM_PORT_HANDLER_TEXTUAL] == scm_true) {
            VM* vm = current_vm();
            scm_obj_t token = int64_to_integer(vm->m_heap, port->mark);
            vm->call_scheme(vect->elts[SCM_PORT_HANDLER_GET_POS], 1, token);
        }
    }
    return port->mark;
}

void
port_set_port_position(scm_port_t port, off64_t off)
{
    port_set_mark(port, off);
}

int
port_output_buffer_mode(scm_port_t port)
{
    return port->buffer_mode;
}

static void
port_putc(scm_port_t port, int32_t ucs4)
{
    switch (port->codec) {
        case SCM_PORT_CODEC_UTF8: {
            if (ucs4 == SCM_PORT_UCS4_LF) {
                if (port->transcoder != scm_false) {
                    switch (port->eol_style) {
                        case SCM_PORT_EOL_STYLE_CR:
                            port_put_utf8(port, SCM_PORT_UCS4_CR);
                            return;
                        case SCM_PORT_EOL_STYLE_CRLF:
                            port_put_utf8(port, SCM_PORT_UCS4_CR);
                            port_put_utf8(port, SCM_PORT_UCS4_LF);
                            return;
                        case SCM_PORT_EOL_STYLE_NEL:
                            port_put_utf8(port, SCM_PORT_UCS4_NEL);
                            return;
                        case SCM_PORT_EOL_STYLE_CRNEL:
                            port_put_utf8(port, SCM_PORT_UCS4_CR);
                            port_put_utf8(port, SCM_PORT_UCS4_NEL);
                            return;
                        case SCM_PORT_EOL_STYLE_LS:
                            port_put_utf8(port, SCM_PORT_UCS4_LS);
                            return;
                        default:
                            break;
                    }
                }
            }
            port_put_utf8(port, ucs4);
            return;
        }
        case SCM_PORT_CODEC_UTF16: {
            if (ucs4 == SCM_PORT_UCS4_LF) {
                if (port->transcoder != scm_false) {
                    switch (port->eol_style) {
                        case SCM_PORT_EOL_STYLE_CR:
                            port_put_utf16(port, SCM_PORT_UCS4_CR);
                            return;
                        case SCM_PORT_EOL_STYLE_CRLF:
                            port_put_utf16(port, SCM_PORT_UCS4_CR);
                            port_put_utf16(port, SCM_PORT_UCS4_LF);
                            return;
                        case SCM_PORT_EOL_STYLE_NEL:
                            port_put_utf16(port, SCM_PORT_UCS4_NEL);
                            return;
                        case SCM_PORT_EOL_STYLE_CRNEL:
                            port_put_utf16(port, SCM_PORT_UCS4_CR);
                            port_put_utf16(port, SCM_PORT_UCS4_NEL);
                            return;
                        case SCM_PORT_EOL_STYLE_LS:
                            port_put_utf16(port, SCM_PORT_UCS4_LS);
                            return;
                        default:
                            break;
                    }
                }
            }
            port_put_utf16(port, ucs4);
            return;
        }
#if _MSC_VER
        case SCM_PORT_CODEC_CP932: {
            if (ucs4 == SCM_PORT_UCS4_LF) {
                if (port->transcoder != scm_false) {
                    switch (port->eol_style) {
                        case SCM_PORT_EOL_STYLE_CR:
                            port_put_cp932(port, SCM_PORT_UCS4_CR);
                            return;
                        case SCM_PORT_EOL_STYLE_CRLF:
                            port_put_cp932(port, SCM_PORT_UCS4_CR);
                            port_put_cp932(port, SCM_PORT_UCS4_LF);
                            return;
                        case SCM_PORT_EOL_STYLE_NEL:
                            port_put_cp932(port, SCM_PORT_UCS4_NEL);
                            return;
                        case SCM_PORT_EOL_STYLE_CRNEL:
                            port_put_cp932(port, SCM_PORT_UCS4_CR);
                            port_put_cp932(port, SCM_PORT_UCS4_NEL);
                            return;
                        case SCM_PORT_EOL_STYLE_LS:
                            port_put_cp932(port, SCM_PORT_UCS4_LS);
                            return;
                        default:
                            break;
                    }
                }
            }
            port_put_cp932(port, ucs4);
            return;
        }
#endif
        case SCM_PORT_CODEC_LATIN1: {
            if (ucs4 <= 0xff) {
                if (ucs4 == SCM_PORT_UCS4_LF) {
                    if (port->transcoder != scm_false) {
                        switch (port->eol_style) {
                            case SCM_PORT_EOL_STYLE_CR:
                                port_put_byte(port, SCM_PORT_UCS4_CR);
                                return;
                            case SCM_PORT_EOL_STYLE_CRLF:
                                port_put_byte(port, SCM_PORT_UCS4_CR);
                                port_put_byte(port, SCM_PORT_UCS4_LF);
                                return;
                            default:
                                break;
                        }
                    }
                }
                port_put_byte(port, ucs4);
                return;
            }
            switch (port->error_handling_mode) {
                case SCM_PORT_ERROR_HANDLING_MODE_IGNORE:
                    return;
                case SCM_PORT_ERROR_HANDLING_MODE_REPLACE:
                    port_put_byte(port, SCM_PORT_BYTE_REPLACEMENT_CHAR);
                    return;
                case SCM_PORT_ERROR_HANDLING_MODE_RAISE:
                    throw_codec_error(SCM_PORT_OPERATION_ENCODE, "encountered a character it cannot encode", MAKECHAR(ucs4));
                default:
                    fatal("%s:%u wrong error handling mode", __FILE__, __LINE__);
            }
        }

        default:
            throw_codec_error(SCM_PORT_OPERATION_ENCODE, "utf-16 codec not supported", scm_eof);
    }
}

void
port_put_char(scm_port_t port, scm_char_t c)
{
    port_putc(port, CHAR(c));
    if (port->type == SCM_PORT_TYPE_CUSTOM) {
        if (port->buf_tail > port->buf + port->buf_size - SCM_CHAR_OCTET_MAX) {
            port_flush_output(port);
        }
    }
}

void
port_puts(scm_port_t port, const char* s)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    if (s) {
        char c;
        while ((c = *s++) != 0) port_put_byte(port, c);
        if (port->type == SCM_PORT_TYPE_CUSTOM) {
            assert(VECTORP(port->handlers));
            scm_vector_t vect = (scm_vector_t)port->handlers;
            if (vect->elts[SCM_PORT_HANDLER_TEXTUAL] == scm_true) port_flush_output(port);
        }
    }
}

void
port_put_string(scm_port_t port, scm_string_t string)
{
    uint8_t* buf = (uint8_t*)string->name;
    if (BOOLP(port->transcoder)) {
        port_puts(port, (char*)buf);
    } else {
        while (true) {
            int bytes;
            uint32_t ucs4;
            if (buf[0] == 0) {
                if (port->type == SCM_PORT_TYPE_CUSTOM) {
                    assert(VECTORP(port->handlers));
                    scm_vector_t vect = (scm_vector_t)port->handlers;
                    if (vect->elts[SCM_PORT_HANDLER_TEXTUAL] == scm_true) port_flush_output(port);
                }
                return;
            }
            if ((bytes = cnvt_utf8_to_ucs4(buf, &ucs4)) > 0) {
                port_putc(port, ucs4);
                buf += bytes;
                continue;
            }
            throw_codec_error(SCM_PORT_OPERATION_DECODE, "encountered invalid utf-8 sequence", scm_false);
        }
    }
}

void
port_format(scm_port_t port, const char *fmt, ...)
{
    assert(PORTP(port));
    char buf[128];
    va_list ap;

    va_start(ap, fmt);
    int n = vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    if (n > -1 && n < (int)sizeof(buf)) {
        port_puts(port, buf);
        return;
    }
    char* buf2 = (char*)malloc(n + 1);
    if (buf2) {
        va_start(ap, fmt);
        vsnprintf(buf2, n + 1, fmt, ap);
        va_end(ap);
        port_puts(port, buf2);
        free(buf2);
        return;
    }
    port_puts(port, buf);
}

scm_obj_t
port_socket(scm_port_t port)
{
    assert(PORTP(port));
    if (port->type == SCM_PORT_TYPE_SOCKET) {
        if (SOCKETP(port->name)) return (scm_socket_t)port->name;
        if (PAIRP(port->name) && (SOCKETP(CADR(port->name)))) return (scm_socket_t)CADR(port->name);
        fatal("%s:%u wrong port name", __FILE__, __LINE__);
    }
    return scm_false;
}

void
port_shutdown_output(scm_port_t port)
{
    assert(PORTP(port));
    port->lock.verify_locked();
    if (port->opened) {
        port_flush_output(port);
        if (port->type == SCM_PORT_TYPE_SOCKET) socket_shutdown((scm_socket_t)port_socket(port), 1);
    }
}
