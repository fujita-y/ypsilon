/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "file.h"
#include "port.h"
#include "arith.h"
#include "ioerror.h"
#include "violation.h"

#if _MSC_VER

    bool win32path(scm_string_t path, wchar_t* ucs2, int count)
    {
        if (MultiByteToWideChar(CP_UTF8, 0, path->name, -1, ucs2, count)) {
            int i = 0;
            while (ucs2[i]) {
                if (ucs2[i] == L'/') ucs2[i] = L'\\';
                i++;
            }
            return true;
        }
        return false;
    }

    bool win32path(const char* path, wchar_t* ucs2, int count)
    {
        if (MultiByteToWideChar(CP_UTF8, 0, path, -1, ucs2, count)) {
            int i = 0;
            while (ucs2[i]) {
                if (ucs2[i] == L'/') ucs2[i] = L'\\';
                i++;
            }
            return true;
        }
        return false;
    }

    bool posixpath(const wchar_t* ucs2, char* utf8, int count)
    {
        if (WideCharToMultiByte(CP_UTF8, 0, ucs2, -1, utf8, count, NULL, NULL)) {
            int i = 0;
            while (utf8[i]) {
                if (utf8[i] == '\\') utf8[i] = '/';
                i++;
            }
            return true;
        }
        return false;
    }

    scm_obj_t file_exists(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            return (_waccess(ucs2, 0) == 0) ? scm_true : scm_false;
        }
        raise_io_error(vm, "file-exists?", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    scm_obj_t stat_mtime(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            HANDLE fd = CreateFileW(ucs2, 0, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
            if (fd != INVALID_HANDLE_VALUE) {
                BY_HANDLE_FILE_INFORMATION fileInfo;
                if (GetFileInformationByHandle(fd, &fileInfo)) {
                    int64_t tm = ((int64_t)fileInfo.ftLastWriteTime.dwHighDateTime << 32) + fileInfo.ftLastWriteTime.dwLowDateTime;
                    return int64_to_integer(vm->m_heap, tm);
                }
            }
            return scm_false;
        }
        raise_io_error(vm, "stat-mtime", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    scm_obj_t directory_list(VM* vm, scm_string_t path)
    {
        char utf8[MAX_PATH];
        snprintf(utf8, sizeof(utf8), "%s\\*", path->name);
        wchar_t ucs2[MAX_PATH];
        if (MultiByteToWideChar(CP_UTF8, 0, utf8, -1, ucs2, array_sizeof(ucs2))) {
            WIN32_FIND_DATAW data;
            HANDLE hdl = FindFirstFileW(ucs2, &data);
            if (hdl != INVALID_HANDLE_VALUE) {
                scm_obj_t lst = scm_nil;
                while (WideCharToMultiByte(CP_UTF8, 0, data.cFileName, -1, utf8, sizeof(utf8), NULL, NULL)) {
                    lst = make_pair(vm->m_heap, make_string_literal(vm->m_heap, utf8), lst);
                    if (FindNextFileW(hdl, &data)) continue;
                    FindClose(hdl);
                    return lst;
                }
            }
        }
        _dosmaperr(GetLastError());
        raise_io_error(vm, "directory-list", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
        return scm_undef;
    }

    scm_obj_t delete_file(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            if (_wremove(ucs2) < 0) {
                _dosmaperr(GetLastError());
                raise_io_error(vm, "delete-file", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
                return scm_undef;
            }
            return scm_unspecified;
        }
        raise_io_error(vm, "delete-file", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    scm_obj_t current_directory(VM* vm)
    {
        wchar_t ucs2[MAX_PATH];
        char utf8[MAX_PATH];
        if (!GetCurrentDirectoryW(MAX_PATH, ucs2) || !posixpath(ucs2, utf8, sizeof(utf8))) {
            _dosmaperr(GetLastError());
            raise_io_error(vm, "current-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, scm_false);
            return scm_undef;
        }
        return make_string_literal(vm->m_heap, utf8);
    }

    scm_obj_t create_directory(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            if (!CreateDirectoryW(ucs2, NULL)) {
                _dosmaperr(GetLastError());
                raise_io_error(vm, "create-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
                return scm_undef;
            }
            return scm_unspecified;
        }
        raise_io_error(vm, "create-directory", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    scm_obj_t set_current_directory(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            if (!SetCurrentDirectoryW(ucs2)) {
                _dosmaperr(GetLastError());
                raise_io_error(vm, "current-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
                return scm_undef;
            }
            return scm_unspecified;
        }
        raise_io_error(vm, "current-directory", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    void*
    load_shared_object(scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            return LoadLibraryW(ucs2);
        }
        return NULL;
    }

    void*
    lookup_shared_object(void* hdl, scm_obj_t proc)
    {
        assert(SYMBOLP(proc) || STRINGP(proc));
        const char* name;
        if (SYMBOLP(proc)) {
            scm_symbol_t symbol = (scm_symbol_t)proc;
            name = symbol->name;
        } else if (STRINGP(proc)) {
            scm_string_t string = (scm_string_t)proc;
            name = string->name;
        }
        return GetProcAddress((HMODULE)hdl, name);
    }

    const char*
    last_shared_object_error()
    {
        static char* s_last_message;
        if (s_last_message) {
            LocalFree(s_last_message);
            s_last_message = NULL;
        }
        FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                        NULL,
                        GetLastError(),
                        MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT),
                        (LPSTR)&s_last_message,
                        0,
                        NULL);
        return s_last_message;
    }

#else

    scm_obj_t file_exists(VM* vm, scm_string_t path)
    {
        return (access(path->name, F_OK) == 0) ? scm_true : scm_false;
    }

    scm_obj_t stat_mtime(VM* vm, scm_string_t path)
    {
        struct stat st;
        if (stat(path->name, &st) == 0) {
#if __DARWIN_64_BIT_INO_T
            return arith_add(vm->m_heap,
                        int32_to_integer(vm->m_heap, st.st_mtimespec.tv_nsec),
                        arith_mul(vm->m_heap,
                            MAKEFIXNUM(1000000000),
                            int32_to_integer(vm->m_heap, st.st_mtimespec.tv_sec)));
#else
            return arith_mul(vm->m_heap,
                            MAKEFIXNUM(1000000000),
                            int32_to_integer(vm->m_heap, st.st_mtime));
#endif
        }
        return scm_false;
    }

    scm_obj_t directory_list(VM* vm, scm_string_t path)
    {
        DIR* dir = opendir(path->name);
        if (dir) {
            scm_obj_t lst = scm_nil;
            while (1) {
                struct dirent* ent = readdir(dir);
                if (ent) {
                    lst = make_pair(vm->m_heap, make_string_literal(vm->m_heap, ent->d_name), lst);
                    continue;
                }
                closedir(dir);
                return lst;
            }
        }
        raise_io_error(vm, "directory-list", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
        return scm_undef;
    }

    scm_obj_t delete_file(VM* vm, scm_string_t path)
    {
        if (remove(path->name) < 0) {
            raise_io_error(vm, "delete-file", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        return scm_unspecified;
    }

    scm_obj_t current_directory(VM* vm)
    {
        char buf[MAXPATHLEN];
        if (getcwd(buf, MAXPATHLEN) == NULL) {
            raise_io_error(vm, "current-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, scm_false);
            return scm_undef;
        }
        return make_string_literal(vm->m_heap, buf);
    }

    scm_obj_t create_directory(VM* vm, scm_string_t path)
    {
        if (mkdir(path->name, S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
            raise_io_error(vm, "create-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        return scm_unspecified;
    }

    scm_obj_t set_current_directory(VM* vm, scm_string_t path)
    {
        if (chdir(path->name) < 0) {
            raise_io_error(vm, "current-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        return scm_unspecified;
    }

    void*
    load_shared_object(scm_string_t path)
    {
        return dlopen(path->name, RTLD_LAZY | RTLD_GLOBAL);
    }

    void*
    lookup_shared_object(void* hdl, scm_obj_t proc)
    {
        assert(SYMBOLP(proc) || STRINGP(proc));
        const char* name;
        if (SYMBOLP(proc)) {
            scm_symbol_t symbol = (scm_symbol_t)proc;
            name = symbol->name;
        } else if (STRINGP(proc)) {
            scm_string_t string = (scm_string_t)proc;
            name = string->name;
        }
        return dlsym(hdl, name);
    }

    const char*
    last_shared_object_error()
    {
        return dlerror();
    }

#endif
