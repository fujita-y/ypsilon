/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
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

    scm_obj_t file_stat_mtime(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            HANDLE fd = CreateFileW(ucs2, 0, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_NORMAL, NULL);
            if (fd != INVALID_HANDLE_VALUE) {
                BY_HANDLE_FILE_INFORMATION fileInfo;
                if (GetFileInformationByHandle(fd, &fileInfo)) {
                    CloseHandle(fd);
                    int64_t tm = ((int64_t)fileInfo.ftLastWriteTime.dwHighDateTime << 32) + fileInfo.ftLastWriteTime.dwLowDateTime;
                    return int64_to_integer(vm->m_heap, tm);
                }
                CloseHandle(fd);
            }
            _dosmaperr(GetLastError());
            raise_io_error(vm, "file-stat-mtime", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        raise_io_error(vm, "file-stat-mtime", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    scm_obj_t file_stat_ctime(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            HANDLE fd = CreateFileW(ucs2, 0, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_NORMAL, NULL);
            if (fd != INVALID_HANDLE_VALUE) {
                BY_HANDLE_FILE_INFORMATION fileInfo;
                if (GetFileInformationByHandle(fd, &fileInfo)) {
                    CloseHandle(fd);
                    int64_t tm = ((int64_t)fileInfo.ftCreationTime.dwHighDateTime << 32) + fileInfo.ftCreationTime.dwLowDateTime;
                    return int64_to_integer(vm->m_heap, tm);
                }
                CloseHandle(fd);
            }
            _dosmaperr(GetLastError());
            raise_io_error(vm, "file-stat-ctime", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        raise_io_error(vm, "file-stat-ctime", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    scm_obj_t file_size_in_bytes(VM *vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
           HANDLE fd = CreateFileW(ucs2, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
            if (fd != INVALID_HANDLE_VALUE) {
                LARGE_INTEGER bsize;
                if (GetFileSizeEx(fd, &bsize)) {
                    CloseHandle(fd);
                    return int64_to_integer(vm->m_heap, bsize.QuadPart);
                }
                CloseHandle(fd);
            }
            _dosmaperr(GetLastError());
            raise_io_error(vm, "file-size-in-bytes", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        raise_io_error(vm, "file-size-in-bytes", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    scm_obj_t file_regular(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            HANDLE fd = CreateFileW(ucs2, 0, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
            if (fd != INVALID_HANDLE_VALUE) {
                DWORD type = GetFileType(fd) & ~FILE_TYPE_REMOTE;
                CloseHandle(fd);
                return (type == FILE_TYPE_DISK) ? scm_true : scm_false;
            }
            return scm_false;
        }
        return scm_false;
    }

    scm_obj_t file_directory(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            if (PathIsDirectoryW(ucs2)) return scm_true;
            return scm_false;
        }
        return scm_false;
    }

    scm_obj_t file_symbolic_link(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            return scm_false;
        }
        return scm_false;
    }

    static bool ucs2_file_exists(wchar_t* ucs2)
    {
        HANDLE fd = CreateFileW(ucs2, 0, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_NORMAL, NULL);
        if (fd == INVALID_HANDLE_VALUE) return false;
        CloseHandle(fd);
        return true;
    }

    scm_obj_t file_exists(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            return ucs2_file_exists(ucs2) ? scm_true : scm_false;
        }
        return scm_false;
    }
        
    scm_obj_t file_readable(VM* vm, scm_string_t path)
    {
        return file_exists(vm, path);
    }

    scm_obj_t file_writable(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            HANDLE fd = CreateFileW(ucs2, 0, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_NORMAL, NULL);
            if (fd != INVALID_HANDLE_VALUE) {
                BY_HANDLE_FILE_INFORMATION fileInfo;
                if (GetFileInformationByHandle(fd, &fileInfo)) {
                    CloseHandle(fd);
                    return (fileInfo.dwFileAttributes & FILE_ATTRIBUTE_READONLY) ? scm_false : scm_true;
                }
            }
            CloseHandle(fd);
        }
        return scm_false;
    }

    scm_obj_t file_executable(VM* vm, scm_string_t path)
    {
        const wchar_t* pathext = TEXT(".COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC");
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            if (ucs2_file_exists(ucs2)) {
                if (PathIsDirectoryW(ucs2)) return scm_true;
                wchar_t extension[MAX_PATH];
                wcsncpy(extension, PathFindExtensionW(ucs2), MAX_PATH);
                if (extension[0] == 0) return scm_false;
                return (wstrstr(_wcsupr(extension), pathext)) ? scm_true : scm_false;
            }
        }
        return scm_false;
    }
    /*

    scm_obj_t file_executable(VM* vm, scm_string_t path)
    {
        const wchar_t* pathext = TEXT("*.com;*.exe;*.bat;*.cmd;*.vbs;*.vbe;*.js;*.jse;*.wsf;*.wsh;*.msc");
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            if (ucs2_file_exists(ucs2)) {
                if (PathIsDirectoryW(ucs2)) return scm_true;
                if (PathMatchSpecExW(ucs2, pathext, PMSF_MULTIPLE) == 0) return scm_true;
            }
        }
        return scm_false;
    }
    
    */

    scm_obj_t delete_file(VM* vm, scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) {
            if (DeleteFile(ucs2)) return scm_unspecified;
            _dosmaperr(GetLastError());
            raise_io_error(vm, "delete-file", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        raise_io_error(vm, "delete-file", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, path);
        return scm_undef;
    }

    scm_obj_t rename_file(VM* vm, scm_string_t old_path, scm_string_t new_path)
    {
        wchar_t old_ucs2[MAX_PATH];
        if (win32path(old_path, old_ucs2, array_sizeof(old_ucs2))) {
            wchar_t new_ucs2[MAX_PATH];
            if (win32path(new_path, new_ucs2, array_sizeof(new_ucs2))) {
                if (MoveFileEx(old_ucs2, new_ucs2, MOVEFILE_REPLACE_EXISTING)) return scm_unspecified;
                _dosmaperr(GetLastError());
                raise_io_filesystem_error(vm, "rename-file", strerror(errno), errno, old_path, new_path);
                return scm_undef;
            }
            raise_io_error(vm, "rename-file", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, new_path);
            return scm_undef;
        }
        raise_io_error(vm, "rename-file", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, old_path);
        return scm_undef;
    }

    typedef BOOL (WINAPI* ProcCreateSymbolicLink) (LPCTSTR, LPCTSTR, DWORD); 
    typedef BOOL (WINAPI* ProcCreateHardLink) (LPCTSTR, LPCTSTR, LPSECURITY_ATTRIBUTES); 

    scm_obj_t create_symbolic_link(VM* vm, scm_string_t old_path, scm_string_t new_path)
    {
        ProcCreateSymbolicLink win32CreateSymbolicLink = (ProcCreateSymbolicLink)GetProcAddress(LoadLibraryA("kernel32"), "CreateSymbolicLinkW");
        if (win32CreateSymbolicLink) {
            wchar_t old_ucs2[MAX_PATH];
            if (win32path(old_path, old_ucs2, array_sizeof(old_ucs2))) {
                wchar_t new_ucs2[MAX_PATH];
                if (win32path(new_path, new_ucs2, array_sizeof(new_ucs2))) {
                    DWORD flag = PathIsDirectoryW(new_ucs2) ? 1 : 0; // SYMBOLIC_LINK_FLAG_DIRECTORY == 1
                    if (win32CreateSymbolicLink(new_ucs2, old_ucs2, flag)) return scm_unspecified;
                    _dosmaperr(GetLastError());
                    raise_io_filesystem_error(vm, "create-symbolic-link", strerror(errno), errno, old_path, new_path);
                    return scm_undef;
                }
                raise_io_error(vm, "create-symbolic-link", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, new_path);
                return scm_undef;
            }
            raise_io_error(vm, "create-symbolic-link", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, old_path);
            return scm_undef;
        }
        raise_io_filesystem_error(vm, "create-symbolic-link", "operating system does not support symblic link", 0, old_path, new_path);
        return scm_undef;        
    }

    scm_obj_t create_hard_link(VM* vm, scm_string_t old_path, scm_string_t new_path)
    {
        ProcCreateHardLink win32CreateHardLink = (ProcCreateHardLink)GetProcAddress(LoadLibraryA("kernel32"), "CreateHardLinkW");
        if (win32CreateHardLink) {
            wchar_t old_ucs2[MAX_PATH];
            if (win32path(old_path, old_ucs2, array_sizeof(old_ucs2))) {
                wchar_t new_ucs2[MAX_PATH];
                if (win32path(new_path, new_ucs2, array_sizeof(new_ucs2))) {
                    if (win32CreateHardLink(new_ucs2, old_ucs2, NULL)) return scm_unspecified;
                    _dosmaperr(GetLastError());
                    raise_io_filesystem_error(vm, "create-hard-link", strerror(errno), errno, old_path, new_path);
                    return scm_undef;
                }
                raise_io_error(vm, "create-hard-link", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, new_path);
                return scm_undef;
            }
            raise_io_error(vm, "create-hard-link", SCM_PORT_OPERATION_OPEN, strerror(ENOENT), ENOENT, scm_false, old_path);
            return scm_undef;
        }
        raise_io_filesystem_error(vm, "create-hard-link", "operating system does not support hard link", 0, old_path, new_path);
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

    void*
    load_shared_object(scm_string_t path)
    {
        wchar_t ucs2[MAX_PATH];
        if (win32path(path, ucs2, array_sizeof(ucs2))) return LoadLibraryW(ucs2);
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
    scm_obj_t file_stat_mtime(VM* vm, scm_string_t path)
    {
        struct stat st;
        if (stat(path->name, &st) == 0) {
#if __DARWIN_64_BIT_INO_T
            return arith_add(vm->m_heap,
                        int32_to_integer(vm->m_heap, st.st_mtimespec.tv_nsec),
                        arith_mul(vm->m_heap,
                            MAKEFIXNUM(1000000000),
                            int32_to_integer(vm->m_heap, st.st_mtimespec.tv_sec)));
#elif defined(_BSD_SOURCE) || defined(_SVID_SOURCE)
            return arith_add(vm->m_heap,
                        int32_to_integer(vm->m_heap, st.st_atim.tv_nsec),
                        arith_mul(vm->m_heap,
                            MAKEFIXNUM(1000000000),
                            int32_to_integer(vm->m_heap, st.st_atim.tv_sec)));
#else
            return arith_mul(vm->m_heap,
                            MAKEFIXNUM(1000000000),
                            int32_to_integer(vm->m_heap, st.st_mtime));
#endif
        }
        raise_io_error(vm, "file-stat-mtime", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
        return scm_undef;
    }

    scm_obj_t file_stat_ctime(VM* vm, scm_string_t path)
    {
        struct stat st;
        if (stat(path->name, &st) == 0) {
#if __DARWIN_64_BIT_INO_T
            return arith_add(vm->m_heap,
                        int32_to_integer(vm->m_heap, st.st_ctimespec.tv_nsec),
                        arith_mul(vm->m_heap,
                            MAKEFIXNUM(1000000000),
                            int32_to_integer(vm->m_heap, st.st_ctimespec.tv_sec)));
#elif defined(_BSD_SOURCE) || defined(_SVID_SOURCE)
            return arith_add(vm->m_heap,
                        int32_to_integer(vm->m_heap, st.st_ctim.tv_nsec),
                        arith_mul(vm->m_heap,
                            MAKEFIXNUM(1000000000),
                            int32_to_integer(vm->m_heap, st.st_ctim.tv_sec)));
#else
            return arith_mul(vm->m_heap,
                            MAKEFIXNUM(1000000000),
                            int32_to_integer(vm->m_heap, st.st_ctime));
#endif
        }
        raise_io_error(vm, "file-stat-ctime", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
        return scm_undef;
    }

    scm_obj_t file_size_in_bytes(VM *vm, scm_string_t path)
    {
        struct stat st;
        if (stat(path->name, &st) == 0) return int64_to_integer(vm->m_heap, st.st_size);
        raise_io_error (vm, "file-size-in-bytes", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
        return scm_undef;
    }

    scm_obj_t file_regular(VM* vm, scm_string_t path)
    {
        struct stat st;
        if (lstat(path->name, &st) == 0) return S_ISREG(st.st_mode) ? scm_true : scm_false;
        return scm_false;
    }

    scm_obj_t file_directory(VM* vm, scm_string_t path)
    {
        struct stat st;
        if (lstat(path->name, &st) == 0) return S_ISDIR(st.st_mode) ? scm_true : scm_false;
        return scm_false;
    }

    scm_obj_t file_symbolic_link(VM* vm, scm_string_t path)
    {
        struct stat st;
        if (lstat(path->name, &st) == 0) return S_ISLNK(st.st_mode) ? scm_true : scm_false;
        return scm_false;
    }

    scm_obj_t file_exists(VM* vm, scm_string_t path)
    {
        return (access(path->name, F_OK) == 0) ? scm_true : scm_false;
    }

    scm_obj_t file_readable(VM* vm, scm_string_t path)
    {
        return (access(path->name, R_OK) == 0) ? scm_true : scm_false;
    }

    scm_obj_t file_writable(VM* vm, scm_string_t path)
    {
        return (access(path->name, W_OK) == 0) ? scm_true : scm_false;
    }

    scm_obj_t file_executable(VM* vm, scm_string_t path)
    {
        return (access(path->name, X_OK) == 0) ? scm_true : scm_false;
    }

    scm_obj_t delete_file(VM* vm, scm_string_t path)
    {
        if (remove(path->name) < 0) {
            raise_io_error(vm, "delete-file", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        return scm_unspecified;
    }

    scm_obj_t rename_file(VM* vm, scm_string_t old_path, scm_string_t new_path)
    {
        if (rename(old_path->name, new_path->name) < 0) {
            raise_io_filesystem_error(vm, "rename-file", strerror(errno), errno, old_path, new_path);
            return scm_undef;
        }
        return scm_unspecified;
    }

    scm_obj_t create_symbolic_link(VM* vm, scm_string_t old_path, scm_string_t new_path)
    {
        if (symlink(old_path->name, new_path->name) < 0) {
            raise_io_error(vm, "create-symbolic-link", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, new_path);
            return scm_undef;
        }
        return scm_unspecified;
    }

    scm_obj_t create_hard_link(VM* vm, scm_string_t old_path, scm_string_t new_path)
    {
        if (link(old_path->name, new_path->name) < 0) {
            raise_io_error(vm, "create-hard-link", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, new_path);
            return scm_undef;
        }
        return scm_unspecified;
    }

    scm_obj_t create_directory(VM* vm, scm_string_t path)
    {
        if (mkdir(path->name, S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
            raise_io_error(vm, "create-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
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

    scm_obj_t set_current_directory(VM* vm, scm_string_t path)
    {
        if (chdir(path->name) < 0) {
            raise_io_error(vm, "current-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
            return scm_undef;
        }
        return scm_unspecified;
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
