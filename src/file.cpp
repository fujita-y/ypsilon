// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "file.h"
#include "arith.h"
#include "ioerror.h"
#include "port.h"
#include "violation.h"
#include "vm.h"

scm_obj_t file_stat_atime(VM* vm, scm_string_t path) {
  struct stat st;
  if (stat(path->name, &st) == 0) {
#if __DARWIN_64_BIT_INO_T
    return arith_add(vm->m_heap, int32_to_integer(vm->m_heap, st.st_atimespec.tv_nsec),
                     arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_atimespec.tv_sec)));
#elif defined(_BSD_SOURCE) || defined(_SVID_SOURCE)
    return arith_add(vm->m_heap, int32_to_integer(vm->m_heap, st.st_atim.tv_nsec),
                     arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_atim.tv_sec)));
#else
    return arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_atime));
#endif
  }
  raise_io_error(vm, "file-stat-atime", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
  return scm_undef;
}

scm_obj_t file_stat_mtime(VM* vm, scm_string_t path) {
  struct stat st;
  if (stat(path->name, &st) == 0) {
#if __DARWIN_64_BIT_INO_T
    return arith_add(vm->m_heap, int32_to_integer(vm->m_heap, st.st_mtimespec.tv_nsec),
                     arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_mtimespec.tv_sec)));
#elif defined(_BSD_SOURCE) || defined(_SVID_SOURCE)
    return arith_add(vm->m_heap, int32_to_integer(vm->m_heap, st.st_mtim.tv_nsec),
                     arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_mtim.tv_sec)));
#else
    return arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_mtime));
#endif
  }
  raise_io_error(vm, "file-stat-mtime", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
  return scm_undef;
}

scm_obj_t file_stat_ctime(VM* vm, scm_string_t path) {
  struct stat st;
  if (stat(path->name, &st) == 0) {
#if __DARWIN_64_BIT_INO_T
    return arith_add(vm->m_heap, int32_to_integer(vm->m_heap, st.st_ctimespec.tv_nsec),
                     arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_ctimespec.tv_sec)));
#elif defined(_BSD_SOURCE) || defined(_SVID_SOURCE)
    return arith_add(vm->m_heap, int32_to_integer(vm->m_heap, st.st_ctim.tv_nsec),
                     arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_ctim.tv_sec)));
#else
    return arith_mul(vm->m_heap, MAKEFIXNUM(1000000000), int32_to_integer(vm->m_heap, st.st_ctime));
#endif
  }
  raise_io_error(vm, "file-stat-ctime", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
  return scm_undef;
}

scm_obj_t file_size_in_bytes(VM* vm, scm_string_t path) {
  struct stat st;
  if (stat(path->name, &st) == 0) return int64_to_integer(vm->m_heap, st.st_size);
  raise_io_error(vm, "file-size-in-bytes", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
  return scm_undef;
}

scm_obj_t file_regular(VM* vm, scm_string_t path) {
  struct stat st;
  if (stat(path->name, &st) == 0) return S_ISREG(st.st_mode) ? scm_true : scm_false;
  return scm_false;
}

scm_obj_t file_directory(VM* vm, scm_string_t path) {
  struct stat st;
  if (stat(path->name, &st) == 0) return S_ISDIR(st.st_mode) ? scm_true : scm_false;
  return scm_false;
}

scm_obj_t file_symbolic_link(VM* vm, scm_string_t path) {
  struct stat st;
  if (lstat(path->name, &st) == 0) return S_ISLNK(st.st_mode) ? scm_true : scm_false;
  return scm_false;
}

scm_obj_t file_exists(VM* vm, scm_string_t path) { return (access(path->name, F_OK) == 0) ? scm_true : scm_false; }

scm_obj_t file_readable(VM* vm, scm_string_t path) { return (access(path->name, R_OK) == 0) ? scm_true : scm_false; }

scm_obj_t file_writable(VM* vm, scm_string_t path) { return (access(path->name, W_OK) == 0) ? scm_true : scm_false; }

scm_obj_t file_executable(VM* vm, scm_string_t path) { return (access(path->name, X_OK) == 0) ? scm_true : scm_false; }

scm_obj_t delete_file(VM* vm, scm_string_t path) {
  if (remove(path->name) < 0) {
    raise_io_error(vm, "delete-file", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
    return scm_undef;
  }
  return scm_unspecified;
}

scm_obj_t change_file_mode(VM* vm, scm_string_t path, int mode) {
  if (chmod(path->name, mode) < 0) {
    raise_io_error(vm, "change-file-mode", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
    return scm_undef;
  }
  return scm_unspecified;
}

scm_obj_t rename_file(VM* vm, scm_string_t old_path, scm_string_t new_path) {
  if (rename(old_path->name, new_path->name) < 0) {
    raise_io_filesystem_error(vm, "rename-file", strerror(errno), errno, old_path, new_path);
    return scm_undef;
  }
  return scm_unspecified;
}

scm_obj_t create_symbolic_link(VM* vm, scm_string_t old_path, scm_string_t new_path) {
  if (symlink(old_path->name, new_path->name) < 0) {
    raise_io_error(vm, "create-symbolic-link", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, new_path);
    return scm_undef;
  }
  return scm_unspecified;
}

scm_obj_t create_hard_link(VM* vm, scm_string_t old_path, scm_string_t new_path) {
  if (link(old_path->name, new_path->name) < 0) {
    raise_io_error(vm, "create-hard-link", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, new_path);
    return scm_undef;
  }
  return scm_unspecified;
}

scm_obj_t create_directory(VM* vm, scm_string_t path) {
  if (mkdir(path->name, S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
    raise_io_error(vm, "create-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
    return scm_undef;
  }
  return scm_unspecified;
}

scm_obj_t acquire_lockfile(VM* vm, scm_string_t path) {
  int fd = open(path->name, O_CREAT, S_IRUSR | S_IWUSR);
  if (fd < 0) {
    raise_io_error(vm, "acquire_lockfile", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
    return scm_undef;
  }
  if (flock(fd, LOCK_EX) < 0) {
    raise_io_error(vm, "acquire_lockfile", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
    return scm_undef;
  }
  return intptr_to_integer(vm->m_heap, fd);
}

scm_obj_t release_lockfile(VM* vm, scm_obj_t descriptor) {
  int fd = coerce_exact_integer_to_intptr(descriptor);
  if (flock(fd, LOCK_UN) < 0) {
    raise_io_error(vm, "release_lockfile", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, descriptor);
    return scm_undef;
  }
  close(fd);
  return scm_unspecified;
}

scm_obj_t current_directory(VM* vm) {
  char buf[MAXPATHLEN];
  if (getcwd(buf, MAXPATHLEN) == NULL) {
    raise_io_error(vm, "current-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, scm_false);
    return scm_undef;
  }
  return make_string_literal(vm->m_heap, buf);
}

scm_obj_t set_current_directory(VM* vm, scm_string_t path) {
  if (chdir(path->name) < 0) {
    raise_io_error(vm, "current-directory", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, path);
    return scm_undef;
  }
  return scm_unspecified;
}

scm_obj_t directory_list(VM* vm, scm_string_t path) {
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

void* load_shared_object(scm_string_t path) {
  if (path == NULL) return dlopen(NULL, RTLD_LAZY | RTLD_GLOBAL);
  return dlopen(path->name, RTLD_LAZY | RTLD_GLOBAL);
}

void* lookup_shared_object(void* hdl, scm_obj_t proc) {
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

const char* last_shared_object_error() { return dlerror(); }
