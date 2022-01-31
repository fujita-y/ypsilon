// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef FILE_H_INCLUDED
#define FILE_H_INCLUDED

#include "core.h"
#include "object.h"

scm_obj_t file_size_in_bytes(VM* vm, scm_string_t path);
scm_obj_t file_regular(VM* vm, scm_string_t path);
scm_obj_t file_directory(VM* vm, scm_string_t path);
scm_obj_t file_symbolic_link(VM* vm, scm_string_t path);
scm_obj_t file_readable(VM* vm, scm_string_t path);
scm_obj_t file_writable(VM* vm, scm_string_t path);
scm_obj_t file_executable(VM* vm, scm_string_t path);
scm_obj_t file_stat_ctime(VM* vm, scm_string_t path);
scm_obj_t file_stat_mtime(VM* vm, scm_string_t path);
scm_obj_t file_stat_atime(VM* vm, scm_string_t path);
scm_obj_t create_symbolic_link(VM* vm, scm_string_t old_path, scm_string_t new_path);
scm_obj_t create_hard_link(VM* vm, scm_string_t old_path, scm_string_t new_path);
scm_obj_t acquire_lockfile(VM* vm, scm_string_t path);
scm_obj_t release_lockfile(VM* vm, scm_obj_t descriptor);
scm_obj_t rename_file(VM* vm, scm_string_t old_path, scm_string_t new_path);
scm_obj_t change_file_mode(VM* vm, scm_string_t path, int mode);

scm_obj_t file_exists(VM* vm, scm_string_t path);
scm_obj_t delete_file(VM* vm, scm_string_t path);
scm_obj_t directory_list(VM* vm, scm_string_t path);
scm_obj_t current_directory(VM* vm);
scm_obj_t create_directory(VM* vm, scm_string_t path);
scm_obj_t set_current_directory(VM* vm, scm_string_t path);

void* load_shared_object(scm_string_t path);
void* lookup_shared_object(void* hdl, scm_obj_t proc);
const char* last_shared_object_error();

#endif
