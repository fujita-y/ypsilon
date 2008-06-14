/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef	FILE_H_INCLUDED
#define	FILE_H_INCLUDED

#include "core.h"
#include "object.h"

scm_obj_t file_exists(VM* vm, scm_string_t path);
scm_obj_t delete_file(VM* vm, scm_string_t path);
scm_obj_t stat_mtime(VM* vm, scm_string_t path);
scm_obj_t directory_list(VM* vm, scm_string_t path);
scm_obj_t current_directory(VM* vm);
scm_obj_t create_directory(VM* vm, scm_string_t path);
scm_obj_t set_current_directory(VM* vm, scm_string_t path);

void* load_shared_object(scm_string_t path);
void* lookup_shared_object(void* hdl, scm_obj_t proc);
char* last_shared_object_error();

#if _MSC_VER
bool win32path(scm_string_t path, wchar_t* buf, int count);
bool win32path(const char* path, wchar_t* buf, int count);
bool posixpath(const wchar_t* ucs2, char* utf8, int count);
#endif

#endif
