/*
  Ypsilon Scheme System
  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "file.h"
#include "violation.h"

// current-directory
scm_obj_t
subr_current_directory(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            return set_current_directory(vm, (scm_string_t)argv[0]);
        }
        wrong_type_argument_violation(vm, "current-directory", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return current_directory(vm);
    wrong_number_of_arguments_violation(vm, "current-directory", 0, 1, argc, argv);
    return scm_undef;
}

// create-directory
scm_obj_t
subr_create_directory(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            return create_directory(vm, (scm_string_t)argv[0]);
        }
        wrong_type_argument_violation(vm, "create-directory", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "create-directory", 1, 1, argc, argv);
    return scm_undef;
}

// file-exists?
scm_obj_t
subr_file_exists_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_exists(vm, string);
        }
        wrong_type_argument_violation(vm, "file-exists?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-exists?", 1, 1, argc, argv);
    return scm_undef;
}

// delete-file
scm_obj_t
subr_delete_file(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return delete_file(vm, string);
        }
        wrong_type_argument_violation(vm, "delete-file", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "delete-file", 1, 1, argc, argv);
    return scm_undef;
}

// directory-list
scm_obj_t
subr_directory_list(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return directory_list(vm, string);
        }
        wrong_type_argument_violation(vm, "directory-list", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "directory-list", 1, 1, argc, argv);
    return scm_undef;
}

// file-size-in-bytes
scm_obj_t
subr_file_size_in_bytes(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_size_in_bytes(vm, string);
        }
        wrong_type_argument_violation(vm, "file-size-in-bytes", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-size-in-bytes", 1, 1, argc, argv);
    return scm_undef;
}

// file-regular?
scm_obj_t
subr_file_regular_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_regular(vm, string);
        }
        wrong_type_argument_violation(vm, "file-regular?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-regular?", 1, 1, argc, argv);
    return scm_undef;
}

// file-directory?
scm_obj_t
subr_file_directory_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_directory(vm, string);
        }
        wrong_type_argument_violation(vm, "file-directory?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-directory?", 1, 1, argc, argv);
    return scm_undef;
}

// file-symbolic-link?
scm_obj_t
subr_file_symbolic_link_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_symbolic_link(vm, string);
        }
        wrong_type_argument_violation(vm, "file-symbolic-link?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-symbolic-link?", 1, 1, argc, argv);
    return scm_undef;
}

// file-readable?
scm_obj_t
subr_file_readable_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_readable(vm, string);
        }
        wrong_type_argument_violation(vm, "file-readable?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-readable?", 1, 1, argc, argv);
    return scm_undef;
}

// file-writable?
scm_obj_t
subr_file_writable_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_writable(vm, string);
        }
        wrong_type_argument_violation(vm, "file-writable?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-rwritable?", 1, 1, argc, argv);
    return scm_undef;
}

// file-executable?
scm_obj_t
subr_file_executable_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_executable(vm, string);
        }
        wrong_type_argument_violation(vm, "file-executable?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-executable?", 1, 1, argc, argv);
    return scm_undef;
}

// file-stat-ctime
scm_obj_t
subr_file_stat_ctime(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_stat_ctime(vm, string);
        }
        wrong_type_argument_violation(vm, "file-stat-ctime", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-stat-ctime", 1, 1, argc, argv);
    return scm_undef;
}

// file-stat-mtime
scm_obj_t
subr_file_stat_mtime(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_stat_mtime(vm, string);
        }
        wrong_type_argument_violation(vm, "file-stat-mtime", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-stat-mtime", 1, 1, argc, argv);
    return scm_undef;
}

// file-stat-atime
scm_obj_t
subr_file_stat_atime(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return file_stat_atime(vm, string);
        }
        wrong_type_argument_violation(vm, "file-stat-atime", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "file-stat-atime", 1, 1, argc, argv);
    return scm_undef;
}

// create-symbolic-link
scm_obj_t
subr_create_symbolic_link(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1])) {
                return create_symbolic_link(vm, (scm_string_t)argv[0], (scm_string_t)argv[1]);
            }
            wrong_type_argument_violation(vm, "create-symbolic-link", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "create-symbolic-link", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "create-symbolic-link", 2, 2, argc, argv);
    return scm_undef;
}

// create-hard-link
scm_obj_t
subr_create_hard_link(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1])) {
                return create_hard_link(vm, (scm_string_t)argv[0], (scm_string_t)argv[1]);
            }
            wrong_type_argument_violation(vm, "create-hard-link", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "create-hard-link", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "create-hard-link", 2, 2, argc, argv);
    return scm_undef;
}

// rename-file
scm_obj_t
subr_rename_file(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1])) {
                return rename_file(vm, (scm_string_t)argv[0], (scm_string_t)argv[1]);
            }
            wrong_type_argument_violation(vm, "rename-file", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "rename-file", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "rename-file", 2, 2, argc, argv);
    return scm_undef;
}

// change-file-mode
scm_obj_t
subr_change_file_mode(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                return change_file_mode(vm, (scm_string_t)argv[0], FIXNUM(argv[1]));
            }
            wrong_type_argument_violation(vm, "change-file-mode", 1, "fixnum", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "change-file-mode", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "change-file-mode", 2, 2, argc, argv);
    return scm_undef;
}

// system-share-path
scm_obj_t
subr_system_share_path(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        return make_string_literal(vm->m_heap, SYSTEM_SHARE_PATH);
    }
    wrong_number_of_arguments_violation(vm, "system-share-path", 0, 0, argc, argv);
    return scm_undef;
}

void
init_subr_file(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("current-directory", subr_current_directory);
    DEFSUBR("create-directory", subr_create_directory);
    DEFSUBR("file-exists?", subr_file_exists_pred);
    DEFSUBR("delete-file", subr_delete_file);
    DEFSUBR("directory-list", subr_directory_list);
    DEFSUBR("file-size-in-bytes", subr_file_size_in_bytes);
    DEFSUBR("file-regular?", subr_file_regular_pred);
    DEFSUBR("file-directory?", subr_file_directory_pred);
    DEFSUBR("file-symbolic-link?", subr_file_symbolic_link_pred);
    DEFSUBR("file-readable?", subr_file_readable_pred);
    DEFSUBR("file-writable?", subr_file_writable_pred);
    DEFSUBR("file-executable?", subr_file_executable_pred);
    DEFSUBR("file-stat-ctime", subr_file_stat_ctime);
    DEFSUBR("file-stat-mtime", subr_file_stat_mtime);
    DEFSUBR("file-stat-atime", subr_file_stat_atime);
    DEFSUBR("change-file-mode", subr_change_file_mode);
    DEFSUBR("create-symbolic-link", subr_create_symbolic_link);
    DEFSUBR("create-hard-link", subr_create_hard_link);
    DEFSUBR("rename-file", subr_rename_file);
    DEFSUBR("system-share-path", subr_system_share_path);
}
