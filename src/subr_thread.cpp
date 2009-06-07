/*
  Ypsilon Scheme System
  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "violation.h"
#include "serialize.h"
#if USE_PARALLEL_VM
#include "bag.h"
#include "list.h"
#include "interpreter.h"
#endif

#define USE_SHARED_QUEUE_QUICK_ENCODE    1
#define CYCLIC_CHECK_BEFORE_SERIALIZE    1

// spawn
scm_obj_t
subr_spawn(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc >= 1) {
        if (CLOSUREP(argv[0])) {
            vm->m_interp->update(vm, VM_STATE_BLOCK);
            int n = vm->m_interp->spawn(vm, (scm_closure_t)argv[0], argc - 1, argv + 1);
            vm->m_interp->update(vm, VM_STATE_ACTIVE);
            if (n < 0) return scm_timeout;
            return MAKEFIXNUM(n);
        }
        wrong_type_argument_violation(vm, "spawn", 0, "closure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "spawn", 1, -1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u spawn not supported on this build", __FILE__, __LINE__);
#endif
}

// spawn-heap-limit
scm_obj_t
subr_spawn_heap_limit(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            vm->m_spawn_heap_limit = FIXNUM(argv[0]);
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "spawn-heap-limit", 0, "non-negative fixnum", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return MAKEFIXNUM(vm->m_spawn_heap_limit);
    wrong_number_of_arguments_violation(vm, "spawn-heap-limit", 0, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u spawn-heap-limit not supported on this build", __FILE__, __LINE__);
#endif
}

// spawn-timeout
scm_obj_t
subr_spawn_timeout(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1) {
        if ((FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) || argv[0] == scm_false) {
            vm->m_spawn_timeout = argv[0];
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "spawn-timeout", 0, "#f or non-negative fixnum", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return vm->m_spawn_timeout;
    wrong_number_of_arguments_violation(vm, "spawn-timeout", 0, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u spawn-timeout not supported on this build", __FILE__, __LINE__);
#endif
}

// on-primordial-thread?
scm_obj_t
subr_on_primordial_thread_pred(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 0) return (vm->m_id == 0) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "on-primordial-thread?", 0, 0, argc, argv);
    return scm_undef;
#else
    if (argc == 0) return scm_true;
    wrong_number_of_arguments_violation(vm, "on-primordial-thread?", 0, 0, argc, argv);
    return scm_undef;
#endif
}

// display-thread-status
scm_obj_t
subr_display_thread_status(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 0) {
        vm->m_interp->display_status(vm);
        return scm_unspecified;
    }
    wrong_number_of_arguments_violation(vm, "display-thread-status", 0, 0, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u display-thread-status not supported on this build", __FILE__, __LINE__);
#endif
}

// make-shared-queue
scm_obj_t
subr_make_shared_queue(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 0) {
        scm_sharedqueue_t queue = make_sharedqueue(vm->m_heap, 1);
        return queue;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) {
            scm_sharedqueue_t queue = make_sharedqueue(vm->m_heap, FIXNUM(argv[0]));
            return queue;
        }
        wrong_type_argument_violation(vm, "make-shared-queue", 1, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-shared-queue", 0, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u make-shared-queue not supported on this build", __FILE__, __LINE__);
#endif
}

// shared-queue?
scm_obj_t
subr_shared_queue_pred(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1) return SHAREDQUEUEP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "shared-queue?", 1, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u shared-queue? not supported on this build", __FILE__, __LINE__);
#endif
}

// shared-queue-push!
scm_obj_t
subr_shared_queue_push(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 2 || argc == 3) {
        if (SHAREDQUEUEP(argv[0])) {
            int timeout = 0;
            if (argc == 3) {
                if (FIXNUMP(argv[2]) && FIXNUM(argv[2]) >= 0) {
                    timeout = FIXNUM(argv[2]);
                } else {
                    wrong_type_argument_violation(vm, "shared-queue-push!", 2, "non-negative fixnum", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            scm_sharedqueue_t queue = (scm_sharedqueue_t)argv[0];
            intptr_t id;
#if USE_SHARED_QUEUE_QUICK_ENCODE
            if (FIXNUMP(argv[1])) {
                id = FIXNUM(argv[1]) | INTPTR_MIN;
            }
            else if (argv[1] == scm_true) {
                id = INTPTR_MAX;
            }
            else if (argv[1] == scm_false) {
                id = INTPTR_MAX - 1;
            }
            else
#endif
            {
#if CYCLIC_CHECK_BEFORE_SERIALIZE
                if (cyclic_objectp(vm->m_heap, argv[1])) {
                    serialize_cyclic_object_violation(vm, "shared-queue-push!", argv[1], argc, argv);
                    return scm_undef;
                }
#endif
                scm_obj_t obj = serializer_t(vm->m_heap).translate(argv[1]);
                if (BVECTORP(obj)) {
                    scm_bvector_t bvector = (scm_bvector_t)obj;
                    id = queue->buf.put(bvector->elts, bvector->count);
                } else {
                    non_serializable_object_violation(vm, "shared-queue-push!", obj, argc, argv);
                    return scm_undef;
                }
            }
            if (queue->queue.wait_lock_try_put(id)) return scm_true;
            if (argc == 3) {
                vm->m_interp->update(vm, VM_STATE_BLOCK);
                bool succ = queue->queue.put(id, timeout);
                vm->m_interp->update(vm, VM_STATE_ACTIVE);
                if (succ) return scm_true;
                if (queue->queue.no_more_put()) return scm_shutdown;
                return scm_timeout;
            } else {
                vm->m_interp->update(vm, VM_STATE_BLOCK);
                bool succ = queue->queue.put(id);
                vm->m_interp->update(vm, VM_STATE_ACTIVE);
                return succ ? scm_true : scm_shutdown;
            }
        }
        wrong_type_argument_violation(vm, "shared-queue-push!", 0, "shared queue", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shared-queue-push!", 2, 3, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u shared-queue-push! not supported on this build", __FILE__, __LINE__);
#endif
}

// shared-queue-pop!
scm_obj_t subr_shared_queue_pop(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1 || argc == 2) {
        if (SHAREDQUEUEP(argv[0])) {
            int timeout = 0;
            if (argc == 2) {
                if (FIXNUMP(argv[1]) && FIXNUM(argv[1]) >= 0) {
                    timeout = FIXNUM(argv[1]);
                } else {
                    wrong_type_argument_violation(vm, "shared-queue-pop!", 1, "non-negative fixnum", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            scm_sharedqueue_t queue = (scm_sharedqueue_t)argv[0];
            intptr_t id;
            bool succ;
            if (queue->queue.wait_lock_try_get(&id)) goto receive;
            if (argc == 2) {
                if (timeout == 0) goto timeout;
                vm->m_interp->update(vm, VM_STATE_BLOCK);
                succ = queue->queue.get(&id, timeout);
                vm->m_interp->update(vm, VM_STATE_ACTIVE);
                if (!succ) goto timeout;
            } else {
                vm->m_interp->update(vm, VM_STATE_BLOCK);
                succ = queue->queue.get(&id);
                vm->m_interp->update(vm, VM_STATE_ACTIVE);
                if (!succ) return scm_shutdown;
            }

        receive:
            {
#if USE_SHARED_QUEUE_QUICK_ENCODE
                if (id < 0) return MAKEFIXNUM(id);
                if (id == INTPTR_MAX) return scm_true;
                if (id == INTPTR_MAX - 1) return scm_false;
#endif
                scm_bvector_t bvector = make_bvector(vm->m_heap, queue->buf.size(id));
                queue->buf.get(id, bvector->elts);
                scm_obj_t obj = deserializer_t(vm->m_heap).translate((scm_bvector_t)bvector);
                if (obj) return obj;
                invalid_serialized_object_violation(vm, "shared-queue-pop!", bvector, argc, argv);
                return scm_undef;
            }

        timeout:
            if (queue->queue.no_more_get()) return scm_shutdown;
            return scm_timeout;
        }
        wrong_type_argument_violation(vm, "shared-queue-pop!", 0, "shared queue", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shared-queue-pop!", 1, 2, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u shared-queue-pop! not supported on this build",__FILE__ , __LINE__);
#endif
}

// shared-queue-shutdown
scm_obj_t subr_shared_queue_shutdown(VM* vm, int argc, scm_obj_t argv[]) {
#if USE_PARALLEL_VM
    if (argc == 1) {
        if (SHAREDQUEUEP(argv[0])) {
            scm_sharedqueue_t queue = (scm_sharedqueue_t)argv[0];
            queue->queue.shutdown();
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "shared-queue-shutdown", 0, "shared queue", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shared-queue-shutdown", 1, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u shared-queue-shutdown not supported on this build",__FILE__ , __LINE__);
#endif
}

// make-shared-bag
scm_obj_t
subr_make_shared_bag(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 0) {
        scm_sharedbag_t bag = make_sharedbag(vm->m_heap, 1);
        return bag;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) {
            scm_sharedbag_t bag = make_sharedbag(vm->m_heap, FIXNUM(argv[0]));
            return bag;
        }
        wrong_type_argument_violation(vm, "make-shared-bag", 1, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-shared-bag", 0, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u make-shared-bag not supported on this build", __FILE__, __LINE__);
#endif
}

// shared-bag?
scm_obj_t
subr_shared_bag_pred(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1) return SHAREDBAGP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "shared-bag?", 1, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u shared-bag? not supported on this build", __FILE__, __LINE__);
#endif
}

// shared-bag-put!
scm_obj_t
subr_shared_bag_put(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 3 || argc == 4) {
        if (SHAREDBAGP(argv[0])) {
            if (STRINGP(argv[1])) {
                int timeout = 0;
                if (argc == 4) {
                    if (FIXNUMP(argv[3]) && FIXNUM(argv[3]) >= 0) {
                        timeout = FIXNUM(argv[3]);
                    } else {
                        wrong_type_argument_violation(vm, "shared-bag-put!", 3, "non-negative fixnum", argv[3], argc, argv);
                        return scm_undef;
                    }
                }
                scm_string_t string = (scm_string_t)argv[1];
                sharedbag_slot_t* slot = lookup_sharedbag((scm_sharedbag_t)argv[0], string->name, string->size);
                assert(slot);
#if CYCLIC_CHECK_BEFORE_SERIALIZE
                if (cyclic_objectp(vm->m_heap, argv[2])) {
                    serialize_cyclic_object_violation(vm, "shared-bag-put!", argv[2], argc, argv);
                    return scm_undef;
                }
#endif
                scm_obj_t obj = serializer_t(vm->m_heap).translate(argv[2]);
                if (BVECTORP(obj)) {
                    scm_bvector_t bvector = (scm_bvector_t)obj;
                    int id = slot->buf.put(bvector->elts, bvector->count);
                    if (slot->queue.wait_lock_try_put(id)) return scm_true;
                    if (argc == 4) {
                        vm->m_interp->update(vm, VM_STATE_BLOCK);
                        bool succ = slot->queue.put(id, timeout);
                        vm->m_interp->update(vm, VM_STATE_ACTIVE);
                        if (succ) return scm_true;
                        if (slot->queue.no_more_put()) return scm_shutdown;
                        return scm_timeout;
                    } else {
                        vm->m_interp->update(vm, VM_STATE_BLOCK);
                        bool succ = slot->queue.put(id);
                        vm->m_interp->update(vm, VM_STATE_ACTIVE);
                        return succ ? scm_true : scm_shutdown;
                    }
                }
                non_serializable_object_violation(vm, "shared-bag-put!", obj, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "shared-bag-put!", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "shared-bag-put!", 0, "shared bag", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shared-bag-put!", 3, 4, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u shared-bag-put! not supported on this build", __FILE__, __LINE__);
#endif
}

// shared-bag-get!
scm_obj_t
subr_shared_bag_get(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 2 || argc == 3) {
        if (SHAREDBAGP(argv[0])) {
            int timeout = 0;
            if (argc == 3) {
                if (FIXNUMP(argv[2]) && FIXNUM(argv[2]) >= 0) {
                    timeout = FIXNUM(argv[2]);
                } else {
                    wrong_type_argument_violation(vm, "shared-bag-get!", 2, "non-negative fixnum", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            scm_string_t string = (scm_string_t)argv[1];
            sharedbag_slot_t* slot = lookup_sharedbag((scm_sharedbag_t)argv[0], string->name, string->size);
            assert(slot);
            intptr_t id;
            bool succ;
            if (slot->queue.wait_lock_try_get(&id)) goto receive;
            if (argc == 3) {
                if (timeout == 0) goto timeout;
                vm->m_interp->update(vm, VM_STATE_BLOCK);
                succ = slot->queue.get(&id, timeout);
                vm->m_interp->update(vm, VM_STATE_ACTIVE);
                if (!succ) goto timeout;
            } else {
                vm->m_interp->update(vm, VM_STATE_BLOCK);
                succ = slot->queue.get(&id);
                vm->m_interp->update(vm, VM_STATE_ACTIVE);
                if (!succ) return scm_shutdown;
            }

        receive:
            {
                scm_bvector_t bvector = make_bvector(vm->m_heap, slot->buf.size(id));
                slot->buf.get(id, bvector->elts);
                scm_obj_t obj = deserializer_t(vm->m_heap).translate((scm_bvector_t)bvector);
                if (obj) return obj;
                invalid_serialized_object_violation(vm, "shared-bag-get!", bvector, argc, argv);
                return scm_undef;
            }

        timeout:
            if (slot->queue.no_more_get()) return scm_shutdown;
            return scm_timeout;
        }
        wrong_type_argument_violation(vm, "shared-bag-get!", 0, "shared bag", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shared-bag-get!", 1, 2, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u shared-bag-get! not supported on this build",__FILE__ , __LINE__);
#endif
}

// timeout-object?
scm_obj_t
subr_timeout_object_pred(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1) return argv[0] == scm_timeout ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "timeout-object?", 1, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u timeout-object? not supported on this build", __FILE__, __LINE__);
#endif
}

// shutdown-object?
scm_obj_t
subr_shutdown_object_pred(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1) return argv[0] == scm_shutdown ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "shutdown-object?", 1, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u shutdown-object? not supported on this build", __FILE__, __LINE__);
#endif
}

// serializable?
scm_obj_t
subr_serializable_pred(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1) {
        if (cyclic_objectp(vm->m_heap, argv[0])) return scm_false;
        return serializer_t(vm->m_heap).test(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "serializable?", 1, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u serializable? not supported on this build", __FILE__, __LINE__);
#endif
}

// local-heap-object?
scm_obj_t
subr_local_heap_object_pred(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (argc == 1) {
        if (CELLP(argv[0])) {
            return vm->m_heap->in_heap(argv[0]) ? scm_true : scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "local-heap-object?", 1, 1, argc, argv);
    return scm_undef;
#else
    fatal("%s:%u local-heap-object? not supported on this build", __FILE__, __LINE__);
#endif
}

void
init_subr_thread(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("spawn", subr_spawn);
    DEFSUBR("spawn-heap-limit", subr_spawn_heap_limit);
    DEFSUBR("spawn-timeout", subr_spawn_timeout);
    DEFSUBR("make-shared-queue", subr_make_shared_queue);
    DEFSUBR("shared-queue-shutdown", subr_shared_queue_shutdown);
    DEFSUBR("shared-queue-push!", subr_shared_queue_push);
    DEFSUBR("shared-queue-pop!", subr_shared_queue_pop);
    DEFSUBR("shared-queue?", subr_shared_queue_pred);
    DEFSUBR("timeout-object?", subr_timeout_object_pred);
    DEFSUBR("shutdown-object?", subr_shutdown_object_pred);
    DEFSUBR("make-shared-bag", subr_make_shared_bag);
    DEFSUBR("shared-bag?", subr_shared_bag_pred);
    DEFSUBR("shared-bag-put!", subr_shared_bag_put);
    DEFSUBR("shared-bag-get!", subr_shared_bag_get);
    DEFSUBR("on-primordial-thread?", subr_on_primordial_thread_pred);
    DEFSUBR("local-heap-object?", subr_local_heap_object_pred);
    DEFSUBR("display-thread-status", subr_display_thread_status);
    DEFSUBR("serializable?", subr_serializable_pred);
}
