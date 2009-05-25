/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "hash.h"
#include "list.h"
#include "arith.h"
#include "interpreter.h"
#include "vm.h"
#include "port.h"
#include "printer.h"

#define REPORT_REMEMBER_SET     0

#if USE_PARALLEL_VM

#define SPAWN_INITIAL_HEAP_SIZE (OBJECT_SLAB_SIZE * 16)
#define SPAWN_TIMEOUT_USLEEP    10000

void
Interpreter::init(VM* root, int n)
{
    if (n > MAX_VIRTUAL_MACHINE) n = MAX_VIRTUAL_MACHINE;
    m_lock.init();
    m_uuid_lock.init();
    m_remember_set.init(64);
    m_capacity = n;
    m_table = new vm_table_rec_t* [m_capacity];
    for (int i = 0; i < m_capacity; i++) {
        m_table[i] = new vm_table_rec_t;
        m_table[i]->interp = this;
        m_table[i]->notify.init();
        m_table[i]->state = VM_STATE_FREE;
        m_table[i]->name[0] = 0;
    }
    root->m_interp = this;
    root->m_parent = NULL;
    root->m_id = 0;
    root->m_child = 0;
    root->m_spawn_timeout = scm_false;
    root->m_spawn_heap_limit = DEFAULT_HEAP_LIMIT * 1024 * 1024;
    m_table[0]->interp = this;
    m_table[0]->state = VM_STATE_ACTIVE;
    m_table[0]->vm = root;
    m_table[0]->parent = VM_PARENT_NONE;
    m_table[0]->param = scm_nil;
    snprintf(m_table[0]->name, sizeof(m_table[0]->name), "<root>");
    m_live = 1;
}

void
Interpreter::destroy()
{
    m_lock.destroy();
    m_uuid_lock.destroy();
    delete [] m_table;
}

int
Interpreter::spawn(VM* parent, scm_closure_t func, int argc, scm_obj_t argv[])
{
    double timeout = (FIXNUMP(parent->m_spawn_timeout)) ? msec() + FIXNUM(parent->m_spawn_timeout) : 0.0;
again:
    {
        scoped_lock lock(m_lock);
        for (int i = 0; i < m_capacity; i++) {
            if (m_table[i]->state == VM_STATE_FREE) {
                object_heap_t* heap = new object_heap_t;
                int heap_init = SPAWN_INITIAL_HEAP_SIZE;
                int heap_limit = parent->m_spawn_heap_limit;
                if (heap_limit <= heap_init + heap_init) heap_limit = heap_init + heap_init;
                heap->init_child(heap_limit, heap_init, parent->m_heap);
                VM* vm = new VM;
                vm->m_heap = heap;
                vm->m_stack_size = VM_STACK_BYTESIZE;
                vm->m_stack_top = (scm_obj_t*)vm->m_heap->allocate(vm->m_stack_size, false, false);
                vm->m_stack_limit = (scm_obj_t*)((intptr_t)vm->m_stack_top + vm->m_stack_size);
                memset(vm->m_stack_top, 0, vm->m_stack_size);
                vm->m_to_stack_top = (scm_obj_t*)vm->m_heap->allocate(vm->m_stack_size, false, false);
                vm->m_to_stack_limit = (scm_obj_t*)((intptr_t)vm->m_to_stack_top + vm->m_stack_size);
                memset(vm->m_to_stack_top, 0, vm->m_stack_size);
                vm->m_interp = parent->m_interp;
                vm->m_parent = parent;
                vm->m_id = i;
                vm->m_child = 0;
                vm->m_spawn_timeout = parent->m_spawn_timeout;
                vm->m_spawn_heap_limit = parent->m_spawn_heap_limit;
                vm->m_bootport = (scm_port_t)scm_unspecified;
                vm->m_current_environment = parent->m_current_environment;
                vm->m_current_input = parent->m_current_input;
                vm->m_current_output = parent->m_current_output;
                vm->m_current_error = parent->m_current_error;
                vm->m_current_source_comments = scm_false;
                vm->m_current_exception_handler = scm_false;
                vm->m_current_dynamic_environment = clone_weakhashtable(vm->m_heap, parent->m_current_dynamic_environment, false);
                vm->m_current_dynamic_wind_record = scm_nil;
                vm->m_recursion_level = 0;
                vm->m_shared_object_errno = 0;
                vm->m_shared_object_win32_lasterror = 0;
                memcpy(&vm->flags, &parent->flags, sizeof(parent->flags));
                vm->run(true);
                vm->reset();
                for (int n = 0; n < argc; n++) vm->m_sp[n] = argv[n];
                vm->m_sp += argc;
                vm_env_t env = (vm_env_t)vm->m_sp;
                env->count = argc;
                env->up = func->env;
                vm->m_sp = vm->m_fp = (scm_obj_t*)(env + 1);
                vm->m_pc = func->code;
                vm->m_env = &env->up;
                scm_obj_t context = scm_nil;
                if (argc > 0) {
                    for (int n = argc - 1; n >= 0; n--) context = make_pair(parent->m_heap, argv[n], context);
                    context = make_pair(parent->m_heap, func, context);
                } else {
                    context = make_list(parent->m_heap, 1, func);
                }
                m_table[i]->param = make_list(parent->m_heap,
                                              5,
                                              context,
                                              parent->m_current_environment,
                                              parent->m_current_input,
                                              parent->m_current_output,
                                              parent->m_current_error);
                m_table[i]->parent = parent->m_id;
                m_table[i]->vm = vm;
                m_table[i]->id = i;
                m_table[i]->state = VM_STATE_ACTIVE;
                if (func->doc == scm_nil) {
                    snprintf(m_table[i]->name, sizeof(m_table[i]->name), "[%p]", func);
                } else {
                    const char* name = "";
                    if (SYMBOLP(func->doc)) name = ((scm_symbol_t)func->doc)->name;
                    if (STRINGP(func->doc)) name = ((scm_string_t)func->doc)->name;
                    snprintf(m_table[i]->name, sizeof(m_table[i]->name), "%s", name);
                }
                m_live = m_live + 1;
                parent->m_child++;
                thread_start(mutator_thread, m_table[i]);
                return i;
            }
        }
    }
    if (timeout == 0.0 || timeout > msec()) {
        usleep(SPAWN_TIMEOUT_USLEEP);
        goto again;
    }
    return -1;
}

thread_main_t
Interpreter::mutator_thread(void* param)
{
    vm_table_rec_t* table_rec = (vm_table_rec_t*)param;
    Interpreter* interp = table_rec->interp;
    VM* vm = table_rec->vm;
    set_current_vm(vm);

loop:
    try {
        vm->run(false);
    } catch (vm_exit_t& e) {
        exit(e.m_code);
    } catch (vm_exception_t& e) {
        vm->backtrace(vm->m_current_error);
    } catch (io_exception_t& e) {
        if (e.m_err == EINTR) goto loop;
        if (e.m_err == EIO) goto loop;
        fatal("fatal in thread(0x%x): unexpected io_expecption_t(%d, %s)", vm, e.m_err, e.m_message);
    } catch (reader_exception_t& e) {
        fatal("fatal in thread(0x%x): unhandled exception reader_expecption_t(%s)", vm, e.m_message);
    } catch (io_codec_exception_t& e) {
        fatal("fatal in thread(0x%x): unhandled exception io_codec_exception_t(%d, %s)", vm, e.m_operation, e.m_message);
    } catch (vm_escape_t& e) {
        fatal("fatal in thread(0x%x): unhandled exception vm_escape_t, maybe (escape) procedure in bad context", vm);
    } catch (vm_continue_t& e) {
        fatal("fatal in thread(0x%x): unhandled exception vm_continue_t, maybe (escape) procedure in bad context", vm);
    } catch (int code) {
        fatal("fatal in thread(0x%x): unexpected exception (errno %d, %s)", vm, code, strerror(code));
    } catch (...) {
        fatal("fatal in thread(0x%x): unknown exception", vm);
    }
    vm->m_heap->m_collector_lock.lock();
    vm->m_heap->m_collector_terminating = true;
    vm->m_heap->m_collector_wake.signal();
    vm->m_heap->m_collector_lock.unlock();
    while (true) {
        vm->m_heap->m_collector_lock.lock();
        if (vm->m_heap->m_collector_terminating == false) {
            vm->m_heap->m_collector_lock.unlock();
            break;
        }
        while (vm->m_heap->m_stop_the_world) {
            vm->m_heap->m_mutator_stopped = true;
            vm->m_heap->m_collector_wake.signal();
            vm->m_heap->m_mutator_wake.wait(vm->m_heap->m_collector_lock);
            vm->m_heap->m_mutator_stopped = false;
        }
        vm->m_heap->m_collector_wake.signal();
        vm->m_heap->m_collector_lock.unlock();
        thread_yield();
    }
    {
        scoped_lock lock(interp->m_lock);
        interp->m_table[table_rec->parent]->vm->m_child--;
        interp->m_remember_set.clear(1 << vm->m_id);

    wait_again:
        for (int i = 0; i < interp->m_capacity; i++) {
            switch (interp->m_table[i]->state) {
            case VM_STATE_FREE:
                break;
            case VM_STATE_ACTIVE:
            case VM_STATE_BLOCK:
            case VM_STATE_SYNC:
                if (interp->m_table[i]->parent == table_rec->id) {
                    table_rec->state = VM_STATE_SYNC;
                    table_rec->notify.wait(interp->m_lock);
                    goto wait_again;
                }
                break;
            }
        }
    }
    vm->m_heap->destroy();
    delete vm->m_heap;
    delete vm;
    interp->m_lock.lock();
    table_rec->state = VM_STATE_FREE;
    interp->m_live = interp->m_live - 1;
    interp->m_table[table_rec->parent]->notify.signal();
    interp->m_lock.unlock();
    return NULL;
}

void
Interpreter::update(VM* vm, int state)
{
    Interpreter* interp = vm->m_interp;
    scoped_lock lock(interp->m_lock);
    vm_table_rec_t** table = interp->m_table;
    for (int i = 0; i < interp->m_capacity; i++) {
        vm_table_rec_t* rec = table[i];
        if (rec->vm == vm) {
            rec->state = state;
            break;
        }
    }
}

void
Interpreter::display_status(VM* vm)
{
    scm_port_t port = vm->m_current_output;
    scoped_lock lock1(port->lock);
    Interpreter* interp = vm->m_interp;
    scoped_lock lock2(interp->m_lock);
    vm_table_rec_t** table = interp->m_table;
    int name_pad = 8;
    for (int i = 0; i < interp->m_capacity; i++) {
        vm_table_rec_t* rec = table[i];
        int n = strlen(rec->name);
        if (n > name_pad) name_pad = n;
    }
#if ARCH_LP64
    port_puts(port, "\n  VM ADRS           STATUS CT ID");
#else
    port_puts(port, "\n  VM ADRS       STATUS CT ID");
#endif
    for (int c = 0; c < name_pad - 2; c++) port_puts(port, " ");
    port_puts(port, "  MEM\n");
    for (int i = 0; i < interp->m_capacity; i++) {
        vm_table_rec_t* rec = table[i];
        const char* stat = "unknown";
        scm_obj_t param = scm_nil;
        switch (rec->state) {
            case VM_STATE_FREE:
                continue;
            case VM_STATE_ACTIVE:
                stat = "active";
                param = rec->param;
                break;
            case VM_STATE_BLOCK:
                stat = "block";
                param = rec->param;
                break;
            case VM_STATE_SYNC:
                stat = "wait";
                param = rec->param;
                break;
            default: break;
        }
        port_format(port, "  %2d 0x%08lx %-6s %2d %s", i, rec->vm, stat, rec->vm->m_child, rec->name);
        int pad = name_pad - strlen(rec->name);
        for (int c = 0; c < pad; c++) port_puts(port, " ");
        port_format(port, "  %d/%dM", rec->vm->m_heap->m_pool_watermark * OBJECT_SLAB_SIZE / 1024 / 1024, rec->vm->m_heap->m_pool_size / 1024 / 1024);
        port_puts(port, "\n");
    }
#if REPORT_REMEMBER_SET
    m_remember_set.display_status(vm);
#else
    port_puts(port, "\n");
#endif
    }

void
Interpreter::snapshot(VM* vm, bool retry)
{
    scoped_lock lock(m_lock);
    int id = vm->m_id;
    for (int i = 0; i < m_capacity; i++) {
        switch (m_table[i]->state) {
            case VM_STATE_ACTIVE:
            case VM_STATE_BLOCK:
            if (m_table[i]->parent == id) vm->m_heap->enqueue_root(m_table[i]->param);
            break;
        }
    }
    for (int i = 0; i < m_capacity; i++) {
        if (m_table[i]->state == VM_STATE_FREE) continue;
        if (m_table[i]->parent == id) {
            m_remember_set.snapshot(vm, retry);
            return;
        }
    }
}

bool
Interpreter::primordial(int id)
{
    scoped_lock lock(m_lock);
    return (m_table[id]->parent == VM_PARENT_NONE);
}

void
Interpreter::set_thread_name(int id, const char* name)
{
    scoped_lock lock(m_lock);
    strncpy(m_table[id]->name, name, sizeof(m_table[id]->name));
}

void
Interpreter::get_thread_name(int id, char* name, int len)
{
    scoped_lock lock(m_lock);
    strncpy(name, m_table[id]->name, len);
}

static bool
subset_of_list(scm_obj_t lst, scm_obj_t elt)
{
    if (lst == scm_nil) return (elt == scm_nil);
    scm_obj_t fast = lst;
    scm_obj_t slow = fast;
    while (PAIRP(fast)) {
        if (fast == elt) return true;
        fast = CDR(fast);
        if (PAIRP(fast)) {
            if (fast == elt) return true;
            fast = CDR(fast);
            slow = CDR(slow);
            if (slow == fast) return false;
        } else {
            return false;
        }
    }
    return false;
}

void
Interpreter::remember(scm_obj_t lhs, scm_obj_t rhs)
{
    assert(lhs);
    if (CELLP(lhs)) {
        if (lhs == rhs) return;
        if (PAIRP(rhs)) {
            if (CDR(rhs) == lhs) return; // eliminate if rhs == (x . lhs)
            if (PAIRP(lhs) && subset_of_list(rhs, lhs)) return;  // eliminate if rhs == (x ... lhs)
        }
        scoped_lock lock(m_lock);
        uint32_t bits = 0;
        for (int i = 0; i < m_capacity; i++) {
            switch (m_table[i]->state) {
            case VM_STATE_ACTIVE:
            case VM_STATE_BLOCK:
                if (m_table[i]->parent != VM_PARENT_NONE) bits |= (1 << i);
                break;
            }
        }
        if (bits) m_remember_set.set(lhs, bits);
    }
}

remember_set_t::remember_set_t()
{
    m_elts = NULL;
}

remember_set_t::~remember_set_t()
{
    free(m_elts);
}

void
remember_set_t::init(int n)
{
    m_count = lookup_mutable_hashtable_size(n);
    m_live = 0;
    m_elts = (element_t*)malloc(sizeof(element_t) * m_count);
    for (int i = 0; i < m_count; i++) {
        m_elts[i].obj = scm_hash_free;
        m_elts[i].bits = 0;
    }
}

void
remember_set_t::set(scm_obj_t obj, uint32_t bits)
{
    int hash1 = address_hash1(obj, m_count);
    int hash2 = address_hash2(obj, m_count);
    int index = hash1;
    do {
        scm_obj_t tag = m_elts[index].obj;
        if (tag == obj) {
            m_elts[index].bits |= bits;
            return;
        }
        if (tag == scm_hash_free || tag == scm_hash_deleted) {
            m_elts[index].obj = obj;
            m_elts[index].bits = bits;
            m_live++;
            if (m_live >= HASH_BUSY_THRESHOLD(m_count)) rehash(lookup_mutable_hashtable_size(m_count));
            return;
        }
        index += hash2;
        if (index >= m_count) index -= m_count;
    } while (index != hash1);
    fatal("%s:%u remember set overflow", __FILE__, __LINE__);
}

void
remember_set_t::clear(uint32_t bits)
{
    int n = m_live;
    for (int i = 0; i < m_count; i++) {
        scm_obj_t tag = m_elts[i].obj;
        if (tag == scm_hash_free || tag == scm_hash_deleted) continue;
        m_elts[i].bits &= (~bits);
        if (m_elts[i].bits == 0) {
            m_elts[i].obj = scm_hash_deleted;
            m_live--;
        }
    }
    if (n != m_live) rehash(lookup_mutable_hashtable_size(m_live));
}

void
remember_set_t::rehash(int ncount)
{
    assert(ncount >= m_live);
    int save_count = m_count;
    element_t* save_elts = m_elts;
    m_count = ncount;
    m_elts = (element_t*)malloc(sizeof(element_t) * m_count);
    for (int i = 0; i < m_count; i++) {
        m_elts[i].obj = scm_hash_free;
        m_elts[i].bits = 0;
    }
    for (int i = 0; i < save_count; i++) {
        scm_obj_t obj = save_elts[i].obj;
        if (obj == scm_hash_free) continue;
        if (obj == scm_hash_deleted) continue;
        int hash1 = address_hash1(obj, m_count);
        int hash2 = address_hash2(obj, m_count);
        int index = hash1;
        do {
            if (m_elts[index].obj == scm_hash_free) {
                m_elts[index] = save_elts[i];
                break;
            }
            index += hash2;
            if (index >= m_count) index -= m_count;
            assert(index != hash1);
        } while (true);
    }
    free(save_elts);
}

void
remember_set_t::snapshot(VM* vm, bool retry)
{
    int n = m_live;
    for (int i = 0; i < m_count; i++) {
        scm_obj_t obj = m_elts[i].obj;
        if (m_elts[i].obj != scm_hash_free && m_elts[i].obj != scm_hash_deleted) {
            if (vm->m_heap->in_heap(obj)) {
                if (retry) {
                    vm->m_heap->enqueue_root(obj);
                    continue;
                }
                if (OBJECT_SLAB_TRAITS_OF(obj)->cache->state(obj)) {
                    m_elts[i].obj = scm_hash_deleted;
                    m_live--;
                    continue;
                }
                vm->m_heap->enqueue_root(obj);
            }
        }
    }
    if (n != m_live) rehash(lookup_mutable_hashtable_size(m_live));
}

void
remember_set_t::display_status(VM* vm)
{
    scm_port_t port = vm->m_current_output;
    scoped_lock lock(port->lock);
    int n = 0;
    for (int i = 0; i < m_count; i++) {
        if (m_elts[i].obj == scm_hash_free) continue;
        if (m_elts[i].obj == scm_hash_deleted) continue;
        n++;
    }
    port_format(port, "\n   [%d objects in remember set]\n", n);
}

/*
  Reference: A Universally Unique IDentifier (UUID) URN Namespace
             http://www.ietf.org/rfc/rfc4122.txt

      UUID                   = time-low "-" time-mid "-"
                               time-high-and-version "-"
                               clock-seq-and-reserved
                               clock-seq-low "-" node
      time-low               = 4hexOctet
      time-mid               = 2hexOctet
      time-high-and-version  = 2hexOctet
      clock-seq-and-reserved = hexOctet
      clock-seq-low          = hexOctet
      node                   = 6hexOctet
      hexOctet               = hexDigit hexDigit
      hexDigit =
            "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" /
            "a" / "b" / "c" / "d" / "e" / "f" /
            "A" / "B" / "C" / "D" / "E" / "F"
*/

static uint32_t rand8()
{
    uint32_t a = random();
    return (a & 0xff);
}

static uint32_t rand16()
{
    uint32_t a = random();
    uint32_t b = random();
    return ((a & 0xff) << 8) + (b & 0xff);
}

static uint32_t rand32()
{
    uint32_t a = random();
    uint32_t b = random();
    uint32_t c = random();
    return ((a & 0x3ff) << 22) + ((b & 0x7ff) << 11) + (c & 0x7ff);
}

static uint64_t rand48()
{
    uint64_t a = random();
    uint64_t b = random();
    uint64_t c = random();
    uint64_t d = random();
    return ((a & 0xfff) << 36) + ((b & 0xfff) << 24) + ((c & 0xfff) << 12) + (d & 0xfff);
}

void
Interpreter::generate_uuid(char* buf, int bufsize) // version 4
{
    assert(bufsize > 36);
    scoped_lock lock(m_uuid_lock);
    uint32_t time_low;                      // octet[0-3]
    uint16_t time_mid;                      // octet[4-5]
    uint16_t time_hi_and_version;           // octet[6-7]
    uint8_t clock_seq_hi_and_reserved;      // octet[8]
    uint8_t clock_seq_lo;                   // octet[9]
    uint64_t node;                          // octed[10-15]
    time_low = rand32();
    time_mid = rand16();
    time_hi_and_version = rand16();
    clock_seq_hi_and_reserved = rand8();
    clock_seq_lo = rand8();
    node = rand48();
    uint16_t version = 0x4000;              // octet[6] 0100 xxxx octet[7] xxxx xxxx
    time_hi_and_version = version + (time_hi_and_version & 0xfff);
    uint8_t variant = 0x80;                 // octet[8] 100x xxxx
    clock_seq_hi_and_reserved = variant + (clock_seq_hi_and_reserved & 0x1f);
    snprintf(buf,
             bufsize,
             "%08x-%04x-%04x-%02x%02x-%04x%08x",
             time_low,
             time_mid,
             time_hi_and_version,
             clock_seq_hi_and_reserved,
             clock_seq_lo,
             (uint32_t)(node >> 32),
             (uint32_t)node);
}

#endif
