# Ypsilon: R7RS/R6RS Scheme Implementation

* Conforms to R7RS/R6RS
* Mostly concurrent garbage collector optimized for multi-core processor
* Incremental native code generation with background compilation threads
* On-the-fly FFI with native stub code generation
* Full features of R6RS and R6RS standard libraries including:
  * arbitrary precision integer arithmetic
  * rational number
  * exact and inexact complex number
  * top-level program
  * proper tail recursion
  * call/cc and dynamic wind
  * unicode
  * bytevectors
  * records
  * exceptions and conditions
  * i/o
  * syntax-case
  * hashtables
  * enumerations
* Full features of R7RS-small and following R7RS-large libraries:
  *  (scheme comparator)
  *  (scheme hash-table)
  *  (scheme list)
  *  (scheme sort)

* See [LICENSE](https://github.com/fujita-y/ypsilon/blob/master/LICENSE) for terms and conditions of use.

### Performance

* Ypsilon 2.0.8 outperform Guile 3.0.8 in [r7rs-benchmarks](https://github.com/ecraven/r7rs-benchmarks) for 31 of 55 programs on WSL2, Ubuntu 20.04, Ryzen 3700X 3.6GHz. https://fujita-y.github.io/benchmarks/ubuntu-llvm12-ryzen3700x-ypsilon-208-guile-308.pdf

### Documents

* [API Reference](https://fujita-y.github.io/ypsilon-api/)

### Requirements

* LLVM 10, 11, 12, 13, 14, 15 or 19

### Installation

```
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
cmake --install .
```

- On MacOS, you may want using [Homebrew](https://brew.sh/) to install LLVM 15.
```
brew update
brew install llvm
echo 'export PATH="/opt/homebrew/opt/llvm/bin:$PATH"' >> ~/.zshrc
```

### Optional

* You may want using [rlwrap](https://github.com/hanslub42/rlwrap) to make the REPL easier to use.
```bash
# Ubuntu 21.04
apt install rlwrap
```
```bash
# Homebrew on MacOS
brew install rlwrap
```

### Run

* To run R7RS sample script, try following from project root:
```
ypsilon --r7rs test/r7rs-sample.scm
```

* To run FFI demo program, try follwing from project root:
```
ypsilon --r6rs --top-level-program demo/glut-demo.scm # (OpenGL 1.x, GLUT)
ypsilon --r6rs --top-level-program demo/glfw-demo.scm # (OpenGL 1.x, GLFW)
ypsilon --r6rs --top-level-program demo/glcorearb-demo.scm # (OpenGL Core Profile, GLFW)
ypsilon --r6rs --top-level-program demo/freetype-demo.scm # (OpenGL Core Profile, GLFW, freetype)
ypsilon --r6rs --top-level-program demo/widget-demo.scm # (OpenGL Core Profile, GLFW, freetype)
```

### Docker

* [Container image](https://hub.docker.com/r/fujitay/ypsilon) is available on Docker Hub for amd64 and arm64.
```
$ docker run --rm -it fujitay/ypsilon:latest ypsilon --r7rs
ypsilon-2.0.8 (r7rs)
> (magnitude 3/22+2/11i)
5/22
> (exit)
```

### Packages

[![Packaging status](https://repology.org/badge/vertical-allrepos/ypsilon.svg)](https://repology.org/project/ypsilon/versions)

### Notes

* REPL start with ```(import (scheme base) (scheme process-context))``` or ```(import (rnrs base) (rnrs programs))``` depending on the command line options specified.
* Without ```--top-level-program``` option, the contents of the specified script file will be interpreted as if they had been entered into the REPL.

### Rebuild heap files

* Open 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* ```make```
* Edit .scm files in 'heap/boot' directory
* ```cd heap; make; cd ..; make; cd heap; make; cd ..```
* Open 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '0'
* ```make```

### Versions

* Ypsilon version 2 have major design changes. Former version is available at [version-0.9](https://github.com/fujita-y/ypsilon/tree/version-0.9).
* This project is migrated from code.google.com/p/ypsilon

# Behind the scenes: Concurrent Garbage Collector

Ypsilon implements a **Concurrent Mark-Sweep (CMS)** garbage collector designed for low-latency, real-time performance. It minimizes "stop-the-world" (STW) pauses by performing most of the marking and sweeping work concurrently with the mutator (the Scheme program).

## Heap Architecture

The heap is organized into a hierarchy of structures to support efficient allocation and concurrent GC:

### 1. Concurrent Pool (`concurrent_pool_t`)
The top-level memory manager that allocates large chunks of memory from the OS. It manages the overall heap layout and provides memory for slabs and large objects.

### 2. Tag Directory
The first few slabs of the pool are reserved for a "tag directory." Each byte in this directory stores the attributes (tags) of a corresponding slab in the pool.
- **PTAG_FREE**: Slab is available for allocation.
- **PTAG_USED**: Slab is in use.
- **PTAG_SLAB**: Slab is managed by a `concurrent_slab_t`.
- **PTAG_GC**: Slab contains collectible objects managed by the GC.
- **PTAG_EXTENT**: Used for multi-slab objects (large objects).

### 3. Concurrent Slab (`concurrent_slab_t`)
Small objects are allocated from "slabs" (typically 4KB or 8KB). Each slab contains multiple objects of a fixed size.
- **Slab Traits**: Metadata located at the end of each slab containing free lists and cache references.
- **Mark Bitmap**: A bitmap stored within the slab (just before the traits) that tracks the mark state of each object. One bit per object: `0` for unmarked (white), `1` for marked (gray/black).

### 3. Object Layout
Ypsilon uses tagged pointers to distinguish between immediate values (like fixnums) and heap-allocated cells. The GC only tracks "collectible" pointers.

---

## GC Cycle and Phases

A typical GC cycle consists of the following phases:

### 1. Initiation
The GC is triggered when its allocation trip-byte threshold is reached. A dedicated **collector thread** manages the cycle.

### 2. Root Snapshotting (STW)
To start marking safely, the GC must capture the "roots" (global variables, thread stacks, etc.). 
- It uses a "stop-the-world" pause to snapshot global roots.
- It resumes the mutator and performs concurrent marking.
- Later, it pauses again to snapshot stack-based local roots.
- This staggered approach minimizes the duration of each individual pause.

### 3. Concurrent Marking
The collector thread performs a Depth-First Search (DFS) of the object graph using a **mark stack**.
- **Tri-color Marking**: 
  - **White**: Unmarked objects (bit `0`).
  - **Gray**: Marked (bit `1`) but children haven't been traced. These are currently in the `mark_stack` or `shade_queue`.
  - **Black**: Marked (bit `1`) and children have been traced.
- **Write Barrier (Dijkstra-style)**: When the mutator stores a pointer to an object `B` into object `A`, a write barrier is triggered if the GC is in the marking phase. If `B` is not yet marked, it is "shaded" (added to a `shade_queue`) to ensure it isn't missed even if it was moved during concurrent marking.

### 4. Final Mark (STW)
A final STW phase ensures that all objects reachable via roots and the `shade_queue` are fully traced. Ypsilon includes "Ensured Real-time" logic: if the final mark takes too long, it can resume the mutator and return to concurrent marking to avoid long pauses.

### 5. Concurrent Sweeping
Once marking is complete, the GC begins reclaiming memory.
- **Sweep Wavefront**: The collector iterates through slabs, resetting free lists for unmarked objects. It maintains a "wavefront" pointer to track progress through the heap.
- **Alloc Barrier**: If the mutator allocates a new object *ahead* of the sweep wavefront, the object is immediately marked. This prevents it from being incorrectly reclaimed by the ongoing sweep.
- **Read Barrier**: Used for global sets (like the symbol table). If the mutator retrieves an object from such a set during the sweep phase, it is marked to ensure visibility.

---

## Special Features

### Weak References
Objects of type `TC_WEAKMAPPING` are handled specially. The collector only marks the value if the key is already marked. If the key is not marked by the end of the marking phase, the mapping is broken.

### Finalization
The GC supports object finalization. Objects requiring finalization are traced and, if found unreachable, are added to a finalization queue instead of being immediately swept.

### Compaction
While the primary GC is mark-sweep, Ypsilon also includes a **compactor** (`object_heap_compact.cpp`) that can relocate objects to reduce fragmentation. This is typically used during heap expansion or as a fallback when fragmentation becomes high.

---

## Concurrency Control

The GC and mutator interact via highly optimized primitives:
- **Shade Queue**: A lock-free-style queue used by the write barrier to communicate new roots found by the mutator to the collector.
- **Slab Locks**: Each `concurrent_slab_t` has its own mutex to synchronize allocation and sweeping.
- **Condition Variables**: Used for coordination between the collector thread and mutator threads during STW phases.

# Behind the scenes: Native Code Generation (JIT)

Ypsilon includes a Just-In-Time (JIT) compiler named **Digamma** that translates Scheme bytecode into high-performance native machine code using the **LLVM** backend.

## Overview

The JIT compiler works by translating the Virtual Machine's instruction sequence (bytecode) of a closure into LLVM Intermediate Representation (IR). This IR is then optimized and compiled into native code by LLVM's ORCJIT execution engine.

## Compilation Strategy

### 1. Concurrent Compilation
Native code generation is decoupled from the main VM execution thread.
- **Codegen Thread**: A dedicated thread (`codegen_thread` in `digamma.cpp`) waits for compilation requests.
- **Queueing**: When the VM encounters a closure that is a candidate for compilation (e.g., during on-demand execution or AOT pre-scanning), it pushes the closure onto a `codegen_queue`.
- **Background Work**: The codegen thread picks up closures from the queue, generates LLVM IR, applies optimizations, and registers the resulting native thunk with the closure.

### 2. Translation (VM to LLVM IR)
The `digamma_t::transform` method is the heart of the compiler. It performs a single-pass translation of VM instructions:
- **Instruction Mapping**: Each VM opcode (e.g., `VMOP_PUSH`, `VMOP_CALL`, `VMOP_IF_TRUE`) has a corresponding `emit_*` method (e.g., `emit_push`, `emit_call`, `emit_if_true`) that generates the equivalent LLVM IR.
- **Register Caching**: Frequent VM "registers" (like `m_sp`, `m_fp`, `m_env`) are cached in LLVM values during the lifetime of a function to minimize memory accesses.
- **Stack Management**: The JIT-generated code still uses the VM's global stack. It includes explicit `emit_stack_overflow_check` calls to ensure safe stack growth by calling back into the VM's `collect_stack`.

### 3. Optimization
Ypsilon utilizes LLVM's O2 optimization pipeline (`optimizeModule` in `digamma.cpp`). This includes:
- Constant folding
- Dead code elimination
- Loop optimizations
- Register allocation (handled by LLVM)

## Native Thunk Protocol

Once a closure is compiled, its `code` field is set to the address of a "native thunk." The VM executes this thunk directly:

```cpp
intptr_t (*thunk)(intptr_t) = (intptr_t (*)(intptr_t))closure->code;
intptr_t n = (*thunk)((intptr_t)vm);
```

The thunk returns a status code (protocol) that tells the VM how to proceed after the native execution:
- `native_thunk_pop_cont`: The function finished; pop the continuation.
- `native_thunk_apply`: A new application is required (e.g., a tail call to an uncompiled closure).
- `native_thunk_loop`: Continue interpretation from the updated PC.
- `native_thunk_escape`: Exit the VM loop.

## Integration with VM

### C Helpers (`extern "C"`)
Since native code needs to perform complex tasks like allocation, error reporting, and arithmetic on complex types, `digamma.cpp` provides a set of `c_*` helper functions with C linkage. These functions are called from the LLVM-generated code to safely interact with the VM's C++ internals.

### Lazy Compilation
Ypsilon uses a multi-level execution strategy:
1.  **Interpretation**: Closures start by being interpreted by the VM.
2.  **On-demand Compilation**: If a closure is called, it is added to the codegen queue.
3.  **Native Execution**: Once compilation is complete, subsequent calls use the native thunk.

## Configuration

The JIT is controlled by several macros in `core.h`:
- `ENABLE_LLVM_JIT`: Master toggle for the JIT compiler.
- `COMPILE_THREAD_COUNT`: Number of concurrent compilation threads (default is 4).
- `USE_LLVM_OPTIMIZE`: Whether to run the LLVM optimization passes.

# Behind the scenes: Foreign Function Interface (FFI)

Ypsilon provides a powerful Foreign Function Interface (FFI) that allows Scheme programs to call C functions and be called by C code (callbacks). It leverages the **LLVM** backend to dynamically generate high-efficiency transition code (thunks).

## Core Concepts

### 1. Callouts (Scheme calling C)
A "callout" is a Scheme procedure that, when invoked, executes a native C function.
- **Dynamic Codegen**: Ypsilon uses LLVM to JIT-compile a custom "callout thunk" for each unique function signature.
- **Signature Mapping**: Signatures are defined using single-character type codes (e.g., `'i'` for void, `'q'` for 32-bit int, `'o'` for 64-bit int, `'x'` for double).
- **Marshalling**: The generated thunk automatically handles the conversion between Scheme objects (fixnums, flonums, bytevectors) and native C types.

### 2. Callbacks (C calling Scheme)
A "callback" is a C function pointer that, when executed by C code, invokes a Scheme closure.
- **Trampolines**: Ypsilon generates a native entry point (trampoline) using LLVM.
- **Context Preservation**: The trampoline ensures that the VM state is correctly passed back into the Scheme environment.
- **Thread Safety**: Callbacks are tracked via a `m_trampolines` hashtable in the object heap to ensure they correctly map to the intended Scheme closure.

### 3. Type Mapping
Ypsilon supports various C types via signature strings:
- `b`: bool (bool in C, `#t`/`#f` in Scheme)
- `u`: int8_t
- `d`: int16_t
- `q`: int32_t
- `o`: int64_t
- `s`: float
- `x`: double
- `i`: void (only as a return type)

## Native Data Access

Since C functions often work with raw pointers, Ypsilon provides integrated support through **Bytevectors**:
- **Automatic Marshalling**: When a bytevector is passed to a C function expecting a pointer, the FFI automatically passes the address of the bytevector's internal data.
- **Alignment**: Bytevector accessors support both native-endian and specific-endian (little/big) operations, which is crucial for interacting with C structs.

## Low-level Interface

The primary internal procedures for FFI (found in `subr_c_ffi.cpp`) include:
- `codegen-cdecl-callout`: Generates a Scheme subr that calls a C function at a specific address.
- `codegen-cdecl-callback`: Generates a C-compatible function pointer that wraps a Scheme closure.
- `c-main-argc` / `c-main-argv`: Provide access to the program's command-line arguments as raw C values.

## Safety and Performance

- **Error Handling**: The FFI includes `try-catch` blocks around callback execution to prevent unhandled Scheme exceptions from corrupting the C caller's stack.
- **JIT Efficiency**: By generating native code specific to the function's signature, Ypsilon avoids the overhead of interpreted marshalling found in many other FFI implementations.
- **GC Integration**: The FFI correctly interacts with Ypsilon's concurrent GC. For example, when pointers are stored or retrieved, appropriate write barriers are applied where necessary.
