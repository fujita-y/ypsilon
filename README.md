# Ypsilon: R7RS/R6RS Scheme Implementation

* Conforms R7RS/R6RS
* Mostly concurrent garbage collection for remarkably shorter GC pause time
* Compilation thread incrementally generates native code in the background
* On-the-fly FFI with native stub code generation

See [LICENSE](https://github.com/fujita-y/ypsilon/blob/master/LICENSE) for terms and conditions of use.

### Versions

Ypsilon version 2 have major design changes. Former version is available at [version-0.9](https://github.com/fujita-y/ypsilon/tree/version-0.9).

### Requirements

LLVM 10

### Build and Install

```
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
cmake --install .
```

### Run

* To run R7RS sample script, try following from project root:
```
ypsilon --r7rs --top-level-program --disable-acc -- test/r7rs-sample.scm
```

* To run FFI demo program, try follwing from project root:
```
ypsilon --r6rs --top-level-program demo/glut-demo.scm # (OpenGL 1.x, GLUT)
ypsilon --r6rs --top-level-program demo/glfw-demo.scm # (OpenGL 1.x, GLFW)
ypsilon --r6rs --top-level-program demo/glcorearb-demo.scm # (OpenGL Core Profile, GLFW)
ypsilon --r6rs --top-level-program demo/freetype-demo.scm # (OpenGL Core Profile, GLFW, freetype)
ypsilon --r6rs --top-level-program demo/widget-demo.scm # (OpenGL Core Profile, GLFW, freetype)
```

### Notes

* REPL start with '(import (core))' regardless what command line option is given.
* Without '--top-level-program', the contents of the specified script file will be interpreted as if they had been entered into the REPL.

### Rebuild Heap Files

* Edit 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* ```make```
* Edit .scm files in 'heap/boot' directory
* ```cd heap; make; cd ..; make; cd heap; make; cd ..```
* Edit 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '0'
* ```make```


*This project is migrated from code.google.com/p/ypsilon*