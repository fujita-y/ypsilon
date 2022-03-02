# Ypsilon: R7RS/R6RS Scheme Implementation

* Conforms R7RS/R6RS
* Mostly concurrent garbage collection for remarkably shorter GC pause time
* Compilation thread incrementally generates native code in the background
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

### Documents

* [API Reference](https://fujita-y.github.io/ypsilon-api/)

### Versions

* Ypsilon version 2 have major design changes. Former version is available at [version-0.9](https://github.com/fujita-y/ypsilon/tree/version-0.9).

### Requirements

* LLVM 10, 11, 12, or 13

### Build and Install

```
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
cmake --install .
```

- On MacOS, you may want using [Homebrew](https://brew.sh/) to install llvm@13.
```
brew update
brew install llvm@13
echo 'export PATH="/opt/homebrew/opt/llvm/bin:$PATH"' >> ~/.zshrc
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

### Notes

* REPL start with ```(import (scheme base) (scheme process-context))``` or ```(import (rnrs base (6)) (rnrs programs))``` depending on the command line options specified.
* Without ```--top-level-program``` option, the contents of the specified script file will be interpreted as if they had been entered into the REPL.

### Rebuild Heap Files

* Edit 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '1'
* ```make```
* Edit .scm files in 'heap/boot' directory
* ```cd heap; make; cd ..; make; cd heap; make; cd ..```
* Edit 'src/core.h' and set 'UNBOUND_GLOC_RETURN_UNSPEC' to '0'
* ```make```


*This project is migrated from code.google.com/p/ypsilon*
