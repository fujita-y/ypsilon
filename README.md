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

* LLVM 10, 11, 12, 13, or 14

### Installation

[![Packaging status](https://repology.org/badge/vertical-allrepos/ypsilon.svg)](https://repology.org/project/ypsilon/versions)

Ypsilon packaged by distros may not be up-to-date. You can build and install Ypsilon from source:

```
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
cmake --install .
```

- On MacOS, you may want using [Homebrew](https://brew.sh/) to install LLVM 14.
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
