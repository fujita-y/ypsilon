# Ypsilon: The implementation of R6RS Scheme Programming Language for real-time applications #

Ypsilon is the implementation of Scheme Programming Language, which conforms to the standard R<sup>6</sup>RS. It achieves a remarkably short GC pause time and the best performance in parallel execution as it implements "mostly concurrent garbage collection", which is optimized for the multi-core CPU.

Ypsilon is easy to use as well as good for applications of any kind that require quick, reliable, and interactive data processing. It implements full features of R<sup>6</sup>RS and R<sup>6</sup>RS standard libraries including:
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

The implementation of the library system is adopted to be suitable for interactive development. Ypsilon always instantiate the library upon every import regardless of its phase, and identifiers which has been exported from it can be referred in every level. The "for" form for explicit-phased models will be syntax-checked but the contents will be ignored.


More libraries are included to support a wide variety of software development. Also it has built-in FFI which is easy to use. Please refer to the following files for FFI overview.
  * [example/gtk-hello.scm](https://github.com/fujita-y/ypsilon/blob/master/example/gtk-hello.scm)
  * [example/glut-demo.scm](https://github.com/fujita-y/ypsilon/blob/master/example/glut-demo.scm)
  * [sitelib/ypsilon/glut.scm](https://github.com/fujita-y/ypsilon/blob/master/sitelib/ypsilon/glut.scm)
  * [sitelib/ypsilon/gl.scm](https://github.com/fujita-y/ypsilon/blob/master/sitelib/ypsilon/gl.scm)
  * [sitelib/ypsilon/ffi.scm](https://github.com/fujita-y/ypsilon/blob/master/sitelib/ypsilon/ffi.scm)
  
This project is migrated from code.google.com/p/ypsilon
