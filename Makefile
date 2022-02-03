#   Makefile for Linux, Darwin
#   Requirements: GNU Make, llvm-10
#   Options: DESTDIR, PREFIX, DATAMODEL(ILP32/LP64)

PROG = ypsilon

PREFIX = /usr/local

CPPFLAGS = -DNDEBUG -DSYSTEM_SHARE_PATH='"$(DESTDIR)$(PREFIX)/share/$(PROG)"' -DSYSTEM_EXTENSION_PATH='"$(DESTDIR)$(PREFIX)/lib/$(PROG)"'

CXX = clang++

CXXFLAGS = `llvm-config --cxxflags` -fcxx-exceptions

SRCS = file.cpp main.cpp object_heap_compact.cpp subr_flonum.cpp vm0.cpp vm1.cpp vm2.cpp vm3.cpp object_set.cpp \
       object_slab.cpp subr_list.cpp serialize.cpp vm3.cpp port.cpp subr_others.cpp arith.cpp printer.cpp \
       subr_port.cpp subr_r5rs_arith.cpp equiv.cpp reader.cpp subr_base.cpp uuid.cpp subr_socket.cpp subr_hash.cpp \
       subr_unicode.cpp hash.cpp subr_base_arith.cpp ucs4.cpp ioerror.cpp subr_bitwise.cpp utf8.cpp subr_bvector.cpp \
       violation.cpp object_factory.cpp subr_file.cpp subr_process.cpp object_heap.cpp subr_fixnum.cpp bit.cpp \
       list.cpp fasl.cpp socket.cpp subr_c_ffi.cpp subr_codegen.cpp digamma.cpp subr_linmath.cpp

VPATH = src

UNAME = $(shell uname -a)

ifndef DATAMODEL
  ifeq (,$(shell echo | $(CXX) -E -dM - | grep '__LP64__'))
    DATAMODEL = ILP32
    CPPFLAGS += -DDEFAULT_HEAP_LIMIT=512
  else
    DATAMODEL = LP64
    CPPFLAGS += -DDEFAULT_HEAP_LIMIT=2048
  endif
endif

ifneq (,$(findstring Linux, $(UNAME)))
  ifneq (,$(findstring aarch64, $(UNAME)))
    ifeq ($(DATAMODEL), ILP32)
      CXXFLAGS += -march=armv7-a
    else
      CXXFLAGS += -march=armv8-a
    endif
  endif
  ifneq (,$(findstring x86, $(UNAME)))
    ifeq ($(DATAMODEL), ILP32)
      CXXFLAGS += -march=x86
    else
      CXXFLAGS += -march=x86-64
    endif
  endif
  CXXFLAGS += -O3 -pthread -fomit-frame-pointer -momit-leaf-frame-pointer
  LDFLAGS = -fuse-ld=lld
  LDLIBS = -Wl,--as-needed $(shell llvm-config --ldflags --system-libs --libs all) -pthread -Wl,--no-as-needed -ldl
endif

ifneq (,$(findstring Darwin, $(UNAME)))
  CXXFLAGS += -O3 -momit-leaf-frame-pointer
  LDLIBS = $(shell llvm-config --ldflags --system-libs --libs all)
endif

OBJS = $(patsubst %.cpp, %.o, $(filter %.cpp, $(SRCS))) $(patsubst %.s, %.o, $(filter %.s, $(SRCS)))
DEPS = $(patsubst %.cpp, %.d, $(filter %.cpp, $(SRCS)))

.PHONY: all install uninstall sitelib stdlib extension check bench clean distclean

all: $(PROG) $(EXTS)
	@mkdir -p -m755 $(HOME)/.ypsilon

$(PROG): $(OBJS)
	$(CXX) $(LDFLAGS) $^ $(LDLIBS) -o $@

install: all stdlib sitelib extension
	mkdir -p -m755 $(DESTDIR)$(PREFIX)/bin
	cp $(PROG) $(DESTDIR)$(PREFIX)/bin/$(PROG)
	chmod 755 $(DESTDIR)$(PREFIX)/bin/$(PROG)

uninstall:
	-rm -rf $(DESTDIR)$(PREFIX)/share/$(PROG)/stdlib
	-rm -rf $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib
	-rm -rf $(DESTDIR)$(PREFIX)/lib/$(PROG)
	-rm -f $(DESTDIR)$(PREFIX)/bin/$(PROG)
	-rmdir $(DESTDIR)$(PREFIX)/share/$(PROG)

stdlib:
	mkdir -p -m755 $(DESTDIR)$(PREFIX)/share/$(PROG)/stdlib
	find stdlib -type f -name '*.scm' | cpio -pdu $(DESTDIR)$(PREFIX)/share/$(PROG)
	find $(DESTDIR)$(PREFIX)/share/$(PROG)/stdlib -type d -exec chmod 755 {} \;
	find $(DESTDIR)$(PREFIX)/share/$(PROG)/stdlib -type f -exec chmod 644 {} \;

sitelib:
	mkdir -p -m755 $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib
	find sitelib -type f -name '*.scm' | cpio -pdu $(DESTDIR)$(PREFIX)/share/$(PROG)
	find $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib -type d -exec chmod 755 {} \;
	find $(DESTDIR)$(PREFIX)/share/$(PROG)/sitelib -type f -exec chmod 644 {} \;

extension:
	mkdir -p -m755 $(DESTDIR)$(PREFIX)/lib/$(PROG)
	find . -type f -name '*.dylib' | cpio -pdu $(DESTDIR)$(PREFIX)/lib/$(PROG)
	find $(DESTDIR)$(PREFIX)/lib/$(PROG) -type d -exec chmod 755 {} \;
	find $(DESTDIR)$(PREFIX)/lib/$(PROG) -type f -exec chmod 755 {} \;

check: all
	@echo '----------------------------------------'
	@echo 'r4rstest.scm:'
	@./$(PROG) --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/r4rstest.scm
	@echo '----------------------------------------'
	@echo 'tspl.scm:'
	@./$(PROG) --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/tspl.scm
	@echo '----------------------------------------'
	@echo 'arith.scm:'
	@./$(PROG) --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/arith.scm
	@echo '----------------------------------------'
	@echo 'r5rs_pitfall.scm:'
	@./$(PROG) --r6rs --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/r5rs-pitfall.scm
	@echo '----------------------------------------'
	@echo 'r6rs.scm:'
	@./$(PROG) --r6rs --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/r6rs.scm
	@echo '----------------------------------------'
	@echo 'r6rs-lib.scm:'
	@./$(PROG) --r6rs --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/r6rs-lib.scm
	@echo '----------------------------------------'
	@echo 'r6rs-more.scm:'
	@./$(PROG) --r6rs --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/r6rs-more.scm
	@echo '----------------------------------------'
	@echo 'r7rs-test.scm:'
	@./$(PROG) --r7rs --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/r7rs-test.scm
	@echo '----------------------------------------'
	@echo 'c-ffi-test.scm:'
	@./$(PROG) --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/c-ffi-test.scm
	@echo '----------------------------------------'
	@echo 'syntax-rule-stress-test.scm:'
	@./$(PROG) --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib ./test/syntax-rule-stress-test.scm
	@echo '----------------------------------------'
	@echo 'Passed all tests'
	@rm -f ./test/tmp*

eval: all
	./$(PROG) --verbose --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./sitelib

bench: all
	./$(PROG) --heap-limit=128 --acc=/tmp --clean-acc --sitelib=./test:./sitelib -- bench/run-ypsilon.scm

clean:
	rm -f *.o *.d *.dylib
	rm -f $(HOME)/.ypsilon/*.cache
	rm -f $(HOME)/.ypsilon/*.time

distclean: clean
	rm -f tmp1 tmp2 tmp3 spheres.pgm
	rm -f ./test/tmp*
	rm -f ./bench/gambit-benchmarks/tmp*
	rm -f ./bench/gambit-benchmarks/spheres.pgm
	rm -f ./heap/debug-*.vmi
	rm -f $(PROG)

moreclean: distclean
	find . -type f -name .DS_Store -print0 | xargs -0 rm -f
	find . -type f -name '*~' -print0 | xargs -0 rm -f

%.d: %.cpp
	$(SHELL) -ec '$(CXX) -MM $(CPPFLAGS) $(CXXFLAGS) $< | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; [ -s $@ ] || rm -f $@'

ifeq ($(findstring clean, $(MAKECMDGOALS)), )
  ifeq ($(findstring uninstall, $(MAKECMDGOALS)), )
    -include $(DEPS)
  endif
endif
