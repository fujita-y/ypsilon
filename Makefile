#
#   Makefile for Linux and Darwin
#   Use Win32 native build for Cygwin
#

PROG 	 = ypsilon

PREFIX 	 = /usr/local

CPPFLAGS = -DNDEBUG -DDEFAULT_HEAP_LIMIT=32 -DSYSTEM_SHARE_PATH='"$(PREFIX)/share/$(PROG)"'

CXXFLAGS = -x c++ -pthread -msse -mfpmath=sse -O3 -fstrict-aliasing \
	   -fomit-frame-pointer -momit-leaf-frame-pointer \
	   -fno-align-labels -fno-align-loops -fno-align-jumps

SRCS 	 = file.cpp main.cpp vm0.cpp object_heap_compact.cpp subr_flonum.cpp vm1.cpp object_set.cpp \
	   subr_hash.cpp vm2.cpp object_slab.cpp subr_list.cpp \
           vm3.cpp port.cpp subr_others.cpp arith.cpp printer.cpp subr_port.cpp subr_r5rs_arith.cpp \
	   equiv.cpp reader.cpp ffi.cpp subr_base.cpp \
           subr_unicode.cpp hash.cpp subr_base_arith.cpp ucs4.cpp ioerror.cpp subr_bitwise.cpp utf8.cpp \
	   main.cpp subr_bvector.cpp violation.cpp object_factory.cpp \
           subr_ffi.cpp object_heap.cpp subr_fixnum.cpp bit.cpp list.cpp fasl.cpp

VPATH 	 = src

UNAME 	 = $(shell uname)

ifneq (, $(findstring Linux, $(UNAME)))
  ifeq ($(shell $(CXX) -dumpspecs | grep 'march=native')), )
    CXXFLAGS += -m32 -march=i686
  else
    CXXFLAGS += -m32 -march=native
  endif
  ASFLAGS = --32
  LDFLAGS = -m32 -lpthread -ldl
  SRCS += ffi_stub_linux.s
endif

ifneq (, $(findstring Darwin, $(UNAME)))
  CXXFLAGS += -arch i386
  SRCS += ffi_stub_darwin.s
endif

OBJS =	$(patsubst %.cpp, %.o, $(filter %.cpp, $(SRCS))) $(patsubst %.s, %.o, $(filter %.s, $(SRCS)))

DEPS =	$(patsubst %.cpp, %.d, $(filter %.cpp, $(SRCS)))

.PHONY: all install uninstall sitelib stdlib check bench clean

all: $(PROG)
	@mkdir -p -m755 $(HOME)/.ypsilon

$(PROG): $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $^

# VM1.s:
#	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -fno-reorder-blocks -fno-crossjumping -fverbose-asm -S src/VM1.cpp

VM1.o: VM1.cpp
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -fno-reorder-blocks -fno-crossjumping -c src/VM1.cpp

install: all stdlib sitelib
	mkdir -pv -m755 $(PREFIX)/bin
	mkdir -pv -m755 $(PREFIX)/share/man/man1
	cp $(PROG) $(PREFIX)/bin/$(PROG)
	cp $(PROG).1 $(PREFIX)/share/man/man1/$(PROG).1
	chmod 755 $(PREFIX)/bin/$(PROG)
	chmod 644 $(PREFIX)/share/man/man1/$(PROG).1

uninstall:
	-rm -rf $(PREFIX)/share/$(PROG)/stdlib
	-rm -rf $(PREFIX)/share/$(PROG)/sitelib
	-rm -f $(PREFIX)/share/man/man1/$(PROG).1
	-rm -f $(PREFIX)/bin/$(PROG)
	-rmdir $(PREFIX)/share/$(PROG)

stdlib:
	find stdlib -type f -name '*.scm' | cpio -pdu $(PREFIX)/share/$(PROG)

sitelib:
	find sitelib -type f -name '*.scm' | cpio -pdu $(PREFIX)/share/$(PROG)

check: all
	@echo '----------------------------------------'
	@echo 'r4rstest.scm:'
	@./$(PROG) --acc=/tmp --sitelib=./test:./sitelib:./stdlib ./test/r4rstest.scm
	@echo '----------------------------------------'
	@echo 'tspl.scm:'
	@./$(PROG) --acc=/tmp --sitelib=./test:./sitelib:./stdlib ./test/tspl.scm
	@echo '----------------------------------------'
	@echo 'arith.scm:'
	@./$(PROG) --acc=/tmp --sitelib=./test:./sitelib:./stdlib ./test/arith.scm
	@echo '----------------------------------------'
	@echo 'r5rs_pitfall.scm:'
	@./$(PROG) --acc=/tmp --sitelib=./test:./sitelib:./stdlib ./test/r5rs_pitfall.scm
	@echo '----------------------------------------'
	@echo 'syntax-rule-stress-test.scm:'
	@./$(PROG) --acc=/tmp --sitelib=./test:./sitelib:./stdlib ./test/syntax-rule-stress-test.scm
	@echo '----------------------------------------'
	@echo 'r6rs.scm:'
	@./$(PROG) --acc=/tmp --sitelib=./test:./sitelib:./stdlib ./test/r6rs.scm
	@echo '----------------------------------------'
	@echo 'r6rs-lib.scm:'
	@./$(PROG) --acc=/tmp --sitelib=./test:./sitelib:./stdlib ./test/r6rs-lib.scm
	@echo '----------------------------------------'
	@echo 'Passed all tests'
	@rm -f ./test/tmp* 

eval: all
	./$(PROG) --acc=/tmp --sitelib=./sitelib:./stdlib

bench: all
	./$(PROG) --acc=/tmp --sitelib=./test:./sitelib:./stdlib -- bench/run-ypsilon.scm

clean:
	rm -f *.o *.d
	rm -f $(HOME)/.ypsilon/*.cache 
	rm -f $(HOME)/.ypsilon/*.time

distclean: clean
	rm -f tmp1 tmp2 tmp3 spheres.pgm
	find . -type f -name .DS_Store -print0 | xargs -0 rm -f
	find . -type f -name '*~' -print0 | xargs -0 rm -f
	rm -f ./test/tmp* 
	rm -f ./bench/gambit-benchmarks/tmp*
	rm -f ./bench/gambit-benchmarks/spheres.pgm
	rm -f -r ./build/* ./build-win32/* ./setup-win32/Debug ./setup-win32/Release
	rm -f ./ypsilon.xcodeproj/*.mode1v3 ./ypsilon.xcodeproj/*.pbxuser
	rm -f ./ypsilon.ncb
	rm -f ./ypsilon

%.d: %.cpp
	$(SHELL) -ec '$(CXX) -MM $(CPPFLAGS) $< \
	| sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; [ -s $@ ] || rm -f $@'

ifneq (, $(findstring clean, $(MAKEFLAGS)))
  -include $(DEPS)
endif

