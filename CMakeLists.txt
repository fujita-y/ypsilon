cmake_minimum_required(VERSION 3.13.4)

if(APPLE)
  set(CMAKE_OSX_SYSROOT "")
endif(APPLE)

project(Ypsilon)

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release) # Debug Release RelWithDebInfo MinSizeRel
endif()
if(CMAKE_BUILD_TYPE)
  message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
endif()
if(CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Configuration types: ${CMAKE_CONFIGURATION_TYPES}")
endif()

execute_process(
  COMMAND llvm-config --cmakedir
  OUTPUT_VARIABLE llvm_cmakedir
  OUTPUT_STRIP_TRAILING_WHITESPACE)
find_package(LLVM REQUIRED CONFIG PATHS ${llvm_cmakedir} NO_DEFAULT_PATH)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

set(CMAKE_C_COMPILER "clang")
set(CMAKE_CXX_COMPILER "clang++")

if(NOT APPLE)
  add_link_options("-fuse-ld=lld")
endif(NOT APPLE)

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -flto")
set(CMAKE_LINKER_FLAGS_RELEASE "${CMAKE_LINKER_FLAGS_RELEASE} -flto")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=address")
set(CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -fsanitize=address")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=address")
set(CMAKE_LINKER_FLAGS_RELWITHDEBINFO "${CMAKE_LINKER_FLAGS_RELWITHDEBINFO} -fsanitize=address")

set(YPSILON_SHARE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/share/ypsilon)
set(YPSILON_EXTENSION_DIRECTORY ${CMAKE_INSTALL_PREFIX}/lib/ypsilon)
add_compile_definitions(SYSTEM_SHARE_PATH="${YPSILON_SHARE_DIRECTORY}")
add_compile_definitions(SYSTEM_EXTENSION_PATH="${YPSILON_EXTENSION_DIRECTORY}")

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  add_compile_definitions(DEFAULT_HEAP_LIMIT=512)
else()
  add_compile_definitions(DEFAULT_HEAP_LIMIT=2048)
endif()

add_executable(
  ypsilon
  src/arith.cpp
  src/bit.cpp
  src/digamma.cpp
  src/equiv.cpp
  src/fasl.cpp
  src/file.cpp
  src/hash.cpp
  src/ioerror.cpp
  src/list.cpp
  src/main.cpp
  src/object_factory.cpp
  src/object_heap_compact.cpp
  src/object_heap.cpp
  src/object_set.cpp
  src/object_slab.cpp
  src/port.cpp
  src/printer.cpp
  src/reader.cpp
  src/serialize.cpp
  src/socket.cpp
  src/subr_base_arith.cpp
  src/subr_base.cpp
  src/subr_bitwise.cpp
  src/subr_bvector.cpp
  src/subr_c_ffi.cpp
  src/subr_codegen.cpp
  src/subr_file.cpp
  src/subr_fixnum.cpp
  src/subr_flonum.cpp
  src/subr_hash.cpp
  src/subr_linmath.cpp
  src/subr_list.cpp
  src/subr_others.cpp
  src/subr_port.cpp
  src/subr_process.cpp
  src/subr_r5rs_arith.cpp
  src/subr_socket.cpp
  src/subr_unicode.cpp
  src/ucs4.cpp
  src/utf8.cpp
  src/uuid.cpp
  src/violation.cpp
  src/vm0.cpp
  src/vm1.cpp
  src/vm2.cpp
  src/vm3.cpp)
execute_process(
  COMMAND llvm-config --libs all
  OUTPUT_VARIABLE llvm_libs
  OUTPUT_STRIP_TRAILING_WHITESPACE)
target_link_libraries(ypsilon ${llvm_libs} -pthread -ldl)

file(MAKE_DIRECTORY $ENV{HOME}/.ypsilon)

set(YPSILON_SITELIB_DIRECTORY ${YPSILON_SHARE_DIRECTORY}/sitelib)
install(CODE "execute_process(COMMAND rm -rf ${YPSILON_SITELIB_DIRECTORY})")
install(DIRECTORY sitelib/ DESTINATION ${YPSILON_SITELIB_DIRECTORY})
install(TARGETS ypsilon DESTINATION bin)

enable_testing()
set(YPSILON_COMMON_TEST_OPTIONS --heap-limit=128 --clean-acc --sitelib=./test:./sitelib)
add_test(
  NAME r4rstest
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} ./test/r4rstest.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME tspl
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} ./test/tspl.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME arith
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} ./test/arith.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r5rs-pitfall
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} --r6rs ./test/r5rs-pitfall.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r6rs
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} --r6rs ./test/r6rs.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r6rs-lib
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} --r6rs ./test/r6rs-lib.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r6rs-more
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} --r6rs ./test/r6rs-more.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME r7rs-test
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} --r7rs ./test/r7rs-test.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME c-ffi-test
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} ./test/c-ffi-test.scm
  WORKING_DIRECTORY ..)
add_test(
  NAME syntax-rule-stress-test
  COMMAND $<TARGET_FILE:ypsilon> ${YPSILON_COMMON_TEST_OPTIONS} ./test/syntax-rule-stress-test.scm
  WORKING_DIRECTORY ..)
