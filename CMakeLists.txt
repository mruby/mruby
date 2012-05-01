# 
# Cmake build system for muby by beoran@rubyforhe.org, 2012.
# Released under the same license as mruby.
#
# NOTE: the original Makefile build system had a few hacks in them,
# whch I didn't duplicate. In stead the build logic is like this: 
# 1) First libritevm_static.a & libritevm.so are built from all C files in src.
# 2) Then mrbc is built and linked with libritevm_static.a .
# 3) Then libmrblib_static.a & lib_mrblib.so are built from the rb files in
#    the mrblib subdirectory
# 4) Finally, mruby is built and linked with  libritevm_static.a 
#    and libmrblib_static.a. 
# As a result, applications that embed mruby will have to link against 
# libritevm_static.a and libmrblib_static.a. in that order.
#
# TODO: make this work on windows too, support build options to generate 
# mrbconf.h, etc...
#

# Setup
# Need at least cmake version 2.6
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
if(COMMAND cmake_policy)
    cmake_policy(SET CMP0003 NEW)
    cmake_policy(SET CMP0015 NEW)
endif(COMMAND cmake_policy)

# Set the project name, we use only plain C.
project(MRUBY C)

# C compiler flags.
set(CMAKE_C_FLAGS "-Wall -g")
# should use  -O3 if it's a release build.bin/mrb 


# Version of mruby, useful for versoning .so and .dll files.
set(MRUBY_VERSION 1.0.0)
string(REGEX MATCH "^[0-9]+[.][0-9]+" MRUBY_SOVERSION ${MRUBY_VERSION})
string(REPLACE "." "" MRUBY_DLL_SHORTVER ${MRUBY_SOVERSION})

# Search in the `cmake' directory for additional CMake modules if needed.
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

# Search for C header files in these directories.
include_directories(${CMAKE_SOURCE_DIR}/include ${CMAKE_SOURCE_DIR}/src)

# Search for libaries too link tools with here: 
link_directories("lib")
link_directories("mrblib")

# put binaries that get built in bin
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)
# Put libraries that get built into `lib'.
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/lib)

if(NOT IPHONE)
option(SHARED "Build shared libraries" on)
set(BUILD_SHARED_LIBS ${SHARED})    # actual CMake variable
endif(NOT IPHONE)

# On some 64-bit platforms, libraries should be installed into `lib64'
# instead of `lib'.  Set this to 64 to do that.
set(LIB_SUFFIX "" CACHE STRING "Suffix for 'lib' directories, e.g. '64'")

set(FRAMEWORK_INSTALL_PREFIX "/Library/Frameworks" CACHE STRING
    "Directory in which to install Mac OS X frameworks")

# Options (none yet).

# Set up compilers.

include(CheckCSourceCompiles)

#   Begin tests

include(CheckFunctionExists)
include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)
include(FindPkgConfig)
include(TestBigEndian)


# lib Libraries that mruby uses itself (just libm)
set(MRUBY_LIBS m)

# Compile the sources to make libritevm
add_subdirectory("src")

# compile the compiler tool
add_subdirectory("tools/mrbc")

# compile libmrblib 
add_subdirectory("mrblib")

# compile the interpreter tool
add_subdirectory("tools/mruby")

# for make install, install the header files
install(FILES include/mruby.h     DESTINATION include)
install(FILES include/mrbconf.h   DESTINATION include)
install(DIRECTORY include/mruby   DESTINATION include)





