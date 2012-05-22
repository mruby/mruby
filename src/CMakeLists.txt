# build the core mruby C files

find_package(BISON)
bison_target(mruby parse.y "${CMAKE_CURRENT_BINARY_DIR}/parse.c")

file(GLOB MRUBY_SRC_C "*.c")
list(APPEND MRUBY_SRC_C "${CMAKE_CURRENT_BINARY_DIR}/parse.c")

add_library(mruby_object OBJECT ${MRUBY_SRC_C} ${BISON_mruby_OUTPUTS})
add_library(mruby_static STATIC EXCLUDE_FROM_ALL $<TARGET_OBJECTS:mruby_object>)

# vim: ts=2 sts=2 sw=2 et
