# build the core mruby C files

find_program(BISON_EXECUTABLE NAMES bison DOC "Bison executable")
if(NOT BISON_EXECUTABLE)
  message(FATAL_ERROR "Could not find \"bison\" executable")
endif(NOT BISON_EXECUTABLE)
add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/parse.c"
    DEPENDS parse.y
    COMMAND ${BISON_EXECUTABLE}
    ARGS -o "${CMAKE_CURRENT_BINARY_DIR}/parse.c" "${CMAKE_CURRENT_SOURCE_DIR}/parse.y"
)

if("${CMAKE_VER}" STRLESS "002008008" )
  # "add_library(OBJECT)" was added in cmake 2.8.8.  If it isn't there, just
  # build a normal library out of these sources
  if("${CMAKE_VER}" STRGREATER "002004006" )
    add_library(mruby_static STATIC EXCLUDE_FROM_ALL ${MRUBY_SRC_C})
  else("${CMAKE_VER}" STRGREATER "002004006" )
    # before cmake 2.4.7, EXCLUDE_FROM_ALL didn't work on libraries
    add_library(mruby_static STATIC ${MRUBY_SRC_C})
  endif("${CMAKE_VER}" STRGREATER "002004006" )
else("${CMAKE_VER}" STRLESS "002008008" )
  add_library(mruby_object OBJECT ${MRUBY_SRC_C})
  add_library(mruby_static STATIC EXCLUDE_FROM_ALL $<TARGET_OBJECTS:mruby_object>)
endif("${CMAKE_VER}" STRLESS "002008008" )

# vim: ts=2 sts=2 sw=2 et
