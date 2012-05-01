# Build the C files in the mruby src directory

cmake_minimum_required(VERSION 2.6)
if(CMAKE_VERSION VERSION_GREATER "2.8.0")
  cmake_policy(SET CMP0012 OLD)
endif()  

find_package(BISON)
BISON_TARGET(mruby parse.y ${CMAKE_CURRENT_BINARY_DIR}/parse.c)

# configure_file("config.in.h" "config.h")
file(GLOB MRUBY_SRC_C "*.c")
add_library(ritevm_object OBJECT ${MRUBY_SRC_C} ${BISON_mruby_OUTPUTS})
add_library(ritevm_static STATIC $<TARGET_OBJECTS:ritevm_object>)
add_library(ritevm SHARED $<TARGET_OBJECTS:ritevm_object>)


# target_link_libraries(ritevm ${MRUBY_LIBS})
# target_link_libraries(ritevm_static ${MRUBY_LIBS})
#  install(TARGETS ritevm ritevm_static
#         LIBRARY DESTINATION lib
#         ARCHIVE DESTINATION lib)

