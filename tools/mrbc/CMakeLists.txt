# build tools/mrbc executable

file(GLOB MRBC_SRC_C "*.c")
add_executable(mrbc ${MRBC_SRC_C})
target_link_libraries(mrbc mruby_static ${MRUBY_LIBS})

install(TARGETS mrbc RUNTIME DESTINATION bin)

# vim: ts=2 sts=2 sw=2 et
