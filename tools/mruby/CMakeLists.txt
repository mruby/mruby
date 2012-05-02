# builds tools/mrbc
file(GLOB MRUBYBIN_SRC_C "*.c")
add_executable(mruby ${MRUBYBIN_SRC_C})
target_link_libraries(mruby mrubylib_static ${MRUBY_LIBS})
install(TARGETS mruby RUNTIME DESTINATION bin)


