# builds tools/mrbc
file(GLOB MRBC_SRC_C "*.c")
add_executable(mrbc ${MRBC_SRC_C})
target_link_libraries(mrbc ritevm_static ${MRUBY_LIBS})
install(TARGETS mrbc RUNTIME DESTINATION bin)


