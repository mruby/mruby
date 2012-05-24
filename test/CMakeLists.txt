# build standalone mrbtest runner containing all *.rb tests

if(NOT CMAKE_CROSSCOMPILING)
  set(XPCAT_COMMAND xpcat)
  set(MRBC_COMMAND mrbc)
  if("${CMAKE_VER}" STRLESS "002006000")
    # Prior to cmake 2.6, add_custom_command couldn't automatically find the
    # location of a target binary
    get_target_property(XPCAT_COMMAND xpcat LOCATION)
    get_target_property(MRBC_COMMAND mrbc LOCATION)
  endif("${CMAKE_VER}" STRLESS "002006000")

  file(GLOB MRBTEST_SRC_RB "assert.rb" "t/*.rb")

  # generate a single rb file from all existing test *.rb
  add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.rbtmp"
    DEPENDS xpcat
    COMMAND ${XPCAT_COMMAND} -o "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.rbtmp" ${MRBTEST_SRC_RB}
    )

  # mruby compile and generate C byte array representation
  add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.ctmp"
    DEPENDS mrbc "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.rbtmp"
    COMMAND ${MRBC_COMMAND} -Bmrbtest_irep -o"${CMAKE_CURRENT_BINARY_DIR}/mrbtest.ctmp"
                 "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.rbtmp"
    )

  # aggregate mruby's *.rb test files as a single C file
  add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.c"
    DEPENDS xpcat init_mrbtest.c "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.ctmp"
    COMMAND ${XPCAT_COMMAND} -o "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.c"
                     "${CMAKE_CURRENT_SOURCE_DIR}/init_mrbtest.c"
                     "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.ctmp"
    )

  add_executable(mrbtest
    EXCLUDE_FROM_ALL
    "${CMAKE_CURRENT_SOURCE_DIR}/driver.c"
    "${CMAKE_CURRENT_BINARY_DIR}/mrbtest.c"
    )
  target_link_libraries(mrbtest libmruby_static ${MRUBY_LIBS})

  add_custom_target(test
    DEPENDS mrbtest
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/mrbtest"
    )

endif(NOT CMAKE_CROSSCOMPILING)

# vim: ts=2 sts=2 sw=2 et
