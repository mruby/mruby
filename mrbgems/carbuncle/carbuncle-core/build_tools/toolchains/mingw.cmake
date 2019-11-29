set(TOOLCHAIN_PREFIX x86_64-w64-mingw32)

# cross compilers to use for C, C++ and Fortran
set(CMAKE_C_COMPILER ${TOOLCHAIN_PREFIX}-gcc)
set(CMAKE_CXX_COMPILER ${TOOLCHAIN_PREFIX}-g++)
set(CMAKE_Fortran_COMPILER ${TOOLCHAIN_PREFIX}-gfortran)
set(CMAKE_RC_COMPILER ${TOOLCHAIN_PREFIX}-windres)

if (APPLE)
  execute_process(COMMAND "brew --prefix mingw-w64" OUTPUT_VARIABLE MINGW_BREW_PREFIX)
  set(CMAKE_FIND_ROOT_PATH
      /usr/${TOOLCHAIN_PREFIX}
      ${MINGW_BREW_PREFIX}/toolchain-x86_64/x86_64-w64-mingw32/include/
  )
else(UNIX)
  # target environment on the build host system
  set(CMAKE_FIND_ROOT_PATH /usr/${TOOLCHAIN_PREFIX})
endif ()

set(CMAKE_SYSTEM_NAME Windows)

set(MINGW, true)

# modify default behavior of FIND_XXX() commands
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

set(CMAKE_FIND_LIBRARY_PREFIXES "lib" "")
set(CMAKE_FIND_LIBRARY_SUFFIXES ".dll" ".dll.a" ".lib" ".a")
