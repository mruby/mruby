# system capabilities checking

# initial system defaults
if(CMAKE_COMPILER_IS_GNUCC)
  set(MRUBY_DEFAULT_CFLAGS "-Wall -Werror-implicit-function-declaration")
  set(CMAKE_C_FLAGS "${MRUBY_DEFAULT_CFLAGS}")
  set(CMAKE_C_FLAGS_DEBUG "-O3 -ggdb")
  set(CMAKE_C_FLAGS_MINSIZEREL "-Os -DNDEBUG")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O3 -g")
  set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG")

  set(MRUBY_LIBS m)
else(CMAKE_COMPILER_IS_GNUCC)
  if(MSVC)
    # TODO default MSVC flags
    add_definitions(
      -D_CRT_SECURE_NO_WARNINGS
      -wd4018  # suppress 'signed/unsigned mismatch'
      )
  endif(MSVC)
endif(CMAKE_COMPILER_IS_GNUCC)

if(MSVC)
  add_definitions(
    -DRUBY_EXPORT   # required by oniguruma.h
    )
endif(MSVC)


# include helpers
include(CheckIncludeFile)
include(CheckSymbolExists)

# header checks
CHECK_INCLUDE_FILE(string.h HAVE_STRING_H)
if(HAVE_STRING_H)
  add_definitions(-DHAVE_STRING_H)
endif(HAVE_STRING_H)

CHECK_INCLUDE_FILE(float.h HAVE_FLOAT_H)
if(HAVE_FLOAT_H)
  add_definitions(-DHAVE_FLOAT_H)
endif(HAVE_FLOAT_H)


# symbol checks
CHECK_SYMBOL_EXISTS(gettimeofday sys/time.h HAVE_GETTIMEOFDAY)
if(NOT HAVE_GETTIMEOFDAY)
  add_definitions(-DNO_GETTIMEOFDAY)
endif(NOT HAVE_GETTIMEOFDAY)

# vim: ts=2 sts=2 sw=2 et
