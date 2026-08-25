/**
** @file mruby/platform.h - target platform identification
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_PLATFORM_H
#define MRUBY_PLATFORM_H

#include "common.h"

/**
 * Target platform definition macros
 *
 * `MRUBY_PLATFORM` names the machine the built binary runs on, in the
 * `cpu-os` form CRuby's `RUBY_PLATFORM` uses: `"x86_64-linux"`,
 * `"arm64-darwin"`, `"x64-mingw-ucrt"`.  It is what the `MRUBY_PLATFORM`
 * global constant carries.
 *
 * The name is spelled the way the platform itself spells it rather than the
 * way any one canonical-triple scheme would: the same 64-bit ARM core is
 * `arm64` on darwin and under MSVC but `aarch64` elsewhere, and the same
 * x86_64 is `x64` on Windows.  Code that reads this constant matches it
 * against a pattern (`/mingw|mswin/`, `/darwin/`), so agreeing with the
 * spelling in use beats being internally consistent.
 *
 * Everything here is decided by the compiler's own predefined macros, since
 * mruby is cross compiled far more often than not and the machine running
 * the build says nothing about the machine the result runs on.  A target the
 * detection below does not know reports `"unknown"` for that half; a target
 * with no operating system under it (bare metal, or an RTOS the compiler
 * does not announce) reports `"none"`, as a triple would.
 *
 * Three overrides, in the order they are consulted: define `MRUBY_PLATFORM`
 * to name the platform outright, or define `MRUBY_PLATFORM_CPU` and/or
 * `MRUBY_PLATFORM_OS` to correct one half and let the other be detected.
 * A build that wants a name of its own (`"esp32-freertos"`) takes the first.
 */
MRB_BEGIN_DECL

/*
 * The operating system half of `MRUBY_PLATFORM`.
 *
 * The WebAssembly hosts and Android come before the systems whose macros
 * they also define: `__EMSCRIPTEN__` and `__wasi__` sit on a Unix-ish
 * environment, and Android is a Linux.  The more specific name is the one
 * worth reporting, so it is tested first.
 */
#ifndef MRUBY_PLATFORM_OS
# if defined(__EMSCRIPTEN__)
#  define MRUBY_PLATFORM_OS "emscripten"
# elif defined(__wasi__)
#  define MRUBY_PLATFORM_OS "wasi"
# elif defined(__ANDROID__)
#  define MRUBY_PLATFORM_OS "linux-android"
# elif defined(__linux__)
#  define MRUBY_PLATFORM_OS "linux"
# elif defined(__APPLE__)
#  define MRUBY_PLATFORM_OS "darwin"
# elif defined(__FreeBSD__)
#  define MRUBY_PLATFORM_OS "freebsd"
# elif defined(__NetBSD__)
#  define MRUBY_PLATFORM_OS "netbsd"
# elif defined(__OpenBSD__)
#  define MRUBY_PLATFORM_OS "openbsd"
# elif defined(__DragonFly__)
#  define MRUBY_PLATFORM_OS "dragonfly"
# elif defined(__sun)
#  define MRUBY_PLATFORM_OS "solaris"
# elif defined(__HAIKU__)
#  define MRUBY_PLATFORM_OS "haiku"
# elif defined(__CYGWIN__)
#  define MRUBY_PLATFORM_OS "cygwin"
# elif defined(__MINGW32__)
   /* mingw-w64 defines `__MINGW32__` on both widths; `_UCRT` is what tells
      the UCRT toolchain from the msvcrt one, the way CRuby's `mingw-ucrt`
      and `mingw32` names do. */
#  if defined(_UCRT)
#   define MRUBY_PLATFORM_OS "mingw-ucrt"
#  else
#   define MRUBY_PLATFORM_OS "mingw32"
#  endif
# elif defined(_WIN32)
   /* CRuby spells the MSVC runtime version into the name (`mswin64_140`).
      That version is a property of the runtime the binary links against,
      not of the compiler, so it is left out rather than guessed from
      `_MSC_VER`. */
#  if defined(_WIN64)
#   define MRUBY_PLATFORM_OS "mswin64"
#  else
#   define MRUBY_PLATFORM_OS "mswin32"
#  endif
# elif defined(__unix__) || defined(__unix)
#  define MRUBY_PLATFORM_OS "unix"
# else
#  define MRUBY_PLATFORM_OS "none"
# endif
#endif

/*
 * The CPU half of `MRUBY_PLATFORM`.
 *
 * Windows and darwin spell the two widest architectures their own way, so
 * the spelling is chosen per system rather than per architecture.
 */
#ifndef MRUBY_PLATFORM_CPU
# if defined(__wasm64__)
#  define MRUBY_PLATFORM_CPU "wasm64"
# elif defined(__wasm32__) || defined(__wasm__)
#  define MRUBY_PLATFORM_CPU "wasm32"
# elif defined(__x86_64__) || defined(__amd64__) || defined(_M_X64) || defined(_M_AMD64)
#  if defined(_WIN32)
#   define MRUBY_PLATFORM_CPU "x64"
#  else
#   define MRUBY_PLATFORM_CPU "x86_64"
#  endif
# elif defined(__i386__) || defined(_M_IX86)
#  define MRUBY_PLATFORM_CPU "i386"
# elif defined(__aarch64__) || defined(_M_ARM64)
   /* Windows spells 64-bit ARM `arm64` only in the MSVC toolchain, which is
      where CRuby's name for it comes from; the mingw one keeps `aarch64`
      (`aarch64-mingw-ucrt`) even though the same width of x86 is `x64` on
      both.  `_MSC_VER` is what tells the two apart. */
#  if defined(__APPLE__) || defined(_MSC_VER)
#   define MRUBY_PLATFORM_CPU "arm64"
#  else
#   define MRUBY_PLATFORM_CPU "aarch64"
#  endif
# elif defined(__arm__) || defined(_M_ARM)
#  define MRUBY_PLATFORM_CPU "arm"
# elif defined(__riscv)
#  if __riscv_xlen == 64
#   define MRUBY_PLATFORM_CPU "riscv64"
#  else
#   define MRUBY_PLATFORM_CPU "riscv32"
#  endif
# elif defined(__powerpc64__) || defined(__ppc64__)
#  if defined(__LITTLE_ENDIAN__) || (defined(__BYTE_ORDER__) && defined(__ORDER_LITTLE_ENDIAN__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#   define MRUBY_PLATFORM_CPU "powerpc64le"
#  else
#   define MRUBY_PLATFORM_CPU "powerpc64"
#  endif
# elif defined(__powerpc__) || defined(__ppc__)
#  define MRUBY_PLATFORM_CPU "powerpc"
# elif defined(__mips64) || defined(__mips64__)
#  define MRUBY_PLATFORM_CPU "mips64"
# elif defined(__mips__) || defined(__mips)
#  define MRUBY_PLATFORM_CPU "mips"
# elif defined(__s390x__)
#  define MRUBY_PLATFORM_CPU "s390x"
# elif defined(__sparc64__) || defined(__sparcv9) || defined(__sparc_v9__)
#  define MRUBY_PLATFORM_CPU "sparc64"
# elif defined(__sparc__) || defined(__sparc)
#  define MRUBY_PLATFORM_CPU "sparc"
# elif defined(__XTENSA__)
#  define MRUBY_PLATFORM_CPU "xtensa"
# elif defined(__AVR__)
#  define MRUBY_PLATFORM_CPU "avr"
# else
#  define MRUBY_PLATFORM_CPU "unknown"
# endif
#endif

/*
 * The platform mruby was built for.
 */
#ifndef MRUBY_PLATFORM
#define MRUBY_PLATFORM MRUBY_PLATFORM_CPU "-" MRUBY_PLATFORM_OS
#endif

MRB_END_DECL

#endif  /* MRUBY_PLATFORM_H */
