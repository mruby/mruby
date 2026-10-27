/**
** @file common.h - mruby common platform definition"
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_COMMON_H
#define MRUBY_COMMON_H

#ifdef __APPLE__
  #ifndef __TARGETCONDITIONALS__
  #include "TargetConditionals.h"
  #endif
#endif

#ifdef __cplusplus
#ifdef MRB_USE_CXX_ABI
#define MRB_BEGIN_DECL
#define MRB_END_DECL
#else
# define MRB_BEGIN_DECL extern "C" {
# define MRB_END_DECL }
#endif
#else
/** Start declarations in C mode */
# define MRB_BEGIN_DECL
/** End declarations in C mode */
# define MRB_END_DECL
#endif

#include <sys/types.h>
#if defined _MSC_VER
#include <BaseTsd.h>
typedef SSIZE_T ssize_t;
#endif

/**
 * Shared compiler macros
 */
MRB_BEGIN_DECL

/** Declare a function that never returns. */
#if defined __STDC_VERSION__ && __STDC_VERSION__ >= 201112L
# define mrb_noreturn _Noreturn
#elif defined __GNUC__ && !defined __STRICT_ANSI__
# define mrb_noreturn __attribute__((noreturn))
#elif defined _MSC_VER
# define mrb_noreturn __declspec(noreturn)
#else
# define mrb_noreturn
#endif

/** Mark a function as deprecated. */
#if defined __GNUC__ && !defined __STRICT_ANSI__
# define mrb_deprecated __attribute__((deprecated))
#elif defined _MSC_VER
# define mrb_deprecated __declspec(deprecated)
#else
# define mrb_deprecated
#endif

/** Branch prediction hints for optimization. */
#if defined(__GNUC__) || defined(__clang__)
# define mrb_likely(x)   __builtin_expect(!!(x), 1)
# define mrb_unlikely(x) __builtin_expect(!!(x), 0)
#else
# define mrb_likely(x)   (x)
# define mrb_unlikely(x) (x)
#endif

/** Declare a type or object as an alignment requirement. */
#ifndef mrb_alignas
# if defined(__cplusplus) && __cplusplus >= 201103L
#  // https://en.cppreference.com/w/cpp/language/alignas
#  define mrb_alignas(n) alignas(n)
# elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#  // https://en.cppreference.com/w/c/language/_Alignas
#  define mrb_alignas(n) _Alignas(n)
# elif defined(_MSC_VER) || defined(__INTEL_COMPILER)
#  // https://learn.microsoft.com/en-us/cpp/cpp/align-cpp?view=msvc-170
#  define mrb_alignas(n) __declspec(align(n))
# elif defined(__GNUC__) || defined(__clang__)
#  // https://gcc.gnu.org/onlinedocs/gcc/Common-Type-Attributes.html#index-aligned-type-attribute
#  define mrb_alignas(n) __attribute__((aligned(n)))
# else
#  // `mrb_alignas` defined as dummy. If necessary, send issues to https://github.com/mruby/mruby .
#  define mrb_alignas(n)
# endif
#endif

/** Declare a function as always inlined. */
#if defined _MSC_VER && _MSC_VER < 1900
# ifndef __cplusplus
#  define inline __inline
# endif
#endif
#define MRB_INLINE static inline

/** Declare a public mruby API function. */
#ifndef MRB_API
#if defined(MRB_BUILD_AS_DLL)
#if defined(MRB_CORE) || defined(MRB_LIB)
# define MRB_API __declspec(dllexport)
#else
# define MRB_API __declspec(dllimport)
#endif
#else
# define MRB_API extern
#endif
#endif

/** Declare mingw versions */
#if defined(__MINGW32__) || defined(__MINGW64__)
# include <_mingw.h>
# if defined(__MINGW64_VERSION_MAJOR)
#  define MRB_MINGW64_VERSION  (__MINGW64_VERSION_MAJOR * 1000 + __MINGW64_VERSION_MINOR)
# elif defined(__MINGW32_MAJOR_VERSION)
#  define MRB_MINGW32_VERSION  (__MINGW32_MAJOR_VERSION * 1000 + __MINGW32_MINOR_VERSION)
# endif
# if defined(__MINGW32__) && !defined(__MINGW64__)
#   define MRB_MINGW32_LEGACY
# endif
#endif

/*
 * Storage class of the two variables mruby-compiler keeps for the whole
 * process: `global_mrb`, the state the Prism allocator allocates through,
 * and the Prism arena current for a parse.
 *
 * By default it is empty, and a process compiles on one thread at a time:
 * two threads compiling at once, even on mrb_states of their own, share both
 * variables and allocate through each other's state and arena.  A host that
 * compiles on several threads defines MRC_USE_THREAD_LOCAL, and both become
 * per-thread.  It is a choice rather than the default because a target whose
 * runtime has no thread-local storage (a bare-metal toolchain may accept the
 * keyword and then fail to link) has to build as before.
 *
 * It is defined here, among macros only, because prism_xallocator.h reads it
 * from inside the extern "C" block mrc_common.h puts around prism.h, where a
 * header carrying declarations would change their linkage under
 * MRB_USE_CXX_ABI.  GCC and Clang get `__thread` in C and in C++ alike: the
 * compiler glue is C++ in that build while Prism stays C, and C++
 * `thread_local` would reach the variable through a wrapper function the C
 * side does not call.  A compiler not listed can be served by defining
 * MRC_THREAD_LOCAL itself.
 */
#ifndef MRC_THREAD_LOCAL
# if !defined(MRC_USE_THREAD_LOCAL)
#  define MRC_THREAD_LOCAL
# elif defined(_MSC_VER)
#  define MRC_THREAD_LOCAL __declspec(thread)
# elif defined(__GNUC__) || defined(__clang__)
#  define MRC_THREAD_LOCAL __thread
# elif defined(__cplusplus) && __cplusplus >= 201103L
#  define MRC_THREAD_LOCAL thread_local
# elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#  define MRC_THREAD_LOCAL _Thread_local
# else
#  error "MRC_USE_THREAD_LOCAL: no thread-local storage qualifier is known for this compiler; define MRC_THREAD_LOCAL"
# endif
#endif

MRB_END_DECL

#endif  /* MRUBY_COMMON_H */
