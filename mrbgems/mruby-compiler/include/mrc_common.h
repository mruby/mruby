#ifndef MRC_COMMON_H
#define MRC_COMMON_H

#define MRC_STRINGIZE0(expr) #expr
#define MRC_STRINGIZE(expr) MRC_STRINGIZE0(expr)

#if defined(PICORB_VM_MRUBY)
  #if !defined(MRC_TARGET_MRUBY)
    #define MRC_TARGET_MRUBY
  #endif
#endif
#if defined(PICORB_VM_MRUBYC)
  #if !defined(MRC_TARGET_MRUBYC)
    #define MRC_TARGET_MRUBYC
  #endif
#endif

/* mruby.h must be included before <stdint.h> (it enforces this ordering on
   some platforms) and it carries the core API's linkage, so include it up
   front -- and outside the extern "C" wrap below. prism.h pulls mruby.h in
   transitively through prism_xallocator.h; keeping it out of the wrap means
   that under MRB_USE_CXX_ABI the core keeps its C++ linkage while only Prism
   gets C linkage. */
#if defined(MRC_TARGET_MRUBY)
  #include <mruby.h>
#elif defined(MRC_TARGET_MRUBYC)
  #include <mrubyc.h>
  #define mrb_state void
#else
  /* May be building standalone mrbc. mruby.h would declare a core API this
     binary does not link, but mrbconf.h on its own is self-contained, and it
     is what settles the target's mrb_int width -- which mrc_int has to match,
     because this mrbc dumps irep for that target (see MRC_INT32 below). */
  #include <stdint.h>
  #include <mrbconf.h>
  #define mrb_state void
#endif

#include <stdint.h>
/* For the PRI* macros MRC_PRId and friends stand for below; the 64-bit ones
   are supplied where the libc leaves them out, see prism_xallocator.h. */
#include <inttypes.h>

#if !defined(PRISM_XALLOCATOR)
  #define PRISM_XALLOCATOR
#endif
/* Prism is a vendored C library and is always compiled as C (its generated
   code uses C constructs -- designated initializers, implicit void* casts --
   that a C++ compiler cannot build). When this header is included from a C++
   translation unit -- e.g. an MRB_USE_CXX_ABI build -- its declarations must
   use C linkage so they match the C-compiled Prism objects. */
/* Prism 2 keeps the parser, the options and the constant pool opaque behind
   its public headers. The compiler uses the constant pool as its symbol
   table, switches the parser's file path as a parse crosses from one input
   file to the next, and embeds the parser in its context, so it reads the
   internal headers too. Prism is vendored and built with the compiler, so
   the two always agree on these layouts. */
#ifdef __cplusplus
extern "C" {
#endif
#include "prism.h"
#include "prism/internal/allocator.h"
#include "prism/internal/arena.h"
#include "prism/internal/buffer.h"
#include "prism/internal/constant_pool.h"
#include "prism/internal/integer.h"
#include "prism/internal/line_offset_list.h"
#include "prism/internal/options.h"
#include "prism/internal/parser.h"
#ifdef __cplusplus
}
#endif

#ifndef MRC_COMMIT_TIMESTAMP
  #define MRC_COMMIT_TIMESTAMP "unknown"
#endif
#ifndef MRC_COMMIT_BRANCH
  #define MRC_COMMIT_BRANCH "unknown"
#endif
#ifndef MRC_COMMIT_HASH
  #define MRC_COMMIT_HASH "unknown"
#endif
#define MRC_BUILD_INFO MRC_COMMIT_TIMESTAMP " " MRC_COMMIT_BRANCH " " MRC_COMMIT_HASH

#ifdef MRB_USE_CXX_ABI
#define MRC_USE_CXX_ABI
#endif

#ifdef __cplusplus
  #ifdef MRC_USE_CXX_ABI
    #define MRC_BEGIN_DECL
    #define MRC_END_DECL
  #else
    #define MRC_BEGIN_DECL extern "C" {
    #define MRC_END_DECL }
  #endif
#else
  /** Start declarations in C mode */
  # define MRC_BEGIN_DECL
  /** End declarations in C mode */
  # define MRC_END_DECL
#endif

/** Declare a public mruby API function. */
#ifndef MRC_API
#if defined(MRC_BUILD_AS_DLL)
#if defined(MRC_CORE) || defined(MRC_LIB)
# define MRC_API __declspec(dllexport)
#else
# define MRC_API __declspec(dllimport)
#endif
#else
# define MRC_API extern
#endif
#endif

#if defined(__cplusplus) || (defined(__bool_true_false_are_defined) && __bool_true_false_are_defined)
typedef bool mrc_bool;

# ifndef FALSE
#  define FALSE false
# endif
# ifndef TRUE
#  define TRUE true
# endif
#else
# if __STDC_VERSION__ >= 199901L
typedef _Bool mrc_bool;
# else
typedef uint8_t mrc_bool;
# endif

# ifndef FALSE
#  define FALSE 0
# endif
# ifndef TRUE
#  define TRUE 1
# endif
#endif

/* mrc_int must be as wide as the VM's mrb_int and no wider: the pool literals
   and the constant folding below are dumped for a target whose loader rejects
   an IREP_TT_INT64 entry unless it was built with MRB_INT64 (src/load.c). */
#if defined(MRB_INT32) && !defined(MRC_INT32)
#define MRC_INT32 1
#endif

#if !defined(MRC_INT32)
#define MRC_INT64 1
#endif
#if !defined(MRC_32BIT)
#define MRC_64BIT 1
#endif

#if defined(MRC_INT64)
  typedef int64_t mrc_int;
  typedef uint64_t mrc_uint;
  #define MRC_INT_BIT 64
  #define MRC_INT_MIN INT64_MIN
  #define MRC_INT_MAX INT64_MAX
  #define MRC_PRIo PRIo64
  #define MRC_PRId PRId64
  #define MRC_PRIx PRIx64
#else
  typedef int32_t mrc_int;
  typedef uint32_t mrc_uint;
  #define MRC_INT_BIT 32
  #define MRC_INT_MIN INT32_MIN
  #define MRC_INT_MAX INT32_MAX
  #define MRC_PRIo PRIo32
  #define MRC_PRId PRId32
  #define MRC_PRIx PRIx32
#endif

#ifdef MRB_NO_FLOAT
#define MRC_NO_FLOAT
#endif
#ifdef MRB_USE_FLOAT32
#define MRC_USE_FLOAT32
#endif

#ifndef MRC_NO_FLOAT
#ifdef MRC_USE_FLOAT32
  typedef float mrc_float;
#else
  typedef double mrc_float;
#endif
#endif

typedef uint32_t mrc_sym;
typedef uint8_t mrc_code;

/**
 * \class mrb_aspec
 *
 * Specifies the number of arguments a function takes
 *
 * Example: `MRB_ARGS_REQ(2) | MRB_ARGS_OPT(1)` for a method that expects 2..3 arguments
 */
typedef uint32_t mrc_aspec;

#ifdef MRC_DEBUG
  #include <assert.h>
  #define mrc_assert(p) assert(p)
  #define mrc_assert_int_fit(t1,n,t2,max) assert((n)>=0 && ((sizeof(n)<=sizeof(t2))||(n<=(t1)(max))))
#else
  #define mrc_assert(p) ((void)0)
  #define mrc_assert_int_fit(t1,n,t2,max) ((void)0)
#endif

#endif  /* MRC_COMMON_H */
