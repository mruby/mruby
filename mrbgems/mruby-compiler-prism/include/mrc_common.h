#ifndef MRC_COMMON_H
#define MRC_COMMON_H

#include <stdint.h>

#define MRC_STRINGIZE0(expr) #expr
#define MRC_STRINGIZE(expr) MRC_STRINGIZE0(expr)

#if defined(PICORB_VM_MRUBY)
  #if !defined(MRC_TARGET_MRUBY)
    #define MRC_TARGET_MRUBY
  #endif
  #include <mruby.h>
#endif
#if defined(PICORB_VM_MRUBYC)
  #if !defined(MRC_TARGET_MRUBYC)
    #define MRC_TARGET_MRUBYC
  #endif
  #include <mrubyc.h>
  #define mrb_state void
#endif

#if !defined(MRC_TARGET_MRUBY) && !defined(PICORB_VM_MRUBYC)
  /* May be building mrbc (picorbc) */
  #define mrb_state void
#endif

#if !defined(PRISM_XALLOCATOR)
  #define PRISM_XALLOCATOR
#endif
#include "prism.h"

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
