/*
** mrbconf.h - mruby core configuration
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBYCONF_H
#define MRUBYCONF_H

#include <stdint.h>

/* configuration options: */
/* add -DMRB_USE_FLOAT to use float instead of double for floating point numbers */
//#define MRB_USE_FLOAT

/* add -DMRB_INT64 to use 64bit integer for mrb_int */
//#define MRB_INT64

/* represent mrb_value in boxed double; conflict with MRB_USE_FLOAT */
//#define MRB_NAN_BOXING

/* define on big endian machines; used by MRB_NAN_BOXING */
//#define MRB_ENDIAN_BIG

/* argv max size in mrb_funcall */
//#define MRB_FUNCALL_ARGC_MAX 16 

/* number of object per heap page */
//#define MRB_HEAP_PAGE_SIZE 1024

/* use segmented list for IV table */
//#define MRB_USE_IV_SEGLIST

/* initial size for IV khash; ignored when MRB_USE_IV_SEGLIST is set */
//#define MRB_IVHASH_INIT_SIZE 8

/* default size of khash table bucket */
//#define KHASH_DEFAULT_SIZE 32

/* allocated memory address alignment */
//#define POOL_ALIGNMENT 4

/* page size of memory pool */
//#define POOL_PAGE_SIZE 16000

/* -DDISABLE_XXXX to drop following features */
//#define DISABLE_SPRINTF	/* Kernel.sprintf method */
//#define DISABLE_MATH		/* Math functions */
//#define DISABLE_TIME		/* Time class */
//#define DISABLE_STRUCT	/* Struct class */
//#define DISABLE_STDIO		/* use of stdio */

/* -DENABLE_XXXX to enable following features */
//#define ENABLE_DEBUG		/* hooks for debugger */

/* end of configuration */

#ifdef MRB_USE_FLOAT
  typedef float mrb_float;
# define mrb_float_to_str(buf, i) sprintf(buf, "%.7e", i)
# define str_to_mrb_float(buf) strtof(buf, NULL)
#else
  typedef double mrb_float;
# define mrb_float_to_str(buf, i) sprintf(buf, "%.16e", i)
# define str_to_mrb_float(buf) strtod(buf, NULL)
#endif

#ifdef MRB_INT64
# ifdef MRB_NAN_BOXING
#  error Cannot use NaN boxing when mrb_int is 64bit
# else
   typedef int64_t mrb_int;
#  define MRB_INT_MIN INT64_MIN
#  define MRB_INT_MAX INT64_MAX
#  define str_to_mrb_int(buf) strtoll(buf, NULL, 10)
# endif
#else
  typedef int32_t mrb_int;
# define MRB_INT_MIN INT32_MIN
# define MRB_INT_MAX INT32_MAX
# define str_to_mrb_int(buf) strtol(buf, NULL, 10)
#endif
typedef short mrb_sym;

/* define ENABLE_XXXX from DISABLE_XXX */
#ifndef DISABLE_REGEXP
#define ENABLE_REGEXP
#endif
#ifndef DISABLE_SPRINTF
#define ENABLE_SPRINTF
#endif
#ifndef DISABLE_MATH
#define ENABLE_MATH
#endif
#ifndef DISABLE_TIME
#define ENABLE_TIME
#endif
#ifndef DISABLE_STRUCT
#define ENABLE_STRUCT
#endif
#ifndef DISABLE_STDIO
#define ENABLE_STDIO
#endif
#ifndef ENABLE_DEBUG
#define DISABLE_DEBUG
#endif

#ifndef FALSE
# define FALSE 0
#endif

#ifndef TRUE
# define TRUE 1
#endif

#ifdef _MSC_VER
# include <float.h>
# define inline __inline
# define snprintf _snprintf
# define isnan _isnan
# define isinf(n) (!_finite(n) && !_isnan(n))
# define strtoll _strtoi64
# define PRId32 "I32d"
# define PRId64 "I64d"
#else
# include <inttypes.h>
#endif

#endif  /* MRUBYCONF_H */
