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

/* -DDISABLE_XXXX to drop the feature */
//#define DISABLE_REGEXP	        /* regular expression classes */
//#define DISABLE_SPRINTF	/* Kernel.sprintf method */
//#define DISABLE_MATH		/* Math functions */
//#define DISABLE_TIME		/* Time class */
//#define DISABLE_STRUCT	/* Struct class */
//#define DISABLE_STDIO		/* use of stdio */
//#define DISABLE_IO
//#define DISABLE_DIR
//#define DISABLE_SOCKET
//#define DISABLE_PROCESS
//#define DISABLE_ENV
//#define DISABLE_DIGEST	/* requires CommonCrypto or OpenSSL */
//#define DISABLE_PACK
//#define DISABLE_SYSLOG	/* requires Syslog */
//#define DISABLE_ERRNO
//#define DISABLE_REQUIRE
//#define DISABLE_RANDOM

#define INCLUDE_ENCODING

#undef  HAVE_UNISTD_H /* WINDOWS */
#define HAVE_UNISTD_H /* LINUX */

#ifdef __APPLE__
#define USE_DIGEST_OSX_COMMONCRYPTO
#else
#define USE_DIGEST_OPENSSL
#endif

/* end of configuration */

#ifdef MRB_USE_FLOAT
  typedef float mrb_float;
# define mrb_float_to_str(buf, i) sprintf((buf), "%.7e", (i))
# define str_to_mrb_float(buf) (mrb_float)strtof((buf),NULL)
#else
  typedef double mrb_float;
# define mrb_float_to_str(buf, i) sprintf((buf), "%.16e", (i))
# define str_to_mrb_float(buf) (mrb_float)strtod((buf),NULL)
#endif

#ifdef MRB_NAN_BOXING
# ifdef MRB_INT64
#  error Cannot use NaN boxing when mrb_int is 64bit
# else
   typedef int32_t mrb_int;
#  define MRB_INT_MIN INT32_MIN
#  define MRB_INT_MAX INT32_MAX
#  define mrb_int_to_str(buf, i) sprintf((buf), "%d", (i))
#  define str_to_mrb_int(buf) (mrb_int)strtol((buf), NULL, 10)
# endif
#else
# ifdef MRB_INT64
   typedef int64_t mrb_int;
#  define MRB_INT_MIN INT64_MIN
#  define MRB_INT_MAX INT64_MAX
#  define mrb_int_to_str(buf, i) sprintf((buf), "%ld", (i))
#  define str_to_mrb_int(buf) (mrb_int)strtoll((buf), NULL, 10)
# else
   typedef int mrb_int;
#  define MRB_INT_MIN INT_MIN
#  define MRB_INT_MAX INT_MAX
#  define mrb_int_to_str(buf, i) sprintf((buf), "%d", (i))
#  define str_to_mrb_int(buf) (mrb_int)strtol((buf), NULL, 10)
# endif
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
#ifndef DISABLE_IO
#define ENABLE_IO
#endif
#ifndef DISABLE_DIR
#define ENABLE_DIR
#endif
#ifndef DISABLE_SOCKET
#define ENABLE_SOCKET
#endif
#ifndef DISABLE_PROCESS
#define ENABLE_PROCESS
#endif
#ifndef DISABLE_ENV
#define ENABLE_ENV
#endif
#ifndef DISABLE_DIGEST
#define ENABLE_DIGEST
#endif
#ifndef DISABLE_PACK
#define ENABLE_PACK
#endif
#ifndef DISABLE_SYSLOG
#define ENABLE_SYSLOG
#endif
#ifndef DISABLE_ERRNO
#define ENABLE_ERRNO
#endif
#ifndef DISABLE_REQUIRE
#define ENABLE_REQUIRE
#endif
#ifndef DISABLE_RANDOM
#define ENABLE_RANDOM
#endif

#ifndef FALSE
# define FALSE 0
#endif

#ifndef TRUE
# define TRUE 1
#endif

#ifdef _MSC_VER
# define inline __inline
# define snprintf _snprintf
# define isnan _isnan
# define isinf(n) (!_finite(n) && !_isnan(n))
#endif

#endif  /* MRUBYCONF_H */
