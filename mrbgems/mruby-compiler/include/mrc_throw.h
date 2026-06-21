/**
** @file mruby/throw.h - mruby exception throwing handler
**
** See Copyright Notice in mruby.h
*/

#ifndef MRC_THROW_H
#define MRC_THROW_H

#if defined(MRC_USE_CXX_ABI) && !defined(__cplusplus)
#  error Trying to use C++ exception handling in C code
#endif

#if defined(MRC_USE_CXX_EXCEPTION)

# if defined(__cplusplus)

#define MRC_TRY(buf) try {
#define MRC_CATCH(buf) } catch(mrc_jmpbuf *e) { if (e != (buf)) { throw e; }
#define MRC_END_EXC(buf)  }

#define MRC_THROW(buf) throw(buf)
typedef void *mrc_jmpbuf_impl;

# else
# error "need to be compiled with C++ compiler"
# endif  /* __cplusplus */

#else

#include <setjmp.h>

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define MRC_SETJMP _setjmp
#define MRC_LONGJMP _longjmp
#elif defined(__MINGW64__) && defined(__GNUC__) && __GNUC__ >= 4
#define MRC_SETJMP __builtin_setjmp
#define MRC_LONGJMP __builtin_longjmp
#else
#define MRC_SETJMP setjmp
#define MRC_LONGJMP longjmp
#endif

#define MRC_TRY(buf) if (MRC_SETJMP((buf)->impl) == 0) {
#define MRC_CATCH(buf) } else {
#define MRC_END_EXC(buf) }

#define MRC_THROW(buf) MRC_LONGJMP((buf)->impl, 1);
#define mrc_jmpbuf_impl jmp_buf

#endif

struct mrc_jmpbuf {
  mrc_jmpbuf_impl impl;
};

#endif  /* MRC_THROW_H */
