/**
** @file mruby/throw.h - mruby exception throwing handler
**
** See Copyright Notice in mruby.h
*/

#ifndef MRB_THROW_H
#define MRB_THROW_H

#if defined(MRB_USE_CXX_ABI) && !defined(__cplusplus)
#  error Trying to use C++ exception handling in C code
#endif

#if defined(MRB_USE_CXX_EXCEPTION)

# if defined(__cplusplus)

#define MRB_TRY(buf) try {
#define MRB_CATCH(buf) } catch(mrb_jmpbuf *e) { if (e != (buf)) { throw e; }
#define MRB_END_EXC(buf)  }

#define MRB_THROW(buf) throw(buf)
typedef void *mrb_jmpbuf_impl;

# else
# error "need to be compiled with C++ compiler"
# endif  /* __cplusplus */

#else

#include <setjmp.h>

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define MRB_SETJMP _setjmp
#define MRB_LONGJMP _longjmp
#elif defined(__MINGW64__) && defined(__GNUC__) && __GNUC__ >= 4
#define MRB_SETJMP __builtin_setjmp
#define MRB_LONGJMP __builtin_longjmp
#else
#define MRB_SETJMP setjmp
#define MRB_LONGJMP longjmp
#endif

#define MRB_TRY(buf) if (MRB_SETJMP((buf)->impl) == 0) {
#define MRB_CATCH(buf) } else {
#define MRB_END_EXC(buf) }

#define MRB_THROW(buf) MRB_LONGJMP((buf)->impl, 1);
#define mrb_jmpbuf_impl jmp_buf

#endif

struct mrb_jmpbuf {
  mrb_jmpbuf_impl impl;
};

#endif  /* MRB_THROW_H */
