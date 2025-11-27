/**
** @file mruby/bigint.h - Multi-precision Integer
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_BIGINT_H
#define MRUBY_BIGINT_H
/*
 * FREE GMP - a public domain implementation of a subset of the
 *           gmp library
 *
 * I hereby place the file in the public domain.
 *
 * Do whatever you want with this code. Change it. Sell it. Claim you
 *  wrote it.
 * Bugs, complaints, flames, rants: please send email to
 *    Mark Henderson <markh@wimsey.bc.ca>
 * I'm already aware that fgmp is considerably slower than gmp
 *
 * CREDITS:
 *  Paul Rouse <par@r-cube.demon.co.uk> - generic bugfixes, mpz_sqrt and
 *    mpz_sqrtrem, and modifications to get fgmp to compile on a system
 *    with int and long of different sizes (specifically MS-DOS,286 compiler)
 *  Also see the file "notes" included with the fgmp distribution, for
 *    more credits.
 *
 * VERSION 1.0 - beta 5
 */

#include <sys/types.h>

#if defined(MRB_INT32) && defined(_WIN32) && !defined(MRB_NO_MPZ64BIT)
#define MRB_NO_MPZ64BIT
#endif

#ifdef MRB_NO_MPZ64BIT
typedef uint16_t mp_limb;
typedef uint32_t mp_dbl_limb;
typedef int32_t mp_dbl_limb_signed;
#define MPZ_DIG_SIZE 16
#else
typedef uint32_t mp_limb;
typedef uint64_t mp_dbl_limb;
typedef int64_t mp_dbl_limb_signed;
#define MPZ_DIG_SIZE 32
#endif

#define RBIGINT_EMBED_SIZE_MAX ((sizeof(void*) * 3) / sizeof(mp_limb))

typedef struct _mpz_t {
  mp_limb *p;
  short sn;
  size_t sz;
} mpz_t;

struct RBigint {
  MRB_OBJECT_HEADER;
  union {
    mpz_t heap;
    mp_limb ary[RBIGINT_EMBED_SIZE_MAX];
  } as;
};
#define RBIGINT(v) ((struct RBigint*)mrb_ptr(v))

/*
 *  flags of struct RBigint
 *
 *  6..:  UNUSED
 *  4..5: sign flags
 *        00: negative  (<--> -1)
 *        01: zero      (<-->  0)
 *        10: positive  (<--> +1)
 *        11: UNUSED
 *  0..3: size of embedded array; 15 means used with heap
 */

#define RBIGINT_EMBED_SIZE_MASK 0x0f
#define RBIGINT_EMBED_SIZE_OVER RBIGINT_EMBED_SIZE_MASK
#define RBIGINT_EMBED_SIZE_SHIFT 0
#define RBIGINT_EMBED_SIGN_MASK 0x03
#define RBIGINT_EMBED_SIGN_SHIFT 4

#define RBIGINT_ARY(m) (RBIGINT_EMBED_P(m) ? RBIGINT_EMBED_ARY(m) : RBIGINT_HEAP_ARY(m))
#define RBIGINT_SIGN(m) (RBIGINT_EMBED_P(m) ? RBIGINT_EMBED_SIGN(m) : RBIGINT_HEAP_SIGN(m))
#define RBIGINT_SIZE(m) (RBIGINT_EMBED_P(m) ? RBIGINT_EMBED_SIZE(m) : RBIGINT_HEAP_SIZE(m))

#define RBIGINT_HEAP_ARY(m) ((m)->as.heap.p)
#define RBIGINT_HEAP_SIGN(m) ((m)->as.heap.sn)
#define RBIGINT_HEAP_SIZE(m) ((m)->as.heap.sz)
#define RBIGINT_SET_HEAP(m) do { \
  (m)->flags |= RBIGINT_EMBED_SIZE_OVER << RBIGINT_EMBED_SIZE_SHIFT; \
} while (0)
#define RBIGINT_SET_HEAP_SIGN(m, s) do { \
  (m)->as.heap.sn = (s); \
} while (0)
#define RBIGINT_SET_HEAP_SIZE(m, s) do { \
  (m)->as.heap.sz = (s); \
} while (0)

#define RBIGINT_EMBED_P(m) ((((m)->flags >> RBIGINT_EMBED_SIZE_SHIFT) & RBIGINT_EMBED_SIZE_MASK) < RBIGINT_EMBED_SIZE_OVER)
#define RBIGINT_EMBED_ARY(m) ((m)->as.ary)
#define RBIGINT_EMBED_SIGN(m) ((short)(((m)->flags >> RBIGINT_EMBED_SIGN_SHIFT) & RBIGINT_EMBED_SIGN_MASK) - 1)
#define RBIGINT_EMBED_SIZE(m) (size_t)(((m)->flags >> RBIGINT_EMBED_SIZE_SHIFT) & RBIGINT_EMBED_SIZE_MASK)
#define RBIGINT_SET_EMBED_ZERO(m) do { \
  (m)->flags &= ~(RBIGINT_EMBED_SIZE_MASK << RBIGINT_EMBED_SIZE_SHIFT); \
} while (0)
#define RBIGINT_SET_EMBED_SIGN(m, s) do { \
  (m)->flags = ((((s) + 1) & RBIGINT_EMBED_SIGN_MASK) << RBIGINT_EMBED_SIGN_SHIFT) | \
               ((m)->flags & ~(RBIGINT_EMBED_SIGN_MASK << RBIGINT_EMBED_SIGN_SHIFT)); \
} while (0)
#define RBIGINT_SET_EMBED_SIZE(m, s) do { \
  size_t s_tmp = (s); \
  mrb_assert((s_tmp) <= RBIGINT_EMBED_SIZE_MAX); \
  RBIGINT_SET_EMBED_ZERO(m); \
  (m)->flags |= (s_tmp) << RBIGINT_EMBED_SIZE_SHIFT; \
} while (0)

mrb_static_assert_object_size(struct RBigint);

#endif  /* MRUBY_BIGINT_H */
