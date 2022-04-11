/**
** @file mruby/bigint.h - Multi-precision, Integer
**
** See Copyright Notice in mruby.h
*/
#ifdef MRB_USE_BIGINT

#ifndef MRUBY_BIGINT_H
#define MRUBY_BIGINT_H
/*
 * FREE GMP - a public domain implementation of a subset of the
 *           gmp library
 *
 * I hearby place the file in the public domain.
 *
 * Do whatever you want with this code. Change it. Sell it. Claim you
 *  wrote it.
 * Bugs, complaints, flames, rants: please send email to
 *    Mark Henderson <markh@wimsey.bc.ca>
 * I'm already aware that fgmp is considerably slower than gmp
 *
 * CREDITS:
 *  Paul Rouse <par@r-cube.demon.co.uk> - generic bug fixes, mpz_sqrt and
 *    mpz_sqrtrem, and modifications to get fgmp to compile on a system
 *    with int and long of different sizes (specifically MS-DOS,286 compiler)
 *  Also see the file "notes" included with the fgmp distribution, for
 *    more credits.
 *
 * VERSION 1.0 - beta 5
 */

#include <sys/types.h>

#if !(defined(MRB_64BIT) || defined(MRB_INT64))
/*
 * The values below are for 32 bit machines (i.e. machines with a
 *  32 bit long type)
 * You'll need to change them, if you're using something else
 * If DIGITBITS is odd, see the comment at the top of mpz_sqrtrem
 */
typedef int32_t mp_limb;
#define LMAX 0x3fffffffL
#define LC   0xc0000000L
#define OVMASK 0x2
#define CMASK (LMAX+1)
#define HLMAX 0x7fffL
#define HCMASK (HLMAX + 1)
#define HIGH(x) (((x) & 0x3fff8000L) >> 15)
#define LOW(x)  ((x) & 0x7fffL)

#else
/* 64 bit long type */
typedef int64_t mp_limb;
#define LMAX 0x3fffffffffffffffLL
#define LC 0xc000000000000000LL
#define OVMASK 0x2
#define CMASK (LMAX+1)
#define HLMAX 0x7fffffffLL
#define HCMASK (HLMAX + 1)
#define HIGH(x) (((x) & 0x3fffffff80000000LL) >> 31)
#define LOW(x) ((x) & 0x7fffffffLL)
#endif

typedef struct _mpz_t {
  mp_limb *p;
  short sn;
  size_t sz;
} mpz_t;

struct RBigint {
  MRB_OBJECT_HEADER;
  mpz_t mp;
};
#define RBIGINT(v) ((struct RBigint*)mrb_ptr(v))

#define iabs(x) ((x>0) ? (x) : (-x))
#define imax(x,y) ((x>y)?x:y)
#define LONGBITS (sizeof(mp_limb)*8)
#define DIGITBITS (LONGBITS-2)
#define HALFDIGITBITS ((LONGBITS-2)/2)

#define hd(x,i)  (((size_t)(i)>=2*((x)->sz))? 0:(((i)%2) ? HIGH((x)->p[(i)/2]) \
    : LOW((x)->p[(i)/2])))
#define dg(x,i) (((size_t)(i) < (x)->sz) ? ((x)->p)[i] : 0)

#define RBIGINT(v) ((struct RBigint*)mrb_ptr(v))

mrb_value mrb_bint_new_int(mrb_state *mrb, mrb_int x);
mrb_value mrb_bint_add(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_sub(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_mul(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_div(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_divmod(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_add_ii(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_value mrb_bint_sub_ii(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_value mrb_bint_mul_ii(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_value mrb_bint_div_ii(mrb_state *mrb, mrb_int x, mrb_int y);
mrb_value mrb_bint_mod(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_rem(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_pow(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_powm(mrb_state *mrb, mrb_value x, mrb_int y, mrb_value z);
mrb_value mrb_bint_and(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_or(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_xor(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_bint_rev(mrb_state *mrb, mrb_value x);
mrb_value mrb_bint_lshift(mrb_state *mrb, mrb_value x, mrb_int width);
mrb_value mrb_bint_rshift(mrb_state *mrb, mrb_value x, mrb_int width);
mrb_value mrb_bint_to_s(mrb_state *mrb, mrb_value x, mrb_int base);
#ifndef MRB_NO_FLOAT
mrb_float mrb_bint_as_float(mrb_state *mrb, mrb_value x);
#endif
mrb_int mrb_bint_as_int(mrb_state *mrb, mrb_value x);
mrb_int mrb_bint_cmp(mrb_state *mrb, mrb_value x, mrb_value y);
void mrb_gc_free_bint(mrb_state *mrb, struct RBasic *x);
#endif

#endif  /* MRB_USE_BIGINT */
