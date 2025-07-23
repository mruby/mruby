/**
** @file mruby/bigint.c - Multi-precision Integer
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/object.h>
#include <mruby/numeric.h>
#include <mruby/array.h>
#include <mruby/string.h>
#include <mruby/internal.h>
#include <string.h>
#include "bigint.h"

#define DIG_SIZE (MPZ_DIG_SIZE)
#define DIG_BASE (1ULL << DIG_SIZE)
#define DIG_MASK (DIG_BASE - 1)
#define HIGH(x) ((x) >> DIG_SIZE)
#define LOW(x)  ((x) & DIG_MASK)

#define iabs(x) (((x)>0)?(x):(-x))
#define imax(x,y) (((x)>(y))?(x):(y))
#define imin(x,y) (((x)<(y))?(x):(y))
#define dg(x,i) (((size_t)i < (x)->sz)?(x)->p[i]:0)

static void
mpz_init(mrb_state *mrb, mpz_t *s)
{
  s->p = NULL;
  s->sn = 0;
  s->sz = 0;
}

/* Helper macros for safer temporary variable management */
#define MPZ_TMP_INIT(mrb, var) \
  mpz_t var; \
  mpz_init(mrb, &var)

#define MPZ_TMP_CLEAR(mrb, var) \
  mpz_clear(mrb, &var)

static void
mpz_realloc(mrb_state *mrb, mpz_t *x, size_t size)
{
  if (x->sz < size) {
    /* Check for overflow in size calculation */
    if (size > SIZE_MAX / sizeof(mp_limb)) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "bigint size too large");
    }

    size_t old_sz = x->sz;
    x->p = (mp_limb*)mrb_realloc(mrb, x->p, size * sizeof(mp_limb));

    /* Zero-initialize new limbs */
    for (size_t i = old_sz; i < size; i++) {
      x->p[i] = 0;
    }
    x->sz = size;
  }
}

static void
mpz_set(mrb_state *mrb, mpz_t *y, mpz_t *x)
{
  size_t i, k = x->sz;

  mpz_realloc(mrb, y, k);
  for (i=0;i < k; i++)
    y->p[i] = x->p[i];

  y->sz = k;
  y->sn = x->sn;
}

static void
mpz_init_set(mrb_state *mrb, mpz_t *s, mpz_t *t)
{
  mpz_init(mrb, s);
  mpz_set(mrb, s, t);
}

static void
mpz_set_int(mrb_state *mrb, mpz_t *y, mrb_int v)
{
  mrb_uint u;

  if (v == 0) {
    y->sn=0;
    u = 0;
  }
  else if (v > 0) {
    y->sn = 1;
    u = v;
  }
  else /* if (v < 0) */ {
    y->sn = -1;
    if (v == MRB_INT_MIN) u = v;
    else u = -v;
  }
#if MRB_INT_BIT > DIG_SIZE
  if ((u & ~DIG_MASK) != 0) {
    mpz_realloc(mrb, y, 2);
    y->p[1] = (mp_limb)HIGH(u);
    y->p[0] = (mp_limb)LOW(u);
    return;
  }
#endif
  mpz_realloc(mrb, y, 1);
  y->p[0] = (mp_limb)u;
}

static void
mpz_set_uint64(mrb_state *mrb, mpz_t *y, uint64_t u)
{
  size_t len = 0;

  for (uint64_t u0=u; u0; u0>>=DIG_SIZE,len++)
    ;
  y->sn = (u != 0);
  mpz_realloc(mrb, y, len);
  for (size_t i=0; i<len; i++) {
    y->p[i] = (mp_limb)LOW(u);
    u >>= DIG_SIZE;
  }
}

#ifdef MRB_INT32
static void
mpz_set_int64(mrb_state *mrb, mpz_t *y, int64_t v)
{
  uint64_t u;

  if (v < 0) {
    if (v == INT64_MIN) u = v;
    else u = -v;
  }
  else {
    u = v;
  }
  mpz_set_uint64(mrb, y, u);
  if (v < 0) {
    y->sn = -1;
  }
}
#endif

static void
mpz_init_set_int(mrb_state *mrb, mpz_t *y, mrb_int v)
{
  mpz_init(mrb, y);
  mpz_set_int(mrb, y, v);
}

static void
mpz_clear(mrb_state *mrb, mpz_t *s)
{
  if (s->p) {
    mrb_free(mrb, s->p);
    s->p = NULL;  /* Prevent double-free */
  }
  s->sn = 0;
  s->sz = 0;
}

static void
mpz_move(mrb_state *mrb, mpz_t *y, mpz_t *x)
{
  mpz_clear(mrb, y);
  y->sn = x->sn;
  y->sz = x->sz;
  y->p = x->p;
  x->p = NULL;
  x->sn = 0;
  x->sz = 0;
}

static size_t
digits(mpz_t *x)
{
  size_t i;

  if (x->sz == 0) return 0;
  for (i = x->sz - 1; x->p[i] == 0 && i > 0; i--)
    ;
  return i+1;
}

static void
trim(mpz_t *x)
{
  while (x->sz && x->p[x->sz-1] == 0) {
    x->sz--;
  }
}

/* z = x + y, without regard for sign */
static void
uadd(mrb_state *mrb, mpz_t *z, mpz_t *x, mpz_t *y)
{
  if (y->sz < x->sz) {
    mpz_t *t;                   /* swap x,y */
    t=x; x=y; y=t;
  }

  /* now y->sz >= x->sz */
  mpz_realloc(mrb, z, y->sz+1);

  mp_dbl_limb c = 0;
  size_t i;
  for (i=0; i<x->sz; i++) {
    c += (mp_dbl_limb)y->p[i] + (mp_dbl_limb)x->p[i];
    z->p[i] = LOW(c);
    c >>= DIG_SIZE;
  }
  for (;i<y->sz; i++) {
    c += y->p[i];
    z->p[i] = LOW(c);
    c >>= DIG_SIZE;
  }
  z->p[y->sz] = (mp_limb)c;
}

/* z = y - x, ignoring sign */
/* precondition: abs(y) >= abs(x) */
static void
usub(mrb_state *mrb, mpz_t *z, mpz_t *y, mpz_t *x)
{
  mpz_realloc(mrb, z, (size_t)(y->sz));
  mp_dbl_limb_signed b = 0;
  size_t i;
  for (i=0;i<x->sz;i++) {
    b += (mp_dbl_limb_signed)y->p[i];
    b -= (mp_dbl_limb_signed)x->p[i];
    z->p[i] = LOW(b);
    b = HIGH(b);
  }
  for (;i<y->sz; i++) {
    b += y->p[i];
    z->p[i] = LOW(b);
    b = HIGH(b);
  }
  z->sz = digits(z);
}

/* compare abs(x) and abs(y) */
static int
ucmp(mpz_t *y, mpz_t *x)
{
  if (y->sz < x->sz) return -1;
  if (y->sz > x->sz) return 1;
  if (x->sz == 0) return 0;
  for (size_t i=x->sz-1;; i--) {
    mp_limb a = y->p[i];
    mp_limb b = x->p[i];
    if (a > b) return 1;
    if (a < b) return -1;
    if (i == 0) break;
  }
  return 0;
}

#define zero_p(x) ((x)->sn == 0)

/* check if all digits are zero */
static int
uzero_p(mpz_t *x)
{
  if (x->sz == 0) return 1;
  for (size_t i=x->sz-1;; i--) {
    if (x->p[i] != 0)
      return 0;
    if (i == 0) break;
  }
  return 1;
}

static void
zero(mpz_t *x)
{
  x->sn=0;
  if (x->p) {
    x->sz=1;
    x->p[0]=0;
  }
  else {
    x->sz=0;
  }
}

/* z = x + y */
static void
mpz_add(mrb_state *mrb, mpz_t *zz, mpz_t *x, mpz_t *y)
{
  if (zero_p(x)) {
    mpz_set(mrb, zz, y);
    return;
  }
  if (zero_p(y)) {
    mpz_set(mrb, zz, x);
    return;
  }
  mpz_t z;
  mpz_init(mrb, &z);

  if (x->sn > 0 && y->sn > 0) {
    uadd(mrb, &z, x, y);
    z.sn = 1;
  }
  else if (x->sn < 0 && y->sn < 0) {
    uadd(mrb, &z, x, y);
    z.sn = -1;
  }
  else {
    int mg;

    /* signs differ */
    if ((mg = ucmp(x,y)) == 0) {
      zero(&z);
    }
    else if (mg > 0) {  /* abs(y) < abs(x) */
      usub(mrb, &z, x, y);
      z.sn = (x->sn > 0 && y->sn < 0) ? 1 : (-1);
    }
    else { /* abs(y) > abs(x) */
      usub(mrb, &z, y, x);
      z.sn = (x->sn < 0 && y->sn > 0) ? 1 : (-1);
    }
  }
  trim(&z);
  mpz_move(mrb, zz, &z);
}

/* x += n                                              */
/*   ignores sign of x                                 */
/*   assumes n is positive and small (fits in mp_limb) */
static void
mpz_add_int(mrb_state *mrb, mpz_t *x, mrb_int n)
{
  // If n is zero, no operation is needed
  if (n == 0) return;

  // Assume x is positive and n is a small positive integer
  mp_dbl_limb carry = n; // Initialize carry with n
  for (size_t i = 0; i < x->sz && carry; i++) {
    carry += (mp_dbl_limb)x->p[i]; // Add current limb and carry
    x->p[i] = LOW(carry);          // Store lower 32 bits in current limb
    carry = HIGH(carry);           // Update carry with higher bits
  }

  if (carry != 0) {
    mpz_realloc(mrb, x, x->sz + 1);
    x->p[x->sz-1] = (mp_limb)carry;
    x->sn = 1;
  }
  trim(x);
}

/* z = x - y  -- just use mpz_add - I'm lazy */
static void
mpz_sub(mrb_state *mrb, mpz_t *z, mpz_t *x, mpz_t *y)
{
  mpz_t u;

  /* Initialize u as a view of y with negated sign - no new memory allocated */
  u.p = y->p;
  u.sz = y->sz;
  u.sn = -(y->sn);
  mpz_add(mrb, z, x, &u);
  /* No mpz_clear needed since u.p points to y->p (no separate allocation) */
}

/* x -= n                                              */
/*   ignores sign of x                                 */
/*   assumes n is positive and small (fits in mp_limb) */
static void
mpz_sub_int(mrb_state *mrb, mpz_t *x, mrb_int n)
{
  // If n is zero, no operation is needed
  if (n == 0) return;

  // If x is zero, set x to n
  if (zero_p(x) || x->sz == 0) {
    mpz_set_int(mrb, x, n);
    return;
  }

  // Initialize borrow and start decrement
  mp_dbl_limb_signed borrow = (mp_limb)n;
  size_t i = 0;

  // Subtract 1 from the least significant limb and propagate if necessary
  borrow = (mp_dbl_limb_signed)x->p[i] - borrow;
  x->p[i] = LOW(borrow);
  borrow = (borrow < 0) ? 1 : 0;

  // Continue through limbs while there is a borrow
  for (i = 1; i < x->sz && borrow; i++) {
    borrow = (mp_dbl_limb_signed)x->p[i] - borrow;
    x->p[i] = LOW(borrow);
    borrow = (borrow < 0) ? 1 : 0;
  }

  // Trim any unnecessary leading zeros
  trim(x);
}

/* w = u * v */
/* Simple Multiply */
static void
mpz_mul(mrb_state *mrb, mpz_t *ww, mpz_t *u, mpz_t *v)
{
  if (zero_p(u) || zero_p(v)) {
    zero(ww);
    return;
  }

  // Ensure consistent operand ordering: smaller operand as second argument
  mpz_t *first, *second;
  if (u->sz <= v->sz) {
    first = u;
    second = v;
  }
  else {
    first = v;
    second = u;
  }

  mpz_t w;
  mpz_init(mrb, &w);
  mpz_realloc(mrb, &w, first->sz + second->sz + 1);

  // Standard multiplication algorithm with consistent operand order
  for (size_t j = 0; j < first->sz; j++) {
    mp_limb u0 = first->p[j];
    if (u0 == 0) continue;

    mp_dbl_limb cc = 0;
    size_t i;
    for (i = 0; i < second->sz; i++) {
      mp_limb v0 = second->p[i];
      cc += (mp_dbl_limb)w.p[i + j] + (mp_dbl_limb)u0 * (mp_dbl_limb)v0;
      w.p[i + j] = LOW(cc);
      cc = HIGH(cc);
    }

    // Propagate carries
    while (cc && (i + j) < w.sz) {
      cc += (mp_dbl_limb)w.p[i + j];
      w.p[i + j] = LOW(cc);
      cc = HIGH(cc);
      i++;
    }
  }

  w.sn = u->sn * v->sn;
  trim(&w);
  mpz_move(mrb, ww, &w);
}

/* number of leading zero bits in digit */
static int
lzb(mp_limb x)
{
  if (x == 0) return 0;
#if (defined(__GNUC__) || __has_builtin(__builtin_clz))
  if (sizeof(mp_limb) == sizeof(int64_t))
    return __builtin_clzll(x);
  else if (sizeof(mp_limb) == sizeof(int32_t))
    return __builtin_clz(x);
#endif

  int j=0;

  for (mp_limb i = ((mp_limb)1 << (DIG_SIZE-1)); i && !(x&i); j++,i>>=1)
    ;
  return j;
}

/* c1 = a>>n */
/* n must be < DIG_SIZE */
static void
urshift(mrb_state *mrb, mpz_t *c1, mpz_t *a, size_t n)
{
  mrb_assert(n < DIG_SIZE);

  if (n == 0)
    mpz_set(mrb, c1, a);
  else if (uzero_p(a)) {
    zero(c1);
  }
  else {
    mpz_t c;
    mp_limb cc = 0;
    mp_dbl_limb rm = (((mp_dbl_limb)1<<n) - 1);

    mpz_init(mrb, &c);
    mpz_realloc(mrb, &c, a->sz);
    for (size_t i=a->sz-1;; i--) {
      c.p[i] = ((a->p[i] >> n) | cc) & DIG_MASK;
      cc = (a->p[i] & rm) << (DIG_SIZE - n);
      if (i == 0) break;
    }
    trim(&c);
    mpz_move(mrb, c1, &c);
  }
}

/* c1 = a<<n */
/* n must be < DIG_SIZE */
static void
ulshift(mrb_state *mrb, mpz_t *c1, mpz_t *a, size_t n)
{
  mrb_assert(n < DIG_SIZE);
  if (n == 0)
    mpz_set(mrb, c1, a);
  else if (uzero_p(a)) {
    zero(c1);
  }
  else {
    mp_limb cc = 0;
    mpz_t c;
    mp_limb rm = (((mp_dbl_limb)1<<n) - 1) << (DIG_SIZE-n);

    mpz_init(mrb, &c);
    mpz_realloc(mrb, &c, a->sz+1);

    size_t i;
    for (i=0; i<a->sz; i++) {
      c.p[i] = ((a->p[i] << n) | cc) & DIG_MASK;
      cc = (a->p[i] & rm) >> (DIG_SIZE-n);
    }
    c.p[i] = cc;
    trim(&c);
    mpz_move(mrb, c1, &c);
  }
}

/* Fast division by single limb */
static void
mpz_div_limb(mrb_state *mrb, mpz_t *q, mpz_t *r, mpz_t *x, mp_limb d)
{
  if (zero_p(x)) {
    zero(q);
    zero(r);
    return;
  }

  if (d == 0) {
    mrb_raise(mrb, E_ZERODIV_ERROR, "divided by 0");
  }

  /* Power-of-2 divisor optimization */
  if ((d & (d - 1)) == 0) {
    /* d is power of 2, use bit operations */
    int shift = 0;
    mp_limb temp = d;
    while (temp > 1) {
      temp >>= 1;
      shift++;
    }

    /* Quotient = x >> shift */
    if (shift == 0) {
      mpz_set(mrb, q, x);
    }
    else {
      /* Manual right shift implementation */
      size_t limb_shift = shift / DIG_SIZE;
      size_t bit_shift = shift % DIG_SIZE;

      if (limb_shift >= x->sz) {
        zero(q);
      }
      else {
        size_t new_size = x->sz - limb_shift;
        mpz_realloc(mrb, q, new_size);

        if (bit_shift == 0) {
          /* Simple limb copy */
          for (size_t i = 0; i < new_size; i++) {
            q->p[i] = x->p[i + limb_shift];
          }
        }
        else {
          /* Bit shift within limbs */
          mp_limb carry = 0;
          for (size_t i = new_size; i > 0; i--) {
            mp_limb current = x->p[i - 1 + limb_shift];
            q->p[i - 1] = (current >> bit_shift) | carry;
            carry = (current << (DIG_SIZE - bit_shift)) & DIG_MASK;
          }
        }
        q->sz = new_size;
        trim(q);
        q->sn = (q->sz == 0) ? 0 : 1;
      }
    }

    /* Remainder = x & (d - 1) */
    mpz_realloc(mrb, r, 1);
    r->p[0] = x->p[0] & (d - 1);
    r->sz = (r->p[0] == 0) ? 0 : 1;
    r->sn = (r->sz == 0) ? 0 : 1;
    return;
  }

  /* General single-limb division */
  if (x->sz == 1) {
    /* Both dividend and divisor are single limb */
    mpz_realloc(mrb, q, 1);
    mpz_realloc(mrb, r, 1);

    q->p[0] = x->p[0] / d;
    r->p[0] = x->p[0] % d;

    q->sz = (q->p[0] == 0) ? 0 : 1;
    q->sn = (q->sz == 0) ? 0 : 1;

    r->sz = (r->p[0] == 0) ? 0 : 1;
    r->sn = (r->sz == 0) ? 0 : 1;
    return;
  }

  /* Multi-limb dividend, single-limb divisor */
  size_t n = x->sz;
  mpz_realloc(mrb, q, n);

  mp_dbl_limb remainder = 0;

  /* Process from most significant limb to least significant */
  for (size_t i = n; i > 0; i--) {
    remainder = (remainder << DIG_SIZE) + x->p[i-1];
    q->p[i-1] = (mp_limb)(remainder / d);
    remainder = remainder % d;
  }

  /* Set remainder */
  mpz_realloc(mrb, r, 1);
  r->p[0] = (mp_limb)remainder;
  r->sz = (remainder == 0) ? 0 : 1;
  r->sn = (r->sz == 0) ? 0 : 1;

  /* Trim leading zeros from quotient */
  trim(q);
  q->sn = (q->sz == 0) ? 0 : 1;
}

/* internal routine to compute x/y and x%y ignoring signs */
/* qq = xx/yy; rr = xx%yy */
static void
udiv(mrb_state *mrb, mpz_t *qq, mpz_t *rr, mpz_t *xx, mpz_t *yy)
{
  /* simple cases */
  int cmp = ucmp(xx, yy);
  if (cmp == 0) {
    mpz_set_int(mrb, qq, 1);
    zero(rr);
    return;
  }
  else if (cmp < 0) {
    zero(qq);
    mpz_set(mrb, rr, xx);
    return;
  }

  /* Fast path for single-limb divisor */
  if (yy->sz == 1) {
    mpz_div_limb(mrb, qq, rr, xx, yy->p[0]);
    return;
  }

  mpz_t q, x, y;

  mrb_assert(yy->sn != 0);      /* divided by zero */
  mrb_assert(yy->sz > 0);       /* divided by zero */
  mpz_init(mrb, &q);
  mpz_init(mrb, &x);
  mpz_init(mrb, &y);
  mpz_realloc(mrb, &x, xx->sz+1);
  size_t yd = digits(yy);
  size_t ns = lzb(yy->p[yd-1]);
  ulshift(mrb, &x, xx, ns);
  ulshift(mrb, &y, yy, ns);
  size_t xd = digits(&x);
  mpz_realloc(mrb, &q, xd-yd+1);  // Quotient has xd-yd+1 digits maximum
  mp_dbl_limb z = y.p[yd-1];
  if (xd>=yd) {
    for (size_t j=xd-yd;; j--) {
      mp_dbl_limb qhat;
      mp_dbl_limb rhat;
      if (j+yd == xd) {
        // Only one high limb available
        mp_dbl_limb dividend = (((mp_dbl_limb)0 << DIG_SIZE) + x.p[j+yd-1]);
        qhat = dividend / z;
        rhat = dividend % z;
      }
      else {
        // Two limbs available - use enhanced estimation for better accuracy
        mp_dbl_limb dividend = ((mp_dbl_limb)x.p[j+yd] << DIG_SIZE) + x.p[j+yd-1];
        qhat = dividend / z;
        rhat = dividend % z;

        // Three-limb pre-adjustment when available (reduces correction iterations)
        if (yd >= 2 && j+yd-2 < x.sz && y.p[yd-2] != 0) {
          mp_dbl_limb y_second = y.p[yd-2];
          mp_dbl_limb x_third = x.p[j+yd-2];

          // Pre-check: if qhat * y_second > rhat * base + x_third, reduce qhat
          if (qhat > 0) {
            mp_dbl_limb left = qhat * y_second;
            mp_dbl_limb right = (rhat << DIG_SIZE) + x_third;

            if (qhat >= ((mp_dbl_limb)1 << DIG_SIZE) || left > right) {
              qhat--;
              rhat += z;
              // Note: This pre-adjustment reduces work in the refinement loop
            }
          }
        }
      }

      // Enhanced qhat refinement step - reduced redundant calculations
      if (yd > 1) {  // Apply refinement for all iterations, including j=0
        mp_dbl_limb y_second = y.p[yd-2];
        mp_dbl_limb x_third = (j+yd-2 < x.sz) ? x.p[j+yd-2] : 0;
        mp_dbl_limb left_side = qhat * y_second;
        mp_dbl_limb right_side = (rhat << DIG_SIZE) + x_third;

        while (qhat >= ((mp_dbl_limb)1 << DIG_SIZE) || (left_side > right_side)) {
          qhat--;
          rhat += z;
          if (rhat >= ((mp_dbl_limb)1 << DIG_SIZE)) break;
          left_side -= y_second;  // Optimized: subtract instead of multiply
          right_side = (rhat << DIG_SIZE) + x_third;
        }
      }
      if (qhat > 0) {
        // Optimized subtraction: x -= qhat * y
        mp_dbl_limb_signed borrow = 0;
        size_t i;

        for (i = 0; i < yd; i++) {
          mp_dbl_limb product = qhat * y.p[i];
          mp_dbl_limb_signed diff = (mp_dbl_limb_signed)x.p[i+j] - (mp_dbl_limb_signed)LOW(product) + borrow;
          x.p[i+j] = LOW(diff);
          borrow = HIGH(diff) - (mp_dbl_limb_signed)HIGH(product);
        }

        // Handle final borrow propagation
        if (i+j < x.sz) {
          borrow += (mp_dbl_limb_signed)x.p[i+j];
          x.p[i+j] = LOW(borrow);
          borrow = HIGH(borrow);
        }

        // Correction: if borrow is negative, qhat was too large, add back
        if (borrow < 0) {
          qhat--;
          mp_dbl_limb carry = 0;
          for (i = 0; i < yd; i++) {
            carry += (mp_dbl_limb)x.p[i+j] + (mp_dbl_limb)y.p[i];
            x.p[i+j] = LOW(carry);
            carry = HIGH(carry);
          }
          if (i+j < x.sz && carry > 0) {
            x.p[i+j] += (mp_limb)carry;
          }
        }
      }
      q.p[j] = (mp_limb)qhat;
      if (j == 0) break;
    }
  }
  x.sz = yy->sz;
  urshift(mrb, rr, &x, ns);
  trim(&q);
  mpz_move(mrb, qq, &q);
  mpz_clear(mrb, &x);
  mpz_clear(mrb, &y);
}

static void
mpz_mdiv(mrb_state *mrb, mpz_t *q, mpz_t *x, mpz_t *y)
{
  mpz_t r;
  short sn1 = x->sn, sn2 = y->sn, qsign;

  if (zero_p(x)) {
    mpz_init_set_int(mrb, q, 0);
    return;
  }
  mpz_init(mrb, &r);
  udiv(mrb, q, &r, x, y);
  qsign = q->sn = sn1*sn2;
  if (uzero_p(q))
    q->sn = 0;
  /* now if r != 0 and q < 0 we need to round q towards -inf */
  if (!uzero_p(&r) && qsign < 0) {
    /* add 1 to magnitude */
    mpz_add_int(mrb, q, 1);
    /* force negative sign in case the value of q was zero before rounding */
    q->sn = -1;
  }
  mpz_clear(mrb, &r);
}

static void
mpz_mmod(mrb_state *mrb, mpz_t *r, mpz_t *x, mpz_t *y)
{
  mpz_t q;
  short sn1 = x->sn, sn2 = y->sn, sn3;

  mpz_init(mrb, &q);
  if (sn1 == 0) {
    zero(r);
    return;
  }
  udiv(mrb, &q, r, x, y);
  mpz_clear(mrb, &q);
  if (uzero_p(r)) {
    r->sn = 0;
    return;
  }
  sn3 = sn1*sn2;
  if (sn3 > 0)
    r->sn = sn1;
  else if (sn1 < 0 && sn2 > 0) {
    r->sn = 1;
    mpz_sub(mrb, r, y, r);
  }
  else {
    r->sn = 1;
    mpz_add(mrb, r, y, r);
  }
}

static void
mpz_mdivmod(mrb_state *mrb, mpz_t *q, mpz_t *r, mpz_t *x, mpz_t *y)
{
  short sn1 = x->sn, sn2 = y->sn, qsign;

  if (sn1 == 0) {
    zero(q);
    zero(r);
    return;
  }
  udiv(mrb, q, r, x, y);
  qsign = q->sn = sn1*sn2;
  if (uzero_p(r)) {
    /* q != 0, since q=r=0 would mean x=0, which was tested above */
    r->sn = 0;
    return;
  }
  if (q->sn > 0)
    r->sn = sn1;
  else if (sn1 < 0 && sn2 > 0) {
    r->sn = 1;
    mpz_sub(mrb, r, y, r);
  }
  else {
    r->sn = 1;
    mpz_add(mrb, r, y, r);
  }
  if (uzero_p(q))
    q->sn = 0;
  /* now if r != 0 and q < 0 we need to round q towards -inf */
  if (!uzero_p(r) && qsign < 0) {
    /* add 1 to magnitude */
    mpz_add_int(mrb, q, 1);
    /* force negative sign in case the value of q was zero before rounding */
    q->sn = -1;
  }
}

/* Fast modular reduction for single-limb modulus */
static void
mpz_mod_limb(mrb_state *mrb, mpz_t *r, mpz_t *x, mp_limb m)
{
  if (zero_p(x)) {
    zero(r);
    return;
  }

  if (x->sz == 1) {
    /* Single limb case - simple modulo */
    mp_limb result = x->p[0] % m;
    mpz_set_int(mrb, r, result);
    r->sn = x->sn;
    return;
  }

  /* Multi-limb case - use repeated division */
  mp_dbl_limb remainder = 0;
  for (size_t i = x->sz; i > 0; i--) {
    remainder = (remainder << DIG_SIZE) | x->p[i-1];
    remainder %= m;
  }

  mpz_set_int(mrb, r, (mp_limb)remainder);
  r->sn = x->sn;
  if (remainder == 0)
    r->sn = 0;
}

/* Forward declarations for Barrett reduction functions */
static void mpz_barrett_mu(mrb_state *mrb, mpz_t *mu, mpz_t *m);
static void mpz_barrett_reduce(mrb_state *mrb, mpz_t *r, mpz_t *x, mpz_t *m, mpz_t *mu);

static void
mpz_mod(mrb_state *mrb, mpz_t *r, mpz_t *x, mpz_t *y)
{
  mpz_t q;
  short sn = x->sn;

  if (zero_p(x)) {
    zero(r);
    return;
  }

  /* Fast path for single-limb modulus */
  if (y->sz == 1) {
    mpz_mod_limb(mrb, r, x, y->p[0]);
    if (y->sn < 0) r->sn = -r->sn;
    return;
  }

  /* Barrett reduction for moderate-sized moduli */
  if (y->sz >= 2 && y->sz <= 8 && x->sz >= y->sz + 2) {
    mpz_t mu;
    mpz_init(mrb, &mu);
    mpz_barrett_mu(mrb, &mu, y);
    mpz_barrett_reduce(mrb, r, x, y, &mu);
    r->sn = sn;
    if (uzero_p(r))
      r->sn = 0;
    mpz_clear(mrb, &mu);
    return;
  }

  /* General division fallback */
  mpz_init(mrb, &q);
  udiv(mrb, &q, r, x, y);
  r->sn = sn;
  if (uzero_p(r))
    r->sn = 0;
  mpz_clear(mrb, &q);
}

static mrb_int
mpz_cmp(mrb_state *mrb, mpz_t *x, mpz_t *y)
{
  if (x->sn < 0 && y->sn > 0)
    return (-1);
  if (x->sn > 0 && y->sn < 0)
    return 1;
  int abscmp=ucmp(x, y);
  if (x->sn >=0 && y->sn >=0)
    return abscmp;
  return (-abscmp);          // if (x->sn <=0 && y->sn <=0)
}

/* 2<=base<=36 - this overestimates the optimal value, which is OK */
static size_t
mpz_sizeinbase(mpz_t *x, mrb_int base)
{
  size_t i, j;

  size_t bits = digits(x) * DIG_SIZE;
  mrb_assert(2 <= base && base <= 36);

  if (zero_p(x) || x->sz == 0) return 0;
  for (j=0,i=1; i<=(size_t)base; i*=2,j++)
    ;
  return bits/(j-1)+1;
}

/* x = y * n (only called from mpz_init_set_str) */
/*   assumes x and n are positive or zero        */
/*   assumes n is small (fits in mp_limb)        */
static void
mpz_mul_int(mrb_state *mrb, mpz_t *x, mrb_int n)
{
  if (n == 0 || zero_p(x)) {
    zero(x);
    return;
  }

  size_t x_sz = x->sz;
  size_t new_sz = x_sz + 1; // Maximum possible size after multiplication

  // Reallocate x if necessary
  mpz_realloc(mrb, x, new_sz);

  mp_dbl_limb cc = 0;
  mp_limb n_limb = (mp_limb)n;

  for (size_t i = 0; i < x_sz; i++) {
    // Multiply each limb and add carry
    cc += (mp_dbl_limb)x->p[i] * n_limb;
    x->p[i] = LOW(cc);
    cc = HIGH(cc);
  }

  if (cc) {
    // If there is a remaining carry, store it
    x->p[x_sz] = (mp_limb)cc;
  }
  else {
    x->sz = x_sz;
  }

  x->sn = 1;
  trim(x);
}

static int
mpz_init_set_str(mrb_state *mrb, mpz_t *x, const char *s, mrb_int len, mrb_int base)
{
  int retval = 0;
  short sn;
  uint8_t k;

  mpz_init(mrb, x);
  if (*s == '-') {
    sn = -1; s++;
  }
  else if (*s == '+') {
    sn = 1; s++;
  }
  else
    sn = 1;
  for (mrb_int i=0; i<len; i++) {
    if (s[i]=='_') continue;
    if (s[i] >= '0' && s[i] <= '9')
      k = (uint8_t)s[i] - (uint8_t)'0';
    else if (s[i] >= 'A' && s[i] <= 'Z')
      k = (uint8_t)s[i] - (uint8_t)'A'+10;
    else if (s[i] >= 'a' && s[i] <= 'z')
      k = (uint8_t)s[i] - (uint8_t)'a'+10;
    else {
      retval = (-1);
      break;
    }
    if (k >= base) {
      retval = (-1);
      break;
    }
    mpz_mul_int(mrb, x, base);
    mpz_add_int(mrb, x, k);
  }
  x->sn = x->sz == 0 ? 0 : sn;
  return retval;
}

/* power of base no bigger than DIG_BASE */
/* power of 2 is handled differently */
static const mp_limb base_limit[34*2] = {
#ifdef MRB_NO_MPZ64BIT
  59049,        // 3^10
  0,            // 4^8 (skip)
  15625,        // 5^6
  46656,        // 6^6
  16807,        // 7^5
  0,            // 8^5 (skip)
  59049,        // 9^5
  10000,        // 10^4
  14641,        // 11^4
  20736,        // 12^4
  28561,        // 13^4
  38416,        // 14^4
  50625,        // 15^4
  0,            // 16^4 (skip)
  4913,         // 17^3
  5832,         // 18^3
  6859,         // 19^3
  8000,         // 20^3
  9261,         // 21^3
  10648,        // 22^3
  12167,        // 23^3
  13824,        // 24^3
  15625,        // 25^3
  17576,        // 26^3
  19683,        // 27^3
  21952,        // 28^3
  24389,        // 29^3
  27000,        // 30^3
  29791,        // 31^3
  0,            // 32^3 (skip)
  35937,        // 33^3
  39304,        // 34^3
  42875,        // 35^3
  46656,        // 36^3
#else
  3486784401UL, // 3^20
  0,            // 4^16 (skip)
  1220703125UL, // 5^13
  2176782336UL, // 6^12
  1977326743UL, // 7^11
  0,            // 8^10 (skip)
  3486784401UL, // 9^10
  1000000000UL, // 10^9
  2357947691UL, // 11^9
  429981696UL,  // 12^8
  815730721UL,  // 13^8
  1475789056UL, // 14^8
  2562890625UL, // 15^8
  0,            // 16^8 (skip)
  410338673UL,  // 17^7
  612220032UL,  // 18^7
  893871739UL,  // 19^7
  1280000000UL, // 20^7
  1801088541UL, // 21^7
  2494357888UL, // 22^7
  3404825447UL, // 23^7
  191102976UL,  // 24^6
  244140625UL,  // 25^6
  308915776UL,  // 26^6
  387420489UL,  // 27^6
  481890304UL,  // 28^6
  594823321UL,  // 29^6
  729000000UL,  // 30^6
  887503681UL,  // 31^6
  0,            // 32^6 (skip)
  1291467969UL, // 33^6
  1544804416UL, // 34^6
  1838265625UL, // 35^6
  2176782336UL, // 36^6
#endif
};

static char*
mpz_get_str(mrb_state *mrb, char *s, mrb_int sz, mrb_int base, mpz_t *x)
{
  mrb_assert(2 <= base && base <= 36);
  if (zero_p(x)) {
    *s='0';
    *(s+1)='\0';
    return s;
  }

  char *ps = s;
  char *se = s+sz;
  int xlen = (int)digits(x);

  if ((base & (base - 1)) == 0) {  // base is a power of 2
    int shift = 0;
    while ((1 << shift) < base) shift++;
    mp_limb mask = (mp_limb)base - 1;
    mp_dbl_limb value = 0;
    int bits = 0;

    /* Process all limbs */
    for (int i = 0; i < xlen; i++) {
      value |= (mp_dbl_limb)x->p[i] << bits;
      bits += DIG_SIZE;
      while (bits >= shift) {
        mp_limb digit = value & mask;
        value >>= shift;
        bits -= shift;

        if (digit < 10) *s++ = '0' + digit;
        else *s++ = 'a' + digit - 10;
      }
    }

    /* Handle any remaining bits */
    while (bits > 0) {
      mp_limb digit = value & mask;
      value >>= shift;
      bits -= shift;

      if (digit < 10) *s++ = '0' + digit;
      else *s++ = 'a' + digit - 10;
    }
  }
  else {
    /* Check for overflow in size calculation */
    if (xlen > SIZE_MAX / sizeof(mp_limb)) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "bigint size too large for string conversion");
    }

    mp_limb *t = (mp_limb*)mrb_malloc(mrb, xlen * sizeof(mp_limb));

    mp_limb *tend = t + xlen;
    memcpy(t, x->p, xlen * sizeof(mp_limb));
    mp_limb b2 = base_limit[base-3];

    for (;;) {
      mp_limb *d = tend;
      mp_dbl_limb a = 0;
      while (--d >= t) {
        mp_limb d0 = *d;
        a = (a<<DIG_SIZE) | d0;
        *d = (mp_limb)(a / b2);
        a %= b2;
      }

      // convert to character
      for (mp_limb b=b2; b>=base; b/=base) {
        char a0 = (char)(a % base);
        if (a0 < 10) a0 += '0';
        else a0 += 'a' - 10;
        if (s == se) break;
        *s++ = a0;
        a /= base;
      }

      // check if number is zero
      for (d = t; d < tend; d++) {
        if (*d != 0) break;
      }
      if (d == tend) break;
    }
    mrb_free(mrb, t);
  }

  while (ps<s && s[-1]=='0') s--;
  if (x->sn < 0) {
    *s++ = '-';
  }

  /* reverse string */
  for (char *u = ps,*v=s-1; u < v; u++,v--) {
    char temp = *u;
    *u = *v;
    *v = temp;
  }
  *s = '\0'; /* null termination */
  return ps;
}

static int
mpz_get_int(mpz_t *y, mrb_int *v)
{
  if (zero_p(y)) {
    *v = 0;
    return TRUE;
  }

  mp_dbl_limb i = 0;
  mp_limb *d = y->p + y->sz;

  while (d-- > y->p) {
    if (HIGH(i) != 0) {
      /* will overflow */
      return FALSE;
    }
    i = (i << DIG_SIZE) | *d;
  }
  if (i > MRB_INT_MAX) {
    /* overflow */
    return FALSE;
  }
  if (y->sn < 0) {
    *v = -(mrb_int)i;
  }
  else {
    *v = (mrb_int)i;
  }
  return TRUE;
}

static void
mpz_mul_2exp(mrb_state *mrb, mpz_t *z, mpz_t *x, mrb_int e)
{
  if (e==0)
    mpz_set(mrb, z, x);
  else {
    short sn = x->sn;
    size_t digs = e / DIG_SIZE;
    size_t bs = e % DIG_SIZE;
    mpz_t y;

    mpz_init(mrb, &y);
    mpz_realloc(mrb, &y, x->sz+digs);
    for (size_t i=0;i<x->sz;i++)
      y.p[i+digs] = x->p[i];
    if (bs) {
      ulshift(mrb, z, &y, bs);
      mpz_clear(mrb, &y);
    }
    else {
      mpz_move(mrb, z, &y);
    }
    z->sn = sn;
  }
}

static void
mpz_div_2exp(mrb_state *mrb, mpz_t *z, mpz_t *x, mrb_int e)
{
  short sn = x->sn;
  if (e==0)
    mpz_set(mrb, z, x);
  else {
    size_t digs = e / DIG_SIZE;
    size_t bs = e % DIG_SIZE;
    mpz_t y;

    mpz_init(mrb, &y);
    mpz_realloc(mrb, &y, x->sz-digs);
    for (size_t i=0; i < x->sz-digs; i++)
      y.p[i] = x->p[i+digs];
    if (bs) {
      urshift(mrb, z, &y, bs);
      mpz_clear(mrb, &y);
    }
    else {
      mpz_move(mrb, z, &y);
    }
    if (uzero_p(z))
      z->sn = 0;
    else {
      z->sn = sn;
    }
  }
}

static void
mpz_neg(mrb_state *mrb, mpz_t *x, mpz_t *y)
{
  mpz_set(mrb, x, y);
  x->sn = -(y->sn);
}

/* Fast modular reduction by power of 2: z = x mod 2^e */
static void
mpz_mod_2exp(mrb_state *mrb, mpz_t *z, mpz_t *x, mrb_int e)
{
  if (e <= 0) {
    zero(z);
    return;
  }

  size_t eint = e / DIG_SIZE;
  size_t bs = e % DIG_SIZE;
  size_t sz = x->sz;

  if (eint >= sz) {
    /* x < 2^e, so x mod 2^e = x */
    mpz_set(mrb, z, x);
    return;
  }

  /* Need to mask off high bits */
  size_t result_sz = eint + (bs > 0 ? 1 : 0);
  mpz_realloc(mrb, z, result_sz);
  z->sn = x->sn;
  z->sz = result_sz;

  /* Copy full limbs */
  for (size_t i = 0; i < eint; i++) {
    z->p[i] = x->p[i];
  }

  /* Mask partial limb if needed */
  if (bs > 0) {
    mp_limb mask = (1UL << bs) - 1;
    z->p[eint] = x->p[eint] & mask;
  }

  trim(z);
}

#define make_2comp(v,c) do { v=~(v)+(c); c=((v)==0 && (c));} while (0)

static void
mpz_and(mrb_state *mrb, mpz_t *z, mpz_t *x, mpz_t *y)
{
  if (zero_p(x) || zero_p(y)) {
    zero(z);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;
  mpz_realloc(mrb, z, max_sz);
  z->sn = (x->sn == y->sn) ? x->sn : 1;

  char c1 = 1, c2 = 1, c3 = 1;
  for (size_t i = 0; i < max_sz; i++) {
    mp_limb xv = (i < x->sz) ? x->p[i] : 0;
    mp_limb yv = (i < y->sz) ? y->p[i] : 0;

    if (x->sn < 0) make_2comp(xv, c1);
    if (y->sn < 0) make_2comp(yv, c2);
    mp_limb zv = xv & yv;
    if (z->sn < 0) make_2comp(zv, c3);
    z->p[i] = zv;
  }
}

static void
mpz_or(mrb_state *mrb, mpz_t *z, mpz_t *x, mpz_t *y)  /* not the most efficient way to do this */
{
  if (zero_p(x)) {
    mpz_set(mrb, z, y);
    return;
  }
  if (zero_p(y)) {
    mpz_set(mrb, z, x);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;
  mpz_realloc(mrb, z, max_sz);
  z->sn = (x->sn == y->sn) ? x->sn : -1;

  char c1 = 1, c2 = 1, c3 = 1;
  for (size_t i = 0; i < max_sz; i++) {
    mp_limb xv = (i < x->sz) ? x->p[i] : 0;
    mp_limb yv = (i < y->sz) ? y->p[i] : 0;

    if (x->sn < 0) make_2comp(xv, c1);
    if (y->sn < 0) make_2comp(yv, c2);
    mp_limb zv = xv | yv;
    if (z->sn < 0) make_2comp(zv, c3);
    z->p[i] = zv;
  }
}

static void
mpz_xor(mrb_state *mrb, mpz_t *z, mpz_t *x, mpz_t *y)  /* not the most efficient way to do this */
{
  if (zero_p(x)) {
    mpz_set(mrb, z, y);
    return;
  }
  if (zero_p(y)) {
    mpz_set(mrb, z, x);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;
  mpz_realloc(mrb, z, max_sz);
  z->sn = (x->sn == y->sn) ? 1 : -1;

  char c1 = 1, c2 = 1, c3 = 1;
  for (size_t i = 0; i < max_sz; i++) {
    mp_limb xv = (i < x->sz) ? x->p[i] : 0;
    mp_limb yv = (i < y->sz) ? y->p[i] : 0;

    if (x->sn < 0) make_2comp(xv, c1);
    if (y->sn < 0) make_2comp(yv, c2);
    mp_limb zv = xv ^ yv;
    if (z->sn < 0) make_2comp(zv, c3);
    z->p[i] = zv;
  }
}

static void
mpz_pow(mrb_state *mrb, mpz_t *zz, mpz_t *x, mrb_int e)
{
  mpz_t t;
  mrb_uint mask = 1ULL<<(sizeof(mrb_int)*8-1);

  if (e==0) {
    mpz_set_int(mrb, zz, 1L);
    return;
  }

  mpz_init_set(mrb, &t, x);
  for (;!(mask &e); mask>>=1)
    ;
  mask>>=1;
  for (;mask!=0; mask>>=1) {
    mpz_mul(mrb, &t, &t, &t);
    if (e & mask)
      mpz_mul(mrb, &t, &t, x);
  }
  mpz_move(mrb, zz, &t);
}

static void
mpz_powm(mrb_state *mrb, mpz_t *zz, mpz_t *x, mpz_t *ex, mpz_t *n)
{
  if (zero_p(ex) || uzero_p(ex)) {
    mpz_set_int(mrb, zz, 1);
    return;
  }

  if (ex->sn < 0) {
    return;
  }

  mpz_t t, b;
  mpz_init_set_int(mrb, &t, 1);
  mpz_init_set(mrb, &b, x);

  /* Optimize with Barrett reduction for moderate-sized moduli */
  mpz_t mu, temp;
  int use_barrett = (n->sz >= 2 && n->sz <= 8);
  mpz_init(mrb, &temp);
  if (use_barrett) {
    mpz_init(mrb, &mu);
    mpz_barrett_mu(mrb, &mu, n);
  }

  size_t len = digits(ex);
  for (size_t i=0; i<len; i++) {
    mp_limb e = ex->p[i];
    for (size_t j=0; j<sizeof(mp_limb)*8; j++) {
      if ((e & 1) == 1) {
        mpz_mul(mrb, &temp, &t, &b);
        if (use_barrett) {
          mpz_barrett_reduce(mrb, &t, &temp, n, &mu);
        }
        else {
          mpz_mod(mrb, &t, &temp, n);
        }
      }
      e >>= 1;
      mpz_mul(mrb, &temp, &b, &b);
      if (use_barrett) {
        mpz_barrett_reduce(mrb, &b, &temp, n, &mu);
      }
      else {
        mpz_mod(mrb, &b, &temp, n);
      }
    }
  }

  mpz_clear(mrb, &temp);
  if (use_barrett) {
    mpz_clear(mrb, &mu);
  }
  mpz_move(mrb, zz, &t);
  mpz_clear(mrb, &b);
}

static void
mpz_powm_i(mrb_state *mrb, mpz_t *zz, mpz_t *x, mrb_int ex, mpz_t *n)
{
  if (ex == 0) {
    mpz_set_int(mrb, zz, 1);
    return;
  }

  if (ex < 0) {
    return;
  }

  mpz_t t, b;
  mpz_init_set_int(mrb, &t, 1);
  mpz_init_set(mrb, &b, x);

  /* Optimize with Barrett reduction for moderate-sized moduli */
  mpz_t mu, temp;
  int use_barrett = (n->sz >= 2 && n->sz <= 8);
  mpz_init(mrb, &temp);
  if (use_barrett) {
    mpz_init(mrb, &mu);
    mpz_barrett_mu(mrb, &mu, n);
  }

  while (ex > 0) {
    if ((ex & 1) == 1) {
      mpz_mul(mrb, &temp, &t, &b);
      if (use_barrett) {
        mpz_barrett_reduce(mrb, &t, &temp, n, &mu);
      }
      else {
        mpz_mod(mrb, &t, &temp, n);
      }
    }
    ex >>= 1;
    if (ex > 0) {  /* Skip final squaring when ex becomes 0 */
      mpz_mul(mrb, &temp, &b, &b);
      if (use_barrett) {
        mpz_barrett_reduce(mrb, &b, &temp, n, &mu);
      }
      else {
        mpz_mod(mrb, &b, &temp, n);
      }
    }
  }

  mpz_clear(mrb, &temp);
  if (use_barrett) {
    mpz_clear(mrb, &mu);
  }
  mpz_move(mrb, zz, &t);
  mpz_clear(mrb, &b);
}

static void
mpz_abs(mrb_state *mrb, mpz_t *x, mpz_t *y)
{
  mpz_init_set(mrb, x, y);
  if (zero_p(y))
    x->sn = 0;
  else
    x->sn = 1;
}

/* Fast GCD for single limbs using binary algorithm */
static mp_limb
limb_gcd(mp_limb a, mp_limb b)
{
  if (a == 0) return b;
  if (b == 0) return a;

  /* Find power of 2 dividing both a and b */
  int shift = 0;
  while (((a | b) & 1) == 0) {
    a >>= 1;
    b >>= 1;
    shift++;
  }

  /* Make a odd */
  while ((a & 1) == 0) {
    a >>= 1;
  }

  /* From here on, a is always odd */
  do {
    /* Make b odd */
    while ((b & 1) == 0) {
      b >>= 1;
    }

    /* Now both a and b are odd. Ensure a >= b */
    if (a < b) {
      mp_limb temp = a;
      a = b;
      b = temp;
    }

    /* Replace b with (b - a) */
    b = b - a;

  } while (b != 0);

  /* Restore common factors of 2 */
  return a << shift;
}

/* Count trailing zero bits in a multi-precision integer */
static size_t
mpz_trailing_zeros(mpz_t *x)
{
  if (zero_p(x) || x->sz == 0) return 0;

  size_t zeros = 0;

  /* Count complete zero limbs */
  size_t i = 0;
  while (i < x->sz && x->p[i] == 0) {
    zeros += DIG_SIZE;
    i++;
  }

  /* Count trailing zeros in first non-zero limb */
  if (i < x->sz) {
    mp_limb limb = x->p[i];
#if (defined(__GNUC__) || __has_builtin(__builtin_ctzll))
    if (sizeof(mp_limb) == sizeof(unsigned long long)) {
      zeros += __builtin_ctzll(limb);
    }
    else if (sizeof(mp_limb) == sizeof(unsigned long)) {
      zeros += __builtin_ctzl(limb);
    }
    else {
      zeros += __builtin_ctz(limb);
    }
#else
    /* Fallback bit counting */
    while ((limb & 1) == 0) {
      limb >>= 1;
      zeros++;
    }
#endif
  }

  return zeros;
}

/* Check if a number is a power of 2 */
static int
mpz_power_of_2_p(mpz_t *x)
{
  if (zero_p(x) || x->sz == 0) return 0;

  /* Count non-zero limbs */
  size_t non_zero_limbs = 0;
  size_t non_zero_index = 0;

  for (size_t i = 0; i < x->sz; i++) {
    if (x->p[i] != 0) {
      non_zero_limbs++;
      non_zero_index = i;
      if (non_zero_limbs > 1) return 0; /* More than one non-zero limb */
    }
  }

  if (non_zero_limbs == 0) return 0; /* All zero */
  if (non_zero_limbs > 1) return 0;  /* Multiple non-zero limbs */

  /* Check if the single non-zero limb is a power of 2 */
  mp_limb limb = x->p[non_zero_index];
  return (limb != 0) && ((limb & (limb - 1)) == 0);
}

/* Binary GCD algorithm (Stein's algorithm) - faster than Euclidean GCD */
static void
mpz_gcd(mrb_state *mrb, mpz_t *gg, mpz_t *aa, mpz_t *bb)
{
  mpz_t a, b;

  /* Handle special cases */
  if (zero_p(aa)) {
    mpz_abs(mrb, gg, bb);
    return;
  }
  if (zero_p(bb)) {
    mpz_abs(mrb, gg, aa);
    return;
  }

  /* Fast path for single-limb numbers */
  if (aa->sz <= 1 && bb->sz <= 1) {
    mp_limb a_limb = (aa->sz == 0) ? 0 : aa->p[0];
    mp_limb b_limb = (bb->sz == 0) ? 0 : bb->p[0];
    mp_limb result = limb_gcd(a_limb, b_limb);

    mpz_init(mrb, gg);
    if (result == 0) {
      gg->sn = 0;
      gg->sz = 0;
    }
    else {
      mpz_realloc(mrb, gg, 1);
      gg->p[0] = result;
      gg->sn = 1;
    }
    return;
  }

  /* Fast path for powers of 2 */
  if (mpz_power_of_2_p(aa)) {
    size_t a_zeros = mpz_trailing_zeros(aa);
    size_t b_zeros = mpz_trailing_zeros(bb);
    size_t min_zeros = (a_zeros < b_zeros) ? a_zeros : b_zeros;

    mpz_init_set_int(mrb, gg, 1);
    mpz_mul_2exp(mrb, gg, gg, min_zeros);
    return;
  }
  if (mpz_power_of_2_p(bb)) {
    size_t a_zeros = mpz_trailing_zeros(aa);
    size_t b_zeros = mpz_trailing_zeros(bb);
    size_t min_zeros = (a_zeros < b_zeros) ? a_zeros : b_zeros;

    mpz_init_set_int(mrb, gg, 1);
    mpz_mul_2exp(mrb, gg, gg, min_zeros);
    return;
  }

  mpz_abs(mrb, &a, aa);
  mpz_abs(mrb, &b, bb);

  /* Find power of 2 that divides both a and b */
  size_t a_zeros = mpz_trailing_zeros(&a);
  size_t b_zeros = mpz_trailing_zeros(&b);
  size_t shift = (a_zeros < b_zeros) ? a_zeros : b_zeros;

  /* Remove common factors of 2 */
  if (shift > 0) {
    mpz_div_2exp(mrb, &a, &a, shift);
    mpz_div_2exp(mrb, &b, &b, shift);
  }

  /* Remove remaining factors of 2 from a */
  if (a_zeros > shift) {
    mpz_div_2exp(mrb, &a, &a, a_zeros - shift);
  }

  /* Remove remaining factors of 2 from b */
  if (b_zeros > shift) {
    mpz_div_2exp(mrb, &b, &b, b_zeros - shift);
  }

  /* Use Lehmer's algorithm for large multi-limb numbers (> 3 limbs) */
  if (a.sz > 3 && b.sz > 3) {
    /* Extract the two most significant limbs for approximation */
    mp_limb a_high = a.p[a.sz - 1];
    mp_limb a_low = a.p[a.sz - 2];
    mp_limb b_high = b.p[b.sz - 1];
    mp_limb b_low = b.p[b.sz - 2];

    /* Perform Lehmer reduction on double-precision approximations */
    mp_limb u0 = 1, u1 = 0, v0 = 0, v1 = 1;

    while (b_high > 0) {
      /* Calculate quotient using double-precision approximation */
      mp_limb q;
      if (a_high == b_high) {
        q = (a_low >= b_low) ? 1 : 0;
      }
      else {
        /* Approximate quotient from most significant limbs */
        q = a_high / (b_high + 1);
      }

      if (q == 0) break;

      /* Check if applying this quotient would cause overflow */
      mp_limb max_limb = (mp_limb)(-1);
      if (u1 > 0 && q > max_limb / u1) break;
      if (v1 > 0 && q > max_limb / v1) break;

      /* Update transformation matrix */
      mp_limb t;
      t = u0 - q * u1; u0 = u1; u1 = t;
      t = v0 - q * v1; v0 = v1; v1 = t;
      t = a_high - q * b_high; a_high = b_high; b_high = t;

      /* Stop if coefficients get too large */
      if (u1 == 0 && v1 == 0) break;
    }

    /* Apply the transformation if it's non-trivial */
    if (u1 != 0 || v1 != 0) {
      mpz_t temp_a, temp_b, u0_a, v0_b, u1_a, v1_b;
      mpz_init(mrb, &temp_a);
      mpz_init(mrb, &temp_b);
      mpz_init_set(mrb, &u0_a, &a);
      mpz_init_set(mrb, &v0_b, &b);
      mpz_init_set(mrb, &u1_a, &a);
      mpz_init_set(mrb, &v1_b, &b);

      /* Compute u0*a, v0*b, u1*a, v1*b */
      mpz_mul_int(mrb, &u0_a, u0);
      mpz_mul_int(mrb, &v0_b, v0);
      mpz_mul_int(mrb, &u1_a, u1);
      mpz_mul_int(mrb, &v1_b, v1);

      /* temp_a = u0*a + v0*b */
      mpz_add(mrb, &temp_a, &u0_a, &v0_b);

      /* temp_b = u1*a + v1*b */
      mpz_add(mrb, &temp_b, &u1_a, &v1_b);

      /* Update a and b */
      mpz_set(mrb, &a, &temp_a);
      mpz_set(mrb, &b, &temp_b);

      mpz_clear(mrb, &temp_a);
      mpz_clear(mrb, &temp_b);
      mpz_clear(mrb, &u0_a);
      mpz_clear(mrb, &v0_b);
      mpz_clear(mrb, &u1_a);
      mpz_clear(mrb, &v1_b);

      /* Ensure a >= b after transformation */
      if (mpz_cmp(mrb, &a, &b) < 0) {
        mpz_t temp_holder = a;
        a = b;
        b = temp_holder;
      }
    }
  }

  /* From here on, a is always odd */
  do {
    /* Make b odd efficiently */
    if ((b.p[0] & 1) == 0) {
      size_t b_trailing = mpz_trailing_zeros(&b);
      if (b_trailing > 0) {
        mpz_div_2exp(mrb, &b, &b, b_trailing);
      }
    }

    /* Now both a and b are odd. Ensure a >= b */
    if (mpz_cmp(mrb, &a, &b) < 0) {
      /* In-place swap without temporary variable */
      mpz_t temp_holder = a;
      a = b;
      b = temp_holder;
    }

    /* Replace a with (a - b) */
    mpz_sub(mrb, &a, &a, &b);

    /* Remove factors of 2 from the result if it's even */
    if (a.sz > 0 && (a.p[0] & 1) == 0) {
      size_t a_trailing = mpz_trailing_zeros(&a);
      if (a_trailing > 0) {
        mpz_div_2exp(mrb, &a, &a, a_trailing);
      }
    }

  } while (!zero_p(&a));

  /* Restore common factors of 2 */
  mpz_mul_2exp(mrb, &b, &b, shift);

  trim(&b);
  mpz_move(mrb, gg, &b);
  mpz_clear(mrb, &a);
}

static size_t
mpz_bits(const mpz_t *x)
{
  if (x->sz == 0 || x->sn == 0) return 0;

  size_t limb_bits = sizeof(mp_limb) * 8;

  // Get the most significant limb
  size_t i = x->sz - 1;
  mp_limb high = x->p[i];

  // Number of bits = total full limbs + significant bits in top limb
  return i * limb_bits + (limb_bits - lzb(high));
}

/* Compute Barrett parameter μ = floor(2^(2k) / m) where k ≈ log₂(m) */
static void
mpz_barrett_mu(mrb_state *mrb, mpz_t *mu, mpz_t *m)
{
  size_t k = mpz_bits(m);
  mpz_t temp;

  mpz_init_set_int(mrb, &temp, 1);
  mpz_mul_2exp(mrb, &temp, &temp, 2 * k);  /* temp = 2^(2k) */
  mpz_mdiv(mrb, mu, &temp, m);             /* mu = floor(2^(2k) / m) */
  mpz_clear(mrb, &temp);
}

/* Barrett reduction: r = x mod m using precomputed μ */
static void
mpz_barrett_reduce(mrb_state *mrb, mpz_t *r, mpz_t *x, mpz_t *m, mpz_t *mu)
{
  size_t k = mpz_bits(m);

  /* If x < m, then x mod m = x */
  if (mpz_cmp(mrb, x, m) < 0) {
    mpz_set(mrb, r, x);
    return;
  }

  mpz_t q1, q2, q3, r1, r2;
  mpz_init(mrb, &q1);
  mpz_init(mrb, &q2);
  mpz_init(mrb, &q3);
  mpz_init(mrb, &r1);
  mpz_init(mrb, &r2);

  /* Step 1: q1 = floor(x / 2^(k-1)) */
  if (k > 1) {
    mpz_div_2exp(mrb, &q1, x, k - 1);
  }
  else {
    mpz_set(mrb, &q1, x);
  }

  /* Step 2: q2 = q1 * μ */
  mpz_mul(mrb, &q2, &q1, mu);

  /* Step 3: q3 = floor(q2 / 2^(k+1)) */
  mpz_div_2exp(mrb, &q3, &q2, k + 1);

  /* Step 4: r1 = x mod 2^(k+1) */
  mpz_mod_2exp(mrb, &r1, x, k + 1);

  /* Step 5: r2 = (q3 * m) mod 2^(k+1) */
  mpz_mul(mrb, &r2, &q3, m);
  mpz_mod_2exp(mrb, &r2, &r2, k + 1);

  /* Step 6: r = r1 - r2 */
  if (mpz_cmp(mrb, &r1, &r2) >= 0) {
    mpz_sub(mrb, r, &r1, &r2);
  }
  else {
    /* r1 < r2, so add 2^(k+1) to r1 */
    mpz_t power;
    mpz_init_set_int(mrb, &power, 1);
    mpz_mul_2exp(mrb, &power, &power, k + 1);
    mpz_add(mrb, &r1, &r1, &power);
    mpz_sub(mrb, r, &r1, &r2);
    mpz_clear(mrb, &power);
  }

  /* Step 7: Final correction - ensure 0 ≤ r < m */
  while (mpz_cmp(mrb, r, m) >= 0) {
    mpz_sub(mrb, r, r, m);
  }

  mpz_clear(mrb, &q1);
  mpz_clear(mrb, &q2);
  mpz_clear(mrb, &q3);
  mpz_clear(mrb, &r1);
  mpz_clear(mrb, &r2);
}

static void
mpz_sqrt(mrb_state *mrb, mpz_t *z, mpz_t *x)
{
  mrb_assert(x->sn >= 0);

  if (x->sz == 0) {
    // sqrt(0) = 0
    z->sn = 0;
    z->sz = 0;
    return;
  }

  // Estimate initial value: 1 << (bit_length(x) / 2)
  size_t xbits = mpz_bits(x);
  size_t sbit = (xbits + 1) / 2;
  mpz_t s, t;
  mpz_init_set_int(mrb, &s, 1);
  mpz_mul_2exp(mrb, &s, &s, sbit);

  mpz_init(mrb, &t);

  // Iteratively refine s using Newton-Raphson method:
  // s = (s + x / s) / 2
  for (;;) {
    mpz_mdiv(mrb, &t, x, &s);     // t = x / s
    mpz_add(mrb, &t, &t, &s);     // t = s + x/s
    mpz_div_2exp(mrb, &t, &t, 1); // t = (s + x/s) / 2

    if (mpz_cmp(mrb, &t, &s) >= 0) {
      // Converged: t >= s
      break;
    }

    mpz_set(mrb, &s, &t);
  }

  mpz_move(mrb, z, &s);
  mpz_clear(mrb, &t);
}

/* Barrett reduction for efficient modular arithmetic with repeated operations */

/* --- mruby functions --- */
/* initialize mpz_t from RBigint (not need to clear) */
static void
bint_as_mpz(struct RBigint *b, mpz_t *x)
{
  x->p = RBIGINT_ARY(b);
  x->sz = RBIGINT_SIZE(b);
  x->sn = RBIGINT_SIGN(b);
}

static struct RBigint*
bint_new(mrb_state *mrb, mpz_t *x)
{
  struct RBigint *b = MRB_OBJ_ALLOC(mrb, MRB_TT_BIGINT, mrb->integer_class);
  if (x->sz <= RBIGINT_EMBED_SIZE_MAX) {
    RBIGINT_SET_EMBED_SIZE(b, x->sz);
    RBIGINT_SET_EMBED_SIGN(b, x->sn);
    if (x->p) memcpy(RBIGINT_EMBED_ARY(b), x->p, x->sz*sizeof(mp_limb));
    mpz_clear(mrb, x);
  }
  else {
    RBIGINT_SET_HEAP(b);
    b->as.heap = *x;
  }
  return b;
}

static struct RBigint*
bint_new_int(mrb_state *mrb, mrb_int n)
{
  mpz_t x;

  mpz_init_set_int(mrb, &x, n);
  return bint_new(mrb, &x);
}

mrb_value
mrb_bint_new_int(mrb_state *mrb, mrb_int x)
{
  struct RBigint *b = bint_new_int(mrb, x);
  return mrb_obj_value(b);
}

#ifdef MRB_INT32
mrb_value
mrb_bint_new_int64(mrb_state *mrb, int64_t n)
{
  mpz_t x;
  mpz_set_int64(mrb, &x, n);
  struct RBigint *b = bint_new(mrb, &x);
  return mrb_obj_value(b);
}
#endif

mrb_value
mrb_bint_new_uint64(mrb_state *mrb, uint64_t x)
{
  mpz_t z;
  mpz_init(mrb, &z);
  mpz_set_uint64(mrb, &z, x);
  struct RBigint *b = bint_new(mrb ,&z);
  return mrb_obj_value(b);
}

mrb_value
mrb_bint_new_str(mrb_state *mrb, const char *x, mrb_int len, mrb_int base)
{
  mpz_t z;

  int sn = 1;
  if (base < 0) {
    base = -base;
    sn = -1;
  }
  mrb_assert(2 <= base && base <= 36);
  mpz_init_set_str(mrb, &z, x, len, base);
  if (sn < 0) {
    z.sn = sn;
  }
  struct RBigint *b = bint_new(mrb, &z);
  return mrb_obj_value(b);
}

static mrb_value
bint_norm(mrb_state *mrb, struct RBigint *b)
{
  mrb_int i;
  mpz_t a;

  bint_as_mpz(b, &a);
  if (mpz_get_int(&a, &i)) {
    return mrb_int_value(mrb, i);
  }
  return mrb_obj_value(b);
}

void
mrb_gc_free_bint(mrb_state *mrb, struct RBasic *x)
{
  struct RBigint *b = (struct RBigint*)x;
  if (!RBIGINT_EMBED_P(b)) {
    mpz_clear(mrb, &b->as.heap);
  }
}

#ifndef MRB_NO_FLOAT
mrb_value
mrb_bint_new_float(mrb_state *mrb, mrb_float x)
{
  /* x should not be NaN nor Infinity */
  mrb_assert(x == x && x != x * 0.5);

  if (FIXABLE_FLOAT(x)) {
    return mrb_int_value(mrb, (mrb_int)x);
  }

  int sn;
  if (x < 0.0) {
    x = -x;
    sn = -1;
  }
  else {
    sn = 1;
  }
  if (x < 1.0) {
    return mrb_fixnum_value(0);
  }

  mpz_t r;
  mpz_init(mrb, &r);
  r.sn = sn;

  mrb_float b = (double)DIG_BASE;
  mrb_float bi = 1.0 / b;
  size_t rn;

  for (rn = 1; x >= b; rn++)
    x *= bi;

  mpz_realloc(mrb, &r, rn);
  mp_limb *rp = r.p;
  for (size_t i=rn-1;;i--) {
    mp_limb f = LOW((mp_limb)x);
    x -= f;
    mrb_assert(x < 1.0);
    rp[i] = f;
    if (i == 0) break;
  }
  return bint_norm(mrb, bint_new(mrb, &r));
}

mrb_float
mrb_bint_as_float(mrb_state *mrb, mrb_value self)
{
  mpz_t m;
  bint_as_mpz(RBIGINT(self), &m);

  mp_limb *d = m.p + m.sz;
  mrb_float val = 0;

  while (d-- > m.p) {
    val = val * DIG_BASE + *d;
  }

  if (m.sn < 0) {
    val = -val;
  }
  return val;
}
#endif

mrb_value
mrb_as_bint(mrb_state *mrb, mrb_value x)
{
  if (mrb_bigint_p(x)) return x;
  return mrb_bint_new_int(mrb, mrb_as_int(mrb, x));
}

mrb_int
mrb_bint_as_int(mrb_state *mrb, mrb_value x)
{
  mpz_t m;
  mrb_int i;

  bint_as_mpz(RBIGINT(x), &m);
  if (!mpz_get_int(&m, &i)) {
    mrb_raise(mrb, E_RANGE_ERROR, "integer out of range");
  }
  return i;
}

#ifdef MRB_INT32
int64_t
mrb_bint_as_int64(mrb_state *mrb, mrb_value x)
{
  mpz_t m;
  bint_as_mpz(RBIGINT(x), &m);

  uint64_t u = 0;
  size_t len = digits(&m);

  if (len*sizeof(mp_limb) > sizeof(uint64_t)) {
  out_of_range:
    mrb_raise(mrb, E_RANGE_ERROR, "integer out of range");
  }
  for (size_t i=len-1; ; i--) {
    u <<= DIG_SIZE;
    u |= m.p[i];
    if (i==0) break;
  }
  if (u > INT64_MAX) goto out_of_range;
  if (m.sn < 0) return -(int64_t)u;
  return (int64_t)u;
}
#endif

uint64_t
mrb_bint_as_uint64(mrb_state *mrb, mrb_value x)
{
  mpz_t m;
  bint_as_mpz(RBIGINT(x), &m);

  uint64_t u = 0;
  size_t len = digits(&m);

  if (m.sn < 0 || len*sizeof(mp_limb) > sizeof(uint64_t)) {
    mrb_raise(mrb, E_RANGE_ERROR, "integer out of range");
  }
  for (size_t i=len-1; ; i--) {
    u <<= DIG_SIZE;
    u |= m.p[i];
    if (i==0) break;
  }
  return u;
}

static mrb_bool
int_fit_limb_p(mrb_int i)
{
#if DIG_SIZE == 32
# ifdef MRB_INT64
  // if mp_limb is int32_t
  return (i > INT32_MIN && i <= INT32_MAX);
# else
  // if mp_limb is also int32_t, it always fits
  return TRUE;
# endif
#else /* if DIG_SIZE == 16 */
  // if mp_limb is int16_t
  return (i > INT16_MIN && i <= INT16_MAX);
#endif
}

/* unnormalize version of mrb_bint_add */
mrb_value
mrb_bint_add_n(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b, z;

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y)) {
    mrb_int n = mrb_integer(y);
    if (int_fit_limb_p(n)) {
      mpz_init_set(mrb, &z, &a);
      if ((n > 0) ^ (z.sn > 0)) {
        mpz_sub_int(mrb, &z, n<0 ? -n : n);
      }
      else {
        mpz_add_int(mrb, &z, n<0 ? -n : n);
      }
      struct RBigint *v = bint_new(mrb, &z);
      return mrb_obj_value(v);
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init(mrb, &z);
  mpz_add(mrb, &z, &a, &b);
  struct RBigint *v = bint_new(mrb, &z);
  return mrb_obj_value(v);
}

mrb_value
mrb_bint_add(mrb_state *mrb, mrb_value x, mrb_value y)
{
#ifndef MRB_NO_FLOAT
  if (mrb_float_p(y)) {
    mrb_float v1 = mrb_bint_as_float(mrb, x);
    mrb_float v2 = mrb_float(y);
    return mrb_float_value(mrb,v1+v2);
  }
#endif
  x = mrb_bint_add_n(mrb, x, y);
  return bint_norm(mrb, RBIGINT(x));
}

/* unnormalize version of mrb_bint_sub */
mrb_value
mrb_bint_sub_n(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b, z;

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y)) {
    mrb_int n = mrb_integer(y);
    if (int_fit_limb_p(n)) {
      mpz_init_set(mrb, &z, &a);
      if ((n > 0) ^ (z.sn > 0)) {
        mpz_add_int(mrb, &z, n<0 ? -n : n);
      }
      else {
        mpz_sub_int(mrb, &z, n<0 ? -n : n);
      }
      struct RBigint *v = bint_new(mrb, &z);
      return mrb_obj_value(v);
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init(mrb, &z);
  mpz_sub(mrb, &z, &a, &b);
  struct RBigint *v = bint_new(mrb, &z);
  return mrb_obj_value(v);
}

mrb_value
mrb_bint_sub(mrb_state *mrb, mrb_value x, mrb_value y)
{
#ifndef MRB_NO_FLOAT
  if (mrb_float_p(y)) {
    mrb_float v1 = mrb_bint_as_float(mrb, x);
    mrb_float v2 = mrb_float(y);
    return mrb_float_value(mrb,v1-v2);
  }
#endif
  x = mrb_bint_sub_n(mrb, x, y);
  return bint_norm(mrb, RBIGINT(x));
}

static struct RBigint*
bint_mul(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b, z;

  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(x), &a);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init(mrb, &z);
  mpz_mul(mrb, &z, &a, &b);
  return bint_new(mrb, &z);
}

mrb_value
mrb_bint_mul(mrb_state *mrb, mrb_value x, mrb_value y)
{
  if (mrb_integer_p(y)) {
    if (mrb_integer(y) == 0) return mrb_fixnum_value(0);
    if (mrb_integer(y) == 1) return bint_norm(mrb, RBIGINT(x));
  }
#ifndef MRB_NO_FLOAT
  if (mrb_float_p(y)) {
    mrb_float v1 = mrb_bint_as_float(mrb, x);
    mrb_float v2 = mrb_float(y);
    return mrb_float_value(mrb,v1*v2);
  }
#endif
  return bint_norm(mrb, bint_mul(mrb, x, y));
}

mrb_value
mrb_bint_mul_n(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct RBigint *b = bint_mul(mrb, x, y);
  return mrb_obj_value(b);
}

mrb_value
mrb_bint_div(mrb_state *mrb, mrb_value x, mrb_value y)
{
  if (mrb_integer_p(y)) {
    if (mrb_integer(y) == 0) mrb_int_zerodiv(mrb);
    if (mrb_integer(y) == 1) return bint_norm(mrb, RBIGINT(x));
  }
#ifndef MRB_NO_FLOAT
  if (mrb_float_p(y)) {
    mrb_float v1 = mrb_bint_as_float(mrb, x);
    mrb_float v2 = mrb_float(y);
    return mrb_float_value(mrb,v1*v2);
  }
#endif
  mpz_t a, b, z;

  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&b) || uzero_p(&b)) {
    mrb_int_zerodiv(mrb);
  }
  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &z);
  mpz_mdiv(mrb, &z, &a, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_add_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;

  mpz_init_set_int(mrb, &a, x);
  mpz_init_set_int(mrb, &b, y);
  mpz_init(mrb, &z);
  mpz_add(mrb, &z, &a, &b);
  mpz_clear(mrb, &a);
  mpz_clear(mrb, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_sub_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;

  mpz_init_set_int(mrb, &a, x);
  mpz_init_set_int(mrb, &b, y);
  mpz_init(mrb, &z);
  mpz_sub(mrb, &z, &a, &b);
  mpz_clear(mrb, &a);
  mpz_clear(mrb, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_mul_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;

  mpz_init_set_int(mrb, &a, x);
  mpz_init_set_int(mrb, &b, y);
  mpz_init(mrb, &z);
  mpz_mul(mrb, &z, &a, &b);
  mpz_clear(mrb, &a);
  mpz_clear(mrb, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_mod(mrb_state *mrb, mrb_value x, mrb_value y)
{
#ifndef MRB_NO_FLOAT
  if (mrb_float_p(y)) {
    mrb_float v1 = mrb_bint_as_float(mrb, x);
    mrb_float v2 = mrb_float(y);
    return mrb_float_value(mrb, fmod(v1, v2));
  }
#endif
  if (mrb_integer_p(y) && mrb_integer(y) == 0) {
    mrb_int_zerodiv(mrb);
  }
  mpz_t a, b, z;
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&b) || uzero_p(&b)) {
    mrb_int_zerodiv(mrb);
  }
  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &z);
  mpz_mmod(mrb, &z, &a, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_rem(mrb_state *mrb, mrb_value x, mrb_value y)
{
  /* called from mrbgems/mruby-numeric-ext/src/numeric_ext.c */
  /* y should not be float */
  if (mrb_integer_p(y) && mrb_integer(y) == 0) {
    mrb_int_zerodiv(mrb);
  }
  mpz_t a, b, z;
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&b) || uzero_p(&b)) {
    mrb_int_zerodiv(mrb);
  }
  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &z);
  mpz_mod(mrb, &z, &a, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_divmod(mrb_state *mrb, mrb_value x, mrb_value y)
{
  /* called from src/numeric.c */
  /* y should not be float */
  if (mrb_integer_p(y) && mrb_integer(y) == 0) {
    mrb_int_zerodiv(mrb);
  }
  y = mrb_as_bint(mrb, y);
  mpz_t a, b, c, d;
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&b) || uzero_p(&b)) {
    mrb_int_zerodiv(mrb);
  }
  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &c);
  mpz_init(mrb, &d);
  mpz_mdivmod(mrb, &c, &d, &a, &b);
  return mrb_assoc_new(mrb, bint_norm(mrb, bint_new(mrb, &c)), bint_norm(mrb, bint_new(mrb, &d)));
}

mrb_int
mrb_bint_cmp(mrb_state *mrb, mrb_value x, mrb_value y)
{
#ifndef MRB_NO_FLOAT
  if (mrb_float_p(y)) {
    mrb_float v1 = mrb_bint_as_float(mrb, x);
    mrb_float v2 = mrb_float(y);
    if (v1 == v2) return 0;
    if (v1 > v2)  return 1;
    return -1;
  }
#endif
  mpz_t a;

  bint_as_mpz(RBIGINT(x), &a);
  if (!mrb_bigint_p(y)) {
    if (!mrb_integer_p(y)) return -2; /* type mismatch */

    mrb_int i1, i2 = mrb_integer(y);
    if (mpz_get_int(&a, &i1)) {
      if (i1 == i2) return 0;
      if (i1 > i2) return 1;
      return -1;
    }
    if (a.sn > 0) return 1;
    return -1;
  }
  mpz_t b;
  bint_as_mpz(RBIGINT(y), &b);
  return mpz_cmp(mrb, &a, &b);
}

mrb_value
mrb_bint_pow(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a;

  bint_as_mpz(RBIGINT(x), &a);
  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
    break;
  case MRB_TT_BIGINT:
    mrb_raise(mrb, E_TYPE_ERROR, "too big power");
  default:
    mrb_raisef(mrb, E_TYPE_ERROR, "%Y cannot be convert to integer", y);
  }

  mpz_t z;
  mpz_init(mrb, &z);
  mpz_pow(mrb, &z, &a, mrb_integer(y));

  struct RBigint *b = bint_new(mrb, &z);
  return mrb_obj_value(b);
}

mrb_value
mrb_bint_powm(mrb_state *mrb, mrb_value x, mrb_value exp, mrb_value mod)
{
  mpz_t a, b, c, z;

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(mod)) {
    mrb_int m = mrb_integer(mod);
    if (m == 0) mrb_int_zerodiv(mrb);
    mpz_init_set_int(mrb, &c, m);
  }
  else {
    mod = mrb_as_bint(mrb, mod);
    bint_as_mpz(RBIGINT(mod), &c);
    if (zero_p(&c) || uzero_p(&c)) {
      mrb_int_zerodiv(mrb);
    }
  }
  mpz_init(mrb, &z);
  if (mrb_bigint_p(exp)) {
    bint_as_mpz(RBIGINT(exp), &b);
    if (b.sn < 0) goto raise;
    mpz_powm(mrb, &z, &a, &b, &c);
  }
  else {
    mrb_int e = mrb_integer(exp);
    if (e < 0) goto raise;
    mpz_powm_i(mrb, &z, &a, e, &c);
  }
  if (mrb_integer_p(mod)) mpz_clear(mrb, &c);
  return bint_norm(mrb, bint_new(mrb, &z));

 raise:
  if (mrb_integer_p(mod)) mpz_clear(mrb, &c);
  mrb_raise(mrb, E_ARGUMENT_ERROR, "int.pow(n,m): n must be positive");
  /* not reached */
  return mrb_nil_value();
}

mrb_value
mrb_bint_to_s(mrb_state *mrb, mrb_value x, mrb_int base)
{
  mpz_t a;

  bint_as_mpz(RBIGINT(x), &a);
  if (zero_p(&a) || uzero_p(&a)) {
    return mrb_str_new_lit(mrb, "0");
  }
  size_t len = mpz_sizeinbase(&a, (int)base);
  if (sizeof(size_t) >= sizeof(mrb_int) && MRB_INT_MAX-2 < len) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "too long string from Integer");
  }
  mrb_value str = mrb_str_new(mrb, NULL, len+2);
  mpz_get_str(mrb, RSTRING_PTR(str), len, base, &a);
  RSTR_SET_LEN(RSTRING(str), strlen(RSTRING_PTR(str)));
  return str;
}

mrb_value
mrb_bint_and(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b, c;

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y)) {
    mrb_int z = mrb_integer(y);
    if (z == 0) return mrb_fixnum_value(0);
    if (z > 0 && (mp_dbl_limb)z < DIG_BASE) {
      z &= a.p[0];
      return mrb_int_value(mrb, z);
    }
    if (z == -1) return x;
  }

  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&a) || zero_p(&b)) return mrb_fixnum_value(0);
  mpz_init(mrb, &c);
  mpz_and(mrb, &c, &a, &b);
  return bint_norm(mrb, bint_new(mrb, &c));
}

mrb_value
mrb_bint_or(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b, c;

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y)) {
    mrb_int z = mrb_integer(y);
    if (z == 0) return x;
    if (z == -1) return y;
  }

  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&a)) return y;
  if (zero_p(&b)) return x;
  mpz_init(mrb, &c);
  mpz_or(mrb, &c, &b, &a);
  return bint_norm(mrb, bint_new(mrb, &c));
}

mrb_value
mrb_bint_xor(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b, c;

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y) && a.sn > 0) {
    mrb_int z = mrb_integer(y);
    if (z == 0) return x;
    if (0 < z && (mp_dbl_limb)z < DIG_BASE) {
      mpz_init_set(mrb, &c, &a);
      c.p[0] ^= z;
      return bint_norm(mrb, bint_new(mrb, &c));
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&a)) return y;
  if (zero_p(&b)) return x;
  mpz_init(mrb, &c);
  mpz_xor(mrb, &c, &a, &b);
  return bint_norm(mrb, bint_new(mrb, &c));
}

mrb_value
mrb_bint_neg(mrb_state *mrb, mrb_value x)
{
  mpz_t a, b;

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &b);
  mpz_neg(mrb, &b, &a);
  struct RBigint *b2 = bint_new(mrb, &b);
  /* no normalization */
  return mrb_obj_value(b2);
}

mrb_value
mrb_bint_rev(mrb_state *mrb, mrb_value x)
{
  mpz_t a, b;
  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &b);
  mpz_neg(mrb, &b, &a);
  mpz_sub_int(mrb, &b, 1);
  return bint_norm(mrb, bint_new(mrb, &b));
}

mrb_value
mrb_bint_lshift(mrb_state *mrb, mrb_value x, mrb_int width)
{
  mpz_t a, z;

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &z);
  if (width < 0) {
    mpz_div_2exp(mrb, &z, &a, -width);
  }
  else {
    mpz_mul_2exp(mrb, &z, &a, width);
  }
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_rshift(mrb_state *mrb, mrb_value x, mrb_int width)
{
  mpz_t a, z;

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &z);
  if (width < 0) {
    mpz_mul_2exp(mrb, &z, &a, -width);
  }
  else {
    mpz_div_2exp(mrb, &z, &a, width);
  }
  return bint_norm(mrb, bint_new(mrb, &z));
}

void
mrb_bint_copy(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b;

  bint_as_mpz(RBIGINT(x), &a);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init_set(mrb, &a, &b);
}

size_t
mrb_bint_memsize(mrb_value x)
{
  mpz_t z;

  bint_as_mpz(RBIGINT(x), &z);
  return z.sz * sizeof(mp_limb);
}

mrb_value
mrb_bint_sqrt(mrb_state *mrb, mrb_value x)
{
  mpz_t a;

  bint_as_mpz(RBIGINT(x), &a);
  if (a.sn < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "square root of negative number");
  }

  mpz_t z;
  mpz_init(mrb, &z);
  mpz_sqrt(mrb, &z, &a);

  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_int
mrb_bint_sign(mrb_state *mrb, mrb_value bint)
{
  return RBIGINT_SIGN(RBIGINT(bint));
}

mrb_int
mrb_bint_size(mrb_state *mrb, mrb_value bint)
{
  mpz_t z;
  bint_as_mpz(RBIGINT(bint), &z);
  return z.sz * sizeof(mp_limb);
}

mrb_value
mrb_bint_from_bytes(mrb_state *mrb, const uint8_t *bytes, mrb_int len)
{
  mpz_t z;
  mpz_init(mrb, &z);
  size_t limb_len = (len + sizeof(mp_limb) - 1) / sizeof(mp_limb);
  mpz_realloc(mrb, &z, limb_len);
  memcpy(z.p, bytes, len);
  z.sn = (len > 0) ? 1 : 0;
  z.sz = limb_len;
  trim(&z);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_hash(mrb_state *mrb, mrb_value x)
{
  mpz_t z;

  bint_as_mpz(RBIGINT(x), &z);
  uint32_t hash = mrb_byte_hash((uint8_t*)z.p, z.sz*sizeof(mp_limb));
  hash = mrb_byte_hash_step((uint8_t*)&z.sn, sizeof(z.sn), hash);
  return mrb_int_value(mrb, hash);
}

/* to be used only from mruby-sprintf */
mrb_value
mrb_bint_2comp(mrb_state *mrb, mrb_value x)
{
  mpz_t a, z;

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(mrb, &z);
  mrb_assert(a.sn < 0);
  size_t size = a.sz;
  mpz_realloc(mrb, &z, size);
  mp_limb *ds = a.p;
  mp_limb *dd = z.p;
  char carry = 1;
  for (size_t i=0; i<size; i++) {
    mp_limb xv = ds[i];
    make_2comp(xv, carry);
    dd[i] = xv;
  }
  z.sn = 1;

  struct RBigint *b2 = bint_new(mrb, &z);
  return mrb_obj_value(b2);
}

#ifdef MRB_USE_RATIONAL
void
mrb_bint_reduce(mrb_state *mrb, mrb_value *xp, mrb_value *yp)
{
  mpz_t r, x, y, a, b;
  mpz_init(mrb, &r);
  mpz_init(mrb, &a); mpz_init(mrb, &b);

  bint_as_mpz(RBIGINT(*xp), &x);
  bint_as_mpz(RBIGINT(*yp), &y);

  mpz_gcd(mrb, &r, &x, &y);

  mpz_mdiv(mrb, &a, &x, &r);
  mpz_mdiv(mrb, &b, &y, &r);

  mpz_clear(mrb, &r);

  struct RBigint *b1 = bint_new(mrb, &a);
  struct RBigint *b2 = bint_new(mrb, &b);
  *xp = mrb_obj_value(b1);
  *yp = mrb_obj_value(b2);
}
#endif

mrb_value
mrb_bint_gcd(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t r, a, b;

  mpz_init(mrb, &r);
  bint_as_mpz(RBIGINT(x), &a);
  bint_as_mpz(RBIGINT(y), &b);

  mpz_gcd(mrb, &r, &a, &b);

  struct RBigint *result = bint_new(mrb, &r);
  mpz_clear(mrb, &r);
  return bint_norm(mrb, result);
}

mrb_value
mrb_bint_lcm(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t gcd_val, x_mpz, y_mpz, abs_x, abs_y, product, result_mpz;
  mrb_value zero = mrb_bint_new_int(mrb, 0);

  if (mrb_bint_cmp(mrb, x, zero) == 0 || mrb_bint_cmp(mrb, y, zero) == 0) {
    return zero;
  }

  mpz_init(mrb, &gcd_val);
  mpz_init(mrb, &abs_x);
  mpz_init(mrb, &abs_y);
  mpz_init(mrb, &product);
  mpz_init(mrb, &result_mpz);

  bint_as_mpz(RBIGINT(x), &x_mpz);
  bint_as_mpz(RBIGINT(y), &y_mpz);

  mpz_abs(mrb, &abs_x, &x_mpz);
  mpz_abs(mrb, &abs_y, &y_mpz);

  mpz_gcd(mrb, &gcd_val, &abs_x, &abs_y);
  mpz_mul(mrb, &product, &abs_x, &abs_y);
  mpz_mdiv(mrb, &result_mpz, &product, &gcd_val);

  mpz_clear(mrb, &gcd_val);
  mpz_clear(mrb, &abs_x);
  mpz_clear(mrb, &abs_y);
  mpz_clear(mrb, &product);

  struct RBigint *result = bint_new(mrb, &result_mpz);
  mpz_clear(mrb, &result_mpz);
  return mrb_obj_value(result);
}

mrb_value
mrb_bint_abs(mrb_state *mrb, mrb_value x)
{
  mpz_t a, result_mpz;

  mpz_init(mrb, &result_mpz);
  bint_as_mpz(RBIGINT(x), &a);
  mpz_abs(mrb, &result_mpz, &a);

  struct RBigint *result = bint_new(mrb, &result_mpz);
  mpz_clear(mrb, &result_mpz);
  return mrb_obj_value(result);
}
