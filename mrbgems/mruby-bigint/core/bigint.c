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
#define dg(x,i) (((size_t)i < RBIGINT_SIZE(x))?RBIGINT_ARY(x)[i]:0)

#define LIMB_COPY(x, y, num) do { \
  mp_limb *_x = (x); \
  const mp_limb *_y = (y); \
  for (size_t _num = (num); _num > 0; _num--) { \
    *_x++ = *_y++; \
  } \
} while (0)
#define LIMB_ZERO(x, num) do { \
  mp_limb *_x = (x); \
  for (size_t _num = (num); _num > 0; _num--) { \
    *_x++ = 0; \
  } \
} while (0)

static void
mpz_init(mrb_state *mrb, struct RBigint *s)
{
  LIMB_ZERO(s->as.ary, RBIGINT_EMBED_SIZE_MAX);
  RBIGINT_SET_EMBED_SIGN(s, 0);
  RBIGINT_SET_EMBED_ZERO(s);
}

static void
mpz_realloc(mrb_state *mrb, struct RBigint *x, size_t size)
{
  if (RBIGINT_SIZE(x) < size) {
    if (RBIGINT_EMBED_P(x)) {
      if (size <= RBIGINT_EMBED_SIZE_MAX) {
        LIMB_ZERO(RBIGINT_EMBED_ARY(x) + RBIGINT_EMBED_SIZE(x), size - RBIGINT_EMBED_SIZE(x));
        RBIGINT_SET_EMBED_SIZE(x, size);
        return;
      }

      mp_limb *p = (mp_limb*)mrb_malloc(mrb, size*sizeof(mp_limb));
      LIMB_COPY(p, RBIGINT_EMBED_ARY(x), RBIGINT_EMBED_SIZE(x));
      x->as.heap.p = p;
      x->as.heap.sn = RBIGINT_EMBED_SIGN(x);
      x->as.heap.sz = RBIGINT_EMBED_SIZE(x);
      RBIGINT_SET_HEAP(x);
    }
    else {
      x->as.heap.p = (mp_limb*)mrb_realloc(mrb, x->as.heap.p, size*sizeof(mp_limb));
    }

    LIMB_ZERO(x->as.heap.p + x->as.heap.sz, size - x->as.heap.sz);
    x->as.heap.sz = size;
  }
}

static void
mpz_set(mrb_state *mrb, struct RBigint *y, struct RBigint *x)
{
  size_t k = RBIGINT_SIZE(x);

  mpz_realloc(mrb, y, k);
  LIMB_COPY(RBIGINT_ARY(y), RBIGINT_ARY(x), k);
  RBIGINT_SET_SIZE(y, k);
  RBIGINT_SET_SIGN(y, RBIGINT_SIGN(x));
}

static void
mpz_init_set(mrb_state *mrb, struct RBigint *s, struct RBigint *t)
{
  mpz_init(mrb, s);
  mpz_set(mrb, s, t);
}

static void
mpz_set_int(mrb_state *mrb, struct RBigint *y, mrb_int v)
{
  mrb_uint u;

  if (v == 0) {
    RBIGINT_SET_SIGN(y, 0);
    u = 0;
  }
  else if (v > 0) {
    RBIGINT_SET_SIGN(y, 1);
    u = v;
  }
  else /* if (v < 0) */ {
    RBIGINT_SET_SIGN(y, -1);
    if (v == MRB_INT_MIN) u = v;
    else u = -v;
  }
#if MRB_INT_BIT > DIG_SIZE
  if ((u & ~DIG_MASK) != 0) {
    mpz_realloc(mrb, y, 2);
    y->as.ary[1] = (mp_limb)HIGH(u);
    y->as.ary[0] = (mp_limb)LOW(u);
  }
  else
#endif
  {
    mpz_realloc(mrb, y, 1);
    y->as.ary[0] = (mp_limb)u;
  }
}

static void
mpz_set_uint64(mrb_state *mrb, struct RBigint *y, uint64_t u)
{
  const size_t len = sizeof(uint64_t) / sizeof(mp_limb);

  mpz_realloc(mrb, y, len);
  mrb_assert(RBIGINT_EMBED_P(y));
  RBIGINT_SET_EMBED_SIGN(y, (u != 0 ? 1 : 0));
  for (size_t i=0; i<len; i++) {
    y->as.ary[i++] = (mp_limb)LOW(u);
    u >>= DIG_SIZE;
  }
}

#ifdef MRB_INT32
static void
mpz_set_int64(mrb_state *mrb, struct RBigint *y, int64_t v)
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
    RBIGINT_SET_EMBED_SIGN(y, -1);
  }
}
#endif

static void
mpz_init_set_int(mrb_state *mrb, struct RBigint *y, mrb_int v)
{
  mpz_init(mrb, y);
  mpz_set_int(mrb, y, v);
}

static void
mpz_clear(mrb_state *mrb, struct RBigint *s)
{
  if (!RBIGINT_EMBED_P(s)) mrb_free(mrb, RBIGINT_HEAP_ARY(s));
  for (size_t i = 0; i < RBIGINT_EMBED_SIZE_MAX; i++) {
    s->as.ary[i] = 0;
  }
  RBIGINT_SET_EMBED_SIGN(s, 0);
  RBIGINT_SET_EMBED_ZERO(s);
}

static void
mpz_move(mrb_state *mrb, struct RBigint *y, struct RBigint *x)
{
  mpz_clear(mrb, y);

  if (RBIGINT_EMBED_P(x)) {
    LIMB_COPY(y->as.ary, x->as.ary, RBIGINT_EMBED_SIZE_MAX);
    RBIGINT_SET_EMBED_SIGN(y, RBIGINT_EMBED_SIGN(x));
    RBIGINT_SET_EMBED_SIZE(y, RBIGINT_EMBED_SIZE(x));
  }
  else {
    y->as.heap.sn = x->as.heap.sn;
    y->as.heap.sz = x->as.heap.sz;
    y->as.heap.p = x->as.heap.p;
    RBIGINT_SET_HEAP(y);
    x->as.heap.p = NULL;
    x->as.heap.sn = 0;
    x->as.heap.sz = 0;
  }
}

static size_t
digits(struct RBigint *x)
{
  size_t i;

  if (RBIGINT_SIZE(x) == 0) return 0;
  const mp_limb *p = RBIGINT_ARY(x);
  for (i = RBIGINT_SIZE(x) - 1; p[i] == 0; i--)
    if (i == 0) break;
  return i+1;
}

static void
trim(struct RBigint *x)
{
  size_t n = RBIGINT_SIZE(x);
  const mp_limb *p = RBIGINT_ARY(x) + n;
  while (n > 0 && *--p == 0) {
    n--;
  }
  RBIGINT_SET_SIZE(x, n);
}

/* z = x + y, without regard for sign */
static void
uadd(mrb_state *mrb, struct RBigint *z, struct RBigint *x, struct RBigint *y)
{
  size_t xsz = RBIGINT_SIZE(x);
  size_t ysz = RBIGINT_SIZE(y);
  if (ysz < xsz) {
    struct RBigint *t;          /* swap x,y */
    t=x; x=y; y=t;
    xsz = RBIGINT_SIZE(x);
    ysz = RBIGINT_SIZE(y);
  }

  /* now RBIGINT_SIZE(y) >= RBIGINT_SIZE(x) */
  mpz_realloc(mrb, z, ysz+1);

  const mp_limb *xp = RBIGINT_ARY(x);
  const mp_limb *yp = RBIGINT_ARY(y);
  mp_limb *zp = RBIGINT_ARY(z);
  mp_dbl_limb c = 0;
  size_t i;
  for (i=0; i<xsz; i++) {
    c += (mp_dbl_limb)yp[i] + (mp_dbl_limb)xp[i];
    zp[i] = LOW(c);
    c >>= DIG_SIZE;
  }
  for (;i<ysz; i++) {
    c += yp[i];
    zp[i] = LOW(c);
    c >>= DIG_SIZE;
  }
  zp[ysz] = (mp_limb)c;
  trim(z);
}

/* z = y - x, ignoring sign */
/* precondition: abs(y) >= abs(x) */
static void
usub(mrb_state *mrb, struct RBigint *z, struct RBigint *y, struct RBigint *x)
{
  size_t xsz = RBIGINT_SIZE(x);
  size_t ysz = RBIGINT_SIZE(y);
  mpz_realloc(mrb, z, ysz);
  mp_dbl_limb_signed b = 0;
  const mp_limb *xp = RBIGINT_ARY(x);
  const mp_limb *yp = RBIGINT_ARY(y);
  mp_limb *zp = RBIGINT_ARY(z);
  size_t i;
  for (i=0;i<xsz;i++) {
    b += (mp_dbl_limb_signed)yp[i];
    b -= (mp_dbl_limb_signed)xp[i];
    zp[i] = LOW(b);
    b = HIGH(b);
  }
  for (;i<ysz; i++) {
    b += yp[i];
    zp[i] = LOW(b);
    b = HIGH(b);
  }
  RBIGINT_SET_SIZE(z, digits(z));
}

/* compare abs(x) and abs(y) */
static int
ucmp(struct RBigint *y, struct RBigint *x)
{
  size_t xsz = RBIGINT_SIZE(x);
  size_t ysz = RBIGINT_SIZE(y);
  if (ysz < xsz) return -1;
  if (ysz > xsz) return 1;
  if (xsz == 0) return 0;
  const mp_limb *xp = RBIGINT_ARY(x);
  const mp_limb *yp = RBIGINT_ARY(y);
  for (size_t i=xsz-1;; i--) {
    mp_limb a = yp[i];
    mp_limb b = xp[i];
    if (a > b) return 1;
    if (a < b) return -1;
    if (i == 0) break;
  }
  return 0;
}

#define zero_p(x) (RBIGINT_SIGN(x) == 0)

/* check if all digits are zero */
static int
uzero_p(struct RBigint *x)
{
  size_t xsz = RBIGINT_SIZE(x);
  if (xsz == 0) return 1;
  const mp_limb *xp = RBIGINT_ARY(x);
  for (size_t i=0; i < xsz; i++)
    if (xp[i] != 0)
      return 0;
  return 1;
}

static void
zero(struct RBigint *x)
{
  RBIGINT_SET_SIGN(x, 0);
  if (RBIGINT_EMBED_P(x)) {
    for (size_t i = 0; i < RBIGINT_EMBED_SIZE_MAX; i++) {
      x->as.ary[i] = 0;
    }
  }
  else if (x->as.heap.p) {
    RBIGINT_SET_HEAP_SIZE(x, 1);
    RBIGINT_HEAP_ARY(x)[0]=0;
  }
  else {
    RBIGINT_SET_HEAP_SIZE(x, 0);
  }
}

/* z = x + y */
static void
mpz_add(mrb_state *mrb, struct RBigint *zz, struct RBigint *x, struct RBigint *y)
{
  if (zero_p(x)) {
    mpz_set(mrb, zz, y);
    return;
  }
  if (zero_p(y)) {
    mpz_set(mrb, zz, x);
    return;
  }
  struct RBigint z;
  mpz_init(mrb, &z);

  if (RBIGINT_SIGN(x) > 0 && RBIGINT_SIGN(y) > 0) {
    uadd(mrb, &z, x, y);
    RBIGINT_SET_SIGN(&z, 1);
  }
  else if (RBIGINT_SIGN(x) < 0 && RBIGINT_SIGN(y) < 0) {
    uadd(mrb, &z, x, y);
    RBIGINT_SET_SIGN(&z, -1);
  }
  else {
    int mg;

    /* signs differ */
    if ((mg = ucmp(x,y)) == 0) {
      zero(&z);
    }
    else if (mg > 0) {  /* abs(y) < abs(x) */
      usub(mrb, &z, x, y);
      RBIGINT_SET_SIGN(&z, (RBIGINT_SIGN(x) > 0 && RBIGINT_SIGN(y) < 0) ? 1 : (-1));
    }
    else { /* abs(y) > abs(x) */
      usub(mrb, &z, y, x);
      RBIGINT_SET_SIGN(&z, (RBIGINT_SIGN(x) < 0 && RBIGINT_SIGN(y) > 0) ? 1 : (-1));
    }
  }
  trim(&z);
  mpz_move(mrb, zz, &z);
}

/* x = y - n */
static void
mpz_add_int(mrb_state *mrb, struct RBigint *x, struct RBigint *y, mrb_int n)
{
  struct RBigint z;

  mpz_init_set_int(mrb, &z, n);
  mpz_add(mrb, x, y, &z);
  mpz_clear(mrb, &z);
}

/* z = x - y  -- just use mpz_add - I'm lazy */
static void
mpz_sub(mrb_state *mrb, struct RBigint *z, struct RBigint *x, struct RBigint *y)
{
  struct RBigint u;

  mpz_init(mrb, &u);
  mpz_set(mrb, &u, y);
  RBIGINT_SET_SIGN(&u, -(RBIGINT_SIGN(&u)));
  mpz_add(mrb, z, x, &u);
  mpz_clear(mrb, &u);
}

/* x = y - n */
static void
mpz_sub_int(mrb_state *mrb, struct RBigint *x, struct RBigint *y, mrb_int n)
{
  struct RBigint z;

  mpz_init_set_int(mrb, &z, n);
  mpz_sub(mrb, x, y, &z);
  mpz_clear(mrb, &z);
}

/* w = u * v */
/* Simple Multiply */
static void
mul_base(mrb_state *mrb, struct RBigint *ww, struct RBigint *u, struct RBigint *v)
{
  if (zero_p(u) || zero_p(v)) {
    mpz_set_int(mrb, ww, 0);
    return;
  }

  size_t usz = RBIGINT_SIZE(u);
  size_t vsz = RBIGINT_SIZE(v);
  struct RBigint w;
  mpz_init(mrb, &w);
  mpz_realloc(mrb, &w, usz + vsz);

  const mp_limb *up = RBIGINT_ARY(u);
  const mp_limb *vp = RBIGINT_ARY(v);
  mp_limb *wp = RBIGINT_ARY(&w);
  for (size_t j = 0; j < usz; j++) {
    size_t i;
    mp_dbl_limb cc = (mp_limb)0;
    mp_limb u0 = up[j];
    if (u0 == 0) continue;
    for (i = 0; i < vsz; i++) {
      mp_limb v0 = vp[i];
      if (v0 == 0) continue;
      cc += (mp_dbl_limb)wp[i + j] + (mp_dbl_limb)u0 * (mp_dbl_limb)v0;
      wp[i + j] = LOW(cc);
      cc = HIGH(cc);
    }
    if (cc) {
      wp[i + j] = (mp_limb)cc;
    }
  }
  RBIGINT_SET_SIGN(&w, RBIGINT_SIGN(u) * RBIGINT_SIGN(v));
  trim(&w);
  mpz_move(mrb, ww, &w);
}

static void mpz_mul_2exp(mrb_state *mrb, struct RBigint *z, struct RBigint *x, mrb_int e);

/* Thresholds */
#define KARATSUBA_THRESHOLD 32
#define MAX_RECURSION_DEPTH 16

/* Karatsuba Multiply */
static void
mul_karatsuba(mrb_state *mrb, struct RBigint *ww, struct RBigint *u, struct RBigint *v, int depth)
{
  if (depth > MAX_RECURSION_DEPTH || RBIGINT_SIZE(u) < KARATSUBA_THRESHOLD || RBIGINT_SIZE(v) < KARATSUBA_THRESHOLD) {
    mul_base(mrb, ww, u, v);
    return;
  }

  size_t n = (RBIGINT_SIZE(u) > RBIGINT_SIZE(v) ? RBIGINT_SIZE(v) : RBIGINT_SIZE(u)) / 2;

  /* Split u, v into low/high */
  struct RBigint u0, u1, v0, v1;
  mpz_init(mrb, &u0); mpz_init(mrb, &u1);
  mpz_init(mrb, &v0); mpz_init(mrb, &v1);
  RBIGINT_SET_SIGN(&u0, 1);
  RBIGINT_SET_SIGN(&u1, 1);
  RBIGINT_SET_SIGN(&v0, 1);
  RBIGINT_SET_SIGN(&v1, 1);

  mpz_realloc(mrb, &u0, n);
  mpz_realloc(mrb, &u1, RBIGINT_SIZE(u) - n);
  mpz_realloc(mrb, &v0, n);
  mpz_realloc(mrb, &v1, RBIGINT_SIZE(v) - n);

  LIMB_COPY(RBIGINT_ARY(&u0), RBIGINT_ARY(u), n);
  LIMB_COPY(RBIGINT_ARY(&u1), RBIGINT_ARY(u) + n, RBIGINT_SIZE(u) - n);
  LIMB_COPY(RBIGINT_ARY(&v0), RBIGINT_ARY(v), n);
  LIMB_COPY(RBIGINT_ARY(&v1), RBIGINT_ARY(v) + n, RBIGINT_SIZE(v) - n);

  /* u1*v1 (high part) */
  struct RBigint z2;
  mpz_init(mrb, &z2);
  mul_karatsuba(mrb, &z2, &u1, &v1, depth + 1);

  /* u0*v0 (low part) */
  struct RBigint z0;
  mpz_init(mrb, &z0);
  mul_karatsuba(mrb, &z0, &u0, &v0, depth + 1);

  /* (u1+u0)*(v1+v0) */
  struct RBigint u0u1, v0v1, z1;
  mpz_init(mrb, &u0u1); mpz_init(mrb, &v0v1); mpz_init(mrb, &z1);

  mpz_add(mrb, &u0u1, &u0, &u1);
  mpz_add(mrb, &v0v1, &v0, &v1);
  mul_karatsuba(mrb, &z1, &u0u1, &v0v1, depth + 1);
  mpz_sub(mrb, &z1, &z1, &z0);
  mpz_sub(mrb, &z1, &z1, &z2);

  /* Bitshift z1/z2 */
  mpz_mul_2exp(mrb, &z2, &z2, 2 * n * sizeof(mp_limb) * 8);
  mpz_mul_2exp(mrb, &z1, &z1, n * sizeof(mp_limb) * 8);

  /* Combine the result*/
  mpz_add(mrb, ww, &z2, &z1);
  mpz_add(mrb, ww, ww, &z0);

  // Clean-Up Memory
  mpz_clear(mrb, &u0); mpz_clear(mrb, &u1);
  mpz_clear(mrb, &v0); mpz_clear(mrb, &v1);
  mpz_clear(mrb, &z0); mpz_clear(mrb, &z1); mpz_clear(mrb, &z2);
  mpz_clear(mrb, &u0u1); mpz_clear(mrb, &v0v1);
}

// Multiplication Entry Point */
static void
mpz_mul(mrb_state *mrb, struct RBigint *ww, struct RBigint *u, struct RBigint *v)
{
  mul_karatsuba(mrb, ww, u, v, 0);
  RBIGINT_SET_SIGN(ww, RBIGINT_SIGN(u) * RBIGINT_SIGN(v));
}

static void
mpz_mul_int(mrb_state *mrb, struct RBigint *x, struct RBigint *y, mrb_int n)
{
  if (n == 0) {
    zero(x);
    return;
  }

  struct RBigint z;
  mpz_init_set_int(mrb, &z, n);
  mpz_mul(mrb, x, y, &z);
  mpz_clear(mrb, &z);
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
urshift(mrb_state *mrb, struct RBigint *c1, struct RBigint *a, size_t n)
{
  mrb_assert(n < DIG_SIZE);

  if (n == 0)
    mpz_set(mrb, c1, a);
  else if (uzero_p(a)) {
    mpz_set_int(mrb, c1, 0);
  }
  else {
    struct RBigint c;
    mp_limb cc = 0;
    mp_dbl_limb rm = (((mp_dbl_limb)1<<n) - 1);
    size_t asz = RBIGINT_SIZE(a);

    mpz_init(mrb, &c);
    mpz_realloc(mrb, &c, asz);
    const mp_limb *ap = RBIGINT_ARY(a);
    mp_limb *cp = RBIGINT_ARY(&c);
    for (size_t i=asz-1;; i--) {
      cp[i] = ((ap[i] >> n) | cc) & DIG_MASK;
      cc = (ap[i] & rm) << (DIG_SIZE - n);
      if (i == 0) break;
    }
    trim(&c);
    mpz_move(mrb, c1, &c);
  }
}

/* c1 = a<<n */
/* n must be < DIG_SIZE */
static void
ulshift(mrb_state *mrb, struct RBigint *c1, struct RBigint *a, size_t n)
{
  mrb_assert(n < DIG_SIZE);
  if (n == 0)
    mpz_set(mrb, c1, a);
  else if (uzero_p(a)) {
    mpz_set_int(mrb, c1, 0);
  }
  else {
    mp_limb cc = 0;
    struct RBigint c;
    mp_limb rm = (((mp_dbl_limb)1<<n) - 1) << (DIG_SIZE-n);
    size_t asz = RBIGINT_SIZE(a);

    mpz_init(mrb, &c);
    mpz_realloc(mrb, &c, asz+1);

    const mp_limb *ap = RBIGINT_ARY(a);
    mp_limb *cp = RBIGINT_ARY(&c);
    size_t i;
    for (i=0; i<asz; i++) {
      cp[i] = ((ap[i] << n) | cc) & DIG_MASK;
      cc = (ap[i] & rm) >> (DIG_SIZE-n);
    }
    cp[i] = cc;
    trim(&c);
    mpz_move(mrb, c1, &c);
  }
}

/* internal routine to compute x/y and x%y ignoring signs */
/* qq = xx/yy; rr = xx%yy */
static void
udiv(mrb_state *mrb, struct RBigint *qq, struct RBigint *rr, struct RBigint *xx, struct RBigint *yy)
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

  struct RBigint q, x, y;

  mrb_assert(RBIGINT_SIGN(yy) != 0);      /* divided by zero */
  mpz_init(mrb, &q);
  mpz_init(mrb, &x);
  mpz_init(mrb, &y);
  mpz_realloc(mrb, &x, RBIGINT_SIZE(xx)+1);
  size_t yd = digits(yy);
  size_t ns = lzb(RBIGINT_ARY(yy)[yd-1]);
  ulshift(mrb, &x, xx, ns);
  ulshift(mrb, &y, yy, ns);
  size_t xd = digits(&x);
  mpz_realloc(mrb, &q, xd);
  mp_dbl_limb z = RBIGINT_ARY(&y)[yd-1];
  if (xd>=yd) {
    mp_limb *xp = RBIGINT_ARY(&x);
    const mp_limb *yp = RBIGINT_ARY(&y);
    mp_limb *qp = RBIGINT_ARY(&q);
    for (size_t j=xd-yd;; j--) {
      mp_dbl_limb_signed b=0;
      mp_dbl_limb qhat;

      if (j+yd == xd)
        qhat = xp[j+yd-1] / z;
      else
        qhat = (((mp_dbl_limb)xp[j+yd] << DIG_SIZE) + xp[j+yd-1]) / z;
      if (qhat) {
        size_t i;

        for (i=0; i<yd; i++) {
          mp_dbl_limb zz = qhat * yp[i];
          mp_dbl_limb_signed u = LOW(b)+xp[i+j]-LOW(zz);
          xp[i+j] = LOW(u);
          b = HIGH(b) - HIGH(zz) + HIGH(u);
        }
        if (xd > i+j) {
          b += xp[i+j];
        }
      }
      for (; b!=0; qhat--) {
        mp_dbl_limb c = 0;
        for (size_t i=0; i<yd; i++) {
          c += (mp_dbl_limb)xp[i+j] + (mp_dbl_limb)yp[i];
          xp[i+j] = LOW(c);
          c = HIGH(c);
        }
        b += c;
      }
      qp[j] = (mp_limb)qhat;
      if (j == 0) break;
    }
  }
  RBIGINT_SET_SIZE(&x, RBIGINT_SIZE(yy));
  urshift(mrb, rr, &x, ns);
  trim(&q);
  mpz_move(mrb, qq, &q);
  mpz_clear(mrb, &x);
  mpz_clear(mrb, &y);
}

static void
mpz_mdiv(mrb_state *mrb, struct RBigint *q, struct RBigint *x, struct RBigint *y)
{
  struct RBigint r;
  short sn1 = RBIGINT_SIGN(x), sn2 = RBIGINT_SIGN(y), qsign;

  if (zero_p(x)) {
    mpz_init_set_int(mrb, q, 0);
    return;
  }
  mpz_init(mrb, &r);
  udiv(mrb, q, &r, x, y);
  RBIGINT_SET_SIGN(q, qsign = sn1*sn2);
  if (uzero_p(q))
    RBIGINT_SET_SIGN(q, 0);
  /* now if r != 0 and q < 0 we need to round q towards -inf */
  if (!uzero_p(&r) && qsign < 0)
    mpz_sub_int(mrb, q, q, 1);
  mpz_clear(mrb, &r);
}

static void
mpz_mmod(mrb_state *mrb, struct RBigint *r, struct RBigint *x, struct RBigint *y)
{
  struct RBigint q;
  short sn1 = RBIGINT_SIGN(x), sn2 = RBIGINT_SIGN(y), sn3;

  mpz_init(mrb, &q);
  if (sn1 == 0) {
    zero(r);
    return;
  }
  udiv(mrb, &q, r, x, y);
  mpz_clear(mrb, &q);
  if (uzero_p(r)) {
    RBIGINT_SET_SIGN(r, 0);
    return;
  }
  sn3 = sn1*sn2;
  if (sn3 > 0)
    RBIGINT_SET_SIGN(r, sn1);
  else if (sn1 < 0 && sn2 > 0) {
    RBIGINT_SET_SIGN(r, 1);
    mpz_sub(mrb, r, y, r);
  }
  else {
    RBIGINT_SET_SIGN(r, 1);
    mpz_add(mrb, r, y, r);
  }
}

static void
mpz_mdivmod(mrb_state *mrb, struct RBigint *q, struct RBigint *r, struct RBigint *x, struct RBigint *y)
{
  short sn1 = RBIGINT_SIGN(x), sn2 = RBIGINT_SIGN(y), qsign;

  if (sn1 == 0) {
    zero(q);
    zero(r);
    return;
  }
  udiv(mrb, q, r, x, y);
  RBIGINT_SET_SIGN(q, qsign = sn1*sn2);
  if (uzero_p(r)) {
    /* q != 0, since q=r=0 would mean x=0, which was tested above */
    RBIGINT_SET_SIGN(r, 0);
    return;
  }
  if (RBIGINT_SIGN(q) > 0)
    RBIGINT_SET_SIGN(r, sn1);
  else if (sn1 < 0 && sn2 > 0) {
    RBIGINT_SET_SIGN(r, 1);
    mpz_sub(mrb, r, y, r);
  }
  else {
    RBIGINT_SET_SIGN(r, 1);
    mpz_add(mrb, r, y, r);
  }
  if (uzero_p(q))
    RBIGINT_SET_SIGN(q, 0);
  /* now if r != 0 and q < 0 we need to round q towards -inf */
  if (!uzero_p(r) && qsign < 0)
    mpz_sub_int(mrb, q, q, 1);
}

static void
mpz_mod(mrb_state *mrb, struct RBigint *r, struct RBigint *x, struct RBigint *y)
{
  struct RBigint q;
  short sn = RBIGINT_SIGN(x);

  if (zero_p(x)) {
    zero(r);
    return;
  }
  mpz_init(mrb, &q);
  udiv(mrb, &q, r, x, y);
  RBIGINT_SET_SIGN(r, sn);
  if (uzero_p(r))
    RBIGINT_SET_SIGN(r, 0);
  mpz_clear(mrb, &q);
}

static mrb_int
mpz_cmp(mrb_state *mrb, struct RBigint *x, struct RBigint *y)
{
  if (RBIGINT_SIGN(x) < 0 && RBIGINT_SIGN(y) > 0)
    return (-1);
  if (RBIGINT_SIGN(x) > 0 && RBIGINT_SIGN(y) < 0)
    return 1;
  int abscmp=ucmp(x, y);
  if (RBIGINT_SIGN(x) >=0 && RBIGINT_SIGN(y) >=0)
    return abscmp;
  return (-abscmp);          // if (RBIGINT_SIGN(x) <=0 && RBIGINT_SIGN(y) <=0)
}

/* 2<=base<=36 - this overestimates the optimal value, which is OK */
static size_t
mpz_sizeinbase(struct RBigint *x, mrb_int base)
{
  size_t i, j;

  size_t bits = digits(x) * DIG_SIZE;
  mrb_assert(2 <= base && base <= 36);

  if (zero_p(x) || RBIGINT_SIZE(x) == 0) return 0;
  for (j=0,i=1; i<=(size_t)base; i*=2,j++)
    ;
  return bits/(j-1)+1;
}

static int
mpz_init_set_str(mrb_state *mrb, struct RBigint *x, const char *s, mrb_int len, mrb_int base)
{
  int retval = 0;
  short sn;
  uint8_t k;
  mpz_init(mrb, x);
  zero(x);
  if (*s == '-') {
    sn = -1; s++;
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
    mpz_mul_int(mrb, x, x, base);
    mpz_add_int(mrb, x, x, k);
  }
  RBIGINT_SET_SIGN(x, sn);
  return retval;
}

static char*
mpz_get_str(mrb_state *mrb, char *s, mrb_int sz, mrb_int base, struct RBigint *x)
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
    mp_limb mask = base - 1;
    mp_dbl_limb value = 0;
    int bits = 0;

    const mp_limb *xp = RBIGINT_ARY(x);
    for (int i = 0; i < xlen; i++) {
      value |= (mp_dbl_limb)xp[i] << bits;
      bits += DIG_SIZE;
      while (bits >= shift) {
        mp_limb digit = value & mask;
        value >>= shift;
        bits -= shift;

        if (digit < 10) *s++ = '0' + digit;
        else *s++ = 'a' + digit - 10;
      }
    }
  }
  else {
    mp_limb *t = (mp_limb*)mrb_malloc(mrb, xlen*sizeof(mp_limb));
    mp_limb *tend = t + xlen;
    memcpy(t, RBIGINT_ARY(x), xlen*sizeof(mp_limb));
    mp_limb b2 = (mp_limb)base;
    const int blim = (sizeof(mp_limb)<4)?(base<=10?4:3):(base<=10?9:5);
    for (int i=1; i<blim; i++) {
      b2 *= (mp_limb)base;
    }

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
      for (int i=0; i<blim; i++) {
        mp_limb a0 = (mp_limb)(a % base);
        if (a0 < 10) a0 += '0';
        else a0 += 'a' - 10;
        if (s == se) break;
        *s++ = (char)a0;
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
  if (RBIGINT_SIGN(x) < 0) {
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
mpz_get_int(struct RBigint *y, mrb_int *v)
{
  if (zero_p(y)) {
    *v = 0;
    return TRUE;
  }

  mp_dbl_limb i = 0;
  size_t ysz = RBIGINT_SIZE(y);
  const mp_limb *yp = RBIGINT_ARY(y);
  const mp_limb *d = yp + ysz;

  while (d-- > yp) {
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
  if (RBIGINT_SIGN(y) < 0) {
    *v = -(mrb_int)i;
  }
  else {
    *v = (mrb_int)i;
  }
  return TRUE;
}

static void
mpz_mul_2exp(mrb_state *mrb, struct RBigint *z, struct RBigint *x, mrb_int e)
{
  if (e==0)
    mpz_set(mrb, z, x);
  else {
    short sn = RBIGINT_SIGN(x);
    size_t digs = e / DIG_SIZE;
    size_t bs = e % DIG_SIZE;
    size_t xsz = RBIGINT_SIZE(x);
    struct RBigint y;

    mpz_init(mrb, &y);
    mpz_realloc(mrb, &y, xsz+digs);
    const mp_limb *xp = RBIGINT_ARY(x);
    mp_limb *yp = RBIGINT_ARY(&y);
    for (size_t i=0;i<xsz;i++)
      yp[i+digs] = xp[i];
    if (bs) {
      ulshift(mrb, z, &y, bs);
      mpz_clear(mrb, &y);
    }
    else {
      mpz_move(mrb, z, &y);
    }
    RBIGINT_SET_SIGN(z, sn);
  }
}

static void
mpz_div_2exp(mrb_state *mrb, struct RBigint *z, struct RBigint *x, mrb_int e)
{
  short sn = RBIGINT_SIGN(x);
  if (e==0)
    mpz_set(mrb, z, x);
  else {
    size_t digs = e / DIG_SIZE;
    size_t bs = e % DIG_SIZE;
    size_t xsz = RBIGINT_SIZE(x);
    struct RBigint y;

    mpz_init(mrb, &y);
    mpz_realloc(mrb, &y, xsz-digs);
    const mp_limb *xp = RBIGINT_ARY(x);
    mp_limb *yp = RBIGINT_ARY(&y);
    for (size_t i=0; i < xsz-digs; i++)
      yp[i] = xp[i+digs];
    if (bs) {
      urshift(mrb, z, &y, bs);
      mpz_clear(mrb, &y);
    }
    else {
      mpz_move(mrb, z, &y);
    }
    if (uzero_p(z))
      RBIGINT_SET_SIGN(z, 0);
    else {
      RBIGINT_SET_SIGN(z, sn);
    }
  }
}

static void
mpz_neg(mrb_state *mrb, struct RBigint *x, struct RBigint *y)
{
  mpz_set(mrb, x, y);
  RBIGINT_SET_SIGN(x, -RBIGINT_SIGN(y));
}

#define make_2comp(v,c) do { v=~(v)+(c); c=((v)==0 && (c));} while (0)

void
mpz_and(mrb_state *mrb, struct RBigint *z, struct RBigint *x, struct RBigint *y)
{
  if (zero_p(x) || zero_p(y)) {
    zero(z);
    return;
  }
  size_t xsz = RBIGINT_SIZE(x);
  size_t ysz = RBIGINT_SIZE(y);
  mrb_assert(xsz > 0 || ysz > 0);

  size_t max_sz = (xsz > ysz) ? xsz : ysz;
  mpz_realloc(mrb, z, max_sz);
  RBIGINT_SET_SIGN(z, (RBIGINT_SIGN(x) == RBIGINT_SIGN(y)) ? RBIGINT_SIGN(x) : 1);

  const mp_limb *xp = RBIGINT_ARY(x);
  const mp_limb *yp = RBIGINT_ARY(y);
  mp_limb *zp = RBIGINT_ARY(z);
  mrb_bool xneg = RBIGINT_SIGN(x) < 0;
  mrb_bool yneg = RBIGINT_SIGN(y) < 0;
  mrb_bool zneg = RBIGINT_SIGN(z) < 0;
  char c1 = 1, c2 = 1, c3 = 1;
  for (size_t i = 0; i < max_sz; i++) {
    mp_limb xv = (i < xsz) ? xp[i] : 0;
    mp_limb yv = (i < ysz) ? yp[i] : 0;

    if (xneg) make_2comp(xv, c1);
    if (yneg) make_2comp(yv, c2);
    mp_limb zv = xv & yv;
    if (zneg) make_2comp(zv, c3);
    zp[i] = zv;
  }
}

static void
mpz_or(mrb_state *mrb, struct RBigint *z, struct RBigint *x, struct RBigint *y)  /* not the most efficient way to do this */
{
  if (zero_p(x)) {
    mpz_set(mrb, z, y);
    return;
  }
  if (zero_p(y)) {
    mpz_set(mrb, z, x);
    return;
  }
  size_t xsz = RBIGINT_SIZE(x);
  size_t ysz = RBIGINT_SIZE(y);
  mrb_assert(xsz > 0 || ysz > 0);

  size_t max_sz = (xsz > ysz) ? xsz : ysz;
  mpz_realloc(mrb, z, max_sz);
  RBIGINT_SET_SIGN(z, (RBIGINT_SIGN(x) == RBIGINT_SIGN(y)) ? RBIGINT_SIGN(x) : -1);

  const mp_limb *xp = RBIGINT_ARY(x);
  const mp_limb *yp = RBIGINT_ARY(y);
  mp_limb *zp = RBIGINT_ARY(z);
  mrb_bool xneg = RBIGINT_SIGN(x) < 0;
  mrb_bool yneg = RBIGINT_SIGN(y) < 0;
  mrb_bool zneg = RBIGINT_SIGN(z) < 0;
  char c1 = 1, c2 = 1, c3 = 1;
  for (size_t i = 0; i < max_sz; i++) {
    mp_limb xv = (i < xsz) ? xp[i] : 0;
    mp_limb yv = (i < ysz) ? yp[i] : 0;

    if (xneg) make_2comp(xv, c1);
    if (yneg) make_2comp(yv, c2);
    mp_limb zv = xv | yv;
    if (zneg) make_2comp(zv, c3);
    zp[i] = zv;
  }
}

static void
mpz_xor(mrb_state *mrb, struct RBigint *z, struct RBigint *x, struct RBigint *y)  /* not the most efficient way to do this */
{
  if (zero_p(x)) {
    mpz_set(mrb, z, y);
    return;
  }
  if (zero_p(y)) {
    mpz_set(mrb, z, x);
    return;
  }
  size_t xsz = RBIGINT_SIZE(x);
  size_t ysz = RBIGINT_SIZE(y);
  mrb_assert(xsz > 0 || ysz > 0);

  size_t max_sz = (xsz > ysz) ? xsz : ysz;
  mpz_realloc(mrb, z, max_sz);
  RBIGINT_SET_SIGN(z, (RBIGINT_SIGN(x) == RBIGINT_SIGN(y)) ? 1 : -1);

  const mp_limb *xp = RBIGINT_ARY(x);
  const mp_limb *yp = RBIGINT_ARY(y);
  mp_limb *zp = RBIGINT_ARY(z);
  mrb_bool xneg = RBIGINT_SIGN(x) < 0;
  mrb_bool yneg = RBIGINT_SIGN(y) < 0;
  mrb_bool zneg = RBIGINT_SIGN(z) < 0;
  char c1 = 1, c2 = 1, c3 = 1;
  for (size_t i = 0; i < max_sz; i++) {
    mp_limb xv = (i < xsz) ? xp[i] : 0;
    mp_limb yv = (i < ysz) ? yp[i] : 0;

    if (xneg) make_2comp(xv, c1);
    if (yneg) make_2comp(yv, c2);
    mp_limb zv = xv ^ yv;
    if (zneg) make_2comp(zv, c3);
    zp[i] = zv;
  }
}

static void
mpz_pow(mrb_state *mrb, struct RBigint *zz, struct RBigint *x, mrb_int e)
{
  struct RBigint t;
  mrb_uint mask = 1ULL<<(sizeof(mrb_int)*8-1);

  if (e==0) {
    mpz_set_int(mrb, zz, 1L);
    return;
  }

  mpz_init(mrb, &t);
  mpz_set(mrb, &t, x);
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
mpz_powm(mrb_state *mrb, struct RBigint *zz, struct RBigint *x, struct RBigint *ex, struct RBigint *n)
{
  if (zero_p(ex)) {
    mpz_set_int(mrb, zz, 1);
    return;
  }

  if (RBIGINT_SIGN(ex) < 0) {
    return;
  }

  struct RBigint t, b;
  mpz_init_set_int(mrb, &t, 1);
  mpz_init_set(mrb, &b, x);

  size_t len = digits(ex);
  for (size_t i=0; i<len; i++) {
    mp_limb e = RBIGINT_ARY(ex)[i];
    for (size_t j=0; j<sizeof(mp_limb)*8; j++) {
      if ((e & 1) == 1) {
        mpz_mul(mrb, &t, &t, &b);
        mpz_mod(mrb, &t, &t, n);
      }
      e >>= 1;
      mpz_mul(mrb, &b, &b, &b);
      mpz_mod(mrb, &b, &b, n);
    }
  }
  mpz_move(mrb, zz, &t);
  mpz_clear(mrb, &b);
}

static void
mpz_powm_i(mrb_state *mrb, struct RBigint *zz, struct RBigint *x, mrb_int ex, struct RBigint *n)
{
  if (ex == 0) {
    mpz_set_int(mrb, zz, 1);
    return;
  }

  if (ex < 0) {
    return;
  }

  struct RBigint t, b;
  mpz_init_set_int(mrb, &t, 1);
  mpz_init_set(mrb, &b, x);

  while (ex > 0) {
    if ((ex & 1) == 1) {
      mpz_mul(mrb, &t, &t, &b);
      mpz_mod(mrb, &t, &t, n);
    }
    ex >>= 1;
    mpz_mul(mrb, &b, &b, &b);
    mpz_mod(mrb, &b, &b, n);
  }
  mpz_move(mrb, zz, &t);
  mpz_clear(mrb, &b);
}

#ifdef MRB_USE_RATIONAL
static void
mpz_abs(mrb_state *mrb, struct RBigint *x, struct RBigint *y)
{
  mpz_init_set(mrb, x, y);
  if (RBIGINT_SIGN(y) == 0)
    RBIGINT_SET_SIGN(x, 0);
  else
    RBIGINT_SET_SIGN(x, 1);
}

static void
mpz_gcd(mrb_state *mrb, struct RBigint *gg, struct RBigint *aa, struct RBigint *bb)
{
  struct RBigint a, b, t;
  mpz_abs(mrb, &a, aa); mpz_abs(mrb, &b, bb);
  mpz_init(mrb, &t);

  while (RBIGINT_SIGN(&b) != 0) {
    mpz_mod(mrb, &t, &a, &b);
    mpz_set(mrb, &a, &b);
    mpz_set(mrb, &b, &t);
  }
  trim(&a);
  mpz_move(mrb, gg, &a);
  mpz_clear(mrb, &b);
  mpz_clear(mrb, &t);
}
#endif

/* --- mruby functions --- */
static struct RBigint*
bint_new(mrb_state *mrb)
{
  struct RBigint *b = MRB_OBJ_ALLOC(mrb, MRB_TT_BIGINT, mrb->integer_class);
  mpz_init(mrb, b);
  return b;
}

static struct RBigint*
bint_new_int(mrb_state *mrb, mrb_int x)
{
  struct RBigint *b = MRB_OBJ_ALLOC(mrb, MRB_TT_BIGINT, mrb->integer_class);
  mpz_init_set_int(mrb, b, x);
  return b;
}

mrb_value
mrb_bint_new_int(mrb_state *mrb, mrb_int x)
{
  struct RBigint *b = bint_new_int(mrb, x);
  return mrb_obj_value(b);
}

#ifdef MRB_INT32
mrb_value
mrb_bint_new_int64(mrb_state *mrb, int64_t x)
{
  struct RBigint *b = bint_new(mrb);
  mpz_init(mrb, b);
  mpz_set_int64(mrb, b, x);
  return mrb_obj_value(b);
}
#endif

mrb_value
mrb_bint_new_uint64(mrb_state *mrb, uint64_t x)
{
  struct RBigint *b = bint_new(mrb);
  mpz_init(mrb, b);
  mpz_set_uint64(mrb, b, x);
  return mrb_obj_value(b);
}

mrb_value
mrb_bint_new_str(mrb_state *mrb, const char *x, mrb_int len, mrb_int base)
{
  struct RBigint *b = MRB_OBJ_ALLOC(mrb, MRB_TT_BIGINT, mrb->integer_class);
  int sn = 1;
  if (base < 0) {
    base = -base;
    sn = -1;
  }
  mrb_assert(2 <= base && base <= 36);
  mpz_init_set_str(mrb, b, x, len, base);
  if (sn < 0) {
    RBIGINT_SET_SIGN(b, sn);
  }
  return mrb_obj_value(b);
}

static mrb_value
bint_norm(mrb_state *mrb, struct RBigint *b)
{
  mrb_int i;

  if (mpz_get_int(b, &i)) {
    return mrb_int_value(mrb, i);
  }
  return mrb_obj_value(b);
}

void
mrb_gc_free_bint(mrb_state *mrb, struct RBasic *x)
{
  struct RBigint *b = (struct RBigint*)x;
  mpz_clear(mrb, b);
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

  struct RBigint *bint = bint_new(mrb);
  struct RBigint *r = bint;
  RBIGINT_SET_SIGN(r, sn);

  mrb_float b = (double)DIG_BASE;
  mrb_float bi = 1.0 / b;
  size_t rn;
  mp_limb *rp;

  for (rn = 1; x >= b; rn++)
    x *= bi;

  mpz_realloc(mrb, r, rn);
  rp = RBIGINT_ARY(r);
  for (size_t i=rn-1;;i--) {
    mp_limb f = LOW((mp_limb)x);
    x -= f;
    mrb_assert(x < 1.0);
    rp[i] = f;
    if (i == 0) break;
  }
  return bint_norm(mrb, bint);
}

mrb_float
mrb_bint_as_float(mrb_state *mrb, mrb_value self)
{
  struct RBigint *b = RBIGINT(self);
  struct RBigint *i = b;
  mp_limb *d = RBIGINT_ARY(i) + RBIGINT_SIZE(i);
  mrb_float val = 0;

  while (d-- > RBIGINT_ARY(i)) {
    val = val * DIG_BASE + *d;
  }

  if (RBIGINT_SIGN(i) < 0) {
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
  struct RBigint *b = RBIGINT(x);
  mrb_int i;

  if (!mpz_get_int(b, &i)) {
    mrb_raise(mrb, E_RANGE_ERROR, "integer out of range");
  }
  return i;
}

#ifdef MRB_INT32
int64_t
mrb_bint_as_int64(mrb_state *mrb, mrb_value x)
{
  struct RBigint *b = RBIGINT(x);
  struct RBigint *m = b;
  uint64_t u = 0;
  size_t len = digits(m);

  if (len*sizeof(mp_limb) > sizeof(uint64_t)) {
  out_of_range:
    mrb_raise(mrb, E_RANGE_ERROR, "integer out of range");
  }
  for (size_t i=len-1; ; i--) {
    u <<= DIG_SIZE;
    u |= RBIGINT_ARY(m)[i];
    if (i==0) break;
  }
  if (u > INT64_MAX) goto out_of_range;
  if (RBIGINT_SIGN(m) < 0) return -(int64_t)u;
  return (int64_t)u;
}
#endif

uint64_t
mrb_bint_as_uint64(mrb_state *mrb, mrb_value x)
{
  struct RBigint *b = RBIGINT(x);
  struct RBigint *m = b;
  uint64_t u = 0;
  size_t len = digits(m);

  if (RBIGINT_SIGN(m) < 0 || len*sizeof(mp_limb) > sizeof(uint64_t)) {
    mrb_raise(mrb, E_RANGE_ERROR, "integer out of range");
  }
  for (size_t i=len-1; ; i--) {
    u <<= DIG_SIZE;
    u |= RBIGINT_ARY(m)[i];
    if (i==0) break;
  }
  return u;
}

/* unnormalize version of mrb_bint_add */
mrb_value
mrb_bint_add_n(mrb_state *mrb, mrb_value x, mrb_value y)
{
  y = mrb_as_bint(mrb, y);
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  mpz_add(mrb, b3, b, b2);
  return mrb_obj_value(b3);
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
  y = mrb_as_bint(mrb, y);
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  mpz_sub(mrb, b3, b, b2);
  return mrb_obj_value(b3);
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
  y = mrb_as_bint(mrb, y);
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  mpz_mul(mrb, b3, b, b2);
  return b3;
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
  y = mrb_as_bint(mrb, y);
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  if (zero_p(b2)) {
    mrb_int_zerodiv(mrb);
  }
  mpz_mdiv(mrb, b3, b, b2);
  return bint_norm(mrb, b3);
}

mrb_value
mrb_bint_add_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  struct RBigint *b = bint_new(mrb);
  struct RBigint z1, z2;

  mpz_init_set_int(mrb, &z1, x);
  mpz_init_set_int(mrb, &z2, y);
  mpz_add(mrb, b, &z1, &z2);
  mpz_clear(mrb, &z1);
  mpz_clear(mrb, &z2);
  return bint_norm(mrb, b);
}

mrb_value
mrb_bint_sub_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  struct RBigint *b = bint_new(mrb);
  struct RBigint z1, z2;

  mpz_init_set_int(mrb, &z1, x);
  mpz_init_set_int(mrb, &z2, y);
  mpz_sub(mrb, b, &z1, &z2);
  mpz_clear(mrb, &z1);
  mpz_clear(mrb, &z2);
  return bint_norm(mrb, b);
}

mrb_value
mrb_bint_mul_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  struct RBigint *b = bint_new(mrb);
  struct RBigint z1, z2;

  mpz_init_set_int(mrb, &z1, x);
  mpz_init_set_int(mrb, &z2, y);
  mpz_mul(mrb, b, &z1, &z2);
  mpz_clear(mrb, &z1);
  mpz_clear(mrb, &z2);
  return bint_norm(mrb, b);
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
  y = mrb_as_bint(mrb, y);
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  if (zero_p(b2)) {
    mrb_int_zerodiv(mrb);
  }
  mpz_mmod(mrb, b3, b, b2);
  return bint_norm(mrb, b3);
}

mrb_value
mrb_bint_rem(mrb_state *mrb, mrb_value x, mrb_value y)
{
  /* called from mrbgems/mruby-numeric-ext/src/numeric_ext.c */
  /* y should not be float */
  if (mrb_integer_p(y) && mrb_integer(y) == 0) {
    mrb_int_zerodiv(mrb);
  }
  y = mrb_as_bint(mrb, y);
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  if (zero_p(b2)) {
    mrb_int_zerodiv(mrb);
  }
  mpz_mod(mrb, b3, b, b2);
  return bint_norm(mrb, b3);
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
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  struct RBigint *b4 = bint_new(mrb);
  if (zero_p(b2)) {
    mrb_int_zerodiv(mrb);
  }
  mpz_mdivmod(mrb, b3, b4, b, b2);
  x = bint_norm(mrb, b3);
  y = bint_norm(mrb, b4);
  return mrb_assoc_new(mrb, x, y);
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
  struct RBigint *b = RBIGINT(x);
  if (!mrb_bigint_p(y)) {
    if (!mrb_integer_p(y)) return -2; /* type mismatch */

    mrb_int i1, i2 = mrb_integer(y);
    if (mpz_get_int(b, &i1)) {
      if (i1 == i2) return 0;
      if (i1 > i2) return 1;
      return -1;
    }
    if (RBIGINT_SIGN(b) > 0) return 1;
    return -1;
  }
  struct RBigint *b2 = RBIGINT(y);
  return mpz_cmp(mrb, b, b2);
}

mrb_value
mrb_bint_pow(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct RBigint *b = RBIGINT(x);
  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
    {
      struct RBigint *b3 = bint_new(mrb);
      mpz_pow(mrb, b3, b, mrb_integer(y));
      return mrb_obj_value(b3);
    }
  case MRB_TT_BIGINT:
    mrb_raise(mrb, E_TYPE_ERROR, "too big power");
  default:
    mrb_raisef(mrb, E_TYPE_ERROR, "%Y cannot be convert to integer", y);
  }
  return mrb_nil_value();
}

mrb_value
mrb_bint_powm(mrb_state *mrb, mrb_value x, mrb_value exp, mrb_value mod)
{
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2, *b3;

  if (mrb_bigint_p(mod)) {
    b2 = RBIGINT(mod);
    if (zero_p(b2)) mrb_int_zerodiv(mrb);
  }
  else {
    mrb_int m = mrb_integer(mod);
    if (m == 0) mrb_int_zerodiv(mrb);
    b2 = bint_new_int(mrb, m);
  }
  b3 = bint_new(mrb);
  if (mrb_bigint_p(exp)) {
    struct RBigint *be = RBIGINT(exp);
    if (RBIGINT_SIGN(be) < 0) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "int.pow(n,m): n must be positive");
    }
    mpz_powm(mrb, b3, b, be, b2);
  }
  else {
    mrb_int e = mrb_integer(exp);
    if (e < 0) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "int.pow(n,m): n must be positive");
    }
    mpz_powm_i(mrb, b3, b, e, b2);
  }
  return bint_norm(mrb, b3);
}

mrb_value
mrb_bint_to_s(mrb_state *mrb, mrb_value x, mrb_int base)
{
  struct RBigint *b = RBIGINT(x);

  if (zero_p(b) || RBIGINT_SIZE(b) == 0)
    return mrb_str_new_lit(mrb, "0");

  size_t len = mpz_sizeinbase(b, (int)base);
  if (MRB_INT_MAX-2 < len) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "too long string from Integer");
  }
  mrb_value str = mrb_str_new(mrb, NULL, len+2);
  mpz_get_str(mrb, RSTRING_PTR(str), len, base, b);
  RSTR_SET_LEN(RSTRING(str), strlen(RSTRING_PTR(str)));
  return str;
}

mrb_value
mrb_bint_and(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct RBigint *b1 = RBIGINT(x);

  if (mrb_integer_p(y)) {
    mrb_int z = mrb_integer(y);
    if (z == 0) return mrb_fixnum_value(0);
    if (z > 0 && (mp_dbl_limb)z < DIG_BASE) {
      z &= RBIGINT_ARY(b1)[0];
      return mrb_int_value(mrb, z);
    }
    if (z == -1) return x;
    if (z < 0 && (mp_dbl_limb)-z < DIG_BASE) {
      struct RBigint *b3 = bint_new(mrb);
      mpz_set(mrb, b3, b1);
      RBIGINT_ARY(b3)[0] &= (mp_limb)z;
      return bint_norm(mrb, b3);
    }
  }
  y = mrb_as_bint(mrb, y);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  mpz_and(mrb, b3, b1, b2);
  return bint_norm(mrb, b3);
}

mrb_value
mrb_bint_or(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct RBigint *b1 = RBIGINT(x);

  if (mrb_integer_p(y)) {
    mrb_int z = mrb_integer(y);
    if (z == 0) return x;
    if (z == -1) return y;
    if (z > 0 && (mp_dbl_limb)z < DIG_BASE) {
      z |= RBIGINT_ARY(b1)[0];
      return mrb_int_value(mrb, z);
    }
    if (z < 0 && (mp_dbl_limb)-z < DIG_BASE) {
      struct RBigint *b3 = bint_new(mrb);
      mpz_set(mrb, b3, b1);
      RBIGINT_ARY(b3)[0] |= (mp_limb)z;
      return bint_norm(mrb, b3);
    }
  }

  y = mrb_as_bint(mrb, y);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  mpz_or(mrb, b3, b1, b2);
  return bint_norm(mrb, b3);
}

mrb_value
mrb_bint_neg(mrb_state *mrb, mrb_value x)
{
  struct RBigint *b1 = RBIGINT(x);
  struct RBigint *b2 = bint_new(mrb);

  mpz_neg(mrb, b2, b1);
  /* no normalization */
  return mrb_obj_value(b2);
}

mrb_value
mrb_bint_xor(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct RBigint *b1 = RBIGINT(x);

  if (mrb_integer_p(y)) {
    mrb_int z = mrb_integer(y);
    if (z == 0) return x;
    // if (z == -1) return ;
    if (0 < z && (mp_dbl_limb)z < DIG_BASE) {
      z ^= RBIGINT_ARY(b1)[0];
      return mrb_int_value(mrb, z);
    }
  }
  y = mrb_as_bint(mrb, y);
  struct RBigint *b2 = RBIGINT(y);
  struct RBigint *b3 = bint_new(mrb);
  mpz_xor(mrb, b3, b1, b2);
  return bint_norm(mrb, b3);
}

mrb_value
mrb_bint_rev(mrb_state *mrb, mrb_value x)
{
  struct RBigint *b1 = RBIGINT(x);
  struct RBigint *b2 = bint_new(mrb);

  mpz_neg(mrb, b2, b1);
  mpz_sub_int(mrb, b2, b2, 1);
  return bint_norm(mrb, b2);
}

mrb_value
mrb_bint_lshift(mrb_state *mrb, mrb_value x, mrb_int width)
{
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = bint_new(mrb);
  if (width < 0) {
    mpz_div_2exp(mrb, b2, b, -width);
  }
  else {
    mpz_mul_2exp(mrb, b2, b, width);
  }
  return bint_norm(mrb, b2);
}

mrb_value
mrb_bint_rshift(mrb_state *mrb, mrb_value x, mrb_int width)
{
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = bint_new(mrb);
  if (width < 0) {
    mpz_mul_2exp(mrb, b2, b, -width);
  }
  else {
    mpz_div_2exp(mrb, b2, b, width);
  }
  return bint_norm(mrb, b2);
}

void
mrb_bint_copy(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = RBIGINT(y);
  mpz_init_set(mrb, b, b2);
}

size_t
mrb_bint_memsize(mrb_value x)
{
  struct RBigint *b = RBIGINT(x);
  return RBIGINT_EMBED_P(b) ? 0 : RBIGINT_SIZE(b) * sizeof(mp_limb);
}

mrb_value
mrb_bint_hash(mrb_state *mrb, mrb_value x)
{
  struct RBigint *b = RBIGINT(x);
  short sign = RBIGINT_SIGN(b);
  uint32_t hash = mrb_byte_hash((uint8_t*)RBIGINT_ARY(b), RBIGINT_SIZE(b)*sizeof(mp_limb));
  hash = mrb_byte_hash_step((uint8_t*)&sign, sizeof(sign), hash);
  return mrb_int_value(mrb, hash);
}

/* to be used only from mruby-sprintf */
mrb_value
mrb_bint_2comp(mrb_state *mrb, mrb_value x)
{
  struct RBigint *b = RBIGINT(x);
  struct RBigint *b2 = bint_new(mrb);

  mrb_assert(RBIGINT_SIGN(b) < 0);
  size_t size = RBIGINT_SIZE(b);
  mpz_realloc(mrb, b2, size);
  mp_limb *ds = RBIGINT_ARY(b);
  mp_limb *dd = RBIGINT_ARY(b2);
  char carry = 1;
  for (size_t i=0; i<size; i++) {
    mp_limb xv = ds[i];
    make_2comp(xv, carry);
    dd[i] = xv;
  }
  RBIGINT_SET_SIGN(b2, 1);
  return mrb_obj_value(b2);
}

#ifdef MRB_USE_RATIONAL
void
mrb_bint_reduce(mrb_state *mrb, mrb_value *xp, mrb_value *yp)
{
  struct RBigint *b1 = RBIGINT(*xp);
  struct RBigint *b2 = RBIGINT(*yp);
  struct RBigint r; mpz_init(mrb, &r);

  mpz_gcd(mrb, &r, b1, b2);

  struct RBigint *b3 = bint_new(mrb);
  struct RBigint *b4 = bint_new(mrb);
  mpz_mdiv(mrb, b3, b1, &r);
  mpz_mdiv(mrb, b4, b2, &r);

  mpz_clear(mrb, &r);
  *xp = mrb_obj_value(b3);
  *yp = mrb_obj_value(b4);
}
#endif
