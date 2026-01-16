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
#include <mruby/error.h>
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

#ifndef MRB_BIGINT_POOL_SIZE
#define MRB_BIGINT_POOL_SIZE 512  /* 2KB on 32-bit, 4KB on 64-bit */
#endif

/* Scoped Memory Pool Infrastructure */
#if MRB_BIGINT_POOL_SIZE == 0
#define mpz_ctx_t mrb_state
#define MPZ_MRB(ctx) (ctx)
#define MPZ_HAS_POOL(ctx) (0)
#define MPZ_CTX_INIT(mrb_ptr, ctx, pool_ptr) mrb_state *ctx = (mrb_ptr);
#define pool_save(ctx) 0
#define pool_restore(ctx, state) (void)state
#define pool_alloc(pool, limbs) NULL
#else
typedef struct mpz_pool {
  mp_limb data[MRB_BIGINT_POOL_SIZE];
  size_t used;
} mpz_pool_t;

/* MPZ Context Architecture - unified parameter for mrb_state and optional pool */
typedef struct mpz_context {
  mrb_state *mrb;
  mpz_pool_t *pool;  /* NULL for heap-only operations */
} mpz_ctx_t;

/* Convenience macros for context creation */
#define MPZ_CTX_INIT(mrb_ptr, ctx, pool_ptr) \
  mpz_pool_t pool ## _storage = {{0}};\
  mpz_pool_t *pool_ptr = &pool ## _storage;\
  mpz_ctx_t ctx ## _struct = ((mpz_ctx_t){.mrb = (mrb_ptr), .pool = (pool_ptr)}); \
  mpz_ctx_t *ctx = &(ctx ## _struct);

/* Access macros for readability */
#define MPZ_MRB(ctx) ((ctx)->mrb)
#define MPZ_POOL(ctx) ((ctx)->pool)
#define MPZ_HAS_POOL(ctx) ((ctx)->pool != NULL)

static size_t
pool_save(mpz_ctx_t *ctx)
{
  mpz_pool_t *pool = MPZ_POOL(ctx);
  return pool ? pool->used : 0;
}

static void
pool_restore(mpz_ctx_t *ctx, size_t state)
{
  mpz_pool_t *pool = MPZ_POOL(ctx);
  if (pool) {
    pool->used = state;
  }
}

/* Forward declarations */
static void mpz_mul_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e);
static void mpz_div_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e);
static void mpz_mod_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e);
static void mpz_set_int(mpz_ctx_t *ctx, mpz_t *y, mrb_int v);
static void mpz_mul(mpz_ctx_t *ctx, mpz_t *ww, mpz_t *u, mpz_t *v);

static mp_limb*
pool_alloc(mpz_pool_t *pool, size_t limbs)
{
  if (!pool || pool->used + limbs > MRB_BIGINT_POOL_SIZE) {
    return NULL;  /* Force fallback to heap */
  }

  mp_limb *ptr = &pool->data[pool->used];
  pool->used += limbs;
  return ptr;
}
#endif

/* Zero n limbs at p */
static inline void
mpn_zero(mp_limb *p, size_t n)
{
  memset(p, 0, n * sizeof(mp_limb));
}

static void
mpz_init(mpz_ctx_t *ctx, mpz_t *s)
{
  s->p = NULL;
  s->sn = 0;
  s->sz = 0;
}

/* Heap-preferred allocation */
static void
mpz_init_heap(mpz_ctx_t *ctx, mpz_t *s, size_t hint)
{
  s->sn = 0;
  if (hint > 0) {
    /* Check for overflow in size calculation (same check as mpz_realloc) */
    if (hint > SIZE_MAX / sizeof(mp_limb)) {
      mrb_state *mrb = MPZ_MRB(ctx);
      mrb_raise(mrb, E_RUNTIME_ERROR, "bigint size too large");
    }
    s->p = (mp_limb*)mrb_malloc(MPZ_MRB(ctx), hint * sizeof(mp_limb));
    mpn_zero(s->p, hint);
    s->sz = hint;
  }
  else {
    s->p = NULL;  /* Lazy allocation via mpz_realloc later */
    s->sz = 0;
  }
}

#if MRB_BIGINT_POOL_SIZE > 0
/* Pool-preferred allocation (future: mpz_init_temp) */
static void
mpz_init_temp(mpz_ctx_t *ctx, mpz_t *s, size_t hint)
{
  s->sn = 0;


  if (hint > 0 && MPZ_HAS_POOL(ctx)) {
    mp_limb *pool_ptr = pool_alloc(MPZ_POOL(ctx), hint);
    if (pool_ptr) {
      s->p = pool_ptr;
      s->sz = hint;
      return;
    }
  }
  /* Fallback to heap allocation */
  mpz_init_heap(ctx, s, hint);
}
#else
#define mpz_init_temp(ctx, s, hint) mpz_init_heap(ctx, s, hint)
#endif

/* Check if mpz_t uses pool memory */
#if MRB_BIGINT_POOL_SIZE > 0
static int
is_pool_memory(mpz_t *z, mpz_pool_t *pool)
{
  if (!pool || !z->p) return 0;
  uintptr_t ptr_addr = (uintptr_t)z->p;
  uintptr_t pool_start = (uintptr_t)pool->data;
  uintptr_t pool_end = pool_start + sizeof(pool->data);
  return ptr_addr >= pool_start && ptr_addr < pool_end;
}
#endif

static void
mpz_realloc(mpz_ctx_t *ctx, mpz_t *x, size_t size)
{
  if (x->sz < size) {
    /* Check for overflow in size calculation */
    if (size > SIZE_MAX / sizeof(mp_limb)) {
      mrb_state *mrb = MPZ_MRB(ctx);
      mrb_raise(mrb, E_RUNTIME_ERROR, "bigint size too large");
    }

    size_t old_sz = x->sz;

#if MRB_BIGINT_POOL_SIZE > 0
    /* Pool memory cannot be reallocated - must use heap */
    if (MPZ_HAS_POOL(ctx) && is_pool_memory(x, MPZ_POOL(ctx))) {
      /* Allocate new heap memory and copy from pool */
      mp_limb *new_p = (mp_limb*)mrb_malloc(MPZ_MRB(ctx), size * sizeof(mp_limb));
      if (x->p) {
        memcpy(new_p, x->p, old_sz * sizeof(mp_limb));
      }
      x->p = new_p;
    }
    else {
#endif
      /* Regular heap reallocation */
      x->p = (mp_limb*)mrb_realloc(MPZ_MRB(ctx), x->p, size * sizeof(mp_limb));
#if MRB_BIGINT_POOL_SIZE > 0
    }
#endif

    /* Zero-initialize new limbs */
    mpn_zero(x->p + old_sz, size - old_sz);
    x->sz = size;
  }
}

static void
mpz_set(mpz_ctx_t *ctx, mpz_t *y, mpz_t *x)
{
  size_t i, k = x->sz;

  mpz_realloc(ctx, y, k);
  for (i=0;i < k; i++)
    y->p[i] = x->p[i];

  y->sz = k;
  y->sn = x->sn;
}

static void
mpz_init_set(mpz_ctx_t *ctx, mpz_t *s, mpz_t *t)
{
  mpz_init(ctx, s);
  mpz_set(ctx, s, t);
}

static void
mpz_set_int(mpz_ctx_t *ctx, mpz_t *y, mrb_int v)
{
  mrb_uint u;

  if (v == 0) {
    y->sn = 0;
    y->sz = 0;
    return;
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
    mpz_realloc(ctx, y, 2);
    y->p[1] = (mp_limb)HIGH(u);
    y->p[0] = (mp_limb)LOW(u);
    y->sz = 2;
    return;
  }
#endif
  mpz_realloc(ctx, y, 1);
  y->p[0] = (mp_limb)u;
  y->sz = 1;
}


static void
mpz_set_uint64(mpz_ctx_t *ctx, mpz_t *y, uint64_t u)
{
  size_t len = 0;

  for (uint64_t u0=u; u0; u0>>=DIG_SIZE,len++)
    ;
  y->sn = (u != 0);
  mpz_realloc(ctx, y, len);
  y->sz = len;
  for (size_t i=0; i<len; i++) {
    y->p[i] = (mp_limb)LOW(u);
    u >>= DIG_SIZE;
  }
}

#ifdef MRB_INT32
static void
mpz_set_int64(mpz_ctx_t *ctx, mpz_t *y, int64_t v)
{
  uint64_t u;

  if (v < 0) {
    if (v == INT64_MIN) u = v;
    else u = -v;
  }
  else {
    u = v;
  }
  mpz_set_uint64(ctx, y, u);
  if (v < 0) {
    y->sn = -1;
  }
}
#endif

static void
mpz_init_set_int(mpz_ctx_t *ctx, mpz_t *y, mrb_int v)
{
  mpz_init(ctx, y);
  mpz_set_int(ctx, y, v);
}

static void
mpz_clear(mpz_ctx_t *ctx, mpz_t *s)
{
  if (s->p) {
#if MRB_BIGINT_POOL_SIZE > 0
    if (MPZ_HAS_POOL(ctx) && is_pool_memory(s, MPZ_POOL(ctx))) {
      /* Pool memory - don't free, just mark as unused */
    }
    else {
      mrb_free(MPZ_MRB(ctx), s->p);
    }
#else
    mrb_free(MPZ_MRB(ctx), s->p);
#endif
    s->p = NULL;
  }
  s->sn = 0;
  s->sz = 0;
}

static void
mpz_move(mpz_ctx_t *ctx, mpz_t *y, mpz_t *x)
{
#if MRB_BIGINT_POOL_SIZE > 0
  if (MPZ_HAS_POOL(ctx) && is_pool_memory(x, MPZ_POOL(ctx))) {
    /* Source is pool memory - use deep copy instead of pointer transfer */
    mpz_set(ctx, y, x);
    mpz_clear(ctx, x);
    return;
  }
#endif
  /* Normal move: transfer ownership */
  mpz_clear(ctx, y);
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
/* Core addition algorithm for unsigned operands */
/* Note: mpn_add/mpn_sub are defined later in the file, forward declare here */
static mp_limb mpn_add(mp_limb*, const mp_limb*, size_t, const mp_limb*, size_t);
static mp_limb mpn_sub(mp_limb*, const mp_limb*, size_t, const mp_limb*, size_t);

static void
uadd(mpz_t *z, mpz_t *x, mpz_t *y)
{
  mp_limb carry;
  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;

  /* Ensure larger array is first argument to mpn_add */
  if (x->sz >= y->sz) {
    carry = mpn_add(z->p, x->p, x->sz, y->p, y->sz);
  }
  else {
    carry = mpn_add(z->p, y->p, y->sz, x->p, x->sz);
  }

  /* Store final carry */
  z->p[max_sz] = carry;
}

/* z = y - x, ignoring sign */
/* precondition: abs(y) >= abs(x) */
/* Core subtraction algorithm for unsigned operands */
static void
usub(mpz_t *z, mpz_t *y, mpz_t *x)
{
  /* y->sz >= x->sz is guaranteed by precondition */
  mpn_sub(z->p, y->p, y->sz, x->p, x->sz);

  /* Normalize result size */
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
mpz_add(mpz_ctx_t *ctx, mpz_t *zz, mpz_t *x, mpz_t *y)
{
  if (zero_p(x)) {
    mpz_set(ctx, zz, y);
    trim(zz);
    return;
  }
  if (zero_p(y)) {
    mpz_set(ctx, zz, x);
    trim(zz);
    return;
  }

  /* Fast path: single-limb + multi-limb */
  if (y->sz == 1 && x->sz > 1) {
    mp_limb y_limb = y->p[0];
    mpz_t z;
    mpz_init_heap(ctx, &z, x->sz + 1);

    if ((x->sn > 0 && y->sn > 0) || (x->sn < 0 && y->sn < 0)) {
      /* Same signs: addition */
      mp_dbl_limb carry = y_limb;
      carry += x->p[0];
      z.p[0] = (mp_limb)carry;
      carry >>= DIG_SIZE;

      /* Propagate carry through remaining limbs */
      for (size_t i = 1; i < x->sz; i++) {
        carry += x->p[i];
        z.p[i] = (mp_limb)carry;
        carry >>= DIG_SIZE;
      }
      z.p[x->sz] = (mp_limb)carry;
      z.sn = x->sn;
    }
    else {
      /* Different signs: subtraction */
      if (x->sz == 1 && y_limb == x->p[0]) {
        /* Equal magnitude: result is zero */
        zero(&z);
      }
      else if (x->sz == 1 && x->p[0] > y_limb) {
        /* |x| > |y|: result has sign of x */
        z.p[0] = x->p[0] - y_limb;
        z.p[1] = 0;
        z.sn = x->sn;
      }
      else {
        /* |x| > |y|: subtract y from x */
        mp_dbl_limb borrow = y_limb;
        if (x->p[0] >= borrow) {
          z.p[0] = x->p[0] - (mp_limb)borrow;
          borrow = 0;
        }
        else {
          z.p[0] = (mp_limb)(((mp_dbl_limb)1 << DIG_SIZE) + x->p[0] - (mp_limb)borrow);
          borrow = 1;
        }

        /* Propagate borrow through remaining limbs */
        for (size_t i = 1; i < x->sz; i++) {
          if (x->p[i] >= borrow) {
            z.p[i] = x->p[i] - (mp_limb)borrow;
            borrow = 0;
          }
          else {
            z.p[i] = (mp_limb)(((mp_dbl_limb)1 << DIG_SIZE) + x->p[i] - (mp_limb)borrow);
            borrow = 1;
          }
        }
        z.sn = x->sn;
      }
    }
    trim(&z);
    mpz_move(ctx, zz, &z);
    return;
  }

  if (x->sz == 1 && y->sz > 1) {
    /* Swap and use the same fast path */
    mpz_add(ctx, zz, y, x);
    return;
  }

  mpz_t z;
  size_t estimated_size = ((x->sz > y->sz) ? x->sz : y->sz) + 1;
  mpz_init_heap(ctx, &z, estimated_size);

  if (x->sn > 0 && y->sn > 0) {
    uadd(&z, x, y);
    z.sn = 1;
  }
  else if (x->sn < 0 && y->sn < 0) {
    uadd(&z, x, y);
    z.sn = -1;
  }
  else {
    int mg;

    /* signs differ */
    if ((mg = ucmp(x,y)) == 0) {
      zero(&z);
    }
    else if (mg > 0) {  /* abs(y) < abs(x) */
      usub(&z, x, y);
      z.sn = (x->sn > 0 && y->sn < 0) ? 1 : (-1);
    }
    else { /* abs(y) > abs(x) */
      usub(&z, y, x);
      z.sn = (x->sn < 0 && y->sn > 0) ? 1 : (-1);
    }
  }
  trim(&z);
  mpz_move(ctx, zz, &z);
}

/* x += n                                              */
/*   ignores sign of x                                 */
/*   assumes n is positive and small (fits in mp_limb) */
static void
mpz_add_int(mpz_ctx_t *ctx, mpz_t *x, mrb_int n)
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
    mpz_realloc(ctx, x, x->sz + 1);
    x->p[x->sz-1] = (mp_limb)carry;
    x->sn = 1;
  }
  trim(x);
}

/* z = x - y */
static void
mpz_sub(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mpz_t *y)
{
  /* In-place optimization: z == x, both positive, x >= y */
  if (z == x && x->sn > 0 && y->sn > 0 && ucmp(x, y) >= 0) {
    mpn_sub(x->p, x->p, x->sz, y->p, y->sz);
    x->sz = digits(x);
    if (x->sz == 0) x->sn = 0;
    return;
  }

  /* General case: create view of y with negated sign and use mpz_add */
  mpz_t u;
  u.p = y->p;
  u.sz = y->sz;
  u.sn = -(y->sn);
  mpz_add(ctx, z, x, &u);
}

/* x -= n                                              */
/*   ignores sign of x                                 */
/*   assumes n is positive and small (fits in mp_limb) */
static void
mpz_sub_int(mpz_ctx_t *ctx, mpz_t *x, mrb_int n)
{
  // If n is zero, no operation is needed
  if (n == 0) return;

  // If x is zero, set x to n
  if (zero_p(x) || x->sz == 0) {
    mpz_set_int(ctx, x, n);
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

/* Multiply-and-add: rp[0..n-1] += s1p[0..n-1] * limb; return carry (high limb) */
static inline mp_limb
mpn_addmul_1(mp_limb *rp, const mp_limb *s1p, size_t n, mp_limb limb)
{
#if defined(__SIZEOF_INT128__) && (__SIZEOF_INT128__ == 16)
  /* Use 128-bit arithmetic with 8x unrolling for maximum efficiency */
  unsigned __int128 acc = 0;
  size_t i;

  /* 8x unrolled loop for large operands */
  for (i = 0; i + 8 <= n; i += 8) {
    acc += (unsigned __int128)rp[i] + (unsigned __int128)s1p[i] * (unsigned __int128)limb;
    rp[i] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+1] + (unsigned __int128)s1p[i+1] * (unsigned __int128)limb;
    rp[i+1] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+2] + (unsigned __int128)s1p[i+2] * (unsigned __int128)limb;
    rp[i+2] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+3] + (unsigned __int128)s1p[i+3] * (unsigned __int128)limb;
    rp[i+3] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+4] + (unsigned __int128)s1p[i+4] * (unsigned __int128)limb;
    rp[i+4] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+5] + (unsigned __int128)s1p[i+5] * (unsigned __int128)limb;
    rp[i+5] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+6] + (unsigned __int128)s1p[i+6] * (unsigned __int128)limb;
    rp[i+6] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+7] + (unsigned __int128)s1p[i+7] * (unsigned __int128)limb;
    rp[i+7] = (mp_limb)acc;
    acc >>= DIG_SIZE;
  }

  /* 4x unrolled loop for medium operands */
  for (; i + 4 <= n; i += 4) {
    acc += (unsigned __int128)rp[i] + (unsigned __int128)s1p[i] * (unsigned __int128)limb;
    rp[i] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+1] + (unsigned __int128)s1p[i+1] * (unsigned __int128)limb;
    rp[i+1] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+2] + (unsigned __int128)s1p[i+2] * (unsigned __int128)limb;
    rp[i+2] = (mp_limb)acc;
    acc >>= DIG_SIZE;

    acc += (unsigned __int128)rp[i+3] + (unsigned __int128)s1p[i+3] * (unsigned __int128)limb;
    rp[i+3] = (mp_limb)acc;
    acc >>= DIG_SIZE;
  }

  /* Handle remaining elements */
  for (; i < n; i++) {
    acc += (unsigned __int128)rp[i] + (unsigned __int128)s1p[i] * (unsigned __int128)limb;
    rp[i] = (mp_limb)acc;
    acc >>= DIG_SIZE;
  }

  return (mp_limb)acc;

#else
  /* Portable double-limb path with 4x unrolling */
  mp_dbl_limb acc = 0;
  size_t i;

  /* 4x unrolled loop for better performance */
  for (i = 0; i + 4 <= n; i += 4) {
    acc += (mp_dbl_limb)rp[i] + (mp_dbl_limb)s1p[i] * (mp_dbl_limb)limb;
    rp[i] = LOW(acc);
    acc = HIGH(acc);

    acc += (mp_dbl_limb)rp[i+1] + (mp_dbl_limb)s1p[i+1] * (mp_dbl_limb)limb;
    rp[i+1] = LOW(acc);
    acc = HIGH(acc);

    acc += (mp_dbl_limb)rp[i+2] + (mp_dbl_limb)s1p[i+2] * (mp_dbl_limb)limb;
    rp[i+2] = LOW(acc);
    acc = HIGH(acc);

    acc += (mp_dbl_limb)rp[i+3] + (mp_dbl_limb)s1p[i+3] * (mp_dbl_limb)limb;
    rp[i+3] = LOW(acc);
    acc = HIGH(acc);
  }

  /* Handle remaining elements */
  for (; i < n; i++) {
    acc += (mp_dbl_limb)rp[i] + (mp_dbl_limb)s1p[i] * (mp_dbl_limb)limb;
    rp[i] = LOW(acc);
    acc = HIGH(acc);
  }

  return (mp_limb)acc;
#endif
}

/* Multiply-and-subtract: rp[0..n-1] -= s1p[0..n-1] * limb; return borrow */
static inline mp_limb
mpn_submul_1(mp_limb *rp, const mp_limb *s1p, size_t n, mp_limb limb)
{
  mp_dbl_limb borrow = 0;
  for (size_t i = 0; i < n; i++) {
    mp_dbl_limb prod = (mp_dbl_limb)s1p[i] * (mp_dbl_limb)limb;
    mp_dbl_limb sub = (mp_dbl_limb)rp[i] - LOW(prod) - borrow;
    rp[i] = LOW(sub);
    /* Borrow is 1 if sub underflowed, plus HIGH(prod) */
    borrow = HIGH(prod) + (sub >> (sizeof(mp_dbl_limb) * 8 - 1));
  }
  return (mp_limb)borrow;
}

/* Compare two same-length limb arrays: returns <0, 0, or >0 */
static inline int
mpn_cmp(const mp_limb *ap, const mp_limb *bp, size_t n)
{
  while (n-- > 0) {
    if (ap[n] != bp[n]) {
      return (ap[n] > bp[n]) ? 1 : -1;
    }
  }
  return 0;
}

/* w = u * v (optimized schoolbook using mpn_addmul_1) */
static void
mpz_mul_basic(mpz_ctx_t *ctx, mpz_t *ww, mpz_t *u, mpz_t *v)
{
  if (zero_p(u) || zero_p(v)) {
    zero(ww);
    return;
  }

  /* Ensure outer loop iterates over the shorter operand for better cache use */
  mpz_t *a, *b;
  if (v->sz > u->sz) {
    a = v; b = u;
  }
  else {
    a = u; b = v;
  }

  /* Fast path: single-limb * multi-limb */
  if (b->sz == 1) {
    mp_limb scalar = b->p[0];
    mpz_t w;
    mpz_init_heap(ctx, &w, a->sz + 1);
    mpn_zero(w.p, a->sz + 1);

    mp_limb carry = mpn_addmul_1(w.p, a->p, a->sz, scalar);
    w.p[a->sz] = carry;

    w.sn = a->sn * b->sn;
    trim(&w);
    mpz_move(ctx, ww, &w);
    return;
  }

  mpz_t w;
  mpz_init_heap(ctx, &w, a->sz + b->sz);
  mpn_zero(w.p, a->sz + b->sz);

  for (size_t j = 0; j < a->sz; j++) {
    mp_limb a_limb = a->p[j];
    if (a_limb == 0) continue;

    mp_limb carry = mpn_addmul_1(w.p + j, b->p, b->sz, a_limb);

    /* Properly handle carry propagation to avoid overflow */
    size_t k = j + b->sz;
    while (carry && k < a->sz + b->sz) {
      mp_dbl_limb sum = (mp_dbl_limb)w.p[k] + (mp_dbl_limb)carry;
      w.p[k] = LOW(sum);
      carry = HIGH(sum);
      k++;
    }
  }

  w.sn = a->sn * b->sn;
  trim(&w);
  mpz_move(ctx, ww, &w);
}

/* Allocation-free multiplication helper functions */

/* Copy limbs forward: dest[0..n-1] = src[0..n-1] */
static void
mpn_copyi(mp_limb *dest, const mp_limb *src, size_t n)
{
  if (n > 0) {
    memcpy(dest, src, n * sizeof(mp_limb));
  }
}

/* Add limbs at offset: dest[offset..offset+n-1] += src[0..n-1] */
static void
limb_add_at(mp_limb *dest, size_t dest_len, const mp_limb *src, size_t n, size_t offset)
{
  mp_limb carry = 0;
  size_t i = 0;
  for (i = 0; i < n; i++) {
    mp_dbl_limb sum = (mp_dbl_limb)dest[offset + i] + (mp_dbl_limb)src[i] + carry;
    dest[offset + i] = LOW(sum);
    carry = HIGH(sum);
  }
  /* Propagate final carry */
  i = offset + n;
  while (carry && i < dest_len) {
    mp_dbl_limb sum = (mp_dbl_limb)dest[i] + carry;
    dest[i] = LOW(sum);
    carry = HIGH(sum);
    i++;
  }
}

/* Basic multiplication for small operands */
static void
mpz_mul_basic_limbs(mp_limb *result, const mp_limb *x, size_t x_len,
                    const mp_limb *y, size_t y_len)
{
  mpn_zero(result, x_len + y_len);

  for (size_t i = 0; i < x_len; i++) {
    if (x[i] == 0) continue;
    mp_limb carry = mpn_addmul_1(result + i, y, y_len, x[i]);
    if (i + y_len < x_len + y_len) {
      result[i + y_len] += carry;
    }
  }
}

/*
 * Schoolbook squaring - exploits symmetry for ~1.5x speedup.
 *
 * For x = [x0, x1, x2, ...], x^2 has terms:
 *   - Diagonal: xi^2 (computed once)
 *   - Off-diagonal: 2*xi*xj for i<j (computed once, doubled)
 *
 * This reduces multiplications from n^2 to n(n+1)/2.
 *
 * Algorithm:
 *   1. Compute off-diagonal products xi*xj for i<j
 *   2. Double the off-diagonal sum
 *   3. Add diagonal terms xi^2
 */
static void
mpz_sqr_basic_limbs(mp_limb *result, const mp_limb *x, size_t n)
{
  size_t result_len = 2 * n;
  mpn_zero(result, result_len);

  /* Step 1: Compute off-diagonal terms xi * xj for i < j */
  for (size_t i = 0; i < n; i++) {
    mp_limb xi = x[i];
    if (xi == 0) continue;

    if (i + 1 < n) {
      /* Compute xi * x[i+1..n-1] and add at position 2*i+1 */
      mp_limb carry = mpn_addmul_1(result + 2*i + 1, x + i + 1, n - i - 1, xi);

      /* Propagate carry */
      size_t k = 2*i + 1 + (n - i - 1);
      while (carry && k < result_len) {
        mp_dbl_limb sum = (mp_dbl_limb)result[k] + carry;
        result[k] = LOW(sum);
        carry = HIGH(sum);
        k++;
      }
    }
  }

  /* Step 2: Double the off-diagonal sum (shift left by 1) */
  mp_limb carry = 0;
  for (size_t i = 0; i < result_len; i++) {
    mp_dbl_limb val = ((mp_dbl_limb)result[i] << 1) | carry;
    result[i] = LOW(val);
    carry = HIGH(val);
  }

  /* Step 3: Add diagonal terms xi^2 */
  for (size_t i = 0; i < n; i++) {
    mp_limb xi = x[i];
    if (xi == 0) continue;

    mp_dbl_limb sq = (mp_dbl_limb)xi * xi;
    mp_dbl_limb acc = (mp_dbl_limb)result[2*i] + LOW(sq);
    result[2*i] = LOW(acc);
    acc = (acc >> DIG_SIZE) + HIGH(sq);

    for (size_t k = 2*i + 1; acc && k < result_len; k++) {
      acc += result[k];
      result[k] = LOW(acc);
      acc >>= DIG_SIZE;
    }
  }
}

/*
 * Karatsuba Multiplication
 *
 * Splits inputs into 2 parts: A = A1*B^half + A0, B = B1*B^half + B0
 * Computes: z0 = A0*B0, z2 = A1*B1, z1 = (A0+A1)*(B0+B1) - z0 - z2
 * Result: z2*B^(2*half) + z1*B^half + z0
 *
 * Complexity: O(n^1.585) - trades 4 multiplications for 3 plus additions
 */

#define KARATSUBA_THRESHOLD 32

static inline mrb_bool
should_use_karatsuba(size_t n)
{
  return n >= KARATSUBA_THRESHOLD;
}

/* Calculate scratch space needed for Karatsuba */
static size_t
karatsuba_scratch_size(size_t n)
{
  if (n < KARATSUBA_THRESHOLD) {
    return 0;
  }

  size_t half = (n + 1) / 2;

  /*
   * Per level storage:
   * - 2 evaluation results: (A0+A1), (B0+B1), each up to (half+1) limbs
   * - 3 products: z0, z1, z2, each up to 2*(half+1) limbs
   */
  size_t eval_len = half + 1;
  size_t prod_len = 2 * eval_len;

  size_t eval_size = 2 * eval_len;    /* 2 evaluation temps */
  size_t prod_size = 3 * prod_len;    /* 3 products */
  size_t current_level = eval_size + prod_size;

  /* Recursive scratch - sequential calls reuse same buffer */
  size_t sub_scratch = karatsuba_scratch_size(eval_len);

  return current_level + sub_scratch + 8;  /* +8 safety margin */
}

/* Forward declaration for recursive calls */
static void
mpz_mul_karatsuba_limbs(mp_limb *result,
                        const mp_limb *x, size_t x_len,
                        const mp_limb *y, size_t y_len,
                        mp_limb *scratch);

/*
 * Karatsuba multiplication on raw limb arrays.
 *
 * result must have space for x_len + y_len limbs.
 * scratch must have karatsuba_scratch_size(max(x_len, y_len)) limbs.
 */
static void
mpz_mul_karatsuba_limbs(mp_limb *result,
                        const mp_limb *x, size_t x_len,
                        const mp_limb *y, size_t y_len,
                        mp_limb *scratch)
{
  size_t min_len = (x_len < y_len) ? x_len : y_len;
  size_t max_len = (x_len > y_len) ? x_len : y_len;

  /* Base case - use schoolbook */
  if (!should_use_karatsuba(min_len)) {
    mpz_mul_basic_limbs(result, x, x_len, y, y_len);
    return;
  }

  /*
   * Split: x = x1*B^half + x0, y = y1*B^half + y0
   * where B = base^half
   */
  size_t half = (max_len + 1) / 2;

  /* Determine actual lengths of each part */
  size_t x0_len = (x_len > half) ? half : x_len;
  size_t x1_len = (x_len > half) ? x_len - half : 0;
  size_t y0_len = (y_len > half) ? half : y_len;
  size_t y1_len = (y_len > half) ? y_len - half : 0;

  const mp_limb *x0 = x;
  const mp_limb *x1 = x + half;
  const mp_limb *y0 = y;
  const mp_limb *y1 = y + half;

  /* Allocate scratch space */
  size_t eval_len = half + 1;
  size_t prod_len = 2 * eval_len;

  size_t offset = 0;
  mp_limb *sum_x = scratch + offset; offset += eval_len;  /* x0 + x1 */
  mp_limb *sum_y = scratch + offset; offset += eval_len;  /* y0 + y1 */
  mp_limb *z0 = scratch + offset; offset += prod_len;     /* x0 * y0 */
  mp_limb *z2 = scratch + offset; offset += prod_len;     /* x1 * y1 */
  mp_limb *z1 = scratch + offset; offset += prod_len;     /* (x0+x1)*(y0+y1) */
  mp_limb *recursive_scratch = scratch + offset;

  /* Compute sum_x = x0 + x1 */
  mpn_zero(sum_x, eval_len);
  mpn_copyi(sum_x, x0, x0_len);
  if (x1_len > 0) {
    mpn_add(sum_x, sum_x, eval_len, x1, x1_len);
  }
  size_t sum_x_len = eval_len;
  while (sum_x_len > 1 && sum_x[sum_x_len - 1] == 0) sum_x_len--;

  /* Compute sum_y = y0 + y1 */
  mpn_zero(sum_y, eval_len);
  mpn_copyi(sum_y, y0, y0_len);
  if (y1_len > 0) {
    mpn_add(sum_y, sum_y, eval_len, y1, y1_len);
  }
  size_t sum_y_len = eval_len;
  while (sum_y_len > 1 && sum_y[sum_y_len - 1] == 0) sum_y_len--;

  /* z0 = x0 * y0 */
  mpn_zero(z0, prod_len);
  if (x0_len > 0 && y0_len > 0) {
    mpz_mul_karatsuba_limbs(z0, x0, x0_len, y0, y0_len, recursive_scratch);
  }

  /* z2 = x1 * y1 */
  mpn_zero(z2, prod_len);
  if (x1_len > 0 && y1_len > 0) {
    mpz_mul_karatsuba_limbs(z2, x1, x1_len, y1, y1_len, recursive_scratch);
  }

  /* z1 = (x0 + x1) * (y0 + y1) */
  mpn_zero(z1, prod_len);
  mpz_mul_karatsuba_limbs(z1, sum_x, sum_x_len, sum_y, sum_y_len, recursive_scratch);

  /* z1 = z1 - z0 - z2 */
  mpn_sub(z1, z1, prod_len, z0, prod_len);
  mpn_sub(z1, z1, prod_len, z2, prod_len);

  /*
   * Combine: result = z2*B^(2*half) + z1*B^half + z0
   */
  size_t result_len = x_len + y_len;
  mpn_zero(result, result_len);

  /* Add z0 at position 0 */
  size_t z0_actual_len = prod_len;
  while (z0_actual_len > 0 && z0[z0_actual_len - 1] == 0) z0_actual_len--;
  if (z0_actual_len > 0) {
    mpn_copyi(result, z0, z0_actual_len);
  }

  /* Add z1 at position half */
  size_t z1_actual_len = prod_len;
  while (z1_actual_len > 0 && z1[z1_actual_len - 1] == 0) z1_actual_len--;
  if (z1_actual_len > 0) {
    limb_add_at(result, result_len, z1, z1_actual_len, half);
  }

  /* Add z2 at position 2*half */
  size_t z2_actual_len = prod_len;
  while (z2_actual_len > 0 && z2[z2_actual_len - 1] == 0) z2_actual_len--;
  if (z2_actual_len > 0 && 2 * half < result_len) {
    limb_add_at(result, result_len, z2, z2_actual_len, 2 * half);
  }
}

/*
 * Toom-3 (Toom-Cook 3-way) Multiplication
 *
 * Splits inputs into 3 parts and evaluates at 5 points: 0, 1, -1, 2, ∞
 * Complexity: O(n^1.465) vs Karatsuba's O(n^1.585)
 *
 * For A = a2*B^2 + a1*B + a0 and similarly B:
 *   - Evaluate polynomials at 5 points
 *   - Multiply at each point (5 recursive calls)
 *   - Interpolate to recover result coefficients
 */

#define TOOM3_THRESHOLD 100

static inline mrb_bool
should_use_toom3(size_t n)
{
  return n >= TOOM3_THRESHOLD;
}

/* Calculate scratch space needed for Toom-3 (including Karatsuba at base) */
static size_t
toom3_scratch_size(size_t n)
{
  if (!should_use_toom3(n)) {
    /* For Karatsuba range, return Karatsuba scratch size */
    if (should_use_karatsuba(n)) {
      return karatsuba_scratch_size(n);
    }
    return 0;
  }

  size_t third = n / 3;

  /*
   * Per level storage:
   * - 6 evaluation results: v1_x, v1_y, vm1_x, vm1_y, v2_x, v2_y
   *   Each up to (third + 3) limbs for carries
   * - 5 product results: w0, w1, wm1, w2, winf
   *   Each up to 2*(third + 3) + 16 limbs (recursive Toom-3 needs extra margin)
   * - 4 interpolation temps: t4, t5, t6, r2_tmp
   *   Each up to 2*(third + 3) + 16 limbs (in recursive_scratch area)
   */
  size_t eval_len = third + 3;
  size_t prod_len = 2 * eval_len + 16;

  size_t eval_size = 6 * eval_len;           /* 6 evaluation temps */
  size_t prod_size = 5 * prod_len;           /* 5 products */
  size_t interp_size = 4 * prod_len;         /* 4 interpolation temps */
  size_t current_level = eval_size + prod_size;

  /* Recursive scratch (sequential calls, reuse same buffer) */
  /* Must be at least interp_size for interpolation temps */
  size_t sub_n = eval_len;  /* largest sub-multiplication size */
  size_t sub_scratch = toom3_scratch_size(sub_n);
  if (sub_scratch < interp_size) {
    sub_scratch = interp_size;
  }

  return current_level + sub_scratch + 8;  /* +8 safety margin */
}

/*
 * Divide limb array by 2 (right shift by 1 bit).
 * Returns the shifted-out bit (0 or 1).
 */
static mp_limb
mpn_rshift1(mp_limb *rp, const mp_limb *ap, size_t n)
{
  mp_limb carry = 0;
  for (size_t i = n; i > 0; i--) {
    mp_limb a = ap[i-1];
    rp[i-1] = (a >> 1) | (carry << (DIG_SIZE - 1));
    carry = a & 1;
  }
  return carry;
}

/*
 * Divide limb array by 3 (exact division).
 * Precondition: the value is divisible by 3.
 */
static void
mpn_divexact_3(mp_limb *rp, const mp_limb *ap, size_t n)
{
  /*
   * Division by 3 using multiplicative inverse.
   * For mod 2^32: inverse of 3 is 0xAAAAAAAB
   * For mod 2^64: inverse of 3 is 0xAAAAAAAAAAAAAAAB
   * x / 3 = x * inverse (mod 2^bits), with borrow propagation
   */
#if DIG_SIZE == 32
  const mp_limb inv3 = 0xAAAAAAABUL;
#else
  const mp_limb inv3 = 0xAAAAAAAAAAAAAAABULL;
#endif

  mp_limb borrow = 0;
  for (size_t i = 0; i < n; i++) {
    mp_limb old_a = ap[i];  /* save before potential in-place overwrite */
    mp_limb a = old_a - borrow;
    mp_limb q = a * inv3;
    rp[i] = q;
    /* borrow = (q * 3 > a) ? ceil((q*3 - a) / 2^DIG_SIZE) : 0 */
    /* Simplified: borrow for next iteration */
    mp_dbl_limb prod = (mp_dbl_limb)q * 3;
    borrow = (mp_limb)(prod >> DIG_SIZE);
    if ((mp_limb)prod > old_a) borrow++;  /* use saved value for in-place safety */
  }
}

/*
 * Add with carry, handling different sizes.
 * rp[0..rn-1] = ap[0..an-1] + bp[0..bn-1]
 * rn must be >= max(an, bn)
 * Returns final carry.
 */
static mp_limb
mpn_add_var(mp_limb *rp, const mp_limb *ap, size_t an,
            const mp_limb *bp, size_t bn, size_t rn)
{
  mp_limb carry = 0;
  size_t i;

  /* Add common part */
  size_t min_n = (an < bn) ? an : bn;
  for (i = 0; i < min_n; i++) {
    mp_dbl_limb sum = (mp_dbl_limb)ap[i] + bp[i] + carry;
    rp[i] = LOW(sum);
    carry = HIGH(sum);
  }

  /* Copy and propagate carry through longer operand */
  if (an > bn) {
    for (; i < an; i++) {
      mp_dbl_limb sum = (mp_dbl_limb)ap[i] + carry;
      rp[i] = LOW(sum);
      carry = HIGH(sum);
    }
  } else {
    for (; i < bn; i++) {
      mp_dbl_limb sum = (mp_dbl_limb)bp[i] + carry;
      rp[i] = LOW(sum);
      carry = HIGH(sum);
    }
  }

  /* Zero-fill remainder and propagate final carry */
  for (; i < rn; i++) {
    rp[i] = carry;
    carry = 0;
  }

  return carry;
}

/*
 * Subtract with borrow, result may be negative.
 * rp[0..n-1] = ap[0..n-1] - bp[0..n-1]
 * Returns 1 if result is negative (borrow out), 0 otherwise.
 */
static mp_limb
mpn_sub_var(mp_limb *rp, const mp_limb *ap, size_t an,
            const mp_limb *bp, size_t bn, size_t n)
{
  mp_dbl_limb_signed borrow = 0;
  size_t i;

  for (i = 0; i < n; i++) {
    mp_limb a = (i < an) ? ap[i] : 0;
    mp_limb b = (i < bn) ? bp[i] : 0;
    borrow += (mp_dbl_limb_signed)a - (mp_dbl_limb_signed)b;
    rp[i] = LOW(borrow);
    borrow = HIGH(borrow);
  }

  return (borrow < 0) ? 1 : 0;
}

/*
 * Negate a limb array (two's complement).
 * rp[0..n-1] = -ap[0..n-1]
 */
static void
mpn_neg(mp_limb *rp, const mp_limb *ap, size_t n)
{
  mp_limb carry = 1;
  for (size_t i = 0; i < n; i++) {
    mp_dbl_limb sum = (mp_dbl_limb)(~ap[i]) + carry;
    rp[i] = LOW(sum);
    carry = HIGH(sum);
  }
}

/* Pool-aware Toom-3 multiplication */
static void
mpz_mul_toom3(mpz_ctx_t *ctx, mp_limb *result,
              const mp_limb *x, size_t x_len,
              const mp_limb *y, size_t y_len,
              mp_limb *scratch)
{
  /*
   * Base case - use Karatsuba or schoolbook.
   * Toom-3 requires both operands to be large enough to avoid
   * buffer overflow when writing at offset 4*third.
   */
  size_t min_len = (x_len < y_len) ? x_len : y_len;
  size_t n = (x_len > y_len) ? x_len : y_len;
  if (!should_use_toom3(min_len)) {
    if (should_use_karatsuba(min_len)) {
      mpz_mul_karatsuba_limbs(result, x, x_len, y, y_len, scratch);
    }
    else {
      mpz_mul_basic_limbs(result, x, x_len, y, y_len);
    }
    return;
  }

  /*
   * Split: x = x2*B^2 + x1*B + x0, y = y2*B^2 + y1*B + y0
   * where B = base^third
   */
  size_t third = n / 3;
  size_t x0_len = (x_len > third) ? third : x_len;
  size_t x1_len = (x_len > 2*third) ? third : ((x_len > third) ? x_len - third : 0);
  size_t x2_len = (x_len > 2*third) ? x_len - 2*third : 0;
  size_t y0_len = (y_len > third) ? third : y_len;
  size_t y1_len = (y_len > 2*third) ? third : ((y_len > third) ? y_len - third : 0);
  size_t y2_len = (y_len > 2*third) ? y_len - 2*third : 0;

  const mp_limb *x0 = x;
  const mp_limb *x1 = x + third;
  const mp_limb *x2 = x + 2*third;
  const mp_limb *y0 = y;
  const mp_limb *y1 = y + third;
  const mp_limb *y2 = y + 2*third;

  /* Allocate scratch space */
  size_t eval_len = third + 3;  /* max size after evaluation with carries */
  /*
   * Product buffers need extra space because recursive Toom-3 calls
   * write x_len + y_len + 16 limbs. With x_len, y_len <= eval_len,
   * maximum is 2*eval_len + 16.
   */
  size_t prod_len = 2 * eval_len + 16;

  size_t offset = 0;
  mp_limb *v1_x = scratch + offset; offset += eval_len;
  mp_limb *v1_y = scratch + offset; offset += eval_len;
  mp_limb *vm1_x = scratch + offset; offset += eval_len;
  mp_limb *vm1_y = scratch + offset; offset += eval_len;
  mp_limb *v2_x = scratch + offset; offset += eval_len;
  mp_limb *v2_y = scratch + offset; offset += eval_len;

  mp_limb *w0 = scratch + offset; offset += prod_len;
  mp_limb *w1 = scratch + offset; offset += prod_len;
  mp_limb *wm1 = scratch + offset; offset += prod_len;
  mp_limb *w2 = scratch + offset; offset += prod_len;
  mp_limb *winf = scratch + offset; offset += prod_len;

  mp_limb *recursive_scratch = scratch + offset;

  /*
   * Evaluation at 5 points:
   * v0 = x0, y0 (reuse input)
   * v1 = x0 + x1 + x2, y0 + y1 + y2
   * vm1 = x0 - x1 + x2, y0 - y1 + y2
   * v2 = x0 + 2*x1 + 4*x2, y0 + 2*y1 + 4*y2
   * vinf = x2, y2 (reuse input)
   */

  /* v1 = x0 + x1 + x2 */
  mpn_zero(v1_x, eval_len);
  mpn_copyi(v1_x, x0, x0_len);
  if (x1_len > 0) mpn_add(v1_x, v1_x, eval_len, x1, x1_len);
  if (x2_len > 0) mpn_add(v1_x, v1_x, eval_len, x2, x2_len);
  size_t v1_x_len = eval_len;
  while (v1_x_len > 0 && v1_x[v1_x_len-1] == 0) v1_x_len--;
  if (v1_x_len == 0) v1_x_len = 1;

  mpn_zero(v1_y, eval_len);
  mpn_copyi(v1_y, y0, y0_len);
  if (y1_len > 0) mpn_add(v1_y, v1_y, eval_len, y1, y1_len);
  if (y2_len > 0) mpn_add(v1_y, v1_y, eval_len, y2, y2_len);
  size_t v1_y_len = eval_len;
  while (v1_y_len > 0 && v1_y[v1_y_len-1] == 0) v1_y_len--;
  if (v1_y_len == 0) v1_y_len = 1;

  /* vm1 = x0 - x1 + x2 (may be negative, track sign) */
  mp_limb vm1_x_neg = 0, vm1_y_neg = 0;
  {
    /* t = x0 + x2 */
    mpn_zero(vm1_x, eval_len);
    mpn_copyi(vm1_x, x0, x0_len);
    if (x2_len > 0) mpn_add(vm1_x, vm1_x, eval_len, x2, x2_len);
    /* vm1_x = t - x1 */
    if (x1_len > 0) {
      vm1_x_neg = mpn_sub_var(vm1_x, vm1_x, eval_len, x1, x1_len, eval_len);
      if (vm1_x_neg) mpn_neg(vm1_x, vm1_x, eval_len);
    }
  }
  size_t vm1_x_len = eval_len;
  while (vm1_x_len > 0 && vm1_x[vm1_x_len-1] == 0) vm1_x_len--;
  if (vm1_x_len == 0) vm1_x_len = 1;

  {
    mpn_zero(vm1_y, eval_len);
    mpn_copyi(vm1_y, y0, y0_len);
    if (y2_len > 0) mpn_add(vm1_y, vm1_y, eval_len, y2, y2_len);
    if (y1_len > 0) {
      vm1_y_neg = mpn_sub_var(vm1_y, vm1_y, eval_len, y1, y1_len, eval_len);
      if (vm1_y_neg) mpn_neg(vm1_y, vm1_y, eval_len);
    }
  }
  size_t vm1_y_len = eval_len;
  while (vm1_y_len > 0 && vm1_y[vm1_y_len-1] == 0) vm1_y_len--;
  if (vm1_y_len == 0) vm1_y_len = 1;

  /* v2 = x0 + 2*x1 + 4*x2 */
  {
    mpn_zero(v2_x, eval_len);
    mpn_copyi(v2_x, x0, x0_len);
    /* Add 2*x1 */
    if (x1_len > 0) {
      mp_limb carry = 0;
      for (size_t i = 0; i < x1_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_x[i] + ((mp_dbl_limb)x1[i] << 1) + carry;
        v2_x[i] = LOW(val);
        carry = HIGH(val);
      }
      for (size_t i = x1_len; carry && i < eval_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_x[i] + carry;
        v2_x[i] = LOW(val);
        carry = HIGH(val);
      }
    }
    /* Add 4*x2 */
    if (x2_len > 0) {
      mp_limb carry = 0;
      for (size_t i = 0; i < x2_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_x[i] + ((mp_dbl_limb)x2[i] << 2) + carry;
        v2_x[i] = LOW(val);
        carry = HIGH(val);
      }
      for (size_t i = x2_len; carry && i < eval_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_x[i] + carry;
        v2_x[i] = LOW(val);
        carry = HIGH(val);
      }
    }
  }
  size_t v2_x_len = eval_len;
  while (v2_x_len > 0 && v2_x[v2_x_len-1] == 0) v2_x_len--;
  if (v2_x_len == 0) v2_x_len = 1;

  {
    mpn_zero(v2_y, eval_len);
    mpn_copyi(v2_y, y0, y0_len);
    if (y1_len > 0) {
      mp_limb carry = 0;
      for (size_t i = 0; i < y1_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_y[i] + ((mp_dbl_limb)y1[i] << 1) + carry;
        v2_y[i] = LOW(val);
        carry = HIGH(val);
      }
      for (size_t i = y1_len; carry && i < eval_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_y[i] + carry;
        v2_y[i] = LOW(val);
        carry = HIGH(val);
      }
    }
    if (y2_len > 0) {
      mp_limb carry = 0;
      for (size_t i = 0; i < y2_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_y[i] + ((mp_dbl_limb)y2[i] << 2) + carry;
        v2_y[i] = LOW(val);
        carry = HIGH(val);
      }
      for (size_t i = y2_len; carry && i < eval_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_y[i] + carry;
        v2_y[i] = LOW(val);
        carry = HIGH(val);
      }
    }
  }
  size_t v2_y_len = eval_len;
  while (v2_y_len > 0 && v2_y[v2_y_len-1] == 0) v2_y_len--;
  if (v2_y_len == 0) v2_y_len = 1;

  /*
   * Pointwise multiplication (5 recursive calls)
   */
  mpn_zero(w0, prod_len);
  mpn_zero(w1, prod_len);
  mpn_zero(wm1, prod_len);
  mpn_zero(w2, prod_len);
  mpn_zero(winf, prod_len);

  /* w0 = v0_x * v0_y = x0 * y0 */
  mpz_mul_toom3(ctx, w0, x0, x0_len, y0, y0_len, recursive_scratch);

  /* w1 = v1_x * v1_y */
  mpz_mul_toom3(ctx, w1, v1_x, v1_x_len, v1_y, v1_y_len, recursive_scratch);

  /* wm1 = vm1_x * vm1_y (sign = vm1_x_neg XOR vm1_y_neg) */
  mp_limb wm1_neg = vm1_x_neg ^ vm1_y_neg;
  mpz_mul_toom3(ctx, wm1, vm1_x, vm1_x_len, vm1_y, vm1_y_len, recursive_scratch);

  /* w2 = v2_x * v2_y */
  mpz_mul_toom3(ctx, w2, v2_x, v2_x_len, v2_y, v2_y_len, recursive_scratch);

  /* winf = vinf_x * vinf_y = x2 * y2 */
  if (x2_len > 0 && y2_len > 0) {
    mpz_mul_toom3(ctx, winf, x2, x2_len, y2, y2_len, recursive_scratch);
  }

  /*
   * Interpolation to recover r0, r1, r2, r3, r4 where:
   * result = r0 + r1*B + r2*B^2 + r3*B^3 + r4*B^4
   *
   * Using the sequence:
   * 1. r0 = w0
   * 2. r4 = winf
   * 3. t1 = w1 - r0 - r4           (= r1 + r2 + r3)
   * 4. t2 = wm1 - r0 - r4          (= -r1 + r2 - r3, may need sign adjustment)
   * 5. r2 = (t1 + t2) / 2
   * 6. t3 = w2 - r0 - 16*r4        (= 2r1 + 4r2 + 8r3)
   * 7. t4 = t3 / 2                 (= r1 + 2r2 + 4r3)
   * 8. t5 = (t1 - t2) / 2          (= r1 + r3)
   * 9. t6 = t4 - 2*r2              (= r1 + 4r3)
   * 10. r3 = (t6 - t5) / 3
   * 11. r1 = t5 - r3
   */

  /*
   * Reuse product buffers for interpolation (prod_len sized):
   * - t1 reuses w1 (we compute t1 = w1 - ...)
   * - t2 reuses wm1 (we compute t2 = wm1 - ...)
   * - t3 reuses w2 (we compute t3 = w2 - ...)
   * - t4, t5, t6 need separate space (use recursive_scratch area)
   * We keep w0 and winf for final assembly.
   */
  size_t w_len = prod_len;

  mp_limb *t1 = w1;   /* reuse w1 */
  mp_limb *t2 = wm1;  /* reuse wm1 */
  mp_limb *t3 = w2;   /* reuse w2 */
  /* t4, t5, t6, r2_tmp use recursive_scratch area */
  mp_limb *t4 = recursive_scratch;
  mp_limb *t5 = recursive_scratch + w_len;
  mp_limb *t6 = recursive_scratch + 2 * w_len;
  mp_limb *r2_tmp = recursive_scratch + 3 * w_len;

  /* t1 = w1 - w0 - winf (computed in-place in w1) */
  mpn_sub(t1, t1, w_len, w0, w_len);
  mpn_sub(t1, t1, w_len, winf, w_len);

  /* t2 = wm1 - w0 - winf (with sign handling, computed in-place in wm1) */
  if (wm1_neg) {
    /* t2 = -wm1 - w0 - winf = -(wm1 + w0 + winf) */
    mpn_add(t2, t2, w_len, w0, w_len);
    mpn_add(t2, t2, w_len, winf, w_len);
    mpn_neg(t2, t2, w_len);
  } else {
    mpn_sub(t2, t2, w_len, w0, w_len);
    mpn_sub(t2, t2, w_len, winf, w_len);
  }

  /* r2 = (t1 + t2) / 2 */
  mpn_add_var(r2_tmp, t1, w_len, t2, w_len, w_len);
  mpn_rshift1(r2_tmp, r2_tmp, w_len);

  /* t3 = w2 - w0 - 16*winf (computed in-place in w2) */
  mpn_sub(t3, t3, w_len, w0, w_len);
  /* Subtract 16*winf */
  {
    mp_limb borrow = 0;
    for (size_t i = 0; i < w_len; i++) {
      mp_dbl_limb_signed diff = (mp_dbl_limb_signed)t3[i]
                               - ((mp_dbl_limb_signed)winf[i] << 4) - borrow;
      t3[i] = LOW(diff);
      borrow = (diff < 0) ? (mp_limb)(-(diff >> DIG_SIZE)) : 0;
    }
  }

  /* t4 = t3 / 2 */
  mpn_rshift1(t4, t3, w_len);

  /* t5 = (t1 - t2) / 2 */
  mpn_sub_var(t5, t1, w_len, t2, w_len, w_len);
  mpn_rshift1(t5, t5, w_len);

  /* t6 = t4 - 2*r2 */
  {
    mp_limb borrow = 0;
    for (size_t i = 0; i < w_len; i++) {
      mp_dbl_limb_signed diff = (mp_dbl_limb_signed)t4[i]
                               - ((mp_dbl_limb_signed)r2_tmp[i] << 1) - borrow;
      t6[i] = LOW(diff);
      borrow = (diff < 0) ? (mp_limb)(-(diff >> DIG_SIZE)) : 0;
    }
  }

  /* r3 = (t6 - t5) / 3 */
  mp_limb *r3 = t4;  /* reuse t4 */
  mpn_sub(r3, t6, w_len, t5, w_len);
  mpn_divexact_3(r3, r3, w_len);

  /* r1 = t5 - r3 */
  mp_limb *r1 = t5;  /* in-place */
  mpn_sub(r1, t5, w_len, r3, w_len);

  /*
   * Final assembly: result = r0 + r1*B + r2*B^2 + r3*B^3 + r4*B^4
   * where B = base^third
   *
   * Maximum write position is 4*third + w_len.
   * With w_len = 2*(third+3) + 16 = 2*n/3 + 22 (for recursion margin),
   * max position = 4*n/3 + 2*n/3 + 22 = 2*n + 22.
   * Use 2*n (not x_len + y_len) because third is based on n = max.
   */
  size_t result_len = 2 * n + 24;
  mpn_zero(result, result_len);

  /* r0 at offset 0 */
  mpn_copyi(result, w0, (2*third < result_len) ? 2*third : result_len);

  /* r1 at offset third */
  limb_add_at(result, result_len, r1, w_len, third);

  /* r2 at offset 2*third */
  limb_add_at(result, result_len, r2_tmp, w_len, 2*third);

  /* r3 at offset 3*third */
  limb_add_at(result, result_len, r3, w_len, 3*third);

  /* r4 at offset 4*third */
  limb_add_at(result, result_len, winf, w_len, 4*third);
}

/*
 * Toom-3 squaring scratch size (smaller than multiplication since no y evaluation).
 */
static size_t
toom3_sqr_scratch_size(size_t n)
{
  if (!should_use_toom3(n)) {
    return 0;
  }

  size_t third = n / 3;

  /*
   * Per level storage for squaring:
   * - 3 evaluation results: v1_x, vm1_x, v2_x (no y needed)
   *   Each up to (third + 3) limbs for carries
   * - 5 product results: w0, w1, wm1, w2, winf
   *   Each up to 2*(third + 3) + 16 limbs
   * - 4 interpolation temps: t4, t5, t6, r2_tmp
   *   Each up to 2*(third + 3) + 16 limbs
   */
  size_t eval_len = third + 3;
  size_t prod_len = 2 * eval_len + 16;
  size_t eval_space = 3 * eval_len;      /* v1_x, vm1_x, v2_x */
  size_t prod_space = 5 * prod_len;      /* w0, w1, wm1, w2, winf */
  size_t current_level = eval_space + prod_space;

  /* Interpolation temps (4 * prod_len) are reused with recursive scratch */
  size_t interp_size = 4 * prod_len;

  /* Recursively calculate sub-squaring scratch size */
  size_t sub_n = eval_len;
  size_t sub_scratch = toom3_sqr_scratch_size(sub_n);
  if (sub_scratch < interp_size) {
    sub_scratch = interp_size;
  }

  return current_level + sub_scratch + 8;
}

/* Toom-3 squaring: optimized squaring for large numbers */
static void
mpz_sqr_toom3(mpz_ctx_t *ctx, mp_limb *result,
              const mp_limb *x, size_t x_len,
              mp_limb *scratch)
{
  /*
   * Base case - use schoolbook squaring.
   */
  if (!should_use_toom3(x_len)) {
    mpz_sqr_basic_limbs(result, x, x_len);
    return;
  }

  /*
   * Split: x = x2*B^2 + x1*B + x0
   * where B = base^third
   */
  size_t n = x_len;
  size_t third = n / 3;
  size_t x0_len = (x_len > third) ? third : x_len;
  size_t x1_len = (x_len > 2*third) ? third : ((x_len > third) ? x_len - third : 0);
  size_t x2_len = (x_len > 2*third) ? x_len - 2*third : 0;

  const mp_limb *x0 = x;
  const mp_limb *x1 = x + third;
  const mp_limb *x2 = x + 2*third;

  /* Allocate scratch space (fewer buffers than multiplication) */
  size_t eval_len = third + 3;
  size_t prod_len = 2 * eval_len + 16;

  size_t offset = 0;
  mp_limb *v1_x = scratch + offset; offset += eval_len;
  mp_limb *vm1_x = scratch + offset; offset += eval_len;
  mp_limb *v2_x = scratch + offset; offset += eval_len;

  mp_limb *w0 = scratch + offset; offset += prod_len;
  mp_limb *w1 = scratch + offset; offset += prod_len;
  mp_limb *wm1 = scratch + offset; offset += prod_len;
  mp_limb *w2 = scratch + offset; offset += prod_len;
  mp_limb *winf = scratch + offset; offset += prod_len;

  mp_limb *recursive_scratch = scratch + offset;

  /*
   * Evaluation at 5 points (only x, no y):
   * v0 = x0 (reuse input)
   * v1 = x0 + x1 + x2
   * vm1 = x0 - x1 + x2
   * v2 = x0 + 2*x1 + 4*x2
   * vinf = x2 (reuse input)
   */

  /* v1 = x0 + x1 + x2 */
  mpn_zero(v1_x, eval_len);
  mpn_copyi(v1_x, x0, x0_len);
  if (x1_len > 0) mpn_add(v1_x, v1_x, eval_len, x1, x1_len);
  if (x2_len > 0) mpn_add(v1_x, v1_x, eval_len, x2, x2_len);
  size_t v1_x_len = eval_len;
  while (v1_x_len > 0 && v1_x[v1_x_len-1] == 0) v1_x_len--;
  if (v1_x_len == 0) v1_x_len = 1;

  /* vm1 = x0 - x1 + x2 (may be negative, track sign) */
  mp_limb vm1_x_neg = 0;
  {
    mpn_zero(vm1_x, eval_len);
    mpn_copyi(vm1_x, x0, x0_len);
    if (x2_len > 0) mpn_add(vm1_x, vm1_x, eval_len, x2, x2_len);
    if (x1_len > 0) {
      vm1_x_neg = mpn_sub_var(vm1_x, vm1_x, eval_len, x1, x1_len, eval_len);
      if (vm1_x_neg) mpn_neg(vm1_x, vm1_x, eval_len);
    }
  }
  size_t vm1_x_len = eval_len;
  while (vm1_x_len > 0 && vm1_x[vm1_x_len-1] == 0) vm1_x_len--;
  if (vm1_x_len == 0) vm1_x_len = 1;

  /* v2 = x0 + 2*x1 + 4*x2 */
  {
    mpn_zero(v2_x, eval_len);
    mpn_copyi(v2_x, x0, x0_len);
    if (x1_len > 0) {
      mp_limb carry = 0;
      for (size_t i = 0; i < x1_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_x[i] + ((mp_dbl_limb)x1[i] << 1) + carry;
        v2_x[i] = LOW(val);
        carry = HIGH(val);
      }
      for (size_t i = x1_len; carry && i < eval_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_x[i] + carry;
        v2_x[i] = LOW(val);
        carry = HIGH(val);
      }
    }
    if (x2_len > 0) {
      mp_limb carry = 0;
      for (size_t i = 0; i < x2_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_x[i] + ((mp_dbl_limb)x2[i] << 2) + carry;
        v2_x[i] = LOW(val);
        carry = HIGH(val);
      }
      for (size_t i = x2_len; carry && i < eval_len; i++) {
        mp_dbl_limb val = (mp_dbl_limb)v2_x[i] + carry;
        v2_x[i] = LOW(val);
        carry = HIGH(val);
      }
    }
  }
  size_t v2_x_len = eval_len;
  while (v2_x_len > 0 && v2_x[v2_x_len-1] == 0) v2_x_len--;
  if (v2_x_len == 0) v2_x_len = 1;

  /*
   * Pointwise squaring (5 recursive calls)
   * Key difference from multiplication: squaring instead of multiplication
   */
  mpn_zero(w0, prod_len);
  mpn_zero(w1, prod_len);
  mpn_zero(wm1, prod_len);
  mpn_zero(w2, prod_len);
  mpn_zero(winf, prod_len);

  /* w0 = v0^2 = x0^2 */
  mpz_sqr_toom3(ctx, w0, x0, x0_len, recursive_scratch);

  /* w1 = v1^2 */
  mpz_sqr_toom3(ctx, w1, v1_x, v1_x_len, recursive_scratch);

  /* wm1 = vm1^2 (sign is always positive for squaring!) */
  (void)vm1_x_neg;  /* squaring ignores sign */
  mpz_sqr_toom3(ctx, wm1, vm1_x, vm1_x_len, recursive_scratch);

  /* w2 = v2^2 */
  mpz_sqr_toom3(ctx, w2, v2_x, v2_x_len, recursive_scratch);

  /* winf = vinf^2 = x2^2 */
  if (x2_len > 0) {
    mpz_sqr_toom3(ctx, winf, x2, x2_len, recursive_scratch);
  }

  /*
   * Interpolation (same as multiplication)
   * Recover r0, r1, r2, r3, r4 where:
   * result = r0 + r1*B + r2*B^2 + r3*B^3 + r4*B^4
   *
   * For squaring, wm1 is always positive, simplifying step 4.
   */
  size_t w_len = prod_len;

  mp_limb *t1 = w1;   /* reuse w1 */
  mp_limb *t2 = wm1;  /* reuse wm1 */
  mp_limb *t3 = w2;   /* reuse w2 */
  mp_limb *t4 = recursive_scratch;
  mp_limb *t5 = recursive_scratch + w_len;
  mp_limb *t6 = recursive_scratch + 2 * w_len;
  mp_limb *r2_tmp = recursive_scratch + 3 * w_len;

  /* t1 = w1 - w0 - winf */
  mpn_sub(t1, t1, w_len, w0, w_len);
  mpn_sub(t1, t1, w_len, winf, w_len);

  /* t2 = wm1 - w0 - winf (no sign handling for squaring!) */
  mpn_sub(t2, t2, w_len, w0, w_len);
  mpn_sub(t2, t2, w_len, winf, w_len);

  /* r2 = (t1 + t2) / 2 */
  mpn_add_var(r2_tmp, t1, w_len, t2, w_len, w_len);
  mpn_rshift1(r2_tmp, r2_tmp, w_len);

  /* t3 = w2 - w0 - 16*winf */
  mpn_sub(t3, t3, w_len, w0, w_len);
  {
    mp_limb borrow = 0;
    for (size_t i = 0; i < w_len; i++) {
      mp_dbl_limb_signed diff = (mp_dbl_limb_signed)t3[i]
                               - ((mp_dbl_limb_signed)winf[i] << 4) - borrow;
      t3[i] = LOW(diff);
      borrow = (diff < 0) ? (mp_limb)(-(diff >> DIG_SIZE)) : 0;
    }
  }

  /* t4 = t3 / 2 */
  mpn_rshift1(t4, t3, w_len);

  /* t5 = (t1 - t2) / 2 */
  mpn_sub_var(t5, t1, w_len, t2, w_len, w_len);
  mpn_rshift1(t5, t5, w_len);

  /* t6 = t4 - 2*r2 */
  {
    mp_limb borrow = 0;
    for (size_t i = 0; i < w_len; i++) {
      mp_dbl_limb_signed diff = (mp_dbl_limb_signed)t4[i]
                               - ((mp_dbl_limb_signed)r2_tmp[i] << 1) - borrow;
      t6[i] = LOW(diff);
      borrow = (diff < 0) ? (mp_limb)(-(diff >> DIG_SIZE)) : 0;
    }
  }

  /* r3 = (t6 - t5) / 3 */
  mp_limb *r3 = t4;
  mpn_sub(r3, t6, w_len, t5, w_len);
  mpn_divexact_3(r3, r3, w_len);

  /* r1 = t5 - r3 */
  mp_limb *r1 = t5;
  mpn_sub(r1, t5, w_len, r3, w_len);

  /*
   * Final assembly: result = r0 + r1*B + r2*B^2 + r3*B^3 + r4*B^4
   */
  size_t result_len = 2 * n + 24;
  mpn_zero(result, result_len);

  mpn_copyi(result, w0, (2*third < result_len) ? 2*third : result_len);
  limb_add_at(result, result_len, r1, w_len, third);
  limb_add_at(result, result_len, r2_tmp, w_len, 2*third);
  limb_add_at(result, result_len, r3, w_len, 3*third);
  limb_add_at(result, result_len, winf, w_len, 4*third);
}

/*
 * Check if mpz is "all ones" pattern (2^n - 1).
 * For such a number:
 *   - All limbs except possibly the top one equal DIG_MASK
 *   - The top limb equals (1 << k) - 1 for some k in 1..DIG_SIZE
 * Returns the bit count n if all-ones, 0 otherwise.
 */
static size_t
mpz_all_ones_p(mpz_t *x)
{
  if (x->sn <= 0 || x->sz == 0) return 0;

  /* Check all but top limb */
  for (size_t i = 0; i + 1 < x->sz; i++) {
    if (x->p[i] != DIG_MASK) return 0;
  }

  /* Check top limb: must be (1 << k) - 1 for some k */
  mp_limb top = x->p[x->sz - 1];
  if (top == 0) return 0;

  /* Check if top is all-ones pattern: (top & (top + 1)) == 0 */
  if ((top & (top + 1)) != 0) return 0;

  /* Count bits in top limb */
  size_t top_bits = 0;
  while (top) { top_bits++; top >>= 1; }

  return (x->sz - 1) * DIG_SIZE + top_bits;
}

/*
 * Check if x is a power of 2 (2^n).
 * Returns n if x = 2^n, or 0 otherwise.
 * This is the "mostly-zero" pattern common in fuzzing tests.
 */
static size_t
mpz_power_of_2_exp(mpz_t *x)
{
  if (x->sn <= 0 || x->sz == 0) return 0;

  /* All limbs except top must be zero */
  for (size_t i = 0; i + 1 < x->sz; i++) {
    if (x->p[i] != 0) return 0;
  }

  /* Top limb must be a power of 2: (v & (v - 1)) == 0 */
  mp_limb top = x->p[x->sz - 1];
  if (top == 0 || (top & (top - 1)) != 0) return 0;

  /* Count trailing zeros in top limb to get bit position */
  size_t bit_pos = 0;
  while ((top & 1) == 0) { bit_pos++; top >>= 1; }

  return (x->sz - 1) * DIG_SIZE + bit_pos;
}

/* Count set bits in a limb */
static int
limb_popcount(mp_limb x)
{
#if defined(__GNUC__) || __has_builtin(__builtin_popcount)
  if (sizeof(mp_limb) == sizeof(unsigned long long))
    return __builtin_popcountll(x);
  else
    return __builtin_popcount(x);
#else
  int count = 0;
  while (x) {
    count++;
    x &= x - 1;  /* Clear lowest set bit */
  }
  return count;
#endif
}

/*
 * Count total set bits in mpz.
 * Returns popcount, or max_count+1 if exceeded (for early exit).
 */
static size_t
mpz_popcount(mpz_t *x, size_t max_count)
{
  if (x->sn <= 0 || x->sz == 0) return 0;

  size_t count = 0;
  for (size_t i = 0; i < x->sz && count <= max_count; i++) {
    count += limb_popcount(x->p[i]);
  }
  return count;
}

/* Maximum bits for sparse multiplication optimization */
#define SPARSE_MAX_BITS 8

/*
 * Check if x is sparse (few bits set) and worth optimizing.
 * Only worthwhile for large numbers where Karatsuba would be used.
 * Returns popcount if sparse and optimizable, 0 otherwise.
 */
static size_t
mpz_sparse_p(mpz_t *x)
{
  if (x->sn <= 0 || x->sz < TOOM3_THRESHOLD) return 0;

  size_t popcount = mpz_popcount(x, SPARSE_MAX_BITS);
  if (popcount > SPARSE_MAX_BITS) return 0;

  return popcount;
}

/*
 * Multiply sparse number by dense number using shift-add.
 * sparse * dense = sum of (dense << bit_position) for each set bit
 *
 * O(k * n) where k = popcount, much faster than Karatsuba when k is small.
 */
static void
mpz_mul_sparse(mpz_ctx_t *ctx, mpz_t *w, mpz_t *sparse, mpz_t *dense)
{
  mpz_t shifted, temp;

  mpz_init(ctx, &shifted);
  mpz_init(ctx, &temp);
  zero(w);

  for (size_t i = 0; i < sparse->sz; i++) {
    mp_limb limb = sparse->p[i];
    size_t base_bit = i * DIG_SIZE;

    while (limb) {
      /* Find position of lowest set bit */
      int bit = 0;
#if defined(__GNUC__) || __has_builtin(__builtin_ctz)
      if (sizeof(mp_limb) == sizeof(unsigned long long))
        bit = __builtin_ctzll(limb);
      else
        bit = __builtin_ctz(limb);
#else
      while ((limb & ((mp_limb)1 << bit)) == 0) bit++;
#endif

      /* Add dense << (base_bit + bit) to result */
      mpz_mul_2exp(ctx, &shifted, dense, base_bit + bit);
      mpz_add(ctx, &temp, w, &shifted);
      mpz_set(ctx, w, &temp);

      /* Clear this bit */
      limb &= limb - 1;
    }
  }

  /* Handle sign */
  if (sparse->sn < 0) w->sn = -w->sn;

  mpz_clear(ctx, &shifted);
  mpz_clear(ctx, &temp);
}

/*
 * Multiply two "all ones" numbers using algebraic identity:
 * (2^n - 1) * (2^m - 1) = 2^(n+m) - 2^n - 2^m + 1
 *
 * For squaring (n == m):
 * (2^n - 1)^2 = 2^(2n) - 2^(n+1) + 1
 *
 * This is O(n) instead of O(n^1.585) for Karatsuba.
 */
struct mpz_mul_all_ones_data {
  mpz_ctx_t *ctx;
  mpz_t *w;
  size_t n, m;
  mpz_t a, b;  /* cleanup targets */
};

static mrb_value
mpz_mul_all_ones_body(mrb_state *mrb, void *userdata)
{
  struct mpz_mul_all_ones_data *d = (struct mpz_mul_all_ones_data *)userdata;
  mpz_ctx_t *ctx = d->ctx;
  mpz_t *w = d->w;
  size_t n = d->n, m = d->m;

  if (n == m) {
    /* Squaring: (2^n - 1)^2 = 2^(2n) - 2^(n+1) + 1 */
    /* Start with 2^(2n) */
    mpz_init(ctx, &d->a);
    mpz_set_int(ctx, &d->a, 1);
    mpz_mul_2exp(ctx, w, &d->a, 2*n);

    /* Subtract 2^(n+1) */
    mpz_set_int(ctx, &d->a, 1);
    mpz_mul_2exp(ctx, &d->a, &d->a, n+1);
    mpz_sub(ctx, w, w, &d->a);

    /* Add 1 */
    mpz_add_int(ctx, w, 1);
  }
  else {
    /* General: (2^n - 1) * (2^m - 1) = 2^(n+m) - 2^n - 2^m + 1 */
    mpz_init(ctx, &d->a);
    mpz_init(ctx, &d->b);

    /* Start with 2^(n+m) */
    mpz_set_int(ctx, &d->a, 1);
    mpz_mul_2exp(ctx, w, &d->a, n+m);

    /* Subtract 2^n */
    mpz_set_int(ctx, &d->a, 1);
    mpz_mul_2exp(ctx, &d->a, &d->a, n);
    mpz_sub(ctx, w, w, &d->a);

    /* Subtract 2^m */
    mpz_set_int(ctx, &d->b, 1);
    mpz_mul_2exp(ctx, &d->b, &d->b, m);
    mpz_sub(ctx, w, w, &d->b);

    /* Add 1 */
    mpz_add_int(ctx, w, 1);
  }

  return mrb_nil_value();
}

static void
mpz_mul_all_ones(mpz_ctx_t *ctx, mpz_t *w, size_t n, size_t m)
{
  struct mpz_mul_all_ones_data d = {ctx, w, n, m, {0,0,0}, {0,0,0}};
  mrb_bool error = FALSE;

  mrb_value exc = mrb_protect_error(MPZ_MRB(ctx), mpz_mul_all_ones_body, &d, &error);

  /* Cleanup always runs (mpz_clear is safe on zero-initialized mpz_t) */
  mpz_clear(ctx, &d.a);
  mpz_clear(ctx, &d.b);

  if (error) {
    mrb_exc_raise(MPZ_MRB(ctx), exc);
  }
}

/* w = u^2 (squaring - faster than general multiplication) */
static void
mpz_sqr(mpz_ctx_t *ctx, mpz_t *ww, mpz_t *u)
{
  if (zero_p(u)) {
    zero(ww);
    return;
  }

  /* Fast path for power of 2: (2^n)^2 = 2^(2n) */
  size_t u_pow2 = mpz_power_of_2_exp(u);
  if (u_pow2) {
    mpz_t one;
    mpz_init(ctx, &one);
    mpz_set_int(ctx, &one, 1);
    mpz_mul_2exp(ctx, ww, &one, 2 * u_pow2);
    mpz_clear(ctx, &one);
    return;
  }

  /* Fast path for all-ones: (2^n - 1)^2 = 2^(2n) - 2^(n+1) + 1 */
  size_t u_ones = mpz_all_ones_p(u);
  if (u_ones) {
    mpz_mul_all_ones(ctx, ww, u_ones, u_ones);
    return;
  }

  /* Use schoolbook squaring for small numbers */
  if (!should_use_toom3(u->sz)) {
    mpz_t w;
    mpz_init_heap(ctx, &w, 2 * u->sz);
    mpz_sqr_basic_limbs(w.p, u->p, u->sz);
    w.sz = 2 * u->sz;
    w.sn = 1;  /* Square is always positive */
    trim(&w);
    mpz_move(ctx, ww, &w);
    return;
  }

  /*
   * Toom-3 squaring: use optimized squaring for large numbers.
   * Toom-3 writes at offset 4*third with products up to 2*(third+3)+16 limbs.
   * Maximum write position: 4*n/3 + 2*n/3 + 22 = 2*n + 22.
   * Add extra space to prevent buffer overflow.
   */
  size_t result_size = 2 * u->sz + 24;
  mpz_realloc(ctx, ww, result_size);

  size_t scratch_size = toom3_sqr_scratch_size(u->sz);
  scratch_size += (scratch_size >> 3) + 16;
  size_t pool_state = pool_save(ctx);
  mp_limb *scratch = NULL;

  if (MPZ_HAS_POOL(ctx)) {
    scratch = pool_alloc(MPZ_POOL(ctx), scratch_size);
  }

  if (scratch) {
    mpz_sqr_toom3(ctx, ww->p, u->p, u->sz, scratch);
    pool_restore(ctx, pool_state);
  }
  else {
    scratch = (mp_limb*)mrb_malloc(MPZ_MRB(ctx), scratch_size * sizeof(mp_limb));
    mpz_sqr_toom3(ctx, ww->p, u->p, u->sz, scratch);
    mrb_free(MPZ_MRB(ctx), scratch);
  }

  ww->sz = result_size;
  ww->sn = 1;  /* Square is always positive */
  trim(ww);
}

/*
 * Balance multiplication for asymmetric operands.
 * When one operand is much larger than the other (max >= 2*min),
 * split the larger into chunks of size equal to the smaller,
 * multiply each chunk, and combine with shifts.
 */
static void
mpz_mul_balance(mpz_ctx_t *ctx, mpz_t *ww, mpz_t *a, mpz_t *b)
{
  /* Ensure 'a' is the larger operand */
  if (a->sz < b->sz) {
    mpz_t *t = a; a = b; b = t;
  }

  size_t bsize = b->sz;             /* chunk size = smaller operand size */
  size_t nblocks = a->sz / bsize;   /* number of full chunks */

  size_t pool_state = pool_save(ctx);
  mpz_t chunk, tmp, result;
  mpz_init(ctx, &result);
  mpz_init_heap(ctx, &chunk, bsize);
  mpz_init(ctx, &tmp);

  size_t j = 0;
  for (size_t i = 0; i < nblocks; i++) {
    /* Copy chunk from a */
    memcpy(chunk.p, a->p + j, bsize * sizeof(mp_limb));
    chunk.sz = bsize;
    chunk.sn = 1;
    trim(&chunk);
    j += bsize;

    if (!zero_p(&chunk)) {
      /* Multiply chunk * b */
      mpz_mul(ctx, &tmp, &chunk, b);

      /* Shift tmp left by (i * bsize) limbs */
      if (i > 0) {
        size_t shift_limbs = i * bsize;
        size_t old_sz = tmp.sz;
        size_t new_sz = old_sz + shift_limbs;
        mpz_realloc(ctx, &tmp, new_sz);
        memmove(tmp.p + shift_limbs, tmp.p, old_sz * sizeof(mp_limb));
        memset(tmp.p, 0, shift_limbs * sizeof(mp_limb));
        tmp.sz = new_sz;
      }

      /* Add to result */
      mpz_add(ctx, &result, &result, &tmp);
    }
  }

  /* Handle leftover (remaining limbs after full chunks) */
  if (j < a->sz) {
    size_t remaining = a->sz - j;
    mpz_realloc(ctx, &chunk, remaining);
    memcpy(chunk.p, a->p + j, remaining * sizeof(mp_limb));
    chunk.sz = remaining;
    chunk.sn = 1;
    trim(&chunk);

    if (!zero_p(&chunk)) {
      mpz_mul(ctx, &tmp, &chunk, b);

      /* Shift by j limbs */
      if (j > 0) {
        size_t old_sz = tmp.sz;
        size_t new_sz = old_sz + j;
        mpz_realloc(ctx, &tmp, new_sz);
        memmove(tmp.p + j, tmp.p, old_sz * sizeof(mp_limb));
        memset(tmp.p, 0, j * sizeof(mp_limb));
        tmp.sz = new_sz;
      }

      mpz_add(ctx, &result, &result, &tmp);
    }
  }

  /* Apply sign: result sign = a->sn * b->sn */
  result.sn = a->sn * b->sn;

  mpz_move(ctx, ww, &result);
  mpz_clear(ctx, &chunk);
  mpz_clear(ctx, &tmp);
  pool_restore(ctx, pool_state);
}

/* w = u * v */
static void
mpz_mul(mpz_ctx_t *ctx, mpz_t *ww, mpz_t *u, mpz_t *v)
{
  if (zero_p(u) || zero_p(v)) {
    zero(ww);
    return;
  }

  /* Fast path for squaring: u * u uses optimized squaring algorithm */
  if (u == v) {
    mpz_sqr(ctx, ww, u);
    return;
  }

  /* Fast path for "all ones" numbers (2^n - 1) */
  size_t u_ones = mpz_all_ones_p(u);
  size_t v_ones = mpz_all_ones_p(v);
  if (u_ones && v_ones) {
    mpz_mul_all_ones(ctx, ww, u_ones, v_ones);
    return;
  }

  /* Fast path: (2^n - 1) * y = (y << n) - y */
  if (u_ones && u_ones >= TOOM3_THRESHOLD * DIG_SIZE) {
    mpz_t shifted;
    mpz_init(ctx, &shifted);
    mpz_mul_2exp(ctx, &shifted, v, u_ones);
    mpz_sub(ctx, ww, &shifted, v);
    mpz_clear(ctx, &shifted);
    return;
  }
  if (v_ones && v_ones >= TOOM3_THRESHOLD * DIG_SIZE) {
    mpz_t shifted;
    mpz_init(ctx, &shifted);
    mpz_mul_2exp(ctx, &shifted, u, v_ones);
    mpz_sub(ctx, ww, &shifted, u);
    mpz_clear(ctx, &shifted);
    return;
  }

  /* Fast path for power of 2: x * 2^n = x << n */
  size_t u_pow2 = mpz_power_of_2_exp(u);
  if (u_pow2) {
    mpz_mul_2exp(ctx, ww, v, u_pow2);
    return;
  }
  size_t v_pow2 = mpz_power_of_2_exp(v);
  if (v_pow2) {
    mpz_mul_2exp(ctx, ww, u, v_pow2);
    return;
  }

  /* Fast path for sparse numbers (few bits set): use shift-add */
  size_t u_sparse = mpz_sparse_p(u);
  if (u_sparse) {
    mpz_mul_sparse(ctx, ww, u, v);
    return;
  }
  size_t v_sparse = mpz_sparse_p(v);
  if (v_sparse) {
    mpz_mul_sparse(ctx, ww, v, u);
    return;
  }

  size_t min_sz = (u->sz < v->sz) ? u->sz : v->sz;
  size_t max_sz = (u->sz > v->sz) ? u->sz : v->sz;

  /*
   * Balance multiplication for highly asymmetric operands.
   * When max >= 2*min and the smaller is above Toom-3 threshold,
   * split the larger operand into chunks for better efficiency.
   */
  if (max_sz >= 2 * min_sz && should_use_toom3(min_sz)) {
    mpz_mul_balance(ctx, ww, u, v);
    return;
  }

  /*
   * Use schoolbook for small operands below Karatsuba threshold.
   */
  if (!should_use_karatsuba(min_sz)) {
    mpz_mul_basic(ctx, ww, u, v);
    return;
  }

  /*
   * Karatsuba for medium-sized operands (KARATSUBA_THRESHOLD <= min_sz < TOOM3_THRESHOLD).
   */
  if (!should_use_toom3(min_sz)) {
    size_t result_size = u->sz + v->sz;
    mpz_realloc(ctx, ww, result_size);

    size_t scratch_size = karatsuba_scratch_size(max_sz);
    scratch_size += (scratch_size >> 3) + 16;  /* safety margin */
    size_t pool_state = pool_save(ctx);
    mp_limb *scratch = NULL;

    if (MPZ_HAS_POOL(ctx)) {
      scratch = pool_alloc(MPZ_POOL(ctx), scratch_size);
    }

    if (scratch) {
      mpz_mul_karatsuba_limbs(ww->p, u->p, u->sz, v->p, v->sz, scratch);
      pool_restore(ctx, pool_state);
    }
    else {
      scratch = (mp_limb*)mrb_malloc(MPZ_MRB(ctx), scratch_size * sizeof(mp_limb));
      mpz_mul_karatsuba_limbs(ww->p, u->p, u->sz, v->p, v->sz, scratch);
      mrb_free(MPZ_MRB(ctx), scratch);
    }

    ww->sz = result_size;
    ww->sn = u->sn * v->sn;
    trim(ww);
    return;
  }

  /*
   * Toom-3 writes at offset 4*third with products up to 2*(third+3)+16 limbs.
   * Maximum write position: 4*n/3 + 2*n/3 + 22 = 2*n + 22, where n = max size.
   * Use 2*max_sz (not u->sz + v->sz) because third is based on max.
   */
  size_t result_size = 2 * max_sz + 24;
  mpz_realloc(ctx, ww, result_size);

  size_t scratch_size = toom3_scratch_size(max_sz);
  /* Add safety margin proportional to scratch size. */
  scratch_size += (scratch_size >> 3) + 16;
  size_t pool_state = pool_save(ctx);
  mp_limb *scratch = NULL;

  if (MPZ_HAS_POOL(ctx)) {
    scratch = pool_alloc(MPZ_POOL(ctx), scratch_size);
  }

  if (scratch) {
    mpz_mul_toom3(ctx, ww->p, u->p, u->sz, v->p, v->sz, scratch);
    pool_restore(ctx, pool_state);
  }
  else {
    /* Fallback to heap allocation for scratch space if pool fails */
    scratch = (mp_limb*)mrb_malloc(MPZ_MRB(ctx), scratch_size * sizeof(mp_limb));
    mpz_mul_toom3(ctx, ww->p, u->p, u->sz, v->p, v->sz, scratch);
    mrb_free(MPZ_MRB(ctx), scratch);
  }

  ww->sz = result_size;
  ww->sn = u->sn * v->sn;
  trim(ww);
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

/*
 * mpn-style low-level shift functions.
 * These operate directly on limb arrays without mpz_t overhead.
 */

/*
 * Right shift limb array by cnt bits (0 < cnt < DIG_SIZE).
 * rp[0..n-1] = ap[0..n-1] >> cnt
 * Returns the bits shifted out from the low end.
 * Supports in-place operation (rp == ap).
 *
 * Processing order: LOW to HIGH
 * - rp[i] depends on ap[i] and ap[i+1]
 * - Writing rp[i] doesn't affect ap[i+1], so in-place is safe
 */
static mp_limb
mpn_rshift(mp_limb *rp, const mp_limb *ap, size_t n, unsigned int cnt)
{
  mp_limb shifted_out;
  size_t i;

  mrb_assert(cnt > 0 && cnt < DIG_SIZE);

  shifted_out = (ap[0] << (DIG_SIZE - cnt)) & DIG_MASK;

  for (i = 0; i < n - 1; i++) {
    rp[i] = ((ap[i] >> cnt) | (ap[i + 1] << (DIG_SIZE - cnt))) & DIG_MASK;
  }
  rp[n - 1] = (ap[n - 1] >> cnt) & DIG_MASK;

  return shifted_out;
}

/*
 * Left shift limb array by cnt bits (0 < cnt < DIG_SIZE).
 * rp[0..n-1] = ap[0..n-1] << cnt (lower n limbs)
 * Returns the bits shifted out from the high end (carry).
 * Supports in-place operation (rp == ap).
 *
 * Processing order: HIGH to LOW
 * - rp[i] depends on ap[i] and ap[i-1]
 * - Writing rp[i] doesn't affect ap[i-1], so in-place is safe
 */
static mp_limb
mpn_lshift(mp_limb *rp, const mp_limb *ap, size_t n, unsigned int cnt)
{
  mp_limb carry;
  size_t i;

  mrb_assert(cnt > 0 && cnt < DIG_SIZE);

  carry = (ap[n - 1] >> (DIG_SIZE - cnt)) & DIG_MASK;

  for (i = n - 1; i > 0; i--) {
    rp[i] = ((ap[i] << cnt) | (ap[i - 1] >> (DIG_SIZE - cnt))) & DIG_MASK;
  }
  rp[0] = (ap[0] << cnt) & DIG_MASK;

  return carry;
}

/*
 * Add two limb arrays of the same size.
 * rp[0..n-1] = ap[0..n-1] + bp[0..n-1]
 * Returns carry (0 or 1).
 * Supports in-place operation (rp == ap or rp == bp).
 */
static mp_limb
mpn_add_n(mp_limb *rp, const mp_limb *ap, const mp_limb *bp, size_t n)
{
  mp_dbl_limb carry = 0;

  for (size_t i = 0; i < n; i++) {
    carry += (mp_dbl_limb)ap[i] + (mp_dbl_limb)bp[i];
    rp[i] = LOW(carry);
    carry = HIGH(carry);
  }
  return (mp_limb)carry;
}

/*
 * Add two limb arrays of different sizes.
 * rp[0..an-1] = ap[0..an-1] + bp[0..bn-1]
 * Precondition: an >= bn
 * Returns carry (0 or 1).
 * Supports in-place operation (rp == ap).
 */
static mp_limb
mpn_add(mp_limb *rp, const mp_limb *ap, size_t an, const mp_limb *bp, size_t bn)
{
  mp_dbl_limb carry;

  mrb_assert(an >= bn);

  /* Add overlapping part */
  carry = mpn_add_n(rp, ap, bp, bn);

  /* Propagate carry through remaining limbs of ap */
  for (size_t i = bn; i < an; i++) {
    carry += (mp_dbl_limb)ap[i];
    rp[i] = LOW(carry);
    carry = HIGH(carry);
  }
  return (mp_limb)carry;
}

/*
 * Subtract two limb arrays of the same size.
 * rp[0..n-1] = ap[0..n-1] - bp[0..n-1]
 * Returns borrow (0 or 1).
 * Supports in-place operation (rp == ap).
 */
static mp_limb
mpn_sub_n(mp_limb *rp, const mp_limb *ap, const mp_limb *bp, size_t n)
{
  mp_dbl_limb_signed borrow = 0;

  for (size_t i = 0; i < n; i++) {
    borrow += (mp_dbl_limb_signed)ap[i] - (mp_dbl_limb_signed)bp[i];
    rp[i] = LOW(borrow);
    borrow = HIGH(borrow);
  }
  return (mp_limb)(-borrow);
}

/*
 * Subtract two limb arrays of different sizes.
 * rp[0..an-1] = ap[0..an-1] - bp[0..bn-1]
 * Precondition: an >= bn and ap >= bp (result is non-negative)
 * Returns borrow (0 or 1, should be 0 if precondition met).
 * Supports in-place operation (rp == ap).
 */
static mp_limb
mpn_sub(mp_limb *rp, const mp_limb *ap, size_t an, const mp_limb *bp, size_t bn)
{
  mp_dbl_limb_signed borrow;

  mrb_assert(an >= bn);

  /* Subtract overlapping part */
  borrow = -(mp_dbl_limb_signed)mpn_sub_n(rp, ap, bp, bn);

  /* Propagate borrow through remaining limbs of ap */
  for (size_t i = bn; i < an; i++) {
    borrow += (mp_dbl_limb_signed)ap[i];
    rp[i] = LOW(borrow);
    borrow = HIGH(borrow);
  }
  return (mp_limb)(-borrow);
}

/* c1 = a>>n */
/* n must be < DIG_SIZE */
static void
urshift(mpz_ctx_t *ctx, mpz_t *c1, mpz_t *a, size_t n)
{
  mrb_assert(n < DIG_SIZE);

  if (n == 0) {
    mpz_set(ctx, c1, a);
    trim(c1);
  }
  else if (uzero_p(a)) {
    zero(c1);
  }
  else {
    mpz_realloc(ctx, c1, a->sz);
    mpn_rshift(c1->p, a->p, a->sz, (unsigned int)n);
    c1->sz = a->sz;
    trim(c1);
  }
}

/* c1 = a<<n */
/* n must be < DIG_SIZE */
static void
ulshift(mpz_ctx_t *ctx, mpz_t *c1, mpz_t *a, size_t n)
{
  mrb_assert(n < DIG_SIZE);
  if (n == 0) {
    mpz_set(ctx, c1, a);
    trim(c1);
  }
  else if (uzero_p(a)) {
    zero(c1);
  }
  else if (c1 == a) {
    /* In-place optimization: mpn_lshift works from high to low, safe for aliasing */
    mp_limb carry;
    size_t old_sz = a->sz;

    mpz_realloc(ctx, c1, old_sz + 1);
    carry = mpn_lshift(c1->p, c1->p, old_sz, (unsigned int)n);
    c1->p[old_sz] = carry;
    c1->sz = old_sz + 1;
    trim(c1);
  }
  else {
    mpz_t c;
    mp_limb carry;

    mpz_init_heap(ctx, &c, a->sz + 1);
    carry = mpn_lshift(c.p, a->p, a->sz, (unsigned int)n);
    c.p[a->sz] = carry;
    c.sz = a->sz + 1;
    trim(&c);
    mpz_move(ctx, c1, &c);
  }
}

/* Fast division by single limb */
static void
div_limb(mpz_ctx_t *ctx, mpz_t *q, mpz_t *r, mpz_t *x, mp_limb d)
{
  mrb_state *mrb = MPZ_MRB(ctx);
  size_t pool_state = pool_save(ctx);
  mpz_t temp_q, temp_r;
  size_t n;
  mp_dbl_limb remainder;

  if (zero_p(x)) {
    zero(q);
    zero(r);
    goto cleanup;
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
      mpz_init(ctx, &temp_q);
      mpz_init(ctx, &temp_r);
      mpz_set(ctx, &temp_q, x);
    }
    else {
      /* Manual right shift implementation */
      size_t limb_shift = shift / DIG_SIZE;
      size_t bit_shift = shift % DIG_SIZE;

      if (limb_shift >= x->sz) {
        mpz_init(ctx, &temp_q);
        mpz_init(ctx, &temp_r);
        zero(&temp_q);
      }
      else {
        size_t new_size = x->sz - limb_shift;
        mpz_init_temp(ctx, &temp_q, new_size);
        mpz_init(ctx, &temp_r);

        if (bit_shift == 0) {
          /* Simple limb copy */
          for (size_t i = 0; i < new_size; i++) {
            temp_q.p[i] = x->p[i + limb_shift];
          }
        }
        else {
          /* Bit shift within limbs */
          mp_limb carry = 0;
          for (size_t i = new_size; i > 0; i--) {
            mp_limb current = x->p[i - 1 + limb_shift];
            temp_q.p[i - 1] = (current >> bit_shift) | carry;
            carry = (current << (DIG_SIZE - bit_shift)) & DIG_MASK;
          }
        }
        temp_q.sz = new_size;
        trim(&temp_q);
        temp_q.sn = (temp_q.sz == 0) ? 0 : 1;
      }
    }

    /* Remainder = x & (d - 1) */
    /* temp_r is already initialized in all code paths above */
    mpz_realloc(ctx, &temp_r, 1);
    temp_r.p[0] = x->p[0] & (d - 1);
    temp_r.sz = (temp_r.p[0] == 0) ? 0 : 1;
    temp_r.sn = (temp_r.sz == 0) ? 0 : 1;
    mpz_move(ctx, q, &temp_q);
    mpz_move(ctx, r, &temp_r);
    goto cleanup;
  }

  /* General single-limb division */
  if (x->sz == 1) {
    /* Both dividend and divisor are single limb */
    mpz_init_temp(ctx, &temp_q, 1);
    mpz_init_temp(ctx, &temp_r, 1);

    temp_q.p[0] = x->p[0] / d;
    temp_r.p[0] = x->p[0] % d;

    temp_q.sz = (temp_q.p[0] == 0) ? 0 : 1;
    temp_q.sn = (temp_q.sz == 0) ? 0 : 1;

    temp_r.sz = (temp_r.p[0] == 0) ? 0 : 1;
    temp_r.sn = (temp_r.sz == 0) ? 0 : 1;
    mpz_move(ctx, q, &temp_q);
    mpz_move(ctx, r, &temp_r);
    goto cleanup;
  }

  /* Multi-limb dividend, single-limb divisor */
  n = x->sz;
  mpz_init_temp(ctx, &temp_q, n);
  mpz_init_temp(ctx, &temp_r, 1);

  remainder = 0;

  /* Process from most significant limb to least significant */
  for (size_t i = n; i > 0; i--) {
    remainder = (remainder << DIG_SIZE) + x->p[i-1];
    temp_q.p[i-1] = (mp_limb)(remainder / d);
    remainder = remainder % d;
  }

  /* Set remainder */
  temp_r.p[0] = (mp_limb)remainder;
  temp_r.sz = (remainder == 0) ? 0 : 1;
  temp_r.sn = (temp_r.sz == 0) ? 0 : 1;

  /* Trim leading zeros from quotient */
  trim(&temp_q);
  temp_q.sn = (temp_q.sz == 0) ? 0 : 1;

  /* Copy results to avoid pool/heap mixing */
  mpz_move(ctx, q, &temp_q);
  mpz_move(ctx, r, &temp_r);

cleanup:
  pool_restore(ctx, pool_state);
}

static void
udiv(mpz_ctx_t *ctx, mpz_t *qq, mpz_t *rr, mpz_t *xx, mpz_t *yy)
{
  /* Handle simple cases */
  int cmp = ucmp(xx, yy);
  if (cmp == 0) {
    mpz_set_int(ctx, qq, 1);
    zero(rr);
    return;
  }
  else if (cmp < 0) {
    zero(qq);
    mpz_set(ctx, rr, xx);
    return;
  }

  /* Fast path for single-limb divisor */
  if (yy->sz == 1) {
    div_limb(ctx, qq, rr, xx, yy->p[0]);
    return;
  }

  mrb_assert(yy->sn != 0);      /* divided by zero */
  mrb_assert(yy->sz > 0);       /* divided by zero */
  mrb_assert(!uzero_p(yy));     /* divided by zero */

  /* Pre-check size constraints to avoid memory leaks on exception.
   * Check both dividend and divisor sizes as ulshift uses size+1 for both.
   * Fail early before any allocation to prevent leaks. */
  size_t max_mpz_size = SIZE_MAX / sizeof(mp_limb);
  if (xx->sz + 1 > max_mpz_size || yy->sz + 1 > max_mpz_size) {
    mrb_state *mrb = MPZ_MRB(ctx);
    mrb_raise(mrb, E_RUNTIME_ERROR, "bigint size too large");
  }

  /* Use new context architecture with automatic pool/heap management */
  size_t pool_state = pool_save(ctx);
  mpz_t q, x, y;
  mpz_init_temp(ctx, &q, xx->sz - yy->sz + 1);  /* Quotient size estimate */
  mpz_init_temp(ctx, &x, xx->sz + 1);           /* Dividend with potential carry */
  mpz_init_temp(ctx, &y, yy->sz);               /* Divisor copy */
  mpz_realloc(ctx, &x, xx->sz+1);
  size_t yd = digits(yy);
  size_t ns = lzb(yy->p[yd-1]);
  ulshift(ctx, &x, xx, ns);
  ulshift(ctx, &y, yy, ns);
  trim(&y);  /* Trim after shift to remove any zero limbs */
  size_t xd = digits(&x);
  yd = digits(&y);

  /* Handle edge case: divisor became zero after normalization */
  if (yd == 0 || y.p[yd-1] == 0) {
    /* This should not happen with valid inputs, but handle gracefully */
    zero(qq);
    zero(rr);
    mpz_clear(ctx, &q);
    mpz_clear(ctx, &x);
    mpz_clear(ctx, &y);
    pool_restore(ctx, pool_state);
    return;
  }

  mpz_realloc(ctx, &q, xd-yd+1);  // Quotient has xd-yd+1 digits maximum

  /* Core Knuth Algorithm D division loop */
  mp_dbl_limb z = y.p[yd-1];
  mrb_assert(z != 0);  /* Divisor high limb must be non-zero after normalization */

  if (xd >= yd) {
    for (size_t j = xd - yd;; j--) {
      mp_dbl_limb qhat;
      mp_dbl_limb rhat;

      if (j + yd == xd) {
        /* Only one high limb available */
        mp_dbl_limb dividend_val = (((mp_dbl_limb)0 << DIG_SIZE) + x.p[j+yd-1]);
        qhat = dividend_val / z;
        rhat = dividend_val % z;
      }
      else {
        /* Two limbs available - standard Knuth estimation */
        mp_dbl_limb dividend_val = ((mp_dbl_limb)x.p[j+yd] << DIG_SIZE) + x.p[j+yd-1];
        qhat = dividend_val / z;
        rhat = dividend_val % z;
      }

      /* Standard Knuth Algorithm D qhat refinement (2-limb check) */
      if (yd >= 2) {
        mp_dbl_limb y_second = y.p[yd-2];
        mp_dbl_limb x_third = (j+yd-2 < x.sz) ? x.p[j+yd-2] : 0;
        mp_dbl_limb left_side = qhat * y_second;
        mp_dbl_limb right_side = (rhat << DIG_SIZE) + x_third;

        while (qhat >= ((mp_dbl_limb)1 << DIG_SIZE) || (left_side > right_side)) {
          qhat--;
          rhat += z;
          if (rhat >= ((mp_dbl_limb)1 << DIG_SIZE)) break;
          left_side -= y_second;
          right_side = (rhat << DIG_SIZE) + x_third;
        }
      }

      if (qhat > 0) {
        /* Subtract qhat * divisor from dividend */
        /* Use pointers to avoid repeated i+j index calculation */
        mp_dbl_limb_signed borrow = 0;
        mp_limb *xp = x.p + j;
        mp_limb *xp_end = x.p + x.sz;
        const mp_limb *yp = y.p;

        for (size_t i = 0; i < yd; i++) {
          mp_dbl_limb product = qhat * *yp++;
          mp_dbl_limb_signed diff = (mp_dbl_limb_signed)*xp - (mp_dbl_limb_signed)LOW(product) + borrow;
          *xp++ = LOW(diff);
          borrow = HIGH(diff) - (mp_dbl_limb_signed)HIGH(product);
        }

        /* Handle final borrow propagation */
        if (xp < xp_end) {
          borrow += (mp_dbl_limb_signed)*xp;
          *xp = LOW(borrow);
          borrow = HIGH(borrow);
        }

        /* Correction: if borrow is negative, qhat was too large, add back */
        if (borrow < 0) {
          qhat--;
          mp_dbl_limb carry = 0;
          xp = x.p + j;
          yp = y.p;
          for (size_t i = 0; i < yd; i++) {
            carry += (mp_dbl_limb)*xp + (mp_dbl_limb)*yp++;
            *xp++ = LOW(carry);
            carry = HIGH(carry);
          }
          if (xp < xp_end && carry > 0) {
            *xp += (mp_limb)carry;
          }
        }
      }

      q.p[j] = (mp_limb)qhat;
      if (j == 0) break;
    }
  }
  x.sz = yd;
  urshift(ctx, rr, &x, ns);
  trim(&q);
  mpz_move(ctx, qq, &q);
  mpz_clear(ctx, &q);
  mpz_clear(ctx, &x);
  mpz_clear(ctx, &y);
  pool_restore(ctx, pool_state);
}

static void
mpz_mdiv(mpz_ctx_t *ctx, mpz_t *q, mpz_t *x, mpz_t *y)
{
  mpz_t r;
  short sn1 = x->sn, sn2 = y->sn, qsign;

  if (zero_p(x)) {
    mpz_init_set_int(ctx, q, 0);
    return;
  }
  mpz_init(ctx, &r);
  udiv(ctx, q, &r, x, y);
  qsign = q->sn = sn1 * sn2;
  if (uzero_p(q))
    q->sn = 0;
  /* now if r != 0 and q < 0 we need to round q towards -inf */
  if (!uzero_p(&r) && qsign < 0) {
    /* add 1 to magnitude */
    mpz_add_int(ctx, q, 1);
    /* force negative sign in case the value of q was zero before rounding */
    q->sn = -1;
  }
  mpz_clear(ctx, &r);
}

static void
mpz_mmod(mpz_ctx_t *ctx, mpz_t *r, mpz_t *x, mpz_t *y)
{
  mpz_t q;
  short sn1 = x->sn, sn2 = y->sn, sn3;

  mpz_init(ctx, &q);
  if (sn1 == 0) {
    zero(r);
    return;
  }
  udiv(ctx, &q, r, x, y);
  mpz_clear(ctx, &q);
  if (uzero_p(r)) {
    r->sn = 0;
    return;
  }
  sn3 = sn1 * sn2;
  if (sn3 > 0)
    r->sn = sn1;
  else if (sn1 < 0 && sn2 > 0) {
    r->sn = 1;
    mpz_sub(ctx, r, y, r);
  }
  else {
    r->sn = 1;
    mpz_add(ctx, r, y, r);
  }
}

static void
mpz_mdivmod(mpz_ctx_t *ctx, mpz_t *q, mpz_t *r, mpz_t *x, mpz_t *y)
{
  short sn1 = x->sn, sn2 = y->sn, qsign;

  if (sn1 == 0) {
    zero(q);
    zero(r);
    return;
  }
  udiv(ctx, q, r, x, y);
  qsign = q->sn = sn1 * sn2;
  if (uzero_p(r)) {
    /* q != 0, since q=r=0 would mean x=0, which was tested above */
    r->sn = 0;
    return;
  }
  if (q->sn > 0)
    r->sn = sn1;
  else if (sn1 < 0 && sn2 > 0) {
    r->sn = 1;
    mpz_sub(ctx, r, y, r);
  }
  else {
    r->sn = 1;
    mpz_add(ctx, r, y, r);
  }
  if (uzero_p(q))
    q->sn = 0;
  /* now if r != 0 and q < 0 we need to round q towards -inf */
  if (!uzero_p(r) && qsign < 0) {
    /* add 1 to magnitude */
    mpz_add_int(ctx, q, 1);
    /* force negative sign in case the value of q was zero before rounding */
    q->sn = -1;
  }
}

/* Fast modular reduction for single-limb modulus */
static void
mpz_mod_limb(mpz_ctx_t *ctx, mpz_t *r, mpz_t *x, mp_limb m)
{
  if (zero_p(x)) {
    zero(r);
    return;
  }

  if (x->sz == 1) {
    /* Single limb case - simple modulo */
    mp_limb result = x->p[0] % m;
    mpz_set_int(ctx, r, result);
    if (result == 0)
      r->sn = 0;
    else
      r->sn = x->sn;
    return;
  }

  /* Multi-limb case - use repeated division */
  mp_dbl_limb remainder = 0;
  for (size_t i = x->sz; i > 0; i--) {
    remainder = (remainder << DIG_SIZE) | x->p[i-1];
    remainder %= m;
  }

  mpz_set_int(ctx, r, (mp_limb)remainder);
  r->sn = x->sn;
  if (remainder == 0)
    r->sn = 0;
}

/* Forward declarations for Barrett reduction functions */
static void mpz_barrett_mu(mpz_ctx_t *ctx, mpz_t *mu, mpz_t *m);
static void mpz_barrett_reduce(mpz_ctx_t *ctx, mpz_t *r, mpz_t *x, mpz_t *m, mpz_t *mu);

static void
mpz_mod(mpz_ctx_t *ctx, mpz_t *r, mpz_t *x, mpz_t *y)
{
  short sn = x->sn;

  if (zero_p(x)) {
    zero(r);
    return;
  }

  /* Fast path for single-limb modulus */
  if (y->sz == 1) {
    mpz_mod_limb(ctx, r, x, y->p[0]);
    if (y->sn < 0) r->sn = -r->sn;
    return;
  }

  /* Barrett reduction for moderate-sized moduli (>= 4 limbs where setup is worthwhile) */
  if (y->sz >= 4 && y->sz <= 16 && x->sz >= y->sz + 2) {
    mpz_t mu;
    mpz_init_temp(ctx, &mu, y->sz + 1);
    mpz_barrett_mu(ctx, &mu, y);
    mpz_realloc(ctx, r, y->sz);
    mpz_barrett_reduce(ctx, r, x, y, &mu);
    r->sn = sn;
    if (uzero_p(r))
      r->sn = 0;
    mpz_clear(ctx, &mu);
    return;
  }

  /* General division fallback */
  mpz_t q;
  mpz_init_temp(ctx, &q, x->sz);
  mpz_realloc(ctx, r, y->sz);
  udiv(ctx, &q, r, x, y);
  r->sn = sn;
  if (uzero_p(r))
    r->sn = 0;
  mpz_clear(ctx, &q);
}

static mrb_int
mpz_cmp(mpz_ctx_t *ctx, mpz_t *x, mpz_t *y)
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
mpz_mul_int(mpz_ctx_t *ctx, mpz_t *x, mrb_int n)
{
  if (n == 0 || zero_p(x)) {
    zero(x);
    return;
  }

  size_t x_sz = x->sz;
  size_t new_sz = x_sz + 1; // Maximum possible size after multiplication

  // Reallocate x if necessary
  mpz_realloc(ctx, x, new_sz);

  mp_dbl_limb cc = 0;
  mp_limb n_limb = (mp_limb)n;

  for (size_t i = 0; i < x_sz; i++) {
    // Multiply each limb and add carry
    cc += (mp_dbl_limb)x->p[i] * n_limb;
    x->p[i] = LOW(cc);
    cc = HIGH(cc);
  }

  if (cc) {
    // If there is a remaining carry, store it and update size
    x->p[x_sz] = (mp_limb)cc;
    x->sz = x_sz + 1;
  }
  else {
    x->sz = x_sz;
  }

  x->sn = 1;
  trim(x);
}

/* Forward declarations and constants for decimal base conversion */
#ifdef MRB_NO_MPZ64BIT
#define DECIMAL_BASE_CONV 10000UL      /* 10^4 for 16-bit limbs */
#define DECIMAL_DIGITS_CONV 4
#else
#define DECIMAL_BASE_CONV 1000000000UL /* 10^9 for 32-bit limbs */
#define DECIMAL_DIGITS_CONV 9
#endif

static size_t mpz_str_to_decimal(const char *s, mrb_int len, mp_limb *decimal_out, mrb_int *effective_len);
static size_t mpz_decimal_to_binary(const mp_limb *decimal, size_t decimal_size, mp_limb *limbs_out);

static int
mpz_init_set_str(mpz_ctx_t *ctx, mpz_t *x, const char *s, mrb_int len, mrb_int base)
{
  int retval = 0;
  short sn;
  uint8_t k;

  mpz_init(ctx, x);
  if (*s == '-') {
    sn = -1; s++; len--;
  }
  else if (*s == '+') {
    sn = 1; s++; len--;
  }
  else
    sn = 1;

  if (base == 10) {
    /* Use optimized decimal parsing: parse 9 digits at a time */
    /* First validate that all characters are valid decimal digits */
    for (mrb_int i = 0; i < len; i++) {
      if (s[i] == '_') continue;
      if (s[i] < '0' || s[i] > '9') {
        retval = (-1);
        break;
      }
    }

    if (retval == 0) {
      /* Estimate size: ceil(len / DECIMAL_DIGITS_CONV) decimal chunks */
      size_t decimal_alloc = (size_t)((len + DECIMAL_DIGITS_CONV - 1) / DECIMAL_DIGITS_CONV) + 1;

      /* Try pool first (no Karatsuba in this path), fall back to malloc */
      size_t pool_state = pool_save(ctx);
      mp_limb *decimal = pool_alloc(MPZ_POOL(ctx), decimal_alloc);
      mrb_bool use_heap = (decimal == NULL);
      if (use_heap) {
        decimal = (mp_limb*)mrb_malloc(MPZ_MRB(ctx), decimal_alloc * sizeof(mp_limb));
      }

      mrb_int effective_len;
      size_t decimal_size = mpz_str_to_decimal(s, len, decimal, &effective_len);

      if (decimal_size > 0) {
        /* Estimate binary limbs needed: roughly (effective_len * 10) / (32 * 3) limbs */
        size_t limb_alloc = (size_t)((effective_len * 4 + 9) / 10) + 2;
        /* Use realloc to reuse x->p buffer, avoiding extra malloc+free */
        x->p = (mp_limb*)mrb_realloc(MPZ_MRB(ctx), x->p, limb_alloc * sizeof(mp_limb));
        memset(x->p, 0, limb_alloc * sizeof(mp_limb));

        size_t limb_size = mpz_decimal_to_binary(decimal, decimal_size, x->p);

        x->sz = (mrb_int)limb_size;
        x->sn = limb_size == 0 ? 0 : sn;
      }

      if (use_heap) {
        mrb_free(MPZ_MRB(ctx), decimal);
      }
      else {
        pool_restore(ctx, pool_state);
      }
    }
  }
  else {
    /* Use schoolbook algorithm for other bases */
    for (mrb_int i = 0; i < len; i++) {
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
      mpz_mul_int(ctx, x, base);
      mpz_add_int(ctx, x, k);
    }
    x->sn = x->sz == 0 ? 0 : sn;
  }

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

/*
 * Divide-and-conquer decimal string conversion.
 * For numbers with > DC_GET_STR_THRESHOLD digits, this is O(n log^2 n)
 * instead of O(n^2) for the simple algorithm.
 */
#define DC_GET_STR_THRESHOLD 700

/*
 * Scratch buffer for D&C get_str to avoid repeated allocations.
 * Contains preallocated work areas that are resized as needed.
 *
 * The lo_stack provides a separate buffer for each recursion depth,
 * preventing overwrites between parent and child calls. Each entry
 * is allocated on first use with appropriate size for that depth.
 */
#define DC_MAX_DEPTH 32  /* Covers up to 2^32 * 1000 digits */

typedef struct {
  mpz_t lo_stack[DC_MAX_DEPTH]; /* Depth-indexed lo buffers */
  mrb_bool lo_init[DC_MAX_DEPTH]; /* Initialized flags per depth */
  mpz_t q5;       /* Quotient from division by 5^k */
  mpz_t r5;       /* Remainder from division by 5^k */
  mpz_t q5_low;   /* Low bits of q5 for mod 2^k */
  mpz_t tmp;      /* Temporary for base case (in-place division) */
  mrb_bool initialized;
} dc_get_str_scratch_t;

static void
dc_scratch_init(mpz_ctx_t *ctx, dc_get_str_scratch_t *scratch, size_t max_limbs)
{
  if (scratch->initialized) return;

  /* lo_stack entries are initialized on demand per depth, not here */
  for (size_t i = 0; i < DC_MAX_DEPTH; i++) {
    scratch->lo_init[i] = FALSE;
  }

  /* Preallocate other scratch buffers with estimated sizes */
  mpz_init_heap(ctx, &scratch->q5, max_limbs);
  mpz_init_heap(ctx, &scratch->r5, max_limbs);
  mpz_init_heap(ctx, &scratch->q5_low, max_limbs);
  mpz_init_heap(ctx, &scratch->tmp, max_limbs);
  scratch->initialized = TRUE;
}

static void
dc_scratch_clear(mpz_ctx_t *ctx, dc_get_str_scratch_t *scratch)
{
  if (!scratch->initialized) return;

  /* Clear lo_stack entries that were initialized */
  for (size_t i = 0; i < DC_MAX_DEPTH; i++) {
    if (scratch->lo_init[i]) {
      mpz_clear(ctx, &scratch->lo_stack[i]);
    }
  }

  mpz_clear(ctx, &scratch->q5);
  mpz_clear(ctx, &scratch->r5);
  mpz_clear(ctx, &scratch->q5_low);
  mpz_clear(ctx, &scratch->tmp);
  scratch->initialized = FALSE;
}

/*
 * Recursive D&C conversion helper.
 * Converts x to decimal string, writing exactly num_digits characters.
 * The caller must ensure num_digits >= actual digits in x.
 * Leading zeros are added if x has fewer digits than num_digits.
 */
/* Batch divisor for extracting multiple digits at once */
#ifdef MRB_NO_MPZ64BIT
/* 16-bit limbs: 10^4 fits in 16 bits */
#define BATCH_DIVISOR 10000UL
#define BATCH_DIGITS 4
#else
/* 32-bit limbs: 10^9 fits in 32 bits */
#define BATCH_DIVISOR 1000000000UL
#define BATCH_DIGITS 9
#endif

/*
 * Divide limb array by BATCH_DIVISOR in place, returning remainder.
 * Used for extracting BATCH_DIGITS decimal digits at a time.
 * Compilers optimize constant division to multiplication+shift.
 */
static mp_limb
mpn_div_batch(mp_limb *p, size_t sz)
{
  mp_dbl_limb rem = 0;
  for (size_t i = sz; i > 0; i--) {
    mp_dbl_limb n = (rem << DIG_SIZE) | p[i-1];
    mp_dbl_limb q = n / BATCH_DIVISOR;
    rem = n - q * BATCH_DIVISOR;
    p[i-1] = (mp_limb)q;
  }
  return (mp_limb)rem;
}

/* Lookup table for fast 2-digit conversion (Lemire's small table technique) */
static const char digit_pairs[] =
  "00010203040506070809"
  "10111213141516171819"
  "20212223242526272829"
  "30313233343536373839"
  "40414243444546474849"
  "50515253545556575859"
  "60616263646566676869"
  "70717273747576777879"
  "80818283848586878889"
  "90919293949596979899";

static void
mpz_get_str_dc_recur(mpz_ctx_t *ctx, char *s, mpz_t *x, size_t num_digits,
                  mpz_t *pow5, size_t num_powers, size_t depth,
                  dc_get_str_scratch_t *scratch)
{
  /* Base case: use simple conversion for small numbers */
  if (num_digits <= DC_GET_STR_THRESHOLD || num_powers == 0) {
    /* Convert to string in reverse order using batch extraction */
    size_t pos = num_digits;

    /* Use scratch buffer for working copy */
    mpz_t *tmp = &scratch->tmp;

    /* Ensure tmp has enough capacity and copy x */
    mpz_realloc(ctx, tmp, x->sz);
    mpz_set(ctx, tmp, x);

    while (pos > 0 && !zero_p(tmp)) {
      /* Divide by BATCH_DIVISOR in place, get remainder as BATCH_DIGITS digits */
      mp_limb batch = mpn_div_batch(tmp->p, tmp->sz);
      trim(tmp);

      /* Convert remainder to BATCH_DIGITS digits using table lookup */
#if (BATCH_DIGITS % 2) == 1
      /* Extract last digit separately since BATCH_DIGITS is odd */
      if (pos > 0) {
        s[--pos] = '0' + (char)(batch % 10);
        batch /= 10;
      }
#endif
      /* Extract remaining digits as pairs using lookup table */
      for (int d = 0; d < BATCH_DIGITS / 2 && pos >= 2; d++) {
        mp_limb pair = batch % 100;
        batch /= 100;
        s[--pos] = digit_pairs[pair * 2 + 1];
        s[--pos] = digit_pairs[pair * 2];
      }
      /* Extract any remaining single digit when pos == 1 */
      if (pos > 0 && batch > 0) {
        s[--pos] = '0' + (char)(batch % 10);
      }
    }

    /* Fill remaining positions with zeros */
    while (pos > 0) {
      s[--pos] = '0';
    }

    return;
  }

  /* Find appropriate power of 10 to split on */
  /* We want the largest power that gives roughly half the digits */
  size_t split_idx = 0;
  size_t split_digits = 1;
  for (size_t i = 0; i < num_powers; i++) {
    size_t d = (size_t)1 << i;  /* digits for this power */
    if (d * 2 <= num_digits) {
      split_idx = i;
      split_digits = d;
    }
  }

  /*
   * Optimization: Use the factorization 10^k = 2^k * 5^k
   *
   * Instead of dividing by 10^k directly:
   * 1. Divide by 5^k (smaller divisor = faster division)
   * 2. Use bit operations to handle the 2^k part
   *
   * If x = hi * 10^k + lo, and we compute q5 = x / 5^k, r5 = x % 5^k:
   *   hi = q5 >> k  (right shift by k bits)
   *   lo = (q5 & ((1<<k)-1)) * 5^k + r5
   */

  /* Reuse scratch buffers: q5, r5, q5_low */
  mpz_t *q5 = &scratch->q5;
  mpz_t *r5 = &scratch->r5;
  mpz_t *q5_low = &scratch->q5_low;

  /* Step 1: Divide by 5^k (cheaper than dividing by 10^k) */
  mpz_mdivmod(ctx, q5, r5, x, &pow5[split_idx]);
  r5->sn = (r5->sn < 0) ? -r5->sn : r5->sn;

  /* Step 2: Extract q5_low = q5 mod 2^k BEFORE modifying q5 */
  mpz_mod_2exp(ctx, q5_low, q5, (mrb_int)split_digits);

  /* Step 3: hi = q5 >> k - shift q5 in place to reuse as hi */
  /* This avoids allocating a separate hi buffer */
  mpz_t *hi = q5;  /* Reuse q5 as hi after shifting */
  {
    size_t limb_shift = split_digits / DIG_SIZE;
    size_t bit_shift = split_digits % DIG_SIZE;

    if (limb_shift >= q5->sz) {
      zero(q5);
    }
    else {
      size_t new_sz = q5->sz - limb_shift;
      /* Shift limbs down */
      memmove(q5->p, q5->p + limb_shift, new_sz * sizeof(mp_limb));
      q5->sz = new_sz;
      /* Apply bit shift if needed */
      if (bit_shift > 0) {
        mpn_rshift(q5->p, q5->p, q5->sz, (unsigned int)bit_shift);
      }
      trim(q5);
    }
  }

  /* Step 4: lo = q5_low * 5^k + r5 - use depth-indexed lo buffer */
  if (depth >= DC_MAX_DEPTH) {
    /* Fallback: allocate fresh if depth exceeds limit (shouldn't happen) */
    mpz_t lo;
    mpz_init(ctx, &lo);
    mpz_mul(ctx, &lo, q5_low, &pow5[split_idx]);
    mpz_add(ctx, &lo, &lo, r5);
    lo.sn = (lo.sn < 0) ? -lo.sn : lo.sn;

    size_t hi_digits = num_digits - split_digits;
    mpz_get_str_dc_recur(ctx, s, hi, hi_digits, pow5, split_idx, depth + 1, scratch);
    mpz_get_str_dc_recur(ctx, s + hi_digits, &lo, split_digits, pow5, split_idx, depth + 1, scratch);
    mpz_clear(ctx, &lo);
    return;
  }

  /* Initialize lo_stack[depth] on first use at this depth */
  mpz_t *lo = &scratch->lo_stack[depth];
  if (!scratch->lo_init[depth]) {
    /* Allocate with appropriate size for this depth level */
    /* At depth d, lo is roughly x->sz / 2^(d+1) limbs, plus some margin */
    size_t est_limbs = (x->sz >> 1) + 2;
    mpz_init_heap(ctx, lo, est_limbs);
    scratch->lo_init[depth] = TRUE;
  }

  mpz_mul(ctx, lo, q5_low, &pow5[split_idx]);
  mpz_add(ctx, lo, lo, r5);
  lo->sn = (lo->sn < 0) ? -lo->sn : lo->sn;

  /* Recursively convert high part (using q5 which now contains hi) */
  size_t hi_digits = num_digits - split_digits;
  mpz_get_str_dc_recur(ctx, s, hi, hi_digits, pow5, split_idx, depth + 1, scratch);

  /* Recursively convert low part (exactly split_digits digits with padding) */
  mpz_get_str_dc_recur(ctx, s + hi_digits, lo, split_digits, pow5, split_idx, depth + 1, scratch);
}

/*
 * D&C decimal string conversion entry point.
 * Returns pointer to start of string (after optional sign).
 *
 * Optimization: Uses 10^k = 2^k * 5^k factorization.
 * Dividing by 5^k is ~30% faster than dividing by 10^k because
 * 5^k has fewer bits (2.32k vs 3.32k). The 2^k part is handled
 * with fast bit shifts.
 */
#define MAX_POWERS 64

struct mpz_get_str_dc_data {
  mpz_ctx_t *ctx;
  char *s;          /* output buffer (after sign) */
  mpz_t *x;
  size_t num_digits;
  mpz_t pow5[MAX_POWERS];
  mpz_t tmp;
  dc_get_str_scratch_t scratch;
  size_t num_powers; /* cleanup target count */
};

static mrb_value
mpz_get_str_dc_body(mrb_state *mrb, void *userdata)
{
  struct mpz_get_str_dc_data *d = (struct mpz_get_str_dc_data *)userdata;
  mpz_ctx_t *ctx = d->ctx;
  char *s = d->s;
  mpz_t *x = d->x;
  size_t num_digits = d->num_digits;

  /* 5^1 */
  mpz_init(ctx, &d->pow5[0]);
  mpz_set_int(ctx, &d->pow5[0], 5);
  d->num_powers = 1;

  /* Build powers by squaring: 5^(2^k) = (5^(2^(k-1)))^2 */
  while (d->num_powers < MAX_POWERS) {
    size_t power_digits = (size_t)1 << d->num_powers;
    if (power_digits > num_digits) break;

    mpz_init(ctx, &d->pow5[d->num_powers]);
    mpz_sqr(ctx, &d->pow5[d->num_powers], &d->pow5[d->num_powers - 1]);
    d->num_powers++;
  }

  /* Make a copy of x for conversion (to preserve original) */
  mpz_init_set(ctx, &d->tmp, x);
  d->tmp.sn = 1;  /* Work with absolute value */

  /* Initialize scratch buffers for base case optimization */
  dc_scratch_init(ctx, &d->scratch, x->sz);

  /* Do the recursive conversion (starting at depth 0) */
  mpz_get_str_dc_recur(ctx, s, &d->tmp, num_digits, d->pow5, d->num_powers, 0, &d->scratch);

  /* Null-terminate the string */
  s[num_digits] = '\0';

  return mrb_nil_value();
}

static char*
mpz_get_str_dc(mpz_ctx_t *ctx, char *s, mpz_t *x)
{
  /* Handle sign */
  char *result = s;
  if (x->sn < 0) {
    *s++ = '-';
  }

  /* Calculate number of decimal digits needed */
  /* Use log10(2) ≈ 0.30103, so bits * 0.30103 + 1 gives upper bound */
  size_t bits = digits(x) * DIG_SIZE;
  size_t num_digits = (size_t)(bits * 30103UL / 100000UL) + 2;

  struct mpz_get_str_dc_data d;
  memset(&d, 0, sizeof(d));
  d.ctx = ctx;
  d.s = s;
  d.x = x;
  d.num_digits = num_digits;
  d.num_powers = 0;

  mrb_bool error = FALSE;
  mrb_value exc = mrb_protect_error(MPZ_MRB(ctx), mpz_get_str_dc_body, &d, &error);

  /* Cleanup always runs (mpz_clear is safe on zero-initialized mpz_t) */
  for (size_t i = 0; i < d.num_powers; i++) {
    mpz_clear(ctx, &d.pow5[i]);
  }
  mpz_clear(ctx, &d.tmp);
  dc_scratch_clear(ctx, &d.scratch);

  if (error) {
    mrb_exc_raise(MPZ_MRB(ctx), exc);
  }

  /* Remove leading zeros (but keep at least one digit) */
  char *p = s;
  while (*p == '0' && *(p+1) != '\0') p++;
  if (p > s) {
    memmove(s, p, strlen(p) + 1);
  }

  return result;
}

/*
 * CPython-style base conversion for decimal strings.
 *
 * Converts from base 2^DIG_SIZE to base 10^k (DECIMAL_BASE_CONV).
 * This is faster than repeated division because:
 * - Each input limb is processed only once (MSB to LSB)
 * - The inner loop multiplies/divides by constants (compiler optimizes)
 * - Fewer total operations than dividing entire number repeatedly
 *
 * Returns number of decimal base digits written to decimal_out.
 */
static size_t
mpz_base_convert_decimal(const mp_limb *limbs, size_t size, mp_limb *decimal_out)
{
  size_t decimal_size = 0;

  /* Process input limbs from MSB to LSB */
  for (size_t i = size; i > 0; i--) {
    mp_limb hi = limbs[i - 1];

    /* Multiply existing decimal digits by 2^DIG_SIZE and add hi */
    for (size_t j = 0; j < decimal_size; j++) {
      mp_dbl_limb z = ((mp_dbl_limb)decimal_out[j] << DIG_SIZE) | hi;
      hi = (mp_limb)(z / DECIMAL_BASE_CONV);
      decimal_out[j] = (mp_limb)(z - (mp_dbl_limb)hi * DECIMAL_BASE_CONV);
    }

    /* Handle remaining carries into new decimal digits */
    while (hi) {
      decimal_out[decimal_size++] = hi % DECIMAL_BASE_CONV;
      hi /= DECIMAL_BASE_CONV;
    }
  }

  return decimal_size;
}

/*
 * Parse decimal string into decimal-base array.
 * Parses in chunks of DECIMAL_DIGITS_CONV digits from right to left.
 * Returns number of decimal base digits written to decimal_out.
 * Also returns the effective string length (excluding underscores) via *effective_len.
 */
static size_t
mpz_str_to_decimal(const char *s, mrb_int len, mp_limb *decimal_out, mrb_int *effective_len)
{
  /* First pass: count effective digits (excluding underscores) */
  mrb_int eff_len = 0;
  for (mrb_int i = 0; i < len; i++) {
    if (s[i] != '_') eff_len++;
  }
  *effective_len = eff_len;

  if (eff_len == 0) return 0;

  /* Parse from right to left in chunks of DECIMAL_DIGITS_CONV digits */
  size_t decimal_size = 0;
  mrb_int pos = len - 1;
  mrb_int digits_in_chunk = 0;
  mp_limb chunk = 0;
  mp_limb multiplier = 1;

  while (pos >= 0) {
    char c = s[pos--];
    if (c == '_') continue;

    /* Accumulate digit into chunk */
    mp_limb digit = (mp_limb)(c - '0');
    chunk += digit * multiplier;
    multiplier *= 10;
    digits_in_chunk++;

    if (digits_in_chunk == DECIMAL_DIGITS_CONV) {
      decimal_out[decimal_size++] = chunk;
      chunk = 0;
      multiplier = 1;
      digits_in_chunk = 0;
    }
  }

  /* Handle remaining partial chunk (MSB group) */
  if (digits_in_chunk > 0) {
    decimal_out[decimal_size++] = chunk;
  }

  return decimal_size;
}

/*
 * Convert decimal-base representation to binary limbs.
 * decimal[]: array of values < DECIMAL_BASE_CONV, LSB first
 * This is the reverse of mpz_base_convert_decimal.
 * Returns number of binary limbs written to limbs_out.
 */
static size_t
mpz_decimal_to_binary(const mp_limb *decimal, size_t decimal_size, mp_limb *limbs_out)
{
  if (decimal_size == 0) return 0;

  size_t limb_size = 0;

  /* Process decimal digits from MSB to LSB */
  for (size_t i = decimal_size; i > 0; i--) {
    mp_limb d = decimal[i - 1];

    /* Multiply existing binary limbs by DECIMAL_BASE_CONV and add d */
    mp_dbl_limb carry = d;
    for (size_t j = 0; j < limb_size; j++) {
      mp_dbl_limb z = (mp_dbl_limb)limbs_out[j] * DECIMAL_BASE_CONV + carry;
      limbs_out[j] = (mp_limb)z;
      carry = z >> DIG_SIZE;
    }

    /* Handle remaining carry into new limbs */
    while (carry) {
      limbs_out[limb_size++] = (mp_limb)carry;
      carry >>= DIG_SIZE;
    }
  }

  return limb_size;
}

/*
 * Convert decimal base representation to string.
 * decimal[]: array of values < DECIMAL_BASE_CONV, LSB first
 * Returns pointer past last character written.
 */
static char*
mpz_decimal_to_str(const mp_limb *decimal, size_t decimal_size, char *str)
{
  if (decimal_size == 0) {
    *str++ = '0';
    return str;
  }

  char *s = str;

  /* Output all but MSB group with exactly DECIMAL_DIGITS_CONV digits each */
  /* Use digit_pairs for 2 digits at a time (Lemire's small table technique) */
  for (size_t i = 0; i < decimal_size - 1; i++) {
    mp_limb d = decimal[i];
    for (int j = 0; j < DECIMAL_DIGITS_CONV / 2; j++) {
      mp_limb pair = d % 100;
      d /= 100;
      *s++ = digit_pairs[pair * 2 + 1];
      *s++ = digit_pairs[pair * 2];
    }
#if (DECIMAL_DIGITS_CONV & 1)
    /* Handle odd digit (9th digit for 32-bit limbs) */
    *s++ = '0' + (char)d;
#endif
  }

  /* Output MSB group without leading zeros, 2 digits at a time */
  mp_limb d = decimal[decimal_size - 1];
  do {
    if (d >= 10) {
      mp_limb pair = d % 100;
      d /= 100;
      *s++ = digit_pairs[pair * 2 + 1];
      *s++ = digit_pairs[pair * 2];
    }
    else {
      *s++ = '0' + (char)d;
      break;
    }
  } while (d > 0);

  return s;
}

static char*
mpz_get_str(mpz_ctx_t *ctx, char *s, mrb_int sz, mrb_int base, mpz_t *x)
{
  mrb_state *mrb = MPZ_MRB(ctx);

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
    while (((uint64_t)1 << shift) < (uint64_t)base) shift++;
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
    if ((size_t)xlen > SIZE_MAX / sizeof(mp_limb)) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "bigint size too large for string conversion");
    }

    /* Use D&C algorithm for large base-10 numbers */
    size_t est_digits = (size_t)(xlen * DIG_SIZE * 30103UL / 100000UL) + 2;
    if (base == 10 && est_digits > DC_GET_STR_THRESHOLD) {
      return mpz_get_str_dc(ctx, s, x);
    }

    if (base == 10) {
      /* Use CPython-style base conversion for decimal (faster) */
      size_t decimal_alloc = (size_t)xlen * 2 + 2;
      mp_limb *decimal = (mp_limb*)mrb_malloc(mrb, decimal_alloc * sizeof(mp_limb));
      memset(decimal, 0, decimal_alloc * sizeof(mp_limb));

      size_t dsz = mpz_base_convert_decimal(x->p, (size_t)xlen, decimal);
      s = mpz_decimal_to_str(decimal, dsz, s);

      mrb_free(mrb, decimal);
    }
    else {
      /* Use division-style for other bases */
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
        for (mp_limb b=b2; b>=base; b/=(mp_limb)base) {
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

#ifdef MRB_NO_MPZ64BIT
  /* When using 16-bit limbs, we need to handle larger accumulation */
  mrb_uint i = 0;
  mp_limb *d = y->p + y->sz;

  while (d-- > y->p) {
    /* Check for overflow before shifting */
    if (i > (mrb_uint)(MRB_INT_MAX >> DIG_SIZE)) {
      return FALSE;
    }
    i = (i << DIG_SIZE) | *d;
  }

  if (i > (mrb_uint)MRB_INT_MAX) {
    return FALSE;
  }
#else
  /* Original logic for 32-bit limbs */
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
#endif

  if (y->sn < 0) {
    *v = -(mrb_int)i;
  }
  else {
    *v = (mrb_int)i;
  }
  return TRUE;
}

/* Maximum bits for bigint operations (128MB of limb data) */
#define MRB_BIGINT_BIT_LIMIT (128 * 1024 * 1024 * (size_t)8)

static void
mpz_mul_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e)
{
  if (e==0) {
    mpz_set(ctx, z, x);
    trim(z);
  }
  else {
    short sn = x->sn;
    size_t digs = e / DIG_SIZE;
    size_t bs = e % DIG_SIZE;
    mpz_t y;

    /* Check for result size overflow before allocation */
    size_t result_bits = (size_t)x->sz * DIG_SIZE + (size_t)e;
    if ((size_t)e > MRB_BIGINT_BIT_LIMIT || result_bits > MRB_BIGINT_BIT_LIMIT) {
      mrb_state *mrb = MPZ_MRB(ctx);
      mrb_raise(mrb, E_RANGE_ERROR, "shift width too large");
    }
    mpz_init_heap(ctx, &y, x->sz+digs);
    for (size_t i=0;i<x->sz;i++)
      y.p[i+digs] = x->p[i];
    if (bs) {
      ulshift(ctx, z, &y, bs);
      mpz_clear(ctx, &y);
    }
    else {
      mpz_move(ctx, z, &y);
      trim(z);
    }
    if (uzero_p(z))
      z->sn = 0;
    else
      z->sn = sn;
  }
}

static void
mpz_div_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e)
{
  short sn = x->sn;
  if (e == 0) {
    if (z != x) {
      mpz_clear(ctx, z);
      mpz_init_heap(ctx, z, x->sz);
      mpz_set(ctx, z, x);
      trim(z);
    }
    /* else: z == x, nothing to do */
  }
  else {
    size_t digs = e / DIG_SIZE;
    size_t bs = e % DIG_SIZE;

    /* If shifting by more limbs than we have, result is zero */
    if (digs >= x->sz) {
      zero(z);
      return;
    }

    size_t new_size = x->sz - digs;

    /* In-place optimization: when z == x, no allocation needed */
    if (z == x) {
      /* Shift by whole limbs: memmove in place */
      if (digs > 0) {
        memmove(z->p, z->p + digs, new_size * sizeof(mp_limb));
      }
      z->sz = new_size;
      /* Shift by remaining bits: mpn_rshift supports in-place */
      if (bs) {
        mpn_rshift(z->p, z->p, new_size, (unsigned int)bs);
      }
      trim(z);
      if (uzero_p(z))
        z->sn = 0;
      else
        z->sn = sn;
      return;
    }

    /* General case: z != x, use temporary */
    mpz_t y;
    mpz_init_temp(ctx, &y, new_size);
    mpz_realloc(ctx, &y, new_size);
    for (size_t i = 0; i < new_size; i++)
      y.p[i] = x->p[i + digs];
    if (bs) {
      mpz_clear(ctx, z);
      mpz_init_heap(ctx, z, new_size);
      urshift(ctx, z, &y, bs);
      mpz_clear(ctx, &y);
    }
    else {
      mpz_move(ctx, z, &y);
      trim(z);
    }
    if (uzero_p(z))
      z->sn = 0;
    else {
      z->sn = sn;
    }
  }
}

static void
mpz_neg(mpz_ctx_t *ctx, mpz_t *x, mpz_t *y)
{
  /* In-place optimization: just flip the sign */
  if (x == y) {
    x->sn = -(y->sn);
    return;
  }
  mpz_init_heap(ctx, x, y->sz);
  mpz_set(ctx, x, y);
  trim(x);
  x->sn = -(y->sn);
}

/* Fast modular reduction by power of 2: z = x mod 2^e */
static void
mpz_mod_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e)
{
  if (e <= 0) {
    mpz_clear(ctx, z);
    mpz_init(ctx, z);
    zero(z);
    return;
  }

  size_t eint = e / DIG_SIZE;
  size_t bs = e % DIG_SIZE;
  size_t sz = x->sz;

  if (eint >= sz) {
    /* x < 2^e, so x mod 2^e = x */
    if (z != x) {
      mpz_clear(ctx, z);
      mpz_init_heap(ctx, z, x->sz);
      mpz_set(ctx, z, x);
      trim(z);
    }
    return;
  }

  /* Need to mask off high bits */
  size_t result_sz = eint + (bs > 0 ? 1 : 0);

  /* Handle the case where z == x (in-place operation) */
  if (z == x) {
    /* In-place modification */
    z->sz = result_sz;

    /* Mask partial limb if needed */
    if (bs > 0) {
      mp_limb mask = (1UL << bs) - 1;
      z->p[eint] &= mask;
    }
  }
  else {
    /* z != x, need to copy */
    mpz_clear(ctx, z);
    mpz_init_heap(ctx, z, result_sz);
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
  }

  trim(z);
}

#define make_2comp(v,c) do { v=~(v)+(c); c=((v)==0 && (c));} while (0)

static void
mpz_and(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mpz_t *y)
{
  if (zero_p(x) || zero_p(y)) {
    mpz_init(ctx, z);
    zero(z);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  /* Fast path: both positive - just AND the limbs */
  if (x->sn > 0 && y->sn > 0) {
    size_t min_sz = (x->sz < y->sz) ? x->sz : y->sz;
    mpz_init_heap(ctx, z, min_sz);
    for (size_t i = 0; i < min_sz; i++) {
      z->p[i] = x->p[i] & y->p[i];
    }
    z->sn = 1;
    trim(z);
    return;
  }

  /* Slow path: at least one negative operand */
  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;
  mpz_init_heap(ctx, z, max_sz);
  z->sn = (x->sn < 0 && y->sn < 0) ? -1 : 1;

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
  trim(z);
}

static void
mpz_or(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mpz_t *y)
{
  if (zero_p(x)) {
    mpz_init_heap(ctx, z, y->sz);
    mpz_set(ctx, z, y);
    trim(z);
    return;
  }
  if (zero_p(y)) {
    mpz_init_heap(ctx, z, x->sz);
    mpz_set(ctx, z, x);
    trim(z);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;

  /* Fast path: both positive - just OR the limbs */
  if (x->sn > 0 && y->sn > 0) {
    mpz_init_heap(ctx, z, max_sz);
    size_t min_sz = (x->sz < y->sz) ? x->sz : y->sz;
    for (size_t i = 0; i < min_sz; i++) {
      z->p[i] = x->p[i] | y->p[i];
    }
    /* Copy remaining limbs from the longer operand */
    if (x->sz > y->sz) {
      for (size_t i = min_sz; i < max_sz; i++) z->p[i] = x->p[i];
    }
    else {
      for (size_t i = min_sz; i < max_sz; i++) z->p[i] = y->p[i];
    }
    z->sn = 1;
    trim(z);
    return;
  }

  /* Slow path: at least one negative operand */
  mpz_init_heap(ctx, z, max_sz);
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
  trim(z);
}

static void
mpz_xor(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mpz_t *y)
{
  if (zero_p(x)) {
    mpz_init_heap(ctx, z, y->sz);
    mpz_set(ctx, z, y);
    trim(z);
    return;
  }
  if (zero_p(y)) {
    mpz_init_heap(ctx, z, x->sz);
    mpz_set(ctx, z, x);
    trim(z);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;

  /* Fast path: both positive - just XOR the limbs */
  if (x->sn > 0 && y->sn > 0) {
    mpz_init_heap(ctx, z, max_sz);
    size_t min_sz = (x->sz < y->sz) ? x->sz : y->sz;
    for (size_t i = 0; i < min_sz; i++) {
      z->p[i] = x->p[i] ^ y->p[i];
    }
    /* Copy remaining limbs from the longer operand (XOR with 0) */
    if (x->sz > y->sz) {
      for (size_t i = min_sz; i < max_sz; i++) z->p[i] = x->p[i];
    }
    else {
      for (size_t i = min_sz; i < max_sz; i++) z->p[i] = y->p[i];
    }
    z->sn = 1;
    trim(z);
    return;
  }

  /* Slow path: at least one negative operand */
  mpz_init_heap(ctx, z, max_sz);
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
  trim(z);
}

static void
mpz_pow(mpz_ctx_t *ctx, mpz_t *zz, mpz_t *x, mrb_int e)
{
  if (e == 0) {
    mpz_init_set_int(ctx, zz, 1L);
    return;
  }

  mrb_uint mask = 1ULL << (sizeof(mrb_int) * 8 - 1);
  while (mask != 0 && !(mask & e)) {
    mask >>= 1;
  }

  /* Set initial value to x for exponentiation */
  mpz_init_set(ctx, zz, x);

  if (mask == 0) { /* e is 0 or 1 */
    if (e == 0) mpz_set_int(ctx, zz, 1L);
    return;
  }

  mask >>= 1;

  /* Pre-allocate a single temporary variable */
  mpz_t temp;
  mpz_init(ctx, &temp);

  for (; mask != 0; mask >>= 1) {
    /* squaring: temp = zz * zz */
    mpz_mul(ctx, &temp, zz, zz);

    if (e & mask) {
      /* multiplication: zz = temp * x */
      mpz_mul(ctx, zz, &temp, x);
    }
    else {
      /* move result: zz = temp */
      mpz_move(ctx, zz, &temp);
    }
  }
  mpz_clear(ctx, &temp);
}

/* Forward declaration for Montgomery modular exponentiation */
static void mpz_powm_montgomery(mpz_ctx_t *ctx, mpz_t *result,
                                const mpz_t *base, const mpz_t *exp, const mpz_t *n);

static void
mpz_powm(mpz_ctx_t *ctx, mpz_t *zz, mpz_t *x, mpz_t *ex, mpz_t *n)
{
  /* Handle special cases */
  if (zero_p(ex) || uzero_p(ex)) {
    mpz_set_int(ctx, zz, 1);
    return;
  }

  if (ex->sn < 0) {
    return;
  }

  /*
   * Use Montgomery reduction for odd moduli >= 4 limbs.
   * Montgomery is faster because it replaces division with multiplication.
   * For smaller moduli, Barrett has less setup overhead.
   */
  if (n->sz >= 4 && (n->p[0] & 1) == 1) {
    mpz_powm_montgomery(ctx, zz, x, ex, n);
    return;
  }

  /*
   * Fall back to Barrett reduction for even moduli or small moduli.
   * Barrett works for any modulus and is faster than division.
   */
  size_t pool_state = pool_save(ctx);
  mpz_t t, b;
  mpz_init_set_int(ctx, &t, 1);
  mpz_init_set(ctx, &b, x);

  /*
   * Use Barrett reduction for moduli >= 4 limbs.
   * For smaller moduli (2-3 limbs), simple division is faster because
   * Barrett's 2 multiplications + overhead exceed 1 division cost.
   */
  mpz_t mu, temp;
  int use_barrett = (n->sz >= 4);
  mpz_init_temp(ctx, &temp, n->sz * 2);  /* For intermediate calculations */
  if (use_barrett) {
    mpz_init_temp(ctx, &mu, n->sz + 1);  /* Barrett parameter */
    mpz_barrett_mu(ctx, &mu, n);
  }

  size_t len = digits(ex);
  for (size_t i = 0; i < len; i++) {
    mp_limb e = ex->p[i];
    for (size_t j = 0; j < sizeof(mp_limb) * 8; j++) {
      if ((e & 1) == 1) {
        mpz_mul(ctx, &temp, &t, &b);
        if (use_barrett) {
          mpz_barrett_reduce(ctx, &t, &temp, n, &mu);
        }
        else {
          mpz_mod(ctx, &t, &temp, n);
        }
      }
      e >>= 1;
      mpz_mul(ctx, &temp, &b, &b);
      if (use_barrett) {
        mpz_barrett_reduce(ctx, &b, &temp, n, &mu);
      }
      else {
        mpz_mod(ctx, &b, &temp, n);
      }
    }
  }

  mpz_move(ctx, zz, &t);
  mpz_clear(ctx, &t);
  mpz_clear(ctx, &b);
  mpz_clear(ctx, &temp);
  if (use_barrett) {
    mpz_clear(ctx, &mu);
  }
  pool_restore(ctx, pool_state);
}

static void
mpz_powm_i(mpz_ctx_t *ctx, mpz_t *zz, mpz_t *x, mrb_int ex, mpz_t *n)
{
  if (ex == 0) {
    mpz_set_int(ctx, zz, 1);
    return;
  }

  if (ex < 0) {
    return;
  }

  /*
   * Use Montgomery reduction for odd moduli (common in cryptography).
   * Convert integer exponent to mpz_t and use the main Montgomery implementation.
   */
  if (n->sz >= 2 && (n->p[0] & 1) == 1) {
    mpz_t exp_mpz;
    mpz_init_set_int(ctx, &exp_mpz, ex);
    mpz_powm_montgomery(ctx, zz, x, &exp_mpz, n);
    mpz_clear(ctx, &exp_mpz);
    return;
  }

  /*
   * Fall back to Barrett reduction for even moduli or small moduli.
   */
  size_t pool_state = pool_save(ctx);
  mpz_t t, b;
  mpz_init_set_int(ctx, &t, 1);
  mpz_init_set(ctx, &b, x);

  /*
   * Use Barrett reduction for moduli >= 4 limbs.
   * For smaller moduli (2-3 limbs), simple division is faster.
   */
  mpz_t mu, temp;
  int use_barrett = (n->sz >= 4);

  mpz_init_temp(ctx, &temp, n->sz * 2);  /* For intermediate calculations */
  if (use_barrett) {
    mpz_init_temp(ctx, &mu, n->sz + 1);  /* Barrett parameter */
    mpz_barrett_mu(ctx, &mu, n);
  }

  while (ex > 0) {
    if ((ex & 1) == 1) {
      mpz_mul(ctx, &temp, &t, &b);
      if (use_barrett) {
        mpz_barrett_reduce(ctx, &t, &temp, n, &mu);
      }
      else {
        mpz_mod(ctx, &t, &temp, n);
      }
    }
    ex >>= 1;
    if (ex > 0) {  /* Skip final squaring when ex becomes 0 */
      mpz_mul(ctx, &temp, &b, &b);
      if (use_barrett) {
        mpz_barrett_reduce(ctx, &b, &temp, n, &mu);
      }
      else {
        mpz_mod(ctx, &b, &temp, n);
      }
    }
  }

  mpz_move(ctx, zz, &t);
  mpz_clear(ctx, &t);
  mpz_clear(ctx, &b);
  mpz_clear(ctx, &temp);
  if (use_barrett) {
    mpz_clear(ctx, &mu);
  }
  pool_restore(ctx, pool_state);
}

/* Helper functions for pool-based GCD operations */
static int
mpz_abs_copy(mpz_ctx_t *ctx, mpz_t *result, mpz_t *operand) {
  if (!operand || operand->sz == 0) {
    result->sz = 0;
    result->sn = 0;
    return 1;
  }

  /* Copy limbs */
  for (size_t i = 0; i < operand->sz && i < result->sz; i++) {
    result->p[i] = operand->p[i];
  }
  result->sz = (operand->sz < result->sz) ? operand->sz : result->sz;
  result->sn = (operand->sn < 0) ? -operand->sn : operand->sn; /* Always positive */

  return 1;
}

static void
mpz_abs(mpz_ctx_t *ctx, mpz_t *x, mpz_t *y)
{
  /* In-place optimization: just make sign positive */
  if (x == y) {
    if (x->sn < 0) x->sn = -x->sn;
    return;
  }
  mpz_init_heap(ctx, x, y->sz);
  mpz_abs_copy(ctx, x, y);
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
mpz_gcd(mpz_ctx_t *ctx, mpz_t *gg, mpz_t *aa, mpz_t *bb)
{
  size_t pool_state = pool_save(ctx);
  mpz_t a, b;
  size_t shift;
  size_t a_zeros;
  size_t b_zeros;

  /* Handle special cases */
  if (zero_p(aa)) {
    mpz_abs(ctx, gg, bb);
    goto cleanup;
  }
  if (zero_p(bb)) {
    mpz_abs(ctx, gg, aa);
    goto cleanup;
  }

  /* Fast path for single-limb numbers */
  if (aa->sz <= 1 && bb->sz <= 1) {
    mp_limb a_limb = (aa->sz == 0) ? 0 : aa->p[0];
    mp_limb b_limb = (bb->sz == 0) ? 0 : bb->p[0];
    mp_limb result = limb_gcd(a_limb, b_limb);

    mpz_init(ctx, gg);
    if (result == 0) {
      gg->sn = 0;
      gg->sz = 0;
    }
    else {
      mpz_realloc(ctx, gg, 1);
      gg->p[0] = result;
      gg->sn = 1;
    }
    goto cleanup;
  }

  /* Fast path for powers of 2 */
  if (mpz_power_of_2_p(aa)) {
    a_zeros = mpz_trailing_zeros(aa);
    b_zeros = mpz_trailing_zeros(bb);
    size_t min_zeros = (a_zeros < b_zeros) ? a_zeros : b_zeros;

    mpz_init_set_int(ctx, gg, 1);
    mpz_mul_2exp(ctx, gg, gg, min_zeros);
    goto cleanup;
  }
  if (mpz_power_of_2_p(bb)) {
    a_zeros = mpz_trailing_zeros(aa);
    b_zeros = mpz_trailing_zeros(bb);
    size_t min_zeros = (a_zeros < b_zeros) ? a_zeros : b_zeros;

    mpz_init_set_int(ctx, gg, 1);
    mpz_mul_2exp(ctx, gg, gg, min_zeros);
    goto cleanup;
  }

  mpz_init(ctx, &a);
  mpz_abs(ctx, &a, aa);
  mpz_init(ctx, &b);
  mpz_abs(ctx, &b, bb);

  shift = 0;
  a_zeros = mpz_trailing_zeros(&a);
  b_zeros = mpz_trailing_zeros(&b);
  shift = (a_zeros < b_zeros) ? a_zeros : b_zeros;

  mpz_div_2exp(ctx, &a, &a, a_zeros);
  mpz_div_2exp(ctx, &b, &b, b_zeros);

  /* Euclidean algorithm for multi-limb numbers */
  while (!zero_p(&b)) {
    mpz_t temp;
    mpz_init_temp(ctx, &temp, a.sz);
    mpz_mod(ctx, &temp, &a, &b);
    mpz_move(ctx, &a, &b);
    mpz_move(ctx, &b, &temp);
    mpz_clear(ctx, &temp);
  }
  mpz_mul_2exp(ctx, gg, &a, shift);
  mpz_clear(ctx, &a);
  mpz_clear(ctx, &b);
cleanup:
  pool_restore(ctx, pool_state);
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

/* Compute Barrett parameter mu = floor(2^(2k) / m) where k ~ log2(m) */
static void
mpz_barrett_mu(mpz_ctx_t *ctx, mpz_t *mu, mpz_t *m)
{
  size_t k = mpz_bits(m);
  mpz_t temp;

  mpz_init_set_int(ctx, &temp, 1);
  mpz_mul_2exp(ctx, &temp, &temp, 2 * k);  /* temp = 2^(2k) */
  mpz_mdiv(ctx, mu, &temp, m);             /* mu = floor(2^(2k) / m) */
  mpz_clear(ctx, &temp);
}

/* Barrett reduction: r = x mod m using precomputed mu */
static void
mpz_barrett_reduce(mpz_ctx_t *ctx, mpz_t *r, mpz_t *x, mpz_t *m, mpz_t *mu)
{
  size_t k = mpz_bits(m);

  /* If x < m, then x mod m = x */
  if (mpz_cmp(ctx, x, m) < 0) {
    mpz_set(ctx, r, x);
    return;
  }

  /* Save pool state for proper cleanup of temporary allocations */
  size_t pool_state = pool_save(ctx);

  mpz_t q1, q2, q3, r1, r2;
  /* Conservative size estimates for Barrett reduction temporaries */
  size_t q_size = x->sz + mu->sz + 1;  /* For multiplication results */
  size_t r_size = m->sz + 1;           /* For modular reduction results */

  mpz_init_temp(ctx, &q1, x->sz + 1);
  mpz_init_temp(ctx, &q2, q_size);
  mpz_init_temp(ctx, &q3, q_size);
  mpz_init_temp(ctx, &r1, r_size);
  mpz_init_temp(ctx, &r2, r_size);

  /* Step 1: q1 = floor(x / 2^(k-1)) */
  if (k > 1) {
    mpz_div_2exp(ctx, &q1, x, k - 1);
  }
  else {
    mpz_set(ctx, &q1, x);
  }

  /* Step 2: q2 = q1 * mu */
  mpz_mul(ctx, &q2, &q1, mu);

  /* Step 3: q3 = floor(q2 / 2^(k+1)) */
  mpz_div_2exp(ctx, &q3, &q2, k + 1);

  /* Step 4: r1 = x mod 2^(k+1) */
  mpz_mod_2exp(ctx, &r1, x, k + 1);

  /* Step 5: r2 = (q3 * m) mod 2^(k+1) */
  mpz_mul(ctx, &r2, &q3, m);
  mpz_mod_2exp(ctx, &r2, &r2, k + 1);

  /* Step 6: r = r1 - r2 */
  if (mpz_cmp(ctx, &r1, &r2) >= 0) {
    mpz_sub(ctx, r, &r1, &r2);
  }
  else {
    /* r1 < r2, so add 2^(k+1) to r1 */
    mpz_t power;
    mpz_init_set_int(ctx, &power, 1);
    mpz_mul_2exp(ctx, &power, &power, k + 1);
    mpz_add(ctx, &r1, &r1, &power);
    mpz_sub(ctx, r, &r1, &r2);
    mpz_clear(ctx, &power);
  }

  /* Step 7: Final correction - ensure 0 <= r < m */
  while (mpz_cmp(ctx, r, m) >= 0) {
    mpz_sub(ctx, r, r, m);
  }

  /* Cleanup temporaries */
  mpz_clear(ctx, &q1);
  mpz_clear(ctx, &q2);
  mpz_clear(ctx, &q3);
  mpz_clear(ctx, &r1);
  mpz_clear(ctx, &r2);

  /* Restore pool state to free any temporary pool allocations */
  pool_restore(ctx, pool_state);
}

static void
mpz_sqrt(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x)
{
  mrb_assert(x->sn >= 0);

  if (x->sz == 0) {
    // sqrt(0) = 0
    mpz_init(ctx, z);
    z->sn = 0;
    z->sz = 0;
    return;
  }

  // Use heap-only implementation for now
  size_t xbits = mpz_bits(x);
  size_t sbit = (xbits + 1) / 2;
  mpz_t s, t;
  mpz_init_set_int(ctx, &s, 1);
  mpz_mul_2exp(ctx, &s, &s, sbit);

  mpz_init_temp(ctx, &t, x->sz + 1);

  // Iteratively refine s using Newton-Raphson method:
  // s = (s + x / s) / 2
  for (;;) {
    mpz_mdiv(ctx, &t, x, &s);     // t = x / s
    mpz_add(ctx, &t, &t, &s);     // t = s + x/s
    mpz_div_2exp(ctx, &t, &t, 1); // t = (s + x/s) / 2

    if (mpz_cmp(ctx, &t, &s) >= 0) {
      // Converged: t >= s
      break;
    }

    mpz_set(ctx, &s, &t);
  }

  mpz_move(ctx, z, &s);
  mpz_clear(ctx, &t);
}


/* Barrett reduction for efficient modular arithmetic with repeated operations */

/*
 * Montgomery Reduction
 *
 * Montgomery reduction computes x * R^(-1) mod n without division, where
 * R = 2^(k*64) for a k-limb modulus. This is faster than Barrett for
 * repeated modular operations with the same modulus (e.g., modular exponentiation).
 *
 * Requirements:
 * - n must be ODD (n[0] & 1 == 1)
 * - R > n (automatically satisfied since R = 2^(k*64) >= 2^64 > any k-limb number)
 *
 * Key insight: Instead of computing x mod n directly, we work in "Montgomery form"
 * where a' = a * R mod n. Multiplication in Montgomery form:
 *   a' * b' = (aR)(bR) * R^(-1) mod n = abR mod n = (ab)'
 */

/*
 * Compute rho = -n[0]^(-1) mod 2^64 using Newton's method.
 * This is the Montgomery constant needed for reduction.
 */
static mp_limb
montgomery_setup(mp_limb n0)
{
  /* n must be odd for this to work */
  mrb_assert((n0 & 1) == 1);

  /*
   * Newton's method for modular inverse:
   * x_{i+1} = x_i * (2 - n0 * x_i) mod 2^k
   *
   * Starting with x = 1 (which satisfies x * n0 ≡ 1 mod 2),
   * each iteration doubles the number of correct bits.
   */
  mp_limb x = 1;

  /* 6 iterations: 1 -> 2 -> 4 -> 8 -> 16 -> 32 -> 64 bits */
  for (int i = 0; i < 6; i++) {
    x = x * (2 - n0 * x);  /* Implicit mod 2^64 via overflow */
  }

  /* Return -x mod 2^64 = negation of modular inverse */
  return (mp_limb)0 - x;
}

/*
 * Montgomery reduction: result = x * R^(-1) mod n
 *
 * Algorithm (REDC):
 *   m = (x mod R) * rho mod R
 *   t = (x + m * n) / R
 *   if t >= n: t = t - n
 *   return t
 *
 * The key insight is that (x + m*n) is always divisible by R,
 * so the division is just a right shift (drop low limbs).
 */
static void
mpz_montgomery_reduce(mpz_ctx_t *ctx, mpz_t *result,
                      const mpz_t *x, const mpz_t *n, mp_limb rho)
{
  size_t k = n->sz;  /* Number of limbs in modulus */
  size_t x_len = x->sz;

  /* Allocate workspace: need k+1 extra limbs for the product accumulation */
  size_t work_size = x_len + k + 2;
  size_t pool_state = pool_save(ctx);

  mp_limb *work = NULL;
  if (MPZ_HAS_POOL(ctx)) {
    work = pool_alloc(MPZ_POOL(ctx), work_size);
  }
  mrb_bool heap_alloc = (work == NULL);
  if (heap_alloc) {
    work = (mp_limb*)mrb_malloc(MPZ_MRB(ctx), work_size * sizeof(mp_limb));
  }

  /* Copy x to work buffer, zero-extend if necessary */
  mpn_copyi(work, x->p, x_len);
  mpn_zero(work + x_len, work_size - x_len);

  /*
   * Main Montgomery reduction loop:
   * For i = 0 to k-1:
   *   m_i = work[i] * rho mod 2^64
   *   work += m_i * n * 2^(i*64)
   */
  for (size_t i = 0; i < k; i++) {
    mp_limb m = work[i] * rho;

    /* Add m * n at position i */
    mp_limb carry = mpn_addmul_1(work + i, n->p, k, m);

    /* Propagate carry */
    for (size_t j = i + k; carry && j < work_size; j++) {
      mp_dbl_limb sum = (mp_dbl_limb)work[j] + carry;
      work[j] = LOW(sum);
      carry = HIGH(sum);
    }
  }

  /* Result is work[k..2k-1] (the upper k limbs after dividing by R) */
  mpz_realloc(ctx, result, k + 1);
  mpn_copyi(result->p, work + k, k + 1);
  result->sz = k + 1;
  result->sn = 1;
  trim(result);

  /* Final subtraction if result >= n */
  if (ucmp(result, (mpz_t*)n) >= 0) {
    mpz_sub(ctx, result, result, (mpz_t*)n);
  }

  /* Cleanup */
  if (heap_alloc) {
    mrb_free(MPZ_MRB(ctx), work);
  }
  pool_restore(ctx, pool_state);
}

/*
 * Compute R^2 mod n, needed for converting to Montgomery form.
 * a' = REDC(a * R^2) = a * R mod n
 */
static void
mpz_montgomery_calc_R2(mpz_ctx_t *ctx, mpz_t *R2, const mpz_t *n)
{
  size_t k = n->sz;

  /* R = 2^(k*64), so R^2 = 2^(2*k*64) */
  mpz_init_set_int(ctx, R2, 1);
  mpz_mul_2exp(ctx, R2, R2, 2 * k * DIG_SIZE);

  /* R2 = R^2 mod n */
  mpz_mod(ctx, R2, R2, (mpz_t*)n);
}

/*
 * Montgomery modular exponentiation: result = base^exp mod n
 * Requires n to be odd.
 */
static void
mpz_powm_montgomery(mpz_ctx_t *ctx, mpz_t *result,
                    const mpz_t *base, const mpz_t *exp, const mpz_t *n)
{
  size_t pool_state = pool_save(ctx);

  /* Setup Montgomery parameters */
  mp_limb rho = montgomery_setup(n->p[0]);

  /* Compute R^2 mod n for conversion to Montgomery form */
  mpz_t R2;
  mpz_montgomery_calc_R2(ctx, &R2, n);

  /* Compute R mod n = REDC(R^2) for initializing accumulator to 1 in Montgomery form */
  mpz_t one_mont;
  mpz_init(ctx, &one_mont);
  mpz_montgomery_reduce(ctx, &one_mont, &R2, n, rho);

  /* Convert base to Montgomery form: base_mont = base * R mod n = REDC(base * R^2) */
  mpz_t base_mont, temp;
  mpz_init(ctx, &base_mont);
  mpz_init_temp(ctx, &temp, n->sz * 4);

  mpz_mul(ctx, &temp, (mpz_t*)base, &R2);
  mpz_montgomery_reduce(ctx, &base_mont, &temp, n, rho);

  /* Initialize accumulator to 1 in Montgomery form */
  mpz_t acc;
  mpz_init_set(ctx, &acc, &one_mont);

  /* Binary exponentiation in Montgomery form */
  size_t exp_len = exp->sz;
  for (size_t i = 0; i < exp_len; i++) {
    mp_limb e = exp->p[i];
    for (size_t j = 0; j < sizeof(mp_limb) * 8; j++) {
      if ((e & 1) == 1) {
        /* acc = acc * base_mont in Montgomery form */
        mpz_mul(ctx, &temp, &acc, &base_mont);
        mpz_montgomery_reduce(ctx, &acc, &temp, n, rho);
      }
      e >>= 1;

      /* base_mont = base_mont^2 in Montgomery form */
      mpz_mul(ctx, &temp, &base_mont, &base_mont);
      mpz_montgomery_reduce(ctx, &base_mont, &temp, n, rho);
    }
  }

  /* Convert result back from Montgomery form: result = REDC(acc) */
  mpz_montgomery_reduce(ctx, result, &acc, n, rho);

  /* Cleanup */
  mpz_clear(ctx, &R2);
  mpz_clear(ctx, &one_mont);
  mpz_clear(ctx, &base_mont);
  mpz_clear(ctx, &temp);
  mpz_clear(ctx, &acc);
  pool_restore(ctx, pool_state);
}

/* --- mruby functions --- */
/* initialize mpz_t from RBigint (not need to clear) */
static void
bint_as_mpz(struct RBigint *b, mpz_t *x)
{
  x->p = RBIGINT_ARY(b);
  x->sz = RBIGINT_SIZE(b);
  x->sn = RBIGINT_SIGN(b);
}

/* Transfer mpz_t data to RBigint structure */
static void
bint_set(mpz_ctx_t *ctx, struct RBigint *b, mpz_t *x)
{
  if (x->sz <= RBIGINT_EMBED_SIZE_MAX) {
    RBIGINT_SET_EMBED_SIZE(b, x->sz);
    RBIGINT_SET_EMBED_SIGN(b, x->sn);
    if (x->p) {
      memcpy(RBIGINT_EMBED_ARY(b), x->p, x->sz*sizeof(mp_limb));
    }
    else {
      /* Initialize embedded array to zero when x->p is NULL */
      memset(RBIGINT_EMBED_ARY(b), 0, x->sz*sizeof(mp_limb));
    }
    mpz_clear(ctx, x);
  }
  else {
    RBIGINT_SET_HEAP(b);
    mpz_move(ctx, &b->as.heap, x);
  }
}

static struct RBigint*
bint_new(mpz_ctx_t *ctx, mpz_t *x)
{
  struct RBigint *b = MRB_OBJ_ALLOC(MPZ_MRB(ctx), MRB_TT_BIGINT, MPZ_MRB(ctx)->integer_class);
  bint_set(ctx, b, x);
  return b;
}

static struct RBigint*
bint_new_int(mpz_ctx_t *ctx, mrb_int n)
{
  mpz_t x;
  mpz_init_set_int(ctx, &x, n);
  return bint_new(ctx, &x);
}

mrb_value
mrb_bint_new_int(mrb_state *mrb, mrb_int x)
{
  MPZ_CTX_INIT(mrb, ctx, pool);
  struct RBigint *b = bint_new_int(ctx, x);
  return mrb_obj_value(b);
}

#ifdef MRB_INT32
mrb_value
mrb_bint_new_int64(mrb_state *mrb, int64_t n)
{
  mpz_t x;
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_set_int64(ctx, &x, n);
  struct RBigint *b = bint_new(ctx, &x);
  return mrb_obj_value(b);
}
#endif

mrb_value
mrb_bint_new_uint64(mrb_state *mrb, uint64_t x)
{
  mpz_t z;
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_init(ctx, &z);
  mpz_set_uint64(ctx, &z, x);
  struct RBigint *b = bint_new(ctx, &z);
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

  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_init_set_str(ctx, &z, x, len, base);
  if (sn < 0) {
    z.sn = sn;
  }
  return bint_norm(mrb, bint_new(ctx, &z));
}

void
mrb_gc_free_bint(mrb_state *mrb, struct RBasic *x)
{
  struct RBigint *b = (struct RBigint*)x;
  MPZ_CTX_INIT(mrb, ctx, pool);

  if (!RBIGINT_EMBED_P(b)) {
    mpz_clear(ctx, &b->as.heap);
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

  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_t r;
  mpz_init(ctx, &r);
  r.sn = sn;

  mrb_float b = (double)DIG_BASE;
  mrb_float bi = 1.0 / b;
  size_t rn;

  for (rn = 1; x >= b; rn++)
    x *= bi;

  mpz_realloc(ctx, &r, rn);
  mp_limb *rp = r.p;
  for (size_t i=rn-1;;i--) {
    mp_limb f = LOW((mp_limb)x);
    x -= f;
    mrb_assert(x < 1.0);
    rp[i] = f;
    if (i == 0) break;
  }
  return bint_norm(mrb, bint_new(ctx, &r));
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

  MPZ_CTX_INIT(mrb, ctx, pool);

  if (mrb_integer_p(y)) {
    mrb_int n = mrb_integer(y);
    if (int_fit_limb_p(n)) {
      mpz_init_set(ctx, &z, &a);
      if ((n > 0) ^ (z.sn > 0)) {
        mpz_sub_int(ctx, &z, n<0 ? -n : n);
      }
      else {
        mpz_add_int(ctx, &z, n<0 ? -n : n);
      }
      struct RBigint *v = bint_new(ctx, &z);
      return mrb_obj_value(v);
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init(ctx, &z);
  mpz_add(ctx, &z, &a, &b);
  struct RBigint *v = bint_new(ctx, &z);
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
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y)) {
    mrb_int n = mrb_integer(y);
    if (int_fit_limb_p(n)) {
      mpz_init_set(ctx, &z, &a);
      if ((n > 0) ^ (z.sn > 0)) {
        mpz_add_int(ctx, &z, n<0 ? -n : n);
      }
      else {
        mpz_sub_int(ctx, &z, n<0 ? -n : n);
      }
      struct RBigint *v = bint_new(ctx, &z);
      return mrb_obj_value(v);
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init(ctx, &z);
  mpz_sub(ctx, &z, &a, &b);
  struct RBigint *v = bint_new(ctx, &z);
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

  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_init(ctx, &z);
  mpz_mul(ctx, &z, &a, &b);
  return bint_new(ctx, &z);
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

  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_init(ctx, &z);
  mpz_mdiv(ctx, &z, &a, &b);
  return bint_norm(mrb, bint_new(ctx, &z));
}

mrb_value
mrb_bint_add_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_init(ctx, &z);
  mpz_init_set_int(ctx, &a, x);
  mpz_init_set_int(ctx, &b, y);
  mpz_add(ctx, &z, &a, &b);
  mpz_clear(ctx, &a);
  mpz_clear(ctx, &b);
  return bint_norm(mrb, bint_new(ctx, &z));
}

mrb_value
mrb_bint_sub_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_init(ctx, &z);
  mpz_init_set_int(ctx, &a, x);
  mpz_init_set_int(ctx, &b, y);
  mpz_sub(ctx, &z, &a, &b);
  mpz_clear(ctx, &a);
  mpz_clear(ctx, &b);
  return bint_norm(mrb, bint_new(ctx, &z));
}

mrb_value
mrb_bint_mul_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_init(ctx, &z);
  mpz_init_set_int(ctx, &a, x);
  mpz_init_set_int(ctx, &b, y);
  mpz_mul(ctx, &z, &a, &b);
  mpz_clear(ctx, &a);
  mpz_clear(ctx, &b);
  return bint_norm(mrb, bint_new(ctx, &z));
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
  x = mrb_as_bint(mrb, x);
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&b) || uzero_p(&b)) {
    mrb_int_zerodiv(mrb);
  }
  bint_as_mpz(RBIGINT(x), &a);

  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_init(ctx, &z);
  mpz_mmod(ctx, &z, &a, &b);
  return bint_norm(mrb, bint_new(ctx, &z));
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
  x = mrb_as_bint(mrb, x);
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&b) || uzero_p(&b)) {
    mrb_int_zerodiv(mrb);
  }
  bint_as_mpz(RBIGINT(x), &a);

  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_init(ctx, &z);
  mpz_mod(ctx, &z, &a, &b);
  return bint_norm(mrb, bint_new(ctx, &z));
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

  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_init(ctx, &c);
  mpz_init(ctx, &d);
  mpz_mdivmod(ctx, &c, &d, &a, &b);
  return mrb_assoc_new(mrb, bint_norm(mrb, bint_new(ctx, &c)), bint_norm(mrb, bint_new(ctx, &d)));
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
  MPZ_CTX_INIT(mrb, ctx, pool);
  return mpz_cmp(ctx, &a, &b);
}

/* Maximum bits for power result to prevent resource exhaustion */
/* 1 million bits = ~125KB per bigint, ~300,000 decimal digits */
#define MRB_BIGINT_POW_MAX_BITS 1000000

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

  mrb_int exp = mrb_integer(y);
  if (exp < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative exponent");
  }

  /* Check if result would be too large */
  /* result_bits ≈ base_bits * exp */
  size_t base_bits = mpz_bits(&a);
  if (base_bits == 0) base_bits = 1;  /* handle 0 and 1 */
  if (exp > 0 && (size_t)exp > MRB_BIGINT_POW_MAX_BITS / base_bits) {
    mrb_raise(mrb, E_RANGE_ERROR, "exponent too large");
  }

  mpz_t z;
  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_pow(ctx, &z, &a, exp);

  struct RBigint *b = bint_new(ctx, &z);
  return mrb_obj_value(b);
}

mrb_value
mrb_bint_powm(mrb_state *mrb, mrb_value x, mrb_value exp, mrb_value mod)
{
  mpz_t a, b, c, z;
  mrb_bool neg_mod = FALSE;
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(mod)) {
    mrb_int m = mrb_integer(mod);
    if (m == 0) mrb_int_zerodiv(mrb);
    if (m < 0) {
      neg_mod = TRUE;
      m = -m;
    }
    mpz_init_set_int(ctx, &c, m);
  }
  else {
    mod = mrb_as_bint(mrb, mod);
    bint_as_mpz(RBIGINT(mod), &c);
    if (zero_p(&c) || uzero_p(&c)) {
      mrb_int_zerodiv(mrb);
    }
    if (c.sn < 0) {
      neg_mod = TRUE;
      c.sn = 1;  /* use absolute value */
    }
  }

  /* Check for zero base case: 0^n = 0 for n > 0 */
  if (zero_p(&a) || uzero_p(&a)) {
    mrb_bool exp_positive;
    if (mrb_bigint_p(exp)) {
      bint_as_mpz(RBIGINT(exp), &b);
      exp_positive = (b.sn > 0) && !uzero_p(&b);
    }
    else {
      exp_positive = mrb_integer(exp) > 0;
    }
    if (exp_positive) {
      /* 0^n mod m = 0 for n > 0 */
      if (mrb_integer_p(mod)) mpz_clear(ctx, &c);
      return mrb_fixnum_value(0);
    }
  }

  mpz_init(ctx, &z);
  if (mrb_bigint_p(exp)) {
    bint_as_mpz(RBIGINT(exp), &b);
    if (b.sn < 0) goto raise;
    mpz_powm(ctx, &z, &a, &b, &c);
  }
  else {
    mrb_int e = mrb_integer(exp);
    if (e < 0) goto raise;
    mpz_powm_i(ctx, &z, &a, e, &c);
  }

  /* Apply signed modulo adjustment for negative modulus */
  /* Ruby: result + m for non-zero result when m is negative */
  if (neg_mod && !zero_p(&z) && !uzero_p(&z)) {
    mpz_sub(ctx, &z, &z, &c);  /* z = z - |m| = z + m (since m is negative) */
  }

  if (mrb_integer_p(mod)) mpz_clear(ctx, &c);
  return bint_norm(mrb, bint_new(ctx, &z));

 raise:
  if (mrb_integer_p(mod)) mpz_clear(ctx, &c);
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
  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_get_str(ctx, RSTRING_PTR(str), len, base, &a);
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

  MPZ_CTX_INIT(mrb, ctx, pool);
  mpz_init(ctx, &c);
  mpz_and(ctx, &c, &a, &b);
  return bint_norm(mrb, bint_new(ctx, &c));
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

  MPZ_CTX_INIT(mrb, ctx, pool);
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&a)) return y;
  if (zero_p(&b)) return x;
  mpz_init(ctx, &c);
  mpz_or(ctx, &c, &b, &a);
  return bint_norm(mrb, bint_new(ctx, &c));
}

mrb_value
mrb_bint_xor(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b, c;
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y) && a.sn > 0) {
    mrb_int z = mrb_integer(y);
    if (z == 0) return x;
    if (0 < z && (mp_dbl_limb)z < DIG_BASE) {
      mpz_init_set(ctx, &c, &a);
      if (a.sz == 0) {
        mpz_realloc(ctx, &c, 1);
        c.p[0] = (mp_limb)z;
      }
      else {
        c.p[0] ^= (mp_limb)z;
      }
      return bint_norm(mrb, bint_new(ctx, &c));
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&a)) return y;
  if (zero_p(&b)) return x;
  mpz_init(ctx, &c);
  mpz_xor(ctx, &c, &a, &b);
  return bint_norm(mrb, bint_new(ctx, &c));
}

mrb_value
mrb_bint_neg(mrb_state *mrb, mrb_value x)
{
  mpz_t a, b;
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(ctx, &b);
  mpz_neg(ctx, &b, &a);
  struct RBigint *b2 = bint_new(ctx, &b);
  /* no normalization */
  return mrb_obj_value(b2);
}

mrb_value
mrb_bint_rev(mrb_state *mrb, mrb_value x)
{
  mpz_t a, b;
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(ctx, &b);
  mpz_neg(ctx, &b, &a);
  mpz_sub_int(ctx, &b, 1);
  return bint_norm(mrb, bint_new(ctx, &b));
}

mrb_value
mrb_bint_lshift(mrb_state *mrb, mrb_value x, mrb_int width)
{
  mpz_t a, z;
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(ctx, &z);
  if (width < 0) {
    mpz_div_2exp(ctx, &z, &a, -width);
  }
  else {
    mpz_mul_2exp(ctx, &z, &a, width);
  }
  return bint_norm(mrb, bint_new(ctx, &z));
}

mrb_value
mrb_bint_rshift(mrb_state *mrb, mrb_value x, mrb_int width)
{
  mpz_t a, z;
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(ctx, &z);
  if (width < 0) {
    mpz_mul_2exp(ctx, &z, &a, -width);
  }
  else {
    mpz_div_2exp(ctx, &z, &a, width);
  }
  return bint_norm(mrb, bint_new(ctx, &z));
}

void
mrb_bint_copy(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t b, temp;
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(y), &b);
  mpz_init_set(ctx, &temp, &b);
  bint_set(ctx, RBIGINT(x), &temp);
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
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_t z;
  mpz_init(ctx, &z);
  mpz_sqrt(ctx, &z, &a);

  return bint_norm(mrb, bint_new(ctx, &z));
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
  size_t limb_len = (len + sizeof(mp_limb) - 1) / sizeof(mp_limb);
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_init_heap(ctx, &z, limb_len);
  memcpy(z.p, bytes, len);
  z.sn = (len > 0) ? 1 : 0;
  z.sz = limb_len;
  trim(&z);
  return bint_norm(mrb, bint_new(ctx, &z));
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
  MPZ_CTX_INIT(mrb, ctx, pool);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(ctx, &z);
  mrb_assert(a.sn < 0);
  size_t size = a.sz;
  mpz_realloc(ctx, &z, size);
  mp_limb *ds = a.p;
  mp_limb *dd = z.p;
  char carry = 1;
  for (size_t i=0; i<size; i++) {
    mp_limb xv = ds[i];
    make_2comp(xv, carry);
    dd[i] = xv;
  }
  z.sn = 1;

  struct RBigint *b2 = bint_new(ctx, &z);
  return mrb_obj_value(b2);
}

#ifdef MRB_USE_RATIONAL
void
mrb_bint_reduce(mrb_state *mrb, mrb_value *xp, mrb_value *yp)
{
  mpz_t r, x, y, a, b;
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_init(ctx, &r);
  mpz_init(ctx, &a); mpz_init(ctx, &b);

  bint_as_mpz(RBIGINT(*xp), &x);
  bint_as_mpz(RBIGINT(*yp), &y);

  mpz_gcd(ctx, &r, &x, &y);

  mpz_mdiv(ctx, &a, &x, &r);
  mpz_mdiv(ctx, &b, &y, &r);

  mpz_clear(ctx, &r);

  struct RBigint *b1 = bint_new(ctx, &a);
  struct RBigint *b2 = bint_new(ctx, &b);
  *xp = mrb_obj_value(b1);
  *yp = mrb_obj_value(b2);
}
#endif

mrb_value
mrb_bint_gcd(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t r, a, b;
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_init(ctx, &r);
  bint_as_mpz(RBIGINT(x), &a);
  bint_as_mpz(RBIGINT(y), &b);

  mpz_gcd(ctx, &r, &a, &b);

  struct RBigint *result = bint_new(ctx, &r);
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

  MPZ_CTX_INIT(mrb, ctx, pool);
  /* Get input operand sizes for size estimation */
  size_t x_size = RBIGINT_EMBED_P(RBIGINT(x)) ? RBIGINT_EMBED_SIZE(RBIGINT(x)) : RBIGINT(x)->as.heap.sz;
  size_t y_size = RBIGINT_EMBED_P(RBIGINT(y)) ? RBIGINT_EMBED_SIZE(RBIGINT(y)) : RBIGINT(y)->as.heap.sz;
  size_t max_size = (x_size > y_size) ? x_size : y_size;

  mpz_init_temp(ctx, &gcd_val, max_size);
  mpz_init_temp(ctx, &abs_x, x_size);
  mpz_init_temp(ctx, &abs_y, y_size);
  mpz_init_temp(ctx, &product, x_size + y_size + 1);
  mpz_init_temp(ctx, &result_mpz, x_size + y_size + 1);

  bint_as_mpz(RBIGINT(x), &x_mpz);
  bint_as_mpz(RBIGINT(y), &y_mpz);

  mpz_abs(ctx, &abs_x, &x_mpz);
  mpz_abs(ctx, &abs_y, &y_mpz);

  mpz_gcd(ctx, &gcd_val, &abs_x, &abs_y);
  mpz_mul(ctx, &product, &abs_x, &abs_y);
  mpz_mdiv(ctx, &result_mpz, &product, &gcd_val);

  mpz_clear(ctx, &gcd_val);
  mpz_clear(ctx, &abs_x);
  mpz_clear(ctx, &abs_y);
  mpz_clear(ctx, &product);

  struct RBigint *result = bint_new(ctx, &result_mpz);
  return mrb_obj_value(result);
}

mrb_value
mrb_bint_abs(mrb_state *mrb, mrb_value x)
{
  mpz_t a, result_mpz;
  MPZ_CTX_INIT(mrb, ctx, pool);

  mpz_init(ctx, &result_mpz);
  bint_as_mpz(RBIGINT(x), &a);
  mpz_abs(ctx, &result_mpz, &a);

  struct RBigint *result = bint_new(ctx, &result_mpz);
  return mrb_obj_value(result);
}
