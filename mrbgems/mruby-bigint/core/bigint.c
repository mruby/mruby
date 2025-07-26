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

/* Scoped Memory Pool Infrastructure */
#define BIGINT_POOL_DEFAULT_SIZE 512  /* 2KB on 32-bit, 4KB on 64-bit */

typedef struct mpz_pool {
  mp_limb data[BIGINT_POOL_DEFAULT_SIZE];
  size_t used;
  size_t capacity;
  int active;
} mpz_pool_t;

/* MPZ Context Architecture - unified parameter for mrb_state and optional pool */
typedef struct mpz_context {
  mrb_state *mrb;
  mpz_pool_t *pool;  /* NULL for heap-only operations */
} mpz_ctx_t;

/* Forward declarations */
static int mpz_mul_sliding_window(mpz_ctx_t *ctx, mpz_t *result, mpz_t *first, mpz_t *second);

/* Convenience macros for context creation */
#define MPZ_CTX_HEAP(mrb_ptr) ((mpz_ctx_t){.mrb = (mrb_ptr), .pool = NULL})
#define MPZ_CTX_POOL(mrb_ptr, pool_ptr) ((mpz_ctx_t){.mrb = (mrb_ptr), .pool = (pool_ptr)})

/* Access macros for readability */
#define MPZ_MRB(ctx) ((ctx)->mrb)
#define MPZ_POOL(ctx) ((ctx)->pool)
#define MPZ_HAS_POOL(ctx) ((ctx)->pool != NULL)


#define WITH_SCOPED_POOL(pool_name, code) do { \
  mpz_pool_t pool_name##_storage = {.capacity = BIGINT_POOL_DEFAULT_SIZE, .active = 1}; \
  mpz_pool_t *pool_name = &pool_name##_storage; \
  code \
  pool_name##_storage.active = 0; \
} while(0)

/* Pool memory management helper macros */
#define MPZ_POOL_ALLOC(ctx, var, pool, size) do { \
  mpz_init_pool(ctx, &(var), pool, size); \
  if (!is_pool_memory(&(var), pool)) { \
    mpz_clear_pool(ctx, &(var), pool); \
    return 0; /* Pool allocation failed, fallback */ \
  } \
} while(0)

#define MPZ_POOL_ALLOC_GOTO(ctx, var, pool, size, label) do { \
  mpz_init_pool(ctx, &(var), pool, size); \
  if (!is_pool_memory(&(var), pool)) { \
    mpz_clear_pool(ctx, &(var), pool); \
    goto label; \
  } \
} while(0)

#define MPZ_POOL_CLEANUP(ctx, var, pool) do { \
  if ((var).p) mpz_clear_pool(ctx, &(var), pool); \
} while(0)

#define MPZ_POOL_VERIFY(var, pool) is_pool_memory(&(var), pool)

#define MPZ_POOL_VERIFY_2(var1, var2, pool) \
  (MPZ_POOL_VERIFY(var1, pool) && MPZ_POOL_VERIFY(var2, pool))

#define MPZ_POOL_VERIFY_3(var1, var2, var3, pool) \
  (MPZ_POOL_VERIFY(var1, pool) && MPZ_POOL_VERIFY(var2, pool) && MPZ_POOL_VERIFY(var3, pool))

#define MPZ_POOL_VERIFY_4(var1, var2, var3, var4, pool) \
  (MPZ_POOL_VERIFY(var1, pool) && MPZ_POOL_VERIFY(var2, pool) && \
   MPZ_POOL_VERIFY(var3, pool) && MPZ_POOL_VERIFY(var4, pool))

#define MPZ_POOL_VERIFY_6(var1, var2, var3, var4, var5, var6, pool) \
  (MPZ_POOL_VERIFY(var1, pool) && MPZ_POOL_VERIFY(var2, pool) && \
   MPZ_POOL_VERIFY(var3, pool) && MPZ_POOL_VERIFY(var4, pool) && \
   MPZ_POOL_VERIFY(var5, pool) && MPZ_POOL_VERIFY(var6, pool))

/* Unified pool+heap helper macros for eliminating function duplication */

/* Copy result from pool to heap-allocated destination */
#define MPZ_COPY_FROM_POOL(ctx, dest, src, pool) do { \
  if (is_pool_memory(&(src), pool)) { \
    trim(&(src)); \
    mpz_realloc(ctx, dest, (src).sz); \
    for (size_t i = 0; i < (src).sz; i++) { \
      (dest)->p[i] = (src).p[i]; \
    } \
    (dest)->sz = (src).sz; \
    (dest)->sn = (src).sn; \
  } \
} while(0)

/* Unified operation using existing *_core functions */
/* For binary operations: mpz_operation(result, op1, op2) */
#define MPZ_UNIFIED_BINARY_OP(ctx, core_func, result, op1, op2, estimated_size) do { \
  WITH_SCOPED_POOL(pool, { \
    mpz_t temp_result; \
    MPZ_POOL_ALLOC_GOTO(ctx, temp_result, pool, estimated_size, _pool_failed); \
    /* Pool allocation successful */ \
    core_func(&temp_result, op1, op2); \
    MPZ_COPY_FROM_POOL(ctx, result, temp_result, pool); \
    return; \
    _pool_failed: ; \
  }); \
  /* Pool failed - use heap allocation */ \
  core_func(result, op1, op2); \
} while(0)

/* For unary operations: mpz_operation(result, operand) */
#define MPZ_UNIFIED_UNARY_OP(ctx, core_func, result, operand, estimated_size) do { \
  WITH_SCOPED_POOL(pool, { \
    mpz_t temp_result; \
    MPZ_POOL_ALLOC_GOTO(ctx, temp_result, pool, estimated_size, _pool_failed); \
    /* Pool allocation successful */ \
    core_func(&temp_result, operand); \
    MPZ_COPY_FROM_POOL(ctx, result, temp_result, pool); \
    return; \
    _pool_failed: ; \
  }); \
  /* Pool failed - use heap allocation */ \
  core_func(result, operand); \
} while(0)

/* For binary operations that return int: mpz_operation(result, op1, op2) -> int */
#define MPZ_UNIFIED_BINARY_OP_INT(ctx, core_func, result, op1, op2, estimated_size, success_value) do { \
  WITH_SCOPED_POOL(pool, { \
    mpz_t temp_result; \
    MPZ_POOL_ALLOC_GOTO(ctx, temp_result, pool, estimated_size, _pool_failed); \
    /* Pool allocation successful */ \
    core_func(&temp_result, op1, op2); \
    MPZ_COPY_FROM_POOL(ctx, result, temp_result, pool); \
    return success_value; \
    _pool_failed: ; \
  }); \
  /* Pool failed - use heap allocation */ \
  core_func(result, op1, op2); \
  return success_value; \
} while(0)

/* Pool allocation functions */
static mp_limb*
pool_alloc(mpz_pool_t *pool, size_t limbs)
{
  if (!pool || !pool->active || pool->used + limbs > pool->capacity) {
    return NULL;  /* Force fallback to heap */
  }

  mp_limb *ptr = &pool->data[pool->used];
  pool->used += limbs;
  return ptr;
}

static void
mpz_init_pool(mpz_ctx_t *ctx, mpz_t *z, mpz_pool_t *pool, size_t hint)
{
  z->sn = 0;

  mp_limb *pool_ptr = pool_alloc(pool, hint);
  if (pool_ptr) {
    z->p = pool_ptr;
    z->sz = hint;
  }
  else {
    /* Fallback to heap allocation */
    z->p = mrb_malloc(MPZ_MRB(ctx), hint * sizeof(mp_limb));
    z->sz = hint;
  }
}

static void
mpz_init(mpz_ctx_t *ctx, mpz_t *s)
{
  s->p = NULL;
  s->sn = 0;
  s->sz = 0;
}

/* Helper macros for safer temporary variable management */
#define MPZ_TMP_INIT(ctx, var) \
  mpz_t var; \
  mpz_init(ctx, &var)

#define MPZ_TMP_CLEAR(ctx, var) \
  mpz_clear(ctx, &var)

/* Check if mpz_t uses pool memory */
static int
is_pool_memory(mpz_t *z, mpz_pool_t *pool)
{
  if (!pool || !z->p) return 0;
  uintptr_t ptr_addr = (uintptr_t)z->p;
  uintptr_t pool_start = (uintptr_t)pool->data;
  uintptr_t pool_end = pool_start + sizeof(pool->data);
  return ptr_addr >= pool_start && ptr_addr < pool_end;
}

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

    /* Pool memory cannot be reallocated - must use heap */
    if (MPZ_HAS_POOL(ctx) && is_pool_memory(x, MPZ_POOL(ctx))) {
      /* Allocate new heap memory and copy from pool */
      mp_limb *new_p = (mp_limb*)mrb_malloc(MPZ_MRB(ctx), size * sizeof(mp_limb));
      if (x->p) {
        memcpy(new_p, x->p, old_sz * sizeof(mp_limb));
      }
      x->p = new_p;
    } else {
      /* Regular heap reallocation */
      x->p = (mp_limb*)mrb_realloc(MPZ_MRB(ctx), x->p, size * sizeof(mp_limb));
    }

    /* Zero-initialize new limbs */
    for (size_t i = old_sz; i < size; i++) {
      x->p[i] = 0;
    }
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
    mpz_realloc(ctx, y, 2);
    y->p[1] = (mp_limb)HIGH(u);
    y->p[0] = (mp_limb)LOW(u);
    return;
  }
#endif
  mpz_realloc(ctx, y, 1);
  y->p[0] = (mp_limb)u;
}


static void
mpz_set_uint64(mpz_ctx_t *ctx, mpz_t *y, uint64_t u)
{
  size_t len = 0;

  for (uint64_t u0=u; u0; u0>>=DIG_SIZE,len++)
    ;
  y->sn = (u != 0);
  mpz_realloc(ctx, y, len);
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



/* Clear pool-aware mpz_t */
static void
mpz_clear_pool(mpz_ctx_t *ctx, mpz_t *s, mpz_pool_t *pool)
{
  if (s->p && !is_pool_memory(s, pool)) {
    mrb_free(MPZ_MRB(ctx), s->p);
  }
  s->p = NULL;
  s->sn = 0;
  s->sz = 0;
}

static void
mpz_clear(mpz_ctx_t *ctx, mpz_t *s)
{
  if (s->p) {
    if (MPZ_HAS_POOL(ctx) && is_pool_memory(s, MPZ_POOL(ctx))) {
      /* Pool memory - don't free, just mark as unused */
    } else {
      mrb_free(MPZ_MRB(ctx), s->p);
    }
    s->p = NULL;
  }
  s->sn = 0;
  s->sz = 0;
}

static void
mpz_move(mpz_ctx_t *ctx, mpz_t *y, mpz_t *x)
{
  mpz_clear(ctx, y);
  y->sn = x->sn;
  y->sz = x->sz;
  y->p = x->p;
  x->p = NULL;
  x->sn = 0;
  x->sz = 0;
}

/* Forward declarations for context-aware functions */
static void mpz_realloc(mpz_ctx_t *ctx, mpz_t *x, size_t size);
static void mpz_clear(mpz_ctx_t *ctx, mpz_t *s);
static void mpz_move(mpz_ctx_t *ctx, mpz_t *y, mpz_t *x);

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
static void
uadd(mpz_t *z, mpz_t *x, mpz_t *y)
{
  /* Core multi-limb addition with carry propagation */
  mp_dbl_limb c = 0;
  size_t i;

  /* Add overlapping limbs from both operands */
  for (i = 0; i < x->sz; i++) {
    c += (mp_dbl_limb)y->p[i] + (mp_dbl_limb)x->p[i];
    z->p[i] = LOW(c);
    c >>= DIG_SIZE;
  }

  /* Add remaining limbs from larger operand */
  for (; i < y->sz; i++) {
    c += y->p[i];
    z->p[i] = LOW(c);
    c >>= DIG_SIZE;
  }

  /* Store final carry */
  z->p[y->sz] = (mp_limb)c;
}

/* z = y - x, ignoring sign */
/* precondition: abs(y) >= abs(x) */
/* Core subtraction algorithm for unsigned operands */
static void
usub(mpz_t *z, mpz_t *y, mpz_t *x)
{
  /* Core multi-limb subtraction with borrow propagation */
  mp_dbl_limb_signed b = 0;
  size_t i;

  /* Subtract overlapping limbs from both operands */
  for (i = 0; i < x->sz; i++) {
    b += (mp_dbl_limb_signed)y->p[i];
    b -= (mp_dbl_limb_signed)x->p[i];
    z->p[i] = LOW(b);
    b = HIGH(b);
  }

  /* Process remaining limbs from minuend with borrow */
  for (; i < y->sz; i++) {
    b += y->p[i];
    z->p[i] = LOW(b);
    b = HIGH(b);
  }

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

/* Core signed addition algorithm - handles sign logic and delegates to uadd_core/usub_core */
static void
mpz_add_core(mpz_t *z, mpz_t *x, mpz_t *y)
{
  if (zero_p(x)) {
    /* Copy y to z - z must have enough space allocated */
    for (size_t i = 0; i < y->sz && i < z->sz; i++) {
      z->p[i] = y->p[i];
    }
    z->sz = (y->sz < z->sz) ? y->sz : z->sz;
    z->sn = y->sn;
    return;
  }
  if (zero_p(y)) {
    /* Copy x to z - z must have enough space allocated */
    for (size_t i = 0; i < x->sz && i < z->sz; i++) {
      z->p[i] = x->p[i];
    }
    z->sz = (x->sz < z->sz) ? x->sz : z->sz;
    z->sn = x->sn;
    return;
  }

  if (x->sn > 0 && y->sn > 0) {
    /* Both positive */
    uadd(z, x, y);
    z->sn = 1;
  }
  else if (x->sn < 0 && y->sn < 0) {
    /* Both negative */
    uadd(z, x, y);
    z->sn = -1;
  }
  else {
    /* Signs differ */
    int mg = ucmp(x, y);

    if (mg == 0) {
      zero(z);
    }
    else if (mg > 0) {  /* abs(y) < abs(x) */
      usub(z, x, y);
      z->sn = (x->sn > 0 && y->sn < 0) ? 1 : (-1);
    }
    else { /* abs(y) > abs(x) */
      usub(z, y, x);
      z->sn = (x->sn < 0 && y->sn > 0) ? 1 : (-1);
    }
  }
  trim(z);
}

/* z = x + y */
static void
mpz_add(mpz_ctx_t *ctx, mpz_t *zz, mpz_t *x, mpz_t *y)
{
  size_t estimated_size = ((x->sz > y->sz) ? x->sz : y->sz) + 1;

  /* Ensure destination is properly initialized and sized for heap fallback */
  mpz_init(ctx, zz);
  mpz_realloc(ctx, zz, estimated_size);

  MPZ_UNIFIED_BINARY_OP(ctx, mpz_add_core, zz, x, y, estimated_size);
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

/* z = x - y  -- just use mpz_add - I'm lazy */
static void
mpz_sub(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mpz_t *y)
{
  mpz_t u;

  /* Initialize u as a view of y with negated sign - no new memory allocated */
  u.p = y->p;
  u.sz = y->sz;
  u.sn = -(y->sn);
  mpz_add(ctx, z, x, &u);
  /* No mpz_clear needed since u.p points to y->p (no separate allocation) */
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

/* Sliding window multiplication for improved cache performance */
/* Window size optimized for L1 cache (4 limbs = 16 bytes) */
#define WINDOW_SIZE 4

/* Blocked multiplication for large operands with controlled memory overhead */
/* Block size optimized for cache efficiency while minimizing overhead */
#define BLOCK_SIZE 8  /* 8 limbs = 32 bytes per block */

/* Multiply window: result[offset..] += a[a_start..a_end) * b[b_start..b_end) */
static void
multiply_window(mp_limb *result, size_t offset,
                const mp_limb *a, size_t a_start, size_t a_len,
                const mp_limb *b, size_t b_start, size_t b_len)
{
  for (size_t i = 0; i < a_len; i++) {
    mp_limb u0 = a[a_start + i];
    if (u0 == 0) continue;

    mp_dbl_limb cc = 0;
    size_t j;
    for (j = 0; j < b_len; j++) {
      mp_limb v0 = b[b_start + j];
      size_t pos = offset + i + j;
      cc += (mp_dbl_limb)result[pos] + (mp_dbl_limb)u0 * (mp_dbl_limb)v0;
      result[pos] = LOW(cc);
      cc = HIGH(cc);
    }

    // Propagate carries beyond window
    size_t carry_pos = offset + i + j;
    while (cc) {
      cc += (mp_dbl_limb)result[carry_pos];
      result[carry_pos] = LOW(cc);
      cc = HIGH(cc);
      carry_pos++;
    }
  }
}

/* Core sliding window multiplication algorithm - works with pre-allocated result */
static void
mpz_mul_sliding_window_core(mpz_t *result, mpz_t *first, mpz_t *second)
{
  // Initialize result
  zero(result);

  // Process first operand in windows
  for (size_t a_start = 0; a_start < first->sz; a_start += WINDOW_SIZE) {
    size_t a_end = a_start + WINDOW_SIZE;
    if (a_end > first->sz) a_end = first->sz;
    size_t a_len = a_end - a_start;

    // Process second operand in windows
    for (size_t b_start = 0; b_start < second->sz; b_start += WINDOW_SIZE) {
      size_t b_end = b_start + WINDOW_SIZE;
      if (b_end > second->sz) b_end = second->sz;
      size_t b_len = b_end - b_start;

      // Multiply current windows and add to result
      multiply_window(result->p, a_start + b_start,
                     first->p, a_start, a_len,
                     second->p, b_start, b_len);
    }
  }

  result->sn = first->sn * second->sn;
  trim(result);
}

/* Unified sliding window multiplication - tries pool first, falls back to heap */
static int
mpz_mul_sliding_window(mpz_ctx_t *ctx, mpz_t *result, mpz_t *first, mpz_t *second)
{
  // Only use sliding window for medium-sized operands where cache benefits matter
  size_t max_limbs = (first->sz > second->sz) ? first->sz : second->sz;
  if (max_limbs < 8 || max_limbs > 64) {
    return 0; // Use classical multiplication
  }

  size_t result_size = first->sz + second->sz + 1;

  /* Ensure result is properly initialized and sized for heap fallback */
  mpz_realloc(ctx, result, result_size);

  MPZ_UNIFIED_BINARY_OP_INT(ctx, mpz_mul_sliding_window_core, result, first, second, result_size, 1);
}


/* Memory estimation for blocked multiplication */
static size_t
estimate_blocked_memory(size_t a_limbs, size_t b_limbs)
{
  // Block buffer: BLOCK_SIZE * 2 limbs for intermediate results
  size_t block_buffer = BLOCK_SIZE * 2 * sizeof(mp_limb);

  // Result allocation (already required)
  size_t result_size = (a_limbs + b_limbs + 1) * sizeof(mp_limb);

  // Total memory requirement
  return result_size + block_buffer;
}

/* Check if blocked multiplication should be used */
static int
should_use_blocked_multiplication(size_t a_limbs, size_t b_limbs)
{
  size_t max_limbs = (a_limbs > b_limbs) ? a_limbs : b_limbs;

  // Only use for large operands where block benefits outweigh overhead
  // Target range: 32-128 limbs (beyond sliding window, before classical fallback)
  if (max_limbs < 32 || max_limbs > 128) {
    return 0;
  }

  // Memory constraint check - stay within 2x limit
  size_t classical_memory = (a_limbs + b_limbs + 1) * sizeof(mp_limb);
  size_t blocked_memory = estimate_blocked_memory(a_limbs, b_limbs);

  return (blocked_memory <= classical_memory * 2);
}

/* Multiply single block: result_block = a_block * b_block */
static void
multiply_block(mp_limb *result_block,
               const mp_limb *a_block, size_t a_len,
               const mp_limb *b_block, size_t b_len)
{
  // Initialize result block to zero
  for (size_t i = 0; i < BLOCK_SIZE * 2; i++) {
    result_block[i] = 0;
  }

  // Classical multiplication within block
  for (size_t i = 0; i < a_len; i++) {
    mp_limb u = a_block[i];
    if (u == 0) continue;

    mp_dbl_limb carry = 0;
    for (size_t j = 0; j < b_len; j++) {
      mp_limb v = b_block[j];
      carry += (mp_dbl_limb)result_block[i + j] +
               (mp_dbl_limb)u * (mp_dbl_limb)v;
      result_block[i + j] = LOW(carry);
      carry = HIGH(carry);
    }

    // Propagate final carry within block bounds
    if (carry && (i + b_len < BLOCK_SIZE * 2)) {
      result_block[i + b_len] = LOW(carry);
    }
  }
}

/* Add block result to main result at specified offset */
static void
add_block_to_result(mpz_t *result, const mp_limb *block_result,
                   size_t offset, size_t block_len)
{
  mp_dbl_limb carry = 0;

  // Add block result to main result with carry propagation
  for (size_t i = 0; i < block_len && (offset + i) < result->sz; i++) {
    carry += (mp_dbl_limb)result->p[offset + i] + (mp_dbl_limb)block_result[i];
    result->p[offset + i] = LOW(carry);
    carry = HIGH(carry);
  }

  // Propagate remaining carry beyond block boundary
  size_t carry_pos = offset + block_len;
  while (carry && carry_pos < result->sz) {
    carry += (mp_dbl_limb)result->p[carry_pos];
    result->p[carry_pos] = LOW(carry);
    carry = HIGH(carry);
    carry_pos++;
  }
}

/* Blocked multiplication - processes operands in constant-memory blocks */
static int
mpz_mul_blocked(mpz_ctx_t *ctx, mpz_t *result, mpz_t *first, mpz_t *second)
{
  // Algorithm selection check
  if (!should_use_blocked_multiplication(first->sz, second->sz)) {
    return 0;
  }

  // Initialize result
  mpz_realloc(ctx, result, first->sz + second->sz + 1);
  zero(result);

  // Block buffer allocation (constant memory - key advantage)
  mp_limb block_result[BLOCK_SIZE * 2];

  // Calculate block counts
  size_t a_blocks = (first->sz + BLOCK_SIZE - 1) / BLOCK_SIZE;
  size_t b_blocks = (second->sz + BLOCK_SIZE - 1) / BLOCK_SIZE;

  // Process all block combinations
  for (size_t i = 0; i < a_blocks; i++) {
    for (size_t j = 0; j < b_blocks; j++) {

      // Calculate block boundaries for operand A
      size_t a_start = i * BLOCK_SIZE;
      size_t a_len = (a_start + BLOCK_SIZE <= first->sz) ?
                     BLOCK_SIZE : (first->sz - a_start);

      // Calculate block boundaries for operand B
      size_t b_start = j * BLOCK_SIZE;
      size_t b_len = (b_start + BLOCK_SIZE <= second->sz) ?
                     BLOCK_SIZE : (second->sz - b_start);

      // Multiply current blocks
      multiply_block(block_result,
                    &first->p[a_start], a_len,
                    &second->p[b_start], b_len);

      // Add block result to main result at appropriate offset
      size_t result_offset = (i + j) * BLOCK_SIZE;
      add_block_to_result(result, block_result, result_offset, a_len + b_len);
    }
  }

  result->sn = first->sn * second->sn;
  trim(result);
  return 1; // Success
}

/* w = u * v */
/* Memory-efficient multiplication with algorithm selection hierarchy */
static void
mpz_mul(mpz_ctx_t *ctx, mpz_t *ww, mpz_t *u, mpz_t *v)
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

  // Algorithm selection hierarchy:
  // 1. Try blocked multiplication for large operands (32-128 limbs)
  // 2. Try pool sliding window for medium operands (8-64 limbs) - uses pool memory
  // 3. Fallback to traditional sliding window if pool fails
  // 4. Fallback to classical multiplication
  mpz_t w;
  mpz_init(ctx, &w);

  if (mpz_mul_blocked(ctx, &w, first, second)) {
    mpz_move(ctx, ww, &w);
    return;
  }

  // Try unified sliding window (pool first, then heap fallback)
  if (mpz_mul_sliding_window(ctx, &w, first, second)) {
    mpz_move(ctx, ww, &w);
    return;
  }

  // Fallback to classical multiplication for small/large operands
  mpz_realloc(ctx, &w, first->sz + second->sz + 1);

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
  mpz_move(ctx, ww, &w);
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
urshift(mpz_ctx_t *ctx, mpz_t *c1, mpz_t *a, size_t n)
{
  mrb_assert(n < DIG_SIZE);

  if (n == 0)
    mpz_set(ctx, c1, a);
  else if (uzero_p(a)) {
    zero(c1);
  }
  else {
    mpz_t c;
    mp_limb cc = 0;
    mp_dbl_limb rm = (((mp_dbl_limb)1<<n) - 1);

    mpz_init(ctx, &c);
    mpz_realloc(ctx, &c, a->sz);
    for (size_t i=a->sz-1;; i--) {
      c.p[i] = ((a->p[i] >> n) | cc) & DIG_MASK;
      cc = (a->p[i] & rm) << (DIG_SIZE - n);
      if (i == 0) break;
    }
    trim(&c);
    mpz_move(ctx, c1, &c);
  }
}

/* c1 = a<<n */
/* n must be < DIG_SIZE */
static void
ulshift(mpz_ctx_t *ctx, mpz_t *c1, mpz_t *a, size_t n)
{
  mrb_assert(n < DIG_SIZE);
  if (n == 0)
    mpz_set(ctx, c1, a);
  else if (uzero_p(a)) {
    zero(c1);
  }
  else {
    mp_limb cc = 0;
    mpz_t c;
    mp_limb rm = (((mp_dbl_limb)1<<n) - 1) << (DIG_SIZE-n);

    mpz_init(ctx, &c);
    mpz_realloc(ctx, &c, a->sz+1);

    size_t i;
    for (i=0; i<a->sz; i++) {
      c.p[i] = ((a->p[i] << n) | cc) & DIG_MASK;
      cc = (a->p[i] & rm) >> (DIG_SIZE-n);
    }
    c.p[i] = cc;
    trim(&c);
    mpz_move(ctx, c1, &c);
  }
}

/* Fast division by single limb */
static void
mpz_div_limb(mpz_ctx_t *ctx, mpz_t *q, mpz_t *r, mpz_t *x, mp_limb d)
{
  mrb_state *mrb = MPZ_MRB(ctx);

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
      mpz_set(ctx, q, x);
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
        mpz_realloc(ctx, q, new_size);

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
    mpz_realloc(ctx, r, 1);
    r->p[0] = x->p[0] & (d - 1);
    r->sz = (r->p[0] == 0) ? 0 : 1;
    r->sn = (r->sz == 0) ? 0 : 1;
    return;
  }

  /* General single-limb division */
  if (x->sz == 1) {
    /* Both dividend and divisor are single limb */
    mpz_realloc(ctx, q, 1);
    mpz_realloc(ctx, r, 1);

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
  mpz_realloc(ctx, q, n);

  mp_dbl_limb remainder = 0;

  /* Process from most significant limb to least significant */
  for (size_t i = n; i > 0; i--) {
    remainder = (remainder << DIG_SIZE) + x->p[i-1];
    q->p[i-1] = (mp_limb)(remainder / d);
    remainder = remainder % d;
  }

  /* Set remainder */
  mpz_realloc(ctx, r, 1);
  r->p[0] = (mp_limb)remainder;
  r->sz = (remainder == 0) ? 0 : 1;
  r->sn = (r->sz == 0) ? 0 : 1;

  /* Trim leading zeros from quotient */
  trim(q);
  q->sn = (q->sz == 0) ? 0 : 1;
}

/* Core Knuth Algorithm D division loop */
static void
udiv_core(mpz_t *quotient, mpz_t *dividend, mpz_t *divisor, size_t xd, size_t yd)
{
  mp_dbl_limb z = divisor->p[yd-1];

  if (xd >= yd) {
    for (size_t j = xd - yd;; j--) {
      mp_dbl_limb qhat;
      mp_dbl_limb rhat;

      if (j + yd == xd) {
        /* Only one high limb available */
        mp_dbl_limb dividend_val = (((mp_dbl_limb)0 << DIG_SIZE) + dividend->p[j+yd-1]);
        qhat = dividend_val / z;
        rhat = dividend_val % z;
      }
      else {
        /* Two limbs available - use enhanced estimation */
        mp_dbl_limb dividend_val = ((mp_dbl_limb)dividend->p[j+yd] << DIG_SIZE) + dividend->p[j+yd-1];
        qhat = dividend_val / z;
        rhat = dividend_val % z;

        /* Three-limb pre-adjustment when available */
        if (yd >= 2 && j+yd-2 < dividend->sz && divisor->p[yd-2] != 0) {
          mp_dbl_limb y_second = divisor->p[yd-2];
          mp_dbl_limb x_third = dividend->p[j+yd-2];

          if (qhat > 0) {
            mp_dbl_limb left = qhat * y_second;
            mp_dbl_limb right = (rhat << DIG_SIZE) + x_third;

            if (qhat >= ((mp_dbl_limb)1 << DIG_SIZE) || left > right) {
              qhat--;
              rhat += z;
            }
          }
        }
      }

      /* Enhanced qhat refinement step */
      if (yd > 1) {
        mp_dbl_limb y_second = divisor->p[yd-2];
        mp_dbl_limb x_third = (j+yd-2 < dividend->sz) ? dividend->p[j+yd-2] : 0;
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
        mp_dbl_limb_signed borrow = 0;
        size_t i;

        for (i = 0; i < yd; i++) {
          mp_dbl_limb product = qhat * divisor->p[i];
          mp_dbl_limb_signed diff = (mp_dbl_limb_signed)dividend->p[i+j] - (mp_dbl_limb_signed)LOW(product) + borrow;
          dividend->p[i+j] = LOW(diff);
          borrow = HIGH(diff) - (mp_dbl_limb_signed)HIGH(product);
        }

        /* Handle final borrow propagation */
        if (i+j < dividend->sz) {
          borrow += (mp_dbl_limb_signed)dividend->p[i+j];
          dividend->p[i+j] = LOW(borrow);
          borrow = HIGH(borrow);
        }

        /* Correction: if borrow is negative, qhat was too large, add back */
        if (borrow < 0) {
          qhat--;
          mp_dbl_limb carry = 0;
          for (i = 0; i < yd; i++) {
            carry += (mp_dbl_limb)dividend->p[i+j] + (mp_dbl_limb)divisor->p[i];
            dividend->p[i+j] = LOW(carry);
            carry = HIGH(carry);
          }
          if (i+j < dividend->sz && carry > 0) {
            dividend->p[i+j] += (mp_limb)carry;
          }
        }
      }

      quotient->p[j] = (mp_limb)qhat;
      if (j == 0) break;
    }
  }
}

/* internal routine to compute x/y and x%y ignoring signs */
/* qq = xx/yy; rr = xx%yy */
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
    mpz_div_limb(ctx, qq, rr, xx, yy->p[0]);
    return;
  }

  mrb_assert(yy->sn != 0);      /* divided by zero */
  mrb_assert(yy->sz > 0);       /* divided by zero */

  /* Try pool allocation first for medium-sized operands */
  size_t dividend_limbs = xx->sz;
  size_t divisor_limbs = yy->sz;
  if (dividend_limbs >= 4 && dividend_limbs <= 64 && divisor_limbs <= 32) {
    /* Estimate space needed for temporary variables */
    size_t temp_space = dividend_limbs + 1 + divisor_limbs + (dividend_limbs - divisor_limbs + 1);
    if (temp_space <= BIGINT_POOL_DEFAULT_SIZE / 3) {
      /* Use manual pool management with designated initializer */
      mpz_pool_t pool_storage = {.capacity = BIGINT_POOL_DEFAULT_SIZE, .active = 1};
      mpz_pool_t *pool = &pool_storage;

      mpz_t q_temp, x_temp, y_temp;

      /* Initialize temporary variables in pool */
      mpz_init_pool(ctx, &q_temp, pool, dividend_limbs - divisor_limbs + 1);
      mpz_init_pool(ctx, &x_temp, pool, dividend_limbs + 1);
      mpz_init_pool(ctx, &y_temp, pool, divisor_limbs);

      if (is_pool_memory(&q_temp, pool) &&
          is_pool_memory(&x_temp, pool) &&
          is_pool_memory(&y_temp, pool)) {

        /* Perform division using pool-allocated temporaries */
        size_t yd = digits(yy);
        size_t ns = lzb(yy->p[yd-1]);

        /* Manual shift instead of ulshift to avoid mpz_move issues with pool memory */
        if (ns == 0) {
          /* No shift needed - direct copy */
          for (size_t i = 0; i < xx->sz; i++) {
            x_temp.p[i] = xx->p[i];
          }
          x_temp.sz = xx->sz;

          for (size_t i = 0; i < yy->sz; i++) {
            y_temp.p[i] = yy->p[i];
          }
          y_temp.sz = yy->sz;
        }
        else {
          /* Manual left shift by ns bits */
          mp_limb cc = 0;
          mp_limb rm = (((mp_dbl_limb)1<<ns) - 1) << (DIG_SIZE-ns);

          for (size_t i = 0; i < xx->sz; i++) {
            x_temp.p[i] = ((xx->p[i] << ns) | cc) & DIG_MASK;
            cc = (xx->p[i] & rm) >> (DIG_SIZE-ns);
          }
          x_temp.p[xx->sz] = cc;
          x_temp.sz = xx->sz + (cc ? 1 : 0);

          cc = 0;
          for (size_t i = 0; i < yy->sz; i++) {
            y_temp.p[i] = ((yy->p[i] << ns) | cc) & DIG_MASK;
            cc = (yy->p[i] & rm) >> (DIG_SIZE-ns);
          }
          if (cc && yy->sz < y_temp.sz) {
            y_temp.p[yy->sz] = cc;
            y_temp.sz = yy->sz + 1;
          }
          else {
            y_temp.sz = yy->sz;
          }
        }

        size_t xd = digits(&x_temp);

        /* Zero-initialize quotient */
        for (size_t i = 0; i < q_temp.sz; i++) {
          q_temp.p[i] = 0;
        }

        /* Use core division algorithm */
        udiv_core(&q_temp, &x_temp, &y_temp, xd, yd);

        x_temp.sz = yy->sz;

        /* Copy results from pool to heap-allocated outputs */
        trim(&q_temp);
        mpz_realloc(ctx, qq, q_temp.sz);
        for (size_t i = 0; i < q_temp.sz; i++) {
          qq->p[i] = q_temp.p[i];
        }
        qq->sz = q_temp.sz;
        qq->sn = (qq->sz == 0) ? 0 : 1;

        /* Manual right shift for remainder to avoid mpz_move issues */
        if (ns == 0) {
          /* No shift needed - direct copy to heap result */
          mpz_realloc(ctx, rr, x_temp.sz);
          for (size_t i = 0; i < x_temp.sz; i++) {
            rr->p[i] = x_temp.p[i];
          }
          rr->sz = x_temp.sz;
          rr->sn = (rr->sz == 0) ? 0 : 1;
        }
        else {
          /* Manual right shift by ns bits */
          mpz_realloc(ctx, rr, x_temp.sz);
          mp_limb cc = 0;
          mp_limb lm = ((mp_dbl_limb)1 << ns) - 1;

          for (ssize_t i = (ssize_t)x_temp.sz - 1; i >= 0; i--) {
            rr->p[i] = ((x_temp.p[i] >> ns) | cc) & DIG_MASK;
            cc = (x_temp.p[i] & lm) << (DIG_SIZE - ns);
          }
          rr->sz = x_temp.sz;
          trim(rr);
          rr->sn = (rr->sz == 0) ? 0 : 1;
        }

        pool_storage.active = 0;
        return; /* Success with pool-based division */
      }
      pool_storage.active = 0;
    }
  }

  /* Heap fallback for large numbers or when pool allocation fails */
  mpz_t q, x, y;
  mpz_init(ctx, &q);
  mpz_init(ctx, &x);
  mpz_init(ctx, &y);
  mpz_realloc(ctx, &x, xx->sz+1);
  size_t yd = digits(yy);
  size_t ns = lzb(yy->p[yd-1]);
  ulshift(ctx, &x, xx, ns);
  ulshift(ctx, &y, yy, ns);
  size_t xd = digits(&x);
  mpz_realloc(ctx, &q, xd-yd+1);  // Quotient has xd-yd+1 digits maximum

  /* Use core division algorithm */
  udiv_core(&q, &x, &y, xd, yd);
  x.sz = yy->sz;
  urshift(ctx, rr, &x, ns);
  trim(&q);
  mpz_move(ctx, qq, &q);
  mpz_clear(ctx, &x);
  mpz_clear(ctx, &y);
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
  mpz_t q;
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

  /* Barrett reduction for moderate-sized moduli */
  if (y->sz >= 2 && y->sz <= 8 && x->sz >= y->sz + 2) {
    mpz_t mu;
    mpz_init(ctx, &mu);
    mpz_barrett_mu(ctx, &mu, y);
    mpz_barrett_reduce(ctx, r, x, y, &mu);
    r->sn = sn;
    if (uzero_p(r))
      r->sn = 0;
    mpz_clear(ctx, &mu);
    return;
  }

  /* General division fallback */
  mpz_init(ctx, &q);
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
mpz_init_set_str(mpz_ctx_t *ctx, mpz_t *x, const char *s, mrb_int len, mrb_int base)
{
  int retval = 0;
  short sn;
  uint8_t k;

  mpz_init(ctx, x);
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
    mpz_mul_int(ctx, x, base);
    mpz_add_int(ctx, x, k);
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
mpz_mul_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e)
{
  if (e==0)
    mpz_set(ctx, z, x);
  else {
    short sn = x->sn;
    size_t digs = e / DIG_SIZE;
    size_t bs = e % DIG_SIZE;
    mpz_t y;

    mpz_init(ctx, &y);
    mpz_realloc(ctx, &y, x->sz+digs);
    for (size_t i=0;i<x->sz;i++)
      y.p[i+digs] = x->p[i];
    if (bs) {
      ulshift(ctx, z, &y, bs);
      mpz_clear(ctx, &y);
    }
    else {
      mpz_move(ctx, z, &y);
    }
    z->sn = sn;
  }
}

static void
mpz_div_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e)
{
  short sn = x->sn;
  if (e==0)
    mpz_set(ctx, z, x);
  else {
    size_t digs = e / DIG_SIZE;
    size_t bs = e % DIG_SIZE;
    mpz_t y;

    mpz_init(ctx, &y);
    mpz_realloc(ctx, &y, x->sz-digs);
    for (size_t i=0; i < x->sz-digs; i++)
      y.p[i] = x->p[i+digs];
    if (bs) {
      urshift(ctx, z, &y, bs);
      mpz_clear(ctx, &y);
    }
    else {
      mpz_move(ctx, z, &y);
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
  mpz_set(ctx, x, y);
  x->sn = -(y->sn);
}

/* Fast modular reduction by power of 2: z = x mod 2^e */
static void
mpz_mod_2exp(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mrb_int e)
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
    mpz_set(ctx, z, x);
    return;
  }

  /* Need to mask off high bits */
  size_t result_sz = eint + (bs > 0 ? 1 : 0);
  mpz_realloc(ctx, z, result_sz);
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
mpz_and(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mpz_t *y)
{
  if (zero_p(x) || zero_p(y)) {
    zero(z);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;
  mpz_realloc(ctx, z, max_sz);
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
mpz_or(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mpz_t *y)  /* not the most efficient way to do this */
{
  if (zero_p(x)) {
    mpz_set(ctx, z, y);
    return;
  }
  if (zero_p(y)) {
    mpz_set(ctx, z, x);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;
  mpz_realloc(ctx, z, max_sz);
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
mpz_xor(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x, mpz_t *y)  /* not the most efficient way to do this */
{
  if (zero_p(x)) {
    mpz_set(ctx, z, y);
    return;
  }
  if (zero_p(y)) {
    mpz_set(ctx, z, x);
    return;
  }
  mrb_assert(x->sz > 0 || y->sz > 0);

  size_t max_sz = (x->sz > y->sz) ? x->sz : y->sz;
  mpz_realloc(ctx, z, max_sz);
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
mpz_pow(mpz_ctx_t *ctx, mpz_t *zz, mpz_t *x, mrb_int e)
{
  mpz_t t;
  mrb_uint mask = 1ULL<<(sizeof(mrb_int)*8-1);

  if (e==0) {
    mpz_set_int(ctx, zz, 1L);
    return;
  }

  mpz_init_set(ctx, &t, x);
  for (;!(mask &e); mask>>=1)
    ;
  mask>>=1;
  for (;mask!=0; mask>>=1) {
    mpz_mul(ctx, &t, &t, &t);
    if (e & mask)
      mpz_mul(ctx, &t, &t, x);
  }
  mpz_move(ctx, zz, &t);
}

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

  mpz_t t, b;
  mpz_init_set_int(ctx, &t, 1);
  mpz_init_set(ctx, &b, x);

  /* Optimize with Barrett reduction for moderate-sized moduli */
  mpz_t mu, temp;
  int use_barrett = (n->sz >= 2 && n->sz <= 8);
  mpz_init(ctx, &temp);
  if (use_barrett) {
    mpz_init(ctx, &mu);
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

  mpz_clear(ctx, &temp);
  if (use_barrett) {
    mpz_clear(ctx, &mu);
  }
  mpz_move(ctx, zz, &t);
  mpz_clear(ctx, &b);
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

  mpz_t t, b;
  mpz_init_set_int(ctx, &t, 1);
  mpz_init_set(ctx, &b, x);

  /* Optimize with Barrett reduction for moderate-sized moduli */
  mpz_t mu, temp;
  int use_barrett = (n->sz >= 2 && n->sz <= 8);
  mpz_init(ctx, &temp);
  if (use_barrett) {
    mpz_init(ctx, &mu);
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

  mpz_clear(ctx, &temp);
  if (use_barrett) {
    mpz_clear(ctx, &mu);
  }
  mpz_move(ctx, zz, &t);
  mpz_clear(ctx, &b);
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
  mpz_init(ctx, x);
  mpz_realloc(ctx, x, y->sz);
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
  mpz_t a, b;

  /* Handle special cases */
  if (zero_p(aa)) {
    mpz_abs(ctx, gg, bb);
    return;
  }
  if (zero_p(bb)) {
    mpz_abs(ctx, gg, aa);
    return;
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
    return;
  }

  /* Fast path for powers of 2 */
  if (mpz_power_of_2_p(aa)) {
    size_t a_zeros = mpz_trailing_zeros(aa);
    size_t b_zeros = mpz_trailing_zeros(bb);
    size_t min_zeros = (a_zeros < b_zeros) ? a_zeros : b_zeros;

    mpz_init_set_int(ctx, gg, 1);
    mpz_mul_2exp(ctx, gg, gg, min_zeros);
    return;
  }
  if (mpz_power_of_2_p(bb)) {
    size_t a_zeros = mpz_trailing_zeros(aa);
    size_t b_zeros = mpz_trailing_zeros(bb);
    size_t min_zeros = (a_zeros < b_zeros) ? a_zeros : b_zeros;

    mpz_init_set_int(ctx, gg, 1);
    mpz_mul_2exp(ctx, gg, gg, min_zeros);
    return;
  }

  mpz_abs(ctx, &a, aa);
  mpz_abs(ctx, &b, bb);

  /* Find power of 2 that divides both a and b */
  size_t a_zeros = mpz_trailing_zeros(&a);
  size_t b_zeros = mpz_trailing_zeros(&b);
  size_t shift = (a_zeros < b_zeros) ? a_zeros : b_zeros;

  /* Remove common factors of 2 */
  if (shift > 0) {
    mpz_div_2exp(ctx, &a, &a, shift);
    mpz_div_2exp(ctx, &b, &b, shift);
  }

  /* Remove remaining factors of 2 from a */
  if (a_zeros > shift) {
    mpz_div_2exp(ctx, &a, &a, a_zeros - shift);
  }

  /* Remove remaining factors of 2 from b */
  if (b_zeros > shift) {
    mpz_div_2exp(ctx, &b, &b, b_zeros - shift);
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
      mpz_init(ctx, &temp_a);
      mpz_init(ctx, &temp_b);
      mpz_init_set(ctx, &u0_a, &a);
      mpz_init_set(ctx, &v0_b, &b);
      mpz_init_set(ctx, &u1_a, &a);
      mpz_init_set(ctx, &v1_b, &b);

      /* Compute u0*a, v0*b, u1*a, v1*b */
      mpz_mul_int(ctx, &u0_a, u0);
      mpz_mul_int(ctx, &v0_b, v0);
      mpz_mul_int(ctx, &u1_a, u1);
      mpz_mul_int(ctx, &v1_b, v1);

      /* temp_a = u0*a + v0*b */
      mpz_add(ctx, &temp_a, &u0_a, &v0_b);

      /* temp_b = u1*a + v1*b */
      mpz_add(ctx, &temp_b, &u1_a, &v1_b);

      /* Update a and b */
      mpz_set(ctx, &a, &temp_a);
      mpz_set(ctx, &b, &temp_b);

      mpz_clear(ctx, &temp_a);
      mpz_clear(ctx, &temp_b);
      mpz_clear(ctx, &u0_a);
      mpz_clear(ctx, &v0_b);
      mpz_clear(ctx, &u1_a);
      mpz_clear(ctx, &v1_b);

      /* Ensure a >= b after transformation */
      if (mpz_cmp(ctx, &a, &b) < 0) {
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
        mpz_div_2exp(ctx, &b, &b, b_trailing);
      }
    }

    /* Now both a and b are odd. Ensure a >= b */
    if (mpz_cmp(ctx, &a, &b) < 0) {
      /* In-place swap without temporary variable */
      mpz_t temp_holder = a;
      a = b;
      b = temp_holder;
    }

    /* Replace a with (a - b) */
    mpz_sub(ctx, &a, &a, &b);

    /* Remove factors of 2 from the result if it's even */
    if (a.sz > 0 && (a.p[0] & 1) == 0) {
      size_t a_trailing = mpz_trailing_zeros(&a);
      if (a_trailing > 0) {
        mpz_div_2exp(ctx, &a, &a, a_trailing);
      }
    }

  } while (!zero_p(&a));

  /* Restore common factors of 2 */
  mpz_mul_2exp(ctx, &b, &b, shift);

  trim(&b);
  mpz_move(ctx, gg, &b);
  mpz_clear(ctx, &a);
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
mpz_barrett_mu(mpz_ctx_t *ctx, mpz_t *mu, mpz_t *m)
{
  size_t k = mpz_bits(m);
  mpz_t temp;

  mpz_init_set_int(ctx, &temp, 1);
  mpz_mul_2exp(ctx, &temp, &temp, 2 * k);  /* temp = 2^(2k) */
  mpz_mdiv(ctx, mu, &temp, m);             /* mu = floor(2^(2k) / m) */
  mpz_clear(ctx, &temp);
}

/* Barrett reduction: r = x mod m using precomputed μ */
static void
mpz_barrett_reduce(mpz_ctx_t *ctx, mpz_t *r, mpz_t *x, mpz_t *m, mpz_t *mu)
{
  size_t k = mpz_bits(m);

  /* If x < m, then x mod m = x */
  if (mpz_cmp(ctx, x, m) < 0) {
    mpz_set(ctx, r, x);
    return;
  }

  mpz_t q1, q2, q3, r1, r2;
  mpz_init(ctx, &q1);
  mpz_init(ctx, &q2);
  mpz_init(ctx, &q3);
  mpz_init(ctx, &r1);
  mpz_init(ctx, &r2);

  /* Step 1: q1 = floor(x / 2^(k-1)) */
  if (k > 1) {
    mpz_div_2exp(ctx, &q1, x, k - 1);
  }
  else {
    mpz_set(ctx, &q1, x);
  }

  /* Step 2: q2 = q1 * μ */
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

  /* Step 7: Final correction - ensure 0 ≤ r < m */
  while (mpz_cmp(ctx, r, m) >= 0) {
    mpz_sub(ctx, r, r, m);
  }

  mpz_clear(ctx, &q1);
  mpz_clear(ctx, &q2);
  mpz_clear(ctx, &q3);
  mpz_clear(ctx, &r1);
  mpz_clear(ctx, &r2);
}

static void
mpz_sqrt(mpz_ctx_t *ctx, mpz_t *z, mpz_t *x)
{
  mrb_assert(x->sn >= 0);

  if (x->sz == 0) {
    // sqrt(0) = 0
    z->sn = 0;
    z->sz = 0;
    z->p = NULL;
    return;
  }

  // Use heap-only implementation for now
  size_t xbits = mpz_bits(x);
  size_t sbit = (xbits + 1) / 2;
  mpz_t s, t;
  mpz_init_set_int(ctx, &s, 1);
  mpz_mul_2exp(ctx, &s, &s, sbit);

  mpz_init(ctx, &t);

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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  struct RBigint *b = MRB_OBJ_ALLOC(mrb, MRB_TT_BIGINT, mrb->integer_class);
  if (x->sz <= RBIGINT_EMBED_SIZE_MAX) {
    RBIGINT_SET_EMBED_SIZE(b, x->sz);
    RBIGINT_SET_EMBED_SIGN(b, x->sn);
    if (x->p) memcpy(RBIGINT_EMBED_ARY(b), x->p, x->sz*sizeof(mp_limb));
    mpz_clear(&ctx, x);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init_set_int(&ctx, &x, n);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_set_int64(&ctx, &x, n);
  struct RBigint *b = bint_new(mrb, &x);
  return mrb_obj_value(b);
}
#endif

mrb_value
mrb_bint_new_uint64(mrb_state *mrb, uint64_t x)
{
  mpz_t z;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_init(&ctx, &z);
  mpz_set_uint64(&ctx, &z, x);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init_set_str(&ctx, &z, x, len, base);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  if (!RBIGINT_EMBED_P(b)) {
    mpz_clear(&ctx, &b->as.heap);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_t r;
  mpz_init(&ctx, &r);
  r.sn = sn;

  mrb_float b = (double)DIG_BASE;
  mrb_float bi = 1.0 / b;
  size_t rn;

  for (rn = 1; x >= b; rn++)
    x *= bi;

  mpz_realloc(&ctx, &r, rn);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  if (mrb_integer_p(y)) {
    mrb_int n = mrb_integer(y);
    if (int_fit_limb_p(n)) {
      mpz_init_set(&ctx, &z, &a);
      if ((n > 0) ^ (z.sn > 0)) {
        mpz_sub_int(&ctx, &z, n<0 ? -n : n);
      }
      else {
        mpz_add_int(&ctx, &z, n<0 ? -n : n);
      }
      struct RBigint *v = bint_new(mrb, &z);
      return mrb_obj_value(v);
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init(&ctx, &z);
  mpz_add(&ctx, &z, &a, &b);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y)) {
    mrb_int n = mrb_integer(y);
    if (int_fit_limb_p(n)) {
      mpz_init_set(&ctx, &z, &a);
      if ((n > 0) ^ (z.sn > 0)) {
        mpz_add_int(&ctx, &z, n<0 ? -n : n);
      }
      else {
        mpz_sub_int(&ctx, &z, n<0 ? -n : n);
      }
      struct RBigint *v = bint_new(mrb, &z);
      return mrb_obj_value(v);
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init(&ctx, &z);
  mpz_sub(&ctx, &z, &a, &b);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init(&ctx, &z);
  mpz_mul(&ctx, &z, &a, &b);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init(&ctx, &z);
  mpz_mdiv(&ctx, &z, &a, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_add_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_init_set_int(&ctx, &a, x);
  mpz_init_set_int(&ctx, &b, y);
  mpz_init(&ctx, &z);
  mpz_add(&ctx, &z, &a, &b);
  mpz_clear(&ctx, &a);
  mpz_clear(&ctx, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_sub_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_init_set_int(&ctx, &a, x);
  mpz_init_set_int(&ctx, &b, y);
  mpz_init(&ctx, &z);
  mpz_sub(&ctx, &z, &a, &b);
  mpz_clear(&ctx, &a);
  mpz_clear(&ctx, &b);
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_mul_ii(mrb_state *mrb, mrb_int x, mrb_int y)
{
  mpz_t a, b, z;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_init_set_int(&ctx, &a, x);
  mpz_init_set_int(&ctx, &b, y);
  mpz_init(&ctx, &z);
  mpz_mul(&ctx, &z, &a, &b);
  mpz_clear(&ctx, &a);
  mpz_clear(&ctx, &b);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init(&ctx, &z);
  mpz_mmod(&ctx, &z, &a, &b);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init(&ctx, &z);
  mpz_mod(&ctx, &z, &a, &b);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init(&ctx, &c);
  mpz_init(&ctx, &d);
  mpz_mdivmod(&ctx, &c, &d, &a, &b);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  return mpz_cmp(&ctx, &a, &b);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init(&ctx, &z);
  mpz_pow(&ctx, &z, &a, mrb_integer(y));

  struct RBigint *b = bint_new(mrb, &z);
  return mrb_obj_value(b);
}

mrb_value
mrb_bint_powm(mrb_state *mrb, mrb_value x, mrb_value exp, mrb_value mod)
{
  mpz_t a, b, c, z;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(mod)) {
    mrb_int m = mrb_integer(mod);
    if (m == 0) mrb_int_zerodiv(mrb);
    mpz_init_set_int(&ctx, &c, m);
  }
  else {
    mod = mrb_as_bint(mrb, mod);
    bint_as_mpz(RBIGINT(mod), &c);
    if (zero_p(&c) || uzero_p(&c)) {
      mrb_int_zerodiv(mrb);
    }
  }
  mpz_init(&ctx, &z);
  if (mrb_bigint_p(exp)) {
    bint_as_mpz(RBIGINT(exp), &b);
    if (b.sn < 0) goto raise;
    mpz_powm(&ctx, &z, &a, &b, &c);
  }
  else {
    mrb_int e = mrb_integer(exp);
    if (e < 0) goto raise;
    mpz_powm_i(&ctx, &z, &a, e, &c);
  }
  if (mrb_integer_p(mod)) mpz_clear(&ctx, &c);
  return bint_norm(mrb, bint_new(mrb, &z));

 raise:
  if (mrb_integer_p(mod)) mpz_clear(&ctx, &c);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_get_str(&ctx, RSTRING_PTR(str), len, base, &a);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init(&ctx, &c);
  mpz_and(&ctx, &c, &a, &b);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&a)) return y;
  if (zero_p(&b)) return x;
  mpz_init(&ctx, &c);
  mpz_or(&ctx, &c, &b, &a);
  return bint_norm(mrb, bint_new(mrb, &c));
}

mrb_value
mrb_bint_xor(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b, c;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  if (mrb_integer_p(y) && a.sn > 0) {
    mrb_int z = mrb_integer(y);
    if (z == 0) return x;
    if (0 < z && (mp_dbl_limb)z < DIG_BASE) {
      mpz_init_set(&ctx, &c, &a);
      c.p[0] ^= z;
      return bint_norm(mrb, bint_new(mrb, &c));
    }
  }
  y = mrb_as_bint(mrb, y);
  bint_as_mpz(RBIGINT(y), &b);
  if (zero_p(&a)) return y;
  if (zero_p(&b)) return x;
  mpz_init(&ctx, &c);
  mpz_xor(&ctx, &c, &a, &b);
  return bint_norm(mrb, bint_new(mrb, &c));
}

mrb_value
mrb_bint_neg(mrb_state *mrb, mrb_value x)
{
  mpz_t a, b;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(&ctx, &b);
  mpz_neg(&ctx, &b, &a);
  struct RBigint *b2 = bint_new(mrb, &b);
  /* no normalization */
  return mrb_obj_value(b2);
}

mrb_value
mrb_bint_rev(mrb_state *mrb, mrb_value x)
{
  mpz_t a, b;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(&ctx, &b);
  mpz_neg(&ctx, &b, &a);
  mpz_sub_int(&ctx, &b, 1);
  return bint_norm(mrb, bint_new(mrb, &b));
}

mrb_value
mrb_bint_lshift(mrb_state *mrb, mrb_value x, mrb_int width)
{
  mpz_t a, z;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(&ctx, &z);
  if (width < 0) {
    mpz_div_2exp(&ctx, &z, &a, -width);
  }
  else {
    mpz_mul_2exp(&ctx, &z, &a, width);
  }
  return bint_norm(mrb, bint_new(mrb, &z));
}

mrb_value
mrb_bint_rshift(mrb_state *mrb, mrb_value x, mrb_int width)
{
  mpz_t a, z;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(&ctx, &z);
  if (width < 0) {
    mpz_mul_2exp(&ctx, &z, &a, -width);
  }
  else {
    mpz_div_2exp(&ctx, &z, &a, width);
  }
  return bint_norm(mrb, bint_new(mrb, &z));
}

void
mrb_bint_copy(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mpz_t a, b;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  bint_as_mpz(RBIGINT(y), &b);
  mpz_init_set(&ctx, &a, &b);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_t z;
  mpz_init(&ctx, &z);
  mpz_sqrt(&ctx, &z, &a);

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
  size_t limb_len = (len + sizeof(mp_limb) - 1) / sizeof(mp_limb);
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_init(&ctx, &z);
  mpz_realloc(&ctx, &z, limb_len);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  bint_as_mpz(RBIGINT(x), &a);
  mpz_init(&ctx, &z);
  mrb_assert(a.sn < 0);
  size_t size = a.sz;
  mpz_realloc(&ctx, &z, size);
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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_init(&ctx, &r);
  mpz_init(&ctx, &a); mpz_init(&ctx, &b);

  bint_as_mpz(RBIGINT(*xp), &x);
  bint_as_mpz(RBIGINT(*yp), &y);

  mpz_gcd(&ctx, &r, &x, &y);

  mpz_mdiv(&ctx, &a, &x, &r);
  mpz_mdiv(&ctx, &b, &y, &r);

  mpz_clear(&ctx, &r);

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
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_init(&ctx, &r);
  bint_as_mpz(RBIGINT(x), &a);
  bint_as_mpz(RBIGINT(y), &b);

  mpz_gcd(&ctx, &r, &a, &b);

  struct RBigint *result = bint_new(mrb, &r);
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

  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);
  mpz_init(&ctx, &gcd_val);
  mpz_init(&ctx, &abs_x);
  mpz_init(&ctx, &abs_y);
  mpz_init(&ctx, &product);
  mpz_init(&ctx, &result_mpz);

  bint_as_mpz(RBIGINT(x), &x_mpz);
  bint_as_mpz(RBIGINT(y), &y_mpz);

  mpz_abs(&ctx, &abs_x, &x_mpz);
  mpz_abs(&ctx, &abs_y, &y_mpz);

  mpz_gcd(&ctx, &gcd_val, &abs_x, &abs_y);
  mpz_mul(&ctx, &product, &abs_x, &abs_y);
  mpz_mdiv(&ctx, &result_mpz, &product, &gcd_val);

  mpz_clear(&ctx, &gcd_val);
  mpz_clear(&ctx, &abs_x);
  mpz_clear(&ctx, &abs_y);
  mpz_clear(&ctx, &product);

  struct RBigint *result = bint_new(mrb, &result_mpz);
  mpz_clear(&ctx, &result_mpz);
  return mrb_obj_value(result);
}

mrb_value
mrb_bint_abs(mrb_state *mrb, mrb_value x)
{
  mpz_t a, result_mpz;
  mpz_ctx_t ctx = MPZ_CTX_HEAP(mrb);

  mpz_init(&ctx, &result_mpz);
  bint_as_mpz(RBIGINT(x), &a);
  mpz_abs(&ctx, &result_mpz, &a);

  struct RBigint *result = bint_new(mrb, &result_mpz);
  mpz_clear(&ctx, &result_mpz);
  return mrb_obj_value(result);
}
