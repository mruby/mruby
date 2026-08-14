/*
** string_bitops.c - Basic bit operations for String
*/

/*
 * mruby.h must come first: in C++ builds it defines the
 * __STDC_*_MACROS feature macros before the first inclusion of
 * stdint.h, which some toolchains (e.g. MinGW) require for
 * UINTPTR_MAX and friends to be visible.
 */
#include <mruby.h>
#include <mruby/string.h>
#include <mruby/internal.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>

#if defined(UINTPTR_MAX) && UINTPTR_MAX > 0xFFFFFFFFul
typedef uint64_t bitop_word;
# define BITOP_WORD_SIZE 8
#else
typedef uint32_t bitop_word;
# define BITOP_WORD_SIZE 4
#endif
#define BITOP_WORD_BITS (BITOP_WORD_SIZE * 8)

#ifndef __has_builtin
# define __has_builtin(x) 0
#endif

#if defined(__GNUC__) || __has_builtin(__builtin_popcount)
static unsigned int
bitop_popcount(bitop_word word)
{
#if BITOP_WORD_SIZE == 8
  return (unsigned int)__builtin_popcountll((unsigned long long)word);
#elif UINT_MAX >= 0xFFFFFFFFul
  /* int holds a 32-bit word; avoids the 64-bit helper where long is 64-bit */
  return (unsigned int)__builtin_popcount((unsigned int)word);
#else
  return (unsigned int)__builtin_popcountl((unsigned long)word);
#endif
}
#else
static unsigned int
bitop_popcount(bitop_word word)
{
  /* Generic SWAR popcount; the constants adapt to the word width. */
  const bitop_word m1 = (bitop_word)~(bitop_word)0 / 3;    /* 0x55... */
  const bitop_word m2 = (bitop_word)~(bitop_word)0 / 5;    /* 0x33... */
  const bitop_word m4 = (bitop_word)~(bitop_word)0 / 17;   /* 0x0f... */
  const bitop_word h01 = (bitop_word)~(bitop_word)0 / 255; /* 0x01... */

  word -= (word >> 1) & m1;
  word = (word & m2) + ((word >> 2) & m2);
  word = (word + (word >> 4)) & m4;
  return (unsigned int)((word * h01) >> (BITOP_WORD_BITS - 8));
}
#endif

/*
 * memcpy-based word loops, safe for any alignment.  They are the
 * whole bulk path on non-GNU compilers, and the unaligned fallback
 * on GNU-compatible ones.  The trailing byte loop of each kernel
 * only handles the tail.
 */
#define BITOP_UNARY_MEMCPY_LOOP(dst, src, len, off, expr_word)               \
  {                                                                          \
    mrb_int aligned_end_ = (len) & ~(mrb_int)(BITOP_WORD_SIZE - 1);          \
    for (; (off) < aligned_end_; (off) += BITOP_WORD_SIZE) {                 \
      bitop_word w_;                                                         \
      memcpy(&w_, (src) + (off), BITOP_WORD_SIZE);                           \
      w_ = expr_word(w_);                                                    \
      memcpy((dst) + (off), &w_, BITOP_WORD_SIZE);                           \
    }                                                                        \
  }

#define BITOP_BINARY_MEMCPY_LOOP(dst, lhs, rhs, len, off, expr_word)         \
  {                                                                          \
    mrb_int aligned_end_ = (len) & ~(mrb_int)(BITOP_WORD_SIZE - 1);          \
    for (; (off) < aligned_end_; (off) += BITOP_WORD_SIZE) {                 \
      bitop_word l_, r_;                                                     \
      memcpy(&l_, (lhs) + (off), BITOP_WORD_SIZE);                           \
      memcpy(&r_, (rhs) + (off), BITOP_WORD_SIZE);                           \
      l_ = expr_word(l_, r_);                                                \
      memcpy((dst) + (off), &l_, BITOP_WORD_SIZE);                           \
    }                                                                        \
  }

/*
 * On GNU-compatible compilers, word-aligned buffers are processed
 * through a word pointer; the may_alias type keeps that free of
 * strict-aliasing issues.  Targets without unaligned load support
 * (e.g. Cortex-M0+) still get true word loads this way, which a bare
 * memcpy cannot guarantee.  Alignment is not a given, though:
 * malloc'ed buffers are word-aligned, but embedded strings start
 * right after the RString header, which on 64-bit builds leaves them
 * only 4-byte aligned.  Unaligned buffers fall back to the memcpy
 * word loops above.
 */
#if defined(__GNUC__)
typedef bitop_word __attribute__((__may_alias__)) bitop_word_alias;
# define BITOP_ALIGNED(ptrbits) (((ptrbits) & (BITOP_WORD_SIZE - 1)) == 0)

/*
** The bulk kernels process one machine word at a time.  The word type
** follows the pointer width, so 32-bit targets (the common case for
** mruby) use 32-bit words and avoid emulated 64-bit arithmetic.
*/

#define BITOP_DEFINE_UNARY_KERNEL(name, expr_word, expr_byte)                \
static void                                                                  \
name(unsigned char *dst, const unsigned char *src, mrb_int len)              \
{                                                                            \
  mrb_int off = 0;                                                           \
  if (BITOP_ALIGNED((uintptr_t)dst | (uintptr_t)src)) {                      \
    bitop_word_alias *dw = (bitop_word_alias*)dst;                           \
    const bitop_word_alias *sw = (const bitop_word_alias*)src;               \
    mrb_int words = len / BITOP_WORD_SIZE;                                   \
    mrb_int i = 0;                                                           \
    for (; i + 4 <= words; i += 4) {                                         \
      bitop_word s0 = sw[i], s1 = sw[i+1], s2 = sw[i+2], s3 = sw[i+3];       \
      dw[i]   = expr_word(s0);                                               \
      dw[i+1] = expr_word(s1);                                               \
      dw[i+2] = expr_word(s2);                                               \
      dw[i+3] = expr_word(s3);                                               \
    }                                                                        \
    for (; i < words; i++) {                                                 \
      dw[i] = expr_word(sw[i]);                                              \
    }                                                                        \
    off = words * BITOP_WORD_SIZE;                                           \
  }                                                                          \
  else                                                                       \
    BITOP_UNARY_MEMCPY_LOOP(dst, src, len, off, expr_word)                   \
  for (; off < len; off++) {                                                 \
    dst[off] = expr_byte(src[off]);                                          \
  }                                                                          \
}

#define BITOP_DEFINE_BINARY_KERNEL(name, expr_word, expr_byte)               \
static void                                                                  \
name(unsigned char *dst, const unsigned char *lhs,                           \
     const unsigned char *rhs, mrb_int len)                                  \
{                                                                            \
  mrb_int off = 0;                                                           \
  if (BITOP_ALIGNED((uintptr_t)dst | (uintptr_t)lhs | (uintptr_t)rhs)) {     \
    bitop_word_alias *dw = (bitop_word_alias*)dst;                           \
    const bitop_word_alias *lw = (const bitop_word_alias*)lhs;               \
    const bitop_word_alias *rw = (const bitop_word_alias*)rhs;               \
    mrb_int words = len / BITOP_WORD_SIZE;                                   \
    mrb_int i = 0;                                                           \
    for (; i + 4 <= words; i += 4) {                                         \
      bitop_word l0 = lw[i], l1 = lw[i+1], l2 = lw[i+2], l3 = lw[i+3];       \
      bitop_word r0 = rw[i], r1 = rw[i+1], r2 = rw[i+2], r3 = rw[i+3];       \
      dw[i]   = expr_word(l0, r0);                                           \
      dw[i+1] = expr_word(l1, r1);                                           \
      dw[i+2] = expr_word(l2, r2);                                           \
      dw[i+3] = expr_word(l3, r3);                                           \
    }                                                                        \
    for (; i < words; i++) {                                                 \
      dw[i] = expr_word(lw[i], rw[i]);                                       \
    }                                                                        \
    off = words * BITOP_WORD_SIZE;                                           \
  }                                                                          \
  else                                                                       \
    BITOP_BINARY_MEMCPY_LOOP(dst, lhs, rhs, len, off, expr_word)             \
  for (; off < len; off++) {                                                 \
    dst[off] = expr_byte(lhs[off], rhs[off]);                                \
  }                                                                          \
}

#else /* generic compilers: memcpy word loops only */

#define BITOP_DEFINE_UNARY_KERNEL(name, expr_word, expr_byte)                \
static void                                                                  \
name(unsigned char *dst, const unsigned char *src, mrb_int len)              \
{                                                                            \
  mrb_int off = 0;                                                           \
  BITOP_UNARY_MEMCPY_LOOP(dst, src, len, off, expr_word)                     \
  for (; off < len; off++) {                                                 \
    dst[off] = expr_byte(src[off]);                                          \
  }                                                                          \
}

#define BITOP_DEFINE_BINARY_KERNEL(name, expr_word, expr_byte)               \
static void                                                                  \
name(unsigned char *dst, const unsigned char *lhs,                           \
     const unsigned char *rhs, mrb_int len)                                  \
{                                                                            \
  mrb_int off = 0;                                                           \
  BITOP_BINARY_MEMCPY_LOOP(dst, lhs, rhs, len, off, expr_word)               \
  for (; off < len; off++) {                                                 \
    dst[off] = expr_byte(lhs[off], rhs[off]);                                \
  }                                                                          \
}

#endif

#define BITOP_NOT_WORD(x)    (~(x))
#define BITOP_NOT_BYTE(x)    ((unsigned char)~(x))
#define BITOP_AND_WORD(x, y) ((x) & (y))
#define BITOP_AND_BYTE(x, y) ((unsigned char)((x) & (y)))
#define BITOP_OR_WORD(x, y)  ((x) | (y))
#define BITOP_OR_BYTE(x, y)  ((unsigned char)((x) | (y)))
#define BITOP_XOR_WORD(x, y) ((x) ^ (y))
#define BITOP_XOR_BYTE(x, y) ((unsigned char)((x) ^ (y)))

BITOP_DEFINE_UNARY_KERNEL(bitop_not_kernel, BITOP_NOT_WORD, BITOP_NOT_BYTE)
BITOP_DEFINE_BINARY_KERNEL(bitop_and_kernel, BITOP_AND_WORD, BITOP_AND_BYTE)
BITOP_DEFINE_BINARY_KERNEL(bitop_or_kernel, BITOP_OR_WORD, BITOP_OR_BYTE)
BITOP_DEFINE_BINARY_KERNEL(bitop_xor_kernel, BITOP_XOR_WORD, BITOP_XOR_BYTE)

/*
 * The maximum count is len * 8, which can exceed MRB_INT_MAX on
 * 32-bit mrb_int builds for strings over 256MiB, so accumulate in
 * uint64_t; whether the total fits in mrb_int is decided when boxing
 * the return value.
 */
static uint64_t
bitop_count_bits(const unsigned char *ptr, mrb_int len)
{
  uint64_t count = 0;
  mrb_int off = 0;

#if defined(__GNUC__)
  if (BITOP_ALIGNED((uintptr_t)ptr)) {
    const bitop_word_alias *pw = (const bitop_word_alias*)ptr;
    mrb_int words = len / BITOP_WORD_SIZE;
    mrb_int i = 0;
    for (; i + 4 <= words; i += 4) {
      count += bitop_popcount(pw[i]);
      count += bitop_popcount(pw[i+1]);
      count += bitop_popcount(pw[i+2]);
      count += bitop_popcount(pw[i+3]);
    }
    for (; i < words; i++) {
      count += bitop_popcount(pw[i]);
    }
    off = words * BITOP_WORD_SIZE;
  }
  else
#endif
  {
    mrb_int aligned_end = len & ~(mrb_int)(BITOP_WORD_SIZE - 1);
    for (; off < aligned_end; off += BITOP_WORD_SIZE) {
      bitop_word w;
      memcpy(&w, ptr + off, BITOP_WORD_SIZE);
      count += bitop_popcount(w);
    }
  }
  /* Pack the remaining bytes into one word and popcount it once. */
  if (off < len) {
    bitop_word w = 0;
    unsigned int shift = 0;
    for (; off < len; off++, shift += 8) {
      w |= (bitop_word)ptr[off] << shift;
    }
    count += bitop_popcount(w);
  }
  return count;
}

/*
 * Converts an offset argument to mrb_int.  Unlike CRuby's rb_to_int
 * this does not dispatch to_int: mruby has no implicit conversion
 * protocol in core, so Array.new(obj), ary[obj] and "s" * obj all
 * reject an object that merely defines to_int, and this must not be
 * the one place in the tree that accepts one.
 * mrb_ensure_integer_type() covers exactly the numeric types an
 * offset may be written as, and raises TypeError for the rest.
 */
static mrb_int
bitop_offset_from_index(mrb_state *mrb, mrb_value index)
{
  return mrb_as_int(mrb, mrb_ensure_integer_type(mrb, index));
}

/*
 * Scans "offset" and the optional "lsb_first" keyword argument.
 * Returns the lsb_first flag (default true).
 */
static mrb_bool
bitop_scan_offset(mrb_state *mrb, mrb_int *offset)
{
  mrb_value index;
  mrb_bool lsb_first;
  mrb_sym kw_names[1];
  mrb_value kw_values[1];
  mrb_kwargs kwargs;

  kw_names[0] = MRB_SYM(lsb_first);
  kwargs.num = 1;
  kwargs.required = 0;
  kwargs.table = kw_names;
  kwargs.values = kw_values;
  kwargs.rest = NULL;
  mrb_get_args(mrb, "o:", &index, &kwargs);
  if (mrb_undef_p(kw_values[0]) || mrb_true_p(kw_values[0])) {
    lsb_first = TRUE;
  }
  else if (mrb_false_p(kw_values[0])) {
    lsb_first = FALSE;
  }
  else {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "lsb_first must be true or false");
  }
  *offset = bitop_offset_from_index(mrb, index);
  return lsb_first;
}

static mrb_int
bitop_physical_index(mrb_int logical, mrb_bool lsb_first)
{
  if (lsb_first) return logical;
  return (logical & ~(mrb_int)7) | (7 - (logical & 7));
}

/* Returns 0 or 1, or -1 when offset is beyond the end of str. */
static int
bitop_get_bit(mrb_state *mrb, mrb_value str)
{
  mrb_int offset, physical;
  mrb_bool lsb_first = bitop_scan_offset(mrb, &offset);

  if (offset < 0) {
    mrb_raise(mrb, E_INDEX_ERROR, "bit index out of range");
  }
  /* Compare byte indexes to avoid overflowing len * 8. */
  if (offset / 8 >= RSTRING_LEN(str)) {
    return -1;
  }
  physical = bitop_physical_index(offset, lsb_first);
  return (((unsigned char)RSTRING_PTR(str)[physical / 8]) >> (physical % 8)) & 1;
}

static mrb_value
mrb_str_bit_get(mrb_state *mrb, mrb_value str)
{
  int bit = bitop_get_bit(mrb, str);
  return bit < 0 ? mrb_nil_value() : mrb_fixnum_value(bit);
}

static mrb_value
mrb_str_bit_set_p(mrb_state *mrb, mrb_value str)
{
  int bit = bitop_get_bit(mrb, str);
  return bit < 0 ? mrb_nil_value() : mrb_bool_value(bit != 0);
}

enum bitop_mutation {
  BITOP_MUT_SET,
  BITOP_MUT_CLEAR,
  BITOP_MUT_FLIP
};

static mrb_value
bitop_mutate(mrb_state *mrb, mrb_value str, enum bitop_mutation mutation)
{
  mrb_int offset, physical;
  mrb_bool lsb_first = bitop_scan_offset(mrb, &offset);
  unsigned char *ptr;
  unsigned char mask;

  if (offset < 0 || offset / 8 >= RSTRING_LEN(str)) {
    mrb_raise(mrb, E_INDEX_ERROR, "bit index out of range");
  }
  mrb_str_modify(mrb, mrb_str_ptr(str));
  physical = bitop_physical_index(offset, lsb_first);
  ptr = (unsigned char*)RSTRING_PTR(str);
  mask = (unsigned char)(1u << (physical % 8));
  switch (mutation) {
  case BITOP_MUT_SET:
    ptr[physical / 8] |= mask;
    break;
  case BITOP_MUT_CLEAR:
    ptr[physical / 8] &= (unsigned char)~mask;
    break;
  case BITOP_MUT_FLIP:
    ptr[physical / 8] ^= mask;
    break;
  }
  return str;
}

static mrb_value
mrb_str_bit_set(mrb_state *mrb, mrb_value str)
{
  return bitop_mutate(mrb, str, BITOP_MUT_SET);
}

static mrb_value
mrb_str_bit_clear(mrb_state *mrb, mrb_value str)
{
  return bitop_mutate(mrb, str, BITOP_MUT_CLEAR);
}

static mrb_value
mrb_str_bit_flip(mrb_state *mrb, mrb_value str)
{
  return bitop_mutate(mrb, str, BITOP_MUT_FLIP);
}

static mrb_value
mrb_str_bit_count(mrb_state *mrb, mrb_value str)
{
  uint64_t count;

  mrb_get_args(mrb, "");
  count = bitop_count_bits((const unsigned char*)RSTRING_PTR(str), RSTRING_LEN(str));
  if (count <= (uint64_t)MRB_INT_MAX) {
    return mrb_int_value(mrb, (mrb_int)count);
  }
#ifdef MRB_USE_BIGINT
  return mrb_bint_new_uint64(mrb, count);
#else
  mrb_raise(mrb, E_RANGE_ERROR, "bit count too big for Integer");
#endif
}

/*
 * Matches CRuby: the result of a non-bang bitwise operation is a
 * BINARY (ASCII-8BIT) string.  The flag is observable through
 * String#encoding when mruby-encoding is present, and inert
 * otherwise.
 */
static mrb_value
bitop_result_str(mrb_state *mrb, mrb_int len)
{
  mrb_value result = mrb_str_new(mrb, NULL, len);
  RSTR_SET_BINARY_FLAG(mrb_str_ptr(result));
  return result;
}

static mrb_value
mrb_str_bitwise_not(mrb_state *mrb, mrb_value str)
{
  mrb_int len;
  mrb_value result;

  mrb_get_args(mrb, "");
  len = RSTRING_LEN(str);
  result = bitop_result_str(mrb, len);
  bitop_not_kernel((unsigned char*)RSTRING_PTR(result),
                   (const unsigned char*)RSTRING_PTR(str), len);
  return result;
}

static mrb_value
mrb_str_bitwise_not_bang(mrb_state *mrb, mrb_value str)
{
  unsigned char *ptr;

  mrb_get_args(mrb, "");
  mrb_str_modify(mrb, mrb_str_ptr(str));
  ptr = (unsigned char*)RSTRING_PTR(str);
  bitop_not_kernel(ptr, ptr, RSTRING_LEN(str));
  return str;
}

/*
 * Checks the operand of a binary bitwise operation.  Unlike CRuby's
 * StringValue() this does not dispatch to_str, for the same reason
 * the offset conversion does not dispatch to_int: mruby has no
 * implicit String conversion in core, so honouring to_str here would
 * make this gem more permissive than the tree it sits in.
 */
static mrb_value
bitop_str_operand(mrb_state *mrb, mrb_value other)
{
  if (!mrb_string_p(other)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "%Y cannot be converted to String", other);
  }
  return other;
}

static void
bitop_check_length(mrb_state *mrb, mrb_value str, mrb_value other)
{
  if (RSTRING_LEN(str) != RSTRING_LEN(other)) {
    /* mrb_ssize can be narrower than mrb_int; %i reads an mrb_int */
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "operands must have the same length (%i vs %i)",
               (mrb_int)RSTRING_LEN(str), (mrb_int)RSTRING_LEN(other));
  }
}

typedef void (*bitop_binary_kernel)(unsigned char*, const unsigned char*,
                                    const unsigned char*, mrb_int);

static mrb_value
bitop_binary(mrb_state *mrb, mrb_value str, bitop_binary_kernel kernel)
{
  mrb_value other, result;
  mrb_int len;

  mrb_get_args(mrb, "o", &other);
  other = bitop_str_operand(mrb, other);
  bitop_check_length(mrb, str, other);
  len = RSTRING_LEN(str);
  result = bitop_result_str(mrb, len);
  kernel((unsigned char*)RSTRING_PTR(result),
         (const unsigned char*)RSTRING_PTR(str),
         (const unsigned char*)RSTRING_PTR(other), len);
  return result;
}

static mrb_value
bitop_binary_bang(mrb_state *mrb, mrb_value str, bitop_binary_kernel kernel)
{
  mrb_value other;
  unsigned char *ptr;

  mrb_get_args(mrb, "o", &other);
  other = bitop_str_operand(mrb, other);
  bitop_check_length(mrb, str, other);
  mrb_str_modify(mrb, mrb_str_ptr(str));
  ptr = (unsigned char*)RSTRING_PTR(str);
  kernel(ptr, ptr, (const unsigned char*)RSTRING_PTR(other), RSTRING_LEN(str));
  return str;
}

static mrb_value
mrb_str_bitwise_and(mrb_state *mrb, mrb_value str)
{
  return bitop_binary(mrb, str, bitop_and_kernel);
}

static mrb_value
mrb_str_bitwise_and_bang(mrb_state *mrb, mrb_value str)
{
  return bitop_binary_bang(mrb, str, bitop_and_kernel);
}

static mrb_value
mrb_str_bitwise_or(mrb_state *mrb, mrb_value str)
{
  return bitop_binary(mrb, str, bitop_or_kernel);
}

static mrb_value
mrb_str_bitwise_or_bang(mrb_state *mrb, mrb_value str)
{
  return bitop_binary_bang(mrb, str, bitop_or_kernel);
}

static mrb_value
mrb_str_bitwise_xor(mrb_state *mrb, mrb_value str)
{
  return bitop_binary(mrb, str, bitop_xor_kernel);
}

static mrb_value
mrb_str_bitwise_xor_bang(mrb_state *mrb, mrb_value str)
{
  return bitop_binary_bang(mrb, str, bitop_xor_kernel);
}

void
mrb_mruby_string_bitops_gem_init(mrb_state *mrb)
{
  struct RClass *s = mrb->string_class;

  mrb_define_method_id(mrb, s, MRB_SYM(bit_get), mrb_str_bit_get, MRB_ARGS_REQ(1)|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, s, MRB_SYM_Q(bit_set), mrb_str_bit_set_p, MRB_ARGS_REQ(1)|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, s, MRB_SYM(bit_set), mrb_str_bit_set, MRB_ARGS_REQ(1)|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, s, MRB_SYM(bit_clear), mrb_str_bit_clear, MRB_ARGS_REQ(1)|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, s, MRB_SYM(bit_flip), mrb_str_bit_flip, MRB_ARGS_REQ(1)|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, s, MRB_SYM(bit_count), mrb_str_bit_count, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, s, MRB_SYM(bitwise_not), mrb_str_bitwise_not, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, s, MRB_SYM_B(bitwise_not), mrb_str_bitwise_not_bang, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, s, MRB_SYM(bitwise_and), mrb_str_bitwise_and, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, s, MRB_SYM_B(bitwise_and), mrb_str_bitwise_and_bang, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, s, MRB_SYM(bitwise_or), mrb_str_bitwise_or, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, s, MRB_SYM_B(bitwise_or), mrb_str_bitwise_or_bang, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, s, MRB_SYM(bitwise_xor), mrb_str_bitwise_xor, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, s, MRB_SYM_B(bitwise_xor), mrb_str_bitwise_xor_bang, MRB_ARGS_REQ(1));
}

void
mrb_mruby_string_bitops_gem_final(mrb_state *mrb)
{
}
