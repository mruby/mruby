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
#include <mruby/numeric.h>
#include <mruby/range.h>
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

static mrb_bool
bitop_lsb_first(mrb_state *mrb, mrb_value kw)
{
  if (mrb_undef_p(kw) || mrb_true_p(kw)) return TRUE;
  if (mrb_false_p(kw)) return FALSE;
  mrb_raise(mrb, E_ARGUMENT_ERROR, "lsb_first must be true or false");
}

/*
 * Scans the positional arguments described by fmt, one into a or two
 * into a and b when b is given, and the optional "lsb_first" keyword
 * argument.  Returns the number of positional arguments given.
 */
static mrb_int
bitop_scan_args(mrb_state *mrb, const char *fmt, mrb_value *a, mrb_value *b, mrb_bool *lsb_first)
{
  mrb_int argc;
  mrb_sym kw_names[1];
  mrb_value kw_values[1];
  mrb_kwargs kwargs;

  kw_names[0] = MRB_SYM(lsb_first);
  kwargs.num = 1;
  kwargs.required = 0;
  kwargs.table = kw_names;
  kwargs.values = kw_values;
  kwargs.rest = NULL;
  argc = b ? mrb_get_args(mrb, fmt, a, b, &kwargs) : mrb_get_args(mrb, fmt, a, &kwargs);
  *lsb_first = bitop_lsb_first(mrb, kw_values[0]);
  return argc;
}

/*
 * A bit offset, as an index or a Range endpoint, is never counted from
 * the end: a negative one is an IndexError, not a position.
 */
static uint64_t
bitop_offset(mrb_state *mrb, mrb_value index)
{
  mrb_int offset = bitop_offset_from_index(mrb, index);

  if (offset < 0) {
    mrb_raise(mrb, E_INDEX_ERROR, "bit index out of range");
  }
  return (uint64_t)offset;
}

/*
 * Resolves (offset, length) or a Range into the bit region [start,
 * end).  Nothing is clamped here: end may lie beyond bit_size, and the
 * caller decides whether that is an error or a shorter region.  An
 * inverted or empty Range gives end == start.  A Range with no end
 * runs to bit_size, the position after the last bit.
 *
 * The arithmetic is done in uint64_t: an mrb_int offset and length are
 * each below 2^63, so their sum cannot overflow, and RSTRING_LEN * 8
 * fits for any string, which it does not in a 32-bit mrb_int.
 */
static void
bitop_scan_region(mrb_state *mrb, mrb_int argc, mrb_value a, mrb_value b,
                  uint64_t bit_size, uint64_t *start, uint64_t *end)
{
  if (mrb_range_p(a)) {
    struct RRange *r = mrb_range_ptr(mrb, a);
    mrb_value beg = RANGE_BEG(r), last = RANGE_END(r);

    if (argc == 2) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "a Range and a length cannot both be given");
    }
    *start = mrb_nil_p(beg) ? 0 : bitop_offset(mrb, beg);
    if (mrb_nil_p(last)) {
      *end = bit_size;
    }
    else {
      *end = bitop_offset(mrb, last);
      if (!RANGE_EXCL(r)) (*end)++;
    }
    if (*end < *start) *end = *start;
  }
  else {
    mrb_int length = mrb_as_int(mrb, mrb_ensure_integer_type(mrb, b));

    if (length < 0) {
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "negative length %i", length);
    }
    *start = bitop_offset(mrb, a);
    *end = *start + (uint64_t)length;
  }
}

static mrb_int
bitop_physical_index(mrb_int logical, mrb_bool lsb_first)
{
  if (lsb_first) return logical;
  return (logical & ~(mrb_int)7) | (7 - (logical & 7));
}

/*
 * The physical mask of the logical bits [lo, hi) of one byte,
 * 0 <= lo < hi <= 8.  Under lsb_first: false logical bit i is
 * physical bit 7 - i, so the run is mirrored within the byte.
 */
static unsigned char
bitop_byte_mask(unsigned int lo, unsigned int hi, mrb_bool lsb_first)
{
  if (!lsb_first) {
    unsigned int t = 8 - hi;
    hi = 8 - lo;
    lo = t;
  }
  return (unsigned char)(((1u << hi) - 1) & ~((1u << lo) - 1));
}

/* Returns 0 or 1, or -1 when offset is beyond the end of str. */
static int
bitop_get_bit(mrb_state *mrb, mrb_value str)
{
  mrb_value index;
  mrb_int offset, physical;
  mrb_bool lsb_first;

  bitop_scan_args(mrb, "o:", &index, NULL, &lsb_first);
  offset = bitop_offset_from_index(mrb, index);
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

static void
bitop_mutate_byte(unsigned char *p, unsigned char mask, enum bitop_mutation mutation)
{
  switch (mutation) {
  case BITOP_MUT_SET:
    *p |= mask;
    break;
  case BITOP_MUT_CLEAR:
    *p &= (unsigned char)~mask;
    break;
  case BITOP_MUT_FLIP:
    *p ^= mask;
    break;
  }
}

/*
 * Applies mutation to the non-empty bit region [start, end), which
 * lies within the buffer.  The first and last bytes may be partial;
 * the bytes between them are whole and go through memset or the
 * word-wide not kernel.
 */
static void
bitop_mutate_region(unsigned char *ptr, uint64_t start, uint64_t end,
                    mrb_bool lsb_first, enum bitop_mutation mutation)
{
  mrb_int sb = (mrb_int)(start / 8), eb = (mrb_int)((end - 1) / 8);
  unsigned int lo = (unsigned int)(start % 8), hi = (unsigned int)((end - 1) % 8) + 1;

  if (sb == eb) {
    bitop_mutate_byte(ptr + sb, bitop_byte_mask(lo, hi, lsb_first), mutation);
    return;
  }
  bitop_mutate_byte(ptr + sb, bitop_byte_mask(lo, 8, lsb_first), mutation);
  if (eb - sb > 1) {
    unsigned char *mid = ptr + sb + 1;
    mrb_int n = eb - sb - 1;

    switch (mutation) {
    case BITOP_MUT_SET:
      memset(mid, 0xFF, (size_t)n);
      break;
    case BITOP_MUT_CLEAR:
      memset(mid, 0, (size_t)n);
      break;
    case BITOP_MUT_FLIP:
      bitop_not_kernel(mid, mid, n);
      break;
    }
  }
  bitop_mutate_byte(ptr + eb, bitop_byte_mask(0, hi, lsb_first), mutation);
}

/*
 * bit_set(offset), bit_set(offset, length) and bit_set(range), and the
 * same for bit_clear and bit_flip.  A lone offset is the one-bit region
 * [offset, offset + 1), so the whole-region bound applies to it too.
 * The region must fit, and an empty one must still start no later than
 * the position after the last bit, before any byte is touched; a frozen
 * receiver is then refused even for an empty region.
 */
static mrb_value
bitop_mutate(mrb_state *mrb, mrb_value str, enum bitop_mutation mutation)
{
  mrb_value a, b;
  mrb_bool lsb_first;
  uint64_t start, end, bit_size = (uint64_t)RSTRING_LEN(str) * 8;
  mrb_int argc = bitop_scan_args(mrb, "o|o:", &a, &b, &lsb_first);

  if (argc == 1 && !mrb_range_p(a)) {
    start = bitop_offset(mrb, a);
    end = start + 1;
  }
  else {
    bitop_scan_region(mrb, argc, a, b, bit_size, &start, &end);
  }
  if (start > bit_size || end > bit_size) {
    mrb_raise(mrb, E_INDEX_ERROR, "bit index out of range");
  }
  mrb_str_modify(mrb, mrb_str_ptr(str));
  if (end == start) return str;
  bitop_mutate_region((unsigned char*)RSTRING_PTR(str), start, end, lsb_first, mutation);
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

/* Counts the set bits of the non-empty region [start, end) within the buffer. */
static uint64_t
bitop_count_region(const unsigned char *ptr, uint64_t start, uint64_t end, mrb_bool lsb_first)
{
  mrb_int sb = (mrb_int)(start / 8), eb = (mrb_int)((end - 1) / 8);
  unsigned int lo = (unsigned int)(start % 8), hi = (unsigned int)((end - 1) % 8) + 1;
  uint64_t count;

  if (sb == eb) {
    return bitop_popcount((bitop_word)(ptr[sb] & bitop_byte_mask(lo, hi, lsb_first)));
  }
  count = bitop_popcount((bitop_word)(ptr[sb] & bitop_byte_mask(lo, 8, lsb_first)));
  if (eb - sb > 1) {
    count += bitop_count_bits(ptr + sb + 1, eb - sb - 1);
  }
  count += bitop_popcount((bitop_word)(ptr[eb] & bitop_byte_mask(0, hi, lsb_first)));
  return count;
}

/*
 * bit_count, bit_count(offset, length) and bit_count(range).  Unlike
 * the mutations this clamps: only the bits that exist are counted, and
 * a region entirely beyond the end counts 0.  There is no one-bit form,
 * so a lone offset is an error rather than a count to the end.
 */
static mrb_value
mrb_str_bit_count(mrb_state *mrb, mrb_value str)
{
  mrb_value a, b;
  mrb_bool lsb_first;
  uint64_t start, end, bit_size = (uint64_t)RSTRING_LEN(str) * 8;
  const unsigned char *ptr = (const unsigned char*)RSTRING_PTR(str);
  mrb_int argc = bitop_scan_args(mrb, "|oo:", &a, &b, &lsb_first);

  if (argc == 0) {
    return mrb_uint64_value(mrb, bitop_count_bits(ptr, RSTRING_LEN(str)));
  }
  if (argc == 1 && !mrb_range_p(a)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "bit_count takes a Range or an offset and a length");
  }
  bitop_scan_region(mrb, argc, a, b, bit_size, &start, &end);
  if (end > bit_size) end = bit_size;
  if (start >= end) return mrb_fixnum_value(0);
  return mrb_uint64_value(mrb, bitop_count_region(ptr, start, end, lsb_first));
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
  RSTR_ENCODING_SET(mrb_str_ptr(result), MRB_STR_ENCODING_BINARY);
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
  mrb_define_method_id(mrb, s, MRB_SYM(bit_set), mrb_str_bit_set, MRB_ARGS_ARG(1,1)|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, s, MRB_SYM(bit_clear), mrb_str_bit_clear, MRB_ARGS_ARG(1,1)|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, s, MRB_SYM(bit_flip), mrb_str_bit_flip, MRB_ARGS_ARG(1,1)|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, s, MRB_SYM(bit_count), mrb_str_bit_count, MRB_ARGS_OPT(2)|MRB_ARGS_KEY(1, 0));
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
