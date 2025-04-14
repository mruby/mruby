/*
** string.c - String class
**
** See Copyright Notice in mruby.h
*/

#ifdef _MSC_VER
# define _CRT_NONSTDC_NO_DEPRECATE
# define WIN32_LEAN_AND_MEAN
#endif

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/numeric.h>
#include <mruby/internal.h>
#include <mruby/presym.h>
#include <string.h>

typedef struct mrb_shared_string {
  int refcnt;
  mrb_int capa;
  char *ptr;
} mrb_shared_string;

const char mrb_digitmap[] = "0123456789abcdefghijklmnopqrstuvwxyz";

#define mrb_obj_alloc_string(mrb) MRB_OBJ_ALLOC((mrb), MRB_TT_STRING, (mrb)->string_class)

#ifndef MRB_STR_LENGTH_MAX
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__)
#define MRB_STR_LENGTH_MAX 0
#else
#define MRB_STR_LENGTH_MAX 1048576
#endif
#endif

static void
str_check_length(mrb_state *mrb, mrb_int len)
{
  if (len < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative (or overflowed) string size");
  }
#if MRB_STR_LENGTH_MAX != 0
  if (len > MRB_STR_LENGTH_MAX-1) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "string too long (len=%i max=" MRB_STRINGIZE(MRB_STR_LENGTH_MAX) ")", len);
  }
#endif
}

static struct RString*
str_init_normal_capa(mrb_state *mrb, struct RString *s,
                     const char *p, mrb_int len, mrb_int capa)
{
  str_check_length(mrb, capa);
  char *dst = (char*)mrb_malloc(mrb, capa + 1);
  if (p) memcpy(dst, p, len);
  dst[len] = '\0';
  s->as.heap.ptr = dst;
  s->as.heap.len = len;
  s->as.heap.aux.capa = capa;
  RSTR_SET_TYPE(s, NORMAL);
  return s;
}

static struct RString*
str_init_normal(mrb_state *mrb, struct RString *s, const char *p, mrb_int len)
{
  return str_init_normal_capa(mrb, s, p, len, len);
}

static struct RString*
str_init_embed(struct RString *s, const char *p, mrb_int len)
{
  mrb_assert(len >= 0);
  if (p) memcpy(RSTR_EMBED_PTR(s), p, len);
  RSTR_EMBED_PTR(s)[len] = '\0';
  RSTR_SET_TYPE(s, EMBED);
  RSTR_SET_EMBED_LEN(s, len);
  return s;
}

static struct RString*
str_init_nofree(struct RString *s, const char *p, mrb_int len)
{
  s->as.heap.ptr = (char*)p;
  s->as.heap.len = len;
  s->as.heap.aux.capa = 0;             /* nofree */
  RSTR_SET_TYPE(s, NOFREE);
  return s;
}

static struct RString*
str_init_shared(mrb_state *mrb, const struct RString *orig, struct RString *s, mrb_shared_string *shared)
{
  if (shared) {
    shared->refcnt++;
  }
  else {
    shared = (mrb_shared_string*)mrb_malloc(mrb, sizeof(mrb_shared_string));
    shared->refcnt = 1;
    shared->ptr = orig->as.heap.ptr;
    shared->capa = orig->as.heap.aux.capa;
  }
  s->as.heap.ptr = orig->as.heap.ptr;
  s->as.heap.len = orig->as.heap.len;
  s->as.heap.aux.shared = shared;
  RSTR_SET_TYPE(s, SHARED);
  return s;
}

static struct RString*
str_init_fshared(const struct RString *orig, struct RString *s, struct RString *fshared)
{
  s->as.heap.ptr = orig->as.heap.ptr;
  s->as.heap.len = orig->as.heap.len;
  s->as.heap.aux.fshared = fshared;
  RSTR_SET_TYPE(s, FSHARED);
  return s;
}

static struct RString*
str_init_modifiable(mrb_state *mrb, struct RString *s, const char *p, mrb_int len)
{
  if (RSTR_EMBEDDABLE_P(len)) {
    return str_init_embed(s, p, len);
  }
  return str_init_normal(mrb, s, p, len);
}

static struct RString*
str_new_static(mrb_state *mrb, const char *p, mrb_int len)
{
  if (RSTR_EMBEDDABLE_P(len)) {
    return str_init_embed(mrb_obj_alloc_string(mrb), p, len);
  }
  return str_init_nofree(mrb_obj_alloc_string(mrb), p, len);
}

static struct RString*
str_new(mrb_state *mrb, const char *p, mrb_int len)
{
  str_check_length(mrb, len);
  if (RSTR_EMBEDDABLE_P(len)) {
    return str_init_embed(mrb_obj_alloc_string(mrb), p, len);
  }
  if (p && mrb_ro_data_p(p)) {
    return str_init_nofree(mrb_obj_alloc_string(mrb), p, len);
  }
  return str_init_normal(mrb, mrb_obj_alloc_string(mrb), p, len);
}

MRB_API mrb_value
mrb_str_new_capa(mrb_state *mrb, mrb_int capa)
{
  struct RString *s = mrb_obj_alloc_string(mrb);

  if (RSTR_EMBEDDABLE_P(capa)) {
    s = str_init_embed(s, NULL, 0);
  }
  else {
    s = str_init_normal_capa(mrb, s, NULL, 0, capa);
  }
  return mrb_obj_value(s);
}

static void
resize_capa(mrb_state *mrb, struct RString *s, mrb_int capacity)
{
  if (RSTR_EMBED_P(s)) {
    if (!RSTR_EMBEDDABLE_P(capacity)) {
      str_init_normal_capa(mrb, s, RSTR_EMBED_PTR(s), RSTR_EMBED_LEN(s), capacity);
    }
  }
  else {
    str_check_length(mrb, capacity);
    s->as.heap.ptr = (char*)mrb_realloc(mrb, RSTR_PTR(s), capacity+1);
    s->as.heap.aux.capa = (mrb_ssize)capacity;
  }
}

MRB_API mrb_value
mrb_str_new(mrb_state *mrb, const char *p, mrb_int len)
{
  return mrb_obj_value(str_new(mrb, p, len));
}

MRB_API mrb_value
mrb_str_new_cstr(mrb_state *mrb, const char *p)
{
  struct RString *s;
  mrb_int len;

  if (p) {
    len = strlen(p);
  }
  else {
    len = 0;
  }

  s = str_new(mrb, p, len);

  return mrb_obj_value(s);
}

MRB_API mrb_value
mrb_str_new_static(mrb_state *mrb, const char *p, mrb_int len)
{
  struct RString *s = str_new_static(mrb, p, len);
  return mrb_obj_value(s);
}

static void
str_decref(mrb_state *mrb, mrb_shared_string *shared)
{
  shared->refcnt--;
  if (shared->refcnt == 0) {
    mrb_free(mrb, shared->ptr);
    mrb_free(mrb, shared);
  }
}

static void
str_unshare_buffer(mrb_state *mrb, struct RString *s)
{
  if (RSTR_SHARED_P(s)) {
    mrb_shared_string *shared = s->as.heap.aux.shared;

    if (shared->refcnt == 1 && s->as.heap.ptr == shared->ptr) {
      s->as.heap.aux.capa = shared->capa;
      s->as.heap.ptr[s->as.heap.len] = '\0';
      RSTR_SET_TYPE(s, NORMAL);
      mrb_free(mrb, shared);
    }
    else {
      str_init_modifiable(mrb, s, s->as.heap.ptr, s->as.heap.len);
      str_decref(mrb, shared);
    }
  }
  else if (RSTR_NOFREE_P(s) || RSTR_FSHARED_P(s)) {
    str_init_modifiable(mrb, s, s->as.heap.ptr, s->as.heap.len);
  }
}

static void
check_null_byte(mrb_state *mrb, struct RString *str)
{
  const char *p = RSTR_PTR(str);
  if (p && memchr(p, '\0', RSTR_LEN(str))) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "string contains null byte");
  }
}

void
mrb_gc_free_str(mrb_state *mrb, struct RString *str)
{
  if (RSTR_EMBED_P(str))
    /* no code */;
  else if (RSTR_SHARED_P(str))
    str_decref(mrb, str->as.heap.aux.shared);
  else if (!RSTR_NOFREE_P(str) && !RSTR_FSHARED_P(str))
    mrb_free(mrb, str->as.heap.ptr);
}

#if defined(__i386) || defined(__i386__) || defined(_M_IX86) || \
     defined(__x86_64) || defined(__x86_64__) || defined(_M_AMD64) || \
     defined(__powerpc64__) || defined(__POWERPC__) || defined(__aarch64__) || \
     defined(__mc68020__)
# define ALIGNED_WORD_ACCESS 0
#else
# define ALIGNED_WORD_ACCESS 1
#endif

#ifdef MRB_64BIT
#define bitint uint64_t
#define MASK01 0x0101010101010101ull
#else
#define bitint uint32_t
#define MASK01 0x01010101ul
#endif

#ifdef MRB_UTF8_STRING

#define NOASCII(c) ((c) & 0x80)

#ifdef SIMPLE_SEARCH_NONASCII
/* the naive implementation. define SIMPLE_SEARCH_NONASCII, */
/* if you need it for any constraint (e.g. code size).      */
static const char*
search_nonascii(const char* p, const char *e)
{
  for (; p < e; ++p) {
    if (NOASCII(*p)) return p;
  }
  return e;
}

#elif defined(__SSE2__)
# include <emmintrin.h>

static inline const char *
search_nonascii(const char *p, const char *e)
{
  if (sizeof(__m128i) < (size_t)(e - p)) {
    if (!_mm_movemask_epi8(_mm_loadu_si128((__m128i const*)p))) {
      const intptr_t lowbits = sizeof(__m128i) - 1;
      const __m128i *s, *t;
      s = (const __m128i*)(~lowbits & ((intptr_t)p + lowbits));
      t = (const __m128i*)(~lowbits & (intptr_t)e);
      for (; s < t; ++s) {
        if (_mm_movemask_epi8(_mm_load_si128(s))) break;
      }
      p = (const char *)s;
    }
  }
  switch (e - p) {
  default:
  case 15: if (NOASCII(*p)) return p; ++p;
  case 14: if (NOASCII(*p)) return p; ++p;
  case 13: if (NOASCII(*p)) return p; ++p;
  case 12: if (NOASCII(*p)) return p; ++p;
  case 11: if (NOASCII(*p)) return p; ++p;
  case 10: if (NOASCII(*p)) return p; ++p;
  case 9:  if (NOASCII(*p)) return p; ++p;
  case 8:  if (NOASCII(*p)) return p; ++p;
  case 7:  if (NOASCII(*p)) return p; ++p;
  case 6:  if (NOASCII(*p)) return p; ++p;
  case 5:  if (NOASCII(*p)) return p; ++p;
  case 4:  if (NOASCII(*p)) return p; ++p;
  case 3:  if (NOASCII(*p)) return p; ++p;
  case 2:  if (NOASCII(*p)) return p; ++p;
  case 1:  if (NOASCII(*p)) return p; ++p;
           if (NOASCII(*p)) return p;
  case 0:  break;
  }
  return e;
}

#else

static const char*
search_nonascii(const char *p, const char *e)
{
  ptrdiff_t byte_len = e - p;

  const char *be = p + sizeof(bitint) * (byte_len / sizeof(bitint));
  for (; p < be; p+=sizeof(bitint)) {
    bitint t0;

    memcpy(&t0, p, sizeof(bitint));
    const bitint t1 = t0 & (MASK01*0x80);
    if (t1) {
      e = p + sizeof(bitint)-1;
      byte_len = sizeof(bitint)-1;
      break;
    }
  }

  switch (byte_len % sizeof(bitint)) {
#ifdef MRB_64BIT
  case 7: if (e[-7]&0x80) return e-7;
  case 6: if (e[-6]&0x80) return e-6;
  case 5: if (e[-5]&0x80) return e-5;
  case 4: if (e[-4]&0x80) return e-4;
#endif
  case 3: if (e[-3]&0x80) return e-3;
  case 2: if (e[-2]&0x80) return e-2;
  case 1: if (e[-1]&0x80) return e-1;
  }
  return e;
}

#endif  /* SIMPLE_SEARCH_NONASCII */

#define utf8_islead(c) ((unsigned char)((c)&0xc0) != 0x80)

extern const char mrb_utf8len_table[];
const char mrb_utf8len_table[] = {
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
};

mrb_int
mrb_utf8len(const char* p, const char* e)
{
  mrb_int len = mrb_utf8len_table[(unsigned char)p[0] >> 3];
  if (len > e - p) return 1;
  switch (len) {
  case 0:
    return 1;
  case 4:
    if (utf8_islead(p[3])) return 1;
  case 3:
    if (utf8_islead(p[2])) return 1;
  case 2:
    if (utf8_islead(p[1])) return 1;
  }
  return len;
}

#if defined(__GNUC__) || __has_builtin(__builtin_popcount)
# ifdef MRB_64BIT
# define popcount(x) __builtin_popcountll(x)
# else
# define popcount(x) __builtin_popcountl(x)
# endif
#else
static inline uint32_t popcount(bitint x)
{
  x = (x & (MASK01*0x55)) + ((x >>  1) & (MASK01*0x55));
  x = (x & (MASK01*0x33)) + ((x >>  2) & (MASK01*0x33));
  x = (x & (MASK01*0x0F)) + ((x >>  4) & (MASK01*0x0F));
  return (x * MASK01) >> 56;
}
#endif

mrb_int
mrb_utf8_strlen(const char *str, mrb_int byte_len)
{
  const char *p = str;
  const char *e = str + byte_len;
  mrb_int len = 0;

  while (p < e) {
    const char *np = search_nonascii(p, e);

    len += np - p;
    if (np == e) break;
    p = np;
    while (NOASCII(*p)) {
      p += mrb_utf8len(p, e);
      len++;
    }
  }
  return len;
}

static mrb_int
utf8_strlen(mrb_value str)
{
  struct RString *s = mrb_str_ptr(str);
  mrb_int byte_len = RSTR_LEN(s);

  if (RSTR_SINGLE_BYTE_P(s)) {
    return byte_len;
  }
  else {
    mrb_int utf8_len = mrb_utf8_strlen(RSTR_PTR(s), byte_len);
    mrb_assert(utf8_len <= byte_len);
    if (byte_len == utf8_len) RSTR_SET_SINGLE_BYTE_FLAG(s);
    return utf8_len;
  }
}

#define RSTRING_CHAR_LEN(s) utf8_strlen(s)

/* map character index to byte offset index */
static mrb_int
chars2bytes(mrb_value str, mrb_int off, mrb_int idx)
{
  struct RString *s = mrb_str_ptr(str);
  if (RSTR_SINGLE_BYTE_P(s) || RSTR_BINARY_P(s)) {
    return idx;
  }

  const char *o = RSTR_PTR(s);
  const char *p0 = o + off;
  const char *p = p0;
  const char *e = o + RSTR_LEN(s);
  mrb_int i = 0;

  while (p<e && i<idx) {
    if ((*p & 0x80) == 0) {
      const char *np = search_nonascii(p, e);
      ptrdiff_t alen = np - p;
      if (idx < i+alen) {
        p += idx-i;
        i=idx;
      }
      else {
        p = np;
        i += alen;
      }
    }
    else {
      p += mrb_utf8len(p, e);
      i++;
    }
  }

  mrb_int len = (mrb_int)(p-p0);
  if (i<idx) len++;
  return len;
}

/* map byte offset to character index */
static mrb_int
bytes2chars(mrb_value str, mrb_int bi)
{
  struct RString *s = mrb_str_ptr(str);
  if (RSTR_SINGLE_BYTE_P(s) || RSTR_BINARY_P(s)) {
    return bi;
  }

  const char *p = RSTR_PTR(s);
  const char *e = p + RSTR_LEN(s);
  const char *pivot = p + bi;
  mrb_int i = 0;

  if (e < pivot) return -1;
  while (p < pivot) {
    if ((*p & 0x80) == 0) {
      const char *np = search_nonascii(p, pivot);
      i += np - p;
      p = np;
    }
    else {
      p += mrb_utf8len(p, e);
      i++;
    }
  }
  if (p != pivot) return -1;
  return i;
}

static const char*
char_adjust(const char *ptr, const char *end)
{
  ptrdiff_t len = end - ptr;
  if (len < 1 || utf8_islead(ptr[0])) return ptr;
  if (len > 1 && utf8_islead(ptr[1])) return ptr+1;
  if (len > 2 && utf8_islead(ptr[2])) return ptr+2;
  if (len > 3 && utf8_islead(ptr[3])) return ptr+3;
  return ptr;
}

static const char*
char_backtrack(const char *ptr, const char *end)
{
  ptrdiff_t len = end - ptr;
  if (len < 1 || utf8_islead(end[-1])) return end-1;
  if (len > 1 && utf8_islead(end[-2])) return end-2;
  if (len > 2 && utf8_islead(end[-3])) return end-3;
  if (len > 3 && utf8_islead(end[-4])) return end-4;
  return end - 1;
}

static mrb_int
str_index_str_by_char(mrb_state *mrb, mrb_value str, mrb_value sub, mrb_int pos)
{
  const char *ptr = RSTRING_PTR(sub);
  mrb_int len = RSTRING_LEN(sub);

  if (pos > 0) {
    pos = chars2bytes(str, 0, pos);
  }

  pos = mrb_str_index(mrb, str, ptr, len, pos);

  if (pos > 0) {
    pos = bytes2chars(str, pos);
  }
  return pos;
}

#else
#define RSTRING_CHAR_LEN(s) RSTRING_LEN(s)
#define chars2bytes(s, off, ci) (ci)
#define bytes2chars(s, bi) (bi)
#define char_adjust(ptr, end) (ptr)
#define char_backtrack(ptr, end) ((end) - 1)
#define str_index_str_by_char(mrb, str, sub, pos) str_index_str((mrb), (str), (sub), (pos))
#endif

/* memsearch_swar (SWAR stands for SIMD within a register)                 */
/* See https://en.wikipedia.org/wiki/SWAR                                  */
/* The function is taken from http://0x80.pl/articles/simd-strfind.html    */
/* The original source code is under 2-clause BSD license; see LEGAL file. */
/* The modifications:
   * port from C++ to C
   * returns mrb_int
   * remove alignment issue
   * support bigendian CPU
   * fixed potential buffer overflow
*/
static inline mrb_int
memsearch_swar(const char *xs, mrb_int m, const char *ys, mrb_int n)
{
#define MASK7f (MASK01*0x7f)
#define MASK80 (MASK01*0x80)
#if defined(MRB_ENDIAN_BIG)
#ifdef MRB_64BIT
#define MASKtop 0x8000000000000000ull
#else
#define MASKtop 0x80000000ul
#endif
#else
#define MASKtop 0x80
#endif

  const bitint first = MASK01 * (uint8_t)xs[0];
  const bitint last  = MASK01 * (uint8_t)xs[m-1];

  const char *s0 = ys;
  const char *s1 = ys+m-1;

  const mrb_int lim = n - m - (mrb_int)sizeof(bitint);
  mrb_int i;

  for (i=0; i < lim; i+=sizeof(bitint)) {
    bitint t0, t1;

    memcpy(&t0, s0+i, sizeof(bitint));
    memcpy(&t1, s1+i, sizeof(bitint));

    const bitint eq = (t0 ^ first) | (t1 ^ last);
    bitint zeros = ((~eq & MASK7f) + MASK01) & (~eq & MASK80);

    for (size_t j = 0; zeros; j++) {
      if (zeros & MASKtop) {
        const mrb_int idx = i + j;
        const char* p = s0 + idx + 1;
        if (memcmp(p, xs + 1, m - 2) == 0) {
          return idx;
        }
      }

#if defined(MRB_ENDIAN_BIG)
      zeros <<= 8;
#else
      zeros >>= 8;
#endif
    }
  }

  if (i+m < n) {
    const char *p = s0;
    const char *e = ys + n;
    while (p<e) {
      p = (const char*)memchr(p, *xs, e - p);
      if (p == NULL || (e - p) < m) break;
      if (memcmp(p+1, xs+1, m-1) == 0) return (mrb_int)(p - ys);
      p++;
    }
  }

  return -1;
}

static mrb_int
mrb_memsearch(const char *x, mrb_int m, const char *y, mrb_int n)
{
  if (m > n) return -1;
  else if (m == n) {
    return memcmp(x, y, m) == 0 ? 0 : -1;
  }
  else if (m < 1) {
    return 0;
  }
  else if (m == 1) {
    const char *p = (const char*)memchr(y, *x, n);

    if (p) return (mrb_int)(p - y);
    return -1;
  }
  return memsearch_swar(x, m, y, n);
}

static void
str_share(mrb_state *mrb, struct RString *orig, struct RString *s)
{
  size_t len = (size_t)orig->as.heap.len;

  mrb_assert(!RSTR_EMBED_P(orig));
  if (RSTR_NOFREE_P(orig)) {
    str_init_nofree(s, orig->as.heap.ptr, len);
  }
  else if (RSTR_SHARED_P(orig)) {
    str_init_shared(mrb, orig, s, orig->as.heap.aux.shared);
  }
  else if (RSTR_FSHARED_P(orig)) {
    str_init_fshared(orig, s, orig->as.heap.aux.fshared);
  }
  else {
    if (orig->as.heap.aux.capa > orig->as.heap.len) {
      orig->as.heap.ptr = (char*)mrb_realloc(mrb, orig->as.heap.ptr, len+1);
      orig->as.heap.aux.capa = (mrb_ssize)len;
    }
    str_init_shared(mrb, orig, s, NULL);
    str_init_shared(mrb, orig, orig, s->as.heap.aux.shared);
  }
}

mrb_value
mrb_str_byte_subseq(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len)
{
  struct RString *orig = mrb_str_ptr(str);
  struct RString *s = mrb_obj_alloc_string(mrb);

  if (RSTR_EMBEDDABLE_P(len)) {
    str_init_embed(s, RSTR_PTR(orig)+beg, len);
  }
  else {
    str_share(mrb, orig, s);
    s->as.heap.ptr += (mrb_ssize)beg;
    s->as.heap.len = (mrb_ssize)len;
  }
  RSTR_COPY_SINGLE_BYTE_FLAG(s, orig);
  return mrb_obj_value(s);
}

#ifdef MRB_UTF8_STRING
static inline mrb_value
str_subseq(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len)
{
  beg = chars2bytes(str, 0, beg);
  len = chars2bytes(str, beg, len);
  return mrb_str_byte_subseq(mrb, str, beg, len);
}
#else
#define str_subseq(mrb, str, beg, len) mrb_str_byte_subseq(mrb, str, beg, len)
#endif

mrb_bool
mrb_str_beg_len(mrb_int str_len, mrb_int *begp, mrb_int *lenp)
{
  if (str_len < *begp || *lenp < 0) return FALSE;
  if (*begp < 0) {
    *begp += str_len;
    if (*begp < 0) return FALSE;
  }
  if (*lenp > str_len - *begp)
    *lenp = str_len - *begp;
  if (*lenp <= 0) {
    *lenp = 0;
  }
  return TRUE;
}

static mrb_value
str_substr(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len)
{
  return mrb_str_beg_len(RSTRING_CHAR_LEN(str), &beg, &len) ?
    str_subseq(mrb, str, beg, len) : mrb_nil_value();
}

MRB_API mrb_int
mrb_str_index(mrb_state *mrb, mrb_value str, const char *sptr, mrb_int slen, mrb_int offset)
{
  mrb_int len = RSTRING_LEN(str);

  if (offset < 0) {
    offset += len;
    if (offset < 0) return -1;
  }
  if (len - offset < slen) return -1;

  char *s = RSTRING_PTR(str);
  if (offset) {
    s += offset;
  }
  if (slen == 0) return offset;
  /* need proceed one character at a time */
  len = RSTRING_LEN(str) - offset;

  mrb_int pos = mrb_memsearch(sptr, slen, s, len);
  if (pos < 0) return pos;
  return pos + offset;
}

static mrb_int
str_index_str(mrb_state *mrb, mrb_value str, mrb_value str2, mrb_int offset)
{
  const char *ptr = RSTRING_PTR(str2);
  mrb_int len = RSTRING_LEN(str2);

  return mrb_str_index(mrb, str, ptr, len, offset);
}

static mrb_value
str_replace(mrb_state *mrb, struct RString *s1, struct RString *s2)
{
  mrb_check_frozen(mrb, s1);
  if (s1 == s2) return mrb_obj_value(s1);
  RSTR_COPY_SINGLE_BYTE_FLAG(s1, s2);
  if (RSTR_SHARED_P(s1)) {
    str_decref(mrb, s1->as.heap.aux.shared);
  }
  else if (!RSTR_EMBED_P(s1) && !RSTR_NOFREE_P(s1) && !RSTR_FSHARED_P(s1)) {
    mrb_free(mrb, s1->as.heap.ptr);
  }

  size_t len = (size_t)RSTR_LEN(s2);
  if (RSTR_EMBEDDABLE_P(len)) {
    str_init_embed(s1, RSTR_PTR(s2), len);
  }
  else {
    str_share(mrb, s2, s1);
  }

  return mrb_obj_value(s1);
}

static mrb_int
str_rindex(mrb_state *mrb, mrb_value str, mrb_value sub, mrb_int pos)
{
  const char *s, *sbeg, *send, *t;
  struct RString *ps = mrb_str_ptr(str);
  mrb_int len = RSTRING_LEN(sub);
  mrb_int slen = RSTR_LEN(ps);

  /* substring longer than string */
  if (slen < len) return -1;
  if (slen - pos < len) {
    pos = slen - len;
  }
  sbeg = RSTR_PTR(ps);
  send = sbeg + slen;
  s = sbeg + pos;
  t = RSTRING_PTR(sub);
  if (len) {
    s = char_adjust(s, send);
    while (sbeg <= s) {
      if ((mrb_int)(send - s) >= len && memcmp(s, t, len) == 0) {
        return (mrb_int)(s - sbeg);
      }
      s = char_backtrack(sbeg, s);
    }
    return -1;
  }
  else {
    return pos;
  }
}

#ifdef _WIN32
#include <stdlib.h>
#include <malloc.h>
#include <windows.h>

char*
mrb_utf8_from_locale(const char *str, int len)
{
  wchar_t* wcsp;
  char* mbsp;
  int mbssize, wcssize;

  if (len == 0)
    return strdup("");
  if (len == -1)
    len = (int)strlen(str);
  wcssize = MultiByteToWideChar(GetACP(), 0, str, len,  NULL, 0);
  wcsp = (wchar_t*) malloc((wcssize + 1) * sizeof(wchar_t));
  if (!wcsp)
    return NULL;
  wcssize = MultiByteToWideChar(GetACP(), 0, str, len, wcsp, wcssize + 1);
  wcsp[wcssize] = 0;

  mbssize = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR) wcsp, -1, NULL, 0, NULL, NULL);
  mbsp = (char*) malloc((mbssize + 1));
  if (!mbsp) {
    free(wcsp);
    return NULL;
  }
  mbssize = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR) wcsp, -1, mbsp, mbssize, NULL, NULL);
  mbsp[mbssize] = 0;
  free(wcsp);
  return mbsp;
}

char*
mrb_locale_from_utf8(const char *utf8, int len)
{
  wchar_t* wcsp;
  char* mbsp;
  int mbssize, wcssize;

  if (len == 0)
    return strdup("");
  if (len == -1)
    len = (int)strlen(utf8);
  wcssize = MultiByteToWideChar(CP_UTF8, 0, utf8, len,  NULL, 0);
  wcsp = (wchar_t*) malloc((wcssize + 1) * sizeof(wchar_t));
  if (!wcsp)
    return NULL;
  wcssize = MultiByteToWideChar(CP_UTF8, 0, utf8, len, wcsp, wcssize + 1);
  wcsp[wcssize] = 0;
  mbssize = WideCharToMultiByte(GetACP(), 0, (LPCWSTR) wcsp, -1, NULL, 0, NULL, NULL);
  mbsp = (char*) malloc((mbssize + 1));
  if (!mbsp) {
    free(wcsp);
    return NULL;
  }
  mbssize = WideCharToMultiByte(GetACP(), 0, (LPCWSTR) wcsp, -1, mbsp, mbssize, NULL, NULL);
  mbsp[mbssize] = 0;
  free(wcsp);
  return mbsp;
}
#endif

MRB_API void
mrb_str_modify_keep_ascii(mrb_state *mrb, struct RString *s)
{
  mrb_check_frozen(mrb, s);
  str_unshare_buffer(mrb, s);
}

MRB_API void
mrb_str_modify(mrb_state *mrb, struct RString *s)
{
  mrb_str_modify_keep_ascii(mrb, s);
  RSTR_UNSET_SINGLE_BYTE_FLAG(s);
}

MRB_API mrb_value
mrb_str_resize(mrb_state *mrb, mrb_value str, mrb_int len)
{
  mrb_int slen;
  struct RString *s = mrb_str_ptr(str);

  str_check_length(mrb, len);
  mrb_str_modify(mrb, s);
  slen = RSTR_LEN(s);
  if (len != slen) {
    if (slen < len || slen - len > 256) {
      resize_capa(mrb, s, len);
    }
    RSTR_SET_LEN(s, len);
    RSTR_PTR(s)[len] = '\0';   /* sentinel */
  }
  return str;
}

MRB_API char*
mrb_str_to_cstr(mrb_state *mrb, mrb_value str0)
{
  struct RString *s;

  const char *p = RSTRING_PTR(str0);
  mrb_int len = RSTRING_LEN(str0);
  check_null_byte(mrb, RSTRING(str0));
  s = str_init_modifiable(mrb, mrb_obj_alloc_string(mrb), p, len);
  return RSTR_PTR(s);
}

MRB_API void
mrb_str_concat(mrb_state *mrb, mrb_value self, mrb_value other)
{
  other = mrb_obj_as_string(mrb, other);
  mrb_str_cat_str(mrb, self, other);
}

MRB_API mrb_value
mrb_str_plus(mrb_state *mrb, mrb_value a, mrb_value b)
{
  struct RString *s = mrb_str_ptr(a);
  struct RString *s2 = mrb_str_ptr(b);
  struct RString *t;
  mrb_int slen = RSTR_LEN(s);
  mrb_int s2len = RSTR_LEN(s2);
  const char *p = RSTR_PTR(s);
  const char *p2 = RSTR_PTR(s2);

  t = str_new(mrb, 0, slen + s2len);
  char *pt = RSTR_PTR(t);
  memcpy(pt, p, slen);
  memcpy(pt + slen, p2, s2len);

  return mrb_obj_value(t);
}

/* 15.2.10.5.2  */

/*
 *  call-seq:
 *     str + other_str   -> new_str
 *
 *  Concatenation---Returns a new <code>String</code> containing
 *  <i>other_str</i> concatenated to <i>str</i>.
 *
 *     "Hello from " + self.to_s   #=> "Hello from main"
 */
static mrb_value
mrb_str_plus_m(mrb_state *mrb, mrb_value self)
{
  mrb_value str;

  mrb_get_args(mrb, "S", &str);
  return mrb_str_plus(mrb, self, str);
}

/* 15.2.10.5.26 */
/* 15.2.10.5.33 */
/*
 *  call-seq:
 *     "abcd".size   => int
 *
 *  Returns the length of string.
 */
static mrb_value
mrb_str_size(mrb_state *mrb, mrb_value self)
{
  mrb_int len = RSTRING_CHAR_LEN(self);
  return mrb_int_value(mrb, len);
}

static mrb_value
mrb_str_bytesize(mrb_state *mrb, mrb_value self)
{
  return mrb_int_value(mrb, RSTRING_LEN(self));
}

/* 15.2.10.5.1  */
/*
 *  call-seq:
 *     str * integer   => new_str
 *
 *  Copy---Returns a new <code>String</code> containing <i>integer</i> copies of
 *  the receiver.
 *
 *     "Ho! " * 3   #=> "Ho! Ho! Ho! "
 */
static mrb_value
mrb_str_times(mrb_state *mrb, mrb_value self)
{
  mrb_int len, times;

  mrb_get_args(mrb, "i", &times);
  if (times < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative argument");
  }
  if (mrb_int_mul_overflow(RSTRING_LEN(self), times, &len)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "argument too big");
  }

  struct RString *str2 = str_new(mrb, 0, len);
  char *p = RSTR_PTR(str2);
  if (len > 0) {
    mrb_int n = RSTRING_LEN(self);
    memcpy(p, RSTRING_PTR(self), n);
    while (n <= len/2) {
      memcpy(p + n, p, n);
      n *= 2;
    }
    memcpy(p + n, p, len-n);
  }
  p[RSTR_LEN(str2)] = '\0';
  RSTR_COPY_SINGLE_BYTE_FLAG(str2, mrb_str_ptr(self));

  return mrb_obj_value(str2);
}
/* -------------------------------------------------------------- */

#define lesser(a,b) (((a)>(b))?(b):(a))

/* ---------------------------*/
/*
 *  call-seq:
 *     mrb_value str1 <=> mrb_value str2   => int
 *                     >  1
 *                     =  0
 *                     <  -1
 */
MRB_API int
mrb_str_cmp(mrb_state *mrb, mrb_value str1, mrb_value str2)
{
  struct RString *s1 = mrb_str_ptr(str1);
  struct RString *s2 = mrb_str_ptr(str2);

  mrb_int len1 = RSTR_LEN(s1);
  mrb_int len2 = RSTR_LEN(s2);
  mrb_int len = lesser(len1, len2);
  mrb_int retval = memcmp(RSTR_PTR(s1), RSTR_PTR(s2), len);
  if (retval == 0) {
    if (len1 == len2) return 0;
    if (len1 > len2)  return 1;
    return -1;
  }
  if (retval > 0) return 1;
  return -1;
}

/* 15.2.10.5.3  */

/*
 *  call-seq:
 *     str <=> other_str   => -1, 0, +1
 *
 *  Comparison---Returns -1 if <i>other_str</i> is less than, 0 if
 *  <i>other_str</i> is equal to, and +1 if <i>other_str</i> is greater than
 *  <i>str</i>. If the strings are of different lengths, and the strings are
 *  equal when compared up to the shortest length, then the longer string is
 *  considered greater than the shorter one. If the variable <code>$=</code> is
 *  <code>false</code>, the comparison is based on comparing the binary values
 *  of each character in the string. In older versions of Ruby, setting
 *  <code>$=</code> allowed case-insensitive comparisons; this is now deprecated
 *  in favor of using <code>String#casecmp</code>.
 *
 *  <code><=></code> is the basis for the methods <code><</code>,
 *  <code><=</code>, <code>></code>, <code>>=</code>, and <code>between?</code>,
 *  included from module <code>Comparable</code>.  The method
 *  <code>String#==</code> does not use <code>Comparable#==</code>.
 *
 *     "abcdef" <=> "abcde"     #=> 1
 *     "abcdef" <=> "abcdef"    #=> 0
 *     "abcdef" <=> "abcdefg"   #=> -1
 *     "abcdef" <=> "ABCDEF"    #=> 1
 */
static mrb_value
mrb_str_cmp_m(mrb_state *mrb, mrb_value str1)
{
  mrb_value str2 = mrb_get_arg1(mrb);

  if (!mrb_string_p(str2)) {
    return mrb_nil_value();
  }

  mrb_int result = mrb_str_cmp(mrb, str1, str2);
  return mrb_int_value(mrb, result);
}

static mrb_bool
str_eql(mrb_state *mrb, const mrb_value str1, const mrb_value str2)
{
  const mrb_int len = RSTRING_LEN(str1);

  if (len != RSTRING_LEN(str2)) return FALSE;
  return (memcmp(RSTRING_PTR(str1), RSTRING_PTR(str2), (size_t)len) == 0);
}

MRB_API mrb_bool
mrb_str_equal(mrb_state *mrb, mrb_value str1, mrb_value str2)
{
  if (!mrb_string_p(str2)) return FALSE;
  return str_eql(mrb, str1, str2);
}

/* 15.2.10.5.4  */
/*
 *  call-seq:
 *     str == obj   => true or false
 *
 *  Equality---
 *  If <i>obj</i> is not a <code>String</code>, returns <code>false</code>.
 *  Otherwise, returns <code>false</code> or <code>true</code>
 *
 *   caution:if <i>str</i> <code><=></code> <i>obj</i> returns zero.
 */
static mrb_value
mrb_str_equal_m(mrb_state *mrb, mrb_value str1)
{
  mrb_value str2 = mrb_get_arg1(mrb);

  return mrb_bool_value(mrb_str_equal(mrb, str1, str2));
}
/* ---------------------------------- */

MRB_API mrb_value
mrb_str_dup(mrb_state *mrb, mrb_value str)
{
  struct RString *s = mrb_str_ptr(str);
  struct RString *dup = str_new(mrb, 0, 0);

  return str_replace(mrb, dup, s);
}

enum str_convert_range {
  /* `beg` and `len` are byte unit in `0 ... str.bytesize` */
  STR_BYTE_RANGE_CORRECTED = 1,

  /* `beg` and `len` are char unit in any range */
  STR_CHAR_RANGE = 2,

  /* `beg` and `len` are char unit in `0 ... str.size` */
  STR_CHAR_RANGE_CORRECTED = 3,

  /* `beg` is out of range */
  STR_OUT_OF_RANGE = -1
};

static enum str_convert_range
str_convert_range(mrb_state *mrb, mrb_value str, mrb_value idx, mrb_value alen, mrb_int *beg, mrb_int *len)
{
  if (!mrb_undef_p(alen)) {
    *beg = mrb_as_int(mrb, idx);
    *len = mrb_as_int(mrb, alen);
    return STR_CHAR_RANGE;
  }
  else {
    switch (mrb_type(idx)) {
      default:
        idx = mrb_ensure_int_type(mrb, idx);
        /* fall through */
      case MRB_TT_INTEGER:
        *beg = mrb_integer(idx);
        *len = 1;
        return STR_CHAR_RANGE;

      case MRB_TT_STRING:
        *beg = str_index_str(mrb, str, idx, 0);
        if (*beg < 0) { break; }
        *len = RSTRING_LEN(idx);
        return STR_BYTE_RANGE_CORRECTED;

      case MRB_TT_RANGE:
        *len = RSTRING_CHAR_LEN(str);
        switch (mrb_range_beg_len(mrb, idx, beg, len, *len, TRUE)) {
          case MRB_RANGE_OK:
            return STR_CHAR_RANGE_CORRECTED;
          case MRB_RANGE_OUT:
            return STR_OUT_OF_RANGE;
          default:
            break;
        }
    }
  }
  return STR_OUT_OF_RANGE;
}

mrb_value
mrb_str_aref(mrb_state *mrb, mrb_value str, mrb_value idx, mrb_value alen)
{
  mrb_int beg, len;

  switch (str_convert_range(mrb, str, idx, alen, &beg, &len)) {
    case STR_CHAR_RANGE_CORRECTED:
      return str_subseq(mrb, str, beg, len);
    case STR_CHAR_RANGE:
      str = str_substr(mrb, str, beg, len);
      if (mrb_undef_p(alen) && !mrb_nil_p(str) && RSTRING_LEN(str) == 0) return mrb_nil_value();
      return str;
    case STR_BYTE_RANGE_CORRECTED:
      if (mrb_string_p(idx)) {
        return mrb_str_dup(mrb, idx);
      }
      else {
        return mrb_str_byte_subseq(mrb, str, beg, len);
      }
    case STR_OUT_OF_RANGE:
    default:
      return mrb_nil_value();
  }
}

/* 15.2.10.5.6  */
/* 15.2.10.5.34 */
/*
 *  call-seq:
 *     str[int]                 => int or nil
 *     str[int, int]            => new_str or nil
 *     str[range]               => new_str or nil
 *     str[other_str]           => new_str or nil
 *     str.slice(int)           => int or nil
 *     str.slice(int, int)      => new_str or nil
 *     str.slice(range)         => new_str or nil
 *     str.slice(other_str)     => new_str or nil
 *
 *  Element Reference---If passed a single <code>Integer</code>, returns the code
 *  of the character at that position. If passed two <code>Integer</code>
 *  objects, returns a substring starting at the offset given by the first, and
 *  a length given by the second. If given a range, a substring containing
 *  characters at offsets given by the range is returned. In all three cases, if
 *  an offset is negative, it is counted from the end of <i>str</i>. Returns
 *  <code>nil</code> if the initial offset falls outside the string, the length
 *  is negative, or the beginning of the range is greater than the end.
 *
 *  If a <code>String</code> is given, that string is returned if it occurs in
 *  <i>str</i>. In both cases, <code>nil</code> is returned if there is no
 *  match.
 *
 *     a = "hello there"
 *     a[1]                   #=> 101(1.8.7) "e"(1.9.2)
 *     a[1.1]                 #=>            "e"(1.9.2)
 *     a[1,3]                 #=> "ell"
 *     a[1..3]                #=> "ell"
 *     a[-3,2]                #=> "er"
 *     a[-4..-2]              #=> "her"
 *     a[12..-1]              #=> nil
 *     a[-2..-4]              #=> ""
 *     a["lo"]                #=> "lo"
 *     a["bye"]               #=> nil
 */
static mrb_value
mrb_str_aref_m(mrb_state *mrb, mrb_value str)
{
  mrb_value a1, a2;

  if (mrb_get_args(mrb, "o|o", &a1, &a2) == 1) {
    a2 = mrb_undef_value();
  }

  return mrb_str_aref(mrb, str, a1, a2);
}

static mrb_noreturn void
str_out_of_index(mrb_state *mrb, mrb_value index)
{
  mrb_raisef(mrb, E_INDEX_ERROR, "index %v out of string", index);
}

static mrb_value
str_replace_partial(mrb_state *mrb, mrb_value src, mrb_int pos, mrb_int end, mrb_value rep)
{
  const mrb_int shrink_threshold = 256;
  struct RString *str = mrb_str_ptr(src);
  mrb_int len = RSTR_LEN(str);
  mrb_int replen, newlen;
  char *strp;

  if (end > len) { end = len; }

  if (pos < 0 || pos > len) {
    str_out_of_index(mrb, mrb_int_value(mrb, pos));
  }

  replen = (mrb_nil_p(rep) ? 0 : RSTRING_LEN(rep));
  if (mrb_int_add_overflow(replen, len - (end - pos), &newlen)) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "string size too big");
  }

  mrb_str_modify(mrb, str);

  if (len < newlen) {
    resize_capa(mrb, str, newlen);
  }

  strp = RSTR_PTR(str);

  memmove(strp + newlen - (len - end), strp + end, len - end);
  if (!mrb_nil_p(rep)) {
    memmove(strp + pos, RSTRING_PTR(rep), replen);
  }
  RSTR_SET_LEN(str, newlen);
  strp[newlen] = '\0';

  if (len - newlen >= shrink_threshold) {
    resize_capa(mrb, str, newlen);
  }

  return src;
}

#define IS_EVSTR(p,e) ((p) < (e) && (*(p) == '$' || *(p) == '@' || *(p) == '{'))

static mrb_value
str_escape(mrb_state *mrb, mrb_value str, mrb_bool inspect)
{
  const char *p, *pend;
  char buf[4];  /* `\x??` or UTF-8 character */
  mrb_value result = mrb_str_new_lit(mrb, "\"");
#ifdef MRB_UTF8_STRING
  uint32_t sb_flag = MRB_STR_SINGLE_BYTE;
#endif

  p = RSTRING_PTR(str); pend = RSTRING_END(str);
  for (;p < pend; p++) {
    unsigned char c, cc;
#ifdef MRB_UTF8_STRING
    if (inspect) {
      mrb_int clen = mrb_utf8len(p, pend);
      if (clen > 1) {
        mrb_str_cat(mrb, result, p, clen);
        p += clen-1;
        sb_flag = 0;
        continue;
      }
    }
#endif
    c = *p;
    if (c == '"'|| c == '\\' || (c == '#' && IS_EVSTR(p+1, pend))) {
      buf[0] = '\\'; buf[1] = c;
      mrb_str_cat(mrb, result, buf, 2);
      continue;
    }
    if (ISPRINT(c)) {
      buf[0] = c;
      mrb_str_cat(mrb, result, buf, 1);
      continue;
    }
    switch (c) {
      case '\n': cc = 'n'; break;
      case '\r': cc = 'r'; break;
      case '\t': cc = 't'; break;
      case '\f': cc = 'f'; break;
      case '\013': cc = 'v'; break;
      case '\010': cc = 'b'; break;
      case '\007': cc = 'a'; break;
      case 033: cc = 'e'; break;
      default: cc = 0; break;
    }
    buf[0] = '\\';
    if (cc) {
      buf[1] = (char)cc;
      mrb_str_cat(mrb, result, buf, 2);
    }
    else {
      buf[1] = 'x';
      buf[3] = mrb_digitmap[c % 16]; c /= 16;
      buf[2] = mrb_digitmap[c % 16];
      mrb_str_cat(mrb, result, buf, 4);
    }
  }
  mrb_str_cat_lit(mrb, result, "\"");
#ifdef MRB_UTF8_STRING
  if (inspect) {
    mrb_str_ptr(str)->flags |= sb_flag;
    mrb_str_ptr(result)->flags |= sb_flag;
  }
  else {
    RSTR_SET_SINGLE_BYTE_FLAG(mrb_str_ptr(result));
  }
#endif

  return result;
}

static void
mrb_str_aset(mrb_state *mrb, mrb_value str, mrb_value idx, mrb_value alen, mrb_value replace)
{
  mrb_int beg, len, charlen;

  mrb_ensure_string_type(mrb, replace);
  switch (str_convert_range(mrb, str, idx, alen, &beg, &len)) {
    case STR_OUT_OF_RANGE:
    default:
      mrb_raise(mrb, E_INDEX_ERROR, "string not matched");
    case STR_CHAR_RANGE:
      if (len < 0) {
        mrb_raisef(mrb, E_INDEX_ERROR, "negative length %v", alen);
      }
      charlen = RSTRING_CHAR_LEN(str);
      if (beg < 0) { beg += charlen; }
      if (beg < 0 || beg > charlen) { str_out_of_index(mrb, idx); }
      /* fall through */
    case STR_CHAR_RANGE_CORRECTED:
      beg = chars2bytes(str, 0, beg);
      len = chars2bytes(str, beg, len);
      /* fall through */
    case STR_BYTE_RANGE_CORRECTED:
      if (mrb_int_add_overflow(beg, len, &len)) {
        mrb_raise(mrb, E_RUNTIME_ERROR, "string index too big");
      }
      str_replace_partial(mrb, str, beg, len, replace);
  }
}

/*
 * call-seq:
 *    str[int] = replace
 *    str[int, int] = replace
 *    str[range] = replace
 *    str[other_str] = replace
 *
 * Modify +self+ by replacing the content of +self+.
 * The portion of the string affected is determined using the same criteria as +String#[]+.
 * The return value of this expression is +replace+.
 */
static mrb_value
mrb_str_aset_m(mrb_state *mrb, mrb_value str)
{
  mrb_value idx, alen, replace;

  switch (mrb_get_args(mrb, "oo|S!", &idx, &alen, &replace)) {
    case 2:
      replace = alen;
      alen = mrb_undef_value();
      break;
    case 3:
      break;
  }
  mrb_str_aset(mrb, str, idx, alen, replace);
  return replace;
}

/* 15.2.10.5.8  */
/*
 *  call-seq:
 *     str.capitalize!   => str or nil
 *
 *  Modifies <i>str</i> by converting the first character to uppercase and the
 *  remainder to lowercase. Returns <code>nil</code> if no changes are made.
 *
 *     a = "hello"
 *     a.capitalize!   #=> "Hello"
 *     a               #=> "Hello"
 *     a.capitalize!   #=> nil
 */
static mrb_value
mrb_str_capitalize_bang(mrb_state *mrb, mrb_value str)
{
  mrb_bool modify = FALSE;
  struct RString *s = mrb_str_ptr(str);
  mrb_int len = RSTR_LEN(s);

  mrb_str_modify_keep_ascii(mrb, s);
  char *p = RSTR_PTR(s);
  char *pend = RSTR_PTR(s) + len;
  if (len == 0 || p == NULL) return mrb_nil_value();
  if (ISLOWER(*p)) {
    *p = TOUPPER(*p);
    modify = TRUE;
  }
  while (++p < pend) {
    if (ISUPPER(*p)) {
      *p = TOLOWER(*p);
      modify = TRUE;
    }
  }
  if (modify) return str;
  return mrb_nil_value();
}

/* 15.2.10.5.7  */
/*
 *  call-seq:
 *     str.capitalize   => new_str
 *
 *  Returns a copy of <i>str</i> with the first character converted to uppercase
 *  and the remainder to lowercase.
 *
 *     "hello".capitalize    #=> "Hello"
 *     "HELLO".capitalize    #=> "Hello"
 *     "123ABC".capitalize   #=> "123abc"
 */
static mrb_value
mrb_str_capitalize(mrb_state *mrb, mrb_value self)
{
  mrb_value str;

  str = mrb_str_dup(mrb, self);
  mrb_str_capitalize_bang(mrb, str);
  return str;
}

/* 15.2.10.5.10  */
/*
 *  call-seq:
 *     str.chomp!(separator="\n")   => str or nil
 *
 *  Modifies <i>str</i> in place as described for <code>String#chomp</code>,
 *  returning <i>str</i>, or <code>nil</code> if no modifications were made.
 */
static mrb_value
mrb_str_chomp_bang(mrb_state *mrb, mrb_value str)
{
  mrb_value rs;
  mrb_int newline;
  char *p, *pp;
  mrb_int rslen;
  mrb_int len;
  mrb_int argc;
  struct RString *s = mrb_str_ptr(str);

  argc = mrb_get_args(mrb, "|S", &rs);
  mrb_str_modify_keep_ascii(mrb, s);
  len = RSTR_LEN(s);
  if (argc == 0) {
    if (len == 0) return mrb_nil_value();
  smart_chomp:
    if (RSTR_PTR(s)[len-1] == '\n') {
      RSTR_SET_LEN(s, RSTR_LEN(s) - 1);
      if (RSTR_LEN(s) > 0 &&
          RSTR_PTR(s)[RSTR_LEN(s)-1] == '\r') {
        RSTR_SET_LEN(s, RSTR_LEN(s) - 1);
      }
    }
    else if (RSTR_PTR(s)[len-1] == '\r') {
      RSTR_SET_LEN(s, RSTR_LEN(s) - 1);
    }
    else {
      return mrb_nil_value();
    }
    RSTR_PTR(s)[RSTR_LEN(s)] = '\0';
    return str;
  }

  if (len == 0 || mrb_nil_p(rs)) return mrb_nil_value();
  p = RSTR_PTR(s);
  rslen = RSTRING_LEN(rs);
  if (rslen == 0) {
    while (len>0 && p[len-1] == '\n') {
      len--;
      if (len>0 && p[len-1] == '\r')
        len--;
    }
    if (len < RSTR_LEN(s)) {
      RSTR_SET_LEN(s, len);
      p[len] = '\0';
      return str;
    }
    return mrb_nil_value();
  }
  if (rslen > len) return mrb_nil_value();
  newline = RSTRING_PTR(rs)[rslen-1];
  if (rslen == 1 && newline == '\n')
    newline = RSTRING_PTR(rs)[rslen-1];
  if (rslen == 1 && newline == '\n')
    goto smart_chomp;

  pp = p + len - rslen;
  if (p[len-1] == newline &&
     (rslen <= 1 ||
     memcmp(RSTRING_PTR(rs), pp, rslen) == 0)) {
    RSTR_SET_LEN(s, len - rslen);
    p[RSTR_LEN(s)] = '\0';
    return str;
  }
  return mrb_nil_value();
}

/* 15.2.10.5.9  */
/*
 *  call-seq:
 *     str.chomp(separator="\n")   => new_str
 *
 *  Returns a new <code>String</code> with the given record separator removed
 *  from the end of <i>str</i> (if present). <code>chomp</code> also removes
 *  carriage return characters (that is it will remove <code>\n</code>,
 *  <code>\r</code>, and <code>\r\n</code>).
 *
 *     "hello".chomp            #=> "hello"
 *     "hello\n".chomp          #=> "hello"
 *     "hello\r\n".chomp        #=> "hello"
 *     "hello\n\r".chomp        #=> "hello\n"
 *     "hello\r".chomp          #=> "hello"
 *     "hello \n there".chomp   #=> "hello \n there"
 *     "hello".chomp("llo")     #=> "he"
 */
static mrb_value
mrb_str_chomp(mrb_state *mrb, mrb_value self)
{
  mrb_value str;

  str = mrb_str_dup(mrb, self);
  mrb_str_chomp_bang(mrb, str);
  return str;
}

/* 15.2.10.5.12 */
/*
 *  call-seq:
 *     str.chop!   => str or nil
 *
 *  Processes <i>str</i> as for <code>String#chop</code>, returning <i>str</i>,
 *  or <code>nil</code> if <i>str</i> is the empty string.  See also
 *  <code>String#chomp!</code>.
 */
static mrb_value
mrb_str_chop_bang(mrb_state *mrb, mrb_value str)
{
  struct RString *s = mrb_str_ptr(str);

  mrb_str_modify_keep_ascii(mrb, s);
  if (RSTR_LEN(s) > 0) {
    mrb_int len;
#ifdef MRB_UTF8_STRING
    const char* t = RSTR_PTR(s), *p = t;
    const char* e = p + RSTR_LEN(s);
    while (p<e) {
      mrb_int clen = mrb_utf8len(p, e);
      if (p + clen>=e) break;
      p += clen;
    }
    len = p - t;
#else
    len = RSTR_LEN(s) - 1;
#endif
    if (RSTR_PTR(s)[len] == '\n') {
      if (len > 0 &&
          RSTR_PTR(s)[len-1] == '\r') {
        len--;
      }
    }
    RSTR_SET_LEN(s, len);
    RSTR_PTR(s)[len] = '\0';
    return str;
  }
  return mrb_nil_value();
}

/* 15.2.10.5.11 */
/*
 *  call-seq:
 *     str.chop   => new_str
 *
 *  Returns a new <code>String</code> with the last character removed.  If the
 *  string ends with <code>\r\n</code>, both characters are removed. Applying
 *  <code>chop</code> to an empty string returns an empty
 *  string. <code>String#chomp</code> is often a safer alternative, as it leaves
 *  the string unchanged if it doesn't end in a record separator.
 *
 *     "string\r\n".chop   #=> "string"
 *     "string\n\r".chop   #=> "string\n"
 *     "string\n".chop     #=> "string"
 *     "string".chop       #=> "strin"
 *     "x".chop            #=> ""
 */
static mrb_value
mrb_str_chop(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  str = mrb_str_dup(mrb, self);
  mrb_str_chop_bang(mrb, str);
  return str;
}

/* 15.2.10.5.14 */
/*
 *  call-seq:
 *     str.downcase!   => str or nil
 *
 *  Downcases the contents of <i>str</i>, returning <code>nil</code> if no
 *  changes were made.
 */
static mrb_value
mrb_str_downcase_bang(mrb_state *mrb, mrb_value str)
{
  char *p, *pend;
  mrb_bool modify = FALSE;
  struct RString *s = mrb_str_ptr(str);

  mrb_str_modify_keep_ascii(mrb, s);
  p = RSTR_PTR(s);
  pend = RSTR_PTR(s) + RSTR_LEN(s);
  while (p < pend) {
    if (ISUPPER(*p)) {
      *p = TOLOWER(*p);
      modify = TRUE;
    }
    p++;
  }

  if (modify) return str;
  return mrb_nil_value();
}

/* 15.2.10.5.13 */
/*
 *  call-seq:
 *     str.downcase   => new_str
 *
 *  Returns a copy of <i>str</i> with all uppercase letters replaced with their
 *  lowercase counterparts. The operation is locale insensitive---only
 *  characters 'A' to 'Z' are affected.
 *
 *     "hEllO".downcase   #=> "hello"
 */
static mrb_value
mrb_str_downcase(mrb_state *mrb, mrb_value self)
{
  mrb_value str;

  str = mrb_str_dup(mrb, self);
  mrb_str_downcase_bang(mrb, str);
  return str;
}

/* 15.2.10.5.16 */
/*
 *  call-seq:
 *     str.empty?   => true or false
 *
 *  Returns <code>true</code> if <i>str</i> has a length of zero.
 *
 *     "hello".empty?   #=> false
 *     "".empty?        #=> true
 */
static mrb_value
mrb_str_empty_p(mrb_state *mrb, mrb_value self)
{
  struct RString *s = mrb_str_ptr(self);

  return mrb_bool_value(RSTR_LEN(s) == 0);
}

/* 15.2.10.5.17 */
/*
 * call-seq:
 *   str.eql?(other)   => true or false
 *
 * Two strings are equal if the have the same length and content.
 */
static mrb_value
mrb_str_eql(mrb_state *mrb, mrb_value self)
{
  mrb_value str2 = mrb_get_arg1(mrb);
  mrb_bool eql_p = (mrb_string_p(str2)) && str_eql(mrb, self, str2);

  return mrb_bool_value(eql_p);
}

MRB_API mrb_value
mrb_str_substr(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len)
{
  return str_substr(mrb, str, beg, len);
}

/*
 * 32-bit magic FNV-0 and FNV-1 prime
b */
#define FNV_32_PRIME ((uint32_t)0x01000193)
#define FNV1_32_INIT ((uint32_t)0x811c9dc5)

uint32_t
mrb_byte_hash_step(const uint8_t *s, mrb_int len, uint32_t hval)
{
  const uint8_t *send = s + len;

  /*
   * FNV-1 hash each octet in the buffer
   */
  while (s < send) {
    /* multiply by the 32-bit FNV magic prime mod 2^32 */
#if defined(NO_FNV_GCC_OPTIMIZATION)
    hval *= FNV_32_PRIME;
#else
    hval += (hval<<1) + (hval<<4) + (hval<<7) + (hval<<8) + (hval<<24);
#endif

    /* xor the bottom with the current octet */
    hval ^= (uint32_t)*s++;
  }

  /* return our new hash value */
  return hval;
}

uint32_t
mrb_byte_hash(const uint8_t *s, mrb_int len)
{
  return mrb_byte_hash_step(s, len, FNV1_32_INIT);
}

uint32_t
mrb_str_hash(mrb_state *mrb, mrb_value str)
{
  struct RString *s = mrb_str_ptr(str);
  return mrb_byte_hash((uint8_t*)RSTR_PTR(s), RSTR_LEN(s));
}

/* 15.2.10.5.20 */
/*
 * call-seq:
 *    str.hash   => int
 *
 * Return a hash based on the string's length and content.
 */
static mrb_value
mrb_str_hash_m(mrb_state *mrb, mrb_value self)
{
  mrb_int key = mrb_str_hash(mrb, self);
  return mrb_int_value(mrb, key);
}

/* 15.2.10.5.21 */
/*
 *  call-seq:
 *     str.include? other_str   => true or false
 *     str.include? int         => true or false
 *
 *  Returns <code>true</code> if <i>str</i> contains the given string or
 *  character.
 *
 *     "hello".include? "lo"   #=> true
 *     "hello".include? "ol"   #=> false
 *     "hello".include? ?h     #=> true
 */
static mrb_value
mrb_str_include(mrb_state *mrb, mrb_value self)
{
  mrb_value str2;

  mrb_get_args(mrb, "S", &str2);
  if (str_index_str(mrb, self, str2, 0) < 0)
    return mrb_bool_value(FALSE);
  return mrb_bool_value(TRUE);
}

/*
 *  call-seq:
 *    str.byteindex(substring, offset = 0) -> integer or nil
 *
 *  Returns the \Integer byte-based index of the first occurrence of the given +substring+,
 *  or +nil+ if none found:
 *
 *    'foo'.byteindex('f') # => 0
 *    'foo'.byteindex('oo') # => 1
 *    'foo'.byteindex('ooo') # => nil
 */
static mrb_value
mrb_str_byteindex_m(mrb_state *mrb, mrb_value str)
{
  mrb_value sub;
  mrb_int pos;

  if (mrb_get_args(mrb, "S|i", &sub, &pos) == 1) {
    pos = 0;
  }
  else if (pos < 0) {
    pos += RSTRING_LEN(str);
    if (pos < 0) {
      return mrb_nil_value();
    }
  }
  pos = str_index_str(mrb, str, sub, pos);

  if (pos == -1) return mrb_nil_value();
  return mrb_int_value(mrb, pos);
}

/* 15.2.10.5.22 */
/*
 *  call-seq:
 *     str.index(substring [, offset])   => int or nil
 *
 *  Returns the index of the first occurrence of the given
 *  <i>substring</i>. Returns <code>nil</code> if not found.
 *  If the second parameter is present, it
 *  specifies the position in the string to begin the search.
 *
 *     "hello".index('l')             #=> 2
 *     "hello".index('lo')            #=> 3
 *     "hello".index('a')             #=> nil
 *     "hello".index('l', -2)         #=> 3
 */
#ifdef MRB_UTF8_STRING
static mrb_value
mrb_str_index_m(mrb_state *mrb, mrb_value str)
{
  if (RSTR_SINGLE_BYTE_P(mrb_str_ptr(str))) {
    return mrb_str_byteindex_m(mrb, str);
  }

  mrb_value sub;
  mrb_int pos;

  if (mrb_get_args(mrb, "S|i", &sub, &pos) == 1) {
    pos = 0;
  }
  else if (pos < 0) {
    mrb_int clen = RSTRING_CHAR_LEN(str);
    pos += clen;
    if (pos < 0) {
      return mrb_nil_value();
    }
  }
  pos = str_index_str_by_char(mrb, str, sub, pos);

  if (pos == -1) return mrb_nil_value();
  return mrb_int_value(mrb, pos);
}
#else
#define mrb_str_index_m mrb_str_byteindex_m
#endif

/* 15.2.10.5.24 */
/* 15.2.10.5.28 */
/*
 *  call-seq:
 *     str.replace(other_str)   => str
 *
 *     s = "hello"         #=> "hello"
 *     s.replace "world"   #=> "world"
 */
static mrb_value
mrb_str_replace(mrb_state *mrb, mrb_value str)
{
  mrb_value str2;

  mrb_get_args(mrb, "S", &str2);
  return str_replace(mrb, mrb_str_ptr(str), mrb_str_ptr(str2));
}

/* 15.2.10.5.23 */
/*
 *  call-seq:
 *     String.new(str="")   => new_str
 *
 *  Returns a new string object containing a copy of <i>str</i>.
 */
static mrb_value
mrb_str_init(mrb_state *mrb, mrb_value self)
{
  mrb_value str2;

  if (mrb_get_args(mrb, "|S", &str2) == 0) {
    str2 = mrb_str_new(mrb, 0, 0);
  }
  str_replace(mrb, mrb_str_ptr(self), mrb_str_ptr(str2));
  return self;
}

/* 15.2.10.5.25 */
/* 15.2.10.5.41 */
/*
 *  call-seq:
 *     str.intern   => symbol
 *     str.to_sym   => symbol
 *
 *  Returns the <code>Symbol</code> corresponding to <i>str</i>, creating the
 *  symbol if it did not previously exist.
 *
 *     "Koala".intern         #=> :Koala
 *     s = 'cat'.to_sym       #=> :cat
 *     s == :cat              #=> true
 *     s = '@cat'.to_sym      #=> :@cat
 *     s == :@cat             #=> true
 *
 *  This can also be used to create symbols that cannot be represented using the
 *  <code>:xxx</code> notation.
 *
 *     'cat and dog'.to_sym   #=> :"cat and dog"
 */
MRB_API mrb_value
mrb_str_intern(mrb_state *mrb, mrb_value self)
{
  return mrb_symbol_value(mrb_intern_str(mrb, self));
}
/* ---------------------------------- */
MRB_API mrb_value
mrb_obj_as_string(mrb_state *mrb, mrb_value obj)
{
  switch (mrb_type(obj)) {
  case MRB_TT_STRING:
    return obj;
  case MRB_TT_SYMBOL:
    return mrb_sym_str(mrb, mrb_symbol(obj));
  case MRB_TT_INTEGER:
    return mrb_integer_to_str(mrb, obj, 10);
  case MRB_TT_SCLASS:
  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
    return mrb_mod_to_s(mrb, obj);
  default:
    return mrb_type_convert(mrb, obj, MRB_TT_STRING, MRB_SYM(to_s));
  }
}

MRB_API mrb_value
mrb_ptr_to_str(mrb_state *mrb, void *p)
{
  struct RString *p_str;
  char *p1;
  char *p2;
  uintptr_t n = (uintptr_t)p;

  p_str = str_new(mrb, NULL, 2 + sizeof(uintptr_t) * CHAR_BIT / 4);
  p1 = RSTR_PTR(p_str);
  *p1++ = '0';
  *p1++ = 'x';
  p2 = p1;

  do {
    *p2++ = mrb_digitmap[n % 16];
    n /= 16;
  } while (n > 0);
  *p2 = '\0';
  RSTR_SET_LEN(p_str, (mrb_int)(p2 - RSTR_PTR(p_str)));

  while (p1 < p2) {
    const char  c = *p1;
    *p1++ = *--p2;
    *p2 = c;
  }

  return mrb_obj_value(p_str);
}

static inline void
str_reverse(char *p, char *e)
{
  char c;

  while (p < e) {
    c = *p;
    *p++ = *e;
    *e-- = c;
  }
}

/* 15.2.10.5.30 */
/*
 *  call-seq:
 *     str.reverse!   => str
 *
 *  Reverses <i>str</i> in place.
 */
static mrb_value
mrb_str_reverse_bang(mrb_state *mrb, mrb_value str)
{
  struct RString *s = mrb_str_ptr(str);
  char *p, *e;

#ifdef MRB_UTF8_STRING
  mrb_int utf8_len = RSTRING_CHAR_LEN(str);
  mrb_int len = RSTR_LEN(s);

  if (utf8_len < 2) return str;
  if (utf8_len < len) {
    mrb_str_modify(mrb, s);
    p = RSTR_PTR(s);
    e = p + RSTR_LEN(s);
    while (p<e) {
      mrb_int clen = mrb_utf8len(p, e);
      str_reverse(p, p + clen - 1);
      p += clen;
    }
    goto bytes;
  }
#endif

  if (RSTR_LEN(s) > 1) {
    mrb_str_modify(mrb, s);
    goto bytes;
  }
  return str;

 bytes:
  p = RSTR_PTR(s);
  e = p + RSTR_LEN(s) - 1;
  str_reverse(p, e);
  return str;
}

/* ---------------------------------- */
/* 15.2.10.5.29 */
/*
 *  call-seq:
 *     str.reverse   => new_str
 *
 *  Returns a new string with the characters from <i>str</i> in reverse order.
 *
 *     "stressed".reverse   #=> "desserts"
 */
static mrb_value
mrb_str_reverse(mrb_state *mrb, mrb_value str)
{
  mrb_value str2 = mrb_str_dup(mrb, str);
  mrb_str_reverse_bang(mrb, str2);
  return str2;
}

/*
 *  call-seq:
 *    byterindex(substring, offset = self.bytesize) -> integer or nil
 *
 *  Returns the \Integer byte-based index of the _last_ occurrence of the given +substring+,
 *  or +nil+ if none found:
 *
 *    'foo'.byterindex('f') # => 0
 *    'foo'.byterindex('o') # => 2
 *    'foo'.byterindex('oo') # => 1
 *    'foo'.byterindex('ooo') # => nil
 */
static mrb_value
mrb_str_byterindex_m(mrb_state *mrb, mrb_value str)
{
  mrb_value sub;
  mrb_int pos;
  mrb_int len = RSTRING_LEN(str);

  if (mrb_get_args(mrb, "S|i", &sub, &pos) == 1) {
    pos = len;
  }
  else {
    if (pos < 0) {
      pos += len;
      if (pos < 0) {
        return mrb_nil_value();
      }
    }
    if (pos > len) pos = len;
  }
  pos = str_rindex(mrb, str, sub, pos);
  if (pos < 0) {
    return mrb_nil_value();
  }
  return mrb_int_value(mrb, pos);
}

/* 15.2.10.5.31 */
/*
 *  call-seq:
 *     str.rindex(substring [, offset])   => int or nil
 *
 *  Returns the index of the last occurrence of the given <i>substring</i>.
 *  Returns <code>nil</code> if not found. If the second parameter is
 *  present, it specifies the position in the string to end the
 *  search---characters beyond this point will not be considered.
 *
 *     "hello".rindex('e')             #=> 1
 *     "hello".rindex('l')             #=> 3
 *     "hello".rindex('a')             #=> nil
 *     "hello".rindex('l', 2)          #=> 2
 */
#ifdef MRB_UTF8_STRING
static mrb_value
mrb_str_rindex_m(mrb_state *mrb, mrb_value str)
{
  if (RSTR_SINGLE_BYTE_P(mrb_str_ptr(str))) {
    return mrb_str_byterindex_m(mrb, str);
  }

  mrb_value sub;
  mrb_int pos;

  if (mrb_get_args(mrb, "S|i", &sub, &pos) == 1) {
    pos = RSTRING_LEN(str);
  }
  else if (pos >= 0) {
    pos = chars2bytes(str, 0, pos);
  }
  else {
    const char *p = RSTRING_PTR(str);
    const char *e = RSTRING_END(str);
    while (pos++ < 0 && p < e) {
      e = char_backtrack(p, e);
    }
    if (p == e) return mrb_nil_value();
    pos = (mrb_int)(e - p);
  }
  pos = str_rindex(mrb, str, sub, pos);
  if (pos >= 0) {
    pos = bytes2chars(str, pos);
    if (pos < 0) return mrb_nil_value();
    return mrb_int_value(mrb, pos);
  }
  return mrb_nil_value();
}
#else
#define mrb_str_rindex_m mrb_str_byterindex_m
#endif

/* 15.2.10.5.35 */

/*
 *  call-seq:
 *     str.split(separator=nil, [limit])   => anArray
 *
 *  Divides <i>str</i> into substrings based on a delimiter, returning an array
 *  of these substrings.
 *
 *  If <i>separator</i> is a <code>String</code>, then its contents are used as
 *  the delimiter when splitting <i>str</i>. If <i>separator</i> is a single
 *  space, <i>str</i> is split on whitespace, with leading whitespace and runs
 *  of contiguous whitespace characters ignored.
 *
 *  If <i>separator</i> is omitted or <code>nil</code> (which is the default),
 *  <i>str</i> is split on whitespace as if ' ' were specified.
 *
 *  If the <i>limit</i> parameter is omitted, trailing null fields are
 *  suppressed. If <i>limit</i> is a positive number, at most that number of
 *  fields will be returned (if <i>limit</i> is <code>1</code>, the entire
 *  string is returned as the only entry in an array). If negative, there is no
 *  limit to the number of fields returned, and trailing null fields are not
 *  suppressed.
 *
 *     " now's  the time".split        #=> ["now's", "the", "time"]
 *     " now's  the time".split(' ')   #=> ["now's", "the", "time"]
 *
 *     "mellow yellow".split("ello")   #=> ["m", "w y", "w"]
 *     "1,2,,3,4,,".split(',')         #=> ["1", "2", "", "3", "4"]
 *     "1,2,,3,4,,".split(',', 4)      #=> ["1", "2", "", "3,4,,"]
 *     "1,2,,3,4,,".split(',', -4)     #=> ["1", "2", "", "3", "4", "", ""]
 */

static mrb_value
mrb_str_split_m(mrb_state *mrb, mrb_value str)
{
  mrb_int argc;
  mrb_value spat = mrb_nil_value();
  enum {awk, string} split_type = string;
  mrb_int i = 0;
  mrb_int beg;
  mrb_int end;
  mrb_int lim = 0;
  mrb_bool lim_p;
  mrb_value result, tmp;

  argc = mrb_get_args(mrb, "|oi", &spat, &lim);
  lim_p = (lim > 0 && argc == 2);
  if (argc == 2) {
    if (lim == 1) {
      if (RSTRING_LEN(str) == 0)
        return mrb_ary_new_capa(mrb, 0);
      return mrb_ary_new_from_values(mrb, 1, &str);
    }
    i = 1;
  }

  if (argc == 0 || mrb_nil_p(spat)) {
    split_type = awk;
  }
  else if (!mrb_string_p(spat)) {
    mrb_raise(mrb, E_TYPE_ERROR, "expected String");
  }
  else if (RSTRING_LEN(spat) == 1 && RSTRING_PTR(spat)[0] == ' ') {
    split_type = awk;
  }

  result = mrb_ary_new(mrb);
  beg = 0;
  if (split_type == awk) {
    mrb_bool skip = TRUE;
    mrb_int idx = 0;
    mrb_int str_len = RSTRING_LEN(str);
    unsigned int c;
    int ai = mrb_gc_arena_save(mrb);

    idx = end = beg;
    while (idx < str_len) {
      c = (unsigned char)RSTRING_PTR(str)[idx++];
      if (skip) {
        if (ISSPACE(c)) {
          beg = idx;
        }
        else {
          end = idx;
          skip = FALSE;
          if (lim_p && lim <= i) break;
        }
      }
      else if (ISSPACE(c)) {
        mrb_ary_push(mrb, result, mrb_str_byte_subseq(mrb, str, beg, end-beg));
        mrb_gc_arena_restore(mrb, ai);
        skip = TRUE;
        beg = idx;
        if (lim_p) i++;
      }
      else {
        end = idx;
      }
    }
  }
  else {                        /* split_type == string */
    mrb_int str_len = RSTRING_LEN(str);
    mrb_int pat_len = RSTRING_LEN(spat);
    mrb_int idx = 0;
    int ai = mrb_gc_arena_save(mrb);

    while (idx < str_len) {
      if (pat_len > 0) {
        end = mrb_memsearch(RSTRING_PTR(spat), pat_len, RSTRING_PTR(str)+idx, str_len - idx);
        if (end < 0) break;
      }
      else {
        end = chars2bytes(str, idx, 1);
      }
      mrb_ary_push(mrb, result, mrb_str_byte_subseq(mrb, str, idx, end));
      mrb_gc_arena_restore(mrb, ai);
      idx += end + pat_len;
      if (lim_p && lim <= ++i) break;
    }
    beg = idx;
  }
  if (RSTRING_LEN(str) > 0 && (lim_p || RSTRING_LEN(str) > beg || lim < 0)) {
    if (RSTRING_LEN(str) == beg) {
      tmp = mrb_str_new(mrb, 0, 0);
    }
    else {
      tmp = mrb_str_byte_subseq(mrb, str, beg, RSTRING_LEN(str)-beg);
    }
    mrb_ary_push(mrb, result, tmp);
  }
  if (!lim_p && lim == 0) {
    mrb_int len;
    while ((len = RARRAY_LEN(result)) > 0 &&
           (tmp = RARRAY_PTR(result)[len-1], RSTRING_LEN(tmp) == 0))
      mrb_ary_pop(mrb, result);
  }

  return result;
}

static mrb_bool
trailingbad(const char *str, const char *p, const char *pend)
{
  if (p == str) return TRUE;             /* no number */
  if (*(p - 1) == '_') return TRUE;      /* trailing '_' */
  while (p<pend && ISSPACE(*p)) p++;
  if (p<pend) return TRUE;               /* trailing garbage */
  return FALSE;
}

static mrb_value
mrb_str_len_to_integer(mrb_state *mrb, const char *str, size_t len, mrb_int base, int badcheck)
{
  const char *p = str;
  const char *pend = str + len;
#ifdef MRB_USE_BIGINT
  const char *p2 = NULL;
#endif
  char sign = 1;
  int c;
  mrb_int n = 0;
  mrb_int val;

#define conv_digit(c) \
    (ISDIGIT(c) ? ((c) - '0') : \
     ISLOWER(c) ? ((c) - 'a' + 10) : \
     ISUPPER(c) ? ((c) - 'A' + 10) : \
     -1)

  if (!p) {
    if (badcheck) goto bad;
    return mrb_fixnum_value(0);
  }
  while (p<pend && ISSPACE(*p))
    p++;

  if (p[0] == '+') {
    p++;
  }
  else if (p[0] == '-') {
    p++;
    sign = 0;
  }
  if (base <= 0) {
    if (p[0] == '0') {
      switch (p[1]) {
        case 'x': case 'X':
          base = 16;
          break;
        case 'b': case 'B':
          base = 2;
          break;
        case 'o': case 'O':
          base = 8;
          break;
        case 'd': case 'D':
          base = 10;
          break;
        default:
          base = 8;
          break;
      }
    }
    else if (base < -1) {
      base = -base;
    }
    else {
      base = 10;
    }
  }
  switch (base) {
    case 2:
      if (p[0] == '0' && (p[1] == 'b'||p[1] == 'B')) {
        p += 2;
      }
      break;
    case 3:
      break;
    case 8:
      if (p[0] == '0' && (p[1] == 'o'||p[1] == 'O')) {
        p += 2;
      }
    case 4: case 5: case 6: case 7:
      break;
    case 10:
      if (p[0] == '0' && (p[1] == 'd'||p[1] == 'D')) {
        p += 2;
      }
    case 9: case 11: case 12: case 13: case 14: case 15:
      break;
    case 16:
      if (p[0] == '0' && (p[1] == 'x'||p[1] == 'X')) {
        p += 2;
      }
      break;
    default:
      if (base < 2 || 36 < base) {
        mrb_raisef(mrb, E_ARGUMENT_ERROR, "illegal radix %i", base);
      }
      break;
  } /* end of switch (base) { */
  if (p>=pend) {
    if (badcheck) goto bad;
    return mrb_fixnum_value(0);
  }
  if (*p == '0') {    /* squeeze preceding 0s */
    p++;
    while (p<pend) {
      c = *p++;
      if (c == '_') {
        if (p<pend && *p == '_') {
          if (badcheck) goto bad;
          break;
        }
        continue;
      }
      if (c != '0') {
        p--;
        break;
      }
    }
    if (*(p - 1) == '0')
      p--;
  }
  if (p == pend || *p == '_') {
    if (badcheck) goto bad;
    return mrb_fixnum_value(0);
  }
#ifdef MRB_USE_BIGINT
  p2 = p;
#endif
  for (;p<pend; p++) {
    if (*p == '_') {
      p++;
      if (p==pend) {
        if (badcheck) goto bad;
        continue;
      }
      if (*p == '_') {
        if (badcheck) goto bad;
        break;
      }
    }
    if (badcheck && *p == '\0') {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "string contains null byte");
    }
    c = conv_digit(*p);
    if (c < 0 || c >= base) {
      break;
    }
    if (mrb_int_mul_overflow(n, base, &n)) goto overflow;
    if (MRB_INT_MAX - c < n) {
      if (sign == 0 && MRB_INT_MAX - n == c - 1) {
        n = MRB_INT_MIN;
        sign = 1;
        break;
      }
    overflow:
#ifdef MRB_USE_BIGINT
      ;
      const char *p3 = p2;
      while (p3 < pend) {
        char c = TOLOWER(*p3);
        const char *p4 = strchr(mrb_digitmap, c);
        if (p4 == NULL && c != '_') break;
        if (p4 - mrb_digitmap >= base) break;
        p3++;
      }
      if (badcheck && trailingbad(str, p, pend)) goto bad;
      return mrb_bint_new_str(mrb, p2, (mrb_int)(p3-p2), sign ? base : -base);
#else
      mrb_raisef(mrb, E_RANGE_ERROR, "string (%l) too big for integer", str, pend-str);
#endif
    }
    n += c;
  }
  val = (mrb_int)n;
  if (badcheck && trailingbad(str, p, pend)) goto bad;
  return mrb_int_value(mrb, sign ? val : -val);
 bad:
  mrb_raisef(mrb, E_ARGUMENT_ERROR, "invalid string for number(%!l)", str, pend-str);
  /* not reached */
  return mrb_fixnum_value(0);
}

/* obsolete: use RSTRING_CSTR() or mrb_string_cstr() */
MRB_API const char*
mrb_string_value_cstr(mrb_state *mrb, mrb_value *ptr)
{
  struct RString *ps;
  const char *p;
  mrb_int len;

  mrb_ensure_string_type(mrb, *ptr);
  ps = mrb_str_ptr(*ptr);
  check_null_byte(mrb, ps);
  p = RSTR_PTR(ps);
  len = RSTR_LEN(ps);
  if (p == NULL) return "";
  if (p[len] == '\0') {
    return p;
  }

  /*
   * Even after str_modify_keep_ascii(), NULL termination is not ensured if
   * RSTR_SET_LEN() is used explicitly (e.g. String#delete_suffix!).
   */
  str_unshare_buffer(mrb, ps);
  RSTR_PTR(ps)[len] = '\0';
  return RSTR_PTR(ps);
}

MRB_API const char*
mrb_string_cstr(mrb_state *mrb, mrb_value str)
{
  return mrb_string_value_cstr(mrb, &str);
}

MRB_API mrb_value
mrb_str_to_integer(mrb_state *mrb, mrb_value str, mrb_int base, mrb_bool badcheck)
{
  const char *s;
  mrb_int len;

  mrb_ensure_string_type(mrb, str);
  s = RSTRING_PTR(str);
  len = RSTRING_LEN(str);
  return mrb_str_len_to_integer(mrb, s, len, base, badcheck);
}

/* 15.2.10.5.38 */
/*
 *  call-seq:
 *     str.to_i(base=10)   => integer
 *
 *  Returns the result of interpreting leading characters in <i>str</i> as an
 *  integer base <i>base</i> (between 2 and 36). Extraneous characters past the
 *  end of a valid number are ignored. If there is not a valid number at the
 *  start of <i>str</i>, <code>0</code> is returned. This method never raises an
 *  exception.
 *
 *     "12345".to_i             #=> 12345
 *     "99 red balloons".to_i   #=> 99
 *     "0a".to_i                #=> 0
 *     "0a".to_i(16)            #=> 10
 *     "hello".to_i             #=> 0
 *     "1100101".to_i(2)        #=> 101
 *     "1100101".to_i(8)        #=> 294977
 *     "1100101".to_i(10)       #=> 1100101
 *     "1100101".to_i(16)       #=> 17826049
 */
static mrb_value
mrb_str_to_i(mrb_state *mrb, mrb_value self)
{
  mrb_int base = 10;

  mrb_get_args(mrb, "|i", &base);
  if (base < 0 || 36 < base) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "illegal radix %i", base);
  }
  return mrb_str_to_integer(mrb, self, base, FALSE);
}

#ifndef MRB_NO_FLOAT
static double
mrb_str_len_to_dbl(mrb_state *mrb, const char *s, size_t len, mrb_bool badcheck)
{
  char buf[DBL_DIG * 4 + 20];
  const char *p = s, *p2;
  const char *pend = p + len;
  char *end;
  char *n;
  char prev = 0;
  double d;
  mrb_bool dot = FALSE;

  if (!p) return 0.0;
  while (p<pend && ISSPACE(*p)) p++;
  p2 = p;

  if (pend - p > 2 && p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
    mrb_value x;

    if (!badcheck) return 0.0;
    x = mrb_str_len_to_integer(mrb, p, pend-p, 0, badcheck);
    if (mrb_integer_p(x))
      d = (double)mrb_integer(x);
    else /* if (mrb_float_p(x)) */
      d = mrb_float(x);
    return d;
  }
  while (p < pend) {
    if (!*p) {
      if (badcheck) {
        mrb_raise(mrb, E_ARGUMENT_ERROR, "string for Float contains null byte");
        /* not reached */
      }
      pend = p;
      p = p2;
      goto nocopy;
    }
    if (!badcheck && *p == ' ') {
      pend = p;
      p = p2;
      goto nocopy;
    }
    if (*p == '_') break;
    p++;
  }
  p = p2;
  n = buf;
  while (p < pend) {
    char c = *p++;
    if (c == '.') dot = TRUE;
    if (c == '_') {
      /* remove an underscore between digits */
      if (n == buf || !ISDIGIT(prev) || p == pend) {
        if (badcheck) goto bad;
        break;
      }
    }
    else if (badcheck && prev == '_' && !ISDIGIT(c)) goto bad;
    else {
      const char *bend = buf+sizeof(buf)-1;
      if (n==bend) {            /* buffer overflow */
        if (dot) break;         /* cut off remaining fractions */
        return INFINITY;
      }
      *n++ = c;
    }
    prev = c;
  }
  *n = '\0';
  p = buf;
  pend = n;
nocopy:
  if (mrb_read_float(p, &end, &d) == FALSE) {
    if (badcheck) {
bad:
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "invalid string for float(%!s)", s);
      /* not reached */
    }
    return 0.0;
  }
  if (badcheck) {
    if (!end || p == end) goto bad;
    while (end<pend && ISSPACE(*end)) end++;
    if (end<pend) goto bad;
  }
  return d;
}

MRB_API double
mrb_str_to_dbl(mrb_state *mrb, mrb_value str, mrb_bool badcheck)
{
  return mrb_str_len_to_dbl(mrb, RSTRING_PTR(str), RSTRING_LEN(str), badcheck);
}

/* 15.2.10.5.39 */
/*
 *  call-seq:
 *     str.to_f   => float
 *
 *  Returns the result of interpreting leading characters in <i>str</i> as a
 *  floating-point number. Extraneous characters past the end of a valid number
 *  are ignored. If there is not a valid number at the start of <i>str</i>,
 *  <code>0.0</code> is returned. This method never raises an exception.
 *
 *     "123.45e1".to_f        #=> 1234.5
 *     "45.67 degrees".to_f   #=> 45.67
 *     "thx1138".to_f         #=> 0.0
 */
static mrb_value
mrb_str_to_f(mrb_state *mrb, mrb_value self)
{
  return mrb_float_value(mrb, mrb_str_to_dbl(mrb, self, FALSE));
}
#endif

/* 15.2.10.5.40 */
/*
 *  call-seq:
 *     str.to_s     => str
 *
 *  Returns the receiver.
 */
static mrb_value
mrb_str_to_s(mrb_state *mrb, mrb_value self)
{
  if (mrb_obj_class(mrb, self) != mrb->string_class) {
    return mrb_str_dup(mrb, self);
  }
  return self;
}

/* 15.2.10.5.43 */
/*
 *  call-seq:
 *     str.upcase!   => str or nil
 *
 *  Upcases the contents of <i>str</i>, returning <code>nil</code> if no changes
 *  were made.
 */
static mrb_value
mrb_str_upcase_bang(mrb_state *mrb, mrb_value str)
{
  struct RString *s = mrb_str_ptr(str);
  char *p, *pend;
  mrb_bool modify = FALSE;

  mrb_str_modify_keep_ascii(mrb, s);
  p = RSTRING_PTR(str);
  pend = RSTRING_END(str);
  while (p < pend) {
    if (ISLOWER(*p)) {
      *p = TOUPPER(*p);
      modify = TRUE;
    }
    p++;
  }

  if (modify) return str;
  return mrb_nil_value();
}

/* 15.2.10.5.42 */
/*
 *  call-seq:
 *     str.upcase   => new_str
 *
 *  Returns a copy of <i>str</i> with all lowercase letters replaced with their
 *  uppercase counterparts. The operation is locale insensitive---only
 *  characters 'a' to 'z' are affected.
 *
 *     "hEllO".upcase   #=> "HELLO"
 */
static mrb_value
mrb_str_upcase(mrb_state *mrb, mrb_value self)
{
  mrb_value str;

  str = mrb_str_dup(mrb, self);
  mrb_str_upcase_bang(mrb, str);
  return str;
}

/*
 *  call-seq:
 *     str.dump   -> new_str
 *
 *  Produces a version of <i>str</i> with all nonprinting characters replaced by
 *  <code>\nnn</code> notation and all special characters escaped.
 */
mrb_value
mrb_str_dump(mrb_state *mrb, mrb_value str)
{
  return str_escape(mrb, str, FALSE);
}

MRB_API mrb_value
mrb_str_cat(mrb_state *mrb, mrb_value str, const char *ptr, size_t len)
{
  struct RString *s = mrb_str_ptr(str);
  mrb_int capa;
  mrb_int total;
  ptrdiff_t off = -1;

  if (len == 0) return str;
  mrb_str_modify(mrb, s);
  if (ptr >= RSTR_PTR(s) && ptr <= RSTR_PTR(s) + (size_t)RSTR_LEN(s)) {
      off = ptr - RSTR_PTR(s);
  }

  capa = RSTR_CAPA(s);
  if (mrb_int_add_overflow(RSTR_LEN(s), len, &total)) {
  size_error:
    mrb_raise(mrb, E_ARGUMENT_ERROR, "string size too big");
  }
  if (capa <= total) {
    if (capa == 0) capa = 1;
    while (capa <= total) {
      if (mrb_int_mul_overflow(capa, 2, &capa)) goto size_error;
    }
    resize_capa(mrb, s, capa);
  }
  if (off != -1) {
      ptr = RSTR_PTR(s) + off;
  }
  memcpy(RSTR_PTR(s) + RSTR_LEN(s), ptr, len);
  RSTR_SET_LEN(s, total);
  RSTR_PTR(s)[total] = '\0';   /* sentinel */
  return str;
}

MRB_API mrb_value
mrb_str_cat_cstr(mrb_state *mrb, mrb_value str, const char *ptr)
{
  return mrb_str_cat(mrb, str, ptr, ptr ? strlen(ptr) : 0);
}

MRB_API mrb_value
mrb_str_cat_str(mrb_state *mrb, mrb_value str, mrb_value str2)
{
  if (mrb_str_ptr(str) == mrb_str_ptr(str2)) {
    mrb_str_modify(mrb, mrb_str_ptr(str));
  }
  return mrb_str_cat(mrb, str, RSTRING_PTR(str2), RSTRING_LEN(str2));
}

MRB_API mrb_value
mrb_str_append(mrb_state *mrb, mrb_value str1, mrb_value str2)
{
  mrb_ensure_string_type(mrb, str2);
  return mrb_str_cat_str(mrb, str1, str2);
}

/*
 * call-seq:
 *   str.inspect   -> string
 *
 * Returns a printable version of _str_, surrounded by quote marks,
 * with special characters escaped.
 *
 *    str = "hello"
 *    str[3] = "\b"
 *    str.inspect       #=> "\"hel\\bo\""
 */
mrb_value
mrb_str_inspect(mrb_state *mrb, mrb_value str)
{
  return str_escape(mrb, str, TRUE);
}

/*
 * call-seq:
 *   str.bytes   -> array of int
 *
 * Returns an array of bytes in _str_.
 *
 *    str = "hello"
 *    str.bytes       #=> [104, 101, 108, 108, 111]
 */
static mrb_value
mrb_str_bytes(mrb_state *mrb, mrb_value str)
{
  struct RString *s = mrb_str_ptr(str);
  mrb_value a = mrb_ary_new_capa(mrb, RSTR_LEN(s));
  unsigned char *p = (unsigned char*)(RSTR_PTR(s)), *pend = p + RSTR_LEN(s);

  while (p < pend) {
    mrb_ary_push(mrb, a, mrb_fixnum_value(p[0]));
    p++;
  }
  return a;
}

/*
 *  call-seq:
 *     str.getbyte(index)          -> 0 .. 255
 *
 *  returns the <i>index</i>th byte as an integer.
 */
static mrb_value
mrb_str_getbyte(mrb_state *mrb, mrb_value str)
{
  mrb_int pos;
  mrb_get_args(mrb, "i", &pos);

  if (pos < 0)
    pos += RSTRING_LEN(str);
  if (pos < 0 ||  RSTRING_LEN(str) <= pos)
    return mrb_nil_value();

  return mrb_fixnum_value((unsigned char)RSTRING_PTR(str)[pos]);
}

/*
 *  call-seq:
 *     str.setbyte(index, integer) -> integer
 *
 *  modifies the <i>index</i>th byte as <i>integer</i>.
 */
static mrb_value
mrb_str_setbyte(mrb_state *mrb, mrb_value str)
{
  mrb_int pos, byte;
  mrb_int len;

  mrb_get_args(mrb, "ii", &pos, &byte);

  len = RSTRING_LEN(str);
  if (pos < -len || len <= pos)
    mrb_raisef(mrb, E_INDEX_ERROR, "index %i out of string", pos);
  if (pos < 0)
    pos += len;

  mrb_str_modify(mrb, mrb_str_ptr(str));
  byte &= 0xff;
  RSTRING_PTR(str)[pos] = (unsigned char)byte;
  return mrb_fixnum_value((unsigned char)byte);
}

/*
 *  call-seq:
 *     str.byteslice(integer)           -> new_str or nil
 *     str.byteslice(integer, integer)  -> new_str or nil
 *     str.byteslice(range)             -> new_str or nil
 *
 *  Byte Reference---If passed a single Integer, returns a
 *  substring of one byte at that position. If passed two Integer
 *  objects, returns a substring starting at the offset given by the first, and
 *  a length given by the second. If given a Range, a substring containing
 *  bytes at offsets given by the range is returned. In all three cases, if
 *  an offset is negative, it is counted from the end of <i>str</i>. Returns
 *  <code>nil</code> if the initial offset falls outside the string, the length
 *  is negative, or the beginning of the range is greater than the end.
 *  The encoding of the resulted string keeps original encoding.
 *
 *     "hello".byteslice(1)     #=> "e"
 *     "hello".byteslice(-1)    #=> "o"
 *     "hello".byteslice(1, 2)  #=> "el"
 *     "\x80\u3042".byteslice(1, 3) #=> "\u3042"
 *     "\x03\u3042\xff".byteslice(1..3) #=> "\u3042"
 */
static mrb_value
mrb_str_byteslice(mrb_state *mrb, mrb_value str)
{
  mrb_value a1;
  mrb_int str_len, beg, len;
  mrb_bool empty = TRUE;

  len = mrb_get_argc(mrb);
  switch (len) {
  case 2:
    mrb_get_args(mrb, "ii", &beg, &len);
    str_len = RSTRING_LEN(str);
    break;
  case 1:
    a1 = mrb_get_arg1(mrb);
    str_len = RSTRING_LEN(str);
    if (mrb_range_p(a1)) {
      if (mrb_range_beg_len(mrb, a1, &beg, &len, str_len, TRUE) != MRB_RANGE_OK) {
        return mrb_nil_value();
      }
    }
    else {
      beg = mrb_as_int(mrb, a1);
      len = 1;
      empty = FALSE;
    }
    break;
  default:
    mrb_argnum_error(mrb, len, 1, 2);
    break;
  }
  if (mrb_str_beg_len(str_len, &beg, &len) && (empty || len != 0)) {
    return mrb_str_byte_subseq(mrb, str, beg, len);
  }
  else {
    return mrb_nil_value();
  }
}

static mrb_value
sub_replace(mrb_state *mrb, mrb_value self)
{
  char *p, *match;
  mrb_int plen, mlen;
  mrb_int found, offset;
  mrb_value result;

  mrb_get_args(mrb, "ssi", &p, &plen, &match, &mlen, &found);
  if (found < 0 || RSTRING_LEN(self) < found) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "argument out of range");
  }
  result = mrb_str_new(mrb, 0, 0);
  for (mrb_int i=0; i<plen; i++) {
    if (p[i] != '\\' || i+1==plen) {
      mrb_str_cat(mrb, result, p+i, 1);
      continue;
    }
    i++;
    switch (p[i]) {
    case '\\':
      mrb_str_cat(mrb, result, "\\", 1);
      break;
    case '`':
      mrb_str_cat(mrb, result, RSTRING_PTR(self), found);
      break;
    case '&': case '0':
      mrb_str_cat(mrb, result, match, mlen);
      break;
    case '\'':
      offset = found + mlen;
      if (RSTRING_LEN(self) > offset) {
        mrb_str_cat(mrb, result, RSTRING_PTR(self)+offset, RSTRING_LEN(self)-offset);
      }
      break;
    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9':
      /* ignore sub-group match (no Regexp supported) */
      break;
    default:
      mrb_str_cat(mrb, result, &p[i-1], 2);
      break;
    }
  }
  return result;
}


static mrb_value
str_bytesplice(mrb_state *mrb, mrb_value str, mrb_int idx1, mrb_int len1, mrb_value replace, mrb_int idx2, mrb_int len2)
{
  struct RString *s = RSTRING(str);
  if (idx1 < 0) {
    idx1 += RSTR_LEN(s);
  }
  if (idx2 < 0) {
    idx2 += RSTRING_LEN(replace);
  }
  if (RSTR_LEN(s) < idx1 || idx1 < 0 || RSTRING_LEN(replace) < idx2 || idx2 < 0) {
    mrb_raise(mrb, E_INDEX_ERROR, "index out of string");
  }
  if (len1 < 0 || len2 < 0) {
    mrb_raise(mrb, E_INDEX_ERROR, "negative length");
  }
  mrb_int n;
  if (mrb_int_add_overflow(idx1, len1, &n) || RSTR_LEN(s) < n) {
    len1 = RSTR_LEN(s) - idx1;
  }
  if (mrb_int_add_overflow(idx2, len2, &n) || RSTRING_LEN(replace) < n) {
    len2 = RSTRING_LEN(replace) - idx2;
  }
  mrb_str_modify(mrb, s);
  if (len1 >= len2) {
    memmove(RSTR_PTR(s)+idx1, RSTRING_PTR(replace)+idx2, len2);
    if (len1 > len2) {
      memmove(RSTR_PTR(s)+idx1+len2, RSTR_PTR(s)+idx1+len1, RSTR_LEN(s)-(idx1+len1));
      RSTR_SET_LEN(s, RSTR_LEN(s)-(len1-len2));
    }
  }
  else { /* len1 < len2 */
    mrb_int slen = RSTR_LEN(s);
    mrb_str_resize(mrb, str, slen+len2-len1);
    memmove(RSTR_PTR(s)+idx1+len2, RSTR_PTR(s)+idx1+len1, slen-(idx1+len1));
    memmove(RSTR_PTR(s)+idx1, RSTRING_PTR(replace)+idx2, len2);
  }
  return str;
}

/*
 *  call-seq:
 *    bytesplice(index, length, str) -> string
 *    bytesplice(index, length, str, str_index, str_length) -> string
 *    bytesplice(range, str) -> string
 *    bytesplice(range, str, str_range) -> string
 *
 *  Replaces some or all of the content of +self+ with +str+, and returns +self+.
 *  The portion of the string affected is determined using
 *  the same criteria as String#byteslice, except that +length+ cannot be omitted.
 *  If the replacement string is not the same length as the text it is replacing,
 *  the string will be adjusted accordingly.
 *
 *  If +str_index+ and +str_length+, or +str_range+ are given, the content of +self+ is replaced by str.byteslice(str_index, str_length) or str.byteslice(str_range); however the substring of +str+ is not allocated as a new string.
 *
 *  The form that take an Integer will raise an IndexError if the value is out
 *  of range; the Range form will raise a RangeError.
 *  If the beginning or ending offset does not land on character (codepoint)
 *  boundary, an IndexError will be raised.
 */
static mrb_value
mrb_str_bytesplice(mrb_state *mrb, mrb_value str)
{
  mrb_int idx1, len1, idx2, len2;
  mrb_value range1, range2, replace;
  switch (mrb_get_argc(mrb)) {
  case 3:
    mrb_get_args(mrb, "ooo", &range1, &replace, &range2);
    if (mrb_integer_p(range1)) {
      mrb_get_args(mrb, "iiS", &idx1, &len1, &replace);
      return str_bytesplice(mrb, str, idx1, len1, replace, 0, RSTRING_LEN(replace));
    }
    mrb_ensure_string_type(mrb, replace);
    if (mrb_range_beg_len(mrb, range1, &idx1, &len1, RSTRING_LEN(str), FALSE) != MRB_RANGE_OK) break;
    if (mrb_range_beg_len(mrb, range2, &idx2, &len2, RSTRING_LEN(replace), FALSE) != MRB_RANGE_OK) break;
    return str_bytesplice(mrb, str, idx1, len1, replace, idx2, len2);
  case 5:
    mrb_get_args(mrb, "iiSii", &idx1, &len1, &replace, &idx2, &len2);
    return str_bytesplice(mrb, str, idx1, len1, replace, idx2, len2);
  case 2:
    mrb_get_args(mrb, "oS", &range1, &replace);
    if (mrb_range_beg_len(mrb, range1, &idx1, &len1, RSTRING_LEN(str), FALSE) == MRB_RANGE_OK) {
      return str_bytesplice(mrb, str, idx1, len1, replace, 0, RSTRING_LEN(replace));
    }
  default:
    break;
  }
  mrb_raise(mrb, E_ARGUMENT_ERROR, "wrong number of arumgnts");
}

static mrb_value
mrb_encoding(mrb_state *mrb, mrb_value self)
{
  mrb_get_args(mrb, "");
#ifdef MRB_UTF8_STRING
  return mrb_str_new_lit(mrb, "UTF-8");
#else
  return mrb_str_new_lit(mrb, "ASCII-8BIT");
#endif
}

/* ---------------------------*/
void
mrb_init_string(mrb_state *mrb)
{
  struct RClass *s;

  mrb_static_assert(RSTRING_EMBED_LEN_MAX < (1 << MRB_STR_EMBED_LEN_BITS),
                    "pointer size too big for embedded string");

  mrb->string_class = s = mrb_define_class_id(mrb, MRB_SYM(String), mrb->object_class);             /* 15.2.10 */
  MRB_SET_INSTANCE_TT(s, MRB_TT_STRING);

  mrb_define_method_id(mrb, s, MRB_SYM(bytesize),        mrb_str_bytesize,        MRB_ARGS_NONE());

  mrb_define_method_id(mrb, s, MRB_OPSYM(cmp),           mrb_str_cmp_m,           MRB_ARGS_REQ(1)); /* 15.2.10.5.1  */
  mrb_define_method_id(mrb, s, MRB_OPSYM(eq),            mrb_str_equal_m,         MRB_ARGS_REQ(1)); /* 15.2.10.5.2  */
  mrb_define_method_id(mrb, s, MRB_OPSYM(add),           mrb_str_plus_m,          MRB_ARGS_REQ(1)); /* 15.2.10.5.4  */
  mrb_define_method_id(mrb, s, MRB_OPSYM(mul),           mrb_str_times,           MRB_ARGS_REQ(1)); /* 15.2.10.5.5  */
  mrb_define_method_id(mrb, s, MRB_OPSYM(aref),          mrb_str_aref_m,          MRB_ARGS_ANY());  /* 15.2.10.5.6  */
  mrb_define_method_id(mrb, s, MRB_OPSYM(aset),          mrb_str_aset_m,          MRB_ARGS_ANY());
  mrb_define_method_id(mrb, s, MRB_SYM(capitalize),      mrb_str_capitalize,      MRB_ARGS_NONE()); /* 15.2.10.5.7  */
  mrb_define_method_id(mrb, s, MRB_SYM_B(capitalize),    mrb_str_capitalize_bang, MRB_ARGS_NONE()); /* 15.2.10.5.8  */
  mrb_define_method_id(mrb, s, MRB_SYM(chomp),           mrb_str_chomp,           MRB_ARGS_ANY());  /* 15.2.10.5.9  */
  mrb_define_method_id(mrb, s, MRB_SYM_B(chomp),         mrb_str_chomp_bang,      MRB_ARGS_ANY());  /* 15.2.10.5.10 */
  mrb_define_method_id(mrb, s, MRB_SYM(chop),            mrb_str_chop,            MRB_ARGS_NONE()); /* 15.2.10.5.11 */
  mrb_define_method_id(mrb, s, MRB_SYM_B(chop),          mrb_str_chop_bang,       MRB_ARGS_NONE()); /* 15.2.10.5.12 */
  mrb_define_method_id(mrb, s, MRB_SYM(downcase),        mrb_str_downcase,        MRB_ARGS_NONE()); /* 15.2.10.5.13 */
  mrb_define_method_id(mrb, s, MRB_SYM_B(downcase),      mrb_str_downcase_bang,   MRB_ARGS_NONE()); /* 15.2.10.5.14 */
  mrb_define_method_id(mrb, s, MRB_SYM_Q(empty),         mrb_str_empty_p,         MRB_ARGS_NONE()); /* 15.2.10.5.16 */
  mrb_define_method_id(mrb, s, MRB_SYM_Q(eql),           mrb_str_eql,             MRB_ARGS_REQ(1)); /* 15.2.10.5.17 */

  mrb_define_method_id(mrb, s, MRB_SYM(hash),            mrb_str_hash_m,          MRB_ARGS_NONE()); /* 15.2.10.5.20 */
  mrb_define_method_id(mrb, s, MRB_SYM_Q(include),       mrb_str_include,         MRB_ARGS_REQ(1)); /* 15.2.10.5.21 */
  mrb_define_method_id(mrb, s, MRB_SYM(index),           mrb_str_index_m,         MRB_ARGS_ARG(1,1));  /* 15.2.10.5.22 */
  mrb_define_method_id(mrb, s, MRB_SYM(initialize),      mrb_str_init,            MRB_ARGS_REQ(1)); /* 15.2.10.5.23 */
  mrb_define_method_id(mrb, s, MRB_SYM(initialize_copy), mrb_str_replace,         MRB_ARGS_REQ(1)); /* 15.2.10.5.24 */
  mrb_define_method_id(mrb, s, MRB_SYM(intern),          mrb_str_intern,          MRB_ARGS_NONE()); /* 15.2.10.5.25 */
  mrb_define_method_id(mrb, s, MRB_SYM(length),          mrb_str_size,            MRB_ARGS_NONE()); /* 15.2.10.5.26 */
  mrb_define_method_id(mrb, s, MRB_SYM(replace),         mrb_str_replace,         MRB_ARGS_REQ(1)); /* 15.2.10.5.28 */
  mrb_define_method_id(mrb, s, MRB_SYM(reverse),         mrb_str_reverse,         MRB_ARGS_NONE()); /* 15.2.10.5.29 */
  mrb_define_method_id(mrb, s, MRB_SYM_B(reverse),       mrb_str_reverse_bang,    MRB_ARGS_NONE()); /* 15.2.10.5.30 */
  mrb_define_method_id(mrb, s, MRB_SYM(rindex),          mrb_str_rindex_m,        MRB_ARGS_ANY());  /* 15.2.10.5.31 */
  mrb_define_method_id(mrb, s, MRB_SYM(size),            mrb_str_size,            MRB_ARGS_NONE()); /* 15.2.10.5.33 */
  mrb_define_method_id(mrb, s, MRB_SYM(slice),           mrb_str_aref_m,          MRB_ARGS_ANY());  /* 15.2.10.5.34 */
  mrb_define_method_id(mrb, s, MRB_SYM(split),           mrb_str_split_m,         MRB_ARGS_ANY());  /* 15.2.10.5.35 */

#ifndef MRB_NO_FLOAT
  mrb_define_method_id(mrb, s, MRB_SYM(to_f),            mrb_str_to_f,            MRB_ARGS_NONE()); /* 15.2.10.5.38 */
#endif
  mrb_define_method_id(mrb, s, MRB_SYM(to_i),            mrb_str_to_i,            MRB_ARGS_ANY());  /* 15.2.10.5.39 */
  mrb_define_method_id(mrb, s, MRB_SYM(to_s),            mrb_str_to_s,            MRB_ARGS_NONE()); /* 15.2.10.5.40 */
  mrb_define_method_id(mrb, s, MRB_SYM(to_str),          mrb_str_to_s,            MRB_ARGS_NONE());
  mrb_define_method_id(mrb, s, MRB_SYM(to_sym),          mrb_str_intern,          MRB_ARGS_NONE()); /* 15.2.10.5.41 */
  mrb_define_method_id(mrb, s, MRB_SYM(upcase),          mrb_str_upcase,          MRB_ARGS_NONE()); /* 15.2.10.5.42 */
  mrb_define_method_id(mrb, s, MRB_SYM_B(upcase),        mrb_str_upcase_bang,     MRB_ARGS_NONE()); /* 15.2.10.5.43 */
  mrb_define_method_id(mrb, s, MRB_SYM(inspect),         mrb_str_inspect,         MRB_ARGS_NONE()); /* 15.2.10.5.46(x) */
  mrb_define_method_id(mrb, s, MRB_SYM(bytes),           mrb_str_bytes,           MRB_ARGS_NONE());

  mrb_define_method_id(mrb, s, MRB_SYM(getbyte),         mrb_str_getbyte,         MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, s, MRB_SYM(setbyte),         mrb_str_setbyte,         MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, s, MRB_SYM(byteindex),       mrb_str_byteindex_m,     MRB_ARGS_ARG(1,1));
  mrb_define_method_id(mrb, s, MRB_SYM(byterindex),      mrb_str_byterindex_m,    MRB_ARGS_ARG(1,1));
  mrb_define_method_id(mrb, s, MRB_SYM(byteslice),       mrb_str_byteslice,       MRB_ARGS_ARG(1,1));
  mrb_define_method_id(mrb, s, MRB_SYM(bytesplice),      mrb_str_bytesplice,      MRB_ARGS_ANY());

  mrb_define_method_id(mrb, s, MRB_SYM(__sub_replace),   sub_replace,             MRB_ARGS_REQ(3)); /* internal */

  mrb_define_method_id(mrb, mrb->kernel_module, MRB_SYM(__ENCODING__), mrb_encoding, MRB_ARGS_NONE());
}
