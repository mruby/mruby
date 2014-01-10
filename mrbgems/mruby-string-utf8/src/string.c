#include "mruby.h"
#include "mruby/string.h"
#include "mruby/range.h"
#include <ctype.h>
#include <string.h>

/* TODO: duplicate definition in src/re.h */
#define REGEXP_CLASS "Regexp"

static size_t utf8len_tab[256] =
{
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1,
};

static size_t
utf8len(unsigned char* p)
{
  size_t    len;
  int    i;

  if (*p == 0)
    return 1;
  len = utf8len_tab[*p];
  for (i = 1; i < len; ++i)
    if ((p[i] & 0xc0) != 0x80)
      return 1;
  return len;
}

static size_t
mrb_utf8_strlen(mrb_value str)
{
  size_t total = 0;
  unsigned char* p = (unsigned char*) RSTRING_PTR(str);
  unsigned char* e = p + RSTRING_LEN(str);
  while (p<e) {
    p += utf8len(p);
    total++;
  }
  return total;
}

static mrb_value
mrb_str_size(mrb_state *mrb, mrb_value str)
{
  size_t size = mrb_utf8_strlen(str);

  return mrb_fixnum_value(size);
}

#define RSTRING_LEN_UTF8(s) mrb_utf8_strlen(s)

static mrb_value
noregexp(mrb_state *mrb, mrb_value self)
{
  mrb_raise(mrb, E_NOTIMP_ERROR, "Regexp class not implemented");
  return mrb_nil_value();
}

static void
regexp_check(mrb_state *mrb, mrb_value obj)
{
  if (!memcmp(mrb_obj_classname(mrb, obj), REGEXP_CLASS, sizeof(REGEXP_CLASS) - 1)) {
    noregexp(mrb, obj);
  }
}

static inline mrb_int
mrb_memsearch_qs(const unsigned char *xs, mrb_int m, const unsigned char *ys, mrb_int n)
{
  const unsigned char *x = xs, *xe = xs + m;
  const unsigned char *y = ys;
  int i, qstable[256];

  /* Preprocessing */
  for (i = 0; i < 256; ++i)
    qstable[i] = m + 1;
  for (; x < xe; ++x)
    qstable[*x] = xe - x;
 /* Searching */
  for (; y + m <= ys + n; y += *(qstable + y[m])) {
    if (*xs == *y && memcmp(xs, y, m) == 0)
        return y - ys;
  }
  return -1;
}
static mrb_int
mrb_memsearch(const void *x0, mrb_int m, const void *y0, mrb_int n)
{
  const unsigned char *x = (const unsigned char *)x0, *y = (const unsigned char *)y0;

  if (m > n) return -1;
  else if (m == n) {
    return memcmp(x0, y0, m) == 0 ? 0 : -1;
  }
  else if (m < 1) {
    return 0;
  }
  else if (m == 1) {
    const unsigned char *ys = y, *ye = ys + n;
    for (; y < ye; ++y) {
      if (*x == *y)
        return y - ys;
    }
    return -1;
  }
  return mrb_memsearch_qs((const unsigned char *)x0, m, (const unsigned char *)y0, n);
}

static mrb_value
str_subseq(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len)
{
  int i;
  unsigned char *p = (unsigned char*) RSTRING_PTR(str), *t;
  unsigned char *e = p + RSTRING_LEN(str);
  

  for (i = 0; i < beg && p<e; i++) {
    p += utf8len(p);
  }
  t = p;
  for (i = 0; i < len && t<e; i++) {
    t += utf8len(t);
  }
  return mrb_str_new(mrb, (const char*)p, (int)(t - p));
}

static mrb_value
str_substr(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len)
{
  mrb_value str2;
  int len8 = RSTRING_LEN_UTF8(str);

  if (len < 0) return mrb_nil_value();
  if (len8 == 0) {
    len = 0;
  }
  else if (beg < 0) {
    beg = len8 + beg;
  }
  if (beg > len8) return mrb_nil_value();
  if (beg < 0) {
    beg += len8;
    if (beg < 0) return mrb_nil_value();
  }
  if (beg + len > len8)
    len = len8 - beg;
  if (len <= 0) {
    len = 0;
  }
  str2 = str_subseq(mrb, str, beg, len);

  return str2;
}

static mrb_int
str_index(mrb_state *mrb, mrb_value str, mrb_value sub, mrb_int offset)
{
  mrb_int pos;
  char *s, *sptr;
  mrb_int len, slen;

  len = RSTRING_LEN(str);
  slen = RSTRING_LEN(sub);
  if (offset < 0) {
    offset += len;
    if (offset < 0) return -1;
  }
  if (len - offset < slen) return -1;
  s = RSTRING_PTR(str);
  if (offset) {
    s += offset;
  }
  if (slen == 0) return offset;
  /* need proceed one character at a time */
  sptr = RSTRING_PTR(sub);
  slen = RSTRING_LEN(sub);
  len = RSTRING_LEN(str) - offset;
  pos = mrb_memsearch(sptr, slen, s, len);
  if (pos < 0) return pos;
  return pos + offset;
}

static mrb_value
mrb_str_aref(mrb_state *mrb, mrb_value str, mrb_value indx)
{
  mrb_int idx;

  regexp_check(mrb, indx);
  switch (mrb_type(indx)) {
    case MRB_TT_FIXNUM:
      idx = mrb_fixnum(indx);

num_index:
      str = str_substr(mrb, str, idx, 1);
      if (!mrb_nil_p(str) && RSTRING_LEN(str) == 0) return mrb_nil_value();
      return str;

    case MRB_TT_STRING:
      if (str_index(mrb, str, indx, 0) != -1)
        return mrb_str_dup(mrb, indx);
      return mrb_nil_value();

    case MRB_TT_RANGE:
      /* check if indx is Range */
      {
        mrb_int beg, len;
        mrb_value tmp;

        len = RSTRING_LEN_UTF8(str);
        if (mrb_range_beg_len(mrb, indx, &beg, &len, len)) {
          tmp = str_subseq(mrb, str, beg, len);
          return tmp;
        }
        else {
          return mrb_nil_value();
        }
      }
    default:
      idx = mrb_fixnum(indx);
      goto num_index;
    }
    return mrb_nil_value();    /* not reached */
}

static mrb_value
mrb_str_aref_m(mrb_state *mrb, mrb_value str)
{
  mrb_value a1, a2;
  int argc;

  argc = mrb_get_args(mrb, "o|o", &a1, &a2);
  if (argc == 2) {
    regexp_check(mrb, a1);
    return str_substr(mrb, str, mrb_fixnum(a1), mrb_fixnum(a2));
  }
  if (argc != 1) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (%S for 1)", mrb_fixnum_value(argc));
  }
  return mrb_str_aref(mrb, str, a1);
}

static mrb_value
mrb_str_reverse_bang(mrb_state *mrb, mrb_value str)
{
  int utf8_len = mrb_utf8_strlen(str);
  if (utf8_len > 1) {
    int len = RSTRING_LEN(str);
    char *buf = (char *)mrb_malloc(mrb, len);
    unsigned char* p = (unsigned char*)buf;
    unsigned char* e = (unsigned char*)buf + len;
    unsigned char* r = (unsigned char*)RSTRING_END(str);
    
    memcpy(buf, RSTRING_PTR(str), len);
    mrb_str_modify(mrb, mrb_str_ptr(str));
    
    while (p<e) {
      int clen = utf8len(p);
      r -= clen;
      memcpy(r, p, clen);
      p += clen;
    }
    mrb_free(mrb, buf);
  }
  
  return str;
}

static mrb_value
mrb_str_reverse(mrb_state *mrb, mrb_value str)
{
  return mrb_str_reverse_bang(mrb, mrb_str_dup(mrb, str));
}

void
mrb_mruby_string_utf8_gem_init(mrb_state* mrb)
{
  struct RClass * s = mrb->string_class;

  mrb_define_method(mrb, s, "size", mrb_str_size, MRB_ARGS_NONE());
  mrb_define_method(mrb, s, "length", mrb_str_size, MRB_ARGS_NONE());
  mrb_define_method(mrb, s, "[]", mrb_str_aref_m, MRB_ARGS_ANY());
  mrb_define_method(mrb, s, "slice", mrb_str_aref_m, MRB_ARGS_ANY());
  mrb_define_method(mrb, s, "reverse",  mrb_str_reverse,      MRB_ARGS_NONE());
  mrb_define_method(mrb, s, "reverse!", mrb_str_reverse_bang, MRB_ARGS_NONE());
}

void
mrb_mruby_string_utf8_gem_final(mrb_state* mrb)
{
}
