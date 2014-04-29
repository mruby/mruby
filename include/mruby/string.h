/*
** mruby/string.h - String class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_STRING_H
#define MRUBY_STRING_H

#if defined(__cplusplus)
extern "C" {
#endif

#define IS_EVSTR(p,e) ((p) < (e) && (*(p) == '$' || *(p) == '@' || *(p) == '{'))

extern const char mrb_digitmap[];

#define RSTRING_EMBED_LEN_MAX ((mrb_int)(sizeof(void*) * 3 - 1))

struct RString {
  MRB_OBJECT_HEADER;
  union {
    struct {
      mrb_int len;
      union {
        mrb_int capa;
        struct mrb_shared_string *shared;
      } aux;
      char *ptr;
    } heap;
    char ary[RSTRING_EMBED_LEN_MAX + 1];
  } as;
};

#define mrb_str_ptr(s)    ((struct RString*)(mrb_ptr(s)))
#define RSTRING(s)        ((struct RString*)(mrb_ptr(s)))
#define RSTRING_PTR(s)\
  ((RSTRING(s)->flags & MRB_STR_EMBED) ?\
   RSTRING(s)->as.ary :\
   RSTRING(s)->as.heap.ptr)
#define RSTRING_LEN(s)\
  ((RSTRING(s)->flags & MRB_STR_EMBED) ?\
  (mrb_int)((RSTRING(s)->flags & MRB_STR_EMBED_LEN_MASK) >> MRB_STR_EMBED_LEN_SHIFT) :\
   RSTRING(s)->as.heap.len)
#define RSTRING_CAPA(s)\
  ((RSTRING(s)->flags & MRB_STR_EMBED) ?\
   RSTRING_EMBED_LEN_MAX :\
   RSTRING(s)->as.heap.aux.capa)
#define RSTRING_END(s)    (RSTRING_PTR(s) + RSTRING_LEN(s))
mrb_int mrb_str_strlen(mrb_state*, struct RString*);

#define MRB_STR_SHARED    1
#define MRB_STR_NOFREE    2
#define MRB_STR_EMBED     4
#define MRB_STR_EMBED_LEN_MASK 0xf8
#define MRB_STR_EMBED_LEN_SHIFT 3

void mrb_gc_free_str(mrb_state*, struct RString*);
void mrb_str_modify(mrb_state*, struct RString*);
void mrb_str_concat(mrb_state*, mrb_value, mrb_value);
mrb_value mrb_str_plus(mrb_state*, mrb_value, mrb_value);
mrb_value mrb_ptr_to_str(mrb_state *, void*);
mrb_value mrb_obj_as_string(mrb_state *mrb, mrb_value obj);
mrb_value mrb_str_resize(mrb_state *mrb, mrb_value str, mrb_int len);
mrb_value mrb_str_substr(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len);
mrb_value mrb_string_type(mrb_state *mrb, mrb_value str);
mrb_value mrb_check_string_type(mrb_state *mrb, mrb_value str);
mrb_value mrb_str_buf_new(mrb_state *mrb, size_t capa);

char *mrb_string_value_cstr(mrb_state *mrb, mrb_value *ptr);
char *mrb_string_value_ptr(mrb_state *mrb, mrb_value ptr);
mrb_value mrb_str_dup(mrb_state *mrb, mrb_value str);
mrb_value mrb_str_intern(mrb_state *mrb, mrb_value self);
mrb_value mrb_str_to_inum(mrb_state *mrb, mrb_value str, mrb_int base, mrb_bool badcheck);
double mrb_str_to_dbl(mrb_state *mrb, mrb_value str, mrb_bool badcheck);
mrb_value mrb_str_to_str(mrb_state *mrb, mrb_value str);
mrb_int mrb_str_hash(mrb_state *mrb, mrb_value str);
mrb_value mrb_str_inspect(mrb_state *mrb, mrb_value str);
mrb_bool mrb_str_equal(mrb_state *mrb, mrb_value str1, mrb_value str2);
mrb_value mrb_str_dump(mrb_state *mrb, mrb_value str);
mrb_value mrb_str_cat(mrb_state *mrb, mrb_value str, const char *ptr, size_t len);
mrb_value mrb_str_cat_cstr(mrb_state *mrb, mrb_value str, const char *ptr);
mrb_value mrb_str_cat_str(mrb_state *mrb, mrb_value str, mrb_value str2);
#define mrb_str_cat_lit(mrb, str, lit) mrb_str_cat(mrb, str, lit, mrb_strlen_lit(lit))
mrb_value mrb_str_append(mrb_state *mrb, mrb_value str, mrb_value str2);

int mrb_str_cmp(mrb_state *mrb, mrb_value str1, mrb_value str2);
char *mrb_str_to_cstr(mrb_state *mrb, mrb_value str);
mrb_value mrb_str_pool(mrb_state *mrb, mrb_value str);

/* For backward compatibility */
static inline mrb_value
mrb_str_cat2(mrb_state *mrb, mrb_value str, const char *ptr) {
  return mrb_str_cat_cstr(mrb, str, ptr);
}

static inline mrb_value
mrb_str_buf_cat(mrb_state *mrb, mrb_value str, const char *ptr, size_t len)
{
  return mrb_str_cat(mrb, str, ptr, len);
}

static inline mrb_value
mrb_str_buf_append(mrb_state *mrb, mrb_value str, mrb_value str2)
{
  return mrb_str_cat_str(mrb, str, str2);
}

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_STRING_H */
