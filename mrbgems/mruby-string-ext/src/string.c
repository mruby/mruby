#include <ctype.h>
#include <string.h>
#include "mruby.h"
#include "mruby/string.h"

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
 *     str.swapcase!   -> str or nil
 *
 *  Equivalent to <code>String#swapcase</code>, but modifies the receiver in
 *  place, returning <i>str</i>, or <code>nil</code> if no changes were made.
 *  Note: case conversion is effective only in ASCII region.
 */
static mrb_value
mrb_str_swapcase_bang(mrb_state *mrb, mrb_value str)
{
  char *p, *pend;
  int modify = 0;
  struct RString *s = mrb_str_ptr(str);

  mrb_str_modify(mrb, s);
  p = RSTRING_PTR(str);
  pend = p + RSTRING_LEN(str);
  while (p < pend) {
    if (ISUPPER(*p)) {
      *p = TOLOWER(*p);
      modify = 1;
    }
    else if (ISLOWER(*p)) {
      *p = TOUPPER(*p);
      modify = 1;
    }
    p++;
  }

  if (modify) return str;
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     str.swapcase   -> new_str
 *
 *  Returns a copy of <i>str</i> with uppercase alphabetic characters converted
 *  to lowercase and lowercase characters converted to uppercase.
 *  Note: case conversion is effective only in ASCII region.
 *
 *     "Hello".swapcase          #=> "hELLO"
 *     "cYbEr_PuNk11".swapcase   #=> "CyBeR_pUnK11"
 */
static mrb_value
mrb_str_swapcase(mrb_state *mrb, mrb_value self)
{
  mrb_value str;

  str = mrb_str_dup(mrb, self);
  mrb_str_swapcase_bang(mrb, str);
  return str;
}

/*
 *  call-seq:
 *     str << integer       -> str
 *     str.concat(integer)  -> str
 *     str << obj           -> str
 *     str.concat(obj)      -> str
 *
 *  Append---Concatenates the given object to <i>str</i>. If the object is a
 *  <code>Integer</code>, it is considered as a codepoint, and is converted
 *  to a character before concatenation.
 *
 *     a = "hello "
 *     a << "world"   #=> "hello world"
 *     a.concat(33)   #=> "hello world!"
 */
static mrb_value
mrb_str_concat2(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  mrb_str_concat(mrb, self, str);
  return self;
}

/*
 *  call-seq:
 *     str.start_with?([prefixes]+)   -> true or false
 *
 *  Returns true if +str+ starts with one of the +prefixes+ given.
 *
 *    "hello".start_with?("hell")               #=> true
 *
 *    # returns true if one of the prefixes matches.
 *    "hello".start_with?("heaven", "hell")     #=> true
 *    "hello".start_with?("heaven", "paradise") #=> false
 *    "h".start_with?("heaven", "hell")         #=> false
 */
static mrb_value
mrb_str_start_with(mrb_state *mrb, mrb_value self)
{
  mrb_value *argv, sub;
  mrb_int argc, i;
  mrb_get_args(mrb, "*", &argv, &argc);

  for (i = 0; i < argc; i++) {
    size_t len_l, len_r;
    int ai = mrb_gc_arena_save(mrb);
    sub = mrb_string_type(mrb, argv[i]);
    mrb_gc_arena_restore(mrb, ai);
    len_l = RSTRING_LEN(self);
    len_r = RSTRING_LEN(sub);
    if (len_l >= len_r) {
      if (memcmp(RSTRING_PTR(self), RSTRING_PTR(sub), len_r) == 0) {
        return mrb_true_value();
      }
    }
  }
  return mrb_false_value();
}

/*
 *  call-seq:
 *     str.end_with?([suffixes]+)   -> true or false
 *
 *  Returns true if +str+ ends with one of the +suffixes+ given.
 */
static mrb_value
mrb_str_end_with(mrb_state *mrb, mrb_value self)
{
  mrb_value *argv, sub;
  mrb_int argc, i;
  mrb_get_args(mrb, "*", &argv, &argc);

  for (i = 0; i < argc; i++) {
    size_t len_l, len_r;
    int ai = mrb_gc_arena_save(mrb);
    sub = mrb_string_type(mrb, argv[i]);
    mrb_gc_arena_restore(mrb, ai);
    len_l = RSTRING_LEN(self);
    len_r = RSTRING_LEN(sub);
    if (len_l >= len_r) {
      if (memcmp(RSTRING_PTR(self) + (len_l - len_r),
                 RSTRING_PTR(sub),
                 len_r) == 0) {
        return mrb_true_value();
      }
    }
  }
  return mrb_false_value();
}

static mrb_value
mrb_str_hex(mrb_state *mrb, mrb_value self)
{
  return mrb_str_to_inum(mrb, self, 16, FALSE);
}

static mrb_value
mrb_str_oct(mrb_state *mrb, mrb_value self)
{
  return mrb_str_to_inum(mrb, self, 8, FALSE);
}

void
mrb_mruby_string_ext_gem_init(mrb_state* mrb)
{
  struct RClass * s = mrb->string_class;

  mrb_define_method(mrb, s, "dump",            mrb_str_dump,            MRB_ARGS_NONE());
  mrb_define_method(mrb, s, "getbyte",         mrb_str_getbyte,         MRB_ARGS_REQ(1));
  mrb_define_method(mrb, s, "swapcase!",       mrb_str_swapcase_bang,   MRB_ARGS_NONE());
  mrb_define_method(mrb, s, "swapcase",        mrb_str_swapcase,        MRB_ARGS_NONE());
  mrb_define_method(mrb, s, "concat",          mrb_str_concat2,         MRB_ARGS_REQ(1));
  mrb_define_method(mrb, s, "<<",              mrb_str_concat2,         MRB_ARGS_REQ(1));
  mrb_define_method(mrb, s, "start_with?",     mrb_str_start_with,      MRB_ARGS_REST());
  mrb_define_method(mrb, s, "end_with?",       mrb_str_end_with,        MRB_ARGS_REST());
  mrb_define_method(mrb, s, "hex",             mrb_str_hex,             MRB_ARGS_NONE());
  mrb_define_method(mrb, s, "oct",             mrb_str_oct,             MRB_ARGS_NONE());
}

void
mrb_mruby_string_ext_gem_final(mrb_state* mrb)
{
}
