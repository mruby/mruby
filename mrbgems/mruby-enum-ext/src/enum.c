#include <mruby.h>
#include <mruby/array.h>
#include <mruby/error.h>

/* `Enumerable#minmax` reaches an element through a call to `each`, a block
   call and a `__svalue` send, then compares it with two `<=>` sends. An Array
   is walked in place instead, and `mrb_cmp()` answers for an Integer, a Float
   and a String without a send at all, as `Array#sort` already does. */
static mrb_int
ary_cmp_ordered(mrb_state *mrb, mrb_value a, mrb_value b)
{
  mrb_int cmp = mrb_cmp(mrb, a, b);
  if (cmp == -2) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "comparison of %T with %T failed", a, b);
  }
  return cmp;
}

/* `<=>` can run Ruby, which can grow the array, shrink it or drop an element
   held as an answer so far, so the length and the element are read afresh each
   time round and both answers are kept in the arena. */
static mrb_value
ary_minmax(mrb_state *mrb, mrb_value self)
{
  mrb_value pair = mrb_ary_new_capa(mrb, 2);

  if (RARRAY_LEN(self) == 0) {
    mrb_ary_push(mrb, pair, mrb_nil_value());
    mrb_ary_push(mrb, pair, mrb_nil_value());
    return pair;
  }

  mrb_value min = RARRAY_PTR(self)[0];
  mrb_value max = min;
  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 1; i < RARRAY_LEN(self); i++) {
    mrb_value val = RARRAY_PTR(self)[i];
    if (ary_cmp_ordered(mrb, val, max) > 0) max = val;
    if (ary_cmp_ordered(mrb, val, min) < 0) min = val;
    mrb_gc_arena_restore(mrb, ai);
    mrb_gc_protect(mrb, min);
    mrb_gc_protect(mrb, max);
  }
  mrb_ary_push(mrb, pair, min);
  mrb_ary_push(mrb, pair, max);
  return pair;
}

void
mrb_mruby_enum_ext_gem_init(mrb_state *mrb)
{
  mrb_define_method_id(mrb, mrb->array_class, MRB_SYM(__minmax), ary_minmax, MRB_ARGS_NONE());
}

void
mrb_mruby_enum_ext_gem_final(mrb_state *mrb)
{
}
