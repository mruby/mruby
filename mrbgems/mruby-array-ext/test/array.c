#include <mruby.h>
#include <mruby/array.h>

/* mrb_ary_unshift() from C, which Array#unshift does not reach: the method
   goes through the same code with a count, and only this entry passes one
   value. */
static mrb_value
ary_unshift_from_c(mrb_state *mrb, mrb_value self)
{
  mrb_value item;
  mrb_get_args(mrb, "o", &item);
  return mrb_ary_unshift(mrb, self, item);
}

void
mrb_mruby_array_ext_gem_test(mrb_state *mrb)
{
  mrb_define_method(mrb, mrb->array_class, "__unshift_from_c",
                    ary_unshift_from_c, MRB_ARGS_REQ(1));
}
