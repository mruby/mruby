#include "mruby.h"
#include "mruby/error.h"
#include "mruby/object.h"
#include "mruby/array.h"

/*
 *  call-seq:
 *     __method__         -> symbol
 *
 *  Returns the name at the definition of the current method as a
 *  Symbol.
 *  If called outside of a method, it returns <code>nil</code>.
 *
 */
static mrb_value
mrb_f_method(mrb_state *mrb, mrb_value self)
{
  mrb_callinfo *ci = mrb->c->ci;
  ci--;
  if (ci->mid)
    return mrb_symbol_value(ci->mid);
  else
    return mrb_nil_value();
}

/*
 *  call-seq:
 *     Array(arg)    -> array
 *
 *  Returns +arg+ as an Array.
 *
 *  First tries to call Array#to_ary on +arg+, then Array#to_a.
 *
 *     Array(1..5)   #=> [1, 2, 3, 4, 5]
 *
 */
static mrb_value
mrb_f_array(mrb_state *mrb, mrb_value self)
{
  mrb_value arg, tmp;

  mrb_get_args(mrb, "o", &arg);
  tmp = mrb_check_convert_type(mrb, arg, MRB_TT_ARRAY, "Array", "to_ary");
  if (mrb_nil_p(tmp)) {
    tmp = mrb_check_convert_type(mrb, arg, MRB_TT_ARRAY, "Array", "to_a");
  }
  if (mrb_nil_p(tmp)) {
    tmp = mrb_ary_new_from_values(mrb, 1, &arg);
  }

  return tmp;
}

void
mrb_mruby_kernel_ext_gem_init(mrb_state *mrb)
{
  struct RClass *krn = mrb->kernel_module;

  mrb_define_module_function(mrb, krn, "fail", mrb_f_raise, MRB_ARGS_OPT(2));
  mrb_define_method(mrb, krn, "__method__", mrb_f_method, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, krn, "Array", mrb_f_array, MRB_ARGS_REQ(1));
}

void
mrb_mruby_kernel_ext_gem_final(mrb_state *mrb)
{
}
