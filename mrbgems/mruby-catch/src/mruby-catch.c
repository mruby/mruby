#include <mruby.h>
#include <mruby/class.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/proc.h>
#include <mruby/presym.h>

#define ID_PRESERVED_CATCH MRB_SYM(__preserved_catch_proc)

static const mrb_callinfo *
find_catcher(mrb_state *mrb, mrb_value tag)
{
  mrb_value pval = mrb_obj_iv_get(mrb, (struct RObject *)mrb->kernel_module, ID_PRESERVED_CATCH);
  mrb_assert(mrb_proc_p(pval));
  const struct RProc *proc = mrb_proc_ptr(pval);

  const mrb_callinfo *ci = mrb->c->ci;
  size_t n = ci - mrb->c->cibase;
  ci--;
  for (; n > 0; n--, ci--) {
    const mrb_value *arg1 = ci->stack + 1;
    if (ci->proc == proc && mrb_obj_eq(mrb, *arg1, tag)) {
      return ci;
    }
  }

  return NULL;
}

static mrb_value
mrb_f_throw(mrb_state *mrb, mrb_value self)
{
  mrb_value tag, obj;
  mrb_get_args(mrb, "oo", &tag, &obj);

  const mrb_callinfo *ci = find_catcher(mrb, tag);
  if (ci) {
    struct RBreak *b = (struct RBreak *)mrb_obj_alloc(mrb, MRB_TT_BREAK, NULL);
    mrb_break_value_set(b, obj);
    mrb_break_proc_set(b, ci[2].proc); /* Back to the closure in `catch` method */
    mrb_exc_raise(mrb, mrb_obj_value(b));
  }

  return mrb_nil_value();
}

static mrb_value
mrb_s_preserve_catch(mrb_state *mrb, mrb_value self)
{
  mrb_method_t m = mrb_method_search(mrb, mrb->kernel_module, MRB_SYM(catch));
  mrb_assert(!MRB_METHOD_UNDEF_P(m));
  mrb_assert(!MRB_METHOD_CFUNC_P(m));
  mrb_obj_iv_set(mrb, (struct RObject *)mrb->kernel_module, ID_PRESERVED_CATCH, mrb_obj_value(MRB_METHOD_PROC(m)));

  mrb_remove_method(mrb, mrb_class(mrb, mrb_obj_value(mrb->kernel_module)), MRB_SYM(__preserve_catch_method));

  return mrb_nil_value();
}

void
mrb_mruby_catch_gem_init(mrb_state *mrb)
{
  mrb_define_method(mrb, mrb->kernel_module, "__throw", mrb_f_throw, MRB_ARGS_REQ(2));
  mrb_define_class_method(mrb, mrb->kernel_module, "__preserve_catch_method", mrb_s_preserve_catch, MRB_ARGS_NONE());
}

void
mrb_mruby_catch_gem_final(mrb_state *mrb)
{
}
