#include "mruby.h"
#include "mruby/proc.h"
#include "mruby/array.h"
#include "mruby/string.h"
#include "mruby/debug.h"

static mrb_value
mrb_proc_lambda(mrb_state *mrb, mrb_value self)
{
  struct RProc *p = mrb_proc_ptr(self);
  return mrb_bool_value(MRB_PROC_STRICT_P(p));
}

static mrb_value
mrb_proc_source_location(mrb_state *mrb, mrb_value self)
{
  struct RProc *p = mrb_proc_ptr(self);

  if (MRB_PROC_CFUNC_P(p)) {
    return mrb_nil_value();
  }
  else {
    mrb_irep *irep = p->body.irep;
    int32_t line;
    const char *filename;

    filename = mrb_debug_get_filename(irep, 0);
    line = mrb_debug_get_line(irep, 0);

    return (!filename && line == -1)? mrb_nil_value()
        : mrb_assoc_new(mrb, mrb_str_new_cstr(mrb, filename), mrb_fixnum_value(line));
  }
}

static mrb_value
mrb_proc_inspect(mrb_state *mrb, mrb_value self)
{
  struct RProc *p = mrb_proc_ptr(self);
  mrb_value str = mrb_str_new_lit(mrb, "#<Proc:");
  mrb_str_concat(mrb, str, mrb_ptr_to_str(mrb, mrb_cptr(self)));

  if (!MRB_PROC_CFUNC_P(p)) {
    mrb_irep *irep = p->body.irep;
    mrb_str_cat_lit(mrb, str, "@");

    if (irep->filename) {
      mrb_str_cat_cstr(mrb, str, irep->filename);
    }
    else {
      mrb_str_cat_lit(mrb, str, "-");
    }
    mrb_str_cat_lit(mrb, str, ":");

    if (irep->lines) {
      mrb_str_append(mrb, str, mrb_fixnum_value(*irep->lines));
    }
    else {
      mrb_str_cat_lit(mrb, str, "-");
    }
  }

  if (MRB_PROC_STRICT_P(p)) {
    mrb_str_cat_lit(mrb, str, " (lambda)");
  }

  mrb_str_cat_lit(mrb, str, ">");
  return str;
}

static mrb_value
mrb_kernel_proc(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;

  mrb_get_args(mrb, "&", &blk);
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "tried to create Proc object without a block");
  }

  return blk;
}

void
mrb_mruby_proc_ext_gem_init(mrb_state* mrb)
{
  struct RClass *p = mrb->proc_class;
  mrb_define_method_lit(mrb, p, "lambda?",         mrb_proc_lambda,          MRB_ARGS_NONE());
  mrb_define_method_lit(mrb, p, "source_location", mrb_proc_source_location, MRB_ARGS_NONE());
  mrb_define_method_lit(mrb, p, "to_s",            mrb_proc_inspect,         MRB_ARGS_NONE());
  mrb_define_method_lit(mrb, p, "inspect",         mrb_proc_inspect,         MRB_ARGS_NONE());

  mrb_define_class_method(mrb, mrb->kernel_module, "proc", mrb_kernel_proc, MRB_ARGS_NONE());
  mrb_define_method_lit(mrb, mrb->kernel_module,       "proc", mrb_kernel_proc, MRB_ARGS_NONE());
}

void
mrb_mruby_proc_ext_gem_final(mrb_state* mrb)
{
}
