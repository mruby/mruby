#include "mruby.h"
#include "mruby/compile.h"

static struct RProc*
create_proc_from_string(mrb_state *mrb, char *s, int len, mrb_value binding, char *file, mrb_int line)
{
  mrbc_context *cxt;
  struct mrb_parser_state *p;
  struct RProc *proc;

  if (!mrb_nil_p(binding)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Binding of eval must be nil.");
  }

  cxt = mrbc_context_new(mrb);
  cxt->lineno = line;
  if (file) {
    mrbc_filename(mrb, cxt, file);
  }

  p = mrb_parser_new(mrb);
  p->s = s;
  p->send = s + len;
  mrb_parser_parse(p, cxt);

  if (0 < p->nerr) {
    /* parse error */
    char buf[256];
    int n;
    n = snprintf(buf, sizeof(buf), "line %d: %s\n", p->error_buffer[0].lineno, p->error_buffer[0].message);
    mrb_parser_free(p);
    mrbc_context_free(mrb, cxt);
    mrb_exc_raise(mrb, mrb_exc_new(mrb, E_SYNTAX_ERROR, buf, n));
  }

  proc = mrb_generate_code(mrb, p);

  mrb_parser_free(p);
  mrbc_context_free(mrb, cxt);

  if (proc == NULL) {
    /* codegen error */
    mrb_raise(mrb, E_SCRIPT_ERROR, "codegen error");
  }

  return proc;
}

static mrb_value
f_eval(mrb_state *mrb, mrb_value self)
{
  char *s;
  mrb_int len;
  mrb_value binding = mrb_nil_value();
  char *file = NULL;
  mrb_int line = 1;
  mrb_value ret;

  mrb_get_args(mrb, "s|ozi", &s, &len, &binding, &file, &line);

  ret = mrb_toplevel_run(mrb, create_proc_from_string(mrb, s, len, binding, file, line));
  if (mrb->exc) {
    mrb_exc_raise(mrb, mrb_obj_value(mrb->exc));
  }

  return ret;
}

void
mrb_mruby_eval_gem_init(mrb_state* mrb)
{
  mrb_define_class_method(mrb, mrb->kernel_module, "eval", f_eval, MRB_ARGS_REQ(1) | MRB_ARGS_OPT(3));
}

void
mrb_mruby_eval_gem_final(mrb_state* mrb)
{
}
