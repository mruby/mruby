#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/compile.h>
#include <mruby/error.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

/* provided by mruby-eval */
mrb_value mrb_f_eval(mrb_state *mrb, mrb_value self);
/* provided by mruby-binding-core */
const struct RProc *mrb_binding_extract_proc(mrb_state *mrb, mrb_value binding);
struct REnv *mrb_binding_extract_env(mrb_state *mrb, mrb_value binding);
/* provided by mruby-compiler */
typedef mrb_bool mrb_parser_foreach_top_variable_func(mrb_state *mrb, mrb_sym sym, void *user);
void mrb_parser_foreach_top_variable(mrb_state *mrb, struct mrb_parser_state *p, mrb_parser_foreach_top_variable_func *func, void *user);

static void
binding_eval_error_check(mrb_state *mrb, struct mrb_parser_state *p, const char *file)
{
  if (!p) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "Failed to create parser state (out of memory)");
  }

  if (0 < p->nerr) {
    mrb_value str;

    if (file) {
      str = mrb_format(mrb, "file %s line %d: %s",
                       file,
                       p->error_buffer[0].lineno,
                       p->error_buffer[0].message);
    }
    else {
      str = mrb_format(mrb, "line %d: %s",
                       p->error_buffer[0].lineno,
                       p->error_buffer[0].message);
    }
    mrb_exc_raise(mrb, mrb_exc_new_str(mrb, E_SYNTAX_ERROR, str));
  }
}

#define LV_BUFFERS 8

struct expand_lvspace {
  mrb_irep *irep;
  struct REnv *env;
  int numvar;
  mrb_sym syms[LV_BUFFERS];
};

static mrb_bool
expand_lvspace(mrb_state *mrb, mrb_sym sym, void *user)
{
  struct expand_lvspace *p = (struct expand_lvspace*)user;
  mrb_int symlen;
  const char *symname = mrb_sym_name_len(mrb, sym, &symlen);

  if (symname && symlen > 0) {
    if (symname[0] != '&' && symname[0] != '*') {
      p->syms[p->numvar++] = sym;
      if (p->numvar >= LV_BUFFERS) {
        mrb_proc_merge_lvar(mrb, p->irep, p->env, p->numvar, p->syms, NULL);
        p->numvar = 0;
      }
    }
  }

  return TRUE;
}

struct binding_eval_prepare_body {
  mrb_value binding;
  const char *file;
  const char *expr;
  mrb_int exprlen;
  mrbc_context *mrbc;
  struct mrb_parser_state *pstate;
};

static mrb_value
binding_eval_prepare_body(mrb_state *mrb, void *opaque)
{
  struct binding_eval_prepare_body *p = (struct binding_eval_prepare_body*)opaque;

  const struct RProc *proc = mrb_binding_extract_proc(mrb, p->binding);
  mrb_assert(!MRB_PROC_CFUNC_P(proc));

  p->mrbc = mrbc_context_new(mrb);
  mrbc_filename(mrb, p->mrbc, p->file ? p->file : "(eval)");
  p->mrbc->upper = proc;
  p->mrbc->capture_errors = TRUE;
  p->pstate = mrb_parse_nstring(mrb, p->expr, p->exprlen, p->mrbc);
  binding_eval_error_check(mrb, p->pstate, p->file);

  struct expand_lvspace args = {
    (mrb_irep*)proc->body.irep,
    mrb_binding_extract_env(mrb, p->binding),
    0,
    { 0 }
  };
  mrb_parser_foreach_top_variable(mrb, p->pstate, expand_lvspace, &args);
  if (args.numvar > 0) {
    mrb_proc_merge_lvar(mrb, args.irep, args.env, args.numvar, args.syms, NULL);
  }

  return mrb_nil_value();
}

static void
binding_eval_prepare(mrb_state *mrb, mrb_value binding)
{
  struct binding_eval_prepare_body d = { binding, NULL, NULL, 0, NULL, NULL };
  mrb_int argc;
  mrb_value *argv;
  mrb_get_args(mrb, "s|z*!", &d.expr, &d.exprlen, &d.file, &argv, &argc);

  /* `eval` should take (string[, file, line]) */
  if (argc > 3) mrb_argnum_error(mrb, argc, 1, 3);
  mrb_bool error;
  mrb_value ret = mrb_protect_error(mrb, binding_eval_prepare_body, &d, &error);
  if (d.pstate) mrb_parser_free(d.pstate);
  if (d.mrbc) mrbc_context_free(mrb, d.mrbc);
  if (error) mrb_exc_raise(mrb, ret);
}

static mrb_value
mrb_binding_eval(mrb_state *mrb, mrb_value binding)
{
  binding_eval_prepare(mrb, binding);

  mrb_callinfo *ci = mrb->c->ci;
  int argc = ci->n;
  mrb_value *argv = ci->stack + 1;

  if (argc < 15) {
    argv[0] = mrb_ary_new_from_values(mrb, argc, argv);
    argv[1] = argv[argc];       /* copy block */
    ci->n = 15;
  }
  mrb_ary_splice(mrb, argv[0], 1, 0, binding); /* insert binding as 2nd argument */
  return mrb_f_eval(mrb, binding);
}

void
mrb_mruby_binding_gem_init(mrb_state *mrb)
{
  struct RClass *binding = mrb_class_get_id(mrb, MRB_SYM(Binding));
  mrb_define_method(mrb, binding, "eval", mrb_binding_eval, MRB_ARGS_ANY());
}

void
mrb_mruby_binding_gem_final(mrb_state *mrb)
{
}
