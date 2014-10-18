#include "mruby.h"
#include "mruby/class.h"
#include "mruby/compile.h"
#include "mruby/irep.h"
#include "mruby/proc.h"
#include "mruby/opcode.h"

static struct mrb_irep *
get_closure_irep(mrb_state *mrb, int level)
{
  struct REnv *e = mrb->c->ci[-1].proc->env;
  struct RProc *proc;

  if (level == 0) {
    proc = mrb->c->ci[-1].proc;
    if (MRB_PROC_CFUNC_P(proc)) {
      return NULL;
    }
    return proc->body.irep;
  }

  while (--level) {
    e = (struct REnv*)e->c;
    if (!e) return NULL;
  }

  if (!e) return NULL;
  proc = mrb->c->cibase[e->cioff].proc;

  if (MRB_PROC_CFUNC_P(proc)) {
    return NULL;
  }
  return proc->body.irep;
}

static inline mrb_code
search_variable(mrb_state *mrb, mrb_sym vsym, int bnest)
{
  mrb_irep *virep;
  int level;
  int pos;

  for (level = 0; (virep = get_closure_irep(mrb, level)); level++) {
    if (!virep || virep->lv == NULL) {
      continue;
    }
    for (pos = 0; pos < virep->nlocals - 1; pos++) {
      if (vsym == virep->lv[pos].name) {
        return (MKARG_B(pos + 1) | MKARG_C(level + bnest));
      }
    }
  }

  return 0;
}


static void
patch_irep(mrb_state *mrb, mrb_irep *irep, int bnest)
{
  size_t i;
  mrb_code c;

  for (i = 0; i < irep->ilen; i++) {
    c = irep->iseq[i];
    switch(GET_OPCODE(c)){
    case OP_EPUSH:
      patch_irep(mrb, irep->reps[GETARG_Bx(c)], bnest + 1);
      break;

    case OP_LAMBDA:
      {
        int arg_c = GETARG_c(c);
        if (arg_c & OP_L_CAPTURE) {
          patch_irep(mrb, irep->reps[GETARG_b(c)], bnest + 1);
        }
      }
      break;

    case OP_SEND:
      if (GETARG_C(c) != 0) {
        break;
      }
      {
        mrb_code arg = search_variable(mrb, irep->syms[GETARG_B(c)], bnest);
        if (arg != 0) {
          /* must replace */
          irep->iseq[i] = MKOPCODE(OP_GETUPVAR) | MKARG_A(GETARG_A(c)) | arg;
        }
      }
      break;

    case OP_MOVE:
      /* src part */
      if (GETARG_B(c) < irep->nlocals) {
        mrb_code arg = search_variable(mrb, irep->lv[GETARG_B(c) - 1].name, bnest);
        if (arg != 0) {
          /* must replace */
          irep->iseq[i] = MKOPCODE(OP_GETUPVAR) | MKARG_A(GETARG_A(c)) | arg;
        }
      }
      /* dst part */
      if (GETARG_A(c) < irep->nlocals) {
        mrb_code arg = search_variable(mrb, irep->lv[GETARG_A(c) - 1].name, bnest);
        if (arg != 0) {
          /* must replace */
          irep->iseq[i] = MKOPCODE(OP_SETUPVAR) | MKARG_A(GETARG_B(c)) | arg;
        }
      }
      break;
    }
  }
}

static struct RProc*
create_proc_from_string(mrb_state *mrb, char *s, int len, mrb_value binding, char *file, mrb_int line)
{
  mrbc_context *cxt;
  struct mrb_parser_state *p;
  struct RProc *proc;
  struct REnv *e;

  if (!mrb_nil_p(binding)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Binding of eval must be nil.");
  }

  cxt = mrbc_context_new(mrb);
  cxt->lineno = line;
  if (file) {
    mrbc_filename(mrb, cxt, file);
  }
  cxt->capture_errors = TRUE;

  p = mrb_parse_nstring(mrb, s, len, cxt);

  /* only occur when memory ran out */
  if (!p) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "Failed to create parser state.");
  }

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
  if (proc == NULL) {
    /* codegen error */
    mrb_parser_free(p);
    mrbc_context_free(mrb, cxt);
    mrb_raise(mrb, E_SCRIPT_ERROR, "codegen error");
  }
  if (mrb->c->ci[-1].proc->target_class) {
    proc->target_class = mrb->c->ci[-1].proc->target_class;
  }
  e = (struct REnv*)mrb_obj_alloc(mrb, MRB_TT_ENV, (struct RClass*)mrb->c->ci[-1].proc->env);
  e->mid = mrb->c->ci[-1].mid;
  e->cioff = mrb->c->ci - mrb->c->cibase - 1;
  e->stack = mrb->c->ci->stackent;
  mrb->c->ci->env = e;
  proc->env = e;
  patch_irep(mrb, proc->body.irep, 0);

  mrb_parser_free(p);
  mrbc_context_free(mrb, cxt);

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
  struct RProc *proc;

  mrb_get_args(mrb, "s|ozi", &s, &len, &binding, &file, &line);

  proc = create_proc_from_string(mrb, s, len, binding, file, line);
  ret = mrb_toplevel_run(mrb, proc);
  if (mrb->exc) {
    mrb_exc_raise(mrb, mrb_obj_value(mrb->exc));
  }

  return ret;
}

mrb_value mrb_obj_instance_eval(mrb_state *mrb, mrb_value self);

#define CI_ACC_SKIP    -1

static mrb_value
f_instance_eval(mrb_state *mrb, mrb_value self)
{
  mrb_value b;
  mrb_int argc; mrb_value *argv;

  mrb_get_args(mrb, "*&", &argv, &argc, &b);

  if (mrb_nil_p(b)) {
    char *s;
    mrb_int len;
    char *file = NULL;
    mrb_int line = 1;

    mrb_get_args(mrb, "s|zi", &s, &len, &file, &line);
    mrb->c->ci->acc = CI_ACC_SKIP;
    if (mrb->c->ci->target_class->tt == MRB_TT_ICLASS) {
      mrb->c->ci->target_class = mrb->c->ci->target_class->c;
    }
    return mrb_run(mrb, create_proc_from_string(mrb, s, len, mrb_nil_value(), file, line), self);
  }
  else {
    mrb_get_args(mrb, "&", &b);
    return mrb_obj_instance_eval(mrb, self);
  }
}

void
mrb_mruby_eval_gem_init(mrb_state* mrb)
{
  mrb_define_module_function(mrb, mrb->kernel_module, "eval", f_eval, MRB_ARGS_ARG(1, 3));
  mrb_define_method(mrb, mrb->kernel_module, "instance_eval", f_instance_eval, MRB_ARGS_ARG(1, 2));
}

void
mrb_mruby_eval_gem_final(mrb_state* mrb)
{
}
