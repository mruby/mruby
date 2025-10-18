#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/compile.h>
#include <mruby/irep.h>
#include <mruby/proc.h>
#include <mruby/opcode.h>
#include <mruby/error.h>
#include <mruby/presym.h>
#include <mruby/variable.h>
#include <mruby/internal.h>

/* provided by mruby-binding */
mrb_bool mrb_binding_p(mrb_state *mrb, mrb_value binding);
const struct RProc * mrb_binding_extract_proc(mrb_state *mrb, mrb_value binding);
struct REnv * mrb_binding_extract_env(mrb_state *mrb, mrb_value binding);

/* provided by mruby-compiler */
typedef mrb_bool mrb_parser_foreach_top_variable_func(mrb_state *mrb, mrb_sym sym, void *user);
void mrb_parser_foreach_top_variable(mrb_state *mrb, struct mrb_parser_state *p, mrb_parser_foreach_top_variable_func *func, void *user);

static struct RProc*
create_proc_from_string(mrb_state *mrb, const char *s, mrb_int len, mrb_value binding, const char *file, mrb_int line)
{
  mrb_ccontext *cxt;
  struct mrb_parser_state *p;
  struct RProc *proc;
  const struct RProc *scope;
  struct REnv *e;
  mrb_callinfo *ci; /* callinfo of eval caller */
  struct RClass *target_class = NULL;
  struct mrb_context *c = mrb->c;

  if (!mrb_nil_p(binding)) {
    if (!mrb_binding_p(mrb, binding)) {
      mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %C (expected binding)",
                 mrb_obj_class(mrb, binding));
    }
    scope = mrb_binding_extract_proc(mrb, binding);
    if (MRB_PROC_CFUNC_P(scope)) {
      e = NULL;
    }
    else {
      e = mrb_binding_extract_env(mrb, binding);
      mrb_assert(e != NULL);
    }
  }
  else {
    ci = (c->ci > c->cibase) ? c->ci - 1 : c->cibase;
    scope = ci->proc;
    e = NULL;
  }

  if (file) {
    if (strlen(file) >= UINT16_MAX) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "filename too long");
    }
  }
  else {
    file = "(eval)";
  }

  cxt = mrb_ccontext_new(mrb);
  cxt->lineno = (uint16_t)line;

  mrb_ccontext_filename(mrb, cxt, file);
  cxt->capture_errors = TRUE;
  cxt->no_optimize = TRUE;
  cxt->upper = scope && MRB_PROC_CFUNC_P(scope) ? NULL : scope;

  p = mrb_parse_nstring(mrb, s, len, cxt);

  /* only occur when memory ran out */
  if (!p) {
    mrb_ccontext_free(mrb, cxt);
    mrb_raise(mrb, E_RUNTIME_ERROR, "Failed to create parser state (out of memory)");
  }

  if (0 < p->nerr) {
    /* parse error */
    mrb_value str;

    mrb_ccontext_free(mrb, cxt);
    if (!p->error_buffer[0].message) {
      mrb_parser_free(p);
      mrb_raise(mrb, E_SYNTAX_ERROR, "compile error");
    }
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
    mrb_parser_free(p);
    mrb_exc_raise(mrb, mrb_exc_new_str(mrb, E_SYNTAX_ERROR, str));
  }

  proc = mrb_generate_code(mrb, p);
  if (proc == NULL) {
    /* codegen error */
    mrb_parser_free(p);
    mrb_ccontext_free(mrb, cxt);
    mrb_raise(mrb, E_SCRIPT_ERROR, "codegen error");
  }
  if (c->ci > c->cibase) {
    ci = &c->ci[-1];
  }
  else {
    ci = c->cibase;
  }
  if (scope) {
    target_class = MRB_PROC_TARGET_CLASS(scope);
    if (!MRB_PROC_CFUNC_P(scope)) {
      if (e == NULL) {
        /* when `binding` is nil */
        e = mrb_vm_ci_env(ci);
        if (e == NULL) {
          e = mrb_env_new(mrb, c, ci, ci->proc->body.irep->nlocals, ci->stack, target_class);
          ci->u.env = e;
        }
      }
      proc->e.env = e;
      proc->flags |= MRB_PROC_ENVSET;
      mrb_field_write_barrier(mrb, (struct RBasic*)proc, (struct RBasic*)e);
    }
  }
  proc->upper = scope;
  mrb_vm_ci_target_class_set(mrb->c->ci, target_class);
  /* mrb_codedump_all(mrb, proc); */

  mrb_parser_free(p);
  mrb_ccontext_free(mrb, cxt);

  return proc;
}

static mrb_value
exec_irep(mrb_state *mrb, mrb_value self, struct RProc *proc)
{
  mrb_callinfo *ci = mrb->c->ci;

  /* no argument passed from eval() */
  ci->n = 0;
  ci->nk = 0;
  /* clear visibility */
  MRB_CI_SET_VISIBILITY_BREAK(ci);
  /* clear block */
  ci->stack[1] = mrb_nil_value();
  return mrb_exec_irep(mrb, self, proc);
}

static void
binding_eval_error_check(mrb_state *mrb, struct mrb_parser_state *p, const char *file)
{
  if (!p) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "Failed to create parser state (out of memory)");
  }

  if (0 < p->nerr) {
    if (p->mrb->exc) {
      mrb_exc_raise(mrb, mrb_obj_value(p->mrb->exc));
    }

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
  mrb_ccontext *cxt;
  struct mrb_parser_state *pstate;
};

static mrb_value
binding_eval_prepare_body(mrb_state *mrb, void *opaque)
{
  struct binding_eval_prepare_body *p = (struct binding_eval_prepare_body*)opaque;
  binding_eval_error_check(mrb, p->pstate, p->file);

  struct expand_lvspace args = {
    (mrb_irep*)p->cxt->upper->body.irep,
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
binding_eval_prepare(mrb_state *mrb, mrb_value binding, const char *expr, mrb_int exprlen, const char *file)
{
  struct binding_eval_prepare_body d = { binding };
  const struct RProc *proc = mrb_binding_extract_proc(mrb, binding);
  mrb_assert(!MRB_PROC_CFUNC_P(proc));

  d.cxt = mrb_ccontext_new(mrb);
  d.file = mrb_ccontext_filename(mrb, d.cxt, file ? file : "(eval)");
  d.cxt->capture_errors = TRUE;
  d.cxt->upper = proc;
  d.pstate = mrb_parse_nstring(mrb, expr, exprlen, d.cxt);

  mrb_bool error;
  mrb_value ret = mrb_protect_error(mrb, binding_eval_prepare_body, &d, &error);
  if (d.pstate) mrb_parser_free(d.pstate);
  if (d.cxt) mrb_ccontext_free(mrb, d.cxt);
  if (error) mrb_exc_raise(mrb, ret);
}

/*
 * call-seq:
 *   eval(string, binding = nil, filename = nil, lineno = 1) -> obj
 *
 * Evaluates the Ruby expression(s) in string. If binding is given,
 * which must be a Binding object, the evaluation is performed in its
 * context. If filename is given, it is used for error reporting.
 * If lineno is given, it is used as the starting line number for error reporting.
 *
 *   eval("1 + 2")                    #=> 3
 *   eval("x = 10; x * 2")            #=> 20
 *
 *   x = 5
 *   b = binding
 *   eval("x", b)                     #=> 5
 *   eval("x = 100", b)               #=> 100
 *   x                                #=> 100
 */
static mrb_value
f_eval(mrb_state *mrb, mrb_value self)
{
  const char *s;
  mrb_int len;
  mrb_value binding = mrb_nil_value();
  const char *file = NULL;
  mrb_int line = 1;
  struct RProc *proc;

  mrb_get_args(mrb, "s|ozi", &s, &len, &binding, &file, &line);

  if (!mrb_nil_p(binding)) {
    binding_eval_prepare(mrb, binding, s, len, file);
  }
  proc = create_proc_from_string(mrb, s, len, binding, file, line);
  if (!mrb_nil_p(binding)) {
    self = mrb_iv_get(mrb, binding, MRB_SYM(recv));
  }
  mrb_assert(!MRB_PROC_CFUNC_P(proc));
  return exec_irep(mrb, self, proc);
}

static mrb_value
object_eval(mrb_state *mrb, mrb_value self, mrb_bool class_eval)
{
  if (mrb_block_given_p(mrb)) {
    mrb_get_args(mrb, "");
    return class_eval ? mrb_mod_module_eval(mrb, self) : mrb_obj_instance_eval(mrb, self);
  }

  const char *s;
  mrb_int len;
  const char *file = NULL;
  mrb_int line = 1;
  mrb_get_args(mrb, "s|zi", &s, &len, &file, &line);

  struct RClass *c = class_eval ? mrb_class_ptr(self) : mrb_singleton_class_ptr(mrb, self);
  struct RProc *proc = create_proc_from_string(mrb, s, len, mrb_nil_value(), file, line);
  MRB_PROC_SET_TARGET_CLASS(proc, c);
  mrb_assert(!MRB_PROC_CFUNC_P(proc));
  mrb_vm_ci_target_class_set(mrb->c->ci, c);
  return exec_irep(mrb, self, proc);
}

/*
 * call-seq:
 *   obj.instance_eval(string, filename = nil, lineno = 1) -> obj
 *   obj.instance_eval {|obj| block } -> obj
 *
 * Evaluates a string containing Ruby source code, or the given block,
 * within the context of the receiver (obj). In order to set the context,
 * the variable self is set to obj while the code is executing, giving
 * the code access to obj's instance variables and private methods.
 *
 *   class KlassWithSecret
 *     def initialize
 *       @secret = 99
 *     end
 *     private
 *     def the_secret
 *       "Ssssh! The secret is #{@secret}."
 *     end
 *   end
 *   k = KlassWithSecret.new
 *   k.instance_eval { @secret }          #=> 99
 *   k.instance_eval { the_secret }       #=> "Ssssh! The secret is 99."
 *   k.instance_eval("@secret = 5")       #=> 5
 */
static mrb_value
f_instance_eval(mrb_state *mrb, mrb_value self)
{
  return object_eval(mrb, self, FALSE);
}

/*
 * call-seq:
 *   mod.class_eval(string, filename = nil, lineno = 1) -> obj
 *   mod.class_eval {|mod| block } -> obj
 *   mod.module_eval(string, filename = nil, lineno = 1) -> obj
 *   mod.module_eval {|mod| block } -> obj
 *
 * Evaluates the string or block in the context of mod, except that when
 * a block is given, constant/class variable lookup is not affected.
 * This can be used to add methods to a class. module_eval returns the
 * result of evaluating its argument.
 *
 *   class Thing
 *   end
 *   a = %q{def hello() "Hello there!" end}
 *   Thing.module_eval(a)
 *   puts Thing.new.hello()           #=> "Hello there!"
 *
 *   Thing.class_eval("@@var = 99")
 *   Thing.class_eval { @@var }       #=> 99
 */
static mrb_value
f_class_eval(mrb_state *mrb, mrb_value self)
{
  return object_eval(mrb, self, TRUE);
}

/*
 * call-seq:
 *   binding.eval(string, filename = nil, lineno = 1) -> obj
 *
 * Evaluates the given string in the context of the binding.
 * This is equivalent to calling eval(string, binding, filename, lineno).
 *
 *   def get_binding(param)
 *     binding
 *   end
 *   b = get_binding("hello")
 *   b.eval("param")                  #=> "hello"
 *   b.eval("x = 10; x + param.length") #=> 15
 */
static mrb_value
mrb_binding_eval(mrb_state *mrb, mrb_value binding)
{
  mrb_callinfo *ci = mrb->c->ci;
  int argc = ci->n;
  mrb_value *argv = ci->stack + 1;

  if (argc < 15) {
    argv[0] = mrb_ary_new_from_values(mrb, argc, argv);
    argv[1] = argv[argc];       /* copy block */
    ci->n = 15;
  }
  mrb_ary_splice(mrb, argv[0], 1, 0, binding); /* insert binding as 2nd argument */
  return f_eval(mrb, binding);
}

void
mrb_mruby_eval_gem_init(mrb_state* mrb)
{
  mrb_define_private_method_id(mrb, mrb->kernel_module, MRB_SYM(eval), f_eval, MRB_ARGS_ARG(1, 3));
  mrb_define_method_id(mrb, mrb_class_get_id(mrb, MRB_SYM(BasicObject)), MRB_SYM(instance_eval), f_instance_eval, MRB_ARGS_OPT(3)|MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, mrb->module_class, MRB_SYM(module_eval), f_class_eval, MRB_ARGS_OPT(3)|MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, mrb->module_class, MRB_SYM(class_eval), f_class_eval, MRB_ARGS_OPT(3)|MRB_ARGS_BLOCK());

  struct RClass *binding = mrb_class_get_id(mrb, MRB_SYM(Binding));
  mrb_define_method_id(mrb, binding, MRB_SYM(eval), mrb_binding_eval, MRB_ARGS_ANY());
}

void
mrb_mruby_eval_gem_final(mrb_state* mrb)
{
}
