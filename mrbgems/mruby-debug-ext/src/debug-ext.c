#define MRB_DEBUG_DUMP_FUNCTIONS

#include "mruby.h"
#include "mruby/compile.h"
#include "mruby/proc.h"
#include "mruby/string.h"
#include "mruby/variable.h"

static mrb_value
proc_codedump(mrb_state *mrb, mrb_value self)
{
  struct RProc *proc = mrb_proc_ptr(self);
  if (MRB_PROC_CFUNC_P(proc)) {
    mrb_raise(mrb, E_TYPE_ERROR, "Cannot dump cfunc proc.");
  }

  mrb_codedump_all(mrb, proc);

  return self;
}

static mrb_value
string_parser_dump(mrb_state *mrb, mrb_value self)
{
  mrbc_context *ctx;
  struct mrb_parser_state *result;

  ctx = mrbc_context_new(mrb);
  ctx->capture_errors = TRUE;
  result = mrb_parse_nstring(mrb, RSTRING_PTR(self), RSTRING_LEN(self), ctx);

  if (!result->tree || result->nerr) {
    char buf[256];
    int n;
    mrb_value exc;

    n = snprintf(buf, sizeof(buf), "line %d: %s",
                 result->error_buffer[0].lineno, result->error_buffer[0].message);
    exc = mrb_exc_new(mrb, E_SYNTAX_ERROR, buf, n);
    mrb_parser_free(result);
    mrbc_context_free(mrb, ctx);
    mrb_exc_raise(mrb, exc);
  } else {
    mrb_parser_dump(mrb, result->tree, 0);
    mrb_parser_free(result);
    mrbc_context_free(mrb, ctx);
  }

  return self;
}

FILE*
mrb_debug_output(mrb_state *mrb)
{
  struct RClass *mod;
  mrb_value output;

  mod = mrb_module_get(mrb, "DebugExt");
  output = mrb_mod_cv_get(mrb, mod, mrb_intern_lit(mrb, "Output"));
  mrb_assert(mrb_type(output) == MRB_TT_VOIDP);
  return (FILE*)mrb_voidp(output);
}

void
set_output(mrb_state *mrb, FILE *fp)
{
  mrb_value output_value;
  FILE *output;
  struct RClass *mod;

  mod = mrb_module_get(mrb, "DebugExt");

  output_value = mrb_mod_cv_get(mrb, mod, mrb_intern_lit(mrb, "Output"));
  mrb_assert(mrb_type(output_value) == MRB_TT_VOIDP);

  output = (FILE*)mrb_voidp(output_value);
  if(output && output != stdout && output != stderr) {
    fclose(output);
  }

  mrb_mod_cv_set(mrb, mod, mrb_intern_lit(mrb, "Output"), mrb_voidp_value(mrb, fp));
}

static mrb_value
disable_output(mrb_state *mrb, mrb_value self)
{
  set_output(mrb, NULL);
  return self;
}

static mrb_value
output_to_stdout(mrb_state *mrb, mrb_value self)
{
  set_output(mrb, stdout);
  return self;
}

static mrb_value
output_to_stderr(mrb_state *mrb, mrb_value self)
{
  set_output(mrb, stderr);
  return self;
}

static mrb_value
output_to_file(mrb_state *mrb, mrb_value self)
{
  FILE *fp;
  mrb_value filename;

  mrb_get_args(mrb, "S", &filename);
  fp = fopen(mrb_string_value_ptr(mrb, filename), "w");
  if(!fp) {
    mrb_raisef(mrb, E_RUNTIME_ERROR, "cannot open file for debug output: %S", filename);
  }

  set_output(mrb, fp);
  return self;
}

void
mrb_mruby_debug_ext_gem_init(mrb_state *mrb)
{
  struct RClass *mod;

  mrb_define_method(mrb, mrb->proc_class, "codedump", proc_codedump, MRB_ARGS_NONE());
  mrb_define_method(mrb, mrb->string_class, "parser_dump", string_parser_dump, MRB_ARGS_NONE());

  mod = mrb_define_module(mrb, "DebugExt");

  mrb_define_const(mrb, mod, "Output", mrb_voidp_value(mrb, stdout));
  mrb_define_module_function(mrb, mod, "disable_output", disable_output, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, mod, "output_to_stdout", output_to_stdout, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, mod, "output_to_stderr", output_to_stderr, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, mod, "output_to_file", output_to_file, MRB_ARGS_REQ(1));
}

void
mrb_mruby_debug_ext_gem_final(mrb_state *mrb)
{
  (void)mrb;
}
