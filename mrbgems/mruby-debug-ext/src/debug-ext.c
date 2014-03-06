#include "mruby.h"
#include "mruby/compile.h"
#include "mruby/proc.h"
#include "mruby/string.h"

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

void
mrb_mruby_debug_gem_init(mrb_state *mrb)
{
  mrb_define_method(mrb, mrb->proc_class, "codedump", &proc_codedump, MRB_ARGS_NONE());
  mrb_define_method(mrb, mrb->string_class, "parser_dump", &string_parser_dump, MRB_ARGS_NONE());
}

void
mrb_mruby_debug_gem_final(mrb_state *mrb)
{
  (void)mrb;
}
