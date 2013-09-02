/*
** backtrace.c -
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/variable.h"
#include "mruby/proc.h"
#include "mruby/array.h"
#include "mruby/string.h"
#include "mruby/class.h"
#include "mruby/debug.h"
#include <stdarg.h>

typedef void (*output_stream_func)(mrb_state*, void*, int, const char*, ...);

#ifdef ENABLE_STDIO
static void
print_backtrace_i(mrb_state *mrb, void *stream, int level, const char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  vfprintf((FILE*)stream, format, ap);
  va_end(ap);
}
#endif

#define MIN_BUFSIZE 127

static void
get_backtrace_i(mrb_state *mrb, void *stream, int level, const char *format, ...)
{
  va_list ap;
  mrb_value ary, str;
  int ai;

  if (level > 0) {
    return;
  }

  ai = mrb_gc_arena_save(mrb);
  ary = mrb_obj_value((struct RArray*)stream);

  va_start(ap, format);
  str = mrb_str_new(mrb, 0, vsnprintf(NULL, 0, format, ap) + 1);
  va_end(ap);

  va_start(ap, format);
  vsnprintf(RSTRING_PTR(str), RSTRING_LEN(str), format, ap);
  va_end(ap);

  mrb_str_resize(mrb, str, RSTRING_LEN(str) - 1);
  mrb_ary_push(mrb, ary, str);
  mrb_gc_arena_restore(mrb, ai);
}

static void
mrb_output_backtrace(mrb_state *mrb, struct RObject *exc, output_stream_func func, void *stream, mrb_bool verbose)
{
  mrb_callinfo *ci;
  mrb_int ciidx;
  const char *filename, *method, *sep;
  int i, line;

  func(mrb, stream, 1, "trace:\n");
  ciidx = mrb_fixnum(mrb_obj_iv_get(mrb, exc, mrb_intern2(mrb, "ciidx", 5)));
  if (ciidx >= mrb->c->ciend - mrb->c->cibase)
    ciidx = 10; /* ciidx is broken... */

  for (i = ciidx; i >= 0; i--) {
    ci = &mrb->c->cibase[i];
    filename = NULL;
    line = -1;

    if (!verbose && MRB_PROC_CFUNC_P(ci->proc)) {
      continue;
    }
    if(!MRB_PROC_CFUNC_P(ci->proc)) {
      mrb_irep *irep = ci->proc->body.irep;
      mrb_code *pc;

      if (i+1 <= ciidx) {
        pc = mrb->c->cibase[i+1].pc;
      }
      else {
        pc = (mrb_code*)mrb_voidp(mrb_obj_iv_get(mrb, exc, mrb_intern2(mrb, "lastpc", 6)));
      }
      filename = mrb_debug_get_filename(irep, pc - irep->iseq - 1);
      line = mrb_debug_get_line(irep, pc - irep->iseq - 1);
    }
    if (!verbose && line == -1) continue;
    if (ci->target_class == ci->proc->target_class)
      sep = ".";
    else
      sep = "#";

    if (!filename) {
      filename = "(unknown)";
    }

    method = mrb_sym2name(mrb, ci->mid);
    if (verbose && !method && ci->proc->env) {
      method = mrb_sym2name(mrb, ci->proc->env->mid);
    }
    if (method) {
      const char *cn = NULL;
      if(verbose && ci->proc->target_class->tt == MRB_TT_SCLASS) {
        cn = RSTRING_PTR(mrb_str_cat_cstr(
            mrb, mrb_str_new_cstr(mrb, "(singleton class of)"),
            mrb_class_name(mrb, mrb_class_ptr(mrb_mod_cv_get(
                mrb, ci->proc->target_class, mrb_intern_cstr(mrb, "__attached__"))))));
      }
      else {
        cn = mrb_class_name(mrb, ci->proc->target_class);
      }

      if (cn) {
        func(mrb, stream, 1, "\t[%d] ", i);
        func(mrb, stream, 0, "%s:%d:in %s%s%s", filename, line, cn, sep, method);
        func(mrb, stream, 1, "\n");
      }
      else {
        func(mrb, stream, 1, "\t[%d] ", i);
        func(mrb, stream, 0, "%s:%d:in %s", filename, line, method);
        func(mrb, stream, 1, "\n");
      }
    }
    else {
        func(mrb, stream, 1, "\t[%d] ", i);
        func(mrb, stream, 0, "%s:%d", filename, line);
        func(mrb, stream, 1, "\n");
    }
  }
}

void
mrb_print_backtrace(mrb_state *mrb)
{
#ifdef ENABLE_STDIO
  mrb_output_backtrace(mrb, mrb->exc, print_backtrace_i, (void*)stderr, 0);
#endif
}

void
mrb_print_verbose_backtrace(mrb_state *mrb)
{
#ifdef ENABLE_STDIO
  mrb_output_backtrace(mrb, mrb->exc, print_backtrace_i, (void*)stderr, 1);
#endif
}

mrb_value
mrb_get_backtrace(mrb_state *mrb, mrb_value self)
{
  mrb_value ary;

  ary = mrb_ary_new(mrb);
  mrb_output_backtrace(mrb, mrb_obj_ptr(self), get_backtrace_i, (void*)mrb_ary_ptr(ary), 0);

  return ary;
}

mrb_value
mrb_get_verbose_backtrace(mrb_state *mrb, mrb_value self)
{
  mrb_value ary;

  ary = mrb_ary_new(mrb);
  mrb_output_backtrace(mrb, mrb_obj_ptr(self), get_backtrace_i, (void*)mrb_ary_ptr(ary), 1);

  return ary;
}
