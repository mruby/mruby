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
  int len, ai;

  if (level > 0) {
    return;
  }

  ai = mrb_gc_arena_save(mrb);
  ary = mrb_obj_value((struct RArray*)stream);
  va_start(ap, format);
  len = vsnprintf(NULL, 0, format, ap);
  str = mrb_str_new(mrb, 0, len);
  vsnprintf(RSTRING_PTR(str), len, format, ap);
  mrb_ary_push(mrb, ary, str);
  va_end(ap);
  mrb_gc_arena_restore(mrb, ai);
}

static void
mrb_output_backtrace(mrb_state *mrb, struct RObject *exc, output_stream_func func, void *stream)
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
    filename = "(unknown)";
    line = -1;

    if (MRB_PROC_CFUNC_P(ci->proc)) {
      continue;
    }
    else {
      mrb_irep *irep = ci->proc->body.irep;
      if (irep->filename != NULL)
        filename = irep->filename;
      if (irep->lines != NULL) {
        mrb_code *pc;

        if (i+1 <= ciidx) {
          pc = mrb->c->cibase[i+1].pc;
        }
        else {
          pc = (mrb_code*)mrb_voidp(mrb_obj_iv_get(mrb, exc, mrb_intern2(mrb, "lastpc", 6)));
        }
        if (irep->iseq <= pc && pc < irep->iseq + irep->ilen) {
          line = irep->lines[pc - irep->iseq - 1];
        }
      }
    }
    if (line == -1) continue;
    if (ci->target_class == ci->proc->target_class)
      sep = ".";
    else
      sep = "#";

    method = mrb_sym2name(mrb, ci->mid);
    if (method) {
      const char *cn = mrb_class_name(mrb, ci->proc->target_class);

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
  mrb_output_backtrace(mrb, mrb->exc, print_backtrace_i, (void*)stderr);
#endif
}

mrb_value
mrb_get_backtrace(mrb_state *mrb, mrb_value self)
{
  mrb_value ary;

  ary = mrb_ary_new(mrb);
  mrb_output_backtrace(mrb, mrb_obj_ptr(self), get_backtrace_i, (void*)mrb_ary_ptr(ary));

  return ary;
}
