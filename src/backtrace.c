/*
** backtrace.c -
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/variable.h>
#include <mruby/proc.h>
#include <mruby/array.h>
#include <mruby/string.h>
#include <mruby/class.h>
#include <mruby/debug.h>
#include <mruby/error.h>
#include <mruby/numeric.h>

struct backtrace_location {
  int lineno;
  const char *filename;
  mrb_sym method_id;
};

typedef void (*each_backtrace_func)(mrb_state*, int i, struct backtrace_location*, void*);

static void
each_backtrace(mrb_state *mrb, mrb_int ciidx, mrb_code *pc0, each_backtrace_func func, void *data)
{
  int i, j;

  if (ciidx >= mrb->c->ciend - mrb->c->cibase)
    ciidx = 10; /* ciidx is broken... */

  for (i=ciidx, j=0; i >= 0; i--,j++) {
    struct backtrace_location loc;
    mrb_callinfo *ci;
    mrb_irep *irep;
    mrb_code *pc;

    ci = &mrb->c->cibase[i];

    if (!ci->proc) continue;
    if (MRB_PROC_CFUNC_P(ci->proc)) continue;

    irep = ci->proc->body.irep;
    if (!irep) continue;

    if (mrb->c->cibase[i].err) {
      pc = mrb->c->cibase[i].err;
    }
    else if (i+1 <= ciidx) {
      pc = mrb->c->cibase[i+1].pc - 1;
    }
    else {
      pc = pc0;
    }
    loc.filename = mrb_debug_get_filename(irep, (uint32_t)(pc - irep->iseq));
    loc.lineno = mrb_debug_get_line(irep, (uint32_t)(pc - irep->iseq));

    if (loc.lineno == -1) continue;

    if (!loc.filename) {
      loc.filename = "(unknown)";
    }

    loc.method_id = ci->mid;
    func(mrb, j, &loc, data);
  }
}

/* mrb_print_backtrace

   function to retrieve backtrace information from the exception.
   note that if you call method after the exception, call stack will be
   overwritten.  So invoke these functions just after detecting exceptions.
*/

#ifndef MRB_DISABLE_STDIO

static void
print_backtrace(mrb_state *mrb, mrb_value backtrace)
{
  int i, n;
  FILE *stream = stderr;

  if (!mrb_array_p(backtrace)) return;
  fprintf(stream, "trace:\n");

  n = RARRAY_LEN(backtrace);
  for (i = 0; i < n; i++) {
    mrb_value entry = RARRAY_PTR(backtrace)[i];

    if (mrb_string_p(entry)) {
      fprintf(stream, "\t[%d] %.*s\n", i, (int)RSTRING_LEN(entry), RSTRING_PTR(entry));
    }
  }
}

static void
print_packed_backtrace(mrb_state *mrb, mrb_value packed)
{
  FILE *stream = stderr;
  struct backtrace_location *bt;
  mrb_int n, i;

  if (!mrb_string_p(packed)) {
  broken:
    mrb_raise(mrb, E_RUNTIME_ERROR, "broken backtrace");
  }
  n = RSTRING_LEN(packed);
  if (n%sizeof(struct backtrace_location) != 0) goto broken;
  n /= sizeof(struct backtrace_location);
  bt = (struct backtrace_location*)RSTRING_PTR(packed);

  fprintf(stream, "trace:\n");
  for (i = 0; i < n; i++) {
    int ai = mrb_gc_arena_save(mrb);
    struct backtrace_location *entry = &bt[i];
    if (entry->filename == NULL) continue;
    fprintf(stream, "\t[%d] %s:%d", (int)i, entry->filename, entry->lineno);
    if (entry->method_id != 0) {
      const char *method_name;

      method_name = mrb_sym2name(mrb, entry->method_id);
      fprintf(stream, ":in %s", method_name);
      mrb_gc_arena_restore(mrb, ai);
    }
    fprintf(stream, "\n");
  }
}

MRB_API void
mrb_print_backtrace(mrb_state *mrb)
{
  mrb_value backtrace;

  if (!mrb->exc) {
    return;
  }

  backtrace = mrb_obj_iv_get(mrb, mrb->exc, mrb_intern_lit(mrb, "backtrace"));
  if (mrb_array_p(backtrace)) {
    print_backtrace(mrb, backtrace);
  }
  else {
    print_packed_backtrace(mrb, backtrace);
  }
}
#else

MRB_API void
mrb_print_backtrace(mrb_state *mrb)
{
}

#endif

static void
pack_backtrace_i(mrb_state *mrb,
                 int i,
                 struct backtrace_location *loc,
                 void *data)
{
  struct backtrace_location *entry = (struct backtrace_location*)data;

  entry[i] = *loc;
}

static mrb_value
packed_backtrace(mrb_state *mrb)
{
  mrb_value backtrace;
  mrb_int ciidx = mrb->c->ci - mrb->c->cibase;
  mrb_int len = (ciidx+1)*sizeof(struct backtrace_location);
  char *ptr;

  backtrace = mrb_str_new(mrb, NULL, len);
  ptr = RSTRING_PTR(backtrace);
  memset(ptr, 0, len);
  each_backtrace(mrb, ciidx, mrb->c->ci->pc, pack_backtrace_i, ptr);
  return backtrace;
}

void
mrb_keep_backtrace(mrb_state *mrb, mrb_value exc)
{
  mrb_value backtrace;
  int ai = mrb_gc_arena_save(mrb);

  backtrace = packed_backtrace(mrb);
  mrb_iv_set(mrb, exc, mrb_intern_lit(mrb, "backtrace"), backtrace);
  mrb_gc_arena_restore(mrb, ai);
}

mrb_value
mrb_unpack_backtrace(mrb_state *mrb, mrb_value backtrace)
{
  mrb_value str;
  struct backtrace_location *bt;
  mrb_int n, i;

  if (mrb_array_p(backtrace)) return backtrace;
  if (!mrb_string_p(backtrace)) {
  broken:
    mrb_raise(mrb, E_RUNTIME_ERROR, "broken backtrace");
  }
  str = backtrace;
  n = RSTRING_LEN(str);
  if (n%sizeof(struct backtrace_location) != 0) goto broken;
  n /= sizeof(struct backtrace_location);
  bt = (struct backtrace_location*)RSTRING_PTR(str);
  backtrace = mrb_ary_new_capa(mrb, n);
  for (i = 0; i < n; i++) {
    int ai = mrb_gc_arena_save(mrb);
    struct backtrace_location *entry = &bt[i];
    mrb_value btline;

    if (entry->filename == NULL) continue;
    btline = mrb_format(mrb, "%S:%S",
                              mrb_str_new_cstr(mrb, entry->filename),
                              mrb_fixnum_value(entry->lineno));
    if (entry->method_id != 0) {
      mrb_str_cat_lit(mrb, btline, ":in ");
      mrb_str_cat_cstr(mrb, btline, mrb_sym2name(mrb, entry->method_id));
    }
    mrb_ary_push(mrb, backtrace, btline);
    mrb_gc_arena_restore(mrb, ai);
  }

  return backtrace;
}

mrb_value
mrb_get_backtrace(mrb_state *mrb)
{
  return mrb_unpack_backtrace(mrb, packed_backtrace(mrb));
}
