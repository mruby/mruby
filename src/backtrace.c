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
#include <mruby/data.h>
#include <mruby/presym.h>

struct backtrace_location {
  int32_t lineno;
  mrb_sym method_id;
  const char *filename;
};

typedef void (*each_backtrace_func)(mrb_state*, const struct backtrace_location*, void*);

static const mrb_data_type bt_type = { "Backtrace", mrb_free };

mrb_value mrb_exc_inspect(mrb_state *mrb, mrb_value exc);
mrb_value mrb_unpack_backtrace(mrb_state *mrb, mrb_value backtrace);

static void
each_backtrace(mrb_state *mrb, ptrdiff_t ciidx, each_backtrace_func func, void *data)
{
  if (ciidx >= mrb->c->ciend - mrb->c->cibase)
    ciidx = 10; /* ciidx is broken... */

  for (ptrdiff_t i=ciidx; i >= 0; i--) {
    struct backtrace_location loc;
    mrb_callinfo *ci;
    const mrb_irep *irep = 0;
    const mrb_code *pc;
    uint32_t idx;

    ci = &mrb->c->cibase[i];

    if (!ci->proc || MRB_PROC_CFUNC_P(ci->proc)) {
      if (!ci->mid) continue;
      loc.lineno = -1;
      idx = 0;
    }
    else {
      irep = ci->proc->body.irep;
      if (!irep) continue;
      if (mrb->c->cibase[i].pc) {
        pc = &mrb->c->cibase[i].pc[-1];
      }
      else {
        continue;
      }
      idx = (uint32_t)(pc - irep->iseq);
      loc.lineno = mrb_debug_get_line(mrb, irep, idx);
    }
    loc.method_id = ci->mid;
    if (loc.lineno == -1) {
      for (ptrdiff_t j=i-1; j >= 0; j--) {
        ci = &mrb->c->cibase[j];

        if (!ci->proc) continue;
        if (MRB_PROC_CFUNC_P(ci->proc)) continue;

        irep = ci->proc->body.irep;
        if (!irep) continue;

        if (mrb->c->cibase[j].pc) {
          pc = &mrb->c->cibase[j].pc[-1];
        }
        else {
          continue;
        }

        idx = (uint32_t)(pc - irep->iseq);
        loc.lineno = mrb_debug_get_line(mrb, irep, idx);
        if (loc.lineno > 0) break;
      }
    }

    loc.filename = mrb_debug_get_filename(mrb, irep, idx);
    if (!loc.filename) {
      loc.filename = "(unknown)";
    }

    func(mrb, &loc, data);
  }
}

#ifndef MRB_NO_STDIO

static void
print_backtrace(mrb_state *mrb, struct RObject *exc, mrb_value backtrace)
{
  mrb_value mesg = mrb_exc_inspect(mrb, mrb_obj_value(exc));
  mrb_int n = RARRAY_LEN(backtrace);
  mrb_value *loc;

  if (n != 0) {
    loc = RARRAY_PTR(backtrace);
    if (mrb_string_p(*loc)) {
      fprintf(stderr, "%.*s: ", (int)RSTRING_LEN(*loc), RSTRING_PTR(*loc));
    }
    loc++;
    n--;
  }
  fwrite(RSTRING_PTR(mesg), RSTRING_LEN(mesg), 1, stderr);
  fputc('\n', stderr);
  for (; n > 0; n--, loc++) {
    if (mrb_string_p(*loc)) {
      fprintf(stderr, "\tfrom %.*s\n",
              (int)RSTRING_LEN(*loc), RSTRING_PTR(*loc));
    }
  }
}

/* mrb_print_backtrace

   function to retrieve backtrace information from the last exception.
*/

MRB_API void
mrb_print_backtrace(mrb_state *mrb)
{
  mrb_value backtrace;

  if (!mrb->exc) {
    return;
  }

  backtrace = mrb_obj_iv_get(mrb, mrb->exc, MRB_SYM(backtrace));
  if (mrb_nil_p(backtrace)) return;
  if (!mrb_array_p(backtrace)) backtrace = mrb_unpack_backtrace(mrb, backtrace);
  print_backtrace(mrb, mrb->exc, backtrace);
}
#else

MRB_API void
mrb_print_backtrace(mrb_state *mrb)
{
}

#endif

static void
count_backtrace_i(mrb_state *mrb,
                 const struct backtrace_location *loc,
                 void *data)
{
  int *lenp = (int*)data;

  (*lenp)++;
}

static void
pack_backtrace_i(mrb_state *mrb,
                 const struct backtrace_location *loc,
                 void *data)
{
  struct backtrace_location **pptr = (struct backtrace_location**)data;
  struct backtrace_location *ptr = *pptr;

  *ptr = *loc;
  *pptr = ptr+1;
}

static mrb_value
packed_backtrace(mrb_state *mrb)
{
  struct RData *backtrace;
  ptrdiff_t ciidx = mrb->c->ci - mrb->c->cibase;
  int len = 0;
  int size;
  void *ptr;

  each_backtrace(mrb, ciidx, count_backtrace_i, &len);
  size = len * sizeof(struct backtrace_location);
  ptr = mrb_malloc(mrb, size);
  backtrace = mrb_data_object_alloc(mrb, NULL, ptr, &bt_type);
  backtrace->flags = (uint32_t)len;
  each_backtrace(mrb, ciidx, pack_backtrace_i, &ptr);
  return mrb_obj_value(backtrace);
}

void
mrb_keep_backtrace(mrb_state *mrb, mrb_value exc)
{
  mrb_sym sym = MRB_SYM(backtrace);
  mrb_value backtrace;
  int ai;

  if (mrb_iv_defined(mrb, exc, sym)) return;
  ai = mrb_gc_arena_save(mrb);
  backtrace = packed_backtrace(mrb);
  mrb_iv_set(mrb, exc, sym, backtrace);
  mrb_gc_arena_restore(mrb, ai);
}

mrb_value
mrb_unpack_backtrace(mrb_state *mrb, mrb_value backtrace)
{
  const struct backtrace_location *bt;
  mrb_int n, i;
  int ai;

  if (mrb_nil_p(backtrace)) {
  empty_backtrace:
    return mrb_ary_new_capa(mrb, 0);
  }
  if (mrb_array_p(backtrace)) return backtrace;
  bt = (struct backtrace_location*)mrb_data_check_get_ptr(mrb, backtrace, &bt_type);
  if (bt == NULL) goto empty_backtrace;
  n = (mrb_int)RDATA(backtrace)->flags;
  if (n == 0) goto empty_backtrace;
  backtrace = mrb_ary_new_capa(mrb, n);
  ai = mrb_gc_arena_save(mrb);
  for (i = 0; i < n; i++) {
    const struct backtrace_location *entry = &bt[i];
    mrb_value btline;

    if (entry->lineno != -1) {//debug info was available
      btline = mrb_format(mrb, "%s:%d", entry->filename, (int)entry->lineno);
    }
    else { //all that was left was the stack frame
      btline = mrb_format(mrb, "%s:0", entry->filename);
    }
    if (entry->method_id != 0) {
      mrb_str_cat_lit(mrb, btline, ":in ");
      mrb_str_cat_cstr(mrb, btline, mrb_sym_name(mrb, entry->method_id));
    }
    mrb_ary_push(mrb, backtrace, btline);
    mrb_gc_arena_restore(mrb, ai);
  }

  return backtrace;
}

mrb_value
mrb_exc_backtrace(mrb_state *mrb, mrb_value exc)
{
  mrb_value backtrace;

  backtrace = mrb_iv_get(mrb, exc, MRB_SYM(backtrace));
  if (mrb_nil_p(backtrace) || mrb_array_p(backtrace)) {
    return backtrace;
  }
  /* unpack packed-backtrace */
  backtrace = mrb_unpack_backtrace(mrb, backtrace);
  mrb_iv_set(mrb, exc, MRB_SYM(backtrace), backtrace);
  return backtrace;
}

mrb_value
mrb_get_backtrace(mrb_state *mrb)
{
  return mrb_unpack_backtrace(mrb, packed_backtrace(mrb));
}
