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
#include <mruby/internal.h>

#define MAX_IREP_REFCNT UINT16_MAX
#define UNKNOWN_LINENO -1
#define UNKNOWN_LOCATION "(unknown):0"

static void
copy_backtrace(mrb_state *mrb,
                 const struct mrb_backtrace_location *loc,
                 struct mrb_backtrace_location *ptr,
                 size_t n)
{
  ptr[n] = *loc;
  if (loc->irep) {
    if (loc->irep->refcnt == MAX_IREP_REFCNT) {
      ptr[n].irep = NULL;
    }
    else {
      mrb_irep_incref(mrb, (mrb_irep*)loc->irep);
    }
  }
}

/**
 * Creates a packed backtrace from the current call stack
 *
 * @param mrb The mruby state
 * @param ciidx The current callinfo index
 * @param ptr Pointer to store the backtrace locations
 * @return Number of backtrace entries
 * @note This function handles both proc and non-proc cases, managing IREP references
 *       and building location entries for each stack frame.
 */
static size_t
pack_backtrace(mrb_state *mrb, ptrdiff_t ciidx, struct mrb_backtrace_location *ptr)
{
  size_t n = 0;

  for (ptrdiff_t i=ciidx; i >= 0; i--) {
    struct mrb_backtrace_location loc;
    mrb_callinfo *ci;
    const mrb_code *pc;

    ci = &mrb->c->cibase[i];
    loc.method_id = ci->mid;

    if (ci->proc && !MRB_PROC_CFUNC_P(ci->proc)) {
      mrb_assert(!MRB_PROC_ALIAS_P(ci->proc));
      loc.irep = ci->proc->body.irep;
      if (!loc.irep) continue;
      if (!loc.irep->debug_info) continue;
      if (!ci->pc) continue;
      pc = &ci->pc[-1];
      loc.idx = (uint32_t)(pc - loc.irep->iseq);
    }
    else {
      if (!loc.method_id) continue;
      loc.irep = NULL;
      for (ptrdiff_t j=i-1; j >= 0; j--) {
        ci = &mrb->c->cibase[j];

        if (!ci->proc) continue;
        if (MRB_PROC_CFUNC_P(ci->proc)) continue;
        mrb_assert(!MRB_PROC_ALIAS_P(ci->proc));

        const mrb_irep *irep = ci->proc->body.irep;
        if (!irep) continue;
        if (!irep->debug_info) continue;
        if (!ci->pc) continue;
        pc = &ci->pc[-1];
        loc.irep = irep;
        loc.idx = (uint32_t)(pc - irep->iseq);
        break;
      }
    }
    copy_backtrace(mrb, &loc, ptr, n);
    n++;
  }
  return n;
}

static struct RBasic*
packed_backtrace(mrb_state *mrb)
{
  ptrdiff_t ciidx = mrb->c->ci - mrb->c->cibase;

  if (ciidx >= mrb->c->ciend - mrb->c->cibase)
    ciidx = mrb->c->ciend - mrb->c->cibase; /* ciidx is broken... */

  ptrdiff_t len = ciidx + 1;

  struct RBacktrace *backtrace = MRB_OBJ_ALLOC(mrb, MRB_TT_BACKTRACE, NULL);

  void *ptr = mrb_malloc(mrb, len * sizeof(struct mrb_backtrace_location));
  backtrace->locations = (struct mrb_backtrace_location*)ptr;
  backtrace->len = pack_backtrace(mrb, ciidx, backtrace->locations);

  return (struct RBasic*)backtrace;
}

static void
store_backtrace(mrb_state *mrb, mrb_value exc, struct RBasic *backtrace)
{
  struct RException *e = mrb_exc_ptr(exc);
  e->backtrace = backtrace;
  mrb_field_write_barrier(mrb, (struct RBasic*)e, backtrace);
}

void
mrb_keep_backtrace(mrb_state *mrb, mrb_value exc)
{
  int ai;

  if (mrb->c->ci == NULL) return;
  if (mrb_exc_ptr(exc)->backtrace) return;
  ai = mrb_gc_arena_save(mrb);
  struct RBasic *backtrace = packed_backtrace(mrb);
  store_backtrace(mrb, exc, backtrace);
  mrb_gc_arena_restore(mrb, ai);
}

static mrb_value
decode_location(mrb_state *mrb, const struct mrb_backtrace_location *entry)
{
  mrb_value btline;
  int32_t lineno;
  const char *filename;

  // Case 1: No IREP or debug info available
  if (!entry->irep) {
    return mrb_str_new_lit(mrb, UNKNOWN_LOCATION);
  }

  // Case 2: Debug info lookup failed
  if (!mrb_debug_get_position(mrb, entry->irep, entry->idx, &lineno, &filename)) {
    return mrb_str_new_lit(mrb, UNKNOWN_LOCATION);
  }

  // Case 3: Valid debug info
  if (lineno != UNKNOWN_LINENO) {
    btline = mrb_format(mrb, "%s:%d", filename, (int)lineno);
  }
  else { //all that was left was the stack frame
    btline = mrb_format(mrb, "%s:0", filename);
  }

  // Add method name if available
  if (entry->method_id != 0) {
    mrb_str_cat_lit(mrb, btline, ":in ");
    mrb_str_cat_cstr(mrb, btline, mrb_sym_name(mrb, entry->method_id));
  }

  return btline;
}

static struct RBasic*
mrb_unpack_backtrace(mrb_state *mrb, struct RBasic *backtrace)
{
  if (backtrace == NULL) {
    return mrb_basic_ptr(mrb_ary_new_capa(mrb, 0));
  }
  if (backtrace->tt == MRB_TT_ARRAY) return backtrace;

  mrb_assert(backtrace->tt == MRB_TT_BACKTRACE);

  struct RBacktrace *bt = (struct RBacktrace*)backtrace;
  mrb_int n = (mrb_int)bt->len;
  const struct mrb_backtrace_location *loc = bt->locations;

  backtrace = mrb_basic_ptr(mrb_ary_new_capa(mrb, n));
  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < n; i++) {
    mrb_value btline = decode_location(mrb, &loc[i]);
    mrb_ary_push(mrb, mrb_obj_value(backtrace), btline);
    mrb_gc_arena_restore(mrb, ai);
  }

  return backtrace;
}

mrb_value
mrb_exc_backtrace(mrb_state *mrb, mrb_value exc)
{
  struct RBasic *backtrace = mrb_exc_ptr(exc)->backtrace;
  if (backtrace == NULL) {
    return mrb_nil_value();
  }
  if (backtrace->tt == MRB_TT_ARRAY) {
    return mrb_obj_value(backtrace);
  }
  /* the packed backtrace stays, for backtrace_locations; the strings
     unpacked from it are kept beside it */
  struct RObject *e = mrb_obj_ptr(exc);
  mrb_value bt = mrb_obj_iv_get(mrb, e, MRB_SYM(__backtrace__));
  if (mrb_array_p(bt)) return bt;
  bt = mrb_obj_value(mrb_unpack_backtrace(mrb, backtrace));
  mrb_obj_iv_set_force(mrb, e, MRB_SYM(__backtrace__), bt);
  return bt;
}

mrb_value
mrb_get_backtrace(mrb_state *mrb)
{
  return mrb_obj_value(mrb_unpack_backtrace(mrb, packed_backtrace(mrb)));
}

/* The current stack, packed; with mrb_backtrace_len and
   mrb_backtrace_location, read an entry at a time. */
mrb_value
mrb_get_packed_backtrace(mrb_state *mrb)
{
  return mrb_obj_value(packed_backtrace(mrb));
}

/* The packed backtrace of a raised exception; nil before it is raised, or
   once set_backtrace has replaced it. */
mrb_value
mrb_exc_packed_backtrace(mrb_state *mrb, mrb_value exc)
{
  struct RBasic *backtrace = mrb_exc_ptr(exc)->backtrace;
  if (backtrace == NULL || backtrace->tt != MRB_TT_BACKTRACE) {
    return mrb_nil_value();
  }
  return mrb_obj_value(backtrace);
}

mrb_int
mrb_backtrace_len(mrb_value bt)
{
  return (mrb_int)((struct RBacktrace*)mrb_basic_ptr(bt))->len;
}

/* Entry `i` of a packed backtrace: its file and line, FALSE when they are
   unknown, and its method, 0 for none. The string is the one Kernel#caller
   and Exception#backtrace give for it. */
mrb_bool
mrb_backtrace_location(mrb_state *mrb, mrb_value bt, mrb_int i,
                       const char **file, int32_t *lineno, mrb_sym *mid, mrb_value *str)
{
  const struct mrb_backtrace_location *entry = &((struct RBacktrace*)mrb_basic_ptr(bt))->locations[i];

  if (mid) *mid = entry->method_id;
  if (str) *str = decode_location(mrb, entry);
  if (!entry->irep) return FALSE;
  return mrb_debug_get_position(mrb, entry->irep, entry->idx, lineno, file);
}

#ifndef MRB_NO_STDIO

static void
print_backtrace(mrb_state *mrb, struct RObject *exc, struct RBasic *ptr)
{
  struct RArray *ary = NULL;
  struct RBacktrace *bt = NULL;
  mrb_int n = 0;

  if (ptr) {
    if (ptr->tt == MRB_TT_ARRAY) {
      ary = (struct RArray*)ptr;
      n = ARY_LEN(ary);
    }
    else {
      bt = (struct RBacktrace*)ptr;
      n = (mrb_int)bt->len;
    }
  }

  if (n != 0) {
    mrb_value btline;

    fputs("trace (most recent call last):\n", stderr);
    for (mrb_int i=n-1; i>0; i--) {
      if (ary) btline = ARY_PTR(ary)[i];
      else btline = decode_location(mrb, &bt->locations[i]);
      if (mrb_string_p(btline)) {
        fprintf(stderr, "\t[%d] ", (int)i);
        fwrite(RSTRING_PTR(btline), (int)RSTRING_LEN(btline), 1, stderr);
        fputc('\n', stderr);
      }
    }
    if (ary) btline = ARY_PTR(ary)[0];
    else btline = decode_location(mrb, &bt->locations[0]);
    if (mrb_string_p(btline)) {
      fwrite(RSTRING_PTR(btline), (int)RSTRING_LEN(btline), 1, stderr);
      fputs(": ", stderr);
    }
  }
  else {
    fputs(UNKNOWN_LOCATION ": ", stderr);
  }

  if (exc == mrb->nomem_err) {
    static const char nomem[] = "Out of memory (NoMemoryError)\n";
    fwrite(nomem, sizeof(nomem)-1, 1, stderr);
  }
  else {
    mrb_value output = mrb_exc_get_output(mrb, exc);
    fwrite(RSTRING_PTR(output), RSTRING_LEN(output), 1, stderr);
    fputc('\n', stderr);
  }
}

/* mrb_print_backtrace

   function to retrieve backtrace information from the last exception.
*/

MRB_API void
mrb_print_backtrace(mrb_state *mrb)
{
  if (!mrb->exc || mrb->exc->tt != MRB_TT_EXCEPTION) {
    return;
  }

  print_backtrace(mrb, mrb->exc, ((struct RException*)mrb->exc)->backtrace);
}
#else
MRB_API void
mrb_print_backtrace(mrb_state *mrb)
{
}
#endif
