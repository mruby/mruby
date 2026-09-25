/*
** Test helpers for the C-level load API.
**
** The read-failure path is only reachable from C: no core method hands a
** FILE* to mrb_load_file_cxt(), so the Ruby suite needs a door onto it.
*/

#include <mruby.h>

#ifndef MRB_NO_STDIO

#include <mruby/compile.h>
#include <mruby/string.h>
#include <stdio.h>

/*
 * Opens `path`, loads it as source through mrb_load_file_cxt(), and answers
 * the exception that was left behind, or false when none was.  Answers nil
 * when the platform refuses to open `path` at all: Windows does that for a
 * directory, and never reaches the reader under test.
 *
 * The exception is taken off the state rather than propagated, so the caller
 * examines it as an object instead of rescuing it.
 */
static mrb_value
load_file_exc(mrb_state *mrb, mrb_value self)
{
  const char *path;
  FILE *f;
  mrb_ccontext *c;
  mrb_value exc = mrb_false_value();

  mrb_get_args(mrb, "z", &path);
  f = fopen(path, "r");
  if (f == NULL) return mrb_nil_value();

  c = mrb_ccontext_new(mrb);
  mrb_ccontext_filename(mrb, c, path);
  mrb_load_file_cxt(mrb, f, c);
  fclose(f);
  mrb_ccontext_free(mrb, c);

  if (mrb->exc) {
    exc = mrb_obj_value(mrb->exc);
    mrb->exc = NULL;
  }
  return exc;
}

#endif /* MRB_NO_STDIO */

#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/irep.h>
#include <mruby/dump.h>
#include <mruby/array.h>
#include <mruby/string.h>
#include <string.h>

/*
 * A float literal in bytecode from an mrbc with Float, run where there is
 * none: the no-float build's own mrbc never writes one, so the entry is made
 * here. Compiles `src`, turns its first integer pool entry into the float 1.5,
 * dumps that, and answers [same, value]: whether reading the dump back and
 * dumping it again gives the same bytes (what mruby-strip does), and what
 * running the dump answers. An exception from the run is raised. Answers
 * nil where `src` does not compile to an integer pool entry.
 */
static mrb_value
float_pool_roundtrip(mrb_state *mrb, mrb_value self)
{
  const char *src;
  mrb_get_args(mrb, "z", &src);

  mrb_ccontext *c = mrb_ccontext_new(mrb);
  c->no_exec = TRUE;
  mrb_value proc = mrb_load_string_cxt(mrb, src, c);
  mrb_ccontext_free(mrb, c);
  if (mrb->exc || !mrb_proc_p(proc)) {
    mrb->exc = NULL;
    return mrb_nil_value();
  }

  mrb_irep *irep = (mrb_irep*)mrb_proc_ptr(proc)->body.irep;
  mrb_irep_pool *pool = (mrb_irep_pool*)irep->pool;
  int k;
  for (k = 0; k < irep->plen; k++) {
    if (pool[k].tt == IREP_TT_INT32 || pool[k].tt == IREP_TT_INT64) break;
  }
  if (k == irep->plen) return mrb_nil_value();

  pool[k].tt = IREP_TT_FLOAT;
#ifndef MRB_NO_FLOAT
  pool[k].u.f = 1.5;
#else
  {
    /* 1.5 as a .mrb holds it, the bytes load.c keeps */
    static const uint8_t bits[MRB_DUMP_FLOAT_SIZE] = {0, 0, 0, 0, 0, 0, 0xf8, 0x3f};
    memcpy(&pool[k].u.i64, bits, sizeof(bits));
  }
#endif

  uint8_t *bin1 = NULL, *bin2 = NULL;
  size_t size1 = 0, size2 = 0;
  if (mrb_dump_irep(mrb, irep, 0, &bin1, &size1) != MRB_DUMP_OK) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "dump failed");
  }
  mrb_irep *again = mrb_read_irep_buf(mrb, bin1, size1);
  mrb_bool same = FALSE;
  if (again) {
    if (mrb_dump_irep(mrb, again, 0, &bin2, &size2) == MRB_DUMP_OK) {
      same = (size1 == size2 && memcmp(bin1, bin2, size1) == 0);
    }
    mrb_irep_decref(mrb, again);
    mrb_free(mrb, bin2);
  }

  /* Running the dump can raise past this frame, so the bytes move into a
     String the GC owns before it runs. */
  mrb_value buf = mrb_str_new(mrb, (const char*)bin1, (mrb_int)size1);
  mrb_free(mrb, bin1);
  mrb_value v = mrb_load_irep_buf(mrb, RSTRING_PTR(buf), size1);
  if (mrb->exc) {
    mrb_value e = mrb_obj_value(mrb->exc);
    mrb->exc = NULL;
    mrb_exc_raise(mrb, e);
  }
  return mrb_assoc_new(mrb, mrb_bool_value(same), v);
}

/*
 * Two parser states alive at once, the older one freed first.  A parser
 * state keeps its compiler context, and with it everything Prism allocated
 * for the parse, until mrb_parser_free(); a caller that holds several (an
 * interactive shell with a context per session, say) frees them in whatever
 * order it likes.  Answers what the source of the parser that outlived the
 * other evaluates to.
 */
static mrb_value
parsers_outlive_each_other(mrb_state *mrb, mrb_value self)
{
  const char *first, *second;
  mrb_get_args(mrb, "zz", &first, &second);

  mrb_ccontext *c = mrb_ccontext_new(mrb);
  struct mrb_parser_state *p1 = mrb_parse_string(mrb, first, c);
  struct mrb_parser_state *p2 = mrb_parse_string(mrb, second, c);
  /* Must give back what p1 took and nothing of p2's. */
  mrb_parser_free(p1);
  struct RProc *proc = mrb_generate_code(mrb, p2);
  mrb_parser_free(p2);
  mrb_ccontext_free(mrb, c);
  if (proc == NULL) return mrb_nil_value();

  return mrb_top_run(mrb, proc, mrb_top_self(mrb), 0);
}

void
mrb_mruby_compiler_gem_test(mrb_state *mrb)
{
#ifndef MRB_NO_STDIO
  mrb_define_method(mrb, mrb->object_class, "load_file_exc", load_file_exc, MRB_ARGS_REQ(1));
#endif
  mrb_define_method(mrb, mrb->object_class, "__float_pool_roundtrip", float_pool_roundtrip, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, mrb->object_class, "__parsers_outlive_each_other", parsers_outlive_each_other, MRB_ARGS_REQ(2));
}
