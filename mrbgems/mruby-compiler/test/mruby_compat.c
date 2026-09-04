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

void
mrb_mruby_compiler_gem_test(mrb_state *mrb)
{
#ifndef MRB_NO_STDIO
  mrb_define_method(mrb, mrb->object_class, "load_file_exc", load_file_exc, MRB_ARGS_REQ(1));
#endif
}
