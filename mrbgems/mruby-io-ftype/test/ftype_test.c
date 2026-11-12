#include <mruby/common.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#include <direct.h>
#include <io.h>
/* mingw-w64 has mkdtemp() since 14.0 */
#if !defined(MRB_MINGW64_VERSION) || MRB_MINGW64_VERSION < 14000
static char*
mkdtemp(char *temp)
{
  char *path = _mktemp(temp);
  if (path == NULL || path[0] == 0) return NULL;
  if (_mkdir(path) < 0) return NULL;
  return path;
}
#endif
#define rmdir(path) _rmdir(path)
#else
#include <unistd.h>
#endif

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/error.h>

/* a new directory under the current one, for a test to put files in */
static mrb_value
ftype_test_mkdtemp(mrb_state *mrb, mrb_value klass)
{
  char path[] = "mruby-io-ftype-test.XXXXXX";

  if (mkdtemp(path) == NULL) mrb_sys_fail(mrb, "mkdtemp");
  return mrb_str_new_cstr(mrb, path);
}

static mrb_value
ftype_test_rmdir(mrb_state *mrb, mrb_value klass)
{
  const char *path;

  mrb_get_args(mrb, "z", &path);
  if (rmdir(path) == -1) mrb_sys_fail(mrb, "rmdir");
  return mrb_nil_value();
}

static mrb_value
ftype_test_win_p(mrb_state *mrb, mrb_value klass)
{
#if defined(_WIN32) && !defined(__CYGWIN__)
  return mrb_true_value();
#else
  return mrb_false_value();
#endif
}

void
mrb_mruby_io_ftype_gem_test(mrb_state *mrb)
{
  struct RClass *m = mrb_define_module(mrb, "FtypeTest");

  mrb_define_class_method(mrb, m, "mkdtemp", ftype_test_mkdtemp, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, m, "rmdir", ftype_test_rmdir, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, m, "win?", ftype_test_win_p, MRB_ARGS_NONE());
}
