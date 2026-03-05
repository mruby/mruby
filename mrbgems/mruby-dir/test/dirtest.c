#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include "dir_hal.h"

#if defined(_WIN32)
#include <io.h>
#include <direct.h>
#else
#include <unistd.h>
#endif

static void
make_dir(mrb_state *mrb, const char *name, const char *up)
{
  if (mrb_hal_dir_mkdir(mrb, name, 0) == -1) {
    if (mrb_hal_dir_chdir(mrb, "..") == 0) {
      mrb_hal_dir_rmdir(mrb, up);
    }
    mrb_raisef(mrb, E_RUNTIME_ERROR, "mkdir(%s) failed", mrb_str_new_cstr(mrb, name));
  }
}

mrb_value
mrb_dirtest_setup(mrb_state *mrb, mrb_value klass)
{
  char buf[2048];
  char cwd[1024];
  const char *aname = "a";
  const char *bname = "b";

  /* save current working directory */
  if (mrb_hal_dir_getcwd(mrb, cwd, sizeof(cwd)) != 0) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "getcwd() failed");
  }
  mrb_cv_set(mrb, klass, mrb_intern_cstr(mrb, "pwd"), mrb_str_new_cstr(mrb, cwd));

  /* create sandbox */
#if defined(_WIN32)
  snprintf(buf, sizeof(buf), "%s\\mruby-dir-test.XXXXXX", cwd);
  if ((_mktemp(buf) == NULL) || mrb_hal_dir_mkdir(mrb, buf, 0) != 0) {
    mrb_raisef(mrb, E_RUNTIME_ERROR, "mkdtemp(%s) failed", buf);
  }
#else
  snprintf(buf, sizeof(buf), "%s/mruby-dir-test.XXXXXX", P_tmpdir);
  if (mkdtemp(buf) == NULL) {
    mrb_raisef(mrb, E_RUNTIME_ERROR, "mkdtemp(%s) failed", buf);
  }
#endif
  mrb_cv_set(mrb, klass, mrb_intern_cstr(mrb, "sandbox"), mrb_str_new_cstr(mrb, buf));

  /* go to sandbox */
  if (mrb_hal_dir_chdir(mrb, buf) == -1) {
    mrb_hal_dir_rmdir(mrb, buf);
    mrb_raisef(mrb, E_RUNTIME_ERROR, "chdir(%s) failed", buf);
  }

  /* make some directories in the sandbox */
  make_dir(mrb, aname, buf);
  make_dir(mrb, bname, buf);

  return mrb_true_value();
}

mrb_value
mrb_dirtest_teardown(mrb_state *mrb, mrb_value klass)
{
  mrb_value d, sandbox;
  mrb_dir_handle *dirp;
  const char *name;
  const char *path;

  /* cleanup sandbox */
  sandbox = mrb_cv_get(mrb, klass, mrb_intern_cstr(mrb, "sandbox"));
  path = mrb_str_to_cstr(mrb, sandbox);

  dirp = mrb_hal_dir_open(mrb, path);
  while ((name = mrb_hal_dir_read(mrb, dirp)) != NULL) {
    if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0)
      continue;
    if (mrb_hal_dir_rmdir(mrb, name) == -1) {
      mrb_raisef(mrb, E_RUNTIME_ERROR, "rmdir(%s) failed", name);
    }
  }
  mrb_hal_dir_close(mrb, dirp);

  /* back to original pwd */
  d = mrb_cv_get(mrb, klass, mrb_intern_cstr(mrb, "pwd"));
  path = mrb_str_to_cstr(mrb, d);
  if (mrb_hal_dir_chdir(mrb, path) == -1) {
    mrb_raisef(mrb, E_RUNTIME_ERROR, "chdir(%s) failed", path);
  }

  /* remove sandbox directory */
  sandbox = mrb_cv_get(mrb, klass, mrb_intern_cstr(mrb, "sandbox"));
  path = mrb_str_to_cstr(mrb, sandbox);
  if (mrb_hal_dir_rmdir(mrb, path) == -1) {
    mrb_raisef(mrb, E_RUNTIME_ERROR, "rmdir(%s) failed", path);
  }

  return mrb_true_value();
}

mrb_value
mrb_dirtest_sandbox(mrb_state *mrb, mrb_value klass)
{
  return mrb_cv_get(mrb, klass, mrb_intern_cstr(mrb, "sandbox"));
}

void
mrb_mruby_dir_gem_test(mrb_state *mrb)
{
  struct RClass *c = mrb_define_module(mrb, "DirTest");

  mrb_define_class_method(mrb, c, "sandbox", mrb_dirtest_sandbox, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, c, "setup", mrb_dirtest_setup, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, c, "teardown", mrb_dirtest_teardown, MRB_ARGS_NONE());
}
