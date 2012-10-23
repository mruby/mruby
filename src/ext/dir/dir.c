/*
** dir.c - Dir
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/class.h"
#include "mruby/string.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#ifdef ENABLE_DIR
mrb_value
mrb_dir_mkdir(mrb_state *mrb, mrb_value klass)
{
  mrb_int mode;
  mrb_value spath;
  char *path;

  mode = 0777;
  mrb_get_args(mrb, "S|i", &spath, &mode);
  path = mrb_str_to_cstr(mrb, spath);
  if (mkdir(path, mode) == -1) {
    mrb_sys_fail(mrb, path);
  }
  return mrb_fixnum_value(0);
}

void
mrb_init_dir(mrb_state *mrb)
{
  struct RClass *d;

  d = mrb_define_class(mrb, "Dir", mrb->object_class);
  mrb_define_class_method(mrb, d, "mkdir", mrb_dir_mkdir, ARGS_REQ(1)|ARGS_OPT(1));

}

#endif /* ENABLE_DIR */
