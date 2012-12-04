/*
** dir.c - Dir
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/class.h"
#include "mruby/string.h"
#include "mruby/ext/io.h"
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#ifdef ENABLE_DIR
struct mrb_dir {
  DIR *dir;
};

void
mrb_dir_free(mrb_state *mrb, void *ptr)
{
  struct mrb_dir *mdir = ptr;

  if (mdir->dir) {
    closedir(mdir->dir);
    mdir->dir = NULL;
  }
  mrb_free(mrb, mdir);
}

static struct mrb_data_type mrb_dir_type = { "DIR", mrb_dir_free };

mrb_value
mrb_dir_close(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;
  mdir = (struct mrb_dir *)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->dir) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  if (closedir(mdir->dir) == -1) {
    mrb_sys_fail(mrb, "closedir");
  }
  mdir->dir = NULL;
  return mrb_nil_value();
}

mrb_value
mrb_dir_init(mrb_state *mrb, mrb_value self)
{
  DIR *dir;
  struct mrb_dir *mdir;
  mrb_value path;
  char *cpath;

  mdir = (struct mrb_dir *)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (mdir) {
    mrb_dir_free(mrb, mdir);
  }

  mdir = (struct mrb_dir *)mrb_malloc(mrb, sizeof(*mdir));
  mdir->dir = NULL;
  DATA_PTR(self) = mdir;
  DATA_TYPE(self) = &mrb_dir_type;

  mrb_get_args(mrb, "S", &path);
  cpath = mrb_str_to_cstr(mrb, path);
  if ((dir = opendir(cpath)) == NULL) {
    mrb_sys_fail(mrb, cpath);
  }
  mdir->dir = dir;
  return self;
}

mrb_value
mrb_dir_delete(mrb_state *mrb, mrb_value klass)
{
  mrb_value path;
  char *cpath;

  mrb_get_args(mrb, "S", &path);
  cpath = mrb_str_to_cstr(mrb, path);
  if (rmdir(cpath) == -1) {
    mrb_sys_fail(mrb, cpath);
  }
  return mrb_fixnum_value(0);
}

mrb_value
mrb_dir_existp(mrb_state *mrb, mrb_value klass)
{
  mrb_value path;
  struct stat sb;
  char *cpath;

  mrb_get_args(mrb, "S", &path);
  cpath = mrb_str_to_cstr(mrb, path);
  if (stat(cpath, &sb) == 0 && S_ISDIR(sb.st_mode)) {
    return mrb_true_value();
  } else {
    return mrb_false_value();
  }
}

mrb_value
mrb_dir_getwd(mrb_state *mrb, mrb_value klass)
{
  mrb_value path;

  path = mrb_str_buf_new(mrb, MAXPATHLEN);
  if (getcwd(RSTRING_PTR(path), MAXPATHLEN) == NULL) {
    mrb_sys_fail(mrb, "getcwd(2)");
  }
  mrb_str_resize(mrb, path, strlen(RSTRING_PTR(path)));
  return path;
}

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

mrb_value
mrb_dir_read(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;
  struct dirent *dp;

  mdir = (struct mrb_dir *)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->dir) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  dp = readdir(mdir->dir);
  if (dp != NULL) {
    return mrb_str_new2(mrb, dp->d_name);
  } else {
    return mrb_nil_value();
  }
}

mrb_value
mrb_dir_rewind(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;

  mdir = (struct mrb_dir *)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->dir) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  rewinddir(mdir->dir);
  return self;
}

mrb_value
mrb_dir_seek(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;
  mrb_int pos;

  mdir = (struct mrb_dir *)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->dir) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  mrb_get_args(mrb, "i", &pos);
  seekdir(mdir->dir, (long)pos);
  return self;
}

mrb_value
mrb_dir_tell(mrb_state *mrb, mrb_value self)
{
  struct mrb_dir *mdir;
  mrb_int pos;

  mdir = (struct mrb_dir *)mrb_get_datatype(mrb, self, &mrb_dir_type);
  if (!mdir) return mrb_nil_value();
  if (!mdir->dir) {
    mrb_raise(mrb, E_IO_ERROR, "closed directory");
  }
  pos = (mrb_int)telldir(mdir->dir);
  return mrb_fixnum_value(pos);
}

void
mrb_init_dir(mrb_state *mrb)
{
  struct RClass *d;

  d = mrb_define_class(mrb, "Dir", mrb->object_class);
  MRB_SET_INSTANCE_TT(d, MRB_TT_DATA);
  mrb_define_class_method(mrb, d, "delete", mrb_dir_delete, ARGS_REQ(1));
  mrb_define_class_method(mrb, d, "exist?", mrb_dir_existp, ARGS_REQ(1));
  mrb_define_class_method(mrb, d, "getwd", mrb_dir_getwd, ARGS_NONE());
  mrb_define_class_method(mrb, d, "mkdir", mrb_dir_mkdir, ARGS_REQ(1)|ARGS_OPT(1));
  mrb_define_method(mrb, d, "close", mrb_dir_close, ARGS_NONE());
  mrb_define_method(mrb, d, "initialize", mrb_dir_init, ARGS_REQ(1));
  mrb_define_method(mrb, d, "read", mrb_dir_read, ARGS_NONE());
  mrb_define_method(mrb, d, "rewind", mrb_dir_rewind, ARGS_NONE());
  mrb_define_method(mrb, d, "seek", mrb_dir_seek, ARGS_REQ(1));
  mrb_define_method(mrb, d, "tell", mrb_dir_tell, ARGS_NONE());
}

#endif /* ENABLE_DIR */
