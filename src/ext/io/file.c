/*
** file.c - File class
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#ifdef ENABLE_IO

#include "mruby/ext/io.h"
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/string.h"
#include "error.h"

#include <libgen.h>

/*********************************************************/
#define STAT(p, s)        stat(p, s)

/*********************************************************/

static mrb_value
mrb_file_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value *argv;
  int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc < 3) argv[2] = mrb_nil_value();
  if (argc < 2) argv[1] = mrb_nil_value();
  if (RFILE(self)->fptr) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "reinitializing File");
  }
  if (argc == 1 || argc == 2) {
    mrb_value fd = mrb_check_convert_type(mrb, argv[0], MRB_TT_FIXNUM, "Fixnum", "to_int");

    if (!mrb_nil_p(fd)) {
      /* first argument is file descriptor */
      argv[0] = fd;
      return rb_io_initialize(mrb, argc, argv, self);
    }
  }
  mrb_open_file(mrb, argc, argv, self);
  return self;
}

mrb_value
mrb_file_s_open(mrb_state *mrb, mrb_value klass)
{
  struct RFile *file;
  file = (struct RFile*)mrb_obj_alloc(mrb, MRB_TT_FILE, mrb_class_obj_get(mrb, "File"));
  mrb_value io = mrb_obj_value(file);
  return mrb_file_initialize(mrb, io);
}

mrb_value
mrb_file_s_umask(mrb_state *mrb, mrb_value klass)
{
  mrb_value *argv;
  int argc;
  mrb_get_args(mrb, "*", &argv, &argc);

  int omask = 0;
  if (argc == 0) {
    omask = umask(0);
    umask(omask);
  } else if (argc == 1) {
    mrb_value mask = argv[0];
    if (!mrb_nil_p(mask) && !FIXNUM_P(mask)) {
      mask = mrb_check_convert_type(mrb, mask, MRB_TT_FIXNUM, "Fixnum", "to_int");
    }
    if (!FIXNUM_P(mask)) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid argument type");
    }
    omask = umask(mrb_fixnum(mask));
  } else {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (%d for 0..1)", argc);
  }
  return mrb_fixnum_value(omask);
}

static mrb_value
mrb_file_path(mrb_state *mrb, mrb_value self)
{
  struct mrb_io *fptr;

  fptr = RFILE(self)->fptr;
  mrb_io_check_initialized(mrb, fptr);
  if (mrb_nil_p(fptr->path)) return mrb_nil_value();
  return mrb_str_dup(mrb, fptr->path);
}

static mrb_value
mrb_file_dirname(mrb_state *mrb, mrb_value self)
{
  char *path, *dname;
  mrb_value dir, fname;

  mrb_get_args(mrb, "o", &fname);
  path = mrb_string_value_cstr(mrb, &fname);

  if ((dname = dirname(path)) == NULL) {
    mrb_sys_fail(mrb, "mrb_file_dirname failed.");
  }

  dir = mrb_str_new(mrb, dname, strlen(dname));

  return dir;
}

static int
mrb_stat(mrb_state *mrb, mrb_value file, struct stat *st)
{
  mrb_value tmp;

  tmp = mrb_check_convert_type(mrb, file, MRB_TT_FILE, "IO", "to_io");
  if (!mrb_nil_p(tmp)) {
    struct mrb_io *fptr;

    GetOpenFile(mrb, tmp, fptr);
    return fstat(fileno(fptr->f), st);
  }
  mrb_string_value(mrb, &file);
  char* tmpStr = mrb_string_value_cstr(mrb, &file);
  return STAT(tmpStr, st);
}

static mrb_value
mrb_file_exist_p(mrb_state *mrb, mrb_value obj)
{
  mrb_value fname;
  struct stat st;

  mrb_get_args(mrb, "o", &fname);

  if (mrb_stat(mrb, fname, &st) < 0) return mrb_false_value();
  return mrb_true_value();
}

static mrb_value
mrb_file_s_unlink(mrb_state *mrb, mrb_value obj)
{
  mrb_value *argv;
  int n, i, argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  for (i = 0, n = 0; i < argc; i++) {
    mrb_value pathv = argv[i];
    if (mrb_type(pathv) != MRB_TT_STRING) {
      mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, pathv));
    }
    const char *path = mrb_string_value_cstr(mrb, &pathv);;
    if (unlink(path) < 0) {
      mrb_sys_fail(mrb, path);
    } else {
      n++;
    }
  }
  return mrb_fixnum_value(n);
}

static mrb_value
mrb_file_rename_internal(mrb_state *mrb, mrb_value from, mrb_value to)
{
  const char *src, *dst;
  src = mrb_string_value_cstr(mrb, &from);
  dst = mrb_string_value_cstr(mrb, &to);

  if (rename(src, dst) < 0) {
    if (chmod(dst, 0666) == 0 &&
	    unlink(dst) == 0 &&
	    rename(src, dst) == 0)
	return mrb_fixnum_value(0);
    mrb_sys_fail(mrb, "mrb_file_rename_internal failed.");
  }

  return mrb_fixnum_value(0);
}

static mrb_value
mrb_file_s_rename(mrb_state *mrb, mrb_value obj)
{
  mrb_value *argv;
  int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc != 2) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (%d for 2)", argc);
    return mrb_nil_value();
  }
  if (mrb_type(argv[0]) != MRB_TT_STRING) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, argv[0]));
    return mrb_nil_value();
  }
  if (mrb_type(argv[1]) != MRB_TT_STRING) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, argv[1]));
    return mrb_nil_value();
  }

  return mrb_file_rename_internal(mrb, argv[0], argv[1]);
}


void
mrb_init_file(mrb_state *mrb)
{
  struct RClass *cIO;
  struct RClass *file;

  cIO = mrb_class_obj_get(mrb, "IO");
  file = mrb_define_class(mrb, "File", cIO);
  MRB_SET_INSTANCE_TT(file, MRB_TT_FILE);
  mrb_define_class_method(mrb, file, "open", mrb_file_s_open, ARGS_ANY());
  mrb_define_class_method(mrb, file, "umask", mrb_file_s_umask, ARGS_REQ(1));
  mrb_define_class_method(mrb, file, "unlink", mrb_file_s_unlink, ARGS_ANY());
  mrb_define_class_method(mrb, file, "delete", mrb_file_s_unlink, ARGS_ANY());
  mrb_define_class_method(mrb, file, "rename", mrb_file_s_rename, ARGS_REQ(2));

  mrb_define_class_method(mrb, file, "exist?",     mrb_file_exist_p,    ARGS_REQ(1));              /* 15.2.21.3.1  */
  mrb_define_class_method(mrb, file, "exists?",    mrb_file_exist_p,    ARGS_REQ(1));              /* 15.2.21.3.1  */
  mrb_define_method(mrb, file,       "initialize", mrb_file_initialize, ARGS_ANY());               /* 15.2.21.4.1  */
  mrb_define_method(mrb, file,       "path",       mrb_file_path,       ARGS_NONE());              /* 15.2.21.4.2  */
  mrb_define_class_method(mrb, file, "dirname",   mrb_file_dirname,    ARGS_REQ(1));
}

#endif /* ENABLE_IO */
