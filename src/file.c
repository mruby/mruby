/*
** file.c - File class
*/

#include "mruby.h"

#include "mruby/ext/io.h"
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/string.h"
#include "error.h"

#include <sys/file.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <pwd.h>

#define FILE_SEPARATOR "/"

#ifndef LOCK_SH
#define LOCK_SH 1
#endif
#ifndef LOCK_EX
#define LOCK_EX 2
#endif
#ifndef LOCK_NB
#define LOCK_NB 4
#endif
#ifndef LOCK_UN
#define LOCK_UN 8
#endif

#define STAT(p, s)        stat(p, s)

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
    if (!mrb_nil_p(mask) && !mrb_fixnum_p(mask)) {
      mask = mrb_check_convert_type(mrb, mask, MRB_TT_FIXNUM, "Fixnum", "to_int");
    }
    if (!mrb_fixnum_p(mask)) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid argument type");
    }
    omask = umask(mrb_fixnum(mask));
  } else {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (%d for 0..1)", argc);
  }
  return mrb_fixnum_value(omask);
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
      mrb_raisef(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, pathv));
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
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (%d for 2)", argc);
    return mrb_nil_value();
  }
  if (mrb_type(argv[0]) != MRB_TT_STRING) {
    mrb_raisef(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, argv[0]));
    return mrb_nil_value();
  }
  if (mrb_type(argv[1]) != MRB_TT_STRING) {
    mrb_raisef(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, argv[1]));
    return mrb_nil_value();
  }

  return mrb_file_rename_internal(mrb, argv[0], argv[1]);
}


static mrb_value
mrb_file_dirname(mrb_state *mrb, mrb_value klass)
{
  char *cp, *dname;
  mrb_int n;
  mrb_value fname;

  mrb_get_args(mrb, "s", &cp, &n);
  fname = mrb_str_new(mrb, cp, n);

  if ((dname = dirname(RSTRING_PTR(fname))) == NULL) {
    mrb_sys_fail(mrb, "mrb_file_dirname failed.");
  }

  return mrb_str_new(mrb, dname, strlen(dname));
}

static mrb_value
mrb_file_basename(mrb_state *mrb, mrb_value klass)
{
  char *cp, *bname;
  mrb_int n;
  mrb_value fname;

  mrb_get_args(mrb, "s", &cp, &n);
  fname = mrb_str_new(mrb, cp, n);

  if ((bname = basename(RSTRING_PTR(fname))) == NULL) {
    mrb_sys_fail(mrb, "mrb_file_basename failed.");
  }

  return mrb_str_new(mrb, bname, strlen(bname));
}

static mrb_value
mrb_file_realpath(mrb_state *mrb, mrb_value klass)
{
  mrb_value pathname, dir_string, s, result;
  int argc;
  char *cpath;

  argc = mrb_get_args(mrb, "S|S", &pathname, &dir_string);
  if (argc == 2) {
    s = mrb_str_dup(mrb, dir_string);
    s = mrb_str_append(mrb, s, mrb_str_new_cstr(mrb, FILE_SEPARATOR));
    s = mrb_str_append(mrb, s, pathname);
    pathname = s;
  }
  cpath = mrb_str_to_cstr(mrb, pathname);
  result = mrb_str_buf_new(mrb, PATH_MAX);
  if (realpath(cpath, RSTRING_PTR(result)) == NULL)
    mrb_sys_fail(mrb, cpath);
  mrb_str_resize(mrb, result, strlen(RSTRING_PTR(result)));
  return result;
}

static mrb_value
mrb_file_size(mrb_state *mrb, mrb_value klass)
{
  char *cp;
  FILE *fp;
  mrb_int n;
  mrb_int filesize;
  mrb_value filename;

  mrb_get_args(mrb, "s", &cp, &n);
  filename = mrb_str_new(mrb, cp, n);

  fp = fopen(RSTRING_PTR(filename), "rb");
  if (fp == NULL) {
    mrb_sys_fail(mrb, "mrb_file_size failed.");
    return mrb_nil_value();
  }

  fseek(fp, 0, SEEK_END);
  filesize = (mrb_int) ftell(fp);

  fclose(fp);

  return mrb_fixnum_value(filesize);
}

mrb_value
mrb_file__getwd(mrb_state *mrb, mrb_value klass)
{
  mrb_value path;

  path = mrb_str_buf_new(mrb, MAXPATHLEN);
  if (getcwd(RSTRING_PTR(path), MAXPATHLEN) == NULL) {
    mrb_sys_fail(mrb, "getcwd(2)");
  }
  mrb_str_resize(mrb, path, strlen(RSTRING_PTR(path)));
  return path;
}

static int
mrb_file_is_absolute_path(const char *path)
{
  if (path[0] == '/')
    return 1;
  return 0;
}

static mrb_value
mrb_file__gethome(mrb_state *mrb, mrb_value klass)
{
  mrb_value username, result;
  char *cuser, *home;
  int argc;
  struct passwd *pwd;

  argc = mrb_get_args(mrb, "|S", &username);

  if (argc == 0) {
    home = getenv("HOME");
  } else {
    cuser = mrb_str_to_cstr(mrb, username);
    if ((pwd = getpwnam(cuser)) == NULL) {
      return mrb_nil_value();
    } else {
      home = pwd->pw_dir;
    }
  }

  if (!mrb_file_is_absolute_path(home)) {
    if (argc && strlen(cuser) > 0) {
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "non-absolute home of ~%s", cuser);
    } else {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "non-absolute home");
    }
  }

  result = mrb_str_buf_new(mrb, strlen(home));
  strcpy( RSTRING_PTR(result), home);
  mrb_str_resize(mrb, result, strlen(RSTRING_PTR(result)));

  return result;
}

void
mrb_init_file(mrb_state *mrb)
{
  struct RClass *io, *file, *cnst;

  io   = mrb_class_obj_get(mrb, "IO");
  file = mrb_define_class(mrb, "File", io);
  MRB_SET_INSTANCE_TT(file, MRB_TT_DATA);
  mrb_define_class_method(mrb, file, "umask", mrb_file_s_umask, ARGS_REQ(1));
  mrb_define_class_method(mrb, file, "unlink", mrb_file_s_unlink, ARGS_ANY());
  mrb_define_class_method(mrb, file, "delete", mrb_file_s_unlink, ARGS_ANY());
  mrb_define_class_method(mrb, file, "rename", mrb_file_s_rename, ARGS_REQ(2));

  mrb_define_class_method(mrb, file, "dirname",   mrb_file_dirname,    ARGS_REQ(1));
  mrb_define_class_method(mrb, file, "basename",  mrb_file_basename,   ARGS_REQ(1));
  mrb_define_class_method(mrb, file, "realpath",  mrb_file_realpath,   ARGS_REQ(1)|ARGS_OPT(1));
  mrb_define_class_method(mrb, file, "size",      mrb_file_size,       ARGS_REQ(1));
  mrb_define_class_method(mrb, file, "_getwd", mrb_file__getwd, ARGS_NONE());
  mrb_define_class_method(mrb, file, "_gethome", mrb_file__gethome, ARGS_OPT(1));

  cnst = mrb_define_module_under(mrb, file, "Constants");
  mrb_define_const(mrb, cnst, "LOCK_SH", mrb_fixnum_value(LOCK_SH));
  mrb_define_const(mrb, cnst, "LOCK_EX", mrb_fixnum_value(LOCK_EX));
  mrb_define_const(mrb, cnst, "LOCK_UN", mrb_fixnum_value(LOCK_UN));
  mrb_define_const(mrb, cnst, "LOCK_NB", mrb_fixnum_value(LOCK_NB));
  mrb_define_const(mrb, cnst, "SEPARATOR", mrb_str_new_cstr(mrb, FILE_SEPARATOR));
}
