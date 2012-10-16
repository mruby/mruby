/*
** require.c - require
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#ifdef ENABLE_REQUIRE

#include "mruby/data.h"
#include "mruby/string.h"
#include "mruby/dump.h"
#include "mruby/proc.h"
#include "mruby/compile.h"
#include "mruby/variable.h"
#include "mruby/array.h"
#include "mruby/numeric.h"

#include "opcode.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/param.h>
#include <unistd.h>
#include <libgen.h>

#define E_LOAD_ERROR                (mrb_class_obj_get(mrb, "LoadError"))

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#if 0
  #include <stdarg.h>
  #define debug(s,...)   printf("%s:%d " s, __FILE__, __LINE__,__VA_ARGS__)
#else
  #define debug(...)     ((void)0)
#endif

mrb_value
mrb_yield_internal(mrb_state *mrb, mrb_value b, int argc, mrb_value *argv, mrb_value self, struct RClass *c);

static mrb_value
find_file_check(mrb_state *mrb, mrb_value path, mrb_value fname, mrb_value ext)
{
  mrb_value filepath = mrb_str_dup(mrb, path);
  mrb_str_cat2(mrb, filepath, "/");
  mrb_str_buf_append(mrb, filepath, fname);
  if (!mrb_nil_p(ext)) {
    mrb_str_buf_append(mrb, filepath, ext);
  }

  if (mrb_nil_p(filepath)) {
    return mrb_nil_value();
  }
  debug("filepath: %s\n", RSTRING_PTR(filepath));

  char fpath[MAXPATHLEN];
  realpath(RSTRING_PTR(filepath), fpath);
  if (fpath == NULL) {
    return mrb_nil_value();
  }
  debug("fpath: %s\n", fpath);

  FILE *fp = fopen(fpath, "r");
  if (fp == NULL) {
    return mrb_nil_value();
  }
  fclose(fp);

  return mrb_str_new2(mrb, fpath);
}

static mrb_value
find_file(mrb_state *mrb, mrb_value filename)
{
  char *fname = RSTRING_PTR(filename);
  mrb_value filepath = mrb_nil_value();
  mrb_value load_path = mrb_obj_dup(mrb, mrb_gv_get(mrb, mrb_intern(mrb, "$:")));
  load_path = mrb_check_array_type(mrb, load_path);

  if(mrb_nil_p(load_path)) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "invalid $:");
    return mrb_undef_value();
  }

  char *ext = strrchr(fname, '.');
  mrb_value exts = mrb_ary_new(mrb);
  if (ext == NULL) {
    mrb_ary_push(mrb, exts, mrb_str_new2(mrb, ".rb"));
    mrb_ary_push(mrb, exts, mrb_str_new2(mrb, ".mrb"));
  } else {
    mrb_ary_push(mrb, exts, mrb_nil_value());
  }

  /* when a filename start with '.', $: = ['.'] */
  if (*fname == '.') {
    load_path = mrb_ary_new(mrb);
    mrb_ary_push(mrb, load_path, mrb_str_new2(mrb, "."));
  }

  int i, j;
  for (i = 0; i < RARRAY_LEN(load_path); i++) {
    for (j = 0; j < RARRAY_LEN(exts); j++) {
      filepath = find_file_check(mrb, RARRAY_PTR(load_path)[i], filename, RARRAY_PTR(exts)[j]);
      if (!mrb_nil_p(filepath)) {
        return filepath;
      }
    }
  }

  mrb_raise(mrb, E_LOAD_ERROR, "cannot load such file -- %s", RSTRING_PTR(filename));
  return mrb_nil_value();
}

static void
replace_stop_with_return(mrb_state *mrb, mrb_irep *irep)
{
  if (irep->iseq[irep->ilen - 1] == MKOP_A(OP_STOP, 0)) {
    irep->iseq = mrb_realloc(mrb, irep->iseq, (irep->ilen + 1) * sizeof(mrb_code));
    irep->iseq[irep->ilen - 1] = MKOP_A(OP_LOADNIL, 0);
    irep->iseq[irep->ilen] = MKOP_AB(OP_RETURN, 0, OP_R_NORMAL);
    irep->ilen++;
  }
}

static void
load_mrb_file(mrb_state *mrb, mrb_value filepath)
{
  char *fpath = RSTRING_PTR(filepath);

  {
    FILE *fp = fopen(fpath, "r");
    if (fp == NULL) {
      mrb_raise(mrb, E_LOAD_ERROR, "can't load %s", fpath);
      return;
    }
    fclose(fp);
  }

  int arena_idx = mrb_gc_arena_save(mrb);

  FILE *fp = fopen(fpath, "r");
  int n = mrb_load_irep(mrb, fp);
  fclose(fp);

  mrb_gc_arena_restore(mrb, arena_idx);

  if (n >= 0) {
    struct RProc *proc;
    mrb_irep *irep = mrb->irep[n];

    replace_stop_with_return(mrb, irep);
    proc = mrb_proc_new(mrb, irep);
    proc->target_class = mrb->object_class;
    mrb_yield_internal(mrb, mrb_obj_value(proc), 0, NULL, mrb_top_self(mrb), mrb->object_class);
  } else if (mrb->exc) {
    // fail to load
    longjmp(*(jmp_buf*)mrb->jmp, 1);
  }
}

static void
load_rb_file(mrb_state *mrb, mrb_value filepath)
{
  char *fpath = RSTRING_PTR(filepath);

  {
    FILE *fp = fopen(fpath, "r");
    if (fp == NULL) {
      mrb_raise(mrb, E_LOAD_ERROR, "can't load %s", fpath);
      return;
    }
    fclose(fp);
  }

  mrb_value pid = mrb_fixnum_value((int)getpid());
  mrb_value outfilepath = mrb_str_new2(mrb, "/tmp/mruby.");
  mrb_str_buf_append(mrb, outfilepath, mrb_fix2str(mrb, pid, 10));
  debug("outfilepath: %s\n", RSTRING_PTR(outfilepath));

  mrb_value mrbc_bin = mrb_funcall(mrb, mrb_obj_value(mrb->object_class), "find_mrbc", 0);
  if (mrb_nil_p(mrbc_bin)) {
    mrb_raise(mrb, E_LOAD_ERROR, "can't find mrbc.");
    return;
  }
  debug("mrbc_bin: %s\n", RSTRING_PTR(mrbc_bin));

  mrb_value params[3];
  params[0] = mrbc_bin;
  params[1] = outfilepath;
  params[2] = filepath;
  mrb_value fmt = mrb_str_new2(mrb, "%s -o%s %s");

  mrb_value mrb_cmd = mrb_str_format(mrb, 3, params, fmt);

  system(RSTRING_PTR(mrb_cmd));
  load_mrb_file(mrb, outfilepath);

  remove(RSTRING_PTR(outfilepath));
}


static void
load_file(mrb_state *mrb, mrb_value filepath)
{
  char *ext = strrchr(RSTRING_PTR(filepath), '.');
  if (ext == NULL) {
    mrb_raise(mrb, E_LOAD_ERROR, "Filepath '%s' is invalid.", RSTRING_PTR(filepath));
    return ;
  }

  if (strcmp(ext, ".mrb") == 0) {
    load_mrb_file(mrb, filepath);
  } else if (strcmp(ext, ".rb") == 0) {
    load_rb_file(mrb, filepath);
  } else {
    mrb_raise(mrb, E_LOAD_ERROR, "Filepath '%s' is invalid extension.",
        RSTRING_PTR(filepath));
    return;
  }
}

mrb_value
mrb_load(mrb_state *mrb, mrb_value filename)
{
  mrb_value filepath = find_file(mrb, filename);
  load_file(mrb, filepath);
  return mrb_true_value(); // TODO: ??
}

mrb_value
mrb_f_load(mrb_state *mrb, mrb_value self)
{
  mrb_value filename;

  mrb_get_args(mrb, "o", &filename);
  if (mrb_type(filename) != MRB_TT_STRING) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String",
        mrb_obj_classname(mrb, filename));
    return mrb_nil_value();
  }

  return mrb_load(mrb, filename);
}

static int
loaded_files_check(mrb_state *mrb, mrb_value filepath)
{
  mrb_value loaded_files = mrb_gv_get(mrb, mrb_intern(mrb, "$\""));
  int i;
  for (i = 0; i < RARRAY_LEN(loaded_files); i++) {
    if (mrb_str_cmp(mrb, RARRAY_PTR(loaded_files)[i], filepath) == 0) {
      return 0;
    }
  }

  mrb_value loading_files = mrb_gv_get(mrb, mrb_intern(mrb, "$\"_"));
  if (mrb_nil_p(loading_files)) {
    return 1;
  }
  for (i = 0; i < RARRAY_LEN(loading_files); i++) {
    if (mrb_str_cmp(mrb, RARRAY_PTR(loading_files)[i], filepath) == 0) {
      return 0;
    }
  }

  return 1;
}

static void
loading_files_add(mrb_state *mrb, mrb_value filepath)
{
  mrb_value loading_files = mrb_gv_get(mrb, mrb_intern(mrb, "$\"_"));
  if (mrb_nil_p(loading_files)) {
    loading_files = mrb_ary_new(mrb);
  }
  mrb_ary_push(mrb, loading_files, filepath);

  mrb_gv_set(mrb, mrb_intern(mrb, "$\"_"), loading_files);

  return;
}

static void
loaded_files_add(mrb_state *mrb, mrb_value filepath)
{
  mrb_value loaded_files = mrb_gv_get(mrb, mrb_intern(mrb, "$\""));
  mrb_ary_push(mrb, loaded_files, filepath);

  mrb_gv_set(mrb, mrb_intern(mrb, "$\""), loaded_files);

  return;
}

mrb_value
mrb_require(mrb_state *mrb, mrb_value filename)
{
  mrb_value filepath = find_file(mrb, filename);
  if (loaded_files_check(mrb, filepath)) {
    loading_files_add(mrb, filepath);
    load_file(mrb, filepath);
    loaded_files_add(mrb, filepath);
    return mrb_true_value();
  }

  return mrb_false_value();
}

mrb_value
mrb_f_require(mrb_state *mrb, mrb_value self)
{
  mrb_value filename;

  mrb_get_args(mrb, "o", &filename);
  if (mrb_type(filename) != MRB_TT_STRING) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, filename));
    return mrb_nil_value();
  }

  return mrb_require(mrb, filename);
}

void
mrb_init_require(mrb_state *mrb)
{
  struct RClass *krn;
  krn = mrb->kernel_module;

  mrb_define_method(mrb, krn, "load",                       mrb_f_load,            ARGS_REQ(1));
  mrb_define_method(mrb, krn, "require",                    mrb_f_require,         ARGS_REQ(1));

  mrb_gv_set(mrb, mrb_intern(mrb, "$:"), mrb_ary_new(mrb));
  mrb_gv_set(mrb, mrb_intern(mrb, "$\""), mrb_ary_new(mrb));
}

#endif /* ENABLE_REQUIRE */
