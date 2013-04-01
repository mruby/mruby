#include "mruby.h"
#include "mruby/array.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static mrb_value
mrb_io_test_io_setup(mrb_state *mrb, mrb_value self)
{
  char rfname[] = "tmp.XXXXXXXX";
  char wfname[] = "tmp.XXXXXXXX";
  char msg[] = "mruby io test";
  FILE *fp;
  mrb_value ary = mrb_ary_new(mrb);

  mktemp(rfname);
  mktemp(wfname);
  mrb_gv_set(mrb, mrb_intern(mrb, "$mrbtest_io_rfname"), mrb_str_new_cstr(mrb, rfname));
  mrb_gv_set(mrb, mrb_intern(mrb, "$mrbtest_io_wfname"), mrb_str_new_cstr(mrb, wfname));
  mrb_gv_set(mrb, mrb_intern(mrb, "$mrbtest_io_msg"), mrb_str_new_cstr(mrb, msg));

  mrb_ary_push(mrb, ary, mrb_str_new_cstr(mrb, rfname));
  mrb_ary_push(mrb, ary, mrb_str_new_cstr(mrb, wfname));
  mrb_ary_push(mrb, ary, mrb_str_new_cstr(mrb, msg));

  fp = fopen(rfname, "w");
  if (fp == NULL) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "can't open temporary file");
    return mrb_nil_value();
  }
  fprintf(fp, "%s\n", msg);
  fclose(fp);

  fp = fopen(wfname, "w");
  if (fp == NULL) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "can't open temporary file");
    return mrb_nil_value();
  }
  fclose(fp);

  return ary;
}

static mrb_value
mrb_io_test_io_cleanup(mrb_state *mrb, mrb_value self)
{
  mrb_value rfname = mrb_gv_get(mrb, mrb_intern(mrb, "$mrbtest_io_rfname"));
  mrb_value wfname = mrb_gv_get(mrb, mrb_intern(mrb, "$mrbtest_io_wfname"));

  if (mrb_type(rfname) == MRB_TT_STRING) {
    remove(RSTRING_PTR(rfname));
  }
  if (mrb_type(wfname) == MRB_TT_STRING) {
    remove(RSTRING_PTR(wfname));
  }

  mrb_gv_set(mrb, mrb_intern(mrb, "$mrbtest_io_rfname"), mrb_nil_value());
  mrb_gv_set(mrb, mrb_intern(mrb, "$mrbtest_io_wfname"), mrb_nil_value());
  mrb_gv_set(mrb, mrb_intern(mrb, "$mrbtest_io_msg"), mrb_nil_value());

  return mrb_nil_value();
}

static mrb_value
mrb_io_test_file_setup(mrb_state *mrb, mrb_value self)
{
  mrb_value ary = mrb_io_test_io_setup(mrb, self);
  symlink("/usr/bin", "test-bin");

  return ary;
}

static mrb_value
mrb_io_test_file_cleanup(mrb_state *mrb, mrb_value self)
{
  mrb_io_test_io_cleanup(mrb, self);
  remove("test-bin");

  return mrb_nil_value();
}

void
mrb_mruby_io_gem_test(mrb_state* mrb)
{
  struct RClass *io_test = mrb_define_module(mrb, "MRubyIOTestUtil");
  mrb_define_class_method(mrb, io_test, "io_test_setup", mrb_io_test_io_setup, ARGS_NONE());
  mrb_define_class_method(mrb, io_test, "io_test_cleanup", mrb_io_test_io_cleanup, ARGS_NONE());

  mrb_define_class_method(mrb, io_test, "file_test_setup", mrb_io_test_file_setup, ARGS_NONE());
  mrb_define_class_method(mrb, io_test, "file_test_cleanup", mrb_io_test_file_cleanup, ARGS_NONE());

}
