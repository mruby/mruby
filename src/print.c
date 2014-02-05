/*
** print.c - Kernel.#p
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/string.h"
#include "mruby/variable.h"

static void
printstr(mrb_state *mrb, mrb_value obj)
{
#ifdef ENABLE_STDIO
  struct RString *str;
  char *s;
  int len;

  if (mrb_string_p(obj)) {
    str = mrb_str_ptr(obj);
    s = str->ptr;
    len = str->len;
    fwrite(s, len, 1, stdout);
  }
#endif
}

void
mrb_p(mrb_state *mrb, mrb_value obj)
{
#ifdef ENABLE_STDIO
  obj = mrb_funcall(mrb, obj, "inspect", 0);
  printstr(mrb, obj);
  putc('\n', stdout);
#endif
}

void
mrb_print_error(mrb_state *mrb)
{
#ifdef ENABLE_STDIO
  mrb_value s;
  
  mrb_print_backtrace(mrb);
  s = mrb_funcall(mrb, mrb_obj_value(mrb->exc), "inspect", 0);
  if (mrb_string_p(s)) {
    struct RString *str = mrb_str_ptr(s);
    fwrite(str->ptr, str->len, 1, stderr);
    putc('\n', stderr);
  }
#endif
}

void
mrb_show_version(mrb_state *mrb)
{
  static const char version_msg[] = "mruby - Embeddable Ruby  Copyright (c) 2010-2014 mruby developers\n";
  mrb_value msg;

  if (mrb_const_defined(mrb, mrb_obj_value(mrb->object_class), mrb_intern_lit(mrb, "MRUBY_DESCRIPTION"))) {
    msg = mrb_const_get(mrb, mrb_obj_value(mrb->object_class), mrb_intern_lit(mrb, "MRUBY_DESCRIPTION"));
    msg = mrb_funcall(mrb, msg, "+", 1, mrb_str_new_cstr(mrb, "\n"));
  }
  else {
    msg = mrb_str_new(mrb, version_msg, sizeof(version_msg) - 1);
  }
  printstr(mrb, msg);
}

void
mrb_show_copyright(mrb_state *mrb)
{
  static const char copyright_msg[] = "mruby - Copyright (c) 2010-2014 mruby developers\n";
  mrb_value msg;

  if (mrb_const_defined(mrb, mrb_obj_value(mrb->object_class), mrb_intern_lit(mrb, "MRUBY_COPYRIGHT"))) {
    msg = mrb_const_get(mrb, mrb_obj_value(mrb->object_class), mrb_intern_lit(mrb, "MRUBY_COPYRIGHT"));
    msg = mrb_funcall(mrb, msg, "+", 1, mrb_str_new_cstr(mrb, "\n"));
  }
  else {
    msg = mrb_str_new(mrb, copyright_msg, sizeof(copyright_msg) - 1);
  }
  printstr(mrb, msg);
}
