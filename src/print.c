/*
** print.c - Kernel.#p
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/string.h"

void
mrb_error_print(mrb_state *mrb, mrb_value obj)
{
#ifdef ENABLE_STDIO
  struct RString *str;
  char *s;
  int len;

  if (!mrb_string_p(obj)) {
    mrb_value v = mrb_funcall(mrb, obj, "inspect", 0);
    str = mrb_str_ptr(v);
  }
  else {
    str = mrb_str_ptr(obj);
  }

  s = str->ptr;
  len = str->len;
  fwrite(s, len, 1, stderr);
  fwrite("\n", 1, 1, stderr);
#endif
}

void
mrb_p(mrb_state *mrb, mrb_value obj)
{
  mrb_funcall(mrb, mrb_obj_value(mrb->kernel_module), "p", 1, obj);
}

void
mrb_show_version(mrb_state *mrb)
{
  static const char version_msg[] = "mruby - Embeddable Ruby  Copyright (c) 2010-2013 mruby developers";
  mrb_value msg;

  msg = mrb_str_new(mrb, version_msg, sizeof(version_msg) - 1);
  mrb_p(mrb, msg);
}

void
mrb_show_copyright(mrb_state *mrb)
{
  static const char copyright_msg[] = "mruby - Copyright (c) 2010-2013 mruby developers";
  mrb_value msg;

  msg = mrb_str_new(mrb, copyright_msg, sizeof(copyright_msg) - 1);
  mrb_p(mrb, msg);
}
