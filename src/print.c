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
  char *s;
  int len;

  if (mrb_string_p(obj)) {
    s = RSTRING_PTR(obj);
    len = RSTRING_LEN(obj);
    fwrite(s, len, 1, stdout);
  }
#endif
}

MRB_API void
mrb_p(mrb_state *mrb, mrb_value obj)
{
#ifdef ENABLE_STDIO
  obj = mrb_funcall(mrb, obj, "inspect", 0);
  printstr(mrb, obj);
  putc('\n', stdout);
#endif
}

MRB_API void
mrb_print_error(mrb_state *mrb)
{
#ifdef ENABLE_STDIO
  mrb_value s;

  mrb_print_backtrace(mrb);
  s = mrb_funcall(mrb, mrb_obj_value(mrb->exc), "inspect", 0);
  if (mrb_string_p(s)) {
    fwrite(RSTRING_PTR(s), RSTRING_LEN(s), 1, stderr);
    putc('\n', stderr);
  }
#endif
}

MRB_API void
mrb_show_version(mrb_state *mrb)
{
  mrb_value msg;

  msg = mrb_const_get(mrb, mrb_obj_value(mrb->object_class), mrb_intern_lit(mrb, "MRUBY_DESCRIPTION"));
  printstr(mrb, msg);
  printstr(mrb, mrb_str_new_lit(mrb, "\n"));
}

MRB_API void
mrb_show_copyright(mrb_state *mrb)
{
  mrb_value msg;

  msg = mrb_const_get(mrb, mrb_obj_value(mrb->object_class), mrb_intern_lit(mrb, "MRUBY_COPYRIGHT"));
  printstr(mrb, msg);
  printstr(mrb, mrb_str_new_lit(mrb, "\n"));
}
