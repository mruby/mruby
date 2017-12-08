#include <stdio.h>
#include <stdlib.h>

#include "mruby.h"

mrb_value
mrb_sockettest_tmppath(mrb_state *mrb, mrb_value klass)
{
  char *tmp = tempnam(NULL, "mruby-socket");
  mrb_value str = mrb_str_new_cstr(mrb, tmp);
  free(tmp);
  return str;
}

void
mrb_mruby_socket_gem_test(mrb_state* mrb)
{
  struct RClass *c = mrb_define_module(mrb, "SocketTest");
  mrb_define_class_method(mrb, c, "tmppath", mrb_sockettest_tmppath, MRB_ARGS_NONE());
}
