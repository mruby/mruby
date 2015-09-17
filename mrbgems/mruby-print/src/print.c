#include "mruby.h"
#include "mruby/string.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void
printstr(mrb_state *mrb, mrb_value obj)
{
  if (mrb_string_p(obj)) {
    char* ptr = mrb_locale_from_utf8(RSTRING_PTR(obj), RSTRING_LEN(obj));
    if (ptr) {
      fwrite(ptr, strlen(ptr), 1, stdout);
      mrb_locale_free(ptr);
    }
  }
}

/* 15.3.1.2.9  */
/* 15.3.1.3.34 */
mrb_value
mrb_printstr(mrb_state *mrb, mrb_value self)
{
  mrb_value argv;

  mrb_get_args(mrb, "o", &argv);
  printstr(mrb, argv);

  return argv;
}

void
mrb_mruby_print_gem_init(mrb_state* mrb)
{
  struct RClass *krn;
  krn = mrb->kernel_module;
  mrb_define_method(mrb, krn, "__printstr__", mrb_printstr, MRB_ARGS_REQ(1));
}

void
mrb_mruby_print_gem_final(mrb_state* mrb)
{
}
