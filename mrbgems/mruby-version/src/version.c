#include "mruby.h"
#include "mruby/variable.h"
#include "version.h"

void
mrb_mruby_version_gem_init(mrb_state* mrb)
{
  mrb_define_global_const(mrb, "MRUBY_VERSION", mrb_str_new_cstr(mrb, MRUBY_VERSION));
  mrb_define_global_const(mrb, "MRUBY_RELEASE_DATE", mrb_str_new_cstr(mrb, MRUBY_RELEASE_DATE));
}

void
mrb_mruby_version_gem_final(mrb_state* mrb)
{
}

