/*
 * The build writes the revision it read out of the source tree to
 * `mruby/revision.h`, which this file is alone in including: the constant
 * below carries the revision without any other object being compiled with it,
 * so a commit recompiles this one and no more.
 *
 * Not every build writes the header, and the one that does says so with
 * `MRB_REVISION_HEADER`: a build driven by rules other than the ones under
 * `tasks/` has neither the define nor the header, and `mruby/version.h`
 * answers `"HEAD"` there. The preprocess that feeds the presym scan is left
 * out too, the way `mruby/presym.h` leaves out the header it is given: it runs
 * before the header is written, and the revision names no symbol for the scan
 * to find. The include has to come first, ahead of the `mruby.h` that pulls
 * `mruby/version.h` in.
 */
#if defined(MRB_REVISION_HEADER) && !defined(MRB_PRESYM_SCANNING)
# include <mruby/revision.h>
#endif

#include <mruby.h>
#include <mruby/variable.h>

void
mrb_init_version(mrb_state* mrb)
{
  mrb_value mruby_version = mrb_str_new_lit(mrb, MRUBY_VERSION);

  mrb_define_global_const(mrb, "RUBY_VERSION", mrb_str_new_lit(mrb, MRUBY_RUBY_VERSION));
  mrb_define_global_const(mrb, "RUBY_ENGINE", mrb_str_new_lit(mrb, MRUBY_RUBY_ENGINE));
  mrb_define_global_const(mrb, "RUBY_ENGINE_VERSION", mruby_version);
  mrb_define_global_const(mrb, "MRUBY_VERSION", mruby_version);
  mrb_define_global_const(mrb, "MRUBY_PLATFORM", mrb_str_new_lit_frozen(mrb, MRUBY_PLATFORM));
  mrb_define_global_const(mrb, "MRUBY_RELEASE_NO", mrb_fixnum_value(MRUBY_RELEASE_NO));
  mrb_define_global_const(mrb, "MRUBY_RELEASE_DATE", mrb_str_new_lit(mrb, MRUBY_RELEASE_DATE));
  mrb_define_global_const(mrb, "MRUBY_REVISION", mrb_str_new_lit_frozen(mrb, MRUBY_FULL_REVISION));
  mrb_define_global_const(mrb, "MRUBY_DESCRIPTION", mrb_str_new_lit(mrb, MRUBY_DESCRIPTION));
  mrb_define_global_const(mrb, "MRUBY_COPYRIGHT", mrb_str_new_lit(mrb, MRUBY_COPYRIGHT));
}
