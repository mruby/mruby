#ifndef CARBUNCLE_CORE_H
#define CARBUNCLE_CORE_H

#include <mruby.h>

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_raise(mrb_state *mrb, const char *name, const char *msg);

struct RClass *
mrb_carbuncle_define_data_class(mrb_state *mrb, const char *name, struct RClass *super);

void
mrb_carbuncle_check_frozen(mrb_state *mrb, mrb_value obj);

void
mrb_carbuncle_arg_error(mrb_state *mrb, const char *options, mrb_int argc);

#ifdef __cplusplus
}
#endif

#endif
