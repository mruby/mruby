#ifndef CARBUNCLE_CORE_H
#define CARBUNCLE_CORE_H

#include <mruby.h>
#include <mruby/data.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DATA_GET_DISPOSABLE_PTR(mrb, obj, type, cast) (cast *)mrb_check_disposed(mrb, obj, type);

void
mrb_carbuncle_raise(mrb_state *mrb, const char *name, const char *msg);

struct RClass *
mrb_carbuncle_define_data_class(mrb_state *mrb, const char *name, struct RClass *super);

void
mrb_carbuncle_check_frozen(mrb_state *mrb, mrb_value obj);

void
mrb_carbuncle_arg_error(mrb_state *mrb, const char *options, mrb_int argc);

void *
mrb_check_disposed(mrb_state *mrb, mrb_value obj, const struct mrb_data_type *type);

void
mrb_carbuncle_check_file(mrb_state *mrb, const char *filename);

struct RClass *
mrb_carbuncle_class_get(mrb_state *mrb, const char *name);

struct RClass *
mrb_carbuncle_module_get(mrb_state *mrb, const char *name);

struct RClass *
mrb_carbuncle_get(mrb_state *mrb);

#ifdef __cplusplus
}
#endif

#endif
