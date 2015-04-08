#ifndef MRUBY_JIT_H
#define MRUBY_JIT_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "mruby.h"

typedef struct mrb_jit_page {
    size_t size;
    int32_t *off_tbl;
    uint8_t *data;
} mrb_jit_page;

struct mrb_irep;

mrb_bool mrb_irep_jit(mrb_state *mrb,struct mrb_irep* irep);
void mrb_irep_jit_call(mrb_state *mrb, struct mrb_irep *irep, void *ctx);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_JIT_H */
