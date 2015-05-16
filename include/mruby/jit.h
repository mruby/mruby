#ifndef MRUBY_JIT_H
#define MRUBY_JIT_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "mruby.h"

typedef struct mrb_jit_ctx {
    size_t text_size;
    size_t rodata_size;
    size_t size;

    int32_t *text_off_tbl;
    int32_t *rodata_off_tbl;

    /* .rodata sections */
    uint8_t *rodata;

    /* .text sections */
    uint8_t *text;
} mrb_jit_ctx;

struct mrb_irep;

mrb_bool mrb_irep_jit(mrb_state *mrb,struct mrb_irep* irep);
mrb_noreturn void mrb_irep_jit_call(mrb_state *mrb, struct mrb_irep *irep, void *ctx, mrb_code *pc);
mrb_noreturn void mrb_irep_jit_and_call(mrb_state *mrb, struct mrb_irep *irep, void *ctx, mrb_code *pc);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_JIT_H */
