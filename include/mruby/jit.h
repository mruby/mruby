#ifndef MRUBY_JIT_H
#define MRUBY_JIT_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "mruby.h"

typedef struct mrb_jit_ctx {
    size_t size;
    //size_t data_size;

    int32_t *off_tbl;

    /* data section, .rodata, .data etc. */
    //uint8_t *data;

    /* text section, i.e. code */
    uint8_t *text;
} mrb_jit_ctx;

struct mrb_irep;

mrb_bool mrb_irep_jit(mrb_state *mrb,struct mrb_irep* irep);
void mrb_irep_jit_call(mrb_state *mrb, struct mrb_irep *irep, void *ctx);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_JIT_H */
