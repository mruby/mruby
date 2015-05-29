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

#define MRB_JIT_CALL(irep, pc, op_ctx) \
  do {\
    unsigned index = (pc) - irep->iseq; \
    unsigned off = irep->jit_ctx.text_off_tbl[index]; \
    void (*f)(void *) = (void *)(irep->jit_ctx.text + off); \
    (*f)(op_ctx); \
  } while(0);

mrb_bool mrb_jit_enter(mrb_state *mrb, struct mrb_irep *irep, void *ctx, mrb_code *pc);

mrb_bool
mrb_jit_release(mrb_state *mrb, struct mrb_irep *irep);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_JIT_H */
