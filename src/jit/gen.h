#define MRB_JIT_GEN_DUMMY_WRITE \
  volatile int *p = (int *) 0xFAB;\
  *p = 0xFAB;

void NO_INLINE __mrb_jit_pc_add(struct op_ctx *ctx, int o) {
}

void NO_INLINE __mrb_jit_pc_inc(struct op_ctx *ctx) {
}

intptr_t __mrb_jit_A;
intptr_t __mrb_jit_B;
intptr_t __mrb_jit_C;
intptr_t __mrb_jit_Bx;
intptr_t __mrb_jit_sBx;
intptr_t __mrb_jit_Ax;
intptr_t __mrb_jit_b;
intptr_t __mrb_jit_c;
intptr_t __mrb_jit_pc;

#undef GETARG_A
#define GETARG_A(i) ((uintptr_t)(&__mrb_jit_A))
#undef GETARG_Ax
#define GETARG_Ax(i) ((uintptr_t)(&__mrb_jit_Ax))
#undef GETARG_B
#define GETARG_B(i) ((uintptr_t)(&__mrb_jit_B))
#undef GETARG_b
#define GETARG_b(i) ((uintptr_t)(&__mrb_jit_b))
#undef GETARG_sBx
#define GETARG_sBx(i) ((uintptr_t)(&__mrb_jit_sBx))
#undef GETARG_Bx
#define GETARG_Bx(i) ((uintptr_t)(&__mrb_jit_Bx))
#undef GETARG_C
#define GETARG_C(i) ((uintptr_t)(&__mrb_jit_C))
#undef GETARG_c
#define GETARG_c(i) ((uintptr_t)(&__mrb_jit_c))
#undef PC_ADD
#define PC_ADD(ctx, o) (__mrb_jit_pc_add(ctx, o))
#undef PC_INC
#define PC_INC(ctx) (__mrb_jit_pc_inc(ctx))
#undef PC_GET
#define PC_GET(ctx) ((mrb_code *)&__mrb_jit_pc)
#define OP_IDX(i) 0xDE0000
