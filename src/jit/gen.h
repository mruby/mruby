#define MRB_JIT_GEN_DUMMY_WRITE \
  volatile int *p = (int *) 0xFAB;\
  *p = 0xFAB;

void NO_INLINE __mrb_jit_pc_add__(struct op_ctx *ctx, int o) {
}

void NO_INLINE __mrb_jit_pc_inc__(struct op_ctx *ctx) {
}

intptr_t A;
intptr_t B;
intptr_t C;
intptr_t Bx;
intptr_t sBx;
intptr_t Ax;
intptr_t b;
intptr_t c;
intptr_t pc;

#undef GETARG_A
#define GETARG_A(i) ((uintptr_t)(&A))
#undef GETARG_Ax
#define GETARG_Ax(i) ((uintptr_t)(&Ax))
#undef GETARG_B
#define GETARG_B(i) ((uintptr_t)(&B))
#undef GETARG_b
#define GETARG_b(i) ((uintptr_t)(&b))
#undef GETARG_sBx
#define GETARG_sBx(i) ((uintptr_t)(&sBx))
#undef GETARG_Bx
#define GETARG_Bx(i) ((uintptr_t)(&Bx))
#undef GETARG_C
#define GETARG_C(i) ((uintptr_t)(&C))
#undef GETARG_c
#define GETARG_c(i) ((uintptr_t)(&c))
#undef PC_ADD
#define PC_ADD(ctx, o) (__mrb_jit_pc_add__(ctx, o))
#undef PC_INC
#define PC_INC(ctx) (__mrb_jit_pc_inc__(ctx))
#undef PC_GET
#define PC_GET(ctx) ((mrb_code *)(&pc))
#undef PC_SET
#define PC_SET(ctx, v)
#define OP_IDX(i) 0xDE0000
