
/* args: {} */
static uint8_t op_nop__text[] = {

};
static uint8_t op_nop__rodata[] = {

};

static void op_nop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_nop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_nop_set_args(op, 0,0,0,op_idx);
}


/* args: {"b"=>[[8, 0, 8..11]], "a"=>[[8, 0, 16..19]]} */
static uint8_t op_move__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x08, 0xbc, 0x00,/*4: movsd  0xbc0800(%rax),%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x08, 0xab, 0x00,/*c: movsd  %xmm0,0xab0800(%rax) */

};
static uint8_t op_move__rodata[] = {

};

static void op_move_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 8)) = b * 8 + 0;
  *((int32_t *)(op + 16)) = a * 8 + 0;
}

static void op_move_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_move_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[8, 0, 12..15]], "a"=>[[8, 0, 20..23]]} */
static uint8_t op_loadl__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x20,                   /*4: mov    0x20(%rdi),%rcx */
0xf2, 0x0f, 0x10, 0x81, 0x00, 0x08, 0xbc, 0x00,/*8: movsd  0xbc0800(%rcx),%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x08, 0xab, 0x00,/*10: movsd  %xmm0,0xab0800(%rax) */

};
static uint8_t op_loadl__rodata[] = {

};

static void op_loadl_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 12)) = b * 8 + 0;
  *((int32_t *)(op + 20)) = a * 8 + 0;
}

static void op_loadl_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadl_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 4, 6..9], [8, 0, 20..23]], "b"=>[[1, 0, 24..27]]} */
static uint8_t op_loadi__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x04, 0x08, 0xab, 0x00, 0x00, 0x00, 0xf1, 0xff,/*4: movl   $0xfff10000,0xab0804(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0xbc, 0x00,/*12: movl   $0xbc0000,0xab0800(%rax) */

};
static uint8_t op_loadi__rodata[] = {

};

static void op_loadi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 8 + 4;
  *((int32_t *)(op + 20)) = a * 8 + 0;
  *((int32_t *)(op + 24)) = b * 1 + 0;
}

static void op_loadi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadi_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[8, 4, 6..9], [8, 0, 30..33]], "b"=>[[4, 0, 24..27]]} */
static uint8_t op_loadsym__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x04, 0x08, 0xab, 0x00, 0x00, 0x40, 0xf1, 0xff,/*4: movl   $0xfff14000,0xab0804(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x28,                   /*12: mov    0x28(%rdi),%rcx */
0x8b, 0x89, 0x00, 0x04, 0xbc, 0x00,       /*16: mov    0xbc0400(%rcx),%ecx */
0x89, 0x88, 0x00, 0x08, 0xab, 0x00,       /*1c: mov    %ecx,0xab0800(%rax) */

};
static uint8_t op_loadsym__rodata[] = {

};

static void op_loadsym_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 8 + 4;
  *((int32_t *)(op + 30)) = a * 8 + 0;
  *((int32_t *)(op + 24)) = b * 4 + 0;
}

static void op_loadsym_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadsym_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 4, 6..9], [8, 0, 20..23]]} */
static uint8_t op_loadnil__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x04, 0x08, 0xab, 0x00, 0x00, 0x40, 0xf0, 0xff,/*4: movl   $0xfff04000,0xab0804(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*12: movl   $0x0,0xab0800(%rax) */

};
static uint8_t op_loadnil__rodata[] = {

};

static void op_loadnil_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 8 + 4;
  *((int32_t *)(op + 20)) = a * 8 + 0;
}

static void op_loadnil_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadnil_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 12..15]]} */
static uint8_t op_loadself__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xf2, 0x0f, 0x10, 0x00,                   /*4: movsd  (%rax),%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x08, 0xab, 0x00,/*8: movsd  %xmm0,0xab0800(%rax) */

};
static uint8_t op_loadself__rodata[] = {

};

static void op_loadself_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 12)) = a * 8 + 0;
}

static void op_loadself_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadself_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 4, 6..9], [8, 0, 20..23]]} */
static uint8_t op_loadt__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x04, 0x08, 0xab, 0x00, 0x00, 0xc0, 0xf0, 0xff,/*4: movl   $0xfff0c000,0xab0804(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*12: movl   $0x1,0xab0800(%rax) */

};
static uint8_t op_loadt__rodata[] = {

};

static void op_loadt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 8 + 4;
  *((int32_t *)(op + 20)) = a * 8 + 0;
}

static void op_loadt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadt_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 4, 6..9], [8, 0, 20..23]]} */
static uint8_t op_loadf__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x04, 0x08, 0xab, 0x00, 0x00, 0x40, 0xf0, 0xff,/*4: movl   $0xfff04000,0xab0804(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*12: movl   $0x1,0xab0800(%rax) */

};
static uint8_t op_loadf__rodata[] = {

};

static void op_loadf_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 8 + 4;
  *((int32_t *)(op + 20)) = a * 8 + 0;
}

static void op_loadf_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadf_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[8, 0, 32..35]]} */
static uint8_t op_getglobal__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*6: mov    0x50(%rbx),%rdi */
0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x28,                   /*e: mov    0x28(%rbx),%rax */
0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*12: mov    0xbc0400(%rax),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*18: callq  1d <op_getglobal+0x1d> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*1d: mov    %rax,0xab0800(%r14) */
0x48, 0x89, 0xdf,                         /*24: mov    %rbx,%rdi */
0x5b,                                     /*27: pop    %rbx */
0x41, 0x5e,                               /*28: pop    %r14 */

};
static uint8_t op_getglobal__rodata[] = {

};

static void op_getglobal_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 32)) = a * 8 + 0;
}

static void op_getglobal_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getglobal_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28]]} */
static uint8_t op_setglobal__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*4: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
0x48, 0x8b, 0x90, 0x00, 0x08, 0xab, 0x00, /*16: mov    0xab0800(%rax),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1d: callq  22 <op_setglobal+0x22> */
0x48, 0x89, 0xdf,                         /*22: mov    %rbx,%rdi */
0x5b,                                     /*25: pop    %rbx */

};
static uint8_t op_setglobal__rodata[] = {

};

static void op_setglobal_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
}

static void op_setglobal_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setglobal_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[1, 0, 15..18]], "a"=>[[8, 0, 27..30]]} */
static uint8_t op_getspecial__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x50,                   /*a: mov    0x50(%rbx),%rdi */
0xbe, 0x00, 0x00, 0xbc, 0x00,             /*e: mov    $0xbc0000,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*13: callq  18 <op_getspecial+0x18> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*18: mov    %rax,0xab0800(%r14) */
0x48, 0x89, 0xdf,                         /*1f: mov    %rbx,%rdi */
0x5b,                                     /*22: pop    %rbx */
0x41, 0x5e,                               /*23: pop    %r14 */

};
static uint8_t op_getspecial__rodata[] = {

};

static void op_getspecial_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 1 + 0;
  *((int32_t *)(op + 27)) = a * 8 + 0;
}

static void op_getspecial_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getspecial_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18]], "b"=>[[1, 0, 20..23]]} */
static uint8_t op_setspecial__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x50,                   /*8: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x90, 0x00, 0x08, 0xab, 0x00, /*c: mov    0xab0800(%rax),%rdx */
0xbe, 0x00, 0x00, 0xbc, 0x00,             /*13: mov    $0xbc0000,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*18: callq  1d <op_setspecial+0x1d> */
0x48, 0x89, 0xdf,                         /*1d: mov    %rbx,%rdi */
0x5b,                                     /*20: pop    %rbx */

};
static uint8_t op_setspecial__rodata[] = {

};

static void op_setspecial_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
  *((int32_t *)(op + 20)) = b * 1 + 0;
}

static void op_setspecial_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setspecial_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[8, 0, 32..35]]} */
static uint8_t op_getiv__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*6: mov    0x50(%rbx),%rdi */
0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x28,                   /*e: mov    0x28(%rbx),%rax */
0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*12: mov    0xbc0400(%rax),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*18: callq  1d <op_getiv+0x1d> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*1d: mov    %rax,0xab0800(%r14) */
0x48, 0x89, 0xdf,                         /*24: mov    %rbx,%rdi */
0x5b,                                     /*27: pop    %rbx */
0x41, 0x5e,                               /*28: pop    %r14 */

};
static uint8_t op_getiv__rodata[] = {

};

static void op_getiv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 32)) = a * 8 + 0;
}

static void op_getiv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getiv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28]]} */
static uint8_t op_setiv__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*4: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
0x48, 0x8b, 0x90, 0x00, 0x08, 0xab, 0x00, /*16: mov    0xab0800(%rax),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1d: callq  22 <op_setiv+0x22> */
0x48, 0x89, 0xdf,                         /*22: mov    %rbx,%rdi */
0x5b,                                     /*25: pop    %rbx */

};
static uint8_t op_setiv__rodata[] = {

};

static void op_setiv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
}

static void op_setiv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setiv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 40..43]], "a"=>[[8, 0, 52..55]]} */
static uint8_t op_getcv__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x10,                   /*6: mov    0x10(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x50,                   /*a: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*e: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*12: mov    0x20(%rcx),%rcx */
0x48, 0x89, 0x41, 0x38,                   /*16: mov    %rax,0x38(%rcx) */
0x48, 0x8b, 0x7b, 0x50,                   /*1a: mov    0x50(%rbx),%rdi */
0x4c, 0x8b, 0x73, 0x18,                   /*1e: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x28,                   /*22: mov    0x28(%rbx),%rax */
0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*26: mov    0xbc0400(%rax),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2c: callq  31 <op_getcv+0x31> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*31: mov    %rax,0xab0800(%r14) */
0x48, 0x8b, 0x43, 0x50,                   /*38: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*3c: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*40: mov    0x20(%rax),%rax */
0x48, 0xc7, 0x40, 0x38, 0x00, 0x00, 0x00, 0x00,/*44: movq   $0x0,0x38(%rax) */
0x48, 0x89, 0xdf,                         /*4c: mov    %rbx,%rdi */
0x5b,                                     /*4f: pop    %rbx */
0x41, 0x5e,                               /*50: pop    %r14 */

};
static uint8_t op_getcv__rodata[] = {

};

static void op_getcv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 40)) = b * 4 + 0;
  *((int32_t *)(op + 52)) = a * 8 + 0;
}

static void op_getcv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getcv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28]]} */
static uint8_t op_setcv__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*4: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
0x48, 0x8b, 0x90, 0x00, 0x08, 0xab, 0x00, /*16: mov    0xab0800(%rax),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1d: callq  22 <op_setcv+0x22> */
0x48, 0x89, 0xdf,                         /*22: mov    %rbx,%rdi */
0x5b,                                     /*25: pop    %rbx */

};
static uint8_t op_setcv__rodata[] = {

};

static void op_setcv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
}

static void op_setcv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setcv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 34..37]], "a"=>[[8, 0, 82..85]]} */
static uint8_t op_getconst__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x10,                   /*4: mov    0x10(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x50,                   /*8: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*c: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*10: mov    0x20(%rcx),%rcx */
0x48, 0x89, 0x41, 0x38,                   /*14: mov    %rax,0x38(%rcx) */
0x48, 0x8b, 0x43, 0x28,                   /*18: mov    0x28(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x50,                   /*1c: mov    0x50(%rbx),%rdi */
0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*20: mov    0xbc0400(%rax),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*26: callq  2b <op_getconst+0x2b> */
0x48, 0x8b, 0x4b, 0x50,                   /*2b: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*2f: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*33: mov    0x20(%rcx),%rcx */
0x48, 0xc7, 0x41, 0x38, 0x00, 0x00, 0x00, 0x00,/*37: movq   $0x0,0x38(%rcx) */
0x48, 0x8b, 0x4b, 0x50,                   /*3f: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*43: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*47: mov    0x8(%rcx),%rcx */
0x48, 0x89, 0x4b, 0x18,                   /*4b: mov    %rcx,0x18(%rbx) */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*4f: mov    %rax,0xab0800(%rcx) */
0x48, 0x89, 0xdf,                         /*56: mov    %rbx,%rdi */
0x5b,                                     /*59: pop    %rbx */

};
static uint8_t op_getconst__rodata[] = {

};

static void op_getconst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 34)) = b * 4 + 0;
  *((int32_t *)(op + 82)) = a * 8 + 0;
}

static void op_getconst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getconst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28]]} */
static uint8_t op_setconst__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*4: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
0x48, 0x8b, 0x90, 0x00, 0x08, 0xab, 0x00, /*16: mov    0xab0800(%rax),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1d: callq  22 <op_setconst+0x22> */
0x48, 0x89, 0xdf,                         /*22: mov    %rbx,%rdi */
0x5b,                                     /*25: pop    %rbx */

};
static uint8_t op_setconst__rodata[] = {

};

static void op_setconst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
}

static void op_setconst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setconst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 38..41]], "a"=>[[8, 0, 45..48], [8, 0, 93..96]]} */
static uint8_t op_getmcnst__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x10,                   /*4: mov    0x10(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x50,                   /*8: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*c: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*10: mov    0x20(%rcx),%rcx */
0x48, 0x89, 0x41, 0x38,                   /*14: mov    %rax,0x38(%rcx) */
0x48, 0x8b, 0x7b, 0x50,                   /*18: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*1c: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*20: mov    0x28(%rbx),%rcx */
0x8b, 0x91, 0x00, 0x04, 0xbc, 0x00,       /*24: mov    0xbc0400(%rcx),%edx */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xab, 0x00, /*2a: mov    0xab0800(%rax),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*31: callq  36 <op_getmcnst+0x36> */
0x48, 0x8b, 0x4b, 0x50,                   /*36: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*3a: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*3e: mov    0x20(%rcx),%rcx */
0x48, 0xc7, 0x41, 0x38, 0x00, 0x00, 0x00, 0x00,/*42: movq   $0x0,0x38(%rcx) */
0x48, 0x8b, 0x4b, 0x50,                   /*4a: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*4e: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*52: mov    0x8(%rcx),%rcx */
0x48, 0x89, 0x4b, 0x18,                   /*56: mov    %rcx,0x18(%rbx) */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*5a: mov    %rax,0xab0800(%rcx) */
0x48, 0x89, 0xdf,                         /*61: mov    %rbx,%rdi */
0x5b,                                     /*64: pop    %rbx */

};
static uint8_t op_getmcnst__rodata[] = {

};

static void op_getmcnst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 38)) = b * 4 + 0;
  *((int32_t *)(op + 45)) = a * 8 + 0;
  *((int32_t *)(op + 93)) = a * 8 + 0;
}

static void op_getmcnst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getmcnst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28], [8, 8, 32..35]]} */
static uint8_t op_setmcnst__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*4: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0x91, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%edx */
0x48, 0x8b, 0x88, 0x00, 0x08, 0xab, 0x00, /*16: mov    0xab0800(%rax),%rcx */
0x48, 0x8b, 0xb0, 0x08, 0x08, 0xab, 0x00, /*1d: mov    0xab0808(%rax),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*24: callq  29 <op_setmcnst+0x29> */
0x48, 0x89, 0xdf,                         /*29: mov    %rbx,%rdi */
0x5b,                                     /*2c: pop    %rbx */

};
static uint8_t op_setmcnst__rodata[] = {

};

static void op_setmcnst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
  *((int32_t *)(op + 32)) = a * 8 + 8;
}

static void op_setmcnst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setmcnst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"c"=>[[1, 0, 11..14]], "b"=>[[8, 0, 94..97]], "a"=>[[8, 0, 101..104]]} */
static uint8_t op_getupvar__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*a: mov    $0xcd0000,%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*f: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*13: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*17: mov    0x20(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*1b: mov    0x8(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x28,                   /*1f: mov    0x28(%rcx),%rcx */
0xeb, 0x11,                               /*23: jmp    36 <op_getupvar+0x36> */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*25: data16 nopw %cs:0x0(%rax,%rax,1) */
0xff, 0xc8,                               /*30: dec    %eax */
0x48, 0x8b, 0x49, 0x08,                   /*32: mov    0x8(%rcx),%rcx */
0x48, 0x85, 0xc9,                         /*36: test   %rcx,%rcx */
0x0f, 0x94, 0xc2,                         /*39: sete   %dl */
0x85, 0xc0,                               /*3c: test   %eax,%eax */
0x74, 0x07,                               /*3e: je     47 <op_getupvar+0x47> */
0xf6, 0xc2, 0x01,                         /*40: test   $0x1,%dl */
0x74, 0xeb,                               /*43: je     30 <op_getupvar+0x30> */
0xeb, 0x04,                               /*45: jmp    4b <op_getupvar+0x4b> */
0x84, 0xd2,                               /*47: test   %dl,%dl */
0x74, 0x0c,                               /*49: je     57 <op_getupvar+0x57> */
0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0xf0, 0xff,/*4b: movabs $0xfff0400000000000,%rax */
0xeb, 0x0b,                               /*55: jmp    62 <op_getupvar+0x62> */
0x48, 0x8b, 0x41, 0x18,                   /*57: mov    0x18(%rcx),%rax */
0x48, 0x8b, 0x80, 0x00, 0x08, 0xbc, 0x00, /*5b: mov    0xbc0800(%rax),%rax */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*62: mov    %rax,0xab0800(%r14) */
0x48, 0x89, 0xdf,                         /*69: mov    %rbx,%rdi */
0x5b,                                     /*6c: pop    %rbx */
0x41, 0x5e,                               /*6d: pop    %r14 */

};
static uint8_t op_getupvar__rodata[] = {

};

static void op_getupvar_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 11)) = c * 1 + 0;
  *((int32_t *)(op + 94)) = b * 8 + 0;
  *((int32_t *)(op + 101)) = a * 8 + 0;
}

static void op_getupvar_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getupvar_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"c"=>[[1, 0, 5..8]], "a"=>[[8, 0, 71..74]], "b"=>[[8, 0, 79..82]]} */
static uint8_t op_setupvar__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*4: mov    $0xcd0000,%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*9: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*d: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*11: mov    0x20(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*15: mov    0x8(%rcx),%rcx */
0x48, 0x8b, 0x71, 0x28,                   /*19: mov    0x28(%rcx),%rsi */
0xeb, 0x07,                               /*1d: jmp    26 <op_setupvar+0x26> */
0x90,                                     /*1f: nop */
0xff, 0xc8,                               /*20: dec    %eax */
0x48, 0x8b, 0x76, 0x08,                   /*22: mov    0x8(%rsi),%rsi */
0x48, 0x85, 0xf6,                         /*26: test   %rsi,%rsi */
0x0f, 0x94, 0xc1,                         /*29: sete   %cl */
0x85, 0xc0,                               /*2c: test   %eax,%eax */
0x74, 0x07,                               /*2e: je     37 <op_setupvar+0x37> */
0xf6, 0xc1, 0x01,                         /*30: test   $0x1,%cl */
0x74, 0xeb,                               /*33: je     20 <op_setupvar+0x20> */
0xeb, 0x25,                               /*35: jmp    5c <op_setupvar+0x5c> */
0x84, 0xc9,                               /*37: test   %cl,%cl */
0x75, 0x21,                               /*39: jne    5c <op_setupvar+0x5c> */
0x48, 0x8b, 0x43, 0x18,                   /*3b: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4e, 0x18,                   /*3f: mov    0x18(%rsi),%rcx */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x08, 0xab, 0x00,/*43: movsd  0xab0800(%rax),%xmm0 */
0xf2, 0x0f, 0x11, 0x81, 0x00, 0x08, 0xbc, 0x00,/*4b: movsd  %xmm0,0xbc0800(%rcx) */
0x48, 0x8b, 0x7b, 0x50,                   /*53: mov    0x50(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*57: callq  5c <op_setupvar+0x5c> */
0x48, 0x89, 0xdf,                         /*5c: mov    %rbx,%rdi */
0x5b,                                     /*5f: pop    %rbx */

};
static uint8_t op_setupvar__rodata[] = {

};

static void op_setupvar_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = c * 1 + 0;
  *((int32_t *)(op + 71)) = a * 8 + 0;
  *((int32_t *)(op + 79)) = b * 8 + 0;
}

static void op_setupvar_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setupvar_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_jmp__text[] = {

};
static uint8_t op_jmp__rodata[] = {

};

static void op_jmp_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_jmp_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmp_set_args(op, 0,GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[8, 4, 7..10]]} */
static uint8_t op_jmpif__text[] = {
0x50,                                     /*0: push   %rax */
0x48, 0x8b, 0x47, 0x18,                   /*1: mov    0x18(%rdi),%rax */
0x8b, 0x80, 0x04, 0x08, 0xab, 0x00,       /*5: mov    0xab0804(%rax),%eax */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*b: cmp    $0xfff00001,%eax */
0x72, 0x0c,                               /*10: jb     1e <op_jmpif+0x1e> */
0x25, 0x00, 0xc0, 0x0f, 0x00,             /*12: and    $0xfc000,%eax */
0x3d, 0x00, 0x40, 0x00, 0x00,             /*17: cmp    $0x4000,%eax */
0x74, 0x0b,                               /*1c: je     29 <op_jmpif+0x29> */
0xc7, 0x04, 0x25, 0xab, 0x0f, 0x00, 0x00, 0xab, 0x0f, 0x00, 0x00,/*1e: movl   $0xfab,0xfab */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*29: callq  2e <op_jmpif+0x2e> */
0x58,                                     /*2e: pop    %rax */
0xc3,                                     /*2f: retq */

};
static uint8_t op_jmpif__rodata[] = {

};

static void op_jmpif_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 4;
}

static void op_jmpif_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmpif_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[8, 4, 7..10]]} */
static uint8_t op_jmpnot__text[] = {
0x50,                                     /*0: push   %rax */
0x48, 0x8b, 0x47, 0x18,                   /*1: mov    0x18(%rdi),%rax */
0x8b, 0x80, 0x04, 0x08, 0xab, 0x00,       /*5: mov    0xab0804(%rax),%eax */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*b: cmp    $0xfff00001,%eax */
0x72, 0x17,                               /*10: jb     29 <op_jmpnot+0x29> */
0x25, 0x00, 0xc0, 0x0f, 0x00,             /*12: and    $0xfc000,%eax */
0x3d, 0x00, 0x40, 0x00, 0x00,             /*17: cmp    $0x4000,%eax */
0x75, 0x0b,                               /*1c: jne    29 <op_jmpnot+0x29> */
0xc7, 0x04, 0x25, 0xab, 0x0f, 0x00, 0x00, 0xab, 0x0f, 0x00, 0x00,/*1e: movl   $0xfab,0xfab */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*29: callq  2e <op_jmpnot+0x2e> */
0x58,                                     /*2e: pop    %rax */
0xc3,                                     /*2f: retq */

};
static uint8_t op_jmpnot__rodata[] = {

};

static void op_jmpnot_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 4;
}

static void op_jmpnot_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmpnot_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"op_idx"=>[[4, 0, 92..95]]} */
static uint8_t op_onerr__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x50,                   /*4: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x48, 0x18,                   /*8: mov    0x18(%rax),%rcx */
0x8b, 0x51, 0x40,                         /*c: mov    0x40(%rcx),%edx */
0x48, 0x8b, 0x71, 0x20,                   /*f: mov    0x20(%rcx),%rsi */
0x3b, 0x56, 0x1c,                         /*13: cmp    0x1c(%rsi),%edx */
0x7f, 0x39,                               /*16: jg     51 <op_onerr+0x51> */
0x8d, 0x04, 0x12,                         /*18: lea    (%rdx,%rdx,1),%eax */
0x85, 0xd2,                               /*1b: test   %edx,%edx */
0xba, 0x10, 0x00, 0x00, 0x00,             /*1d: mov    $0x10,%edx */
0x0f, 0x45, 0xd0,                         /*22: cmovne %eax,%edx */
0x89, 0x51, 0x40,                         /*25: mov    %edx,0x40(%rcx) */
0x48, 0x8b, 0x7b, 0x50,                   /*28: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*2c: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x70, 0x38,                   /*30: mov    0x38(%rax),%rsi */
0x48, 0x63, 0x50, 0x40,                   /*34: movslq 0x40(%rax),%rdx */
0x48, 0xc1, 0xe2, 0x03,                   /*38: shl    $0x3,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*3c: callq  41 <op_onerr+0x41> */
0x48, 0x8b, 0x4b, 0x50,                   /*41: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*45: mov    0x18(%rcx),%rcx */
0x48, 0x89, 0x41, 0x38,                   /*49: mov    %rax,0x38(%rcx) */
0x48, 0x8b, 0x43, 0x50,                   /*4d: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x08,                   /*51: mov    0x8(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*55: mov    0x8(%rcx),%rcx */
0x48, 0x8d, 0x91, 0x00, 0x04, 0xde, 0x00, /*59: lea    0xde0400(%rcx),%rdx */
0x48, 0x89, 0x53, 0x10,                   /*60: mov    %rdx,0x10(%rbx) */
0x48, 0x81, 0xc1, 0x00, 0x00, 0x68, 0x06, /*64: add    $0x6680000,%rcx */
0x48, 0x8b, 0x40, 0x18,                   /*6b: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*6f: mov    0x20(%rax),%rax */
0x48, 0x63, 0x50, 0x1c,                   /*73: movslq 0x1c(%rax),%rdx */
0x8d, 0x72, 0x01,                         /*77: lea    0x1(%rdx),%esi */
0x89, 0x70, 0x1c,                         /*7a: mov    %esi,0x1c(%rax) */
0x48, 0x8b, 0x43, 0x50,                   /*7d: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*81: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x38,                   /*85: mov    0x38(%rax),%rax */
0x48, 0x89, 0x0c, 0xd0,                   /*89: mov    %rcx,(%rax,%rdx,8) */
0x48, 0x89, 0xdf,                         /*8d: mov    %rbx,%rdi */
0x5b,                                     /*90: pop    %rbx */

};
static uint8_t op_onerr__rodata[] = {

};

static void op_onerr_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 92)) = op_idx * 4 + 0;
}

static void op_onerr_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_onerr_set_args(op, 0,GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[8, 4, 31..34], [8, 0, 63..66], [8, 0, 75..78], [8, 0, 102..105]]} */
static uint8_t op_rescue__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x50,                   /*4: mov    0x50(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x28,                   /*8: mov    0x28(%rcx),%rcx */
0x8b, 0x09,                               /*c: mov    (%rcx),%ecx */
0xc1, 0xe1, 0x0e,                         /*e: shl    $0xe,%ecx */
0x81, 0xc1, 0x00, 0x40, 0x00, 0x00,       /*11: add    $0x4000,%ecx */
0x81, 0xc9, 0x00, 0x00, 0xf0, 0xff,       /*17: or     $0xfff00000,%ecx */
0x89, 0x88, 0x04, 0x08, 0xab, 0x00,       /*1d: mov    %ecx,0xab0804(%rax) */
0x48, 0x8b, 0x47, 0x50,                   /*23: mov    0x50(%rdi),%rax */
0x48, 0x8b, 0x40, 0x28,                   /*27: mov    0x28(%rax),%rax */
0x0f, 0xb6, 0x08,                         /*2b: movzbl (%rax),%ecx */
0x83, 0xf9, 0x05,                         /*2e: cmp    $0x5,%ecx */
0x77, 0x12,                               /*31: ja     45 <op_rescue+0x45> */
0x83, 0xf9, 0x01,                         /*33: cmp    $0x1,%ecx */
0x74, 0x0d,                               /*36: je     45 <op_rescue+0x45> */
0x48, 0x8b, 0x4f, 0x18,                   /*38: mov    0x18(%rdi),%rcx */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*3c: mov    %rax,0xab0800(%rcx) */
0xeb, 0x25,                               /*43: jmp    6a <op_rescue+0x6a> */
0x48, 0x8b, 0x47, 0x18,                   /*45: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*49: movl   $0x0,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*53: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x50,                   /*57: mov    0x50(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x28,                   /*5b: mov    0x28(%rcx),%rcx */
0x48, 0xc1, 0xe9, 0x02,                   /*5f: shr    $0x2,%rcx */
0x48, 0x09, 0x88, 0x00, 0x08, 0xab, 0x00, /*63: or     %rcx,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x50,                   /*6a: mov    0x50(%rdi),%rax */
0x48, 0xc7, 0x40, 0x28, 0x00, 0x00, 0x00, 0x00,/*6e: movq   $0x0,0x28(%rax) */

};
static uint8_t op_rescue__rodata[] = {

};

static void op_rescue_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 31)) = a * 8 + 4;
  *((int32_t *)(op + 63)) = a * 8 + 0;
  *((int32_t *)(op + 75)) = a * 8 + 0;
  *((int32_t *)(op + 102)) = a * 8 + 0;
}

static void op_rescue_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_rescue_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {} */
static uint8_t op_poperr__text[] = {
0xb8, 0x00, 0x00, 0x55, 0xff,             /*0: mov    $0xff550000,%eax */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*5: data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x4f, 0x50,                   /*10: mov    0x50(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*14: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*18: mov    0x20(%rcx),%rcx */
0xff, 0x49, 0x1c,                         /*1c: decl   0x1c(%rcx) */
0xff, 0xc0,                               /*1f: inc    %eax */
0x75, 0xed,                               /*21: jne    10 <op_poperr+0x10> */

};
static uint8_t op_poperr__rodata[] = {

};

static void op_poperr_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_poperr_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_poperr_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18]]} */
static uint8_t op_raise__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x50,                   /*8: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x80, 0x00, 0x08, 0xab, 0x00, /*c: mov    0xab0800(%rax),%rax */
0x48, 0xc1, 0xe0, 0x02,                   /*13: shl    $0x2,%rax */
0x48, 0xba, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*17: movabs $0xfffffffffffc,%rdx */
0x48, 0x21, 0xc2,                         /*21: and    %rax,%rdx */
0x48, 0x89, 0x51, 0x28,                   /*24: mov    %rdx,0x28(%rcx) */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*28: callq  2d <op_raise+0x2d> */
0x48, 0x89, 0xdf,                         /*2d: mov    %rbx,%rdi */
0x5b,                                     /*30: pop    %rbx */

};
static uint8_t op_raise__rodata[] = {

};

static void op_raise_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
}

static void op_raise_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_raise_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[8, 0, 21..24]]} */
static uint8_t op_epush__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x08,                   /*6: mov    0x8(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x50,                   /*a: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x40, 0x20,                   /*e: mov    0x20(%rax),%rax */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*12: mov    0xbc0800(%rax),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*19: callq  1e <op_epush+0x1e> */
0x49, 0x89, 0xc6,                         /*1e: mov    %rax,%r14 */
0x48, 0x8b, 0x43, 0x50,                   /*21: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*25: mov    0x18(%rax),%rax */
0x8b, 0x70, 0x50,                         /*29: mov    0x50(%rax),%esi */
0x48, 0x8b, 0x48, 0x20,                   /*2c: mov    0x20(%rax),%rcx */
0x8b, 0x51, 0x20,                         /*30: mov    0x20(%rcx),%edx */
0x39, 0xd6,                               /*33: cmp    %edx,%esi */
0x7f, 0x44,                               /*35: jg     7b <op_epush+0x7b> */
0x8d, 0x0c, 0x36,                         /*37: lea    (%rsi,%rsi,1),%ecx */
0x85, 0xf6,                               /*3a: test   %esi,%esi */
0xba, 0x10, 0x00, 0x00, 0x00,             /*3c: mov    $0x10,%edx */
0x0f, 0x45, 0xd1,                         /*41: cmovne %ecx,%edx */
0x89, 0x50, 0x50,                         /*44: mov    %edx,0x50(%rax) */
0x48, 0x8b, 0x7b, 0x50,                   /*47: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*4b: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x70, 0x48,                   /*4f: mov    0x48(%rax),%rsi */
0x48, 0x63, 0x50, 0x50,                   /*53: movslq 0x50(%rax),%rdx */
0x48, 0xc1, 0xe2, 0x03,                   /*57: shl    $0x3,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*5b: callq  60 <op_epush+0x60> */
0x48, 0x8b, 0x4b, 0x50,                   /*60: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*64: mov    0x18(%rcx),%rcx */
0x48, 0x89, 0x41, 0x48,                   /*68: mov    %rax,0x48(%rcx) */
0x48, 0x8b, 0x43, 0x50,                   /*6c: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*70: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*74: mov    0x20(%rax),%rcx */
0x8b, 0x51, 0x20,                         /*78: mov    0x20(%rcx),%edx */
0x8d, 0x42, 0x01,                         /*7b: lea    0x1(%rdx),%eax */
0x89, 0x41, 0x20,                         /*7e: mov    %eax,0x20(%rcx) */
0x48, 0x63, 0xc2,                         /*81: movslq %edx,%rax */
0x48, 0x8b, 0x4b, 0x50,                   /*84: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*88: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x48,                   /*8c: mov    0x48(%rcx),%rcx */
0x4c, 0x89, 0x34, 0xc1,                   /*90: mov    %r14,(%rcx,%rax,8) */
0x8b, 0x43, 0x48,                         /*94: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*97: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*9b: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*a1: mov    %rbx,%rdi */
0x5b,                                     /*a4: pop    %rbx */
0x41, 0x5e,                               /*a5: pop    %r14 */

};
static uint8_t op_epush__rodata[] = {

};

static void op_epush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 8 + 0;
}

static void op_epush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_epush_set_args(op, 0,GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[1, 0, 69..72]]} */
static uint8_t op_epop__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x53,                                     /*5: push   %rbx */
0x49, 0x89, 0xff,                         /*6: mov    %rdi,%r15 */
0x49, 0x8b, 0x47, 0x50,                   /*9: mov    0x50(%r15),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*d: mov    0x18(%rax),%rax */
0x4c, 0x8b, 0x70, 0x20,                   /*11: mov    0x20(%rax),%r14 */
0x41, 0x8b, 0x6e, 0x20,                   /*15: mov    0x20(%r14),%ebp */
0x31, 0xdb,                               /*19: xor    %ebx,%ebx */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*1b: nopl   0x0(%rax,%rax,1) */
0x41, 0x3b, 0x6e, 0xc8,                   /*20: cmp    -0x38(%r14),%ebp */
0x7e, 0x25,                               /*24: jle    4b <op_epop+0x4b> */
0x49, 0x8b, 0x7f, 0x50,                   /*26: mov    0x50(%r15),%rdi */
0xff, 0xcd,                               /*2a: dec    %ebp */
0x89, 0xee,                               /*2c: mov    %ebp,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2e: callq  33 <op_epop+0x33> */
0x41, 0x8b, 0x47, 0x48,                   /*33: mov    0x48(%r15),%eax */
0x49, 0x8b, 0x4f, 0x50,                   /*37: mov    0x50(%r15),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*3b: mov    %eax,0xdc(%rcx) */
0xff, 0xc3,                               /*41: inc    %ebx */
0x81, 0xfb, 0x00, 0x00, 0xab, 0x00,       /*43: cmp    $0xab0000,%ebx */
0x7c, 0xd5,                               /*49: jl     20 <op_epop+0x20> */
0x4c, 0x89, 0xff,                         /*4b: mov    %r15,%rdi */
0x5b,                                     /*4e: pop    %rbx */
0x41, 0x5e,                               /*4f: pop    %r14 */
0x41, 0x5f,                               /*51: pop    %r15 */
0x5d,                                     /*53: pop    %rbp */

};
static uint8_t op_epop__rodata[] = {

};

static void op_epop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 69)) = a * 1 + 0;
}

static void op_epop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_epop_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 15..18]], "op_idx"=>[[4, 0, 36..39]], "b"=>[[1, 0, 54..57]], "c"=>[[1, 0, 60..63]]} */
static uint8_t op_send__text[] = {
0x55,                                     /*0: push   %rbp */
0x48, 0x89, 0xe5,                         /*1: mov    %rsp,%rbp */
0x41, 0x56,                               /*4: push   %r14 */
0x53,                                     /*6: push   %rbx */
0x48, 0x83, 0xe4, 0xf0,                   /*7: and    $0xfffffffffffffff0,%rsp */
0x48, 0x89, 0xfb,                         /*b: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xab, 0x00,             /*e: mov    $0xab0000,%eax */
0x48, 0x8b, 0x4b, 0x08,                   /*13: mov    0x8(%rbx),%rcx */
0x48, 0x8b, 0x53, 0x50,                   /*17: mov    0x50(%rbx),%rdx */
0x48, 0x8b, 0x52, 0x18,                   /*1b: mov    0x18(%rdx),%rdx */
0x4c, 0x8b, 0x72, 0x20,                   /*1f: mov    0x20(%rdx),%r14 */
0xba, 0x00, 0x04, 0xde, 0x00,             /*23: mov    $0xde0400,%edx */
0x48, 0x03, 0x51, 0x08,                   /*28: add    0x8(%rcx),%rdx */
0x48, 0x89, 0x53, 0x10,                   /*2c: mov    %rdx,0x10(%rbx) */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*30: mov    $0x20,%esi */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*35: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*3a: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*40: mov    %rbx,%rdi */
0x89, 0xc2,                               /*43: mov    %eax,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*45: callq  4a <op_send+0x4a> */
0x48, 0x8b, 0x43, 0x50,                   /*4a: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*4e: mov    0x18(%rax),%rax */
0x4c, 0x39, 0x70, 0x20,                   /*52: cmp    %r14,0x20(%rax) */
0x73, 0x0c,                               /*56: jae    64 <op_send+0x64> */
0x48, 0x89, 0xdf,                         /*58: mov    %rbx,%rdi */
0x48, 0x8d, 0x65, 0xf0,                   /*5b: lea    -0x10(%rbp),%rsp */
0x5b,                                     /*5f: pop    %rbx */
0x41, 0x5e,                               /*60: pop    %r14 */
0x5d,                                     /*62: pop    %rbp */
0xc3,                                     /*63: retq */
0x48, 0x89, 0xdf,                         /*64: mov    %rbx,%rdi */
0x48, 0x8d, 0x65, 0xf0,                   /*67: lea    -0x10(%rbp),%rsp */
0x5b,                                     /*6b: pop    %rbx */
0x41, 0x5e,                               /*6c: pop    %r14 */
0x5d,                                     /*6e: pop    %rbp */

};
static uint8_t op_send__rodata[] = {

};

static void op_send_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 1 + 0;
  *((int32_t *)(op + 36)) = op_idx * 4 + 0;
  *((int32_t *)(op + 54)) = b * 1 + 0;
  *((int32_t *)(op + 60)) = c * 1 + 0;
}

static void op_send_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_send_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[1, 0, 17..20]], "b"=>[[1, 0, 27..30]], "c"=>[[1, 0, 33..36]]} */
static uint8_t op_sendb__text[] = {
0x55,                                     /*0: push   %rbp */
0x48, 0x89, 0xe5,                         /*1: mov    %rsp,%rbp */
0x53,                                     /*4: push   %rbx */
0x48, 0x83, 0xe4, 0xf0,                   /*5: and    $0xfffffffffffffff0,%rsp */
0x48, 0x83, 0xec, 0x10,                   /*9: sub    $0x10,%rsp */
0x48, 0x89, 0xfb,                         /*d: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xab, 0x00,             /*10: mov    $0xab0000,%eax */
0xbe, 0x21, 0x00, 0x00, 0x00,             /*15: mov    $0x21,%esi */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*1a: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*1f: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*25: mov    %rbx,%rdi */
0x89, 0xc2,                               /*28: mov    %eax,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2a: callq  2f <op_sendb+0x2f> */
0x48, 0x89, 0xdf,                         /*2f: mov    %rbx,%rdi */
0x48, 0x8d, 0x65, 0xf8,                   /*32: lea    -0x8(%rbp),%rsp */
0x5b,                                     /*36: pop    %rbx */
0x5d,                                     /*37: pop    %rbp */

};
static uint8_t op_sendb__rodata[] = {

};

static void op_sendb_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 1 + 0;
  *((int32_t *)(op + 27)) = b * 1 + 0;
  *((int32_t *)(op + 33)) = c * 1 + 0;
}

static void op_sendb_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sendb_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_fsend__text[] = {

};
static uint8_t op_fsend__rodata[] = {

};

static void op_fsend_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_fsend_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_fsend_set_args(op, 0,0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 345..348]]} */
static uint8_t op_call__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x49, 0x89, 0xff,                         /*5: mov    %rdi,%r15 */
0x49, 0x8b, 0x47, 0x50,                   /*8: mov    0x50(%r15),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*c: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x08,                   /*10: mov    0x8(%rax),%rcx */
0x48, 0x8b, 0x40, 0x20,                   /*14: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x31,                         /*18: mov    (%rcx),%rsi */
0x48, 0x8d, 0x0c, 0xb5, 0x00, 0x00, 0x00, 0x00,/*1b: lea    0x0(,%rsi,4),%rcx */
0x48, 0xbb, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*23: movabs $0xfffffffffffc,%rbx */
0x48, 0x21, 0xcb,                         /*2d: and    %rcx,%rbx */
0x48, 0x8b, 0x4b, 0x20,                   /*30: mov    0x20(%rbx),%rcx */
0x48, 0x89, 0x48, 0x48,                   /*34: mov    %rcx,0x48(%rax) */
0x48, 0x89, 0x58, 0x08,                   /*38: mov    %rbx,0x8(%rax) */
0x48, 0x8b, 0x4b, 0x28,                   /*3c: mov    0x28(%rbx),%rcx */
0x48, 0x85, 0xc9,                         /*40: test   %rcx,%rcx */
0x74, 0x24,                               /*43: je     69 <op_call+0x69> */
0x8b, 0x51, 0x20,                         /*45: mov    0x20(%rcx),%edx */
0x85, 0xd2,                               /*48: test   %edx,%edx */
0x74, 0x06,                               /*4a: je     52 <op_call+0x52> */
0x89, 0x10,                               /*4c: mov    %edx,(%rax) */
0x48, 0x8b, 0x4b, 0x28,                   /*4e: mov    0x28(%rbx),%rcx */
0x48, 0x83, 0x79, 0x18, 0x00,             /*52: cmpq   $0x0,0x18(%rcx) */
0x75, 0x10,                               /*57: jne    69 <op_call+0x69> */
0x49, 0x8b, 0x57, 0x50,                   /*59: mov    0x50(%r15),%rdx */
0x48, 0x8b, 0x52, 0x18,                   /*5d: mov    0x18(%rdx),%rdx */
0x48, 0x8b, 0x52, 0x08,                   /*61: mov    0x8(%rdx),%rdx */
0x48, 0x89, 0x51, 0x18,                   /*65: mov    %rdx,0x18(%rcx) */
0xf6, 0x43, 0x02, 0x04,                   /*69: testb  $0x4,0x2(%rbx) */
0x74, 0x2f,                               /*6d: je     9e <op_call+0x9e> */
0x49, 0x8b, 0x7f, 0x50,                   /*6f: mov    0x50(%r15),%rdi */
0xff, 0x53, 0x18,                         /*73: callq  *0x18(%rbx) */
0x49, 0x89, 0xc6,                         /*76: mov    %rax,%r14 */
0x49, 0x8b, 0x7f, 0x50,                   /*79: mov    0x50(%r15),%rdi */
0x41, 0x8b, 0x77, 0x48,                   /*7d: mov    0x48(%r15),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*81: callq  86 <op_call+0x86> */
0x49, 0x8b, 0x47, 0x50,                   /*86: mov    0x50(%r15),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*8a: cmpq   $0x0,0x28(%rax) */
0x74, 0x57,                               /*8f: je     e8 <op_call+0xe8> */
0x4c, 0x89, 0xff,                         /*91: mov    %r15,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*94: callq  99 <op_call+0x99> */
0xe9, 0x19, 0x01, 0x00, 0x00,             /*99: jmpq   1b7 <op_call+0x1b7> */
0x49, 0x89, 0x1f,                         /*9e: mov    %rbx,(%r15) */
0x48, 0x8b, 0x4b, 0x18,                   /*a1: mov    0x18(%rbx),%rcx */
0x49, 0x89, 0x4f, 0x08,                   /*a5: mov    %rcx,0x8(%r15) */
0x48, 0x85, 0xc9,                         /*a9: test   %rcx,%rcx */
0x0f, 0x84, 0x8d, 0x00, 0x00, 0x00,       /*ac: je     13f <op_call+0x13f> */
0x48, 0x8b, 0x51, 0x10,                   /*b2: mov    0x10(%rcx),%rdx */
0x49, 0x89, 0x57, 0x20,                   /*b6: mov    %rdx,0x20(%r15) */
0x48, 0x8b, 0x51, 0x18,                   /*ba: mov    0x18(%rcx),%rdx */
0x49, 0x89, 0x57, 0x28,                   /*be: mov    %rdx,0x28(%r15) */
0x0f, 0xb7, 0x49, 0x02,                   /*c2: movzwl 0x2(%rcx),%ecx */
0x89, 0x48, 0x18,                         /*c6: mov    %ecx,0x18(%rax) */
0x8b, 0x50, 0x40,                         /*c9: mov    0x40(%rax),%edx */
0x49, 0x8b, 0x47, 0x08,                   /*cc: mov    0x8(%r15),%rax */
0x49, 0x8b, 0x7f, 0x50,                   /*d0: mov    0x50(%r15),%rdi */
0x0f, 0xb7, 0x70, 0x02,                   /*d4: movzwl 0x2(%rax),%esi */
0x85, 0xd2,                               /*d8: test   %edx,%edx */
0x0f, 0x88, 0x89, 0x00, 0x00, 0x00,       /*da: js     169 <op_call+0x169> */
0x83, 0xc2, 0x02,                         /*e0: add    $0x2,%edx */
0xe9, 0x91, 0x00, 0x00, 0x00,             /*e3: jmpq   179 <op_call+0x179> */
0x48, 0x8b, 0x40, 0x18,                   /*e8: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*ec: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x51, 0x10,                   /*f0: mov    0x10(%rcx),%rdx */
0x48, 0x89, 0x50, 0x08,                   /*f4: mov    %rdx,0x8(%rax) */
0x49, 0x89, 0x57, 0x18,                   /*f8: mov    %rdx,0x18(%r15) */
0x48, 0x63, 0x41, 0x44,                   /*fc: movslq 0x44(%rcx),%rax */
0x4c, 0x89, 0x34, 0xc2,                   /*100: mov    %r14,(%rdx,%rax,8) */
0x48, 0x8b, 0x41, 0x30,                   /*104: mov    0x30(%rcx),%rax */
0x49, 0x89, 0x47, 0x10,                   /*108: mov    %rax,0x10(%r15) */
0x49, 0x8b, 0x7f, 0x50,                   /*10c: mov    0x50(%r15),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*110: callq  115 <op_call+0x115> */
0x49, 0x8b, 0x47, 0x50,                   /*115: mov    0x50(%r15),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*119: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*11d: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*121: mov    0x8(%rax),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*125: mov    0x18(%rax),%rax */
0x49, 0x89, 0x47, 0x08,                   /*129: mov    %rax,0x8(%r15) */
0x48, 0x8b, 0x48, 0x10,                   /*12d: mov    0x10(%rax),%rcx */
0x49, 0x89, 0x4f, 0x20,                   /*131: mov    %rcx,0x20(%r15) */
0x48, 0x8b, 0x40, 0x18,                   /*135: mov    0x18(%rax),%rax */
0x49, 0x89, 0x47, 0x28,                   /*139: mov    %rax,0x28(%r15) */
0xeb, 0x78,                               /*13d: jmp    1b7 <op_call+0x1b7> */
0x49, 0x8b, 0x47, 0x50,                   /*13f: mov    0x50(%r15),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*143: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*147: mov    0x8(%rax),%rax */
0x48, 0xb9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0xf0, 0xff,/*14b: movabs $0xfff0400000000000,%rcx */
0x48, 0x89, 0x08,                         /*155: mov    %rcx,(%rax) */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*158: mov    $0xab0000,%esi */
0x31, 0xd2,                               /*15d: xor    %edx,%edx */
0x4c, 0x89, 0xff,                         /*15f: mov    %r15,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*162: callq  167 <op_call+0x167> */
0xeb, 0x4e,                               /*167: jmp    1b7 <op_call+0x1b7> */
0x83, 0xfe, 0x03,                         /*169: cmp    $0x3,%esi */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*16c: mov    $0x3,%eax */
0x0f, 0x42, 0xf0,                         /*171: cmovb  %eax,%esi */
0xba, 0x03, 0x00, 0x00, 0x00,             /*174: mov    $0x3,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*179: callq  17e <op_call+0x17e> */
0x49, 0x8b, 0x47, 0x50,                   /*17e: mov    0x50(%r15),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*182: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*186: mov    0x8(%rax),%rax */
0x49, 0x89, 0x47, 0x18,                   /*18a: mov    %rax,0x18(%r15) */
0x48, 0x8b, 0x4b, 0x28,                   /*18e: mov    0x28(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*192: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x09,                         /*196: mov    (%rcx),%rcx */
0x48, 0x89, 0x08,                         /*199: mov    %rcx,(%rax) */
0x49, 0x8b, 0x47, 0x08,                   /*19c: mov    0x8(%r15),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*1a0: mov    0x8(%rax),%rax */
0x49, 0x89, 0x47, 0x10,                   /*1a4: mov    %rax,0x10(%r15) */
0x49, 0x8b, 0x37,                         /*1a8: mov    (%r15),%rsi */
0x49, 0x8b, 0x7f, 0x50,                   /*1ab: mov    0x50(%r15),%rdi */
0x4c, 0x89, 0xfa,                         /*1af: mov    %r15,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1b2: callq  1b7 <op_call+0x1b7> */
0x4c, 0x89, 0xff,                         /*1b7: mov    %r15,%rdi */
0x5b,                                     /*1ba: pop    %rbx */
0x41, 0x5e,                               /*1bb: pop    %r14 */
0x41, 0x5f,                               /*1bd: pop    %r15 */

};
static uint8_t op_call__rodata[] = {

};

static void op_call_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 345)) = a * 1 + 0;
}

static void op_call_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_call_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"c"=>[[1, 0, 227..230], [1, 1, 340..343]], "a"=>[[1, 1, 305..308], [8, 12, 317..320], [8, 8, 335..338], [8, 0, 411..414], [1, 0, 505..508]]} */
static uint8_t op_super__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x89, 0xfb,                         /*a: mov    %rdi,%rbx */
0x48, 0x8b, 0x6b, 0x50,                   /*d: mov    0x50(%rbx),%rbp */
0x48, 0x8b, 0x45, 0x18,                   /*11: mov    0x18(%rbp),%rax */
0x4c, 0x8b, 0x68, 0x20,                   /*15: mov    0x20(%rax),%r13 */
0x45, 0x8b, 0x65, 0x00,                   /*19: mov    0x0(%r13),%r12d */
0x45, 0x85, 0xe4,                         /*1d: test   %r12d,%r12d */
0x74, 0x61,                               /*20: je     83 <op_super+0x83> */
0x48, 0x8b, 0x43, 0x18,                   /*22: mov    0x18(%rbx),%rax */
0x4c, 0x8b, 0x30,                         /*26: mov    (%rax),%r14 */
0x49, 0x8b, 0x45, 0x48,                   /*29: mov    0x48(%r13),%rax */
0x48, 0x8b, 0x48, 0x28,                   /*2d: mov    0x28(%rax),%rcx */
0x48, 0x89, 0x0c, 0x24,                   /*31: mov    %rcx,(%rsp) */
0x48, 0x8b, 0x03,                         /*35: mov    (%rbx),%rax */
0x48, 0x39, 0x48, 0x38,                   /*38: cmp    %rcx,0x38(%rax) */
0x75, 0x08,                               /*3c: jne    46 <op_super+0x46> */
0x31, 0xd2,                               /*3e: xor    %edx,%edx */
0x44, 0x39, 0x60, 0x30,                   /*40: cmp    %r12d,0x30(%rax) */
0x74, 0x33,                               /*44: je     79 <op_super+0x79> */
0x48, 0x39, 0x48, 0x48,                   /*46: cmp    %rcx,0x48(%rax) */
0x75, 0x0b,                               /*4a: jne    57 <op_super+0x57> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*4c: mov    $0x1,%edx */
0x44, 0x39, 0x60, 0x40,                   /*51: cmp    %r12d,0x40(%rax) */
0x74, 0x22,                               /*55: je     79 <op_super+0x79> */
0x48, 0x39, 0x48, 0x58,                   /*57: cmp    %rcx,0x58(%rax) */
0x75, 0x0b,                               /*5b: jne    68 <op_super+0x68> */
0xba, 0x02, 0x00, 0x00, 0x00,             /*5d: mov    $0x2,%edx */
0x44, 0x39, 0x60, 0x50,                   /*62: cmp    %r12d,0x50(%rax) */
0x74, 0x11,                               /*66: je     79 <op_super+0x79> */
0x48, 0x39, 0x48, 0x68,                   /*68: cmp    %rcx,0x68(%rax) */
0x75, 0x62,                               /*6c: jne    d0 <op_super+0xd0> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*6e: mov    $0x3,%edx */
0x44, 0x39, 0x60, 0x60,                   /*73: cmp    %r12d,0x60(%rax) */
0x75, 0x57,                               /*77: jne    d0 <op_super+0xd0> */
0x4c, 0x8b, 0xbc, 0xd0, 0x90, 0x00, 0x00, 0x00,/*79: mov    0x90(%rax,%rdx,8),%r15 */
0xeb, 0x5f,                               /*81: jmp    e2 <op_super+0xe2> */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*83: mov    0x0(%rip),%rsi        # 8a <op_super+0x8a> */
0x48, 0x89, 0xef,                         /*8a: mov    %rbp,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*8d: callq  92 <op_super+0x92> */
0x49, 0x89, 0xc6,                         /*92: mov    %rax,%r14 */
0x48, 0x8b, 0x7b, 0x50,                   /*95: mov    0x50(%rbx),%rdi */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*99: mov    $0x0,%esi */
0xba, 0x1e, 0x00, 0x00, 0x00,             /*9e: mov    $0x1e,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*a3: callq  a8 <op_super+0xa8> */
0x48, 0x89, 0xef,                         /*a8: mov    %rbp,%rdi */
0x4c, 0x89, 0xf6,                         /*ab: mov    %r14,%rsi */
0x48, 0x89, 0xc2,                         /*ae: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*b1: callq  b6 <op_super+0xb6> */
0x48, 0xc1, 0xe0, 0x02,                   /*b6: shl    $0x2,%rax */
0x48, 0xb9, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*ba: movabs $0xfffffffffffc,%rcx */
0x48, 0x21, 0xc1,                         /*c4: and    %rax,%rcx */
0x48, 0x89, 0x4d, 0x28,                   /*c7: mov    %rcx,0x28(%rbp) */
0xe9, 0x19, 0x01, 0x00, 0x00,             /*cb: jmpq   1e9 <op_super+0x1e9> */
0x48, 0x8d, 0x34, 0x24,                   /*d0: lea    (%rsp),%rsi */
0x48, 0x89, 0xef,                         /*d4: mov    %rbp,%rdi */
0x44, 0x89, 0xe2,                         /*d7: mov    %r12d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*da: callq  df <op_super+0xdf> */
0x49, 0x89, 0xc7,                         /*df: mov    %rax,%r15 */
0xbd, 0x00, 0x00, 0xcd, 0x00,             /*e2: mov    $0xcd0000,%ebp */
0x4d, 0x85, 0xff,                         /*e7: test   %r15,%r15 */
0x75, 0x6c,                               /*ea: jne    158 <op_super+0x158> */
0x48, 0x8b, 0x7b, 0x50,                   /*ec: mov    0x50(%rbx),%rdi */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*f0: mov    $0x0,%esi */
0xba, 0x0e, 0x00, 0x00, 0x00,             /*f5: mov    $0xe,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*fa: callq  ff <op_super+0xff> */
0x41, 0x89, 0xc4,                         /*ff: mov    %eax,%r12d */
0x48, 0x8b, 0x7b, 0x50,                   /*102: mov    0x50(%rbx),%rdi */
0x48, 0x8d, 0x34, 0x24,                   /*106: lea    (%rsp),%rsi */
0x44, 0x89, 0xe2,                         /*10a: mov    %r12d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*10d: callq  112 <op_super+0x112> */
0x49, 0x89, 0xc7,                         /*112: mov    %rax,%r15 */
0x48, 0x8b, 0x43, 0x18,                   /*115: mov    0x18(%rbx),%rax */
0xb9, 0x02, 0x00, 0x78, 0x01,             /*119: mov    $0x1780002,%ecx */
0x66, 0x90,                               /*11e: xchg   %ax,%ax */
0xf2, 0x0f, 0x10, 0x44, 0xc8, 0xf8,       /*120: movsd  -0x8(%rax,%rcx,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xc8,             /*126: movsd  %xmm0,(%rax,%rcx,8) */
0x48, 0xff, 0xc9,                         /*12b: dec    %rcx */
0x48, 0x81, 0xf9, 0x01, 0x00, 0xab, 0x00, /*12e: cmp    $0xab0001,%rcx */
0x75, 0xe9,                               /*135: jne    120 <op_super+0x120> */
0x48, 0x8b, 0x43, 0x18,                   /*137: mov    0x18(%rbx),%rax */
0xc7, 0x80, 0x0c, 0x08, 0xab, 0x00, 0x00, 0x40, 0xf1, 0xff,/*13b: movl   $0xfff14000,0xab080c(%rax) */
0x41, 0x8b, 0x45, 0x00,                   /*145: mov    0x0(%r13),%eax */
0x48, 0x8b, 0x4b, 0x18,                   /*149: mov    0x18(%rbx),%rcx */
0x89, 0x81, 0x08, 0x08, 0xab, 0x00,       /*14d: mov    %eax,0xab0808(%rcx) */
0xbd, 0x01, 0x00, 0xcd, 0x00,             /*153: mov    $0xcd0001,%ebp */
0x48, 0x8b, 0x7b, 0x50,                   /*158: mov    0x50(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*15c: callq  161 <op_super+0x161> */
0x44, 0x89, 0x20,                         /*161: mov    %r12d,(%rax) */
0x4c, 0x89, 0x78, 0x08,                   /*164: mov    %r15,0x8(%rax) */
0x48, 0x8b, 0x4b, 0x50,                   /*168: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*16c: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*170: mov    0x8(%rcx),%rcx */
0x48, 0x89, 0x48, 0x10,                   /*174: mov    %rcx,0x10(%rax) */
0x89, 0x68, 0x40,                         /*178: mov    %ebp,0x40(%rax) */
0x48, 0x8b, 0x0c, 0x24,                   /*17b: mov    (%rsp),%rcx */
0x48, 0x89, 0x48, 0x48,                   /*17f: mov    %rcx,0x48(%rax) */
0x48, 0x8b, 0x4b, 0x10,                   /*183: mov    0x10(%rbx),%rcx */
0x48, 0x83, 0xc1, 0x04,                   /*187: add    $0x4,%rcx */
0x48, 0x89, 0x48, 0x30,                   /*18b: mov    %rcx,0x30(%rax) */
0x48, 0x8b, 0x4b, 0x50,                   /*18f: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*193: mov    0x18(%rcx),%rcx */
0x48, 0x81, 0x41, 0x08, 0x00, 0x08, 0xab, 0x00,/*197: addq   $0xab0800,0x8(%rcx) */
0x48, 0x8b, 0x4b, 0x50,                   /*19f: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*1a3: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*1a7: mov    0x8(%rcx),%rcx */
0x4c, 0x89, 0x31,                         /*1ab: mov    %r14,(%rcx) */
0x41, 0xf6, 0x47, 0x02, 0x04,             /*1ae: testb  $0x4,0x2(%r15) */
0x74, 0x41,                               /*1b3: je     1f6 <op_super+0x1f6> */
0x83, 0xcd, 0x02,                         /*1b5: or     $0x2,%ebp */
0x89, 0x68, 0x18,                         /*1b8: mov    %ebp,0x18(%rax) */
0x48, 0x8b, 0x7b, 0x50,                   /*1bb: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*1bf: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x68, 0x08,                   /*1c3: mov    0x8(%rax),%rbp */
0x4c, 0x89, 0xf6,                         /*1c7: mov    %r14,%rsi */
0x41, 0xff, 0x57, 0x18,                   /*1ca: callq  *0x18(%r15) */
0x48, 0x89, 0x45, 0x00,                   /*1ce: mov    %rax,0x0(%rbp) */
0x48, 0x8b, 0x7b, 0x50,                   /*1d2: mov    0x50(%rbx),%rdi */
0x8b, 0x73, 0x48,                         /*1d6: mov    0x48(%rbx),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1d9: callq  1de <op_super+0x1de> */
0x48, 0x8b, 0x43, 0x50,                   /*1de: mov    0x50(%rbx),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*1e2: cmpq   $0x0,0x28(%rax) */
0x74, 0x77,                               /*1e7: je     260 <op_super+0x260> */
0x48, 0x89, 0xdf,                         /*1e9: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1ec: callq  1f1 <op_super+0x1f1> */
0xe9, 0x87, 0x00, 0x00, 0x00,             /*1f1: jmpq   27d <op_super+0x27d> */
0xc7, 0x40, 0x44, 0x00, 0x00, 0xab, 0x00, /*1f6: movl   $0xab0000,0x44(%rax) */
0x4c, 0x89, 0x78, 0x08,                   /*1fd: mov    %r15,0x8(%rax) */
0x49, 0x8b, 0x4f, 0x18,                   /*201: mov    0x18(%r15),%rcx */
0x48, 0x89, 0x4b, 0x08,                   /*205: mov    %rcx,0x8(%rbx) */
0x48, 0x8b, 0x51, 0x10,                   /*209: mov    0x10(%rcx),%rdx */
0x48, 0x89, 0x53, 0x20,                   /*20d: mov    %rdx,0x20(%rbx) */
0x48, 0x8b, 0x51, 0x18,                   /*211: mov    0x18(%rcx),%rdx */
0x48, 0x89, 0x53, 0x28,                   /*215: mov    %rdx,0x28(%rbx) */
0x0f, 0xb7, 0x49, 0x02,                   /*219: movzwl 0x2(%rcx),%ecx */
0x89, 0x48, 0x18,                         /*21d: mov    %ecx,0x18(%rax) */
0x48, 0x8b, 0x4b, 0x08,                   /*220: mov    0x8(%rbx),%rcx */
0x48, 0x8b, 0x7b, 0x50,                   /*224: mov    0x50(%rbx),%rdi */
0x0f, 0xb7, 0x71, 0x02,                   /*228: movzwl 0x2(%rcx),%esi */
0x8b, 0x50, 0x40,                         /*22c: mov    0x40(%rax),%edx */
0x83, 0xc2, 0x02,                         /*22f: add    $0x2,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*232: callq  237 <op_super+0x237> */
0x48, 0x8b, 0x7b, 0x50,                   /*237: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*23b: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*23f: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x18,                   /*243: mov    %rax,0x18(%rbx) */
0x48, 0x8b, 0x43, 0x08,                   /*247: mov    0x8(%rbx),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*24b: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x10,                   /*24f: mov    %rax,0x10(%rbx) */
0x4c, 0x89, 0xfe,                         /*253: mov    %r15,%rsi */
0x48, 0x89, 0xda,                         /*256: mov    %rbx,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*259: callq  25e <op_super+0x25e> */
0xeb, 0x1d,                               /*25e: jmp    27d <op_super+0x27d> */
0x48, 0x8b, 0x40, 0x18,                   /*260: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*264: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x49, 0x10,                   /*268: mov    0x10(%rcx),%rcx */
0x48, 0x89, 0x48, 0x08,                   /*26c: mov    %rcx,0x8(%rax) */
0x48, 0x89, 0x4b, 0x18,                   /*270: mov    %rcx,0x18(%rbx) */
0x48, 0x8b, 0x7b, 0x50,                   /*274: mov    0x50(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*278: callq  27d <op_super+0x27d> */
0x48, 0x89, 0xdf,                         /*27d: mov    %rbx,%rdi */
0x5b,                                     /*280: pop    %rbx */
0x41, 0x5c,                               /*281: pop    %r12 */
0x41, 0x5d,                               /*283: pop    %r13 */
0x41, 0x5e,                               /*285: pop    %r14 */
0x41, 0x5f,                               /*287: pop    %r15 */
0x5d,                                     /*289: pop    %rbp */

};
static uint8_t op_super__rodata[] = {
0x73, 0x75, 0x70, 0x65, 0x72, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x65, 0x64, 0x20, 0x6f, 0x75, 0x74,
0x73, 0x69, 0x64, 0x65, 0x20, 0x6f, 0x66, 0x20, 0x6d, 0x65, 0x74, 0x68, 0x6f, 0x64, 0x00, 0x6d,
0x65, 0x74, 0x68, 0x6f, 0x64, 0x5f, 0x6d, 0x69, 0x73, 0x73, 0x69, 0x6e, 0x67, 0x00,

};

static void op_super_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 227)) = c * 1 + 0;
  *((int32_t *)(op + 340)) = c * 1 + 1;
  *((int32_t *)(op + 305)) = a * 1 + 1;
  *((int32_t *)(op + 317)) = a * 8 + 12;
  *((int32_t *)(op + 335)) = a * 8 + 8;
  *((int32_t *)(op + 411)) = a * 8 + 0;
  *((int32_t *)(op + 505)) = a * 1 + 0;
}

static void op_super_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_super_set_args(op, GETARG_A(c),0,GETARG_C(c),op_idx);
}


/* args: {"b"=>[[1, 0, 21..24]], "a"=>[[8, 0, 370..373], [8, 0, 444..447], [8, 0, 456..459], [8, 8, 683..686]]} */
static uint8_t op_argary__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x83, 0xec, 0x48,                   /*a: sub    $0x48,%rsp */
0x49, 0x89, 0xfc,                         /*e: mov    %rdi,%r12 */
0xc7, 0x44, 0x24, 0x44, 0x00, 0x00, 0xbc, 0x00,/*11: movl   $0xbc0000,0x44(%rsp) */
0x44, 0x8b, 0x6c, 0x24, 0x44,             /*19: mov    0x44(%rsp),%r13d */
0x44, 0x8b, 0x7c, 0x24, 0x44,             /*1e: mov    0x44(%rsp),%r15d */
0x8b, 0x5c, 0x24, 0x44,                   /*23: mov    0x44(%rsp),%ebx */
0x8b, 0x74, 0x24, 0x44,                   /*27: mov    0x44(%rsp),%esi */
0x83, 0xe6, 0x0f,                         /*2b: and    $0xf,%esi */
0x0f, 0x84, 0x99, 0x00, 0x00, 0x00,       /*2e: je     cd <op_argary+0xcd> */
0x49, 0x8b, 0x6c, 0x24, 0x50,             /*34: mov    0x50(%r12),%rbp */
0x48, 0x8b, 0x45, 0x18,                   /*39: mov    0x18(%rbp),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*3d: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*41: mov    0x8(%rax),%rax */
0x48, 0x8b, 0x40, 0x28,                   /*45: mov    0x28(%rax),%rax */
0x48, 0x85, 0xc0,                         /*49: test   %rax,%rax */
0x0f, 0x94, 0xc1,                         /*4c: sete   %cl */
0x83, 0xfe, 0x01,                         /*4f: cmp    $0x1,%esi */
0x74, 0x1f,                               /*52: je     73 <op_argary+0x73> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*54: mov    $0x1,%edx */
0x29, 0xf2,                               /*59: sub    %esi,%edx */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*5b: nopl   0x0(%rax,%rax,1) */
0xf6, 0xc1, 0x01,                         /*60: test   $0x1,%cl */
0x75, 0x12,                               /*63: jne    77 <op_argary+0x77> */
0x48, 0x8b, 0x40, 0x08,                   /*65: mov    0x8(%rax),%rax */
0x48, 0x85, 0xc0,                         /*69: test   %rax,%rax */
0x0f, 0x94, 0xc1,                         /*6c: sete   %cl */
0xff, 0xc2,                               /*6f: inc    %edx */
0x75, 0xed,                               /*71: jne    60 <op_argary+0x60> */
0x84, 0xc9,                               /*73: test   %cl,%cl */
0x74, 0x5d,                               /*75: je     d4 <op_argary+0xd4> */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*77: mov    0x0(%rip),%rsi        # 7e <op_argary+0x7e> */
0x48, 0x89, 0xef,                         /*7e: mov    %rbp,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*81: callq  86 <op_argary+0x86> */
0x49, 0x89, 0xc6,                         /*86: mov    %rax,%r14 */
0x49, 0x8b, 0x7c, 0x24, 0x50,             /*89: mov    0x50(%r12),%rdi */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*8e: mov    $0x0,%esi */
0xba, 0x1e, 0x00, 0x00, 0x00,             /*93: mov    $0x1e,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*98: callq  9d <op_argary+0x9d> */
0x48, 0x89, 0xef,                         /*9d: mov    %rbp,%rdi */
0x4c, 0x89, 0xf6,                         /*a0: mov    %r14,%rsi */
0x48, 0x89, 0xc2,                         /*a3: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*a6: callq  ab <op_argary+0xab> */
0x48, 0xc1, 0xe0, 0x02,                   /*ab: shl    $0x2,%rax */
0x48, 0xb9, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*af: movabs $0xfffffffffffc,%rcx */
0x48, 0x21, 0xc8,                         /*b9: and    %rcx,%rax */
0x48, 0x89, 0x45, 0x28,                   /*bc: mov    %rax,0x28(%rbp) */
0x4c, 0x89, 0xe7,                         /*c0: mov    %r12,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c3: callq  c8 <op_argary+0xc8> */
0xe9, 0xf2, 0x01, 0x00, 0x00,             /*c8: jmpq   2bf <op_argary+0x2bf> */
0x49, 0x8d, 0x44, 0x24, 0x18,             /*cd: lea    0x18(%r12),%rax */
0xeb, 0x04,                               /*d2: jmp    d8 <op_argary+0xd8> */
0x48, 0x83, 0xc0, 0x18,                   /*d4: add    $0x18,%rax */
0x41, 0xc1, 0xed, 0x0a,                   /*d8: shr    $0xa,%r13d */
0x45, 0x89, 0xee,                         /*dc: mov    %r13d,%r14d */
0x41, 0x83, 0xe6, 0x3f,                   /*df: and    $0x3f,%r14d */
0x41, 0xc1, 0xef, 0x09,                   /*e3: shr    $0x9,%r15d */
0x41, 0x83, 0xe7, 0x01,                   /*e7: and    $0x1,%r15d */
0xc1, 0xeb, 0x04,                         /*eb: shr    $0x4,%ebx */
0x89, 0xda,                               /*ee: mov    %ebx,%edx */
0x83, 0xe2, 0x1f,                         /*f0: and    $0x1f,%edx */
0x48, 0x8b, 0x08,                         /*f3: mov    (%rax),%rcx */
0x48, 0x8d, 0x69, 0x08,                   /*f6: lea    0x8(%rcx),%rbp */
0x45, 0x85, 0xff,                         /*fa: test   %r15d,%r15d */
0x74, 0x46,                               /*fd: je     145 <op_argary+0x145> */
0x44, 0x89, 0x7c, 0x24, 0x38,             /*ff: mov    %r15d,0x38(%rsp) */
0x44, 0x89, 0xf7,                         /*104: mov    %r14d,%edi */
0x8b, 0x44, 0xf9, 0x0c,                   /*107: mov    0xc(%rcx,%rdi,8),%eax */
0x45, 0x31, 0xff,                         /*10b: xor    %r15d,%r15d */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*10e: cmp    $0xfff00001,%eax */
0x72, 0x66,                               /*113: jb     17b <op_argary+0x17b> */
0x25, 0x00, 0xc0, 0x0f, 0x00,             /*115: and    $0xfc000,%eax */
0x3d, 0x00, 0xc0, 0x03, 0x00,             /*11a: cmp    $0x3c000,%eax */
0xb8, 0x00, 0x00, 0x00, 0x00,             /*11f: mov    $0x0,%eax */
0x75, 0x57,                               /*124: jne    17d <op_argary+0x17d> */
0x48, 0x8b, 0x44, 0xf9, 0x08,             /*126: mov    0x8(%rcx,%rdi,8),%rax */
0x48, 0xc1, 0xe0, 0x02,                   /*12b: shl    $0x2,%rax */
0x48, 0xbe, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*12f: movabs $0xfffffffffffc,%rsi */
0x48, 0x21, 0xf0,                         /*139: and    %rsi,%rax */
0x4c, 0x8b, 0x78, 0x28,                   /*13c: mov    0x28(%rax),%r15 */
0x8b, 0x40, 0x18,                         /*140: mov    0x18(%rax),%eax */
0xeb, 0x38,                               /*143: jmp    17d <op_argary+0x17d> */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*145: mov    0x18(%r12),%rax */
0x48, 0x89, 0x44, 0x24, 0x38,             /*14a: mov    %rax,0x38(%rsp) */
0x48, 0x89, 0xcb,                         /*14f: mov    %rcx,%rbx */
0x49, 0x8b, 0x7c, 0x24, 0x50,             /*152: mov    0x50(%r12),%rdi */
0x49, 0x89, 0xd5,                         /*157: mov    %rdx,%r13 */
0x43, 0x8d, 0x74, 0x35, 0x00,             /*15a: lea    0x0(%r13,%r14,1),%esi */
0x48, 0x89, 0xea,                         /*15f: mov    %rbp,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*162: callq  167 <op_argary+0x167> */
0x49, 0x89, 0xd8,                         /*167: mov    %rbx,%r8 */
0x48, 0x8b, 0x4c, 0x24, 0x38,             /*16a: mov    0x38(%rsp),%rcx */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*16f: mov    %rax,0xab0800(%rcx) */
0xe9, 0x16, 0x01, 0x00, 0x00,             /*176: jmpq   291 <op_argary+0x291> */
0x31, 0xc0,                               /*17b: xor    %eax,%eax */
0x48, 0x89, 0x44, 0x24, 0x10,             /*17d: mov    %rax,0x10(%rsp) */
0x48, 0x89, 0x7c, 0x24, 0x20,             /*182: mov    %rdi,0x20(%rsp) */
0x49, 0x8b, 0x74, 0x24, 0x18,             /*187: mov    0x18(%r12),%rsi */
0x48, 0x89, 0x74, 0x24, 0x08,             /*18c: mov    %rsi,0x8(%rsp) */
0x48, 0x89, 0x4c, 0x24, 0x30,             /*191: mov    %rcx,0x30(%rsp) */
0x49, 0x8b, 0x7c, 0x24, 0x50,             /*196: mov    0x50(%r12),%rdi */
0x42, 0x8d, 0x34, 0x32,                   /*19b: lea    (%rdx,%r14,1),%esi */
0x48, 0x89, 0x54, 0x24, 0x28,             /*19f: mov    %rdx,0x28(%rsp) */
0x01, 0xc6,                               /*1a4: add    %eax,%esi */
0x89, 0x74, 0x24, 0x1c,                   /*1a6: mov    %esi,0x1c(%rsp) */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1aa: callq  1af <op_argary+0x1af> */
0x4c, 0x8b, 0x44, 0x24, 0x30,             /*1af: mov    0x30(%rsp),%r8 */
0x48, 0x8b, 0x4c, 0x24, 0x08,             /*1b4: mov    0x8(%rsp),%rcx */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*1b9: mov    %rax,0xab0800(%rcx) */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*1c0: mov    0x18(%r12),%rax */
0x48, 0x8b, 0x80, 0x00, 0x08, 0xab, 0x00, /*1c5: mov    0xab0800(%rax),%rax */
0x48, 0xc1, 0xe0, 0x02,                   /*1cc: shl    $0x2,%rax */
0x48, 0xb9, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*1d0: movabs $0xfffffffffffc,%rcx */
0x48, 0x21, 0xc8,                         /*1da: and    %rcx,%rax */
0x45, 0x85, 0xf6,                         /*1dd: test   %r14d,%r14d */
0x74, 0x2c,                               /*1e0: je     20e <op_argary+0x20e> */
0x48, 0x8b, 0x48, 0x28,                   /*1e2: mov    0x28(%rax),%rcx */
0x49, 0x83, 0xe5, 0x3f,                   /*1e6: and    $0x3f,%r13 */
0x49, 0xf7, 0xdd,                         /*1ea: neg    %r13 */
0x4c, 0x89, 0xc2,                         /*1ed: mov    %r8,%rdx */
0x48, 0x89, 0xee,                         /*1f0: mov    %rbp,%rsi */
0x48, 0x83, 0xc2, 0x10,                   /*1f3: add    $0x10,%rdx */
0xf2, 0x0f, 0x10, 0x06,                   /*1f7: movsd  (%rsi),%xmm0 */
0xf2, 0x0f, 0x11, 0x01,                   /*1fb: movsd  %xmm0,(%rcx) */
0x48, 0x83, 0xc1, 0x08,                   /*1ff: add    $0x8,%rcx */
0x49, 0xff, 0xc5,                         /*203: inc    %r13 */
0x48, 0x89, 0xd5,                         /*206: mov    %rdx,%rbp */
0x48, 0x89, 0xf2,                         /*209: mov    %rsi,%rdx */
0x75, 0xe2,                               /*20c: jne    1f0 <op_argary+0x1f0> */
0x48, 0x8b, 0x7c, 0x24, 0x10,             /*20e: mov    0x10(%rsp),%rdi */
0x85, 0xff,                               /*213: test   %edi,%edi */
0x48, 0x8b, 0x6c, 0x24, 0x20,             /*215: mov    0x20(%rsp),%rbp */
0x7e, 0x2a,                               /*21a: jle    246 <op_argary+0x246> */
0x48, 0x8d, 0x0c, 0xed, 0x00, 0x00, 0x00, 0x00,/*21c: lea    0x0(,%rbp,8),%rcx */
0x48, 0x03, 0x48, 0x28,                   /*224: add    0x28(%rax),%rcx */
0x48, 0x63, 0xd7,                         /*228: movslq %edi,%rdx */
0x48, 0xf7, 0xda,                         /*22b: neg    %rdx */
0x66, 0x90,                               /*22e: xchg   %ax,%ax */
0xf2, 0x41, 0x0f, 0x10, 0x07,             /*230: movsd  (%r15),%xmm0 */
0x49, 0x83, 0xc7, 0x08,                   /*235: add    $0x8,%r15 */
0xf2, 0x0f, 0x11, 0x01,                   /*239: movsd  %xmm0,(%rcx) */
0x48, 0x83, 0xc1, 0x08,                   /*23d: add    $0x8,%rcx */
0x48, 0xff, 0xc2,                         /*241: inc    %rdx */
0x75, 0xea,                               /*244: jne    230 <op_argary+0x230> */
0x4c, 0x8b, 0x6c, 0x24, 0x28,             /*246: mov    0x28(%rsp),%r13 */
0x45, 0x85, 0xed,                         /*24b: test   %r13d,%r13d */
0x74, 0x35,                               /*24e: je     285 <op_argary+0x285> */
0x49, 0x8d, 0x4c, 0xe8, 0x10,             /*250: lea    0x10(%r8,%rbp,8),%rcx */
0x48, 0x63, 0xd7,                         /*255: movslq %edi,%rdx */
0x48, 0x01, 0xea,                         /*258: add    %rbp,%rdx */
0x48, 0xc1, 0xe2, 0x03,                   /*25b: shl    $0x3,%rdx */
0x48, 0x03, 0x50, 0x28,                   /*25f: add    0x28(%rax),%rdx */
0x48, 0x83, 0xe3, 0x1f,                   /*263: and    $0x1f,%rbx */
0x48, 0xf7, 0xdb,                         /*267: neg    %rbx */
0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00,       /*26a: nopw   0x0(%rax,%rax,1) */
0xf2, 0x0f, 0x10, 0x01,                   /*270: movsd  (%rcx),%xmm0 */
0x48, 0x83, 0xc1, 0x08,                   /*274: add    $0x8,%rcx */
0xf2, 0x0f, 0x11, 0x02,                   /*278: movsd  %xmm0,(%rdx) */
0x48, 0x83, 0xc2, 0x08,                   /*27c: add    $0x8,%rdx */
0x48, 0xff, 0xc3,                         /*280: inc    %rbx */
0x75, 0xeb,                               /*283: jne    270 <op_argary+0x270> */
0x8b, 0x4c, 0x24, 0x1c,                   /*285: mov    0x1c(%rsp),%ecx */
0x89, 0x48, 0x18,                         /*289: mov    %ecx,0x18(%rax) */
0x44, 0x8b, 0x7c, 0x24, 0x38,             /*28c: mov    0x38(%rsp),%r15d */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*291: mov    0x18(%r12),%rax */
0x45, 0x01, 0xfe,                         /*296: add    %r15d,%r14d */
0x43, 0x8d, 0x4c, 0x35, 0x01,             /*299: lea    0x1(%r13,%r14,1),%ecx */
0x48, 0x63, 0xc9,                         /*29e: movslq %ecx,%rcx */
0xf2, 0x41, 0x0f, 0x10, 0x04, 0xc8,       /*2a1: movsd  (%r8,%rcx,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x08, 0x08, 0xab, 0x00,/*2a7: movsd  %xmm0,0xab0808(%rax) */
0x41, 0x8b, 0x44, 0x24, 0x48,             /*2af: mov    0x48(%r12),%eax */
0x49, 0x8b, 0x4c, 0x24, 0x50,             /*2b4: mov    0x50(%r12),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*2b9: mov    %eax,0xdc(%rcx) */
0x4c, 0x89, 0xe7,                         /*2bf: mov    %r12,%rdi */
0x48, 0x83, 0xc4, 0x48,                   /*2c2: add    $0x48,%rsp */
0x5b,                                     /*2c6: pop    %rbx */
0x41, 0x5c,                               /*2c7: pop    %r12 */
0x41, 0x5d,                               /*2c9: pop    %r13 */
0x41, 0x5e,                               /*2cb: pop    %r14 */
0x41, 0x5f,                               /*2cd: pop    %r15 */
0x5d,                                     /*2cf: pop    %rbp */

};
static uint8_t op_argary__rodata[] = {
0x73, 0x75, 0x70, 0x65, 0x72, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x65, 0x64, 0x20, 0x6f, 0x75, 0x74,
0x73, 0x69, 0x64, 0x65, 0x20, 0x6f, 0x66, 0x20, 0x6d, 0x65, 0x74, 0x68, 0x6f, 0x64, 0x00,

};

static void op_argary_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 1 + 0;
  *((int32_t *)(op + 370)) = a * 8 + 0;
  *((int32_t *)(op + 444)) = a * 8 + 0;
  *((int32_t *)(op + 456)) = a * 8 + 0;
  *((int32_t *)(op + 683)) = a * 8 + 8;
}

static void op_argary_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_argary_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[1, 0, 23..26]]} */
static uint8_t op_enter__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x83, 0xec, 0x58,                   /*a: sub    $0x58,%rsp */
0x48, 0x89, 0xfd,                         /*e: mov    %rdi,%rbp */
0x48, 0x89, 0x6c, 0x24, 0x40,             /*11: mov    %rbp,0x40(%rsp) */
0xb8, 0x00, 0x00, 0xab, 0x00,             /*16: mov    $0xab0000,%eax */
0x41, 0x89, 0xc7,                         /*1b: mov    %eax,%r15d */
0x44, 0x89, 0xfa,                         /*1e: mov    %r15d,%edx */
0xc1, 0xea, 0x12,                         /*21: shr    $0x12,%edx */
0x83, 0xe2, 0x1f,                         /*24: and    $0x1f,%edx */
0x48, 0x89, 0x54, 0x24, 0x48,             /*27: mov    %rdx,0x48(%rsp) */
0x44, 0x89, 0xf9,                         /*2c: mov    %r15d,%ecx */
0xc1, 0xe9, 0x0d,                         /*2f: shr    $0xd,%ecx */
0x83, 0xe1, 0x1f,                         /*32: and    $0x1f,%ecx */
0x48, 0x89, 0x4c, 0x24, 0x30,             /*35: mov    %rcx,0x30(%rsp) */
0x44, 0x89, 0xfb,                         /*3a: mov    %r15d,%ebx */
0xc1, 0xeb, 0x0c,                         /*3d: shr    $0xc,%ebx */
0x83, 0xe3, 0x01,                         /*40: and    $0x1,%ebx */
0x45, 0x89, 0xfd,                         /*43: mov    %r15d,%r13d */
0x41, 0xc1, 0xed, 0x07,                   /*46: shr    $0x7,%r13d */
0x4c, 0x8b, 0x75, 0x18,                   /*4a: mov    0x18(%rbp),%r14 */
0x48, 0x8b, 0x7d, 0x50,                   /*4e: mov    0x50(%rbp),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*52: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*56: mov    0x20(%rax),%rax */
0x48, 0x63, 0x70, 0x40,                   /*5a: movslq 0x40(%rax),%rsi */
0x48, 0x89, 0x74, 0x24, 0x20,             /*5e: mov    %rsi,0x20(%rsp) */
0x8d, 0x04, 0x0a,                         /*63: lea    (%rdx,%rcx,1),%eax */
0x48, 0x89, 0x44, 0x24, 0x50,             /*66: mov    %rax,0x50(%rsp) */
0x48, 0x85, 0xf6,                         /*6b: test   %rsi,%rsi */
0x48, 0x8d, 0x46, 0x01,                   /*6e: lea    0x1(%rsi),%rax */
0xb9, 0x02, 0x00, 0x00, 0x00,             /*72: mov    $0x2,%ecx */
0x48, 0x0f, 0x49, 0xc8,                   /*77: cmovns %rax,%rcx */
0x49, 0x8d, 0x14, 0xce,                   /*7b: lea    (%r14,%rcx,8),%rdx */
0x41, 0x8b, 0x44, 0xce, 0x04,             /*7f: mov    0x4(%r14,%rcx,8),%eax */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*84: cmp    $0xfff00001,%eax */
0x72, 0x18,                               /*89: jb     a3 <op_enter+0xa3> */
0x25, 0x00, 0xc0, 0x0f, 0x00,             /*8b: and    $0xfc000,%eax */
0x3d, 0x00, 0x80, 0x03, 0x00,             /*90: cmp    $0x38000,%eax */
0x74, 0x2e,                               /*95: je     c5 <op_enter+0xc5> */
0x3d, 0x00, 0x40, 0x00, 0x00,             /*97: cmp    $0x4000,%eax */
0x75, 0x05,                               /*9c: jne    a3 <op_enter+0xa3> */
0x83, 0x3a, 0x00,                         /*9e: cmpl   $0x0,(%rdx) */
0x74, 0x22,                               /*a1: je     c5 <op_enter+0xc5> */
0x49, 0x89, 0xd4,                         /*a3: mov    %rdx,%r12 */
0x49, 0x8b, 0x34, 0x24,                   /*a6: mov    (%r12),%rsi */
0xba, 0x0d, 0x00, 0x00, 0x00,             /*aa: mov    $0xd,%edx */
0xb9, 0x00, 0x00, 0x00, 0x00,             /*af: mov    $0x0,%ecx */
0x41, 0xb8, 0x00, 0x00, 0x00, 0x00,       /*b4: mov    $0x0,%r8d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*ba: callq  bf <op_enter+0xbf> */
0x4c, 0x89, 0xe2,                         /*bf: mov    %r12,%rdx */
0x48, 0x89, 0x02,                         /*c2: mov    %rax,(%rdx) */
0x48, 0x89, 0x54, 0x24, 0x18,             /*c5: mov    %rdx,0x18(%rsp) */
0x45, 0x89, 0xec,                         /*ca: mov    %r13d,%r12d */
0x41, 0x83, 0xe4, 0x1f,                   /*cd: and    $0x1f,%r12d */
0x48, 0x8b, 0x44, 0x24, 0x50,             /*d1: mov    0x50(%rsp),%rax */
0x8d, 0x2c, 0x18,                         /*d6: lea    (%rax,%rbx,1),%ebp */
0x48, 0x89, 0x5c, 0x24, 0x28,             /*d9: mov    %rbx,0x28(%rsp) */
0x49, 0x83, 0xc6, 0x08,                   /*de: add    $0x8,%r14 */
0x4c, 0x89, 0x74, 0x24, 0x38,             /*e2: mov    %r14,0x38(%rsp) */
0xbf, 0x00, 0x00, 0x00, 0x00,             /*e7: mov    $0x0,%edi */
0x31, 0xc0,                               /*ec: xor    %eax,%eax */
0x48, 0x89, 0xd6,                         /*ee: mov    %rdx,%rsi */
0x44, 0x89, 0xfa,                         /*f1: mov    %r15d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*f4: callq  f9 <op_enter+0xf9> */
0x48, 0x8b, 0x5c, 0x24, 0x20,             /*f9: mov    0x20(%rsp),%rbx */
0x85, 0xdb,                               /*fe: test   %ebx,%ebx */
0x4d, 0x89, 0xf1,                         /*100: mov    %r14,%r9 */
0x4c, 0x8b, 0x74, 0x24, 0x40,             /*103: mov    0x40(%rsp),%r14 */
0x79, 0x30,                               /*108: jns    13a <op_enter+0x13a> */
0x49, 0x8b, 0x46, 0x18,                   /*10a: mov    0x18(%r14),%rax */
0x49, 0x8b, 0x7e, 0x50,                   /*10e: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x70, 0x08,                   /*112: mov    0x8(%rax),%rsi */
0x48, 0x8d, 0x04, 0xb5, 0x00, 0x00, 0x00, 0x00,/*116: lea    0x0(,%rsi,4),%rax */
0x48, 0xb9, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*11e: movabs $0xfffffffffffc,%rcx */
0x48, 0x21, 0xc8,                         /*128: and    %rcx,%rax */
0x4c, 0x8b, 0x78, 0x28,                   /*12b: mov    0x28(%rax),%r15 */
0x8b, 0x58, 0x18,                         /*12f: mov    0x18(%rax),%ebx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*132: callq  137 <op_enter+0x137> */
0x4d, 0x89, 0xf9,                         /*137: mov    %r15,%r9 */
0x46, 0x8d, 0x54, 0x25, 0x00,             /*13a: lea    0x0(%rbp,%r12,1),%r10d */
0x49, 0x8b, 0x7e, 0x50,                   /*13f: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*143: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*147: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x48, 0x08,                   /*14b: mov    0x8(%rax),%rcx */
0x48, 0x85, 0xc9,                         /*14f: test   %rcx,%rcx */
0x4d, 0x89, 0xe0,                         /*152: mov    %r12,%r8 */
0x49, 0x89, 0xef,                         /*155: mov    %rbp,%r15 */
0x74, 0x3f,                               /*158: je     199 <op_enter+0x199> */
0xf6, 0x41, 0x02, 0x08,                   /*15a: testb  $0x8,0x2(%rcx) */
0x74, 0x39,                               /*15e: je     199 <op_enter+0x199> */
0x85, 0xdb,                               /*160: test   %ebx,%ebx */
0x0f, 0x88, 0x9a, 0x00, 0x00, 0x00,       /*162: js     202 <op_enter+0x202> */
0x48, 0x8b, 0x4c, 0x24, 0x48,             /*168: mov    0x48(%rsp),%rcx */
0x42, 0x8d, 0x34, 0x01,                   /*16d: lea    (%rcx,%r8,1),%esi */
0x39, 0xf3,                               /*171: cmp    %esi,%ebx */
0x7c, 0x12,                               /*173: jl     187 <op_enter+0x187> */
0x48, 0x8b, 0x4c, 0x24, 0x28,             /*175: mov    0x28(%rsp),%rcx */
0x85, 0xc9,                               /*17a: test   %ecx,%ecx */
0x0f, 0x85, 0x80, 0x00, 0x00, 0x00,       /*17c: jne    202 <op_enter+0x202> */
0x44, 0x39, 0xd3,                         /*182: cmp    %r10d,%ebx */
0x7e, 0x7b,                               /*185: jle    202 <op_enter+0x202> */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*187: callq  18c <op_enter+0x18c> */
0x4c, 0x89, 0xf7,                         /*18c: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*18f: callq  194 <op_enter+0x194> */
0xe9, 0x76, 0x04, 0x00, 0x00,             /*194: jmpq   60f <op_enter+0x60f> */
0x41, 0x83, 0xfa, 0x02,                   /*199: cmp    $0x2,%r10d */
0x7c, 0x63,                               /*19d: jl     202 <op_enter+0x202> */
0x83, 0xfb, 0x01,                         /*19f: cmp    $0x1,%ebx */
0x75, 0x5e,                               /*1a2: jne    202 <op_enter+0x202> */
0x41, 0x8b, 0x49, 0x04,                   /*1a4: mov    0x4(%r9),%ecx */
0xbb, 0x01, 0x00, 0x00, 0x00,             /*1a8: mov    $0x1,%ebx */
0x81, 0xf9, 0x01, 0x00, 0xf0, 0xff,       /*1ad: cmp    $0xfff00001,%ecx */
0x72, 0x4d,                               /*1b3: jb     202 <op_enter+0x202> */
0x81, 0xe1, 0x00, 0xc0, 0x0f, 0x00,       /*1b5: and    $0xfc000,%ecx */
0x81, 0xf9, 0x00, 0xc0, 0x03, 0x00,       /*1bb: cmp    $0x3c000,%ecx */
0x75, 0x3f,                               /*1c1: jne    202 <op_enter+0x202> */
0x49, 0x8b, 0x31,                         /*1c3: mov    (%r9),%rsi */
0x4c, 0x89, 0xc5,                         /*1c6: mov    %r8,%rbp */
0x4d, 0x89, 0xcc,                         /*1c9: mov    %r9,%r12 */
0x4c, 0x89, 0xd3,                         /*1cc: mov    %r10,%rbx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1cf: callq  1d4 <op_enter+0x1d4> */
0x49, 0x89, 0xda,                         /*1d4: mov    %rbx,%r10 */
0x49, 0x89, 0xe8,                         /*1d7: mov    %rbp,%r8 */
0x49, 0x8b, 0x04, 0x24,                   /*1da: mov    (%r12),%rax */
0x48, 0xc1, 0xe0, 0x02,                   /*1de: shl    $0x2,%rax */
0x48, 0xb9, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*1e2: movabs $0xfffffffffffc,%rcx */
0x48, 0x21, 0xc8,                         /*1ec: and    %rcx,%rax */
0x8b, 0x58, 0x18,                         /*1ef: mov    0x18(%rax),%ebx */
0x4c, 0x8b, 0x48, 0x28,                   /*1f2: mov    0x28(%rax),%r9 */
0x49, 0x8b, 0x46, 0x50,                   /*1f6: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*1fa: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*1fe: mov    0x20(%rax),%rax */
0x44, 0x89, 0x50, 0x40,                   /*202: mov    %r10d,0x40(%rax) */
0x44, 0x39, 0xd3,                         /*206: cmp    %r10d,%ebx */
0x0f, 0x8d, 0xa7, 0x00, 0x00, 0x00,       /*209: jge    2b6 <op_enter+0x2b6> */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*20f: mov    0x48(%rsp),%rax */
0x46, 0x8d, 0x24, 0x00,                   /*214: lea    (%rax,%r8,1),%r12d */
0x44, 0x39, 0xe3,                         /*218: cmp    %r12d,%ebx */
0x44, 0x89, 0xc0,                         /*21b: mov    %r8d,%eax */
0x48, 0x8b, 0x74, 0x24, 0x18,             /*21e: mov    0x18(%rsp),%rsi */
0x7d, 0x0e,                               /*223: jge    233 <op_enter+0x233> */
0x31, 0xc9,                               /*225: xor    %ecx,%ecx */
0x89, 0xd8,                               /*227: mov    %ebx,%eax */
0x48, 0x8b, 0x54, 0x24, 0x48,             /*229: mov    0x48(%rsp),%rdx */
0x29, 0xd0,                               /*22e: sub    %edx,%eax */
0x0f, 0x4e, 0xc1,                         /*230: cmovle %ecx,%eax */
0x41, 0xff, 0xc2,                         /*233: inc    %r10d */
0x49, 0x8b, 0x4e, 0x18,                   /*236: mov    0x18(%r14),%rcx */
0xf2, 0x0f, 0x10, 0x06,                   /*23a: movsd  (%rsi),%xmm0 */
0xf2, 0x42, 0x0f, 0x11, 0x04, 0xd1,       /*23e: movsd  %xmm0,(%rcx,%r10,8) */
0x8d, 0x4b, 0x01,                         /*244: lea    0x1(%rbx),%ecx */
0x48, 0x63, 0xc9,                         /*247: movslq %ecx,%rcx */
0x49, 0x8b, 0x56, 0x18,                   /*24a: mov    0x18(%r14),%rdx */
0xc7, 0x44, 0xca, 0x04, 0x00, 0x40, 0xf0, 0xff,/*24e: movl   $0xfff04000,0x4(%rdx,%rcx,8) */
0x49, 0x8b, 0x56, 0x18,                   /*256: mov    0x18(%r14),%rdx */
0xc7, 0x04, 0xca, 0x00, 0x00, 0x00, 0x00, /*25a: movl   $0x0,(%rdx,%rcx,8) */
0x4c, 0x39, 0x4c, 0x24, 0x38,             /*261: cmp    %r9,0x38(%rsp) */
0x0f, 0x84, 0xf2, 0x00, 0x00, 0x00,       /*266: je     35e <op_enter+0x35e> */
0x49, 0x8b, 0x4e, 0x18,                   /*26c: mov    0x18(%r14),%rcx */
0x48, 0x8d, 0x69, 0x08,                   /*270: lea    0x8(%rcx),%rbp */
0x4c, 0x39, 0xcd,                         /*274: cmp    %r9,%rbp */
0x0f, 0x86, 0xa9, 0x00, 0x00, 0x00,       /*277: jbe    326 <op_enter+0x326> */
0x89, 0xde,                               /*27d: mov    %ebx,%esi */
0x29, 0xc6,                               /*27f: sub    %eax,%esi */
0x48, 0x63, 0xf6,                         /*281: movslq %esi,%rsi */
0x49, 0x8d, 0x3c, 0xf1,                   /*284: lea    (%r9,%rsi,8),%rdi */
0x48, 0x39, 0xfd,                         /*288: cmp    %rdi,%rbp */
0x0f, 0x83, 0x95, 0x00, 0x00, 0x00,       /*28b: jae    326 <op_enter+0x326> */
0x39, 0xc3,                               /*291: cmp    %eax,%ebx */
0x0f, 0x84, 0xc5, 0x00, 0x00, 0x00,       /*293: je     35e <op_enter+0x35e> */
0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00, /*299: nopl   0x0(%rax) */
0xf2, 0x41, 0x0f, 0x10, 0x44, 0xf1, 0xf8, /*2a0: movsd  -0x8(%r9,%rsi,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xf1,             /*2a7: movsd  %xmm0,(%rcx,%rsi,8) */
0x48, 0xff, 0xce,                         /*2ac: dec    %rsi */
0x75, 0xef,                               /*2af: jne    2a0 <op_enter+0x2a0> */
0xe9, 0xa8, 0x00, 0x00, 0x00,             /*2b1: jmpq   35e <op_enter+0x35e> */
0x4c, 0x89, 0x7c, 0x24, 0x20,             /*2b6: mov    %r15,0x20(%rsp) */
0x4c, 0x39, 0x4c, 0x24, 0x38,             /*2bb: cmp    %r9,0x38(%rsp) */
0x0f, 0x84, 0x48, 0x01, 0x00, 0x00,       /*2c0: je     40e <op_enter+0x40e> */
0x41, 0x8d, 0x42, 0x01,                   /*2c6: lea    0x1(%r10),%eax */
0x49, 0x8b, 0x4e, 0x18,                   /*2ca: mov    0x18(%r14),%rcx */
0x48, 0x8b, 0x54, 0x24, 0x18,             /*2ce: mov    0x18(%rsp),%rdx */
0xf2, 0x0f, 0x10, 0x02,                   /*2d3: movsd  (%rdx),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xc1,             /*2d7: movsd  %xmm0,(%rcx,%rax,8) */
0x49, 0x8b, 0x46, 0x18,                   /*2dc: mov    0x18(%r14),%rax */
0x48, 0x8d, 0x48, 0x08,                   /*2e0: lea    0x8(%rax),%rcx */
0x48, 0x8b, 0x54, 0x24, 0x50,             /*2e4: mov    0x50(%rsp),%rdx */
0x89, 0xd2,                               /*2e9: mov    %edx,%edx */
0x4c, 0x39, 0xc9,                         /*2eb: cmp    %r9,%rcx */
0x0f, 0x86, 0xe0, 0x00, 0x00, 0x00,       /*2ee: jbe    3d4 <op_enter+0x3d4> */
0x49, 0x8d, 0x34, 0xd1,                   /*2f4: lea    (%r9,%rdx,8),%rsi */
0x48, 0x39, 0xf1,                         /*2f8: cmp    %rsi,%rcx */
0x0f, 0x83, 0xd3, 0x00, 0x00, 0x00,       /*2fb: jae    3d4 <op_enter+0x3d4> */
0x48, 0x8b, 0x4c, 0x24, 0x50,             /*301: mov    0x50(%rsp),%rcx */
0x85, 0xc9,                               /*306: test   %ecx,%ecx */
0x0f, 0x84, 0x00, 0x01, 0x00, 0x00,       /*308: je     40e <op_enter+0x40e> */
0x66, 0x90,                               /*30e: xchg   %ax,%ax */
0xf2, 0x41, 0x0f, 0x10, 0x44, 0xd1, 0xf8, /*310: movsd  -0x8(%r9,%rdx,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xd0,             /*317: movsd  %xmm0,(%rax,%rdx,8) */
0x48, 0xff, 0xca,                         /*31c: dec    %rdx */
0x75, 0xef,                               /*31f: jne    310 <op_enter+0x310> */
0xe9, 0xe8, 0x00, 0x00, 0x00,             /*321: jmpq   40e <op_enter+0x40e> */
0x4c, 0x39, 0xcd,                         /*326: cmp    %r9,%rbp */
0x74, 0x33,                               /*329: je     35e <op_enter+0x35e> */
0x89, 0xde,                               /*32b: mov    %ebx,%esi */
0x29, 0xc6,                               /*32d: sub    %eax,%esi */
0x74, 0x2d,                               /*32f: je     35e <op_enter+0x35e> */
0x48, 0x63, 0xf6,                         /*331: movslq %esi,%rsi */
0x48, 0xf7, 0xde,                         /*334: neg    %rsi */
0x4c, 0x89, 0xcf,                         /*337: mov    %r9,%rdi */
0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00,       /*33a: nopw   0x0(%rax,%rax,1) */
0x48, 0x89, 0xea,                         /*340: mov    %rbp,%rdx */
0x48, 0x83, 0xc1, 0x10,                   /*343: add    $0x10,%rcx */
0xf2, 0x0f, 0x10, 0x07,                   /*347: movsd  (%rdi),%xmm0 */
0x48, 0x83, 0xc7, 0x08,                   /*34b: add    $0x8,%rdi */
0xf2, 0x0f, 0x11, 0x02,                   /*34f: movsd  %xmm0,(%rdx) */
0x48, 0xff, 0xc6,                         /*353: inc    %rsi */
0x48, 0x89, 0xcd,                         /*356: mov    %rcx,%rbp */
0x48, 0x89, 0xd1,                         /*359: mov    %rdx,%rcx */
0x75, 0xe2,                               /*35c: jne    340 <op_enter+0x340> */
0x4d, 0x89, 0xc5,                         /*35e: mov    %r8,%r13 */
0x85, 0xc0,                               /*361: test   %eax,%eax */
0x0f, 0x84, 0xdc, 0x01, 0x00, 0x00,       /*363: je     545 <op_enter+0x545> */
0x41, 0xff, 0xc7,                         /*369: inc    %r15d */
0x4d, 0x8b, 0x46, 0x18,                   /*36c: mov    0x18(%r14),%r8 */
0x4b, 0x8d, 0x0c, 0xf8,                   /*370: lea    (%r8,%r15,8),%rcx */
0x89, 0xda,                               /*374: mov    %ebx,%edx */
0x29, 0xc2,                               /*376: sub    %eax,%edx */
0x48, 0x63, 0xea,                         /*378: movslq %edx,%rbp */
0x49, 0x8d, 0x14, 0xe9,                   /*37b: lea    (%r9,%rbp,8),%rdx */
0x48, 0x39, 0xd1,                         /*37f: cmp    %rdx,%rcx */
0x0f, 0x86, 0x9c, 0x01, 0x00, 0x00,       /*382: jbe    524 <op_enter+0x524> */
0x48, 0x63, 0xf0,                         /*388: movslq %eax,%rsi */
0x48, 0x8d, 0x7c, 0x35, 0x00,             /*38b: lea    0x0(%rbp,%rsi,1),%rdi */
0x49, 0x8d, 0x3c, 0xf9,                   /*390: lea    (%r9,%rdi,8),%rdi */
0x48, 0x39, 0xf9,                         /*394: cmp    %rdi,%rcx */
0x0f, 0x83, 0x87, 0x01, 0x00, 0x00,       /*397: jae    524 <op_enter+0x524> */
0x48, 0x8b, 0x44, 0x24, 0x50,             /*39d: mov    0x50(%rsp),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x28,             /*3a2: mov    0x28(%rsp),%rcx */
0x8d, 0x44, 0x01, 0x01,                   /*3a7: lea    0x1(%rcx,%rax,1),%eax */
0x49, 0x8d, 0x44, 0xc0, 0xf8,             /*3ab: lea    -0x8(%r8,%rax,8),%rax */
0x49, 0x8d, 0x4c, 0xe9, 0xf8,             /*3b0: lea    -0x8(%r9,%rbp,8),%rcx */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*3b5: data16 nopw %cs:0x0(%rax,%rax,1) */
0xf2, 0x0f, 0x10, 0x04, 0xf1,             /*3c0: movsd  (%rcx,%rsi,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xf0,             /*3c5: movsd  %xmm0,(%rax,%rsi,8) */
0x48, 0xff, 0xce,                         /*3ca: dec    %rsi */
0x75, 0xf1,                               /*3cd: jne    3c0 <op_enter+0x3c0> */
0xe9, 0x71, 0x01, 0x00, 0x00,             /*3cf: jmpq   545 <op_enter+0x545> */
0x4c, 0x39, 0xc9,                         /*3d4: cmp    %r9,%rcx */
0x74, 0x35,                               /*3d7: je     40e <op_enter+0x40e> */
0x48, 0x8b, 0x54, 0x24, 0x50,             /*3d9: mov    0x50(%rsp),%rdx */
0x85, 0xd2,                               /*3de: test   %edx,%edx */
0x74, 0x2c,                               /*3e0: je     40e <op_enter+0x40e> */
0x48, 0x8b, 0x54, 0x24, 0x50,             /*3e2: mov    0x50(%rsp),%rdx */
0x48, 0xf7, 0xda,                         /*3e7: neg    %rdx */
0x4c, 0x89, 0xce,                         /*3ea: mov    %r9,%rsi */
0x0f, 0x1f, 0x00,                         /*3ed: nopl   (%rax) */
0x48, 0x89, 0xcf,                         /*3f0: mov    %rcx,%rdi */
0x48, 0x83, 0xc0, 0x10,                   /*3f3: add    $0x10,%rax */
0xf2, 0x0f, 0x10, 0x06,                   /*3f7: movsd  (%rsi),%xmm0 */
0x48, 0x83, 0xc6, 0x08,                   /*3fb: add    $0x8,%rsi */
0xf2, 0x0f, 0x11, 0x07,                   /*3ff: movsd  %xmm0,(%rdi) */
0x48, 0xff, 0xc2,                         /*403: inc    %rdx */
0x48, 0x89, 0xc1,                         /*406: mov    %rax,%rcx */
0x48, 0x89, 0xf8,                         /*409: mov    %rdi,%rax */
0x75, 0xe2,                               /*40c: jne    3f0 <op_enter+0x3f0> */
0x45, 0x31, 0xff,                         /*40e: xor    %r15d,%r15d */
0x48, 0x8b, 0x44, 0x24, 0x28,             /*411: mov    0x28(%rsp),%rax */
0x85, 0xc0,                               /*416: test   %eax,%eax */
0x74, 0x69,                               /*418: je     483 <op_enter+0x483> */
0x48, 0x8b, 0x44, 0x24, 0x50,             /*41a: mov    0x50(%rsp),%rax */
0x48, 0x89, 0xc1,                         /*41f: mov    %rax,%rcx */
0x42, 0x8d, 0x04, 0x01,                   /*422: lea    (%rcx,%r8,1),%eax */
0x41, 0x89, 0xdf,                         /*426: mov    %ebx,%r15d */
0x41, 0x29, 0xc7,                         /*429: sub    %eax,%r15d */
0x8d, 0x41, 0x01,                         /*42c: lea    0x1(%rcx),%eax */
0x48, 0x89, 0x44, 0x24, 0x10,             /*42f: mov    %rax,0x10(%rsp) */
0x49, 0x8b, 0x46, 0x18,                   /*434: mov    0x18(%r14),%rax */
0x48, 0x89, 0x44, 0x24, 0x08,             /*438: mov    %rax,0x8(%rsp) */
0x49, 0x8b, 0x7e, 0x50,                   /*43d: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*441: mov    0x48(%rsp),%rax */
0x89, 0xc0,                               /*446: mov    %eax,%eax */
0x48, 0x8b, 0x4c, 0x24, 0x30,             /*448: mov    0x30(%rsp),%rcx */
0x89, 0xc9,                               /*44d: mov    %ecx,%ecx */
0x48, 0x01, 0xc1,                         /*44f: add    %rax,%rcx */
0x49, 0x8d, 0x14, 0xc9,                   /*452: lea    (%r9,%rcx,8),%rdx */
0x44, 0x89, 0xfe,                         /*456: mov    %r15d,%esi */
0x4c, 0x89, 0xc5,                         /*459: mov    %r8,%rbp */
0x4d, 0x89, 0xce,                         /*45c: mov    %r9,%r14 */
0x4d, 0x89, 0xd4,                         /*45f: mov    %r10,%r12 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*462: callq  467 <op_enter+0x467> */
0x4d, 0x89, 0xe2,                         /*467: mov    %r12,%r10 */
0x4d, 0x89, 0xf1,                         /*46a: mov    %r14,%r9 */
0x49, 0x89, 0xe8,                         /*46d: mov    %rbp,%r8 */
0x4c, 0x8b, 0x74, 0x24, 0x40,             /*470: mov    0x40(%rsp),%r14 */
0x48, 0x8b, 0x4c, 0x24, 0x10,             /*475: mov    0x10(%rsp),%rcx */
0x48, 0x8b, 0x54, 0x24, 0x08,             /*47a: mov    0x8(%rsp),%rdx */
0x48, 0x89, 0x04, 0xca,                   /*47f: mov    %rax,(%rdx,%rcx,8) */
0x45, 0x85, 0xc0,                         /*483: test   %r8d,%r8d */
0x0f, 0x84, 0x39, 0x01, 0x00, 0x00,       /*486: je     5c5 <op_enter+0x5c5> */
0x44, 0x29, 0xc3,                         /*48c: sub    %r8d,%ebx */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*48f: mov    0x48(%rsp),%rax */
0x39, 0xc3,                               /*494: cmp    %eax,%ebx */
0x0f, 0x8e, 0x29, 0x01, 0x00, 0x00,       /*496: jle    5c5 <op_enter+0x5c5> */
0x48, 0x8b, 0x44, 0x24, 0x20,             /*49c: mov    0x20(%rsp),%rax */
0xff, 0xc0,                               /*4a1: inc    %eax */
0x49, 0x8b, 0x76, 0x18,                   /*4a3: mov    0x18(%r14),%rsi */
0x48, 0x8d, 0x04, 0xc6,                   /*4a7: lea    (%rsi,%rax,8),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x50,             /*4ab: mov    0x50(%rsp),%rcx */
0x41, 0x8d, 0x0c, 0x0f,                   /*4b0: lea    (%r15,%rcx,1),%ecx */
0x48, 0x63, 0xf9,                         /*4b4: movslq %ecx,%rdi */
0x49, 0x8d, 0x0c, 0xf9,                   /*4b7: lea    (%r9,%rdi,8),%rcx */
0x44, 0x89, 0xc2,                         /*4bb: mov    %r8d,%edx */
0x48, 0x39, 0xc8,                         /*4be: cmp    %rcx,%rax */
0x0f, 0x86, 0xd8, 0x00, 0x00, 0x00,       /*4c1: jbe    59f <op_enter+0x59f> */
0x48, 0x01, 0xd7,                         /*4c7: add    %rdx,%rdi */
0x49, 0x8d, 0x3c, 0xf9,                   /*4ca: lea    (%r9,%rdi,8),%rdi */
0x48, 0x39, 0xf8,                         /*4ce: cmp    %rdi,%rax */
0x0f, 0x83, 0xc8, 0x00, 0x00, 0x00,       /*4d1: jae    59f <op_enter+0x59f> */
0x48, 0x8b, 0x44, 0x24, 0x50,             /*4d7: mov    0x50(%rsp),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x28,             /*4dc: mov    0x28(%rsp),%rcx */
0x8d, 0x44, 0x01, 0x01,                   /*4e1: lea    0x1(%rcx,%rax,1),%eax */
0x48, 0x8d, 0x44, 0xc6, 0xf8,             /*4e5: lea    -0x8(%rsi,%rax,8),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x48,             /*4ea: mov    0x48(%rsp),%rcx */
0x41, 0x01, 0xcf,                         /*4ef: add    %ecx,%r15d */
0x48, 0x8b, 0x4c, 0x24, 0x30,             /*4f2: mov    0x30(%rsp),%rcx */
0x41, 0x01, 0xcf,                         /*4f7: add    %ecx,%r15d */
0x49, 0x63, 0xcf,                         /*4fa: movslq %r15d,%rcx */
0x49, 0x8d, 0x4c, 0xc9, 0xf8,             /*4fd: lea    -0x8(%r9,%rcx,8),%rcx */
0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*502: data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0xf2, 0x0f, 0x10, 0x04, 0xd1,             /*510: movsd  (%rcx,%rdx,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xd0,             /*515: movsd  %xmm0,(%rax,%rdx,8) */
0x48, 0xff, 0xca,                         /*51a: dec    %rdx */
0x75, 0xf1,                               /*51d: jne    510 <op_enter+0x510> */
0xe9, 0xa1, 0x00, 0x00, 0x00,             /*51f: jmpq   5c5 <op_enter+0x5c5> */
0x48, 0x39, 0xd1,                         /*524: cmp    %rdx,%rcx */
0x74, 0x1c,                               /*527: je     545 <op_enter+0x545> */
0x48, 0x98,                               /*529: cltq */
0x48, 0xf7, 0xd8,                         /*52b: neg    %rax */
0x66, 0x90,                               /*52e: xchg   %ax,%ax */
0xf2, 0x0f, 0x10, 0x02,                   /*530: movsd  (%rdx),%xmm0 */
0x48, 0x83, 0xc2, 0x08,                   /*534: add    $0x8,%rdx */
0xf2, 0x0f, 0x11, 0x01,                   /*538: movsd  %xmm0,(%rcx) */
0x48, 0x83, 0xc1, 0x08,                   /*53c: add    $0x8,%rcx */
0x48, 0xff, 0xc0,                         /*540: inc    %rax */
0x75, 0xeb,                               /*543: jne    530 <op_enter+0x530> */
0x48, 0x8b, 0x44, 0x24, 0x28,             /*545: mov    0x28(%rsp),%rax */
0x85, 0xc0,                               /*54a: test   %eax,%eax */
0x74, 0x1a,                               /*54c: je     568 <op_enter+0x568> */
0x48, 0x8b, 0x6c, 0x24, 0x50,             /*54e: mov    0x50(%rsp),%rbp */
0xff, 0xc5,                               /*553: inc    %ebp */
0x4d, 0x8b, 0x7e, 0x18,                   /*555: mov    0x18(%r14),%r15 */
0x49, 0x8b, 0x7e, 0x50,                   /*559: mov    0x50(%r14),%rdi */
0x31, 0xf6,                               /*55d: xor    %esi,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*55f: callq  564 <op_enter+0x564> */
0x49, 0x89, 0x04, 0xef,                   /*564: mov    %rax,(%r15,%rbp,8) */
0x48, 0x8b, 0x44, 0x24, 0x30,             /*568: mov    0x30(%rsp),%rax */
0x85, 0xc0,                               /*56d: test   %eax,%eax */
0x49, 0x8b, 0x46, 0x08,                   /*56f: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x88, 0xa8, 0x00, 0x00, 0x00, /*573: mov    0xa8(%rax),%rcx */
0x0f, 0x84, 0x7e, 0x00, 0x00, 0x00,       /*57a: je     5fe <op_enter+0x5fe> */
0x44, 0x39, 0xe3,                         /*580: cmp    %r12d,%ebx */
0x7c, 0x79,                               /*583: jl     5fe <op_enter+0x5fe> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*585: mov    $0x1,%edx */
0x48, 0x8b, 0x74, 0x24, 0x48,             /*58a: mov    0x48(%rsp),%rsi */
0x29, 0xf2,                               /*58f: sub    %esi,%edx */
0x44, 0x29, 0xea,                         /*591: sub    %r13d,%edx */
0x01, 0xda,                               /*594: add    %ebx,%edx */
0x48, 0x63, 0xd2,                         /*596: movslq %edx,%rdx */
0x48, 0x8d, 0x0c, 0x91,                   /*599: lea    (%rcx,%rdx,4),%rcx */
0xeb, 0x63,                               /*59d: jmp    602 <op_enter+0x602> */
0x48, 0x39, 0xc8,                         /*59f: cmp    %rcx,%rax */
0x74, 0x21,                               /*5a2: je     5c5 <op_enter+0x5c5> */
0x49, 0x83, 0xe5, 0x1f,                   /*5a4: and    $0x1f,%r13 */
0x49, 0xf7, 0xdd,                         /*5a8: neg    %r13 */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*5ab: nopl   0x0(%rax,%rax,1) */
0xf2, 0x0f, 0x10, 0x01,                   /*5b0: movsd  (%rcx),%xmm0 */
0x48, 0x83, 0xc1, 0x08,                   /*5b4: add    $0x8,%rcx */
0xf2, 0x0f, 0x11, 0x00,                   /*5b8: movsd  %xmm0,(%rax) */
0x48, 0x83, 0xc0, 0x08,                   /*5bc: add    $0x8,%rax */
0x49, 0xff, 0xc5,                         /*5c0: inc    %r13 */
0x75, 0xeb,                               /*5c3: jne    5b0 <op_enter+0x5b0> */
0x4c, 0x39, 0x4c, 0x24, 0x38,             /*5c5: cmp    %r9,0x38(%rsp) */
0x75, 0x16,                               /*5ca: jne    5e2 <op_enter+0x5e2> */
0x41, 0xff, 0xc2,                         /*5cc: inc    %r10d */
0x49, 0x8b, 0x46, 0x18,                   /*5cf: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x18,             /*5d3: mov    0x18(%rsp),%rcx */
0xf2, 0x0f, 0x10, 0x01,                   /*5d8: movsd  (%rcx),%xmm0 */
0xf2, 0x42, 0x0f, 0x11, 0x04, 0xd0,       /*5dc: movsd  %xmm0,(%rax,%r10,8) */
0x48, 0x8b, 0x54, 0x24, 0x30,             /*5e2: mov    0x30(%rsp),%rdx */
0x85, 0xd2,                               /*5e7: test   %edx,%edx */
0x49, 0x8b, 0x46, 0x08,                   /*5e9: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x88, 0xa8, 0x00, 0x00, 0x00, /*5ed: mov    0xa8(%rax),%rcx */
0x74, 0x08,                               /*5f4: je     5fe <op_enter+0x5fe> */
0xff, 0xc2,                               /*5f6: inc    %edx */
0x48, 0x8d, 0x0c, 0x91,                   /*5f8: lea    (%rcx,%rdx,4),%rcx */
0xeb, 0x04,                               /*5fc: jmp    602 <op_enter+0x602> */
0x48, 0x83, 0xc1, 0x04,                   /*5fe: add    $0x4,%rcx */
0x0f, 0xb7, 0x31,                         /*602: movzwl (%rcx),%esi */
0x48, 0x03, 0xb0, 0xc0, 0x00, 0x00, 0x00, /*605: add    0xc0(%rax),%rsi */
0x4c, 0x89, 0xf7,                         /*60c: mov    %r14,%rdi */
0x4c, 0x89, 0xf7,                         /*60f: mov    %r14,%rdi */
0x48, 0x83, 0xc4, 0x58,                   /*612: add    $0x58,%rsp */
0x5b,                                     /*616: pop    %rbx */
0x41, 0x5c,                               /*617: pop    %r12 */
0x41, 0x5d,                               /*619: pop    %r13 */
0x41, 0x5e,                               /*61b: pop    %r14 */
0x41, 0x5f,                               /*61d: pop    %r15 */
0x5d,                                     /*61f: pop    %rbp */
0xff, 0xe6,                               /*620: jmpq   *%rsi */

};
static uint8_t op_enter__rodata[] = {
0x50, 0x72, 0x6f, 0x63, 0x00, 0x74, 0x6f, 0x5f, 0x70, 0x72, 0x6f, 0x63, 0x00, 0x00, 0x00, 0x00,
0x6f, 0x70, 0x5f, 0x65, 0x6e, 0x74, 0x65, 0x72, 0x3a, 0x20, 0x25, 0x70, 0x20, 0x28, 0x25, 0x64,
0x29, 0x0a, 0x00,                         

};

static void op_enter_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 23)) = a * 1 + 0;
}

static void op_enter_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_enter_set_args(op, GETARG_Ax(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 18..21]]} */
static uint8_t op_enter_method_m__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x83, 0xec, 0x18,                   /*a: sub    $0x18,%rsp */
0x48, 0x89, 0xfb,                         /*e: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xab, 0x00,             /*11: mov    $0xab0000,%eax */
0x41, 0x89, 0xc5,                         /*16: mov    %eax,%r13d */
0x4c, 0x8b, 0x63, 0x18,                   /*19: mov    0x18(%rbx),%r12 */
0x48, 0x8b, 0x7b, 0x50,                   /*1d: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*21: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*25: mov    0x20(%rax),%rax */
0x4c, 0x63, 0x70, 0x40,                   /*29: movslq 0x40(%rax),%r14 */
0x49, 0x8d, 0x46, 0x01,                   /*2d: lea    0x1(%r14),%rax */
0x4d, 0x85, 0xf6,                         /*31: test   %r14,%r14 */
0xbd, 0x02, 0x00, 0x00, 0x00,             /*34: mov    $0x2,%ebp */
0x48, 0x0f, 0x49, 0xe8,                   /*39: cmovns %rax,%rbp */
0x41, 0x8b, 0x44, 0xec, 0x04,             /*3d: mov    0x4(%r12,%rbp,8),%eax */
0xb1, 0x01,                               /*42: mov    $0x1,%cl */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*44: cmp    $0xfff00001,%eax */
0x72, 0x1f,                               /*49: jb     6a <op_enter_method_m+0x6a> */
0x25, 0x00, 0xc0, 0x0f, 0x00,             /*4b: and    $0xfc000,%eax */
0x3d, 0x00, 0x40, 0x00, 0x00,             /*50: cmp    $0x4000,%eax */
0x75, 0x07,                               /*55: jne    5e <op_enter_method_m+0x5e> */
0x41, 0x83, 0x3c, 0xec, 0x00,             /*57: cmpl   $0x0,(%r12,%rbp,8) */
0x74, 0x0a,                               /*5c: je     68 <op_enter_method_m+0x68> */
0x3d, 0x00, 0x80, 0x03, 0x00,             /*5e: cmp    $0x38000,%eax */
0x0f, 0x95, 0xc1,                         /*63: setne  %cl */
0xeb, 0x02,                               /*66: jmp    6a <op_enter_method_m+0x6a> */
0x31, 0xc9,                               /*68: xor    %ecx,%ecx */
0x48, 0x89, 0x5c, 0x24, 0x10,             /*6a: mov    %rbx,0x10(%rsp) */
0x41, 0xc1, 0xed, 0x12,                   /*6f: shr    $0x12,%r13d */
0x84, 0xc9,                               /*73: test   %cl,%cl */
0x74, 0x1d,                               /*75: je     94 <op_enter_method_m+0x94> */
0x49, 0x8b, 0x34, 0xec,                   /*77: mov    (%r12,%rbp,8),%rsi */
0xba, 0x0d, 0x00, 0x00, 0x00,             /*7b: mov    $0xd,%edx */
0xb9, 0x00, 0x00, 0x00, 0x00,             /*80: mov    $0x0,%ecx */
0x41, 0xb8, 0x00, 0x00, 0x00, 0x00,       /*85: mov    $0x0,%r8d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*8b: callq  90 <op_enter_method_m+0x90> */
0x49, 0x89, 0x04, 0xec,                   /*90: mov    %rax,(%r12,%rbp,8) */
0x45, 0x89, 0xef,                         /*94: mov    %r13d,%r15d */
0x41, 0x83, 0xe7, 0x1f,                   /*97: and    $0x1f,%r15d */
0x49, 0x8d, 0x54, 0x24, 0x08,             /*9b: lea    0x8(%r12),%rdx */
0x45, 0x85, 0xf6,                         /*a0: test   %r14d,%r14d */
0x78, 0x26,                               /*a3: js     cb <op_enter_method_m+0xcb> */
0x45, 0x39, 0xfe,                         /*a5: cmp    %r15d,%r14d */
0x48, 0x89, 0xd3,                         /*a8: mov    %rdx,%rbx */
0x4c, 0x8b, 0x74, 0x24, 0x10,             /*ab: mov    0x10(%rsp),%r14 */
0x7d, 0x4e,                               /*b0: jge    100 <op_enter_method_m+0x100> */
0x49, 0x8b, 0x7e, 0x50,                   /*b2: mov    0x50(%r14),%rdi */
0x44, 0x89, 0xfe,                         /*b6: mov    %r15d,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*b9: callq  be <op_enter_method_m+0xbe> */
0x4c, 0x89, 0xf7,                         /*be: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c1: callq  c6 <op_enter_method_m+0xc6> */
0xe9, 0xec, 0x00, 0x00, 0x00,             /*c6: jmpq   1b7 <op_enter_method_m+0x1b7> */
0x4c, 0x8b, 0x74, 0x24, 0x10,             /*cb: mov    0x10(%rsp),%r14 */
0x49, 0x8b, 0x46, 0x18,                   /*d0: mov    0x18(%r14),%rax */
0x49, 0x8b, 0x7e, 0x50,                   /*d4: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x70, 0x08,                   /*d8: mov    0x8(%rax),%rsi */
0x48, 0xb8, 0xff, 0xff, 0xff, 0xff, 0xff, 0x3f, 0x00, 0x00,/*dc: movabs $0x3fffffffffff,%rax */
0x48, 0x21, 0xf0,                         /*e6: and    %rsi,%rax */
0x48, 0x8b, 0x1c, 0x85, 0x28, 0x00, 0x00, 0x00,/*e9: mov    0x28(,%rax,4),%rbx */
0x48, 0x89, 0x54, 0x24, 0x08,             /*f1: mov    %rdx,0x8(%rsp) */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*f6: callq  fb <op_enter_method_m+0xfb> */
0x48, 0x8b, 0x54, 0x24, 0x08,             /*fb: mov    0x8(%rsp),%rdx */
0x49, 0x8b, 0x46, 0x50,                   /*100: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*104: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*108: mov    0x20(%rax),%rax */
0x44, 0x89, 0x78, 0x40,                   /*10c: mov    %r15d,0x40(%rax) */
0x41, 0x8d, 0x47, 0x01,                   /*110: lea    0x1(%r15),%eax */
0x49, 0x8b, 0x4e, 0x18,                   /*114: mov    0x18(%r14),%rcx */
0xf2, 0x41, 0x0f, 0x10, 0x04, 0xec,       /*118: movsd  (%r12,%rbp,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xc1,             /*11e: movsd  %xmm0,(%rcx,%rax,8) */
0x48, 0x39, 0xda,                         /*123: cmp    %rbx,%rdx */
0x74, 0x76,                               /*126: je     19e <op_enter_method_m+0x19e> */
0x49, 0x8b, 0x46, 0x18,                   /*128: mov    0x18(%r14),%rax */
0x48, 0x8d, 0x48, 0x08,                   /*12c: lea    0x8(%rax),%rcx */
0x44, 0x89, 0xfa,                         /*130: mov    %r15d,%edx */
0x48, 0x39, 0xd9,                         /*133: cmp    %rbx,%rcx */
0x76, 0x2a,                               /*136: jbe    162 <op_enter_method_m+0x162> */
0x48, 0x8d, 0x34, 0xd3,                   /*138: lea    (%rbx,%rdx,8),%rsi */
0x48, 0x39, 0xf1,                         /*13c: cmp    %rsi,%rcx */
0x73, 0x21,                               /*13f: jae    162 <op_enter_method_m+0x162> */
0x45, 0x85, 0xff,                         /*141: test   %r15d,%r15d */
0x74, 0x58,                               /*144: je     19e <op_enter_method_m+0x19e> */
0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*146: nopw   %cs:0x0(%rax,%rax,1) */
0xf2, 0x0f, 0x10, 0x44, 0xd3, 0xf8,       /*150: movsd  -0x8(%rbx,%rdx,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xd0,             /*156: movsd  %xmm0,(%rax,%rdx,8) */
0x48, 0xff, 0xca,                         /*15b: dec    %rdx */
0x75, 0xf0,                               /*15e: jne    150 <op_enter_method_m+0x150> */
0xeb, 0x3c,                               /*160: jmp    19e <op_enter_method_m+0x19e> */
0x48, 0x39, 0xd9,                         /*162: cmp    %rbx,%rcx */
0x74, 0x37,                               /*165: je     19e <op_enter_method_m+0x19e> */
0x45, 0x85, 0xff,                         /*167: test   %r15d,%r15d */
0x74, 0x32,                               /*16a: je     19e <op_enter_method_m+0x19e> */
0x49, 0x83, 0xe5, 0x1f,                   /*16c: and    $0x1f,%r13 */
0x49, 0xf7, 0xdd,                         /*170: neg    %r13 */
0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*173: data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x89, 0xca,                         /*180: mov    %rcx,%rdx */
0x48, 0x83, 0xc0, 0x10,                   /*183: add    $0x10,%rax */
0xf2, 0x0f, 0x10, 0x03,                   /*187: movsd  (%rbx),%xmm0 */
0x48, 0x83, 0xc3, 0x08,                   /*18b: add    $0x8,%rbx */
0xf2, 0x0f, 0x11, 0x02,                   /*18f: movsd  %xmm0,(%rdx) */
0x49, 0xff, 0xc5,                         /*193: inc    %r13 */
0x48, 0x89, 0xc1,                         /*196: mov    %rax,%rcx */
0x48, 0x89, 0xd0,                         /*199: mov    %rdx,%rax */
0x75, 0xe2,                               /*19c: jne    180 <op_enter_method_m+0x180> */
0x49, 0x8b, 0x46, 0x08,                   /*19e: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x88, 0xa8, 0x00, 0x00, 0x00, /*1a2: mov    0xa8(%rax),%rcx */
0x0f, 0xb7, 0x71, 0x04,                   /*1a9: movzwl 0x4(%rcx),%esi */
0x48, 0x03, 0xb0, 0xc0, 0x00, 0x00, 0x00, /*1ad: add    0xc0(%rax),%rsi */
0x4c, 0x89, 0xf7,                         /*1b4: mov    %r14,%rdi */
0x4c, 0x89, 0xf7,                         /*1b7: mov    %r14,%rdi */
0x48, 0x83, 0xc4, 0x18,                   /*1ba: add    $0x18,%rsp */
0x5b,                                     /*1be: pop    %rbx */
0x41, 0x5c,                               /*1bf: pop    %r12 */
0x41, 0x5d,                               /*1c1: pop    %r13 */
0x41, 0x5e,                               /*1c3: pop    %r14 */
0x41, 0x5f,                               /*1c5: pop    %r15 */
0x5d,                                     /*1c7: pop    %rbp */
0xff, 0xe6,                               /*1c8: jmpq   *%rsi */

};
static uint8_t op_enter_method_m__rodata[] = {
0x50, 0x72, 0x6f, 0x63, 0x00, 0x74, 0x6f, 0x5f, 0x70, 0x72, 0x6f, 0x63, 0x00,

};

static void op_enter_method_m_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = a * 1 + 0;
}

static void op_enter_method_m_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_enter_method_m_set_args(op, GETARG_Ax(c),0,0,op_idx);
}


/* args: {} */
static uint8_t op_karg__text[] = {

};
static uint8_t op_karg__rodata[] = {

};

static void op_karg_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_karg_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_karg_set_args(op, 0,0,0,op_idx);
}


/* args: {} */
static uint8_t op_kdict__text[] = {

};
static uint8_t op_kdict__rodata[] = {

};

static void op_kdict_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_kdict_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_kdict_set_args(op, 0,0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 17..20]]} */
static uint8_t op_return__text[] = {
0x55,                                     /*0: push   %rbp */
0x48, 0x89, 0xe5,                         /*1: mov    %rsp,%rbp */
0x53,                                     /*4: push   %rbx */
0x48, 0x83, 0xe4, 0xf0,                   /*5: and    $0xfffffffffffffff0,%rsp */
0x48, 0x83, 0xec, 0x10,                   /*9: sub    $0x10,%rsp */
0x48, 0x89, 0xfb,                         /*d: mov    %rdi,%rbx */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*10: mov    $0xab0000,%esi */
0x31, 0xd2,                               /*15: xor    %edx,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*17: callq  1c <op_return+0x1c> */
0x48, 0x89, 0xdf,                         /*1c: mov    %rbx,%rdi */
0x48, 0x8d, 0x65, 0xf8,                   /*1f: lea    -0x8(%rbp),%rsp */
0x5b,                                     /*23: pop    %rbx */
0x5d,                                     /*24: pop    %rbp */
0xc3,                                     /*25: retq */

};
static uint8_t op_return__rodata[] = {

};

static void op_return_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 1 + 0;
}

static void op_return_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_return_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 5..8]], "b"=>[[1, 0, 10..13]]} */
static uint8_t op_break__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*4: mov    $0xab0000,%esi */
0xba, 0x00, 0x00, 0xbc, 0x00,             /*9: mov    $0xbc0000,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*e: callq  13 <op_break+0x13> */
0x48, 0x89, 0xdf,                         /*13: mov    %rbx,%rdi */
0x5b,                                     /*16: pop    %rbx */
0xc3,                                     /*17: retq */

};
static uint8_t op_break__rodata[] = {

};

static void op_break_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = a * 1 + 0;
  *((int32_t *)(op + 10)) = b * 1 + 0;
}

static void op_break_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_break_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 28..31]], "a"=>[[8, 0, 35..38], [1, 1, 321..324], [8, 8, 347..350], [8, 0, 396..399], [1, 0, 526..529]], "c"=>[[1, 0, 235..238], [1, 1, 352..355]]} */
static uint8_t op_tailcall__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x89, 0xfb,                         /*a: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x50,                   /*d: mov    0x50(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x18,                   /*11: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*15: mov    0x28(%rbx),%rcx */
0x44, 0x8b, 0xa1, 0x00, 0x04, 0xbc, 0x00, /*19: mov    0xbc0400(%rcx),%r12d */
0x4c, 0x8b, 0xb8, 0x00, 0x08, 0xab, 0x00, /*20: mov    0xab0800(%rax),%r15 */
0x4c, 0x89, 0xf8,                         /*27: mov    %r15,%rax */
0x48, 0xc1, 0xe8, 0x20,                   /*2a: shr    $0x20,%rax */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*2e: cmp    $0xfff00001,%eax */
0x72, 0x31,                               /*33: jb     66 <op_tailcall+0x66> */
0x4c, 0x89, 0xf9,                         /*35: mov    %r15,%rcx */
0x48, 0xc1, 0xe9, 0x2e,                   /*38: shr    $0x2e,%rcx */
0x83, 0xe1, 0x3f,                         /*3c: and    $0x3f,%ecx */
0x31, 0xc0,                               /*3f: xor    %eax,%eax */
0x83, 0xf9, 0x14,                         /*41: cmp    $0x14,%ecx */
0x7f, 0x26,                               /*44: jg     6c <op_tailcall+0x6c> */
0xff, 0xc9,                               /*46: dec    %ecx */
0x83, 0xf9, 0x07,                         /*48: cmp    $0x7,%ecx */
0x77, 0x24,                               /*4b: ja     71 <op_tailcall+0x71> */
0xff, 0x24, 0xcd, 0x00, 0x00, 0x00, 0x00, /*4d: jmpq   *0x0(,%rcx,8) */
0x45, 0x85, 0xff,                         /*54: test   %r15d,%r15d */
0x0f, 0x84, 0x64, 0x02, 0x00, 0x00,       /*57: je     2c1 <op_tailcall+0x2c1> */
0x49, 0x8b, 0x86, 0x90, 0x00, 0x00, 0x00, /*5d: mov    0x90(%r14),%rax */
0xeb, 0x20,                               /*64: jmp    86 <op_tailcall+0x86> */
0x49, 0x8b, 0x46, 0x78,                   /*66: mov    0x78(%r14),%rax */
0xeb, 0x1a,                               /*6a: jmp    86 <op_tailcall+0x86> */
0x83, 0xf9, 0x15,                         /*6c: cmp    $0x15,%ecx */
0x74, 0x15,                               /*6f: je     86 <op_tailcall+0x86> */
0x48, 0xb8, 0xff, 0xff, 0xff, 0xff, 0xff, 0x3f, 0x00, 0x00,/*71: movabs $0x3fffffffffff,%rax */
0x4c, 0x21, 0xf8,                         /*7b: and    %r15,%rax */
0x48, 0x8b, 0x04, 0x85, 0x08, 0x00, 0x00, 0x00,/*7e: mov    0x8(,%rax,4),%rax */
0x48, 0x89, 0x04, 0x24,                   /*86: mov    %rax,(%rsp) */
0x48, 0x8b, 0x0b,                         /*8a: mov    (%rbx),%rcx */
0x48, 0x39, 0x41, 0x38,                   /*8d: cmp    %rax,0x38(%rcx) */
0x75, 0x08,                               /*91: jne    9b <op_tailcall+0x9b> */
0x31, 0xd2,                               /*93: xor    %edx,%edx */
0x44, 0x39, 0x61, 0x30,                   /*95: cmp    %r12d,0x30(%rcx) */
0x74, 0x33,                               /*99: je     ce <op_tailcall+0xce> */
0x48, 0x39, 0x41, 0x48,                   /*9b: cmp    %rax,0x48(%rcx) */
0x75, 0x0b,                               /*9f: jne    ac <op_tailcall+0xac> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*a1: mov    $0x1,%edx */
0x44, 0x39, 0x61, 0x40,                   /*a6: cmp    %r12d,0x40(%rcx) */
0x74, 0x22,                               /*aa: je     ce <op_tailcall+0xce> */
0x48, 0x39, 0x41, 0x58,                   /*ac: cmp    %rax,0x58(%rcx) */
0x75, 0x0b,                               /*b0: jne    bd <op_tailcall+0xbd> */
0xba, 0x02, 0x00, 0x00, 0x00,             /*b2: mov    $0x2,%edx */
0x44, 0x39, 0x61, 0x50,                   /*b7: cmp    %r12d,0x50(%rcx) */
0x74, 0x11,                               /*bb: je     ce <op_tailcall+0xce> */
0x48, 0x39, 0x41, 0x68,                   /*bd: cmp    %rax,0x68(%rcx) */
0x75, 0x15,                               /*c1: jne    d8 <op_tailcall+0xd8> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*c3: mov    $0x3,%edx */
0x44, 0x39, 0x61, 0x60,                   /*c8: cmp    %r12d,0x60(%rcx) */
0x75, 0x0a,                               /*cc: jne    d8 <op_tailcall+0xd8> */
0x4c, 0x8b, 0xac, 0xd1, 0x90, 0x00, 0x00, 0x00,/*ce: mov    0x90(%rcx,%rdx,8),%r13 */
0xeb, 0x12,                               /*d6: jmp    ea <op_tailcall+0xea> */
0x48, 0x8d, 0x34, 0x24,                   /*d8: lea    (%rsp),%rsi */
0x4c, 0x89, 0xf7,                         /*dc: mov    %r14,%rdi */
0x44, 0x89, 0xe2,                         /*df: mov    %r12d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*e2: callq  e7 <op_tailcall+0xe7> */
0x49, 0x89, 0xc5,                         /*e7: mov    %rax,%r13 */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*ea: mov    $0xcd0000,%eax */
0x4d, 0x85, 0xed,                         /*ef: test   %r13,%r13 */
0x75, 0x73,                               /*f2: jne    167 <op_tailcall+0x167> */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*f4: mov    $0x0,%esi */
0xba, 0x0e, 0x00, 0x00, 0x00,             /*f9: mov    $0xe,%edx */
0x4c, 0x89, 0xf7,                         /*fe: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*101: callq  106 <op_tailcall+0x106> */
0x89, 0xc5,                               /*106: mov    %eax,%ebp */
0x48, 0x8d, 0x34, 0x24,                   /*108: lea    (%rsp),%rsi */
0x4c, 0x89, 0xf7,                         /*10c: mov    %r14,%rdi */
0x89, 0xea,                               /*10f: mov    %ebp,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*111: callq  116 <op_tailcall+0x116> */
0x49, 0x89, 0xc5,                         /*116: mov    %rax,%r13 */
0x48, 0x8b, 0x43, 0x18,                   /*119: mov    0x18(%rbx),%rax */
0xb9, 0x02, 0x00, 0x78, 0x01,             /*11d: mov    $0x1780002,%ecx */
0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*122: data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0xf2, 0x0f, 0x10, 0x44, 0xc8, 0xf8,       /*130: movsd  -0x8(%rax,%rcx,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0xc8,             /*136: movsd  %xmm0,(%rax,%rcx,8) */
0x48, 0xff, 0xc9,                         /*13b: dec    %rcx */
0x48, 0x81, 0xf9, 0x01, 0x00, 0xab, 0x00, /*13e: cmp    $0xab0001,%rcx */
0x75, 0xe9,                               /*145: jne    130 <op_tailcall+0x130> */
0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0xf1, 0xff,/*147: movabs $0xfff1400000000000,%rax */
0x4c, 0x09, 0xe0,                         /*151: or     %r12,%rax */
0x48, 0x8b, 0x4b, 0x18,                   /*154: mov    0x18(%rbx),%rcx */
0x48, 0x89, 0x81, 0x08, 0x08, 0xab, 0x00, /*158: mov    %rax,0xab0808(%rcx) */
0xb8, 0x01, 0x00, 0xcd, 0x00,             /*15f: mov    $0xcd0001,%eax */
0x41, 0x89, 0xec,                         /*164: mov    %ebp,%r12d */
0x49, 0x8b, 0x4e, 0x18,                   /*167: mov    0x18(%r14),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*16b: mov    0x20(%rcx),%rcx */
0x44, 0x89, 0x21,                         /*16f: mov    %r12d,(%rcx) */
0x48, 0x8b, 0x14, 0x24,                   /*172: mov    (%rsp),%rdx */
0x48, 0x89, 0x51, 0x48,                   /*176: mov    %rdx,0x48(%rcx) */
0x89, 0x41, 0x40,                         /*17a: mov    %eax,0x40(%rcx) */
0x49, 0x8b, 0x56, 0x18,                   /*17d: mov    0x18(%r14),%rdx */
0x48, 0x8b, 0x52, 0x08,                   /*181: mov    0x8(%rdx),%rdx */
0x48, 0x8b, 0x7b, 0x18,                   /*185: mov    0x18(%rbx),%rdi */
0x48, 0x8d, 0xb7, 0x00, 0x08, 0xab, 0x00, /*189: lea    0xab0800(%rdi),%rsi */
0xff, 0xc0,                               /*190: inc    %eax */
0x48, 0x39, 0xf2,                         /*192: cmp    %rsi,%rdx */
0x76, 0x2f,                               /*195: jbe    1c6 <op_tailcall+0x1c6> */
0x48, 0x8d, 0xac, 0xc7, 0x00, 0x00, 0x58, 0x05,/*197: lea    0x5580000(%rdi,%rax,8),%rbp */
0x48, 0x39, 0xea,                         /*19f: cmp    %rbp,%rdx */
0x73, 0x22,                               /*1a2: jae    1c6 <op_tailcall+0x1c6> */
0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*1a4: data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0xf2, 0x0f, 0x10, 0x84, 0xc7, 0xf8, 0xff, 0x57, 0x05,/*1b0: movsd  0x557fff8(%rdi,%rax,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x44, 0xc2, 0xf8,       /*1b9: movsd  %xmm0,-0x8(%rdx,%rax,8) */
0x48, 0xff, 0xc8,                         /*1bf: dec    %rax */
0x75, 0xec,                               /*1c2: jne    1b0 <op_tailcall+0x1b0> */
0xeb, 0x1f,                               /*1c4: jmp    1e5 <op_tailcall+0x1e5> */
0x48, 0x39, 0xf2,                         /*1c6: cmp    %rsi,%rdx */
0x74, 0x1a,                               /*1c9: je     1e5 <op_tailcall+0x1e5> */
0x48, 0xf7, 0xd8,                         /*1cb: neg    %rax */
0x66, 0x90,                               /*1ce: xchg   %ax,%ax */
0xf2, 0x0f, 0x10, 0x06,                   /*1d0: movsd  (%rsi),%xmm0 */
0x48, 0x83, 0xc6, 0x08,                   /*1d4: add    $0x8,%rsi */
0xf2, 0x0f, 0x11, 0x02,                   /*1d8: movsd  %xmm0,(%rdx) */
0x48, 0x83, 0xc2, 0x08,                   /*1dc: add    $0x8,%rdx */
0x48, 0xff, 0xc0,                         /*1e0: inc    %rax */
0x75, 0xeb,                               /*1e3: jne    1d0 <op_tailcall+0x1d0> */
0x41, 0xf6, 0x45, 0x02, 0x04,             /*1e5: testb  $0x4,0x2(%r13) */
0x74, 0x32,                               /*1ea: je     21e <op_tailcall+0x21e> */
0x49, 0x8b, 0x46, 0x18,                   /*1ec: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x68, 0x08,                   /*1f0: mov    0x8(%rax),%rbp */
0x4c, 0x89, 0xf7,                         /*1f4: mov    %r14,%rdi */
0x4c, 0x89, 0xfe,                         /*1f7: mov    %r15,%rsi */
0x41, 0xff, 0x55, 0x18,                   /*1fa: callq  *0x18(%r13) */
0x48, 0x89, 0x45, 0x00,                   /*1fe: mov    %rax,0x0(%rbp) */
0x8b, 0x73, 0x48,                         /*202: mov    0x48(%rbx),%esi */
0x4c, 0x89, 0xf7,                         /*205: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*208: callq  20d <op_tailcall+0x20d> */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*20d: mov    $0xab0000,%esi */
0x31, 0xd2,                               /*212: xor    %edx,%edx */
0x48, 0x89, 0xdf,                         /*214: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*217: callq  21c <op_tailcall+0x21c> */
0xeb, 0x67,                               /*21c: jmp    285 <op_tailcall+0x285> */
0x49, 0x8b, 0x45, 0x18,                   /*21e: mov    0x18(%r13),%rax */
0x48, 0x89, 0x43, 0x08,                   /*222: mov    %rax,0x8(%rbx) */
0x48, 0x8b, 0x50, 0x10,                   /*226: mov    0x10(%rax),%rdx */
0x48, 0x89, 0x53, 0x20,                   /*22a: mov    %rdx,0x20(%rbx) */
0x48, 0x8b, 0x50, 0x18,                   /*22e: mov    0x18(%rax),%rdx */
0x48, 0x89, 0x53, 0x28,                   /*232: mov    %rdx,0x28(%rbx) */
0x8b, 0x51, 0x40,                         /*236: mov    0x40(%rcx),%edx */
0x0f, 0xb7, 0x70, 0x02,                   /*239: movzwl 0x2(%rax),%esi */
0x85, 0xd2,                               /*23d: test   %edx,%edx */
0x78, 0x05,                               /*23f: js     246 <op_tailcall+0x246> */
0x83, 0xc2, 0x02,                         /*241: add    $0x2,%edx */
0xeb, 0x10,                               /*244: jmp    256 <op_tailcall+0x256> */
0x83, 0xfe, 0x03,                         /*246: cmp    $0x3,%esi */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*249: mov    $0x3,%eax */
0x0f, 0x42, 0xf0,                         /*24e: cmovb  %eax,%esi */
0xba, 0x03, 0x00, 0x00, 0x00,             /*251: mov    $0x3,%edx */
0x4c, 0x89, 0xf7,                         /*256: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*259: callq  25e <op_tailcall+0x25e> */
0x49, 0x8b, 0x46, 0x18,                   /*25e: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*262: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x18,                   /*266: mov    %rax,0x18(%rbx) */
0x48, 0x8b, 0x43, 0x08,                   /*26a: mov    0x8(%rbx),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*26e: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x10,                   /*272: mov    %rax,0x10(%rbx) */
0x48, 0x8b, 0x7b, 0x50,                   /*276: mov    0x50(%rbx),%rdi */
0x4c, 0x89, 0xee,                         /*27a: mov    %r13,%rsi */
0x48, 0x89, 0xda,                         /*27d: mov    %rbx,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*280: callq  285 <op_tailcall+0x285> */
0x48, 0x89, 0xdf,                         /*285: mov    %rbx,%rdi */
0x5b,                                     /*288: pop    %rbx */
0x41, 0x5c,                               /*289: pop    %r12 */
0x41, 0x5d,                               /*28b: pop    %r13 */
0x41, 0x5e,                               /*28d: pop    %r14 */
0x41, 0x5f,                               /*28f: pop    %r15 */
0x5d,                                     /*291: pop    %rbp */
0xeb, 0x39,                               /*292: jmp    2cd <_str_const_method_missing+0x28d> */
0x49, 0x8b, 0x86, 0x88, 0x00, 0x00, 0x00, /*294: mov    0x88(%r14),%rax */
0xe9, 0xe6, 0xfd, 0xff, 0xff,             /*29b: jmpq   86 <op_tailcall+0x86> */
0x49, 0x8b, 0x86, 0x80, 0x00, 0x00, 0x00, /*2a0: mov    0x80(%r14),%rax */
0xe9, 0xda, 0xfd, 0xff, 0xff,             /*2a7: jmpq   86 <op_tailcall+0x86> */
0x49, 0x8b, 0x86, 0xa0, 0x00, 0x00, 0x00, /*2ac: mov    0xa0(%r14),%rax */
0xe9, 0xce, 0xfd, 0xff, 0xff,             /*2b3: jmpq   86 <op_tailcall+0x86> */
0x49, 0x8b, 0x46, 0x40,                   /*2b8: mov    0x40(%r14),%rax */
0xe9, 0xc5, 0xfd, 0xff, 0xff,             /*2bc: jmpq   86 <op_tailcall+0x86> */
0x49, 0x8b, 0x86, 0x98, 0x00, 0x00, 0x00, /*2c1: mov    0x98(%r14),%rax */
0xe9, 0xb9, 0xfd, 0xff, 0xff,             /*2c8: jmpq   86 <op_tailcall+0x86> */

};
static uint8_t op_tailcall__rodata[] = {
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x6d, 0x65, 0x74, 0x68, 0x6f, 0x64, 0x5f, 0x6d, 0x69, 0x73, 0x73, 0x69, 0x6e, 0x67, 0x00,

};

static void op_tailcall_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 28)) = b * 4 + 0;
  *((int32_t *)(op + 35)) = a * 8 + 0;
  *((int32_t *)(op + 321)) = a * 1 + 1;
  *((int32_t *)(op + 347)) = a * 8 + 8;
  *((int32_t *)(op + 396)) = a * 8 + 0;
  *((int32_t *)(op + 526)) = a * 1 + 0;
  *((int32_t *)(op + 235)) = c * 1 + 0;
  *((int32_t *)(op + 352)) = c * 1 + 1;
}

static void op_tailcall_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_tailcall_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[1, 0, 14..17]], "a"=>[[8, 0, 187..190]]} */
static uint8_t op_blkpush__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x7e, 0x50,                   /*6: mov    0x50(%r14),%rdi */
0xc7, 0x44, 0x24, 0x04, 0x00, 0x00, 0xbc, 0x00,/*a: movl   $0xbc0000,0x4(%rsp) */
0x44, 0x8b, 0x4c, 0x24, 0x04,             /*12: mov    0x4(%rsp),%r9d */
0x8b, 0x44, 0x24, 0x04,                   /*17: mov    0x4(%rsp),%eax */
0x44, 0x8b, 0x44, 0x24, 0x04,             /*1b: mov    0x4(%rsp),%r8d */
0x8b, 0x54, 0x24, 0x04,                   /*20: mov    0x4(%rsp),%edx */
0x83, 0xe2, 0x0f,                         /*24: and    $0xf,%edx */
0x74, 0x52,                               /*27: je     7b <op_blkpush+0x7b> */
0x49, 0x8b, 0x4e, 0x50,                   /*29: mov    0x50(%r14),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*2d: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*31: mov    0x20(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*35: mov    0x8(%rcx),%rcx */
0x48, 0x8b, 0x71, 0x28,                   /*39: mov    0x28(%rcx),%rsi */
0x48, 0x85, 0xf6,                         /*3d: test   %rsi,%rsi */
0x0f, 0x94, 0xc3,                         /*40: sete   %bl */
0x83, 0xfa, 0x01,                         /*43: cmp    $0x1,%edx */
0x74, 0x1b,                               /*46: je     63 <op_blkpush+0x63> */
0xb9, 0x01, 0x00, 0x00, 0x00,             /*48: mov    $0x1,%ecx */
0x29, 0xd1,                               /*4d: sub    %edx,%ecx */
0x90,                                     /*4f: nop */
0xf6, 0xc3, 0x01,                         /*50: test   $0x1,%bl */
0x75, 0x12,                               /*53: jne    67 <op_blkpush+0x67> */
0x48, 0x8b, 0x76, 0x08,                   /*55: mov    0x8(%rsi),%rsi */
0x48, 0x85, 0xf6,                         /*59: test   %rsi,%rsi */
0x0f, 0x94, 0xc3,                         /*5c: sete   %bl */
0xff, 0xc1,                               /*5f: inc    %ecx */
0x75, 0xed,                               /*61: jne    50 <op_blkpush+0x50> */
0x84, 0xdb,                               /*63: test   %bl,%bl */
0x74, 0x1d,                               /*65: je     84 <op_blkpush+0x84> */
0xbe, 0x02, 0x00, 0x00, 0x00,             /*67: mov    $0x2,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*6c: callq  71 <op_blkpush+0x71> */
0x4c, 0x89, 0xf7,                         /*71: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*74: callq  79 <op_blkpush+0x79> */
0xeb, 0x44,                               /*79: jmp    bf <op_blkpush+0xbf> */
0x49, 0x8d, 0x4e, 0x18,                   /*7b: lea    0x18(%r14),%rcx */
0x48, 0x89, 0xce,                         /*7f: mov    %rcx,%rsi */
0xeb, 0x08,                               /*82: jmp    8c <op_blkpush+0x8c> */
0x48, 0x83, 0xc6, 0x18,                   /*84: add    $0x18,%rsi */
0x49, 0x8d, 0x4e, 0x18,                   /*88: lea    0x18(%r14),%rcx */
0x41, 0xc1, 0xe9, 0x0a,                   /*8c: shr    $0xa,%r9d */
0x41, 0x83, 0xe1, 0x3f,                   /*90: and    $0x3f,%r9d */
0xc1, 0xe8, 0x09,                         /*94: shr    $0x9,%eax */
0x83, 0xe0, 0x01,                         /*97: and    $0x1,%eax */
0x41, 0xc1, 0xe8, 0x04,                   /*9a: shr    $0x4,%r8d */
0x41, 0x83, 0xe0, 0x1f,                   /*9e: and    $0x1f,%r8d */
0x48, 0x8b, 0x16,                         /*a2: mov    (%rsi),%rdx */
0x48, 0x8b, 0x09,                         /*a5: mov    (%rcx),%rcx */
0x44, 0x01, 0xc8,                         /*a8: add    %r9d,%eax */
0x41, 0x8d, 0x44, 0x00, 0x01,             /*ab: lea    0x1(%r8,%rax,1),%eax */
0x48, 0x98,                               /*b0: cltq */
0xf2, 0x0f, 0x10, 0x04, 0xc2,             /*b2: movsd  (%rdx,%rax,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x81, 0x00, 0x08, 0xab, 0x00,/*b7: movsd  %xmm0,0xab0800(%rcx) */
0x4c, 0x89, 0xf7,                         /*bf: mov    %r14,%rdi */
0x5b,                                     /*c2: pop    %rbx */
0x41, 0x5e,                               /*c3: pop    %r14 */

};
static uint8_t op_blkpush__rodata[] = {

};

static void op_blkpush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = b * 1 + 0;
  *((int32_t *)(op + 187)) = a * 8 + 0;
}

static void op_blkpush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_blkpush_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 4, 18..21], [8, 12, 56..59], [8, 0, 113..116], [8, 8, 119..122], [8, 0, 161..164], [8, 8, 168..171], [8, 0, 183..186], [8, 8, 203..206], [8, 0, 211..214], [8, 0, 228..231], [8, 8, 236..239], [8, 0, 259..262], [8, 0, 276..279], [8, 8, 284..287], [8, 0, 292..295], [8, 4, 310..313], [8, 0, 320..323]]} */
static uint8_t op_add__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x49, 0x89, 0xff,                         /*5: mov    %rdi,%r15 */
0x49, 0x8b, 0x5f, 0x18,                   /*8: mov    0x18(%r15),%rbx */
0x4d, 0x8b, 0x77, 0x50,                   /*c: mov    0x50(%r15),%r14 */
0x8b, 0x83, 0x04, 0x08, 0xab, 0x00,       /*10: mov    0xab0804(%rbx),%eax */
0xb9, 0x00, 0x06, 0x00, 0x00,             /*16: mov    $0x600,%ecx */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*1b: cmp    $0xfff00001,%eax */
0x72, 0x14,                               /*20: jb     36 <op_add+0x36> */
0xc1, 0xe8, 0x06,                         /*22: shr    $0x6,%eax */
0x25, 0x00, 0x3f, 0x00, 0x00,             /*25: and    $0x3f00,%eax */
0x05, 0x00, 0xff, 0xff, 0x00,             /*2a: add    $0xffff00,%eax */
0x25, 0x00, 0xff, 0xff, 0x00,             /*2f: and    $0xffff00,%eax */
0x89, 0xc1,                               /*34: mov    %eax,%ecx */
0x8b, 0x93, 0x0c, 0x08, 0xab, 0x00,       /*36: mov    0xab080c(%rbx),%edx */
0xb8, 0x06, 0x00, 0x00, 0x00,             /*3c: mov    $0x6,%eax */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*41: cmp    $0xfff00001,%edx */
0x72, 0x0f,                               /*47: jb     58 <op_add+0x58> */
0xc1, 0xea, 0x0e,                         /*49: shr    $0xe,%edx */
0x83, 0xe2, 0x3f,                         /*4c: and    $0x3f,%edx */
0x81, 0xc2, 0xff, 0x00, 0x00, 0x00,       /*4f: add    $0xff,%edx */
0x0f, 0xb6, 0xc2,                         /*55: movzbl %dl,%eax */
0x09, 0xc8,                               /*58: or     %ecx,%eax */
0x3d, 0x0f, 0x10, 0x00, 0x00,             /*5a: cmp    $0x100f,%eax */
0x7f, 0x32,                               /*5f: jg     93 <op_add+0x93> */
0x3d, 0x02, 0x06, 0x00, 0x00,             /*61: cmp    $0x602,%eax */
0x7f, 0x58,                               /*66: jg     c0 <op_add+0xc0> */
0x3d, 0x03, 0x03, 0x00, 0x00,             /*68: cmp    $0x303,%eax */
0x75, 0x6a,                               /*6d: jne    d9 <op_add+0xd9> */
0x8b, 0x83, 0x00, 0x08, 0xab, 0x00,       /*6f: mov    0xab0800(%rbx),%eax */
0x8b, 0x8b, 0x08, 0x08, 0xab, 0x00,       /*75: mov    0xab0808(%rbx),%ecx */
0x89, 0xc2,                               /*7b: mov    %eax,%edx */
0x01, 0xca,                               /*7d: add    %ecx,%edx */
0x0f, 0x81, 0xaf, 0x00, 0x00, 0x00,       /*7f: jno    134 <op_add+0x134> */
0xf2, 0x0f, 0x2a, 0xc8,                   /*85: cvtsi2sd %eax,%xmm1 */
0xf2, 0x0f, 0x2a, 0xc1,                   /*89: cvtsi2sd %ecx,%xmm0 */
0xf2, 0x0f, 0x58, 0xc1,                   /*8d: addsd  %xmm1,%xmm0 */
0xeb, 0x5d,                               /*91: jmp    f0 <op_add+0xf0> */
0x3d, 0x10, 0x10, 0x00, 0x00,             /*93: cmp    $0x1010,%eax */
0x0f, 0x85, 0x8c, 0x00, 0x00, 0x00,       /*98: jne    12a <op_add+0x12a> */
0x48, 0x8b, 0xb3, 0x00, 0x08, 0xab, 0x00, /*9e: mov    0xab0800(%rbx),%rsi */
0x48, 0x8b, 0x93, 0x08, 0x08, 0xab, 0x00, /*a5: mov    0xab0808(%rbx),%rdx */
0x4c, 0x89, 0xf7,                         /*ac: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*af: callq  b4 <op_add+0xb4> */
0x48, 0x89, 0x83, 0x00, 0x08, 0xab, 0x00, /*b4: mov    %rax,0xab0800(%rbx) */
0xe9, 0x84, 0x00, 0x00, 0x00,             /*bb: jmpq   144 <op_add+0x144> */
0x3d, 0x03, 0x06, 0x00, 0x00,             /*c0: cmp    $0x603,%eax */
0x75, 0x42,                               /*c5: jne    109 <op_add+0x109> */
0xf2, 0x0f, 0x2a, 0x83, 0x08, 0x08, 0xab, 0x00,/*c7: cvtsi2sdl 0xab0808(%rbx),%xmm0 */
0xf2, 0x0f, 0x58, 0x83, 0x00, 0x08, 0xab, 0x00,/*cf: addsd  0xab0800(%rbx),%xmm0 */
0xeb, 0x47,                               /*d7: jmp    120 <op_add+0x120> */
0x3d, 0x06, 0x03, 0x00, 0x00,             /*d9: cmp    $0x306,%eax */
0x75, 0x4a,                               /*de: jne    12a <op_add+0x12a> */
0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x08, 0xab, 0x00,/*e0: cvtsi2sdl 0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x58, 0x83, 0x08, 0x08, 0xab, 0x00,/*e8: addsd  0xab0808(%rbx),%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*f0: ucomisd %xmm0,%xmm0 */
0x7b, 0x2a,                               /*f4: jnp    120 <op_add+0x120> */
0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f,/*f6: movabs $0x7ff8000000000000,%rax */
0x48, 0x89, 0x83, 0x00, 0x08, 0xab, 0x00, /*100: mov    %rax,0xab0800(%rbx) */
0xeb, 0x3b,                               /*107: jmp    144 <op_add+0x144> */
0x3d, 0x06, 0x06, 0x00, 0x00,             /*109: cmp    $0x606,%eax */
0x75, 0x1a,                               /*10e: jne    12a <op_add+0x12a> */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x08, 0xab, 0x00,/*110: movsd  0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x58, 0x83, 0x08, 0x08, 0xab, 0x00,/*118: addsd  0xab0808(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x08, 0xab, 0x00,/*120: movsd  %xmm0,0xab0800(%rbx) */
0xeb, 0x1a,                               /*128: jmp    144 <op_add+0x144> */
0x4c, 0x89, 0xff,                         /*12a: mov    %r15,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*12d: callq  132 <op_add+0x132> */
0xeb, 0x1b,                               /*132: jmp    14f <op_add+0x14f> */
0xc7, 0x83, 0x04, 0x08, 0xab, 0x00, 0x00, 0x00, 0xf1, 0xff,/*134: movl   $0xfff10000,0xab0804(%rbx) */
0x89, 0x93, 0x00, 0x08, 0xab, 0x00,       /*13e: mov    %edx,0xab0800(%rbx) */
0x41, 0x8b, 0x47, 0x48,                   /*144: mov    0x48(%r15),%eax */
0x41, 0x89, 0x86, 0xdc, 0x00, 0x00, 0x00, /*148: mov    %eax,0xdc(%r14) */
0x4c, 0x89, 0xff,                         /*14f: mov    %r15,%rdi */
0x5b,                                     /*152: pop    %rbx */
0x41, 0x5e,                               /*153: pop    %r14 */
0x41, 0x5f,                               /*155: pop    %r15 */

};
static uint8_t op_add__rodata[] = {

};

static void op_add_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = a * 8 + 4;
  *((int32_t *)(op + 56)) = a * 8 + 12;
  *((int32_t *)(op + 113)) = a * 8 + 0;
  *((int32_t *)(op + 119)) = a * 8 + 8;
  *((int32_t *)(op + 161)) = a * 8 + 0;
  *((int32_t *)(op + 168)) = a * 8 + 8;
  *((int32_t *)(op + 183)) = a * 8 + 0;
  *((int32_t *)(op + 203)) = a * 8 + 8;
  *((int32_t *)(op + 211)) = a * 8 + 0;
  *((int32_t *)(op + 228)) = a * 8 + 0;
  *((int32_t *)(op + 236)) = a * 8 + 8;
  *((int32_t *)(op + 259)) = a * 8 + 0;
  *((int32_t *)(op + 276)) = a * 8 + 0;
  *((int32_t *)(op + 284)) = a * 8 + 8;
  *((int32_t *)(op + 292)) = a * 8 + 0;
  *((int32_t *)(op + 310)) = a * 8 + 4;
  *((int32_t *)(op + 320)) = a * 8 + 0;
}

static void op_add_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_add_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"c"=>[[1, 0, 5..8]], "a"=>[[8, 4, 15..18], [8, 0, 46..49], [8, 0, 54..57], [8, 0, 74..77], [8, 0, 106..109], [8, 12, 114..117], [8, 8, 124..127], [1, 0, 134..137], [8, 4, 161..164], [8, 0, 171..174], [8, 0, 190..193]], "b"=>[[1, 0, 139..142]]} */
static uint8_t op_addi__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*4: mov    $0xcd0000,%eax */
0x48, 0x8b, 0x4b, 0x18,                   /*9: mov    0x18(%rbx),%rcx */
0x8b, 0x91, 0x04, 0x08, 0xab, 0x00,       /*d: mov    0xab0804(%rcx),%edx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*13: cmp    $0xfff00001,%edx */
0x72, 0x0b,                               /*19: jb     26 <op_addi+0x26> */
0xc1, 0xea, 0x0e,                         /*1b: shr    $0xe,%edx */
0x83, 0xe2, 0x3f,                         /*1e: and    $0x3f,%edx */
0x83, 0xfa, 0x07,                         /*21: cmp    $0x7,%edx */
0x75, 0x1d,                               /*24: jne    43 <op_addi+0x43> */
0xf2, 0x0f, 0x2a, 0xc0,                   /*26: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x58, 0x81, 0x00, 0x08, 0xab, 0x00,/*2a: addsd  0xab0800(%rcx),%xmm0 */
0xf2, 0x0f, 0x11, 0x81, 0x00, 0x08, 0xab, 0x00,/*32: movsd  %xmm0,0xab0800(%rcx) */
0x48, 0x89, 0xdf,                         /*3a: mov    %rbx,%rdi */
0x5b,                                     /*3d: pop    %rbx */
0xe9, 0x84, 0x00, 0x00, 0x00,             /*3e: jmpq   c7 <op_addi+0xc7> */
0x83, 0xfa, 0x04,                         /*43: cmp    $0x4,%edx */
0x75, 0x28,                               /*46: jne    70 <op_addi+0x70> */
0x8b, 0x91, 0x00, 0x08, 0xab, 0x00,       /*48: mov    0xab0800(%rcx),%edx */
0x89, 0xd6,                               /*4e: mov    %edx,%esi */
0x01, 0xc6,                               /*50: add    %eax,%esi */
0x71, 0x4b,                               /*52: jno    9f <op_addi+0x9f> */
0xf2, 0x0f, 0x2a, 0xca,                   /*54: cvtsi2sd %edx,%xmm1 */
0xf2, 0x0f, 0x2a, 0xc0,                   /*58: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x58, 0xc1,                   /*5c: addsd  %xmm1,%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*60: ucomisd %xmm0,%xmm0 */
0x7a, 0x4b,                               /*64: jp     b1 <op_addi+0xb1> */
0xf2, 0x0f, 0x11, 0x81, 0x00, 0x08, 0xab, 0x00,/*66: movsd  %xmm0,0xab0800(%rcx) */
0xeb, 0xca,                               /*6e: jmp    3a <op_addi+0x3a> */
0xc7, 0x81, 0x0c, 0x08, 0xab, 0x00, 0x00, 0x00, 0xf1, 0xff,/*70: movl   $0xfff10000,0xab080c(%rcx) */
0x89, 0x81, 0x08, 0x08, 0xab, 0x00,       /*7a: mov    %eax,0xab0808(%rcx) */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*80: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*85: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*8a: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x01, 0x00, 0x00, 0x00,       /*8f: mov    $0x1,%r8d */
0x48, 0x89, 0xdf,                         /*95: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*98: callq  9d <op_addi+0x9d> */
0xeb, 0x9b,                               /*9d: jmp    3a <op_addi+0x3a> */
0xc7, 0x81, 0x04, 0x08, 0xab, 0x00, 0x00, 0x00, 0xf1, 0xff,/*9f: movl   $0xfff10000,0xab0804(%rcx) */
0x89, 0xb1, 0x00, 0x08, 0xab, 0x00,       /*a9: mov    %esi,0xab0800(%rcx) */
0xeb, 0x89,                               /*af: jmp    3a <op_addi+0x3a> */
0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f,/*b1: movabs $0x7ff8000000000000,%rax */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*bb: mov    %rax,0xab0800(%rcx) */
0xe9, 0x73, 0xff, 0xff, 0xff,             /*c2: jmpq   3a <op_addi+0x3a> */

};
static uint8_t op_addi__rodata[] = {

};

static void op_addi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = c * 1 + 0;
  *((int32_t *)(op + 15)) = a * 8 + 4;
  *((int32_t *)(op + 46)) = a * 8 + 0;
  *((int32_t *)(op + 54)) = a * 8 + 0;
  *((int32_t *)(op + 74)) = a * 8 + 0;
  *((int32_t *)(op + 106)) = a * 8 + 0;
  *((int32_t *)(op + 114)) = a * 8 + 12;
  *((int32_t *)(op + 124)) = a * 8 + 8;
  *((int32_t *)(op + 134)) = a * 1 + 0;
  *((int32_t *)(op + 161)) = a * 8 + 4;
  *((int32_t *)(op + 171)) = a * 8 + 0;
  *((int32_t *)(op + 190)) = a * 8 + 0;
  *((int32_t *)(op + 139)) = b * 1 + 0;
}

static void op_addi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_addi_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 4, 10..13], [8, 12, 52..55], [8, 0, 105..108], [8, 8, 111..114], [8, 0, 151..154], [8, 0, 172..175], [8, 8, 180..183], [8, 0, 192..195], [8, 0, 210..213], [8, 8, 218..221], [8, 0, 232..235], [8, 0, 250..253], [8, 8, 258..261], [8, 0, 266..269], [8, 4, 284..287], [8, 0, 294..297], [8, 0, 313..316]]} */
static uint8_t op_sub__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x04, 0x08, 0xab, 0x00,       /*8: mov    0xab0804(%rax),%edx */
0xb9, 0x00, 0x06, 0x00, 0x00,             /*e: mov    $0x600,%ecx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*13: cmp    $0xfff00001,%edx */
0x72, 0x17,                               /*19: jb     32 <op_sub+0x32> */
0xc1, 0xea, 0x06,                         /*1b: shr    $0x6,%edx */
0x81, 0xe2, 0x00, 0x3f, 0x00, 0x00,       /*1e: and    $0x3f00,%edx */
0x81, 0xc2, 0x00, 0xff, 0xff, 0x00,       /*24: add    $0xffff00,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*2a: and    $0xffff00,%edx */
0x89, 0xd1,                               /*30: mov    %edx,%ecx */
0x8b, 0xb0, 0x0c, 0x08, 0xab, 0x00,       /*32: mov    0xab080c(%rax),%esi */
0xba, 0x06, 0x00, 0x00, 0x00,             /*38: mov    $0x6,%edx */
0x81, 0xfe, 0x01, 0x00, 0xf0, 0xff,       /*3d: cmp    $0xfff00001,%esi */
0x72, 0x10,                               /*43: jb     55 <op_sub+0x55> */
0xc1, 0xee, 0x0e,                         /*45: shr    $0xe,%esi */
0x83, 0xe6, 0x3f,                         /*48: and    $0x3f,%esi */
0x81, 0xc6, 0xff, 0x00, 0x00, 0x00,       /*4b: add    $0xff,%esi */
0x40, 0x0f, 0xb6, 0xd6,                   /*51: movzbl %sil,%edx */
0x09, 0xca,                               /*55: or     %ecx,%edx */
0x81, 0xfa, 0x02, 0x06, 0x00, 0x00,       /*57: cmp    $0x602,%edx */
0x7f, 0x41,                               /*5d: jg     a0 <op_sub+0xa0> */
0x81, 0xfa, 0x03, 0x03, 0x00, 0x00,       /*5f: cmp    $0x303,%edx */
0x75, 0x5f,                               /*65: jne    c6 <op_sub+0xc6> */
0x8b, 0x88, 0x00, 0x08, 0xab, 0x00,       /*67: mov    0xab0800(%rax),%ecx */
0x8b, 0x90, 0x08, 0x08, 0xab, 0x00,       /*6d: mov    0xab0808(%rax),%edx */
0x89, 0xce,                               /*73: mov    %ecx,%esi */
0x29, 0xd6,                               /*75: sub    %edx,%esi */
0x0f, 0x81, 0x9d, 0x00, 0x00, 0x00,       /*77: jno    11a <op_sub+0x11a> */
0xf2, 0x0f, 0x2a, 0xc1,                   /*7d: cvtsi2sd %ecx,%xmm0 */
0xf2, 0x0f, 0x2a, 0xca,                   /*81: cvtsi2sd %edx,%xmm1 */
0xf2, 0x0f, 0x5c, 0xc1,                   /*85: subsd  %xmm1,%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*89: ucomisd %xmm0,%xmm0 */
0x0f, 0x8a, 0x99, 0x00, 0x00, 0x00,       /*8d: jp     12c <op_sub+0x12c> */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x08, 0xab, 0x00,/*93: movsd  %xmm0,0xab0800(%rax) */
0xe9, 0x9d, 0x00, 0x00, 0x00,             /*9b: jmpq   13d <op_sub+0x13d> */
0x81, 0xfa, 0x03, 0x06, 0x00, 0x00,       /*a0: cmp    $0x603,%edx */
0x75, 0x46,                               /*a6: jne    ee <op_sub+0xee> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x08, 0xab, 0x00,/*a8: movsd  0xab0800(%rax),%xmm0 */
0xf2, 0x0f, 0x2a, 0x88, 0x08, 0x08, 0xab, 0x00,/*b0: cvtsi2sdl 0xab0808(%rax),%xmm1 */
0xf2, 0x0f, 0x5c, 0xc1,                   /*b8: subsd  %xmm1,%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x08, 0xab, 0x00,/*bc: movsd  %xmm0,0xab0800(%rax) */
0xeb, 0x77,                               /*c4: jmp    13d <op_sub+0x13d> */
0x81, 0xfa, 0x06, 0x03, 0x00, 0x00,       /*c6: cmp    $0x306,%edx */
0x75, 0x42,                               /*cc: jne    110 <op_sub+0x110> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x08, 0xab, 0x00,/*ce: cvtsi2sdl 0xab0800(%rax),%xmm0 */
0xf2, 0x0f, 0x5c, 0x80, 0x08, 0x08, 0xab, 0x00,/*d6: subsd  0xab0808(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*de: ucomisd %xmm0,%xmm0 */
0x7a, 0x48,                               /*e2: jp     12c <op_sub+0x12c> */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x08, 0xab, 0x00,/*e4: movsd  %xmm0,0xab0800(%rax) */
0xeb, 0x4f,                               /*ec: jmp    13d <op_sub+0x13d> */
0x81, 0xfa, 0x06, 0x06, 0x00, 0x00,       /*ee: cmp    $0x606,%edx */
0x75, 0x1a,                               /*f4: jne    110 <op_sub+0x110> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x08, 0xab, 0x00,/*f6: movsd  0xab0800(%rax),%xmm0 */
0xf2, 0x0f, 0x5c, 0x80, 0x08, 0x08, 0xab, 0x00,/*fe: subsd  0xab0808(%rax),%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x08, 0xab, 0x00,/*106: movsd  %xmm0,0xab0800(%rax) */
0xeb, 0x2d,                               /*10e: jmp    13d <op_sub+0x13d> */
0x48, 0x89, 0xdf,                         /*110: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*113: callq  118 <op_sub+0x118> */
0xeb, 0x23,                               /*118: jmp    13d <op_sub+0x13d> */
0xc7, 0x80, 0x04, 0x08, 0xab, 0x00, 0x00, 0x00, 0xf1, 0xff,/*11a: movl   $0xfff10000,0xab0804(%rax) */
0x89, 0xb0, 0x00, 0x08, 0xab, 0x00,       /*124: mov    %esi,0xab0800(%rax) */
0xeb, 0x11,                               /*12a: jmp    13d <op_sub+0x13d> */
0x48, 0xb9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f,/*12c: movabs $0x7ff8000000000000,%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*136: mov    %rcx,0xab0800(%rax) */
0x48, 0x89, 0xdf,                         /*13d: mov    %rbx,%rdi */
0x5b,                                     /*140: pop    %rbx */

};
static uint8_t op_sub__rodata[] = {

};

static void op_sub_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 8 + 4;
  *((int32_t *)(op + 52)) = a * 8 + 12;
  *((int32_t *)(op + 105)) = a * 8 + 0;
  *((int32_t *)(op + 111)) = a * 8 + 8;
  *((int32_t *)(op + 151)) = a * 8 + 0;
  *((int32_t *)(op + 172)) = a * 8 + 0;
  *((int32_t *)(op + 180)) = a * 8 + 8;
  *((int32_t *)(op + 192)) = a * 8 + 0;
  *((int32_t *)(op + 210)) = a * 8 + 0;
  *((int32_t *)(op + 218)) = a * 8 + 8;
  *((int32_t *)(op + 232)) = a * 8 + 0;
  *((int32_t *)(op + 250)) = a * 8 + 0;
  *((int32_t *)(op + 258)) = a * 8 + 8;
  *((int32_t *)(op + 266)) = a * 8 + 0;
  *((int32_t *)(op + 284)) = a * 8 + 4;
  *((int32_t *)(op + 294)) = a * 8 + 0;
  *((int32_t *)(op + 313)) = a * 8 + 0;
}

static void op_sub_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sub_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"c"=>[[1, 0, 5..8]], "a"=>[[8, 4, 15..18], [8, 0, 46..49], [8, 0, 58..61], [8, 0, 78..81], [8, 0, 110..113], [8, 12, 118..121], [8, 8, 128..131], [1, 0, 138..141], [8, 4, 165..168], [8, 0, 175..178], [8, 0, 194..197]], "b"=>[[1, 0, 143..146]]} */
static uint8_t op_subi__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*4: mov    $0xcd0000,%eax */
0x48, 0x8b, 0x4b, 0x18,                   /*9: mov    0x18(%rbx),%rcx */
0x8b, 0x91, 0x04, 0x08, 0xab, 0x00,       /*d: mov    0xab0804(%rcx),%edx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*13: cmp    $0xfff00001,%edx */
0x72, 0x0b,                               /*19: jb     26 <op_subi+0x26> */
0xc1, 0xea, 0x0e,                         /*1b: shr    $0xe,%edx */
0x83, 0xe2, 0x3f,                         /*1e: and    $0x3f,%edx */
0x83, 0xfa, 0x07,                         /*21: cmp    $0x7,%edx */
0x75, 0x21,                               /*24: jne    47 <op_subi+0x47> */
0xf2, 0x0f, 0x2a, 0xc0,                   /*26: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x10, 0x89, 0x00, 0x08, 0xab, 0x00,/*2a: movsd  0xab0800(%rcx),%xmm1 */
0xf2, 0x0f, 0x5c, 0xc8,                   /*32: subsd  %xmm0,%xmm1 */
0xf2, 0x0f, 0x11, 0x89, 0x00, 0x08, 0xab, 0x00,/*36: movsd  %xmm1,0xab0800(%rcx) */
0x48, 0x89, 0xdf,                         /*3e: mov    %rbx,%rdi */
0x5b,                                     /*41: pop    %rbx */
0xe9, 0x84, 0x00, 0x00, 0x00,             /*42: jmpq   cb <op_subi+0xcb> */
0x83, 0xfa, 0x04,                         /*47: cmp    $0x4,%edx */
0x75, 0x28,                               /*4a: jne    74 <op_subi+0x74> */
0x8b, 0x91, 0x00, 0x08, 0xab, 0x00,       /*4c: mov    0xab0800(%rcx),%edx */
0x89, 0xd6,                               /*52: mov    %edx,%esi */
0x29, 0xc6,                               /*54: sub    %eax,%esi */
0x71, 0x4b,                               /*56: jno    a3 <op_subi+0xa3> */
0xf2, 0x0f, 0x2a, 0xc2,                   /*58: cvtsi2sd %edx,%xmm0 */
0xf2, 0x0f, 0x2a, 0xc8,                   /*5c: cvtsi2sd %eax,%xmm1 */
0xf2, 0x0f, 0x5c, 0xc1,                   /*60: subsd  %xmm1,%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*64: ucomisd %xmm0,%xmm0 */
0x7a, 0x4b,                               /*68: jp     b5 <op_subi+0xb5> */
0xf2, 0x0f, 0x11, 0x81, 0x00, 0x08, 0xab, 0x00,/*6a: movsd  %xmm0,0xab0800(%rcx) */
0xeb, 0xca,                               /*72: jmp    3e <op_subi+0x3e> */
0xc7, 0x81, 0x0c, 0x08, 0xab, 0x00, 0x00, 0x00, 0xf1, 0xff,/*74: movl   $0xfff10000,0xab080c(%rcx) */
0x89, 0x81, 0x08, 0x08, 0xab, 0x00,       /*7e: mov    %eax,0xab0808(%rcx) */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*84: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*89: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*8e: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x01, 0x00, 0x00, 0x00,       /*93: mov    $0x1,%r8d */
0x48, 0x89, 0xdf,                         /*99: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*9c: callq  a1 <op_subi+0xa1> */
0xeb, 0x9b,                               /*a1: jmp    3e <op_subi+0x3e> */
0xc7, 0x81, 0x04, 0x08, 0xab, 0x00, 0x00, 0x00, 0xf1, 0xff,/*a3: movl   $0xfff10000,0xab0804(%rcx) */
0x89, 0xb1, 0x00, 0x08, 0xab, 0x00,       /*ad: mov    %esi,0xab0800(%rcx) */
0xeb, 0x89,                               /*b3: jmp    3e <op_subi+0x3e> */
0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f,/*b5: movabs $0x7ff8000000000000,%rax */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*bf: mov    %rax,0xab0800(%rcx) */
0xe9, 0x73, 0xff, 0xff, 0xff,             /*c6: jmpq   3e <op_subi+0x3e> */

};
static uint8_t op_subi__rodata[] = {

};

static void op_subi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = c * 1 + 0;
  *((int32_t *)(op + 15)) = a * 8 + 4;
  *((int32_t *)(op + 46)) = a * 8 + 0;
  *((int32_t *)(op + 58)) = a * 8 + 0;
  *((int32_t *)(op + 78)) = a * 8 + 0;
  *((int32_t *)(op + 110)) = a * 8 + 0;
  *((int32_t *)(op + 118)) = a * 8 + 12;
  *((int32_t *)(op + 128)) = a * 8 + 8;
  *((int32_t *)(op + 138)) = a * 1 + 0;
  *((int32_t *)(op + 165)) = a * 8 + 4;
  *((int32_t *)(op + 175)) = a * 8 + 0;
  *((int32_t *)(op + 194)) = a * 8 + 0;
  *((int32_t *)(op + 143)) = b * 1 + 0;
}

static void op_subi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_subi_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 4, 16..19], [8, 12, 58..61], [8, 0, 111..114], [8, 8, 118..121], [8, 8, 190..193], [8, 0, 198..201], [8, 0, 206..209], [8, 0, 224..227], [8, 8, 232..235], [8, 0, 246..249], [8, 0, 264..267], [8, 8, 272..275], [8, 0, 280..283], [8, 0, 309..312], [8, 4, 328..331], [8, 0, 338..341]]} */
static uint8_t op_mul__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x49, 0x8b, 0x7e, 0x50,                   /*a: mov    0x50(%r14),%rdi */
0x8b, 0x8b, 0x04, 0x08, 0xab, 0x00,       /*e: mov    0xab0804(%rbx),%ecx */
0xb8, 0x00, 0x06, 0x00, 0x00,             /*14: mov    $0x600,%eax */
0x81, 0xf9, 0x01, 0x00, 0xf0, 0xff,       /*19: cmp    $0xfff00001,%ecx */
0x72, 0x17,                               /*1f: jb     38 <op_mul+0x38> */
0xc1, 0xe9, 0x06,                         /*21: shr    $0x6,%ecx */
0x81, 0xe1, 0x00, 0x3f, 0x00, 0x00,       /*24: and    $0x3f00,%ecx */
0x81, 0xc1, 0x00, 0xff, 0xff, 0x00,       /*2a: add    $0xffff00,%ecx */
0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*30: and    $0xffff00,%ecx */
0x89, 0xc8,                               /*36: mov    %ecx,%eax */
0x8b, 0x93, 0x0c, 0x08, 0xab, 0x00,       /*38: mov    0xab080c(%rbx),%edx */
0xb9, 0x06, 0x00, 0x00, 0x00,             /*3e: mov    $0x6,%ecx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*43: cmp    $0xfff00001,%edx */
0x72, 0x0f,                               /*49: jb     5a <op_mul+0x5a> */
0xc1, 0xea, 0x0e,                         /*4b: shr    $0xe,%edx */
0x83, 0xe2, 0x3f,                         /*4e: and    $0x3f,%edx */
0x81, 0xc2, 0xff, 0x00, 0x00, 0x00,       /*51: add    $0xff,%edx */
0x0f, 0xb6, 0xca,                         /*57: movzbl %dl,%ecx */
0x09, 0xc1,                               /*5a: or     %eax,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*5c: cmp    $0x602,%ecx */
0x7f, 0x4e,                               /*62: jg     b2 <op_mul+0xb2> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*64: cmp    $0x303,%ecx */
0x75, 0x68,                               /*6a: jne    d4 <op_mul+0xd4> */
0x48, 0x8b, 0xb3, 0x00, 0x08, 0xab, 0x00, /*6c: mov    0xab0800(%rbx),%rsi */
0x48, 0x8b, 0x93, 0x08, 0x08, 0xab, 0x00, /*73: mov    0xab0808(%rbx),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*7a: callq  7f <op_mul+0x7f> */
0x48, 0x89, 0xc1,                         /*7f: mov    %rax,%rcx */
0x48, 0xc1, 0xe9, 0x20,                   /*82: shr    $0x20,%rcx */
0x81, 0xf9, 0x01, 0x00, 0xf0, 0xff,       /*86: cmp    $0xfff00001,%ecx */
0x72, 0x13,                               /*8c: jb     a1 <op_mul+0xa1> */
0x48, 0x89, 0xc1,                         /*8e: mov    %rax,%rcx */
0x48, 0xc1, 0xe9, 0x2e,                   /*91: shr    $0x2e,%rcx */
0x83, 0xe1, 0x3f,                         /*95: and    $0x3f,%ecx */
0x83, 0xf9, 0x07,                         /*98: cmp    $0x7,%ecx */
0x0f, 0x85, 0xa0, 0x00, 0x00, 0x00,       /*9b: jne    141 <op_mul+0x141> */
0x66, 0x48, 0x0f, 0x6e, 0xc0,             /*a1: movq   %rax,%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*a6: ucomisd %xmm0,%xmm0 */
0x0f, 0x8b, 0x82, 0x00, 0x00, 0x00,       /*aa: jnp    132 <op_mul+0x132> */
0xeb, 0x76,                               /*b0: jmp    128 <op_mul+0x128> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*b2: cmp    $0x603,%ecx */
0x75, 0x42,                               /*b8: jne    fc <op_mul+0xfc> */
0xf2, 0x0f, 0x2a, 0x83, 0x08, 0x08, 0xab, 0x00,/*ba: cvtsi2sdl 0xab0808(%rbx),%xmm0 */
0xf2, 0x0f, 0x59, 0x83, 0x00, 0x08, 0xab, 0x00,/*c2: mulsd  0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x08, 0xab, 0x00,/*ca: movsd  %xmm0,0xab0800(%rbx) */
0xeb, 0x65,                               /*d2: jmp    139 <op_mul+0x139> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*d4: cmp    $0x306,%ecx */
0x75, 0x42,                               /*da: jne    11e <op_mul+0x11e> */
0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x08, 0xab, 0x00,/*dc: cvtsi2sdl 0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x59, 0x83, 0x08, 0x08, 0xab, 0x00,/*e4: mulsd  0xab0808(%rbx),%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*ec: ucomisd %xmm0,%xmm0 */
0x7a, 0x36,                               /*f0: jp     128 <op_mul+0x128> */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x08, 0xab, 0x00,/*f2: movsd  %xmm0,0xab0800(%rbx) */
0xeb, 0x3d,                               /*fa: jmp    139 <op_mul+0x139> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*fc: cmp    $0x606,%ecx */
0x75, 0x1a,                               /*102: jne    11e <op_mul+0x11e> */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x08, 0xab, 0x00,/*104: movsd  0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x59, 0x83, 0x08, 0x08, 0xab, 0x00,/*10c: mulsd  0xab0808(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x08, 0xab, 0x00,/*114: movsd  %xmm0,0xab0800(%rbx) */
0xeb, 0x1b,                               /*11c: jmp    139 <op_mul+0x139> */
0x4c, 0x89, 0xf7,                         /*11e: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*121: callq  126 <op_mul+0x126> */
0xeb, 0x11,                               /*126: jmp    139 <op_mul+0x139> */
0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f,/*128: movabs $0x7ff8000000000000,%rax */
0x48, 0x89, 0x83, 0x00, 0x08, 0xab, 0x00, /*132: mov    %rax,0xab0800(%rbx) */
0x4c, 0x89, 0xf7,                         /*139: mov    %r14,%rdi */
0x5b,                                     /*13c: pop    %rbx */
0x41, 0x5e,                               /*13d: pop    %r14 */
0xeb, 0x17,                               /*13f: jmp    158 <op_mul+0x158> */
0x83, 0xf9, 0x04,                         /*141: cmp    $0x4,%ecx */
0x75, 0xf3,                               /*144: jne    139 <op_mul+0x139> */
0xc7, 0x83, 0x04, 0x08, 0xab, 0x00, 0x00, 0x00, 0xf1, 0xff,/*146: movl   $0xfff10000,0xab0804(%rbx) */
0x89, 0x83, 0x00, 0x08, 0xab, 0x00,       /*150: mov    %eax,0xab0800(%rbx) */
0xeb, 0xe1,                               /*156: jmp    139 <op_mul+0x139> */

};
static uint8_t op_mul__rodata[] = {

};

static void op_mul_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 16)) = a * 8 + 4;
  *((int32_t *)(op + 58)) = a * 8 + 12;
  *((int32_t *)(op + 111)) = a * 8 + 0;
  *((int32_t *)(op + 118)) = a * 8 + 8;
  *((int32_t *)(op + 190)) = a * 8 + 8;
  *((int32_t *)(op + 198)) = a * 8 + 0;
  *((int32_t *)(op + 206)) = a * 8 + 0;
  *((int32_t *)(op + 224)) = a * 8 + 0;
  *((int32_t *)(op + 232)) = a * 8 + 8;
  *((int32_t *)(op + 246)) = a * 8 + 0;
  *((int32_t *)(op + 264)) = a * 8 + 0;
  *((int32_t *)(op + 272)) = a * 8 + 8;
  *((int32_t *)(op + 280)) = a * 8 + 0;
  *((int32_t *)(op + 309)) = a * 8 + 0;
  *((int32_t *)(op + 328)) = a * 8 + 4;
  *((int32_t *)(op + 338)) = a * 8 + 0;
}

static void op_mul_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_mul_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 4, 12..15], [8, 12, 54..57], [8, 0, 108..111], [8, 8, 116..119], [8, 0, 138..141], [8, 8, 146..149], [8, 0, 172..175], [8, 8, 180..183], [8, 0, 203..206], [8, 0, 221..224], [8, 8, 229..232], [8, 0, 237..240], [8, 0, 245..248], [8, 0, 294..297]]} */
static uint8_t op_div__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x8b, 0x8b, 0x04, 0x08, 0xab, 0x00,       /*a: mov    0xab0804(%rbx),%ecx */
0xb8, 0x00, 0x06, 0x00, 0x00,             /*10: mov    $0x600,%eax */
0x81, 0xf9, 0x01, 0x00, 0xf0, 0xff,       /*15: cmp    $0xfff00001,%ecx */
0x72, 0x17,                               /*1b: jb     34 <op_div+0x34> */
0xc1, 0xe9, 0x06,                         /*1d: shr    $0x6,%ecx */
0x81, 0xe1, 0x00, 0x3f, 0x00, 0x00,       /*20: and    $0x3f00,%ecx */
0x81, 0xc1, 0x00, 0xff, 0xff, 0x00,       /*26: add    $0xffff00,%ecx */
0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*2c: and    $0xffff00,%ecx */
0x89, 0xc8,                               /*32: mov    %ecx,%eax */
0x8b, 0x93, 0x0c, 0x08, 0xab, 0x00,       /*34: mov    0xab080c(%rbx),%edx */
0xb9, 0x06, 0x00, 0x00, 0x00,             /*3a: mov    $0x6,%ecx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*3f: cmp    $0xfff00001,%edx */
0x72, 0x0f,                               /*45: jb     56 <op_div+0x56> */
0xc1, 0xea, 0x0e,                         /*47: shr    $0xe,%edx */
0x83, 0xe2, 0x3f,                         /*4a: and    $0x3f,%edx */
0x81, 0xc2, 0xff, 0x00, 0x00, 0x00,       /*4d: add    $0xff,%edx */
0x0f, 0xb6, 0xca,                         /*53: movzbl %dl,%ecx */
0x09, 0xc1,                               /*56: or     %eax,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*58: cmp    $0x602,%ecx */
0x7f, 0x1e,                               /*5e: jg     7e <op_div+0x7e> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*60: cmp    $0x303,%ecx */
0x75, 0x34,                               /*66: jne    9c <op_div+0x9c> */
0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x08, 0xab, 0x00,/*68: cvtsi2sdl 0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x2a, 0x8b, 0x08, 0x08, 0xab, 0x00,/*70: cvtsi2sdl 0xab0808(%rbx),%xmm1 */
0xf2, 0x0f, 0x5e, 0xc1,                   /*78: divsd  %xmm1,%xmm0 */
0xeb, 0x3a,                               /*7c: jmp    b8 <op_div+0xb8> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*7e: cmp    $0x603,%ecx */
0x75, 0x4b,                               /*84: jne    d1 <op_div+0xd1> */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x08, 0xab, 0x00,/*86: movsd  0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x2a, 0x8b, 0x08, 0x08, 0xab, 0x00,/*8e: cvtsi2sdl 0xab0808(%rbx),%xmm1 */
0xf2, 0x0f, 0x5e, 0xc1,                   /*96: divsd  %xmm1,%xmm0 */
0xeb, 0x4d,                               /*9a: jmp    e9 <op_div+0xe9> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*9c: cmp    $0x306,%ecx */
0x0f, 0x85, 0x84, 0x00, 0x00, 0x00,       /*a2: jne    12c <op_div+0x12c> */
0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x08, 0xab, 0x00,/*a8: cvtsi2sdl 0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x5e, 0x83, 0x08, 0x08, 0xab, 0x00,/*b0: divsd  0xab0808(%rbx),%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*b8: ucomisd %xmm0,%xmm0 */
0x7b, 0x2b,                               /*bc: jnp    e9 <op_div+0xe9> */
0x48, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f,/*be: movabs $0x7ff8000000000000,%rax */
0x48, 0x89, 0x83, 0x00, 0x08, 0xab, 0x00, /*c8: mov    %rax,0xab0800(%rbx) */
0xeb, 0x20,                               /*cf: jmp    f1 <op_div+0xf1> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*d1: cmp    $0x606,%ecx */
0x75, 0x53,                               /*d7: jne    12c <op_div+0x12c> */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x08, 0xab, 0x00,/*d9: movsd  0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x5e, 0x83, 0x08, 0x08, 0xab, 0x00,/*e1: divsd  0xab0808(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x08, 0xab, 0x00,/*e9: movsd  %xmm0,0xab0800(%rbx) */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x08, 0xab, 0x00,/*f1: movsd  0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x04, 0x24,             /*f9: movsd  %xmm0,(%rsp) */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*fe: callq  103 <op_div+0x103> */
0x85, 0xc0,                               /*103: test   %eax,%eax */
0x74, 0x2d,                               /*105: je     134 <op_div+0x134> */
0xf2, 0x0f, 0x10, 0x04, 0x24,             /*107: movsd  (%rsp),%xmm0 */
0x66, 0x0f, 0x2e, 0xc0,                   /*10c: ucomisd %xmm0,%xmm0 */
0x66, 0x48, 0x0f, 0x7e, 0xc0,             /*110: movq   %xmm0,%rax */
0x48, 0xb9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f,/*115: movabs $0x7ff8000000000000,%rcx */
0x48, 0x0f, 0x4b, 0xc8,                   /*11f: cmovnp %rax,%rcx */
0x48, 0x89, 0x8b, 0x00, 0x08, 0xab, 0x00, /*123: mov    %rcx,0xab0800(%rbx) */
0xeb, 0x08,                               /*12a: jmp    134 <op_div+0x134> */
0x4c, 0x89, 0xf7,                         /*12c: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*12f: callq  134 <op_div+0x134> */
0x4c, 0x89, 0xf7,                         /*134: mov    %r14,%rdi */
0x5b,                                     /*137: pop    %rbx */
0x41, 0x5e,                               /*138: pop    %r14 */

};
static uint8_t op_div__rodata[] = {

};

static void op_div_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 12)) = a * 8 + 4;
  *((int32_t *)(op + 54)) = a * 8 + 12;
  *((int32_t *)(op + 108)) = a * 8 + 0;
  *((int32_t *)(op + 116)) = a * 8 + 8;
  *((int32_t *)(op + 138)) = a * 8 + 0;
  *((int32_t *)(op + 146)) = a * 8 + 8;
  *((int32_t *)(op + 172)) = a * 8 + 0;
  *((int32_t *)(op + 180)) = a * 8 + 8;
  *((int32_t *)(op + 203)) = a * 8 + 0;
  *((int32_t *)(op + 221)) = a * 8 + 0;
  *((int32_t *)(op + 229)) = a * 8 + 8;
  *((int32_t *)(op + 237)) = a * 8 + 0;
  *((int32_t *)(op + 245)) = a * 8 + 0;
  *((int32_t *)(op + 294)) = a * 8 + 0;
}

static void op_div_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_div_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 17..20], [8, 8, 24..27], [8, 4, 43..46], [8, 12, 85..88], [8, 0, 137..140], [8, 8, 143..146], [8, 8, 167..170], [8, 0, 175..178], [8, 0, 194..197], [8, 0, 212..215], [8, 8, 220..223], [8, 0, 250..253], [1, 0, 280..283]], "b"=>[[1, 0, 285..288]], "c"=>[[1, 0, 291..294]]} */
static uint8_t op_eq__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x49, 0x8b, 0x7e, 0x50,                   /*a: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0xb3, 0x00, 0x08, 0xab, 0x00, /*e: mov    0xab0800(%rbx),%rsi */
0x48, 0x8b, 0x93, 0x08, 0x08, 0xab, 0x00, /*15: mov    0xab0808(%rbx),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1c: callq  21 <op_eq+0x21> */
0x84, 0xc0,                               /*21: test   %al,%al */
0x0f, 0x85, 0xc4, 0x00, 0x00, 0x00,       /*23: jne    ed <op_eq+0xed> */
0x8b, 0x8b, 0x04, 0x08, 0xab, 0x00,       /*29: mov    0xab0804(%rbx),%ecx */
0xb8, 0x00, 0x06, 0x00, 0x00,             /*2f: mov    $0x600,%eax */
0x81, 0xf9, 0x01, 0x00, 0xf0, 0xff,       /*34: cmp    $0xfff00001,%ecx */
0x72, 0x17,                               /*3a: jb     53 <op_eq+0x53> */
0xc1, 0xe9, 0x06,                         /*3c: shr    $0x6,%ecx */
0x81, 0xe1, 0x00, 0x3f, 0x00, 0x00,       /*3f: and    $0x3f00,%ecx */
0x81, 0xc1, 0x00, 0xff, 0xff, 0x00,       /*45: add    $0xffff00,%ecx */
0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*4b: and    $0xffff00,%ecx */
0x89, 0xc8,                               /*51: mov    %ecx,%eax */
0x8b, 0x93, 0x0c, 0x08, 0xab, 0x00,       /*53: mov    0xab080c(%rbx),%edx */
0xb9, 0x06, 0x00, 0x00, 0x00,             /*59: mov    $0x6,%ecx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*5e: cmp    $0xfff00001,%edx */
0x72, 0x0f,                               /*64: jb     75 <op_eq+0x75> */
0xc1, 0xea, 0x0e,                         /*66: shr    $0xe,%edx */
0x83, 0xe2, 0x3f,                         /*69: and    $0x3f,%edx */
0x81, 0xc2, 0xff, 0x00, 0x00, 0x00,       /*6c: add    $0xff,%edx */
0x0f, 0xb6, 0xca,                         /*72: movzbl %dl,%ecx */
0x09, 0xc1,                               /*75: or     %eax,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*77: cmp    $0x602,%ecx */
0x7f, 0x1c,                               /*7d: jg     9b <op_eq+0x9b> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*7f: cmp    $0x303,%ecx */
0x75, 0x2f,                               /*85: jne    b6 <op_eq+0xb6> */
0x8b, 0x83, 0x00, 0x08, 0xab, 0x00,       /*87: mov    0xab0800(%rbx),%eax */
0x3b, 0x83, 0x08, 0x08, 0xab, 0x00,       /*8d: cmp    0xab0808(%rbx),%eax */
0x0f, 0x94, 0xc0,                         /*93: sete   %al */
0x0f, 0xb6, 0xc0,                         /*96: movzbl %al,%eax */
0xeb, 0x4e,                               /*99: jmp    e9 <op_eq+0xe9> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*9b: cmp    $0x603,%ecx */
0x75, 0x25,                               /*a1: jne    c8 <op_eq+0xc8> */
0xf2, 0x0f, 0x2a, 0x83, 0x08, 0x08, 0xab, 0x00,/*a3: cvtsi2sdl 0xab0808(%rbx),%xmm0 */
0xf2, 0x0f, 0xc2, 0x83, 0x00, 0x08, 0xab, 0x00, 0x00,/*ab: cmpeqsd 0xab0800(%rbx),%xmm0 */
0xeb, 0x2b,                               /*b4: jmp    e1 <op_eq+0xe1> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*b6: cmp    $0x306,%ecx */
0x75, 0x54,                               /*bc: jne    112 <op_eq+0x112> */
0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x08, 0xab, 0x00,/*be: cvtsi2sdl 0xab0800(%rbx),%xmm0 */
0xeb, 0x10,                               /*c6: jmp    d8 <op_eq+0xd8> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*c8: cmp    $0x606,%ecx */
0x75, 0x42,                               /*ce: jne    112 <op_eq+0x112> */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x08, 0xab, 0x00,/*d0: movsd  0xab0800(%rbx),%xmm0 */
0xf2, 0x0f, 0xc2, 0x83, 0x08, 0x08, 0xab, 0x00, 0x00,/*d8: cmpeqsd 0xab0808(%rbx),%xmm0 */
0x66, 0x48, 0x0f, 0x7e, 0xc0,             /*e1: movq   %xmm0,%rax */
0x83, 0xe0, 0x01,                         /*e6: and    $0x1,%eax */
0x85, 0xc0,                               /*e9: test   %eax,%eax */
0x74, 0x19,                               /*eb: je     106 <op_eq+0x106> */
0x48, 0xb8, 0x01, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xf0, 0xff,/*ed: movabs $0xfff0c00000000001,%rax */
0x48, 0x89, 0x83, 0x00, 0x08, 0xab, 0x00, /*f7: mov    %rax,0xab0800(%rbx) */
0x4c, 0x89, 0xf7,                         /*fe: mov    %r14,%rdi */
0x5b,                                     /*101: pop    %rbx */
0x41, 0x5e,                               /*102: pop    %r14 */
0xeb, 0x2b,                               /*104: jmp    131 <op_eq+0x131> */
0x48, 0xb8, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0xf0, 0xff,/*106: movabs $0xfff0400000000001,%rax */
0xeb, 0xe5,                               /*110: jmp    f7 <op_eq+0xf7> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*112: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*117: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*11c: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*121: mov    $0xcd0000,%r8d */
0x4c, 0x89, 0xf7,                         /*127: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*12a: callq  12f <op_eq+0x12f> */
0xeb, 0xcd,                               /*12f: jmp    fe <op_eq+0xfe> */

};
static uint8_t op_eq__rodata[] = {

};

static void op_eq_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 8 + 0;
  *((int32_t *)(op + 24)) = a * 8 + 8;
  *((int32_t *)(op + 43)) = a * 8 + 4;
  *((int32_t *)(op + 85)) = a * 8 + 12;
  *((int32_t *)(op + 137)) = a * 8 + 0;
  *((int32_t *)(op + 143)) = a * 8 + 8;
  *((int32_t *)(op + 167)) = a * 8 + 8;
  *((int32_t *)(op + 175)) = a * 8 + 0;
  *((int32_t *)(op + 194)) = a * 8 + 0;
  *((int32_t *)(op + 212)) = a * 8 + 0;
  *((int32_t *)(op + 220)) = a * 8 + 8;
  *((int32_t *)(op + 250)) = a * 8 + 0;
  *((int32_t *)(op + 280)) = a * 1 + 0;
  *((int32_t *)(op + 285)) = b * 1 + 0;
  *((int32_t *)(op + 291)) = c * 1 + 0;
}

static void op_eq_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_eq_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 4, 10..13], [8, 12, 52..55], [8, 0, 105..108], [8, 8, 111..114], [8, 8, 132..135], [8, 0, 150..153], [8, 8, 158..161], [8, 8, 183..186], [8, 0, 191..194], [8, 0, 230..233], [1, 0, 242..245]], "b"=>[[1, 0, 247..250]], "c"=>[[1, 0, 253..256]]} */
static uint8_t op_lt__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x04, 0x08, 0xab, 0x00,       /*8: mov    0xab0804(%rax),%edx */
0xb9, 0x00, 0x06, 0x00, 0x00,             /*e: mov    $0x600,%ecx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*13: cmp    $0xfff00001,%edx */
0x72, 0x17,                               /*19: jb     32 <op_lt+0x32> */
0xc1, 0xea, 0x06,                         /*1b: shr    $0x6,%edx */
0x81, 0xe2, 0x00, 0x3f, 0x00, 0x00,       /*1e: and    $0x3f00,%edx */
0x81, 0xc2, 0x00, 0xff, 0xff, 0x00,       /*24: add    $0xffff00,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*2a: and    $0xffff00,%edx */
0x89, 0xd1,                               /*30: mov    %edx,%ecx */
0x8b, 0xb0, 0x0c, 0x08, 0xab, 0x00,       /*32: mov    0xab080c(%rax),%esi */
0xba, 0x06, 0x00, 0x00, 0x00,             /*38: mov    $0x6,%edx */
0x81, 0xfe, 0x01, 0x00, 0xf0, 0xff,       /*3d: cmp    $0xfff00001,%esi */
0x72, 0x10,                               /*43: jb     55 <op_lt+0x55> */
0xc1, 0xee, 0x0e,                         /*45: shr    $0xe,%esi */
0x83, 0xe6, 0x3f,                         /*48: and    $0x3f,%esi */
0x81, 0xc6, 0xff, 0x00, 0x00, 0x00,       /*4b: add    $0xff,%esi */
0x40, 0x0f, 0xb6, 0xd6,                   /*51: movzbl %sil,%edx */
0x09, 0xca,                               /*55: or     %ecx,%edx */
0x81, 0xfa, 0x02, 0x06, 0x00, 0x00,       /*57: cmp    $0x602,%edx */
0x7f, 0x19,                               /*5d: jg     78 <op_lt+0x78> */
0x81, 0xfa, 0x03, 0x03, 0x00, 0x00,       /*5f: cmp    $0x303,%edx */
0x75, 0x23,                               /*65: jne    8a <op_lt+0x8a> */
0x8b, 0x88, 0x00, 0x08, 0xab, 0x00,       /*67: mov    0xab0800(%rax),%ecx */
0x3b, 0x88, 0x08, 0x08, 0xab, 0x00,       /*6d: cmp    0xab0808(%rax),%ecx */
0x0f, 0x9c, 0xc1,                         /*73: setl   %cl */
0xeb, 0x4e,                               /*76: jmp    c6 <op_lt+0xc6> */
0x81, 0xfa, 0x03, 0x06, 0x00, 0x00,       /*78: cmp    $0x603,%edx */
0x75, 0x2b,                               /*7e: jne    ab <op_lt+0xab> */
0xf2, 0x0f, 0x2a, 0x80, 0x08, 0x08, 0xab, 0x00,/*80: cvtsi2sdl 0xab0808(%rax),%xmm0 */
0xeb, 0x31,                               /*88: jmp    bb <op_lt+0xbb> */
0x81, 0xfa, 0x06, 0x03, 0x00, 0x00,       /*8a: cmp    $0x306,%edx */
0x75, 0x5a,                               /*90: jne    ec <op_lt+0xec> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x08, 0xab, 0x00,/*92: cvtsi2sdl 0xab0800(%rax),%xmm0 */
0xf2, 0x0f, 0x10, 0x88, 0x08, 0x08, 0xab, 0x00,/*9a: movsd  0xab0808(%rax),%xmm1 */
0x66, 0x0f, 0x2e, 0xc8,                   /*a2: ucomisd %xmm0,%xmm1 */
0x0f, 0x97, 0xc1,                         /*a6: seta   %cl */
0xeb, 0x1b,                               /*a9: jmp    c6 <op_lt+0xc6> */
0x81, 0xfa, 0x06, 0x06, 0x00, 0x00,       /*ab: cmp    $0x606,%edx */
0x75, 0x39,                               /*b1: jne    ec <op_lt+0xec> */
0xf2, 0x0f, 0x10, 0x80, 0x08, 0x08, 0xab, 0x00,/*b3: movsd  0xab0808(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0x80, 0x00, 0x08, 0xab, 0x00,/*bb: ucomisd 0xab0800(%rax),%xmm0 */
0x0f, 0x97, 0xc1,                         /*c3: seta   %cl */
0x0f, 0xb6, 0xc9,                         /*c6: movzbl %cl,%ecx */
0x85, 0xc9,                               /*c9: test   %ecx,%ecx */
0x74, 0x0c,                               /*cb: je     d9 <op_lt+0xd9> */
0x48, 0xb9, 0x01, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xf0, 0xff,/*cd: movabs $0xfff0c00000000001,%rcx */
0xeb, 0x0a,                               /*d7: jmp    e3 <op_lt+0xe3> */
0x48, 0xb9, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0xf0, 0xff,/*d9: movabs $0xfff0400000000001,%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*e3: mov    %rcx,0xab0800(%rax) */
0xeb, 0x1d,                               /*ea: jmp    109 <op_lt+0x109> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*ec: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*f1: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*f6: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*fb: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*101: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*104: callq  109 <op_lt+0x109> */
0x48, 0x89, 0xdf,                         /*109: mov    %rbx,%rdi */
0x5b,                                     /*10c: pop    %rbx */

};
static uint8_t op_lt__rodata[] = {

};

static void op_lt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 8 + 4;
  *((int32_t *)(op + 52)) = a * 8 + 12;
  *((int32_t *)(op + 105)) = a * 8 + 0;
  *((int32_t *)(op + 111)) = a * 8 + 8;
  *((int32_t *)(op + 132)) = a * 8 + 8;
  *((int32_t *)(op + 150)) = a * 8 + 0;
  *((int32_t *)(op + 158)) = a * 8 + 8;
  *((int32_t *)(op + 183)) = a * 8 + 8;
  *((int32_t *)(op + 191)) = a * 8 + 0;
  *((int32_t *)(op + 230)) = a * 8 + 0;
  *((int32_t *)(op + 242)) = a * 1 + 0;
  *((int32_t *)(op + 247)) = b * 1 + 0;
  *((int32_t *)(op + 253)) = c * 1 + 0;
}

static void op_lt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_lt_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 4, 10..13], [8, 12, 52..55], [8, 0, 105..108], [8, 8, 111..114], [8, 8, 132..135], [8, 0, 150..153], [8, 8, 158..161], [8, 8, 183..186], [8, 0, 191..194], [8, 0, 230..233], [1, 0, 242..245]], "b"=>[[1, 0, 247..250]], "c"=>[[1, 0, 253..256]]} */
static uint8_t op_le__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x04, 0x08, 0xab, 0x00,       /*8: mov    0xab0804(%rax),%edx */
0xb9, 0x00, 0x06, 0x00, 0x00,             /*e: mov    $0x600,%ecx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*13: cmp    $0xfff00001,%edx */
0x72, 0x17,                               /*19: jb     32 <op_le+0x32> */
0xc1, 0xea, 0x06,                         /*1b: shr    $0x6,%edx */
0x81, 0xe2, 0x00, 0x3f, 0x00, 0x00,       /*1e: and    $0x3f00,%edx */
0x81, 0xc2, 0x00, 0xff, 0xff, 0x00,       /*24: add    $0xffff00,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*2a: and    $0xffff00,%edx */
0x89, 0xd1,                               /*30: mov    %edx,%ecx */
0x8b, 0xb0, 0x0c, 0x08, 0xab, 0x00,       /*32: mov    0xab080c(%rax),%esi */
0xba, 0x06, 0x00, 0x00, 0x00,             /*38: mov    $0x6,%edx */
0x81, 0xfe, 0x01, 0x00, 0xf0, 0xff,       /*3d: cmp    $0xfff00001,%esi */
0x72, 0x10,                               /*43: jb     55 <op_le+0x55> */
0xc1, 0xee, 0x0e,                         /*45: shr    $0xe,%esi */
0x83, 0xe6, 0x3f,                         /*48: and    $0x3f,%esi */
0x81, 0xc6, 0xff, 0x00, 0x00, 0x00,       /*4b: add    $0xff,%esi */
0x40, 0x0f, 0xb6, 0xd6,                   /*51: movzbl %sil,%edx */
0x09, 0xca,                               /*55: or     %ecx,%edx */
0x81, 0xfa, 0x02, 0x06, 0x00, 0x00,       /*57: cmp    $0x602,%edx */
0x7f, 0x19,                               /*5d: jg     78 <op_le+0x78> */
0x81, 0xfa, 0x03, 0x03, 0x00, 0x00,       /*5f: cmp    $0x303,%edx */
0x75, 0x23,                               /*65: jne    8a <op_le+0x8a> */
0x8b, 0x88, 0x00, 0x08, 0xab, 0x00,       /*67: mov    0xab0800(%rax),%ecx */
0x3b, 0x88, 0x08, 0x08, 0xab, 0x00,       /*6d: cmp    0xab0808(%rax),%ecx */
0x0f, 0x9e, 0xc1,                         /*73: setle  %cl */
0xeb, 0x4e,                               /*76: jmp    c6 <op_le+0xc6> */
0x81, 0xfa, 0x03, 0x06, 0x00, 0x00,       /*78: cmp    $0x603,%edx */
0x75, 0x2b,                               /*7e: jne    ab <op_le+0xab> */
0xf2, 0x0f, 0x2a, 0x80, 0x08, 0x08, 0xab, 0x00,/*80: cvtsi2sdl 0xab0808(%rax),%xmm0 */
0xeb, 0x31,                               /*88: jmp    bb <op_le+0xbb> */
0x81, 0xfa, 0x06, 0x03, 0x00, 0x00,       /*8a: cmp    $0x306,%edx */
0x75, 0x5a,                               /*90: jne    ec <op_le+0xec> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x08, 0xab, 0x00,/*92: cvtsi2sdl 0xab0800(%rax),%xmm0 */
0xf2, 0x0f, 0x10, 0x88, 0x08, 0x08, 0xab, 0x00,/*9a: movsd  0xab0808(%rax),%xmm1 */
0x66, 0x0f, 0x2e, 0xc8,                   /*a2: ucomisd %xmm0,%xmm1 */
0x0f, 0x93, 0xc1,                         /*a6: setae  %cl */
0xeb, 0x1b,                               /*a9: jmp    c6 <op_le+0xc6> */
0x81, 0xfa, 0x06, 0x06, 0x00, 0x00,       /*ab: cmp    $0x606,%edx */
0x75, 0x39,                               /*b1: jne    ec <op_le+0xec> */
0xf2, 0x0f, 0x10, 0x80, 0x08, 0x08, 0xab, 0x00,/*b3: movsd  0xab0808(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0x80, 0x00, 0x08, 0xab, 0x00,/*bb: ucomisd 0xab0800(%rax),%xmm0 */
0x0f, 0x93, 0xc1,                         /*c3: setae  %cl */
0x0f, 0xb6, 0xc9,                         /*c6: movzbl %cl,%ecx */
0x85, 0xc9,                               /*c9: test   %ecx,%ecx */
0x74, 0x0c,                               /*cb: je     d9 <op_le+0xd9> */
0x48, 0xb9, 0x01, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xf0, 0xff,/*cd: movabs $0xfff0c00000000001,%rcx */
0xeb, 0x0a,                               /*d7: jmp    e3 <op_le+0xe3> */
0x48, 0xb9, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0xf0, 0xff,/*d9: movabs $0xfff0400000000001,%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*e3: mov    %rcx,0xab0800(%rax) */
0xeb, 0x1d,                               /*ea: jmp    109 <op_le+0x109> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*ec: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*f1: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*f6: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*fb: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*101: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*104: callq  109 <op_le+0x109> */
0x48, 0x89, 0xdf,                         /*109: mov    %rbx,%rdi */
0x5b,                                     /*10c: pop    %rbx */

};
static uint8_t op_le__rodata[] = {

};

static void op_le_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 8 + 4;
  *((int32_t *)(op + 52)) = a * 8 + 12;
  *((int32_t *)(op + 105)) = a * 8 + 0;
  *((int32_t *)(op + 111)) = a * 8 + 8;
  *((int32_t *)(op + 132)) = a * 8 + 8;
  *((int32_t *)(op + 150)) = a * 8 + 0;
  *((int32_t *)(op + 158)) = a * 8 + 8;
  *((int32_t *)(op + 183)) = a * 8 + 8;
  *((int32_t *)(op + 191)) = a * 8 + 0;
  *((int32_t *)(op + 230)) = a * 8 + 0;
  *((int32_t *)(op + 242)) = a * 1 + 0;
  *((int32_t *)(op + 247)) = b * 1 + 0;
  *((int32_t *)(op + 253)) = c * 1 + 0;
}

static void op_le_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_le_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 4, 10..13], [8, 12, 52..55], [8, 0, 105..108], [8, 8, 111..114], [8, 0, 132..135], [8, 8, 140..143], [8, 0, 165..168], [8, 0, 183..186], [8, 8, 191..194], [8, 0, 230..233], [1, 0, 242..245]], "b"=>[[1, 0, 247..250]], "c"=>[[1, 0, 253..256]]} */
static uint8_t op_gt__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x04, 0x08, 0xab, 0x00,       /*8: mov    0xab0804(%rax),%edx */
0xb9, 0x00, 0x06, 0x00, 0x00,             /*e: mov    $0x600,%ecx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*13: cmp    $0xfff00001,%edx */
0x72, 0x17,                               /*19: jb     32 <op_gt+0x32> */
0xc1, 0xea, 0x06,                         /*1b: shr    $0x6,%edx */
0x81, 0xe2, 0x00, 0x3f, 0x00, 0x00,       /*1e: and    $0x3f00,%edx */
0x81, 0xc2, 0x00, 0xff, 0xff, 0x00,       /*24: add    $0xffff00,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*2a: and    $0xffff00,%edx */
0x89, 0xd1,                               /*30: mov    %edx,%ecx */
0x8b, 0xb0, 0x0c, 0x08, 0xab, 0x00,       /*32: mov    0xab080c(%rax),%esi */
0xba, 0x06, 0x00, 0x00, 0x00,             /*38: mov    $0x6,%edx */
0x81, 0xfe, 0x01, 0x00, 0xf0, 0xff,       /*3d: cmp    $0xfff00001,%esi */
0x72, 0x10,                               /*43: jb     55 <op_gt+0x55> */
0xc1, 0xee, 0x0e,                         /*45: shr    $0xe,%esi */
0x83, 0xe6, 0x3f,                         /*48: and    $0x3f,%esi */
0x81, 0xc6, 0xff, 0x00, 0x00, 0x00,       /*4b: add    $0xff,%esi */
0x40, 0x0f, 0xb6, 0xd6,                   /*51: movzbl %sil,%edx */
0x09, 0xca,                               /*55: or     %ecx,%edx */
0x81, 0xfa, 0x02, 0x06, 0x00, 0x00,       /*57: cmp    $0x602,%edx */
0x7f, 0x19,                               /*5d: jg     78 <op_gt+0x78> */
0x81, 0xfa, 0x03, 0x03, 0x00, 0x00,       /*5f: cmp    $0x303,%edx */
0x75, 0x32,                               /*65: jne    99 <op_gt+0x99> */
0x8b, 0x88, 0x00, 0x08, 0xab, 0x00,       /*67: mov    0xab0800(%rax),%ecx */
0x3b, 0x88, 0x08, 0x08, 0xab, 0x00,       /*6d: cmp    0xab0808(%rax),%ecx */
0x0f, 0x9f, 0xc1,                         /*73: setg   %cl */
0xeb, 0x4e,                               /*76: jmp    c6 <op_gt+0xc6> */
0x81, 0xfa, 0x03, 0x06, 0x00, 0x00,       /*78: cmp    $0x603,%edx */
0x75, 0x2b,                               /*7e: jne    ab <op_gt+0xab> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x08, 0xab, 0x00,/*80: movsd  0xab0800(%rax),%xmm0 */
0xf2, 0x0f, 0x2a, 0x88, 0x08, 0x08, 0xab, 0x00,/*88: cvtsi2sdl 0xab0808(%rax),%xmm1 */
0x66, 0x0f, 0x2e, 0xc1,                   /*90: ucomisd %xmm1,%xmm0 */
0x0f, 0x97, 0xc1,                         /*94: seta   %cl */
0xeb, 0x2d,                               /*97: jmp    c6 <op_gt+0xc6> */
0x81, 0xfa, 0x06, 0x03, 0x00, 0x00,       /*99: cmp    $0x306,%edx */
0x75, 0x4b,                               /*9f: jne    ec <op_gt+0xec> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x08, 0xab, 0x00,/*a1: cvtsi2sdl 0xab0800(%rax),%xmm0 */
0xeb, 0x10,                               /*a9: jmp    bb <op_gt+0xbb> */
0x81, 0xfa, 0x06, 0x06, 0x00, 0x00,       /*ab: cmp    $0x606,%edx */
0x75, 0x39,                               /*b1: jne    ec <op_gt+0xec> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x08, 0xab, 0x00,/*b3: movsd  0xab0800(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0x80, 0x08, 0x08, 0xab, 0x00,/*bb: ucomisd 0xab0808(%rax),%xmm0 */
0x0f, 0x97, 0xc1,                         /*c3: seta   %cl */
0x0f, 0xb6, 0xc9,                         /*c6: movzbl %cl,%ecx */
0x85, 0xc9,                               /*c9: test   %ecx,%ecx */
0x74, 0x0c,                               /*cb: je     d9 <op_gt+0xd9> */
0x48, 0xb9, 0x01, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xf0, 0xff,/*cd: movabs $0xfff0c00000000001,%rcx */
0xeb, 0x0a,                               /*d7: jmp    e3 <op_gt+0xe3> */
0x48, 0xb9, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0xf0, 0xff,/*d9: movabs $0xfff0400000000001,%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*e3: mov    %rcx,0xab0800(%rax) */
0xeb, 0x1d,                               /*ea: jmp    109 <op_gt+0x109> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*ec: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*f1: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*f6: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*fb: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*101: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*104: callq  109 <op_gt+0x109> */
0x48, 0x89, 0xdf,                         /*109: mov    %rbx,%rdi */
0x5b,                                     /*10c: pop    %rbx */

};
static uint8_t op_gt__rodata[] = {

};

static void op_gt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 8 + 4;
  *((int32_t *)(op + 52)) = a * 8 + 12;
  *((int32_t *)(op + 105)) = a * 8 + 0;
  *((int32_t *)(op + 111)) = a * 8 + 8;
  *((int32_t *)(op + 132)) = a * 8 + 0;
  *((int32_t *)(op + 140)) = a * 8 + 8;
  *((int32_t *)(op + 165)) = a * 8 + 0;
  *((int32_t *)(op + 183)) = a * 8 + 0;
  *((int32_t *)(op + 191)) = a * 8 + 8;
  *((int32_t *)(op + 230)) = a * 8 + 0;
  *((int32_t *)(op + 242)) = a * 1 + 0;
  *((int32_t *)(op + 247)) = b * 1 + 0;
  *((int32_t *)(op + 253)) = c * 1 + 0;
}

static void op_gt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_gt_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 4, 10..13], [8, 12, 52..55], [8, 0, 105..108], [8, 8, 111..114], [8, 0, 132..135], [8, 8, 140..143], [8, 0, 165..168], [8, 0, 183..186], [8, 8, 191..194], [8, 0, 230..233], [1, 0, 242..245]], "b"=>[[1, 0, 247..250]], "c"=>[[1, 0, 253..256]]} */
static uint8_t op_ge__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x04, 0x08, 0xab, 0x00,       /*8: mov    0xab0804(%rax),%edx */
0xb9, 0x00, 0x06, 0x00, 0x00,             /*e: mov    $0x600,%ecx */
0x81, 0xfa, 0x01, 0x00, 0xf0, 0xff,       /*13: cmp    $0xfff00001,%edx */
0x72, 0x17,                               /*19: jb     32 <op_ge+0x32> */
0xc1, 0xea, 0x06,                         /*1b: shr    $0x6,%edx */
0x81, 0xe2, 0x00, 0x3f, 0x00, 0x00,       /*1e: and    $0x3f00,%edx */
0x81, 0xc2, 0x00, 0xff, 0xff, 0x00,       /*24: add    $0xffff00,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*2a: and    $0xffff00,%edx */
0x89, 0xd1,                               /*30: mov    %edx,%ecx */
0x8b, 0xb0, 0x0c, 0x08, 0xab, 0x00,       /*32: mov    0xab080c(%rax),%esi */
0xba, 0x06, 0x00, 0x00, 0x00,             /*38: mov    $0x6,%edx */
0x81, 0xfe, 0x01, 0x00, 0xf0, 0xff,       /*3d: cmp    $0xfff00001,%esi */
0x72, 0x10,                               /*43: jb     55 <op_ge+0x55> */
0xc1, 0xee, 0x0e,                         /*45: shr    $0xe,%esi */
0x83, 0xe6, 0x3f,                         /*48: and    $0x3f,%esi */
0x81, 0xc6, 0xff, 0x00, 0x00, 0x00,       /*4b: add    $0xff,%esi */
0x40, 0x0f, 0xb6, 0xd6,                   /*51: movzbl %sil,%edx */
0x09, 0xca,                               /*55: or     %ecx,%edx */
0x81, 0xfa, 0x02, 0x06, 0x00, 0x00,       /*57: cmp    $0x602,%edx */
0x7f, 0x19,                               /*5d: jg     78 <op_ge+0x78> */
0x81, 0xfa, 0x03, 0x03, 0x00, 0x00,       /*5f: cmp    $0x303,%edx */
0x75, 0x32,                               /*65: jne    99 <op_ge+0x99> */
0x8b, 0x88, 0x00, 0x08, 0xab, 0x00,       /*67: mov    0xab0800(%rax),%ecx */
0x3b, 0x88, 0x08, 0x08, 0xab, 0x00,       /*6d: cmp    0xab0808(%rax),%ecx */
0x0f, 0x9d, 0xc1,                         /*73: setge  %cl */
0xeb, 0x4e,                               /*76: jmp    c6 <op_ge+0xc6> */
0x81, 0xfa, 0x03, 0x06, 0x00, 0x00,       /*78: cmp    $0x603,%edx */
0x75, 0x2b,                               /*7e: jne    ab <op_ge+0xab> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x08, 0xab, 0x00,/*80: movsd  0xab0800(%rax),%xmm0 */
0xf2, 0x0f, 0x2a, 0x88, 0x08, 0x08, 0xab, 0x00,/*88: cvtsi2sdl 0xab0808(%rax),%xmm1 */
0x66, 0x0f, 0x2e, 0xc1,                   /*90: ucomisd %xmm1,%xmm0 */
0x0f, 0x93, 0xc1,                         /*94: setae  %cl */
0xeb, 0x2d,                               /*97: jmp    c6 <op_ge+0xc6> */
0x81, 0xfa, 0x06, 0x03, 0x00, 0x00,       /*99: cmp    $0x306,%edx */
0x75, 0x4b,                               /*9f: jne    ec <op_ge+0xec> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x08, 0xab, 0x00,/*a1: cvtsi2sdl 0xab0800(%rax),%xmm0 */
0xeb, 0x10,                               /*a9: jmp    bb <op_ge+0xbb> */
0x81, 0xfa, 0x06, 0x06, 0x00, 0x00,       /*ab: cmp    $0x606,%edx */
0x75, 0x39,                               /*b1: jne    ec <op_ge+0xec> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x08, 0xab, 0x00,/*b3: movsd  0xab0800(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0x80, 0x08, 0x08, 0xab, 0x00,/*bb: ucomisd 0xab0808(%rax),%xmm0 */
0x0f, 0x93, 0xc1,                         /*c3: setae  %cl */
0x0f, 0xb6, 0xc9,                         /*c6: movzbl %cl,%ecx */
0x85, 0xc9,                               /*c9: test   %ecx,%ecx */
0x74, 0x0c,                               /*cb: je     d9 <op_ge+0xd9> */
0x48, 0xb9, 0x01, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xf0, 0xff,/*cd: movabs $0xfff0c00000000001,%rcx */
0xeb, 0x0a,                               /*d7: jmp    e3 <op_ge+0xe3> */
0x48, 0xb9, 0x01, 0x00, 0x00, 0x00, 0x00, 0x40, 0xf0, 0xff,/*d9: movabs $0xfff0400000000001,%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*e3: mov    %rcx,0xab0800(%rax) */
0xeb, 0x1d,                               /*ea: jmp    109 <op_ge+0x109> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*ec: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*f1: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*f6: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*fb: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*101: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*104: callq  109 <op_ge+0x109> */
0x48, 0x89, 0xdf,                         /*109: mov    %rbx,%rdi */
0x5b,                                     /*10c: pop    %rbx */

};
static uint8_t op_ge__rodata[] = {

};

static void op_ge_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 8 + 4;
  *((int32_t *)(op + 52)) = a * 8 + 12;
  *((int32_t *)(op + 105)) = a * 8 + 0;
  *((int32_t *)(op + 111)) = a * 8 + 8;
  *((int32_t *)(op + 132)) = a * 8 + 0;
  *((int32_t *)(op + 140)) = a * 8 + 8;
  *((int32_t *)(op + 165)) = a * 8 + 0;
  *((int32_t *)(op + 183)) = a * 8 + 0;
  *((int32_t *)(op + 191)) = a * 8 + 8;
  *((int32_t *)(op + 230)) = a * 8 + 0;
  *((int32_t *)(op + 242)) = a * 1 + 0;
  *((int32_t *)(op + 247)) = b * 1 + 0;
  *((int32_t *)(op + 253)) = c * 1 + 0;
}

static void op_ge_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_ge_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[8, 0, 17..20]], "c"=>[[1, 0, 22..25]], "a"=>[[8, 0, 34..37]]} */
static uint8_t op_array__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x50,                   /*a: mov    0x50(%rbx),%rdi */
0x49, 0x8d, 0x96, 0x00, 0x08, 0xbc, 0x00, /*e: lea    0xbc0800(%r14),%rdx */
0xbe, 0x00, 0x00, 0xcd, 0x00,             /*15: mov    $0xcd0000,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1a: callq  1f <op_array+0x1f> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*1f: mov    %rax,0xab0800(%r14) */
0x8b, 0x43, 0x48,                         /*26: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*29: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*2d: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*33: mov    %rbx,%rdi */
0x5b,                                     /*36: pop    %rbx */
0x41, 0x5e,                               /*37: pop    %r14 */

};
static uint8_t op_array__rodata[] = {

};

static void op_array_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 8 + 0;
  *((int32_t *)(op + 22)) = c * 1 + 0;
  *((int32_t *)(op + 34)) = a * 8 + 0;
}

static void op_array_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_array_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[8, 0, 19..22]], "a"=>[[8, 0, 34..37]]} */
static uint8_t op_arycat__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x48, 0x89, 0xfb,                         /*5: mov    %rdi,%rbx */
0x4c, 0x8b, 0x7b, 0x18,                   /*8: mov    0x18(%rbx),%r15 */
0x4c, 0x8b, 0x73, 0x50,                   /*c: mov    0x50(%rbx),%r14 */
0x49, 0x8b, 0xb7, 0x00, 0x08, 0xbc, 0x00, /*10: mov    0xbc0800(%r15),%rsi */
0x4c, 0x89, 0xf7,                         /*17: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1a: callq  1f <op_arycat+0x1f> */
0x49, 0x8b, 0xb7, 0x00, 0x08, 0xab, 0x00, /*1f: mov    0xab0800(%r15),%rsi */
0x4c, 0x89, 0xf7,                         /*26: mov    %r14,%rdi */
0x48, 0x89, 0xc2,                         /*29: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2c: callq  31 <op_arycat+0x31> */
0x8b, 0x43, 0x48,                         /*31: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*34: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*38: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*3e: mov    %rbx,%rdi */
0x5b,                                     /*41: pop    %rbx */
0x41, 0x5e,                               /*42: pop    %r14 */
0x41, 0x5f,                               /*44: pop    %r15 */

};
static uint8_t op_arycat__rodata[] = {

};

static void op_arycat_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = b * 8 + 0;
  *((int32_t *)(op + 34)) = a * 8 + 0;
}

static void op_arycat_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_arycat_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18]], "b"=>[[8, 0, 22..25]]} */
static uint8_t op_arypush__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x50,                   /*8: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xab, 0x00, /*c: mov    0xab0800(%rax),%rsi */
0x48, 0x8b, 0x90, 0x00, 0x08, 0xbc, 0x00, /*13: mov    0xbc0800(%rax),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1a: callq  1f <op_arypush+0x1f> */
0x48, 0x89, 0xdf,                         /*1f: mov    %rbx,%rdi */
0x5b,                                     /*22: pop    %rbx */

};
static uint8_t op_arypush__rodata[] = {

};

static void op_arypush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
  *((int32_t *)(op + 22)) = b * 8 + 0;
}

static void op_arypush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_arypush_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[8, 0, 13..16]], "c"=>[[1, 0, 48..51]], "a"=>[[8, 0, 60..63], [8, 4, 69..72], [8, 0, 83..86]]} */
static uint8_t op_aref__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x49, 0x8b, 0xb6, 0x00, 0x08, 0xbc, 0x00, /*a: mov    0xbc0800(%r14),%rsi */
0x48, 0x89, 0xf0,                         /*11: mov    %rsi,%rax */
0x48, 0xc1, 0xe8, 0x20,                   /*14: shr    $0x20,%rax */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*18: cmp    $0xfff00001,%eax */
0x72, 0x23,                               /*1d: jb     42 <op_aref+0x42> */
0x25, 0x00, 0xc0, 0x0f, 0x00,             /*1f: and    $0xfc000,%eax */
0x3d, 0x00, 0xc0, 0x03, 0x00,             /*24: cmp    $0x3c000,%eax */
0x75, 0x17,                               /*29: jne    42 <op_aref+0x42> */
0x48, 0x8b, 0x7b, 0x50,                   /*2b: mov    0x50(%rbx),%rdi */
0xba, 0x00, 0x00, 0xcd, 0x00,             /*2f: mov    $0xcd0000,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*34: callq  39 <op_aref+0x39> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*39: mov    %rax,0xab0800(%r14) */
0xeb, 0x19,                               /*40: jmp    5b <op_aref+0x5b> */
0x41, 0xc7, 0x86, 0x04, 0x08, 0xab, 0x00, 0x00, 0x40, 0xf0, 0xff,/*42: movl   $0xfff04000,0xab0804(%r14) */
0x48, 0x8b, 0x43, 0x18,                   /*4d: mov    0x18(%rbx),%rax */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*51: movl   $0x0,0xab0800(%rax) */
0x48, 0x89, 0xdf,                         /*5b: mov    %rbx,%rdi */
0x5b,                                     /*5e: pop    %rbx */
0x41, 0x5e,                               /*5f: pop    %r14 */

};
static uint8_t op_aref__rodata[] = {

};

static void op_aref_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = b * 8 + 0;
  *((int32_t *)(op + 48)) = c * 1 + 0;
  *((int32_t *)(op + 60)) = a * 8 + 0;
  *((int32_t *)(op + 69)) = a * 8 + 4;
  *((int32_t *)(op + 83)) = a * 8 + 0;
}

static void op_aref_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_aref_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[8, 0, 15..18]], "a"=>[[8, 0, 22..25]], "c"=>[[1, 0, 27..30]]} */
static uint8_t op_aset__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x50,                   /*8: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*c: mov    0xbc0800(%rax),%rsi */
0x48, 0x8b, 0x88, 0x00, 0x08, 0xab, 0x00, /*13: mov    0xab0800(%rax),%rcx */
0xba, 0x00, 0x00, 0xcd, 0x00,             /*1a: mov    $0xcd0000,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1f: callq  24 <op_aset+0x24> */
0x48, 0x89, 0xdf,                         /*24: mov    %rbx,%rdi */
0x5b,                                     /*27: pop    %rbx */

};
static uint8_t op_aset__rodata[] = {

};

static void op_aset_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 8 + 0;
  *((int32_t *)(op + 22)) = a * 8 + 0;
  *((int32_t *)(op + 27)) = c * 1 + 0;
}

static void op_aset_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_aset_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 21..24], [8, 0, 127..130], [8, 0, 206..209], [8, 0, 284..287]], "b"=>[[8, 0, 114..117], [1, 1, 291..294]], "c"=>[[1, 0, 178..181], [1, 0, 259..262], [1, 0, 364..367]]} */
static uint8_t op_apost__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x89, 0xfd,                         /*a: mov    %rdi,%rbp */
0x4c, 0x8b, 0x65, 0x18,                   /*d: mov    0x18(%rbp),%r12 */
0x49, 0x8b, 0x84, 0x24, 0x00, 0x08, 0xab, 0x00,/*11: mov    0xab0800(%r12),%rax */
0x48, 0x89, 0xc1,                         /*19: mov    %rax,%rcx */
0x48, 0xc1, 0xe9, 0x20,                   /*1c: shr    $0x20,%rcx */
0x81, 0xf9, 0x01, 0x00, 0xf0, 0xff,       /*20: cmp    $0xfff00001,%ecx */
0x0f, 0x82, 0x91, 0x00, 0x00, 0x00,       /*26: jb     bd <op_apost+0xbd> */
0x81, 0xe1, 0x00, 0xc0, 0x0f, 0x00,       /*2c: and    $0xfc000,%ecx */
0x81, 0xf9, 0x00, 0xc0, 0x03, 0x00,       /*32: cmp    $0x3c000,%ecx */
0x0f, 0x85, 0x7f, 0x00, 0x00, 0x00,       /*38: jne    bd <op_apost+0xbd> */
0x48, 0xc1, 0xe0, 0x02,                   /*3e: shl    $0x2,%rax */
0x49, 0xbe, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*42: movabs $0xfffffffffffc,%r14 */
0x49, 0x21, 0xc6,                         /*4c: and    %rax,%r14 */
0x45, 0x8b, 0x7e, 0x18,                   /*4f: mov    0x18(%r14),%r15d */
0x48, 0x8b, 0x7d, 0x50,                   /*53: mov    0x50(%rbp),%rdi */
0x41, 0x81, 0xff, 0x01, 0x00, 0x89, 0x01, /*57: cmp    $0x1890001,%r15d */
0x0f, 0x8c, 0xaa, 0x00, 0x00, 0x00,       /*5e: jl     10e <op_apost+0x10e> */
0x41, 0x8d, 0xb7, 0x00, 0x00, 0x77, 0xfe, /*64: lea    -0x1890000(%r15),%esi */
0x49, 0x8b, 0x56, 0x28,                   /*6b: mov    0x28(%r14),%rdx */
0x48, 0x81, 0xc2, 0x00, 0x08, 0xbc, 0x00, /*6f: add    $0xbc0800,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*76: callq  7b <op_apost+0x7b> */
0x49, 0x89, 0x84, 0x24, 0x00, 0x08, 0xab, 0x00,/*7b: mov    %rax,0xab0800(%r12) */
0x41, 0x81, 0xc7, 0x00, 0x00, 0x33, 0xff, /*83: add    $0xff330000,%r15d */
0x31, 0xc0,                               /*8a: xor    %eax,%eax */
0x0f, 0x1f, 0x40, 0x00,                   /*8c: nopl   0x0(%rax) */
0x48, 0x8b, 0x4d, 0x18,                   /*90: mov    0x18(%rbp),%rcx */
0x41, 0x8d, 0x14, 0x07,                   /*94: lea    (%r15,%rax,1),%edx */
0x48, 0x63, 0xd2,                         /*98: movslq %edx,%rdx */
0x49, 0x8b, 0x76, 0x28,                   /*9b: mov    0x28(%r14),%rsi */
0xf2, 0x0f, 0x10, 0x04, 0xd6,             /*9f: movsd  (%rsi,%rdx,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x84, 0xc1, 0x08, 0x00, 0x58, 0x05,/*a4: movsd  %xmm0,0x5580008(%rcx,%rax,8) */
0x48, 0xff, 0xc0,                         /*ad: inc    %rax */
0x48, 0x3d, 0x00, 0x00, 0xcd, 0x00,       /*b0: cmp    $0xcd0000,%rax */
0x75, 0xd8,                               /*b6: jne    90 <op_apost+0x90> */
0xe9, 0xe9, 0x00, 0x00, 0x00,             /*b8: jmpq   1a6 <op_apost+0x1a6> */
0x48, 0x8b, 0x7d, 0x50,                   /*bd: mov    0x50(%rbp),%rdi */
0x31, 0xdb,                               /*c1: xor    %ebx,%ebx */
0x31, 0xf6,                               /*c3: xor    %esi,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c5: callq  ca <op_apost+0xca> */
0x49, 0x89, 0x84, 0x24, 0x00, 0x08, 0xab, 0x00,/*ca: mov    %rax,0xab0800(%r12) */
0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*d2: data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x45, 0x18,                   /*e0: mov    0x18(%rbp),%rax */
0xc7, 0x84, 0xd8, 0x0c, 0x00, 0x58, 0x05, 0x00, 0x40, 0xf0, 0xff,/*e4: movl   $0xfff04000,0x558000c(%rax,%rbx,8) */
0x48, 0x8b, 0x45, 0x18,                   /*ef: mov    0x18(%rbp),%rax */
0xc7, 0x84, 0xd8, 0x08, 0x00, 0x58, 0x05, 0x00, 0x00, 0x00, 0x00,/*f3: movl   $0x0,0x5580008(%rax,%rbx,8) */
0x48, 0xff, 0xc3,                         /*fe: inc    %rbx */
0x81, 0xfb, 0x00, 0x00, 0xcd, 0x00,       /*101: cmp    $0xcd0000,%ebx */
0x75, 0xd7,                               /*107: jne    e0 <op_apost+0xe0> */
0xe9, 0x98, 0x00, 0x00, 0x00,             /*109: jmpq   1a6 <op_apost+0x1a6> */
0x45, 0x31, 0xed,                         /*10e: xor    %r13d,%r13d */
0x31, 0xf6,                               /*111: xor    %esi,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*113: callq  118 <op_apost+0x118> */
0x49, 0x89, 0x84, 0x24, 0x00, 0x08, 0xab, 0x00,/*118: mov    %rax,0xab0800(%r12) */
0x41, 0x81, 0xff, 0x01, 0x00, 0xbc, 0x00, /*120: cmp    $0xbc0001,%r15d */
0x7c, 0x42,                               /*127: jl     16b <op_apost+0x16b> */
0x45, 0x89, 0xfd,                         /*129: mov    %r15d,%r13d */
0x41, 0x81, 0xc5, 0x00, 0x00, 0x44, 0xff, /*12c: add    $0xff440000,%r13d */
0x31, 0xc0,                               /*133: xor    %eax,%eax */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*135: data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x4d, 0x18,                   /*140: mov    0x18(%rbp),%rcx */
0x49, 0x8b, 0x56, 0x28,                   /*144: mov    0x28(%r14),%rdx */
0xf2, 0x0f, 0x10, 0x84, 0xc2, 0x00, 0x00, 0xe0, 0x05,/*148: movsd  0x5e00000(%rdx,%rax,8),%xmm0 */
0xf2, 0x0f, 0x11, 0x84, 0xc1, 0x08, 0x00, 0x58, 0x05,/*151: movsd  %xmm0,0x5580008(%rcx,%rax,8) */
0x48, 0xff, 0xc0,                         /*15a: inc    %rax */
0x41, 0x39, 0xc5,                         /*15d: cmp    %eax,%r13d */
0x75, 0xde,                               /*160: jne    140 <op_apost+0x140> */
0x41, 0x81, 0xfd, 0xff, 0xff, 0xcc, 0x00, /*162: cmp    $0xccffff,%r13d */
0x7f, 0x3b,                               /*169: jg     1a6 <op_apost+0x1a6> */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*16b: mov    $0xcd0000,%eax */
0x44, 0x29, 0xe8,                         /*170: sub    %r13d,%eax */
0x49, 0x63, 0xcd,                         /*173: movslq %r13d,%rcx */
0x48, 0xc1, 0xe1, 0x03,                   /*176: shl    $0x3,%rcx */
0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00,       /*17a: nopw   0x0(%rax,%rax,1) */
0x48, 0x8b, 0x55, 0x18,                   /*180: mov    0x18(%rbp),%rdx */
0xc7, 0x84, 0x0a, 0x0c, 0x00, 0x58, 0x05, 0x00, 0x40, 0xf0, 0xff,/*184: movl   $0xfff04000,0x558000c(%rdx,%rcx,1) */
0x48, 0x8b, 0x55, 0x18,                   /*18f: mov    0x18(%rbp),%rdx */
0xc7, 0x84, 0x0a, 0x08, 0x00, 0x58, 0x05, 0x00, 0x00, 0x00, 0x00,/*193: movl   $0x0,0x5580008(%rdx,%rcx,1) */
0x48, 0x83, 0xc1, 0x08,                   /*19e: add    $0x8,%rcx */
0xff, 0xc8,                               /*1a2: dec    %eax */
0x75, 0xda,                               /*1a4: jne    180 <op_apost+0x180> */
0x8b, 0x45, 0x48,                         /*1a6: mov    0x48(%rbp),%eax */
0x48, 0x8b, 0x4d, 0x50,                   /*1a9: mov    0x50(%rbp),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*1ad: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xef,                         /*1b3: mov    %rbp,%rdi */
0x5b,                                     /*1b6: pop    %rbx */
0x41, 0x5c,                               /*1b7: pop    %r12 */
0x41, 0x5d,                               /*1b9: pop    %r13 */
0x41, 0x5e,                               /*1bb: pop    %r14 */
0x41, 0x5f,                               /*1bd: pop    %r15 */
0x5d,                                     /*1bf: pop    %rbp */

};
static uint8_t op_apost__rodata[] = {

};

static void op_apost_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 127)) = a * 8 + 0;
  *((int32_t *)(op + 206)) = a * 8 + 0;
  *((int32_t *)(op + 284)) = a * 8 + 0;
  *((int32_t *)(op + 114)) = b * 8 + 0;
  *((int32_t *)(op + 291)) = b * 1 + 1;
  *((int32_t *)(op + 178)) = c * 1 + 0;
  *((int32_t *)(op + 259)) = c * 1 + 0;
  *((int32_t *)(op + 364)) = c * 1 + 0;
}

static void op_apost_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_apost_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[8, 0, 21..24]], "a"=>[[8, 0, 33..36]]} */
static uint8_t op_string__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*6: mov    0x50(%rbx),%rdi */
0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x20,                   /*e: mov    0x20(%rbx),%rax */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*12: mov    0xbc0800(%rax),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*19: callq  1e <op_string+0x1e> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*1e: mov    %rax,0xab0800(%r14) */
0x8b, 0x43, 0x48,                         /*25: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*28: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*2c: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*32: mov    %rbx,%rdi */
0x5b,                                     /*35: pop    %rbx */
0x41, 0x5e,                               /*36: pop    %r14 */

};
static uint8_t op_string__rodata[] = {

};

static void op_string_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 8 + 0;
  *((int32_t *)(op + 33)) = a * 8 + 0;
}

static void op_string_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_string_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18]], "b"=>[[8, 0, 22..25]]} */
static uint8_t op_strcat__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x50,                   /*8: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xab, 0x00, /*c: mov    0xab0800(%rax),%rsi */
0x48, 0x8b, 0x90, 0x00, 0x08, 0xbc, 0x00, /*13: mov    0xbc0800(%rax),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1a: callq  1f <op_strcat+0x1f> */
0x48, 0x89, 0xdf,                         /*1f: mov    %rbx,%rdi */
0x5b,                                     /*22: pop    %rbx */

};
static uint8_t op_strcat__rodata[] = {

};

static void op_strcat_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
  *((int32_t *)(op + 22)) = b * 8 + 0;
}

static void op_strcat_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_strcat_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[1, 0, 13..16]], "c"=>[[1, 0, 20..23]], "a"=>[[8, 0, 124..127]]} */
static uint8_t op_hash__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x53,                                     /*5: push   %rbx */
0x48, 0x89, 0xfb,                         /*6: mov    %rdi,%rbx */
0xc7, 0x44, 0x24, 0x04, 0x00, 0x00, 0xbc, 0x00,/*9: movl   $0xbc0000,0x4(%rsp) */
0xc7, 0x04, 0x24, 0x00, 0x00, 0xcd, 0x00, /*11: movl   $0xcd0000,(%rsp) */
0x8b, 0x44, 0x24, 0x04,                   /*18: mov    0x4(%rsp),%eax */
0x8b, 0x0c, 0x24,                         /*1c: mov    (%rsp),%ecx */
0x44, 0x8d, 0x3c, 0x48,                   /*1f: lea    (%rax,%rcx,2),%r15d */
0x48, 0x8b, 0x7b, 0x50,                   /*23: mov    0x50(%rbx),%rdi */
0x8b, 0x34, 0x24,                         /*27: mov    (%rsp),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2a: callq  2f <op_hash+0x2f> */
0x49, 0x89, 0xc6,                         /*2f: mov    %rax,%r14 */
0xeb, 0x38,                               /*32: jmp    6c <op_hash+0x6c> */
0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*34: data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x7b, 0x50,                   /*40: mov    0x50(%rbx),%rdi */
0x48, 0x63, 0x44, 0x24, 0x04,             /*44: movslq 0x4(%rsp),%rax */
0x48, 0x8b, 0x4b, 0x18,                   /*49: mov    0x18(%rbx),%rcx */
0x48, 0x63, 0x74, 0x24, 0x04,             /*4d: movslq 0x4(%rsp),%rsi */
0x48, 0x8b, 0x6b, 0x18,                   /*52: mov    0x18(%rbx),%rbp */
0x48, 0x8b, 0x14, 0xc1,                   /*56: mov    (%rcx,%rax,8),%rdx */
0x48, 0x8b, 0x4c, 0xf5, 0x08,             /*5a: mov    0x8(%rbp,%rsi,8),%rcx */
0x4c, 0x89, 0xf6,                         /*5f: mov    %r14,%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*62: callq  67 <op_hash+0x67> */
0x83, 0x44, 0x24, 0x04, 0x02,             /*67: addl   $0x2,0x4(%rsp) */
0x8b, 0x44, 0x24, 0x04,                   /*6c: mov    0x4(%rsp),%eax */
0x44, 0x39, 0xf8,                         /*70: cmp    %r15d,%eax */
0x7c, 0xcb,                               /*73: jl     40 <op_hash+0x40> */
0x48, 0x8b, 0x43, 0x18,                   /*75: mov    0x18(%rbx),%rax */
0x4c, 0x89, 0xb0, 0x00, 0x08, 0xab, 0x00, /*79: mov    %r14,0xab0800(%rax) */
0x8b, 0x43, 0x48,                         /*80: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*83: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*87: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*8d: mov    %rbx,%rdi */
0x5b,                                     /*90: pop    %rbx */
0x41, 0x5e,                               /*91: pop    %r14 */
0x41, 0x5f,                               /*93: pop    %r15 */
0x5d,                                     /*95: pop    %rbp */

};
static uint8_t op_hash__rodata[] = {

};

static void op_hash_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = b * 1 + 0;
  *((int32_t *)(op + 20)) = c * 1 + 0;
  *((int32_t *)(op + 124)) = a * 8 + 0;
}

static void op_hash_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_hash_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[1, 0, 5..8]], "b"=>[[1, 0, 10..13]], "c"=>[[1, 0, 15..18]]} */
static uint8_t op_lambda__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*4: mov    $0xab0000,%esi */
0xba, 0x00, 0x00, 0xbc, 0x00,             /*9: mov    $0xbc0000,%edx */
0xb9, 0x00, 0x00, 0xcd, 0x00,             /*e: mov    $0xcd0000,%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*13: callq  18 <op_lambda+0x18> */
0x48, 0x89, 0xdf,                         /*18: mov    %rbx,%rdi */
0x5b,                                     /*1b: pop    %rbx */

};
static uint8_t op_lambda__rodata[] = {

};

static void op_lambda_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = a * 1 + 0;
  *((int32_t *)(op + 10)) = b * 1 + 0;
  *((int32_t *)(op + 15)) = c * 1 + 0;
}

static void op_lambda_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_lambda_set_args(op, GETARG_A(c),GETARG_b(c),GETARG_c(c),op_idx);
}


/* args: {"b"=>[[8, 0, 17..20], [8, 8, 24..27]], "c"=>[[1, 0, 29..32]], "a"=>[[8, 0, 41..44]]} */
static uint8_t op_range__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x50,                   /*a: mov    0x50(%rbx),%rdi */
0x49, 0x8b, 0xb6, 0x00, 0x08, 0xbc, 0x00, /*e: mov    0xbc0800(%r14),%rsi */
0x49, 0x8b, 0x96, 0x08, 0x08, 0xbc, 0x00, /*15: mov    0xbc0808(%r14),%rdx */
0xb9, 0x00, 0x00, 0xcd, 0x00,             /*1c: mov    $0xcd0000,%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*21: callq  26 <op_range+0x26> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*26: mov    %rax,0xab0800(%r14) */
0x8b, 0x43, 0x48,                         /*2d: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*30: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*34: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*3a: mov    %rbx,%rdi */
0x5b,                                     /*3d: pop    %rbx */
0x41, 0x5e,                               /*3e: pop    %r14 */

};
static uint8_t op_range__rodata[] = {

};

static void op_range_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 8 + 0;
  *((int32_t *)(op + 24)) = b * 8 + 8;
  *((int32_t *)(op + 29)) = c * 1 + 0;
  *((int32_t *)(op + 41)) = a * 8 + 0;
}

static void op_range_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_range_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 56..59]]} */
static uint8_t op_oclass__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x50,                   /*4: mov    0x50(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x40,                   /*8: mov    0x40(%rcx),%rcx */
0x8b, 0x11,                               /*c: mov    (%rcx),%edx */
0x0f, 0xb6, 0xf2,                         /*e: movzbl %dl,%esi */
0x83, 0xfe, 0x05,                         /*11: cmp    $0x5,%esi */
0x77, 0x05,                               /*14: ja     1b <op_oclass+0x1b> */
0x83, 0xfe, 0x01,                         /*16: cmp    $0x1,%esi */
0x75, 0x1a,                               /*19: jne    35 <op_oclass+0x35> */
0xc1, 0xe2, 0x0e,                         /*1b: shl    $0xe,%edx */
0x81, 0xc2, 0x00, 0x40, 0x00, 0x00,       /*1e: add    $0x4000,%edx */
0x81, 0xca, 0x00, 0x00, 0xf0, 0xff,       /*24: or     $0xfff00000,%edx */
0x48, 0xc1, 0xe2, 0x20,                   /*2a: shl    $0x20,%rdx */
0x48, 0xc1, 0xe9, 0x02,                   /*2e: shr    $0x2,%rcx */
0x48, 0x09, 0xd1,                         /*32: or     %rdx,%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*35: mov    %rcx,0xab0800(%rax) */

};
static uint8_t op_oclass__rodata[] = {

};

static void op_oclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 56)) = a * 8 + 0;
}

static void op_oclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_oclass_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[4, 0, 14..17]], "a"=>[[8, 0, 21..24], [8, 8, 28..31], [8, 0, 173..176]]} */
static uint8_t op_class__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*8: mov    0x28(%rbx),%rcx */
0x8b, 0x89, 0x00, 0x04, 0xbc, 0x00,       /*c: mov    0xbc0400(%rcx),%ecx */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xab, 0x00, /*12: mov    0xab0800(%rax),%rsi */
0x48, 0x8b, 0x90, 0x08, 0x08, 0xab, 0x00, /*19: mov    0xab0808(%rax),%rdx */
0x48, 0x8b, 0x7b, 0x50,                   /*20: mov    0x50(%rbx),%rdi */
0x85, 0xf6,                               /*24: test   %esi,%esi */
0x75, 0x50,                               /*26: jne    78 <op_class+0x78> */
0x48, 0x89, 0xf0,                         /*28: mov    %rsi,%rax */
0x48, 0xc1, 0xe8, 0x20,                   /*2b: shr    $0x20,%rax */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*2f: cmp    $0xfff00001,%eax */
0x72, 0x42,                               /*34: jb     78 <op_class+0x78> */
0x25, 0x00, 0xc0, 0x0f, 0x00,             /*36: and    $0xfc000,%eax */
0x3d, 0x00, 0x40, 0x00, 0x00,             /*3b: cmp    $0x4000,%eax */
0x75, 0x36,                               /*40: jne    78 <op_class+0x78> */
0x48, 0x8b, 0x47, 0x18,                   /*42: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*46: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x70, 0x48,                   /*4a: mov    0x48(%rax),%rsi */
0x8b, 0x06,                               /*4e: mov    (%rsi),%eax */
0x44, 0x0f, 0xb6, 0xc0,                   /*50: movzbl %al,%r8d */
0x41, 0x83, 0xf8, 0x05,                   /*54: cmp    $0x5,%r8d */
0x77, 0x06,                               /*58: ja     60 <op_class+0x60> */
0x41, 0x83, 0xf8, 0x01,                   /*5a: cmp    $0x1,%r8d */
0x75, 0x18,                               /*5e: jne    78 <op_class+0x78> */
0xc1, 0xe0, 0x0e,                         /*60: shl    $0xe,%eax */
0x05, 0x00, 0x40, 0x00, 0x00,             /*63: add    $0x4000,%eax */
0x0d, 0x00, 0x00, 0xf0, 0xff,             /*68: or     $0xfff00000,%eax */
0x48, 0xc1, 0xe0, 0x20,                   /*6d: shl    $0x20,%rax */
0x48, 0xc1, 0xee, 0x02,                   /*71: shr    $0x2,%rsi */
0x48, 0x09, 0xc6,                         /*75: or     %rax,%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*78: callq  7d <op_class+0x7d> */
0x48, 0x8b, 0x4b, 0x18,                   /*7d: mov    0x18(%rbx),%rcx */
0x8b, 0x10,                               /*81: mov    (%rax),%edx */
0x0f, 0xb6, 0xf2,                         /*83: movzbl %dl,%esi */
0x83, 0xfe, 0x05,                         /*86: cmp    $0x5,%esi */
0x77, 0x05,                               /*89: ja     90 <op_class+0x90> */
0x83, 0xfe, 0x01,                         /*8b: cmp    $0x1,%esi */
0x75, 0x1a,                               /*8e: jne    aa <op_class+0xaa> */
0xc1, 0xe2, 0x0e,                         /*90: shl    $0xe,%edx */
0x81, 0xc2, 0x00, 0x40, 0x00, 0x00,       /*93: add    $0x4000,%edx */
0x81, 0xca, 0x00, 0x00, 0xf0, 0xff,       /*99: or     $0xfff00000,%edx */
0x48, 0xc1, 0xe2, 0x20,                   /*9f: shl    $0x20,%rdx */
0x48, 0xc1, 0xe8, 0x02,                   /*a3: shr    $0x2,%rax */
0x48, 0x09, 0xd0,                         /*a7: or     %rdx,%rax */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*aa: mov    %rax,0xab0800(%rcx) */
0x8b, 0x43, 0x48,                         /*b1: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*b4: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*b8: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*be: mov    %rbx,%rdi */
0x5b,                                     /*c1: pop    %rbx */

};
static uint8_t op_class__rodata[] = {

};

static void op_class_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = b * 4 + 0;
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 28)) = a * 8 + 8;
  *((int32_t *)(op + 173)) = a * 8 + 0;
}

static void op_class_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_class_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 14..17]], "a"=>[[8, 0, 21..24], [8, 0, 163..166]]} */
static uint8_t op_module__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*8: mov    0x28(%rbx),%rcx */
0x8b, 0x91, 0x00, 0x04, 0xbc, 0x00,       /*c: mov    0xbc0400(%rcx),%edx */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xab, 0x00, /*12: mov    0xab0800(%rax),%rsi */
0x48, 0x8b, 0x7b, 0x50,                   /*19: mov    0x50(%rbx),%rdi */
0x85, 0xf6,                               /*1d: test   %esi,%esi */
0x75, 0x4d,                               /*1f: jne    6e <op_module+0x6e> */
0x48, 0x89, 0xf0,                         /*21: mov    %rsi,%rax */
0x48, 0xc1, 0xe8, 0x20,                   /*24: shr    $0x20,%rax */
0x3d, 0x01, 0x00, 0xf0, 0xff,             /*28: cmp    $0xfff00001,%eax */
0x72, 0x3f,                               /*2d: jb     6e <op_module+0x6e> */
0x25, 0x00, 0xc0, 0x0f, 0x00,             /*2f: and    $0xfc000,%eax */
0x3d, 0x00, 0x40, 0x00, 0x00,             /*34: cmp    $0x4000,%eax */
0x75, 0x33,                               /*39: jne    6e <op_module+0x6e> */
0x48, 0x8b, 0x47, 0x18,                   /*3b: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*3f: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x70, 0x48,                   /*43: mov    0x48(%rax),%rsi */
0x8b, 0x06,                               /*47: mov    (%rsi),%eax */
0x0f, 0xb6, 0xc8,                         /*49: movzbl %al,%ecx */
0x83, 0xf9, 0x05,                         /*4c: cmp    $0x5,%ecx */
0x77, 0x05,                               /*4f: ja     56 <op_module+0x56> */
0x83, 0xf9, 0x01,                         /*51: cmp    $0x1,%ecx */
0x75, 0x18,                               /*54: jne    6e <op_module+0x6e> */
0xc1, 0xe0, 0x0e,                         /*56: shl    $0xe,%eax */
0x05, 0x00, 0x40, 0x00, 0x00,             /*59: add    $0x4000,%eax */
0x0d, 0x00, 0x00, 0xf0, 0xff,             /*5e: or     $0xfff00000,%eax */
0x48, 0xc1, 0xe0, 0x20,                   /*63: shl    $0x20,%rax */
0x48, 0xc1, 0xee, 0x02,                   /*67: shr    $0x2,%rsi */
0x48, 0x09, 0xc6,                         /*6b: or     %rax,%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*6e: callq  73 <op_module+0x73> */
0x48, 0x8b, 0x4b, 0x18,                   /*73: mov    0x18(%rbx),%rcx */
0x8b, 0x10,                               /*77: mov    (%rax),%edx */
0x0f, 0xb6, 0xf2,                         /*79: movzbl %dl,%esi */
0x83, 0xfe, 0x05,                         /*7c: cmp    $0x5,%esi */
0x77, 0x05,                               /*7f: ja     86 <op_module+0x86> */
0x83, 0xfe, 0x01,                         /*81: cmp    $0x1,%esi */
0x75, 0x1a,                               /*84: jne    a0 <op_module+0xa0> */
0xc1, 0xe2, 0x0e,                         /*86: shl    $0xe,%edx */
0x81, 0xc2, 0x00, 0x40, 0x00, 0x00,       /*89: add    $0x4000,%edx */
0x81, 0xca, 0x00, 0x00, 0xf0, 0xff,       /*8f: or     $0xfff00000,%edx */
0x48, 0xc1, 0xe2, 0x20,                   /*95: shl    $0x20,%rdx */
0x48, 0xc1, 0xe8, 0x02,                   /*99: shr    $0x2,%rax */
0x48, 0x09, 0xd0,                         /*9d: or     %rdx,%rax */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*a0: mov    %rax,0xab0800(%rcx) */
0x8b, 0x43, 0x48,                         /*a7: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*aa: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*ae: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*b4: mov    %rbx,%rdi */
0x5b,                                     /*b7: pop    %rbx */

};
static uint8_t op_module__rodata[] = {

};

static void op_module_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = b * 4 + 0;
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 163)) = a * 8 + 0;
}

static void op_module_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_module_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 21..24], [1, 0, 48..51], [8, 0, 118..121]], "b"=>[[8, 0, 137..140]]} */
static uint8_t op_exec__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x41, 0x54,                               /*4: push   %r12 */
0x53,                                     /*6: push   %rbx */
0x49, 0x89, 0xfe,                         /*7: mov    %rdi,%r14 */
0x49, 0x8b, 0x46, 0x18,                   /*a: mov    0x18(%r14),%rax */
0x49, 0x8b, 0x7e, 0x50,                   /*e: mov    0x50(%r14),%rdi */
0x4c, 0x8b, 0xb8, 0x00, 0x08, 0xab, 0x00, /*12: mov    0xab0800(%rax),%r15 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*19: callq  1e <op_exec+0x1e> */
0x48, 0x89, 0xc3,                         /*1e: mov    %rax,%rbx */
0x49, 0x8b, 0x46, 0x10,                   /*21: mov    0x10(%r14),%rax */
0x48, 0x83, 0xc0, 0x04,                   /*25: add    $0x4,%rax */
0x48, 0x89, 0x43, 0x30,                   /*29: mov    %rax,0x30(%rbx) */
0xc7, 0x43, 0x44, 0x00, 0x00, 0xab, 0x00, /*2d: movl   $0xab0000,0x44(%rbx) */
0xc7, 0x03, 0x00, 0x00, 0x00, 0x00,       /*34: movl   $0x0,(%rbx) */
0x49, 0x8b, 0x46, 0x50,                   /*3a: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*3e: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*42: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x10,                   /*46: mov    %rax,0x10(%rbx) */
0xc7, 0x43, 0x40, 0x00, 0x00, 0x00, 0x00, /*4a: movl   $0x0,0x40(%rbx) */
0x4a, 0x8d, 0x04, 0xbd, 0x00, 0x00, 0x00, 0x00,/*51: lea    0x0(,%r15,4),%rax */
0x48, 0xb9, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*59: movabs $0xfffffffffffc,%rcx */
0x48, 0x21, 0xc1,                         /*63: and    %rax,%rcx */
0x48, 0x89, 0x4b, 0x48,                   /*66: mov    %rcx,0x48(%rbx) */
0x49, 0x8b, 0x46, 0x50,                   /*6a: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*6e: mov    0x18(%rax),%rax */
0x48, 0x81, 0x40, 0x08, 0x00, 0x08, 0xab, 0x00,/*72: addq   $0xab0800,0x8(%rax) */
0x49, 0x8b, 0x46, 0x08,                   /*7a: mov    0x8(%r14),%rax */
0x49, 0x8b, 0x7e, 0x50,                   /*7e: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x40, 0x20,                   /*82: mov    0x20(%rax),%rax */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*86: mov    0xbc0800(%rax),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*8d: callq  92 <op_exec+0x92> */
0x49, 0x89, 0xc4,                         /*92: mov    %rax,%r12 */
0x48, 0x8b, 0x43, 0x48,                   /*95: mov    0x48(%rbx),%rax */
0x49, 0x89, 0x44, 0x24, 0x20,             /*99: mov    %rax,0x20(%r12) */
0x4c, 0x89, 0x63, 0x08,                   /*9e: mov    %r12,0x8(%rbx) */
0x41, 0xf6, 0x44, 0x24, 0x02, 0x04,       /*a2: testb  $0x4,0x2(%r12) */
0x74, 0x40,                               /*a8: je     ea <op_exec+0xea> */
0xc7, 0x43, 0x18, 0x00, 0x00, 0x00, 0x00, /*aa: movl   $0x0,0x18(%rbx) */
0x49, 0x8b, 0x7e, 0x50,                   /*b1: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*b5: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x58, 0x08,                   /*b9: mov    0x8(%rax),%rbx */
0x4c, 0x89, 0xfe,                         /*bd: mov    %r15,%rsi */
0x41, 0xff, 0x54, 0x24, 0x18,             /*c0: callq  *0x18(%r12) */
0x48, 0x89, 0x03,                         /*c5: mov    %rax,(%rbx) */
0x49, 0x8b, 0x7e, 0x50,                   /*c8: mov    0x50(%r14),%rdi */
0x41, 0x8b, 0x76, 0x48,                   /*cc: mov    0x48(%r14),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*d0: callq  d5 <op_exec+0xd5> */
0x49, 0x8b, 0x46, 0x50,                   /*d5: mov    0x50(%r14),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*d9: cmpq   $0x0,0x28(%rax) */
0x74, 0x69,                               /*de: je     149 <op_exec+0x149> */
0x4c, 0x89, 0xf7,                         /*e0: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*e3: callq  e8 <op_exec+0xe8> */
0xeb, 0x7c,                               /*e8: jmp    166 <op_exec+0x166> */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*ea: mov    0x18(%r12),%rax */
0x49, 0x89, 0x46, 0x08,                   /*ef: mov    %rax,0x8(%r14) */
0x48, 0x8b, 0x48, 0x10,                   /*f3: mov    0x10(%rax),%rcx */
0x49, 0x89, 0x4e, 0x20,                   /*f7: mov    %rcx,0x20(%r14) */
0x48, 0x8b, 0x48, 0x18,                   /*fb: mov    0x18(%rax),%rcx */
0x49, 0x89, 0x4e, 0x28,                   /*ff: mov    %rcx,0x28(%r14) */
0x49, 0x8b, 0x7e, 0x50,                   /*103: mov    0x50(%r14),%rdi */
0x0f, 0xb7, 0x70, 0x02,                   /*107: movzwl 0x2(%rax),%esi */
0xba, 0x01, 0x00, 0x00, 0x00,             /*10b: mov    $0x1,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*110: callq  115 <op_exec+0x115> */
0x49, 0x8b, 0x46, 0x08,                   /*115: mov    0x8(%r14),%rax */
0x0f, 0xb7, 0x40, 0x02,                   /*119: movzwl 0x2(%rax),%eax */
0x89, 0x43, 0x18,                         /*11d: mov    %eax,0x18(%rbx) */
0x49, 0x8b, 0x7e, 0x50,                   /*120: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*124: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*128: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x18,                   /*12c: mov    %rax,0x18(%r14) */
0x49, 0x8b, 0x46, 0x08,                   /*130: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*134: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x10,                   /*138: mov    %rax,0x10(%r14) */
0x4c, 0x89, 0xe6,                         /*13c: mov    %r12,%rsi */
0x4c, 0x89, 0xf2,                         /*13f: mov    %r14,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*142: callq  147 <op_exec+0x147> */
0xeb, 0x1d,                               /*147: jmp    166 <op_exec+0x166> */
0x48, 0x8b, 0x40, 0x18,                   /*149: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*14d: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x49, 0x10,                   /*151: mov    0x10(%rcx),%rcx */
0x48, 0x89, 0x48, 0x08,                   /*155: mov    %rcx,0x8(%rax) */
0x49, 0x89, 0x4e, 0x18,                   /*159: mov    %rcx,0x18(%r14) */
0x49, 0x8b, 0x7e, 0x50,                   /*15d: mov    0x50(%r14),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*161: callq  166 <op_exec+0x166> */
0x4c, 0x89, 0xf7,                         /*166: mov    %r14,%rdi */
0x5b,                                     /*169: pop    %rbx */
0x41, 0x5c,                               /*16a: pop    %r12 */
0x41, 0x5e,                               /*16c: pop    %r14 */
0x41, 0x5f,                               /*16e: pop    %r15 */

};
static uint8_t op_exec__rodata[] = {

};

static void op_exec_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 48)) = a * 1 + 0;
  *((int32_t *)(op + 118)) = a * 8 + 0;
  *((int32_t *)(op + 137)) = b * 8 + 0;
}

static void op_exec_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_exec_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18], [8, 8, 22..25]], "b"=>[[4, 0, 49..52]]} */
static uint8_t op_method__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x53, 0x28,                   /*8: mov    0x28(%rbx),%rdx */
0x48, 0x8b, 0xb8, 0x00, 0x08, 0xab, 0x00, /*c: mov    0xab0800(%rax),%rdi */
0x48, 0x8b, 0x88, 0x08, 0x08, 0xab, 0x00, /*13: mov    0xab0808(%rax),%rcx */
0x48, 0xc1, 0xe7, 0x02,                   /*1a: shl    $0x2,%rdi */
0x48, 0xbe, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*1e: movabs $0xfffffffffffc,%rsi */
0x48, 0x21, 0xfe,                         /*28: and    %rdi,%rsi */
0x48, 0x8b, 0x7b, 0x50,                   /*2b: mov    0x50(%rbx),%rdi */
0x8b, 0x92, 0x00, 0x04, 0xbc, 0x00,       /*2f: mov    0xbc0400(%rdx),%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*35: callq  3a <op_method+0x3a> */
0x8b, 0x43, 0x48,                         /*3a: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*3d: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*41: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*47: mov    %rbx,%rdi */
0x5b,                                     /*4a: pop    %rbx */

};
static uint8_t op_method__rodata[] = {

};

static void op_method_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
  *((int32_t *)(op + 22)) = a * 8 + 8;
  *((int32_t *)(op + 49)) = b * 4 + 0;
}

static void op_method_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_method_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[8, 0, 17..20]], "a"=>[[8, 0, 29..32]]} */
static uint8_t op_sclass__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x50,                   /*a: mov    0x50(%rbx),%rdi */
0x49, 0x8b, 0xb6, 0x00, 0x08, 0xbc, 0x00, /*e: mov    0xbc0800(%r14),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*15: callq  1a <op_sclass+0x1a> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*1a: mov    %rax,0xab0800(%r14) */
0x8b, 0x43, 0x48,                         /*21: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*24: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*28: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*2e: mov    %rbx,%rdi */
0x5b,                                     /*31: pop    %rbx */
0x41, 0x5e,                               /*32: pop    %r14 */

};
static uint8_t op_sclass__rodata[] = {

};

static void op_sclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 8 + 0;
  *((int32_t *)(op + 29)) = a * 8 + 0;
}

static void op_sclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sclass_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 77..80]]} */
static uint8_t op_tclass__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x49, 0x89, 0xfe,                         /*5: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x50,                   /*8: mov    0x50(%r14),%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*c: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*10: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x48,                   /*14: mov    0x48(%rax),%rax */
0x48, 0x85, 0xc0,                         /*18: test   %rax,%rax */
0x74, 0x36,                               /*1b: je     53 <op_tclass+0x53> */
0x49, 0x8b, 0x4e, 0x18,                   /*1d: mov    0x18(%r14),%rcx */
0x8b, 0x10,                               /*21: mov    (%rax),%edx */
0x0f, 0xb6, 0xf2,                         /*23: movzbl %dl,%esi */
0x83, 0xfe, 0x05,                         /*26: cmp    $0x5,%esi */
0x77, 0x05,                               /*29: ja     30 <op_tclass+0x30> */
0x83, 0xfe, 0x01,                         /*2b: cmp    $0x1,%esi */
0x75, 0x1a,                               /*2e: jne    4a <op_tclass+0x4a> */
0xc1, 0xe2, 0x0e,                         /*30: shl    $0xe,%edx */
0x81, 0xc2, 0x00, 0x40, 0x00, 0x00,       /*33: add    $0x4000,%edx */
0x81, 0xca, 0x00, 0x00, 0xf0, 0xff,       /*39: or     $0xfff00000,%edx */
0x48, 0xc1, 0xe2, 0x20,                   /*3f: shl    $0x20,%rdx */
0x48, 0xc1, 0xe8, 0x02,                   /*43: shr    $0x2,%rax */
0x48, 0x09, 0xd0,                         /*47: or     %rdx,%rax */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*4a: mov    %rax,0xab0800(%rcx) */
0xeb, 0x53,                               /*51: jmp    a6 <op_tclass+0xa6> */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*53: mov    0x0(%rip),%rsi        # 5a <op_tclass+0x5a> */
0x48, 0x89, 0xdf,                         /*5a: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*5d: callq  62 <op_tclass+0x62> */
0x49, 0x89, 0xc7,                         /*62: mov    %rax,%r15 */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*65: mov    $0x0,%esi */
0xba, 0x19, 0x00, 0x00, 0x00,             /*6a: mov    $0x19,%edx */
0x48, 0x89, 0xdf,                         /*6f: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*72: callq  77 <op_tclass+0x77> */
0x48, 0x89, 0xdf,                         /*77: mov    %rbx,%rdi */
0x4c, 0x89, 0xfe,                         /*7a: mov    %r15,%rsi */
0x48, 0x89, 0xc2,                         /*7d: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*80: callq  85 <op_tclass+0x85> */
0x48, 0xc1, 0xe0, 0x02,                   /*85: shl    $0x2,%rax */
0x48, 0xb9, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*89: movabs $0xfffffffffffc,%rcx */
0x48, 0x21, 0xc1,                         /*93: and    %rax,%rcx */
0x49, 0x8b, 0x46, 0x50,                   /*96: mov    0x50(%r14),%rax */
0x48, 0x89, 0x48, 0x28,                   /*9a: mov    %rcx,0x28(%rax) */
0x4c, 0x89, 0xf7,                         /*9e: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*a1: callq  a6 <op_tclass+0xa6> */
0x4c, 0x89, 0xf7,                         /*a6: mov    %r14,%rdi */
0x5b,                                     /*a9: pop    %rbx */
0x41, 0x5e,                               /*aa: pop    %r14 */
0x41, 0x5f,                               /*ac: pop    %r15 */

};
static uint8_t op_tclass__rodata[] = {
0x6e, 0x6f, 0x20, 0x74, 0x61, 0x72, 0x67, 0x65, 0x74, 0x20, 0x63, 0x6c, 0x61, 0x73, 0x73, 0x20,
0x6f, 0x72, 0x20, 0x6d, 0x6f, 0x64, 0x75, 0x6c, 0x65, 0x00,

};

static void op_tclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 77)) = a * 8 + 0;
}

static void op_tclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_tclass_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 10..13]], "b"=>[[1, 0, 15..18]], "c"=>[[1, 0, 20..23]]} */
static uint8_t op_debug__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xbf, 0x00, 0x00, 0x00, 0x00,             /*4: mov    $0x0,%edi */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*9: mov    $0xab0000,%esi */
0xba, 0x00, 0x00, 0xbc, 0x00,             /*e: mov    $0xbc0000,%edx */
0xb9, 0x00, 0x00, 0xcd, 0x00,             /*13: mov    $0xcd0000,%ecx */
0x31, 0xc0,                               /*18: xor    %eax,%eax */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1a: callq  1f <op_debug+0x1f> */
0x48, 0x89, 0xdf,                         /*1f: mov    %rbx,%rdi */
0x5b,                                     /*22: pop    %rbx */

};
static uint8_t op_debug__rodata[] = {
0x4f, 0x50, 0x5f, 0x44, 0x45, 0x42, 0x55, 0x47, 0x20, 0x25, 0x64, 0x20, 0x25, 0x64, 0x20, 0x25,
0x64, 0x0a, 0x00,                         

};

static void op_debug_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 1 + 0;
  *((int32_t *)(op + 15)) = b * 1 + 0;
  *((int32_t *)(op + 20)) = c * 1 + 0;
}

static void op_debug_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_debug_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_stop__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*4: callq  9 <op_stop+0x9> */
0x48, 0x89, 0xdf,                         /*9: mov    %rbx,%rdi */
0x5b,                                     /*c: pop    %rbx */

};
static uint8_t op_stop__rodata[] = {

};

static void op_stop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_stop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_stop_set_args(op, 0,0,0,op_idx);
}


/* args: {"b"=>[[8, 0, 19..22]]} */
static uint8_t op_err__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x48, 0x89, 0xfb,                         /*5: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x20,                   /*8: mov    0x20(%rbx),%rax */
0x4c, 0x8b, 0x73, 0x50,                   /*c: mov    0x50(%rbx),%r14 */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*10: mov    0xbc0800(%rax),%rsi */
0x4c, 0x89, 0xf7,                         /*17: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1a: callq  1f <op_err+0x1f> */
0x49, 0x89, 0xc7,                         /*1f: mov    %rax,%r15 */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*22: mov    0x0(%rip),%rsi        # 29 <op_err+0x29> */
0x4c, 0x89, 0xf7,                         /*29: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2c: callq  31 <op_err+0x31> */
0x4c, 0x89, 0xf7,                         /*31: mov    %r14,%rdi */
0x48, 0x89, 0xc6,                         /*34: mov    %rax,%rsi */
0x4c, 0x89, 0xfa,                         /*37: mov    %r15,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*3a: callq  3f <op_err+0x3f> */
0x48, 0xc1, 0xe0, 0x02,                   /*3f: shl    $0x2,%rax */
0x48, 0xb9, 0xfc, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00,/*43: movabs $0xfffffffffffc,%rcx */
0x48, 0x21, 0xc1,                         /*4d: and    %rax,%rcx */
0x48, 0x8b, 0x43, 0x50,                   /*50: mov    0x50(%rbx),%rax */
0x48, 0x89, 0x48, 0x28,                   /*54: mov    %rcx,0x28(%rax) */
0x48, 0x89, 0xdf,                         /*58: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*5b: callq  60 <op_err+0x60> */
0x48, 0x89, 0xdf,                         /*60: mov    %rbx,%rdi */
0x5b,                                     /*63: pop    %rbx */
0x41, 0x5e,                               /*64: pop    %r14 */
0x41, 0x5f,                               /*66: pop    %r15 */

};
static uint8_t op_err__rodata[] = {

};

static void op_err_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = b * 8 + 0;
}

static void op_err_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_err_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}

typedef void (*jit_args_func_t)(uint8_t *op, mrb_code c, uint32_t op_idx);
typedef void (*jit_link_func_t)(uint8_t *op, uint8_t *data);
static jit_args_func_t arg_funcs[78];
extern jit_link_func_t link_funcs[];
uint8_t* ops_text[78];
uint8_t* ops_rodata[78];
static char *op_names[78];

static size_t op_sizes_text[] = {
  sizeof(op_nop__text), /* 0 */
  sizeof(op_move__text), /* 20 */
  sizeof(op_loadl__text), /* 24 */
  sizeof(op_loadi__text), /* 28 */
  sizeof(op_loadsym__text), /* 34 */
  sizeof(op_loadnil__text), /* 28 */
  sizeof(op_loadself__text), /* 16 */
  sizeof(op_loadt__text), /* 28 */
  sizeof(op_loadf__text), /* 28 */
  sizeof(op_getglobal__text), /* 42 */
  sizeof(op_setglobal__text), /* 38 */
  sizeof(op_getspecial__text), /* 37 */
  sizeof(op_setspecial__text), /* 33 */
  sizeof(op_getiv__text), /* 42 */
  sizeof(op_setiv__text), /* 38 */
  sizeof(op_getcv__text), /* 82 */
  sizeof(op_setcv__text), /* 38 */
  sizeof(op_getconst__text), /* 90 */
  sizeof(op_setconst__text), /* 38 */
  sizeof(op_getmcnst__text), /* 101 */
  sizeof(op_setmcnst__text), /* 45 */
  sizeof(op_getupvar__text), /* 111 */
  sizeof(op_setupvar__text), /* 96 */
  sizeof(op_jmp__text), /* 0 */
  sizeof(op_jmpif__text), /* 48 */
  sizeof(op_jmpnot__text), /* 48 */
  sizeof(op_onerr__text), /* 145 */
  sizeof(op_rescue__text), /* 118 */
  sizeof(op_poperr__text), /* 35 */
  sizeof(op_raise__text), /* 49 */
  sizeof(op_epush__text), /* 167 */
  sizeof(op_epop__text), /* 84 */
  sizeof(op_send__text), /* 111 */
  sizeof(op_sendb__text), /* 56 */
  sizeof(op_fsend__text), /* 0 */
  sizeof(op_call__text), /* 447 */
  sizeof(op_super__text), /* 650 */
  sizeof(op_argary__text), /* 720 */
  sizeof(op_enter__text), /* 1570 */
  sizeof(op_enter_method_m__text), /* 458 */
  sizeof(op_karg__text), /* 0 */
  sizeof(op_kdict__text), /* 0 */
  sizeof(op_return__text), /* 38 */
  sizeof(op_break__text), /* 24 */
  sizeof(op_tailcall__text), /* 717 */
  sizeof(op_blkpush__text), /* 197 */
  sizeof(op_add__text), /* 343 */
  sizeof(op_addi__text), /* 199 */
  sizeof(op_sub__text), /* 321 */
  sizeof(op_subi__text), /* 203 */
  sizeof(op_mul__text), /* 344 */
  sizeof(op_div__text), /* 314 */
  sizeof(op_eq__text), /* 305 */
  sizeof(op_lt__text), /* 269 */
  sizeof(op_le__text), /* 269 */
  sizeof(op_gt__text), /* 269 */
  sizeof(op_ge__text), /* 269 */
  sizeof(op_array__text), /* 57 */
  sizeof(op_arycat__text), /* 70 */
  sizeof(op_arypush__text), /* 35 */
  sizeof(op_aref__text), /* 97 */
  sizeof(op_aset__text), /* 40 */
  sizeof(op_apost__text), /* 448 */
  sizeof(op_string__text), /* 56 */
  sizeof(op_strcat__text), /* 35 */
  sizeof(op_hash__text), /* 150 */
  sizeof(op_lambda__text), /* 28 */
  sizeof(op_range__text), /* 64 */
  sizeof(op_oclass__text), /* 60 */
  sizeof(op_class__text), /* 194 */
  sizeof(op_module__text), /* 184 */
  sizeof(op_exec__text), /* 368 */
  sizeof(op_method__text), /* 75 */
  sizeof(op_sclass__text), /* 52 */
  sizeof(op_tclass__text), /* 174 */
  sizeof(op_debug__text), /* 35 */
  sizeof(op_stop__text), /* 13 */
  sizeof(op_err__text), /* 104 */

};
static size_t op_sizes_rodata[] = {
  sizeof(op_nop__rodata), /* 0 */
  sizeof(op_move__rodata), /* 0 */
  sizeof(op_loadl__rodata), /* 0 */
  sizeof(op_loadi__rodata), /* 0 */
  sizeof(op_loadsym__rodata), /* 0 */
  sizeof(op_loadnil__rodata), /* 0 */
  sizeof(op_loadself__rodata), /* 0 */
  sizeof(op_loadt__rodata), /* 0 */
  sizeof(op_loadf__rodata), /* 0 */
  sizeof(op_getglobal__rodata), /* 0 */
  sizeof(op_setglobal__rodata), /* 0 */
  sizeof(op_getspecial__rodata), /* 0 */
  sizeof(op_setspecial__rodata), /* 0 */
  sizeof(op_getiv__rodata), /* 0 */
  sizeof(op_setiv__rodata), /* 0 */
  sizeof(op_getcv__rodata), /* 0 */
  sizeof(op_setcv__rodata), /* 0 */
  sizeof(op_getconst__rodata), /* 0 */
  sizeof(op_setconst__rodata), /* 0 */
  sizeof(op_getmcnst__rodata), /* 0 */
  sizeof(op_setmcnst__rodata), /* 0 */
  sizeof(op_getupvar__rodata), /* 0 */
  sizeof(op_setupvar__rodata), /* 0 */
  sizeof(op_jmp__rodata), /* 0 */
  sizeof(op_jmpif__rodata), /* 0 */
  sizeof(op_jmpnot__rodata), /* 0 */
  sizeof(op_onerr__rodata), /* 0 */
  sizeof(op_rescue__rodata), /* 0 */
  sizeof(op_poperr__rodata), /* 0 */
  sizeof(op_raise__rodata), /* 0 */
  sizeof(op_epush__rodata), /* 0 */
  sizeof(op_epop__rodata), /* 0 */
  sizeof(op_send__rodata), /* 0 */
  sizeof(op_sendb__rodata), /* 0 */
  sizeof(op_fsend__rodata), /* 0 */
  sizeof(op_call__rodata), /* 0 */
  sizeof(op_super__rodata), /* 46 */
  sizeof(op_argary__rodata), /* 31 */
  sizeof(op_enter__rodata), /* 35 */
  sizeof(op_enter_method_m__rodata), /* 13 */
  sizeof(op_karg__rodata), /* 0 */
  sizeof(op_kdict__rodata), /* 0 */
  sizeof(op_return__rodata), /* 0 */
  sizeof(op_break__rodata), /* 0 */
  sizeof(op_tailcall__rodata), /* 79 */
  sizeof(op_blkpush__rodata), /* 0 */
  sizeof(op_add__rodata), /* 0 */
  sizeof(op_addi__rodata), /* 0 */
  sizeof(op_sub__rodata), /* 0 */
  sizeof(op_subi__rodata), /* 0 */
  sizeof(op_mul__rodata), /* 0 */
  sizeof(op_div__rodata), /* 0 */
  sizeof(op_eq__rodata), /* 0 */
  sizeof(op_lt__rodata), /* 0 */
  sizeof(op_le__rodata), /* 0 */
  sizeof(op_gt__rodata), /* 0 */
  sizeof(op_ge__rodata), /* 0 */
  sizeof(op_array__rodata), /* 0 */
  sizeof(op_arycat__rodata), /* 0 */
  sizeof(op_arypush__rodata), /* 0 */
  sizeof(op_aref__rodata), /* 0 */
  sizeof(op_aset__rodata), /* 0 */
  sizeof(op_apost__rodata), /* 0 */
  sizeof(op_string__rodata), /* 0 */
  sizeof(op_strcat__rodata), /* 0 */
  sizeof(op_hash__rodata), /* 0 */
  sizeof(op_lambda__rodata), /* 0 */
  sizeof(op_range__rodata), /* 0 */
  sizeof(op_oclass__rodata), /* 0 */
  sizeof(op_class__rodata), /* 0 */
  sizeof(op_module__rodata), /* 0 */
  sizeof(op_exec__rodata), /* 0 */
  sizeof(op_method__rodata), /* 0 */
  sizeof(op_sclass__rodata), /* 0 */
  sizeof(op_tclass__rodata), /* 26 */
  sizeof(op_debug__rodata), /* 19 */
  sizeof(op_stop__rodata), /* 0 */
  sizeof(op_err__rodata), /* 0 */

};
static int8_t op_algn_text[] = {
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,
  16,

};
static int8_t op_algn_rodata[] = {
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  16,
  16,
  16,
  1,
  -1,
  -1,
  -1,
  -1,
  8,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  16,
  16,
  -1,
  -1,

};

extern void init_linker();
void init_ops() {
  static int init = 0;
  if(init == 0) {
    init = 1;
    init_linker();
    ops_text[0] = op_nop__text;
    ops_rodata[0] = op_nop__rodata;
    op_names[0] = "op_nop";
    arg_funcs[0] = op_nop_set_args_from_code;
    ops_text[1] = op_move__text;
    ops_rodata[1] = op_move__rodata;
    op_names[1] = "op_move";
    arg_funcs[1] = op_move_set_args_from_code;
    ops_text[2] = op_loadl__text;
    ops_rodata[2] = op_loadl__rodata;
    op_names[2] = "op_loadl";
    arg_funcs[2] = op_loadl_set_args_from_code;
    ops_text[3] = op_loadi__text;
    ops_rodata[3] = op_loadi__rodata;
    op_names[3] = "op_loadi";
    arg_funcs[3] = op_loadi_set_args_from_code;
    ops_text[4] = op_loadsym__text;
    ops_rodata[4] = op_loadsym__rodata;
    op_names[4] = "op_loadsym";
    arg_funcs[4] = op_loadsym_set_args_from_code;
    ops_text[5] = op_loadnil__text;
    ops_rodata[5] = op_loadnil__rodata;
    op_names[5] = "op_loadnil";
    arg_funcs[5] = op_loadnil_set_args_from_code;
    ops_text[6] = op_loadself__text;
    ops_rodata[6] = op_loadself__rodata;
    op_names[6] = "op_loadself";
    arg_funcs[6] = op_loadself_set_args_from_code;
    ops_text[7] = op_loadt__text;
    ops_rodata[7] = op_loadt__rodata;
    op_names[7] = "op_loadt";
    arg_funcs[7] = op_loadt_set_args_from_code;
    ops_text[8] = op_loadf__text;
    ops_rodata[8] = op_loadf__rodata;
    op_names[8] = "op_loadf";
    arg_funcs[8] = op_loadf_set_args_from_code;
    ops_text[9] = op_getglobal__text;
    ops_rodata[9] = op_getglobal__rodata;
    op_names[9] = "op_getglobal";
    arg_funcs[9] = op_getglobal_set_args_from_code;
    ops_text[10] = op_setglobal__text;
    ops_rodata[10] = op_setglobal__rodata;
    op_names[10] = "op_setglobal";
    arg_funcs[10] = op_setglobal_set_args_from_code;
    ops_text[11] = op_getspecial__text;
    ops_rodata[11] = op_getspecial__rodata;
    op_names[11] = "op_getspecial";
    arg_funcs[11] = op_getspecial_set_args_from_code;
    ops_text[12] = op_setspecial__text;
    ops_rodata[12] = op_setspecial__rodata;
    op_names[12] = "op_setspecial";
    arg_funcs[12] = op_setspecial_set_args_from_code;
    ops_text[13] = op_getiv__text;
    ops_rodata[13] = op_getiv__rodata;
    op_names[13] = "op_getiv";
    arg_funcs[13] = op_getiv_set_args_from_code;
    ops_text[14] = op_setiv__text;
    ops_rodata[14] = op_setiv__rodata;
    op_names[14] = "op_setiv";
    arg_funcs[14] = op_setiv_set_args_from_code;
    ops_text[15] = op_getcv__text;
    ops_rodata[15] = op_getcv__rodata;
    op_names[15] = "op_getcv";
    arg_funcs[15] = op_getcv_set_args_from_code;
    ops_text[16] = op_setcv__text;
    ops_rodata[16] = op_setcv__rodata;
    op_names[16] = "op_setcv";
    arg_funcs[16] = op_setcv_set_args_from_code;
    ops_text[17] = op_getconst__text;
    ops_rodata[17] = op_getconst__rodata;
    op_names[17] = "op_getconst";
    arg_funcs[17] = op_getconst_set_args_from_code;
    ops_text[18] = op_setconst__text;
    ops_rodata[18] = op_setconst__rodata;
    op_names[18] = "op_setconst";
    arg_funcs[18] = op_setconst_set_args_from_code;
    ops_text[19] = op_getmcnst__text;
    ops_rodata[19] = op_getmcnst__rodata;
    op_names[19] = "op_getmcnst";
    arg_funcs[19] = op_getmcnst_set_args_from_code;
    ops_text[20] = op_setmcnst__text;
    ops_rodata[20] = op_setmcnst__rodata;
    op_names[20] = "op_setmcnst";
    arg_funcs[20] = op_setmcnst_set_args_from_code;
    ops_text[21] = op_getupvar__text;
    ops_rodata[21] = op_getupvar__rodata;
    op_names[21] = "op_getupvar";
    arg_funcs[21] = op_getupvar_set_args_from_code;
    ops_text[22] = op_setupvar__text;
    ops_rodata[22] = op_setupvar__rodata;
    op_names[22] = "op_setupvar";
    arg_funcs[22] = op_setupvar_set_args_from_code;
    ops_text[23] = op_jmp__text;
    ops_rodata[23] = op_jmp__rodata;
    op_names[23] = "op_jmp";
    arg_funcs[23] = op_jmp_set_args_from_code;
    ops_text[24] = op_jmpif__text;
    ops_rodata[24] = op_jmpif__rodata;
    op_names[24] = "op_jmpif";
    arg_funcs[24] = op_jmpif_set_args_from_code;
    ops_text[25] = op_jmpnot__text;
    ops_rodata[25] = op_jmpnot__rodata;
    op_names[25] = "op_jmpnot";
    arg_funcs[25] = op_jmpnot_set_args_from_code;
    ops_text[26] = op_onerr__text;
    ops_rodata[26] = op_onerr__rodata;
    op_names[26] = "op_onerr";
    arg_funcs[26] = op_onerr_set_args_from_code;
    ops_text[27] = op_rescue__text;
    ops_rodata[27] = op_rescue__rodata;
    op_names[27] = "op_rescue";
    arg_funcs[27] = op_rescue_set_args_from_code;
    ops_text[28] = op_poperr__text;
    ops_rodata[28] = op_poperr__rodata;
    op_names[28] = "op_poperr";
    arg_funcs[28] = op_poperr_set_args_from_code;
    ops_text[29] = op_raise__text;
    ops_rodata[29] = op_raise__rodata;
    op_names[29] = "op_raise";
    arg_funcs[29] = op_raise_set_args_from_code;
    ops_text[30] = op_epush__text;
    ops_rodata[30] = op_epush__rodata;
    op_names[30] = "op_epush";
    arg_funcs[30] = op_epush_set_args_from_code;
    ops_text[31] = op_epop__text;
    ops_rodata[31] = op_epop__rodata;
    op_names[31] = "op_epop";
    arg_funcs[31] = op_epop_set_args_from_code;
    ops_text[32] = op_send__text;
    ops_rodata[32] = op_send__rodata;
    op_names[32] = "op_send";
    arg_funcs[32] = op_send_set_args_from_code;
    ops_text[33] = op_sendb__text;
    ops_rodata[33] = op_sendb__rodata;
    op_names[33] = "op_sendb";
    arg_funcs[33] = op_sendb_set_args_from_code;
    ops_text[34] = op_fsend__text;
    ops_rodata[34] = op_fsend__rodata;
    op_names[34] = "op_fsend";
    arg_funcs[34] = op_fsend_set_args_from_code;
    ops_text[35] = op_call__text;
    ops_rodata[35] = op_call__rodata;
    op_names[35] = "op_call";
    arg_funcs[35] = op_call_set_args_from_code;
    ops_text[36] = op_super__text;
    ops_rodata[36] = op_super__rodata;
    op_names[36] = "op_super";
    arg_funcs[36] = op_super_set_args_from_code;
    ops_text[37] = op_argary__text;
    ops_rodata[37] = op_argary__rodata;
    op_names[37] = "op_argary";
    arg_funcs[37] = op_argary_set_args_from_code;
    ops_text[38] = op_enter__text;
    ops_rodata[38] = op_enter__rodata;
    op_names[38] = "op_enter";
    arg_funcs[38] = op_enter_set_args_from_code;
    ops_text[39] = op_enter_method_m__text;
    ops_rodata[39] = op_enter_method_m__rodata;
    op_names[39] = "op_enter_method_m";
    arg_funcs[39] = op_enter_method_m_set_args_from_code;
    ops_text[40] = op_karg__text;
    ops_rodata[40] = op_karg__rodata;
    op_names[40] = "op_karg";
    arg_funcs[40] = op_karg_set_args_from_code;
    ops_text[41] = op_kdict__text;
    ops_rodata[41] = op_kdict__rodata;
    op_names[41] = "op_kdict";
    arg_funcs[41] = op_kdict_set_args_from_code;
    ops_text[42] = op_return__text;
    ops_rodata[42] = op_return__rodata;
    op_names[42] = "op_return";
    arg_funcs[42] = op_return_set_args_from_code;
    ops_text[43] = op_break__text;
    ops_rodata[43] = op_break__rodata;
    op_names[43] = "op_break";
    arg_funcs[43] = op_break_set_args_from_code;
    ops_text[44] = op_tailcall__text;
    ops_rodata[44] = op_tailcall__rodata;
    op_names[44] = "op_tailcall";
    arg_funcs[44] = op_tailcall_set_args_from_code;
    ops_text[45] = op_blkpush__text;
    ops_rodata[45] = op_blkpush__rodata;
    op_names[45] = "op_blkpush";
    arg_funcs[45] = op_blkpush_set_args_from_code;
    ops_text[46] = op_add__text;
    ops_rodata[46] = op_add__rodata;
    op_names[46] = "op_add";
    arg_funcs[46] = op_add_set_args_from_code;
    ops_text[47] = op_addi__text;
    ops_rodata[47] = op_addi__rodata;
    op_names[47] = "op_addi";
    arg_funcs[47] = op_addi_set_args_from_code;
    ops_text[48] = op_sub__text;
    ops_rodata[48] = op_sub__rodata;
    op_names[48] = "op_sub";
    arg_funcs[48] = op_sub_set_args_from_code;
    ops_text[49] = op_subi__text;
    ops_rodata[49] = op_subi__rodata;
    op_names[49] = "op_subi";
    arg_funcs[49] = op_subi_set_args_from_code;
    ops_text[50] = op_mul__text;
    ops_rodata[50] = op_mul__rodata;
    op_names[50] = "op_mul";
    arg_funcs[50] = op_mul_set_args_from_code;
    ops_text[51] = op_div__text;
    ops_rodata[51] = op_div__rodata;
    op_names[51] = "op_div";
    arg_funcs[51] = op_div_set_args_from_code;
    ops_text[52] = op_eq__text;
    ops_rodata[52] = op_eq__rodata;
    op_names[52] = "op_eq";
    arg_funcs[52] = op_eq_set_args_from_code;
    ops_text[53] = op_lt__text;
    ops_rodata[53] = op_lt__rodata;
    op_names[53] = "op_lt";
    arg_funcs[53] = op_lt_set_args_from_code;
    ops_text[54] = op_le__text;
    ops_rodata[54] = op_le__rodata;
    op_names[54] = "op_le";
    arg_funcs[54] = op_le_set_args_from_code;
    ops_text[55] = op_gt__text;
    ops_rodata[55] = op_gt__rodata;
    op_names[55] = "op_gt";
    arg_funcs[55] = op_gt_set_args_from_code;
    ops_text[56] = op_ge__text;
    ops_rodata[56] = op_ge__rodata;
    op_names[56] = "op_ge";
    arg_funcs[56] = op_ge_set_args_from_code;
    ops_text[57] = op_array__text;
    ops_rodata[57] = op_array__rodata;
    op_names[57] = "op_array";
    arg_funcs[57] = op_array_set_args_from_code;
    ops_text[58] = op_arycat__text;
    ops_rodata[58] = op_arycat__rodata;
    op_names[58] = "op_arycat";
    arg_funcs[58] = op_arycat_set_args_from_code;
    ops_text[59] = op_arypush__text;
    ops_rodata[59] = op_arypush__rodata;
    op_names[59] = "op_arypush";
    arg_funcs[59] = op_arypush_set_args_from_code;
    ops_text[60] = op_aref__text;
    ops_rodata[60] = op_aref__rodata;
    op_names[60] = "op_aref";
    arg_funcs[60] = op_aref_set_args_from_code;
    ops_text[61] = op_aset__text;
    ops_rodata[61] = op_aset__rodata;
    op_names[61] = "op_aset";
    arg_funcs[61] = op_aset_set_args_from_code;
    ops_text[62] = op_apost__text;
    ops_rodata[62] = op_apost__rodata;
    op_names[62] = "op_apost";
    arg_funcs[62] = op_apost_set_args_from_code;
    ops_text[63] = op_string__text;
    ops_rodata[63] = op_string__rodata;
    op_names[63] = "op_string";
    arg_funcs[63] = op_string_set_args_from_code;
    ops_text[64] = op_strcat__text;
    ops_rodata[64] = op_strcat__rodata;
    op_names[64] = "op_strcat";
    arg_funcs[64] = op_strcat_set_args_from_code;
    ops_text[65] = op_hash__text;
    ops_rodata[65] = op_hash__rodata;
    op_names[65] = "op_hash";
    arg_funcs[65] = op_hash_set_args_from_code;
    ops_text[66] = op_lambda__text;
    ops_rodata[66] = op_lambda__rodata;
    op_names[66] = "op_lambda";
    arg_funcs[66] = op_lambda_set_args_from_code;
    ops_text[67] = op_range__text;
    ops_rodata[67] = op_range__rodata;
    op_names[67] = "op_range";
    arg_funcs[67] = op_range_set_args_from_code;
    ops_text[68] = op_oclass__text;
    ops_rodata[68] = op_oclass__rodata;
    op_names[68] = "op_oclass";
    arg_funcs[68] = op_oclass_set_args_from_code;
    ops_text[69] = op_class__text;
    ops_rodata[69] = op_class__rodata;
    op_names[69] = "op_class";
    arg_funcs[69] = op_class_set_args_from_code;
    ops_text[70] = op_module__text;
    ops_rodata[70] = op_module__rodata;
    op_names[70] = "op_module";
    arg_funcs[70] = op_module_set_args_from_code;
    ops_text[71] = op_exec__text;
    ops_rodata[71] = op_exec__rodata;
    op_names[71] = "op_exec";
    arg_funcs[71] = op_exec_set_args_from_code;
    ops_text[72] = op_method__text;
    ops_rodata[72] = op_method__rodata;
    op_names[72] = "op_method";
    arg_funcs[72] = op_method_set_args_from_code;
    ops_text[73] = op_sclass__text;
    ops_rodata[73] = op_sclass__rodata;
    op_names[73] = "op_sclass";
    arg_funcs[73] = op_sclass_set_args_from_code;
    ops_text[74] = op_tclass__text;
    ops_rodata[74] = op_tclass__rodata;
    op_names[74] = "op_tclass";
    arg_funcs[74] = op_tclass_set_args_from_code;
    ops_text[75] = op_debug__text;
    ops_rodata[75] = op_debug__rodata;
    op_names[75] = "op_debug";
    arg_funcs[75] = op_debug_set_args_from_code;
    ops_text[76] = op_stop__text;
    ops_rodata[76] = op_stop__rodata;
    op_names[76] = "op_stop";
    arg_funcs[76] = op_stop_set_args_from_code;
    ops_text[77] = op_err__text;
    ops_rodata[77] = op_err__rodata;
    op_names[77] = "op_err";
    arg_funcs[77] = op_err_set_args_from_code;
  }
}

uint8_t *jit_return(uint8_t *b) {
  *b++ = 0xc3;
  return b;
}
uint8_t *jit_jump(uint8_t *b, int32_t n, int force_rel16) {
  if(n >= -128 && n < 127 && !force_rel16) {
    *b++ = 235;
    *b++ = (int8_t) n;
    return b;
  }
  else {
    *b++ = 233;
    *((int32_t *)(b)) = (int32_t) n;
    b += sizeof(int32_t);
    return b;
  }
  return NULL;
}
uint8_t *jit_jump_if(uint8_t *b, int32_t n, int force_rel16) {
  if(n >= -128 && n < 127 && !force_rel16) {
    *b++ = 116;
    *b++ = (int8_t) n;
    return b;
  }
  else {
    *((uint16_t *)(b)) = (uint16_t) -31729;
    b += sizeof(uint16_t);
    *((int32_t *)(b)) = (int32_t) n;
    b += sizeof(int32_t);
    return b;
  }
  return NULL;
}
uint8_t *jit_jump_not(uint8_t *b, int32_t n, int force_rel16) {
  if(n >= -128 && n < 127 && !force_rel16) {
    *b++ = 117;
    *b++ = (int8_t) n;
    return b;
  }
  else {
    *((uint16_t *)(b)) = (uint16_t) -31473;
    b += sizeof(uint16_t);
    *((int32_t *)(b)) = (int32_t) n;
    b += sizeof(int32_t);
    return b;
  }
  return NULL;
}
