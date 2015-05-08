
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


/* args: {"b"=>[[16, 0, 7..10], [16, 8, 14..17]], "a"=>[[16, 8, 21..24], [16, 0, 28..31]]} */
static uint8_t op_move__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x88, 0x00, 0x10, 0xbc, 0x00, /*4: mov    0xbc1000(%rax),%rcx */
0x48, 0x8b, 0x90, 0x08, 0x10, 0xbc, 0x00, /*b: mov    0xbc1008(%rax),%rdx */
0x48, 0x89, 0x90, 0x08, 0x10, 0xab, 0x00, /*12: mov    %rdx,0xab1008(%rax) */
0x48, 0x89, 0x88, 0x00, 0x10, 0xab, 0x00, /*19: mov    %rcx,0xab1000(%rax) */

};
static uint8_t op_move__rodata[] = {

};

static void op_move_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = b * 16 + 0;
  *((int32_t *)(op + 14)) = b * 16 + 8;
  *((int32_t *)(op + 21)) = a * 16 + 8;
  *((int32_t *)(op + 28)) = a * 16 + 0;
}

static void op_move_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_move_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[16, 0, 11..14], [16, 8, 18..21]], "a"=>[[16, 8, 25..28], [16, 0, 32..35]]} */
static uint8_t op_loadl__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x20,                   /*4: mov    0x20(%rdi),%rcx */
0x48, 0x8b, 0x91, 0x00, 0x10, 0xbc, 0x00, /*8: mov    0xbc1000(%rcx),%rdx */
0x48, 0x8b, 0x89, 0x08, 0x10, 0xbc, 0x00, /*f: mov    0xbc1008(%rcx),%rcx */
0x48, 0x89, 0x88, 0x08, 0x10, 0xab, 0x00, /*16: mov    %rcx,0xab1008(%rax) */
0x48, 0x89, 0x90, 0x00, 0x10, 0xab, 0x00, /*1d: mov    %rdx,0xab1000(%rax) */

};
static uint8_t op_loadl__rodata[] = {

};

static void op_loadl_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 11)) = b * 16 + 0;
  *((int32_t *)(op + 18)) = b * 16 + 8;
  *((int32_t *)(op + 25)) = a * 16 + 8;
  *((int32_t *)(op + 32)) = a * 16 + 0;
}

static void op_loadl_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadl_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9], [16, 0, 20..23]], "b"=>[[1, 0, 24..27]]} */
static uint8_t op_loadi__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*4: movl   $0x3,0xab1008(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x00, 0x00, 0xbc, 0x00,/*12: movl   $0xbc0000,0xab1000(%rax) */

};
static uint8_t op_loadi__rodata[] = {

};

static void op_loadi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
  *((int32_t *)(op + 20)) = a * 16 + 0;
  *((int32_t *)(op + 24)) = b * 1 + 0;
}

static void op_loadi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadi_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9], [16, 0, 30..33]], "b"=>[[4, 0, 24..27]]} */
static uint8_t op_loadsym__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*4: movl   $0x4,0xab1008(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x28,                   /*12: mov    0x28(%rdi),%rcx */
0x8b, 0x89, 0x00, 0x04, 0xbc, 0x00,       /*16: mov    0xbc0400(%rcx),%ecx */
0x89, 0x88, 0x00, 0x10, 0xab, 0x00,       /*1c: mov    %ecx,0xab1000(%rax) */

};
static uint8_t op_loadsym__rodata[] = {

};

static void op_loadsym_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
  *((int32_t *)(op + 30)) = a * 16 + 0;
  *((int32_t *)(op + 24)) = b * 4 + 0;
}

static void op_loadsym_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadsym_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9], [16, 0, 20..23]]} */
static uint8_t op_loadnil__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movl   $0x0,0xab1008(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*12: movl   $0x0,0xab1000(%rax) */

};
static uint8_t op_loadnil__rodata[] = {

};

static void op_loadnil_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
  *((int32_t *)(op + 20)) = a * 16 + 0;
}

static void op_loadnil_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadnil_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 14..17], [16, 0, 21..24]]} */
static uint8_t op_loadself__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x08,                         /*4: mov    (%rax),%rcx */
0x48, 0x8b, 0x50, 0x08,                   /*7: mov    0x8(%rax),%rdx */
0x48, 0x89, 0x90, 0x08, 0x10, 0xab, 0x00, /*b: mov    %rdx,0xab1008(%rax) */
0x48, 0x89, 0x88, 0x00, 0x10, 0xab, 0x00, /*12: mov    %rcx,0xab1000(%rax) */

};
static uint8_t op_loadself__rodata[] = {

};

static void op_loadself_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = a * 16 + 8;
  *((int32_t *)(op + 21)) = a * 16 + 0;
}

static void op_loadself_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadself_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9], [16, 0, 20..23]]} */
static uint8_t op_loadt__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*4: movl   $0x2,0xab1008(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*12: movl   $0x1,0xab1000(%rax) */

};
static uint8_t op_loadt__rodata[] = {

};

static void op_loadt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
  *((int32_t *)(op + 20)) = a * 16 + 0;
}

static void op_loadt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadt_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9], [16, 0, 20..23]]} */
static uint8_t op_loadf__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movl   $0x0,0xab1008(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*12: movl   $0x1,0xab1000(%rax) */

};
static uint8_t op_loadf__rodata[] = {

};

static void op_loadf_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
  *((int32_t *)(op + 20)) = a * 16 + 0;
}

static void op_loadf_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadf_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[16, 0, 32..35], [16, 8, 39..42]]} */
static uint8_t op_getglobal__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*6: mov    0x58(%rbx),%rdi */
0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x28,                   /*e: mov    0x28(%rbx),%rax */
0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*12: mov    0xbc0400(%rax),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*18: callq  1d <op_getglobal+0x1d> */
0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*1d: mov    %rax,0xab1000(%r14) */
0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*24: mov    %edx,0xab1008(%r14) */
0x48, 0x89, 0xdf,                         /*2b: mov    %rbx,%rdi */
0x5b,                                     /*2e: pop    %rbx */
0x41, 0x5e,                               /*2f: pop    %r14 */

};
static uint8_t op_getglobal__rodata[] = {

};

static void op_getglobal_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 32)) = a * 16 + 0;
  *((int32_t *)(op + 39)) = a * 16 + 8;
}

static void op_getglobal_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getglobal_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 8, 31..34]]} */
static uint8_t op_setglobal__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rdx */
0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*1d: mov    0xab1008(%rax),%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*23: callq  28 <op_setglobal+0x28> */
0x48, 0x89, 0xdf,                         /*28: mov    %rbx,%rdi */
0x5b,                                     /*2b: pop    %rbx */

};
static uint8_t op_setglobal__rodata[] = {

};

static void op_setglobal_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 31)) = a * 16 + 8;
}

static void op_setglobal_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setglobal_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[1, 0, 15..18]], "a"=>[[16, 0, 27..30], [16, 8, 34..37]]} */
static uint8_t op_getspecial__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
0xbe, 0x00, 0x00, 0xbc, 0x00,             /*e: mov    $0xbc0000,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*13: callq  18 <op_getspecial+0x18> */
0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*18: mov    %rax,0xab1000(%r14) */
0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*1f: mov    %edx,0xab1008(%r14) */
0x48, 0x89, 0xdf,                         /*26: mov    %rbx,%rdi */
0x5b,                                     /*29: pop    %rbx */
0x41, 0x5e,                               /*2a: pop    %r14 */

};
static uint8_t op_getspecial__rodata[] = {

};

static void op_getspecial_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 1 + 0;
  *((int32_t *)(op + 27)) = a * 16 + 0;
  *((int32_t *)(op + 34)) = a * 16 + 8;
}

static void op_getspecial_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getspecial_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 15..18], [16, 8, 21..24]], "b"=>[[1, 0, 26..29]]} */
static uint8_t op_setspecial__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x58,                   /*8: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*c: mov    0xab1000(%rax),%rdx */
0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*13: mov    0xab1008(%rax),%ecx */
0xbe, 0x00, 0x00, 0xbc, 0x00,             /*19: mov    $0xbc0000,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1e: callq  23 <op_setspecial+0x23> */
0x48, 0x89, 0xdf,                         /*23: mov    %rbx,%rdi */
0x5b,                                     /*26: pop    %rbx */

};
static uint8_t op_setspecial__rodata[] = {

};

static void op_setspecial_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 16 + 0;
  *((int32_t *)(op + 21)) = a * 16 + 8;
  *((int32_t *)(op + 26)) = b * 1 + 0;
}

static void op_setspecial_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setspecial_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[16, 0, 32..35], [16, 8, 39..42]]} */
static uint8_t op_getiv__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*6: mov    0x58(%rbx),%rdi */
0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x28,                   /*e: mov    0x28(%rbx),%rax */
0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*12: mov    0xbc0400(%rax),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*18: callq  1d <op_getiv+0x1d> */
0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*1d: mov    %rax,0xab1000(%r14) */
0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*24: mov    %edx,0xab1008(%r14) */
0x48, 0x89, 0xdf,                         /*2b: mov    %rbx,%rdi */
0x5b,                                     /*2e: pop    %rbx */
0x41, 0x5e,                               /*2f: pop    %r14 */

};
static uint8_t op_getiv__rodata[] = {

};

static void op_getiv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 32)) = a * 16 + 0;
  *((int32_t *)(op + 39)) = a * 16 + 8;
}

static void op_getiv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getiv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 8, 31..34]]} */
static uint8_t op_setiv__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rdx */
0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*1d: mov    0xab1008(%rax),%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*23: callq  28 <op_setiv+0x28> */
0x48, 0x89, 0xdf,                         /*28: mov    %rbx,%rdi */
0x5b,                                     /*2b: pop    %rbx */

};
static uint8_t op_setiv__rodata[] = {

};

static void op_setiv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 31)) = a * 16 + 8;
}

static void op_setiv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setiv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 40..43]], "a"=>[[16, 0, 52..55], [16, 8, 59..62]]} */
static uint8_t op_getcv__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x10,                   /*6: mov    0x10(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x58,                   /*a: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*e: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*12: mov    0x20(%rcx),%rcx */
0x48, 0x89, 0x41, 0x38,                   /*16: mov    %rax,0x38(%rcx) */
0x48, 0x8b, 0x7b, 0x58,                   /*1a: mov    0x58(%rbx),%rdi */
0x4c, 0x8b, 0x73, 0x18,                   /*1e: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x28,                   /*22: mov    0x28(%rbx),%rax */
0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*26: mov    0xbc0400(%rax),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2c: callq  31 <op_getcv+0x31> */
0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*31: mov    %rax,0xab1000(%r14) */
0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*38: mov    %edx,0xab1008(%r14) */
0x48, 0x8b, 0x43, 0x58,                   /*3f: mov    0x58(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*43: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*47: mov    0x20(%rax),%rax */
0x48, 0xc7, 0x40, 0x38, 0x00, 0x00, 0x00, 0x00,/*4b: movq   $0x0,0x38(%rax) */
0x48, 0x89, 0xdf,                         /*53: mov    %rbx,%rdi */
0x5b,                                     /*56: pop    %rbx */
0x41, 0x5e,                               /*57: pop    %r14 */

};
static uint8_t op_getcv__rodata[] = {

};

static void op_getcv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 40)) = b * 4 + 0;
  *((int32_t *)(op + 52)) = a * 16 + 0;
  *((int32_t *)(op + 59)) = a * 16 + 8;
}

static void op_getcv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getcv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 8, 31..34]]} */
static uint8_t op_setcv__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rdx */
0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*1d: mov    0xab1008(%rax),%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*23: callq  28 <op_setcv+0x28> */
0x48, 0x89, 0xdf,                         /*28: mov    %rbx,%rdi */
0x5b,                                     /*2b: pop    %rbx */

};
static uint8_t op_setcv__rodata[] = {

};

static void op_setcv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 31)) = a * 16 + 8;
}

static void op_setcv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setcv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 34..37]], "a"=>[[16, 0, 82..85], [16, 8, 88..91]]} */
static uint8_t op_getconst__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x10,                   /*4: mov    0x10(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x58,                   /*8: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*c: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*10: mov    0x20(%rcx),%rcx */
0x48, 0x89, 0x41, 0x38,                   /*14: mov    %rax,0x38(%rcx) */
0x48, 0x8b, 0x43, 0x28,                   /*18: mov    0x28(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x58,                   /*1c: mov    0x58(%rbx),%rdi */
0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*20: mov    0xbc0400(%rax),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*26: callq  2b <op_getconst+0x2b> */
0x48, 0x8b, 0x4b, 0x58,                   /*2b: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*2f: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*33: mov    0x20(%rcx),%rcx */
0x48, 0xc7, 0x41, 0x38, 0x00, 0x00, 0x00, 0x00,/*37: movq   $0x0,0x38(%rcx) */
0x48, 0x8b, 0x4b, 0x58,                   /*3f: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*43: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*47: mov    0x8(%rcx),%rcx */
0x48, 0x89, 0x4b, 0x18,                   /*4b: mov    %rcx,0x18(%rbx) */
0x48, 0x89, 0x81, 0x00, 0x10, 0xab, 0x00, /*4f: mov    %rax,0xab1000(%rcx) */
0x89, 0x91, 0x08, 0x10, 0xab, 0x00,       /*56: mov    %edx,0xab1008(%rcx) */
0x48, 0x89, 0xdf,                         /*5c: mov    %rbx,%rdi */
0x5b,                                     /*5f: pop    %rbx */

};
static uint8_t op_getconst__rodata[] = {

};

static void op_getconst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 34)) = b * 4 + 0;
  *((int32_t *)(op + 82)) = a * 16 + 0;
  *((int32_t *)(op + 88)) = a * 16 + 8;
}

static void op_getconst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getconst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 8, 31..34]]} */
static uint8_t op_setconst__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rdx */
0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*1d: mov    0xab1008(%rax),%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*23: callq  28 <op_setconst+0x28> */
0x48, 0x89, 0xdf,                         /*28: mov    %rbx,%rdi */
0x5b,                                     /*2b: pop    %rbx */

};
static uint8_t op_setconst__rodata[] = {

};

static void op_setconst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 31)) = a * 16 + 8;
}

static void op_setconst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setconst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 38..41]], "a"=>[[16, 0, 45..48], [16, 8, 51..54], [16, 0, 99..102], [16, 8, 105..108]]} */
static uint8_t op_getmcnst__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x10,                   /*4: mov    0x10(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x58,                   /*8: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*c: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*10: mov    0x20(%rcx),%rcx */
0x48, 0x89, 0x41, 0x38,                   /*14: mov    %rax,0x38(%rcx) */
0x48, 0x8b, 0x7b, 0x58,                   /*18: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*1c: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*20: mov    0x28(%rbx),%rcx */
0x8b, 0x89, 0x00, 0x04, 0xbc, 0x00,       /*24: mov    0xbc0400(%rcx),%ecx */
0x48, 0x8b, 0xb0, 0x00, 0x10, 0xab, 0x00, /*2a: mov    0xab1000(%rax),%rsi */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*31: mov    0xab1008(%rax),%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*37: callq  3c <op_getmcnst+0x3c> */
0x48, 0x8b, 0x4b, 0x58,                   /*3c: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*40: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*44: mov    0x20(%rcx),%rcx */
0x48, 0xc7, 0x41, 0x38, 0x00, 0x00, 0x00, 0x00,/*48: movq   $0x0,0x38(%rcx) */
0x48, 0x8b, 0x4b, 0x58,                   /*50: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*54: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*58: mov    0x8(%rcx),%rcx */
0x48, 0x89, 0x4b, 0x18,                   /*5c: mov    %rcx,0x18(%rbx) */
0x48, 0x89, 0x81, 0x00, 0x10, 0xab, 0x00, /*60: mov    %rax,0xab1000(%rcx) */
0x89, 0x91, 0x08, 0x10, 0xab, 0x00,       /*67: mov    %edx,0xab1008(%rcx) */
0x48, 0x89, 0xdf,                         /*6d: mov    %rbx,%rdi */
0x5b,                                     /*70: pop    %rbx */

};
static uint8_t op_getmcnst__rodata[] = {

};

static void op_getmcnst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 38)) = b * 4 + 0;
  *((int32_t *)(op + 45)) = a * 16 + 0;
  *((int32_t *)(op + 51)) = a * 16 + 8;
  *((int32_t *)(op + 99)) = a * 16 + 0;
  *((int32_t *)(op + 105)) = a * 16 + 8;
}

static void op_getmcnst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getmcnst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 16, 25..28], [16, 24, 31..34], [16, 0, 38..41], [16, 8, 45..48]]} */
static uint8_t op_setmcnst__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0x89, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%ecx */
0x48, 0x8b, 0xb0, 0x10, 0x10, 0xab, 0x00, /*16: mov    0xab1010(%rax),%rsi */
0x8b, 0x90, 0x18, 0x10, 0xab, 0x00,       /*1d: mov    0xab1018(%rax),%edx */
0x4c, 0x8b, 0x80, 0x00, 0x10, 0xab, 0x00, /*23: mov    0xab1000(%rax),%r8 */
0x44, 0x8b, 0x88, 0x08, 0x10, 0xab, 0x00, /*2a: mov    0xab1008(%rax),%r9d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*31: callq  36 <op_setmcnst+0x36> */
0x48, 0x89, 0xdf,                         /*36: mov    %rbx,%rdi */
0x5b,                                     /*39: pop    %rbx */

};
static uint8_t op_setmcnst__rodata[] = {

};

static void op_setmcnst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 16;
  *((int32_t *)(op + 31)) = a * 16 + 24;
  *((int32_t *)(op + 38)) = a * 16 + 0;
  *((int32_t *)(op + 45)) = a * 16 + 8;
}

static void op_setmcnst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setmcnst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 13..16]], "c"=>[[1, 0, 18..21]], "b"=>[[16, 0, 98..101], [16, 8, 105..108]]} */
static uint8_t op_getupvar__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x48, 0x81, 0xc3, 0x00, 0x10, 0xab, 0x00, /*a: add    $0xab1000,%rbx */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*11: mov    $0xcd0000,%eax */
0x49, 0x8b, 0x4e, 0x58,                   /*16: mov    0x58(%r14),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*1a: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*1e: mov    0x20(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*22: mov    0x8(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x28,                   /*26: mov    0x28(%rcx),%rcx */
0xeb, 0x0a,                               /*2a: jmp    36 <op_getupvar+0x36> */
0x0f, 0x1f, 0x40, 0x00,                   /*2c: nopl   0x0(%rax) */
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
0x74, 0x10,                               /*49: je     5b <op_getupvar+0x5b> */
0x48, 0xc7, 0x03, 0x00, 0x00, 0x00, 0x00, /*4b: movq   $0x0,(%rbx) */
0xc7, 0x43, 0x08, 0x00, 0x00, 0x00, 0x00, /*52: movl   $0x0,0x8(%rbx) */
0xeb, 0x19,                               /*59: jmp    74 <op_getupvar+0x74> */
0x48, 0x8b, 0x41, 0x18,                   /*5b: mov    0x18(%rcx),%rax */
0x48, 0x8b, 0x88, 0x00, 0x10, 0xbc, 0x00, /*5f: mov    0xbc1000(%rax),%rcx */
0x48, 0x8b, 0x80, 0x08, 0x10, 0xbc, 0x00, /*66: mov    0xbc1008(%rax),%rax */
0x48, 0x89, 0x43, 0x08,                   /*6d: mov    %rax,0x8(%rbx) */
0x48, 0x89, 0x0b,                         /*71: mov    %rcx,(%rbx) */
0x4c, 0x89, 0xf7,                         /*74: mov    %r14,%rdi */
0x5b,                                     /*77: pop    %rbx */
0x41, 0x5e,                               /*78: pop    %r14 */

};
static uint8_t op_getupvar__rodata[] = {

};

static void op_getupvar_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = a * 16 + 0;
  *((int32_t *)(op + 18)) = c * 1 + 0;
  *((int32_t *)(op + 98)) = b * 16 + 0;
  *((int32_t *)(op + 105)) = b * 16 + 8;
}

static void op_getupvar_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getupvar_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"c"=>[[1, 0, 5..8]], "a"=>[[16, 0, 70..73], [16, 8, 77..80]], "b"=>[[16, 8, 84..87], [16, 0, 91..94]]} */
static uint8_t op_setupvar__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*4: mov    $0xcd0000,%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*9: mov    0x58(%rbx),%rcx */
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
0xeb, 0x31,                               /*35: jmp    68 <op_setupvar+0x68> */
0x84, 0xc9,                               /*37: test   %cl,%cl */
0x75, 0x2d,                               /*39: jne    68 <op_setupvar+0x68> */
0x48, 0x8b, 0x43, 0x18,                   /*3b: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4e, 0x18,                   /*3f: mov    0x18(%rsi),%rcx */
0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*43: mov    0xab1000(%rax),%rdx */
0x48, 0x8b, 0x80, 0x08, 0x10, 0xab, 0x00, /*4a: mov    0xab1008(%rax),%rax */
0x48, 0x89, 0x81, 0x08, 0x10, 0xbc, 0x00, /*51: mov    %rax,0xbc1008(%rcx) */
0x48, 0x89, 0x91, 0x00, 0x10, 0xbc, 0x00, /*58: mov    %rdx,0xbc1000(%rcx) */
0x48, 0x8b, 0x7b, 0x58,                   /*5f: mov    0x58(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*63: callq  68 <op_setupvar+0x68> */
0x48, 0x89, 0xdf,                         /*68: mov    %rbx,%rdi */
0x5b,                                     /*6b: pop    %rbx */

};
static uint8_t op_setupvar__rodata[] = {

};

static void op_setupvar_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = c * 1 + 0;
  *((int32_t *)(op + 70)) = a * 16 + 0;
  *((int32_t *)(op + 77)) = a * 16 + 8;
  *((int32_t *)(op + 84)) = b * 16 + 8;
  *((int32_t *)(op + 91)) = b * 16 + 0;
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


/* args: {"a"=>[[16, 8, 6..9]]} */
static uint8_t op_jmpif__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x83, 0xb8, 0x08, 0x10, 0xab, 0x00, 0x00, /*4: cmpl   $0x0,0xab1008(%rax) */

};
static uint8_t op_jmpif__rodata[] = {

};

static void op_jmpif_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
}

static void op_jmpif_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmpif_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9]]} */
static uint8_t op_jmpnot__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x83, 0xb8, 0x08, 0x10, 0xab, 0x00, 0x00, /*4: cmpl   $0x0,0xab1008(%rax) */

};
static uint8_t op_jmpnot__rodata[] = {

};

static void op_jmpnot_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
}

static void op_jmpnot_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmpnot_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"op_idx"=>[[4, 0, 92..95]]} */
static uint8_t op_onerr__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x58,                   /*4: mov    0x58(%rbx),%rax */
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
0x48, 0x8b, 0x7b, 0x58,                   /*28: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*2c: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x70, 0x38,                   /*30: mov    0x38(%rax),%rsi */
0x48, 0x63, 0x50, 0x40,                   /*34: movslq 0x40(%rax),%rdx */
0x48, 0xc1, 0xe2, 0x03,                   /*38: shl    $0x3,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*3c: callq  41 <op_onerr+0x41> */
0x48, 0x8b, 0x4b, 0x58,                   /*41: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*45: mov    0x18(%rcx),%rcx */
0x48, 0x89, 0x41, 0x38,                   /*49: mov    %rax,0x38(%rcx) */
0x48, 0x8b, 0x43, 0x58,                   /*4d: mov    0x58(%rbx),%rax */
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
0x48, 0x8b, 0x43, 0x58,                   /*7d: mov    0x58(%rbx),%rax */
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


/* args: {"a"=>[[16, 8, 17..20], [16, 0, 36..39]]} */
static uint8_t op_rescue__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x58,                   /*4: mov    0x58(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x28,                   /*8: mov    0x28(%rcx),%rcx */
0x0f, 0xb6, 0x09,                         /*c: movzbl (%rcx),%ecx */
0x89, 0x88, 0x08, 0x10, 0xab, 0x00,       /*f: mov    %ecx,0xab1008(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*15: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x58,                   /*19: mov    0x58(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x28,                   /*1d: mov    0x28(%rcx),%rcx */
0x48, 0x89, 0x88, 0x00, 0x10, 0xab, 0x00, /*21: mov    %rcx,0xab1000(%rax) */
0x48, 0x8b, 0x47, 0x58,                   /*28: mov    0x58(%rdi),%rax */
0x48, 0xc7, 0x40, 0x28, 0x00, 0x00, 0x00, 0x00,/*2c: movq   $0x0,0x28(%rax) */

};
static uint8_t op_rescue__rodata[] = {

};

static void op_rescue_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 16 + 8;
  *((int32_t *)(op + 36)) = a * 16 + 0;
}

static void op_rescue_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_rescue_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {} */
static uint8_t op_poperr__text[] = {
0xb8, 0x00, 0x00, 0x55, 0xff,             /*0: mov    $0xff550000,%eax */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*5: data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x4f, 0x58,                   /*10: mov    0x58(%rdi),%rcx */
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


/* args: {"a"=>[[16, 0, 15..18]]} */
static uint8_t op_raise__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x58,                   /*8: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x80, 0x00, 0x10, 0xab, 0x00, /*c: mov    0xab1000(%rax),%rax */
0x48, 0x89, 0x41, 0x28,                   /*13: mov    %rax,0x28(%rcx) */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*17: callq  1c <op_raise+0x1c> */
0x48, 0x89, 0xdf,                         /*1c: mov    %rbx,%rdi */
0x5b,                                     /*1f: pop    %rbx */

};
static uint8_t op_raise__rodata[] = {

};

static void op_raise_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 16 + 0;
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
0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x40, 0x20,                   /*e: mov    0x20(%rax),%rax */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*12: mov    0xbc0800(%rax),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*19: callq  1e <op_epush+0x1e> */
0x49, 0x89, 0xc6,                         /*1e: mov    %rax,%r14 */
0x48, 0x8b, 0x43, 0x58,                   /*21: mov    0x58(%rbx),%rax */
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
0x48, 0x8b, 0x7b, 0x58,                   /*47: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*4b: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x70, 0x48,                   /*4f: mov    0x48(%rax),%rsi */
0x48, 0x63, 0x50, 0x50,                   /*53: movslq 0x50(%rax),%rdx */
0x48, 0xc1, 0xe2, 0x03,                   /*57: shl    $0x3,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*5b: callq  60 <op_epush+0x60> */
0x48, 0x8b, 0x4b, 0x58,                   /*60: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*64: mov    0x18(%rcx),%rcx */
0x48, 0x89, 0x41, 0x48,                   /*68: mov    %rax,0x48(%rcx) */
0x48, 0x8b, 0x43, 0x58,                   /*6c: mov    0x58(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*70: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*74: mov    0x20(%rax),%rcx */
0x8b, 0x51, 0x20,                         /*78: mov    0x20(%rcx),%edx */
0x8d, 0x42, 0x01,                         /*7b: lea    0x1(%rdx),%eax */
0x89, 0x41, 0x20,                         /*7e: mov    %eax,0x20(%rcx) */
0x48, 0x63, 0xc2,                         /*81: movslq %edx,%rax */
0x48, 0x8b, 0x4b, 0x58,                   /*84: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*88: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x48,                   /*8c: mov    0x48(%rcx),%rcx */
0x4c, 0x89, 0x34, 0xc1,                   /*90: mov    %r14,(%rcx,%rax,8) */
0x8b, 0x43, 0x50,                         /*94: mov    0x50(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*97: mov    0x58(%rbx),%rcx */
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
0x49, 0x8b, 0x47, 0x58,                   /*9: mov    0x58(%r15),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*d: mov    0x18(%rax),%rax */
0x4c, 0x8b, 0x70, 0x20,                   /*11: mov    0x20(%rax),%r14 */
0x41, 0x8b, 0x6e, 0x20,                   /*15: mov    0x20(%r14),%ebp */
0x31, 0xdb,                               /*19: xor    %ebx,%ebx */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*1b: nopl   0x0(%rax,%rax,1) */
0x41, 0x3b, 0x6e, 0xc8,                   /*20: cmp    -0x38(%r14),%ebp */
0x7e, 0x25,                               /*24: jle    4b <op_epop+0x4b> */
0x49, 0x8b, 0x7f, 0x58,                   /*26: mov    0x58(%r15),%rdi */
0xff, 0xcd,                               /*2a: dec    %ebp */
0x89, 0xee,                               /*2c: mov    %ebp,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2e: callq  33 <op_epop+0x33> */
0x41, 0x8b, 0x47, 0x50,                   /*33: mov    0x50(%r15),%eax */
0x49, 0x8b, 0x4f, 0x58,                   /*37: mov    0x58(%r15),%rcx */
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
0x48, 0x8b, 0x53, 0x58,                   /*17: mov    0x58(%rbx),%rdx */
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
0x48, 0x8b, 0x43, 0x58,                   /*4a: mov    0x58(%rbx),%rax */
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


/* args: {"a"=>[[1, 0, 343..346]]} */
static uint8_t op_call__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x56,                               /*1: push   %r14 */
0x53,                                     /*3: push   %rbx */
0x49, 0x89, 0xfe,                         /*4: mov    %rdi,%r14 */
0x49, 0x8b, 0x46, 0x58,                   /*7: mov    0x58(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*b: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x08,                   /*f: mov    0x8(%rax),%rcx */
0x48, 0x8b, 0x40, 0x20,                   /*13: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x19,                         /*17: mov    (%rcx),%rbx */
0x8b, 0x51, 0x08,                         /*1a: mov    0x8(%rcx),%edx */
0x48, 0x8b, 0x4b, 0x20,                   /*1d: mov    0x20(%rbx),%rcx */
0x48, 0x89, 0x48, 0x48,                   /*21: mov    %rcx,0x48(%rax) */
0x48, 0x89, 0x58, 0x08,                   /*25: mov    %rbx,0x8(%rax) */
0x48, 0x8b, 0x4b, 0x28,                   /*29: mov    0x28(%rbx),%rcx */
0x48, 0x85, 0xc9,                         /*2d: test   %rcx,%rcx */
0x74, 0x24,                               /*30: je     56 <op_call+0x56> */
0x8b, 0x71, 0x20,                         /*32: mov    0x20(%rcx),%esi */
0x85, 0xf6,                               /*35: test   %esi,%esi */
0x74, 0x06,                               /*37: je     3f <op_call+0x3f> */
0x89, 0x30,                               /*39: mov    %esi,(%rax) */
0x48, 0x8b, 0x4b, 0x28,                   /*3b: mov    0x28(%rbx),%rcx */
0x48, 0x83, 0x79, 0x18, 0x00,             /*3f: cmpq   $0x0,0x18(%rcx) */
0x75, 0x10,                               /*44: jne    56 <op_call+0x56> */
0x49, 0x8b, 0x76, 0x58,                   /*46: mov    0x58(%r14),%rsi */
0x48, 0x8b, 0x76, 0x18,                   /*4a: mov    0x18(%rsi),%rsi */
0x48, 0x8b, 0x76, 0x08,                   /*4e: mov    0x8(%rsi),%rsi */
0x48, 0x89, 0x71, 0x18,                   /*52: mov    %rsi,0x18(%rcx) */
0xf6, 0x43, 0x02, 0x04,                   /*56: testb  $0x4,0x2(%rbx) */
0x74, 0x34,                               /*5a: je     90 <op_call+0x90> */
0x49, 0x8b, 0x7e, 0x58,                   /*5c: mov    0x58(%r14),%rdi */
0x48, 0x89, 0xde,                         /*60: mov    %rbx,%rsi */
0xff, 0x53, 0x18,                         /*63: callq  *0x18(%rbx) */
0x48, 0x89, 0xc3,                         /*66: mov    %rax,%rbx */
0x89, 0xd5,                               /*69: mov    %edx,%ebp */
0x49, 0x8b, 0x7e, 0x58,                   /*6b: mov    0x58(%r14),%rdi */
0x41, 0x8b, 0x76, 0x50,                   /*6f: mov    0x50(%r14),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*73: callq  78 <op_call+0x78> */
0x49, 0x8b, 0x46, 0x58,                   /*78: mov    0x58(%r14),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*7c: cmpq   $0x0,0x28(%rax) */
0x74, 0x57,                               /*81: je     da <op_call+0xda> */
0x4c, 0x89, 0xf7,                         /*83: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*86: callq  8b <op_call+0x8b> */
0xe9, 0x2d, 0x01, 0x00, 0x00,             /*8b: jmpq   1bd <op_call+0x1bd> */
0x49, 0x89, 0x1e,                         /*90: mov    %rbx,(%r14) */
0x48, 0x8b, 0x4b, 0x18,                   /*93: mov    0x18(%rbx),%rcx */
0x49, 0x89, 0x4e, 0x08,                   /*97: mov    %rcx,0x8(%r14) */
0x48, 0x85, 0xc9,                         /*9b: test   %rcx,%rcx */
0x0f, 0x84, 0x98, 0x00, 0x00, 0x00,       /*9e: je     13c <op_call+0x13c> */
0x48, 0x8b, 0x51, 0x10,                   /*a4: mov    0x10(%rcx),%rdx */
0x49, 0x89, 0x56, 0x20,                   /*a8: mov    %rdx,0x20(%r14) */
0x48, 0x8b, 0x51, 0x18,                   /*ac: mov    0x18(%rcx),%rdx */
0x49, 0x89, 0x56, 0x28,                   /*b0: mov    %rdx,0x28(%r14) */
0x0f, 0xb7, 0x49, 0x02,                   /*b4: movzwl 0x2(%rcx),%ecx */
0x89, 0x48, 0x18,                         /*b8: mov    %ecx,0x18(%rax) */
0x8b, 0x50, 0x40,                         /*bb: mov    0x40(%rax),%edx */
0x49, 0x8b, 0x46, 0x08,                   /*be: mov    0x8(%r14),%rax */
0x49, 0x8b, 0x7e, 0x58,                   /*c2: mov    0x58(%r14),%rdi */
0x0f, 0xb7, 0x70, 0x02,                   /*c6: movzwl 0x2(%rax),%esi */
0x85, 0xd2,                               /*ca: test   %edx,%edx */
0x0f, 0x88, 0x95, 0x00, 0x00, 0x00,       /*cc: js     167 <op_call+0x167> */
0x83, 0xc2, 0x02,                         /*d2: add    $0x2,%edx */
0xe9, 0x9d, 0x00, 0x00, 0x00,             /*d5: jmpq   177 <op_call+0x177> */
0x48, 0x8b, 0x40, 0x18,                   /*da: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*de: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x51, 0x10,                   /*e2: mov    0x10(%rcx),%rdx */
0x48, 0x89, 0x50, 0x08,                   /*e6: mov    %rdx,0x8(%rax) */
0x49, 0x89, 0x56, 0x18,                   /*ea: mov    %rdx,0x18(%r14) */
0x48, 0x63, 0x41, 0x44,                   /*ee: movslq 0x44(%rcx),%rax */
0x48, 0xc1, 0xe0, 0x04,                   /*f2: shl    $0x4,%rax */
0x48, 0x89, 0x1c, 0x02,                   /*f6: mov    %rbx,(%rdx,%rax,1) */
0x89, 0x6c, 0x02, 0x08,                   /*fa: mov    %ebp,0x8(%rdx,%rax,1) */
0x48, 0x8b, 0x41, 0x30,                   /*fe: mov    0x30(%rcx),%rax */
0x49, 0x89, 0x46, 0x10,                   /*102: mov    %rax,0x10(%r14) */
0x49, 0x8b, 0x7e, 0x58,                   /*106: mov    0x58(%r14),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*10a: callq  10f <op_call+0x10f> */
0x49, 0x8b, 0x46, 0x58,                   /*10f: mov    0x58(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*113: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*117: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*11b: mov    0x8(%rax),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*11f: mov    0x18(%rax),%rax */
0x49, 0x89, 0x46, 0x08,                   /*123: mov    %rax,0x8(%r14) */
0x48, 0x8b, 0x48, 0x10,                   /*127: mov    0x10(%rax),%rcx */
0x49, 0x89, 0x4e, 0x20,                   /*12b: mov    %rcx,0x20(%r14) */
0x48, 0x8b, 0x40, 0x18,                   /*12f: mov    0x18(%rax),%rax */
0x49, 0x89, 0x46, 0x28,                   /*133: mov    %rax,0x28(%r14) */
0xe9, 0x81, 0x00, 0x00, 0x00,             /*137: jmpq   1bd <op_call+0x1bd> */
0x49, 0x8b, 0x46, 0x58,                   /*13c: mov    0x58(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*140: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*144: mov    0x8(%rax),%rax */
0x48, 0xc7, 0x00, 0x00, 0x00, 0x00, 0x00, /*148: movq   $0x0,(%rax) */
0xc7, 0x40, 0x08, 0x00, 0x00, 0x00, 0x00, /*14f: movl   $0x0,0x8(%rax) */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*156: mov    $0xab0000,%esi */
0x31, 0xd2,                               /*15b: xor    %edx,%edx */
0x4c, 0x89, 0xf7,                         /*15d: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*160: callq  165 <op_call+0x165> */
0xeb, 0x56,                               /*165: jmp    1bd <op_call+0x1bd> */
0x83, 0xfe, 0x03,                         /*167: cmp    $0x3,%esi */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*16a: mov    $0x3,%eax */
0x0f, 0x42, 0xf0,                         /*16f: cmovb  %eax,%esi */
0xba, 0x03, 0x00, 0x00, 0x00,             /*172: mov    $0x3,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*177: callq  17c <op_call+0x17c> */
0x49, 0x8b, 0x46, 0x58,                   /*17c: mov    0x58(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*180: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*184: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x18,                   /*188: mov    %rax,0x18(%r14) */
0x48, 0x8b, 0x4b, 0x28,                   /*18c: mov    0x28(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*190: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x11,                         /*194: mov    (%rcx),%rdx */
0x48, 0x8b, 0x49, 0x08,                   /*197: mov    0x8(%rcx),%rcx */
0x48, 0x89, 0x48, 0x08,                   /*19b: mov    %rcx,0x8(%rax) */
0x48, 0x89, 0x10,                         /*19f: mov    %rdx,(%rax) */
0x49, 0x8b, 0x46, 0x08,                   /*1a2: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*1a6: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x10,                   /*1aa: mov    %rax,0x10(%r14) */
0x49, 0x8b, 0x36,                         /*1ae: mov    (%r14),%rsi */
0x49, 0x8b, 0x7e, 0x58,                   /*1b1: mov    0x58(%r14),%rdi */
0x4c, 0x89, 0xf2,                         /*1b5: mov    %r14,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1b8: callq  1bd <op_call+0x1bd> */
0x4c, 0x89, 0xf7,                         /*1bd: mov    %r14,%rdi */
0x5b,                                     /*1c0: pop    %rbx */
0x41, 0x5e,                               /*1c1: pop    %r14 */
0x5d,                                     /*1c3: pop    %rbp */

};
static uint8_t op_call__rodata[] = {

};

static void op_call_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 343)) = a * 1 + 0;
}

static void op_call_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_call_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"c"=>[[1, 0, 234..237], [1, 1, 294..297], [1, 1, 370..373]], "a"=>[[1, 0, 308..311], [1, 0, 316..319], [1, 1, 324..327], [1, 1, 332..335], [16, 24, 348..351], [16, 16, 365..368], [16, 0, 442..445], [1, 0, 554..557]]} */
static uint8_t op_super__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x83, 0xec, 0x18,                   /*a: sub    $0x18,%rsp */
0x48, 0x89, 0xfb,                         /*e: mov    %rdi,%rbx */
0x48, 0x8b, 0x6b, 0x58,                   /*11: mov    0x58(%rbx),%rbp */
0x48, 0x8b, 0x45, 0x18,                   /*15: mov    0x18(%rbp),%rax */
0x4c, 0x8b, 0x70, 0x20,                   /*19: mov    0x20(%rax),%r14 */
0x45, 0x8b, 0x2e,                         /*1d: mov    (%r14),%r13d */
0x45, 0x85, 0xed,                         /*20: test   %r13d,%r13d */
0x74, 0x73,                               /*23: je     98 <op_super+0x98> */
0x48, 0x8b, 0x43, 0x18,                   /*25: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x08,                         /*29: mov    (%rax),%rcx */
0x48, 0x89, 0x4c, 0x24, 0x08,             /*2c: mov    %rcx,0x8(%rsp) */
0x4c, 0x8b, 0x78, 0x08,                   /*31: mov    0x8(%rax),%r15 */
0x4c, 0x89, 0x3c, 0x24,                   /*35: mov    %r15,(%rsp) */
0x49, 0xc1, 0xef, 0x20,                   /*39: shr    $0x20,%r15 */
0x49, 0x8b, 0x46, 0x48,                   /*3d: mov    0x48(%r14),%rax */
0x48, 0x8b, 0x48, 0x28,                   /*41: mov    0x28(%rax),%rcx */
0x48, 0x89, 0x4c, 0x24, 0x10,             /*45: mov    %rcx,0x10(%rsp) */
0x48, 0x8b, 0x03,                         /*4a: mov    (%rbx),%rax */
0x48, 0x39, 0x48, 0x38,                   /*4d: cmp    %rcx,0x38(%rax) */
0x75, 0x08,                               /*51: jne    5b <op_super+0x5b> */
0x31, 0xd2,                               /*53: xor    %edx,%edx */
0x44, 0x39, 0x68, 0x30,                   /*55: cmp    %r13d,0x30(%rax) */
0x74, 0x33,                               /*59: je     8e <op_super+0x8e> */
0x48, 0x39, 0x48, 0x48,                   /*5b: cmp    %rcx,0x48(%rax) */
0x75, 0x0b,                               /*5f: jne    6c <op_super+0x6c> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*61: mov    $0x1,%edx */
0x44, 0x39, 0x68, 0x40,                   /*66: cmp    %r13d,0x40(%rax) */
0x74, 0x22,                               /*6a: je     8e <op_super+0x8e> */
0x48, 0x39, 0x48, 0x58,                   /*6c: cmp    %rcx,0x58(%rax) */
0x75, 0x0b,                               /*70: jne    7d <op_super+0x7d> */
0xba, 0x02, 0x00, 0x00, 0x00,             /*72: mov    $0x2,%edx */
0x44, 0x39, 0x68, 0x50,                   /*77: cmp    %r13d,0x50(%rax) */
0x74, 0x11,                               /*7b: je     8e <op_super+0x8e> */
0x48, 0x39, 0x48, 0x68,                   /*7d: cmp    %rcx,0x68(%rax) */
0x75, 0x53,                               /*81: jne    d6 <op_super+0xd6> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*83: mov    $0x3,%edx */
0x44, 0x39, 0x68, 0x60,                   /*88: cmp    %r13d,0x60(%rax) */
0x75, 0x48,                               /*8c: jne    d6 <op_super+0xd6> */
0x4c, 0x8b, 0xa4, 0xd0, 0x90, 0x00, 0x00, 0x00,/*8e: mov    0x90(%rax,%rdx,8),%r12 */
0xeb, 0x51,                               /*96: jmp    e9 <op_super+0xe9> */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*98: mov    0x0(%rip),%rsi        # 9f <op_super+0x9f> */
0x48, 0x89, 0xef,                         /*9f: mov    %rbp,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*a2: callq  a7 <op_super+0xa7> */
0x49, 0x89, 0xc6,                         /*a7: mov    %rax,%r14 */
0x48, 0x8b, 0x7b, 0x58,                   /*aa: mov    0x58(%rbx),%rdi */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*ae: mov    $0x0,%esi */
0xba, 0x1e, 0x00, 0x00, 0x00,             /*b3: mov    $0x1e,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*b8: callq  bd <op_super+0xbd> */
0x89, 0xd1,                               /*bd: mov    %edx,%ecx */
0x48, 0x89, 0xef,                         /*bf: mov    %rbp,%rdi */
0x4c, 0x89, 0xf6,                         /*c2: mov    %r14,%rsi */
0x48, 0x89, 0xc2,                         /*c5: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c8: callq  cd <op_super+0xcd> */
0x48, 0x89, 0x45, 0x28,                   /*cd: mov    %rax,0x28(%rbp) */
0xe9, 0x44, 0x01, 0x00, 0x00,             /*d1: jmpq   21a <op_super+0x21a> */
0x48, 0x8d, 0x74, 0x24, 0x10,             /*d6: lea    0x10(%rsp),%rsi */
0x48, 0x89, 0xef,                         /*db: mov    %rbp,%rdi */
0x44, 0x89, 0xea,                         /*de: mov    %r13d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*e1: callq  e6 <op_super+0xe6> */
0x49, 0x89, 0xc4,                         /*e6: mov    %rax,%r12 */
0xbd, 0x00, 0x00, 0xcd, 0x00,             /*e9: mov    $0xcd0000,%ebp */
0x4d, 0x85, 0xe4,                         /*ee: test   %r12,%r12 */
0x0f, 0x85, 0x7f, 0x00, 0x00, 0x00,       /*f1: jne    176 <op_super+0x176> */
0x48, 0x8b, 0x7b, 0x58,                   /*f7: mov    0x58(%rbx),%rdi */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*fb: mov    $0x0,%esi */
0xba, 0x0e, 0x00, 0x00, 0x00,             /*100: mov    $0xe,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*105: callq  10a <op_super+0x10a> */
0x41, 0x89, 0xc5,                         /*10a: mov    %eax,%r13d */
0x48, 0x8b, 0x7b, 0x58,                   /*10d: mov    0x58(%rbx),%rdi */
0x48, 0x8d, 0x74, 0x24, 0x10,             /*111: lea    0x10(%rsp),%rsi */
0x44, 0x89, 0xea,                         /*116: mov    %r13d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*119: callq  11e <op_super+0x11e> */
0x49, 0x89, 0xc4,                         /*11e: mov    %rax,%r12 */
0x48, 0x8b, 0x43, 0x18,                   /*121: mov    0x18(%rbx),%rax */
0xb9, 0x10, 0x00, 0xd0, 0x0c,             /*125: mov    $0xcd00010,%ecx */
0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00,       /*12a: nopw   0x0(%rax,%rax,1) */
0x48, 0x8b, 0x94, 0x08, 0x00, 0x00, 0xb0, 0x0a,/*130: mov    0xab00000(%rax,%rcx,1),%rdx */
0x48, 0x8b, 0xb4, 0x08, 0x08, 0x00, 0xb0, 0x0a,/*138: mov    0xab00008(%rax,%rcx,1),%rsi */
0x48, 0x89, 0xb4, 0x08, 0x18, 0x00, 0xb0, 0x0a,/*140: mov    %rsi,0xab00018(%rax,%rcx,1) */
0x48, 0x89, 0x94, 0x08, 0x10, 0x00, 0xb0, 0x0a,/*148: mov    %rdx,0xab00010(%rax,%rcx,1) */
0x48, 0x83, 0xc1, 0xf0,                   /*150: add    $0xfffffffffffffff0,%rcx */
0x75, 0xda,                               /*154: jne    130 <op_super+0x130> */
0x48, 0x8b, 0x43, 0x18,                   /*156: mov    0x18(%rbx),%rax */
0xc7, 0x80, 0x18, 0x10, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*15a: movl   $0x4,0xab1018(%rax) */
0x41, 0x8b, 0x06,                         /*164: mov    (%r14),%eax */
0x48, 0x8b, 0x4b, 0x18,                   /*167: mov    0x18(%rbx),%rcx */
0x89, 0x81, 0x10, 0x10, 0xab, 0x00,       /*16b: mov    %eax,0xab1010(%rcx) */
0xbd, 0x01, 0x00, 0xcd, 0x00,             /*171: mov    $0xcd0001,%ebp */
0x48, 0x8b, 0x7b, 0x58,                   /*176: mov    0x58(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*17a: callq  17f <op_super+0x17f> */
0x44, 0x89, 0x28,                         /*17f: mov    %r13d,(%rax) */
0x4c, 0x89, 0x60, 0x08,                   /*182: mov    %r12,0x8(%rax) */
0x48, 0x8b, 0x4b, 0x58,                   /*186: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*18a: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*18e: mov    0x8(%rcx),%rcx */
0x48, 0x89, 0x48, 0x10,                   /*192: mov    %rcx,0x10(%rax) */
0x89, 0x68, 0x40,                         /*196: mov    %ebp,0x40(%rax) */
0x48, 0x8b, 0x4c, 0x24, 0x10,             /*199: mov    0x10(%rsp),%rcx */
0x48, 0x89, 0x48, 0x48,                   /*19e: mov    %rcx,0x48(%rax) */
0x48, 0x8b, 0x4b, 0x10,                   /*1a2: mov    0x10(%rbx),%rcx */
0x48, 0x83, 0xc1, 0x04,                   /*1a6: add    $0x4,%rcx */
0x48, 0x89, 0x48, 0x30,                   /*1aa: mov    %rcx,0x30(%rax) */
0x48, 0x8b, 0x4b, 0x58,                   /*1ae: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*1b2: mov    0x18(%rcx),%rcx */
0x48, 0x81, 0x41, 0x08, 0x00, 0x10, 0xab, 0x00,/*1b6: addq   $0xab1000,0x8(%rcx) */
0x48, 0x8b, 0x4b, 0x58,                   /*1be: mov    0x58(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*1c2: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*1c6: mov    0x8(%rcx),%rcx */
0x48, 0x8b, 0x74, 0x24, 0x08,             /*1ca: mov    0x8(%rsp),%rsi */
0x48, 0x89, 0x31,                         /*1cf: mov    %rsi,(%rcx) */
0x48, 0x8b, 0x14, 0x24,                   /*1d2: mov    (%rsp),%rdx */
0x89, 0x51, 0x08,                         /*1d6: mov    %edx,0x8(%rcx) */
0x44, 0x89, 0x79, 0x0c,                   /*1d9: mov    %r15d,0xc(%rcx) */
0x41, 0xf6, 0x44, 0x24, 0x02, 0x04,       /*1dd: testb  $0x4,0x2(%r12) */
0x74, 0x42,                               /*1e3: je     227 <op_super+0x227> */
0x83, 0xcd, 0x02,                         /*1e5: or     $0x2,%ebp */
0x89, 0x68, 0x18,                         /*1e8: mov    %ebp,0x18(%rax) */
0x48, 0x8b, 0x7b, 0x58,                   /*1eb: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*1ef: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x68, 0x08,                   /*1f3: mov    0x8(%rax),%rbp */
0x41, 0xff, 0x54, 0x24, 0x18,             /*1f7: callq  *0x18(%r12) */
0x48, 0x89, 0x45, 0x00,                   /*1fc: mov    %rax,0x0(%rbp) */
0x89, 0x55, 0x08,                         /*200: mov    %edx,0x8(%rbp) */
0x48, 0x8b, 0x7b, 0x58,                   /*203: mov    0x58(%rbx),%rdi */
0x8b, 0x73, 0x50,                         /*207: mov    0x50(%rbx),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*20a: callq  20f <op_super+0x20f> */
0x48, 0x8b, 0x43, 0x58,                   /*20f: mov    0x58(%rbx),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*213: cmpq   $0x0,0x28(%rax) */
0x74, 0x78,                               /*218: je     292 <op_super+0x292> */
0x48, 0x89, 0xdf,                         /*21a: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*21d: callq  222 <op_super+0x222> */
0xe9, 0x88, 0x00, 0x00, 0x00,             /*222: jmpq   2af <op_super+0x2af> */
0xc7, 0x40, 0x44, 0x00, 0x00, 0xab, 0x00, /*227: movl   $0xab0000,0x44(%rax) */
0x4c, 0x89, 0x60, 0x08,                   /*22e: mov    %r12,0x8(%rax) */
0x49, 0x8b, 0x4c, 0x24, 0x18,             /*232: mov    0x18(%r12),%rcx */
0x48, 0x89, 0x4b, 0x08,                   /*237: mov    %rcx,0x8(%rbx) */
0x48, 0x8b, 0x51, 0x10,                   /*23b: mov    0x10(%rcx),%rdx */
0x48, 0x89, 0x53, 0x20,                   /*23f: mov    %rdx,0x20(%rbx) */
0x48, 0x8b, 0x51, 0x18,                   /*243: mov    0x18(%rcx),%rdx */
0x48, 0x89, 0x53, 0x28,                   /*247: mov    %rdx,0x28(%rbx) */
0x0f, 0xb7, 0x49, 0x02,                   /*24b: movzwl 0x2(%rcx),%ecx */
0x89, 0x48, 0x18,                         /*24f: mov    %ecx,0x18(%rax) */
0x48, 0x8b, 0x4b, 0x08,                   /*252: mov    0x8(%rbx),%rcx */
0x48, 0x8b, 0x7b, 0x58,                   /*256: mov    0x58(%rbx),%rdi */
0x0f, 0xb7, 0x71, 0x02,                   /*25a: movzwl 0x2(%rcx),%esi */
0x8b, 0x50, 0x40,                         /*25e: mov    0x40(%rax),%edx */
0x83, 0xc2, 0x02,                         /*261: add    $0x2,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*264: callq  269 <op_super+0x269> */
0x48, 0x8b, 0x7b, 0x58,                   /*269: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*26d: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*271: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x18,                   /*275: mov    %rax,0x18(%rbx) */
0x48, 0x8b, 0x43, 0x08,                   /*279: mov    0x8(%rbx),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*27d: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x10,                   /*281: mov    %rax,0x10(%rbx) */
0x4c, 0x89, 0xe6,                         /*285: mov    %r12,%rsi */
0x48, 0x89, 0xda,                         /*288: mov    %rbx,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*28b: callq  290 <op_super+0x290> */
0xeb, 0x1d,                               /*290: jmp    2af <op_super+0x2af> */
0x48, 0x8b, 0x40, 0x18,                   /*292: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*296: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x49, 0x10,                   /*29a: mov    0x10(%rcx),%rcx */
0x48, 0x89, 0x48, 0x08,                   /*29e: mov    %rcx,0x8(%rax) */
0x48, 0x89, 0x4b, 0x18,                   /*2a2: mov    %rcx,0x18(%rbx) */
0x48, 0x8b, 0x7b, 0x58,                   /*2a6: mov    0x58(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2aa: callq  2af <op_super+0x2af> */
0x48, 0x89, 0xdf,                         /*2af: mov    %rbx,%rdi */
0x48, 0x83, 0xc4, 0x18,                   /*2b2: add    $0x18,%rsp */
0x5b,                                     /*2b6: pop    %rbx */
0x41, 0x5c,                               /*2b7: pop    %r12 */
0x41, 0x5d,                               /*2b9: pop    %r13 */
0x41, 0x5e,                               /*2bb: pop    %r14 */
0x41, 0x5f,                               /*2bd: pop    %r15 */
0x5d,                                     /*2bf: pop    %rbp */

};
static uint8_t op_super__rodata[] = {
0x73, 0x75, 0x70, 0x65,                   
0x73, 0x69, 0x64, 0x65,                   
0x65, 0x74, 0x68, 0x6f,                   

};

static void op_super_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 234)) = c * 1 + 0;
  *((int32_t *)(op + 294)) = c * 1 + 1;
  *((int32_t *)(op + 370)) = c * 1 + 1;
  *((int32_t *)(op + 308)) = a * 1 + 0;
  *((int32_t *)(op + 316)) = a * 1 + 0;
  *((int32_t *)(op + 324)) = a * 1 + 1;
  *((int32_t *)(op + 332)) = a * 1 + 1;
  *((int32_t *)(op + 348)) = a * 16 + 24;
  *((int32_t *)(op + 365)) = a * 16 + 16;
  *((int32_t *)(op + 442)) = a * 16 + 0;
  *((int32_t *)(op + 554)) = a * 1 + 0;
}

static void op_super_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_super_set_args(op, GETARG_A(c),0,GETARG_C(c),op_idx);
}


/* args: {"b"=>[[1, 0, 21..24]], "a"=>[[16, 0, 354..357], [16, 8, 360..363], [16, 0, 372..375], [16, 0, 637..640], [16, 8, 644..647], [16, 24, 680..683], [16, 16, 687..690]]} */
static uint8_t op_argary__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x83, 0xec, 0x38,                   /*a: sub    $0x38,%rsp */
0x49, 0x89, 0xfc,                         /*e: mov    %rdi,%r12 */
0xc7, 0x44, 0x24, 0x34, 0x00, 0x00, 0xbc, 0x00,/*11: movl   $0xbc0000,0x34(%rsp) */
0x44, 0x8b, 0x6c, 0x24, 0x34,             /*19: mov    0x34(%rsp),%r13d */
0x44, 0x8b, 0x74, 0x24, 0x34,             /*1e: mov    0x34(%rsp),%r14d */
0x8b, 0x5c, 0x24, 0x34,                   /*23: mov    0x34(%rsp),%ebx */
0x8b, 0x74, 0x24, 0x34,                   /*27: mov    0x34(%rsp),%esi */
0x83, 0xe6, 0x0f,                         /*2b: and    $0xf,%esi */
0x0f, 0x84, 0x8a, 0x00, 0x00, 0x00,       /*2e: je     be <op_argary+0xbe> */
0x49, 0x8b, 0x6c, 0x24, 0x58,             /*34: mov    0x58(%r12),%rbp */
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
0x74, 0x4e,                               /*75: je     c5 <op_argary+0xc5> */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*77: mov    0x0(%rip),%rsi        # 7e <op_argary+0x7e> */
0x48, 0x89, 0xef,                         /*7e: mov    %rbp,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*81: callq  86 <op_argary+0x86> */
0x49, 0x89, 0xc6,                         /*86: mov    %rax,%r14 */
0x49, 0x8b, 0x7c, 0x24, 0x58,             /*89: mov    0x58(%r12),%rdi */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*8e: mov    $0x0,%esi */
0xba, 0x1e, 0x00, 0x00, 0x00,             /*93: mov    $0x1e,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*98: callq  9d <op_argary+0x9d> */
0x89, 0xd1,                               /*9d: mov    %edx,%ecx */
0x48, 0x89, 0xef,                         /*9f: mov    %rbp,%rdi */
0x4c, 0x89, 0xf6,                         /*a2: mov    %r14,%rsi */
0x48, 0x89, 0xc2,                         /*a5: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*a8: callq  ad <op_argary+0xad> */
0x48, 0x89, 0x45, 0x28,                   /*ad: mov    %rax,0x28(%rbp) */
0x4c, 0x89, 0xe7,                         /*b1: mov    %r12,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*b4: callq  b9 <op_argary+0xb9> */
0xe9, 0x05, 0x02, 0x00, 0x00,             /*b9: jmpq   2c3 <op_argary+0x2c3> */
0x49, 0x8d, 0x44, 0x24, 0x18,             /*be: lea    0x18(%r12),%rax */
0xeb, 0x04,                               /*c3: jmp    c9 <op_argary+0xc9> */
0x48, 0x83, 0xc0, 0x18,                   /*c5: add    $0x18,%rax */
0x41, 0xc1, 0xed, 0x0a,                   /*c9: shr    $0xa,%r13d */
0x45, 0x89, 0xe9,                         /*cd: mov    %r13d,%r9d */
0x41, 0x83, 0xe1, 0x3f,                   /*d0: and    $0x3f,%r9d */
0x41, 0xc1, 0xee, 0x09,                   /*d4: shr    $0x9,%r14d */
0x41, 0x83, 0xe6, 0x01,                   /*d8: and    $0x1,%r14d */
0xc1, 0xeb, 0x04,                         /*dc: shr    $0x4,%ebx */
0x89, 0xd9,                               /*df: mov    %ebx,%ecx */
0x83, 0xe1, 0x1f,                         /*e1: and    $0x1f,%ecx */
0x4c, 0x8b, 0x00,                         /*e4: mov    (%rax),%r8 */
0x49, 0x8d, 0x68, 0x10,                   /*e7: lea    0x10(%r8),%rbp */
0x45, 0x85, 0xf6,                         /*eb: test   %r14d,%r14d */
0x0f, 0x84, 0x61, 0x01, 0x00, 0x00,       /*ee: je     255 <op_argary+0x255> */
0x44, 0x89, 0x74, 0x24, 0x30,             /*f4: mov    %r14d,0x30(%rsp) */
0x44, 0x89, 0xc8,                         /*f9: mov    %r9d,%eax */
0x48, 0x89, 0x44, 0x24, 0x10,             /*fc: mov    %rax,0x10(%rsp) */
0x48, 0x8d, 0x40, 0x01,                   /*101: lea    0x1(%rax),%rax */
0x48, 0xc1, 0xe0, 0x04,                   /*105: shl    $0x4,%rax */
0x45, 0x31, 0xff,                         /*109: xor    %r15d,%r15d */
0x41, 0x83, 0x7c, 0x00, 0x08, 0x0e,       /*10c: cmpl   $0xe,0x8(%r8,%rax,1) */
0xba, 0x00, 0x00, 0x00, 0x00,             /*112: mov    $0x0,%edx */
0x75, 0x0b,                               /*117: jne    124 <op_argary+0x124> */
0x49, 0x8b, 0x04, 0x00,                   /*119: mov    (%r8,%rax,1),%rax */
0x4c, 0x8b, 0x78, 0x28,                   /*11d: mov    0x28(%rax),%r15 */
0x8b, 0x50, 0x18,                         /*121: mov    0x18(%rax),%edx */
0x48, 0x89, 0x54, 0x24, 0x08,             /*124: mov    %rdx,0x8(%rsp) */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*129: mov    0x18(%r12),%rax */
0x48, 0x89, 0x04, 0x24,                   /*12e: mov    %rax,(%rsp) */
0x49, 0x8b, 0x7c, 0x24, 0x58,             /*132: mov    0x58(%r12),%rdi */
0x42, 0x8d, 0x34, 0x09,                   /*137: lea    (%rcx,%r9,1),%esi */
0x48, 0x89, 0x4c, 0x24, 0x20,             /*13b: mov    %rcx,0x20(%rsp) */
0x01, 0xd6,                               /*140: add    %edx,%esi */
0x89, 0x74, 0x24, 0x1c,                   /*142: mov    %esi,0x1c(%rsp) */
0x4c, 0x89, 0x4c, 0x24, 0x28,             /*146: mov    %r9,0x28(%rsp) */
0x4d, 0x89, 0xc6,                         /*14b: mov    %r8,%r14 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*14e: callq  153 <op_argary+0x153> */
0x4d, 0x89, 0xf1,                         /*153: mov    %r14,%r9 */
0x4c, 0x8b, 0x44, 0x24, 0x28,             /*156: mov    0x28(%rsp),%r8 */
0x48, 0x8b, 0x0c, 0x24,                   /*15b: mov    (%rsp),%rcx */
0x48, 0x89, 0x81, 0x00, 0x10, 0xab, 0x00, /*15f: mov    %rax,0xab1000(%rcx) */
0x89, 0x91, 0x08, 0x10, 0xab, 0x00,       /*166: mov    %edx,0xab1008(%rcx) */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*16c: mov    0x18(%r12),%rax */
0x4c, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*171: mov    0xab1000(%rax),%r10 */
0x45, 0x85, 0xc0,                         /*178: test   %r8d,%r8d */
0x74, 0x37,                               /*17b: je     1b4 <op_argary+0x1b4> */
0x49, 0x8b, 0x4a, 0x28,                   /*17d: mov    0x28(%r10),%rcx */
0x49, 0x83, 0xe5, 0x3f,                   /*181: and    $0x3f,%r13 */
0x49, 0xf7, 0xdd,                         /*185: neg    %r13 */
0x4c, 0x89, 0xca,                         /*188: mov    %r9,%rdx */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*18b: nopl   0x0(%rax,%rax,1) */
0x48, 0x89, 0xee,                         /*190: mov    %rbp,%rsi */
0x48, 0x83, 0xc2, 0x20,                   /*193: add    $0x20,%rdx */
0x48, 0x8b, 0x3e,                         /*197: mov    (%rsi),%rdi */
0x48, 0x8b, 0x6e, 0x08,                   /*19a: mov    0x8(%rsi),%rbp */
0x48, 0x89, 0x69, 0x08,                   /*19e: mov    %rbp,0x8(%rcx) */
0x48, 0x89, 0x39,                         /*1a2: mov    %rdi,(%rcx) */
0x48, 0x8d, 0x49, 0x10,                   /*1a5: lea    0x10(%rcx),%rcx */
0x49, 0xff, 0xc5,                         /*1a9: inc    %r13 */
0x48, 0x89, 0xd5,                         /*1ac: mov    %rdx,%rbp */
0x48, 0x89, 0xf2,                         /*1af: mov    %rsi,%rdx */
0x75, 0xdc,                               /*1b2: jne    190 <op_argary+0x190> */
0x48, 0x8b, 0x44, 0x24, 0x08,             /*1b4: mov    0x8(%rsp),%rax */
0x85, 0xc0,                               /*1b9: test   %eax,%eax */
0x44, 0x8b, 0x74, 0x24, 0x30,             /*1bb: mov    0x30(%rsp),%r14d */
0x4c, 0x8b, 0x5c, 0x24, 0x10,             /*1c0: mov    0x10(%rsp),%r11 */
0x7e, 0x34,                               /*1c5: jle    1fb <op_argary+0x1fb> */
0x4c, 0x89, 0xd9,                         /*1c7: mov    %r11,%rcx */
0x48, 0xc1, 0xe1, 0x04,                   /*1ca: shl    $0x4,%rcx */
0x49, 0x03, 0x4a, 0x28,                   /*1ce: add    0x28(%r10),%rcx */
0x48, 0x63, 0xd0,                         /*1d2: movslq %eax,%rdx */
0x48, 0xf7, 0xda,                         /*1d5: neg    %rdx */
0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*1d8: nopl   0x0(%rax,%rax,1) */
0x49, 0x8b, 0x37,                         /*1e0: mov    (%r15),%rsi */
0x49, 0x8b, 0x7f, 0x08,                   /*1e3: mov    0x8(%r15),%rdi */
0x4d, 0x8d, 0x7f, 0x10,                   /*1e7: lea    0x10(%r15),%r15 */
0x48, 0x89, 0x79, 0x08,                   /*1eb: mov    %rdi,0x8(%rcx) */
0x48, 0x89, 0x31,                         /*1ef: mov    %rsi,(%rcx) */
0x48, 0x8d, 0x49, 0x10,                   /*1f2: lea    0x10(%rcx),%rcx */
0x48, 0xff, 0xc2,                         /*1f6: inc    %rdx */
0x75, 0xe5,                               /*1f9: jne    1e0 <op_argary+0x1e0> */
0x48, 0x8b, 0x6c, 0x24, 0x20,             /*1fb: mov    0x20(%rsp),%rbp */
0x85, 0xed,                               /*200: test   %ebp,%ebp */
0x74, 0x47,                               /*202: je     24b <op_argary+0x24b> */
0x4c, 0x89, 0xd9,                         /*204: mov    %r11,%rcx */
0x48, 0xc1, 0xe1, 0x04,                   /*207: shl    $0x4,%rcx */
0x4a, 0x8d, 0x4c, 0x09, 0x20,             /*20b: lea    0x20(%rcx,%r9,1),%rcx */
0x48, 0x63, 0xd0,                         /*210: movslq %eax,%rdx */
0x4c, 0x01, 0xda,                         /*213: add    %r11,%rdx */
0x48, 0xc1, 0xe2, 0x04,                   /*216: shl    $0x4,%rdx */
0x49, 0x03, 0x52, 0x28,                   /*21a: add    0x28(%r10),%rdx */
0x48, 0x83, 0xe3, 0x1f,                   /*21e: and    $0x1f,%rbx */
0x48, 0xf7, 0xdb,                         /*222: neg    %rbx */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*225: data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x31,                         /*230: mov    (%rcx),%rsi */
0x48, 0x8b, 0x79, 0x08,                   /*233: mov    0x8(%rcx),%rdi */
0x48, 0x8d, 0x49, 0x10,                   /*237: lea    0x10(%rcx),%rcx */
0x48, 0x89, 0x7a, 0x08,                   /*23b: mov    %rdi,0x8(%rdx) */
0x48, 0x89, 0x32,                         /*23f: mov    %rsi,(%rdx) */
0x48, 0x8d, 0x52, 0x10,                   /*242: lea    0x10(%rdx),%rdx */
0x48, 0xff, 0xc3,                         /*246: inc    %rbx */
0x75, 0xe5,                               /*249: jne    230 <op_argary+0x230> */
0x8b, 0x4c, 0x24, 0x1c,                   /*24b: mov    0x1c(%rsp),%ecx */
0x41, 0x89, 0x4a, 0x18,                   /*24f: mov    %ecx,0x18(%r10) */
0xeb, 0x33,                               /*253: jmp    288 <op_argary+0x288> */
0x4d, 0x8b, 0x7c, 0x24, 0x18,             /*255: mov    0x18(%r12),%r15 */
0x49, 0x8b, 0x7c, 0x24, 0x58,             /*25a: mov    0x58(%r12),%rdi */
0x42, 0x8d, 0x34, 0x09,                   /*25f: lea    (%rcx,%r9,1),%esi */
0x48, 0x89, 0xea,                         /*263: mov    %rbp,%rdx */
0x4d, 0x89, 0xcd,                         /*266: mov    %r9,%r13 */
0x4c, 0x89, 0xc3,                         /*269: mov    %r8,%rbx */
0x48, 0x89, 0xcd,                         /*26c: mov    %rcx,%rbp */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*26f: callq  274 <op_argary+0x274> */
0x49, 0x89, 0xd9,                         /*274: mov    %rbx,%r9 */
0x4d, 0x89, 0xe8,                         /*277: mov    %r13,%r8 */
0x49, 0x89, 0x87, 0x00, 0x10, 0xab, 0x00, /*27a: mov    %rax,0xab1000(%r15) */
0x41, 0x89, 0x97, 0x08, 0x10, 0xab, 0x00, /*281: mov    %edx,0xab1008(%r15) */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*288: mov    0x18(%r12),%rax */
0x45, 0x01, 0xf0,                         /*28d: add    %r14d,%r8d */
0x42, 0x8d, 0x4c, 0x05, 0x01,             /*290: lea    0x1(%rbp,%r8,1),%ecx */
0x48, 0x63, 0xc9,                         /*295: movslq %ecx,%rcx */
0x48, 0xc1, 0xe1, 0x04,                   /*298: shl    $0x4,%rcx */
0x49, 0x8b, 0x14, 0x09,                   /*29c: mov    (%r9,%rcx,1),%rdx */
0x49, 0x8b, 0x4c, 0x09, 0x08,             /*2a0: mov    0x8(%r9,%rcx,1),%rcx */
0x48, 0x89, 0x88, 0x18, 0x10, 0xab, 0x00, /*2a5: mov    %rcx,0xab1018(%rax) */
0x48, 0x89, 0x90, 0x10, 0x10, 0xab, 0x00, /*2ac: mov    %rdx,0xab1010(%rax) */
0x41, 0x8b, 0x44, 0x24, 0x50,             /*2b3: mov    0x50(%r12),%eax */
0x49, 0x8b, 0x4c, 0x24, 0x58,             /*2b8: mov    0x58(%r12),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*2bd: mov    %eax,0xdc(%rcx) */
0x4c, 0x89, 0xe7,                         /*2c3: mov    %r12,%rdi */
0x48, 0x83, 0xc4, 0x38,                   /*2c6: add    $0x38,%rsp */
0x5b,                                     /*2ca: pop    %rbx */
0x41, 0x5c,                               /*2cb: pop    %r12 */
0x41, 0x5d,                               /*2cd: pop    %r13 */
0x41, 0x5e,                               /*2cf: pop    %r14 */
0x41, 0x5f,                               /*2d1: pop    %r15 */
0x5d,                                     /*2d3: pop    %rbp */

};
static uint8_t op_argary__rodata[] = {
0x73, 0x75, 0x70, 0x65,                   
0x73, 0x69, 0x64, 0x65,                   

};

static void op_argary_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 1 + 0;
  *((int32_t *)(op + 354)) = a * 16 + 0;
  *((int32_t *)(op + 360)) = a * 16 + 8;
  *((int32_t *)(op + 372)) = a * 16 + 0;
  *((int32_t *)(op + 637)) = a * 16 + 0;
  *((int32_t *)(op + 644)) = a * 16 + 8;
  *((int32_t *)(op + 680)) = a * 16 + 24;
  *((int32_t *)(op + 687)) = a * 16 + 16;
}

static void op_argary_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_argary_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[1, 0, 18..21]]} */
static uint8_t op_enter__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x83, 0xec, 0x58,                   /*a: sub    $0x58,%rsp */
0x48, 0x89, 0xfb,                         /*e: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xab, 0x00,             /*11: mov    $0xab0000,%eax */
0x41, 0x89, 0xc0,                         /*16: mov    %eax,%r8d */
0x44, 0x89, 0xc2,                         /*19: mov    %r8d,%edx */
0xc1, 0xea, 0x12,                         /*1c: shr    $0x12,%edx */
0x83, 0xe2, 0x1f,                         /*1f: and    $0x1f,%edx */
0x48, 0x89, 0x54, 0x24, 0x48,             /*22: mov    %rdx,0x48(%rsp) */
0x44, 0x89, 0xc1,                         /*27: mov    %r8d,%ecx */
0xc1, 0xe9, 0x0d,                         /*2a: shr    $0xd,%ecx */
0x83, 0xe1, 0x1f,                         /*2d: and    $0x1f,%ecx */
0x48, 0x89, 0x4c, 0x24, 0x28,             /*30: mov    %rcx,0x28(%rsp) */
0x44, 0x89, 0xc5,                         /*35: mov    %r8d,%ebp */
0xc1, 0xed, 0x0c,                         /*38: shr    $0xc,%ebp */
0x83, 0xe5, 0x01,                         /*3b: and    $0x1,%ebp */
0x45, 0x89, 0xc5,                         /*3e: mov    %r8d,%r13d */
0x41, 0xc1, 0xed, 0x07,                   /*41: shr    $0x7,%r13d */
0x4c, 0x8b, 0x7b, 0x18,                   /*45: mov    0x18(%rbx),%r15 */
0x48, 0x8b, 0x7b, 0x58,                   /*49: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*4d: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*51: mov    0x20(%rax),%rax */
0x48, 0x63, 0x70, 0x40,                   /*55: movslq 0x40(%rax),%rsi */
0x48, 0x89, 0x74, 0x24, 0x40,             /*59: mov    %rsi,0x40(%rsp) */
0x8d, 0x04, 0x0a,                         /*5e: lea    (%rdx,%rcx,1),%eax */
0x48, 0x89, 0x44, 0x24, 0x50,             /*61: mov    %rax,0x50(%rsp) */
0x48, 0x85, 0xf6,                         /*66: test   %rsi,%rsi */
0x48, 0x8d, 0x4e, 0x01,                   /*69: lea    0x1(%rsi),%rcx */
0xb8, 0x02, 0x00, 0x00, 0x00,             /*6d: mov    $0x2,%eax */
0x48, 0x0f, 0x49, 0xc1,                   /*72: cmovns %rcx,%rax */
0x48, 0xc1, 0xe0, 0x04,                   /*76: shl    $0x4,%rax */
0x49, 0x8d, 0x34, 0x07,                   /*7a: lea    (%r15,%rax,1),%rsi */
0x41, 0x8b, 0x54, 0x07, 0x08,             /*7e: mov    0x8(%r15,%rax,1),%edx */
0x83, 0xfa, 0x0d,                         /*83: cmp    $0xd,%edx */
0x74, 0x43,                               /*86: je     cb <op_enter+0xcb> */
0x85, 0xd2,                               /*88: test   %edx,%edx */
0x75, 0x05,                               /*8a: jne    91 <op_enter+0x91> */
0x83, 0x3e, 0x00,                         /*8c: cmpl   $0x0,(%rsi) */
0x74, 0x3a,                               /*8f: je     cb <op_enter+0xcb> */
0x48, 0x89, 0x5c, 0x24, 0x30,             /*91: mov    %rbx,0x30(%rsp) */
0x4d, 0x8d, 0x74, 0x07, 0x08,             /*96: lea    0x8(%r15,%rax,1),%r14 */
0x48, 0x89, 0xf3,                         /*9b: mov    %rsi,%rbx */
0x48, 0x8b, 0x33,                         /*9e: mov    (%rbx),%rsi */
0xb9, 0x0d, 0x00, 0x00, 0x00,             /*a1: mov    $0xd,%ecx */
0x45, 0x89, 0xc4,                         /*a6: mov    %r8d,%r12d */
0x41, 0xb8, 0x00, 0x00, 0x00, 0x00,       /*a9: mov    $0x0,%r8d */
0x41, 0xb9, 0x00, 0x00, 0x00, 0x00,       /*af: mov    $0x0,%r9d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*b5: callq  ba <op_enter+0xba> */
0x45, 0x89, 0xe0,                         /*ba: mov    %r12d,%r8d */
0x48, 0x89, 0xde,                         /*bd: mov    %rbx,%rsi */
0x48, 0x89, 0x06,                         /*c0: mov    %rax,(%rsi) */
0x41, 0x89, 0x16,                         /*c3: mov    %edx,(%r14) */
0x48, 0x8b, 0x5c, 0x24, 0x30,             /*c6: mov    0x30(%rsp),%rbx */
0x48, 0x89, 0x74, 0x24, 0x18,             /*cb: mov    %rsi,0x18(%rsp) */
0x45, 0x89, 0xee,                         /*d0: mov    %r13d,%r14d */
0x41, 0x83, 0xe6, 0x1f,                   /*d3: and    $0x1f,%r14d */
0x48, 0x8b, 0x44, 0x24, 0x50,             /*d7: mov    0x50(%rsp),%rax */
0x48, 0x89, 0x6c, 0x24, 0x20,             /*dc: mov    %rbp,0x20(%rsp) */
0x44, 0x8d, 0x24, 0x28,                   /*e1: lea    (%rax,%rbp,1),%r12d */
0x49, 0x83, 0xc7, 0x10,                   /*e5: add    $0x10,%r15 */
0x4c, 0x89, 0x7c, 0x24, 0x38,             /*e9: mov    %r15,0x38(%rsp) */
0xbf, 0x00, 0x00, 0x00, 0x00,             /*ee: mov    $0x0,%edi */
0x31, 0xc0,                               /*f3: xor    %eax,%eax */
0x44, 0x89, 0xc2,                         /*f5: mov    %r8d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*f8: callq  fd <op_enter+0xfd> */
0x4c, 0x8b, 0x4c, 0x24, 0x40,             /*fd: mov    0x40(%rsp),%r9 */
0x45, 0x85, 0xc9,                         /*102: test   %r9d,%r9d */
0x4d, 0x89, 0xfa,                         /*105: mov    %r15,%r10 */
0x48, 0x89, 0xdd,                         /*108: mov    %rbx,%rbp */
0x79, 0x27,                               /*10b: jns    134 <op_enter+0x134> */
0x48, 0x8b, 0x45, 0x18,                   /*10d: mov    0x18(%rbp),%rax */
0x48, 0x8b, 0x7d, 0x58,                   /*111: mov    0x58(%rbp),%rdi */
0x48, 0x8b, 0x70, 0x10,                   /*115: mov    0x10(%rax),%rsi */
0x49, 0x89, 0xef,                         /*119: mov    %rbp,%r15 */
0x48, 0x8b, 0x6e, 0x28,                   /*11c: mov    0x28(%rsi),%rbp */
0x8b, 0x5e, 0x18,                         /*120: mov    0x18(%rsi),%ebx */
0x8b, 0x50, 0x18,                         /*123: mov    0x18(%rax),%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*126: callq  12b <op_enter+0x12b> */
0x49, 0x89, 0xea,                         /*12b: mov    %rbp,%r10 */
0x4c, 0x89, 0xfd,                         /*12e: mov    %r15,%rbp */
0x49, 0x89, 0xd9,                         /*131: mov    %rbx,%r9 */
0x47, 0x8d, 0x04, 0x34,                   /*134: lea    (%r12,%r14,1),%r8d */
0x48, 0x8b, 0x7d, 0x58,                   /*138: mov    0x58(%rbp),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*13c: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*140: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x48, 0x08,                   /*144: mov    0x8(%rax),%rcx */
0x48, 0x85, 0xc9,                         /*148: test   %rcx,%rcx */
0x74, 0x39,                               /*14b: je     186 <op_enter+0x186> */
0xf6, 0x41, 0x02, 0x08,                   /*14d: testb  $0x8,0x2(%rcx) */
0x74, 0x33,                               /*151: je     186 <op_enter+0x186> */
0x45, 0x85, 0xc9,                         /*153: test   %r9d,%r9d */
0x78, 0x74,                               /*156: js     1cc <op_enter+0x1cc> */
0x48, 0x8b, 0x4c, 0x24, 0x48,             /*158: mov    0x48(%rsp),%rcx */
0x42, 0x8d, 0x34, 0x31,                   /*15d: lea    (%rcx,%r14,1),%esi */
0x41, 0x39, 0xf1,                         /*161: cmp    %esi,%r9d */
0x7c, 0x0e,                               /*164: jl     174 <op_enter+0x174> */
0x48, 0x8b, 0x4c, 0x24, 0x20,             /*166: mov    0x20(%rsp),%rcx */
0x85, 0xc9,                               /*16b: test   %ecx,%ecx */
0x75, 0x5d,                               /*16d: jne    1cc <op_enter+0x1cc> */
0x45, 0x39, 0xc1,                         /*16f: cmp    %r8d,%r9d */
0x7e, 0x58,                               /*172: jle    1cc <op_enter+0x1cc> */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*174: callq  179 <op_enter+0x179> */
0x48, 0x89, 0xef,                         /*179: mov    %rbp,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*17c: callq  181 <op_enter+0x181> */
0xe9, 0x67, 0x05, 0x00, 0x00,             /*181: jmpq   6ed <op_enter+0x6ed> */
0x41, 0x83, 0xf8, 0x02,                   /*186: cmp    $0x2,%r8d */
0x7c, 0x40,                               /*18a: jl     1cc <op_enter+0x1cc> */
0x41, 0x83, 0xf9, 0x01,                   /*18c: cmp    $0x1,%r9d */
0x75, 0x3a,                               /*190: jne    1cc <op_enter+0x1cc> */
0x41, 0xb9, 0x01, 0x00, 0x00, 0x00,       /*192: mov    $0x1,%r9d */
0x41, 0x83, 0x7a, 0x08, 0x0e,             /*198: cmpl   $0xe,0x8(%r10) */
0x75, 0x2d,                               /*19d: jne    1cc <op_enter+0x1cc> */
0x49, 0x8b, 0x32,                         /*19f: mov    (%r10),%rsi */
0xba, 0x0e, 0x00, 0x00, 0x00,             /*1a2: mov    $0xe,%edx */
0x4d, 0x89, 0xd7,                         /*1a7: mov    %r10,%r15 */
0x4c, 0x89, 0xc3,                         /*1aa: mov    %r8,%rbx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1ad: callq  1b2 <op_enter+0x1b2> */
0x49, 0x89, 0xd8,                         /*1b2: mov    %rbx,%r8 */
0x49, 0x8b, 0x07,                         /*1b5: mov    (%r15),%rax */
0x44, 0x8b, 0x48, 0x18,                   /*1b8: mov    0x18(%rax),%r9d */
0x4c, 0x8b, 0x50, 0x28,                   /*1bc: mov    0x28(%rax),%r10 */
0x48, 0x8b, 0x45, 0x58,                   /*1c0: mov    0x58(%rbp),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*1c4: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*1c8: mov    0x20(%rax),%rax */
0x44, 0x89, 0x40, 0x40,                   /*1cc: mov    %r8d,0x40(%rax) */
0x45, 0x39, 0xc1,                         /*1d0: cmp    %r8d,%r9d */
0x0f, 0x8d, 0xe7, 0x00, 0x00, 0x00,       /*1d3: jge    2c0 <op_enter+0x2c0> */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*1d9: mov    0x48(%rsp),%rax */
0x46, 0x8d, 0x2c, 0x30,                   /*1de: lea    (%rax,%r14,1),%r13d */
0x45, 0x39, 0xe9,                         /*1e2: cmp    %r13d,%r9d */
0x44, 0x89, 0xf0,                         /*1e5: mov    %r14d,%eax */
0x48, 0x8b, 0x74, 0x24, 0x18,             /*1e8: mov    0x18(%rsp),%rsi */
0x7d, 0x0f,                               /*1ed: jge    1fe <op_enter+0x1fe> */
0x31, 0xc9,                               /*1ef: xor    %ecx,%ecx */
0x44, 0x89, 0xc8,                         /*1f1: mov    %r9d,%eax */
0x48, 0x8b, 0x54, 0x24, 0x48,             /*1f4: mov    0x48(%rsp),%rdx */
0x29, 0xd0,                               /*1f9: sub    %edx,%eax */
0x0f, 0x4e, 0xc1,                         /*1fb: cmovle %ecx,%eax */
0x41, 0xff, 0xc0,                         /*1fe: inc    %r8d */
0x48, 0x8b, 0x4d, 0x18,                   /*201: mov    0x18(%rbp),%rcx */
0x49, 0xc1, 0xe0, 0x04,                   /*205: shl    $0x4,%r8 */
0x48, 0x8b, 0x16,                         /*209: mov    (%rsi),%rdx */
0x48, 0x8b, 0x76, 0x08,                   /*20c: mov    0x8(%rsi),%rsi */
0x4a, 0x89, 0x74, 0x01, 0x08,             /*210: mov    %rsi,0x8(%rcx,%r8,1) */
0x4a, 0x89, 0x14, 0x01,                   /*215: mov    %rdx,(%rcx,%r8,1) */
0x41, 0x8d, 0x49, 0x01,                   /*219: lea    0x1(%r9),%ecx */
0x48, 0x63, 0xc9,                         /*21d: movslq %ecx,%rcx */
0x48, 0x8b, 0x55, 0x18,                   /*220: mov    0x18(%rbp),%rdx */
0x48, 0xc1, 0xe1, 0x04,                   /*224: shl    $0x4,%rcx */
0xc7, 0x44, 0x0a, 0x08, 0x00, 0x00, 0x00, 0x00,/*228: movl   $0x0,0x8(%rdx,%rcx,1) */
0x48, 0x8b, 0x55, 0x18,                   /*230: mov    0x18(%rbp),%rdx */
0x49, 0x89, 0xe8,                         /*234: mov    %rbp,%r8 */
0xc7, 0x04, 0x0a, 0x00, 0x00, 0x00, 0x00, /*237: movl   $0x0,(%rdx,%rcx,1) */
0x4c, 0x39, 0x54, 0x24, 0x38,             /*23e: cmp    %r10,0x38(%rsp) */
0x0f, 0x84, 0x6b, 0x01, 0x00, 0x00,       /*243: je     3b4 <op_enter+0x3b4> */
0x49, 0x8b, 0x48, 0x18,                   /*249: mov    0x18(%r8),%rcx */
0x48, 0x8d, 0x69, 0x10,                   /*24d: lea    0x10(%rcx),%rbp */
0x4c, 0x39, 0xd5,                         /*251: cmp    %r10,%rbp */
0x0f, 0x86, 0x16, 0x01, 0x00, 0x00,       /*254: jbe    370 <op_enter+0x370> */
0x44, 0x89, 0xce,                         /*25a: mov    %r9d,%esi */
0x29, 0xc6,                               /*25d: sub    %eax,%esi */
0x48, 0x63, 0xf6,                         /*25f: movslq %esi,%rsi */
0x48, 0xc1, 0xe6, 0x04,                   /*262: shl    $0x4,%rsi */
0x4c, 0x01, 0xd6,                         /*266: add    %r10,%rsi */
0x48, 0x39, 0xf5,                         /*269: cmp    %rsi,%rbp */
0x0f, 0x83, 0xfe, 0x00, 0x00, 0x00,       /*26c: jae    370 <op_enter+0x370> */
0x44, 0x89, 0xca,                         /*272: mov    %r9d,%edx */
0x29, 0xc2,                               /*275: sub    %eax,%edx */
0x0f, 0x84, 0x37, 0x01, 0x00, 0x00,       /*277: je     3b4 <op_enter+0x3b4> */
0x48, 0x63, 0xd2,                         /*27d: movslq %edx,%rdx */
0x48, 0x89, 0xd6,                         /*280: mov    %rdx,%rsi */
0x48, 0xc1, 0xe6, 0x04,                   /*283: shl    $0x4,%rsi */
0x48, 0x01, 0xf1,                         /*287: add    %rsi,%rcx */
0x4a, 0x8d, 0x74, 0x16, 0xf0,             /*28a: lea    -0x10(%rsi,%r10,1),%rsi */
0x48, 0xf7, 0xda,                         /*28f: neg    %rdx */
0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*292: data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x3e,                         /*2a0: mov    (%rsi),%rdi */
0x48, 0x8b, 0x6e, 0x08,                   /*2a3: mov    0x8(%rsi),%rbp */
0x48, 0x89, 0x69, 0x08,                   /*2a7: mov    %rbp,0x8(%rcx) */
0x48, 0x89, 0x39,                         /*2ab: mov    %rdi,(%rcx) */
0x48, 0x83, 0xc1, 0xf0,                   /*2ae: add    $0xfffffffffffffff0,%rcx */
0x48, 0x83, 0xc6, 0xf0,                   /*2b2: add    $0xfffffffffffffff0,%rsi */
0x48, 0xff, 0xc2,                         /*2b6: inc    %rdx */
0x75, 0xe5,                               /*2b9: jne    2a0 <op_enter+0x2a0> */
0xe9, 0xf4, 0x00, 0x00, 0x00,             /*2bb: jmpq   3b4 <op_enter+0x3b4> */
0x48, 0x89, 0x6c, 0x24, 0x30,             /*2c0: mov    %rbp,0x30(%rsp) */
0x4c, 0x39, 0x54, 0x24, 0x38,             /*2c5: cmp    %r10,0x38(%rsp) */
0x0f, 0x84, 0xb4, 0x01, 0x00, 0x00,       /*2ca: je     484 <op_enter+0x484> */
0x41, 0x8d, 0x40, 0x01,                   /*2d0: lea    0x1(%r8),%eax */
0x48, 0x8b, 0x7c, 0x24, 0x30,             /*2d4: mov    0x30(%rsp),%rdi */
0x48, 0x8b, 0x4f, 0x18,                   /*2d9: mov    0x18(%rdi),%rcx */
0x48, 0xc1, 0xe0, 0x04,                   /*2dd: shl    $0x4,%rax */
0x48, 0x8b, 0x74, 0x24, 0x18,             /*2e1: mov    0x18(%rsp),%rsi */
0x48, 0x8b, 0x16,                         /*2e6: mov    (%rsi),%rdx */
0x48, 0x8b, 0x76, 0x08,                   /*2e9: mov    0x8(%rsi),%rsi */
0x48, 0x89, 0x74, 0x01, 0x08,             /*2ed: mov    %rsi,0x8(%rcx,%rax,1) */
0x48, 0x89, 0x14, 0x01,                   /*2f2: mov    %rdx,(%rcx,%rax,1) */
0x48, 0x8b, 0x47, 0x18,                   /*2f6: mov    0x18(%rdi),%rax */
0x48, 0x8d, 0x48, 0x10,                   /*2fa: lea    0x10(%rax),%rcx */
0x48, 0x8b, 0x54, 0x24, 0x50,             /*2fe: mov    0x50(%rsp),%rdx */
0x89, 0xd2,                               /*303: mov    %edx,%edx */
0x4c, 0x39, 0xd1,                         /*305: cmp    %r10,%rcx */
0x0f, 0x86, 0x32, 0x01, 0x00, 0x00,       /*308: jbe    440 <op_enter+0x440> */
0x48, 0xc1, 0xe2, 0x04,                   /*30e: shl    $0x4,%rdx */
0x4c, 0x01, 0xd2,                         /*312: add    %r10,%rdx */
0x48, 0x39, 0xd1,                         /*315: cmp    %rdx,%rcx */
0x0f, 0x83, 0x22, 0x01, 0x00, 0x00,       /*318: jae    440 <op_enter+0x440> */
0x48, 0x8b, 0x4c, 0x24, 0x50,             /*31e: mov    0x50(%rsp),%rcx */
0x85, 0xc9,                               /*323: test   %ecx,%ecx */
0x0f, 0x84, 0x59, 0x01, 0x00, 0x00,       /*325: je     484 <op_enter+0x484> */
0x48, 0x8b, 0x4c, 0x24, 0x50,             /*32b: mov    0x50(%rsp),%rcx */
0x48, 0x89, 0xca,                         /*330: mov    %rcx,%rdx */
0x48, 0xc1, 0xe1, 0x04,                   /*333: shl    $0x4,%rcx */
0x48, 0x01, 0xc8,                         /*337: add    %rcx,%rax */
0x4a, 0x8d, 0x4c, 0x11, 0xf0,             /*33a: lea    -0x10(%rcx,%r10,1),%rcx */
0x48, 0xf7, 0xda,                         /*33f: neg    %rdx */
0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*342: data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x31,                         /*350: mov    (%rcx),%rsi */
0x48, 0x8b, 0x79, 0x08,                   /*353: mov    0x8(%rcx),%rdi */
0x48, 0x89, 0x78, 0x08,                   /*357: mov    %rdi,0x8(%rax) */
0x48, 0x89, 0x30,                         /*35b: mov    %rsi,(%rax) */
0x48, 0x83, 0xc0, 0xf0,                   /*35e: add    $0xfffffffffffffff0,%rax */
0x48, 0x83, 0xc1, 0xf0,                   /*362: add    $0xfffffffffffffff0,%rcx */
0x48, 0xff, 0xc2,                         /*366: inc    %rdx */
0x75, 0xe5,                               /*369: jne    350 <op_enter+0x350> */
0xe9, 0x14, 0x01, 0x00, 0x00,             /*36b: jmpq   484 <op_enter+0x484> */
0x4c, 0x39, 0xd5,                         /*370: cmp    %r10,%rbp */
0x74, 0x3f,                               /*373: je     3b4 <op_enter+0x3b4> */
0x44, 0x89, 0xce,                         /*375: mov    %r9d,%esi */
0x29, 0xc6,                               /*378: sub    %eax,%esi */
0x74, 0x38,                               /*37a: je     3b4 <op_enter+0x3b4> */
0x48, 0x63, 0xf6,                         /*37c: movslq %esi,%rsi */
0x48, 0xf7, 0xde,                         /*37f: neg    %rsi */
0x4c, 0x89, 0xd7,                         /*382: mov    %r10,%rdi */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*385: data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x89, 0xea,                         /*390: mov    %rbp,%rdx */
0x48, 0x83, 0xc1, 0x20,                   /*393: add    $0x20,%rcx */
0x48, 0x8b, 0x2f,                         /*397: mov    (%rdi),%rbp */
0x48, 0x8b, 0x5f, 0x08,                   /*39a: mov    0x8(%rdi),%rbx */
0x48, 0x8d, 0x7f, 0x10,                   /*39e: lea    0x10(%rdi),%rdi */
0x48, 0x89, 0x5a, 0x08,                   /*3a2: mov    %rbx,0x8(%rdx) */
0x48, 0x89, 0x2a,                         /*3a6: mov    %rbp,(%rdx) */
0x48, 0xff, 0xc6,                         /*3a9: inc    %rsi */
0x48, 0x89, 0xcd,                         /*3ac: mov    %rcx,%rbp */
0x48, 0x89, 0xd1,                         /*3af: mov    %rdx,%rcx */
0x75, 0xdc,                               /*3b2: jne    390 <op_enter+0x390> */
0x85, 0xc0,                               /*3b4: test   %eax,%eax */
0x4c, 0x89, 0xc5,                         /*3b6: mov    %r8,%rbp */
0x0f, 0x84, 0x3f, 0x02, 0x00, 0x00,       /*3b9: je     5fe <op_enter+0x5fe> */
0x41, 0xff, 0xc4,                         /*3bf: inc    %r12d */
0x48, 0x8b, 0x55, 0x18,                   /*3c2: mov    0x18(%rbp),%rdx */
0x49, 0xc1, 0xe4, 0x04,                   /*3c6: shl    $0x4,%r12 */
0x49, 0x01, 0xd4,                         /*3ca: add    %rdx,%r12 */
0x44, 0x89, 0xc9,                         /*3cd: mov    %r9d,%ecx */
0x29, 0xc1,                               /*3d0: sub    %eax,%ecx */
0x48, 0x63, 0xf1,                         /*3d2: movslq %ecx,%rsi */
0x48, 0x89, 0xf1,                         /*3d5: mov    %rsi,%rcx */
0x48, 0xc1, 0xe1, 0x04,                   /*3d8: shl    $0x4,%rcx */
0x4c, 0x01, 0xd1,                         /*3dc: add    %r10,%rcx */
0x48, 0x98,                               /*3df: cltq */
0x49, 0x39, 0xcc,                         /*3e1: cmp    %rcx,%r12 */
0x0f, 0x86, 0xe6, 0x01, 0x00, 0x00,       /*3e4: jbe    5d0 <op_enter+0x5d0> */
0x48, 0x01, 0xc6,                         /*3ea: add    %rax,%rsi */
0x48, 0xc1, 0xe6, 0x04,                   /*3ed: shl    $0x4,%rsi */
0x49, 0x8d, 0x3c, 0x32,                   /*3f1: lea    (%r10,%rsi,1),%rdi */
0x49, 0x39, 0xfc,                         /*3f5: cmp    %rdi,%r12 */
0x0f, 0x83, 0xd2, 0x01, 0x00, 0x00,       /*3f8: jae    5d0 <op_enter+0x5d0> */
0x48, 0x8b, 0x4c, 0x24, 0x50,             /*3fe: mov    0x50(%rsp),%rcx */
0x48, 0x8b, 0x7c, 0x24, 0x20,             /*403: mov    0x20(%rsp),%rdi */
0x8d, 0x4c, 0x0f, 0x01,                   /*408: lea    0x1(%rdi,%rcx,1),%ecx */
0x48, 0x01, 0xc1,                         /*40c: add    %rax,%rcx */
0x48, 0xc1, 0xe1, 0x04,                   /*40f: shl    $0x4,%rcx */
0x48, 0x8d, 0x4c, 0x11, 0xf0,             /*413: lea    -0x10(%rcx,%rdx,1),%rcx */
0x4a, 0x8d, 0x54, 0x16, 0xf0,             /*418: lea    -0x10(%rsi,%r10,1),%rdx */
0x48, 0xf7, 0xd8,                         /*41d: neg    %rax */
0x48, 0x8b, 0x32,                         /*420: mov    (%rdx),%rsi */
0x48, 0x8b, 0x7a, 0x08,                   /*423: mov    0x8(%rdx),%rdi */
0x48, 0x89, 0x79, 0x08,                   /*427: mov    %rdi,0x8(%rcx) */
0x48, 0x89, 0x31,                         /*42b: mov    %rsi,(%rcx) */
0x48, 0x83, 0xc1, 0xf0,                   /*42e: add    $0xfffffffffffffff0,%rcx */
0x48, 0x83, 0xc2, 0xf0,                   /*432: add    $0xfffffffffffffff0,%rdx */
0x48, 0xff, 0xc0,                         /*436: inc    %rax */
0x75, 0xe5,                               /*439: jne    420 <op_enter+0x420> */
0xe9, 0xbe, 0x01, 0x00, 0x00,             /*43b: jmpq   5fe <op_enter+0x5fe> */
0x4c, 0x39, 0xd1,                         /*440: cmp    %r10,%rcx */
0x74, 0x3f,                               /*443: je     484 <op_enter+0x484> */
0x48, 0x8b, 0x54, 0x24, 0x50,             /*445: mov    0x50(%rsp),%rdx */
0x85, 0xd2,                               /*44a: test   %edx,%edx */
0x74, 0x36,                               /*44c: je     484 <op_enter+0x484> */
0x48, 0x8b, 0x54, 0x24, 0x50,             /*44e: mov    0x50(%rsp),%rdx */
0x48, 0xf7, 0xda,                         /*453: neg    %rdx */
0x4c, 0x89, 0xd6,                         /*456: mov    %r10,%rsi */
0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00, /*459: nopl   0x0(%rax) */
0x48, 0x89, 0xcf,                         /*460: mov    %rcx,%rdi */
0x48, 0x83, 0xc0, 0x20,                   /*463: add    $0x20,%rax */
0x48, 0x8b, 0x0e,                         /*467: mov    (%rsi),%rcx */
0x48, 0x8b, 0x6e, 0x08,                   /*46a: mov    0x8(%rsi),%rbp */
0x48, 0x8d, 0x76, 0x10,                   /*46e: lea    0x10(%rsi),%rsi */
0x48, 0x89, 0x6f, 0x08,                   /*472: mov    %rbp,0x8(%rdi) */
0x48, 0x89, 0x0f,                         /*476: mov    %rcx,(%rdi) */
0x48, 0xff, 0xc2,                         /*479: inc    %rdx */
0x48, 0x89, 0xc1,                         /*47c: mov    %rax,%rcx */
0x48, 0x89, 0xf8,                         /*47f: mov    %rdi,%rax */
0x75, 0xdc,                               /*482: jne    460 <op_enter+0x460> */
0x31, 0xdb,                               /*484: xor    %ebx,%ebx */
0x48, 0x8b, 0x44, 0x24, 0x20,             /*486: mov    0x20(%rsp),%rax */
0x85, 0xc0,                               /*48b: test   %eax,%eax */
0x74, 0x76,                               /*48d: je     505 <op_enter+0x505> */
0x48, 0x8b, 0x44, 0x24, 0x50,             /*48f: mov    0x50(%rsp),%rax */
0x48, 0x89, 0xc1,                         /*494: mov    %rax,%rcx */
0x42, 0x8d, 0x04, 0x31,                   /*497: lea    (%rcx,%r14,1),%eax */
0x44, 0x89, 0xcb,                         /*49b: mov    %r9d,%ebx */
0x29, 0xc3,                               /*49e: sub    %eax,%ebx */
0x8d, 0x69, 0x01,                         /*4a0: lea    0x1(%rcx),%ebp */
0x48, 0xc1, 0xe5, 0x04,                   /*4a3: shl    $0x4,%rbp */
0x48, 0x8b, 0x44, 0x24, 0x30,             /*4a7: mov    0x30(%rsp),%rax */
0x48, 0x8b, 0x48, 0x18,                   /*4ac: mov    0x18(%rax),%rcx */
0x48, 0x89, 0x4c, 0x24, 0x08,             /*4b0: mov    %rcx,0x8(%rsp) */
0x48, 0x8b, 0x78, 0x58,                   /*4b5: mov    0x58(%rax),%rdi */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*4b9: mov    0x48(%rsp),%rax */
0x89, 0xc0,                               /*4be: mov    %eax,%eax */
0x48, 0x8b, 0x4c, 0x24, 0x28,             /*4c0: mov    0x28(%rsp),%rcx */
0x89, 0xca,                               /*4c5: mov    %ecx,%edx */
0x48, 0x01, 0xc2,                         /*4c7: add    %rax,%rdx */
0x48, 0xc1, 0xe2, 0x04,                   /*4ca: shl    $0x4,%rdx */
0x4c, 0x01, 0xd2,                         /*4ce: add    %r10,%rdx */
0x89, 0xde,                               /*4d1: mov    %ebx,%esi */
0x4c, 0x89, 0x74, 0x24, 0x10,             /*4d3: mov    %r14,0x10(%rsp) */
0x4c, 0x89, 0x4c, 0x24, 0x40,             /*4d8: mov    %r9,0x40(%rsp) */
0x4d, 0x89, 0xd7,                         /*4dd: mov    %r10,%r15 */
0x4d, 0x89, 0xc6,                         /*4e0: mov    %r8,%r14 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*4e3: callq  4e8 <op_enter+0x4e8> */
0x4d, 0x89, 0xf0,                         /*4e8: mov    %r14,%r8 */
0x4d, 0x89, 0xfa,                         /*4eb: mov    %r15,%r10 */
0x4c, 0x8b, 0x4c, 0x24, 0x40,             /*4ee: mov    0x40(%rsp),%r9 */
0x4c, 0x8b, 0x74, 0x24, 0x10,             /*4f3: mov    0x10(%rsp),%r14 */
0x48, 0x8b, 0x4c, 0x24, 0x08,             /*4f8: mov    0x8(%rsp),%rcx */
0x48, 0x89, 0x04, 0x29,                   /*4fd: mov    %rax,(%rcx,%rbp,1) */
0x89, 0x54, 0x29, 0x08,                   /*501: mov    %edx,0x8(%rcx,%rbp,1) */
0x45, 0x85, 0xf6,                         /*505: test   %r14d,%r14d */
0x48, 0x8b, 0x6c, 0x24, 0x30,             /*508: mov    0x30(%rsp),%rbp */
0x48, 0x8b, 0x7c, 0x24, 0x28,             /*50d: mov    0x28(%rsp),%rdi */
0x0f, 0x84, 0x86, 0x01, 0x00, 0x00,       /*512: je     69e <op_enter+0x69e> */
0x45, 0x29, 0xf1,                         /*518: sub    %r14d,%r9d */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*51b: mov    0x48(%rsp),%rax */
0x41, 0x39, 0xc1,                         /*520: cmp    %eax,%r9d */
0x0f, 0x8e, 0x75, 0x01, 0x00, 0x00,       /*523: jle    69e <op_enter+0x69e> */
0x41, 0xff, 0xc4,                         /*529: inc    %r12d */
0x48, 0x8b, 0x4d, 0x18,                   /*52c: mov    0x18(%rbp),%rcx */
0x49, 0xc1, 0xe4, 0x04,                   /*530: shl    $0x4,%r12 */
0x49, 0x01, 0xcc,                         /*534: add    %rcx,%r12 */
0x48, 0x8b, 0x44, 0x24, 0x50,             /*537: mov    0x50(%rsp),%rax */
0x8d, 0x04, 0x03,                         /*53c: lea    (%rbx,%rax,1),%eax */
0x48, 0x63, 0xd0,                         /*53f: movslq %eax,%rdx */
0x48, 0x89, 0xd0,                         /*542: mov    %rdx,%rax */
0x48, 0xc1, 0xe0, 0x04,                   /*545: shl    $0x4,%rax */
0x4c, 0x01, 0xd0,                         /*549: add    %r10,%rax */
0x44, 0x89, 0xf6,                         /*54c: mov    %r14d,%esi */
0x49, 0x39, 0xc4,                         /*54f: cmp    %rax,%r12 */
0x0f, 0x86, 0x1a, 0x01, 0x00, 0x00,       /*552: jbe    672 <op_enter+0x672> */
0x48, 0x01, 0xf2,                         /*558: add    %rsi,%rdx */
0x48, 0xc1, 0xe2, 0x04,                   /*55b: shl    $0x4,%rdx */
0x4c, 0x01, 0xd2,                         /*55f: add    %r10,%rdx */
0x49, 0x39, 0xd4,                         /*562: cmp    %rdx,%r12 */
0x0f, 0x83, 0x07, 0x01, 0x00, 0x00,       /*565: jae    672 <op_enter+0x672> */
0x48, 0x8b, 0x44, 0x24, 0x50,             /*56b: mov    0x50(%rsp),%rax */
0x48, 0x8b, 0x54, 0x24, 0x20,             /*570: mov    0x20(%rsp),%rdx */
0x8d, 0x44, 0x02, 0x01,                   /*575: lea    0x1(%rdx,%rax,1),%eax */
0x49, 0x83, 0xe5, 0x1f,                   /*579: and    $0x1f,%r13 */
0x4c, 0x01, 0xe8,                         /*57d: add    %r13,%rax */
0x48, 0xc1, 0xe0, 0x04,                   /*580: shl    $0x4,%rax */
0x48, 0x8d, 0x44, 0x08, 0xf0,             /*584: lea    -0x10(%rax,%rcx,1),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x48,             /*589: mov    0x48(%rsp),%rcx */
0x01, 0xcb,                               /*58e: add    %ecx,%ebx */
0x01, 0xfb,                               /*590: add    %edi,%ebx */
0x48, 0x63, 0xcb,                         /*592: movslq %ebx,%rcx */
0x4c, 0x01, 0xe9,                         /*595: add    %r13,%rcx */
0x48, 0xc1, 0xe1, 0x04,                   /*598: shl    $0x4,%rcx */
0x4a, 0x8d, 0x4c, 0x11, 0xf0,             /*59c: lea    -0x10(%rcx,%r10,1),%rcx */
0x49, 0xf7, 0xdd,                         /*5a1: neg    %r13 */
0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*5a4: data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x11,                         /*5b0: mov    (%rcx),%rdx */
0x48, 0x8b, 0x71, 0x08,                   /*5b3: mov    0x8(%rcx),%rsi */
0x48, 0x89, 0x70, 0x08,                   /*5b7: mov    %rsi,0x8(%rax) */
0x48, 0x89, 0x10,                         /*5bb: mov    %rdx,(%rax) */
0x48, 0x83, 0xc0, 0xf0,                   /*5be: add    $0xfffffffffffffff0,%rax */
0x48, 0x83, 0xc1, 0xf0,                   /*5c2: add    $0xfffffffffffffff0,%rcx */
0x49, 0xff, 0xc5,                         /*5c6: inc    %r13 */
0x75, 0xe5,                               /*5c9: jne    5b0 <op_enter+0x5b0> */
0xe9, 0xce, 0x00, 0x00, 0x00,             /*5cb: jmpq   69e <op_enter+0x69e> */
0x49, 0x39, 0xcc,                         /*5d0: cmp    %rcx,%r12 */
0x74, 0x29,                               /*5d3: je     5fe <op_enter+0x5fe> */
0x48, 0xf7, 0xd8,                         /*5d5: neg    %rax */
0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*5d8: nopl   0x0(%rax,%rax,1) */
0x48, 0x8b, 0x11,                         /*5e0: mov    (%rcx),%rdx */
0x48, 0x8b, 0x71, 0x08,                   /*5e3: mov    0x8(%rcx),%rsi */
0x48, 0x8d, 0x49, 0x10,                   /*5e7: lea    0x10(%rcx),%rcx */
0x49, 0x89, 0x74, 0x24, 0x08,             /*5eb: mov    %rsi,0x8(%r12) */
0x49, 0x89, 0x14, 0x24,                   /*5f0: mov    %rdx,(%r12) */
0x4d, 0x8d, 0x64, 0x24, 0x10,             /*5f4: lea    0x10(%r12),%r12 */
0x48, 0xff, 0xc0,                         /*5f9: inc    %rax */
0x75, 0xe2,                               /*5fc: jne    5e0 <op_enter+0x5e0> */
0x48, 0x8b, 0x44, 0x24, 0x20,             /*5fe: mov    0x20(%rsp),%rax */
0x85, 0xc0,                               /*603: test   %eax,%eax */
0x74, 0x2f,                               /*605: je     636 <op_enter+0x636> */
0x48, 0x8b, 0x5c, 0x24, 0x50,             /*607: mov    0x50(%rsp),%rbx */
0xff, 0xc3,                               /*60c: inc    %ebx */
0x48, 0xc1, 0xe3, 0x04,                   /*60e: shl    $0x4,%rbx */
0x4c, 0x8b, 0x7d, 0x18,                   /*612: mov    0x18(%rbp),%r15 */
0x48, 0x8b, 0x7d, 0x58,                   /*616: mov    0x58(%rbp),%rdi */
0x31, 0xf6,                               /*61a: xor    %esi,%esi */
0x49, 0x89, 0xec,                         /*61c: mov    %rbp,%r12 */
0x4c, 0x89, 0xcd,                         /*61f: mov    %r9,%rbp */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*622: callq  627 <op_enter+0x627> */
0x49, 0x89, 0xe9,                         /*627: mov    %rbp,%r9 */
0x4c, 0x89, 0xe5,                         /*62a: mov    %r12,%rbp */
0x49, 0x89, 0x04, 0x1f,                   /*62d: mov    %rax,(%r15,%rbx,1) */
0x41, 0x89, 0x54, 0x1f, 0x08,             /*631: mov    %edx,0x8(%r15,%rbx,1) */
0x48, 0x8b, 0x44, 0x24, 0x28,             /*636: mov    0x28(%rsp),%rax */
0x85, 0xc0,                               /*63b: test   %eax,%eax */
0x48, 0x8b, 0x45, 0x08,                   /*63d: mov    0x8(%rbp),%rax */
0x48, 0x8b, 0x88, 0x98, 0x00, 0x00, 0x00, /*641: mov    0x98(%rax),%rcx */
0x0f, 0x84, 0x8e, 0x00, 0x00, 0x00,       /*648: je     6dc <op_enter+0x6dc> */
0x45, 0x39, 0xe9,                         /*64e: cmp    %r13d,%r9d */
0x0f, 0x8c, 0x85, 0x00, 0x00, 0x00,       /*651: jl     6dc <op_enter+0x6dc> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*657: mov    $0x1,%edx */
0x48, 0x8b, 0x74, 0x24, 0x48,             /*65c: mov    0x48(%rsp),%rsi */
0x29, 0xf2,                               /*661: sub    %esi,%edx */
0x44, 0x29, 0xf2,                         /*663: sub    %r14d,%edx */
0x44, 0x01, 0xca,                         /*666: add    %r9d,%edx */
0x48, 0x63, 0xd2,                         /*669: movslq %edx,%rdx */
0x48, 0x8d, 0x0c, 0x91,                   /*66c: lea    (%rcx,%rdx,4),%rcx */
0xeb, 0x6e,                               /*670: jmp    6e0 <op_enter+0x6e0> */
0x49, 0x39, 0xc4,                         /*672: cmp    %rax,%r12 */
0x74, 0x27,                               /*675: je     69e <op_enter+0x69e> */
0x49, 0x83, 0xe5, 0x1f,                   /*677: and    $0x1f,%r13 */
0x49, 0xf7, 0xdd,                         /*67b: neg    %r13 */
0x66, 0x90,                               /*67e: xchg   %ax,%ax */
0x48, 0x8b, 0x08,                         /*680: mov    (%rax),%rcx */
0x48, 0x8b, 0x50, 0x08,                   /*683: mov    0x8(%rax),%rdx */
0x48, 0x8d, 0x40, 0x10,                   /*687: lea    0x10(%rax),%rax */
0x49, 0x89, 0x54, 0x24, 0x08,             /*68b: mov    %rdx,0x8(%r12) */
0x49, 0x89, 0x0c, 0x24,                   /*690: mov    %rcx,(%r12) */
0x4d, 0x8d, 0x64, 0x24, 0x10,             /*694: lea    0x10(%r12),%r12 */
0x49, 0xff, 0xc5,                         /*699: inc    %r13 */
0x75, 0xe2,                               /*69c: jne    680 <op_enter+0x680> */
0x4c, 0x39, 0x54, 0x24, 0x38,             /*69e: cmp    %r10,0x38(%rsp) */
0x75, 0x20,                               /*6a3: jne    6c5 <op_enter+0x6c5> */
0x41, 0xff, 0xc0,                         /*6a5: inc    %r8d */
0x48, 0x8b, 0x45, 0x18,                   /*6a8: mov    0x18(%rbp),%rax */
0x49, 0xc1, 0xe0, 0x04,                   /*6ac: shl    $0x4,%r8 */
0x48, 0x8b, 0x54, 0x24, 0x18,             /*6b0: mov    0x18(%rsp),%rdx */
0x48, 0x8b, 0x0a,                         /*6b5: mov    (%rdx),%rcx */
0x48, 0x8b, 0x52, 0x08,                   /*6b8: mov    0x8(%rdx),%rdx */
0x4a, 0x89, 0x54, 0x00, 0x08,             /*6bc: mov    %rdx,0x8(%rax,%r8,1) */
0x4a, 0x89, 0x0c, 0x00,                   /*6c1: mov    %rcx,(%rax,%r8,1) */
0x85, 0xff,                               /*6c5: test   %edi,%edi */
0x48, 0x8b, 0x45, 0x08,                   /*6c7: mov    0x8(%rbp),%rax */
0x48, 0x8b, 0x88, 0x98, 0x00, 0x00, 0x00, /*6cb: mov    0x98(%rax),%rcx */
0x74, 0x08,                               /*6d2: je     6dc <op_enter+0x6dc> */
0xff, 0xc7,                               /*6d4: inc    %edi */
0x48, 0x8d, 0x0c, 0xb9,                   /*6d6: lea    (%rcx,%rdi,4),%rcx */
0xeb, 0x04,                               /*6da: jmp    6e0 <op_enter+0x6e0> */
0x48, 0x83, 0xc1, 0x04,                   /*6dc: add    $0x4,%rcx */
0x0f, 0xb7, 0x31,                         /*6e0: movzwl (%rcx),%esi */
0x48, 0x03, 0xb0, 0xa0, 0x00, 0x00, 0x00, /*6e3: add    0xa0(%rax),%rsi */
0x48, 0x89, 0xef,                         /*6ea: mov    %rbp,%rdi */
0x48, 0x89, 0xef,                         /*6ed: mov    %rbp,%rdi */
0x48, 0x83, 0xc4, 0x58,                   /*6f0: add    $0x58,%rsp */
0x5b,                                     /*6f4: pop    %rbx */
0x41, 0x5c,                               /*6f5: pop    %r12 */
0x41, 0x5d,                               /*6f7: pop    %r13 */
0x41, 0x5e,                               /*6f9: pop    %r14 */
0x41, 0x5f,                               /*6fb: pop    %r15 */
0x5d,                                     /*6fd: pop    %rbp */
0xff, 0xe6,                               /*6fe: jmpq   *%rsi */

};
static uint8_t op_enter__rodata[] = {
0x50, 0x72, 0x6f, 0x63,                   
0x6f, 0x70, 0x5f, 0x65,                   

};

static void op_enter_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = a * 1 + 0;
}

static void op_enter_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_enter_set_args(op, GETARG_Ax(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 14..17]]} */
static uint8_t op_enter_method_m__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x49, 0x89, 0xfe,                         /*a: mov    %rdi,%r14 */
0xb8, 0x00, 0x00, 0xab, 0x00,             /*d: mov    $0xab0000,%eax */
0x41, 0x89, 0xc5,                         /*12: mov    %eax,%r13d */
0x4d, 0x8b, 0x66, 0x18,                   /*15: mov    0x18(%r14),%r12 */
0x49, 0x8b, 0x7e, 0x58,                   /*19: mov    0x58(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*1d: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*21: mov    0x20(%rax),%rax */
0x48, 0x63, 0x58, 0x40,                   /*25: movslq 0x40(%rax),%rbx */
0x48, 0x8d, 0x4b, 0x01,                   /*29: lea    0x1(%rbx),%rcx */
0x48, 0x85, 0xdb,                         /*2d: test   %rbx,%rbx */
0xb8, 0x02, 0x00, 0x00, 0x00,             /*30: mov    $0x2,%eax */
0x48, 0x0f, 0x49, 0xc1,                   /*35: cmovns %rcx,%rax */
0x48, 0xc1, 0xe0, 0x04,                   /*39: shl    $0x4,%rax */
0x49, 0x8d, 0x2c, 0x04,                   /*3d: lea    (%r12,%rax,1),%rbp */
0x41, 0x8b, 0x54, 0x04, 0x08,             /*41: mov    0x8(%r12,%rax,1),%edx */
0x85, 0xd2,                               /*46: test   %edx,%edx */
0x75, 0x0a,                               /*48: jne    54 <op_enter_method_m+0x54> */
0x83, 0x7d, 0x00, 0x00,                   /*4a: cmpl   $0x0,0x0(%rbp) */
0x0f, 0x84, 0x68, 0x01, 0x00, 0x00,       /*4e: je     1bc <op_enter_method_m+0x1bc> */
0x83, 0xfa, 0x0d,                         /*54: cmp    $0xd,%edx */
0x0f, 0x95, 0xc1,                         /*57: setne  %cl */
0x41, 0xc1, 0xed, 0x12,                   /*5a: shr    $0x12,%r13d */
0x84, 0xc9,                               /*5e: test   %cl,%cl */
0x74, 0x26,                               /*60: je     88 <op_enter_method_m+0x88> */
0x4d, 0x8d, 0x7c, 0x04, 0x08,             /*62: lea    0x8(%r12,%rax,1),%r15 */
0x48, 0x8b, 0x75, 0x00,                   /*67: mov    0x0(%rbp),%rsi */
0xb9, 0x0d, 0x00, 0x00, 0x00,             /*6b: mov    $0xd,%ecx */
0x41, 0xb8, 0x00, 0x00, 0x00, 0x00,       /*70: mov    $0x0,%r8d */
0x41, 0xb9, 0x00, 0x00, 0x00, 0x00,       /*76: mov    $0x0,%r9d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*7c: callq  81 <op_enter_method_m+0x81> */
0x48, 0x89, 0x45, 0x00,                   /*81: mov    %rax,0x0(%rbp) */
0x41, 0x89, 0x17,                         /*85: mov    %edx,(%r15) */
0x45, 0x89, 0xef,                         /*88: mov    %r13d,%r15d */
0x41, 0x83, 0xe7, 0x1f,                   /*8b: and    $0x1f,%r15d */
0x49, 0x83, 0xc4, 0x10,                   /*8f: add    $0x10,%r12 */
0x85, 0xdb,                               /*93: test   %ebx,%ebx */
0x78, 0x21,                               /*95: js     b8 <op_enter_method_m+0xb8> */
0x44, 0x39, 0xfb,                         /*97: cmp    %r15d,%ebx */
0x4c, 0x89, 0xe3,                         /*9a: mov    %r12,%rbx */
0x7d, 0x31,                               /*9d: jge    d0 <op_enter_method_m+0xd0> */
0x49, 0x8b, 0x7e, 0x58,                   /*9f: mov    0x58(%r14),%rdi */
0x44, 0x89, 0xfe,                         /*a3: mov    %r15d,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*a6: callq  ab <op_enter_method_m+0xab> */
0x4c, 0x89, 0xf7,                         /*ab: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*ae: callq  b3 <op_enter_method_m+0xb3> */
0xe9, 0xf5, 0x00, 0x00, 0x00,             /*b3: jmpq   1ad <op_enter_method_m+0x1ad> */
0x49, 0x8b, 0x46, 0x18,                   /*b8: mov    0x18(%r14),%rax */
0x49, 0x8b, 0x7e, 0x58,                   /*bc: mov    0x58(%r14),%rdi */
0x48, 0x8b, 0x70, 0x10,                   /*c0: mov    0x10(%rax),%rsi */
0x48, 0x8b, 0x5e, 0x28,                   /*c4: mov    0x28(%rsi),%rbx */
0x8b, 0x50, 0x18,                         /*c8: mov    0x18(%rax),%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*cb: callq  d0 <op_enter_method_m+0xd0> */
0x49, 0x8b, 0x46, 0x58,                   /*d0: mov    0x58(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*d4: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*d8: mov    0x20(%rax),%rax */
0x44, 0x89, 0x78, 0x40,                   /*dc: mov    %r15d,0x40(%rax) */
0x41, 0x8d, 0x47, 0x01,                   /*e0: lea    0x1(%r15),%eax */
0x49, 0x8b, 0x4e, 0x18,                   /*e4: mov    0x18(%r14),%rcx */
0x48, 0xc1, 0xe0, 0x04,                   /*e8: shl    $0x4,%rax */
0x48, 0x8b, 0x55, 0x00,                   /*ec: mov    0x0(%rbp),%rdx */
0x48, 0x8b, 0x75, 0x08,                   /*f0: mov    0x8(%rbp),%rsi */
0x48, 0x89, 0x74, 0x01, 0x08,             /*f4: mov    %rsi,0x8(%rcx,%rax,1) */
0x48, 0x89, 0x14, 0x01,                   /*f9: mov    %rdx,(%rcx,%rax,1) */
0x49, 0x39, 0xdc,                         /*fd: cmp    %rbx,%r12 */
0x0f, 0x84, 0x8e, 0x00, 0x00, 0x00,       /*100: je     194 <op_enter_method_m+0x194> */
0x49, 0x8b, 0x46, 0x18,                   /*106: mov    0x18(%r14),%rax */
0x48, 0x8d, 0x48, 0x10,                   /*10a: lea    0x10(%rax),%rcx */
0x44, 0x89, 0xfa,                         /*10e: mov    %r15d,%edx */
0x48, 0x39, 0xd9,                         /*111: cmp    %rbx,%rcx */
0x76, 0x47,                               /*114: jbe    15d <op_enter_method_m+0x15d> */
0x48, 0xc1, 0xe2, 0x04,                   /*116: shl    $0x4,%rdx */
0x48, 0x01, 0xda,                         /*11a: add    %rbx,%rdx */
0x48, 0x39, 0xd1,                         /*11d: cmp    %rdx,%rcx */
0x73, 0x3b,                               /*120: jae    15d <op_enter_method_m+0x15d> */
0x45, 0x85, 0xff,                         /*122: test   %r15d,%r15d */
0x74, 0x6d,                               /*125: je     194 <op_enter_method_m+0x194> */
0x49, 0x83, 0xe5, 0x1f,                   /*127: and    $0x1f,%r13 */
0x4c, 0x89, 0xe9,                         /*12b: mov    %r13,%rcx */
0x48, 0xc1, 0xe1, 0x04,                   /*12e: shl    $0x4,%rcx */
0x48, 0x01, 0xc8,                         /*132: add    %rcx,%rax */
0x48, 0x8d, 0x4c, 0x19, 0xf0,             /*135: lea    -0x10(%rcx,%rbx,1),%rcx */
0x49, 0xf7, 0xdd,                         /*13a: neg    %r13 */
0x0f, 0x1f, 0x00,                         /*13d: nopl   (%rax) */
0x48, 0x8b, 0x11,                         /*140: mov    (%rcx),%rdx */
0x48, 0x8b, 0x71, 0x08,                   /*143: mov    0x8(%rcx),%rsi */
0x48, 0x89, 0x70, 0x08,                   /*147: mov    %rsi,0x8(%rax) */
0x48, 0x89, 0x10,                         /*14b: mov    %rdx,(%rax) */
0x48, 0x83, 0xc0, 0xf0,                   /*14e: add    $0xfffffffffffffff0,%rax */
0x48, 0x83, 0xc1, 0xf0,                   /*152: add    $0xfffffffffffffff0,%rcx */
0x49, 0xff, 0xc5,                         /*156: inc    %r13 */
0x75, 0xe5,                               /*159: jne    140 <op_enter_method_m+0x140> */
0xeb, 0x37,                               /*15b: jmp    194 <op_enter_method_m+0x194> */
0x48, 0x39, 0xd9,                         /*15d: cmp    %rbx,%rcx */
0x74, 0x32,                               /*160: je     194 <op_enter_method_m+0x194> */
0x45, 0x85, 0xff,                         /*162: test   %r15d,%r15d */
0x74, 0x2d,                               /*165: je     194 <op_enter_method_m+0x194> */
0x49, 0x83, 0xe5, 0x1f,                   /*167: and    $0x1f,%r13 */
0x49, 0xf7, 0xdd,                         /*16b: neg    %r13 */
0x66, 0x90,                               /*16e: xchg   %ax,%ax */
0x48, 0x89, 0xca,                         /*170: mov    %rcx,%rdx */
0x48, 0x83, 0xc0, 0x20,                   /*173: add    $0x20,%rax */
0x48, 0x8b, 0x0b,                         /*177: mov    (%rbx),%rcx */
0x48, 0x8b, 0x73, 0x08,                   /*17a: mov    0x8(%rbx),%rsi */
0x48, 0x8d, 0x5b, 0x10,                   /*17e: lea    0x10(%rbx),%rbx */
0x48, 0x89, 0x72, 0x08,                   /*182: mov    %rsi,0x8(%rdx) */
0x48, 0x89, 0x0a,                         /*186: mov    %rcx,(%rdx) */
0x49, 0xff, 0xc5,                         /*189: inc    %r13 */
0x48, 0x89, 0xc1,                         /*18c: mov    %rax,%rcx */
0x48, 0x89, 0xd0,                         /*18f: mov    %rdx,%rax */
0x75, 0xdc,                               /*192: jne    170 <op_enter_method_m+0x170> */
0x49, 0x8b, 0x46, 0x08,                   /*194: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x88, 0x98, 0x00, 0x00, 0x00, /*198: mov    0x98(%rax),%rcx */
0x0f, 0xb7, 0x71, 0x04,                   /*19f: movzwl 0x4(%rcx),%esi */
0x48, 0x03, 0xb0, 0xa0, 0x00, 0x00, 0x00, /*1a3: add    0xa0(%rax),%rsi */
0x4c, 0x89, 0xf7,                         /*1aa: mov    %r14,%rdi */
0x4c, 0x89, 0xf7,                         /*1ad: mov    %r14,%rdi */
0x5b,                                     /*1b0: pop    %rbx */
0x41, 0x5c,                               /*1b1: pop    %r12 */
0x41, 0x5d,                               /*1b3: pop    %r13 */
0x41, 0x5e,                               /*1b5: pop    %r14 */
0x41, 0x5f,                               /*1b7: pop    %r15 */
0x5d,                                     /*1b9: pop    %rbp */
0xeb, 0x09,                               /*1ba: jmp    1c5 <_str_const_to_proc+0x1c0> */
0x31, 0xc9,                               /*1bc: xor    %ecx,%ecx */
0xe9, 0x97, 0xfe, 0xff, 0xff,             /*1be: jmpq   5a <op_enter_method_m+0x5a> */
0xff, 0xe6,                               /*1c3: jmpq   *%rsi */

};
static uint8_t op_enter_method_m__rodata[] = {
0x50, 0x72, 0x6f, 0x63,                   

};

static void op_enter_method_m_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = a * 1 + 0;
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


/* args: {"b"=>[[4, 0, 32..35]], "a"=>[[16, 0, 39..42], [16, 8, 46..49], [1, 0, 292..295], [1, 0, 300..303], [1, 1, 308..311], [1, 1, 316..319], [16, 16, 333..336], [16, 24, 339..342], [16, 0, 403..406], [1, 0, 425..428], [1, 0, 588..591]], "c"=>[[1, 0, 211..214], [1, 1, 279..282], [1, 1, 348..351]]} */
static uint8_t op_tailcall__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x83, 0xec, 0x18,                   /*a: sub    $0x18,%rsp */
0x48, 0x89, 0xfb,                         /*e: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x58,                   /*11: mov    0x58(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x18,                   /*15: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*19: mov    0x28(%rbx),%rcx */
0x44, 0x8b, 0xa9, 0x00, 0x04, 0xbc, 0x00, /*1d: mov    0xbc0400(%rcx),%r13d */
0x4c, 0x8b, 0xb8, 0x00, 0x10, 0xab, 0x00, /*24: mov    0xab1000(%rax),%r15 */
0x44, 0x8b, 0x80, 0x08, 0x10, 0xab, 0x00, /*2b: mov    0xab1008(%rax),%r8d */
0x31, 0xc0,                               /*32: xor    %eax,%eax */
0x41, 0x83, 0xf8, 0x13,                   /*34: cmp    $0x13,%r8d */
0x7f, 0x22,                               /*38: jg     5c <op_tailcall+0x5c> */
0x44, 0x89, 0xc0,                         /*3a: mov    %r8d,%eax */
0x41, 0x83, 0xf8, 0x07,                   /*3d: cmp    $0x7,%r8d */
0x77, 0x1f,                               /*41: ja     62 <op_tailcall+0x62> */
0xff, 0x24, 0xc5, 0x00, 0x00, 0x00, 0x00, /*43: jmpq   *0x0(,%rax,8) */
0x45, 0x85, 0xff,                         /*4a: test   %r15d,%r15d */
0x0f, 0x84, 0xba, 0x02, 0x00, 0x00,       /*4d: je     30d <op_tailcall+0x30d> */
0x49, 0x8b, 0x86, 0x90, 0x00, 0x00, 0x00, /*53: mov    0x90(%r14),%rax */
0xeb, 0x0a,                               /*5a: jmp    66 <op_tailcall+0x66> */
0x41, 0x83, 0xf8, 0x14,                   /*5c: cmp    $0x14,%r8d */
0x74, 0x04,                               /*60: je     66 <op_tailcall+0x66> */
0x49, 0x8b, 0x47, 0x08,                   /*62: mov    0x8(%r15),%rax */
0x48, 0x89, 0x44, 0x24, 0x10,             /*66: mov    %rax,0x10(%rsp) */
0x48, 0x8b, 0x0b,                         /*6b: mov    (%rbx),%rcx */
0x48, 0x39, 0x41, 0x38,                   /*6e: cmp    %rax,0x38(%rcx) */
0x75, 0x08,                               /*72: jne    7c <op_tailcall+0x7c> */
0x31, 0xd2,                               /*74: xor    %edx,%edx */
0x44, 0x39, 0x69, 0x30,                   /*76: cmp    %r13d,0x30(%rcx) */
0x74, 0x33,                               /*7a: je     af <op_tailcall+0xaf> */
0x48, 0x39, 0x41, 0x48,                   /*7c: cmp    %rax,0x48(%rcx) */
0x75, 0x0b,                               /*80: jne    8d <op_tailcall+0x8d> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*82: mov    $0x1,%edx */
0x44, 0x39, 0x69, 0x40,                   /*87: cmp    %r13d,0x40(%rcx) */
0x74, 0x22,                               /*8b: je     af <op_tailcall+0xaf> */
0x48, 0x39, 0x41, 0x58,                   /*8d: cmp    %rax,0x58(%rcx) */
0x75, 0x0b,                               /*91: jne    9e <op_tailcall+0x9e> */
0xba, 0x02, 0x00, 0x00, 0x00,             /*93: mov    $0x2,%edx */
0x44, 0x39, 0x69, 0x50,                   /*98: cmp    %r13d,0x50(%rcx) */
0x74, 0x11,                               /*9c: je     af <op_tailcall+0xaf> */
0x48, 0x39, 0x41, 0x68,                   /*9e: cmp    %rax,0x68(%rcx) */
0x75, 0x15,                               /*a2: jne    b9 <op_tailcall+0xb9> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*a4: mov    $0x3,%edx */
0x44, 0x39, 0x69, 0x60,                   /*a9: cmp    %r13d,0x60(%rcx) */
0x75, 0x0a,                               /*ad: jne    b9 <op_tailcall+0xb9> */
0x4c, 0x8b, 0xa4, 0xd1, 0x90, 0x00, 0x00, 0x00,/*af: mov    0x90(%rcx,%rdx,8),%r12 */
0xeb, 0x19,                               /*b7: jmp    d2 <op_tailcall+0xd2> */
0x48, 0x8d, 0x74, 0x24, 0x10,             /*b9: lea    0x10(%rsp),%rsi */
0x4c, 0x89, 0xf7,                         /*be: mov    %r14,%rdi */
0x44, 0x89, 0xea,                         /*c1: mov    %r13d,%edx */
0x44, 0x89, 0xc5,                         /*c4: mov    %r8d,%ebp */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c7: callq  cc <op_tailcall+0xcc> */
0x41, 0x89, 0xe8,                         /*cc: mov    %ebp,%r8d */
0x49, 0x89, 0xc4,                         /*cf: mov    %rax,%r12 */
0xbd, 0x00, 0x00, 0xcd, 0x00,             /*d2: mov    $0xcd0000,%ebp */
0x4d, 0x85, 0xe4,                         /*d7: test   %r12,%r12 */
0x0f, 0x85, 0x8d, 0x00, 0x00, 0x00,       /*da: jne    16d <op_tailcall+0x16d> */
0x44, 0x89, 0x44, 0x24, 0x04,             /*e0: mov    %r8d,0x4(%rsp) */
0x4c, 0x89, 0x7c, 0x24, 0x08,             /*e5: mov    %r15,0x8(%rsp) */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*ea: mov    $0x0,%esi */
0xba, 0x0e, 0x00, 0x00, 0x00,             /*ef: mov    $0xe,%edx */
0x4c, 0x89, 0xf7,                         /*f4: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*f7: callq  fc <op_tailcall+0xfc> */
0x41, 0x89, 0xc7,                         /*fc: mov    %eax,%r15d */
0x48, 0x8d, 0x74, 0x24, 0x10,             /*ff: lea    0x10(%rsp),%rsi */
0x4c, 0x89, 0xf7,                         /*104: mov    %r14,%rdi */
0x44, 0x89, 0xfa,                         /*107: mov    %r15d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*10a: callq  10f <op_tailcall+0x10f> */
0x49, 0x89, 0xc4,                         /*10f: mov    %rax,%r12 */
0x48, 0x8b, 0x43, 0x18,                   /*112: mov    0x18(%rbx),%rax */
0xb9, 0x10, 0x00, 0xd0, 0x0c,             /*116: mov    $0xcd00010,%ecx */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*11b: nopl   0x0(%rax,%rax,1) */
0x48, 0x8b, 0x94, 0x08, 0x00, 0x00, 0xb0, 0x0a,/*120: mov    0xab00000(%rax,%rcx,1),%rdx */
0x48, 0x8b, 0xb4, 0x08, 0x08, 0x00, 0xb0, 0x0a,/*128: mov    0xab00008(%rax,%rcx,1),%rsi */
0x48, 0x89, 0xb4, 0x08, 0x18, 0x00, 0xb0, 0x0a,/*130: mov    %rsi,0xab00018(%rax,%rcx,1) */
0x48, 0x89, 0x94, 0x08, 0x10, 0x00, 0xb0, 0x0a,/*138: mov    %rdx,0xab00010(%rax,%rcx,1) */
0x48, 0x83, 0xc1, 0xf0,                   /*140: add    $0xfffffffffffffff0,%rcx */
0x75, 0xda,                               /*144: jne    120 <op_tailcall+0x120> */
0x48, 0x8b, 0x43, 0x18,                   /*146: mov    0x18(%rbx),%rax */
0x4c, 0x89, 0xa8, 0x10, 0x10, 0xab, 0x00, /*14a: mov    %r13,0xab1010(%rax) */
0xc7, 0x80, 0x18, 0x10, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*151: movl   $0x4,0xab1018(%rax) */
0xbd, 0x01, 0x00, 0xcd, 0x00,             /*15b: mov    $0xcd0001,%ebp */
0x45, 0x89, 0xfd,                         /*160: mov    %r15d,%r13d */
0x4c, 0x8b, 0x7c, 0x24, 0x08,             /*163: mov    0x8(%rsp),%r15 */
0x44, 0x8b, 0x44, 0x24, 0x04,             /*168: mov    0x4(%rsp),%r8d */
0x49, 0x8b, 0x4e, 0x18,                   /*16d: mov    0x18(%r14),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*171: mov    0x20(%rcx),%rcx */
0x44, 0x89, 0x29,                         /*175: mov    %r13d,(%rcx) */
0x48, 0x8b, 0x54, 0x24, 0x10,             /*178: mov    0x10(%rsp),%rdx */
0x48, 0x89, 0x51, 0x48,                   /*17d: mov    %rdx,0x48(%rcx) */
0x89, 0x69, 0x40,                         /*181: mov    %ebp,0x40(%rcx) */
0x49, 0x8b, 0x56, 0x18,                   /*184: mov    0x18(%r14),%rdx */
0x48, 0x8b, 0x52, 0x08,                   /*188: mov    0x8(%rdx),%rdx */
0x48, 0x8b, 0x7b, 0x18,                   /*18c: mov    0x18(%rbx),%rdi */
0x48, 0x8d, 0xb7, 0x00, 0x10, 0xab, 0x00, /*190: lea    0xab1000(%rdi),%rsi */
0xff, 0xc5,                               /*197: inc    %ebp */
0x48, 0x39, 0xf2,                         /*199: cmp    %rsi,%rdx */
0x76, 0x4f,                               /*19c: jbe    1ed <op_tailcall+0x1ed> */
0x48, 0x89, 0xe8,                         /*19e: mov    %rbp,%rax */
0x48, 0xc1, 0xe0, 0x04,                   /*1a1: shl    $0x4,%rax */
0x48, 0x8d, 0x84, 0x38, 0x00, 0x00, 0xb0, 0x0a,/*1a5: lea    0xab00000(%rax,%rdi,1),%rax */
0x48, 0x39, 0xc2,                         /*1ad: cmp    %rax,%rdx */
0x73, 0x3b,                               /*1b0: jae    1ed <op_tailcall+0x1ed> */
0x48, 0x89, 0xe8,                         /*1b2: mov    %rbp,%rax */
0x48, 0xc1, 0xe0, 0x04,                   /*1b5: shl    $0x4,%rax */
0x48, 0x8d, 0x54, 0x10, 0xf0,             /*1b9: lea    -0x10(%rax,%rdx,1),%rdx */
0x48, 0x8d, 0xb4, 0x38, 0xf0, 0xff, 0xaf, 0x0a,/*1be: lea    0xaaffff0(%rax,%rdi,1),%rsi */
0x48, 0xf7, 0xdd,                         /*1c6: neg    %rbp */
0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00, /*1c9: nopl   0x0(%rax) */
0x48, 0x8b, 0x06,                         /*1d0: mov    (%rsi),%rax */
0x48, 0x8b, 0x7e, 0x08,                   /*1d3: mov    0x8(%rsi),%rdi */
0x48, 0x89, 0x7a, 0x08,                   /*1d7: mov    %rdi,0x8(%rdx) */
0x48, 0x89, 0x02,                         /*1db: mov    %rax,(%rdx) */
0x48, 0x83, 0xc2, 0xf0,                   /*1de: add    $0xfffffffffffffff0,%rdx */
0x48, 0x83, 0xc6, 0xf0,                   /*1e2: add    $0xfffffffffffffff0,%rsi */
0x48, 0xff, 0xc5,                         /*1e6: inc    %rbp */
0x75, 0xe5,                               /*1e9: jne    1d0 <op_tailcall+0x1d0> */
0xeb, 0x2e,                               /*1eb: jmp    21b <op_tailcall+0x21b> */
0x48, 0x39, 0xf2,                         /*1ed: cmp    %rsi,%rdx */
0x74, 0x29,                               /*1f0: je     21b <op_tailcall+0x21b> */
0x48, 0xf7, 0xdd,                         /*1f2: neg    %rbp */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*1f5: data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x06,                         /*200: mov    (%rsi),%rax */
0x48, 0x8b, 0x7e, 0x08,                   /*203: mov    0x8(%rsi),%rdi */
0x48, 0x8d, 0x76, 0x10,                   /*207: lea    0x10(%rsi),%rsi */
0x48, 0x89, 0x7a, 0x08,                   /*20b: mov    %rdi,0x8(%rdx) */
0x48, 0x89, 0x02,                         /*20f: mov    %rax,(%rdx) */
0x48, 0x8d, 0x52, 0x10,                   /*212: lea    0x10(%rdx),%rdx */
0x48, 0xff, 0xc5,                         /*216: inc    %rbp */
0x75, 0xe5,                               /*219: jne    200 <op_tailcall+0x200> */
0x41, 0xf6, 0x44, 0x24, 0x02, 0x04,       /*21b: testb  $0x4,0x2(%r12) */
0x74, 0x39,                               /*221: je     25c <op_tailcall+0x25c> */
0x49, 0x8b, 0x46, 0x18,                   /*223: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x68, 0x08,                   /*227: mov    0x8(%rax),%rbp */
0x4c, 0x89, 0xf7,                         /*22b: mov    %r14,%rdi */
0x4c, 0x89, 0xfe,                         /*22e: mov    %r15,%rsi */
0x44, 0x89, 0xc2,                         /*231: mov    %r8d,%edx */
0x41, 0xff, 0x54, 0x24, 0x18,             /*234: callq  *0x18(%r12) */
0x48, 0x89, 0x45, 0x00,                   /*239: mov    %rax,0x0(%rbp) */
0x89, 0x55, 0x08,                         /*23d: mov    %edx,0x8(%rbp) */
0x8b, 0x73, 0x50,                         /*240: mov    0x50(%rbx),%esi */
0x4c, 0x89, 0xf7,                         /*243: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*246: callq  24b <op_tailcall+0x24b> */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*24b: mov    $0xab0000,%esi */
0x31, 0xd2,                               /*250: xor    %edx,%edx */
0x48, 0x89, 0xdf,                         /*252: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*255: callq  25a <op_tailcall+0x25a> */
0xeb, 0x68,                               /*25a: jmp    2c4 <op_tailcall+0x2c4> */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*25c: mov    0x18(%r12),%rax */
0x48, 0x89, 0x43, 0x08,                   /*261: mov    %rax,0x8(%rbx) */
0x48, 0x8b, 0x50, 0x10,                   /*265: mov    0x10(%rax),%rdx */
0x48, 0x89, 0x53, 0x20,                   /*269: mov    %rdx,0x20(%rbx) */
0x48, 0x8b, 0x50, 0x18,                   /*26d: mov    0x18(%rax),%rdx */
0x48, 0x89, 0x53, 0x28,                   /*271: mov    %rdx,0x28(%rbx) */
0x8b, 0x51, 0x40,                         /*275: mov    0x40(%rcx),%edx */
0x0f, 0xb7, 0x70, 0x02,                   /*278: movzwl 0x2(%rax),%esi */
0x85, 0xd2,                               /*27c: test   %edx,%edx */
0x78, 0x05,                               /*27e: js     285 <op_tailcall+0x285> */
0x83, 0xc2, 0x02,                         /*280: add    $0x2,%edx */
0xeb, 0x10,                               /*283: jmp    295 <op_tailcall+0x295> */
0x83, 0xfe, 0x03,                         /*285: cmp    $0x3,%esi */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*288: mov    $0x3,%eax */
0x0f, 0x42, 0xf0,                         /*28d: cmovb  %eax,%esi */
0xba, 0x03, 0x00, 0x00, 0x00,             /*290: mov    $0x3,%edx */
0x4c, 0x89, 0xf7,                         /*295: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*298: callq  29d <op_tailcall+0x29d> */
0x49, 0x8b, 0x46, 0x18,                   /*29d: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*2a1: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x18,                   /*2a5: mov    %rax,0x18(%rbx) */
0x48, 0x8b, 0x43, 0x08,                   /*2a9: mov    0x8(%rbx),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*2ad: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x10,                   /*2b1: mov    %rax,0x10(%rbx) */
0x48, 0x8b, 0x7b, 0x58,                   /*2b5: mov    0x58(%rbx),%rdi */
0x4c, 0x89, 0xe6,                         /*2b9: mov    %r12,%rsi */
0x48, 0x89, 0xda,                         /*2bc: mov    %rbx,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2bf: callq  2c4 <op_tailcall+0x2c4> */
0x48, 0x89, 0xdf,                         /*2c4: mov    %rbx,%rdi */
0x48, 0x83, 0xc4, 0x18,                   /*2c7: add    $0x18,%rsp */
0x5b,                                     /*2cb: pop    %rbx */
0x41, 0x5c,                               /*2cc: pop    %r12 */
0x41, 0x5d,                               /*2ce: pop    %r13 */
0x41, 0x5e,                               /*2d0: pop    %r14 */
0x41, 0x5f,                               /*2d2: pop    %r15 */
0x5d,                                     /*2d4: pop    %rbp */
0xeb, 0x42,                               /*2d5: jmp    319 <_str_const_method_missing+0x2d9> */
0x49, 0x8b, 0x86, 0x88, 0x00, 0x00, 0x00, /*2d7: mov    0x88(%r14),%rax */
0xe9, 0x83, 0xfd, 0xff, 0xff,             /*2de: jmpq   66 <op_tailcall+0x66> */
0x49, 0x8b, 0x86, 0x80, 0x00, 0x00, 0x00, /*2e3: mov    0x80(%r14),%rax */
0xe9, 0x77, 0xfd, 0xff, 0xff,             /*2ea: jmpq   66 <op_tailcall+0x66> */
0x49, 0x8b, 0x86, 0xa0, 0x00, 0x00, 0x00, /*2ef: mov    0xa0(%r14),%rax */
0xe9, 0x6b, 0xfd, 0xff, 0xff,             /*2f6: jmpq   66 <op_tailcall+0x66> */
0x49, 0x8b, 0x46, 0x78,                   /*2fb: mov    0x78(%r14),%rax */
0xe9, 0x62, 0xfd, 0xff, 0xff,             /*2ff: jmpq   66 <op_tailcall+0x66> */
0x49, 0x8b, 0x46, 0x40,                   /*304: mov    0x40(%r14),%rax */
0xe9, 0x59, 0xfd, 0xff, 0xff,             /*308: jmpq   66 <op_tailcall+0x66> */
0x49, 0x8b, 0x86, 0x98, 0x00, 0x00, 0x00, /*30d: mov    0x98(%r14),%rax */
0xe9, 0x4d, 0xfd, 0xff, 0xff,             /*314: jmpq   66 <op_tailcall+0x66> */

};
static uint8_t op_tailcall__rodata[] = {
0x00, 0x00, 0x00, 0x00,                   
0x00, 0x00, 0x00, 0x00,                   
0x00, 0x00, 0x00, 0x00,                   
0x00, 0x00, 0x00, 0x00,                   
0x6d, 0x65, 0x74, 0x68,                   

};

static void op_tailcall_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 32)) = b * 4 + 0;
  *((int32_t *)(op + 39)) = a * 16 + 0;
  *((int32_t *)(op + 46)) = a * 16 + 8;
  *((int32_t *)(op + 292)) = a * 1 + 0;
  *((int32_t *)(op + 300)) = a * 1 + 0;
  *((int32_t *)(op + 308)) = a * 1 + 1;
  *((int32_t *)(op + 316)) = a * 1 + 1;
  *((int32_t *)(op + 333)) = a * 16 + 16;
  *((int32_t *)(op + 339)) = a * 16 + 24;
  *((int32_t *)(op + 403)) = a * 16 + 0;
  *((int32_t *)(op + 425)) = a * 1 + 0;
  *((int32_t *)(op + 588)) = a * 1 + 0;
  *((int32_t *)(op + 211)) = c * 1 + 0;
  *((int32_t *)(op + 279)) = c * 1 + 1;
  *((int32_t *)(op + 348)) = c * 1 + 1;
}

static void op_tailcall_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_tailcall_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[1, 0, 14..17]], "a"=>[[16, 8, 194..197], [16, 0, 201..204]]} */
static uint8_t op_blkpush__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x7e, 0x58,                   /*6: mov    0x58(%r14),%rdi */
0xc7, 0x44, 0x24, 0x04, 0x00, 0x00, 0xbc, 0x00,/*a: movl   $0xbc0000,0x4(%rsp) */
0x44, 0x8b, 0x4c, 0x24, 0x04,             /*12: mov    0x4(%rsp),%r9d */
0x8b, 0x44, 0x24, 0x04,                   /*17: mov    0x4(%rsp),%eax */
0x44, 0x8b, 0x44, 0x24, 0x04,             /*1b: mov    0x4(%rsp),%r8d */
0x8b, 0x54, 0x24, 0x04,                   /*20: mov    0x4(%rsp),%edx */
0x83, 0xe2, 0x0f,                         /*24: and    $0xf,%edx */
0x74, 0x52,                               /*27: je     7b <op_blkpush+0x7b> */
0x49, 0x8b, 0x4e, 0x58,                   /*29: mov    0x58(%r14),%rcx */
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
0xeb, 0x52,                               /*79: jmp    cd <op_blkpush+0xcd> */
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
0x48, 0xc1, 0xe0, 0x04,                   /*b2: shl    $0x4,%rax */
0x48, 0x8b, 0x34, 0x02,                   /*b6: mov    (%rdx,%rax,1),%rsi */
0x48, 0x8b, 0x44, 0x02, 0x08,             /*ba: mov    0x8(%rdx,%rax,1),%rax */
0x48, 0x89, 0x81, 0x08, 0x10, 0xab, 0x00, /*bf: mov    %rax,0xab1008(%rcx) */
0x48, 0x89, 0xb1, 0x00, 0x10, 0xab, 0x00, /*c6: mov    %rsi,0xab1000(%rcx) */
0x4c, 0x89, 0xf7,                         /*cd: mov    %r14,%rdi */
0x5b,                                     /*d0: pop    %rbx */
0x41, 0x5e,                               /*d1: pop    %r14 */

};
static uint8_t op_blkpush__rodata[] = {

};

static void op_blkpush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = b * 1 + 0;
  *((int32_t *)(op + 194)) = a * 16 + 8;
  *((int32_t *)(op + 201)) = a * 16 + 0;
}

static void op_blkpush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_blkpush_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 18..21], [16, 24, 25..28], [16, 0, 73..76], [16, 16, 79..82], [16, 8, 95..98], [16, 0, 119..122], [16, 0, 142..145], [16, 16, 149..152], [16, 0, 164..167], [16, 8, 170..173], [16, 16, 190..193], [16, 0, 198..201], [16, 0, 206..209], [16, 0, 223..226], [16, 16, 231..234], [16, 8, 237..240], [16, 0, 249..252], [16, 0, 266..269], [16, 16, 274..277], [16, 0, 282..285], [16, 8, 300..303], [16, 0, 310..313]]} */
static uint8_t op_add__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x49, 0x89, 0xff,                         /*5: mov    %rdi,%r15 */
0x49, 0x8b, 0x5f, 0x18,                   /*8: mov    0x18(%r15),%rbx */
0x4d, 0x8b, 0x77, 0x58,                   /*c: mov    0x58(%r15),%r14 */
0x8b, 0x93, 0x08, 0x10, 0xab, 0x00,       /*10: mov    0xab1008(%rbx),%edx */
0x44, 0x8b, 0x83, 0x18, 0x10, 0xab, 0x00, /*16: mov    0xab1018(%rbx),%r8d */
0x89, 0xd1,                               /*1d: mov    %edx,%ecx */
0xc1, 0xe1, 0x08,                         /*1f: shl    $0x8,%ecx */
0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*22: and    $0xffff00,%ecx */
0x41, 0x0f, 0xb6, 0xc0,                   /*28: movzbl %r8b,%eax */
0x09, 0xc8,                               /*2c: or     %ecx,%eax */
0x3d, 0x0f, 0x10, 0x00, 0x00,             /*2e: cmp    $0x100f,%eax */
0x7f, 0x4b,                               /*33: jg     80 <op_add+0x80> */
0x3d, 0x02, 0x06, 0x00, 0x00,             /*35: cmp    $0x602,%eax */
0x7f, 0x77,                               /*3a: jg     b3 <op_add+0xb3> */
0x3d, 0x03, 0x03, 0x00, 0x00,             /*3c: cmp    $0x303,%eax */
0x0f, 0x85, 0x8d, 0x00, 0x00, 0x00,       /*41: jne    d4 <op_add+0xd4> */
0x8b, 0x83, 0x00, 0x10, 0xab, 0x00,       /*47: mov    0xab1000(%rbx),%eax */
0x8b, 0x8b, 0x10, 0x10, 0xab, 0x00,       /*4d: mov    0xab1010(%rbx),%ecx */
0x89, 0xc2,                               /*53: mov    %eax,%edx */
0x01, 0xca,                               /*55: add    %ecx,%edx */
0x0f, 0x81, 0xcd, 0x00, 0x00, 0x00,       /*57: jno    12a <op_add+0x12a> */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*5d: movl   $0x6,0xab1008(%rbx) */
0xf2, 0x0f, 0x2a, 0xc0,                   /*67: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x2a, 0xc9,                   /*6b: cvtsi2sd %ecx,%xmm1 */
0xf2, 0x0f, 0x58, 0xc8,                   /*6f: addsd  %xmm0,%xmm1 */
0xf2, 0x0f, 0x11, 0x8b, 0x00, 0x10, 0xab, 0x00,/*73: movsd  %xmm1,0xab1000(%rbx) */
0xe9, 0xba, 0x00, 0x00, 0x00,             /*7b: jmpq   13a <op_add+0x13a> */
0x3d, 0x10, 0x10, 0x00, 0x00,             /*80: cmp    $0x1010,%eax */
0x0f, 0x85, 0x95, 0x00, 0x00, 0x00,       /*85: jne    120 <op_add+0x120> */
0x48, 0x8b, 0xb3, 0x00, 0x10, 0xab, 0x00, /*8b: mov    0xab1000(%rbx),%rsi */
0x48, 0x8b, 0x8b, 0x10, 0x10, 0xab, 0x00, /*92: mov    0xab1010(%rbx),%rcx */
0x4c, 0x89, 0xf7,                         /*99: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*9c: callq  a1 <op_add+0xa1> */
0x48, 0x89, 0x83, 0x00, 0x10, 0xab, 0x00, /*a1: mov    %rax,0xab1000(%rbx) */
0x89, 0x93, 0x08, 0x10, 0xab, 0x00,       /*a8: mov    %edx,0xab1008(%rbx) */
0xe9, 0x87, 0x00, 0x00, 0x00,             /*ae: jmpq   13a <op_add+0x13a> */
0x3d, 0x03, 0x06, 0x00, 0x00,             /*b3: cmp    $0x603,%eax */
0x75, 0x45,                               /*b8: jne    ff <op_add+0xff> */
0xf2, 0x0f, 0x2a, 0x83, 0x10, 0x10, 0xab, 0x00,/*ba: cvtsi2sdl 0xab1010(%rbx),%xmm0 */
0xf2, 0x0f, 0x58, 0x83, 0x00, 0x10, 0xab, 0x00,/*c2: addsd  0xab1000(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*ca: movsd  %xmm0,0xab1000(%rbx) */
0xeb, 0x66,                               /*d2: jmp    13a <op_add+0x13a> */
0x3d, 0x06, 0x03, 0x00, 0x00,             /*d4: cmp    $0x306,%eax */
0x75, 0x45,                               /*d9: jne    120 <op_add+0x120> */
0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x10, 0xab, 0x00,/*db: cvtsi2sdl 0xab1000(%rbx),%xmm0 */
0xf2, 0x0f, 0x58, 0x83, 0x10, 0x10, 0xab, 0x00,/*e3: addsd  0xab1010(%rbx),%xmm0 */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*eb: movl   $0x6,0xab1008(%rbx) */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*f5: movsd  %xmm0,0xab1000(%rbx) */
0xeb, 0x3b,                               /*fd: jmp    13a <op_add+0x13a> */
0x3d, 0x06, 0x06, 0x00, 0x00,             /*ff: cmp    $0x606,%eax */
0x75, 0x1a,                               /*104: jne    120 <op_add+0x120> */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x10, 0xab, 0x00,/*106: movsd  0xab1000(%rbx),%xmm0 */
0xf2, 0x0f, 0x58, 0x83, 0x10, 0x10, 0xab, 0x00,/*10e: addsd  0xab1010(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*116: movsd  %xmm0,0xab1000(%rbx) */
0xeb, 0x1a,                               /*11e: jmp    13a <op_add+0x13a> */
0x4c, 0x89, 0xff,                         /*120: mov    %r15,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*123: callq  128 <op_add+0x128> */
0xeb, 0x1b,                               /*128: jmp    145 <op_add+0x145> */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*12a: movl   $0x3,0xab1008(%rbx) */
0x89, 0x93, 0x00, 0x10, 0xab, 0x00,       /*134: mov    %edx,0xab1000(%rbx) */
0x41, 0x8b, 0x47, 0x50,                   /*13a: mov    0x50(%r15),%eax */
0x41, 0x89, 0x86, 0xdc, 0x00, 0x00, 0x00, /*13e: mov    %eax,0xdc(%r14) */
0x4c, 0x89, 0xff,                         /*145: mov    %r15,%rdi */
0x5b,                                     /*148: pop    %rbx */
0x41, 0x5e,                               /*149: pop    %r14 */
0x41, 0x5f,                               /*14b: pop    %r15 */

};
static uint8_t op_add__rodata[] = {

};

static void op_add_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = a * 16 + 8;
  *((int32_t *)(op + 25)) = a * 16 + 24;
  *((int32_t *)(op + 73)) = a * 16 + 0;
  *((int32_t *)(op + 79)) = a * 16 + 16;
  *((int32_t *)(op + 95)) = a * 16 + 8;
  *((int32_t *)(op + 119)) = a * 16 + 0;
  *((int32_t *)(op + 142)) = a * 16 + 0;
  *((int32_t *)(op + 149)) = a * 16 + 16;
  *((int32_t *)(op + 164)) = a * 16 + 0;
  *((int32_t *)(op + 170)) = a * 16 + 8;
  *((int32_t *)(op + 190)) = a * 16 + 16;
  *((int32_t *)(op + 198)) = a * 16 + 0;
  *((int32_t *)(op + 206)) = a * 16 + 0;
  *((int32_t *)(op + 223)) = a * 16 + 0;
  *((int32_t *)(op + 231)) = a * 16 + 16;
  *((int32_t *)(op + 237)) = a * 16 + 8;
  *((int32_t *)(op + 249)) = a * 16 + 0;
  *((int32_t *)(op + 266)) = a * 16 + 0;
  *((int32_t *)(op + 274)) = a * 16 + 16;
  *((int32_t *)(op + 282)) = a * 16 + 0;
  *((int32_t *)(op + 300)) = a * 16 + 8;
  *((int32_t *)(op + 310)) = a * 16 + 0;
}

static void op_add_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_add_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"c"=>[[1, 0, 5..8]], "a"=>[[16, 8, 15..18], [16, 0, 32..35], [16, 0, 40..43], [16, 0, 53..56], [16, 8, 65..68], [16, 0, 89..92], [16, 24, 97..100], [16, 16, 107..110], [1, 0, 117..120], [16, 8, 144..147], [16, 0, 154..157]], "b"=>[[1, 0, 122..125]]} */
static uint8_t op_addi__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*4: mov    $0xcd0000,%eax */
0x48, 0x8b, 0x4b, 0x18,                   /*9: mov    0x18(%rbx),%rcx */
0x8b, 0x91, 0x08, 0x10, 0xab, 0x00,       /*d: mov    0xab1008(%rcx),%edx */
0x83, 0xfa, 0x06,                         /*13: cmp    $0x6,%edx */
0x75, 0x16,                               /*16: jne    2e <op_addi+0x2e> */
0xf2, 0x0f, 0x2a, 0xc0,                   /*18: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x58, 0x81, 0x00, 0x10, 0xab, 0x00,/*1c: addsd  0xab1000(%rcx),%xmm0 */
0xf2, 0x0f, 0x11, 0x81, 0x00, 0x10, 0xab, 0x00,/*24: movsd  %xmm0,0xab1000(%rcx) */
0xeb, 0x70,                               /*2c: jmp    9e <op_addi+0x9e> */
0x83, 0xfa, 0x03,                         /*2e: cmp    $0x3,%edx */
0x75, 0x2c,                               /*31: jne    5f <op_addi+0x5f> */
0x8b, 0x91, 0x00, 0x10, 0xab, 0x00,       /*33: mov    0xab1000(%rcx),%edx */
0x89, 0xd6,                               /*39: mov    %edx,%esi */
0x01, 0xc6,                               /*3b: add    %eax,%esi */
0x71, 0x4f,                               /*3d: jno    8e <op_addi+0x8e> */
0xc7, 0x81, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*3f: movl   $0x6,0xab1008(%rcx) */
0xf2, 0x0f, 0x2a, 0xc2,                   /*49: cvtsi2sd %edx,%xmm0 */
0xf2, 0x0f, 0x2a, 0xc8,                   /*4d: cvtsi2sd %eax,%xmm1 */
0xf2, 0x0f, 0x58, 0xc8,                   /*51: addsd  %xmm0,%xmm1 */
0xf2, 0x0f, 0x11, 0x89, 0x00, 0x10, 0xab, 0x00,/*55: movsd  %xmm1,0xab1000(%rcx) */
0xeb, 0x3f,                               /*5d: jmp    9e <op_addi+0x9e> */
0xc7, 0x81, 0x18, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*5f: movl   $0x3,0xab1018(%rcx) */
0x89, 0x81, 0x10, 0x10, 0xab, 0x00,       /*69: mov    %eax,0xab1010(%rcx) */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*6f: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*74: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*79: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x01, 0x00, 0x00, 0x00,       /*7e: mov    $0x1,%r8d */
0x48, 0x89, 0xdf,                         /*84: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*87: callq  8c <op_addi+0x8c> */
0xeb, 0x10,                               /*8c: jmp    9e <op_addi+0x9e> */
0xc7, 0x81, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*8e: movl   $0x3,0xab1008(%rcx) */
0x89, 0xb1, 0x00, 0x10, 0xab, 0x00,       /*98: mov    %esi,0xab1000(%rcx) */
0x48, 0x89, 0xdf,                         /*9e: mov    %rbx,%rdi */
0x5b,                                     /*a1: pop    %rbx */

};
static uint8_t op_addi__rodata[] = {

};

static void op_addi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = c * 1 + 0;
  *((int32_t *)(op + 15)) = a * 16 + 8;
  *((int32_t *)(op + 32)) = a * 16 + 0;
  *((int32_t *)(op + 40)) = a * 16 + 0;
  *((int32_t *)(op + 53)) = a * 16 + 0;
  *((int32_t *)(op + 65)) = a * 16 + 8;
  *((int32_t *)(op + 89)) = a * 16 + 0;
  *((int32_t *)(op + 97)) = a * 16 + 24;
  *((int32_t *)(op + 107)) = a * 16 + 16;
  *((int32_t *)(op + 117)) = a * 1 + 0;
  *((int32_t *)(op + 144)) = a * 16 + 8;
  *((int32_t *)(op + 154)) = a * 16 + 0;
  *((int32_t *)(op + 122)) = b * 1 + 0;
}

static void op_addi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_addi_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 10..13], [16, 24, 26..29], [16, 0, 50..53], [16, 16, 56..59], [16, 8, 72..75], [16, 0, 102..105], [16, 16, 110..113], [16, 0, 122..125], [16, 0, 140..143], [16, 16, 148..151], [16, 8, 154..157], [16, 0, 166..169], [16, 0, 184..187], [16, 16, 192..195], [16, 0, 200..203], [16, 8, 218..221], [16, 0, 228..231]]} */
static uint8_t op_sub__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*8: mov    0xab1008(%rax),%edx */
0xc1, 0xe2, 0x08,                         /*e: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*11: and    $0xffff00,%edx */
0x0f, 0xb6, 0x88, 0x18, 0x10, 0xab, 0x00, /*17: movzbl 0xab1018(%rax),%ecx */
0x09, 0xd1,                               /*1e: or     %edx,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*20: cmp    $0x602,%ecx */
0x7f, 0x32,                               /*26: jg     5a <op_sub+0x5a> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*28: cmp    $0x303,%ecx */
0x75, 0x50,                               /*2e: jne    80 <op_sub+0x80> */
0x8b, 0x88, 0x00, 0x10, 0xab, 0x00,       /*30: mov    0xab1000(%rax),%ecx */
0x8b, 0x90, 0x10, 0x10, 0xab, 0x00,       /*36: mov    0xab1010(%rax),%edx */
0x89, 0xce,                               /*3c: mov    %ecx,%esi */
0x29, 0xd6,                               /*3e: sub    %edx,%esi */
0x0f, 0x81, 0x92, 0x00, 0x00, 0x00,       /*40: jno    d8 <op_sub+0xd8> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*46: movl   $0x6,0xab1008(%rax) */
0xf2, 0x0f, 0x2a, 0xc1,                   /*50: cvtsi2sd %ecx,%xmm0 */
0xf2, 0x0f, 0x2a, 0xca,                   /*54: cvtsi2sd %edx,%xmm1 */
0xeb, 0x18,                               /*58: jmp    72 <op_sub+0x72> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*5a: cmp    $0x603,%ecx */
0x75, 0x4a,                               /*60: jne    ac <op_sub+0xac> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*62: movsd  0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x2a, 0x88, 0x10, 0x10, 0xab, 0x00,/*6a: cvtsi2sdl 0xab1010(%rax),%xmm1 */
0xf2, 0x0f, 0x5c, 0xc1,                   /*72: subsd  %xmm1,%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x10, 0xab, 0x00,/*76: movsd  %xmm0,0xab1000(%rax) */
0xeb, 0x68,                               /*7e: jmp    e8 <op_sub+0xe8> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*80: cmp    $0x306,%ecx */
0x75, 0x46,                               /*86: jne    ce <op_sub+0xce> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x10, 0xab, 0x00,/*88: cvtsi2sdl 0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x5c, 0x80, 0x10, 0x10, 0xab, 0x00,/*90: subsd  0xab1010(%rax),%xmm0 */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*98: movl   $0x6,0xab1008(%rax) */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x10, 0xab, 0x00,/*a2: movsd  %xmm0,0xab1000(%rax) */
0xeb, 0x3c,                               /*aa: jmp    e8 <op_sub+0xe8> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*ac: cmp    $0x606,%ecx */
0x75, 0x1a,                               /*b2: jne    ce <op_sub+0xce> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*b4: movsd  0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x5c, 0x80, 0x10, 0x10, 0xab, 0x00,/*bc: subsd  0xab1010(%rax),%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x10, 0xab, 0x00,/*c4: movsd  %xmm0,0xab1000(%rax) */
0xeb, 0x1a,                               /*cc: jmp    e8 <op_sub+0xe8> */
0x48, 0x89, 0xdf,                         /*ce: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*d1: callq  d6 <op_sub+0xd6> */
0xeb, 0x10,                               /*d6: jmp    e8 <op_sub+0xe8> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*d8: movl   $0x3,0xab1008(%rax) */
0x89, 0xb0, 0x00, 0x10, 0xab, 0x00,       /*e2: mov    %esi,0xab1000(%rax) */
0x48, 0x89, 0xdf,                         /*e8: mov    %rbx,%rdi */
0x5b,                                     /*eb: pop    %rbx */

};
static uint8_t op_sub__rodata[] = {

};

static void op_sub_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 16 + 8;
  *((int32_t *)(op + 26)) = a * 16 + 24;
  *((int32_t *)(op + 50)) = a * 16 + 0;
  *((int32_t *)(op + 56)) = a * 16 + 16;
  *((int32_t *)(op + 72)) = a * 16 + 8;
  *((int32_t *)(op + 102)) = a * 16 + 0;
  *((int32_t *)(op + 110)) = a * 16 + 16;
  *((int32_t *)(op + 122)) = a * 16 + 0;
  *((int32_t *)(op + 140)) = a * 16 + 0;
  *((int32_t *)(op + 148)) = a * 16 + 16;
  *((int32_t *)(op + 154)) = a * 16 + 8;
  *((int32_t *)(op + 166)) = a * 16 + 0;
  *((int32_t *)(op + 184)) = a * 16 + 0;
  *((int32_t *)(op + 192)) = a * 16 + 16;
  *((int32_t *)(op + 200)) = a * 16 + 0;
  *((int32_t *)(op + 218)) = a * 16 + 8;
  *((int32_t *)(op + 228)) = a * 16 + 0;
}

static void op_sub_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sub_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"c"=>[[1, 0, 5..8]], "a"=>[[16, 8, 15..18], [16, 0, 32..35], [16, 0, 44..47], [16, 0, 57..60], [16, 8, 69..72], [16, 0, 93..96], [16, 24, 101..104], [16, 16, 111..114], [1, 0, 121..124], [16, 8, 148..151], [16, 0, 158..161]], "b"=>[[1, 0, 126..129]]} */
static uint8_t op_subi__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*4: mov    $0xcd0000,%eax */
0x48, 0x8b, 0x4b, 0x18,                   /*9: mov    0x18(%rbx),%rcx */
0x8b, 0x91, 0x08, 0x10, 0xab, 0x00,       /*d: mov    0xab1008(%rcx),%edx */
0x83, 0xfa, 0x06,                         /*13: cmp    $0x6,%edx */
0x75, 0x1a,                               /*16: jne    32 <op_subi+0x32> */
0xf2, 0x0f, 0x2a, 0xc0,                   /*18: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x10, 0x89, 0x00, 0x10, 0xab, 0x00,/*1c: movsd  0xab1000(%rcx),%xmm1 */
0xf2, 0x0f, 0x5c, 0xc8,                   /*24: subsd  %xmm0,%xmm1 */
0xf2, 0x0f, 0x11, 0x89, 0x00, 0x10, 0xab, 0x00,/*28: movsd  %xmm1,0xab1000(%rcx) */
0xeb, 0x70,                               /*30: jmp    a2 <op_subi+0xa2> */
0x83, 0xfa, 0x03,                         /*32: cmp    $0x3,%edx */
0x75, 0x2c,                               /*35: jne    63 <op_subi+0x63> */
0x8b, 0x91, 0x00, 0x10, 0xab, 0x00,       /*37: mov    0xab1000(%rcx),%edx */
0x89, 0xd6,                               /*3d: mov    %edx,%esi */
0x29, 0xc6,                               /*3f: sub    %eax,%esi */
0x71, 0x4f,                               /*41: jno    92 <op_subi+0x92> */
0xc7, 0x81, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*43: movl   $0x6,0xab1008(%rcx) */
0xf2, 0x0f, 0x2a, 0xc2,                   /*4d: cvtsi2sd %edx,%xmm0 */
0xf2, 0x0f, 0x2a, 0xc8,                   /*51: cvtsi2sd %eax,%xmm1 */
0xf2, 0x0f, 0x5c, 0xc1,                   /*55: subsd  %xmm1,%xmm0 */
0xf2, 0x0f, 0x11, 0x81, 0x00, 0x10, 0xab, 0x00,/*59: movsd  %xmm0,0xab1000(%rcx) */
0xeb, 0x3f,                               /*61: jmp    a2 <op_subi+0xa2> */
0xc7, 0x81, 0x18, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*63: movl   $0x3,0xab1018(%rcx) */
0x89, 0x81, 0x10, 0x10, 0xab, 0x00,       /*6d: mov    %eax,0xab1010(%rcx) */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*73: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*78: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*7d: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x01, 0x00, 0x00, 0x00,       /*82: mov    $0x1,%r8d */
0x48, 0x89, 0xdf,                         /*88: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*8b: callq  90 <op_subi+0x90> */
0xeb, 0x10,                               /*90: jmp    a2 <op_subi+0xa2> */
0xc7, 0x81, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*92: movl   $0x3,0xab1008(%rcx) */
0x89, 0xb1, 0x00, 0x10, 0xab, 0x00,       /*9c: mov    %esi,0xab1000(%rcx) */
0x48, 0x89, 0xdf,                         /*a2: mov    %rbx,%rdi */
0x5b,                                     /*a5: pop    %rbx */

};
static uint8_t op_subi__rodata[] = {

};

static void op_subi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = c * 1 + 0;
  *((int32_t *)(op + 15)) = a * 16 + 8;
  *((int32_t *)(op + 32)) = a * 16 + 0;
  *((int32_t *)(op + 44)) = a * 16 + 0;
  *((int32_t *)(op + 57)) = a * 16 + 0;
  *((int32_t *)(op + 69)) = a * 16 + 8;
  *((int32_t *)(op + 93)) = a * 16 + 0;
  *((int32_t *)(op + 101)) = a * 16 + 24;
  *((int32_t *)(op + 111)) = a * 16 + 16;
  *((int32_t *)(op + 121)) = a * 1 + 0;
  *((int32_t *)(op + 148)) = a * 16 + 8;
  *((int32_t *)(op + 158)) = a * 16 + 0;
  *((int32_t *)(op + 126)) = b * 1 + 0;
}

static void op_subi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_subi_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 12..15], [16, 24, 19..22], [16, 0, 61..64], [16, 16, 68..71], [16, 8, 88..91], [16, 0, 99..102], [16, 16, 119..122], [16, 0, 127..130], [16, 0, 135..138], [16, 0, 152..155], [16, 16, 160..163], [16, 8, 166..169], [16, 0, 178..181], [16, 0, 195..198], [16, 16, 203..206], [16, 0, 211..214], [16, 8, 234..237], [16, 0, 244..247]]} */
static uint8_t op_mul__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x8b, 0x93, 0x08, 0x10, 0xab, 0x00,       /*a: mov    0xab1008(%rbx),%edx */
0x44, 0x8b, 0x83, 0x18, 0x10, 0xab, 0x00, /*10: mov    0xab1018(%rbx),%r8d */
0x89, 0xd1,                               /*17: mov    %edx,%ecx */
0xc1, 0xe1, 0x08,                         /*19: shl    $0x8,%ecx */
0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*1c: and    $0xffff00,%ecx */
0x41, 0x0f, 0xb6, 0xc0,                   /*22: movzbl %r8b,%eax */
0x09, 0xc8,                               /*26: or     %ecx,%eax */
0x3d, 0x02, 0x06, 0x00, 0x00,             /*28: cmp    $0x602,%eax */
0x7f, 0x3d,                               /*2d: jg     6c <op_mul+0x6c> */
0x3d, 0x03, 0x03, 0x00, 0x00,             /*2f: cmp    $0x303,%eax */
0x75, 0x57,                               /*34: jne    8d <op_mul+0x8d> */
0x49, 0x8b, 0x7e, 0x58,                   /*36: mov    0x58(%r14),%rdi */
0x48, 0x8b, 0xb3, 0x00, 0x10, 0xab, 0x00, /*3a: mov    0xab1000(%rbx),%rsi */
0x48, 0x8b, 0x8b, 0x10, 0x10, 0xab, 0x00, /*41: mov    0xab1010(%rbx),%rcx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*48: callq  4d <op_mul+0x4d> */
0x83, 0xfa, 0x06,                         /*4d: cmp    $0x6,%edx */
0x0f, 0x85, 0x8d, 0x00, 0x00, 0x00,       /*50: jne    e3 <op_mul+0xe3> */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*56: movl   $0x6,0xab1008(%rbx) */
0x48, 0x89, 0x83, 0x00, 0x10, 0xab, 0x00, /*60: mov    %rax,0xab1000(%rbx) */
0xe9, 0x8c, 0x00, 0x00, 0x00,             /*67: jmpq   f8 <op_mul+0xf8> */
0x3d, 0x03, 0x06, 0x00, 0x00,             /*6c: cmp    $0x603,%eax */
0x75, 0x45,                               /*71: jne    b8 <op_mul+0xb8> */
0xf2, 0x0f, 0x2a, 0x83, 0x10, 0x10, 0xab, 0x00,/*73: cvtsi2sdl 0xab1010(%rbx),%xmm0 */
0xf2, 0x0f, 0x59, 0x83, 0x00, 0x10, 0xab, 0x00,/*7b: mulsd  0xab1000(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*83: movsd  %xmm0,0xab1000(%rbx) */
0xeb, 0x6b,                               /*8b: jmp    f8 <op_mul+0xf8> */
0x3d, 0x06, 0x03, 0x00, 0x00,             /*8d: cmp    $0x306,%eax */
0x75, 0x45,                               /*92: jne    d9 <op_mul+0xd9> */
0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x10, 0xab, 0x00,/*94: cvtsi2sdl 0xab1000(%rbx),%xmm0 */
0xf2, 0x0f, 0x59, 0x83, 0x10, 0x10, 0xab, 0x00,/*9c: mulsd  0xab1010(%rbx),%xmm0 */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*a4: movl   $0x6,0xab1008(%rbx) */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*ae: movsd  %xmm0,0xab1000(%rbx) */
0xeb, 0x40,                               /*b6: jmp    f8 <op_mul+0xf8> */
0x3d, 0x06, 0x06, 0x00, 0x00,             /*b8: cmp    $0x606,%eax */
0x75, 0x1a,                               /*bd: jne    d9 <op_mul+0xd9> */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x10, 0xab, 0x00,/*bf: movsd  0xab1000(%rbx),%xmm0 */
0xf2, 0x0f, 0x59, 0x83, 0x10, 0x10, 0xab, 0x00,/*c7: mulsd  0xab1010(%rbx),%xmm0 */
0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*cf: movsd  %xmm0,0xab1000(%rbx) */
0xeb, 0x1f,                               /*d7: jmp    f8 <op_mul+0xf8> */
0x4c, 0x89, 0xf7,                         /*d9: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*dc: callq  e1 <op_mul+0xe1> */
0xeb, 0x15,                               /*e1: jmp    f8 <op_mul+0xf8> */
0x83, 0xfa, 0x03,                         /*e3: cmp    $0x3,%edx */
0x75, 0x10,                               /*e6: jne    f8 <op_mul+0xf8> */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*e8: movl   $0x3,0xab1008(%rbx) */
0x89, 0x83, 0x00, 0x10, 0xab, 0x00,       /*f2: mov    %eax,0xab1000(%rbx) */
0x4c, 0x89, 0xf7,                         /*f8: mov    %r14,%rdi */
0x5b,                                     /*fb: pop    %rbx */
0x41, 0x5e,                               /*fc: pop    %r14 */

};
static uint8_t op_mul__rodata[] = {

};

static void op_mul_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 12)) = a * 16 + 8;
  *((int32_t *)(op + 19)) = a * 16 + 24;
  *((int32_t *)(op + 61)) = a * 16 + 0;
  *((int32_t *)(op + 68)) = a * 16 + 16;
  *((int32_t *)(op + 88)) = a * 16 + 8;
  *((int32_t *)(op + 99)) = a * 16 + 0;
  *((int32_t *)(op + 119)) = a * 16 + 16;
  *((int32_t *)(op + 127)) = a * 16 + 0;
  *((int32_t *)(op + 135)) = a * 16 + 0;
  *((int32_t *)(op + 152)) = a * 16 + 0;
  *((int32_t *)(op + 160)) = a * 16 + 16;
  *((int32_t *)(op + 166)) = a * 16 + 8;
  *((int32_t *)(op + 178)) = a * 16 + 0;
  *((int32_t *)(op + 195)) = a * 16 + 0;
  *((int32_t *)(op + 203)) = a * 16 + 16;
  *((int32_t *)(op + 211)) = a * 16 + 0;
  *((int32_t *)(op + 234)) = a * 16 + 8;
  *((int32_t *)(op + 244)) = a * 16 + 0;
}

static void op_mul_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_mul_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 10..13], [16, 24, 26..29], [16, 0, 52..55], [16, 16, 60..63], [16, 8, 66..69], [16, 0, 88..91], [16, 16, 96..99], [16, 0, 108..111], [16, 0, 126..129], [16, 16, 134..137], [16, 8, 140..143], [16, 0, 152..155], [16, 0, 170..173], [16, 16, 178..181], [16, 0, 186..189]]} */
static uint8_t op_div__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*8: mov    0xab1008(%rax),%edx */
0xc1, 0xe2, 0x08,                         /*e: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*11: and    $0xffff00,%edx */
0x0f, 0xb6, 0x88, 0x18, 0x10, 0xab, 0x00, /*17: movzbl 0xab1018(%rax),%ecx */
0x09, 0xd1,                               /*1e: or     %edx,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*20: cmp    $0x602,%ecx */
0x7f, 0x24,                               /*26: jg     4c <op_div+0x4c> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*28: cmp    $0x303,%ecx */
0x75, 0x42,                               /*2e: jne    72 <op_div+0x72> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x10, 0xab, 0x00,/*30: cvtsi2sdl 0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x2a, 0x88, 0x10, 0x10, 0xab, 0x00,/*38: cvtsi2sdl 0xab1010(%rax),%xmm1 */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*40: movl   $0x6,0xab1008(%rax) */
0xeb, 0x18,                               /*4a: jmp    64 <op_div+0x64> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*4c: cmp    $0x603,%ecx */
0x75, 0x4a,                               /*52: jne    9e <op_div+0x9e> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*54: movsd  0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x2a, 0x88, 0x10, 0x10, 0xab, 0x00,/*5c: cvtsi2sdl 0xab1010(%rax),%xmm1 */
0xf2, 0x0f, 0x5e, 0xc1,                   /*64: divsd  %xmm1,%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x10, 0xab, 0x00,/*68: movsd  %xmm0,0xab1000(%rax) */
0xeb, 0x56,                               /*70: jmp    c8 <op_div+0xc8> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*72: cmp    $0x306,%ecx */
0x75, 0x46,                               /*78: jne    c0 <op_div+0xc0> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x10, 0xab, 0x00,/*7a: cvtsi2sdl 0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x5e, 0x80, 0x10, 0x10, 0xab, 0x00,/*82: divsd  0xab1010(%rax),%xmm0 */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*8a: movl   $0x6,0xab1008(%rax) */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x10, 0xab, 0x00,/*94: movsd  %xmm0,0xab1000(%rax) */
0xeb, 0x2a,                               /*9c: jmp    c8 <op_div+0xc8> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*9e: cmp    $0x606,%ecx */
0x75, 0x1a,                               /*a4: jne    c0 <op_div+0xc0> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*a6: movsd  0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x5e, 0x80, 0x10, 0x10, 0xab, 0x00,/*ae: divsd  0xab1010(%rax),%xmm0 */
0xf2, 0x0f, 0x11, 0x80, 0x00, 0x10, 0xab, 0x00,/*b6: movsd  %xmm0,0xab1000(%rax) */
0xeb, 0x08,                               /*be: jmp    c8 <op_div+0xc8> */
0x48, 0x89, 0xdf,                         /*c0: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c3: callq  c8 <op_div+0xc8> */
0x48, 0x89, 0xdf,                         /*c8: mov    %rbx,%rdi */
0x5b,                                     /*cb: pop    %rbx */

};
static uint8_t op_div__rodata[] = {

};

static void op_div_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 16 + 8;
  *((int32_t *)(op + 26)) = a * 16 + 24;
  *((int32_t *)(op + 52)) = a * 16 + 0;
  *((int32_t *)(op + 60)) = a * 16 + 16;
  *((int32_t *)(op + 66)) = a * 16 + 8;
  *((int32_t *)(op + 88)) = a * 16 + 0;
  *((int32_t *)(op + 96)) = a * 16 + 16;
  *((int32_t *)(op + 108)) = a * 16 + 0;
  *((int32_t *)(op + 126)) = a * 16 + 0;
  *((int32_t *)(op + 134)) = a * 16 + 16;
  *((int32_t *)(op + 140)) = a * 16 + 8;
  *((int32_t *)(op + 152)) = a * 16 + 0;
  *((int32_t *)(op + 170)) = a * 16 + 0;
  *((int32_t *)(op + 178)) = a * 16 + 16;
  *((int32_t *)(op + 186)) = a * 16 + 0;
}

static void op_div_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_div_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 0, 17..20], [16, 8, 23..26], [16, 16, 30..33], [16, 24, 37..40], [16, 8, 56..59], [16, 24, 72..75], [16, 0, 94..97], [16, 16, 100..103], [16, 16, 123..126], [16, 0, 131..134], [16, 0, 149..152], [16, 0, 166..169], [16, 16, 174..177], [16, 8, 193..196], [16, 0, 203..206], [16, 8, 221..224], [1, 0, 237..240]], "b"=>[[1, 0, 242..245]], "c"=>[[1, 0, 248..251]]} */
static uint8_t op_eq__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x49, 0x8b, 0x7e, 0x58,                   /*a: mov    0x58(%r14),%rdi */
0x48, 0x8b, 0xb3, 0x00, 0x10, 0xab, 0x00, /*e: mov    0xab1000(%rbx),%rsi */
0x8b, 0x93, 0x08, 0x10, 0xab, 0x00,       /*15: mov    0xab1008(%rbx),%edx */
0x48, 0x8b, 0x8b, 0x10, 0x10, 0xab, 0x00, /*1b: mov    0xab1010(%rbx),%rcx */
0x44, 0x8b, 0x83, 0x18, 0x10, 0xab, 0x00, /*22: mov    0xab1018(%rbx),%r8d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*29: callq  2e <op_eq+0x2e> */
0x84, 0xc0,                               /*2e: test   %al,%al */
0x0f, 0x85, 0x89, 0x00, 0x00, 0x00,       /*30: jne    bf <op_eq+0xbf> */
0x8b, 0x8b, 0x08, 0x10, 0xab, 0x00,       /*36: mov    0xab1008(%rbx),%ecx */
0xc1, 0xe1, 0x08,                         /*3c: shl    $0x8,%ecx */
0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*3f: and    $0xffff00,%ecx */
0x0f, 0xb6, 0x83, 0x18, 0x10, 0xab, 0x00, /*45: movzbl 0xab1018(%rbx),%eax */
0x09, 0xc8,                               /*4c: or     %ecx,%eax */
0x3d, 0x02, 0x06, 0x00, 0x00,             /*4e: cmp    $0x602,%eax */
0x7f, 0x1b,                               /*53: jg     70 <op_eq+0x70> */
0x3d, 0x03, 0x03, 0x00, 0x00,             /*55: cmp    $0x303,%eax */
0x75, 0x2e,                               /*5a: jne    8a <op_eq+0x8a> */
0x8b, 0x83, 0x00, 0x10, 0xab, 0x00,       /*5c: mov    0xab1000(%rbx),%eax */
0x3b, 0x83, 0x10, 0x10, 0xab, 0x00,       /*62: cmp    0xab1010(%rbx),%eax */
0x0f, 0x94, 0xc0,                         /*68: sete   %al */
0x0f, 0xb6, 0xc0,                         /*6b: movzbl %al,%eax */
0xeb, 0x4b,                               /*6e: jmp    bb <op_eq+0xbb> */
0x3d, 0x03, 0x06, 0x00, 0x00,             /*70: cmp    $0x603,%eax */
0x75, 0x24,                               /*75: jne    9b <op_eq+0x9b> */
0xf2, 0x0f, 0x2a, 0x83, 0x10, 0x10, 0xab, 0x00,/*77: cvtsi2sdl 0xab1010(%rbx),%xmm0 */
0xf2, 0x0f, 0xc2, 0x83, 0x00, 0x10, 0xab, 0x00, 0x00,/*7f: cmpeqsd 0xab1000(%rbx),%xmm0 */
0xeb, 0x29,                               /*88: jmp    b3 <op_eq+0xb3> */
0x3d, 0x06, 0x03, 0x00, 0x00,             /*8a: cmp    $0x306,%eax */
0x75, 0x56,                               /*8f: jne    e7 <op_eq+0xe7> */
0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x10, 0xab, 0x00,/*91: cvtsi2sdl 0xab1000(%rbx),%xmm0 */
0xeb, 0x0f,                               /*99: jmp    aa <op_eq+0xaa> */
0x3d, 0x06, 0x06, 0x00, 0x00,             /*9b: cmp    $0x606,%eax */
0x75, 0x45,                               /*a0: jne    e7 <op_eq+0xe7> */
0xf2, 0x0f, 0x10, 0x83, 0x00, 0x10, 0xab, 0x00,/*a2: movsd  0xab1000(%rbx),%xmm0 */
0xf2, 0x0f, 0xc2, 0x83, 0x10, 0x10, 0xab, 0x00, 0x00,/*aa: cmpeqsd 0xab1010(%rbx),%xmm0 */
0x66, 0x48, 0x0f, 0x7e, 0xc0,             /*b3: movq   %xmm0,%rax */
0x83, 0xe0, 0x01,                         /*b8: and    $0x1,%eax */
0x85, 0xc0,                               /*bb: test   %eax,%eax */
0x74, 0x1c,                               /*bd: je     db <op_eq+0xdb> */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*bf: movl   $0x2,0xab1008(%rbx) */
0xc7, 0x83, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*c9: movl   $0x1,0xab1000(%rbx) */
0x4c, 0x89, 0xf7,                         /*d3: mov    %r14,%rdi */
0x5b,                                     /*d6: pop    %rbx */
0x41, 0x5e,                               /*d7: pop    %r14 */
0xeb, 0x2b,                               /*d9: jmp    106 <op_eq+0x106> */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*db: movl   $0x0,0xab1008(%rbx) */
0xeb, 0xe2,                               /*e5: jmp    c9 <op_eq+0xc9> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*e7: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*ec: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*f1: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*f6: mov    $0xcd0000,%r8d */
0x4c, 0x89, 0xf7,                         /*fc: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*ff: callq  104 <op_eq+0x104> */
0xeb, 0xcd,                               /*104: jmp    d3 <op_eq+0xd3> */

};
static uint8_t op_eq__rodata[] = {

};

static void op_eq_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 16 + 0;
  *((int32_t *)(op + 23)) = a * 16 + 8;
  *((int32_t *)(op + 30)) = a * 16 + 16;
  *((int32_t *)(op + 37)) = a * 16 + 24;
  *((int32_t *)(op + 56)) = a * 16 + 8;
  *((int32_t *)(op + 72)) = a * 16 + 24;
  *((int32_t *)(op + 94)) = a * 16 + 0;
  *((int32_t *)(op + 100)) = a * 16 + 16;
  *((int32_t *)(op + 123)) = a * 16 + 16;
  *((int32_t *)(op + 131)) = a * 16 + 0;
  *((int32_t *)(op + 149)) = a * 16 + 0;
  *((int32_t *)(op + 166)) = a * 16 + 0;
  *((int32_t *)(op + 174)) = a * 16 + 16;
  *((int32_t *)(op + 193)) = a * 16 + 8;
  *((int32_t *)(op + 203)) = a * 16 + 0;
  *((int32_t *)(op + 221)) = a * 16 + 8;
  *((int32_t *)(op + 237)) = a * 1 + 0;
  *((int32_t *)(op + 242)) = b * 1 + 0;
  *((int32_t *)(op + 248)) = c * 1 + 0;
}

static void op_eq_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_eq_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 10..13], [16, 24, 26..29], [16, 0, 50..53], [16, 16, 56..59], [16, 16, 77..80], [16, 0, 95..98], [16, 16, 103..106], [16, 16, 128..131], [16, 0, 136..139], [16, 8, 152..155], [16, 8, 164..167], [16, 0, 174..177], [1, 0, 190..193]], "b"=>[[1, 0, 195..198]], "c"=>[[1, 0, 201..204]]} */
static uint8_t op_lt__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*8: mov    0xab1008(%rax),%edx */
0xc1, 0xe2, 0x08,                         /*e: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*11: and    $0xffff00,%edx */
0x0f, 0xb6, 0x88, 0x18, 0x10, 0xab, 0x00, /*17: movzbl 0xab1018(%rax),%ecx */
0x09, 0xd1,                               /*1e: or     %edx,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*20: cmp    $0x602,%ecx */
0x7f, 0x19,                               /*26: jg     41 <op_lt+0x41> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*28: cmp    $0x303,%ecx */
0x75, 0x23,                               /*2e: jne    53 <op_lt+0x53> */
0x8b, 0x88, 0x00, 0x10, 0xab, 0x00,       /*30: mov    0xab1000(%rax),%ecx */
0x3b, 0x88, 0x10, 0x10, 0xab, 0x00,       /*36: cmp    0xab1010(%rax),%ecx */
0x0f, 0x9c, 0xc1,                         /*3c: setl   %cl */
0xeb, 0x4e,                               /*3f: jmp    8f <op_lt+0x8f> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*41: cmp    $0x603,%ecx */
0x75, 0x2b,                               /*47: jne    74 <op_lt+0x74> */
0xf2, 0x0f, 0x2a, 0x80, 0x10, 0x10, 0xab, 0x00,/*49: cvtsi2sdl 0xab1010(%rax),%xmm0 */
0xeb, 0x31,                               /*51: jmp    84 <op_lt+0x84> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*53: cmp    $0x306,%ecx */
0x75, 0x5d,                               /*59: jne    b8 <op_lt+0xb8> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x10, 0xab, 0x00,/*5b: cvtsi2sdl 0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x10, 0x88, 0x10, 0x10, 0xab, 0x00,/*63: movsd  0xab1010(%rax),%xmm1 */
0x66, 0x0f, 0x2e, 0xc8,                   /*6b: ucomisd %xmm0,%xmm1 */
0x0f, 0x97, 0xc1,                         /*6f: seta   %cl */
0xeb, 0x1b,                               /*72: jmp    8f <op_lt+0x8f> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*74: cmp    $0x606,%ecx */
0x75, 0x3c,                               /*7a: jne    b8 <op_lt+0xb8> */
0xf2, 0x0f, 0x10, 0x80, 0x10, 0x10, 0xab, 0x00,/*7c: movsd  0xab1010(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0x80, 0x00, 0x10, 0xab, 0x00,/*84: ucomisd 0xab1000(%rax),%xmm0 */
0x0f, 0x97, 0xc1,                         /*8c: seta   %cl */
0x0f, 0xb6, 0xc9,                         /*8f: movzbl %cl,%ecx */
0x85, 0xc9,                               /*92: test   %ecx,%ecx */
0x74, 0x0c,                               /*94: je     a2 <op_lt+0xa2> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*96: movl   $0x2,0xab1008(%rax) */
0xeb, 0x0a,                               /*a0: jmp    ac <op_lt+0xac> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*a2: movl   $0x0,0xab1008(%rax) */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*ac: movl   $0x1,0xab1000(%rax) */
0xeb, 0x1d,                               /*b6: jmp    d5 <op_lt+0xd5> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*b8: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*bd: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*c2: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*c7: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*cd: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*d0: callq  d5 <op_lt+0xd5> */
0x48, 0x89, 0xdf,                         /*d5: mov    %rbx,%rdi */
0x5b,                                     /*d8: pop    %rbx */

};
static uint8_t op_lt__rodata[] = {

};

static void op_lt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 16 + 8;
  *((int32_t *)(op + 26)) = a * 16 + 24;
  *((int32_t *)(op + 50)) = a * 16 + 0;
  *((int32_t *)(op + 56)) = a * 16 + 16;
  *((int32_t *)(op + 77)) = a * 16 + 16;
  *((int32_t *)(op + 95)) = a * 16 + 0;
  *((int32_t *)(op + 103)) = a * 16 + 16;
  *((int32_t *)(op + 128)) = a * 16 + 16;
  *((int32_t *)(op + 136)) = a * 16 + 0;
  *((int32_t *)(op + 152)) = a * 16 + 8;
  *((int32_t *)(op + 164)) = a * 16 + 8;
  *((int32_t *)(op + 174)) = a * 16 + 0;
  *((int32_t *)(op + 190)) = a * 1 + 0;
  *((int32_t *)(op + 195)) = b * 1 + 0;
  *((int32_t *)(op + 201)) = c * 1 + 0;
}

static void op_lt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_lt_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 10..13], [16, 24, 26..29], [16, 0, 50..53], [16, 16, 56..59], [16, 16, 77..80], [16, 0, 95..98], [16, 16, 103..106], [16, 16, 128..131], [16, 0, 136..139], [16, 8, 152..155], [16, 8, 164..167], [16, 0, 174..177], [1, 0, 190..193]], "b"=>[[1, 0, 195..198]], "c"=>[[1, 0, 201..204]]} */
static uint8_t op_le__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*8: mov    0xab1008(%rax),%edx */
0xc1, 0xe2, 0x08,                         /*e: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*11: and    $0xffff00,%edx */
0x0f, 0xb6, 0x88, 0x18, 0x10, 0xab, 0x00, /*17: movzbl 0xab1018(%rax),%ecx */
0x09, 0xd1,                               /*1e: or     %edx,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*20: cmp    $0x602,%ecx */
0x7f, 0x19,                               /*26: jg     41 <op_le+0x41> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*28: cmp    $0x303,%ecx */
0x75, 0x23,                               /*2e: jne    53 <op_le+0x53> */
0x8b, 0x88, 0x00, 0x10, 0xab, 0x00,       /*30: mov    0xab1000(%rax),%ecx */
0x3b, 0x88, 0x10, 0x10, 0xab, 0x00,       /*36: cmp    0xab1010(%rax),%ecx */
0x0f, 0x9e, 0xc1,                         /*3c: setle  %cl */
0xeb, 0x4e,                               /*3f: jmp    8f <op_le+0x8f> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*41: cmp    $0x603,%ecx */
0x75, 0x2b,                               /*47: jne    74 <op_le+0x74> */
0xf2, 0x0f, 0x2a, 0x80, 0x10, 0x10, 0xab, 0x00,/*49: cvtsi2sdl 0xab1010(%rax),%xmm0 */
0xeb, 0x31,                               /*51: jmp    84 <op_le+0x84> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*53: cmp    $0x306,%ecx */
0x75, 0x5d,                               /*59: jne    b8 <op_le+0xb8> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x10, 0xab, 0x00,/*5b: cvtsi2sdl 0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x10, 0x88, 0x10, 0x10, 0xab, 0x00,/*63: movsd  0xab1010(%rax),%xmm1 */
0x66, 0x0f, 0x2e, 0xc8,                   /*6b: ucomisd %xmm0,%xmm1 */
0x0f, 0x93, 0xc1,                         /*6f: setae  %cl */
0xeb, 0x1b,                               /*72: jmp    8f <op_le+0x8f> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*74: cmp    $0x606,%ecx */
0x75, 0x3c,                               /*7a: jne    b8 <op_le+0xb8> */
0xf2, 0x0f, 0x10, 0x80, 0x10, 0x10, 0xab, 0x00,/*7c: movsd  0xab1010(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0x80, 0x00, 0x10, 0xab, 0x00,/*84: ucomisd 0xab1000(%rax),%xmm0 */
0x0f, 0x93, 0xc1,                         /*8c: setae  %cl */
0x0f, 0xb6, 0xc9,                         /*8f: movzbl %cl,%ecx */
0x85, 0xc9,                               /*92: test   %ecx,%ecx */
0x74, 0x0c,                               /*94: je     a2 <op_le+0xa2> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*96: movl   $0x2,0xab1008(%rax) */
0xeb, 0x0a,                               /*a0: jmp    ac <op_le+0xac> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*a2: movl   $0x0,0xab1008(%rax) */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*ac: movl   $0x1,0xab1000(%rax) */
0xeb, 0x1d,                               /*b6: jmp    d5 <op_le+0xd5> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*b8: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*bd: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*c2: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*c7: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*cd: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*d0: callq  d5 <op_le+0xd5> */
0x48, 0x89, 0xdf,                         /*d5: mov    %rbx,%rdi */
0x5b,                                     /*d8: pop    %rbx */

};
static uint8_t op_le__rodata[] = {

};

static void op_le_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 16 + 8;
  *((int32_t *)(op + 26)) = a * 16 + 24;
  *((int32_t *)(op + 50)) = a * 16 + 0;
  *((int32_t *)(op + 56)) = a * 16 + 16;
  *((int32_t *)(op + 77)) = a * 16 + 16;
  *((int32_t *)(op + 95)) = a * 16 + 0;
  *((int32_t *)(op + 103)) = a * 16 + 16;
  *((int32_t *)(op + 128)) = a * 16 + 16;
  *((int32_t *)(op + 136)) = a * 16 + 0;
  *((int32_t *)(op + 152)) = a * 16 + 8;
  *((int32_t *)(op + 164)) = a * 16 + 8;
  *((int32_t *)(op + 174)) = a * 16 + 0;
  *((int32_t *)(op + 190)) = a * 1 + 0;
  *((int32_t *)(op + 195)) = b * 1 + 0;
  *((int32_t *)(op + 201)) = c * 1 + 0;
}

static void op_le_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_le_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 10..13], [16, 24, 26..29], [16, 0, 50..53], [16, 16, 56..59], [16, 0, 77..80], [16, 16, 85..88], [16, 0, 110..113], [16, 0, 128..131], [16, 16, 136..139], [16, 8, 152..155], [16, 8, 164..167], [16, 0, 174..177], [1, 0, 190..193]], "b"=>[[1, 0, 195..198]], "c"=>[[1, 0, 201..204]]} */
static uint8_t op_gt__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*8: mov    0xab1008(%rax),%edx */
0xc1, 0xe2, 0x08,                         /*e: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*11: and    $0xffff00,%edx */
0x0f, 0xb6, 0x88, 0x18, 0x10, 0xab, 0x00, /*17: movzbl 0xab1018(%rax),%ecx */
0x09, 0xd1,                               /*1e: or     %edx,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*20: cmp    $0x602,%ecx */
0x7f, 0x19,                               /*26: jg     41 <op_gt+0x41> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*28: cmp    $0x303,%ecx */
0x75, 0x32,                               /*2e: jne    62 <op_gt+0x62> */
0x8b, 0x88, 0x00, 0x10, 0xab, 0x00,       /*30: mov    0xab1000(%rax),%ecx */
0x3b, 0x88, 0x10, 0x10, 0xab, 0x00,       /*36: cmp    0xab1010(%rax),%ecx */
0x0f, 0x9f, 0xc1,                         /*3c: setg   %cl */
0xeb, 0x4e,                               /*3f: jmp    8f <op_gt+0x8f> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*41: cmp    $0x603,%ecx */
0x75, 0x2b,                               /*47: jne    74 <op_gt+0x74> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*49: movsd  0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x2a, 0x88, 0x10, 0x10, 0xab, 0x00,/*51: cvtsi2sdl 0xab1010(%rax),%xmm1 */
0x66, 0x0f, 0x2e, 0xc1,                   /*59: ucomisd %xmm1,%xmm0 */
0x0f, 0x97, 0xc1,                         /*5d: seta   %cl */
0xeb, 0x2d,                               /*60: jmp    8f <op_gt+0x8f> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*62: cmp    $0x306,%ecx */
0x75, 0x4e,                               /*68: jne    b8 <op_gt+0xb8> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x10, 0xab, 0x00,/*6a: cvtsi2sdl 0xab1000(%rax),%xmm0 */
0xeb, 0x10,                               /*72: jmp    84 <op_gt+0x84> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*74: cmp    $0x606,%ecx */
0x75, 0x3c,                               /*7a: jne    b8 <op_gt+0xb8> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*7c: movsd  0xab1000(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0x80, 0x10, 0x10, 0xab, 0x00,/*84: ucomisd 0xab1010(%rax),%xmm0 */
0x0f, 0x97, 0xc1,                         /*8c: seta   %cl */
0x0f, 0xb6, 0xc9,                         /*8f: movzbl %cl,%ecx */
0x85, 0xc9,                               /*92: test   %ecx,%ecx */
0x74, 0x0c,                               /*94: je     a2 <op_gt+0xa2> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*96: movl   $0x2,0xab1008(%rax) */
0xeb, 0x0a,                               /*a0: jmp    ac <op_gt+0xac> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*a2: movl   $0x0,0xab1008(%rax) */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*ac: movl   $0x1,0xab1000(%rax) */
0xeb, 0x1d,                               /*b6: jmp    d5 <op_gt+0xd5> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*b8: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*bd: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*c2: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*c7: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*cd: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*d0: callq  d5 <op_gt+0xd5> */
0x48, 0x89, 0xdf,                         /*d5: mov    %rbx,%rdi */
0x5b,                                     /*d8: pop    %rbx */

};
static uint8_t op_gt__rodata[] = {

};

static void op_gt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 16 + 8;
  *((int32_t *)(op + 26)) = a * 16 + 24;
  *((int32_t *)(op + 50)) = a * 16 + 0;
  *((int32_t *)(op + 56)) = a * 16 + 16;
  *((int32_t *)(op + 77)) = a * 16 + 0;
  *((int32_t *)(op + 85)) = a * 16 + 16;
  *((int32_t *)(op + 110)) = a * 16 + 0;
  *((int32_t *)(op + 128)) = a * 16 + 0;
  *((int32_t *)(op + 136)) = a * 16 + 16;
  *((int32_t *)(op + 152)) = a * 16 + 8;
  *((int32_t *)(op + 164)) = a * 16 + 8;
  *((int32_t *)(op + 174)) = a * 16 + 0;
  *((int32_t *)(op + 190)) = a * 1 + 0;
  *((int32_t *)(op + 195)) = b * 1 + 0;
  *((int32_t *)(op + 201)) = c * 1 + 0;
}

static void op_gt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_gt_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 10..13], [16, 24, 26..29], [16, 0, 50..53], [16, 16, 56..59], [16, 0, 77..80], [16, 16, 85..88], [16, 0, 110..113], [16, 0, 128..131], [16, 16, 136..139], [16, 8, 152..155], [16, 8, 164..167], [16, 0, 174..177], [1, 0, 190..193]], "b"=>[[1, 0, 195..198]], "c"=>[[1, 0, 201..204]]} */
static uint8_t op_ge__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*8: mov    0xab1008(%rax),%edx */
0xc1, 0xe2, 0x08,                         /*e: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*11: and    $0xffff00,%edx */
0x0f, 0xb6, 0x88, 0x18, 0x10, 0xab, 0x00, /*17: movzbl 0xab1018(%rax),%ecx */
0x09, 0xd1,                               /*1e: or     %edx,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*20: cmp    $0x602,%ecx */
0x7f, 0x19,                               /*26: jg     41 <op_ge+0x41> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*28: cmp    $0x303,%ecx */
0x75, 0x32,                               /*2e: jne    62 <op_ge+0x62> */
0x8b, 0x88, 0x00, 0x10, 0xab, 0x00,       /*30: mov    0xab1000(%rax),%ecx */
0x3b, 0x88, 0x10, 0x10, 0xab, 0x00,       /*36: cmp    0xab1010(%rax),%ecx */
0x0f, 0x9d, 0xc1,                         /*3c: setge  %cl */
0xeb, 0x4e,                               /*3f: jmp    8f <op_ge+0x8f> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*41: cmp    $0x603,%ecx */
0x75, 0x2b,                               /*47: jne    74 <op_ge+0x74> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*49: movsd  0xab1000(%rax),%xmm0 */
0xf2, 0x0f, 0x2a, 0x88, 0x10, 0x10, 0xab, 0x00,/*51: cvtsi2sdl 0xab1010(%rax),%xmm1 */
0x66, 0x0f, 0x2e, 0xc1,                   /*59: ucomisd %xmm1,%xmm0 */
0x0f, 0x93, 0xc1,                         /*5d: setae  %cl */
0xeb, 0x2d,                               /*60: jmp    8f <op_ge+0x8f> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*62: cmp    $0x306,%ecx */
0x75, 0x4e,                               /*68: jne    b8 <op_ge+0xb8> */
0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x10, 0xab, 0x00,/*6a: cvtsi2sdl 0xab1000(%rax),%xmm0 */
0xeb, 0x10,                               /*72: jmp    84 <op_ge+0x84> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*74: cmp    $0x606,%ecx */
0x75, 0x3c,                               /*7a: jne    b8 <op_ge+0xb8> */
0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*7c: movsd  0xab1000(%rax),%xmm0 */
0x66, 0x0f, 0x2e, 0x80, 0x10, 0x10, 0xab, 0x00,/*84: ucomisd 0xab1010(%rax),%xmm0 */
0x0f, 0x93, 0xc1,                         /*8c: setae  %cl */
0x0f, 0xb6, 0xc9,                         /*8f: movzbl %cl,%ecx */
0x85, 0xc9,                               /*92: test   %ecx,%ecx */
0x74, 0x0c,                               /*94: je     a2 <op_ge+0xa2> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*96: movl   $0x2,0xab1008(%rax) */
0xeb, 0x0a,                               /*a0: jmp    ac <op_ge+0xac> */
0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*a2: movl   $0x0,0xab1008(%rax) */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*ac: movl   $0x1,0xab1000(%rax) */
0xeb, 0x1d,                               /*b6: jmp    d5 <op_ge+0xd5> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*b8: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*bd: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*c2: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*c7: mov    $0xcd0000,%r8d */
0x48, 0x89, 0xdf,                         /*cd: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*d0: callq  d5 <op_ge+0xd5> */
0x48, 0x89, 0xdf,                         /*d5: mov    %rbx,%rdi */
0x5b,                                     /*d8: pop    %rbx */

};
static uint8_t op_ge__rodata[] = {

};

static void op_ge_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 16 + 8;
  *((int32_t *)(op + 26)) = a * 16 + 24;
  *((int32_t *)(op + 50)) = a * 16 + 0;
  *((int32_t *)(op + 56)) = a * 16 + 16;
  *((int32_t *)(op + 77)) = a * 16 + 0;
  *((int32_t *)(op + 85)) = a * 16 + 16;
  *((int32_t *)(op + 110)) = a * 16 + 0;
  *((int32_t *)(op + 128)) = a * 16 + 0;
  *((int32_t *)(op + 136)) = a * 16 + 16;
  *((int32_t *)(op + 152)) = a * 16 + 8;
  *((int32_t *)(op + 164)) = a * 16 + 8;
  *((int32_t *)(op + 174)) = a * 16 + 0;
  *((int32_t *)(op + 190)) = a * 1 + 0;
  *((int32_t *)(op + 195)) = b * 1 + 0;
  *((int32_t *)(op + 201)) = c * 1 + 0;
}

static void op_ge_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_ge_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[16, 0, 17..20]], "c"=>[[1, 0, 22..25]], "a"=>[[16, 0, 34..37], [16, 8, 41..44]]} */
static uint8_t op_array__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
0x49, 0x8d, 0x96, 0x00, 0x10, 0xbc, 0x00, /*e: lea    0xbc1000(%r14),%rdx */
0xbe, 0x00, 0x00, 0xcd, 0x00,             /*15: mov    $0xcd0000,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1a: callq  1f <op_array+0x1f> */
0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*1f: mov    %rax,0xab1000(%r14) */
0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*26: mov    %edx,0xab1008(%r14) */
0x8b, 0x43, 0x50,                         /*2d: mov    0x50(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*30: mov    0x58(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*34: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*3a: mov    %rbx,%rdi */
0x5b,                                     /*3d: pop    %rbx */
0x41, 0x5e,                               /*3e: pop    %r14 */

};
static uint8_t op_array__rodata[] = {

};

static void op_array_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 16 + 0;
  *((int32_t *)(op + 22)) = c * 1 + 0;
  *((int32_t *)(op + 34)) = a * 16 + 0;
  *((int32_t *)(op + 41)) = a * 16 + 8;
}

static void op_array_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_array_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[16, 0, 19..22], [16, 8, 25..28]], "a"=>[[16, 0, 43..46], [16, 8, 49..52]]} */
static uint8_t op_arycat__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x49, 0x89, 0xff,                         /*5: mov    %rdi,%r15 */
0x49, 0x8b, 0x5f, 0x18,                   /*8: mov    0x18(%r15),%rbx */
0x4d, 0x8b, 0x77, 0x58,                   /*c: mov    0x58(%r15),%r14 */
0x48, 0x8b, 0xb3, 0x00, 0x10, 0xbc, 0x00, /*10: mov    0xbc1000(%rbx),%rsi */
0x8b, 0x93, 0x08, 0x10, 0xbc, 0x00,       /*17: mov    0xbc1008(%rbx),%edx */
0x4c, 0x89, 0xf7,                         /*1d: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*20: callq  25 <op_arycat+0x25> */
0x41, 0x89, 0xd0,                         /*25: mov    %edx,%r8d */
0x48, 0x8b, 0xb3, 0x00, 0x10, 0xab, 0x00, /*28: mov    0xab1000(%rbx),%rsi */
0x8b, 0x93, 0x08, 0x10, 0xab, 0x00,       /*2f: mov    0xab1008(%rbx),%edx */
0x4c, 0x89, 0xf7,                         /*35: mov    %r14,%rdi */
0x48, 0x89, 0xc1,                         /*38: mov    %rax,%rcx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*3b: callq  40 <op_arycat+0x40> */
0x41, 0x8b, 0x47, 0x50,                   /*40: mov    0x50(%r15),%eax */
0x49, 0x8b, 0x4f, 0x58,                   /*44: mov    0x58(%r15),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*48: mov    %eax,0xdc(%rcx) */
0x4c, 0x89, 0xff,                         /*4e: mov    %r15,%rdi */
0x5b,                                     /*51: pop    %rbx */
0x41, 0x5e,                               /*52: pop    %r14 */
0x41, 0x5f,                               /*54: pop    %r15 */

};
static uint8_t op_arycat__rodata[] = {

};

static void op_arycat_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = b * 16 + 0;
  *((int32_t *)(op + 25)) = b * 16 + 8;
  *((int32_t *)(op + 43)) = a * 16 + 0;
  *((int32_t *)(op + 49)) = a * 16 + 8;
}

static void op_arycat_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_arycat_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 15..18], [16, 8, 21..24]], "b"=>[[16, 0, 28..31], [16, 8, 35..38]]} */
static uint8_t op_arypush__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x58,                   /*8: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0xb0, 0x00, 0x10, 0xab, 0x00, /*c: mov    0xab1000(%rax),%rsi */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*13: mov    0xab1008(%rax),%edx */
0x48, 0x8b, 0x88, 0x00, 0x10, 0xbc, 0x00, /*19: mov    0xbc1000(%rax),%rcx */
0x44, 0x8b, 0x80, 0x08, 0x10, 0xbc, 0x00, /*20: mov    0xbc1008(%rax),%r8d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*27: callq  2c <op_arypush+0x2c> */
0x48, 0x89, 0xdf,                         /*2c: mov    %rbx,%rdi */
0x5b,                                     /*2f: pop    %rbx */

};
static uint8_t op_arypush__rodata[] = {

};

static void op_arypush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 16 + 0;
  *((int32_t *)(op + 21)) = a * 16 + 8;
  *((int32_t *)(op + 28)) = b * 16 + 0;
  *((int32_t *)(op + 35)) = b * 16 + 8;
}

static void op_arypush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_arypush_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[16, 8, 12..15], [16, 0, 22..25]], "c"=>[[1, 0, 36..39]], "a"=>[[16, 0, 48..51], [16, 8, 54..57], [16, 8, 62..65], [16, 0, 76..79]]} */
static uint8_t op_aref__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x83, 0xbb, 0x08, 0x10, 0xbc, 0x00, 0x0e, /*a: cmpl   $0xe,0xbc1008(%rbx) */
0x75, 0x29,                               /*11: jne    3c <op_aref+0x3c> */
0x48, 0x8b, 0xb3, 0x00, 0x10, 0xbc, 0x00, /*13: mov    0xbc1000(%rbx),%rsi */
0x49, 0x8b, 0x7e, 0x58,                   /*1a: mov    0x58(%r14),%rdi */
0xba, 0x0e, 0x00, 0x00, 0x00,             /*1e: mov    $0xe,%edx */
0xb9, 0x00, 0x00, 0xcd, 0x00,             /*23: mov    $0xcd0000,%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*28: callq  2d <op_aref+0x2d> */
0x48, 0x89, 0x83, 0x00, 0x10, 0xab, 0x00, /*2d: mov    %rax,0xab1000(%rbx) */
0x89, 0x93, 0x08, 0x10, 0xab, 0x00,       /*34: mov    %edx,0xab1008(%rbx) */
0xeb, 0x18,                               /*3a: jmp    54 <op_aref+0x54> */
0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*3c: movl   $0x0,0xab1008(%rbx) */
0x49, 0x8b, 0x46, 0x18,                   /*46: mov    0x18(%r14),%rax */
0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4a: movl   $0x0,0xab1000(%rax) */
0x4c, 0x89, 0xf7,                         /*54: mov    %r14,%rdi */
0x5b,                                     /*57: pop    %rbx */
0x41, 0x5e,                               /*58: pop    %r14 */

};
static uint8_t op_aref__rodata[] = {

};

static void op_aref_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 12)) = b * 16 + 8;
  *((int32_t *)(op + 22)) = b * 16 + 0;
  *((int32_t *)(op + 36)) = c * 1 + 0;
  *((int32_t *)(op + 48)) = a * 16 + 0;
  *((int32_t *)(op + 54)) = a * 16 + 8;
  *((int32_t *)(op + 62)) = a * 16 + 8;
  *((int32_t *)(op + 76)) = a * 16 + 0;
}

static void op_aref_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_aref_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[16, 0, 15..18], [16, 8, 21..24]], "a"=>[[16, 0, 28..31], [16, 8, 35..38]], "c"=>[[1, 0, 40..43]]} */
static uint8_t op_aset__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x58,                   /*8: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0xb0, 0x00, 0x10, 0xbc, 0x00, /*c: mov    0xbc1000(%rax),%rsi */
0x8b, 0x90, 0x08, 0x10, 0xbc, 0x00,       /*13: mov    0xbc1008(%rax),%edx */
0x4c, 0x8b, 0x80, 0x00, 0x10, 0xab, 0x00, /*19: mov    0xab1000(%rax),%r8 */
0x44, 0x8b, 0x88, 0x08, 0x10, 0xab, 0x00, /*20: mov    0xab1008(%rax),%r9d */
0xb9, 0x00, 0x00, 0xcd, 0x00,             /*27: mov    $0xcd0000,%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2c: callq  31 <op_aset+0x31> */
0x48, 0x89, 0xdf,                         /*31: mov    %rbx,%rdi */
0x5b,                                     /*34: pop    %rbx */

};
static uint8_t op_aset__rodata[] = {

};

static void op_aset_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 16 + 0;
  *((int32_t *)(op + 21)) = b * 16 + 8;
  *((int32_t *)(op + 28)) = a * 16 + 0;
  *((int32_t *)(op + 35)) = a * 16 + 8;
  *((int32_t *)(op + 40)) = c * 1 + 0;
}

static void op_aset_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_aset_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 19..22], [16, 0, 34..37], [16, 0, 84..87], [16, 8, 92..95], [1, 1, 103..106], [16, 0, 184..187], [16, 8, 192..195], [1, 1, 215..218], [1, 1, 230..233], [16, 0, 268..271], [16, 8, 276..279], [1, 1, 295..298], [1, 1, 391..394], [1, 1, 406..409]], "b"=>[[16, 0, 71..74], [1, 1, 282..285], [1, 0, 290..293]], "c"=>[[1, 0, 244..247], [1, 0, 362..365]]} */
static uint8_t op_apost__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x54,                               /*5: push   %r12 */
0x53,                                     /*7: push   %rbx */
0x49, 0x89, 0xfe,                         /*8: mov    %rdi,%r14 */
0x4d, 0x8b, 0x66, 0x18,                   /*b: mov    0x18(%r14),%r12 */
0x41, 0x83, 0xbc, 0x24, 0x08, 0x10, 0xab, 0x00, 0x0e,/*f: cmpl   $0xe,0xab1008(%r12) */
0x0f, 0x85, 0x89, 0x00, 0x00, 0x00,       /*18: jne    a7 <op_apost+0xa7> */
0x4d, 0x8b, 0xbc, 0x24, 0x00, 0x10, 0xab, 0x00,/*1e: mov    0xab1000(%r12),%r15 */
0x41, 0x8b, 0x5f, 0x18,                   /*26: mov    0x18(%r15),%ebx */
0x49, 0x8b, 0x7e, 0x58,                   /*2a: mov    0x58(%r14),%rdi */
0x81, 0xfb, 0x01, 0x00, 0x89, 0x01,       /*2e: cmp    $0x1890001,%ebx */
0x0f, 0x8c, 0xc5, 0x00, 0x00, 0x00,       /*34: jl     ff <op_apost+0xff> */
0x8d, 0xb3, 0x00, 0x00, 0x77, 0xfe,       /*3a: lea    -0x1890000(%rbx),%esi */
0x49, 0x8b, 0x57, 0x28,                   /*40: mov    0x28(%r15),%rdx */
0x48, 0x81, 0xc2, 0x00, 0x10, 0xbc, 0x00, /*44: add    $0xbc1000,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*4b: callq  50 <op_apost+0x50> */
0x49, 0x89, 0x84, 0x24, 0x00, 0x10, 0xab, 0x00,/*50: mov    %rax,0xab1000(%r12) */
0x41, 0x89, 0x94, 0x24, 0x08, 0x10, 0xab, 0x00,/*58: mov    %edx,0xab1008(%r12) */
0x81, 0xc3, 0x00, 0x00, 0x33, 0xff,       /*60: add    $0xff330000,%ebx */
0xb8, 0x10, 0x00, 0xb0, 0x0a,             /*66: mov    $0xab00010,%eax */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*6b: nopl   0x0(%rax,%rax,1) */
0x49, 0x8b, 0x4e, 0x18,                   /*70: mov    0x18(%r14),%rcx */
0x48, 0x63, 0xdb,                         /*74: movslq %ebx,%rbx */
0x49, 0x8b, 0x57, 0x28,                   /*77: mov    0x28(%r15),%rdx */
0x48, 0x89, 0xde,                         /*7b: mov    %rbx,%rsi */
0x48, 0xc1, 0xe6, 0x04,                   /*7e: shl    $0x4,%rsi */
0x48, 0x8b, 0x3c, 0x32,                   /*82: mov    (%rdx,%rsi,1),%rdi */
0x48, 0x8b, 0x54, 0x32, 0x08,             /*86: mov    0x8(%rdx,%rsi,1),%rdx */
0x48, 0x89, 0x54, 0x01, 0x08,             /*8b: mov    %rdx,0x8(%rcx,%rax,1) */
0x48, 0x89, 0x3c, 0x01,                   /*90: mov    %rdi,(%rcx,%rax,1) */
0x48, 0x83, 0xc0, 0x10,                   /*94: add    $0x10,%rax */
0xff, 0xc3,                               /*98: inc    %ebx */
0x48, 0x3d, 0x10, 0x00, 0x80, 0x17,       /*9a: cmp    $0x17800010,%rax */
0x75, 0xce,                               /*a0: jne    70 <op_apost+0x70> */
0xe9, 0xff, 0x00, 0x00, 0x00,             /*a2: jmpq   1a6 <op_apost+0x1a6> */
0x49, 0x8b, 0x7e, 0x58,                   /*a7: mov    0x58(%r14),%rdi */
0x31, 0xdb,                               /*ab: xor    %ebx,%ebx */
0x31, 0xf6,                               /*ad: xor    %esi,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*af: callq  b4 <op_apost+0xb4> */
0x49, 0x89, 0x84, 0x24, 0x00, 0x10, 0xab, 0x00,/*b4: mov    %rax,0xab1000(%r12) */
0x41, 0x89, 0x94, 0x24, 0x08, 0x10, 0xab, 0x00,/*bc: mov    %edx,0xab1008(%r12) */
0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*c4: data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x49, 0x8b, 0x46, 0x18,                   /*d0: mov    0x18(%r14),%rax */
0xc7, 0x84, 0x18, 0x18, 0x00, 0xb0, 0x0a, 0x00, 0x00, 0x00, 0x00,/*d4: movl   $0x0,0xab00018(%rax,%rbx,1) */
0x49, 0x8b, 0x46, 0x18,                   /*df: mov    0x18(%r14),%rax */
0xc7, 0x84, 0x18, 0x10, 0x00, 0xb0, 0x0a, 0x00, 0x00, 0x00, 0x00,/*e3: movl   $0x0,0xab00010(%rax,%rbx,1) */
0x48, 0x83, 0xc3, 0x10,                   /*ee: add    $0x10,%rbx */
0x81, 0xfb, 0x00, 0x00, 0xd0, 0x0c,       /*f2: cmp    $0xcd00000,%ebx */
0x75, 0xd6,                               /*f8: jne    d0 <op_apost+0xd0> */
0xe9, 0xa7, 0x00, 0x00, 0x00,             /*fa: jmpq   1a6 <op_apost+0x1a6> */
0x31, 0xed,                               /*ff: xor    %ebp,%ebp */
0x31, 0xf6,                               /*101: xor    %esi,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*103: callq  108 <op_apost+0x108> */
0x49, 0x89, 0x84, 0x24, 0x00, 0x10, 0xab, 0x00,/*108: mov    %rax,0xab1000(%r12) */
0x41, 0x89, 0x94, 0x24, 0x08, 0x10, 0xab, 0x00,/*110: mov    %edx,0xab1008(%r12) */
0x81, 0xfb, 0x01, 0x00, 0xbc, 0x00,       /*118: cmp    $0xbc0001,%ebx */
0x7c, 0x49,                               /*11e: jl     169 <op_apost+0x169> */
0x8d, 0x83, 0x00, 0x00, 0x44, 0xff,       /*120: lea    -0xbc0000(%rbx),%eax */
0xb9, 0x10, 0x00, 0xb0, 0x0a,             /*126: mov    $0xab00010,%ecx */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*12b: nopl   0x0(%rax,%rax,1) */
0x49, 0x8b, 0x56, 0x18,                   /*130: mov    0x18(%r14),%rdx */
0x49, 0x8b, 0x77, 0x28,                   /*134: mov    0x28(%r15),%rsi */
0x48, 0x8b, 0xbc, 0x0e, 0xf0, 0xff, 0x0f, 0x01,/*138: mov    0x10ffff0(%rsi,%rcx,1),%rdi */
0x48, 0x8b, 0xb4, 0x0e, 0xf8, 0xff, 0x0f, 0x01,/*140: mov    0x10ffff8(%rsi,%rcx,1),%rsi */
0x48, 0x89, 0x74, 0x0a, 0x08,             /*148: mov    %rsi,0x8(%rdx,%rcx,1) */
0x48, 0x89, 0x3c, 0x0a,                   /*14d: mov    %rdi,(%rdx,%rcx,1) */
0x48, 0x83, 0xc1, 0x10,                   /*151: add    $0x10,%rcx */
0xff, 0xc8,                               /*155: dec    %eax */
0x75, 0xd7,                               /*157: jne    130 <op_apost+0x130> */
0x81, 0xc3, 0x00, 0x00, 0x44, 0xff,       /*159: add    $0xff440000,%ebx */
0x81, 0xfb, 0xff, 0xff, 0xcc, 0x00,       /*15f: cmp    $0xccffff,%ebx */
0x89, 0xdd,                               /*165: mov    %ebx,%ebp */
0x7f, 0x3d,                               /*167: jg     1a6 <op_apost+0x1a6> */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*169: mov    $0xcd0000,%eax */
0x29, 0xe8,                               /*16e: sub    %ebp,%eax */
0x48, 0x63, 0xcd,                         /*170: movslq %ebp,%rcx */
0x48, 0xc1, 0xe1, 0x04,                   /*173: shl    $0x4,%rcx */
0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*177: nopw   0x0(%rax,%rax,1) */
0x49, 0x8b, 0x56, 0x18,                   /*180: mov    0x18(%r14),%rdx */
0xc7, 0x84, 0x0a, 0x18, 0x00, 0xb0, 0x0a, 0x00, 0x00, 0x00, 0x00,/*184: movl   $0x0,0xab00018(%rdx,%rcx,1) */
0x49, 0x8b, 0x56, 0x18,                   /*18f: mov    0x18(%r14),%rdx */
0xc7, 0x84, 0x0a, 0x10, 0x00, 0xb0, 0x0a, 0x00, 0x00, 0x00, 0x00,/*193: movl   $0x0,0xab00010(%rdx,%rcx,1) */
0x48, 0x83, 0xc1, 0x10,                   /*19e: add    $0x10,%rcx */
0xff, 0xc8,                               /*1a2: dec    %eax */
0x75, 0xda,                               /*1a4: jne    180 <op_apost+0x180> */
0x41, 0x8b, 0x46, 0x50,                   /*1a6: mov    0x50(%r14),%eax */
0x49, 0x8b, 0x4e, 0x58,                   /*1aa: mov    0x58(%r14),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*1ae: mov    %eax,0xdc(%rcx) */
0x4c, 0x89, 0xf7,                         /*1b4: mov    %r14,%rdi */
0x5b,                                     /*1b7: pop    %rbx */
0x41, 0x5c,                               /*1b8: pop    %r12 */
0x41, 0x5e,                               /*1ba: pop    %r14 */
0x41, 0x5f,                               /*1bc: pop    %r15 */
0x5d,                                     /*1be: pop    %rbp */

};
static uint8_t op_apost__rodata[] = {

};

static void op_apost_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = a * 16 + 8;
  *((int32_t *)(op + 34)) = a * 16 + 0;
  *((int32_t *)(op + 84)) = a * 16 + 0;
  *((int32_t *)(op + 92)) = a * 16 + 8;
  *((int32_t *)(op + 103)) = a * 1 + 1;
  *((int32_t *)(op + 184)) = a * 16 + 0;
  *((int32_t *)(op + 192)) = a * 16 + 8;
  *((int32_t *)(op + 215)) = a * 1 + 1;
  *((int32_t *)(op + 230)) = a * 1 + 1;
  *((int32_t *)(op + 268)) = a * 16 + 0;
  *((int32_t *)(op + 276)) = a * 16 + 8;
  *((int32_t *)(op + 295)) = a * 1 + 1;
  *((int32_t *)(op + 391)) = a * 1 + 1;
  *((int32_t *)(op + 406)) = a * 1 + 1;
  *((int32_t *)(op + 71)) = b * 16 + 0;
  *((int32_t *)(op + 282)) = b * 1 + 1;
  *((int32_t *)(op + 290)) = b * 1 + 0;
  *((int32_t *)(op + 244)) = c * 1 + 0;
  *((int32_t *)(op + 362)) = c * 1 + 0;
}

static void op_apost_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_apost_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[16, 0, 21..24], [16, 8, 27..30]], "a"=>[[16, 0, 39..42], [16, 8, 46..49]]} */
static uint8_t op_string__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*6: mov    0x58(%rbx),%rdi */
0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x43, 0x20,                   /*e: mov    0x20(%rbx),%rax */
0x48, 0x8b, 0xb0, 0x00, 0x10, 0xbc, 0x00, /*12: mov    0xbc1000(%rax),%rsi */
0x8b, 0x90, 0x08, 0x10, 0xbc, 0x00,       /*19: mov    0xbc1008(%rax),%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1f: callq  24 <op_string+0x24> */
0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*24: mov    %rax,0xab1000(%r14) */
0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*2b: mov    %edx,0xab1008(%r14) */
0x8b, 0x43, 0x50,                         /*32: mov    0x50(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*35: mov    0x58(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*39: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*3f: mov    %rbx,%rdi */
0x5b,                                     /*42: pop    %rbx */
0x41, 0x5e,                               /*43: pop    %r14 */

};
static uint8_t op_string__rodata[] = {

};

static void op_string_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 16 + 0;
  *((int32_t *)(op + 27)) = b * 16 + 8;
  *((int32_t *)(op + 39)) = a * 16 + 0;
  *((int32_t *)(op + 46)) = a * 16 + 8;
}

static void op_string_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_string_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 15..18], [16, 8, 21..24]], "b"=>[[16, 0, 28..31], [16, 8, 35..38]]} */
static uint8_t op_strcat__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x7b, 0x58,                   /*8: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0xb0, 0x00, 0x10, 0xab, 0x00, /*c: mov    0xab1000(%rax),%rsi */
0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*13: mov    0xab1008(%rax),%edx */
0x48, 0x8b, 0x88, 0x00, 0x10, 0xbc, 0x00, /*19: mov    0xbc1000(%rax),%rcx */
0x44, 0x8b, 0x80, 0x08, 0x10, 0xbc, 0x00, /*20: mov    0xbc1008(%rax),%r8d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*27: callq  2c <op_strcat+0x2c> */
0x48, 0x89, 0xdf,                         /*2c: mov    %rbx,%rdi */
0x5b,                                     /*2f: pop    %rbx */

};
static uint8_t op_strcat__rodata[] = {

};

static void op_strcat_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 16 + 0;
  *((int32_t *)(op + 21)) = a * 16 + 8;
  *((int32_t *)(op + 28)) = b * 16 + 0;
  *((int32_t *)(op + 35)) = b * 16 + 8;
}

static void op_strcat_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_strcat_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[1, 0, 19..22]], "c"=>[[1, 0, 27..30]], "a"=>[[16, 0, 154..157], [16, 8, 161..164]]} */
static uint8_t op_hash__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x54,                               /*5: push   %r12 */
0x53,                                     /*7: push   %rbx */
0x48, 0x83, 0xec, 0x20,                   /*8: sub    $0x20,%rsp */
0x48, 0x89, 0xfb,                         /*c: mov    %rdi,%rbx */
0xc7, 0x44, 0x24, 0x1c, 0x00, 0x00, 0xbc, 0x00,/*f: movl   $0xbc0000,0x1c(%rsp) */
0xc7, 0x44, 0x24, 0x18, 0x00, 0x00, 0xcd, 0x00,/*17: movl   $0xcd0000,0x18(%rsp) */
0x8b, 0x44, 0x24, 0x1c,                   /*1f: mov    0x1c(%rsp),%eax */
0x8b, 0x4c, 0x24, 0x18,                   /*23: mov    0x18(%rsp),%ecx */
0x44, 0x8d, 0x3c, 0x48,                   /*27: lea    (%rax,%rcx,2),%r15d */
0x48, 0x8b, 0x7b, 0x58,                   /*2b: mov    0x58(%rbx),%rdi */
0x8b, 0x74, 0x24, 0x18,                   /*2f: mov    0x18(%rsp),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*33: callq  38 <op_hash+0x38> */
0x49, 0x89, 0xc6,                         /*38: mov    %rax,%r14 */
0x41, 0x89, 0xd4,                         /*3b: mov    %edx,%r12d */
0xeb, 0x4a,                               /*3e: jmp    8a <op_hash+0x8a> */
0x48, 0x8b, 0x7b, 0x58,                   /*40: mov    0x58(%rbx),%rdi */
0x48, 0x63, 0x44, 0x24, 0x1c,             /*44: movslq 0x1c(%rsp),%rax */
0x48, 0x8b, 0x53, 0x18,                   /*49: mov    0x18(%rbx),%rdx */
0x48, 0xc1, 0xe0, 0x04,                   /*4d: shl    $0x4,%rax */
0x48, 0x63, 0x74, 0x24, 0x1c,             /*51: movslq 0x1c(%rsp),%rsi */
0x48, 0xc1, 0xe6, 0x04,                   /*56: shl    $0x4,%rsi */
0x48, 0x8b, 0x6b, 0x18,                   /*5a: mov    0x18(%rbx),%rbp */
0x48, 0x8b, 0x0c, 0x02,                   /*5e: mov    (%rdx,%rax,1),%rcx */
0x44, 0x8b, 0x44, 0x02, 0x08,             /*62: mov    0x8(%rdx,%rax,1),%r8d */
0x48, 0x8b, 0x44, 0x2e, 0x10,             /*67: mov    0x10(%rsi,%rbp,1),%rax */
0x48, 0x8b, 0x54, 0x2e, 0x18,             /*6c: mov    0x18(%rsi,%rbp,1),%rdx */
0x48, 0x89, 0x54, 0x24, 0x08,             /*71: mov    %rdx,0x8(%rsp) */
0x48, 0x89, 0x04, 0x24,                   /*76: mov    %rax,(%rsp) */
0x4c, 0x89, 0xf6,                         /*7a: mov    %r14,%rsi */
0x44, 0x89, 0xe2,                         /*7d: mov    %r12d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*80: callq  85 <op_hash+0x85> */
0x83, 0x44, 0x24, 0x1c, 0x02,             /*85: addl   $0x2,0x1c(%rsp) */
0x8b, 0x44, 0x24, 0x1c,                   /*8a: mov    0x1c(%rsp),%eax */
0x44, 0x39, 0xf8,                         /*8e: cmp    %r15d,%eax */
0x7c, 0xad,                               /*91: jl     40 <op_hash+0x40> */
0x48, 0x8b, 0x43, 0x18,                   /*93: mov    0x18(%rbx),%rax */
0x4c, 0x89, 0xb0, 0x00, 0x10, 0xab, 0x00, /*97: mov    %r14,0xab1000(%rax) */
0x44, 0x89, 0xa0, 0x08, 0x10, 0xab, 0x00, /*9e: mov    %r12d,0xab1008(%rax) */
0x8b, 0x43, 0x50,                         /*a5: mov    0x50(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*a8: mov    0x58(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*ac: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*b2: mov    %rbx,%rdi */
0x48, 0x83, 0xc4, 0x20,                   /*b5: add    $0x20,%rsp */
0x5b,                                     /*b9: pop    %rbx */
0x41, 0x5c,                               /*ba: pop    %r12 */
0x41, 0x5e,                               /*bc: pop    %r14 */
0x41, 0x5f,                               /*be: pop    %r15 */
0x5d,                                     /*c0: pop    %rbp */

};
static uint8_t op_hash__rodata[] = {

};

static void op_hash_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = b * 1 + 0;
  *((int32_t *)(op + 27)) = c * 1 + 0;
  *((int32_t *)(op + 154)) = a * 16 + 0;
  *((int32_t *)(op + 161)) = a * 16 + 8;
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


/* args: {"b"=>[[16, 0, 17..20], [16, 8, 23..26], [16, 16, 30..33], [16, 24, 37..40]], "c"=>[[1, 0, 43..46]], "a"=>[[16, 0, 55..58], [16, 8, 61..64]]} */
static uint8_t op_range__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x49, 0x8b, 0x7e, 0x58,                   /*a: mov    0x58(%r14),%rdi */
0x48, 0x8b, 0xb3, 0x00, 0x10, 0xbc, 0x00, /*e: mov    0xbc1000(%rbx),%rsi */
0x8b, 0x93, 0x08, 0x10, 0xbc, 0x00,       /*15: mov    0xbc1008(%rbx),%edx */
0x48, 0x8b, 0x8b, 0x10, 0x10, 0xbc, 0x00, /*1b: mov    0xbc1010(%rbx),%rcx */
0x44, 0x8b, 0x83, 0x18, 0x10, 0xbc, 0x00, /*22: mov    0xbc1018(%rbx),%r8d */
0x41, 0xb9, 0x00, 0x00, 0xcd, 0x00,       /*29: mov    $0xcd0000,%r9d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2f: callq  34 <op_range+0x34> */
0x48, 0x89, 0x83, 0x00, 0x10, 0xab, 0x00, /*34: mov    %rax,0xab1000(%rbx) */
0x89, 0x93, 0x08, 0x10, 0xab, 0x00,       /*3b: mov    %edx,0xab1008(%rbx) */
0x41, 0x8b, 0x46, 0x50,                   /*41: mov    0x50(%r14),%eax */
0x49, 0x8b, 0x4e, 0x58,                   /*45: mov    0x58(%r14),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*49: mov    %eax,0xdc(%rcx) */
0x4c, 0x89, 0xf7,                         /*4f: mov    %r14,%rdi */
0x5b,                                     /*52: pop    %rbx */
0x41, 0x5e,                               /*53: pop    %r14 */

};
static uint8_t op_range__rodata[] = {

};

static void op_range_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 16 + 0;
  *((int32_t *)(op + 23)) = b * 16 + 8;
  *((int32_t *)(op + 30)) = b * 16 + 16;
  *((int32_t *)(op + 37)) = b * 16 + 24;
  *((int32_t *)(op + 43)) = c * 1 + 0;
  *((int32_t *)(op + 55)) = a * 16 + 0;
  *((int32_t *)(op + 61)) = a * 16 + 8;
}

static void op_range_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_range_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 0, 18..21], [16, 8, 24..27]]} */
static uint8_t op_oclass__text[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x58,                   /*4: mov    0x58(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x40,                   /*8: mov    0x40(%rcx),%rcx */
0x0f, 0xb6, 0x11,                         /*c: movzbl (%rcx),%edx */
0x48, 0x89, 0x88, 0x00, 0x10, 0xab, 0x00, /*f: mov    %rcx,0xab1000(%rax) */
0x89, 0x90, 0x08, 0x10, 0xab, 0x00,       /*16: mov    %edx,0xab1008(%rax) */

};
static uint8_t op_oclass__rodata[] = {

};

static void op_oclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = a * 16 + 0;
  *((int32_t *)(op + 24)) = a * 16 + 8;
}

static void op_oclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_oclass_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[4, 0, 15..18]], "a"=>[[16, 0, 22..25], [16, 8, 35..38], [16, 16, 42..45], [16, 24, 49..52], [16, 0, 117..120], [16, 8, 123..126]]} */
static uint8_t op_class__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x18,                   /*4: mov    0x18(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x28,                   /*8: mov    0x28(%rbx),%rax */
0x44, 0x8b, 0x88, 0x00, 0x04, 0xbc, 0x00, /*c: mov    0xbc0400(%rax),%r9d */
0x48, 0x8b, 0xb7, 0x00, 0x10, 0xab, 0x00, /*13: mov    0xab1000(%rdi),%rsi */
0x48, 0x89, 0xf0,                         /*1a: mov    %rsi,%rax */
0x48, 0xc1, 0xe8, 0x20,                   /*1d: shr    $0x20,%rax */
0x8b, 0x97, 0x08, 0x10, 0xab, 0x00,       /*21: mov    0xab1008(%rdi),%edx */
0x48, 0x8b, 0x8f, 0x10, 0x10, 0xab, 0x00, /*27: mov    0xab1010(%rdi),%rcx */
0x44, 0x8b, 0x87, 0x18, 0x10, 0xab, 0x00, /*2e: mov    0xab1018(%rdi),%r8d */
0x85, 0xd2,                               /*35: test   %edx,%edx */
0x75, 0x20,                               /*37: jne    59 <op_class+0x59> */
0x31, 0xd2,                               /*39: xor    %edx,%edx */
0x85, 0xf6,                               /*3b: test   %esi,%esi */
0x75, 0x1a,                               /*3d: jne    59 <op_class+0x59> */
0x48, 0x8b, 0x43, 0x58,                   /*3f: mov    0x58(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*43: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*47: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x70, 0x48,                   /*4b: mov    0x48(%rax),%rsi */
0x0f, 0xb6, 0x16,                         /*4f: movzbl (%rsi),%edx */
0x48, 0x89, 0xf0,                         /*52: mov    %rsi,%rax */
0x48, 0xc1, 0xe8, 0x20,                   /*55: shr    $0x20,%rax */
0x48, 0x8b, 0x7b, 0x58,                   /*59: mov    0x58(%rbx),%rdi */
0x48, 0xc1, 0xe0, 0x20,                   /*5d: shl    $0x20,%rax */
0x89, 0xf6,                               /*61: mov    %esi,%esi */
0x48, 0x09, 0xc6,                         /*63: or     %rax,%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*66: callq  6b <op_class+0x6b> */
0x48, 0x8b, 0x4b, 0x18,                   /*6b: mov    0x18(%rbx),%rcx */
0x0f, 0xb6, 0x10,                         /*6f: movzbl (%rax),%edx */
0x48, 0x89, 0x81, 0x00, 0x10, 0xab, 0x00, /*72: mov    %rax,0xab1000(%rcx) */
0x89, 0x91, 0x08, 0x10, 0xab, 0x00,       /*79: mov    %edx,0xab1008(%rcx) */
0x8b, 0x43, 0x50,                         /*7f: mov    0x50(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*82: mov    0x58(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*86: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*8c: mov    %rbx,%rdi */
0x5b,                                     /*8f: pop    %rbx */

};
static uint8_t op_class__rodata[] = {

};

static void op_class_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 4 + 0;
  *((int32_t *)(op + 22)) = a * 16 + 0;
  *((int32_t *)(op + 35)) = a * 16 + 8;
  *((int32_t *)(op + 42)) = a * 16 + 16;
  *((int32_t *)(op + 49)) = a * 16 + 24;
  *((int32_t *)(op + 117)) = a * 16 + 0;
  *((int32_t *)(op + 123)) = a * 16 + 8;
}

static void op_class_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_class_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 14..17]], "a"=>[[16, 0, 21..24], [16, 8, 34..37], [16, 0, 102..105], [16, 8, 108..111]]} */
static uint8_t op_module__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x53, 0x18,                   /*4: mov    0x18(%rbx),%rdx */
0x48, 0x8b, 0x43, 0x28,                   /*8: mov    0x28(%rbx),%rax */
0x8b, 0x88, 0x00, 0x04, 0xbc, 0x00,       /*c: mov    0xbc0400(%rax),%ecx */
0x48, 0x8b, 0xb2, 0x00, 0x10, 0xab, 0x00, /*12: mov    0xab1000(%rdx),%rsi */
0x48, 0x89, 0xf0,                         /*19: mov    %rsi,%rax */
0x48, 0xc1, 0xe8, 0x20,                   /*1c: shr    $0x20,%rax */
0x8b, 0x92, 0x08, 0x10, 0xab, 0x00,       /*20: mov    0xab1008(%rdx),%edx */
0x85, 0xd2,                               /*26: test   %edx,%edx */
0x75, 0x20,                               /*28: jne    4a <op_module+0x4a> */
0x31, 0xd2,                               /*2a: xor    %edx,%edx */
0x85, 0xf6,                               /*2c: test   %esi,%esi */
0x75, 0x1a,                               /*2e: jne    4a <op_module+0x4a> */
0x48, 0x8b, 0x43, 0x58,                   /*30: mov    0x58(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*34: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*38: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x70, 0x48,                   /*3c: mov    0x48(%rax),%rsi */
0x0f, 0xb6, 0x16,                         /*40: movzbl (%rsi),%edx */
0x48, 0x89, 0xf0,                         /*43: mov    %rsi,%rax */
0x48, 0xc1, 0xe8, 0x20,                   /*46: shr    $0x20,%rax */
0x48, 0x8b, 0x7b, 0x58,                   /*4a: mov    0x58(%rbx),%rdi */
0x48, 0xc1, 0xe0, 0x20,                   /*4e: shl    $0x20,%rax */
0x89, 0xf6,                               /*52: mov    %esi,%esi */
0x48, 0x09, 0xc6,                         /*54: or     %rax,%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*57: callq  5c <op_module+0x5c> */
0x48, 0x8b, 0x4b, 0x18,                   /*5c: mov    0x18(%rbx),%rcx */
0x0f, 0xb6, 0x10,                         /*60: movzbl (%rax),%edx */
0x48, 0x89, 0x81, 0x00, 0x10, 0xab, 0x00, /*63: mov    %rax,0xab1000(%rcx) */
0x89, 0x91, 0x08, 0x10, 0xab, 0x00,       /*6a: mov    %edx,0xab1008(%rcx) */
0x8b, 0x43, 0x50,                         /*70: mov    0x50(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*73: mov    0x58(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*77: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*7d: mov    %rbx,%rdi */
0x5b,                                     /*80: pop    %rbx */

};
static uint8_t op_module__rodata[] = {

};

static void op_module_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = b * 4 + 0;
  *((int32_t *)(op + 21)) = a * 16 + 0;
  *((int32_t *)(op + 34)) = a * 16 + 8;
  *((int32_t *)(op + 102)) = a * 16 + 0;
  *((int32_t *)(op + 108)) = a * 16 + 8;
}

static void op_module_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_module_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 22..25], [16, 8, 29..32], [1, 0, 56..59], [16, 0, 105..108]], "b"=>[[8, 0, 124..127]]} */
static uint8_t op_exec__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x54,                               /*5: push   %r12 */
0x53,                                     /*7: push   %rbx */
0x49, 0x89, 0xfe,                         /*8: mov    %rdi,%r14 */
0x49, 0x8b, 0x46, 0x18,                   /*b: mov    0x18(%r14),%rax */
0x49, 0x8b, 0x7e, 0x58,                   /*f: mov    0x58(%r14),%rdi */
0x4c, 0x8b, 0xb8, 0x00, 0x10, 0xab, 0x00, /*13: mov    0xab1000(%rax),%r15 */
0x44, 0x8b, 0xa0, 0x08, 0x10, 0xab, 0x00, /*1a: mov    0xab1008(%rax),%r12d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*21: callq  26 <op_exec+0x26> */
0x48, 0x89, 0xc3,                         /*26: mov    %rax,%rbx */
0x49, 0x8b, 0x46, 0x10,                   /*29: mov    0x10(%r14),%rax */
0x48, 0x83, 0xc0, 0x04,                   /*2d: add    $0x4,%rax */
0x48, 0x89, 0x43, 0x30,                   /*31: mov    %rax,0x30(%rbx) */
0xc7, 0x43, 0x44, 0x00, 0x00, 0xab, 0x00, /*35: movl   $0xab0000,0x44(%rbx) */
0xc7, 0x03, 0x00, 0x00, 0x00, 0x00,       /*3c: movl   $0x0,(%rbx) */
0x49, 0x8b, 0x46, 0x58,                   /*42: mov    0x58(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*46: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*4a: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x10,                   /*4e: mov    %rax,0x10(%rbx) */
0xc7, 0x43, 0x40, 0x00, 0x00, 0x00, 0x00, /*52: movl   $0x0,0x40(%rbx) */
0x4c, 0x89, 0x7b, 0x48,                   /*59: mov    %r15,0x48(%rbx) */
0x49, 0x8b, 0x46, 0x58,                   /*5d: mov    0x58(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*61: mov    0x18(%rax),%rax */
0x48, 0x81, 0x40, 0x08, 0x00, 0x10, 0xab, 0x00,/*65: addq   $0xab1000,0x8(%rax) */
0x49, 0x8b, 0x46, 0x08,                   /*6d: mov    0x8(%r14),%rax */
0x49, 0x8b, 0x7e, 0x58,                   /*71: mov    0x58(%r14),%rdi */
0x48, 0x8b, 0x40, 0x20,                   /*75: mov    0x20(%rax),%rax */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*79: mov    0xbc0800(%rax),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*80: callq  85 <op_exec+0x85> */
0x48, 0x89, 0xc5,                         /*85: mov    %rax,%rbp */
0x48, 0x8b, 0x43, 0x48,                   /*88: mov    0x48(%rbx),%rax */
0x48, 0x89, 0x45, 0x20,                   /*8c: mov    %rax,0x20(%rbp) */
0x48, 0x89, 0x6b, 0x08,                   /*90: mov    %rbp,0x8(%rbx) */
0xf6, 0x45, 0x02, 0x04,                   /*94: testb  $0x4,0x2(%rbp) */
0x74, 0x44,                               /*98: je     de <op_exec+0xde> */
0xc7, 0x43, 0x18, 0x00, 0x00, 0x00, 0x00, /*9a: movl   $0x0,0x18(%rbx) */
0x49, 0x8b, 0x7e, 0x58,                   /*a1: mov    0x58(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*a5: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x58, 0x08,                   /*a9: mov    0x8(%rax),%rbx */
0x4c, 0x89, 0xfe,                         /*ad: mov    %r15,%rsi */
0x44, 0x89, 0xe2,                         /*b0: mov    %r12d,%edx */
0xff, 0x55, 0x18,                         /*b3: callq  *0x18(%rbp) */
0x48, 0x89, 0x03,                         /*b6: mov    %rax,(%rbx) */
0x89, 0x53, 0x08,                         /*b9: mov    %edx,0x8(%rbx) */
0x49, 0x8b, 0x7e, 0x58,                   /*bc: mov    0x58(%r14),%rdi */
0x41, 0x8b, 0x76, 0x50,                   /*c0: mov    0x50(%r14),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c4: callq  c9 <op_exec+0xc9> */
0x49, 0x8b, 0x46, 0x58,                   /*c9: mov    0x58(%r14),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*cd: cmpq   $0x0,0x28(%rax) */
0x74, 0x68,                               /*d2: je     13c <op_exec+0x13c> */
0x4c, 0x89, 0xf7,                         /*d4: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*d7: callq  dc <op_exec+0xdc> */
0xeb, 0x7b,                               /*dc: jmp    159 <op_exec+0x159> */
0x48, 0x8b, 0x45, 0x18,                   /*de: mov    0x18(%rbp),%rax */
0x49, 0x89, 0x46, 0x08,                   /*e2: mov    %rax,0x8(%r14) */
0x48, 0x8b, 0x48, 0x10,                   /*e6: mov    0x10(%rax),%rcx */
0x49, 0x89, 0x4e, 0x20,                   /*ea: mov    %rcx,0x20(%r14) */
0x48, 0x8b, 0x48, 0x18,                   /*ee: mov    0x18(%rax),%rcx */
0x49, 0x89, 0x4e, 0x28,                   /*f2: mov    %rcx,0x28(%r14) */
0x49, 0x8b, 0x7e, 0x58,                   /*f6: mov    0x58(%r14),%rdi */
0x0f, 0xb7, 0x70, 0x02,                   /*fa: movzwl 0x2(%rax),%esi */
0xba, 0x01, 0x00, 0x00, 0x00,             /*fe: mov    $0x1,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*103: callq  108 <op_exec+0x108> */
0x49, 0x8b, 0x46, 0x08,                   /*108: mov    0x8(%r14),%rax */
0x0f, 0xb7, 0x40, 0x02,                   /*10c: movzwl 0x2(%rax),%eax */
0x89, 0x43, 0x18,                         /*110: mov    %eax,0x18(%rbx) */
0x49, 0x8b, 0x7e, 0x58,                   /*113: mov    0x58(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*117: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*11b: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x18,                   /*11f: mov    %rax,0x18(%r14) */
0x49, 0x8b, 0x46, 0x08,                   /*123: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*127: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x10,                   /*12b: mov    %rax,0x10(%r14) */
0x48, 0x89, 0xee,                         /*12f: mov    %rbp,%rsi */
0x4c, 0x89, 0xf2,                         /*132: mov    %r14,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*135: callq  13a <op_exec+0x13a> */
0xeb, 0x1d,                               /*13a: jmp    159 <op_exec+0x159> */
0x48, 0x8b, 0x40, 0x18,                   /*13c: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*140: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x49, 0x10,                   /*144: mov    0x10(%rcx),%rcx */
0x48, 0x89, 0x48, 0x08,                   /*148: mov    %rcx,0x8(%rax) */
0x49, 0x89, 0x4e, 0x18,                   /*14c: mov    %rcx,0x18(%r14) */
0x49, 0x8b, 0x7e, 0x58,                   /*150: mov    0x58(%r14),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*154: callq  159 <op_exec+0x159> */
0x4c, 0x89, 0xf7,                         /*159: mov    %r14,%rdi */
0x5b,                                     /*15c: pop    %rbx */
0x41, 0x5c,                               /*15d: pop    %r12 */
0x41, 0x5e,                               /*15f: pop    %r14 */
0x41, 0x5f,                               /*161: pop    %r15 */
0x5d,                                     /*163: pop    %rbp */

};
static uint8_t op_exec__rodata[] = {

};

static void op_exec_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 22)) = a * 16 + 0;
  *((int32_t *)(op + 29)) = a * 16 + 8;
  *((int32_t *)(op + 56)) = a * 1 + 0;
  *((int32_t *)(op + 105)) = a * 16 + 0;
  *((int32_t *)(op + 124)) = b * 8 + 0;
}

static void op_exec_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_exec_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 16, 32..35], [16, 24, 39..42]]} */
static uint8_t op_method__text[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0x91, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%edx */
0x48, 0x8b, 0xb0, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rsi */
0x48, 0x8b, 0x88, 0x10, 0x10, 0xab, 0x00, /*1d: mov    0xab1010(%rax),%rcx */
0x44, 0x8b, 0x80, 0x18, 0x10, 0xab, 0x00, /*24: mov    0xab1018(%rax),%r8d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*2b: callq  30 <op_method+0x30> */
0x8b, 0x43, 0x50,                         /*30: mov    0x50(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*33: mov    0x58(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*37: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*3d: mov    %rbx,%rdi */
0x5b,                                     /*40: pop    %rbx */

};
static uint8_t op_method__rodata[] = {

};

static void op_method_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 32)) = a * 16 + 16;
  *((int32_t *)(op + 39)) = a * 16 + 24;
}

static void op_method_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_method_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[16, 0, 17..20], [16, 8, 24..27]], "a"=>[[16, 0, 36..39], [16, 8, 43..46]]} */
static uint8_t op_sclass__text[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
0x49, 0x8b, 0xb6, 0x00, 0x10, 0xbc, 0x00, /*e: mov    0xbc1000(%r14),%rsi */
0x41, 0x8b, 0x96, 0x08, 0x10, 0xbc, 0x00, /*15: mov    0xbc1008(%r14),%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1c: callq  21 <op_sclass+0x21> */
0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*21: mov    %rax,0xab1000(%r14) */
0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*28: mov    %edx,0xab1008(%r14) */
0x8b, 0x43, 0x50,                         /*2f: mov    0x50(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x58,                   /*32: mov    0x58(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*36: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*3c: mov    %rbx,%rdi */
0x5b,                                     /*3f: pop    %rbx */
0x41, 0x5e,                               /*40: pop    %r14 */

};
static uint8_t op_sclass__rodata[] = {

};

static void op_sclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 16 + 0;
  *((int32_t *)(op + 24)) = b * 16 + 8;
  *((int32_t *)(op + 36)) = a * 16 + 0;
  *((int32_t *)(op + 43)) = a * 16 + 8;
}

static void op_sclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sclass_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 39..42], [16, 8, 45..48]]} */
static uint8_t op_tclass__text[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x49, 0x89, 0xfe,                         /*5: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x58,                   /*8: mov    0x58(%r14),%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*c: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*10: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x48,                   /*14: mov    0x48(%rax),%rax */
0x48, 0x85, 0xc0,                         /*18: test   %rax,%rax */
0x74, 0x16,                               /*1b: je     33 <op_tclass+0x33> */
0x49, 0x8b, 0x4e, 0x18,                   /*1d: mov    0x18(%r14),%rcx */
0x0f, 0xb6, 0x10,                         /*21: movzbl (%rax),%edx */
0x48, 0x89, 0x81, 0x00, 0x10, 0xab, 0x00, /*24: mov    %rax,0xab1000(%rcx) */
0x89, 0x91, 0x08, 0x10, 0xab, 0x00,       /*2b: mov    %edx,0xab1008(%rcx) */
0xeb, 0x44,                               /*31: jmp    77 <op_tclass+0x77> */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*33: mov    0x0(%rip),%rsi        # 3a <op_tclass+0x3a> */
0x48, 0x89, 0xdf,                         /*3a: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*3d: callq  42 <op_tclass+0x42> */
0x49, 0x89, 0xc7,                         /*42: mov    %rax,%r15 */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*45: mov    $0x0,%esi */
0xba, 0x19, 0x00, 0x00, 0x00,             /*4a: mov    $0x19,%edx */
0x48, 0x89, 0xdf,                         /*4f: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*52: callq  57 <op_tclass+0x57> */
0x89, 0xd1,                               /*57: mov    %edx,%ecx */
0x48, 0x89, 0xdf,                         /*59: mov    %rbx,%rdi */
0x4c, 0x89, 0xfe,                         /*5c: mov    %r15,%rsi */
0x48, 0x89, 0xc2,                         /*5f: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*62: callq  67 <op_tclass+0x67> */
0x49, 0x8b, 0x4e, 0x58,                   /*67: mov    0x58(%r14),%rcx */
0x48, 0x89, 0x41, 0x28,                   /*6b: mov    %rax,0x28(%rcx) */
0x4c, 0x89, 0xf7,                         /*6f: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*72: callq  77 <op_tclass+0x77> */
0x4c, 0x89, 0xf7,                         /*77: mov    %r14,%rdi */
0x5b,                                     /*7a: pop    %rbx */
0x41, 0x5e,                               /*7b: pop    %r14 */
0x41, 0x5f,                               /*7d: pop    %r15 */

};
static uint8_t op_tclass__rodata[] = {
0x6e, 0x6f, 0x20, 0x74,                   
0x6f, 0x72, 0x20, 0x6d,                   

};

static void op_tclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 39)) = a * 16 + 0;
  *((int32_t *)(op + 45)) = a * 16 + 8;
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
0x4f, 0x50, 0x5f, 0x44,                   

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


/* args: {"b"=>[[16, 0, 20..23], [16, 8, 26..29]]} */
static uint8_t op_err__text[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x53,                                     /*5: push   %rbx */
0x48, 0x89, 0xfb,                         /*6: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x20,                   /*9: mov    0x20(%rbx),%rax */
0x4c, 0x8b, 0x73, 0x58,                   /*d: mov    0x58(%rbx),%r14 */
0x48, 0x8b, 0xb0, 0x00, 0x10, 0xbc, 0x00, /*11: mov    0xbc1000(%rax),%rsi */
0x8b, 0x90, 0x08, 0x10, 0xbc, 0x00,       /*18: mov    0xbc1008(%rax),%edx */
0x4c, 0x89, 0xf7,                         /*1e: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*21: callq  26 <op_err+0x26> */
0x49, 0x89, 0xc7,                         /*26: mov    %rax,%r15 */
0x89, 0xd5,                               /*29: mov    %edx,%ebp */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*2b: mov    0x0(%rip),%rsi        # 32 <op_err+0x32> */
0x4c, 0x89, 0xf7,                         /*32: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*35: callq  3a <op_err+0x3a> */
0x4c, 0x89, 0xf7,                         /*3a: mov    %r14,%rdi */
0x48, 0x89, 0xc6,                         /*3d: mov    %rax,%rsi */
0x4c, 0x89, 0xfa,                         /*40: mov    %r15,%rdx */
0x89, 0xe9,                               /*43: mov    %ebp,%ecx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*45: callq  4a <op_err+0x4a> */
0x48, 0x8b, 0x4b, 0x58,                   /*4a: mov    0x58(%rbx),%rcx */
0x48, 0x89, 0x41, 0x28,                   /*4e: mov    %rax,0x28(%rcx) */
0x48, 0x89, 0xdf,                         /*52: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*55: callq  5a <op_err+0x5a> */
0x48, 0x89, 0xdf,                         /*5a: mov    %rbx,%rdi */
0x5b,                                     /*5d: pop    %rbx */
0x41, 0x5e,                               /*5e: pop    %r14 */
0x41, 0x5f,                               /*60: pop    %r15 */
0x5d,                                     /*62: pop    %rbp */

};
static uint8_t op_err__rodata[] = {

};

static void op_err_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 16 + 0;
  *((int32_t *)(op + 26)) = b * 16 + 8;
}

static void op_err_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_err_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}

typedef void (*jit_args_func_t)(uint8_t *op, mrb_code c, uint32_t op_idx);
typedef void (*jit_link_func_t)(uint8_t *op, uint8_t *data);
static jit_args_func_t arg_funcs[78];
extern jit_link_func_t link_funcs[];
uint8_t* ops[78];
static char *op_names[78];

static size_t op_sizes[] = {
  sizeof(op_nop), /* 0 */
  sizeof(op_move), /* 32 */
  sizeof(op_loadl), /* 36 */
  sizeof(op_loadi), /* 28 */
  sizeof(op_loadsym), /* 34 */
  sizeof(op_loadnil), /* 28 */
  sizeof(op_loadself), /* 25 */
  sizeof(op_loadt), /* 28 */
  sizeof(op_loadf), /* 28 */
  sizeof(op_getglobal), /* 49 */
  sizeof(op_setglobal), /* 44 */
  sizeof(op_getspecial), /* 44 */
  sizeof(op_setspecial), /* 39 */
  sizeof(op_getiv), /* 49 */
  sizeof(op_setiv), /* 44 */
  sizeof(op_getcv), /* 89 */
  sizeof(op_setcv), /* 44 */
  sizeof(op_getconst), /* 96 */
  sizeof(op_setconst), /* 44 */
  sizeof(op_getmcnst), /* 113 */
  sizeof(op_setmcnst), /* 58 */
  sizeof(op_getupvar), /* 122 */
  sizeof(op_setupvar), /* 108 */
  sizeof(op_jmp), /* 0 */
  sizeof(op_jmpif), /* 11 */
  sizeof(op_jmpnot), /* 11 */
  sizeof(op_onerr), /* 145 */
  sizeof(op_rescue), /* 52 */
  sizeof(op_poperr), /* 35 */
  sizeof(op_raise), /* 32 */
  sizeof(op_epush), /* 167 */
  sizeof(op_epop), /* 84 */
  sizeof(op_send), /* 111 */
  sizeof(op_sendb), /* 56 */
  sizeof(op_fsend), /* 0 */
  sizeof(op_call), /* 452 */
  sizeof(op_super), /* 704 */
  sizeof(op_argary), /* 724 */
  sizeof(op_enter), /* 1792 */
  sizeof(op_enter_method_m), /* 453 */
  sizeof(op_karg), /* 0 */
  sizeof(op_kdict), /* 0 */
  sizeof(op_return), /* 38 */
  sizeof(op_break), /* 24 */
  sizeof(op_tailcall), /* 793 */
  sizeof(op_blkpush), /* 211 */
  sizeof(op_add), /* 333 */
  sizeof(op_addi), /* 162 */
  sizeof(op_sub), /* 236 */
  sizeof(op_subi), /* 166 */
  sizeof(op_mul), /* 254 */
  sizeof(op_div), /* 204 */
  sizeof(op_eq), /* 262 */
  sizeof(op_lt), /* 217 */
  sizeof(op_le), /* 217 */
  sizeof(op_gt), /* 217 */
  sizeof(op_ge), /* 217 */
  sizeof(op_array), /* 64 */
  sizeof(op_arycat), /* 86 */
  sizeof(op_arypush), /* 48 */
  sizeof(op_aref), /* 90 */
  sizeof(op_aset), /* 53 */
  sizeof(op_apost), /* 447 */
  sizeof(op_string), /* 69 */
  sizeof(op_strcat), /* 48 */
  sizeof(op_hash), /* 193 */
  sizeof(op_lambda), /* 28 */
  sizeof(op_range), /* 85 */
  sizeof(op_oclass), /* 28 */
  sizeof(op_class), /* 144 */
  sizeof(op_module), /* 129 */
  sizeof(op_exec), /* 356 */
  sizeof(op_method), /* 65 */
  sizeof(op_sclass), /* 66 */
  sizeof(op_tclass), /* 127 */
  sizeof(op_debug), /* 35 */
  sizeof(op_stop), /* 13 */
  sizeof(op_err), /* 99 */

};

extern void init_linker();
void init_ops() {
  static int init = 0;
  if(init == 0) {
    init = 1;
    init_linker();
    ops[0] = op_nop;
    op_names[0] = "op_nop";
    arg_funcs[0] = op_nop_set_args_from_code;
    ops[1] = op_move;
    op_names[1] = "op_move";
    arg_funcs[1] = op_move_set_args_from_code;
    ops[2] = op_loadl;
    op_names[2] = "op_loadl";
    arg_funcs[2] = op_loadl_set_args_from_code;
    ops[3] = op_loadi;
    op_names[3] = "op_loadi";
    arg_funcs[3] = op_loadi_set_args_from_code;
    ops[4] = op_loadsym;
    op_names[4] = "op_loadsym";
    arg_funcs[4] = op_loadsym_set_args_from_code;
    ops[5] = op_loadnil;
    op_names[5] = "op_loadnil";
    arg_funcs[5] = op_loadnil_set_args_from_code;
    ops[6] = op_loadself;
    op_names[6] = "op_loadself";
    arg_funcs[6] = op_loadself_set_args_from_code;
    ops[7] = op_loadt;
    op_names[7] = "op_loadt";
    arg_funcs[7] = op_loadt_set_args_from_code;
    ops[8] = op_loadf;
    op_names[8] = "op_loadf";
    arg_funcs[8] = op_loadf_set_args_from_code;
    ops[9] = op_getglobal;
    op_names[9] = "op_getglobal";
    arg_funcs[9] = op_getglobal_set_args_from_code;
    ops[10] = op_setglobal;
    op_names[10] = "op_setglobal";
    arg_funcs[10] = op_setglobal_set_args_from_code;
    ops[11] = op_getspecial;
    op_names[11] = "op_getspecial";
    arg_funcs[11] = op_getspecial_set_args_from_code;
    ops[12] = op_setspecial;
    op_names[12] = "op_setspecial";
    arg_funcs[12] = op_setspecial_set_args_from_code;
    ops[13] = op_getiv;
    op_names[13] = "op_getiv";
    arg_funcs[13] = op_getiv_set_args_from_code;
    ops[14] = op_setiv;
    op_names[14] = "op_setiv";
    arg_funcs[14] = op_setiv_set_args_from_code;
    ops[15] = op_getcv;
    op_names[15] = "op_getcv";
    arg_funcs[15] = op_getcv_set_args_from_code;
    ops[16] = op_setcv;
    op_names[16] = "op_setcv";
    arg_funcs[16] = op_setcv_set_args_from_code;
    ops[17] = op_getconst;
    op_names[17] = "op_getconst";
    arg_funcs[17] = op_getconst_set_args_from_code;
    ops[18] = op_setconst;
    op_names[18] = "op_setconst";
    arg_funcs[18] = op_setconst_set_args_from_code;
    ops[19] = op_getmcnst;
    op_names[19] = "op_getmcnst";
    arg_funcs[19] = op_getmcnst_set_args_from_code;
    ops[20] = op_setmcnst;
    op_names[20] = "op_setmcnst";
    arg_funcs[20] = op_setmcnst_set_args_from_code;
    ops[21] = op_getupvar;
    op_names[21] = "op_getupvar";
    arg_funcs[21] = op_getupvar_set_args_from_code;
    ops[22] = op_setupvar;
    op_names[22] = "op_setupvar";
    arg_funcs[22] = op_setupvar_set_args_from_code;
    ops[23] = op_jmp;
    op_names[23] = "op_jmp";
    arg_funcs[23] = op_jmp_set_args_from_code;
    ops[24] = op_jmpif;
    op_names[24] = "op_jmpif";
    arg_funcs[24] = op_jmpif_set_args_from_code;
    ops[25] = op_jmpnot;
    op_names[25] = "op_jmpnot";
    arg_funcs[25] = op_jmpnot_set_args_from_code;
    ops[26] = op_onerr;
    op_names[26] = "op_onerr";
    arg_funcs[26] = op_onerr_set_args_from_code;
    ops[27] = op_rescue;
    op_names[27] = "op_rescue";
    arg_funcs[27] = op_rescue_set_args_from_code;
    ops[28] = op_poperr;
    op_names[28] = "op_poperr";
    arg_funcs[28] = op_poperr_set_args_from_code;
    ops[29] = op_raise;
    op_names[29] = "op_raise";
    arg_funcs[29] = op_raise_set_args_from_code;
    ops[30] = op_epush;
    op_names[30] = "op_epush";
    arg_funcs[30] = op_epush_set_args_from_code;
    ops[31] = op_epop;
    op_names[31] = "op_epop";
    arg_funcs[31] = op_epop_set_args_from_code;
    ops[32] = op_send;
    op_names[32] = "op_send";
    arg_funcs[32] = op_send_set_args_from_code;
    ops[33] = op_sendb;
    op_names[33] = "op_sendb";
    arg_funcs[33] = op_sendb_set_args_from_code;
    ops[34] = op_fsend;
    op_names[34] = "op_fsend";
    arg_funcs[34] = op_fsend_set_args_from_code;
    ops[35] = op_call;
    op_names[35] = "op_call";
    arg_funcs[35] = op_call_set_args_from_code;
    ops[36] = op_super;
    op_names[36] = "op_super";
    arg_funcs[36] = op_super_set_args_from_code;
    ops[37] = op_argary;
    op_names[37] = "op_argary";
    arg_funcs[37] = op_argary_set_args_from_code;
    ops[38] = op_enter;
    op_names[38] = "op_enter";
    arg_funcs[38] = op_enter_set_args_from_code;
    ops[39] = op_enter_method_m;
    op_names[39] = "op_enter_method_m";
    arg_funcs[39] = op_enter_method_m_set_args_from_code;
    ops[40] = op_karg;
    op_names[40] = "op_karg";
    arg_funcs[40] = op_karg_set_args_from_code;
    ops[41] = op_kdict;
    op_names[41] = "op_kdict";
    arg_funcs[41] = op_kdict_set_args_from_code;
    ops[42] = op_return;
    op_names[42] = "op_return";
    arg_funcs[42] = op_return_set_args_from_code;
    ops[43] = op_break;
    op_names[43] = "op_break";
    arg_funcs[43] = op_break_set_args_from_code;
    ops[44] = op_tailcall;
    op_names[44] = "op_tailcall";
    arg_funcs[44] = op_tailcall_set_args_from_code;
    ops[45] = op_blkpush;
    op_names[45] = "op_blkpush";
    arg_funcs[45] = op_blkpush_set_args_from_code;
    ops[46] = op_add;
    op_names[46] = "op_add";
    arg_funcs[46] = op_add_set_args_from_code;
    ops[47] = op_addi;
    op_names[47] = "op_addi";
    arg_funcs[47] = op_addi_set_args_from_code;
    ops[48] = op_sub;
    op_names[48] = "op_sub";
    arg_funcs[48] = op_sub_set_args_from_code;
    ops[49] = op_subi;
    op_names[49] = "op_subi";
    arg_funcs[49] = op_subi_set_args_from_code;
    ops[50] = op_mul;
    op_names[50] = "op_mul";
    arg_funcs[50] = op_mul_set_args_from_code;
    ops[51] = op_div;
    op_names[51] = "op_div";
    arg_funcs[51] = op_div_set_args_from_code;
    ops[52] = op_eq;
    op_names[52] = "op_eq";
    arg_funcs[52] = op_eq_set_args_from_code;
    ops[53] = op_lt;
    op_names[53] = "op_lt";
    arg_funcs[53] = op_lt_set_args_from_code;
    ops[54] = op_le;
    op_names[54] = "op_le";
    arg_funcs[54] = op_le_set_args_from_code;
    ops[55] = op_gt;
    op_names[55] = "op_gt";
    arg_funcs[55] = op_gt_set_args_from_code;
    ops[56] = op_ge;
    op_names[56] = "op_ge";
    arg_funcs[56] = op_ge_set_args_from_code;
    ops[57] = op_array;
    op_names[57] = "op_array";
    arg_funcs[57] = op_array_set_args_from_code;
    ops[58] = op_arycat;
    op_names[58] = "op_arycat";
    arg_funcs[58] = op_arycat_set_args_from_code;
    ops[59] = op_arypush;
    op_names[59] = "op_arypush";
    arg_funcs[59] = op_arypush_set_args_from_code;
    ops[60] = op_aref;
    op_names[60] = "op_aref";
    arg_funcs[60] = op_aref_set_args_from_code;
    ops[61] = op_aset;
    op_names[61] = "op_aset";
    arg_funcs[61] = op_aset_set_args_from_code;
    ops[62] = op_apost;
    op_names[62] = "op_apost";
    arg_funcs[62] = op_apost_set_args_from_code;
    ops[63] = op_string;
    op_names[63] = "op_string";
    arg_funcs[63] = op_string_set_args_from_code;
    ops[64] = op_strcat;
    op_names[64] = "op_strcat";
    arg_funcs[64] = op_strcat_set_args_from_code;
    ops[65] = op_hash;
    op_names[65] = "op_hash";
    arg_funcs[65] = op_hash_set_args_from_code;
    ops[66] = op_lambda;
    op_names[66] = "op_lambda";
    arg_funcs[66] = op_lambda_set_args_from_code;
    ops[67] = op_range;
    op_names[67] = "op_range";
    arg_funcs[67] = op_range_set_args_from_code;
    ops[68] = op_oclass;
    op_names[68] = "op_oclass";
    arg_funcs[68] = op_oclass_set_args_from_code;
    ops[69] = op_class;
    op_names[69] = "op_class";
    arg_funcs[69] = op_class_set_args_from_code;
    ops[70] = op_module;
    op_names[70] = "op_module";
    arg_funcs[70] = op_module_set_args_from_code;
    ops[71] = op_exec;
    op_names[71] = "op_exec";
    arg_funcs[71] = op_exec_set_args_from_code;
    ops[72] = op_method;
    op_names[72] = "op_method";
    arg_funcs[72] = op_method_set_args_from_code;
    ops[73] = op_sclass;
    op_names[73] = "op_sclass";
    arg_funcs[73] = op_sclass_set_args_from_code;
    ops[74] = op_tclass;
    op_names[74] = "op_tclass";
    arg_funcs[74] = op_tclass_set_args_from_code;
    ops[75] = op_debug;
    op_names[75] = "op_debug";
    arg_funcs[75] = op_debug_set_args_from_code;
    ops[76] = op_stop;
    op_names[76] = "op_stop";
    arg_funcs[76] = op_stop_set_args_from_code;
    ops[77] = op_err;
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
