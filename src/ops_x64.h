
/* args: {} */
static uint8_t op_nop[] = {

};

static void op_nop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_nop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_nop_set_args(op, 0,0,0,op_idx);
}


/* args: {"b"=>[[16, 0, 7..10], [16, 8, 14..17]], "a"=>[[16, 8, 21..24], [16, 0, 28..31]]} */
static uint8_t op_move[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x88, 0x00, 0x10, 0xbc, 0x00, /*4: mov    0xbc1000(%rax),%rcx */
  0x48, 0x8b, 0x90, 0x08, 0x10, 0xbc, 0x00, /*b: mov    0xbc1008(%rax),%rdx */
  0x48, 0x89, 0x90, 0x08, 0x10, 0xab, 0x00, /*12: mov    %rdx,0xab1008(%rax) */
  0x48, 0x89, 0x88, 0x00, 0x10, 0xab, 0x00, /*19: mov    %rcx,0xab1000(%rax) */

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
static uint8_t op_loadl[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x4f, 0x20,                   /*4: mov    0x20(%rdi),%rcx */
  0x48, 0x8b, 0x91, 0x00, 0x10, 0xbc, 0x00, /*8: mov    0xbc1000(%rcx),%rdx */
  0x48, 0x8b, 0x89, 0x08, 0x10, 0xbc, 0x00, /*f: mov    0xbc1008(%rcx),%rcx */
  0x48, 0x89, 0x88, 0x08, 0x10, 0xab, 0x00, /*16: mov    %rcx,0xab1008(%rax) */
  0x48, 0x89, 0x90, 0x00, 0x10, 0xab, 0x00, /*1d: mov    %rdx,0xab1000(%rax) */

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
static uint8_t op_loadi[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*4: movl   $0x3,0xab1008(%rax) */
  0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x00, 0x00, 0xbc, 0x00,/*12: movl   $0xbc0000,0xab1000(%rax) */

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
static uint8_t op_loadsym[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*4: movl   $0x4,0xab1008(%rax) */
  0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x4f, 0x28,                   /*12: mov    0x28(%rdi),%rcx */
  0x8b, 0x89, 0x00, 0x04, 0xbc, 0x00,       /*16: mov    0xbc0400(%rcx),%ecx */
  0x89, 0x88, 0x00, 0x10, 0xab, 0x00,       /*1c: mov    %ecx,0xab1000(%rax) */

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
static uint8_t op_loadnil[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movl   $0x0,0xab1008(%rax) */
  0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*12: movl   $0x0,0xab1000(%rax) */

};

static void op_loadnil_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
  *((int32_t *)(op + 20)) = a * 16 + 0;
}

static void op_loadnil_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadnil_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 14..17], [16, 0, 21..24]]} */
static uint8_t op_loadself[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x08,                         /*4: mov    (%rax),%rcx */
  0x48, 0x8b, 0x50, 0x08,                   /*7: mov    0x8(%rax),%rdx */
  0x48, 0x89, 0x90, 0x08, 0x10, 0xab, 0x00, /*b: mov    %rdx,0xab1008(%rax) */
  0x48, 0x89, 0x88, 0x00, 0x10, 0xab, 0x00, /*12: mov    %rcx,0xab1000(%rax) */

};

static void op_loadself_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = a * 16 + 8;
  *((int32_t *)(op + 21)) = a * 16 + 0;
}

static void op_loadself_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadself_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9], [16, 0, 20..23]]} */
static uint8_t op_loadt[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*4: movl   $0x2,0xab1008(%rax) */
  0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*12: movl   $0x1,0xab1000(%rax) */

};

static void op_loadt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
  *((int32_t *)(op + 20)) = a * 16 + 0;
}

static void op_loadt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadt_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9], [16, 0, 20..23]]} */
static uint8_t op_loadf[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movl   $0x0,0xab1008(%rax) */
  0x48, 0x8b, 0x47, 0x18,                   /*e: mov    0x18(%rdi),%rax */
  0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*12: movl   $0x1,0xab1000(%rax) */

};

static void op_loadf_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
  *((int32_t *)(op + 20)) = a * 16 + 0;
}

static void op_loadf_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadf_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[16, 0, 33..36], [16, 8, 40..43]]} */
static uint8_t op_getglobal[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*6: mov    0x58(%rbx),%rdi */
  0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
  0x48, 0x8b, 0x43, 0x28,                   /*e: mov    0x28(%rbx),%rax */
  0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*12: mov    0xbc0400(%rax),%esi */
  0xff, 0x93, 0xa8, 0x01, 0x00, 0x00,       /*18: callq  *0x1a8(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*1e: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*25: mov    %edx,0xab1008(%r14) */
  0x48, 0x89, 0xdf,                         /*2c: mov    %rbx,%rdi */
  0x5b,                                     /*2f: pop    %rbx */
  0x41, 0x5e,                               /*30: pop    %r14 */

};

static void op_getglobal_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 33)) = a * 16 + 0;
  *((int32_t *)(op + 40)) = a * 16 + 8;
}

static void op_getglobal_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getglobal_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 8, 31..34]]} */
static uint8_t op_setglobal[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
  0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
  0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rdx */
  0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*1d: mov    0xab1008(%rax),%ecx */
  0xff, 0x93, 0xa0, 0x00, 0x00, 0x00,       /*23: callq  *0xa0(%rbx) */
  0x48, 0x89, 0xdf,                         /*29: mov    %rbx,%rdi */
  0x5b,                                     /*2c: pop    %rbx */

};

static void op_setglobal_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 31)) = a * 16 + 8;
}

static void op_setglobal_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setglobal_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[1, 0, 15..18]], "a"=>[[16, 0, 28..31], [16, 8, 35..38]]} */
static uint8_t op_getspecial[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
  0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
  0xbe, 0x00, 0x00, 0xbc, 0x00,             /*e: mov    $0xbc0000,%esi */
  0xff, 0x93, 0x58, 0x02, 0x00, 0x00,       /*13: callq  *0x258(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*19: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*20: mov    %edx,0xab1008(%r14) */
  0x48, 0x89, 0xdf,                         /*27: mov    %rbx,%rdi */
  0x5b,                                     /*2a: pop    %rbx */
  0x41, 0x5e,                               /*2b: pop    %r14 */

};

static void op_getspecial_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 1 + 0;
  *((int32_t *)(op + 28)) = a * 16 + 0;
  *((int32_t *)(op + 35)) = a * 16 + 8;
}

static void op_getspecial_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getspecial_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 15..18], [16, 8, 21..24]], "b"=>[[1, 0, 26..29]]} */
static uint8_t op_setspecial[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x7b, 0x58,                   /*8: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*c: mov    0xab1000(%rax),%rdx */
  0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*13: mov    0xab1008(%rax),%ecx */
  0xbe, 0x00, 0x00, 0xbc, 0x00,             /*19: mov    $0xbc0000,%esi */
  0xff, 0x93, 0x40, 0x01, 0x00, 0x00,       /*1e: callq  *0x140(%rbx) */
  0x48, 0x89, 0xdf,                         /*24: mov    %rbx,%rdi */
  0x5b,                                     /*27: pop    %rbx */

};

static void op_setspecial_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 16 + 0;
  *((int32_t *)(op + 21)) = a * 16 + 8;
  *((int32_t *)(op + 26)) = b * 1 + 0;
}

static void op_setspecial_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setspecial_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[16, 0, 33..36], [16, 8, 40..43]]} */
static uint8_t op_getiv[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*6: mov    0x58(%rbx),%rdi */
  0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
  0x48, 0x8b, 0x43, 0x28,                   /*e: mov    0x28(%rbx),%rax */
  0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00,       /*12: mov    0xbc0400(%rax),%esi */
  0xff, 0x93, 0xc8, 0x00, 0x00, 0x00,       /*18: callq  *0xc8(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*1e: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*25: mov    %edx,0xab1008(%r14) */
  0x48, 0x89, 0xdf,                         /*2c: mov    %rbx,%rdi */
  0x5b,                                     /*2f: pop    %rbx */
  0x41, 0x5e,                               /*30: pop    %r14 */

};

static void op_getiv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 33)) = a * 16 + 0;
  *((int32_t *)(op + 40)) = a * 16 + 8;
}

static void op_getiv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getiv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 8, 31..34]]} */
static uint8_t op_setiv[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
  0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
  0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rdx */
  0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*1d: mov    0xab1008(%rax),%ecx */
  0xff, 0x93, 0x10, 0x02, 0x00, 0x00,       /*23: callq  *0x210(%rbx) */
  0x48, 0x89, 0xdf,                         /*29: mov    %rbx,%rdi */
  0x5b,                                     /*2c: pop    %rbx */

};

static void op_setiv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 31)) = a * 16 + 8;
}

static void op_setiv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setiv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 40..43]], "a"=>[[16, 0, 53..56], [16, 8, 60..63]]} */
static uint8_t op_getcv[] = {
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
  0xff, 0x93, 0xe8, 0x00, 0x00, 0x00,       /*2c: callq  *0xe8(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*32: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*39: mov    %edx,0xab1008(%r14) */
  0x48, 0x8b, 0x43, 0x58,                   /*40: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*44: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x40, 0x20,                   /*48: mov    0x20(%rax),%rax */
  0x48, 0xc7, 0x40, 0x38, 0x00, 0x00, 0x00, 0x00,/*4c: movq   $0x0,0x38(%rax) */
  0x48, 0x89, 0xdf,                         /*54: mov    %rbx,%rdi */
  0x5b,                                     /*57: pop    %rbx */
  0x41, 0x5e,                               /*58: pop    %r14 */

};

static void op_getcv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 40)) = b * 4 + 0;
  *((int32_t *)(op + 53)) = a * 16 + 0;
  *((int32_t *)(op + 60)) = a * 16 + 8;
}

static void op_getcv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getcv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 8, 31..34]]} */
static uint8_t op_setcv[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
  0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
  0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rdx */
  0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*1d: mov    0xab1008(%rax),%ecx */
  0xff, 0x93, 0x38, 0x02, 0x00, 0x00,       /*23: callq  *0x238(%rbx) */
  0x48, 0x89, 0xdf,                         /*29: mov    %rbx,%rdi */
  0x5b,                                     /*2c: pop    %rbx */

};

static void op_setcv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 31)) = a * 16 + 8;
}

static void op_setcv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setcv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 34..37]], "a"=>[[16, 0, 83..86], [16, 8, 89..92]]} */
static uint8_t op_getconst[] = {
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
  0xff, 0x93, 0x98, 0x00, 0x00, 0x00,       /*26: callq  *0x98(%rbx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*2c: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*30: mov    0x18(%rcx),%rcx */
  0x48, 0x8b, 0x49, 0x20,                   /*34: mov    0x20(%rcx),%rcx */
  0x48, 0xc7, 0x41, 0x38, 0x00, 0x00, 0x00, 0x00,/*38: movq   $0x0,0x38(%rcx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*40: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*44: mov    0x18(%rcx),%rcx */
  0x48, 0x8b, 0x49, 0x08,                   /*48: mov    0x8(%rcx),%rcx */
  0x48, 0x89, 0x4b, 0x18,                   /*4c: mov    %rcx,0x18(%rbx) */
  0x48, 0x89, 0x81, 0x00, 0x10, 0xab, 0x00, /*50: mov    %rax,0xab1000(%rcx) */
  0x89, 0x91, 0x08, 0x10, 0xab, 0x00,       /*57: mov    %edx,0xab1008(%rcx) */
  0x48, 0x89, 0xdf,                         /*5d: mov    %rbx,%rdi */
  0x5b,                                     /*60: pop    %rbx */

};

static void op_getconst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 34)) = b * 4 + 0;
  *((int32_t *)(op + 83)) = a * 16 + 0;
  *((int32_t *)(op + 89)) = a * 16 + 8;
}

static void op_getconst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getconst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 8, 31..34]]} */
static uint8_t op_setconst[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
  0x8b, 0xb1, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%esi */
  0x48, 0x8b, 0x90, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rdx */
  0x8b, 0x88, 0x08, 0x10, 0xab, 0x00,       /*1d: mov    0xab1008(%rax),%ecx */
  0xff, 0x93, 0xd8, 0x01, 0x00, 0x00,       /*23: callq  *0x1d8(%rbx) */
  0x48, 0x89, 0xdf,                         /*29: mov    %rbx,%rdi */
  0x5b,                                     /*2c: pop    %rbx */

};

static void op_setconst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 16 + 0;
  *((int32_t *)(op + 31)) = a * 16 + 8;
}

static void op_setconst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setconst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 38..41]], "a"=>[[16, 0, 45..48], [16, 8, 51..54], [16, 0, 100..103], [16, 8, 106..109]]} */
static uint8_t op_getmcnst[] = {
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
  0xff, 0x93, 0x30, 0x02, 0x00, 0x00,       /*37: callq  *0x230(%rbx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*3d: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*41: mov    0x18(%rcx),%rcx */
  0x48, 0x8b, 0x49, 0x20,                   /*45: mov    0x20(%rcx),%rcx */
  0x48, 0xc7, 0x41, 0x38, 0x00, 0x00, 0x00, 0x00,/*49: movq   $0x0,0x38(%rcx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*51: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*55: mov    0x18(%rcx),%rcx */
  0x48, 0x8b, 0x49, 0x08,                   /*59: mov    0x8(%rcx),%rcx */
  0x48, 0x89, 0x4b, 0x18,                   /*5d: mov    %rcx,0x18(%rbx) */
  0x48, 0x89, 0x81, 0x00, 0x10, 0xab, 0x00, /*61: mov    %rax,0xab1000(%rcx) */
  0x89, 0x91, 0x08, 0x10, 0xab, 0x00,       /*68: mov    %edx,0xab1008(%rcx) */
  0x48, 0x89, 0xdf,                         /*6e: mov    %rbx,%rdi */
  0x5b,                                     /*71: pop    %rbx */

};

static void op_getmcnst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 38)) = b * 4 + 0;
  *((int32_t *)(op + 45)) = a * 16 + 0;
  *((int32_t *)(op + 51)) = a * 16 + 8;
  *((int32_t *)(op + 100)) = a * 16 + 0;
  *((int32_t *)(op + 106)) = a * 16 + 8;
}

static void op_getmcnst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getmcnst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 16, 25..28], [16, 24, 31..34], [16, 0, 38..41], [16, 8, 45..48]]} */
static uint8_t op_setmcnst[] = {
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
  0xff, 0x93, 0x10, 0x01, 0x00, 0x00,       /*31: callq  *0x110(%rbx) */
  0x48, 0x89, 0xdf,                         /*37: mov    %rbx,%rdi */
  0x5b,                                     /*3a: pop    %rbx */

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


/* args: {"c"=>[[1, 0, 15..18]], "a"=>[[16, 0, 33..36], [16, 0, 73..76], [16, 8, 80..83]], "b"=>[[16, 0, 44..47], [16, 8, 51..54]]} */
static uint8_t op_getupvar[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
  0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
  0xbe, 0x00, 0x00, 0xcd, 0x00,             /*e: mov    $0xcd0000,%esi */
  0xff, 0x93, 0x90, 0x02, 0x00, 0x00,       /*13: callq  *0x290(%rbx) */
  0x48, 0x85, 0xc0,                         /*19: test   %rax,%rax */
  0x74, 0x22,                               /*1c: je     40 <op_getupvar+0x40> */
  0x49, 0x81, 0xc6, 0x00, 0x10, 0xab, 0x00, /*1e: add    $0xab1000,%r14 */
  0x48, 0x8b, 0x40, 0x18,                   /*25: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x88, 0x00, 0x10, 0xbc, 0x00, /*29: mov    0xbc1000(%rax),%rcx */
  0x48, 0x8b, 0x80, 0x08, 0x10, 0xbc, 0x00, /*30: mov    0xbc1008(%rax),%rax */
  0x49, 0x89, 0x46, 0x08,                   /*37: mov    %rax,0x8(%r14) */
  0x49, 0x89, 0x0e,                         /*3b: mov    %rcx,(%r14) */
  0xeb, 0x14,                               /*3e: jmp    54 <op_getupvar+0x54> */
  0xff, 0x93, 0xc0, 0x00, 0x00, 0x00,       /*40: callq  *0xc0(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*46: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*4d: mov    %edx,0xab1008(%r14) */
  0x48, 0x89, 0xdf,                         /*54: mov    %rbx,%rdi */
  0x5b,                                     /*57: pop    %rbx */
  0x41, 0x5e,                               /*58: pop    %r14 */

};

static void op_getupvar_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = c * 1 + 0;
  *((int32_t *)(op + 33)) = a * 16 + 0;
  *((int32_t *)(op + 73)) = a * 16 + 0;
  *((int32_t *)(op + 80)) = a * 16 + 8;
  *((int32_t *)(op + 44)) = b * 16 + 0;
  *((int32_t *)(op + 51)) = b * 16 + 8;
}

static void op_getupvar_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getupvar_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"c"=>[[1, 0, 9..12]], "a"=>[[16, 0, 35..38], [16, 8, 42..45]], "b"=>[[16, 8, 49..52], [16, 0, 56..59]]} */
static uint8_t op_setupvar[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
  0xbe, 0x00, 0x00, 0xcd, 0x00,             /*8: mov    $0xcd0000,%esi */
  0xff, 0x93, 0x90, 0x02, 0x00, 0x00,       /*d: callq  *0x290(%rbx) */
  0x48, 0x85, 0xc0,                         /*13: test   %rax,%rax */
  0x74, 0x31,                               /*16: je     49 <op_setupvar+0x49> */
  0x48, 0x8b, 0x4b, 0x18,                   /*18: mov    0x18(%rbx),%rcx */
  0x48, 0x8b, 0x50, 0x18,                   /*1c: mov    0x18(%rax),%rdx */
  0x48, 0x8b, 0xb1, 0x00, 0x10, 0xab, 0x00, /*20: mov    0xab1000(%rcx),%rsi */
  0x48, 0x8b, 0x89, 0x08, 0x10, 0xab, 0x00, /*27: mov    0xab1008(%rcx),%rcx */
  0x48, 0x89, 0x8a, 0x08, 0x10, 0xbc, 0x00, /*2e: mov    %rcx,0xbc1008(%rdx) */
  0x48, 0x89, 0xb2, 0x00, 0x10, 0xbc, 0x00, /*35: mov    %rsi,0xbc1000(%rdx) */
  0x48, 0x8b, 0x7b, 0x58,                   /*3c: mov    0x58(%rbx),%rdi */
  0x48, 0x89, 0xc6,                         /*40: mov    %rax,%rsi */
  0xff, 0x93, 0x08, 0x01, 0x00, 0x00,       /*43: callq  *0x108(%rbx) */
  0x48, 0x89, 0xdf,                         /*49: mov    %rbx,%rdi */
  0x5b,                                     /*4c: pop    %rbx */

};

static void op_setupvar_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 9)) = c * 1 + 0;
  *((int32_t *)(op + 35)) = a * 16 + 0;
  *((int32_t *)(op + 42)) = a * 16 + 8;
  *((int32_t *)(op + 49)) = b * 16 + 8;
  *((int32_t *)(op + 56)) = b * 16 + 0;
}

static void op_setupvar_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setupvar_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_jmp[] = {

};

static void op_jmp_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_jmp_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmp_set_args(op, 0,GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9]]} */
static uint8_t op_jmpif[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0x83, 0xb8, 0x08, 0x10, 0xab, 0x00, 0x00, /*4: cmpl   $0x0,0xab1008(%rax) */

};

static void op_jmpif_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
}

static void op_jmpif_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmpif_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 6..9]]} */
static uint8_t op_jmpnot[] = {
  0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
  0x83, 0xb8, 0x08, 0x10, 0xab, 0x00, 0x00, /*4: cmpl   $0x0,0xab1008(%rax) */

};

static void op_jmpnot_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 6)) = a * 16 + 8;
}

static void op_jmpnot_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmpnot_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"op_idx"=>[[4, 0, 93..96]]} */
static uint8_t op_onerr[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x58,                   /*4: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x48, 0x18,                   /*8: mov    0x18(%rax),%rcx */
  0x8b, 0x51, 0x40,                         /*c: mov    0x40(%rcx),%edx */
  0x48, 0x8b, 0x71, 0x20,                   /*f: mov    0x20(%rcx),%rsi */
  0x3b, 0x56, 0x1c,                         /*13: cmp    0x1c(%rsi),%edx */
  0x7f, 0x3a,                               /*16: jg     52 <op_onerr+0x52> */
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
  0xff, 0x93, 0x38, 0x01, 0x00, 0x00,       /*3c: callq  *0x138(%rbx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*42: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*46: mov    0x18(%rcx),%rcx */
  0x48, 0x89, 0x41, 0x38,                   /*4a: mov    %rax,0x38(%rcx) */
  0x48, 0x8b, 0x43, 0x58,                   /*4e: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x4b, 0x08,                   /*52: mov    0x8(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x08,                   /*56: mov    0x8(%rcx),%rcx */
  0x48, 0x8d, 0x91, 0x00, 0x04, 0xde, 0x00, /*5a: lea    0xde0400(%rcx),%rdx */
  0x48, 0x89, 0x53, 0x10,                   /*61: mov    %rdx,0x10(%rbx) */
  0x48, 0x81, 0xc1, 0x00, 0x00, 0x68, 0x06, /*65: add    $0x6680000,%rcx */
  0x48, 0x8b, 0x40, 0x18,                   /*6c: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x40, 0x20,                   /*70: mov    0x20(%rax),%rax */
  0x48, 0x63, 0x50, 0x1c,                   /*74: movslq 0x1c(%rax),%rdx */
  0x8d, 0x72, 0x01,                         /*78: lea    0x1(%rdx),%esi */
  0x89, 0x70, 0x1c,                         /*7b: mov    %esi,0x1c(%rax) */
  0x48, 0x8b, 0x43, 0x58,                   /*7e: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*82: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x40, 0x38,                   /*86: mov    0x38(%rax),%rax */
  0x48, 0x89, 0x0c, 0xd0,                   /*8a: mov    %rcx,(%rax,%rdx,8) */
  0x48, 0x89, 0xdf,                         /*8e: mov    %rbx,%rdi */
  0x5b,                                     /*91: pop    %rbx */

};

static void op_onerr_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 93)) = op_idx * 4 + 0;
}

static void op_onerr_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_onerr_set_args(op, 0,GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 17..20], [16, 0, 36..39]]} */
static uint8_t op_rescue[] = {
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

static void op_rescue_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 16 + 8;
  *((int32_t *)(op + 36)) = a * 16 + 0;
}

static void op_rescue_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_rescue_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {} */
static uint8_t op_poperr[] = {
  0xb8, 0x00, 0x00, 0x55, 0xff,             /*0: mov    $0xff550000,%eax */
  0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*5: data16 nopw %cs:0x0(%rax,%rax,1) */
  0x48, 0x8b, 0x4f, 0x58,                   /*10: mov    0x58(%rdi),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*14: mov    0x18(%rcx),%rcx */
  0x48, 0x8b, 0x49, 0x20,                   /*18: mov    0x20(%rcx),%rcx */
  0xff, 0x49, 0x1c,                         /*1c: decl   0x1c(%rcx) */
  0xff, 0xc0,                               /*1f: inc    %eax */
  0x75, 0xed,                               /*21: jne    10 <op_poperr+0x10> */

};

static void op_poperr_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_poperr_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_poperr_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 0, 15..18]]} */
static uint8_t op_raise[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x4b, 0x58,                   /*8: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x80, 0x00, 0x10, 0xab, 0x00, /*c: mov    0xab1000(%rax),%rax */
  0x48, 0x89, 0x41, 0x28,                   /*13: mov    %rax,0x28(%rcx) */
  0xff, 0x93, 0xd0, 0x01, 0x00, 0x00,       /*17: callq  *0x1d0(%rbx) */
  0x48, 0x89, 0xdf,                         /*1d: mov    %rbx,%rdi */
  0x5b,                                     /*20: pop    %rbx */

};

static void op_raise_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 16 + 0;
}

static void op_raise_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_raise_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[8, 0, 21..24]]} */
static uint8_t op_epush[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x08,                   /*6: mov    0x8(%rbx),%rax */
  0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x40, 0x20,                   /*e: mov    0x20(%rax),%rax */
  0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*12: mov    0xbc0800(%rax),%rsi */
  0xff, 0x93, 0x88, 0x00, 0x00, 0x00,       /*19: callq  *0x88(%rbx) */
  0x49, 0x89, 0xc6,                         /*1f: mov    %rax,%r14 */
  0x48, 0x8b, 0x43, 0x58,                   /*22: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*26: mov    0x18(%rax),%rax */
  0x8b, 0x70, 0x50,                         /*2a: mov    0x50(%rax),%esi */
  0x48, 0x8b, 0x48, 0x20,                   /*2d: mov    0x20(%rax),%rcx */
  0x8b, 0x51, 0x20,                         /*31: mov    0x20(%rcx),%edx */
  0x39, 0xd6,                               /*34: cmp    %edx,%esi */
  0x7f, 0x45,                               /*36: jg     7d <op_epush+0x7d> */
  0x8d, 0x0c, 0x36,                         /*38: lea    (%rsi,%rsi,1),%ecx */
  0x85, 0xf6,                               /*3b: test   %esi,%esi */
  0xba, 0x10, 0x00, 0x00, 0x00,             /*3d: mov    $0x10,%edx */
  0x0f, 0x45, 0xd1,                         /*42: cmovne %ecx,%edx */
  0x89, 0x50, 0x50,                         /*45: mov    %edx,0x50(%rax) */
  0x48, 0x8b, 0x7b, 0x58,                   /*48: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x47, 0x18,                   /*4c: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x70, 0x48,                   /*50: mov    0x48(%rax),%rsi */
  0x48, 0x63, 0x50, 0x50,                   /*54: movslq 0x50(%rax),%rdx */
  0x48, 0xc1, 0xe2, 0x03,                   /*58: shl    $0x3,%rdx */
  0xff, 0x93, 0x38, 0x01, 0x00, 0x00,       /*5c: callq  *0x138(%rbx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*62: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*66: mov    0x18(%rcx),%rcx */
  0x48, 0x89, 0x41, 0x48,                   /*6a: mov    %rax,0x48(%rcx) */
  0x48, 0x8b, 0x43, 0x58,                   /*6e: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*72: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x48, 0x20,                   /*76: mov    0x20(%rax),%rcx */
  0x8b, 0x51, 0x20,                         /*7a: mov    0x20(%rcx),%edx */
  0x8d, 0x42, 0x01,                         /*7d: lea    0x1(%rdx),%eax */
  0x89, 0x41, 0x20,                         /*80: mov    %eax,0x20(%rcx) */
  0x48, 0x63, 0xc2,                         /*83: movslq %edx,%rax */
  0x48, 0x8b, 0x4b, 0x58,                   /*86: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*8a: mov    0x18(%rcx),%rcx */
  0x48, 0x8b, 0x49, 0x48,                   /*8e: mov    0x48(%rcx),%rcx */
  0x4c, 0x89, 0x34, 0xc1,                   /*92: mov    %r14,(%rcx,%rax,8) */
  0x8b, 0x43, 0x50,                         /*96: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*99: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*9d: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*a3: mov    %rbx,%rdi */
  0x5b,                                     /*a6: pop    %rbx */
  0x41, 0x5e,                               /*a7: pop    %r14 */

};

static void op_epush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 8 + 0;
}

static void op_epush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_epush_set_args(op, 0,GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[1, 0, 71..74]]} */
static uint8_t op_epop[] = {
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
  0x7e, 0x27,                               /*24: jle    4d <op_epop+0x4d> */
  0x49, 0x8b, 0x7f, 0x58,                   /*26: mov    0x58(%r15),%rdi */
  0xff, 0xcd,                               /*2a: dec    %ebp */
  0x89, 0xee,                               /*2c: mov    %ebp,%esi */
  0x41, 0xff, 0x97, 0x20, 0x02, 0x00, 0x00, /*2e: callq  *0x220(%r15) */
  0x41, 0x8b, 0x47, 0x50,                   /*35: mov    0x50(%r15),%eax */
  0x49, 0x8b, 0x4f, 0x58,                   /*39: mov    0x58(%r15),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*3d: mov    %eax,0xdc(%rcx) */
  0xff, 0xc3,                               /*43: inc    %ebx */
  0x81, 0xfb, 0x00, 0x00, 0xab, 0x00,       /*45: cmp    $0xab0000,%ebx */
  0x7c, 0xd3,                               /*4b: jl     20 <op_epop+0x20> */
  0x4c, 0x89, 0xff,                         /*4d: mov    %r15,%rdi */
  0x5b,                                     /*50: pop    %rbx */
  0x41, 0x5e,                               /*51: pop    %r14 */
  0x41, 0x5f,                               /*53: pop    %r15 */
  0x5d,                                     /*55: pop    %rbp */

};

static void op_epop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 71)) = a * 1 + 0;
}

static void op_epop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_epop_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"op_idx"=>[[4, 0, 23..26]], "a"=>[[1, 0, 41..44]], "b"=>[[1, 0, 46..49]], "c"=>[[1, 0, 52..55]]} */
static uint8_t op_send[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x08,                   /*6: mov    0x8(%rbx),%rax */
  0x48, 0x8b, 0x4b, 0x58,                   /*a: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*e: mov    0x18(%rcx),%rcx */
  0x4c, 0x8b, 0x71, 0x20,                   /*12: mov    0x20(%rcx),%r14 */
  0xb9, 0x00, 0x04, 0xde, 0x00,             /*16: mov    $0xde0400,%ecx */
  0x48, 0x03, 0x48, 0x08,                   /*1b: add    0x8(%rax),%rcx */
  0x48, 0x89, 0x4b, 0x10,                   /*1f: mov    %rcx,0x10(%rbx) */
  0xbe, 0x20, 0x00, 0x00, 0x00,             /*23: mov    $0x20,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*28: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*2d: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*32: mov    $0xcd0000,%r8d */
  0xff, 0x93, 0xf8, 0x01, 0x00, 0x00,       /*38: callq  *0x1f8(%rbx) */
  0x48, 0x8b, 0x43, 0x58,                   /*3e: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*42: mov    0x18(%rax),%rax */
  0x4c, 0x39, 0x70, 0x20,                   /*46: cmp    %r14,0x20(%rax) */
  0x73, 0x07,                               /*4a: jae    53 <op_send+0x53> */
  0x48, 0x89, 0xdf,                         /*4c: mov    %rbx,%rdi */
  0x5b,                                     /*4f: pop    %rbx */
  0x41, 0x5e,                               /*50: pop    %r14 */
  0xc3,                                     /*52: retq */
  0x48, 0x89, 0xdf,                         /*53: mov    %rbx,%rdi */
  0x5b,                                     /*56: pop    %rbx */
  0x41, 0x5e,                               /*57: pop    %r14 */

};

static void op_send_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 23)) = op_idx * 4 + 0;
  *((int32_t *)(op + 41)) = a * 1 + 0;
  *((int32_t *)(op + 46)) = b * 1 + 0;
  *((int32_t *)(op + 52)) = c * 1 + 0;
}

static void op_send_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_send_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[1, 0, 10..13]], "b"=>[[1, 0, 15..18]], "c"=>[[1, 0, 21..24]]} */
static uint8_t op_sendb[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0xbe, 0x21, 0x00, 0x00, 0x00,             /*4: mov    $0x21,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*9: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*e: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*13: mov    $0xcd0000,%r8d */
  0xff, 0x93, 0xf8, 0x01, 0x00, 0x00,       /*19: callq  *0x1f8(%rbx) */
  0x48, 0x89, 0xdf,                         /*1f: mov    %rbx,%rdi */
  0x5b,                                     /*22: pop    %rbx */

};

static void op_sendb_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 1 + 0;
  *((int32_t *)(op + 15)) = b * 1 + 0;
  *((int32_t *)(op + 21)) = c * 1 + 0;
}

static void op_sendb_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sendb_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_fsend[] = {

};

static void op_fsend_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_fsend_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_fsend_set_args(op, 0,0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 5..8]]} */
static uint8_t op_call[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0xbe, 0x00, 0x00, 0xab, 0x00,             /*4: mov    $0xab0000,%esi */
  0xff, 0x93, 0xb0, 0x02, 0x00, 0x00,       /*9: callq  *0x2b0(%rbx) */
  0x48, 0x89, 0xdf,                         /*f: mov    %rbx,%rdi */
  0x5b,                                     /*12: pop    %rbx */

};

static void op_call_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = a * 1 + 0;
}

static void op_call_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_call_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"c"=>[[1, 0, 95..98], [1, 1, 166..169], [1, 1, 204..207]], "a"=>[[16, 32, 154..157], [16, 16, 161..164], [16, 24, 182..185], [16, 16, 199..202], [16, 0, 281..284], [1, 0, 394..397]]} */
static uint8_t op_super[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x57,                               /*1: push   %r15 */
  0x41, 0x56,                               /*3: push   %r14 */
  0x41, 0x55,                               /*5: push   %r13 */
  0x41, 0x54,                               /*7: push   %r12 */
  0x53,                                     /*9: push   %rbx */
  0x48, 0x83, 0xec, 0x18,                   /*a: sub    $0x18,%rsp */
  0x48, 0x89, 0xfb,                         /*e: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x18,                   /*11: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x7b, 0x58,                   /*15: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x4f, 0x18,                   /*19: mov    0x18(%rdi),%rcx */
  0x4c, 0x8b, 0x71, 0x20,                   /*1d: mov    0x20(%rcx),%r14 */
  0x45, 0x8b, 0x3e,                         /*21: mov    (%r14),%r15d */
  0x48, 0x8b, 0x08,                         /*24: mov    (%rax),%rcx */
  0x48, 0x89, 0x0c, 0x24,                   /*27: mov    %rcx,(%rsp) */
  0x4c, 0x8b, 0x68, 0x08,                   /*2b: mov    0x8(%rax),%r13 */
  0x4c, 0x89, 0x6c, 0x24, 0x08,             /*2f: mov    %r13,0x8(%rsp) */
  0x49, 0xc1, 0xed, 0x20,                   /*34: shr    $0x20,%r13 */
  0x49, 0x8b, 0x46, 0x48,                   /*38: mov    0x48(%r14),%rax */
  0x48, 0x8b, 0x40, 0x28,                   /*3c: mov    0x28(%rax),%rax */
  0x48, 0x89, 0x44, 0x24, 0x10,             /*40: mov    %rax,0x10(%rsp) */
  0x48, 0x8b, 0x33,                         /*45: mov    (%rbx),%rsi */
  0x48, 0x8d, 0x54, 0x24, 0x10,             /*48: lea    0x10(%rsp),%rdx */
  0x44, 0x89, 0xf9,                         /*4d: mov    %r15d,%ecx */
  0xff, 0x93, 0x08, 0x02, 0x00, 0x00,       /*50: callq  *0x208(%rbx) */
  0x49, 0x89, 0xc4,                         /*56: mov    %rax,%r12 */
  0x4d, 0x85, 0xe4,                         /*59: test   %r12,%r12 */
  0x74, 0x07,                               /*5c: je     65 <op_super+0x65> */
  0xbd, 0x00, 0x00, 0xcd, 0x00,             /*5e: mov    $0xcd0000,%ebp */
  0xeb, 0x6b,                               /*63: jmp    d0 <op_super+0xd0> */
  0x48, 0x8b, 0x7b, 0x58,                   /*65: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0xb3, 0xb8, 0x02, 0x00, 0x00, /*69: mov    0x2b8(%rbx),%rsi */
  0xba, 0x0e, 0x00, 0x00, 0x00,             /*70: mov    $0xe,%edx */
  0xff, 0x93, 0xa0, 0x02, 0x00, 0x00,       /*75: callq  *0x2a0(%rbx) */
  0x41, 0x89, 0xc7,                         /*7b: mov    %eax,%r15d */
  0x48, 0x8b, 0x7b, 0x58,                   /*7e: mov    0x58(%rbx),%rdi */
  0x48, 0x8d, 0x74, 0x24, 0x10,             /*82: lea    0x10(%rsp),%rsi */
  0x44, 0x89, 0xfa,                         /*87: mov    %r15d,%edx */
  0xff, 0x93, 0xd8, 0x00, 0x00, 0x00,       /*8a: callq  *0xd8(%rbx) */
  0x49, 0x89, 0xc4,                         /*90: mov    %rax,%r12 */
  0x48, 0x8b, 0x73, 0x18,                   /*93: mov    0x18(%rbx),%rsi */
  0x48, 0x8d, 0xbe, 0x20, 0x10, 0xab, 0x00, /*97: lea    0xab1020(%rsi),%rdi */
  0x48, 0x81, 0xc6, 0x10, 0x10, 0xab, 0x00, /*9e: add    $0xab1010,%rsi */
  0xba, 0x01, 0x00, 0xcd, 0x00,             /*a5: mov    $0xcd0001,%edx */
  0xff, 0x93, 0xb0, 0x00, 0x00, 0x00,       /*aa: callq  *0xb0(%rbx) */
  0x48, 0x8b, 0x43, 0x18,                   /*b0: mov    0x18(%rbx),%rax */
  0xc7, 0x80, 0x18, 0x10, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*b4: movl   $0x4,0xab1018(%rax) */
  0x41, 0x8b, 0x06,                         /*be: mov    (%r14),%eax */
  0x48, 0x8b, 0x4b, 0x18,                   /*c1: mov    0x18(%rbx),%rcx */
  0x89, 0x81, 0x10, 0x10, 0xab, 0x00,       /*c5: mov    %eax,0xab1010(%rcx) */
  0xbd, 0x01, 0x00, 0xcd, 0x00,             /*cb: mov    $0xcd0001,%ebp */
  0x4c, 0x8b, 0x34, 0x24,                   /*d0: mov    (%rsp),%r14 */
  0x48, 0x8b, 0x7b, 0x58,                   /*d4: mov    0x58(%rbx),%rdi */
  0xff, 0x93, 0xf0, 0x00, 0x00, 0x00,       /*d8: callq  *0xf0(%rbx) */
  0x44, 0x89, 0x38,                         /*de: mov    %r15d,(%rax) */
  0x4c, 0x89, 0x60, 0x08,                   /*e1: mov    %r12,0x8(%rax) */
  0x48, 0x8b, 0x4b, 0x58,                   /*e5: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*e9: mov    0x18(%rcx),%rcx */
  0x48, 0x8b, 0x49, 0x08,                   /*ed: mov    0x8(%rcx),%rcx */
  0x48, 0x89, 0x48, 0x10,                   /*f1: mov    %rcx,0x10(%rax) */
  0x89, 0x68, 0x40,                         /*f5: mov    %ebp,0x40(%rax) */
  0x48, 0x8b, 0x4c, 0x24, 0x10,             /*f8: mov    0x10(%rsp),%rcx */
  0x48, 0x89, 0x48, 0x48,                   /*fd: mov    %rcx,0x48(%rax) */
  0x48, 0x8b, 0x4b, 0x10,                   /*101: mov    0x10(%rbx),%rcx */
  0x48, 0x83, 0xc1, 0x04,                   /*105: add    $0x4,%rcx */
  0x48, 0x89, 0x48, 0x30,                   /*109: mov    %rcx,0x30(%rax) */
  0x48, 0x8b, 0x4b, 0x58,                   /*10d: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*111: mov    0x18(%rcx),%rcx */
  0x48, 0x81, 0x41, 0x08, 0x00, 0x10, 0xab, 0x00,/*115: addq   $0xab1000,0x8(%rcx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*11d: mov    0x58(%rbx),%rcx */
  0x48, 0x8b, 0x49, 0x18,                   /*121: mov    0x18(%rcx),%rcx */
  0x48, 0x8b, 0x49, 0x08,                   /*125: mov    0x8(%rcx),%rcx */
  0x4c, 0x89, 0x31,                         /*129: mov    %r14,(%rcx) */
  0x48, 0x8b, 0x54, 0x24, 0x08,             /*12c: mov    0x8(%rsp),%rdx */
  0x89, 0x51, 0x08,                         /*131: mov    %edx,0x8(%rcx) */
  0x44, 0x89, 0x69, 0x0c,                   /*134: mov    %r13d,0xc(%rcx) */
  0x41, 0xf6, 0x44, 0x24, 0x02, 0x04,       /*138: testb  $0x4,0x2(%r12) */
  0x74, 0x47,                               /*13e: je     187 <op_super+0x187> */
  0x83, 0xcd, 0x02,                         /*140: or     $0x2,%ebp */
  0x89, 0x68, 0x18,                         /*143: mov    %ebp,0x18(%rax) */
  0x48, 0x8b, 0x7b, 0x58,                   /*146: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x47, 0x18,                   /*14a: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x68, 0x08,                   /*14e: mov    0x8(%rax),%rbp */
  0x4c, 0x89, 0xf6,                         /*152: mov    %r14,%rsi */
  0x41, 0xff, 0x54, 0x24, 0x18,             /*155: callq  *0x18(%r12) */
  0x48, 0x89, 0x45, 0x00,                   /*15a: mov    %rax,0x0(%rbp) */
  0x89, 0x55, 0x08,                         /*15e: mov    %edx,0x8(%rbp) */
  0x48, 0x8b, 0x7b, 0x58,                   /*161: mov    0x58(%rbx),%rdi */
  0x8b, 0x73, 0x50,                         /*165: mov    0x50(%rbx),%esi */
  0xff, 0x93, 0x18, 0x01, 0x00, 0x00,       /*168: callq  *0x118(%rbx) */
  0x48, 0x8b, 0x43, 0x58,                   /*16e: mov    0x58(%rbx),%rax */
  0x48, 0x83, 0x78, 0x28, 0x00,             /*172: cmpq   $0x0,0x28(%rax) */
  0x74, 0x7b,                               /*177: je     1f4 <op_super+0x1f4> */
  0x48, 0x89, 0xdf,                         /*179: mov    %rbx,%rdi */
  0xff, 0x93, 0xd0, 0x01, 0x00, 0x00,       /*17c: callq  *0x1d0(%rbx) */
  0xe9, 0x8b, 0x00, 0x00, 0x00,             /*182: jmpq   212 <op_super+0x212> */
  0xc7, 0x40, 0x44, 0x00, 0x00, 0xab, 0x00, /*187: movl   $0xab0000,0x44(%rax) */
  0x4c, 0x89, 0x60, 0x08,                   /*18e: mov    %r12,0x8(%rax) */
  0x49, 0x8b, 0x4c, 0x24, 0x18,             /*192: mov    0x18(%r12),%rcx */
  0x48, 0x89, 0x4b, 0x08,                   /*197: mov    %rcx,0x8(%rbx) */
  0x48, 0x8b, 0x51, 0x10,                   /*19b: mov    0x10(%rcx),%rdx */
  0x48, 0x89, 0x53, 0x20,                   /*19f: mov    %rdx,0x20(%rbx) */
  0x48, 0x8b, 0x51, 0x18,                   /*1a3: mov    0x18(%rcx),%rdx */
  0x48, 0x89, 0x53, 0x28,                   /*1a7: mov    %rdx,0x28(%rbx) */
  0x0f, 0xb7, 0x49, 0x02,                   /*1ab: movzwl 0x2(%rcx),%ecx */
  0x89, 0x48, 0x18,                         /*1af: mov    %ecx,0x18(%rax) */
  0x48, 0x8b, 0x4b, 0x08,                   /*1b2: mov    0x8(%rbx),%rcx */
  0x48, 0x8b, 0x7b, 0x58,                   /*1b6: mov    0x58(%rbx),%rdi */
  0x0f, 0xb7, 0x71, 0x02,                   /*1ba: movzwl 0x2(%rcx),%esi */
  0x8b, 0x50, 0x40,                         /*1be: mov    0x40(%rax),%edx */
  0x83, 0xc2, 0x02,                         /*1c1: add    $0x2,%edx */
  0xff, 0x93, 0x50, 0x01, 0x00, 0x00,       /*1c4: callq  *0x150(%rbx) */
  0x48, 0x8b, 0x7b, 0x58,                   /*1ca: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x47, 0x18,                   /*1ce: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x40, 0x08,                   /*1d2: mov    0x8(%rax),%rax */
  0x48, 0x89, 0x43, 0x18,                   /*1d6: mov    %rax,0x18(%rbx) */
  0x48, 0x8b, 0x43, 0x08,                   /*1da: mov    0x8(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x08,                   /*1de: mov    0x8(%rax),%rax */
  0x48, 0x89, 0x43, 0x10,                   /*1e2: mov    %rax,0x10(%rbx) */
  0x4c, 0x89, 0xe6,                         /*1e6: mov    %r12,%rsi */
  0x48, 0x89, 0xda,                         /*1e9: mov    %rbx,%rdx */
  0xff, 0x93, 0x28, 0x01, 0x00, 0x00,       /*1ec: callq  *0x128(%rbx) */
  0xeb, 0x1e,                               /*1f2: jmp    212 <op_super+0x212> */
  0x48, 0x8b, 0x40, 0x18,                   /*1f4: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x48, 0x20,                   /*1f8: mov    0x20(%rax),%rcx */
  0x48, 0x8b, 0x49, 0x10,                   /*1fc: mov    0x10(%rcx),%rcx */
  0x48, 0x89, 0x48, 0x08,                   /*200: mov    %rcx,0x8(%rax) */
  0x48, 0x89, 0x4b, 0x18,                   /*204: mov    %rcx,0x18(%rbx) */
  0x48, 0x8b, 0x7b, 0x58,                   /*208: mov    0x58(%rbx),%rdi */
  0xff, 0x93, 0x68, 0x01, 0x00, 0x00,       /*20c: callq  *0x168(%rbx) */
  0x48, 0x89, 0xdf,                         /*212: mov    %rbx,%rdi */
  0x48, 0x83, 0xc4, 0x18,                   /*215: add    $0x18,%rsp */
  0x5b,                                     /*219: pop    %rbx */
  0x41, 0x5c,                               /*21a: pop    %r12 */
  0x41, 0x5d,                               /*21c: pop    %r13 */
  0x41, 0x5e,                               /*21e: pop    %r14 */
  0x41, 0x5f,                               /*220: pop    %r15 */
  0x5d,                                     /*222: pop    %rbp */

};

static void op_super_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 95)) = c * 1 + 0;
  *((int32_t *)(op + 166)) = c * 1 + 1;
  *((int32_t *)(op + 204)) = c * 1 + 1;
  *((int32_t *)(op + 154)) = a * 16 + 32;
  *((int32_t *)(op + 161)) = a * 16 + 16;
  *((int32_t *)(op + 182)) = a * 16 + 24;
  *((int32_t *)(op + 199)) = a * 16 + 16;
  *((int32_t *)(op + 281)) = a * 16 + 0;
  *((int32_t *)(op + 394)) = a * 1 + 0;
}

static void op_super_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_super_set_args(op, GETARG_A(c),0,GETARG_C(c),op_idx);
}


/* args: {"b"=>[[1, 0, 21..24]], "a"=>[[16, 0, 200..203], [16, 8, 207..210], [16, 0, 218..221], [16, 0, 381..384], [16, 8, 388..391], [16, 24, 423..426], [16, 16, 430..433]]} */
static uint8_t op_argary[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x57,                               /*1: push   %r15 */
  0x41, 0x56,                               /*3: push   %r14 */
  0x41, 0x55,                               /*5: push   %r13 */
  0x41, 0x54,                               /*7: push   %r12 */
  0x53,                                     /*9: push   %rbx */
  0x48, 0x83, 0xec, 0x38,                   /*a: sub    $0x38,%rsp */
  0x48, 0x89, 0xfb,                         /*e: mov    %rdi,%rbx */
  0xc7, 0x44, 0x24, 0x34, 0x00, 0x00, 0xbc, 0x00,/*11: movl   $0xbc0000,0x34(%rsp) */
  0x8b, 0x6c, 0x24, 0x34,                   /*19: mov    0x34(%rsp),%ebp */
  0x44, 0x8b, 0x7c, 0x24, 0x34,             /*1d: mov    0x34(%rsp),%r15d */
  0x44, 0x8b, 0x64, 0x24, 0x34,             /*22: mov    0x34(%rsp),%r12d */
  0x8b, 0x74, 0x24, 0x34,                   /*27: mov    0x34(%rsp),%esi */
  0x83, 0xe6, 0x0f,                         /*2b: and    $0xf,%esi */
  0x74, 0x1b,                               /*2e: je     4b <op_argary+0x4b> */
  0x48, 0x8b, 0x7b, 0x58,                   /*30: mov    0x58(%rbx),%rdi */
  0xff, 0xce,                               /*34: dec    %esi */
  0xff, 0x93, 0x90, 0x02, 0x00, 0x00,       /*36: callq  *0x290(%rbx) */
  0x48, 0x85, 0xc0,                         /*3c: test   %rax,%rax */
  0x0f, 0x84, 0x7c, 0x01, 0x00, 0x00,       /*3f: je     1c1 <op_argary+0x1c1> */
  0x48, 0x83, 0xc0, 0x18,                   /*45: add    $0x18,%rax */
  0xeb, 0x04,                               /*49: jmp    4f <op_argary+0x4f> */
  0x48, 0x8d, 0x43, 0x18,                   /*4b: lea    0x18(%rbx),%rax */
  0xc1, 0xed, 0x0a,                         /*4f: shr    $0xa,%ebp */
  0x83, 0xe5, 0x3f,                         /*52: and    $0x3f,%ebp */
  0x41, 0xc1, 0xef, 0x09,                   /*55: shr    $0x9,%r15d */
  0x41, 0x83, 0xe7, 0x01,                   /*59: and    $0x1,%r15d */
  0x41, 0xc1, 0xec, 0x04,                   /*5d: shr    $0x4,%r12d */
  0x41, 0x83, 0xe4, 0x1f,                   /*61: and    $0x1f,%r12d */
  0x4c, 0x8b, 0x30,                         /*65: mov    (%rax),%r14 */
  0x49, 0x8d, 0x56, 0x10,                   /*68: lea    0x10(%r14),%rdx */
  0x45, 0x85, 0xff,                         /*6c: test   %r15d,%r15d */
  0x0f, 0x84, 0xf3, 0x00, 0x00, 0x00,       /*6f: je     168 <op_argary+0x168> */
  0x48, 0x89, 0x54, 0x24, 0x20,             /*75: mov    %rdx,0x20(%rsp) */
  0x89, 0xe8,                               /*7a: mov    %ebp,%eax */
  0x48, 0x89, 0x44, 0x24, 0x18,             /*7c: mov    %rax,0x18(%rsp) */
  0x48, 0x8d, 0x40, 0x01,                   /*81: lea    0x1(%rax),%rax */
  0x48, 0xc1, 0xe0, 0x04,                   /*85: shl    $0x4,%rax */
  0x31, 0xc9,                               /*89: xor    %ecx,%ecx */
  0x41, 0x83, 0x7c, 0x06, 0x08, 0x0e,       /*8b: cmpl   $0xe,0x8(%r14,%rax,1) */
  0xba, 0x00, 0x00, 0x00, 0x00,             /*91: mov    $0x0,%edx */
  0x75, 0x0b,                               /*96: jne    a3 <op_argary+0xa3> */
  0x49, 0x8b, 0x04, 0x06,                   /*98: mov    (%r14,%rax,1),%rax */
  0x48, 0x8b, 0x50, 0x28,                   /*9c: mov    0x28(%rax),%rdx */
  0x8b, 0x48, 0x18,                         /*a0: mov    0x18(%rax),%ecx */
  0x48, 0x89, 0x54, 0x24, 0x08,             /*a3: mov    %rdx,0x8(%rsp) */
  0x48, 0x89, 0x4c, 0x24, 0x28,             /*a8: mov    %rcx,0x28(%rsp) */
  0x4c, 0x8b, 0x6b, 0x18,                   /*ad: mov    0x18(%rbx),%r13 */
  0x48, 0x8b, 0x7b, 0x58,                   /*b1: mov    0x58(%rbx),%rdi */
  0x41, 0x8d, 0x34, 0x2c,                   /*b5: lea    (%r12,%rbp,1),%esi */
  0x01, 0xce,                               /*b9: add    %ecx,%esi */
  0x89, 0x74, 0x24, 0x14,                   /*bb: mov    %esi,0x14(%rsp) */
  0xff, 0x93, 0xe0, 0x00, 0x00, 0x00,       /*bf: callq  *0xe0(%rbx) */
  0x49, 0x89, 0x85, 0x00, 0x10, 0xab, 0x00, /*c5: mov    %rax,0xab1000(%r13) */
  0x41, 0x89, 0x95, 0x08, 0x10, 0xab, 0x00, /*cc: mov    %edx,0xab1008(%r13) */
  0x48, 0x8b, 0x43, 0x18,                   /*d3: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x88, 0x00, 0x10, 0xab, 0x00, /*d7: mov    0xab1000(%rax),%rcx */
  0x85, 0xed,                               /*de: test   %ebp,%ebp */
  0x4c, 0x8b, 0x6c, 0x24, 0x18,             /*e0: mov    0x18(%rsp),%r13 */
  0x74, 0x1c,                               /*e5: je     103 <op_argary+0x103> */
  0x48, 0x8b, 0x79, 0x28,                   /*e7: mov    0x28(%rcx),%rdi */
  0x48, 0x8b, 0x74, 0x24, 0x20,             /*eb: mov    0x20(%rsp),%rsi */
  0x4c, 0x89, 0xea,                         /*f0: mov    %r13,%rdx */
  0x48, 0x89, 0x4c, 0x24, 0x20,             /*f3: mov    %rcx,0x20(%rsp) */
  0xff, 0x93, 0x80, 0x02, 0x00, 0x00,       /*f8: callq  *0x280(%rbx) */
  0x48, 0x8b, 0x4c, 0x24, 0x20,             /*fe: mov    0x20(%rsp),%rcx */
  0x48, 0x8b, 0x44, 0x24, 0x28,             /*103: mov    0x28(%rsp),%rax */
  0x85, 0xc0,                               /*108: test   %eax,%eax */
  0x7e, 0x28,                               /*10a: jle    134 <op_argary+0x134> */
  0x4c, 0x89, 0xef,                         /*10c: mov    %r13,%rdi */
  0x48, 0xc1, 0xe7, 0x04,                   /*10f: shl    $0x4,%rdi */
  0x48, 0x03, 0x79, 0x28,                   /*113: add    0x28(%rcx),%rdi */
  0x48, 0x63, 0xd0,                         /*117: movslq %eax,%rdx */
  0x48, 0x8b, 0x74, 0x24, 0x08,             /*11a: mov    0x8(%rsp),%rsi */
  0x48, 0x89, 0x4c, 0x24, 0x20,             /*11f: mov    %rcx,0x20(%rsp) */
  0xff, 0x93, 0x80, 0x02, 0x00, 0x00,       /*124: callq  *0x280(%rbx) */
  0x48, 0x8b, 0x4c, 0x24, 0x20,             /*12a: mov    0x20(%rsp),%rcx */
  0x48, 0x8b, 0x44, 0x24, 0x28,             /*12f: mov    0x28(%rsp),%rax */
  0x45, 0x85, 0xe4,                         /*134: test   %r12d,%r12d */
  0x74, 0x26,                               /*137: je     15f <op_argary+0x15f> */
  0x48, 0x63, 0xf8,                         /*139: movslq %eax,%rdi */
  0x4c, 0x01, 0xef,                         /*13c: add    %r13,%rdi */
  0x48, 0xc1, 0xe7, 0x04,                   /*13f: shl    $0x4,%rdi */
  0x48, 0x03, 0x79, 0x28,                   /*143: add    0x28(%rcx),%rdi */
  0x49, 0xc1, 0xe5, 0x04,                   /*147: shl    $0x4,%r13 */
  0x4b, 0x8d, 0x74, 0x35, 0x20,             /*14b: lea    0x20(%r13,%r14,1),%rsi */
  0x44, 0x89, 0xe2,                         /*150: mov    %r12d,%edx */
  0x49, 0x89, 0xcd,                         /*153: mov    %rcx,%r13 */
  0xff, 0x93, 0x80, 0x02, 0x00, 0x00,       /*156: callq  *0x280(%rbx) */
  0x4c, 0x89, 0xe9,                         /*15c: mov    %r13,%rcx */
  0x8b, 0x44, 0x24, 0x14,                   /*15f: mov    0x14(%rsp),%eax */
  0x89, 0x41, 0x18,                         /*163: mov    %eax,0x18(%rcx) */
  0xeb, 0x20,                               /*166: jmp    188 <op_argary+0x188> */
  0x4c, 0x8b, 0x6b, 0x18,                   /*168: mov    0x18(%rbx),%r13 */
  0x48, 0x8b, 0x7b, 0x58,                   /*16c: mov    0x58(%rbx),%rdi */
  0x41, 0x8d, 0x34, 0x2c,                   /*170: lea    (%r12,%rbp,1),%esi */
  0xff, 0x93, 0x50, 0x02, 0x00, 0x00,       /*174: callq  *0x250(%rbx) */
  0x49, 0x89, 0x85, 0x00, 0x10, 0xab, 0x00, /*17a: mov    %rax,0xab1000(%r13) */
  0x41, 0x89, 0x95, 0x08, 0x10, 0xab, 0x00, /*181: mov    %edx,0xab1008(%r13) */
  0x48, 0x8b, 0x43, 0x18,                   /*188: mov    0x18(%rbx),%rax */
  0x44, 0x01, 0xfd,                         /*18c: add    %r15d,%ebp */
  0x41, 0x8d, 0x4c, 0x2c, 0x01,             /*18f: lea    0x1(%r12,%rbp,1),%ecx */
  0x48, 0x63, 0xc9,                         /*194: movslq %ecx,%rcx */
  0x48, 0xc1, 0xe1, 0x04,                   /*197: shl    $0x4,%rcx */
  0x49, 0x8b, 0x14, 0x0e,                   /*19b: mov    (%r14,%rcx,1),%rdx */
  0x49, 0x8b, 0x4c, 0x0e, 0x08,             /*19f: mov    0x8(%r14,%rcx,1),%rcx */
  0x48, 0x89, 0x88, 0x18, 0x10, 0xab, 0x00, /*1a4: mov    %rcx,0xab1018(%rax) */
  0x48, 0x89, 0x90, 0x10, 0x10, 0xab, 0x00, /*1ab: mov    %rdx,0xab1010(%rax) */
  0x8b, 0x43, 0x50,                         /*1b2: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*1b5: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*1b9: mov    %eax,0xdc(%rcx) */
  0xeb, 0x4b,                               /*1bf: jmp    20c <op_argary+0x20c> */
  0x48, 0x8b, 0x6b, 0x58,                   /*1c1: mov    0x58(%rbx),%rbp */
  0x48, 0x8b, 0x43, 0x78,                   /*1c5: mov    0x78(%rbx),%rax */
  0x48, 0x8b, 0x30,                         /*1c9: mov    (%rax),%rsi */
  0x48, 0x89, 0xef,                         /*1cc: mov    %rbp,%rdi */
  0xff, 0x93, 0x20, 0x01, 0x00, 0x00,       /*1cf: callq  *0x120(%rbx) */
  0x49, 0x89, 0xc6,                         /*1d5: mov    %rax,%r14 */
  0x48, 0x8b, 0x7b, 0x58,                   /*1d8: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0xb3, 0xc0, 0x02, 0x00, 0x00, /*1dc: mov    0x2c0(%rbx),%rsi */
  0xba, 0x1e, 0x00, 0x00, 0x00,             /*1e3: mov    $0x1e,%edx */
  0xff, 0x93, 0x80, 0x00, 0x00, 0x00,       /*1e8: callq  *0x80(%rbx) */
  0x89, 0xd1,                               /*1ee: mov    %edx,%ecx */
  0x48, 0x89, 0xef,                         /*1f0: mov    %rbp,%rdi */
  0x4c, 0x89, 0xf6,                         /*1f3: mov    %r14,%rsi */
  0x48, 0x89, 0xc2,                         /*1f6: mov    %rax,%rdx */
  0xff, 0x93, 0xe0, 0x01, 0x00, 0x00,       /*1f9: callq  *0x1e0(%rbx) */
  0x48, 0x89, 0x45, 0x28,                   /*1ff: mov    %rax,0x28(%rbp) */
  0x48, 0x89, 0xdf,                         /*203: mov    %rbx,%rdi */
  0xff, 0x93, 0xd0, 0x01, 0x00, 0x00,       /*206: callq  *0x1d0(%rbx) */
  0x48, 0x89, 0xdf,                         /*20c: mov    %rbx,%rdi */
  0x48, 0x83, 0xc4, 0x38,                   /*20f: add    $0x38,%rsp */
  0x5b,                                     /*213: pop    %rbx */
  0x41, 0x5c,                               /*214: pop    %r12 */
  0x41, 0x5d,                               /*216: pop    %r13 */
  0x41, 0x5e,                               /*218: pop    %r14 */
  0x41, 0x5f,                               /*21a: pop    %r15 */
  0x5d,                                     /*21c: pop    %rbp */

};

static void op_argary_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 1 + 0;
  *((int32_t *)(op + 200)) = a * 16 + 0;
  *((int32_t *)(op + 207)) = a * 16 + 8;
  *((int32_t *)(op + 218)) = a * 16 + 0;
  *((int32_t *)(op + 381)) = a * 16 + 0;
  *((int32_t *)(op + 388)) = a * 16 + 8;
  *((int32_t *)(op + 423)) = a * 16 + 24;
  *((int32_t *)(op + 430)) = a * 16 + 16;
}

static void op_argary_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_argary_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[1, 0, 21..24]]} */
static uint8_t op_enter[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x57,                               /*1: push   %r15 */
  0x41, 0x56,                               /*3: push   %r14 */
  0x41, 0x55,                               /*5: push   %r13 */
  0x41, 0x54,                               /*7: push   %r12 */
  0x53,                                     /*9: push   %rbx */
  0x48, 0x83, 0xec, 0x48,                   /*a: sub    $0x48,%rsp */
  0x49, 0x89, 0xff,                         /*e: mov    %rdi,%r15 */
  0xc7, 0x44, 0x24, 0x44, 0x00, 0x00, 0xab, 0x00,/*11: movl   $0xab0000,0x44(%rsp) */
  0x8b, 0x54, 0x24, 0x44,                   /*19: mov    0x44(%rsp),%edx */
  0xc1, 0xea, 0x12,                         /*1d: shr    $0x12,%edx */
  0x83, 0xe2, 0x1f,                         /*20: and    $0x1f,%edx */
  0x48, 0x89, 0x54, 0x24, 0x38,             /*23: mov    %rdx,0x38(%rsp) */
  0x8b, 0x4c, 0x24, 0x44,                   /*28: mov    0x44(%rsp),%ecx */
  0xc1, 0xe9, 0x0d,                         /*2c: shr    $0xd,%ecx */
  0x83, 0xe1, 0x1f,                         /*2f: and    $0x1f,%ecx */
  0x48, 0x89, 0x4c, 0x24, 0x10,             /*32: mov    %rcx,0x10(%rsp) */
  0x44, 0x8b, 0x64, 0x24, 0x44,             /*37: mov    0x44(%rsp),%r12d */
  0x41, 0xc1, 0xec, 0x0c,                   /*3c: shr    $0xc,%r12d */
  0x41, 0x83, 0xe4, 0x01,                   /*40: and    $0x1,%r12d */
  0x8b, 0x6c, 0x24, 0x44,                   /*44: mov    0x44(%rsp),%ebp */
  0xc1, 0xed, 0x07,                         /*48: shr    $0x7,%ebp */
  0x49, 0x8b, 0x5f, 0x18,                   /*4b: mov    0x18(%r15),%rbx */
  0x49, 0x8b, 0x7f, 0x58,                   /*4f: mov    0x58(%r15),%rdi */
  0x48, 0x8b, 0x47, 0x18,                   /*53: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x40, 0x20,                   /*57: mov    0x20(%rax),%rax */
  0x4c, 0x63, 0x70, 0x40,                   /*5b: movslq 0x40(%rax),%r14 */
  0x44, 0x8d, 0x2c, 0x11,                   /*5f: lea    (%rcx,%rdx,1),%r13d */
  0x4d, 0x85, 0xf6,                         /*63: test   %r14,%r14 */
  0x49, 0x8d, 0x4e, 0x01,                   /*66: lea    0x1(%r14),%rcx */
  0xb8, 0x02, 0x00, 0x00, 0x00,             /*6a: mov    $0x2,%eax */
  0x48, 0x0f, 0x49, 0xc1,                   /*6f: cmovns %rcx,%rax */
  0x48, 0xc1, 0xe0, 0x04,                   /*73: shl    $0x4,%rax */
  0x48, 0x8d, 0x0c, 0x03,                   /*77: lea    (%rbx,%rax,1),%rcx */
  0x48, 0x89, 0x4c, 0x24, 0x30,             /*7b: mov    %rcx,0x30(%rsp) */
  0x8b, 0x54, 0x03, 0x08,                   /*80: mov    0x8(%rbx,%rax,1),%edx */
  0x83, 0xfa, 0x0d,                         /*84: cmp    $0xd,%edx */
  0x74, 0x50,                               /*87: je     d9 <op_enter+0xd9> */
  0x85, 0xd2,                               /*89: test   %edx,%edx */
  0x75, 0x0a,                               /*8b: jne    97 <op_enter+0x97> */
  0x48, 0x8b, 0x4c, 0x24, 0x30,             /*8d: mov    0x30(%rsp),%rcx */
  0x83, 0x39, 0x00,                         /*92: cmpl   $0x0,(%rcx) */
  0x74, 0x42,                               /*95: je     d9 <op_enter+0xd9> */
  0x48, 0x8d, 0x44, 0x03, 0x08,             /*97: lea    0x8(%rbx,%rax,1),%rax */
  0x48, 0x89, 0x44, 0x24, 0x20,             /*9c: mov    %rax,0x20(%rsp) */
  0x4c, 0x89, 0x64, 0x24, 0x28,             /*a1: mov    %r12,0x28(%rsp) */
  0x4c, 0x8b, 0x64, 0x24, 0x30,             /*a6: mov    0x30(%rsp),%r12 */
  0x49, 0x8b, 0x34, 0x24,                   /*ab: mov    (%r12),%rsi */
  0x4d, 0x8b, 0x87, 0xc8, 0x02, 0x00, 0x00, /*af: mov    0x2c8(%r15),%r8 */
  0x4d, 0x8b, 0x8f, 0xd0, 0x02, 0x00, 0x00, /*b6: mov    0x2d0(%r15),%r9 */
  0xb9, 0x0d, 0x00, 0x00, 0x00,             /*bd: mov    $0xd,%ecx */
  0x41, 0xff, 0x97, 0x30, 0x01, 0x00, 0x00, /*c2: callq  *0x130(%r15) */
  0x49, 0x89, 0x04, 0x24,                   /*c9: mov    %rax,(%r12) */
  0x4c, 0x8b, 0x64, 0x24, 0x28,             /*cd: mov    0x28(%rsp),%r12 */
  0x48, 0x8b, 0x44, 0x24, 0x20,             /*d2: mov    0x20(%rsp),%rax */
  0x89, 0x10,                               /*d7: mov    %edx,(%rax) */
  0x83, 0xe5, 0x1f,                         /*d9: and    $0x1f,%ebp */
  0x43, 0x8d, 0x4c, 0x25, 0x00,             /*dc: lea    0x0(%r13,%r12,1),%ecx */
  0x4c, 0x89, 0x6c, 0x24, 0x18,             /*e1: mov    %r13,0x18(%rsp) */
  0x48, 0x83, 0xc3, 0x10,                   /*e6: add    $0x10,%rbx */
  0x45, 0x85, 0xf6,                         /*ea: test   %r14d,%r14d */
  0x49, 0x89, 0xd8,                         /*ed: mov    %rbx,%r8 */
  0x79, 0x2b,                               /*f0: jns    11d <op_enter+0x11d> */
  0x49, 0x8b, 0x47, 0x18,                   /*f2: mov    0x18(%r15),%rax */
  0x48, 0x89, 0x4c, 0x24, 0x20,             /*f6: mov    %rcx,0x20(%rsp) */
  0x49, 0x8b, 0x7f, 0x58,                   /*fb: mov    0x58(%r15),%rdi */
  0x48, 0x8b, 0x70, 0x10,                   /*ff: mov    0x10(%rax),%rsi */
  0x4c, 0x8b, 0x6e, 0x28,                   /*103: mov    0x28(%rsi),%r13 */
  0x44, 0x8b, 0x76, 0x18,                   /*107: mov    0x18(%rsi),%r14d */
  0x8b, 0x50, 0x18,                         /*10b: mov    0x18(%rax),%edx */
  0x41, 0xff, 0x97, 0x00, 0x02, 0x00, 0x00, /*10e: callq  *0x200(%r15) */
  0x4d, 0x89, 0xe8,                         /*115: mov    %r13,%r8 */
  0x48, 0x8b, 0x4c, 0x24, 0x20,             /*118: mov    0x20(%rsp),%rcx */
  0x48, 0x89, 0x4c, 0x24, 0x20,             /*11d: mov    %rcx,0x20(%rsp) */
  0x44, 0x8d, 0x2c, 0x29,                   /*122: lea    (%rcx,%rbp,1),%r13d */
  0x49, 0x8b, 0x7f, 0x58,                   /*126: mov    0x58(%r15),%rdi */
  0x48, 0x8b, 0x47, 0x18,                   /*12a: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x40, 0x20,                   /*12e: mov    0x20(%rax),%rax */
  0x48, 0x8b, 0x48, 0x08,                   /*132: mov    0x8(%rax),%rcx */
  0x48, 0x85, 0xc9,                         /*136: test   %rcx,%rcx */
  0x74, 0x39,                               /*139: je     174 <op_enter+0x174> */
  0xf6, 0x41, 0x02, 0x08,                   /*13b: testb  $0x8,0x2(%rcx) */
  0x74, 0x33,                               /*13f: je     174 <op_enter+0x174> */
  0x45, 0x85, 0xf6,                         /*141: test   %r14d,%r14d */
  0x78, 0x70,                               /*144: js     1b6 <op_enter+0x1b6> */
  0x48, 0x8b, 0x4c, 0x24, 0x38,             /*146: mov    0x38(%rsp),%rcx */
  0x8d, 0x74, 0x0d, 0x00,                   /*14b: lea    0x0(%rbp,%rcx,1),%esi */
  0x41, 0x39, 0xf6,                         /*14f: cmp    %esi,%r14d */
  0x7c, 0x0a,                               /*152: jl     15e <op_enter+0x15e> */
  0x45, 0x85, 0xe4,                         /*154: test   %r12d,%r12d */
  0x75, 0x5d,                               /*157: jne    1b6 <op_enter+0x1b6> */
  0x45, 0x39, 0xee,                         /*159: cmp    %r13d,%r14d */
  0x7e, 0x58,                               /*15c: jle    1b6 <op_enter+0x1b6> */
  0x41, 0xff, 0x97, 0xa8, 0x02, 0x00, 0x00, /*15e: callq  *0x2a8(%r15) */
  0x4c, 0x89, 0xff,                         /*165: mov    %r15,%rdi */
  0x41, 0xff, 0x97, 0xd0, 0x01, 0x00, 0x00, /*168: callq  *0x1d0(%r15) */
  0xe9, 0xd4, 0x02, 0x00, 0x00,             /*16f: jmpq   448 <op_enter+0x448> */
  0x41, 0x83, 0xfd, 0x02,                   /*174: cmp    $0x2,%r13d */
  0x7c, 0x3c,                               /*178: jl     1b6 <op_enter+0x1b6> */
  0x41, 0x83, 0xfe, 0x01,                   /*17a: cmp    $0x1,%r14d */
  0x75, 0x36,                               /*17e: jne    1b6 <op_enter+0x1b6> */
  0x41, 0xbe, 0x01, 0x00, 0x00, 0x00,       /*180: mov    $0x1,%r14d */
  0x41, 0x83, 0x78, 0x08, 0x0e,             /*186: cmpl   $0xe,0x8(%r8) */
  0x75, 0x29,                               /*18b: jne    1b6 <op_enter+0x1b6> */
  0x49, 0x8b, 0x30,                         /*18d: mov    (%r8),%rsi */
  0xba, 0x0e, 0x00, 0x00, 0x00,             /*190: mov    $0xe,%edx */
  0x4d, 0x89, 0xc6,                         /*195: mov    %r8,%r14 */
  0x41, 0xff, 0x97, 0x00, 0x02, 0x00, 0x00, /*198: callq  *0x200(%r15) */
  0x49, 0x8b, 0x06,                         /*19f: mov    (%r14),%rax */
  0x44, 0x8b, 0x70, 0x18,                   /*1a2: mov    0x18(%rax),%r14d */
  0x4c, 0x8b, 0x40, 0x28,                   /*1a6: mov    0x28(%rax),%r8 */
  0x49, 0x8b, 0x47, 0x58,                   /*1aa: mov    0x58(%r15),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*1ae: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x40, 0x20,                   /*1b2: mov    0x20(%rax),%rax */
  0x4c, 0x89, 0x64, 0x24, 0x28,             /*1b6: mov    %r12,0x28(%rsp) */
  0x44, 0x89, 0x68, 0x40,                   /*1bb: mov    %r13d,0x40(%rax) */
  0x45, 0x39, 0xee,                         /*1bf: cmp    %r13d,%r14d */
  0x0f, 0x8d, 0x33, 0x01, 0x00, 0x00,       /*1c2: jge    2fb <op_enter+0x2fb> */
  0x48, 0x8b, 0x44, 0x24, 0x38,             /*1c8: mov    0x38(%rsp),%rax */
  0x8d, 0x44, 0x05, 0x00,                   /*1cd: lea    0x0(%rbp,%rax,1),%eax */
  0x89, 0x04, 0x24,                         /*1d1: mov    %eax,(%rsp) */
  0x41, 0x39, 0xc6,                         /*1d4: cmp    %eax,%r14d */
  0x41, 0x89, 0xec,                         /*1d7: mov    %ebp,%r12d */
  0x48, 0x89, 0x6c, 0x24, 0x08,             /*1da: mov    %rbp,0x8(%rsp) */
  0x7d, 0x11,                               /*1df: jge    1f2 <op_enter+0x1f2> */
  0x31, 0xc0,                               /*1e1: xor    %eax,%eax */
  0x45, 0x89, 0xf4,                         /*1e3: mov    %r14d,%r12d */
  0x48, 0x8b, 0x4c, 0x24, 0x38,             /*1e6: mov    0x38(%rsp),%rcx */
  0x41, 0x29, 0xcc,                         /*1eb: sub    %ecx,%r12d */
  0x44, 0x0f, 0x4e, 0xe0,                   /*1ee: cmovle %eax,%r12d */
  0x41, 0xff, 0xc5,                         /*1f2: inc    %r13d */
  0x49, 0x8b, 0x47, 0x18,                   /*1f5: mov    0x18(%r15),%rax */
  0x49, 0xc1, 0xe5, 0x04,                   /*1f9: shl    $0x4,%r13 */
  0x48, 0x8b, 0x54, 0x24, 0x30,             /*1fd: mov    0x30(%rsp),%rdx */
  0x48, 0x8b, 0x0a,                         /*202: mov    (%rdx),%rcx */
  0x48, 0x8b, 0x52, 0x08,                   /*205: mov    0x8(%rdx),%rdx */
  0x4a, 0x89, 0x54, 0x28, 0x08,             /*209: mov    %rdx,0x8(%rax,%r13,1) */
  0x4a, 0x89, 0x0c, 0x28,                   /*20e: mov    %rcx,(%rax,%r13,1) */
  0x41, 0x8d, 0x46, 0x01,                   /*212: lea    0x1(%r14),%eax */
  0x48, 0x98,                               /*216: cltq */
  0x49, 0x8b, 0x4f, 0x18,                   /*218: mov    0x18(%r15),%rcx */
  0x48, 0xc1, 0xe0, 0x04,                   /*21c: shl    $0x4,%rax */
  0xc7, 0x44, 0x01, 0x08, 0x00, 0x00, 0x00, 0x00,/*220: movl   $0x0,0x8(%rcx,%rax,1) */
  0x49, 0x8b, 0x4f, 0x18,                   /*228: mov    0x18(%r15),%rcx */
  0xc7, 0x04, 0x01, 0x00, 0x00, 0x00, 0x00, /*22c: movl   $0x0,(%rcx,%rax,1) */
  0x4c, 0x39, 0xc3,                         /*233: cmp    %r8,%rbx */
  0x74, 0x21,                               /*236: je     259 <op_enter+0x259> */
  0x49, 0x8b, 0x7f, 0x18,                   /*238: mov    0x18(%r15),%rdi */
  0x48, 0x83, 0xc7, 0x10,                   /*23c: add    $0x10,%rdi */
  0x44, 0x89, 0xf0,                         /*240: mov    %r14d,%eax */
  0x44, 0x29, 0xe0,                         /*243: sub    %r12d,%eax */
  0x48, 0x63, 0xd0,                         /*246: movslq %eax,%rdx */
  0x4c, 0x89, 0xc6,                         /*249: mov    %r8,%rsi */
  0x4c, 0x89, 0xc3,                         /*24c: mov    %r8,%rbx */
  0x41, 0xff, 0x97, 0xb0, 0x00, 0x00, 0x00, /*24f: callq  *0xb0(%r15) */
  0x49, 0x89, 0xd8,                         /*256: mov    %rbx,%r8 */
  0x45, 0x85, 0xe4,                         /*259: test   %r12d,%r12d */
  0x48, 0x8b, 0x5c, 0x24, 0x18,             /*25c: mov    0x18(%rsp),%rbx */
  0x74, 0x2b,                               /*261: je     28e <op_enter+0x28e> */
  0x48, 0x8b, 0x7c, 0x24, 0x20,             /*263: mov    0x20(%rsp),%rdi */
  0xff, 0xc7,                               /*268: inc    %edi */
  0x48, 0xc1, 0xe7, 0x04,                   /*26a: shl    $0x4,%rdi */
  0x49, 0x03, 0x7f, 0x18,                   /*26e: add    0x18(%r15),%rdi */
  0x44, 0x89, 0xf0,                         /*272: mov    %r14d,%eax */
  0x44, 0x29, 0xe0,                         /*275: sub    %r12d,%eax */
  0x48, 0x98,                               /*278: cltq */
  0x48, 0xc1, 0xe0, 0x04,                   /*27a: shl    $0x4,%rax */
  0x49, 0x01, 0xc0,                         /*27e: add    %rax,%r8 */
  0x49, 0x63, 0xd4,                         /*281: movslq %r12d,%rdx */
  0x4c, 0x89, 0xc6,                         /*284: mov    %r8,%rsi */
  0x41, 0xff, 0x97, 0xb0, 0x00, 0x00, 0x00, /*287: callq  *0xb0(%r15) */
  0x48, 0x8b, 0x44, 0x24, 0x28,             /*28e: mov    0x28(%rsp),%rax */
  0x85, 0xc0,                               /*293: test   %eax,%eax */
  0x74, 0x20,                               /*295: je     2b7 <op_enter+0x2b7> */
  0xff, 0xc3,                               /*297: inc    %ebx */
  0x48, 0xc1, 0xe3, 0x04,                   /*299: shl    $0x4,%rbx */
  0x49, 0x8b, 0x6f, 0x18,                   /*29d: mov    0x18(%r15),%rbp */
  0x49, 0x8b, 0x7f, 0x58,                   /*2a1: mov    0x58(%r15),%rdi */
  0x31, 0xf6,                               /*2a5: xor    %esi,%esi */
  0x41, 0xff, 0x97, 0xe0, 0x00, 0x00, 0x00, /*2a7: callq  *0xe0(%r15) */
  0x48, 0x89, 0x44, 0x1d, 0x00,             /*2ae: mov    %rax,0x0(%rbp,%rbx,1) */
  0x89, 0x54, 0x1d, 0x08,                   /*2b3: mov    %edx,0x8(%rbp,%rbx,1) */
  0x48, 0x8b, 0x44, 0x24, 0x10,             /*2b7: mov    0x10(%rsp),%rax */
  0x85, 0xc0,                               /*2bc: test   %eax,%eax */
  0x49, 0x8b, 0x47, 0x08,                   /*2be: mov    0x8(%r15),%rax */
  0x48, 0x8b, 0x88, 0x98, 0x00, 0x00, 0x00, /*2c2: mov    0x98(%rax),%rcx */
  0x0f, 0x84, 0x68, 0x01, 0x00, 0x00,       /*2c9: je     437 <op_enter+0x437> */
  0x44, 0x3b, 0x34, 0x24,                   /*2cf: cmp    (%rsp),%r14d */
  0x0f, 0x8c, 0x5e, 0x01, 0x00, 0x00,       /*2d3: jl     437 <op_enter+0x437> */
  0xba, 0x01, 0x00, 0x00, 0x00,             /*2d9: mov    $0x1,%edx */
  0x48, 0x8b, 0x74, 0x24, 0x38,             /*2de: mov    0x38(%rsp),%rsi */
  0x29, 0xf2,                               /*2e3: sub    %esi,%edx */
  0x48, 0x8b, 0x74, 0x24, 0x08,             /*2e5: mov    0x8(%rsp),%rsi */
  0x29, 0xf2,                               /*2ea: sub    %esi,%edx */
  0x44, 0x01, 0xf2,                         /*2ec: add    %r14d,%edx */
  0x48, 0x63, 0xd2,                         /*2ef: movslq %edx,%rdx */
  0x48, 0x8d, 0x0c, 0x91,                   /*2f2: lea    (%rcx,%rdx,4),%rcx */
  0xe9, 0x40, 0x01, 0x00, 0x00,             /*2f6: jmpq   43b <op_enter+0x43b> */
  0x4c, 0x39, 0xc3,                         /*2fb: cmp    %r8,%rbx */
  0x48, 0x89, 0x1c, 0x24,                   /*2fe: mov    %rbx,(%rsp) */
  0x74, 0x40,                               /*302: je     344 <op_enter+0x344> */
  0x41, 0x8d, 0x45, 0x01,                   /*304: lea    0x1(%r13),%eax */
  0x49, 0x8b, 0x4f, 0x18,                   /*308: mov    0x18(%r15),%rcx */
  0x48, 0xc1, 0xe0, 0x04,                   /*30c: shl    $0x4,%rax */
  0x48, 0x8b, 0x74, 0x24, 0x30,             /*310: mov    0x30(%rsp),%rsi */
  0x48, 0x8b, 0x16,                         /*315: mov    (%rsi),%rdx */
  0x48, 0x8b, 0x76, 0x08,                   /*318: mov    0x8(%rsi),%rsi */
  0x48, 0x89, 0x74, 0x01, 0x08,             /*31c: mov    %rsi,0x8(%rcx,%rax,1) */
  0x48, 0x89, 0x14, 0x01,                   /*321: mov    %rdx,(%rcx,%rax,1) */
  0x49, 0x8b, 0x7f, 0x18,                   /*325: mov    0x18(%r15),%rdi */
  0x48, 0x83, 0xc7, 0x10,                   /*329: add    $0x10,%rdi */
  0x48, 0x8b, 0x44, 0x24, 0x18,             /*32d: mov    0x18(%rsp),%rax */
  0x89, 0xc2,                               /*332: mov    %eax,%edx */
  0x4c, 0x89, 0xc6,                         /*334: mov    %r8,%rsi */
  0x4c, 0x89, 0xc3,                         /*337: mov    %r8,%rbx */
  0x41, 0xff, 0x97, 0xb0, 0x00, 0x00, 0x00, /*33a: callq  *0xb0(%r15) */
  0x49, 0x89, 0xd8,                         /*341: mov    %rbx,%r8 */
  0x45, 0x31, 0xe4,                         /*344: xor    %r12d,%r12d */
  0x48, 0x8b, 0x44, 0x24, 0x28,             /*347: mov    0x28(%rsp),%rax */
  0x85, 0xc0,                               /*34c: test   %eax,%eax */
  0x74, 0x64,                               /*34e: je     3b4 <op_enter+0x3b4> */
  0x48, 0x8b, 0x44, 0x24, 0x18,             /*350: mov    0x18(%rsp),%rax */
  0x48, 0x89, 0xc1,                         /*355: mov    %rax,%rcx */
  0x8d, 0x04, 0x29,                         /*358: lea    (%rcx,%rbp,1),%eax */
  0x45, 0x89, 0xf4,                         /*35b: mov    %r14d,%r12d */
  0x41, 0x29, 0xc4,                         /*35e: sub    %eax,%r12d */
  0x8d, 0x59, 0x01,                         /*361: lea    0x1(%rcx),%ebx */
  0x48, 0xc1, 0xe3, 0x04,                   /*364: shl    $0x4,%rbx */
  0x49, 0x8b, 0x47, 0x18,                   /*368: mov    0x18(%r15),%rax */
  0x48, 0x89, 0x44, 0x24, 0x28,             /*36c: mov    %rax,0x28(%rsp) */
  0x49, 0x8b, 0x7f, 0x58,                   /*371: mov    0x58(%r15),%rdi */
  0x48, 0x8b, 0x44, 0x24, 0x38,             /*375: mov    0x38(%rsp),%rax */
  0x89, 0xc0,                               /*37a: mov    %eax,%eax */
  0x48, 0x8b, 0x4c, 0x24, 0x10,             /*37c: mov    0x10(%rsp),%rcx */
  0x89, 0xca,                               /*381: mov    %ecx,%edx */
  0x48, 0x01, 0xc2,                         /*383: add    %rax,%rdx */
  0x48, 0xc1, 0xe2, 0x04,                   /*386: shl    $0x4,%rdx */
  0x4c, 0x01, 0xc2,                         /*38a: add    %r8,%rdx */
  0x44, 0x89, 0xe6,                         /*38d: mov    %r12d,%esi */
  0x48, 0x89, 0x6c, 0x24, 0x08,             /*390: mov    %rbp,0x8(%rsp) */
  0x4c, 0x89, 0xc5,                         /*395: mov    %r8,%rbp */
  0x41, 0xff, 0x97, 0x50, 0x02, 0x00, 0x00, /*398: callq  *0x250(%r15) */
  0x49, 0x89, 0xe8,                         /*39f: mov    %rbp,%r8 */
  0x48, 0x8b, 0x6c, 0x24, 0x08,             /*3a2: mov    0x8(%rsp),%rbp */
  0x48, 0x8b, 0x4c, 0x24, 0x28,             /*3a7: mov    0x28(%rsp),%rcx */
  0x48, 0x89, 0x04, 0x19,                   /*3ac: mov    %rax,(%rcx,%rbx,1) */
  0x89, 0x54, 0x19, 0x08,                   /*3b0: mov    %edx,0x8(%rcx,%rbx,1) */
  0x85, 0xed,                               /*3b4: test   %ebp,%ebp */
  0x74, 0x3d,                               /*3b6: je     3f5 <op_enter+0x3f5> */
  0x41, 0x29, 0xee,                         /*3b8: sub    %ebp,%r14d */
  0x48, 0x8b, 0x44, 0x24, 0x38,             /*3bb: mov    0x38(%rsp),%rax */
  0x41, 0x39, 0xc6,                         /*3c0: cmp    %eax,%r14d */
  0x7e, 0x30,                               /*3c3: jle    3f5 <op_enter+0x3f5> */
  0x48, 0x8b, 0x7c, 0x24, 0x20,             /*3c5: mov    0x20(%rsp),%rdi */
  0xff, 0xc7,                               /*3ca: inc    %edi */
  0x48, 0xc1, 0xe7, 0x04,                   /*3cc: shl    $0x4,%rdi */
  0x49, 0x03, 0x7f, 0x18,                   /*3d0: add    0x18(%r15),%rdi */
  0x48, 0x8b, 0x44, 0x24, 0x18,             /*3d4: mov    0x18(%rsp),%rax */
  0x41, 0x01, 0xc4,                         /*3d9: add    %eax,%r12d */
  0x49, 0x63, 0xf4,                         /*3dc: movslq %r12d,%rsi */
  0x48, 0xc1, 0xe6, 0x04,                   /*3df: shl    $0x4,%rsi */
  0x4c, 0x01, 0xc6,                         /*3e3: add    %r8,%rsi */
  0x89, 0xea,                               /*3e6: mov    %ebp,%edx */
  0x4c, 0x89, 0xc3,                         /*3e8: mov    %r8,%rbx */
  0x41, 0xff, 0x97, 0xb0, 0x00, 0x00, 0x00, /*3eb: callq  *0xb0(%r15) */
  0x49, 0x89, 0xd8,                         /*3f2: mov    %rbx,%r8 */
  0x4c, 0x39, 0x04, 0x24,                   /*3f5: cmp    %r8,(%rsp) */
  0x75, 0x20,                               /*3f9: jne    41b <op_enter+0x41b> */
  0x41, 0xff, 0xc5,                         /*3fb: inc    %r13d */
  0x49, 0x8b, 0x47, 0x18,                   /*3fe: mov    0x18(%r15),%rax */
  0x49, 0xc1, 0xe5, 0x04,                   /*402: shl    $0x4,%r13 */
  0x48, 0x8b, 0x54, 0x24, 0x30,             /*406: mov    0x30(%rsp),%rdx */
  0x48, 0x8b, 0x0a,                         /*40b: mov    (%rdx),%rcx */
  0x48, 0x8b, 0x52, 0x08,                   /*40e: mov    0x8(%rdx),%rdx */
  0x4a, 0x89, 0x54, 0x28, 0x08,             /*412: mov    %rdx,0x8(%rax,%r13,1) */
  0x4a, 0x89, 0x0c, 0x28,                   /*417: mov    %rcx,(%rax,%r13,1) */
  0x48, 0x8b, 0x54, 0x24, 0x10,             /*41b: mov    0x10(%rsp),%rdx */
  0x85, 0xd2,                               /*420: test   %edx,%edx */
  0x49, 0x8b, 0x47, 0x08,                   /*422: mov    0x8(%r15),%rax */
  0x48, 0x8b, 0x88, 0x98, 0x00, 0x00, 0x00, /*426: mov    0x98(%rax),%rcx */
  0x74, 0x08,                               /*42d: je     437 <op_enter+0x437> */
  0xff, 0xc2,                               /*42f: inc    %edx */
  0x48, 0x8d, 0x0c, 0x91,                   /*431: lea    (%rcx,%rdx,4),%rcx */
  0xeb, 0x04,                               /*435: jmp    43b <op_enter+0x43b> */
  0x48, 0x83, 0xc1, 0x04,                   /*437: add    $0x4,%rcx */
  0x0f, 0xb7, 0x31,                         /*43b: movzwl (%rcx),%esi */
  0x48, 0x03, 0xb0, 0xa0, 0x00, 0x00, 0x00, /*43e: add    0xa0(%rax),%rsi */
  0x4c, 0x89, 0xff,                         /*445: mov    %r15,%rdi */
  0x4c, 0x89, 0xff,                         /*448: mov    %r15,%rdi */
  0x48, 0x83, 0xc4, 0x48,                   /*44b: add    $0x48,%rsp */
  0x5b,                                     /*44f: pop    %rbx */
  0x41, 0x5c,                               /*450: pop    %r12 */
  0x41, 0x5d,                               /*452: pop    %r13 */
  0x41, 0x5e,                               /*454: pop    %r14 */
  0x41, 0x5f,                               /*456: pop    %r15 */
  0x5d,                                     /*458: pop    %rbp */
  0xff, 0xe6,                               /*459: jmpq   *%rsi */

};

static void op_enter_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = a * 1 + 0;
}

static void op_enter_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_enter_set_args(op, GETARG_Ax(c),0,0,op_idx);
}


/* args: {} */
static uint8_t op_karg[] = {

};

static void op_karg_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_karg_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_karg_set_args(op, 0,0,0,op_idx);
}


/* args: {} */
static uint8_t op_kdict[] = {

};

static void op_kdict_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_kdict_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_kdict_set_args(op, 0,0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 17..20]], "b"=>[[1, 0, 22..25]]} */
static uint8_t op_return[] = {
  0x55,                                     /*0: push   %rbp */
  0x48, 0x89, 0xe5,                         /*1: mov    %rsp,%rbp */
  0x53,                                     /*4: push   %rbx */
  0x48, 0x83, 0xe4, 0xf0,                   /*5: and    $0xfffffffffffffff0,%rsp */
  0x48, 0x83, 0xec, 0x10,                   /*9: sub    $0x10,%rsp */
  0x48, 0x89, 0xfb,                         /*d: mov    %rdi,%rbx */
  0xbe, 0x00, 0x00, 0xab, 0x00,             /*10: mov    $0xab0000,%esi */
  0xba, 0x00, 0x00, 0xbc, 0x00,             /*15: mov    $0xbc0000,%edx */
  0xff, 0x93, 0x90, 0x00, 0x00, 0x00,       /*1a: callq  *0x90(%rbx) */
  0x48, 0x89, 0xdf,                         /*20: mov    %rbx,%rdi */
  0x48, 0x8d, 0x65, 0xf8,                   /*23: lea    -0x8(%rbp),%rsp */
  0x5b,                                     /*27: pop    %rbx */
  0x5d,                                     /*28: pop    %rbp */
  0xc3,                                     /*29: retq */

};

static void op_return_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 1 + 0;
  *((int32_t *)(op + 22)) = b * 1 + 0;
}

static void op_return_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_return_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 31..34]], "a"=>[[16, 0, 38..41], [16, 8, 50..53], [16, 32, 169..172], [16, 16, 176..179], [16, 16, 198..201], [16, 24, 208..211], [16, 0, 253..256], [1, 0, 319..322]], "c"=>[[1, 0, 99..102], [1, 1, 181..184], [1, 1, 213..216]]} */
static uint8_t op_tailcall[] = {
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
  0x8b, 0xa9, 0x00, 0x04, 0xbc, 0x00,       /*1d: mov    0xbc0400(%rcx),%ebp */
  0x48, 0x8b, 0xb0, 0x00, 0x10, 0xab, 0x00, /*23: mov    0xab1000(%rax),%rsi */
  0x48, 0x89, 0x74, 0x24, 0x08,             /*2a: mov    %rsi,0x8(%rsp) */
  0x44, 0x8b, 0xa8, 0x08, 0x10, 0xab, 0x00, /*2f: mov    0xab1008(%rax),%r13d */
  0x4c, 0x89, 0xf7,                         /*36: mov    %r14,%rdi */
  0x44, 0x89, 0xea,                         /*39: mov    %r13d,%edx */
  0xff, 0x93, 0x98, 0x02, 0x00, 0x00,       /*3c: callq  *0x298(%rbx) */
  0x48, 0x89, 0x44, 0x24, 0x10,             /*42: mov    %rax,0x10(%rsp) */
  0x48, 0x8b, 0x33,                         /*47: mov    (%rbx),%rsi */
  0x48, 0x8d, 0x54, 0x24, 0x10,             /*4a: lea    0x10(%rsp),%rdx */
  0x4c, 0x89, 0xf7,                         /*4f: mov    %r14,%rdi */
  0x89, 0xe9,                               /*52: mov    %ebp,%ecx */
  0xff, 0x93, 0x08, 0x02, 0x00, 0x00,       /*54: callq  *0x208(%rbx) */
  0x49, 0x89, 0xc7,                         /*5a: mov    %rax,%r15 */
  0x4d, 0x85, 0xff,                         /*5d: test   %r15,%r15 */
  0x74, 0x07,                               /*60: je     69 <op_tailcall+0x69> */
  0xba, 0x00, 0x00, 0xcd, 0x00,             /*62: mov    $0xcd0000,%edx */
  0xeb, 0x70,                               /*67: jmp    d9 <op_tailcall+0xd9> */
  0x89, 0xef,                               /*69: mov    %ebp,%edi */
  0xff, 0x93, 0x60, 0x01, 0x00, 0x00,       /*6b: callq  *0x160(%rbx) */
  0x49, 0x89, 0xc4,                         /*71: mov    %rax,%r12 */
  0x89, 0x54, 0x24, 0x04,                   /*74: mov    %edx,0x4(%rsp) */
  0x48, 0x8b, 0xb3, 0xb8, 0x02, 0x00, 0x00, /*78: mov    0x2b8(%rbx),%rsi */
  0xba, 0x0e, 0x00, 0x00, 0x00,             /*7f: mov    $0xe,%edx */
  0x4c, 0x89, 0xf7,                         /*84: mov    %r14,%rdi */
  0xff, 0x93, 0xa0, 0x02, 0x00, 0x00,       /*87: callq  *0x2a0(%rbx) */
  0x89, 0xc5,                               /*8d: mov    %eax,%ebp */
  0x48, 0x8d, 0x74, 0x24, 0x10,             /*8f: lea    0x10(%rsp),%rsi */
  0x4c, 0x89, 0xf7,                         /*94: mov    %r14,%rdi */
  0x89, 0xea,                               /*97: mov    %ebp,%edx */
  0xff, 0x93, 0xd8, 0x00, 0x00, 0x00,       /*99: callq  *0xd8(%rbx) */
  0x49, 0x89, 0xc7,                         /*9f: mov    %rax,%r15 */
  0x48, 0x8b, 0x73, 0x18,                   /*a2: mov    0x18(%rbx),%rsi */
  0x48, 0x8d, 0xbe, 0x20, 0x10, 0xab, 0x00, /*a6: lea    0xab1020(%rsi),%rdi */
  0x48, 0x81, 0xc6, 0x10, 0x10, 0xab, 0x00, /*ad: add    $0xab1010,%rsi */
  0xba, 0x01, 0x00, 0xcd, 0x00,             /*b4: mov    $0xcd0001,%edx */
  0xff, 0x93, 0xb0, 0x00, 0x00, 0x00,       /*b9: callq  *0xb0(%rbx) */
  0x48, 0x8b, 0x43, 0x18,                   /*bf: mov    0x18(%rbx),%rax */
  0x4c, 0x89, 0xa0, 0x10, 0x10, 0xab, 0x00, /*c3: mov    %r12,0xab1010(%rax) */
  0x8b, 0x4c, 0x24, 0x04,                   /*ca: mov    0x4(%rsp),%ecx */
  0x89, 0x88, 0x18, 0x10, 0xab, 0x00,       /*ce: mov    %ecx,0xab1018(%rax) */
  0xba, 0x01, 0x00, 0xcd, 0x00,             /*d4: mov    $0xcd0001,%edx */
  0x49, 0x8b, 0x46, 0x18,                   /*d9: mov    0x18(%r14),%rax */
  0x4c, 0x8b, 0x60, 0x20,                   /*dd: mov    0x20(%rax),%r12 */
  0x41, 0x89, 0x2c, 0x24,                   /*e1: mov    %ebp,(%r12) */
  0x48, 0x8b, 0x44, 0x24, 0x10,             /*e5: mov    0x10(%rsp),%rax */
  0x49, 0x89, 0x44, 0x24, 0x48,             /*ea: mov    %rax,0x48(%r12) */
  0x41, 0x89, 0x54, 0x24, 0x40,             /*ef: mov    %edx,0x40(%r12) */
  0x49, 0x8b, 0x46, 0x18,                   /*f4: mov    0x18(%r14),%rax */
  0x48, 0x8b, 0x78, 0x08,                   /*f8: mov    0x8(%rax),%rdi */
  0xbe, 0x00, 0x10, 0xab, 0x00,             /*fc: mov    $0xab1000,%esi */
  0x48, 0x03, 0x73, 0x18,                   /*101: add    0x18(%rbx),%rsi */
  0xff, 0xc2,                               /*105: inc    %edx */
  0xff, 0x93, 0xb0, 0x00, 0x00, 0x00,       /*107: callq  *0xb0(%rbx) */
  0x41, 0xf6, 0x47, 0x02, 0x04,             /*10d: testb  $0x4,0x2(%r15) */
  0x74, 0x3c,                               /*112: je     150 <op_tailcall+0x150> */
  0x49, 0x8b, 0x46, 0x18,                   /*114: mov    0x18(%r14),%rax */
  0x48, 0x8b, 0x68, 0x08,                   /*118: mov    0x8(%rax),%rbp */
  0x4c, 0x89, 0xf7,                         /*11c: mov    %r14,%rdi */
  0x48, 0x8b, 0x74, 0x24, 0x08,             /*11f: mov    0x8(%rsp),%rsi */
  0x44, 0x89, 0xea,                         /*124: mov    %r13d,%edx */
  0x41, 0xff, 0x57, 0x18,                   /*127: callq  *0x18(%r15) */
  0x48, 0x89, 0x45, 0x00,                   /*12b: mov    %rax,0x0(%rbp) */
  0x89, 0x55, 0x08,                         /*12f: mov    %edx,0x8(%rbp) */
  0x8b, 0x73, 0x50,                         /*132: mov    0x50(%rbx),%esi */
  0x4c, 0x89, 0xf7,                         /*135: mov    %r14,%rdi */
  0xff, 0x93, 0x18, 0x01, 0x00, 0x00,       /*138: callq  *0x118(%rbx) */
  0xbe, 0x00, 0x00, 0xab, 0x00,             /*13e: mov    $0xab0000,%esi */
  0x31, 0xd2,                               /*143: xor    %edx,%edx */
  0x48, 0x89, 0xdf,                         /*145: mov    %rbx,%rdi */
  0xff, 0x93, 0x90, 0x00, 0x00, 0x00,       /*148: callq  *0x90(%rbx) */
  0xeb, 0x6b,                               /*14e: jmp    1bb <op_tailcall+0x1bb> */
  0x49, 0x8b, 0x47, 0x18,                   /*150: mov    0x18(%r15),%rax */
  0x48, 0x89, 0x43, 0x08,                   /*154: mov    %rax,0x8(%rbx) */
  0x48, 0x8b, 0x48, 0x10,                   /*158: mov    0x10(%rax),%rcx */
  0x48, 0x89, 0x4b, 0x20,                   /*15c: mov    %rcx,0x20(%rbx) */
  0x48, 0x8b, 0x48, 0x18,                   /*160: mov    0x18(%rax),%rcx */
  0x48, 0x89, 0x4b, 0x28,                   /*164: mov    %rcx,0x28(%rbx) */
  0x41, 0x8b, 0x54, 0x24, 0x40,             /*168: mov    0x40(%r12),%edx */
  0x0f, 0xb7, 0x70, 0x02,                   /*16d: movzwl 0x2(%rax),%esi */
  0x85, 0xd2,                               /*171: test   %edx,%edx */
  0x78, 0x05,                               /*173: js     17a <op_tailcall+0x17a> */
  0x83, 0xc2, 0x02,                         /*175: add    $0x2,%edx */
  0xeb, 0x10,                               /*178: jmp    18a <op_tailcall+0x18a> */
  0x83, 0xfe, 0x03,                         /*17a: cmp    $0x3,%esi */
  0xb8, 0x03, 0x00, 0x00, 0x00,             /*17d: mov    $0x3,%eax */
  0x0f, 0x42, 0xf0,                         /*182: cmovb  %eax,%esi */
  0xba, 0x03, 0x00, 0x00, 0x00,             /*185: mov    $0x3,%edx */
  0x4c, 0x89, 0xf7,                         /*18a: mov    %r14,%rdi */
  0xff, 0x93, 0x50, 0x01, 0x00, 0x00,       /*18d: callq  *0x150(%rbx) */
  0x49, 0x8b, 0x46, 0x18,                   /*193: mov    0x18(%r14),%rax */
  0x48, 0x8b, 0x40, 0x08,                   /*197: mov    0x8(%rax),%rax */
  0x48, 0x89, 0x43, 0x18,                   /*19b: mov    %rax,0x18(%rbx) */
  0x48, 0x8b, 0x43, 0x08,                   /*19f: mov    0x8(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x08,                   /*1a3: mov    0x8(%rax),%rax */
  0x48, 0x89, 0x43, 0x10,                   /*1a7: mov    %rax,0x10(%rbx) */
  0x48, 0x8b, 0x7b, 0x58,                   /*1ab: mov    0x58(%rbx),%rdi */
  0x4c, 0x89, 0xfe,                         /*1af: mov    %r15,%rsi */
  0x48, 0x89, 0xda,                         /*1b2: mov    %rbx,%rdx */
  0xff, 0x93, 0x28, 0x01, 0x00, 0x00,       /*1b5: callq  *0x128(%rbx) */
  0x48, 0x89, 0xdf,                         /*1bb: mov    %rbx,%rdi */
  0x48, 0x83, 0xc4, 0x18,                   /*1be: add    $0x18,%rsp */
  0x5b,                                     /*1c2: pop    %rbx */
  0x41, 0x5c,                               /*1c3: pop    %r12 */
  0x41, 0x5d,                               /*1c5: pop    %r13 */
  0x41, 0x5e,                               /*1c7: pop    %r14 */
  0x41, 0x5f,                               /*1c9: pop    %r15 */
  0x5d,                                     /*1cb: pop    %rbp */

};

static void op_tailcall_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 31)) = b * 4 + 0;
  *((int32_t *)(op + 38)) = a * 16 + 0;
  *((int32_t *)(op + 50)) = a * 16 + 8;
  *((int32_t *)(op + 169)) = a * 16 + 32;
  *((int32_t *)(op + 176)) = a * 16 + 16;
  *((int32_t *)(op + 198)) = a * 16 + 16;
  *((int32_t *)(op + 208)) = a * 16 + 24;
  *((int32_t *)(op + 253)) = a * 16 + 0;
  *((int32_t *)(op + 319)) = a * 1 + 0;
  *((int32_t *)(op + 99)) = c * 1 + 0;
  *((int32_t *)(op + 181)) = c * 1 + 1;
  *((int32_t *)(op + 213)) = c * 1 + 1;
}

static void op_tailcall_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_tailcall_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[1, 0, 23..26]], "a"=>[[16, 8, 136..139], [16, 0, 143..146]]} */
static uint8_t op_blkpush[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x57,                               /*1: push   %r15 */
  0x41, 0x56,                               /*3: push   %r14 */
  0x41, 0x54,                               /*5: push   %r12 */
  0x53,                                     /*7: push   %rbx */
  0x48, 0x83, 0xec, 0x10,                   /*8: sub    $0x10,%rsp */
  0x49, 0x89, 0xff,                         /*c: mov    %rdi,%r15 */
  0x4d, 0x8b, 0x77, 0x58,                   /*f: mov    0x58(%r15),%r14 */
  0xc7, 0x44, 0x24, 0x0c, 0x00, 0x00, 0xbc, 0x00,/*13: movl   $0xbc0000,0xc(%rsp) */
  0x8b, 0x6c, 0x24, 0x0c,                   /*1b: mov    0xc(%rsp),%ebp */
  0x8b, 0x5c, 0x24, 0x0c,                   /*1f: mov    0xc(%rsp),%ebx */
  0x44, 0x8b, 0x64, 0x24, 0x0c,             /*23: mov    0xc(%rsp),%r12d */
  0x8b, 0x74, 0x24, 0x0c,                   /*28: mov    0xc(%rsp),%esi */
  0x83, 0xe6, 0x0f,                         /*2c: and    $0xf,%esi */
  0x74, 0x1c,                               /*2f: je     4d <op_blkpush+0x4d> */
  0x49, 0x8b, 0x7f, 0x58,                   /*31: mov    0x58(%r15),%rdi */
  0xff, 0xce,                               /*35: dec    %esi */
  0x41, 0xff, 0x97, 0x90, 0x02, 0x00, 0x00, /*37: callq  *0x290(%r15) */
  0x48, 0x85, 0xc0,                         /*3e: test   %rax,%rax */
  0x74, 0x52,                               /*41: je     95 <op_blkpush+0x95> */
  0x48, 0x83, 0xc0, 0x18,                   /*43: add    $0x18,%rax */
  0x49, 0x8d, 0x4f, 0x18,                   /*47: lea    0x18(%r15),%rcx */
  0xeb, 0x07,                               /*4b: jmp    54 <op_blkpush+0x54> */
  0x49, 0x8d, 0x4f, 0x18,                   /*4d: lea    0x18(%r15),%rcx */
  0x48, 0x89, 0xc8,                         /*51: mov    %rcx,%rax */
  0xc1, 0xed, 0x0a,                         /*54: shr    $0xa,%ebp */
  0x83, 0xe5, 0x3f,                         /*57: and    $0x3f,%ebp */
  0xc1, 0xeb, 0x09,                         /*5a: shr    $0x9,%ebx */
  0x83, 0xe3, 0x01,                         /*5d: and    $0x1,%ebx */
  0x41, 0xc1, 0xec, 0x04,                   /*60: shr    $0x4,%r12d */
  0x41, 0x83, 0xe4, 0x1f,                   /*64: and    $0x1f,%r12d */
  0x48, 0x8b, 0x00,                         /*68: mov    (%rax),%rax */
  0x48, 0x8b, 0x09,                         /*6b: mov    (%rcx),%rcx */
  0x01, 0xeb,                               /*6e: add    %ebp,%ebx */
  0x41, 0x8d, 0x54, 0x1c, 0x01,             /*70: lea    0x1(%r12,%rbx,1),%edx */
  0x48, 0x63, 0xd2,                         /*75: movslq %edx,%rdx */
  0x48, 0xc1, 0xe2, 0x04,                   /*78: shl    $0x4,%rdx */
  0x48, 0x8b, 0x34, 0x10,                   /*7c: mov    (%rax,%rdx,1),%rsi */
  0x48, 0x8b, 0x44, 0x10, 0x08,             /*80: mov    0x8(%rax,%rdx,1),%rax */
  0x48, 0x89, 0x81, 0x08, 0x10, 0xab, 0x00, /*85: mov    %rax,0xab1008(%rcx) */
  0x48, 0x89, 0xb1, 0x00, 0x10, 0xab, 0x00, /*8c: mov    %rsi,0xab1000(%rcx) */
  0xeb, 0x19,                               /*93: jmp    ae <op_blkpush+0xae> */
  0xbe, 0x02, 0x00, 0x00, 0x00,             /*95: mov    $0x2,%esi */
  0x4c, 0x89, 0xf7,                         /*9a: mov    %r14,%rdi */
  0x41, 0xff, 0x97, 0xc8, 0x01, 0x00, 0x00, /*9d: callq  *0x1c8(%r15) */
  0x4c, 0x89, 0xff,                         /*a4: mov    %r15,%rdi */
  0x41, 0xff, 0x97, 0xd0, 0x01, 0x00, 0x00, /*a7: callq  *0x1d0(%r15) */
  0x4c, 0x89, 0xff,                         /*ae: mov    %r15,%rdi */
  0x48, 0x83, 0xc4, 0x10,                   /*b1: add    $0x10,%rsp */
  0x5b,                                     /*b5: pop    %rbx */
  0x41, 0x5c,                               /*b6: pop    %r12 */
  0x41, 0x5e,                               /*b8: pop    %r14 */
  0x41, 0x5f,                               /*ba: pop    %r15 */
  0x5d,                                     /*bc: pop    %rbp */

};

static void op_blkpush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 23)) = b * 1 + 0;
  *((int32_t *)(op + 136)) = a * 16 + 8;
  *((int32_t *)(op + 143)) = a * 16 + 0;
}

static void op_blkpush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_blkpush_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 8, 25..28], [16, 24, 32..35], [16, 0, 85..88], [16, 16, 92..95], [16, 8, 123..126], [16, 0, 149..152], [16, 0, 172..175], [16, 16, 179..182], [16, 0, 195..198], [16, 8, 201..204], [16, 16, 221..224], [16, 0, 229..232], [16, 0, 237..240], [16, 0, 254..257], [16, 16, 262..265], [16, 8, 268..271], [16, 0, 280..283], [16, 0, 297..300], [16, 16, 305..308], [16, 0, 313..316], [16, 8, 332..335], [16, 0, 346..349]]} */
static uint8_t op_add[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x57,                               /*1: push   %r15 */
  0x41, 0x56,                               /*3: push   %r14 */
  0x41, 0x54,                               /*5: push   %r12 */
  0x53,                                     /*7: push   %rbx */
  0x48, 0x83, 0xec, 0x10,                   /*8: sub    $0x10,%rsp */
  0x48, 0x89, 0xfb,                         /*c: mov    %rdi,%rbx */
  0x48, 0x8b, 0x6b, 0x18,                   /*f: mov    0x18(%rbx),%rbp */
  0x4c, 0x8b, 0x73, 0x58,                   /*13: mov    0x58(%rbx),%r14 */
  0x8b, 0x95, 0x08, 0x10, 0xab, 0x00,       /*17: mov    0xab1008(%rbp),%edx */
  0x44, 0x8b, 0x85, 0x18, 0x10, 0xab, 0x00, /*1d: mov    0xab1018(%rbp),%r8d */
  0x89, 0xd1,                               /*24: mov    %edx,%ecx */
  0xc1, 0xe1, 0x08,                         /*26: shl    $0x8,%ecx */
  0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*29: and    $0xffff00,%ecx */
  0x41, 0x0f, 0xb6, 0xc0,                   /*2f: movzbl %r8b,%eax */
  0x09, 0xc8,                               /*33: or     %ecx,%eax */
  0x3d, 0x0f, 0x10, 0x00, 0x00,             /*35: cmp    $0x100f,%eax */
  0x7f, 0x62,                               /*3a: jg     9e <op_add+0x9e> */
  0x3d, 0x02, 0x06, 0x00, 0x00,             /*3c: cmp    $0x602,%eax */
  0x0f, 0x8f, 0x8b, 0x00, 0x00, 0x00,       /*41: jg     d2 <op_add+0xd2> */
  0x3d, 0x03, 0x03, 0x00, 0x00,             /*47: cmp    $0x303,%eax */
  0x0f, 0x85, 0xa1, 0x00, 0x00, 0x00,       /*4c: jne    f3 <op_add+0xf3> */
  0x44, 0x8b, 0xa5, 0x00, 0x10, 0xab, 0x00, /*52: mov    0xab1000(%rbp),%r12d */
  0x44, 0x8b, 0xbd, 0x10, 0x10, 0xab, 0x00, /*59: mov    0xab1010(%rbp),%r15d */
  0x48, 0x8d, 0x54, 0x24, 0x0c,             /*60: lea    0xc(%rsp),%rdx */
  0x44, 0x89, 0xe7,                         /*65: mov    %r12d,%edi */
  0x44, 0x89, 0xfe,                         /*68: mov    %r15d,%esi */
  0xff, 0x93, 0xa0, 0x01, 0x00, 0x00,       /*6b: callq  *0x1a0(%rbx) */
  0x84, 0xc0,                               /*71: test   %al,%al */
  0x0f, 0x84, 0xd1, 0x00, 0x00, 0x00,       /*73: je     14a <op_add+0x14a> */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*79: movl   $0x6,0xab1008(%rbp) */
  0xf2, 0x41, 0x0f, 0x2a, 0xc4,             /*83: cvtsi2sd %r12d,%xmm0 */
  0xf2, 0x41, 0x0f, 0x2a, 0xcf,             /*88: cvtsi2sd %r15d,%xmm1 */
  0xf2, 0x0f, 0x58, 0xc8,                   /*8d: addsd  %xmm0,%xmm1 */
  0xf2, 0x0f, 0x11, 0x8d, 0x00, 0x10, 0xab, 0x00,/*91: movsd  %xmm1,0xab1000(%rbp) */
  0xe9, 0xc0, 0x00, 0x00, 0x00,             /*99: jmpq   15e <op_add+0x15e> */
  0x3d, 0x10, 0x10, 0x00, 0x00,             /*9e: cmp    $0x1010,%eax */
  0x0f, 0x85, 0x96, 0x00, 0x00, 0x00,       /*a3: jne    13f <op_add+0x13f> */
  0x48, 0x8b, 0xb5, 0x00, 0x10, 0xab, 0x00, /*a9: mov    0xab1000(%rbp),%rsi */
  0x48, 0x8b, 0x8d, 0x10, 0x10, 0xab, 0x00, /*b0: mov    0xab1010(%rbp),%rcx */
  0x4c, 0x89, 0xf7,                         /*b7: mov    %r14,%rdi */
  0xff, 0x93, 0x48, 0x01, 0x00, 0x00,       /*ba: callq  *0x148(%rbx) */
  0x48, 0x89, 0x85, 0x00, 0x10, 0xab, 0x00, /*c0: mov    %rax,0xab1000(%rbp) */
  0x89, 0x95, 0x08, 0x10, 0xab, 0x00,       /*c7: mov    %edx,0xab1008(%rbp) */
  0xe9, 0x8c, 0x00, 0x00, 0x00,             /*cd: jmpq   15e <op_add+0x15e> */
  0x3d, 0x03, 0x06, 0x00, 0x00,             /*d2: cmp    $0x603,%eax */
  0x75, 0x45,                               /*d7: jne    11e <op_add+0x11e> */
  0xf2, 0x0f, 0x2a, 0x85, 0x10, 0x10, 0xab, 0x00,/*d9: cvtsi2sdl 0xab1010(%rbp),%xmm0 */
  0xf2, 0x0f, 0x58, 0x85, 0x00, 0x10, 0xab, 0x00,/*e1: addsd  0xab1000(%rbp),%xmm0 */
  0xf2, 0x0f, 0x11, 0x85, 0x00, 0x10, 0xab, 0x00,/*e9: movsd  %xmm0,0xab1000(%rbp) */
  0xeb, 0x6b,                               /*f1: jmp    15e <op_add+0x15e> */
  0x3d, 0x06, 0x03, 0x00, 0x00,             /*f3: cmp    $0x306,%eax */
  0x75, 0x45,                               /*f8: jne    13f <op_add+0x13f> */
  0xf2, 0x0f, 0x2a, 0x85, 0x00, 0x10, 0xab, 0x00,/*fa: cvtsi2sdl 0xab1000(%rbp),%xmm0 */
  0xf2, 0x0f, 0x58, 0x85, 0x10, 0x10, 0xab, 0x00,/*102: addsd  0xab1010(%rbp),%xmm0 */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*10a: movl   $0x6,0xab1008(%rbp) */
  0xf2, 0x0f, 0x11, 0x85, 0x00, 0x10, 0xab, 0x00,/*114: movsd  %xmm0,0xab1000(%rbp) */
  0xeb, 0x40,                               /*11c: jmp    15e <op_add+0x15e> */
  0x3d, 0x06, 0x06, 0x00, 0x00,             /*11e: cmp    $0x606,%eax */
  0x75, 0x1a,                               /*123: jne    13f <op_add+0x13f> */
  0xf2, 0x0f, 0x10, 0x85, 0x00, 0x10, 0xab, 0x00,/*125: movsd  0xab1000(%rbp),%xmm0 */
  0xf2, 0x0f, 0x58, 0x85, 0x10, 0x10, 0xab, 0x00,/*12d: addsd  0xab1010(%rbp),%xmm0 */
  0xf2, 0x0f, 0x11, 0x85, 0x00, 0x10, 0xab, 0x00,/*135: movsd  %xmm0,0xab1000(%rbp) */
  0xeb, 0x1f,                               /*13d: jmp    15e <op_add+0x15e> */
  0x48, 0x89, 0xdf,                         /*13f: mov    %rbx,%rdi */
  0xff, 0x93, 0x68, 0x02, 0x00, 0x00,       /*142: callq  *0x268(%rbx) */
  0xeb, 0x1e,                               /*148: jmp    168 <op_add+0x168> */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*14a: movl   $0x3,0xab1008(%rbp) */
  0x8b, 0x44, 0x24, 0x0c,                   /*154: mov    0xc(%rsp),%eax */
  0x89, 0x85, 0x00, 0x10, 0xab, 0x00,       /*158: mov    %eax,0xab1000(%rbp) */
  0x8b, 0x43, 0x50,                         /*15e: mov    0x50(%rbx),%eax */
  0x41, 0x89, 0x86, 0xdc, 0x00, 0x00, 0x00, /*161: mov    %eax,0xdc(%r14) */
  0x48, 0x89, 0xdf,                         /*168: mov    %rbx,%rdi */
  0x48, 0x83, 0xc4, 0x10,                   /*16b: add    $0x10,%rsp */
  0x5b,                                     /*16f: pop    %rbx */
  0x41, 0x5c,                               /*170: pop    %r12 */
  0x41, 0x5e,                               /*172: pop    %r14 */
  0x41, 0x5f,                               /*174: pop    %r15 */
  0x5d,                                     /*176: pop    %rbp */

};

static void op_add_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 25)) = a * 16 + 8;
  *((int32_t *)(op + 32)) = a * 16 + 24;
  *((int32_t *)(op + 85)) = a * 16 + 0;
  *((int32_t *)(op + 92)) = a * 16 + 16;
  *((int32_t *)(op + 123)) = a * 16 + 8;
  *((int32_t *)(op + 149)) = a * 16 + 0;
  *((int32_t *)(op + 172)) = a * 16 + 0;
  *((int32_t *)(op + 179)) = a * 16 + 16;
  *((int32_t *)(op + 195)) = a * 16 + 0;
  *((int32_t *)(op + 201)) = a * 16 + 8;
  *((int32_t *)(op + 221)) = a * 16 + 16;
  *((int32_t *)(op + 229)) = a * 16 + 0;
  *((int32_t *)(op + 237)) = a * 16 + 0;
  *((int32_t *)(op + 254)) = a * 16 + 0;
  *((int32_t *)(op + 262)) = a * 16 + 16;
  *((int32_t *)(op + 268)) = a * 16 + 8;
  *((int32_t *)(op + 280)) = a * 16 + 0;
  *((int32_t *)(op + 297)) = a * 16 + 0;
  *((int32_t *)(op + 305)) = a * 16 + 16;
  *((int32_t *)(op + 313)) = a * 16 + 0;
  *((int32_t *)(op + 332)) = a * 16 + 8;
  *((int32_t *)(op + 346)) = a * 16 + 0;
}

static void op_add_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_add_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 17..20], [16, 0, 30..33], [16, 0, 44..47], [16, 8, 73..76], [16, 0, 98..101], [16, 24, 106..109], [16, 16, 116..119], [1, 0, 130..133], [16, 8, 158..161], [16, 0, 172..175]], "c"=>[[1, 0, 54..57], [1, 0, 120..123]], "b"=>[[1, 0, 135..138]]} */
static uint8_t op_addi[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x56,                               /*1: push   %r14 */
  0x53,                                     /*3: push   %rbx */
  0x48, 0x83, 0xec, 0x10,                   /*4: sub    $0x10,%rsp */
  0x48, 0x89, 0xfb,                         /*8: mov    %rdi,%rbx */
  0x48, 0x8b, 0x6b, 0x18,                   /*b: mov    0x18(%rbx),%rbp */
  0x8b, 0x85, 0x08, 0x10, 0xab, 0x00,       /*f: mov    0xab1008(%rbp),%eax */
  0x83, 0xf8, 0x06,                         /*15: cmp    $0x6,%eax */
  0x75, 0x0a,                               /*18: jne    24 <op_addi+0x24> */
  0xf2, 0x0f, 0x10, 0x85, 0x00, 0x10, 0xab, 0x00,/*1a: movsd  0xab1000(%rbp),%xmm0 */
  0xeb, 0x32,                               /*22: jmp    56 <op_addi+0x56> */
  0x83, 0xf8, 0x03,                         /*24: cmp    $0x3,%eax */
  0x75, 0x3f,                               /*27: jne    68 <op_addi+0x68> */
  0x44, 0x8b, 0xb5, 0x00, 0x10, 0xab, 0x00, /*29: mov    0xab1000(%rbp),%r14d */
  0x48, 0x8d, 0x54, 0x24, 0x0c,             /*30: lea    0xc(%rsp),%rdx */
  0xbe, 0x00, 0x00, 0xcd, 0x00,             /*35: mov    $0xcd0000,%esi */
  0x44, 0x89, 0xf7,                         /*3a: mov    %r14d,%edi */
  0xff, 0x93, 0xa0, 0x01, 0x00, 0x00,       /*3d: callq  *0x1a0(%rbx) */
  0x84, 0xc0,                               /*43: test   %al,%al */
  0x74, 0x55,                               /*45: je     9c <op_addi+0x9c> */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*47: movl   $0x6,0xab1008(%rbp) */
  0xf2, 0x41, 0x0f, 0x2a, 0xc6,             /*51: cvtsi2sd %r14d,%xmm0 */
  0xf2, 0x0f, 0x58, 0x05, 0x00, 0x00, 0x00, 0x00,/*56: addsd  0x0(%rip),%xmm0        # 5e <op_addi+0x5e> */
  0xf2, 0x0f, 0x11, 0x85, 0x00, 0x10, 0xab, 0x00,/*5e: movsd  %xmm0,0xab1000(%rbp) */
  0xeb, 0x48,                               /*66: jmp    b0 <op_addi+0xb0> */
  0xc7, 0x85, 0x18, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*68: movl   $0x3,0xab1018(%rbp) */
  0xc7, 0x85, 0x10, 0x10, 0xab, 0x00, 0x00, 0x00, 0xcd, 0x00,/*72: movl   $0xcd0000,0xab1010(%rbp) */
  0xbe, 0x20, 0x00, 0x00, 0x00,             /*7c: mov    $0x20,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*81: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*86: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x01, 0x00, 0x00, 0x00,       /*8b: mov    $0x1,%r8d */
  0x48, 0x89, 0xdf,                         /*91: mov    %rbx,%rdi */
  0xff, 0x93, 0xf8, 0x01, 0x00, 0x00,       /*94: callq  *0x1f8(%rbx) */
  0xeb, 0x14,                               /*9a: jmp    b0 <op_addi+0xb0> */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*9c: movl   $0x3,0xab1008(%rbp) */
  0x8b, 0x44, 0x24, 0x0c,                   /*a6: mov    0xc(%rsp),%eax */
  0x89, 0x85, 0x00, 0x10, 0xab, 0x00,       /*aa: mov    %eax,0xab1000(%rbp) */
  0x48, 0x89, 0xdf,                         /*b0: mov    %rbx,%rdi */
  0x48, 0x83, 0xc4, 0x10,                   /*b3: add    $0x10,%rsp */
  0x5b,                                     /*b7: pop    %rbx */
  0x41, 0x5e,                               /*b8: pop    %r14 */
  0x5d,                                     /*ba: pop    %rbp */

};

static void op_addi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 16 + 8;
  *((int32_t *)(op + 30)) = a * 16 + 0;
  *((int32_t *)(op + 44)) = a * 16 + 0;
  *((int32_t *)(op + 73)) = a * 16 + 8;
  *((int32_t *)(op + 98)) = a * 16 + 0;
  *((int32_t *)(op + 106)) = a * 16 + 24;
  *((int32_t *)(op + 116)) = a * 16 + 16;
  *((int32_t *)(op + 130)) = a * 1 + 0;
  *((int32_t *)(op + 158)) = a * 16 + 8;
  *((int32_t *)(op + 172)) = a * 16 + 0;
  *((int32_t *)(op + 54)) = c * 1 + 0;
  *((int32_t *)(op + 120)) = c * 1 + 0;
  *((int32_t *)(op + 135)) = b * 1 + 0;
}

static void op_addi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_addi_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 15..18], [16, 24, 31..34], [16, 0, 54..57], [16, 16, 61..64], [16, 8, 92..95], [16, 0, 123..126], [16, 16, 131..134], [16, 0, 143..146], [16, 0, 160..163], [16, 16, 168..171], [16, 8, 174..177], [16, 0, 186..189], [16, 0, 203..206], [16, 16, 211..214], [16, 0, 219..222], [16, 8, 238..241], [16, 0, 252..255]]} */
static uint8_t op_sub[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x57,                               /*1: push   %r15 */
  0x41, 0x56,                               /*3: push   %r14 */
  0x53,                                     /*5: push   %rbx */
  0x48, 0x89, 0xfb,                         /*6: mov    %rdi,%rbx */
  0x48, 0x8b, 0x6b, 0x18,                   /*9: mov    0x18(%rbx),%rbp */
  0x8b, 0x8d, 0x08, 0x10, 0xab, 0x00,       /*d: mov    0xab1008(%rbp),%ecx */
  0xc1, 0xe1, 0x08,                         /*13: shl    $0x8,%ecx */
  0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*16: and    $0xffff00,%ecx */
  0x0f, 0xb6, 0x85, 0x18, 0x10, 0xab, 0x00, /*1c: movzbl 0xab1018(%rbp),%eax */
  0x09, 0xc8,                               /*23: or     %ecx,%eax */
  0x3d, 0x02, 0x06, 0x00, 0x00,             /*25: cmp    $0x602,%eax */
  0x7f, 0x44,                               /*2a: jg     70 <op_sub+0x70> */
  0x3d, 0x03, 0x03, 0x00, 0x00,             /*2c: cmp    $0x303,%eax */
  0x75, 0x62,                               /*31: jne    95 <op_sub+0x95> */
  0x44, 0x8b, 0xbd, 0x00, 0x10, 0xab, 0x00, /*33: mov    0xab1000(%rbp),%r15d */
  0x44, 0x8b, 0xb5, 0x10, 0x10, 0xab, 0x00, /*3a: mov    0xab1010(%rbp),%r14d */
  0x48, 0x8d, 0x54, 0x24, 0x04,             /*41: lea    0x4(%rsp),%rdx */
  0x44, 0x89, 0xff,                         /*46: mov    %r15d,%edi */
  0x44, 0x89, 0xf6,                         /*49: mov    %r14d,%esi */
  0xff, 0x93, 0x78, 0x01, 0x00, 0x00,       /*4c: callq  *0x178(%rbx) */
  0x84, 0xc0,                               /*52: test   %al,%al */
  0x0f, 0x84, 0x92, 0x00, 0x00, 0x00,       /*54: je     ec <op_sub+0xec> */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*5a: movl   $0x6,0xab1008(%rbp) */
  0xf2, 0x41, 0x0f, 0x2a, 0xc7,             /*64: cvtsi2sd %r15d,%xmm0 */
  0xf2, 0x41, 0x0f, 0x2a, 0xce,             /*69: cvtsi2sd %r14d,%xmm1 */
  0xeb, 0x17,                               /*6e: jmp    87 <op_sub+0x87> */
  0x3d, 0x03, 0x06, 0x00, 0x00,             /*70: cmp    $0x603,%eax */
  0x75, 0x49,                               /*75: jne    c0 <op_sub+0xc0> */
  0xf2, 0x0f, 0x10, 0x85, 0x00, 0x10, 0xab, 0x00,/*77: movsd  0xab1000(%rbp),%xmm0 */
  0xf2, 0x0f, 0x2a, 0x8d, 0x10, 0x10, 0xab, 0x00,/*7f: cvtsi2sdl 0xab1010(%rbp),%xmm1 */
  0xf2, 0x0f, 0x5c, 0xc1,                   /*87: subsd  %xmm1,%xmm0 */
  0xf2, 0x0f, 0x11, 0x85, 0x00, 0x10, 0xab, 0x00,/*8b: movsd  %xmm0,0xab1000(%rbp) */
  0xeb, 0x6b,                               /*93: jmp    100 <op_sub+0x100> */
  0x3d, 0x06, 0x03, 0x00, 0x00,             /*95: cmp    $0x306,%eax */
  0x75, 0x45,                               /*9a: jne    e1 <op_sub+0xe1> */
  0xf2, 0x0f, 0x2a, 0x85, 0x00, 0x10, 0xab, 0x00,/*9c: cvtsi2sdl 0xab1000(%rbp),%xmm0 */
  0xf2, 0x0f, 0x5c, 0x85, 0x10, 0x10, 0xab, 0x00,/*a4: subsd  0xab1010(%rbp),%xmm0 */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*ac: movl   $0x6,0xab1008(%rbp) */
  0xf2, 0x0f, 0x11, 0x85, 0x00, 0x10, 0xab, 0x00,/*b6: movsd  %xmm0,0xab1000(%rbp) */
  0xeb, 0x40,                               /*be: jmp    100 <op_sub+0x100> */
  0x3d, 0x06, 0x06, 0x00, 0x00,             /*c0: cmp    $0x606,%eax */
  0x75, 0x1a,                               /*c5: jne    e1 <op_sub+0xe1> */
  0xf2, 0x0f, 0x10, 0x85, 0x00, 0x10, 0xab, 0x00,/*c7: movsd  0xab1000(%rbp),%xmm0 */
  0xf2, 0x0f, 0x5c, 0x85, 0x10, 0x10, 0xab, 0x00,/*cf: subsd  0xab1010(%rbp),%xmm0 */
  0xf2, 0x0f, 0x11, 0x85, 0x00, 0x10, 0xab, 0x00,/*d7: movsd  %xmm0,0xab1000(%rbp) */
  0xeb, 0x1f,                               /*df: jmp    100 <op_sub+0x100> */
  0x48, 0x89, 0xdf,                         /*e1: mov    %rbx,%rdi */
  0xff, 0x93, 0x68, 0x02, 0x00, 0x00,       /*e4: callq  *0x268(%rbx) */
  0xeb, 0x14,                               /*ea: jmp    100 <op_sub+0x100> */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*ec: movl   $0x3,0xab1008(%rbp) */
  0x8b, 0x44, 0x24, 0x04,                   /*f6: mov    0x4(%rsp),%eax */
  0x89, 0x85, 0x00, 0x10, 0xab, 0x00,       /*fa: mov    %eax,0xab1000(%rbp) */
  0x48, 0x89, 0xdf,                         /*100: mov    %rbx,%rdi */
  0x5b,                                     /*103: pop    %rbx */
  0x41, 0x5e,                               /*104: pop    %r14 */
  0x41, 0x5f,                               /*106: pop    %r15 */
  0x5d,                                     /*108: pop    %rbp */

};

static void op_sub_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 16 + 8;
  *((int32_t *)(op + 31)) = a * 16 + 24;
  *((int32_t *)(op + 54)) = a * 16 + 0;
  *((int32_t *)(op + 61)) = a * 16 + 16;
  *((int32_t *)(op + 92)) = a * 16 + 8;
  *((int32_t *)(op + 123)) = a * 16 + 0;
  *((int32_t *)(op + 131)) = a * 16 + 16;
  *((int32_t *)(op + 143)) = a * 16 + 0;
  *((int32_t *)(op + 160)) = a * 16 + 0;
  *((int32_t *)(op + 168)) = a * 16 + 16;
  *((int32_t *)(op + 174)) = a * 16 + 8;
  *((int32_t *)(op + 186)) = a * 16 + 0;
  *((int32_t *)(op + 203)) = a * 16 + 0;
  *((int32_t *)(op + 211)) = a * 16 + 16;
  *((int32_t *)(op + 219)) = a * 16 + 0;
  *((int32_t *)(op + 238)) = a * 16 + 8;
  *((int32_t *)(op + 252)) = a * 16 + 0;
}

static void op_sub_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sub_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 17..20], [16, 0, 30..33], [16, 0, 44..47], [16, 8, 73..76], [16, 0, 98..101], [16, 24, 106..109], [16, 16, 116..119], [1, 0, 130..133], [16, 8, 158..161], [16, 0, 172..175]], "c"=>[[1, 0, 54..57], [1, 0, 120..123]], "b"=>[[1, 0, 135..138]]} */
static uint8_t op_subi[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x56,                               /*1: push   %r14 */
  0x53,                                     /*3: push   %rbx */
  0x48, 0x83, 0xec, 0x10,                   /*4: sub    $0x10,%rsp */
  0x48, 0x89, 0xfb,                         /*8: mov    %rdi,%rbx */
  0x48, 0x8b, 0x6b, 0x18,                   /*b: mov    0x18(%rbx),%rbp */
  0x8b, 0x85, 0x08, 0x10, 0xab, 0x00,       /*f: mov    0xab1008(%rbp),%eax */
  0x83, 0xf8, 0x06,                         /*15: cmp    $0x6,%eax */
  0x75, 0x0a,                               /*18: jne    24 <op_subi+0x24> */
  0xf2, 0x0f, 0x10, 0x85, 0x00, 0x10, 0xab, 0x00,/*1a: movsd  0xab1000(%rbp),%xmm0 */
  0xeb, 0x32,                               /*22: jmp    56 <op_subi+0x56> */
  0x83, 0xf8, 0x03,                         /*24: cmp    $0x3,%eax */
  0x75, 0x3f,                               /*27: jne    68 <op_subi+0x68> */
  0x44, 0x8b, 0xb5, 0x00, 0x10, 0xab, 0x00, /*29: mov    0xab1000(%rbp),%r14d */
  0x48, 0x8d, 0x54, 0x24, 0x0c,             /*30: lea    0xc(%rsp),%rdx */
  0xbe, 0x00, 0x00, 0xcd, 0x00,             /*35: mov    $0xcd0000,%esi */
  0x44, 0x89, 0xf7,                         /*3a: mov    %r14d,%edi */
  0xff, 0x93, 0x78, 0x01, 0x00, 0x00,       /*3d: callq  *0x178(%rbx) */
  0x84, 0xc0,                               /*43: test   %al,%al */
  0x74, 0x55,                               /*45: je     9c <op_subi+0x9c> */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*47: movl   $0x6,0xab1008(%rbp) */
  0xf2, 0x41, 0x0f, 0x2a, 0xc6,             /*51: cvtsi2sd %r14d,%xmm0 */
  0xf2, 0x0f, 0x58, 0x05, 0x00, 0x00, 0x00, 0x00,/*56: addsd  0x0(%rip),%xmm0        # 5e <op_subi+0x5e> */
  0xf2, 0x0f, 0x11, 0x85, 0x00, 0x10, 0xab, 0x00,/*5e: movsd  %xmm0,0xab1000(%rbp) */
  0xeb, 0x48,                               /*66: jmp    b0 <op_subi+0xb0> */
  0xc7, 0x85, 0x18, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*68: movl   $0x3,0xab1018(%rbp) */
  0xc7, 0x85, 0x10, 0x10, 0xab, 0x00, 0x00, 0x00, 0xcd, 0x00,/*72: movl   $0xcd0000,0xab1010(%rbp) */
  0xbe, 0x20, 0x00, 0x00, 0x00,             /*7c: mov    $0x20,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*81: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*86: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x01, 0x00, 0x00, 0x00,       /*8b: mov    $0x1,%r8d */
  0x48, 0x89, 0xdf,                         /*91: mov    %rbx,%rdi */
  0xff, 0x93, 0xf8, 0x01, 0x00, 0x00,       /*94: callq  *0x1f8(%rbx) */
  0xeb, 0x14,                               /*9a: jmp    b0 <op_subi+0xb0> */
  0xc7, 0x85, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*9c: movl   $0x3,0xab1008(%rbp) */
  0x8b, 0x44, 0x24, 0x0c,                   /*a6: mov    0xc(%rsp),%eax */
  0x89, 0x85, 0x00, 0x10, 0xab, 0x00,       /*aa: mov    %eax,0xab1000(%rbp) */
  0x48, 0x89, 0xdf,                         /*b0: mov    %rbx,%rdi */
  0x48, 0x83, 0xc4, 0x10,                   /*b3: add    $0x10,%rsp */
  0x5b,                                     /*b7: pop    %rbx */
  0x41, 0x5e,                               /*b8: pop    %r14 */
  0x5d,                                     /*ba: pop    %rbp */

};

static void op_subi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 16 + 8;
  *((int32_t *)(op + 30)) = a * 16 + 0;
  *((int32_t *)(op + 44)) = a * 16 + 0;
  *((int32_t *)(op + 73)) = a * 16 + 8;
  *((int32_t *)(op + 98)) = a * 16 + 0;
  *((int32_t *)(op + 106)) = a * 16 + 24;
  *((int32_t *)(op + 116)) = a * 16 + 16;
  *((int32_t *)(op + 130)) = a * 1 + 0;
  *((int32_t *)(op + 158)) = a * 16 + 8;
  *((int32_t *)(op + 172)) = a * 16 + 0;
  *((int32_t *)(op + 54)) = c * 1 + 0;
  *((int32_t *)(op + 120)) = c * 1 + 0;
  *((int32_t *)(op + 135)) = b * 1 + 0;
}

static void op_subi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_subi_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 12..15], [16, 24, 19..22], [16, 0, 61..64], [16, 16, 68..71], [16, 8, 90..93], [16, 0, 101..104], [16, 16, 121..124], [16, 0, 129..132], [16, 0, 137..140], [16, 0, 154..157], [16, 16, 162..165], [16, 8, 168..171], [16, 0, 180..183], [16, 0, 197..200], [16, 16, 205..208], [16, 0, 213..216], [16, 8, 238..241], [16, 0, 248..251]]} */
static uint8_t op_mul[] = {
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
  0x7f, 0x3f,                               /*2d: jg     6e <op_mul+0x6e> */
  0x3d, 0x03, 0x03, 0x00, 0x00,             /*2f: cmp    $0x303,%eax */
  0x75, 0x59,                               /*34: jne    8f <op_mul+0x8f> */
  0x49, 0x8b, 0x7e, 0x58,                   /*36: mov    0x58(%r14),%rdi */
  0x48, 0x8b, 0xb3, 0x00, 0x10, 0xab, 0x00, /*3a: mov    0xab1000(%rbx),%rsi */
  0x48, 0x8b, 0x8b, 0x10, 0x10, 0xab, 0x00, /*41: mov    0xab1010(%rbx),%rcx */
  0x41, 0xff, 0x96, 0x78, 0x02, 0x00, 0x00, /*48: callq  *0x278(%r14) */
  0x83, 0xfa, 0x06,                         /*4f: cmp    $0x6,%edx */
  0x0f, 0x85, 0x8f, 0x00, 0x00, 0x00,       /*52: jne    e7 <op_mul+0xe7> */
  0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*58: movl   $0x6,0xab1008(%rbx) */
  0x48, 0x89, 0x83, 0x00, 0x10, 0xab, 0x00, /*62: mov    %rax,0xab1000(%rbx) */
  0xe9, 0x8e, 0x00, 0x00, 0x00,             /*69: jmpq   fc <op_mul+0xfc> */
  0x3d, 0x03, 0x06, 0x00, 0x00,             /*6e: cmp    $0x603,%eax */
  0x75, 0x45,                               /*73: jne    ba <op_mul+0xba> */
  0xf2, 0x0f, 0x2a, 0x83, 0x10, 0x10, 0xab, 0x00,/*75: cvtsi2sdl 0xab1010(%rbx),%xmm0 */
  0xf2, 0x0f, 0x59, 0x83, 0x00, 0x10, 0xab, 0x00,/*7d: mulsd  0xab1000(%rbx),%xmm0 */
  0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*85: movsd  %xmm0,0xab1000(%rbx) */
  0xeb, 0x6d,                               /*8d: jmp    fc <op_mul+0xfc> */
  0x3d, 0x06, 0x03, 0x00, 0x00,             /*8f: cmp    $0x306,%eax */
  0x75, 0x45,                               /*94: jne    db <op_mul+0xdb> */
  0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x10, 0xab, 0x00,/*96: cvtsi2sdl 0xab1000(%rbx),%xmm0 */
  0xf2, 0x0f, 0x59, 0x83, 0x10, 0x10, 0xab, 0x00,/*9e: mulsd  0xab1010(%rbx),%xmm0 */
  0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*a6: movl   $0x6,0xab1008(%rbx) */
  0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*b0: movsd  %xmm0,0xab1000(%rbx) */
  0xeb, 0x42,                               /*b8: jmp    fc <op_mul+0xfc> */
  0x3d, 0x06, 0x06, 0x00, 0x00,             /*ba: cmp    $0x606,%eax */
  0x75, 0x1a,                               /*bf: jne    db <op_mul+0xdb> */
  0xf2, 0x0f, 0x10, 0x83, 0x00, 0x10, 0xab, 0x00,/*c1: movsd  0xab1000(%rbx),%xmm0 */
  0xf2, 0x0f, 0x59, 0x83, 0x10, 0x10, 0xab, 0x00,/*c9: mulsd  0xab1010(%rbx),%xmm0 */
  0xf2, 0x0f, 0x11, 0x83, 0x00, 0x10, 0xab, 0x00,/*d1: movsd  %xmm0,0xab1000(%rbx) */
  0xeb, 0x21,                               /*d9: jmp    fc <op_mul+0xfc> */
  0x4c, 0x89, 0xf7,                         /*db: mov    %r14,%rdi */
  0x41, 0xff, 0x96, 0x68, 0x02, 0x00, 0x00, /*de: callq  *0x268(%r14) */
  0xeb, 0x15,                               /*e5: jmp    fc <op_mul+0xfc> */
  0x83, 0xfa, 0x03,                         /*e7: cmp    $0x3,%edx */
  0x75, 0x10,                               /*ea: jne    fc <op_mul+0xfc> */
  0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x03, 0x00, 0x00, 0x00,/*ec: movl   $0x3,0xab1008(%rbx) */
  0x89, 0x83, 0x00, 0x10, 0xab, 0x00,       /*f6: mov    %eax,0xab1000(%rbx) */
  0x4c, 0x89, 0xf7,                         /*fc: mov    %r14,%rdi */
  0x5b,                                     /*ff: pop    %rbx */
  0x41, 0x5e,                               /*100: pop    %r14 */

};

static void op_mul_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 12)) = a * 16 + 8;
  *((int32_t *)(op + 19)) = a * 16 + 24;
  *((int32_t *)(op + 61)) = a * 16 + 0;
  *((int32_t *)(op + 68)) = a * 16 + 16;
  *((int32_t *)(op + 90)) = a * 16 + 8;
  *((int32_t *)(op + 101)) = a * 16 + 0;
  *((int32_t *)(op + 121)) = a * 16 + 16;
  *((int32_t *)(op + 129)) = a * 16 + 0;
  *((int32_t *)(op + 137)) = a * 16 + 0;
  *((int32_t *)(op + 154)) = a * 16 + 0;
  *((int32_t *)(op + 162)) = a * 16 + 16;
  *((int32_t *)(op + 168)) = a * 16 + 8;
  *((int32_t *)(op + 180)) = a * 16 + 0;
  *((int32_t *)(op + 197)) = a * 16 + 0;
  *((int32_t *)(op + 205)) = a * 16 + 16;
  *((int32_t *)(op + 213)) = a * 16 + 0;
  *((int32_t *)(op + 238)) = a * 16 + 8;
  *((int32_t *)(op + 248)) = a * 16 + 0;
}

static void op_mul_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_mul_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[16, 8, 10..13], [16, 24, 26..29], [16, 0, 52..55], [16, 16, 60..63], [16, 8, 66..69], [16, 0, 88..91], [16, 16, 96..99], [16, 0, 108..111], [16, 0, 126..129], [16, 16, 134..137], [16, 8, 140..143], [16, 0, 152..155], [16, 0, 170..173], [16, 16, 178..181], [16, 0, 186..189]]} */
static uint8_t op_div[] = {
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
  0xeb, 0x57,                               /*70: jmp    c9 <op_div+0xc9> */
  0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*72: cmp    $0x306,%ecx */
  0x75, 0x46,                               /*78: jne    c0 <op_div+0xc0> */
  0xf2, 0x0f, 0x2a, 0x80, 0x00, 0x10, 0xab, 0x00,/*7a: cvtsi2sdl 0xab1000(%rax),%xmm0 */
  0xf2, 0x0f, 0x5e, 0x80, 0x10, 0x10, 0xab, 0x00,/*82: divsd  0xab1010(%rax),%xmm0 */
  0xc7, 0x80, 0x08, 0x10, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*8a: movl   $0x6,0xab1008(%rax) */
  0xf2, 0x0f, 0x11, 0x80, 0x00, 0x10, 0xab, 0x00,/*94: movsd  %xmm0,0xab1000(%rax) */
  0xeb, 0x2b,                               /*9c: jmp    c9 <op_div+0xc9> */
  0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*9e: cmp    $0x606,%ecx */
  0x75, 0x1a,                               /*a4: jne    c0 <op_div+0xc0> */
  0xf2, 0x0f, 0x10, 0x80, 0x00, 0x10, 0xab, 0x00,/*a6: movsd  0xab1000(%rax),%xmm0 */
  0xf2, 0x0f, 0x5e, 0x80, 0x10, 0x10, 0xab, 0x00,/*ae: divsd  0xab1010(%rax),%xmm0 */
  0xf2, 0x0f, 0x11, 0x80, 0x00, 0x10, 0xab, 0x00,/*b6: movsd  %xmm0,0xab1000(%rax) */
  0xeb, 0x09,                               /*be: jmp    c9 <op_div+0xc9> */
  0x48, 0x89, 0xdf,                         /*c0: mov    %rbx,%rdi */
  0xff, 0x93, 0x68, 0x02, 0x00, 0x00,       /*c3: callq  *0x268(%rbx) */
  0x48, 0x89, 0xdf,                         /*c9: mov    %rbx,%rdi */
  0x5b,                                     /*cc: pop    %rbx */

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


/* args: {"a"=>[[16, 0, 17..20], [16, 8, 23..26], [16, 16, 30..33], [16, 24, 37..40], [16, 8, 58..61], [16, 24, 74..77], [16, 0, 96..99], [16, 16, 102..105], [16, 16, 125..128], [16, 0, 133..136], [16, 0, 151..154], [16, 0, 168..171], [16, 16, 176..179], [16, 8, 195..198], [16, 0, 205..208], [16, 8, 223..226], [1, 0, 239..242]], "b"=>[[1, 0, 244..247]], "c"=>[[1, 0, 250..253]]} */
static uint8_t op_eq[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
  0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
  0x49, 0x8b, 0x7e, 0x58,                   /*a: mov    0x58(%r14),%rdi */
  0x48, 0x8b, 0xb3, 0x00, 0x10, 0xab, 0x00, /*e: mov    0xab1000(%rbx),%rsi */
  0x8b, 0x93, 0x08, 0x10, 0xab, 0x00,       /*15: mov    0xab1008(%rbx),%edx */
  0x48, 0x8b, 0x8b, 0x10, 0x10, 0xab, 0x00, /*1b: mov    0xab1010(%rbx),%rcx */
  0x44, 0x8b, 0x83, 0x18, 0x10, 0xab, 0x00, /*22: mov    0xab1018(%rbx),%r8d */
  0x41, 0xff, 0x96, 0xa8, 0x00, 0x00, 0x00, /*29: callq  *0xa8(%r14) */
  0x84, 0xc0,                               /*30: test   %al,%al */
  0x0f, 0x85, 0x89, 0x00, 0x00, 0x00,       /*32: jne    c1 <op_eq+0xc1> */
  0x8b, 0x8b, 0x08, 0x10, 0xab, 0x00,       /*38: mov    0xab1008(%rbx),%ecx */
  0xc1, 0xe1, 0x08,                         /*3e: shl    $0x8,%ecx */
  0x81, 0xe1, 0x00, 0xff, 0xff, 0x00,       /*41: and    $0xffff00,%ecx */
  0x0f, 0xb6, 0x83, 0x18, 0x10, 0xab, 0x00, /*47: movzbl 0xab1018(%rbx),%eax */
  0x09, 0xc8,                               /*4e: or     %ecx,%eax */
  0x3d, 0x02, 0x06, 0x00, 0x00,             /*50: cmp    $0x602,%eax */
  0x7f, 0x1b,                               /*55: jg     72 <op_eq+0x72> */
  0x3d, 0x03, 0x03, 0x00, 0x00,             /*57: cmp    $0x303,%eax */
  0x75, 0x2e,                               /*5c: jne    8c <op_eq+0x8c> */
  0x8b, 0x83, 0x00, 0x10, 0xab, 0x00,       /*5e: mov    0xab1000(%rbx),%eax */
  0x3b, 0x83, 0x10, 0x10, 0xab, 0x00,       /*64: cmp    0xab1010(%rbx),%eax */
  0x0f, 0x94, 0xc0,                         /*6a: sete   %al */
  0x0f, 0xb6, 0xc0,                         /*6d: movzbl %al,%eax */
  0xeb, 0x4b,                               /*70: jmp    bd <op_eq+0xbd> */
  0x3d, 0x03, 0x06, 0x00, 0x00,             /*72: cmp    $0x603,%eax */
  0x75, 0x24,                               /*77: jne    9d <op_eq+0x9d> */
  0xf2, 0x0f, 0x2a, 0x83, 0x10, 0x10, 0xab, 0x00,/*79: cvtsi2sdl 0xab1010(%rbx),%xmm0 */
  0xf2, 0x0f, 0xc2, 0x83, 0x00, 0x10, 0xab, 0x00, 0x00,/*81: cmpeqsd 0xab1000(%rbx),%xmm0 */
  0xeb, 0x29,                               /*8a: jmp    b5 <op_eq+0xb5> */
  0x3d, 0x06, 0x03, 0x00, 0x00,             /*8c: cmp    $0x306,%eax */
  0x75, 0x56,                               /*91: jne    e9 <op_eq+0xe9> */
  0xf2, 0x0f, 0x2a, 0x83, 0x00, 0x10, 0xab, 0x00,/*93: cvtsi2sdl 0xab1000(%rbx),%xmm0 */
  0xeb, 0x0f,                               /*9b: jmp    ac <op_eq+0xac> */
  0x3d, 0x06, 0x06, 0x00, 0x00,             /*9d: cmp    $0x606,%eax */
  0x75, 0x45,                               /*a2: jne    e9 <op_eq+0xe9> */
  0xf2, 0x0f, 0x10, 0x83, 0x00, 0x10, 0xab, 0x00,/*a4: movsd  0xab1000(%rbx),%xmm0 */
  0xf2, 0x0f, 0xc2, 0x83, 0x10, 0x10, 0xab, 0x00, 0x00,/*ac: cmpeqsd 0xab1010(%rbx),%xmm0 */
  0x66, 0x48, 0x0f, 0x7e, 0xc0,             /*b5: movq   %xmm0,%rax */
  0x83, 0xe0, 0x01,                         /*ba: and    $0x1,%eax */
  0x85, 0xc0,                               /*bd: test   %eax,%eax */
  0x74, 0x1c,                               /*bf: je     dd <op_eq+0xdd> */
  0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*c1: movl   $0x2,0xab1008(%rbx) */
  0xc7, 0x83, 0x00, 0x10, 0xab, 0x00, 0x01, 0x00, 0x00, 0x00,/*cb: movl   $0x1,0xab1000(%rbx) */
  0x4c, 0x89, 0xf7,                         /*d5: mov    %r14,%rdi */
  0x5b,                                     /*d8: pop    %rbx */
  0x41, 0x5e,                               /*d9: pop    %r14 */
  0xeb, 0x2d,                               /*db: jmp    10a <op_eq+0x10a> */
  0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*dd: movl   $0x0,0xab1008(%rbx) */
  0xeb, 0xe2,                               /*e7: jmp    cb <op_eq+0xcb> */
  0xbe, 0x20, 0x00, 0x00, 0x00,             /*e9: mov    $0x20,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*ee: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*f3: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*f8: mov    $0xcd0000,%r8d */
  0x4c, 0x89, 0xf7,                         /*fe: mov    %r14,%rdi */
  0x41, 0xff, 0x96, 0xf8, 0x01, 0x00, 0x00, /*101: callq  *0x1f8(%r14) */
  0xeb, 0xcb,                               /*108: jmp    d5 <op_eq+0xd5> */

};

static void op_eq_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 16 + 0;
  *((int32_t *)(op + 23)) = a * 16 + 8;
  *((int32_t *)(op + 30)) = a * 16 + 16;
  *((int32_t *)(op + 37)) = a * 16 + 24;
  *((int32_t *)(op + 58)) = a * 16 + 8;
  *((int32_t *)(op + 74)) = a * 16 + 24;
  *((int32_t *)(op + 96)) = a * 16 + 0;
  *((int32_t *)(op + 102)) = a * 16 + 16;
  *((int32_t *)(op + 125)) = a * 16 + 16;
  *((int32_t *)(op + 133)) = a * 16 + 0;
  *((int32_t *)(op + 151)) = a * 16 + 0;
  *((int32_t *)(op + 168)) = a * 16 + 0;
  *((int32_t *)(op + 176)) = a * 16 + 16;
  *((int32_t *)(op + 195)) = a * 16 + 8;
  *((int32_t *)(op + 205)) = a * 16 + 0;
  *((int32_t *)(op + 223)) = a * 16 + 8;
  *((int32_t *)(op + 239)) = a * 1 + 0;
  *((int32_t *)(op + 244)) = b * 1 + 0;
  *((int32_t *)(op + 250)) = c * 1 + 0;
}

static void op_eq_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_eq_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 8, 10..13], [16, 24, 26..29], [16, 0, 50..53], [16, 16, 56..59], [16, 16, 77..80], [16, 0, 95..98], [16, 16, 103..106], [16, 16, 128..131], [16, 0, 136..139], [16, 8, 152..155], [16, 8, 164..167], [16, 0, 174..177], [1, 0, 190..193]], "b"=>[[1, 0, 195..198]], "c"=>[[1, 0, 201..204]]} */
static uint8_t op_lt[] = {
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
  0xeb, 0x1e,                               /*b6: jmp    d6 <op_lt+0xd6> */
  0xbe, 0x20, 0x00, 0x00, 0x00,             /*b8: mov    $0x20,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*bd: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*c2: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*c7: mov    $0xcd0000,%r8d */
  0x48, 0x89, 0xdf,                         /*cd: mov    %rbx,%rdi */
  0xff, 0x93, 0xf8, 0x01, 0x00, 0x00,       /*d0: callq  *0x1f8(%rbx) */
  0x48, 0x89, 0xdf,                         /*d6: mov    %rbx,%rdi */
  0x5b,                                     /*d9: pop    %rbx */

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
static uint8_t op_le[] = {
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
  0xeb, 0x1e,                               /*b6: jmp    d6 <op_le+0xd6> */
  0xbe, 0x20, 0x00, 0x00, 0x00,             /*b8: mov    $0x20,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*bd: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*c2: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*c7: mov    $0xcd0000,%r8d */
  0x48, 0x89, 0xdf,                         /*cd: mov    %rbx,%rdi */
  0xff, 0x93, 0xf8, 0x01, 0x00, 0x00,       /*d0: callq  *0x1f8(%rbx) */
  0x48, 0x89, 0xdf,                         /*d6: mov    %rbx,%rdi */
  0x5b,                                     /*d9: pop    %rbx */

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
static uint8_t op_gt[] = {
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
  0xeb, 0x1e,                               /*b6: jmp    d6 <op_gt+0xd6> */
  0xbe, 0x20, 0x00, 0x00, 0x00,             /*b8: mov    $0x20,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*bd: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*c2: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*c7: mov    $0xcd0000,%r8d */
  0x48, 0x89, 0xdf,                         /*cd: mov    %rbx,%rdi */
  0xff, 0x93, 0xf8, 0x01, 0x00, 0x00,       /*d0: callq  *0x1f8(%rbx) */
  0x48, 0x89, 0xdf,                         /*d6: mov    %rbx,%rdi */
  0x5b,                                     /*d9: pop    %rbx */

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
static uint8_t op_ge[] = {
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
  0xeb, 0x1e,                               /*b6: jmp    d6 <op_ge+0xd6> */
  0xbe, 0x20, 0x00, 0x00, 0x00,             /*b8: mov    $0x20,%esi */
  0xba, 0x00, 0x00, 0xab, 0x00,             /*bd: mov    $0xab0000,%edx */
  0xb9, 0x00, 0x00, 0xbc, 0x00,             /*c2: mov    $0xbc0000,%ecx */
  0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*c7: mov    $0xcd0000,%r8d */
  0x48, 0x89, 0xdf,                         /*cd: mov    %rbx,%rdi */
  0xff, 0x93, 0xf8, 0x01, 0x00, 0x00,       /*d0: callq  *0x1f8(%rbx) */
  0x48, 0x89, 0xdf,                         /*d6: mov    %rbx,%rdi */
  0x5b,                                     /*d9: pop    %rbx */

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


/* args: {"b"=>[[16, 0, 17..20]], "c"=>[[1, 0, 22..25]], "a"=>[[16, 0, 35..38], [16, 8, 42..45]]} */
static uint8_t op_array[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
  0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
  0x49, 0x8d, 0x96, 0x00, 0x10, 0xbc, 0x00, /*e: lea    0xbc1000(%r14),%rdx */
  0xbe, 0x00, 0x00, 0xcd, 0x00,             /*15: mov    $0xcd0000,%esi */
  0xff, 0x93, 0x50, 0x02, 0x00, 0x00,       /*1a: callq  *0x250(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*20: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*27: mov    %edx,0xab1008(%r14) */
  0x8b, 0x43, 0x50,                         /*2e: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*31: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*35: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*3b: mov    %rbx,%rdi */
  0x5b,                                     /*3e: pop    %rbx */
  0x41, 0x5e,                               /*3f: pop    %r14 */

};

static void op_array_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 16 + 0;
  *((int32_t *)(op + 22)) = c * 1 + 0;
  *((int32_t *)(op + 35)) = a * 16 + 0;
  *((int32_t *)(op + 42)) = a * 16 + 8;
}

static void op_array_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_array_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[16, 0, 19..22], [16, 8, 26..29]], "a"=>[[16, 0, 45..48], [16, 8, 52..55]]} */
static uint8_t op_arycat[] = {
  0x41, 0x57,                               /*0: push   %r15 */
  0x41, 0x56,                               /*2: push   %r14 */
  0x53,                                     /*4: push   %rbx */
  0x48, 0x89, 0xfb,                         /*5: mov    %rdi,%rbx */
  0x4c, 0x8b, 0x7b, 0x18,                   /*8: mov    0x18(%rbx),%r15 */
  0x4c, 0x8b, 0x73, 0x58,                   /*c: mov    0x58(%rbx),%r14 */
  0x49, 0x8b, 0xb7, 0x00, 0x10, 0xbc, 0x00, /*10: mov    0xbc1000(%r15),%rsi */
  0x41, 0x8b, 0x97, 0x08, 0x10, 0xbc, 0x00, /*17: mov    0xbc1008(%r15),%edx */
  0x4c, 0x89, 0xf7,                         /*1e: mov    %r14,%rdi */
  0xff, 0x93, 0x00, 0x01, 0x00, 0x00,       /*21: callq  *0x100(%rbx) */
  0x41, 0x89, 0xd0,                         /*27: mov    %edx,%r8d */
  0x49, 0x8b, 0xb7, 0x00, 0x10, 0xab, 0x00, /*2a: mov    0xab1000(%r15),%rsi */
  0x41, 0x8b, 0x97, 0x08, 0x10, 0xab, 0x00, /*31: mov    0xab1008(%r15),%edx */
  0x4c, 0x89, 0xf7,                         /*38: mov    %r14,%rdi */
  0x48, 0x89, 0xc1,                         /*3b: mov    %rax,%rcx */
  0xff, 0x93, 0xb0, 0x01, 0x00, 0x00,       /*3e: callq  *0x1b0(%rbx) */
  0x8b, 0x43, 0x50,                         /*44: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*47: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*4b: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*51: mov    %rbx,%rdi */
  0x5b,                                     /*54: pop    %rbx */
  0x41, 0x5e,                               /*55: pop    %r14 */
  0x41, 0x5f,                               /*57: pop    %r15 */

};

static void op_arycat_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = b * 16 + 0;
  *((int32_t *)(op + 26)) = b * 16 + 8;
  *((int32_t *)(op + 45)) = a * 16 + 0;
  *((int32_t *)(op + 52)) = a * 16 + 8;
}

static void op_arycat_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_arycat_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 15..18], [16, 8, 21..24]], "b"=>[[16, 0, 28..31], [16, 8, 35..38]]} */
static uint8_t op_arypush[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x7b, 0x58,                   /*8: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0xb0, 0x00, 0x10, 0xab, 0x00, /*c: mov    0xab1000(%rax),%rsi */
  0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*13: mov    0xab1008(%rax),%edx */
  0x48, 0x8b, 0x88, 0x00, 0x10, 0xbc, 0x00, /*19: mov    0xbc1000(%rax),%rcx */
  0x44, 0x8b, 0x80, 0x08, 0x10, 0xbc, 0x00, /*20: mov    0xbc1008(%rax),%r8d */
  0xff, 0x93, 0x70, 0x02, 0x00, 0x00,       /*27: callq  *0x270(%rbx) */
  0x48, 0x89, 0xdf,                         /*2d: mov    %rbx,%rdi */
  0x5b,                                     /*30: pop    %rbx */

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


/* args: {"b"=>[[16, 8, 12..15], [16, 0, 22..25]], "c"=>[[1, 0, 36..39]], "a"=>[[16, 0, 50..53], [16, 8, 56..59], [16, 8, 64..67], [16, 0, 78..81]]} */
static uint8_t op_aref[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
  0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
  0x83, 0xbb, 0x08, 0x10, 0xbc, 0x00, 0x0e, /*a: cmpl   $0xe,0xbc1008(%rbx) */
  0x75, 0x2b,                               /*11: jne    3e <op_aref+0x3e> */
  0x48, 0x8b, 0xb3, 0x00, 0x10, 0xbc, 0x00, /*13: mov    0xbc1000(%rbx),%rsi */
  0x49, 0x8b, 0x7e, 0x58,                   /*1a: mov    0x58(%r14),%rdi */
  0xba, 0x0e, 0x00, 0x00, 0x00,             /*1e: mov    $0xe,%edx */
  0xb9, 0x00, 0x00, 0xcd, 0x00,             /*23: mov    $0xcd0000,%ecx */
  0x41, 0xff, 0x96, 0x28, 0x02, 0x00, 0x00, /*28: callq  *0x228(%r14) */
  0x48, 0x89, 0x83, 0x00, 0x10, 0xab, 0x00, /*2f: mov    %rax,0xab1000(%rbx) */
  0x89, 0x93, 0x08, 0x10, 0xab, 0x00,       /*36: mov    %edx,0xab1008(%rbx) */
  0xeb, 0x18,                               /*3c: jmp    56 <op_aref+0x56> */
  0xc7, 0x83, 0x08, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*3e: movl   $0x0,0xab1008(%rbx) */
  0x49, 0x8b, 0x46, 0x18,                   /*48: mov    0x18(%r14),%rax */
  0xc7, 0x80, 0x00, 0x10, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4c: movl   $0x0,0xab1000(%rax) */
  0x4c, 0x89, 0xf7,                         /*56: mov    %r14,%rdi */
  0x5b,                                     /*59: pop    %rbx */
  0x41, 0x5e,                               /*5a: pop    %r14 */

};

static void op_aref_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 12)) = b * 16 + 8;
  *((int32_t *)(op + 22)) = b * 16 + 0;
  *((int32_t *)(op + 36)) = c * 1 + 0;
  *((int32_t *)(op + 50)) = a * 16 + 0;
  *((int32_t *)(op + 56)) = a * 16 + 8;
  *((int32_t *)(op + 64)) = a * 16 + 8;
  *((int32_t *)(op + 78)) = a * 16 + 0;
}

static void op_aref_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_aref_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[16, 0, 15..18], [16, 8, 21..24]], "a"=>[[16, 0, 28..31], [16, 8, 35..38]], "c"=>[[1, 0, 40..43]]} */
static uint8_t op_aset[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x7b, 0x58,                   /*8: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0xb0, 0x00, 0x10, 0xbc, 0x00, /*c: mov    0xbc1000(%rax),%rsi */
  0x8b, 0x90, 0x08, 0x10, 0xbc, 0x00,       /*13: mov    0xbc1008(%rax),%edx */
  0x4c, 0x8b, 0x80, 0x00, 0x10, 0xab, 0x00, /*19: mov    0xab1000(%rax),%r8 */
  0x44, 0x8b, 0x88, 0x08, 0x10, 0xab, 0x00, /*20: mov    0xab1008(%rax),%r9d */
  0xb9, 0x00, 0x00, 0xcd, 0x00,             /*27: mov    $0xcd0000,%ecx */
  0xff, 0x93, 0x60, 0x02, 0x00, 0x00,       /*2c: callq  *0x260(%rbx) */
  0x48, 0x89, 0xdf,                         /*32: mov    %rbx,%rdi */
  0x5b,                                     /*35: pop    %rbx */

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


/* args: {"a"=>[[16, 8, 19..22], [16, 0, 34..37], [16, 0, 86..89], [16, 8, 94..97], [1, 1, 105..108], [16, 0, 186..189], [16, 8, 194..197], [1, 1, 215..218], [1, 1, 230..233], [16, 0, 270..273], [16, 8, 278..281], [1, 1, 297..300], [1, 1, 391..394], [1, 1, 406..409]], "b"=>[[16, 0, 71..74], [1, 1, 284..287], [1, 0, 292..295]], "c"=>[[1, 0, 244..247], [1, 0, 362..365]]} */
static uint8_t op_apost[] = {
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
  0x41, 0xff, 0x96, 0x50, 0x02, 0x00, 0x00, /*4b: callq  *0x250(%r14) */
  0x49, 0x89, 0x84, 0x24, 0x00, 0x10, 0xab, 0x00,/*52: mov    %rax,0xab1000(%r12) */
  0x41, 0x89, 0x94, 0x24, 0x08, 0x10, 0xab, 0x00,/*5a: mov    %edx,0xab1008(%r12) */
  0x81, 0xc3, 0x00, 0x00, 0x33, 0xff,       /*62: add    $0xff330000,%ebx */
  0xb8, 0x10, 0x00, 0xb0, 0x0a,             /*68: mov    $0xab00010,%eax */
  0x0f, 0x1f, 0x00,                         /*6d: nopl   (%rax) */
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
  0x41, 0xff, 0x96, 0xe0, 0x00, 0x00, 0x00, /*af: callq  *0xe0(%r14) */
  0x49, 0x89, 0x84, 0x24, 0x00, 0x10, 0xab, 0x00,/*b6: mov    %rax,0xab1000(%r12) */
  0x41, 0x89, 0x94, 0x24, 0x08, 0x10, 0xab, 0x00,/*be: mov    %edx,0xab1008(%r12) */
  0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*c6: nopw   %cs:0x0(%rax,%rax,1) */
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
  0x41, 0xff, 0x96, 0xe0, 0x00, 0x00, 0x00, /*103: callq  *0xe0(%r14) */
  0x49, 0x89, 0x84, 0x24, 0x00, 0x10, 0xab, 0x00,/*10a: mov    %rax,0xab1000(%r12) */
  0x41, 0x89, 0x94, 0x24, 0x08, 0x10, 0xab, 0x00,/*112: mov    %edx,0xab1008(%r12) */
  0x81, 0xfb, 0x01, 0x00, 0xbc, 0x00,       /*11a: cmp    $0xbc0001,%ebx */
  0x7c, 0x47,                               /*120: jl     169 <op_apost+0x169> */
  0x8d, 0x83, 0x00, 0x00, 0x44, 0xff,       /*122: lea    -0xbc0000(%rbx),%eax */
  0xb9, 0x10, 0x00, 0xb0, 0x0a,             /*128: mov    $0xab00010,%ecx */
  0x0f, 0x1f, 0x00,                         /*12d: nopl   (%rax) */
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

static void op_apost_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = a * 16 + 8;
  *((int32_t *)(op + 34)) = a * 16 + 0;
  *((int32_t *)(op + 86)) = a * 16 + 0;
  *((int32_t *)(op + 94)) = a * 16 + 8;
  *((int32_t *)(op + 105)) = a * 1 + 1;
  *((int32_t *)(op + 186)) = a * 16 + 0;
  *((int32_t *)(op + 194)) = a * 16 + 8;
  *((int32_t *)(op + 215)) = a * 1 + 1;
  *((int32_t *)(op + 230)) = a * 1 + 1;
  *((int32_t *)(op + 270)) = a * 16 + 0;
  *((int32_t *)(op + 278)) = a * 16 + 8;
  *((int32_t *)(op + 297)) = a * 1 + 1;
  *((int32_t *)(op + 391)) = a * 1 + 1;
  *((int32_t *)(op + 406)) = a * 1 + 1;
  *((int32_t *)(op + 71)) = b * 16 + 0;
  *((int32_t *)(op + 284)) = b * 1 + 1;
  *((int32_t *)(op + 292)) = b * 1 + 0;
  *((int32_t *)(op + 244)) = c * 1 + 0;
  *((int32_t *)(op + 362)) = c * 1 + 0;
}

static void op_apost_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_apost_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[16, 0, 21..24], [16, 8, 27..30]], "a"=>[[16, 0, 40..43], [16, 8, 47..50]]} */
static uint8_t op_string[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*6: mov    0x58(%rbx),%rdi */
  0x4c, 0x8b, 0x73, 0x18,                   /*a: mov    0x18(%rbx),%r14 */
  0x48, 0x8b, 0x43, 0x20,                   /*e: mov    0x20(%rbx),%rax */
  0x48, 0x8b, 0xb0, 0x00, 0x10, 0xbc, 0x00, /*12: mov    0xbc1000(%rax),%rsi */
  0x8b, 0x90, 0x08, 0x10, 0xbc, 0x00,       /*19: mov    0xbc1008(%rax),%edx */
  0xff, 0x93, 0xe8, 0x01, 0x00, 0x00,       /*1f: callq  *0x1e8(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*25: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*2c: mov    %edx,0xab1008(%r14) */
  0x8b, 0x43, 0x50,                         /*33: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*36: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*3a: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*40: mov    %rbx,%rdi */
  0x5b,                                     /*43: pop    %rbx */
  0x41, 0x5e,                               /*44: pop    %r14 */

};

static void op_string_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 16 + 0;
  *((int32_t *)(op + 27)) = b * 16 + 8;
  *((int32_t *)(op + 40)) = a * 16 + 0;
  *((int32_t *)(op + 47)) = a * 16 + 8;
}

static void op_string_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_string_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 15..18], [16, 8, 21..24]], "b"=>[[16, 0, 28..31], [16, 8, 35..38]]} */
static uint8_t op_strcat[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x7b, 0x58,                   /*8: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0xb0, 0x00, 0x10, 0xab, 0x00, /*c: mov    0xab1000(%rax),%rsi */
  0x8b, 0x90, 0x08, 0x10, 0xab, 0x00,       /*13: mov    0xab1008(%rax),%edx */
  0x48, 0x8b, 0x88, 0x00, 0x10, 0xbc, 0x00, /*19: mov    0xbc1000(%rax),%rcx */
  0x44, 0x8b, 0x80, 0x08, 0x10, 0xbc, 0x00, /*20: mov    0xbc1008(%rax),%r8d */
  0xff, 0x93, 0x18, 0x02, 0x00, 0x00,       /*27: callq  *0x218(%rbx) */
  0x48, 0x89, 0xdf,                         /*2d: mov    %rbx,%rdi */
  0x5b,                                     /*30: pop    %rbx */

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


/* args: {"b"=>[[1, 0, 19..22]], "c"=>[[1, 0, 27..30]], "a"=>[[16, 0, 171..174], [16, 8, 178..181]]} */
static uint8_t op_hash[] = {
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
  0xff, 0x93, 0x70, 0x01, 0x00, 0x00,       /*33: callq  *0x170(%rbx) */
  0x49, 0x89, 0xc6,                         /*39: mov    %rax,%r14 */
  0x41, 0x89, 0xd4,                         /*3c: mov    %edx,%r12d */
  0xeb, 0x5a,                               /*3f: jmp    9b <op_hash+0x9b> */
  0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*41: data16 data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
  0x48, 0x8b, 0x7b, 0x58,                   /*50: mov    0x58(%rbx),%rdi */
  0x48, 0x63, 0x44, 0x24, 0x1c,             /*54: movslq 0x1c(%rsp),%rax */
  0x48, 0x8b, 0x53, 0x18,                   /*59: mov    0x18(%rbx),%rdx */
  0x48, 0xc1, 0xe0, 0x04,                   /*5d: shl    $0x4,%rax */
  0x48, 0x63, 0x74, 0x24, 0x1c,             /*61: movslq 0x1c(%rsp),%rsi */
  0x48, 0xc1, 0xe6, 0x04,                   /*66: shl    $0x4,%rsi */
  0x48, 0x8b, 0x6b, 0x18,                   /*6a: mov    0x18(%rbx),%rbp */
  0x48, 0x8b, 0x0c, 0x02,                   /*6e: mov    (%rdx,%rax,1),%rcx */
  0x44, 0x8b, 0x44, 0x02, 0x08,             /*72: mov    0x8(%rdx,%rax,1),%r8d */
  0x48, 0x8b, 0x44, 0x2e, 0x10,             /*77: mov    0x10(%rsi,%rbp,1),%rax */
  0x48, 0x8b, 0x54, 0x2e, 0x18,             /*7c: mov    0x18(%rsi,%rbp,1),%rdx */
  0x48, 0x89, 0x54, 0x24, 0x08,             /*81: mov    %rdx,0x8(%rsp) */
  0x48, 0x89, 0x04, 0x24,                   /*86: mov    %rax,(%rsp) */
  0x4c, 0x89, 0xf6,                         /*8a: mov    %r14,%rsi */
  0x44, 0x89, 0xe2,                         /*8d: mov    %r12d,%edx */
  0xff, 0x93, 0xc0, 0x01, 0x00, 0x00,       /*90: callq  *0x1c0(%rbx) */
  0x83, 0x44, 0x24, 0x1c, 0x02,             /*96: addl   $0x2,0x1c(%rsp) */
  0x8b, 0x44, 0x24, 0x1c,                   /*9b: mov    0x1c(%rsp),%eax */
  0x44, 0x39, 0xf8,                         /*9f: cmp    %r15d,%eax */
  0x7c, 0xac,                               /*a2: jl     50 <op_hash+0x50> */
  0x48, 0x8b, 0x43, 0x18,                   /*a4: mov    0x18(%rbx),%rax */
  0x4c, 0x89, 0xb0, 0x00, 0x10, 0xab, 0x00, /*a8: mov    %r14,0xab1000(%rax) */
  0x44, 0x89, 0xa0, 0x08, 0x10, 0xab, 0x00, /*af: mov    %r12d,0xab1008(%rax) */
  0x8b, 0x43, 0x50,                         /*b6: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*b9: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*bd: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*c3: mov    %rbx,%rdi */
  0x48, 0x83, 0xc4, 0x20,                   /*c6: add    $0x20,%rsp */
  0x5b,                                     /*ca: pop    %rbx */
  0x41, 0x5c,                               /*cb: pop    %r12 */
  0x41, 0x5e,                               /*cd: pop    %r14 */
  0x41, 0x5f,                               /*cf: pop    %r15 */
  0x5d,                                     /*d1: pop    %rbp */

};

static void op_hash_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = b * 1 + 0;
  *((int32_t *)(op + 27)) = c * 1 + 0;
  *((int32_t *)(op + 171)) = a * 16 + 0;
  *((int32_t *)(op + 178)) = a * 16 + 8;
}

static void op_hash_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_hash_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[1, 0, 5..8]], "b"=>[[1, 0, 10..13]], "c"=>[[1, 0, 15..18]]} */
static uint8_t op_lambda[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0xbe, 0x00, 0x00, 0xab, 0x00,             /*4: mov    $0xab0000,%esi */
  0xba, 0x00, 0x00, 0xbc, 0x00,             /*9: mov    $0xbc0000,%edx */
  0xb9, 0x00, 0x00, 0xcd, 0x00,             /*e: mov    $0xcd0000,%ecx */
  0xff, 0x93, 0x90, 0x01, 0x00, 0x00,       /*13: callq  *0x190(%rbx) */
  0x48, 0x89, 0xdf,                         /*19: mov    %rbx,%rdi */
  0x5b,                                     /*1c: pop    %rbx */

};

static void op_lambda_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = a * 1 + 0;
  *((int32_t *)(op + 10)) = b * 1 + 0;
  *((int32_t *)(op + 15)) = c * 1 + 0;
}

static void op_lambda_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_lambda_set_args(op, GETARG_A(c),GETARG_b(c),GETARG_c(c),op_idx);
}


/* args: {"b"=>[[16, 0, 17..20], [16, 8, 23..26], [16, 16, 30..33], [16, 24, 37..40]], "c"=>[[1, 0, 43..46]], "a"=>[[16, 0, 57..60], [16, 8, 63..66]]} */
static uint8_t op_range[] = {
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
  0x41, 0xff, 0x96, 0x88, 0x01, 0x00, 0x00, /*2f: callq  *0x188(%r14) */
  0x48, 0x89, 0x83, 0x00, 0x10, 0xab, 0x00, /*36: mov    %rax,0xab1000(%rbx) */
  0x89, 0x93, 0x08, 0x10, 0xab, 0x00,       /*3d: mov    %edx,0xab1008(%rbx) */
  0x41, 0x8b, 0x46, 0x50,                   /*43: mov    0x50(%r14),%eax */
  0x49, 0x8b, 0x4e, 0x58,                   /*47: mov    0x58(%r14),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*4b: mov    %eax,0xdc(%rcx) */
  0x4c, 0x89, 0xf7,                         /*51: mov    %r14,%rdi */
  0x5b,                                     /*54: pop    %rbx */
  0x41, 0x5e,                               /*55: pop    %r14 */

};

static void op_range_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 16 + 0;
  *((int32_t *)(op + 23)) = b * 16 + 8;
  *((int32_t *)(op + 30)) = b * 16 + 16;
  *((int32_t *)(op + 37)) = b * 16 + 24;
  *((int32_t *)(op + 43)) = c * 1 + 0;
  *((int32_t *)(op + 57)) = a * 16 + 0;
  *((int32_t *)(op + 63)) = a * 16 + 8;
}

static void op_range_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_range_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[16, 0, 27..30], [16, 8, 34..37]]} */
static uint8_t op_oclass[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
  0x48, 0x8b, 0x43, 0x58,                   /*a: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x78, 0x40,                   /*e: mov    0x40(%rax),%rdi */
  0xff, 0x93, 0x48, 0x02, 0x00, 0x00,       /*12: callq  *0x248(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*18: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*1f: mov    %edx,0xab1008(%r14) */
  0x48, 0x89, 0xdf,                         /*26: mov    %rbx,%rdi */
  0x5b,                                     /*29: pop    %rbx */
  0x41, 0x5e,                               /*2a: pop    %r14 */

};

static void op_oclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 27)) = a * 16 + 0;
  *((int32_t *)(op + 34)) = a * 16 + 8;
}

static void op_oclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_oclass_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[16, 0, 27..30], [16, 8, 40..43], [16, 16, 47..50], [16, 24, 54..57], [16, 0, 141..144], [16, 8, 147..150]]} */
static uint8_t op_class[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x57,                               /*1: push   %r15 */
  0x41, 0x56,                               /*3: push   %r14 */
  0x53,                                     /*5: push   %rbx */
  0x48, 0x89, 0xfb,                         /*6: mov    %rdi,%rbx */
  0x48, 0x8b, 0x73, 0x18,                   /*9: mov    0x18(%rbx),%rsi */
  0x48, 0x8b, 0x43, 0x28,                   /*d: mov    0x28(%rbx),%rax */
  0x44, 0x8b, 0xb0, 0x00, 0x04, 0xbc, 0x00, /*11: mov    0xbc0400(%rax),%r14d */
  0x48, 0x8b, 0x86, 0x00, 0x10, 0xab, 0x00, /*18: mov    0xab1000(%rsi),%rax */
  0x48, 0x89, 0xc1,                         /*1f: mov    %rax,%rcx */
  0x48, 0xc1, 0xe9, 0x20,                   /*22: shr    $0x20,%rcx */
  0x8b, 0x96, 0x08, 0x10, 0xab, 0x00,       /*26: mov    0xab1008(%rsi),%edx */
  0x48, 0x8b, 0xae, 0x10, 0x10, 0xab, 0x00, /*2c: mov    0xab1010(%rsi),%rbp */
  0x44, 0x8b, 0xbe, 0x18, 0x10, 0xab, 0x00, /*33: mov    0xab1018(%rsi),%r15d */
  0x85, 0xd2,                               /*3a: test   %edx,%edx */
  0x75, 0x23,                               /*3c: jne    61 <op_class+0x61> */
  0x31, 0xd2,                               /*3e: xor    %edx,%edx */
  0x85, 0xc0,                               /*40: test   %eax,%eax */
  0x75, 0x1d,                               /*42: jne    61 <op_class+0x61> */
  0x48, 0x8b, 0x43, 0x58,                   /*44: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*48: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x40, 0x20,                   /*4c: mov    0x20(%rax),%rax */
  0x48, 0x8b, 0x78, 0x48,                   /*50: mov    0x48(%rax),%rdi */
  0xff, 0x93, 0x48, 0x02, 0x00, 0x00,       /*54: callq  *0x248(%rbx) */
  0x48, 0x89, 0xc1,                         /*5a: mov    %rax,%rcx */
  0x48, 0xc1, 0xe9, 0x20,                   /*5d: shr    $0x20,%rcx */
  0x48, 0x8b, 0x7b, 0x58,                   /*61: mov    0x58(%rbx),%rdi */
  0x48, 0xc1, 0xe1, 0x20,                   /*65: shl    $0x20,%rcx */
  0x89, 0xc6,                               /*69: mov    %eax,%esi */
  0x48, 0x09, 0xce,                         /*6b: or     %rcx,%rsi */
  0x48, 0x89, 0xe9,                         /*6e: mov    %rbp,%rcx */
  0x45, 0x89, 0xf8,                         /*71: mov    %r15d,%r8d */
  0x45, 0x89, 0xf1,                         /*74: mov    %r14d,%r9d */
  0xff, 0x93, 0x98, 0x01, 0x00, 0x00,       /*77: callq  *0x198(%rbx) */
  0x48, 0x8b, 0x6b, 0x18,                   /*7d: mov    0x18(%rbx),%rbp */
  0x48, 0x89, 0xc7,                         /*81: mov    %rax,%rdi */
  0xff, 0x93, 0x48, 0x02, 0x00, 0x00,       /*84: callq  *0x248(%rbx) */
  0x48, 0x89, 0x85, 0x00, 0x10, 0xab, 0x00, /*8a: mov    %rax,0xab1000(%rbp) */
  0x89, 0x95, 0x08, 0x10, 0xab, 0x00,       /*91: mov    %edx,0xab1008(%rbp) */
  0x8b, 0x43, 0x50,                         /*97: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*9a: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*9e: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*a4: mov    %rbx,%rdi */
  0x5b,                                     /*a7: pop    %rbx */
  0x41, 0x5e,                               /*a8: pop    %r14 */
  0x41, 0x5f,                               /*aa: pop    %r15 */
  0x5d,                                     /*ac: pop    %rbp */

};

static void op_class_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 27)) = a * 16 + 0;
  *((int32_t *)(op + 40)) = a * 16 + 8;
  *((int32_t *)(op + 47)) = a * 16 + 16;
  *((int32_t *)(op + 54)) = a * 16 + 24;
  *((int32_t *)(op + 141)) = a * 16 + 0;
  *((int32_t *)(op + 147)) = a * 16 + 8;
}

static void op_class_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_class_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 15..18]], "a"=>[[16, 0, 22..25], [16, 8, 35..38], [16, 0, 115..118], [16, 8, 121..124]]} */
static uint8_t op_module[] = {
  0x55,                                     /*0: push   %rbp */
  0x53,                                     /*1: push   %rbx */
  0x48, 0x89, 0xfb,                         /*2: mov    %rdi,%rbx */
  0x48, 0x8b, 0x53, 0x18,                   /*5: mov    0x18(%rbx),%rdx */
  0x48, 0x8b, 0x43, 0x28,                   /*9: mov    0x28(%rbx),%rax */
  0x8b, 0xa8, 0x00, 0x04, 0xbc, 0x00,       /*d: mov    0xbc0400(%rax),%ebp */
  0x48, 0x8b, 0x82, 0x00, 0x10, 0xab, 0x00, /*13: mov    0xab1000(%rdx),%rax */
  0x48, 0x89, 0xc1,                         /*1a: mov    %rax,%rcx */
  0x48, 0xc1, 0xe9, 0x20,                   /*1d: shr    $0x20,%rcx */
  0x8b, 0x92, 0x08, 0x10, 0xab, 0x00,       /*21: mov    0xab1008(%rdx),%edx */
  0x85, 0xd2,                               /*27: test   %edx,%edx */
  0x75, 0x23,                               /*29: jne    4e <op_module+0x4e> */
  0x31, 0xd2,                               /*2b: xor    %edx,%edx */
  0x85, 0xc0,                               /*2d: test   %eax,%eax */
  0x75, 0x1d,                               /*2f: jne    4e <op_module+0x4e> */
  0x48, 0x8b, 0x43, 0x58,                   /*31: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*35: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x40, 0x20,                   /*39: mov    0x20(%rax),%rax */
  0x48, 0x8b, 0x78, 0x48,                   /*3d: mov    0x48(%rax),%rdi */
  0xff, 0x93, 0x48, 0x02, 0x00, 0x00,       /*41: callq  *0x248(%rbx) */
  0x48, 0x89, 0xc1,                         /*47: mov    %rax,%rcx */
  0x48, 0xc1, 0xe9, 0x20,                   /*4a: shr    $0x20,%rcx */
  0x48, 0x8b, 0x7b, 0x58,                   /*4e: mov    0x58(%rbx),%rdi */
  0x48, 0xc1, 0xe1, 0x20,                   /*52: shl    $0x20,%rcx */
  0x89, 0xc6,                               /*56: mov    %eax,%esi */
  0x48, 0x09, 0xce,                         /*58: or     %rcx,%rsi */
  0x89, 0xe9,                               /*5b: mov    %ebp,%ecx */
  0xff, 0x93, 0xb8, 0x00, 0x00, 0x00,       /*5d: callq  *0xb8(%rbx) */
  0x48, 0x8b, 0x6b, 0x18,                   /*63: mov    0x18(%rbx),%rbp */
  0x48, 0x89, 0xc7,                         /*67: mov    %rax,%rdi */
  0xff, 0x93, 0x48, 0x02, 0x00, 0x00,       /*6a: callq  *0x248(%rbx) */
  0x48, 0x89, 0x85, 0x00, 0x10, 0xab, 0x00, /*70: mov    %rax,0xab1000(%rbp) */
  0x89, 0x95, 0x08, 0x10, 0xab, 0x00,       /*77: mov    %edx,0xab1008(%rbp) */
  0x8b, 0x43, 0x50,                         /*7d: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*80: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*84: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*8a: mov    %rbx,%rdi */
  0x5b,                                     /*8d: pop    %rbx */
  0x5d,                                     /*8e: pop    %rbp */

};

static void op_module_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 4 + 0;
  *((int32_t *)(op + 22)) = a * 16 + 0;
  *((int32_t *)(op + 35)) = a * 16 + 8;
  *((int32_t *)(op + 115)) = a * 16 + 0;
  *((int32_t *)(op + 121)) = a * 16 + 8;
}

static void op_module_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_module_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 22..25], [16, 8, 29..32], [1, 0, 58..61], [16, 0, 109..112]], "b"=>[[8, 0, 128..131]]} */
static uint8_t op_exec[] = {
  0x55,                                     /*0: push   %rbp */
  0x41, 0x57,                               /*1: push   %r15 */
  0x41, 0x56,                               /*3: push   %r14 */
  0x41, 0x54,                               /*5: push   %r12 */
  0x53,                                     /*7: push   %rbx */
  0x48, 0x89, 0xfb,                         /*8: mov    %rdi,%rbx */
  0x48, 0x8b, 0x43, 0x18,                   /*b: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x7b, 0x58,                   /*f: mov    0x58(%rbx),%rdi */
  0x4c, 0x8b, 0xb8, 0x00, 0x10, 0xab, 0x00, /*13: mov    0xab1000(%rax),%r15 */
  0x44, 0x8b, 0xa0, 0x08, 0x10, 0xab, 0x00, /*1a: mov    0xab1008(%rax),%r12d */
  0xff, 0x93, 0xf0, 0x00, 0x00, 0x00,       /*21: callq  *0xf0(%rbx) */
  0x49, 0x89, 0xc6,                         /*27: mov    %rax,%r14 */
  0x48, 0x8b, 0x43, 0x10,                   /*2a: mov    0x10(%rbx),%rax */
  0x48, 0x83, 0xc0, 0x04,                   /*2e: add    $0x4,%rax */
  0x49, 0x89, 0x46, 0x30,                   /*32: mov    %rax,0x30(%r14) */
  0x41, 0xc7, 0x46, 0x44, 0x00, 0x00, 0xab, 0x00,/*36: movl   $0xab0000,0x44(%r14) */
  0x41, 0xc7, 0x06, 0x00, 0x00, 0x00, 0x00, /*3e: movl   $0x0,(%r14) */
  0x48, 0x8b, 0x43, 0x58,                   /*45: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*49: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x40, 0x08,                   /*4d: mov    0x8(%rax),%rax */
  0x49, 0x89, 0x46, 0x10,                   /*51: mov    %rax,0x10(%r14) */
  0x41, 0xc7, 0x46, 0x40, 0x00, 0x00, 0x00, 0x00,/*55: movl   $0x0,0x40(%r14) */
  0x4d, 0x89, 0x7e, 0x48,                   /*5d: mov    %r15,0x48(%r14) */
  0x48, 0x8b, 0x43, 0x58,                   /*61: mov    0x58(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x18,                   /*65: mov    0x18(%rax),%rax */
  0x48, 0x81, 0x40, 0x08, 0x00, 0x10, 0xab, 0x00,/*69: addq   $0xab1000,0x8(%rax) */
  0x48, 0x8b, 0x43, 0x08,                   /*71: mov    0x8(%rbx),%rax */
  0x48, 0x8b, 0x7b, 0x58,                   /*75: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x40, 0x20,                   /*79: mov    0x20(%rax),%rax */
  0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*7d: mov    0xbc0800(%rax),%rsi */
  0xff, 0x93, 0x58, 0x01, 0x00, 0x00,       /*84: callq  *0x158(%rbx) */
  0x48, 0x89, 0xc5,                         /*8a: mov    %rax,%rbp */
  0x49, 0x8b, 0x46, 0x48,                   /*8d: mov    0x48(%r14),%rax */
  0x48, 0x89, 0x45, 0x20,                   /*91: mov    %rax,0x20(%rbp) */
  0x49, 0x89, 0x6e, 0x08,                   /*95: mov    %rbp,0x8(%r14) */
  0xf6, 0x45, 0x02, 0x04,                   /*99: testb  $0x4,0x2(%rbp) */
  0x74, 0x47,                               /*9d: je     e6 <op_exec+0xe6> */
  0x41, 0xc7, 0x46, 0x18, 0x00, 0x00, 0x00, 0x00,/*9f: movl   $0x0,0x18(%r14) */
  0x48, 0x8b, 0x7b, 0x58,                   /*a7: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x47, 0x18,                   /*ab: mov    0x18(%rdi),%rax */
  0x4c, 0x8b, 0x70, 0x08,                   /*af: mov    0x8(%rax),%r14 */
  0x4c, 0x89, 0xfe,                         /*b3: mov    %r15,%rsi */
  0x44, 0x89, 0xe2,                         /*b6: mov    %r12d,%edx */
  0xff, 0x55, 0x18,                         /*b9: callq  *0x18(%rbp) */
  0x49, 0x89, 0x06,                         /*bc: mov    %rax,(%r14) */
  0x41, 0x89, 0x56, 0x08,                   /*bf: mov    %edx,0x8(%r14) */
  0x48, 0x8b, 0x7b, 0x58,                   /*c3: mov    0x58(%rbx),%rdi */
  0x8b, 0x73, 0x50,                         /*c7: mov    0x50(%rbx),%esi */
  0xff, 0x93, 0x18, 0x01, 0x00, 0x00,       /*ca: callq  *0x118(%rbx) */
  0x48, 0x8b, 0x43, 0x58,                   /*d0: mov    0x58(%rbx),%rax */
  0x48, 0x83, 0x78, 0x28, 0x00,             /*d4: cmpq   $0x0,0x28(%rax) */
  0x74, 0x6c,                               /*d9: je     147 <op_exec+0x147> */
  0x48, 0x89, 0xdf,                         /*db: mov    %rbx,%rdi */
  0xff, 0x93, 0xd0, 0x01, 0x00, 0x00,       /*de: callq  *0x1d0(%rbx) */
  0xeb, 0x7f,                               /*e4: jmp    165 <op_exec+0x165> */
  0x48, 0x8b, 0x45, 0x18,                   /*e6: mov    0x18(%rbp),%rax */
  0x48, 0x89, 0x43, 0x08,                   /*ea: mov    %rax,0x8(%rbx) */
  0x48, 0x8b, 0x48, 0x10,                   /*ee: mov    0x10(%rax),%rcx */
  0x48, 0x89, 0x4b, 0x20,                   /*f2: mov    %rcx,0x20(%rbx) */
  0x48, 0x8b, 0x48, 0x18,                   /*f6: mov    0x18(%rax),%rcx */
  0x48, 0x89, 0x4b, 0x28,                   /*fa: mov    %rcx,0x28(%rbx) */
  0x48, 0x8b, 0x7b, 0x58,                   /*fe: mov    0x58(%rbx),%rdi */
  0x0f, 0xb7, 0x70, 0x02,                   /*102: movzwl 0x2(%rax),%esi */
  0xba, 0x01, 0x00, 0x00, 0x00,             /*106: mov    $0x1,%edx */
  0xff, 0x93, 0x50, 0x01, 0x00, 0x00,       /*10b: callq  *0x150(%rbx) */
  0x48, 0x8b, 0x43, 0x08,                   /*111: mov    0x8(%rbx),%rax */
  0x0f, 0xb7, 0x40, 0x02,                   /*115: movzwl 0x2(%rax),%eax */
  0x41, 0x89, 0x46, 0x18,                   /*119: mov    %eax,0x18(%r14) */
  0x48, 0x8b, 0x7b, 0x58,                   /*11d: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x47, 0x18,                   /*121: mov    0x18(%rdi),%rax */
  0x48, 0x8b, 0x40, 0x08,                   /*125: mov    0x8(%rax),%rax */
  0x48, 0x89, 0x43, 0x18,                   /*129: mov    %rax,0x18(%rbx) */
  0x48, 0x8b, 0x43, 0x08,                   /*12d: mov    0x8(%rbx),%rax */
  0x48, 0x8b, 0x40, 0x08,                   /*131: mov    0x8(%rax),%rax */
  0x48, 0x89, 0x43, 0x10,                   /*135: mov    %rax,0x10(%rbx) */
  0x48, 0x89, 0xee,                         /*139: mov    %rbp,%rsi */
  0x48, 0x89, 0xda,                         /*13c: mov    %rbx,%rdx */
  0xff, 0x93, 0x28, 0x01, 0x00, 0x00,       /*13f: callq  *0x128(%rbx) */
  0xeb, 0x1e,                               /*145: jmp    165 <op_exec+0x165> */
  0x48, 0x8b, 0x40, 0x18,                   /*147: mov    0x18(%rax),%rax */
  0x48, 0x8b, 0x48, 0x20,                   /*14b: mov    0x20(%rax),%rcx */
  0x48, 0x8b, 0x49, 0x10,                   /*14f: mov    0x10(%rcx),%rcx */
  0x48, 0x89, 0x48, 0x08,                   /*153: mov    %rcx,0x8(%rax) */
  0x48, 0x89, 0x4b, 0x18,                   /*157: mov    %rcx,0x18(%rbx) */
  0x48, 0x8b, 0x7b, 0x58,                   /*15b: mov    0x58(%rbx),%rdi */
  0xff, 0x93, 0x68, 0x01, 0x00, 0x00,       /*15f: callq  *0x168(%rbx) */
  0x48, 0x89, 0xdf,                         /*165: mov    %rbx,%rdi */
  0x5b,                                     /*168: pop    %rbx */
  0x41, 0x5c,                               /*169: pop    %r12 */
  0x41, 0x5e,                               /*16b: pop    %r14 */
  0x41, 0x5f,                               /*16d: pop    %r15 */
  0x5d,                                     /*16f: pop    %rbp */

};

static void op_exec_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 22)) = a * 16 + 0;
  *((int32_t *)(op + 29)) = a * 16 + 8;
  *((int32_t *)(op + 58)) = a * 1 + 0;
  *((int32_t *)(op + 109)) = a * 16 + 0;
  *((int32_t *)(op + 128)) = b * 8 + 0;
}

static void op_exec_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_exec_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[16, 0, 25..28], [16, 16, 32..35], [16, 24, 39..42]]} */
static uint8_t op_method[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0x7b, 0x58,                   /*4: mov    0x58(%rbx),%rdi */
  0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
  0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
  0x8b, 0x91, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%edx */
  0x48, 0x8b, 0xb0, 0x00, 0x10, 0xab, 0x00, /*16: mov    0xab1000(%rax),%rsi */
  0x48, 0x8b, 0x88, 0x10, 0x10, 0xab, 0x00, /*1d: mov    0xab1010(%rax),%rcx */
  0x44, 0x8b, 0x80, 0x18, 0x10, 0xab, 0x00, /*24: mov    0xab1018(%rax),%r8d */
  0xff, 0x93, 0xf8, 0x00, 0x00, 0x00,       /*2b: callq  *0xf8(%rbx) */
  0x8b, 0x43, 0x50,                         /*31: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*34: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*38: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*3e: mov    %rbx,%rdi */
  0x5b,                                     /*41: pop    %rbx */

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


/* args: {"b"=>[[16, 0, 17..20], [16, 8, 24..27]], "a"=>[[16, 0, 37..40], [16, 8, 44..47]]} */
static uint8_t op_sclass[] = {
  0x41, 0x56,                               /*0: push   %r14 */
  0x53,                                     /*2: push   %rbx */
  0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
  0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
  0x48, 0x8b, 0x7b, 0x58,                   /*a: mov    0x58(%rbx),%rdi */
  0x49, 0x8b, 0xb6, 0x00, 0x10, 0xbc, 0x00, /*e: mov    0xbc1000(%r14),%rsi */
  0x41, 0x8b, 0x96, 0x08, 0x10, 0xbc, 0x00, /*15: mov    0xbc1008(%r14),%edx */
  0xff, 0x93, 0xd0, 0x00, 0x00, 0x00,       /*1c: callq  *0xd0(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*22: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*29: mov    %edx,0xab1008(%r14) */
  0x8b, 0x43, 0x50,                         /*30: mov    0x50(%rbx),%eax */
  0x48, 0x8b, 0x4b, 0x58,                   /*33: mov    0x58(%rbx),%rcx */
  0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*37: mov    %eax,0xdc(%rcx) */
  0x48, 0x89, 0xdf,                         /*3d: mov    %rbx,%rdi */
  0x5b,                                     /*40: pop    %rbx */
  0x41, 0x5e,                               /*41: pop    %r14 */

};

static void op_sclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 16 + 0;
  *((int32_t *)(op + 24)) = b * 16 + 8;
  *((int32_t *)(op + 37)) = a * 16 + 0;
  *((int32_t *)(op + 44)) = a * 16 + 8;
}

static void op_sclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sclass_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[16, 0, 42..45], [16, 8, 49..52]]} */
static uint8_t op_tclass[] = {
  0x41, 0x57,                               /*0: push   %r15 */
  0x41, 0x56,                               /*2: push   %r14 */
  0x53,                                     /*4: push   %rbx */
  0x48, 0x89, 0xfb,                         /*5: mov    %rdi,%rbx */
  0x4c, 0x8b, 0x73, 0x58,                   /*8: mov    0x58(%rbx),%r14 */
  0x49, 0x8b, 0x46, 0x18,                   /*c: mov    0x18(%r14),%rax */
  0x48, 0x8b, 0x40, 0x20,                   /*10: mov    0x20(%rax),%rax */
  0x48, 0x8b, 0x78, 0x48,                   /*14: mov    0x48(%rax),%rdi */
  0x48, 0x85, 0xff,                         /*18: test   %rdi,%rdi */
  0x74, 0x1a,                               /*1b: je     37 <op_tclass+0x37> */
  0x4c, 0x8b, 0x73, 0x18,                   /*1d: mov    0x18(%rbx),%r14 */
  0xff, 0x93, 0x48, 0x02, 0x00, 0x00,       /*21: callq  *0x248(%rbx) */
  0x49, 0x89, 0x86, 0x00, 0x10, 0xab, 0x00, /*27: mov    %rax,0xab1000(%r14) */
  0x41, 0x89, 0x96, 0x08, 0x10, 0xab, 0x00, /*2e: mov    %edx,0xab1008(%r14) */
  0xeb, 0x4d,                               /*35: jmp    84 <op_tclass+0x84> */
  0x48, 0x8b, 0x83, 0xb8, 0x01, 0x00, 0x00, /*37: mov    0x1b8(%rbx),%rax */
  0x48, 0x8b, 0x30,                         /*3e: mov    (%rax),%rsi */
  0x4c, 0x89, 0xf7,                         /*41: mov    %r14,%rdi */
  0xff, 0x93, 0x20, 0x01, 0x00, 0x00,       /*44: callq  *0x120(%rbx) */
  0x49, 0x89, 0xc7,                         /*4a: mov    %rax,%r15 */
  0x48, 0x8b, 0xb3, 0xd8, 0x02, 0x00, 0x00, /*4d: mov    0x2d8(%rbx),%rsi */
  0xba, 0x19, 0x00, 0x00, 0x00,             /*54: mov    $0x19,%edx */
  0x4c, 0x89, 0xf7,                         /*59: mov    %r14,%rdi */
  0xff, 0x93, 0x80, 0x00, 0x00, 0x00,       /*5c: callq  *0x80(%rbx) */
  0x89, 0xd1,                               /*62: mov    %edx,%ecx */
  0x4c, 0x89, 0xf7,                         /*64: mov    %r14,%rdi */
  0x4c, 0x89, 0xfe,                         /*67: mov    %r15,%rsi */
  0x48, 0x89, 0xc2,                         /*6a: mov    %rax,%rdx */
  0xff, 0x93, 0xe0, 0x01, 0x00, 0x00,       /*6d: callq  *0x1e0(%rbx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*73: mov    0x58(%rbx),%rcx */
  0x48, 0x89, 0x41, 0x28,                   /*77: mov    %rax,0x28(%rcx) */
  0x48, 0x89, 0xdf,                         /*7b: mov    %rbx,%rdi */
  0xff, 0x93, 0xd0, 0x01, 0x00, 0x00,       /*7e: callq  *0x1d0(%rbx) */
  0x48, 0x89, 0xdf,                         /*84: mov    %rbx,%rdi */
  0x5b,                                     /*87: pop    %rbx */
  0x41, 0x5e,                               /*88: pop    %r14 */
  0x41, 0x5f,                               /*8a: pop    %r15 */

};

static void op_tclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 42)) = a * 16 + 0;
  *((int32_t *)(op + 49)) = a * 16 + 8;
}

static void op_tclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_tclass_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 12..15]], "b"=>[[1, 0, 17..20]], "c"=>[[1, 0, 22..25]]} */
static uint8_t op_debug[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0x48, 0x8b, 0xbb, 0xe0, 0x02, 0x00, 0x00, /*4: mov    0x2e0(%rbx),%rdi */
  0xbe, 0x00, 0x00, 0xab, 0x00,             /*b: mov    $0xab0000,%esi */
  0xba, 0x00, 0x00, 0xbc, 0x00,             /*10: mov    $0xbc0000,%edx */
  0xb9, 0x00, 0x00, 0xcd, 0x00,             /*15: mov    $0xcd0000,%ecx */
  0x31, 0xc0,                               /*1a: xor    %eax,%eax */
  0xff, 0x93, 0x80, 0x01, 0x00, 0x00,       /*1c: callq  *0x180(%rbx) */
  0x48, 0x89, 0xdf,                         /*22: mov    %rbx,%rdi */
  0x5b,                                     /*25: pop    %rbx */

};

static void op_debug_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 12)) = a * 1 + 0;
  *((int32_t *)(op + 17)) = b * 1 + 0;
  *((int32_t *)(op + 22)) = c * 1 + 0;
}

static void op_debug_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_debug_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_stop[] = {
  0x53,                                     /*0: push   %rbx */
  0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
  0xff, 0x93, 0xf0, 0x01, 0x00, 0x00,       /*4: callq  *0x1f0(%rbx) */
  0x48, 0x89, 0xdf,                         /*a: mov    %rbx,%rdi */
  0x5b,                                     /*d: pop    %rbx */

};

static void op_stop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_stop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_stop_set_args(op, 0,0,0,op_idx);
}


/* args: {"b"=>[[16, 0, 20..23], [16, 8, 26..29]]} */
static uint8_t op_err[] = {
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
  0xff, 0x93, 0xe8, 0x01, 0x00, 0x00,       /*21: callq  *0x1e8(%rbx) */
  0x49, 0x89, 0xc7,                         /*27: mov    %rax,%r15 */
  0x89, 0xd5,                               /*2a: mov    %edx,%ebp */
  0x48, 0x8b, 0x83, 0x88, 0x02, 0x00, 0x00, /*2c: mov    0x288(%rbx),%rax */
  0x48, 0x8b, 0x30,                         /*33: mov    (%rax),%rsi */
  0x4c, 0x89, 0xf7,                         /*36: mov    %r14,%rdi */
  0xff, 0x93, 0x20, 0x01, 0x00, 0x00,       /*39: callq  *0x120(%rbx) */
  0x4c, 0x89, 0xf7,                         /*3f: mov    %r14,%rdi */
  0x48, 0x89, 0xc6,                         /*42: mov    %rax,%rsi */
  0x4c, 0x89, 0xfa,                         /*45: mov    %r15,%rdx */
  0x89, 0xe9,                               /*48: mov    %ebp,%ecx */
  0xff, 0x93, 0xe0, 0x01, 0x00, 0x00,       /*4a: callq  *0x1e0(%rbx) */
  0x48, 0x8b, 0x4b, 0x58,                   /*50: mov    0x58(%rbx),%rcx */
  0x48, 0x89, 0x41, 0x28,                   /*54: mov    %rax,0x28(%rcx) */
  0x48, 0x89, 0xdf,                         /*58: mov    %rbx,%rdi */
  0xff, 0x93, 0xd0, 0x01, 0x00, 0x00,       /*5b: callq  *0x1d0(%rbx) */
  0x48, 0x89, 0xdf,                         /*61: mov    %rbx,%rdi */
  0x5b,                                     /*64: pop    %rbx */
  0x41, 0x5e,                               /*65: pop    %r14 */
  0x41, 0x5f,                               /*67: pop    %r15 */
  0x5d,                                     /*69: pop    %rbp */

};

static void op_err_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 16 + 0;
  *((int32_t *)(op + 26)) = b * 16 + 8;
}

static void op_err_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_err_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}

typedef void (*jit_args_func_t)(uint8_t *op, mrb_code c, uint32_t op_idx);
static jit_args_func_t arg_funcs[76];
uint8_t* ops[76];
static char *op_names[76];
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
  sizeof(op_getglobal), /* 50 */
  sizeof(op_setglobal), /* 45 */
  sizeof(op_getspecial), /* 45 */
  sizeof(op_setspecial), /* 40 */
  sizeof(op_getiv), /* 50 */
  sizeof(op_setiv), /* 45 */
  sizeof(op_getcv), /* 90 */
  sizeof(op_setcv), /* 45 */
  sizeof(op_getconst), /* 97 */
  sizeof(op_setconst), /* 45 */
  sizeof(op_getmcnst), /* 114 */
  sizeof(op_setmcnst), /* 59 */
  sizeof(op_getupvar), /* 90 */
  sizeof(op_setupvar), /* 77 */
  sizeof(op_jmp), /* 0 */
  sizeof(op_jmpif), /* 11 */
  sizeof(op_jmpnot), /* 11 */
  sizeof(op_onerr), /* 146 */
  sizeof(op_rescue), /* 52 */
  sizeof(op_poperr), /* 35 */
  sizeof(op_raise), /* 33 */
  sizeof(op_epush), /* 169 */
  sizeof(op_epop), /* 86 */
  sizeof(op_send), /* 89 */
  sizeof(op_sendb), /* 35 */
  sizeof(op_fsend), /* 0 */
  sizeof(op_call), /* 19 */
  sizeof(op_super), /* 547 */
  sizeof(op_argary), /* 541 */
  sizeof(op_enter), /* 1115 */
  sizeof(op_karg), /* 0 */
  sizeof(op_kdict), /* 0 */
  sizeof(op_return), /* 42 */
  sizeof(op_tailcall), /* 460 */
  sizeof(op_blkpush), /* 189 */
  sizeof(op_add), /* 375 */
  sizeof(op_addi), /* 187 */
  sizeof(op_sub), /* 265 */
  sizeof(op_subi), /* 187 */
  sizeof(op_mul), /* 258 */
  sizeof(op_div), /* 205 */
  sizeof(op_eq), /* 266 */
  sizeof(op_lt), /* 218 */
  sizeof(op_le), /* 218 */
  sizeof(op_gt), /* 218 */
  sizeof(op_ge), /* 218 */
  sizeof(op_array), /* 65 */
  sizeof(op_arycat), /* 89 */
  sizeof(op_arypush), /* 49 */
  sizeof(op_aref), /* 92 */
  sizeof(op_aset), /* 54 */
  sizeof(op_apost), /* 447 */
  sizeof(op_string), /* 70 */
  sizeof(op_strcat), /* 49 */
  sizeof(op_hash), /* 210 */
  sizeof(op_lambda), /* 29 */
  sizeof(op_range), /* 87 */
  sizeof(op_oclass), /* 44 */
  sizeof(op_class), /* 173 */
  sizeof(op_module), /* 143 */
  sizeof(op_exec), /* 368 */
  sizeof(op_method), /* 66 */
  sizeof(op_sclass), /* 67 */
  sizeof(op_tclass), /* 140 */
  sizeof(op_debug), /* 38 */
  sizeof(op_stop), /* 14 */
  sizeof(op_err), /* 106 */

};

extern void init_symtbl();
void init_ops() {
  static int init = 0;
  if(init == 0) {
    init = 1;
    init_symtbl();
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
    ops[39] = op_karg;
    op_names[39] = "op_karg";
    arg_funcs[39] = op_karg_set_args_from_code;
    ops[40] = op_kdict;
    op_names[40] = "op_kdict";
    arg_funcs[40] = op_kdict_set_args_from_code;
    ops[41] = op_return;
    op_names[41] = "op_return";
    arg_funcs[41] = op_return_set_args_from_code;
    ops[42] = op_tailcall;
    op_names[42] = "op_tailcall";
    arg_funcs[42] = op_tailcall_set_args_from_code;
    ops[43] = op_blkpush;
    op_names[43] = "op_blkpush";
    arg_funcs[43] = op_blkpush_set_args_from_code;
    ops[44] = op_add;
    op_names[44] = "op_add";
    arg_funcs[44] = op_add_set_args_from_code;
    ops[45] = op_addi;
    op_names[45] = "op_addi";
    arg_funcs[45] = op_addi_set_args_from_code;
    ops[46] = op_sub;
    op_names[46] = "op_sub";
    arg_funcs[46] = op_sub_set_args_from_code;
    ops[47] = op_subi;
    op_names[47] = "op_subi";
    arg_funcs[47] = op_subi_set_args_from_code;
    ops[48] = op_mul;
    op_names[48] = "op_mul";
    arg_funcs[48] = op_mul_set_args_from_code;
    ops[49] = op_div;
    op_names[49] = "op_div";
    arg_funcs[49] = op_div_set_args_from_code;
    ops[50] = op_eq;
    op_names[50] = "op_eq";
    arg_funcs[50] = op_eq_set_args_from_code;
    ops[51] = op_lt;
    op_names[51] = "op_lt";
    arg_funcs[51] = op_lt_set_args_from_code;
    ops[52] = op_le;
    op_names[52] = "op_le";
    arg_funcs[52] = op_le_set_args_from_code;
    ops[53] = op_gt;
    op_names[53] = "op_gt";
    arg_funcs[53] = op_gt_set_args_from_code;
    ops[54] = op_ge;
    op_names[54] = "op_ge";
    arg_funcs[54] = op_ge_set_args_from_code;
    ops[55] = op_array;
    op_names[55] = "op_array";
    arg_funcs[55] = op_array_set_args_from_code;
    ops[56] = op_arycat;
    op_names[56] = "op_arycat";
    arg_funcs[56] = op_arycat_set_args_from_code;
    ops[57] = op_arypush;
    op_names[57] = "op_arypush";
    arg_funcs[57] = op_arypush_set_args_from_code;
    ops[58] = op_aref;
    op_names[58] = "op_aref";
    arg_funcs[58] = op_aref_set_args_from_code;
    ops[59] = op_aset;
    op_names[59] = "op_aset";
    arg_funcs[59] = op_aset_set_args_from_code;
    ops[60] = op_apost;
    op_names[60] = "op_apost";
    arg_funcs[60] = op_apost_set_args_from_code;
    ops[61] = op_string;
    op_names[61] = "op_string";
    arg_funcs[61] = op_string_set_args_from_code;
    ops[62] = op_strcat;
    op_names[62] = "op_strcat";
    arg_funcs[62] = op_strcat_set_args_from_code;
    ops[63] = op_hash;
    op_names[63] = "op_hash";
    arg_funcs[63] = op_hash_set_args_from_code;
    ops[64] = op_lambda;
    op_names[64] = "op_lambda";
    arg_funcs[64] = op_lambda_set_args_from_code;
    ops[65] = op_range;
    op_names[65] = "op_range";
    arg_funcs[65] = op_range_set_args_from_code;
    ops[66] = op_oclass;
    op_names[66] = "op_oclass";
    arg_funcs[66] = op_oclass_set_args_from_code;
    ops[67] = op_class;
    op_names[67] = "op_class";
    arg_funcs[67] = op_class_set_args_from_code;
    ops[68] = op_module;
    op_names[68] = "op_module";
    arg_funcs[68] = op_module_set_args_from_code;
    ops[69] = op_exec;
    op_names[69] = "op_exec";
    arg_funcs[69] = op_exec_set_args_from_code;
    ops[70] = op_method;
    op_names[70] = "op_method";
    arg_funcs[70] = op_method_set_args_from_code;
    ops[71] = op_sclass;
    op_names[71] = "op_sclass";
    arg_funcs[71] = op_sclass_set_args_from_code;
    ops[72] = op_tclass;
    op_names[72] = "op_tclass";
    arg_funcs[72] = op_tclass_set_args_from_code;
    ops[73] = op_debug;
    op_names[73] = "op_debug";
    arg_funcs[73] = op_debug_set_args_from_code;
    ops[74] = op_stop;
    op_names[74] = "op_stop";
    arg_funcs[74] = op_stop_set_args_from_code;
    ops[75] = op_err;
    op_names[75] = "op_err";
    arg_funcs[75] = op_err_set_args_from_code;
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
