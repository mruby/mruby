
/* args: {} */
static uint8_t op_nop[] = {

};
static void op_nop_link(uint8_t *op) {
}

static void op_nop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_nop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_nop_set_args(op, 0,0,0,op_idx);
}


/* args: {"b"=>[[8, 0, 7..10]], "a"=>[[8, 0, 14..17]]} */
static uint8_t op_move[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x88, 0x00, 0x08, 0xbc, 0x00, /*4: mov    0xbc0800(%rax),%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*b: mov    %rcx,0xab0800(%rax) */

};
static void op_move_link(uint8_t *op) {
}

static void op_move_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = b * 8 + 0;
  *((int32_t *)(op + 14)) = a * 8 + 0;
}

static void op_move_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_move_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[8, 0, 11..14]], "a"=>[[8, 0, 18..21]]} */
static uint8_t op_loadl[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x20,                   /*4: mov    0x20(%rdi),%rcx */
0x48, 0x8b, 0x89, 0x00, 0x08, 0xbc, 0x00, /*8: mov    0xbc0800(%rcx),%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*f: mov    %rcx,0xab0800(%rax) */

};
static void op_loadl_link(uint8_t *op) {
}

static void op_loadl_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 11)) = b * 8 + 0;
  *((int32_t *)(op + 18)) = a * 8 + 0;
}

static void op_loadl_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadl_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 7..10], [8, 0, 21..24], [8, 0, 36..39], [8, 0, 46..49]]} */
static uint8_t op_loadi[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movq   $0x0,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*f: mov    0x18(%rdi),%rax */
0x8b, 0x88, 0x00, 0x08, 0xab, 0x00,       /*13: mov    0xab0800(%rax),%ecx */
0x83, 0xe1, 0x01,                         /*19: and    $0x1,%ecx */
0x81, 0xc9, 0x00, 0x00, 0x78, 0x01,       /*1c: or     $0x1780000,%ecx */
0x89, 0x88, 0x00, 0x08, 0xab, 0x00,       /*22: mov    %ecx,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*28: mov    0x18(%rdi),%rax */
0x83, 0x88, 0x00, 0x08, 0xab, 0x00, 0x01, /*2c: orl    $0x1,0xab0800(%rax) */

};
static void op_loadi_link(uint8_t *op) {
}

static void op_loadi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 0;
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 36)) = a * 8 + 0;
  *((int32_t *)(op + 46)) = a * 8 + 0;
}

static void op_loadi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadi_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 7..10], [8, 4, 31..34], [8, 0, 41..44]], "b"=>[[4, 0, 25..28]]} */
static uint8_t op_loadsym[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movq   $0x0,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*f: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x28,                   /*13: mov    0x28(%rdi),%rcx */
0x8b, 0x89, 0x00, 0x04, 0xbc, 0x00,       /*17: mov    0xbc0400(%rcx),%ecx */
0x89, 0x88, 0x04, 0x08, 0xab, 0x00,       /*1d: mov    %ecx,0xab0804(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*23: mov    0x18(%rdi),%rax */
0xc6, 0x80, 0x00, 0x08, 0xab, 0x00, 0x0e, /*27: movb   $0xe,0xab0800(%rax) */

};
static void op_loadsym_link(uint8_t *op) {
}

static void op_loadsym_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 0;
  *((int32_t *)(op + 31)) = a * 8 + 4;
  *((int32_t *)(op + 41)) = a * 8 + 0;
  *((int32_t *)(op + 25)) = b * 4 + 0;
}

static void op_loadsym_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadsym_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 7..10], [8, 0, 21..24], [8, 0, 33..36]]} */
static uint8_t op_loadnil[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movq   $0x0,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*f: mov    0x18(%rdi),%rax */
0x83, 0xa0, 0x00, 0x08, 0xab, 0x00, 0x01, /*13: andl   $0x1,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*1a: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*1e: movq   $0x0,0xab0800(%rax) */

};
static void op_loadnil_link(uint8_t *op) {
}

static void op_loadnil_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 0;
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 33)) = a * 8 + 0;
}

static void op_loadnil_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadnil_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 10..13]]} */
static uint8_t op_loadself[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x08,                         /*4: mov    (%rax),%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*7: mov    %rcx,0xab0800(%rax) */

};
static void op_loadself_link(uint8_t *op) {
}

static void op_loadself_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 8 + 0;
}

static void op_loadself_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadself_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 7..10], [8, 0, 21..24], [8, 0, 33..36], [8, 0, 44..47]]} */
static uint8_t op_loadt[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movq   $0x0,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*f: mov    0x18(%rdi),%rax */
0x8b, 0x88, 0x00, 0x08, 0xab, 0x00,       /*13: mov    0xab0800(%rax),%ecx */
0x83, 0xe1, 0x01,                         /*19: and    $0x1,%ecx */
0x83, 0xc9, 0x02,                         /*1c: or     $0x2,%ecx */
0x89, 0x88, 0x00, 0x08, 0xab, 0x00,       /*1f: mov    %ecx,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*25: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*29: movq   $0x4,0xab0800(%rax) */

};
static void op_loadt_link(uint8_t *op) {
}

static void op_loadt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 0;
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 33)) = a * 8 + 0;
  *((int32_t *)(op + 44)) = a * 8 + 0;
}

static void op_loadt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadt_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 7..10], [8, 0, 21..24], [8, 0, 33..36], [8, 0, 44..47]]} */
static uint8_t op_loadf[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movq   $0x0,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*f: mov    0x18(%rdi),%rax */
0x8b, 0x88, 0x00, 0x08, 0xab, 0x00,       /*13: mov    0xab0800(%rax),%ecx */
0x83, 0xe1, 0x01,                         /*19: and    $0x1,%ecx */
0x83, 0xc9, 0x02,                         /*1c: or     $0x2,%ecx */
0x89, 0x88, 0x00, 0x08, 0xab, 0x00,       /*1f: mov    %ecx,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*25: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*29: movq   $0x2,0xab0800(%rax) */

};
static void op_loadf_link(uint8_t *op) {
}

static void op_loadf_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 0;
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 33)) = a * 8 + 0;
  *((int32_t *)(op + 44)) = a * 8 + 0;
}

static void op_loadf_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_loadf_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[8, 0, 32..35]]} */
static uint8_t op_getglobal[] = {
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
static void op_getglobal_link(uint8_t *op) {
  *((int32_t *)(op + 25)) = (uint32_t)(((uint8_t *)mrb_gv_get) + (0) - (op + 25));
}

static void op_getglobal_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 32)) = a * 8 + 0;
}

static void op_getglobal_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getglobal_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28]]} */
static uint8_t op_setglobal[] = {
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
static void op_setglobal_link(uint8_t *op) {
  *((int32_t *)(op + 30)) = (uint32_t)(((uint8_t *)mrb_gv_set) + (0) - (op + 30));
}

static void op_setglobal_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
}

static void op_setglobal_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setglobal_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[1, 0, 15..18]], "a"=>[[8, 0, 27..30]]} */
static uint8_t op_getspecial[] = {
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
static void op_getspecial_link(uint8_t *op) {
  *((int32_t *)(op + 20)) = (uint32_t)(((uint8_t *)mrb_vm_special_get) + (0) - (op + 20));
}

static void op_getspecial_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 1 + 0;
  *((int32_t *)(op + 27)) = a * 8 + 0;
}

static void op_getspecial_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getspecial_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18]], "b"=>[[1, 0, 20..23]]} */
static uint8_t op_setspecial[] = {
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
static void op_setspecial_link(uint8_t *op) {
  *((int32_t *)(op + 25)) = (uint32_t)(((uint8_t *)mrb_vm_special_set) + (0) - (op + 25));
}

static void op_setspecial_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
  *((int32_t *)(op + 20)) = b * 1 + 0;
}

static void op_setspecial_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setspecial_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 20..23]], "a"=>[[8, 0, 32..35]]} */
static uint8_t op_getiv[] = {
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
static void op_getiv_link(uint8_t *op) {
  *((int32_t *)(op + 25)) = (uint32_t)(((uint8_t *)mrb_vm_iv_get) + (0) - (op + 25));
}

static void op_getiv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = b * 4 + 0;
  *((int32_t *)(op + 32)) = a * 8 + 0;
}

static void op_getiv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getiv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28]]} */
static uint8_t op_setiv[] = {
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
static void op_setiv_link(uint8_t *op) {
  *((int32_t *)(op + 30)) = (uint32_t)(((uint8_t *)mrb_vm_iv_set) + (0) - (op + 30));
}

static void op_setiv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
}

static void op_setiv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setiv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 40..43]], "a"=>[[8, 0, 52..55]]} */
static uint8_t op_getcv[] = {
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
static void op_getcv_link(uint8_t *op) {
  *((int32_t *)(op + 45)) = (uint32_t)(((uint8_t *)mrb_vm_cv_get) + (0) - (op + 45));
}

static void op_getcv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 40)) = b * 4 + 0;
  *((int32_t *)(op + 52)) = a * 8 + 0;
}

static void op_getcv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getcv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28]]} */
static uint8_t op_setcv[] = {
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
static void op_setcv_link(uint8_t *op) {
  *((int32_t *)(op + 30)) = (uint32_t)(((uint8_t *)mrb_vm_cv_set) + (0) - (op + 30));
}

static void op_setcv_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
}

static void op_setcv_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setcv_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 34..37]], "a"=>[[8, 0, 82..85]]} */
static uint8_t op_getconst[] = {
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
static void op_getconst_link(uint8_t *op) {
  *((int32_t *)(op + 39)) = (uint32_t)(((uint8_t *)mrb_vm_const_get) + (0) - (op + 39));
}

static void op_getconst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 34)) = b * 4 + 0;
  *((int32_t *)(op + 82)) = a * 8 + 0;
}

static void op_getconst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getconst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28]]} */
static uint8_t op_setconst[] = {
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
static void op_setconst_link(uint8_t *op) {
  *((int32_t *)(op + 30)) = (uint32_t)(((uint8_t *)mrb_vm_const_set) + (0) - (op + 30));
}

static void op_setconst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
}

static void op_setconst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setconst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 38..41]], "a"=>[[8, 0, 45..48], [8, 0, 93..96]]} */
static uint8_t op_getmcnst[] = {
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
static void op_getmcnst_link(uint8_t *op) {
  *((int32_t *)(op + 50)) = (uint32_t)(((uint8_t *)mrb_const_get) + (0) - (op + 50));
}

static void op_getmcnst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 38)) = b * 4 + 0;
  *((int32_t *)(op + 45)) = a * 8 + 0;
  *((int32_t *)(op + 93)) = a * 8 + 0;
}

static void op_getmcnst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getmcnst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28], [8, 8, 32..35]]} */
static uint8_t op_setmcnst[] = {
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
static void op_setmcnst_link(uint8_t *op) {
  *((int32_t *)(op + 37)) = (uint32_t)(((uint8_t *)mrb_const_set) + (0) - (op + 37));
}

static void op_setmcnst_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
  *((int32_t *)(op + 32)) = a * 8 + 8;
}

static void op_setmcnst_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setmcnst_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"c"=>[[1, 0, 11..14]], "a"=>[[8, 0, 78..81], [8, 0, 102..105]], "b"=>[[8, 0, 95..98]]} */
static uint8_t op_getupvar[] = {
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
0x74, 0x0d,                               /*49: je     58 <op_getupvar+0x58> */
0x49, 0xc7, 0x86, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4b: movq   $0x0,0xab0800(%r14) */
0xeb, 0x12,                               /*56: jmp    6a <op_getupvar+0x6a> */
0x48, 0x8b, 0x41, 0x18,                   /*58: mov    0x18(%rcx),%rax */
0x48, 0x8b, 0x80, 0x00, 0x08, 0xbc, 0x00, /*5c: mov    0xbc0800(%rax),%rax */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*63: mov    %rax,0xab0800(%r14) */
0x48, 0x89, 0xdf,                         /*6a: mov    %rbx,%rdi */
0x5b,                                     /*6d: pop    %rbx */
0x41, 0x5e,                               /*6e: pop    %r14 */

};
static void op_getupvar_link(uint8_t *op) {
}

static void op_getupvar_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 11)) = c * 1 + 0;
  *((int32_t *)(op + 78)) = a * 8 + 0;
  *((int32_t *)(op + 102)) = a * 8 + 0;
  *((int32_t *)(op + 95)) = b * 8 + 0;
}

static void op_getupvar_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_getupvar_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"c"=>[[1, 0, 5..8]], "a"=>[[8, 0, 70..73]], "b"=>[[8, 0, 77..80]]} */
static uint8_t op_setupvar[] = {
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
0xeb, 0x23,                               /*35: jmp    5a <op_setupvar+0x5a> */
0x84, 0xc9,                               /*37: test   %cl,%cl */
0x75, 0x1f,                               /*39: jne    5a <op_setupvar+0x5a> */
0x48, 0x8b, 0x43, 0x18,                   /*3b: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4e, 0x18,                   /*3f: mov    0x18(%rsi),%rcx */
0x48, 0x8b, 0x80, 0x00, 0x08, 0xab, 0x00, /*43: mov    0xab0800(%rax),%rax */
0x48, 0x89, 0x81, 0x00, 0x08, 0xbc, 0x00, /*4a: mov    %rax,0xbc0800(%rcx) */
0x48, 0x8b, 0x7b, 0x50,                   /*51: mov    0x50(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*55: callq  5a <op_setupvar+0x5a> */
0x48, 0x89, 0xdf,                         /*5a: mov    %rbx,%rdi */
0x5b,                                     /*5d: pop    %rbx */

};
static void op_setupvar_link(uint8_t *op) {
  *((int32_t *)(op + 86)) = (uint32_t)(((uint8_t *)mrb_write_barrier) + (0) - (op + 86));
}

static void op_setupvar_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = c * 1 + 0;
  *((int32_t *)(op + 70)) = a * 8 + 0;
  *((int32_t *)(op + 77)) = b * 8 + 0;
}

static void op_setupvar_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_setupvar_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_jmp[] = {

};
static void op_jmp_link(uint8_t *op) {
}

static void op_jmp_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_jmp_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmp_set_args(op, 0,GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 7..10]]} */
static uint8_t op_jmpif[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x80, 0x00, 0x08, 0xab, 0x00, /*4: mov    0xab0800(%rax),%rax */
0x48, 0x83, 0xc8, 0x02,                   /*b: or     $0x2,%rax */
0x48, 0x83, 0xf8, 0x02,                   /*f: cmp    $0x2,%rax */

};
static void op_jmpif_link(uint8_t *op) {
}

static void op_jmpif_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 0;
}

static void op_jmpif_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmpif_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 7..10]]} */
static uint8_t op_jmpnot[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x80, 0x00, 0x08, 0xab, 0x00, /*4: mov    0xab0800(%rax),%rax */
0x48, 0x83, 0xc8, 0x02,                   /*b: or     $0x2,%rax */
0x48, 0x83, 0xf8, 0x02,                   /*f: cmp    $0x2,%rax */

};
static void op_jmpnot_link(uint8_t *op) {
}

static void op_jmpnot_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 0;
}

static void op_jmpnot_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_jmpnot_set_args(op, GETARG_A(c),GETARG_sBx(c),0,op_idx);
}


/* args: {"op_idx"=>[[4, 0, 92..95]]} */
static uint8_t op_onerr[] = {
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
static void op_onerr_link(uint8_t *op) {
  *((int32_t *)(op + 61)) = (uint32_t)(((uint8_t *)mrb_realloc) + (0) - (op + 61));
}

static void op_onerr_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 92)) = op_idx * 4 + 0;
}

static void op_onerr_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_onerr_set_args(op, 0,GETARG_sBx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 7..10], [8, 0, 30..33], [8, 0, 81..84], [8, 0, 94..97], [8, 0, 114..117], [8, 0, 130..133], [8, 0, 143..146], [8, 0, 157..160]]} */
static uint8_t op_rescue[] = {
0x48, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*4: movq   $0x0,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x18,                   /*f: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x4f, 0x50,                   /*13: mov    0x50(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x28,                   /*17: mov    0x28(%rcx),%rcx */
0x48, 0x89, 0x88, 0x00, 0x08, 0xab, 0x00, /*1b: mov    %rcx,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x50,                   /*22: mov    0x50(%rdi),%rax */
0x48, 0x8b, 0x48, 0x28,                   /*26: mov    0x28(%rax),%rcx */
0x8b, 0x01,                               /*2a: mov    (%rcx),%eax */
0x0f, 0xb6, 0xf0,                         /*2c: movzbl %al,%esi */
0x0f, 0xb6, 0xd0,                         /*2f: movzbl %al,%edx */
0x83, 0xfe, 0x05,                         /*32: cmp    $0x5,%esi */
0x77, 0x20,                               /*35: ja     57 <op_rescue+0x57> */
0xff, 0x24, 0xd5, 0x00, 0x00, 0x00, 0x00, /*37: jmpq   *0x0(,%rdx,8) */
0x48, 0x85, 0xc9,                         /*3e: test   %rcx,%rcx */
0x0f, 0x95, 0xc0,                         /*41: setne  %al */
0x0f, 0xb6, 0xc0,                         /*44: movzbl %al,%eax */
0x48, 0x01, 0xc0,                         /*47: add    %rax,%rax */
0x48, 0x8b, 0x4f, 0x18,                   /*4a: mov    0x18(%rdi),%rcx */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*4e: mov    %rax,0xab0800(%rcx) */
0xeb, 0x4e,                               /*55: jmp    a5 <op_rescue+0xa5> */
0x48, 0x8b, 0x4f, 0x18,                   /*57: mov    0x18(%rdi),%rcx */
0x48, 0x8b, 0x89, 0x00, 0x08, 0xab, 0x00, /*5b: mov    0xab0800(%rcx),%rcx */
0x48, 0x85, 0xc9,                         /*62: test   %rcx,%rcx */
0x74, 0x3e,                               /*65: je     a5 <op_rescue+0xa5> */
0x88, 0x01,                               /*67: mov    %al,(%rcx) */
0xeb, 0x3a,                               /*69: jmp    a5 <op_rescue+0xa5> */
0x48, 0x8b, 0x47, 0x18,                   /*6b: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*6f: movq   $0x4,0xab0800(%rax) */
0xeb, 0x29,                               /*7a: jmp    a5 <op_rescue+0xa5> */
0x48, 0x8b, 0x47, 0x18,                   /*7c: mov    0x18(%rdi),%rax */
0x83, 0x88, 0x00, 0x08, 0xab, 0x00, 0x01, /*80: orl    $0x1,0xab0800(%rax) */
0xeb, 0x1c,                               /*87: jmp    a5 <op_rescue+0xa5> */
0x48, 0x8b, 0x47, 0x18,                   /*89: mov    0x18(%rdi),%rax */
0xc6, 0x80, 0x00, 0x08, 0xab, 0x00, 0x0e, /*8d: movb   $0xe,0xab0800(%rax) */
0xeb, 0x0f,                               /*94: jmp    a5 <op_rescue+0xa5> */
0x48, 0x8b, 0x47, 0x18,                   /*96: mov    0x18(%rdi),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x06, 0x00, 0x00, 0x00,/*9a: movq   $0x6,0xab0800(%rax) */
0x48, 0x8b, 0x47, 0x50,                   /*a5: mov    0x50(%rdi),%rax */
0x48, 0xc7, 0x40, 0x28, 0x00, 0x00, 0x00, 0x00,/*a9: movq   $0x0,0x28(%rax) */

};
static void op_rescue_link(uint8_t *op) {
}

static void op_rescue_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 7)) = a * 8 + 0;
  *((int32_t *)(op + 30)) = a * 8 + 0;
  *((int32_t *)(op + 81)) = a * 8 + 0;
  *((int32_t *)(op + 94)) = a * 8 + 0;
  *((int32_t *)(op + 114)) = a * 8 + 0;
  *((int32_t *)(op + 130)) = a * 8 + 0;
  *((int32_t *)(op + 143)) = a * 8 + 0;
  *((int32_t *)(op + 157)) = a * 8 + 0;
}

static void op_rescue_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_rescue_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {} */
static uint8_t op_poperr[] = {
0xb8, 0x00, 0x00, 0x55, 0xff,             /*0: mov    $0xff550000,%eax */
0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*5: data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x4f, 0x50,                   /*10: mov    0x50(%rdi),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*14: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*18: mov    0x20(%rcx),%rcx */
0xff, 0x49, 0x1c,                         /*1c: decl   0x1c(%rcx) */
0xff, 0xc0,                               /*1f: inc    %eax */
0x75, 0xed,                               /*21: jne    10 <op_poperr+0x10> */

};
static void op_poperr_link(uint8_t *op) {
}

static void op_poperr_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_poperr_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_poperr_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18]]} */
static uint8_t op_raise[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x50,                   /*8: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x80, 0x00, 0x08, 0xab, 0x00, /*c: mov    0xab0800(%rax),%rax */
0x48, 0x89, 0x41, 0x28,                   /*13: mov    %rax,0x28(%rcx) */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*17: callq  1c <op_raise+0x1c> */
0x48, 0x89, 0xdf,                         /*1c: mov    %rbx,%rdi */
0x5b,                                     /*1f: pop    %rbx */

};
static void op_raise_link(uint8_t *op) {
  *((int32_t *)(op + 24)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 24));
}

static void op_raise_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
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
static void op_epush_link(uint8_t *op) {
  *((int32_t *)(op + 26)) = (uint32_t)(((uint8_t *)mrb_closure_new) + (0) - (op + 26));
  *((int32_t *)(op + 92)) = (uint32_t)(((uint8_t *)mrb_realloc) + (0) - (op + 92));
}

static void op_epush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 8 + 0;
}

static void op_epush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_epush_set_args(op, 0,GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[1, 0, 69..72]]} */
static uint8_t op_epop[] = {
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
static void op_epop_link(uint8_t *op) {
  *((int32_t *)(op + 47)) = (uint32_t)(((uint8_t *)ecall) + (0) - (op + 47));
}

static void op_epop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 69)) = a * 1 + 0;
}

static void op_epop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_epop_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 15..18]], "op_idx"=>[[4, 0, 36..39]], "b"=>[[1, 0, 54..57]], "c"=>[[1, 0, 60..63]]} */
static uint8_t op_send[] = {
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
static void op_send_link(uint8_t *op) {
  *((int32_t *)(op + 70)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 70));
}

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
static uint8_t op_sendb[] = {
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
static void op_sendb_link(uint8_t *op) {
  *((int32_t *)(op + 43)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 43));
}

static void op_sendb_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 1 + 0;
  *((int32_t *)(op + 27)) = b * 1 + 0;
  *((int32_t *)(op + 33)) = c * 1 + 0;
}

static void op_sendb_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sendb_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_fsend[] = {

};
static void op_fsend_link(uint8_t *op) {
}

static void op_fsend_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_fsend_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_fsend_set_args(op, 0,0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 319..322]]} */
static uint8_t op_call[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x46, 0x50,                   /*6: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*a: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x08,                   /*e: mov    0x8(%rax),%rcx */
0x48, 0x8b, 0x40, 0x20,                   /*12: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x19,                         /*16: mov    (%rcx),%rbx */
0x48, 0x8b, 0x4b, 0x20,                   /*19: mov    0x20(%rbx),%rcx */
0x48, 0x89, 0x48, 0x48,                   /*1d: mov    %rcx,0x48(%rax) */
0x48, 0x89, 0x58, 0x08,                   /*21: mov    %rbx,0x8(%rax) */
0x48, 0x8b, 0x4b, 0x28,                   /*25: mov    0x28(%rbx),%rcx */
0x48, 0x85, 0xc9,                         /*29: test   %rcx,%rcx */
0x74, 0x24,                               /*2c: je     52 <op_call+0x52> */
0x8b, 0x51, 0x20,                         /*2e: mov    0x20(%rcx),%edx */
0x85, 0xd2,                               /*31: test   %edx,%edx */
0x74, 0x06,                               /*33: je     3b <op_call+0x3b> */
0x89, 0x10,                               /*35: mov    %edx,(%rax) */
0x48, 0x8b, 0x4b, 0x28,                   /*37: mov    0x28(%rbx),%rcx */
0x48, 0x83, 0x79, 0x18, 0x00,             /*3b: cmpq   $0x0,0x18(%rcx) */
0x75, 0x10,                               /*40: jne    52 <op_call+0x52> */
0x49, 0x8b, 0x56, 0x50,                   /*42: mov    0x50(%r14),%rdx */
0x48, 0x8b, 0x52, 0x18,                   /*46: mov    0x18(%rdx),%rdx */
0x48, 0x8b, 0x52, 0x08,                   /*4a: mov    0x8(%rdx),%rdx */
0x48, 0x89, 0x51, 0x18,                   /*4e: mov    %rdx,0x18(%rcx) */
0xf6, 0x43, 0x02, 0x04,                   /*52: testb  $0x4,0x2(%rbx) */
0x74, 0x32,                               /*56: je     8a <op_call+0x8a> */
0x49, 0x8b, 0x7e, 0x50,                   /*58: mov    0x50(%r14),%rdi */
0x48, 0x89, 0xde,                         /*5c: mov    %rbx,%rsi */
0xff, 0x53, 0x18,                         /*5f: callq  *0x18(%rbx) */
0x48, 0x89, 0xc3,                         /*62: mov    %rax,%rbx */
0x49, 0x8b, 0x7e, 0x50,                   /*65: mov    0x50(%r14),%rdi */
0x41, 0x8b, 0x76, 0x48,                   /*69: mov    0x48(%r14),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*6d: callq  72 <op_call+0x72> */
0x49, 0x8b, 0x46, 0x50,                   /*72: mov    0x50(%r14),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*76: cmpq   $0x0,0x28(%rax) */
0x74, 0x57,                               /*7b: je     d4 <op_call+0xd4> */
0x4c, 0x89, 0xf7,                         /*7d: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*80: callq  85 <op_call+0x85> */
0xe9, 0x13, 0x01, 0x00, 0x00,             /*85: jmpq   19d <op_call+0x19d> */
0x49, 0x89, 0x1e,                         /*8a: mov    %rbx,(%r14) */
0x48, 0x8b, 0x4b, 0x18,                   /*8d: mov    0x18(%rbx),%rcx */
0x49, 0x89, 0x4e, 0x08,                   /*91: mov    %rcx,0x8(%r14) */
0x48, 0x85, 0xc9,                         /*95: test   %rcx,%rcx */
0x0f, 0x84, 0x8d, 0x00, 0x00, 0x00,       /*98: je     12b <op_call+0x12b> */
0x48, 0x8b, 0x51, 0x10,                   /*9e: mov    0x10(%rcx),%rdx */
0x49, 0x89, 0x56, 0x20,                   /*a2: mov    %rdx,0x20(%r14) */
0x48, 0x8b, 0x51, 0x18,                   /*a6: mov    0x18(%rcx),%rdx */
0x49, 0x89, 0x56, 0x28,                   /*aa: mov    %rdx,0x28(%r14) */
0x0f, 0xb7, 0x49, 0x02,                   /*ae: movzwl 0x2(%rcx),%ecx */
0x89, 0x48, 0x18,                         /*b2: mov    %ecx,0x18(%rax) */
0x8b, 0x50, 0x40,                         /*b5: mov    0x40(%rax),%edx */
0x49, 0x8b, 0x46, 0x08,                   /*b8: mov    0x8(%r14),%rax */
0x49, 0x8b, 0x7e, 0x50,                   /*bc: mov    0x50(%r14),%rdi */
0x0f, 0xb7, 0x70, 0x02,                   /*c0: movzwl 0x2(%rax),%esi */
0x85, 0xd2,                               /*c4: test   %edx,%edx */
0x0f, 0x88, 0x83, 0x00, 0x00, 0x00,       /*c6: js     14f <op_call+0x14f> */
0x83, 0xc2, 0x02,                         /*cc: add    $0x2,%edx */
0xe9, 0x8b, 0x00, 0x00, 0x00,             /*cf: jmpq   15f <op_call+0x15f> */
0x48, 0x8b, 0x40, 0x18,                   /*d4: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*d8: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x51, 0x10,                   /*dc: mov    0x10(%rcx),%rdx */
0x48, 0x89, 0x50, 0x08,                   /*e0: mov    %rdx,0x8(%rax) */
0x49, 0x89, 0x56, 0x18,                   /*e4: mov    %rdx,0x18(%r14) */
0x48, 0x63, 0x41, 0x44,                   /*e8: movslq 0x44(%rcx),%rax */
0x48, 0x89, 0x1c, 0xc2,                   /*ec: mov    %rbx,(%rdx,%rax,8) */
0x48, 0x8b, 0x41, 0x30,                   /*f0: mov    0x30(%rcx),%rax */
0x49, 0x89, 0x46, 0x10,                   /*f4: mov    %rax,0x10(%r14) */
0x49, 0x8b, 0x7e, 0x50,                   /*f8: mov    0x50(%r14),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*fc: callq  101 <op_call+0x101> */
0x49, 0x8b, 0x46, 0x50,                   /*101: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*105: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*109: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*10d: mov    0x8(%rax),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*111: mov    0x18(%rax),%rax */
0x49, 0x89, 0x46, 0x08,                   /*115: mov    %rax,0x8(%r14) */
0x48, 0x8b, 0x48, 0x10,                   /*119: mov    0x10(%rax),%rcx */
0x49, 0x89, 0x4e, 0x20,                   /*11d: mov    %rcx,0x20(%r14) */
0x48, 0x8b, 0x40, 0x18,                   /*121: mov    0x18(%rax),%rax */
0x49, 0x89, 0x46, 0x28,                   /*125: mov    %rax,0x28(%r14) */
0xeb, 0x72,                               /*129: jmp    19d <op_call+0x19d> */
0x49, 0x8b, 0x46, 0x50,                   /*12b: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*12f: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*133: mov    0x8(%rax),%rax */
0x48, 0xc7, 0x00, 0x00, 0x00, 0x00, 0x00, /*137: movq   $0x0,(%rax) */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*13e: mov    $0xab0000,%esi */
0x31, 0xd2,                               /*143: xor    %edx,%edx */
0x4c, 0x89, 0xf7,                         /*145: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*148: callq  14d <op_call+0x14d> */
0xeb, 0x4e,                               /*14d: jmp    19d <op_call+0x19d> */
0x83, 0xfe, 0x03,                         /*14f: cmp    $0x3,%esi */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*152: mov    $0x3,%eax */
0x0f, 0x42, 0xf0,                         /*157: cmovb  %eax,%esi */
0xba, 0x03, 0x00, 0x00, 0x00,             /*15a: mov    $0x3,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*15f: callq  164 <op_call+0x164> */
0x49, 0x8b, 0x46, 0x50,                   /*164: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*168: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*16c: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x18,                   /*170: mov    %rax,0x18(%r14) */
0x48, 0x8b, 0x4b, 0x28,                   /*174: mov    0x28(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*178: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x09,                         /*17c: mov    (%rcx),%rcx */
0x48, 0x89, 0x08,                         /*17f: mov    %rcx,(%rax) */
0x49, 0x8b, 0x46, 0x08,                   /*182: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*186: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x10,                   /*18a: mov    %rax,0x10(%r14) */
0x49, 0x8b, 0x36,                         /*18e: mov    (%r14),%rsi */
0x49, 0x8b, 0x7e, 0x50,                   /*191: mov    0x50(%r14),%rdi */
0x4c, 0x89, 0xf2,                         /*195: mov    %r14,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*198: callq  19d <op_call+0x19d> */
0x4c, 0x89, 0xf7,                         /*19d: mov    %r14,%rdi */
0x5b,                                     /*1a0: pop    %rbx */
0x41, 0x5e,                               /*1a1: pop    %r14 */

};
static void op_call_link(uint8_t *op) {
  *((int32_t *)(op + 110)) = (uint32_t)(((uint8_t *)mrb_gc_arena_restore) + (0) - (op + 110));
  *((int32_t *)(op + 129)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 129));
  *((int32_t *)(op + 253)) = (uint32_t)(((uint8_t *)cipop) + (0) - (op + 253));
  *((int32_t *)(op + 329)) = (uint32_t)(((uint8_t *)_op_return) + (0) - (op + 329));
  *((int32_t *)(op + 352)) = (uint32_t)(((uint8_t *)stack_extend) + (0) - (op + 352));
  *((int32_t *)(op + 409)) = (uint32_t)(((uint8_t *)mrb_proc_call_jit) + (0) - (op + 409));
}

static void op_call_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 319)) = a * 1 + 0;
}

static void op_call_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_call_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"c"=>[[1, 0, 210..213], [1, 1, 334..337]], "a"=>[[1, 1, 287..290], [8, 8, 300..303], [8, 12, 318..321], [8, 8, 328..331], [8, 0, 405..408], [1, 0, 499..502]]} */
static uint8_t op_super[] = {
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
0x75, 0x51,                               /*6c: jne    bf <op_super+0xbf> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*6e: mov    $0x3,%edx */
0x44, 0x39, 0x60, 0x60,                   /*73: cmp    %r12d,0x60(%rax) */
0x75, 0x46,                               /*77: jne    bf <op_super+0xbf> */
0x4c, 0x8b, 0xbc, 0xd0, 0x90, 0x00, 0x00, 0x00,/*79: mov    0x90(%rax,%rdx,8),%r15 */
0xeb, 0x4e,                               /*81: jmp    d1 <op_super+0xd1> */
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
0x48, 0x89, 0x45, 0x28,                   /*b6: mov    %rax,0x28(%rbp) */
0xe9, 0x24, 0x01, 0x00, 0x00,             /*ba: jmpq   1e3 <op_super+0x1e3> */
0x48, 0x8d, 0x34, 0x24,                   /*bf: lea    (%rsp),%rsi */
0x48, 0x89, 0xef,                         /*c3: mov    %rbp,%rdi */
0x44, 0x89, 0xe2,                         /*c6: mov    %r12d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c9: callq  ce <op_super+0xce> */
0x49, 0x89, 0xc7,                         /*ce: mov    %rax,%r15 */
0xbd, 0x00, 0x00, 0xcd, 0x00,             /*d1: mov    $0xcd0000,%ebp */
0x4d, 0x85, 0xff,                         /*d6: test   %r15,%r15 */
0x75, 0x77,                               /*d9: jne    152 <op_super+0x152> */
0x48, 0x8b, 0x7b, 0x50,                   /*db: mov    0x50(%rbx),%rdi */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*df: mov    $0x0,%esi */
0xba, 0x0e, 0x00, 0x00, 0x00,             /*e4: mov    $0xe,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*e9: callq  ee <op_super+0xee> */
0x41, 0x89, 0xc4,                         /*ee: mov    %eax,%r12d */
0x48, 0x8b, 0x7b, 0x50,                   /*f1: mov    0x50(%rbx),%rdi */
0x48, 0x8d, 0x34, 0x24,                   /*f5: lea    (%rsp),%rsi */
0x44, 0x89, 0xe2,                         /*f9: mov    %r12d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*fc: callq  101 <op_super+0x101> */
0x49, 0x89, 0xc7,                         /*101: mov    %rax,%r15 */
0x48, 0x8b, 0x43, 0x18,                   /*104: mov    0x18(%rbx),%rax */
0xb9, 0x02, 0x00, 0x78, 0x01,             /*108: mov    $0x1780002,%ecx */
0x0f, 0x1f, 0x00,                         /*10d: nopl   (%rax) */
0x48, 0x8b, 0x54, 0xc8, 0xf8,             /*110: mov    -0x8(%rax,%rcx,8),%rdx */
0x48, 0x89, 0x14, 0xc8,                   /*115: mov    %rdx,(%rax,%rcx,8) */
0x48, 0xff, 0xc9,                         /*119: dec    %rcx */
0x48, 0x81, 0xf9, 0x01, 0x00, 0xab, 0x00, /*11c: cmp    $0xab0001,%rcx */
0x75, 0xeb,                               /*123: jne    110 <op_super+0x110> */
0x48, 0x8b, 0x43, 0x18,                   /*125: mov    0x18(%rbx),%rax */
0x48, 0xc7, 0x80, 0x08, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*129: movq   $0x0,0xab0808(%rax) */
0x41, 0x8b, 0x45, 0x00,                   /*134: mov    0x0(%r13),%eax */
0x48, 0x8b, 0x4b, 0x18,                   /*138: mov    0x18(%rbx),%rcx */
0x89, 0x81, 0x0c, 0x08, 0xab, 0x00,       /*13c: mov    %eax,0xab080c(%rcx) */
0x48, 0x8b, 0x43, 0x18,                   /*142: mov    0x18(%rbx),%rax */
0xc6, 0x80, 0x08, 0x08, 0xab, 0x00, 0x0e, /*146: movb   $0xe,0xab0808(%rax) */
0xbd, 0x01, 0x00, 0xcd, 0x00,             /*14d: mov    $0xcd0001,%ebp */
0x48, 0x8b, 0x7b, 0x50,                   /*152: mov    0x50(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*156: callq  15b <op_super+0x15b> */
0x44, 0x89, 0x20,                         /*15b: mov    %r12d,(%rax) */
0x4c, 0x89, 0x78, 0x08,                   /*15e: mov    %r15,0x8(%rax) */
0x48, 0x8b, 0x4b, 0x50,                   /*162: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*166: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*16a: mov    0x8(%rcx),%rcx */
0x48, 0x89, 0x48, 0x10,                   /*16e: mov    %rcx,0x10(%rax) */
0x89, 0x68, 0x40,                         /*172: mov    %ebp,0x40(%rax) */
0x48, 0x8b, 0x0c, 0x24,                   /*175: mov    (%rsp),%rcx */
0x48, 0x89, 0x48, 0x48,                   /*179: mov    %rcx,0x48(%rax) */
0x48, 0x8b, 0x4b, 0x10,                   /*17d: mov    0x10(%rbx),%rcx */
0x48, 0x83, 0xc1, 0x04,                   /*181: add    $0x4,%rcx */
0x48, 0x89, 0x48, 0x30,                   /*185: mov    %rcx,0x30(%rax) */
0x48, 0x8b, 0x4b, 0x50,                   /*189: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*18d: mov    0x18(%rcx),%rcx */
0x48, 0x81, 0x41, 0x08, 0x00, 0x08, 0xab, 0x00,/*191: addq   $0xab0800,0x8(%rcx) */
0x48, 0x8b, 0x4b, 0x50,                   /*199: mov    0x50(%rbx),%rcx */
0x48, 0x8b, 0x49, 0x18,                   /*19d: mov    0x18(%rcx),%rcx */
0x48, 0x8b, 0x49, 0x08,                   /*1a1: mov    0x8(%rcx),%rcx */
0x4c, 0x89, 0x31,                         /*1a5: mov    %r14,(%rcx) */
0x41, 0xf6, 0x47, 0x02, 0x04,             /*1a8: testb  $0x4,0x2(%r15) */
0x74, 0x41,                               /*1ad: je     1f0 <op_super+0x1f0> */
0x83, 0xcd, 0x02,                         /*1af: or     $0x2,%ebp */
0x89, 0x68, 0x18,                         /*1b2: mov    %ebp,0x18(%rax) */
0x48, 0x8b, 0x7b, 0x50,                   /*1b5: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*1b9: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x68, 0x08,                   /*1bd: mov    0x8(%rax),%rbp */
0x4c, 0x89, 0xf6,                         /*1c1: mov    %r14,%rsi */
0x41, 0xff, 0x57, 0x18,                   /*1c4: callq  *0x18(%r15) */
0x48, 0x89, 0x45, 0x00,                   /*1c8: mov    %rax,0x0(%rbp) */
0x48, 0x8b, 0x7b, 0x50,                   /*1cc: mov    0x50(%rbx),%rdi */
0x8b, 0x73, 0x48,                         /*1d0: mov    0x48(%rbx),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1d3: callq  1d8 <op_super+0x1d8> */
0x48, 0x8b, 0x43, 0x50,                   /*1d8: mov    0x50(%rbx),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*1dc: cmpq   $0x0,0x28(%rax) */
0x74, 0x77,                               /*1e1: je     25a <op_super+0x25a> */
0x48, 0x89, 0xdf,                         /*1e3: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1e6: callq  1eb <op_super+0x1eb> */
0xe9, 0x87, 0x00, 0x00, 0x00,             /*1eb: jmpq   277 <op_super+0x277> */
0xc7, 0x40, 0x44, 0x00, 0x00, 0xab, 0x00, /*1f0: movl   $0xab0000,0x44(%rax) */
0x4c, 0x89, 0x78, 0x08,                   /*1f7: mov    %r15,0x8(%rax) */
0x49, 0x8b, 0x4f, 0x18,                   /*1fb: mov    0x18(%r15),%rcx */
0x48, 0x89, 0x4b, 0x08,                   /*1ff: mov    %rcx,0x8(%rbx) */
0x48, 0x8b, 0x51, 0x10,                   /*203: mov    0x10(%rcx),%rdx */
0x48, 0x89, 0x53, 0x20,                   /*207: mov    %rdx,0x20(%rbx) */
0x48, 0x8b, 0x51, 0x18,                   /*20b: mov    0x18(%rcx),%rdx */
0x48, 0x89, 0x53, 0x28,                   /*20f: mov    %rdx,0x28(%rbx) */
0x0f, 0xb7, 0x49, 0x02,                   /*213: movzwl 0x2(%rcx),%ecx */
0x89, 0x48, 0x18,                         /*217: mov    %ecx,0x18(%rax) */
0x48, 0x8b, 0x4b, 0x08,                   /*21a: mov    0x8(%rbx),%rcx */
0x48, 0x8b, 0x7b, 0x50,                   /*21e: mov    0x50(%rbx),%rdi */
0x0f, 0xb7, 0x71, 0x02,                   /*222: movzwl 0x2(%rcx),%esi */
0x8b, 0x50, 0x40,                         /*226: mov    0x40(%rax),%edx */
0x83, 0xc2, 0x02,                         /*229: add    $0x2,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*22c: callq  231 <op_super+0x231> */
0x48, 0x8b, 0x7b, 0x50,                   /*231: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*235: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*239: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x18,                   /*23d: mov    %rax,0x18(%rbx) */
0x48, 0x8b, 0x43, 0x08,                   /*241: mov    0x8(%rbx),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*245: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x10,                   /*249: mov    %rax,0x10(%rbx) */
0x4c, 0x89, 0xfe,                         /*24d: mov    %r15,%rsi */
0x48, 0x89, 0xda,                         /*250: mov    %rbx,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*253: callq  258 <op_super+0x258> */
0xeb, 0x1d,                               /*258: jmp    277 <op_super+0x277> */
0x48, 0x8b, 0x40, 0x18,                   /*25a: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*25e: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x49, 0x10,                   /*262: mov    0x10(%rcx),%rcx */
0x48, 0x89, 0x48, 0x08,                   /*266: mov    %rcx,0x8(%rax) */
0x48, 0x89, 0x4b, 0x18,                   /*26a: mov    %rcx,0x18(%rbx) */
0x48, 0x8b, 0x7b, 0x50,                   /*26e: mov    0x50(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*272: callq  277 <op_super+0x277> */
0x48, 0x89, 0xdf,                         /*277: mov    %rbx,%rdi */
0x5b,                                     /*27a: pop    %rbx */
0x41, 0x5c,                               /*27b: pop    %r12 */
0x41, 0x5d,                               /*27d: pop    %r13 */
0x41, 0x5e,                               /*27f: pop    %r14 */
0x41, 0x5f,                               /*281: pop    %r15 */
0x5d,                                     /*283: pop    %rbp */

};
static void op_super_link(uint8_t *op) {
  *((int32_t *)(op + 134)) = (uint32_t)(((uint8_t *)_mrb_str_const_nomethod_error) + (0) - (op + 134));
  *((int32_t *)(op + 142)) = (uint32_t)(((uint8_t *)mrb_class_get) + (0) - (op + 142));
  *((int32_t *)(op + 164)) = (uint32_t)(((uint8_t *)mrb_str_new_static) + (0) - (op + 164));
  *((int32_t *)(op + 178)) = (uint32_t)(((uint8_t *)mrb_exc_new_str) + (0) - (op + 178));
  *((int32_t *)(op + 202)) = (uint32_t)(((uint8_t *)_mrb_method_search_vm) + (0) - (op + 202));
  *((int32_t *)(op + 234)) = (uint32_t)(((uint8_t *)mrb_intern_static) + (0) - (op + 234));
  *((int32_t *)(op + 253)) = (uint32_t)(((uint8_t *)mrb_method_search_vm) + (0) - (op + 253));
  *((int32_t *)(op + 343)) = (uint32_t)(((uint8_t *)cipush) + (0) - (op + 343));
  *((int32_t *)(op + 468)) = (uint32_t)(((uint8_t *)mrb_gc_arena_restore) + (0) - (op + 468));
  *((int32_t *)(op + 487)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 487));
  *((int32_t *)(op + 557)) = (uint32_t)(((uint8_t *)stack_extend) + (0) - (op + 557));
  *((int32_t *)(op + 596)) = (uint32_t)(((uint8_t *)mrb_proc_call_jit) + (0) - (op + 596));
  *((int32_t *)(op + 627)) = (uint32_t)(((uint8_t *)cipop) + (0) - (op + 627));
}

static void op_super_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 210)) = c * 1 + 0;
  *((int32_t *)(op + 334)) = c * 1 + 1;
  *((int32_t *)(op + 287)) = a * 1 + 1;
  *((int32_t *)(op + 300)) = a * 8 + 8;
  *((int32_t *)(op + 318)) = a * 8 + 12;
  *((int32_t *)(op + 328)) = a * 8 + 8;
  *((int32_t *)(op + 405)) = a * 8 + 0;
  *((int32_t *)(op + 499)) = a * 1 + 0;
}

static void op_super_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_super_set_args(op, GETARG_A(c),0,GETARG_C(c),op_idx);
}


/* args: {"b"=>[[1, 0, 21..24]], "a"=>[[8, 0, 368..371], [8, 0, 423..426], [8, 0, 434..437], [8, 8, 646..649]]} */
static uint8_t op_argary[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x83, 0xec, 0x38,                   /*a: sub    $0x38,%rsp */
0x49, 0x89, 0xff,                         /*e: mov    %rdi,%r15 */
0xc7, 0x44, 0x24, 0x34, 0x00, 0x00, 0xbc, 0x00,/*11: movl   $0xbc0000,0x34(%rsp) */
0x44, 0x8b, 0x6c, 0x24, 0x34,             /*19: mov    0x34(%rsp),%r13d */
0x44, 0x8b, 0x64, 0x24, 0x34,             /*1e: mov    0x34(%rsp),%r12d */
0x8b, 0x5c, 0x24, 0x34,                   /*23: mov    0x34(%rsp),%ebx */
0x8b, 0x74, 0x24, 0x34,                   /*27: mov    0x34(%rsp),%esi */
0x83, 0xe6, 0x0f,                         /*2b: and    $0xf,%esi */
0x0f, 0x84, 0x87, 0x00, 0x00, 0x00,       /*2e: je     bb <op_argary+0xbb> */
0x49, 0x8b, 0x6f, 0x50,                   /*34: mov    0x50(%r15),%rbp */
0x48, 0x8b, 0x45, 0x18,                   /*38: mov    0x18(%rbp),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*3c: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*40: mov    0x8(%rax),%rax */
0x48, 0x8b, 0x40, 0x28,                   /*44: mov    0x28(%rax),%rax */
0x48, 0x85, 0xc0,                         /*48: test   %rax,%rax */
0x0f, 0x94, 0xc1,                         /*4b: sete   %cl */
0x83, 0xfe, 0x01,                         /*4e: cmp    $0x1,%esi */
0x74, 0x20,                               /*51: je     73 <op_argary+0x73> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*53: mov    $0x1,%edx */
0x29, 0xf2,                               /*58: sub    %esi,%edx */
0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00,       /*5a: nopw   0x0(%rax,%rax,1) */
0xf6, 0xc1, 0x01,                         /*60: test   $0x1,%cl */
0x75, 0x12,                               /*63: jne    77 <op_argary+0x77> */
0x48, 0x8b, 0x40, 0x08,                   /*65: mov    0x8(%rax),%rax */
0x48, 0x85, 0xc0,                         /*69: test   %rax,%rax */
0x0f, 0x94, 0xc1,                         /*6c: sete   %cl */
0xff, 0xc2,                               /*6f: inc    %edx */
0x75, 0xed,                               /*71: jne    60 <op_argary+0x60> */
0x84, 0xc9,                               /*73: test   %cl,%cl */
0x74, 0x4a,                               /*75: je     c1 <op_argary+0xc1> */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*77: mov    0x0(%rip),%rsi        # 7e <op_argary+0x7e> */
0x48, 0x89, 0xef,                         /*7e: mov    %rbp,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*81: callq  86 <op_argary+0x86> */
0x48, 0x89, 0xc3,                         /*86: mov    %rax,%rbx */
0x49, 0x8b, 0x7f, 0x50,                   /*89: mov    0x50(%r15),%rdi */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*8d: mov    $0x0,%esi */
0xba, 0x1e, 0x00, 0x00, 0x00,             /*92: mov    $0x1e,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*97: callq  9c <op_argary+0x9c> */
0x48, 0x89, 0xef,                         /*9c: mov    %rbp,%rdi */
0x48, 0x89, 0xde,                         /*9f: mov    %rbx,%rsi */
0x48, 0x89, 0xc2,                         /*a2: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*a5: callq  aa <op_argary+0xaa> */
0x48, 0x89, 0x45, 0x28,                   /*aa: mov    %rax,0x28(%rbp) */
0x4c, 0x89, 0xff,                         /*ae: mov    %r15,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*b1: callq  b6 <op_argary+0xb6> */
0xe9, 0xdd, 0x01, 0x00, 0x00,             /*b6: jmpq   298 <op_argary+0x298> */
0x49, 0x8d, 0x47, 0x18,                   /*bb: lea    0x18(%r15),%rax */
0xeb, 0x04,                               /*bf: jmp    c5 <op_argary+0xc5> */
0x48, 0x83, 0xc0, 0x18,                   /*c1: add    $0x18,%rax */
0x41, 0xc1, 0xed, 0x0a,                   /*c5: shr    $0xa,%r13d */
0x45, 0x89, 0xee,                         /*c9: mov    %r13d,%r14d */
0x41, 0x83, 0xe6, 0x3f,                   /*cc: and    $0x3f,%r14d */
0x41, 0xc1, 0xec, 0x09,                   /*d0: shr    $0x9,%r12d */
0x41, 0x83, 0xe4, 0x01,                   /*d4: and    $0x1,%r12d */
0xc1, 0xeb, 0x04,                         /*d8: shr    $0x4,%ebx */
0x41, 0x89, 0xd8,                         /*db: mov    %ebx,%r8d */
0x41, 0x83, 0xe0, 0x1f,                   /*de: and    $0x1f,%r8d */
0x48, 0x8b, 0x08,                         /*e2: mov    (%rax),%rcx */
0x48, 0x8d, 0x69, 0x08,                   /*e5: lea    0x8(%rcx),%rbp */
0x45, 0x85, 0xe4,                         /*e9: test   %r12d,%r12d */
0x74, 0x5f,                               /*ec: je     14d <op_argary+0x14d> */
0x44, 0x89, 0x64, 0x24, 0x30,             /*ee: mov    %r12d,0x30(%rsp) */
0x44, 0x89, 0xf0,                         /*f3: mov    %r14d,%eax */
0x48, 0x89, 0x44, 0x24, 0x18,             /*f6: mov    %rax,0x18(%rsp) */
0x48, 0x8b, 0x44, 0xc1, 0x08,             /*fb: mov    0x8(%rcx,%rax,8),%rax */
0x48, 0x89, 0x4c, 0x24, 0x28,             /*100: mov    %rcx,0x28(%rsp) */
0x48, 0x83, 0xf8, 0x07,                   /*105: cmp    $0x7,%rax */
0x73, 0x15,                               /*109: jae    120 <op_argary+0x120> */
0x0f, 0xb6, 0xc8,                         /*10b: movzbl %al,%ecx */
0x45, 0x31, 0xe4,                         /*10e: xor    %r12d,%r12d */
0xba, 0x55, 0x00, 0x00, 0x00,             /*111: mov    $0x55,%edx */
0x0f, 0xa3, 0xca,                         /*116: bt     %ecx,%edx */
0xb9, 0x00, 0x00, 0x00, 0x00,             /*119: mov    $0x0,%ecx */
0x72, 0x5b,                               /*11e: jb     17b <op_argary+0x17b> */
0x45, 0x31, 0xe4,                         /*120: xor    %r12d,%r12d */
0xa8, 0x01,                               /*123: test   $0x1,%al */
0x75, 0x52,                               /*125: jne    179 <op_argary+0x179> */
0x0f, 0xb6, 0xc8,                         /*127: movzbl %al,%ecx */
0x83, 0xf9, 0x0e,                         /*12a: cmp    $0xe,%ecx */
0xb9, 0x00, 0x00, 0x00, 0x00,             /*12d: mov    $0x0,%ecx */
0x74, 0x47,                               /*132: je     17b <op_argary+0x17b> */
0x45, 0x31, 0xe4,                         /*134: xor    %r12d,%r12d */
0x0f, 0xb6, 0x08,                         /*137: movzbl (%rax),%ecx */
0x83, 0xf9, 0x0e,                         /*13a: cmp    $0xe,%ecx */
0xb9, 0x00, 0x00, 0x00, 0x00,             /*13d: mov    $0x0,%ecx */
0x75, 0x37,                               /*142: jne    17b <op_argary+0x17b> */
0x4c, 0x8b, 0x60, 0x28,                   /*144: mov    0x28(%rax),%r12 */
0x8b, 0x48, 0x18,                         /*148: mov    0x18(%rax),%ecx */
0xeb, 0x2e,                               /*14b: jmp    17b <op_argary+0x17b> */
0x4d, 0x8b, 0x6f, 0x18,                   /*14d: mov    0x18(%r15),%r13 */
0x49, 0x8b, 0x7f, 0x50,                   /*151: mov    0x50(%r15),%rdi */
0x43, 0x8d, 0x34, 0x30,                   /*155: lea    (%r8,%r14,1),%esi */
0x48, 0x89, 0xea,                         /*159: mov    %rbp,%rdx */
0x48, 0x89, 0xcd,                         /*15c: mov    %rcx,%rbp */
0x4c, 0x89, 0xc3,                         /*15f: mov    %r8,%rbx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*162: callq  167 <op_argary+0x167> */
0x48, 0x89, 0xdf,                         /*167: mov    %rbx,%rdi */
0x49, 0x89, 0xe8,                         /*16a: mov    %rbp,%r8 */
0x49, 0x89, 0x85, 0x00, 0x08, 0xab, 0x00, /*16d: mov    %rax,0xab0800(%r13) */
0xe9, 0xf7, 0x00, 0x00, 0x00,             /*174: jmpq   270 <op_argary+0x270> */
0x31, 0xc9,                               /*179: xor    %ecx,%ecx */
0x48, 0x89, 0x4c, 0x24, 0x08,             /*17b: mov    %rcx,0x8(%rsp) */
0x49, 0x8b, 0x47, 0x18,                   /*180: mov    0x18(%r15),%rax */
0x48, 0x89, 0x04, 0x24,                   /*184: mov    %rax,(%rsp) */
0x49, 0x8b, 0x7f, 0x50,                   /*188: mov    0x50(%r15),%rdi */
0x43, 0x8d, 0x34, 0x30,                   /*18c: lea    (%r8,%r14,1),%esi */
0x4c, 0x89, 0x44, 0x24, 0x20,             /*190: mov    %r8,0x20(%rsp) */
0x01, 0xce,                               /*195: add    %ecx,%esi */
0x89, 0x74, 0x24, 0x14,                   /*197: mov    %esi,0x14(%rsp) */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*19b: callq  1a0 <op_argary+0x1a0> */
0x48, 0x8b, 0x0c, 0x24,                   /*1a0: mov    (%rsp),%rcx */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*1a4: mov    %rax,0xab0800(%rcx) */
0x49, 0x8b, 0x47, 0x18,                   /*1ab: mov    0x18(%r15),%rax */
0x4c, 0x8b, 0x88, 0x00, 0x08, 0xab, 0x00, /*1af: mov    0xab0800(%rax),%r9 */
0x45, 0x85, 0xf6,                         /*1b6: test   %r14d,%r14d */
0x4c, 0x8b, 0x44, 0x24, 0x28,             /*1b9: mov    0x28(%rsp),%r8 */
0x74, 0x2c,                               /*1be: je     1ec <op_argary+0x1ec> */
0x49, 0x8b, 0x49, 0x28,                   /*1c0: mov    0x28(%r9),%rcx */
0x49, 0x83, 0xe5, 0x3f,                   /*1c4: and    $0x3f,%r13 */
0x49, 0xf7, 0xdd,                         /*1c8: neg    %r13 */
0x4c, 0x89, 0xc2,                         /*1cb: mov    %r8,%rdx */
0x66, 0x90,                               /*1ce: xchg   %ax,%ax */
0x48, 0x89, 0xee,                         /*1d0: mov    %rbp,%rsi */
0x48, 0x83, 0xc2, 0x10,                   /*1d3: add    $0x10,%rdx */
0x48, 0x8b, 0x3e,                         /*1d7: mov    (%rsi),%rdi */
0x48, 0x89, 0x39,                         /*1da: mov    %rdi,(%rcx) */
0x48, 0x83, 0xc1, 0x08,                   /*1dd: add    $0x8,%rcx */
0x49, 0xff, 0xc5,                         /*1e1: inc    %r13 */
0x48, 0x89, 0xd5,                         /*1e4: mov    %rdx,%rbp */
0x48, 0x89, 0xf2,                         /*1e7: mov    %rsi,%rdx */
0x75, 0xe4,                               /*1ea: jne    1d0 <op_argary+0x1d0> */
0x48, 0x8b, 0x44, 0x24, 0x08,             /*1ec: mov    0x8(%rsp),%rax */
0x85, 0xc0,                               /*1f1: test   %eax,%eax */
0x48, 0x8b, 0x6c, 0x24, 0x18,             /*1f3: mov    0x18(%rsp),%rbp */
0x7e, 0x2a,                               /*1f8: jle    224 <op_argary+0x224> */
0x48, 0x8d, 0x0c, 0xed, 0x00, 0x00, 0x00, 0x00,/*1fa: lea    0x0(,%rbp,8),%rcx */
0x49, 0x03, 0x49, 0x28,                   /*202: add    0x28(%r9),%rcx */
0x48, 0x63, 0xd0,                         /*206: movslq %eax,%rdx */
0x48, 0xf7, 0xda,                         /*209: neg    %rdx */
0x0f, 0x1f, 0x40, 0x00,                   /*20c: nopl   0x0(%rax) */
0x49, 0x8b, 0x34, 0x24,                   /*210: mov    (%r12),%rsi */
0x49, 0x83, 0xc4, 0x08,                   /*214: add    $0x8,%r12 */
0x48, 0x89, 0x31,                         /*218: mov    %rsi,(%rcx) */
0x48, 0x83, 0xc1, 0x08,                   /*21b: add    $0x8,%rcx */
0x48, 0xff, 0xc2,                         /*21f: inc    %rdx */
0x75, 0xec,                               /*222: jne    210 <op_argary+0x210> */
0x48, 0x8b, 0x7c, 0x24, 0x20,             /*224: mov    0x20(%rsp),%rdi */
0x85, 0xff,                               /*229: test   %edi,%edi */
0x74, 0x36,                               /*22b: je     263 <op_argary+0x263> */
0x49, 0x8d, 0x4c, 0xe8, 0x10,             /*22d: lea    0x10(%r8,%rbp,8),%rcx */
0x48, 0x63, 0xd0,                         /*232: movslq %eax,%rdx */
0x48, 0x01, 0xea,                         /*235: add    %rbp,%rdx */
0x48, 0xc1, 0xe2, 0x03,                   /*238: shl    $0x3,%rdx */
0x49, 0x03, 0x51, 0x28,                   /*23c: add    0x28(%r9),%rdx */
0x48, 0x83, 0xe3, 0x1f,                   /*240: and    $0x1f,%rbx */
0x48, 0xf7, 0xdb,                         /*244: neg    %rbx */
0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*247: nopw   0x0(%rax,%rax,1) */
0x48, 0x8b, 0x31,                         /*250: mov    (%rcx),%rsi */
0x48, 0x83, 0xc1, 0x08,                   /*253: add    $0x8,%rcx */
0x48, 0x89, 0x32,                         /*257: mov    %rsi,(%rdx) */
0x48, 0x83, 0xc2, 0x08,                   /*25a: add    $0x8,%rdx */
0x48, 0xff, 0xc3,                         /*25e: inc    %rbx */
0x75, 0xed,                               /*261: jne    250 <op_argary+0x250> */
0x8b, 0x4c, 0x24, 0x14,                   /*263: mov    0x14(%rsp),%ecx */
0x41, 0x89, 0x49, 0x18,                   /*267: mov    %ecx,0x18(%r9) */
0x44, 0x8b, 0x64, 0x24, 0x30,             /*26b: mov    0x30(%rsp),%r12d */
0x49, 0x8b, 0x47, 0x18,                   /*270: mov    0x18(%r15),%rax */
0x45, 0x01, 0xe6,                         /*274: add    %r12d,%r14d */
0x42, 0x8d, 0x4c, 0x37, 0x01,             /*277: lea    0x1(%rdi,%r14,1),%ecx */
0x48, 0x63, 0xc9,                         /*27c: movslq %ecx,%rcx */
0x49, 0x8b, 0x0c, 0xc8,                   /*27f: mov    (%r8,%rcx,8),%rcx */
0x48, 0x89, 0x88, 0x08, 0x08, 0xab, 0x00, /*283: mov    %rcx,0xab0808(%rax) */
0x41, 0x8b, 0x47, 0x48,                   /*28a: mov    0x48(%r15),%eax */
0x49, 0x8b, 0x4f, 0x50,                   /*28e: mov    0x50(%r15),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*292: mov    %eax,0xdc(%rcx) */
0x4c, 0x89, 0xff,                         /*298: mov    %r15,%rdi */
0x48, 0x83, 0xc4, 0x38,                   /*29b: add    $0x38,%rsp */
0x5b,                                     /*29f: pop    %rbx */
0x41, 0x5c,                               /*2a0: pop    %r12 */
0x41, 0x5d,                               /*2a2: pop    %r13 */
0x41, 0x5e,                               /*2a4: pop    %r14 */
0x41, 0x5f,                               /*2a6: pop    %r15 */
0x5d,                                     /*2a8: pop    %rbp */

};
static void op_argary_link(uint8_t *op) {
  *((int32_t *)(op + 122)) = (uint32_t)(((uint8_t *)_mrb_str_const_nomethod_error) + (0) - (op + 122));
  *((int32_t *)(op + 130)) = (uint32_t)(((uint8_t *)mrb_class_get) + (0) - (op + 130));
  *((int32_t *)(op + 152)) = (uint32_t)(((uint8_t *)mrb_str_new_static) + (0) - (op + 152));
  *((int32_t *)(op + 166)) = (uint32_t)(((uint8_t *)mrb_exc_new_str) + (0) - (op + 166));
  *((int32_t *)(op + 178)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 178));
  *((int32_t *)(op + 355)) = (uint32_t)(((uint8_t *)mrb_ary_new_from_values) + (0) - (op + 355));
  *((int32_t *)(op + 412)) = (uint32_t)(((uint8_t *)mrb_ary_new_capa) + (0) - (op + 412));
}

static void op_argary_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 1 + 0;
  *((int32_t *)(op + 368)) = a * 8 + 0;
  *((int32_t *)(op + 423)) = a * 8 + 0;
  *((int32_t *)(op + 434)) = a * 8 + 0;
  *((int32_t *)(op + 646)) = a * 8 + 8;
}

static void op_argary_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_argary_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[1, 0, 23..26]]} */
static uint8_t op_enter[] = {
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
0x41, 0x89, 0xc6,                         /*1b: mov    %eax,%r14d */
0x44, 0x89, 0xf2,                         /*1e: mov    %r14d,%edx */
0xc1, 0xea, 0x12,                         /*21: shr    $0x12,%edx */
0x83, 0xe2, 0x1f,                         /*24: and    $0x1f,%edx */
0x48, 0x89, 0x54, 0x24, 0x38,             /*27: mov    %rdx,0x38(%rsp) */
0x44, 0x89, 0xf1,                         /*2c: mov    %r14d,%ecx */
0xc1, 0xe9, 0x0d,                         /*2f: shr    $0xd,%ecx */
0x83, 0xe1, 0x1f,                         /*32: and    $0x1f,%ecx */
0x48, 0x89, 0x4c, 0x24, 0x28,             /*35: mov    %rcx,0x28(%rsp) */
0x44, 0x89, 0xf3,                         /*3a: mov    %r14d,%ebx */
0xc1, 0xeb, 0x0c,                         /*3d: shr    $0xc,%ebx */
0x83, 0xe3, 0x01,                         /*40: and    $0x1,%ebx */
0x45, 0x89, 0xf5,                         /*43: mov    %r14d,%r13d */
0x41, 0xc1, 0xed, 0x07,                   /*46: shr    $0x7,%r13d */
0x4c, 0x8b, 0x7d, 0x18,                   /*4a: mov    0x18(%rbp),%r15 */
0x48, 0x8b, 0x7d, 0x50,                   /*4e: mov    0x50(%rbp),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*52: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*56: mov    0x20(%rax),%rax */
0x48, 0x63, 0x70, 0x40,                   /*5a: movslq 0x40(%rax),%rsi */
0x48, 0x89, 0x74, 0x24, 0x18,             /*5e: mov    %rsi,0x18(%rsp) */
0x8d, 0x04, 0x0a,                         /*63: lea    (%rdx,%rcx,1),%eax */
0x48, 0x89, 0x44, 0x24, 0x48,             /*66: mov    %rax,0x48(%rsp) */
0x48, 0x85, 0xf6,                         /*6b: test   %rsi,%rsi */
0x48, 0x8d, 0x46, 0x01,                   /*6e: lea    0x1(%rsi),%rax */
0xb9, 0x02, 0x00, 0x00, 0x00,             /*72: mov    $0x2,%ecx */
0x48, 0x0f, 0x49, 0xc8,                   /*77: cmovns %rax,%rcx */
0x49, 0x8d, 0x04, 0xcf,                   /*7b: lea    (%r15,%rcx,8),%rax */
0x48, 0x89, 0x44, 0x24, 0x50,             /*7f: mov    %rax,0x50(%rsp) */
0x49, 0x8b, 0x34, 0xcf,                   /*84: mov    (%r15,%rcx,8),%rsi */
0x48, 0x83, 0xfe, 0x06,                   /*88: cmp    $0x6,%rsi */
0x77, 0x07,                               /*8c: ja     95 <op_enter+0x95> */
0xff, 0x24, 0xf5, 0x00, 0x00, 0x00, 0x00, /*8e: jmpq   *0x0(,%rsi,8) */
0x40, 0xf6, 0xc6, 0x01,                   /*95: test   $0x1,%sil */
0x75, 0x11,                               /*99: jne    ac <op_enter+0xac> */
0x40, 0x0f, 0xb6, 0xc6,                   /*9b: movzbl %sil,%eax */
0x83, 0xf8, 0x0e,                         /*9f: cmp    $0xe,%eax */
0x74, 0x08,                               /*a2: je     ac <op_enter+0xac> */
0x0f, 0xb6, 0x06,                         /*a4: movzbl (%rsi),%eax */
0x83, 0xf8, 0x0d,                         /*a7: cmp    $0xd,%eax */
0x74, 0x1d,                               /*aa: je     c9 <op_enter+0xc9> */
0xba, 0x0d, 0x00, 0x00, 0x00,             /*ac: mov    $0xd,%edx */
0xb9, 0x00, 0x00, 0x00, 0x00,             /*b1: mov    $0x0,%ecx */
0x41, 0xb8, 0x00, 0x00, 0x00, 0x00,       /*b6: mov    $0x0,%r8d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*bc: callq  c1 <op_enter+0xc1> */
0x48, 0x8b, 0x4c, 0x24, 0x50,             /*c1: mov    0x50(%rsp),%rcx */
0x48, 0x89, 0x01,                         /*c6: mov    %rax,(%rcx) */
0x45, 0x89, 0xec,                         /*c9: mov    %r13d,%r12d */
0x41, 0x83, 0xe4, 0x1f,                   /*cc: and    $0x1f,%r12d */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*d0: mov    0x48(%rsp),%rax */
0x8d, 0x2c, 0x18,                         /*d5: lea    (%rax,%rbx,1),%ebp */
0x48, 0x89, 0x5c, 0x24, 0x20,             /*d8: mov    %rbx,0x20(%rsp) */
0x49, 0x83, 0xc7, 0x08,                   /*dd: add    $0x8,%r15 */
0x4c, 0x89, 0x7c, 0x24, 0x30,             /*e1: mov    %r15,0x30(%rsp) */
0xbf, 0x00, 0x00, 0x00, 0x00,             /*e6: mov    $0x0,%edi */
0x31, 0xc0,                               /*eb: xor    %eax,%eax */
0x48, 0x8b, 0x74, 0x24, 0x50,             /*ed: mov    0x50(%rsp),%rsi */
0x44, 0x89, 0xf2,                         /*f2: mov    %r14d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*f5: callq  fa <op_enter+0xfa> */
0x4c, 0x8b, 0x5c, 0x24, 0x18,             /*fa: mov    0x18(%rsp),%r11 */
0x45, 0x85, 0xdb,                         /*ff: test   %r11d,%r11d */
0x4c, 0x8b, 0x74, 0x24, 0x40,             /*102: mov    0x40(%rsp),%r14 */
0x79, 0x1b,                               /*107: jns    124 <op_enter+0x124> */
0x49, 0x8b, 0x46, 0x18,                   /*109: mov    0x18(%r14),%rax */
0x49, 0x8b, 0x7e, 0x50,                   /*10d: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x70, 0x08,                   /*111: mov    0x8(%rax),%rsi */
0x4c, 0x8b, 0x7e, 0x28,                   /*115: mov    0x28(%rsi),%r15 */
0x8b, 0x5e, 0x18,                         /*119: mov    0x18(%rsi),%ebx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*11c: callq  121 <op_enter+0x121> */
0x49, 0x89, 0xdb,                         /*121: mov    %rbx,%r11 */
0x46, 0x8d, 0x44, 0x25, 0x00,             /*124: lea    0x0(%rbp,%r12,1),%r8d */
0x49, 0x8b, 0x7e, 0x50,                   /*129: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*12d: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*131: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x48, 0x08,                   /*135: mov    0x8(%rax),%rcx */
0x48, 0x85, 0xc9,                         /*139: test   %rcx,%rcx */
0x49, 0x89, 0xea,                         /*13c: mov    %rbp,%r10 */
0x74, 0x3d,                               /*13f: je     17e <op_enter+0x17e> */
0xf6, 0x41, 0x02, 0x08,                   /*141: testb  $0x8,0x2(%rcx) */
0x74, 0x37,                               /*145: je     17e <op_enter+0x17e> */
0x45, 0x85, 0xdb,                         /*147: test   %r11d,%r11d */
0x0f, 0x88, 0x93, 0x00, 0x00, 0x00,       /*14a: js     1e3 <op_enter+0x1e3> */
0x48, 0x8b, 0x4c, 0x24, 0x38,             /*150: mov    0x38(%rsp),%rcx */
0x42, 0x8d, 0x34, 0x21,                   /*155: lea    (%rcx,%r12,1),%esi */
0x41, 0x39, 0xf3,                         /*159: cmp    %esi,%r11d */
0x7c, 0x0e,                               /*15c: jl     16c <op_enter+0x16c> */
0x48, 0x8b, 0x4c, 0x24, 0x20,             /*15e: mov    0x20(%rsp),%rcx */
0x85, 0xc9,                               /*163: test   %ecx,%ecx */
0x75, 0x7c,                               /*165: jne    1e3 <op_enter+0x1e3> */
0x45, 0x39, 0xc3,                         /*167: cmp    %r8d,%r11d */
0x7e, 0x77,                               /*16a: jle    1e3 <op_enter+0x1e3> */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*16c: callq  171 <op_enter+0x171> */
0x4c, 0x89, 0xf7,                         /*171: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*174: callq  179 <op_enter+0x179> */
0xe9, 0x91, 0x04, 0x00, 0x00,             /*179: jmpq   60f <op_enter+0x60f> */
0x41, 0x83, 0xf8, 0x02,                   /*17e: cmp    $0x2,%r8d */
0x7c, 0x5f,                               /*182: jl     1e3 <op_enter+0x1e3> */
0x41, 0x83, 0xfb, 0x01,                   /*184: cmp    $0x1,%r11d */
0x75, 0x59,                               /*188: jne    1e3 <op_enter+0x1e3> */
0x49, 0x8b, 0x37,                         /*18a: mov    (%r15),%rsi */
0x41, 0xbb, 0x01, 0x00, 0x00, 0x00,       /*18d: mov    $0x1,%r11d */
0x48, 0x83, 0xfe, 0x06,                   /*193: cmp    $0x6,%rsi */
0x77, 0x0b,                               /*197: ja     1a4 <op_enter+0x1a4> */
0xb9, 0x55, 0x00, 0x00, 0x00,             /*199: mov    $0x55,%ecx */
0x48, 0x0f, 0xa3, 0xf1,                   /*19e: bt     %rsi,%rcx */
0x72, 0x3f,                               /*1a2: jb     1e3 <op_enter+0x1e3> */
0x40, 0xf6, 0xc6, 0x01,                   /*1a4: test   $0x1,%sil */
0x75, 0x39,                               /*1a8: jne    1e3 <op_enter+0x1e3> */
0x40, 0x0f, 0xb6, 0xce,                   /*1aa: movzbl %sil,%ecx */
0x83, 0xf9, 0x0e,                         /*1ae: cmp    $0xe,%ecx */
0x74, 0x30,                               /*1b1: je     1e3 <op_enter+0x1e3> */
0x4c, 0x89, 0xd5,                         /*1b3: mov    %r10,%rbp */
0x0f, 0xb6, 0x0e,                         /*1b6: movzbl (%rsi),%ecx */
0x83, 0xf9, 0x0e,                         /*1b9: cmp    $0xe,%ecx */
0x75, 0x22,                               /*1bc: jne    1e0 <op_enter+0x1e0> */
0x4c, 0x89, 0xc3,                         /*1be: mov    %r8,%rbx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1c1: callq  1c6 <op_enter+0x1c6> */
0x49, 0x89, 0xd8,                         /*1c6: mov    %rbx,%r8 */
0x49, 0x8b, 0x07,                         /*1c9: mov    (%r15),%rax */
0x44, 0x8b, 0x58, 0x18,                   /*1cc: mov    0x18(%rax),%r11d */
0x4c, 0x8b, 0x78, 0x28,                   /*1d0: mov    0x28(%rax),%r15 */
0x49, 0x8b, 0x46, 0x50,                   /*1d4: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*1d8: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*1dc: mov    0x20(%rax),%rax */
0x49, 0x89, 0xea,                         /*1e0: mov    %rbp,%r10 */
0x44, 0x89, 0x40, 0x40,                   /*1e3: mov    %r8d,0x40(%rax) */
0x45, 0x39, 0xc3,                         /*1e7: cmp    %r8d,%r11d */
0x0f, 0x8d, 0xb3, 0x00, 0x00, 0x00,       /*1ea: jge    2a3 <op_enter+0x2a3> */
0x48, 0x8b, 0x44, 0x24, 0x38,             /*1f0: mov    0x38(%rsp),%rax */
0x46, 0x8d, 0x2c, 0x20,                   /*1f5: lea    (%rax,%r12,1),%r13d */
0x45, 0x39, 0xeb,                         /*1f9: cmp    %r13d,%r11d */
0x44, 0x89, 0xe0,                         /*1fc: mov    %r12d,%eax */
0x7d, 0x0f,                               /*1ff: jge    210 <op_enter+0x210> */
0x31, 0xc9,                               /*201: xor    %ecx,%ecx */
0x44, 0x89, 0xd8,                         /*203: mov    %r11d,%eax */
0x48, 0x8b, 0x54, 0x24, 0x38,             /*206: mov    0x38(%rsp),%rdx */
0x29, 0xd0,                               /*20b: sub    %edx,%eax */
0x0f, 0x4e, 0xc1,                         /*20d: cmovle %ecx,%eax */
0x41, 0xff, 0xc0,                         /*210: inc    %r8d */
0x49, 0x8b, 0x4e, 0x18,                   /*213: mov    0x18(%r14),%rcx */
0x48, 0x8b, 0x54, 0x24, 0x50,             /*217: mov    0x50(%rsp),%rdx */
0x48, 0x8b, 0x12,                         /*21c: mov    (%rdx),%rdx */
0x4a, 0x89, 0x14, 0xc1,                   /*21f: mov    %rdx,(%rcx,%r8,8) */
0x41, 0x8d, 0x4b, 0x01,                   /*223: lea    0x1(%r11),%ecx */
0x48, 0x63, 0xc9,                         /*227: movslq %ecx,%rcx */
0x49, 0x8b, 0x56, 0x18,                   /*22a: mov    0x18(%r14),%rdx */
0x48, 0xc7, 0x04, 0xca, 0x00, 0x00, 0x00, 0x00,/*22e: movq   $0x0,(%rdx,%rcx,8) */
0x49, 0x8b, 0x56, 0x18,                   /*236: mov    0x18(%r14),%rdx */
0x83, 0x24, 0xca, 0x01,                   /*23a: andl   $0x1,(%rdx,%rcx,8) */
0x49, 0x8b, 0x56, 0x18,                   /*23e: mov    0x18(%r14),%rdx */
0x48, 0xc7, 0x04, 0xca, 0x00, 0x00, 0x00, 0x00,/*242: movq   $0x0,(%rdx,%rcx,8) */
0x4c, 0x39, 0x7c, 0x24, 0x30,             /*24a: cmp    %r15,0x30(%rsp) */
0x0f, 0x84, 0xf7, 0x00, 0x00, 0x00,       /*24f: je     34c <op_enter+0x34c> */
0x49, 0x8b, 0x4e, 0x18,                   /*255: mov    0x18(%r14),%rcx */
0x48, 0x8d, 0x69, 0x08,                   /*259: lea    0x8(%rcx),%rbp */
0x4c, 0x39, 0xfd,                         /*25d: cmp    %r15,%rbp */
0x0f, 0x86, 0xad, 0x00, 0x00, 0x00,       /*260: jbe    313 <op_enter+0x313> */
0x44, 0x89, 0xde,                         /*266: mov    %r11d,%esi */
0x29, 0xc6,                               /*269: sub    %eax,%esi */
0x48, 0x63, 0xf6,                         /*26b: movslq %esi,%rsi */
0x49, 0x8d, 0x3c, 0xf7,                   /*26e: lea    (%r15,%rsi,8),%rdi */
0x48, 0x39, 0xfd,                         /*272: cmp    %rdi,%rbp */
0x0f, 0x83, 0x98, 0x00, 0x00, 0x00,       /*275: jae    313 <op_enter+0x313> */
0x41, 0x39, 0xc3,                         /*27b: cmp    %eax,%r11d */
0x0f, 0x84, 0xc8, 0x00, 0x00, 0x00,       /*27e: je     34c <op_enter+0x34c> */
0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*284: data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x49, 0x8b, 0x54, 0xf7, 0xf8,             /*290: mov    -0x8(%r15,%rsi,8),%rdx */
0x48, 0x89, 0x14, 0xf1,                   /*295: mov    %rdx,(%rcx,%rsi,8) */
0x48, 0xff, 0xce,                         /*299: dec    %rsi */
0x75, 0xf2,                               /*29c: jne    290 <op_enter+0x290> */
0xe9, 0xa9, 0x00, 0x00, 0x00,             /*29e: jmpq   34c <op_enter+0x34c> */
0x4c, 0x89, 0x54, 0x24, 0x18,             /*2a3: mov    %r10,0x18(%rsp) */
0x4c, 0x39, 0x7c, 0x24, 0x30,             /*2a8: cmp    %r15,0x30(%rsp) */
0x0f, 0x84, 0x49, 0x01, 0x00, 0x00,       /*2ad: je     3fc <op_enter+0x3fc> */
0x41, 0x8d, 0x40, 0x01,                   /*2b3: lea    0x1(%r8),%eax */
0x49, 0x8b, 0x4e, 0x18,                   /*2b7: mov    0x18(%r14),%rcx */
0x48, 0x8b, 0x54, 0x24, 0x50,             /*2bb: mov    0x50(%rsp),%rdx */
0x48, 0x8b, 0x12,                         /*2c0: mov    (%rdx),%rdx */
0x48, 0x89, 0x14, 0xc1,                   /*2c3: mov    %rdx,(%rcx,%rax,8) */
0x49, 0x8b, 0x46, 0x18,                   /*2c7: mov    0x18(%r14),%rax */
0x48, 0x8d, 0x48, 0x08,                   /*2cb: lea    0x8(%rax),%rcx */
0x48, 0x8b, 0x54, 0x24, 0x48,             /*2cf: mov    0x48(%rsp),%rdx */
0x89, 0xd2,                               /*2d4: mov    %edx,%edx */
0x4c, 0x39, 0xf9,                         /*2d6: cmp    %r15,%rcx */
0x0f, 0x86, 0xe3, 0x00, 0x00, 0x00,       /*2d9: jbe    3c2 <op_enter+0x3c2> */
0x49, 0x8d, 0x34, 0xd7,                   /*2df: lea    (%r15,%rdx,8),%rsi */
0x48, 0x39, 0xf1,                         /*2e3: cmp    %rsi,%rcx */
0x0f, 0x83, 0xd6, 0x00, 0x00, 0x00,       /*2e6: jae    3c2 <op_enter+0x3c2> */
0x48, 0x8b, 0x4c, 0x24, 0x48,             /*2ec: mov    0x48(%rsp),%rcx */
0x85, 0xc9,                               /*2f1: test   %ecx,%ecx */
0x0f, 0x84, 0x03, 0x01, 0x00, 0x00,       /*2f3: je     3fc <op_enter+0x3fc> */
0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00, /*2f9: nopl   0x0(%rax) */
0x49, 0x8b, 0x4c, 0xd7, 0xf8,             /*300: mov    -0x8(%r15,%rdx,8),%rcx */
0x48, 0x89, 0x0c, 0xd0,                   /*305: mov    %rcx,(%rax,%rdx,8) */
0x48, 0xff, 0xca,                         /*309: dec    %rdx */
0x75, 0xf2,                               /*30c: jne    300 <op_enter+0x300> */
0xe9, 0xe9, 0x00, 0x00, 0x00,             /*30e: jmpq   3fc <op_enter+0x3fc> */
0x4c, 0x39, 0xfd,                         /*313: cmp    %r15,%rbp */
0x74, 0x34,                               /*316: je     34c <op_enter+0x34c> */
0x44, 0x89, 0xde,                         /*318: mov    %r11d,%esi */
0x29, 0xc6,                               /*31b: sub    %eax,%esi */
0x74, 0x2d,                               /*31d: je     34c <op_enter+0x34c> */
0x48, 0x63, 0xf6,                         /*31f: movslq %esi,%rsi */
0x48, 0xf7, 0xde,                         /*322: neg    %rsi */
0x4c, 0x89, 0xff,                         /*325: mov    %r15,%rdi */
0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*328: nopl   0x0(%rax,%rax,1) */
0x48, 0x89, 0xea,                         /*330: mov    %rbp,%rdx */
0x48, 0x83, 0xc1, 0x10,                   /*333: add    $0x10,%rcx */
0x48, 0x8b, 0x2f,                         /*337: mov    (%rdi),%rbp */
0x48, 0x83, 0xc7, 0x08,                   /*33a: add    $0x8,%rdi */
0x48, 0x89, 0x2a,                         /*33e: mov    %rbp,(%rdx) */
0x48, 0xff, 0xc6,                         /*341: inc    %rsi */
0x48, 0x89, 0xcd,                         /*344: mov    %rcx,%rbp */
0x48, 0x89, 0xd1,                         /*347: mov    %rdx,%rcx */
0x75, 0xe4,                               /*34a: jne    330 <op_enter+0x330> */
0x85, 0xc0,                               /*34c: test   %eax,%eax */
0x0f, 0x84, 0xdf, 0x01, 0x00, 0x00,       /*34e: je     533 <op_enter+0x533> */
0x41, 0xff, 0xc2,                         /*354: inc    %r10d */
0x4d, 0x8b, 0x46, 0x18,                   /*357: mov    0x18(%r14),%r8 */
0x4b, 0x8d, 0x0c, 0xd0,                   /*35b: lea    (%r8,%r10,8),%rcx */
0x44, 0x89, 0xda,                         /*35f: mov    %r11d,%edx */
0x29, 0xc2,                               /*362: sub    %eax,%edx */
0x48, 0x63, 0xea,                         /*364: movslq %edx,%rbp */
0x49, 0x8d, 0x14, 0xef,                   /*367: lea    (%r15,%rbp,8),%rdx */
0x48, 0x39, 0xd1,                         /*36b: cmp    %rdx,%rcx */
0x0f, 0x86, 0x9e, 0x01, 0x00, 0x00,       /*36e: jbe    512 <op_enter+0x512> */
0x48, 0x63, 0xf0,                         /*374: movslq %eax,%rsi */
0x48, 0x8d, 0x7c, 0x35, 0x00,             /*377: lea    0x0(%rbp,%rsi,1),%rdi */
0x49, 0x8d, 0x3c, 0xff,                   /*37c: lea    (%r15,%rdi,8),%rdi */
0x48, 0x39, 0xf9,                         /*380: cmp    %rdi,%rcx */
0x0f, 0x83, 0x89, 0x01, 0x00, 0x00,       /*383: jae    512 <op_enter+0x512> */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*389: mov    0x48(%rsp),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x20,             /*38e: mov    0x20(%rsp),%rcx */
0x8d, 0x44, 0x01, 0x01,                   /*393: lea    0x1(%rcx,%rax,1),%eax */
0x49, 0x8d, 0x44, 0xc0, 0xf8,             /*397: lea    -0x8(%r8,%rax,8),%rax */
0x49, 0x8d, 0x4c, 0xef, 0xf8,             /*39c: lea    -0x8(%r15,%rbp,8),%rcx */
0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*3a1: data16 data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x14, 0xf1,                   /*3b0: mov    (%rcx,%rsi,8),%rdx */
0x48, 0x89, 0x14, 0xf0,                   /*3b4: mov    %rdx,(%rax,%rsi,8) */
0x48, 0xff, 0xce,                         /*3b8: dec    %rsi */
0x75, 0xf3,                               /*3bb: jne    3b0 <op_enter+0x3b0> */
0xe9, 0x71, 0x01, 0x00, 0x00,             /*3bd: jmpq   533 <op_enter+0x533> */
0x4c, 0x39, 0xf9,                         /*3c2: cmp    %r15,%rcx */
0x74, 0x35,                               /*3c5: je     3fc <op_enter+0x3fc> */
0x48, 0x8b, 0x54, 0x24, 0x48,             /*3c7: mov    0x48(%rsp),%rdx */
0x85, 0xd2,                               /*3cc: test   %edx,%edx */
0x74, 0x2c,                               /*3ce: je     3fc <op_enter+0x3fc> */
0x48, 0x8b, 0x54, 0x24, 0x48,             /*3d0: mov    0x48(%rsp),%rdx */
0x48, 0xf7, 0xda,                         /*3d5: neg    %rdx */
0x4c, 0x89, 0xfe,                         /*3d8: mov    %r15,%rsi */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*3db: nopl   0x0(%rax,%rax,1) */
0x48, 0x89, 0xcf,                         /*3e0: mov    %rcx,%rdi */
0x48, 0x83, 0xc0, 0x10,                   /*3e3: add    $0x10,%rax */
0x48, 0x8b, 0x0e,                         /*3e7: mov    (%rsi),%rcx */
0x48, 0x83, 0xc6, 0x08,                   /*3ea: add    $0x8,%rsi */
0x48, 0x89, 0x0f,                         /*3ee: mov    %rcx,(%rdi) */
0x48, 0xff, 0xc2,                         /*3f1: inc    %rdx */
0x48, 0x89, 0xc1,                         /*3f4: mov    %rax,%rcx */
0x48, 0x89, 0xf8,                         /*3f7: mov    %rdi,%rax */
0x75, 0xe4,                               /*3fa: jne    3e0 <op_enter+0x3e0> */
0x31, 0xed,                               /*3fc: xor    %ebp,%ebp */
0x48, 0x8b, 0x44, 0x24, 0x20,             /*3fe: mov    0x20(%rsp),%rax */
0x85, 0xc0,                               /*403: test   %eax,%eax */
0x74, 0x6d,                               /*405: je     474 <op_enter+0x474> */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*407: mov    0x48(%rsp),%rax */
0x48, 0x89, 0xc1,                         /*40c: mov    %rax,%rcx */
0x42, 0x8d, 0x04, 0x21,                   /*40f: lea    (%rcx,%r12,1),%eax */
0x44, 0x89, 0xdd,                         /*413: mov    %r11d,%ebp */
0x29, 0xc5,                               /*416: sub    %eax,%ebp */
0x8d, 0x41, 0x01,                         /*418: lea    0x1(%rcx),%eax */
0x48, 0x89, 0x44, 0x24, 0x10,             /*41b: mov    %rax,0x10(%rsp) */
0x48, 0x8b, 0x44, 0x24, 0x40,             /*420: mov    0x40(%rsp),%rax */
0x48, 0x8b, 0x48, 0x18,                   /*425: mov    0x18(%rax),%rcx */
0x48, 0x89, 0x4c, 0x24, 0x08,             /*429: mov    %rcx,0x8(%rsp) */
0x48, 0x8b, 0x78, 0x50,                   /*42e: mov    0x50(%rax),%rdi */
0x48, 0x8b, 0x44, 0x24, 0x38,             /*432: mov    0x38(%rsp),%rax */
0x89, 0xc0,                               /*437: mov    %eax,%eax */
0x48, 0x8b, 0x4c, 0x24, 0x28,             /*439: mov    0x28(%rsp),%rcx */
0x89, 0xc9,                               /*43e: mov    %ecx,%ecx */
0x48, 0x01, 0xc1,                         /*440: add    %rax,%rcx */
0x49, 0x8d, 0x14, 0xcf,                   /*443: lea    (%r15,%rcx,8),%rdx */
0x89, 0xee,                               /*447: mov    %ebp,%esi */
0x4c, 0x89, 0xdb,                         /*449: mov    %r11,%rbx */
0x4d, 0x89, 0xe6,                         /*44c: mov    %r12,%r14 */
0x4d, 0x89, 0xfc,                         /*44f: mov    %r15,%r12 */
0x4d, 0x89, 0xc7,                         /*452: mov    %r8,%r15 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*455: callq  45a <op_enter+0x45a> */
0x4d, 0x89, 0xf8,                         /*45a: mov    %r15,%r8 */
0x4d, 0x89, 0xe7,                         /*45d: mov    %r12,%r15 */
0x4d, 0x89, 0xf4,                         /*460: mov    %r14,%r12 */
0x49, 0x89, 0xdb,                         /*463: mov    %rbx,%r11 */
0x48, 0x8b, 0x4c, 0x24, 0x10,             /*466: mov    0x10(%rsp),%rcx */
0x48, 0x8b, 0x54, 0x24, 0x08,             /*46b: mov    0x8(%rsp),%rdx */
0x48, 0x89, 0x04, 0xca,                   /*470: mov    %rax,(%rdx,%rcx,8) */
0x45, 0x85, 0xe4,                         /*474: test   %r12d,%r12d */
0x0f, 0x84, 0x46, 0x01, 0x00, 0x00,       /*477: je     5c3 <op_enter+0x5c3> */
0x45, 0x29, 0xe3,                         /*47d: sub    %r12d,%r11d */
0x48, 0x8b, 0x44, 0x24, 0x38,             /*480: mov    0x38(%rsp),%rax */
0x41, 0x39, 0xc3,                         /*485: cmp    %eax,%r11d */
0x0f, 0x8e, 0x35, 0x01, 0x00, 0x00,       /*488: jle    5c3 <op_enter+0x5c3> */
0x48, 0x8b, 0x44, 0x24, 0x18,             /*48e: mov    0x18(%rsp),%rax */
0xff, 0xc0,                               /*493: inc    %eax */
0x48, 0x8b, 0x4c, 0x24, 0x40,             /*495: mov    0x40(%rsp),%rcx */
0x48, 0x8b, 0x71, 0x18,                   /*49a: mov    0x18(%rcx),%rsi */
0x48, 0x8d, 0x04, 0xc6,                   /*49e: lea    (%rsi,%rax,8),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x48,             /*4a2: mov    0x48(%rsp),%rcx */
0x8d, 0x4c, 0x0d, 0x00,                   /*4a7: lea    0x0(%rbp,%rcx,1),%ecx */
0x48, 0x63, 0xf9,                         /*4ab: movslq %ecx,%rdi */
0x49, 0x8d, 0x0c, 0xff,                   /*4ae: lea    (%r15,%rdi,8),%rcx */
0x44, 0x89, 0xe2,                         /*4b2: mov    %r12d,%edx */
0x48, 0x39, 0xc8,                         /*4b5: cmp    %rcx,%rax */
0x0f, 0x86, 0xdd, 0x00, 0x00, 0x00,       /*4b8: jbe    59b <op_enter+0x59b> */
0x48, 0x01, 0xd7,                         /*4be: add    %rdx,%rdi */
0x49, 0x8d, 0x3c, 0xff,                   /*4c1: lea    (%r15,%rdi,8),%rdi */
0x48, 0x39, 0xf8,                         /*4c5: cmp    %rdi,%rax */
0x0f, 0x83, 0xcd, 0x00, 0x00, 0x00,       /*4c8: jae    59b <op_enter+0x59b> */
0x48, 0x8b, 0x44, 0x24, 0x48,             /*4ce: mov    0x48(%rsp),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x20,             /*4d3: mov    0x20(%rsp),%rcx */
0x8d, 0x44, 0x01, 0x01,                   /*4d8: lea    0x1(%rcx,%rax,1),%eax */
0x48, 0x8d, 0x44, 0xc6, 0xf8,             /*4dc: lea    -0x8(%rsi,%rax,8),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x38,             /*4e1: mov    0x38(%rsp),%rcx */
0x01, 0xcd,                               /*4e6: add    %ecx,%ebp */
0x48, 0x8b, 0x4c, 0x24, 0x28,             /*4e8: mov    0x28(%rsp),%rcx */
0x01, 0xcd,                               /*4ed: add    %ecx,%ebp */
0x48, 0x63, 0xcd,                         /*4ef: movslq %ebp,%rcx */
0x49, 0x8d, 0x4c, 0xcf, 0xf8,             /*4f2: lea    -0x8(%r15,%rcx,8),%rcx */
0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*4f7: nopw   0x0(%rax,%rax,1) */
0x48, 0x8b, 0x34, 0xd1,                   /*500: mov    (%rcx,%rdx,8),%rsi */
0x48, 0x89, 0x34, 0xd0,                   /*504: mov    %rsi,(%rax,%rdx,8) */
0x48, 0xff, 0xca,                         /*508: dec    %rdx */
0x75, 0xf3,                               /*50b: jne    500 <op_enter+0x500> */
0xe9, 0xb1, 0x00, 0x00, 0x00,             /*50d: jmpq   5c3 <op_enter+0x5c3> */
0x48, 0x39, 0xd1,                         /*512: cmp    %rdx,%rcx */
0x74, 0x1c,                               /*515: je     533 <op_enter+0x533> */
0x48, 0x98,                               /*517: cltq */
0x48, 0xf7, 0xd8,                         /*519: neg    %rax */
0x0f, 0x1f, 0x40, 0x00,                   /*51c: nopl   0x0(%rax) */
0x48, 0x8b, 0x32,                         /*520: mov    (%rdx),%rsi */
0x48, 0x83, 0xc2, 0x08,                   /*523: add    $0x8,%rdx */
0x48, 0x89, 0x31,                         /*527: mov    %rsi,(%rcx) */
0x48, 0x83, 0xc1, 0x08,                   /*52a: add    $0x8,%rcx */
0x48, 0xff, 0xc0,                         /*52e: inc    %rax */
0x75, 0xed,                               /*531: jne    520 <op_enter+0x520> */
0x48, 0x8b, 0x44, 0x24, 0x20,             /*533: mov    0x20(%rsp),%rax */
0x85, 0xc0,                               /*538: test   %eax,%eax */
0x4c, 0x89, 0xf5,                         /*53a: mov    %r14,%rbp */
0x74, 0x21,                               /*53d: je     560 <op_enter+0x560> */
0x4c, 0x8b, 0x7c, 0x24, 0x48,             /*53f: mov    0x48(%rsp),%r15 */
0x41, 0xff, 0xc7,                         /*544: inc    %r15d */
0x4c, 0x8b, 0x75, 0x18,                   /*547: mov    0x18(%rbp),%r14 */
0x48, 0x8b, 0x7d, 0x50,                   /*54b: mov    0x50(%rbp),%rdi */
0x31, 0xf6,                               /*54f: xor    %esi,%esi */
0x4c, 0x89, 0xdb,                         /*551: mov    %r11,%rbx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*554: callq  559 <op_enter+0x559> */
0x49, 0x89, 0xdb,                         /*559: mov    %rbx,%r11 */
0x4b, 0x89, 0x04, 0xfe,                   /*55c: mov    %rax,(%r14,%r15,8) */
0x48, 0x8b, 0x44, 0x24, 0x28,             /*560: mov    0x28(%rsp),%rax */
0x85, 0xc0,                               /*565: test   %eax,%eax */
0x48, 0x8b, 0x45, 0x08,                   /*567: mov    0x8(%rbp),%rax */
0x48, 0x8b, 0x88, 0x98, 0x00, 0x00, 0x00, /*56b: mov    0x98(%rax),%rcx */
0x49, 0x89, 0xee,                         /*572: mov    %rbp,%r14 */
0x0f, 0x84, 0x83, 0x00, 0x00, 0x00,       /*575: je     5fe <op_enter+0x5fe> */
0x45, 0x39, 0xeb,                         /*57b: cmp    %r13d,%r11d */
0x7c, 0x7e,                               /*57e: jl     5fe <op_enter+0x5fe> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*580: mov    $0x1,%edx */
0x48, 0x8b, 0x74, 0x24, 0x38,             /*585: mov    0x38(%rsp),%rsi */
0x29, 0xf2,                               /*58a: sub    %esi,%edx */
0x44, 0x29, 0xe2,                         /*58c: sub    %r12d,%edx */
0x44, 0x01, 0xda,                         /*58f: add    %r11d,%edx */
0x48, 0x63, 0xd2,                         /*592: movslq %edx,%rdx */
0x48, 0x8d, 0x0c, 0x91,                   /*595: lea    (%rcx,%rdx,4),%rcx */
0xeb, 0x67,                               /*599: jmp    602 <op_enter+0x602> */
0x48, 0x39, 0xc8,                         /*59b: cmp    %rcx,%rax */
0x74, 0x23,                               /*59e: je     5c3 <op_enter+0x5c3> */
0x49, 0x83, 0xe5, 0x1f,                   /*5a0: and    $0x1f,%r13 */
0x49, 0xf7, 0xdd,                         /*5a4: neg    %r13 */
0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*5a7: nopw   0x0(%rax,%rax,1) */
0x48, 0x8b, 0x11,                         /*5b0: mov    (%rcx),%rdx */
0x48, 0x83, 0xc1, 0x08,                   /*5b3: add    $0x8,%rcx */
0x48, 0x89, 0x10,                         /*5b7: mov    %rdx,(%rax) */
0x48, 0x83, 0xc0, 0x08,                   /*5ba: add    $0x8,%rax */
0x49, 0xff, 0xc5,                         /*5be: inc    %r13 */
0x75, 0xed,                               /*5c1: jne    5b0 <op_enter+0x5b0> */
0x4c, 0x39, 0x7c, 0x24, 0x30,             /*5c3: cmp    %r15,0x30(%rsp) */
0x4c, 0x8b, 0x74, 0x24, 0x40,             /*5c8: mov    0x40(%rsp),%r14 */
0x75, 0x13,                               /*5cd: jne    5e2 <op_enter+0x5e2> */
0x41, 0xff, 0xc0,                         /*5cf: inc    %r8d */
0x49, 0x8b, 0x46, 0x18,                   /*5d2: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x4c, 0x24, 0x50,             /*5d6: mov    0x50(%rsp),%rcx */
0x48, 0x8b, 0x09,                         /*5db: mov    (%rcx),%rcx */
0x4a, 0x89, 0x0c, 0xc0,                   /*5de: mov    %rcx,(%rax,%r8,8) */
0x48, 0x8b, 0x54, 0x24, 0x28,             /*5e2: mov    0x28(%rsp),%rdx */
0x85, 0xd2,                               /*5e7: test   %edx,%edx */
0x49, 0x8b, 0x46, 0x08,                   /*5e9: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x88, 0x98, 0x00, 0x00, 0x00, /*5ed: mov    0x98(%rax),%rcx */
0x74, 0x08,                               /*5f4: je     5fe <op_enter+0x5fe> */
0xff, 0xc2,                               /*5f6: inc    %edx */
0x48, 0x8d, 0x0c, 0x91,                   /*5f8: lea    (%rcx,%rdx,4),%rcx */
0xeb, 0x04,                               /*5fc: jmp    602 <op_enter+0x602> */
0x48, 0x83, 0xc1, 0x04,                   /*5fe: add    $0x4,%rcx */
0x0f, 0xb7, 0x31,                         /*602: movzwl (%rcx),%esi */
0x48, 0x03, 0xb0, 0xa0, 0x00, 0x00, 0x00, /*605: add    0xa0(%rax),%rsi */
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
static void op_enter_link(uint8_t *op) {
  *((int32_t *)(op + 189)) = (uint32_t)(((uint8_t *)mrb_convert_type) + (0) - (op + 189));
  *((int32_t *)(op + 246)) = (uint32_t)(((uint8_t *)printf) + (0) - (op + 246));
  *((int32_t *)(op + 285)) = (uint32_t)(((uint8_t *)mrb_gc_protect) + (0) - (op + 285));
  *((int32_t *)(op + 365)) = (uint32_t)(((uint8_t *)argnum_error) + (0) - (op + 365));
  *((int32_t *)(op + 373)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 373));
  *((int32_t *)(op + 450)) = (uint32_t)(((uint8_t *)mrb_gc_protect) + (0) - (op + 450));
  *((int32_t *)(op + 1110)) = (uint32_t)(((uint8_t *)mrb_ary_new_from_values) + (0) - (op + 1110));
  *((int32_t *)(op + 1365)) = (uint32_t)(((uint8_t *)mrb_ary_new_capa) + (0) - (op + 1365));
}

static void op_enter_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 23)) = a * 1 + 0;
}

static void op_enter_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_enter_set_args(op, GETARG_Ax(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 14..17]]} */
static uint8_t op_enter_method_m[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x49, 0x89, 0xfe,                         /*a: mov    %rdi,%r14 */
0xb8, 0x00, 0x00, 0xab, 0x00,             /*d: mov    $0xab0000,%eax */
0x89, 0xc3,                               /*12: mov    %eax,%ebx */
0x4d, 0x8b, 0x66, 0x18,                   /*14: mov    0x18(%r14),%r12 */
0x49, 0x8b, 0x7e, 0x50,                   /*18: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*1c: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*20: mov    0x20(%rax),%rax */
0x48, 0x63, 0x68, 0x40,                   /*24: movslq 0x40(%rax),%rbp */
0x48, 0x8d, 0x45, 0x01,                   /*28: lea    0x1(%rbp),%rax */
0x48, 0x85, 0xed,                         /*2c: test   %rbp,%rbp */
0x41, 0xbd, 0x02, 0x00, 0x00, 0x00,       /*2f: mov    $0x2,%r13d */
0x4c, 0x0f, 0x49, 0xe8,                   /*35: cmovns %rax,%r13 */
0x4b, 0x8b, 0x34, 0xec,                   /*39: mov    (%r12,%r13,8),%rsi */
0x48, 0x85, 0xf6,                         /*3d: test   %rsi,%rsi */
0x74, 0x3d,                               /*40: je     7f <op_enter_method_m+0x7f> */
0x48, 0x83, 0xfe, 0x07,                   /*42: cmp    $0x7,%rsi */
0x73, 0x13,                               /*46: jae    5b <op_enter_method_m+0x5b> */
0xb8, 0x55, 0x00, 0x00, 0x00,             /*48: mov    $0x55,%eax */
0x0f, 0xa3, 0xf0,                         /*4d: bt     %esi,%eax */
0x73, 0x09,                               /*50: jae    5b <op_enter_method_m+0x5b> */
0x8b, 0x04, 0xb5, 0x00, 0x00, 0x00, 0x00, /*52: mov    0x0(,%rsi,4),%eax */
0xeb, 0x1c,                               /*59: jmp    77 <op_enter_method_m+0x77> */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*5b: mov    $0x3,%eax */
0x40, 0xf6, 0xc6, 0x01,                   /*60: test   $0x1,%sil */
0x75, 0x11,                               /*64: jne    77 <op_enter_method_m+0x77> */
0x40, 0x0f, 0xb6, 0xce,                   /*66: movzbl %sil,%ecx */
0xb8, 0x04, 0x00, 0x00, 0x00,             /*6a: mov    $0x4,%eax */
0x83, 0xf9, 0x0e,                         /*6f: cmp    $0xe,%ecx */
0x74, 0x03,                               /*72: je     77 <op_enter_method_m+0x77> */
0x0f, 0xb6, 0x06,                         /*74: movzbl (%rsi),%eax */
0x83, 0xf8, 0x0d,                         /*77: cmp    $0xd,%eax */
0x0f, 0x95, 0xc0,                         /*7a: setne  %al */
0xeb, 0x02,                               /*7d: jmp    81 <op_enter_method_m+0x81> */
0x31, 0xc0,                               /*7f: xor    %eax,%eax */
0xc1, 0xeb, 0x12,                         /*81: shr    $0x12,%ebx */
0x84, 0xc0,                               /*84: test   %al,%al */
0x74, 0x19,                               /*86: je     a1 <op_enter_method_m+0xa1> */
0xba, 0x0d, 0x00, 0x00, 0x00,             /*88: mov    $0xd,%edx */
0xb9, 0x00, 0x00, 0x00, 0x00,             /*8d: mov    $0x0,%ecx */
0x41, 0xb8, 0x00, 0x00, 0x00, 0x00,       /*92: mov    $0x0,%r8d */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*98: callq  9d <op_enter_method_m+0x9d> */
0x4b, 0x89, 0x04, 0xec,                   /*9d: mov    %rax,(%r12,%r13,8) */
0x41, 0x89, 0xdf,                         /*a1: mov    %ebx,%r15d */
0x41, 0x83, 0xe7, 0x1f,                   /*a4: and    $0x1f,%r15d */
0x4c, 0x89, 0x24, 0x24,                   /*a8: mov    %r12,(%rsp) */
0x4d, 0x8d, 0x64, 0x24, 0x08,             /*ac: lea    0x8(%r12),%r12 */
0x85, 0xed,                               /*b1: test   %ebp,%ebp */
0x78, 0x21,                               /*b3: js     d6 <op_enter_method_m+0xd6> */
0x44, 0x39, 0xfd,                         /*b5: cmp    %r15d,%ebp */
0x4c, 0x89, 0xe5,                         /*b8: mov    %r12,%rbp */
0x7d, 0x2e,                               /*bb: jge    eb <op_enter_method_m+0xeb> */
0x49, 0x8b, 0x7e, 0x50,                   /*bd: mov    0x50(%r14),%rdi */
0x44, 0x89, 0xfe,                         /*c1: mov    %r15d,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c4: callq  c9 <op_enter_method_m+0xc9> */
0x4c, 0x89, 0xf7,                         /*c9: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*cc: callq  d1 <op_enter_method_m+0xd1> */
0xe9, 0xd0, 0x00, 0x00, 0x00,             /*d1: jmpq   1a6 <op_enter_method_m+0x1a6> */
0x49, 0x8b, 0x46, 0x18,                   /*d6: mov    0x18(%r14),%rax */
0x49, 0x8b, 0x7e, 0x50,                   /*da: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x70, 0x08,                   /*de: mov    0x8(%rax),%rsi */
0x48, 0x8b, 0x6e, 0x28,                   /*e2: mov    0x28(%rsi),%rbp */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*e6: callq  eb <op_enter_method_m+0xeb> */
0x49, 0x8b, 0x46, 0x50,                   /*eb: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*ef: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*f3: mov    0x20(%rax),%rax */
0x44, 0x89, 0x78, 0x40,                   /*f7: mov    %r15d,0x40(%rax) */
0x41, 0x8d, 0x47, 0x01,                   /*fb: lea    0x1(%r15),%eax */
0x49, 0x8b, 0x4e, 0x18,                   /*ff: mov    0x18(%r14),%rcx */
0x48, 0x8b, 0x14, 0x24,                   /*103: mov    (%rsp),%rdx */
0x4a, 0x8b, 0x14, 0xea,                   /*107: mov    (%rdx,%r13,8),%rdx */
0x48, 0x89, 0x14, 0xc1,                   /*10b: mov    %rdx,(%rcx,%rax,8) */
0x49, 0x39, 0xec,                         /*10f: cmp    %rbp,%r12 */
0x74, 0x79,                               /*112: je     18d <op_enter_method_m+0x18d> */
0x49, 0x8b, 0x46, 0x18,                   /*114: mov    0x18(%r14),%rax */
0x48, 0x8d, 0x48, 0x08,                   /*118: lea    0x8(%rax),%rcx */
0x44, 0x89, 0xfa,                         /*11c: mov    %r15d,%edx */
0x48, 0x39, 0xe9,                         /*11f: cmp    %rbp,%rcx */
0x76, 0x2c,                               /*122: jbe    150 <op_enter_method_m+0x150> */
0x48, 0x8d, 0x74, 0xd5, 0x00,             /*124: lea    0x0(%rbp,%rdx,8),%rsi */
0x48, 0x39, 0xf1,                         /*129: cmp    %rsi,%rcx */
0x73, 0x22,                               /*12c: jae    150 <op_enter_method_m+0x150> */
0x45, 0x85, 0xff,                         /*12e: test   %r15d,%r15d */
0x74, 0x5a,                               /*131: je     18d <op_enter_method_m+0x18d> */
0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*133: data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x4c, 0xd5, 0xf8,             /*140: mov    -0x8(%rbp,%rdx,8),%rcx */
0x48, 0x89, 0x0c, 0xd0,                   /*145: mov    %rcx,(%rax,%rdx,8) */
0x48, 0xff, 0xca,                         /*149: dec    %rdx */
0x75, 0xf2,                               /*14c: jne    140 <op_enter_method_m+0x140> */
0xeb, 0x3d,                               /*14e: jmp    18d <op_enter_method_m+0x18d> */
0x48, 0x39, 0xe9,                         /*150: cmp    %rbp,%rcx */
0x74, 0x38,                               /*153: je     18d <op_enter_method_m+0x18d> */
0x45, 0x85, 0xff,                         /*155: test   %r15d,%r15d */
0x74, 0x33,                               /*158: je     18d <op_enter_method_m+0x18d> */
0x48, 0x83, 0xe3, 0x1f,                   /*15a: and    $0x1f,%rbx */
0x48, 0xf7, 0xdb,                         /*15e: neg    %rbx */
0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*161: data16 data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x89, 0xca,                         /*170: mov    %rcx,%rdx */
0x48, 0x83, 0xc0, 0x10,                   /*173: add    $0x10,%rax */
0x48, 0x8b, 0x4d, 0x00,                   /*177: mov    0x0(%rbp),%rcx */
0x48, 0x83, 0xc5, 0x08,                   /*17b: add    $0x8,%rbp */
0x48, 0x89, 0x0a,                         /*17f: mov    %rcx,(%rdx) */
0x48, 0xff, 0xc3,                         /*182: inc    %rbx */
0x48, 0x89, 0xc1,                         /*185: mov    %rax,%rcx */
0x48, 0x89, 0xd0,                         /*188: mov    %rdx,%rax */
0x75, 0xe3,                               /*18b: jne    170 <op_enter_method_m+0x170> */
0x49, 0x8b, 0x46, 0x08,                   /*18d: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x88, 0x98, 0x00, 0x00, 0x00, /*191: mov    0x98(%rax),%rcx */
0x0f, 0xb7, 0x71, 0x04,                   /*198: movzwl 0x4(%rcx),%esi */
0x48, 0x03, 0xb0, 0xa0, 0x00, 0x00, 0x00, /*19c: add    0xa0(%rax),%rsi */
0x4c, 0x89, 0xf7,                         /*1a3: mov    %r14,%rdi */
0x4c, 0x89, 0xf7,                         /*1a6: mov    %r14,%rdi */
0x5b,                                     /*1a9: pop    %rbx */
0x41, 0x5c,                               /*1aa: pop    %r12 */
0x41, 0x5d,                               /*1ac: pop    %r13 */
0x41, 0x5e,                               /*1ae: pop    %r14 */
0x41, 0x5f,                               /*1b0: pop    %r15 */
0x5d,                                     /*1b2: pop    %rbp */
0xff, 0xe6,                               /*1b3: jmpq   *%rsi */

};
static void op_enter_method_m_link(uint8_t *op) {
  *((int32_t *)(op + 153)) = (uint32_t)(((uint8_t *)mrb_convert_type) + (0) - (op + 153));
  *((int32_t *)(op + 197)) = (uint32_t)(((uint8_t *)argnum_error) + (0) - (op + 197));
  *((int32_t *)(op + 205)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 205));
  *((int32_t *)(op + 231)) = (uint32_t)(((uint8_t *)mrb_gc_protect) + (0) - (op + 231));
}

static void op_enter_method_m_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = a * 1 + 0;
}

static void op_enter_method_m_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_enter_method_m_set_args(op, GETARG_Ax(c),0,0,op_idx);
}


/* args: {} */
static uint8_t op_karg[] = {

};
static void op_karg_link(uint8_t *op) {
}

static void op_karg_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_karg_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_karg_set_args(op, 0,0,0,op_idx);
}


/* args: {} */
static uint8_t op_kdict[] = {

};
static void op_kdict_link(uint8_t *op) {
}

static void op_kdict_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_kdict_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_kdict_set_args(op, 0,0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 17..20]]} */
static uint8_t op_return[] = {
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
static void op_return_link(uint8_t *op) {
  *((int32_t *)(op + 24)) = (uint32_t)(((uint8_t *)_op_return) + (0) - (op + 24));
}

static void op_return_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 1 + 0;
}

static void op_return_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_return_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 5..8]], "b"=>[[1, 0, 10..13]]} */
static uint8_t op_break[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*4: mov    $0xab0000,%esi */
0xba, 0x00, 0x00, 0xbc, 0x00,             /*9: mov    $0xbc0000,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*e: callq  13 <op_break+0x13> */
0x48, 0x89, 0xdf,                         /*13: mov    %rbx,%rdi */
0x5b,                                     /*16: pop    %rbx */
0xc3,                                     /*17: retq */

};
static void op_break_link(uint8_t *op) {
  *((int32_t *)(op + 15)) = (uint32_t)(((uint8_t *)_op_return) + (0) - (op + 15));
}

static void op_break_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = a * 1 + 0;
  *((int32_t *)(op + 10)) = b * 1 + 0;
}

static void op_break_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_break_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 28..31]], "a"=>[[8, 0, 35..38], [1, 1, 335..338], [8, 8, 348..351], [8, 0, 394..397], [1, 0, 524..527]], "c"=>[[1, 0, 252..255], [1, 1, 353..356]]} */
static uint8_t op_tailcall[] = {
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
0x49, 0x83, 0xff, 0x06,                   /*27: cmp    $0x6,%r15 */
0x77, 0x08,                               /*2b: ja     35 <op_tailcall+0x35> */
0x42, 0xff, 0x24, 0xfd, 0x00, 0x00, 0x00, 0x00,/*2d: jmpq   *0x0(,%r15,8) */
0x41, 0xf6, 0xc7, 0x01,                   /*35: test   $0x1,%r15b */
0x75, 0x2f,                               /*39: jne    6a <op_tailcall+0x6a> */
0x41, 0x0f, 0xb6, 0xc7,                   /*3b: movzbl %r15b,%eax */
0x83, 0xf8, 0x0e,                         /*3f: cmp    $0xe,%eax */
0x74, 0x2f,                               /*42: je     73 <op_tailcall+0x73> */
0x41, 0x0f, 0xb6, 0x0f,                   /*44: movzbl (%r15),%ecx */
0x31, 0xc0,                               /*48: xor    %eax,%eax */
0x83, 0xf9, 0x13,                         /*4a: cmp    $0x13,%ecx */
0x7f, 0x3f,                               /*4d: jg     8e <op_tailcall+0x8e> */
0x83, 0xf9, 0x07,                         /*4f: cmp    $0x7,%ecx */
0x77, 0x3f,                               /*52: ja     93 <op_tailcall+0x93> */
0xff, 0x24, 0xcd, 0x00, 0x00, 0x00, 0x00, /*54: jmpq   *0x0(,%rcx,8) */
0x41, 0x83, 0xff, 0x02,                   /*5b: cmp    $0x2,%r15d */
0x72, 0x24,                               /*5f: jb     85 <op_tailcall+0x85> */
0x49, 0x8b, 0x86, 0x90, 0x00, 0x00, 0x00, /*61: mov    0x90(%r14),%rax */
0xeb, 0x2d,                               /*68: jmp    97 <op_tailcall+0x97> */
0x49, 0x8b, 0x86, 0x80, 0x00, 0x00, 0x00, /*6a: mov    0x80(%r14),%rax */
0xeb, 0x24,                               /*71: jmp    97 <op_tailcall+0x97> */
0x49, 0x8b, 0x86, 0xa0, 0x00, 0x00, 0x00, /*73: mov    0xa0(%r14),%rax */
0xeb, 0x1b,                               /*7a: jmp    97 <op_tailcall+0x97> */
0x49, 0x8b, 0x86, 0x88, 0x00, 0x00, 0x00, /*7c: mov    0x88(%r14),%rax */
0xeb, 0x12,                               /*83: jmp    97 <op_tailcall+0x97> */
0x49, 0x8b, 0x86, 0x98, 0x00, 0x00, 0x00, /*85: mov    0x98(%r14),%rax */
0xeb, 0x09,                               /*8c: jmp    97 <op_tailcall+0x97> */
0x83, 0xf9, 0x14,                         /*8e: cmp    $0x14,%ecx */
0x74, 0x04,                               /*91: je     97 <op_tailcall+0x97> */
0x49, 0x8b, 0x47, 0x08,                   /*93: mov    0x8(%r15),%rax */
0x48, 0x89, 0x04, 0x24,                   /*97: mov    %rax,(%rsp) */
0x48, 0x8b, 0x0b,                         /*9b: mov    (%rbx),%rcx */
0x48, 0x39, 0x41, 0x38,                   /*9e: cmp    %rax,0x38(%rcx) */
0x75, 0x08,                               /*a2: jne    ac <op_tailcall+0xac> */
0x31, 0xd2,                               /*a4: xor    %edx,%edx */
0x44, 0x39, 0x61, 0x30,                   /*a6: cmp    %r12d,0x30(%rcx) */
0x74, 0x33,                               /*aa: je     df <op_tailcall+0xdf> */
0x48, 0x39, 0x41, 0x48,                   /*ac: cmp    %rax,0x48(%rcx) */
0x75, 0x0b,                               /*b0: jne    bd <op_tailcall+0xbd> */
0xba, 0x01, 0x00, 0x00, 0x00,             /*b2: mov    $0x1,%edx */
0x44, 0x39, 0x61, 0x40,                   /*b7: cmp    %r12d,0x40(%rcx) */
0x74, 0x22,                               /*bb: je     df <op_tailcall+0xdf> */
0x48, 0x39, 0x41, 0x58,                   /*bd: cmp    %rax,0x58(%rcx) */
0x75, 0x0b,                               /*c1: jne    ce <op_tailcall+0xce> */
0xba, 0x02, 0x00, 0x00, 0x00,             /*c3: mov    $0x2,%edx */
0x44, 0x39, 0x61, 0x50,                   /*c8: cmp    %r12d,0x50(%rcx) */
0x74, 0x11,                               /*cc: je     df <op_tailcall+0xdf> */
0x48, 0x39, 0x41, 0x68,                   /*ce: cmp    %rax,0x68(%rcx) */
0x75, 0x15,                               /*d2: jne    e9 <op_tailcall+0xe9> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*d4: mov    $0x3,%edx */
0x44, 0x39, 0x61, 0x60,                   /*d9: cmp    %r12d,0x60(%rcx) */
0x75, 0x0a,                               /*dd: jne    e9 <op_tailcall+0xe9> */
0x4c, 0x8b, 0xac, 0xd1, 0x90, 0x00, 0x00, 0x00,/*df: mov    0x90(%rcx,%rdx,8),%r13 */
0xeb, 0x12,                               /*e7: jmp    fb <op_tailcall+0xfb> */
0x48, 0x8d, 0x34, 0x24,                   /*e9: lea    (%rsp),%rsi */
0x4c, 0x89, 0xf7,                         /*ed: mov    %r14,%rdi */
0x44, 0x89, 0xe2,                         /*f0: mov    %r12d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*f3: callq  f8 <op_tailcall+0xf8> */
0x49, 0x89, 0xc5,                         /*f8: mov    %rax,%r13 */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*fb: mov    $0xcd0000,%eax */
0x4d, 0x85, 0xed,                         /*100: test   %r13,%r13 */
0x75, 0x60,                               /*103: jne    165 <op_tailcall+0x165> */
0x49, 0xc1, 0xe4, 0x20,                   /*105: shl    $0x20,%r12 */
0x4c, 0x89, 0xe5,                         /*109: mov    %r12,%rbp */
0x48, 0x83, 0xcd, 0x0e,                   /*10c: or     $0xe,%rbp */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*110: mov    $0x0,%esi */
0xba, 0x0e, 0x00, 0x00, 0x00,             /*115: mov    $0xe,%edx */
0x4c, 0x89, 0xf7,                         /*11a: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*11d: callq  122 <op_tailcall+0x122> */
0x41, 0x89, 0xc4,                         /*122: mov    %eax,%r12d */
0x48, 0x8d, 0x34, 0x24,                   /*125: lea    (%rsp),%rsi */
0x4c, 0x89, 0xf7,                         /*129: mov    %r14,%rdi */
0x44, 0x89, 0xe2,                         /*12c: mov    %r12d,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*12f: callq  134 <op_tailcall+0x134> */
0x49, 0x89, 0xc5,                         /*134: mov    %rax,%r13 */
0x48, 0x8b, 0x43, 0x18,                   /*137: mov    0x18(%rbx),%rax */
0xb9, 0x02, 0x00, 0x78, 0x01,             /*13b: mov    $0x1780002,%ecx */
0x48, 0x8b, 0x54, 0xc8, 0xf8,             /*140: mov    -0x8(%rax,%rcx,8),%rdx */
0x48, 0x89, 0x14, 0xc8,                   /*145: mov    %rdx,(%rax,%rcx,8) */
0x48, 0xff, 0xc9,                         /*149: dec    %rcx */
0x48, 0x81, 0xf9, 0x01, 0x00, 0xab, 0x00, /*14c: cmp    $0xab0001,%rcx */
0x75, 0xeb,                               /*153: jne    140 <op_tailcall+0x140> */
0x48, 0x8b, 0x43, 0x18,                   /*155: mov    0x18(%rbx),%rax */
0x48, 0x89, 0xa8, 0x08, 0x08, 0xab, 0x00, /*159: mov    %rbp,0xab0808(%rax) */
0xb8, 0x01, 0x00, 0xcd, 0x00,             /*160: mov    $0xcd0001,%eax */
0x49, 0x8b, 0x4e, 0x18,                   /*165: mov    0x18(%r14),%rcx */
0x48, 0x8b, 0x49, 0x20,                   /*169: mov    0x20(%rcx),%rcx */
0x44, 0x89, 0x21,                         /*16d: mov    %r12d,(%rcx) */
0x48, 0x8b, 0x14, 0x24,                   /*170: mov    (%rsp),%rdx */
0x48, 0x89, 0x51, 0x48,                   /*174: mov    %rdx,0x48(%rcx) */
0x89, 0x41, 0x40,                         /*178: mov    %eax,0x40(%rcx) */
0x49, 0x8b, 0x56, 0x18,                   /*17b: mov    0x18(%r14),%rdx */
0x48, 0x8b, 0x52, 0x08,                   /*17f: mov    0x8(%rdx),%rdx */
0x48, 0x8b, 0x7b, 0x18,                   /*183: mov    0x18(%rbx),%rdi */
0x48, 0x8d, 0xb7, 0x00, 0x08, 0xab, 0x00, /*187: lea    0xab0800(%rdi),%rsi */
0xff, 0xc0,                               /*18e: inc    %eax */
0x48, 0x39, 0xf2,                         /*190: cmp    %rsi,%rdx */
0x76, 0x2f,                               /*193: jbe    1c4 <op_tailcall+0x1c4> */
0x48, 0x8d, 0xac, 0xc7, 0x00, 0x00, 0x58, 0x05,/*195: lea    0x5580000(%rdi,%rax,8),%rbp */
0x48, 0x39, 0xea,                         /*19d: cmp    %rbp,%rdx */
0x73, 0x22,                               /*1a0: jae    1c4 <op_tailcall+0x1c4> */
0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*1a2: data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0xb4, 0xc7, 0xf8, 0xff, 0x57, 0x05,/*1b0: mov    0x557fff8(%rdi,%rax,8),%rsi */
0x48, 0x89, 0x74, 0xc2, 0xf8,             /*1b8: mov    %rsi,-0x8(%rdx,%rax,8) */
0x48, 0xff, 0xc8,                         /*1bd: dec    %rax */
0x75, 0xee,                               /*1c0: jne    1b0 <op_tailcall+0x1b0> */
0xeb, 0x1f,                               /*1c2: jmp    1e3 <op_tailcall+0x1e3> */
0x48, 0x39, 0xf2,                         /*1c4: cmp    %rsi,%rdx */
0x74, 0x1a,                               /*1c7: je     1e3 <op_tailcall+0x1e3> */
0x48, 0xf7, 0xd8,                         /*1c9: neg    %rax */
0x0f, 0x1f, 0x40, 0x00,                   /*1cc: nopl   0x0(%rax) */
0x48, 0x8b, 0x3e,                         /*1d0: mov    (%rsi),%rdi */
0x48, 0x83, 0xc6, 0x08,                   /*1d3: add    $0x8,%rsi */
0x48, 0x89, 0x3a,                         /*1d7: mov    %rdi,(%rdx) */
0x48, 0x83, 0xc2, 0x08,                   /*1da: add    $0x8,%rdx */
0x48, 0xff, 0xc0,                         /*1de: inc    %rax */
0x75, 0xed,                               /*1e1: jne    1d0 <op_tailcall+0x1d0> */
0x41, 0xf6, 0x45, 0x02, 0x04,             /*1e3: testb  $0x4,0x2(%r13) */
0x74, 0x32,                               /*1e8: je     21c <op_tailcall+0x21c> */
0x49, 0x8b, 0x46, 0x18,                   /*1ea: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x68, 0x08,                   /*1ee: mov    0x8(%rax),%rbp */
0x4c, 0x89, 0xf7,                         /*1f2: mov    %r14,%rdi */
0x4c, 0x89, 0xfe,                         /*1f5: mov    %r15,%rsi */
0x41, 0xff, 0x55, 0x18,                   /*1f8: callq  *0x18(%r13) */
0x48, 0x89, 0x45, 0x00,                   /*1fc: mov    %rax,0x0(%rbp) */
0x8b, 0x73, 0x48,                         /*200: mov    0x48(%rbx),%esi */
0x4c, 0x89, 0xf7,                         /*203: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*206: callq  20b <op_tailcall+0x20b> */
0xbe, 0x00, 0x00, 0xab, 0x00,             /*20b: mov    $0xab0000,%esi */
0x31, 0xd2,                               /*210: xor    %edx,%edx */
0x48, 0x89, 0xdf,                         /*212: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*215: callq  21a <op_tailcall+0x21a> */
0xeb, 0x67,                               /*21a: jmp    283 <op_tailcall+0x283> */
0x49, 0x8b, 0x45, 0x18,                   /*21c: mov    0x18(%r13),%rax */
0x48, 0x89, 0x43, 0x08,                   /*220: mov    %rax,0x8(%rbx) */
0x48, 0x8b, 0x50, 0x10,                   /*224: mov    0x10(%rax),%rdx */
0x48, 0x89, 0x53, 0x20,                   /*228: mov    %rdx,0x20(%rbx) */
0x48, 0x8b, 0x50, 0x18,                   /*22c: mov    0x18(%rax),%rdx */
0x48, 0x89, 0x53, 0x28,                   /*230: mov    %rdx,0x28(%rbx) */
0x8b, 0x51, 0x40,                         /*234: mov    0x40(%rcx),%edx */
0x0f, 0xb7, 0x70, 0x02,                   /*237: movzwl 0x2(%rax),%esi */
0x85, 0xd2,                               /*23b: test   %edx,%edx */
0x78, 0x05,                               /*23d: js     244 <op_tailcall+0x244> */
0x83, 0xc2, 0x02,                         /*23f: add    $0x2,%edx */
0xeb, 0x10,                               /*242: jmp    254 <op_tailcall+0x254> */
0x83, 0xfe, 0x03,                         /*244: cmp    $0x3,%esi */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*247: mov    $0x3,%eax */
0x0f, 0x42, 0xf0,                         /*24c: cmovb  %eax,%esi */
0xba, 0x03, 0x00, 0x00, 0x00,             /*24f: mov    $0x3,%edx */
0x4c, 0x89, 0xf7,                         /*254: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*257: callq  25c <op_tailcall+0x25c> */
0x49, 0x8b, 0x46, 0x18,                   /*25c: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*260: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x18,                   /*264: mov    %rax,0x18(%rbx) */
0x48, 0x8b, 0x43, 0x08,                   /*268: mov    0x8(%rbx),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*26c: mov    0x8(%rax),%rax */
0x48, 0x89, 0x43, 0x10,                   /*270: mov    %rax,0x10(%rbx) */
0x48, 0x8b, 0x7b, 0x50,                   /*274: mov    0x50(%rbx),%rdi */
0x4c, 0x89, 0xee,                         /*278: mov    %r13,%rsi */
0x48, 0x89, 0xda,                         /*27b: mov    %rbx,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*27e: callq  283 <op_tailcall+0x283> */
0x48, 0x89, 0xdf,                         /*283: mov    %rbx,%rdi */
0x5b,                                     /*286: pop    %rbx */
0x41, 0x5c,                               /*287: pop    %r12 */
0x41, 0x5d,                               /*289: pop    %r13 */
0x41, 0x5e,                               /*28b: pop    %r14 */
0x41, 0x5f,                               /*28d: pop    %r15 */
0x5d,                                     /*28f: pop    %rbp */
0xeb, 0x12,                               /*290: jmp    2a4 <_str_const_method_missing+0x22c> */
0x49, 0x8b, 0x46, 0x78,                   /*292: mov    0x78(%r14),%rax */
0xe9, 0xfc, 0xfd, 0xff, 0xff,             /*296: jmpq   97 <op_tailcall+0x97> */
0x49, 0x8b, 0x46, 0x40,                   /*29b: mov    0x40(%r14),%rax */
0xe9, 0xf3, 0xfd, 0xff, 0xff,             /*29f: jmpq   97 <op_tailcall+0x97> */

};
static void op_tailcall_link(uint8_t *op) {
  *((int32_t *)(op + 244)) = (uint32_t)(((uint8_t *)_mrb_method_search_vm) + (0) - (op + 244));
  *((int32_t *)(op + 286)) = (uint32_t)(((uint8_t *)mrb_intern_static) + (0) - (op + 286));
  *((int32_t *)(op + 304)) = (uint32_t)(((uint8_t *)mrb_method_search_vm) + (0) - (op + 304));
  *((int32_t *)(op + 519)) = (uint32_t)(((uint8_t *)mrb_gc_arena_restore) + (0) - (op + 519));
  *((int32_t *)(op + 534)) = (uint32_t)(((uint8_t *)_op_return) + (0) - (op + 534));
  *((int32_t *)(op + 600)) = (uint32_t)(((uint8_t *)stack_extend) + (0) - (op + 600));
  *((int32_t *)(op + 639)) = (uint32_t)(((uint8_t *)mrb_proc_call_jit) + (0) - (op + 639));
}

static void op_tailcall_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 28)) = b * 4 + 0;
  *((int32_t *)(op + 35)) = a * 8 + 0;
  *((int32_t *)(op + 335)) = a * 1 + 1;
  *((int32_t *)(op + 348)) = a * 8 + 8;
  *((int32_t *)(op + 394)) = a * 8 + 0;
  *((int32_t *)(op + 524)) = a * 1 + 0;
  *((int32_t *)(op + 252)) = c * 1 + 0;
  *((int32_t *)(op + 353)) = c * 1 + 1;
}

static void op_tailcall_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_tailcall_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[1, 0, 14..17]], "a"=>[[8, 0, 185..188]]} */
static uint8_t op_blkpush[] = {
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
0xeb, 0x42,                               /*79: jmp    bd <op_blkpush+0xbd> */
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
0x48, 0x8b, 0x04, 0xc2,                   /*b2: mov    (%rdx,%rax,8),%rax */
0x48, 0x89, 0x81, 0x00, 0x08, 0xab, 0x00, /*b6: mov    %rax,0xab0800(%rcx) */
0x4c, 0x89, 0xf7,                         /*bd: mov    %r14,%rdi */
0x5b,                                     /*c0: pop    %rbx */
0x41, 0x5e,                               /*c1: pop    %r14 */

};
static void op_blkpush_link(uint8_t *op) {
  *((int32_t *)(op + 109)) = (uint32_t)(((uint8_t *)localjump_error) + (0) - (op + 109));
  *((int32_t *)(op + 117)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 117));
}

static void op_blkpush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = b * 1 + 0;
  *((int32_t *)(op + 185)) = a * 8 + 0;
}

static void op_blkpush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_blkpush_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 19..22], [8, 8, 87..90], [8, 0, 189..192], [8, 0, 204..207], [8, 0, 229..232], [8, 0, 322..325]]} */
static uint8_t op_add[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x48, 0x89, 0xfb,                         /*5: mov    %rdi,%rbx */
0x4c, 0x8b, 0x7b, 0x18,                   /*8: mov    0x18(%rbx),%r15 */
0x4c, 0x8b, 0x73, 0x50,                   /*c: mov    0x50(%rbx),%r14 */
0x49, 0x8b, 0xb7, 0x00, 0x08, 0xab, 0x00, /*10: mov    0xab0800(%r15),%rsi */
0x48, 0x83, 0xfe, 0x07,                   /*17: cmp    $0x7,%rsi */
0x73, 0x13,                               /*1b: jae    30 <op_add+0x30> */
0xb8, 0x55, 0x00, 0x00, 0x00,             /*1d: mov    $0x55,%eax */
0x0f, 0xa3, 0xf0,                         /*22: bt     %esi,%eax */
0x73, 0x09,                               /*25: jae    30 <op_add+0x30> */
0x8b, 0x04, 0xb5, 0x00, 0x00, 0x00, 0x00, /*27: mov    0x0(,%rsi,4),%eax */
0xeb, 0x1c,                               /*2e: jmp    4c <op_add+0x4c> */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*30: mov    $0x3,%eax */
0x40, 0xf6, 0xc6, 0x01,                   /*35: test   $0x1,%sil */
0x75, 0x11,                               /*39: jne    4c <op_add+0x4c> */
0x40, 0x0f, 0xb6, 0xce,                   /*3b: movzbl %sil,%ecx */
0xb8, 0x04, 0x00, 0x00, 0x00,             /*3f: mov    $0x4,%eax */
0x83, 0xf9, 0x0e,                         /*44: cmp    $0xe,%ecx */
0x74, 0x03,                               /*47: je     4c <op_add+0x4c> */
0x0f, 0xb6, 0x06,                         /*49: movzbl (%rsi),%eax */
0xc1, 0xe0, 0x08,                         /*4c: shl    $0x8,%eax */
0x25, 0x00, 0xff, 0xff, 0x00,             /*4f: and    $0xffff00,%eax */
0x49, 0x8b, 0x97, 0x08, 0x08, 0xab, 0x00, /*54: mov    0xab0808(%r15),%rdx */
0x48, 0x83, 0xfa, 0x07,                   /*5b: cmp    $0x7,%rdx */
0x73, 0x13,                               /*5f: jae    74 <op_add+0x74> */
0xb9, 0x55, 0x00, 0x00, 0x00,             /*61: mov    $0x55,%ecx */
0x0f, 0xa3, 0xd1,                         /*66: bt     %edx,%ecx */
0x73, 0x09,                               /*69: jae    74 <op_add+0x74> */
0x8b, 0x0c, 0x95, 0x00, 0x00, 0x00, 0x00, /*6b: mov    0x0(,%rdx,4),%ecx */
0xeb, 0x1a,                               /*72: jmp    8e <op_add+0x8e> */
0xb9, 0x03, 0x00, 0x00, 0x00,             /*74: mov    $0x3,%ecx */
0xf6, 0xc2, 0x01,                         /*79: test   $0x1,%dl */
0x75, 0x10,                               /*7c: jne    8e <op_add+0x8e> */
0x0f, 0xb6, 0xfa,                         /*7e: movzbl %dl,%edi */
0xb9, 0x04, 0x00, 0x00, 0x00,             /*81: mov    $0x4,%ecx */
0x83, 0xff, 0x0e,                         /*86: cmp    $0xe,%edi */
0x74, 0x03,                               /*89: je     8e <op_add+0x8e> */
0x0f, 0xb6, 0x0a,                         /*8b: movzbl (%rdx),%ecx */
0x0f, 0xb6, 0xc9,                         /*8e: movzbl %cl,%ecx */
0x09, 0xc1,                               /*91: or     %eax,%ecx */
0x81, 0xf9, 0x0f, 0x10, 0x00, 0x00,       /*93: cmp    $0x100f,%ecx */
0x7f, 0x37,                               /*99: jg     d2 <op_add+0xd2> */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*9b: cmp    $0x602,%ecx */
0x7f, 0x48,                               /*a1: jg     eb <op_add+0xeb> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*a3: cmp    $0x303,%ecx */
0x75, 0x55,                               /*a9: jne    100 <op_add+0x100> */
0xd1, 0xfe,                               /*ab: sar    %esi */
0xd1, 0xfa,                               /*ad: sar    %edx */
0x01, 0xf2,                               /*af: add    %esi,%edx */
0x89, 0xd0,                               /*b1: mov    %edx,%eax */
0x05, 0x00, 0x00, 0x00, 0x40,             /*b3: add    $0x40000000,%eax */
0x78, 0x79,                               /*b8: js     133 <op_add+0x133> */
0x49, 0xc7, 0x87, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*ba: movq   $0x0,0xab0800(%r15) */
0x8d, 0x44, 0x12, 0x01,                   /*c5: lea    0x1(%rdx,%rdx,1),%eax */
0x41, 0x89, 0x87, 0x00, 0x08, 0xab, 0x00, /*c9: mov    %eax,0xab0800(%r15) */
0xeb, 0x74,                               /*d0: jmp    146 <op_add+0x146> */
0x81, 0xf9, 0x10, 0x10, 0x00, 0x00,       /*d2: cmp    $0x1010,%ecx */
0x75, 0x4f,                               /*d8: jne    129 <op_add+0x129> */
0x4c, 0x89, 0xf7,                         /*da: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*dd: callq  e2 <op_add+0xe2> */
0x49, 0x89, 0x87, 0x00, 0x08, 0xab, 0x00, /*e2: mov    %rax,0xab0800(%r15) */
0xeb, 0x5b,                               /*e9: jmp    146 <op_add+0x146> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*eb: cmp    $0x603,%ecx */
0x75, 0x22,                               /*f1: jne    115 <op_add+0x115> */
0xd1, 0xfa,                               /*f3: sar    %edx */
0xf2, 0x0f, 0x2a, 0xc2,                   /*f5: cvtsi2sd %edx,%xmm0 */
0xf2, 0x0f, 0x58, 0x46, 0x18,             /*f9: addsd  0x18(%rsi),%xmm0 */
0xeb, 0x37,                               /*fe: jmp    137 <op_add+0x137> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*100: cmp    $0x306,%ecx */
0x75, 0x21,                               /*106: jne    129 <op_add+0x129> */
0xd1, 0xfe,                               /*108: sar    %esi */
0xf2, 0x0f, 0x2a, 0xc6,                   /*10a: cvtsi2sd %esi,%xmm0 */
0xf2, 0x0f, 0x58, 0x42, 0x18,             /*10e: addsd  0x18(%rdx),%xmm0 */
0xeb, 0x22,                               /*113: jmp    137 <op_add+0x137> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*115: cmp    $0x606,%ecx */
0x75, 0x0c,                               /*11b: jne    129 <op_add+0x129> */
0xf2, 0x0f, 0x10, 0x46, 0x18,             /*11d: movsd  0x18(%rsi),%xmm0 */
0xf2, 0x0f, 0x58, 0x42, 0x18,             /*122: addsd  0x18(%rdx),%xmm0 */
0xeb, 0x0e,                               /*127: jmp    137 <op_add+0x137> */
0x48, 0x89, 0xdf,                         /*129: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*12c: callq  131 <op_add+0x131> */
0xeb, 0x1d,                               /*131: jmp    150 <op_add+0x150> */
0xf2, 0x0f, 0x2a, 0xc2,                   /*133: cvtsi2sd %edx,%xmm0 */
0x4c, 0x89, 0xf7,                         /*137: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*13a: callq  13f <op_add+0x13f> */
0x49, 0x89, 0x87, 0x00, 0x08, 0xab, 0x00, /*13f: mov    %rax,0xab0800(%r15) */
0x8b, 0x43, 0x48,                         /*146: mov    0x48(%rbx),%eax */
0x41, 0x89, 0x86, 0xdc, 0x00, 0x00, 0x00, /*149: mov    %eax,0xdc(%r14) */
0x48, 0x89, 0xdf,                         /*150: mov    %rbx,%rdi */
0x5b,                                     /*153: pop    %rbx */
0x41, 0x5e,                               /*154: pop    %r14 */
0x41, 0x5f,                               /*156: pop    %r15 */

};
static void op_add_link(uint8_t *op) {
  *((int32_t *)(op + 222)) = (uint32_t)(((uint8_t *)mrb_str_plus) + (0) - (op + 222));
  *((int32_t *)(op + 301)) = (uint32_t)(((uint8_t *)op_send) + (0) - (op + 301));
  *((int32_t *)(op + 315)) = (uint32_t)(((uint8_t *)mrb_word_boxing_float_value) + (0) - (op + 315));
}

static void op_add_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = a * 8 + 0;
  *((int32_t *)(op + 87)) = a * 8 + 8;
  *((int32_t *)(op + 189)) = a * 8 + 0;
  *((int32_t *)(op + 204)) = a * 8 + 0;
  *((int32_t *)(op + 229)) = a * 8 + 0;
  *((int32_t *)(op + 322)) = a * 8 + 0;
}

static void op_add_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_add_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 24..27], [8, 0, 104..107], [8, 0, 119..122], [8, 8, 128..131], [8, 8, 143..146], [1, 0, 153..156], [8, 0, 198..201]], "b"=>[[1, 0, 158..161]]} */
static uint8_t op_addi[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x8b, 0x04, 0x25, 0xba, 0xab, 0x0f, 0x00, /*6: mov    0xfabba,%eax */
0x4c, 0x8b, 0x73, 0x18,                   /*d: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x50,                   /*11: mov    0x50(%rbx),%rdi */
0x49, 0x8b, 0x8e, 0x00, 0x08, 0xab, 0x00, /*15: mov    0xab0800(%r14),%rcx */
0x48, 0x83, 0xf9, 0x06,                   /*1c: cmp    $0x6,%rcx */
0x77, 0x0b,                               /*20: ja     2d <op_addi+0x2d> */
0xba, 0x55, 0x00, 0x00, 0x00,             /*22: mov    $0x55,%edx */
0x48, 0x0f, 0xa3, 0xca,                   /*27: bt     %rcx,%rdx */
0x72, 0x50,                               /*2b: jb     7d <op_addi+0x7d> */
0xf6, 0xc1, 0x01,                         /*2d: test   $0x1,%cl */
0x75, 0x20,                               /*30: jne    52 <op_addi+0x52> */
0x0f, 0xb6, 0xd1,                         /*32: movzbl %cl,%edx */
0x83, 0xfa, 0x0e,                         /*35: cmp    $0xe,%edx */
0x74, 0x43,                               /*38: je     7d <op_addi+0x7d> */
0x0f, 0xb6, 0x11,                         /*3a: movzbl (%rcx),%edx */
0x83, 0xfa, 0x06,                         /*3d: cmp    $0x6,%edx */
0x75, 0x0b,                               /*40: jne    4d <op_addi+0x4d> */
0xf2, 0x0f, 0x2a, 0xc0,                   /*42: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x58, 0x41, 0x18,             /*46: addsd  0x18(%rcx),%xmm0 */
0xeb, 0x71,                               /*4b: jmp    be <op_addi+0xbe> */
0x83, 0xfa, 0x03,                         /*4d: cmp    $0x3,%edx */
0x75, 0x2b,                               /*50: jne    7d <op_addi+0x7d> */
0xd1, 0xf9,                               /*52: sar    %ecx */
0x8d, 0x34, 0x01,                         /*54: lea    (%rcx,%rax,1),%esi */
0x89, 0xca,                               /*57: mov    %ecx,%edx */
0x01, 0xc2,                               /*59: add    %eax,%edx */
0x70, 0x55,                               /*5b: jo     b2 <op_addi+0xb2> */
0x81, 0xc6, 0x00, 0x00, 0x00, 0x40,       /*5d: add    $0x40000000,%esi */
0x78, 0x4d,                               /*63: js     b2 <op_addi+0xb2> */
0x49, 0xc7, 0x86, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*65: movq   $0x0,0xab0800(%r14) */
0x8d, 0x44, 0x12, 0x01,                   /*70: lea    0x1(%rdx,%rdx,1),%eax */
0x41, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*74: mov    %eax,0xab0800(%r14) */
0xeb, 0x4d,                               /*7b: jmp    ca <op_addi+0xca> */
0x49, 0xc7, 0x86, 0x08, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*7d: movq   $0x0,0xab0808(%r14) */
0x8d, 0x44, 0x00, 0x01,                   /*88: lea    0x1(%rax,%rax,1),%eax */
0x41, 0x89, 0x86, 0x08, 0x08, 0xab, 0x00, /*8c: mov    %eax,0xab0808(%r14) */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*93: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*98: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*9d: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x01, 0x00, 0x00, 0x00,       /*a2: mov    $0x1,%r8d */
0x48, 0x89, 0xdf,                         /*a8: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*ab: callq  b0 <op_addi+0xb0> */
0xeb, 0x18,                               /*b0: jmp    ca <op_addi+0xca> */
0xf2, 0x0f, 0x2a, 0xc9,                   /*b2: cvtsi2sd %ecx,%xmm1 */
0xf2, 0x0f, 0x2a, 0xc0,                   /*b6: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x58, 0xc1,                   /*ba: addsd  %xmm1,%xmm0 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*be: callq  c3 <op_addi+0xc3> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*c3: mov    %rax,0xab0800(%r14) */
0x48, 0x89, 0xdf,                         /*ca: mov    %rbx,%rdi */
0x5b,                                     /*cd: pop    %rbx */
0x41, 0x5e,                               /*ce: pop    %r14 */

};
static void op_addi_link(uint8_t *op) {
  *((int32_t *)(op + 172)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 172));
  *((int32_t *)(op + 191)) = (uint32_t)(((uint8_t *)mrb_word_boxing_float_value) + (0) - (op + 191));
}

static void op_addi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 24)) = a * 8 + 0;
  *((int32_t *)(op + 104)) = a * 8 + 0;
  *((int32_t *)(op + 119)) = a * 8 + 0;
  *((int32_t *)(op + 128)) = a * 8 + 8;
  *((int32_t *)(op + 143)) = a * 8 + 8;
  *((int32_t *)(op + 153)) = a * 1 + 0;
  *((int32_t *)(op + 198)) = a * 8 + 0;
  *((int32_t *)(op + 158)) = b * 1 + 0;
}

static void op_addi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_addi_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 17..20], [8, 8, 83..86], [8, 0, 183..186], [8, 0, 198..201], [8, 0, 292..295]]} */
static uint8_t op_sub[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x50,                   /*a: mov    0x50(%rbx),%rdi */
0x49, 0x8b, 0x86, 0x00, 0x08, 0xab, 0x00, /*e: mov    0xab0800(%r14),%rax */
0x48, 0x83, 0xf8, 0x07,                   /*15: cmp    $0x7,%rax */
0x73, 0x13,                               /*19: jae    2e <op_sub+0x2e> */
0xb9, 0x55, 0x00, 0x00, 0x00,             /*1b: mov    $0x55,%ecx */
0x0f, 0xa3, 0xc1,                         /*20: bt     %eax,%ecx */
0x73, 0x09,                               /*23: jae    2e <op_sub+0x2e> */
0x8b, 0x14, 0x85, 0x00, 0x00, 0x00, 0x00, /*25: mov    0x0(,%rax,4),%edx */
0xeb, 0x19,                               /*2c: jmp    47 <op_sub+0x47> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*2e: mov    $0x3,%edx */
0xa8, 0x01,                               /*33: test   $0x1,%al */
0x75, 0x10,                               /*35: jne    47 <op_sub+0x47> */
0x0f, 0xb6, 0xc8,                         /*37: movzbl %al,%ecx */
0xba, 0x04, 0x00, 0x00, 0x00,             /*3a: mov    $0x4,%edx */
0x83, 0xf9, 0x0e,                         /*3f: cmp    $0xe,%ecx */
0x74, 0x03,                               /*42: je     47 <op_sub+0x47> */
0x0f, 0xb6, 0x10,                         /*44: movzbl (%rax),%edx */
0xc1, 0xe2, 0x08,                         /*47: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*4a: and    $0xffff00,%edx */
0x49, 0x8b, 0x8e, 0x08, 0x08, 0xab, 0x00, /*50: mov    0xab0808(%r14),%rcx */
0x48, 0x83, 0xf9, 0x07,                   /*57: cmp    $0x7,%rcx */
0x73, 0x13,                               /*5b: jae    70 <op_sub+0x70> */
0xbe, 0x55, 0x00, 0x00, 0x00,             /*5d: mov    $0x55,%esi */
0x0f, 0xa3, 0xce,                         /*62: bt     %ecx,%esi */
0x73, 0x09,                               /*65: jae    70 <op_sub+0x70> */
0x8b, 0x34, 0x8d, 0x00, 0x00, 0x00, 0x00, /*67: mov    0x0(,%rcx,4),%esi */
0xeb, 0x1c,                               /*6e: jmp    8c <op_sub+0x8c> */
0xbe, 0x03, 0x00, 0x00, 0x00,             /*70: mov    $0x3,%esi */
0xf6, 0xc1, 0x01,                         /*75: test   $0x1,%cl */
0x75, 0x12,                               /*78: jne    8c <op_sub+0x8c> */
0x44, 0x0f, 0xb6, 0xc1,                   /*7a: movzbl %cl,%r8d */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*7e: mov    $0x4,%esi */
0x41, 0x83, 0xf8, 0x0e,                   /*83: cmp    $0xe,%r8d */
0x74, 0x03,                               /*87: je     8c <op_sub+0x8c> */
0x0f, 0xb6, 0x31,                         /*89: movzbl (%rcx),%esi */
0x40, 0x0f, 0xb6, 0xf6,                   /*8c: movzbl %sil,%esi */
0x09, 0xd6,                               /*90: or     %edx,%esi */
0x81, 0xfe, 0x02, 0x06, 0x00, 0x00,       /*92: cmp    $0x602,%esi */
0x7f, 0x32,                               /*98: jg     cc <op_sub+0xcc> */
0x81, 0xfe, 0x03, 0x03, 0x00, 0x00,       /*9a: cmp    $0x303,%esi */
0x75, 0x3b,                               /*a0: jne    dd <op_sub+0xdd> */
0xd1, 0xf8,                               /*a2: sar    %eax */
0xd1, 0xf9,                               /*a4: sar    %ecx */
0x89, 0xc2,                               /*a6: mov    %eax,%edx */
0x29, 0xca,                               /*a8: sub    %ecx,%edx */
0x89, 0xd6,                               /*aa: mov    %edx,%esi */
0x81, 0xc6, 0x00, 0x00, 0x00, 0x40,       /*ac: add    $0x40000000,%esi */
0x78, 0x5c,                               /*b2: js     110 <op_sub+0x110> */
0x49, 0xc7, 0x86, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*b4: movq   $0x0,0xab0800(%r14) */
0x8d, 0x44, 0x12, 0x01,                   /*bf: lea    0x1(%rdx,%rdx,1),%eax */
0x41, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*c3: mov    %eax,0xab0800(%r14) */
0xeb, 0x5c,                               /*ca: jmp    128 <op_sub+0x128> */
0x81, 0xfe, 0x03, 0x06, 0x00, 0x00,       /*cc: cmp    $0x603,%esi */
0x75, 0x1e,                               /*d2: jne    f2 <op_sub+0xf2> */
0xf2, 0x0f, 0x10, 0x40, 0x18,             /*d4: movsd  0x18(%rax),%xmm0 */
0xd1, 0xf9,                               /*d9: sar    %ecx */
0xeb, 0x37,                               /*db: jmp    114 <op_sub+0x114> */
0x81, 0xfe, 0x06, 0x03, 0x00, 0x00,       /*dd: cmp    $0x306,%esi */
0x75, 0x21,                               /*e3: jne    106 <op_sub+0x106> */
0xd1, 0xf8,                               /*e5: sar    %eax */
0xf2, 0x0f, 0x2a, 0xc0,                   /*e7: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x5c, 0x41, 0x18,             /*eb: subsd  0x18(%rcx),%xmm0 */
0xeb, 0x2a,                               /*f0: jmp    11c <op_sub+0x11c> */
0x81, 0xfe, 0x06, 0x06, 0x00, 0x00,       /*f2: cmp    $0x606,%esi */
0x75, 0x0c,                               /*f8: jne    106 <op_sub+0x106> */
0xf2, 0x0f, 0x10, 0x40, 0x18,             /*fa: movsd  0x18(%rax),%xmm0 */
0xf2, 0x0f, 0x5c, 0x41, 0x18,             /*ff: subsd  0x18(%rcx),%xmm0 */
0xeb, 0x16,                               /*104: jmp    11c <op_sub+0x11c> */
0x48, 0x89, 0xdf,                         /*106: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*109: callq  10e <op_sub+0x10e> */
0xeb, 0x18,                               /*10e: jmp    128 <op_sub+0x128> */
0xf2, 0x0f, 0x2a, 0xc0,                   /*110: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x2a, 0xc9,                   /*114: cvtsi2sd %ecx,%xmm1 */
0xf2, 0x0f, 0x5c, 0xc1,                   /*118: subsd  %xmm1,%xmm0 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*11c: callq  121 <op_sub+0x121> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*121: mov    %rax,0xab0800(%r14) */
0x48, 0x89, 0xdf,                         /*128: mov    %rbx,%rdi */
0x5b,                                     /*12b: pop    %rbx */
0x41, 0x5e,                               /*12c: pop    %r14 */

};
static void op_sub_link(uint8_t *op) {
  *((int32_t *)(op + 266)) = (uint32_t)(((uint8_t *)op_send) + (0) - (op + 266));
  *((int32_t *)(op + 285)) = (uint32_t)(((uint8_t *)mrb_word_boxing_float_value) + (0) - (op + 285));
}

static void op_sub_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 8 + 0;
  *((int32_t *)(op + 83)) = a * 8 + 8;
  *((int32_t *)(op + 183)) = a * 8 + 0;
  *((int32_t *)(op + 198)) = a * 8 + 0;
  *((int32_t *)(op + 292)) = a * 8 + 0;
}

static void op_sub_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sub_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 17..20], [8, 0, 104..107], [8, 8, 113..116], [8, 8, 123..126], [1, 0, 137..140], [8, 0, 165..168], [8, 0, 182..185]], "b"=>[[1, 0, 142..145]]} */
static uint8_t op_subi[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x49, 0x8b, 0x7e, 0x50,                   /*a: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x83, 0x00, 0x08, 0xab, 0x00, /*e: mov    0xab0800(%rbx),%rax */
0x48, 0x83, 0xf8, 0x06,                   /*15: cmp    $0x6,%rax */
0x77, 0x0b,                               /*19: ja     26 <op_subi+0x26> */
0xb9, 0x55, 0x00, 0x00, 0x00,             /*1b: mov    $0x55,%ecx */
0x48, 0x0f, 0xa3, 0xc1,                   /*20: bt     %rax,%rcx */
0x72, 0x48,                               /*24: jb     6e <op_subi+0x6e> */
0xa8, 0x01,                               /*26: test   $0x1,%al */
0x75, 0x24,                               /*28: jne    4e <op_subi+0x4e> */
0x0f, 0xb6, 0xc8,                         /*2a: movzbl %al,%ecx */
0x83, 0xf9, 0x0e,                         /*2d: cmp    $0xe,%ecx */
0x74, 0x3c,                               /*30: je     6e <op_subi+0x6e> */
0x0f, 0xb6, 0x08,                         /*32: movzbl (%rax),%ecx */
0x83, 0xf9, 0x06,                         /*35: cmp    $0x6,%ecx */
0x75, 0x0f,                               /*38: jne    49 <op_subi+0x49> */
0xf2, 0x0f, 0x10, 0x40, 0x18,             /*3a: movsd  0x18(%rax),%xmm0 */
0xf2, 0x0f, 0x58, 0x05, 0x00, 0x00, 0x00, 0x00,/*3f: addsd  0x0(%rip),%xmm0        # 47 <op_subi+0x47> */
0xeb, 0x17,                               /*47: jmp    60 <op_subi+0x60> */
0x83, 0xf9, 0x03,                         /*49: cmp    $0x3,%ecx */
0x75, 0x20,                               /*4c: jne    6e <op_subi+0x6e> */
0xd1, 0xf8,                               /*4e: sar    %eax */
0x3d, 0xff, 0xff, 0xcc, 0xc0,             /*50: cmp    $0xc0ccffff,%eax */
0x7f, 0x4b,                               /*55: jg     a2 <op_subi+0xa2> */
0x05, 0x00, 0x00, 0x33, 0xff,             /*57: add    $0xff330000,%eax */
0xf2, 0x0f, 0x2a, 0xc0,                   /*5c: cvtsi2sd %eax,%xmm0 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*60: callq  65 <op_subi+0x65> */
0x48, 0x89, 0x83, 0x00, 0x08, 0xab, 0x00, /*65: mov    %rax,0xab0800(%rbx) */
0xeb, 0x4c,                               /*6c: jmp    ba <op_subi+0xba> */
0x48, 0xc7, 0x83, 0x08, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*6e: movq   $0x0,0xab0808(%rbx) */
0xc7, 0x83, 0x08, 0x08, 0xab, 0x00, 0x01, 0x00, 0x9a, 0x01,/*79: movl   $0x19a0001,0xab0808(%rbx) */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*83: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*88: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*8d: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x01, 0x00, 0x00, 0x00,       /*92: mov    $0x1,%r8d */
0x4c, 0x89, 0xf7,                         /*98: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*9b: callq  a0 <op_subi+0xa0> */
0xeb, 0x18,                               /*a0: jmp    ba <op_subi+0xba> */
0x48, 0xc7, 0x83, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*a2: movq   $0x0,0xab0800(%rbx) */
0x8d, 0x84, 0x00, 0x01, 0x00, 0x66, 0xfe, /*ad: lea    -0x199ffff(%rax,%rax,1),%eax */
0x89, 0x83, 0x00, 0x08, 0xab, 0x00,       /*b4: mov    %eax,0xab0800(%rbx) */
0x4c, 0x89, 0xf7,                         /*ba: mov    %r14,%rdi */
0x5b,                                     /*bd: pop    %rbx */
0x41, 0x5e,                               /*be: pop    %r14 */

};
static void op_subi_link(uint8_t *op) {
  *((int32_t *)(op + 67)) = (uint32_t)(((uint8_t *).LCPI0_0) + (0) - (op + 67));
  *((int32_t *)(op + 97)) = (uint32_t)(((uint8_t *)mrb_word_boxing_float_value) + (0) - (op + 97));
  *((int32_t *)(op + 156)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 156));
}

static void op_subi_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 8 + 0;
  *((int32_t *)(op + 104)) = a * 8 + 0;
  *((int32_t *)(op + 113)) = a * 8 + 8;
  *((int32_t *)(op + 123)) = a * 8 + 8;
  *((int32_t *)(op + 137)) = a * 1 + 0;
  *((int32_t *)(op + 165)) = a * 8 + 0;
  *((int32_t *)(op + 182)) = a * 8 + 0;
  *((int32_t *)(op + 142)) = b * 1 + 0;
}

static void op_subi_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_subi_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 19..22], [8, 8, 87..90], [8, 0, 294..297], [8, 0, 318..321], [8, 0, 332..335]]} */
static uint8_t op_mul[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x48, 0x89, 0xfb,                         /*5: mov    %rdi,%rbx */
0x4c, 0x8b, 0x7b, 0x18,                   /*8: mov    0x18(%rbx),%r15 */
0x4c, 0x8b, 0x73, 0x50,                   /*c: mov    0x50(%rbx),%r14 */
0x49, 0x8b, 0xb7, 0x00, 0x08, 0xab, 0x00, /*10: mov    0xab0800(%r15),%rsi */
0x48, 0x83, 0xfe, 0x07,                   /*17: cmp    $0x7,%rsi */
0x73, 0x13,                               /*1b: jae    30 <op_mul+0x30> */
0xb8, 0x55, 0x00, 0x00, 0x00,             /*1d: mov    $0x55,%eax */
0x0f, 0xa3, 0xf0,                         /*22: bt     %esi,%eax */
0x73, 0x09,                               /*25: jae    30 <op_mul+0x30> */
0x8b, 0x04, 0xb5, 0x00, 0x00, 0x00, 0x00, /*27: mov    0x0(,%rsi,4),%eax */
0xeb, 0x1c,                               /*2e: jmp    4c <op_mul+0x4c> */
0xb8, 0x03, 0x00, 0x00, 0x00,             /*30: mov    $0x3,%eax */
0x40, 0xf6, 0xc6, 0x01,                   /*35: test   $0x1,%sil */
0x75, 0x11,                               /*39: jne    4c <op_mul+0x4c> */
0x40, 0x0f, 0xb6, 0xce,                   /*3b: movzbl %sil,%ecx */
0xb8, 0x04, 0x00, 0x00, 0x00,             /*3f: mov    $0x4,%eax */
0x83, 0xf9, 0x0e,                         /*44: cmp    $0xe,%ecx */
0x74, 0x03,                               /*47: je     4c <op_mul+0x4c> */
0x0f, 0xb6, 0x06,                         /*49: movzbl (%rsi),%eax */
0xc1, 0xe0, 0x08,                         /*4c: shl    $0x8,%eax */
0x25, 0x00, 0xff, 0xff, 0x00,             /*4f: and    $0xffff00,%eax */
0x49, 0x8b, 0x97, 0x08, 0x08, 0xab, 0x00, /*54: mov    0xab0808(%r15),%rdx */
0x48, 0x83, 0xfa, 0x07,                   /*5b: cmp    $0x7,%rdx */
0x73, 0x13,                               /*5f: jae    74 <op_mul+0x74> */
0xb9, 0x55, 0x00, 0x00, 0x00,             /*61: mov    $0x55,%ecx */
0x0f, 0xa3, 0xd1,                         /*66: bt     %edx,%ecx */
0x73, 0x09,                               /*69: jae    74 <op_mul+0x74> */
0x8b, 0x0c, 0x95, 0x00, 0x00, 0x00, 0x00, /*6b: mov    0x0(,%rdx,4),%ecx */
0xeb, 0x1a,                               /*72: jmp    8e <op_mul+0x8e> */
0xb9, 0x03, 0x00, 0x00, 0x00,             /*74: mov    $0x3,%ecx */
0xf6, 0xc2, 0x01,                         /*79: test   $0x1,%dl */
0x75, 0x10,                               /*7c: jne    8e <op_mul+0x8e> */
0x0f, 0xb6, 0xfa,                         /*7e: movzbl %dl,%edi */
0xb9, 0x04, 0x00, 0x00, 0x00,             /*81: mov    $0x4,%ecx */
0x83, 0xff, 0x0e,                         /*86: cmp    $0xe,%edi */
0x74, 0x03,                               /*89: je     8e <op_mul+0x8e> */
0x0f, 0xb6, 0x0a,                         /*8b: movzbl (%rdx),%ecx */
0x0f, 0xb6, 0xc9,                         /*8e: movzbl %cl,%ecx */
0x09, 0xc1,                               /*91: or     %eax,%ecx */
0x81, 0xf9, 0x02, 0x06, 0x00, 0x00,       /*93: cmp    $0x602,%ecx */
0x7f, 0x44,                               /*99: jg     df <op_mul+0xdf> */
0x81, 0xf9, 0x03, 0x03, 0x00, 0x00,       /*9b: cmp    $0x303,%ecx */
0x75, 0x51,                               /*a1: jne    f4 <op_mul+0xf4> */
0x4c, 0x89, 0xf7,                         /*a3: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*a6: callq  ab <op_mul+0xab> */
0x48, 0x83, 0xf8, 0x06,                   /*ab: cmp    $0x6,%rax */
0x77, 0x0f,                               /*af: ja     c0 <op_mul+0xc0> */
0xb9, 0x55, 0x00, 0x00, 0x00,             /*b1: mov    $0x55,%ecx */
0x48, 0x0f, 0xa3, 0xc1,                   /*b6: bt     %rax,%rcx */
0x0f, 0x82, 0x90, 0x00, 0x00, 0x00,       /*ba: jb     150 <op_mul+0x150> */
0xa8, 0x01,                               /*c0: test   $0x1,%al */
0x75, 0x77,                               /*c2: jne    13b <op_mul+0x13b> */
0x0f, 0xb6, 0xc8,                         /*c4: movzbl %al,%ecx */
0x83, 0xf9, 0x0e,                         /*c7: cmp    $0xe,%ecx */
0x0f, 0x84, 0x80, 0x00, 0x00, 0x00,       /*ca: je     150 <op_mul+0x150> */
0x0f, 0xb6, 0x08,                         /*d0: movzbl (%rax),%ecx */
0x83, 0xf9, 0x06,                         /*d3: cmp    $0x6,%ecx */
0x75, 0x5e,                               /*d6: jne    136 <op_mul+0x136> */
0xf2, 0x0f, 0x10, 0x40, 0x18,             /*d8: movsd  0x18(%rax),%xmm0 */
0xeb, 0x3c,                               /*dd: jmp    11b <op_mul+0x11b> */
0x81, 0xf9, 0x03, 0x06, 0x00, 0x00,       /*df: cmp    $0x603,%ecx */
0x75, 0x22,                               /*e5: jne    109 <op_mul+0x109> */
0xd1, 0xfa,                               /*e7: sar    %edx */
0xf2, 0x0f, 0x2a, 0xc2,                   /*e9: cvtsi2sd %edx,%xmm0 */
0xf2, 0x0f, 0x59, 0x46, 0x18,             /*ed: mulsd  0x18(%rsi),%xmm0 */
0xeb, 0x27,                               /*f2: jmp    11b <op_mul+0x11b> */
0x81, 0xf9, 0x06, 0x03, 0x00, 0x00,       /*f4: cmp    $0x306,%ecx */
0x75, 0x30,                               /*fa: jne    12c <op_mul+0x12c> */
0xd1, 0xfe,                               /*fc: sar    %esi */
0xf2, 0x0f, 0x2a, 0xc6,                   /*fe: cvtsi2sd %esi,%xmm0 */
0xf2, 0x0f, 0x59, 0x42, 0x18,             /*102: mulsd  0x18(%rdx),%xmm0 */
0xeb, 0x12,                               /*107: jmp    11b <op_mul+0x11b> */
0x81, 0xf9, 0x06, 0x06, 0x00, 0x00,       /*109: cmp    $0x606,%ecx */
0x75, 0x1b,                               /*10f: jne    12c <op_mul+0x12c> */
0xf2, 0x0f, 0x10, 0x46, 0x18,             /*111: movsd  0x18(%rsi),%xmm0 */
0xf2, 0x0f, 0x59, 0x42, 0x18,             /*116: mulsd  0x18(%rdx),%xmm0 */
0x4c, 0x89, 0xf7,                         /*11b: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*11e: callq  123 <op_mul+0x123> */
0x49, 0x89, 0x87, 0x00, 0x08, 0xab, 0x00, /*123: mov    %rax,0xab0800(%r15) */
0xeb, 0x24,                               /*12a: jmp    150 <op_mul+0x150> */
0x48, 0x89, 0xdf,                         /*12c: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*12f: callq  134 <op_mul+0x134> */
0xeb, 0x1a,                               /*134: jmp    150 <op_mul+0x150> */
0x83, 0xf9, 0x03,                         /*136: cmp    $0x3,%ecx */
0x75, 0x15,                               /*139: jne    150 <op_mul+0x150> */
0x49, 0xc7, 0x87, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*13b: movq   $0x0,0xab0800(%r15) */
0x83, 0xc8, 0x01,                         /*146: or     $0x1,%eax */
0x41, 0x89, 0x87, 0x00, 0x08, 0xab, 0x00, /*149: mov    %eax,0xab0800(%r15) */
0x48, 0x89, 0xdf,                         /*150: mov    %rbx,%rdi */
0x5b,                                     /*153: pop    %rbx */
0x41, 0x5e,                               /*154: pop    %r14 */
0x41, 0x5f,                               /*156: pop    %r15 */

};
static void op_mul_link(uint8_t *op) {
  *((int32_t *)(op + 167)) = (uint32_t)(((uint8_t *)mrb_fixnum_mul) + (0) - (op + 167));
  *((int32_t *)(op + 287)) = (uint32_t)(((uint8_t *)mrb_word_boxing_float_value) + (0) - (op + 287));
  *((int32_t *)(op + 304)) = (uint32_t)(((uint8_t *)op_send) + (0) - (op + 304));
}

static void op_mul_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = a * 8 + 0;
  *((int32_t *)(op + 87)) = a * 8 + 8;
  *((int32_t *)(op + 294)) = a * 8 + 0;
  *((int32_t *)(op + 318)) = a * 8 + 0;
  *((int32_t *)(op + 332)) = a * 8 + 0;
}

static void op_mul_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_mul_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 17..20], [8, 8, 83..86], [8, 0, 244..247]]} */
static uint8_t op_div[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x48, 0x8b, 0x7b, 0x50,                   /*a: mov    0x50(%rbx),%rdi */
0x49, 0x8b, 0x86, 0x00, 0x08, 0xab, 0x00, /*e: mov    0xab0800(%r14),%rax */
0x48, 0x83, 0xf8, 0x07,                   /*15: cmp    $0x7,%rax */
0x73, 0x13,                               /*19: jae    2e <op_div+0x2e> */
0xb9, 0x55, 0x00, 0x00, 0x00,             /*1b: mov    $0x55,%ecx */
0x0f, 0xa3, 0xc1,                         /*20: bt     %eax,%ecx */
0x73, 0x09,                               /*23: jae    2e <op_div+0x2e> */
0x8b, 0x14, 0x85, 0x00, 0x00, 0x00, 0x00, /*25: mov    0x0(,%rax,4),%edx */
0xeb, 0x19,                               /*2c: jmp    47 <op_div+0x47> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*2e: mov    $0x3,%edx */
0xa8, 0x01,                               /*33: test   $0x1,%al */
0x75, 0x10,                               /*35: jne    47 <op_div+0x47> */
0x0f, 0xb6, 0xc8,                         /*37: movzbl %al,%ecx */
0xba, 0x04, 0x00, 0x00, 0x00,             /*3a: mov    $0x4,%edx */
0x83, 0xf9, 0x0e,                         /*3f: cmp    $0xe,%ecx */
0x74, 0x03,                               /*42: je     47 <op_div+0x47> */
0x0f, 0xb6, 0x10,                         /*44: movzbl (%rax),%edx */
0xc1, 0xe2, 0x08,                         /*47: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*4a: and    $0xffff00,%edx */
0x49, 0x8b, 0x8e, 0x08, 0x08, 0xab, 0x00, /*50: mov    0xab0808(%r14),%rcx */
0x48, 0x83, 0xf9, 0x07,                   /*57: cmp    $0x7,%rcx */
0x73, 0x13,                               /*5b: jae    70 <op_div+0x70> */
0xbe, 0x55, 0x00, 0x00, 0x00,             /*5d: mov    $0x55,%esi */
0x0f, 0xa3, 0xce,                         /*62: bt     %ecx,%esi */
0x73, 0x09,                               /*65: jae    70 <op_div+0x70> */
0x8b, 0x34, 0x8d, 0x00, 0x00, 0x00, 0x00, /*67: mov    0x0(,%rcx,4),%esi */
0xeb, 0x1c,                               /*6e: jmp    8c <op_div+0x8c> */
0xbe, 0x03, 0x00, 0x00, 0x00,             /*70: mov    $0x3,%esi */
0xf6, 0xc1, 0x01,                         /*75: test   $0x1,%cl */
0x75, 0x12,                               /*78: jne    8c <op_div+0x8c> */
0x44, 0x0f, 0xb6, 0xc1,                   /*7a: movzbl %cl,%r8d */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*7e: mov    $0x4,%esi */
0x41, 0x83, 0xf8, 0x0e,                   /*83: cmp    $0xe,%r8d */
0x74, 0x03,                               /*87: je     8c <op_div+0x8c> */
0x0f, 0xb6, 0x31,                         /*89: movzbl (%rcx),%esi */
0x40, 0x0f, 0xb6, 0xf6,                   /*8c: movzbl %sil,%esi */
0x09, 0xd6,                               /*90: or     %edx,%esi */
0x81, 0xfe, 0x02, 0x06, 0x00, 0x00,       /*92: cmp    $0x602,%esi */
0x7f, 0x12,                               /*98: jg     ac <op_div+0xac> */
0x81, 0xfe, 0x03, 0x03, 0x00, 0x00,       /*9a: cmp    $0x303,%esi */
0x75, 0x23,                               /*a0: jne    c5 <op_div+0xc5> */
0xd1, 0xf8,                               /*a2: sar    %eax */
0xd1, 0xf9,                               /*a4: sar    %ecx */
0xf2, 0x0f, 0x2a, 0xc0,                   /*a6: cvtsi2sd %eax,%xmm0 */
0xeb, 0x0f,                               /*aa: jmp    bb <op_div+0xbb> */
0x81, 0xfe, 0x03, 0x06, 0x00, 0x00,       /*ac: cmp    $0x603,%esi */
0x75, 0x26,                               /*b2: jne    da <op_div+0xda> */
0xf2, 0x0f, 0x10, 0x40, 0x18,             /*b4: movsd  0x18(%rax),%xmm0 */
0xd1, 0xf9,                               /*b9: sar    %ecx */
0xf2, 0x0f, 0x2a, 0xc9,                   /*bb: cvtsi2sd %ecx,%xmm1 */
0xf2, 0x0f, 0x5e, 0xc1,                   /*bf: divsd  %xmm1,%xmm0 */
0xeb, 0x27,                               /*c3: jmp    ec <op_div+0xec> */
0x81, 0xfe, 0x06, 0x03, 0x00, 0x00,       /*c5: cmp    $0x306,%esi */
0x75, 0x2d,                               /*cb: jne    fa <op_div+0xfa> */
0xd1, 0xf8,                               /*cd: sar    %eax */
0xf2, 0x0f, 0x2a, 0xc0,                   /*cf: cvtsi2sd %eax,%xmm0 */
0xf2, 0x0f, 0x5e, 0x41, 0x18,             /*d3: divsd  0x18(%rcx),%xmm0 */
0xeb, 0x12,                               /*d8: jmp    ec <op_div+0xec> */
0x81, 0xfe, 0x06, 0x06, 0x00, 0x00,       /*da: cmp    $0x606,%esi */
0x75, 0x18,                               /*e0: jne    fa <op_div+0xfa> */
0xf2, 0x0f, 0x10, 0x40, 0x18,             /*e2: movsd  0x18(%rax),%xmm0 */
0xf2, 0x0f, 0x5e, 0x41, 0x18,             /*e7: divsd  0x18(%rcx),%xmm0 */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*ec: callq  f1 <op_div+0xf1> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*f1: mov    %rax,0xab0800(%r14) */
0xeb, 0x08,                               /*f8: jmp    102 <op_div+0x102> */
0x48, 0x89, 0xdf,                         /*fa: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*fd: callq  102 <op_div+0x102> */
0x48, 0x89, 0xdf,                         /*102: mov    %rbx,%rdi */
0x5b,                                     /*105: pop    %rbx */
0x41, 0x5e,                               /*106: pop    %r14 */

};
static void op_div_link(uint8_t *op) {
  *((int32_t *)(op + 237)) = (uint32_t)(((uint8_t *)mrb_word_boxing_float_value) + (0) - (op + 237));
  *((int32_t *)(op + 254)) = (uint32_t)(((uint8_t *)op_send) + (0) - (op + 254));
}

static void op_div_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 8 + 0;
  *((int32_t *)(op + 83)) = a * 8 + 8;
  *((int32_t *)(op + 244)) = a * 8 + 0;
}

static void op_div_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_div_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[8, 0, 17..20], [8, 8, 24..27], [8, 0, 40..43], [8, 0, 56..59], [8, 8, 122..125], [8, 0, 283..286], [8, 0, 293..296], [8, 0, 306..309], [8, 0, 319..322], [1, 0, 335..338]], "b"=>[[1, 0, 340..343]], "c"=>[[1, 0, 346..349]]} */
static uint8_t op_eq[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x18,                   /*6: mov    0x18(%r14),%rbx */
0x49, 0x8b, 0x7e, 0x50,                   /*a: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0xb3, 0x00, 0x08, 0xab, 0x00, /*e: mov    0xab0800(%rbx),%rsi */
0x48, 0x8b, 0x93, 0x08, 0x08, 0xab, 0x00, /*15: mov    0xab0808(%rbx),%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*1c: callq  21 <op_eq+0x21> */
0x84, 0xc0,                               /*21: test   %al,%al */
0x74, 0x10,                               /*23: je     35 <op_eq+0x35> */
0x48, 0xc7, 0x83, 0x00, 0x08, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*25: movq   $0x4,0xab0800(%rbx) */
0xe9, 0x31, 0x01, 0x00, 0x00,             /*30: jmpq   166 <op_eq+0x166> */
0x48, 0x8b, 0x83, 0x00, 0x08, 0xab, 0x00, /*35: mov    0xab0800(%rbx),%rax */
0x48, 0x83, 0xf8, 0x07,                   /*3c: cmp    $0x7,%rax */
0x73, 0x13,                               /*40: jae    55 <op_eq+0x55> */
0xb9, 0x55, 0x00, 0x00, 0x00,             /*42: mov    $0x55,%ecx */
0x0f, 0xa3, 0xc1,                         /*47: bt     %eax,%ecx */
0x73, 0x09,                               /*4a: jae    55 <op_eq+0x55> */
0x8b, 0x14, 0x85, 0x00, 0x00, 0x00, 0x00, /*4c: mov    0x0(,%rax,4),%edx */
0xeb, 0x19,                               /*53: jmp    6e <op_eq+0x6e> */
0xba, 0x03, 0x00, 0x00, 0x00,             /*55: mov    $0x3,%edx */
0xa8, 0x01,                               /*5a: test   $0x1,%al */
0x75, 0x10,                               /*5c: jne    6e <op_eq+0x6e> */
0x0f, 0xb6, 0xc8,                         /*5e: movzbl %al,%ecx */
0xba, 0x04, 0x00, 0x00, 0x00,             /*61: mov    $0x4,%edx */
0x83, 0xf9, 0x0e,                         /*66: cmp    $0xe,%ecx */
0x74, 0x03,                               /*69: je     6e <op_eq+0x6e> */
0x0f, 0xb6, 0x10,                         /*6b: movzbl (%rax),%edx */
0xc1, 0xe2, 0x08,                         /*6e: shl    $0x8,%edx */
0x81, 0xe2, 0x00, 0xff, 0xff, 0x00,       /*71: and    $0xffff00,%edx */
0x48, 0x8b, 0x8b, 0x08, 0x08, 0xab, 0x00, /*77: mov    0xab0808(%rbx),%rcx */
0x48, 0x83, 0xf9, 0x07,                   /*7e: cmp    $0x7,%rcx */
0x73, 0x13,                               /*82: jae    97 <op_eq+0x97> */
0xbe, 0x55, 0x00, 0x00, 0x00,             /*84: mov    $0x55,%esi */
0x0f, 0xa3, 0xce,                         /*89: bt     %ecx,%esi */
0x73, 0x09,                               /*8c: jae    97 <op_eq+0x97> */
0x8b, 0x34, 0x8d, 0x00, 0x00, 0x00, 0x00, /*8e: mov    0x0(,%rcx,4),%esi */
0xeb, 0x1a,                               /*95: jmp    b1 <op_eq+0xb1> */
0xbe, 0x03, 0x00, 0x00, 0x00,             /*97: mov    $0x3,%esi */
0xf6, 0xc1, 0x01,                         /*9c: test   $0x1,%cl */
0x75, 0x10,                               /*9f: jne    b1 <op_eq+0xb1> */
0x0f, 0xb6, 0xf9,                         /*a1: movzbl %cl,%edi */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*a4: mov    $0x4,%esi */
0x83, 0xff, 0x0e,                         /*a9: cmp    $0xe,%edi */
0x74, 0x03,                               /*ac: je     b1 <op_eq+0xb1> */
0x0f, 0xb6, 0x31,                         /*ae: movzbl (%rcx),%esi */
0x40, 0x0f, 0xb6, 0xf6,                   /*b1: movzbl %sil,%esi */
0x09, 0xd6,                               /*b5: or     %edx,%esi */
0x81, 0xfe, 0x02, 0x06, 0x00, 0x00,       /*b7: cmp    $0x602,%esi */
0x7f, 0x11,                               /*bd: jg     d0 <op_eq+0xd0> */
0x81, 0xfe, 0x03, 0x03, 0x00, 0x00,       /*bf: cmp    $0x303,%esi */
0x75, 0x24,                               /*c5: jne    eb <op_eq+0xeb> */
0x31, 0xc1,                               /*c7: xor    %eax,%ecx */
0x83, 0xf9, 0x02,                         /*c9: cmp    $0x2,%ecx */
0x19, 0xc0,                               /*cc: sbb    %eax,%eax */
0xeb, 0x43,                               /*ce: jmp    113 <op_eq+0x113> */
0x81, 0xfe, 0x03, 0x06, 0x00, 0x00,       /*d0: cmp    $0x603,%esi */
0x75, 0x23,                               /*d6: jne    fb <op_eq+0xfb> */
0xd1, 0xf9,                               /*d8: sar    %ecx */
0xf2, 0x0f, 0x2a, 0xc1,                   /*da: cvtsi2sd %ecx,%xmm0 */
0xf2, 0x0f, 0xc2, 0x40, 0x18, 0x00,       /*de: cmpeqsd 0x18(%rax),%xmm0 */
0x66, 0x48, 0x0f, 0x7e, 0xc0,             /*e4: movq   %xmm0,%rax */
0xeb, 0x28,                               /*e9: jmp    113 <op_eq+0x113> */
0x81, 0xfe, 0x06, 0x03, 0x00, 0x00,       /*eb: cmp    $0x306,%esi */
0x75, 0x56,                               /*f1: jne    149 <op_eq+0x149> */
0xd1, 0xf8,                               /*f3: sar    %eax */
0xf2, 0x0f, 0x2a, 0xc0,                   /*f5: cvtsi2sd %eax,%xmm0 */
0xeb, 0x0d,                               /*f9: jmp    108 <op_eq+0x108> */
0x81, 0xfe, 0x06, 0x06, 0x00, 0x00,       /*fb: cmp    $0x606,%esi */
0x75, 0x46,                               /*101: jne    149 <op_eq+0x149> */
0xf2, 0x0f, 0x10, 0x40, 0x18,             /*103: movsd  0x18(%rax),%xmm0 */
0xf2, 0x0f, 0xc2, 0x41, 0x18, 0x00,       /*108: cmpeqsd 0x18(%rcx),%xmm0 */
0x66, 0x48, 0x0f, 0x7e, 0xc0,             /*10e: movq   %xmm0,%rax */
0x83, 0xe0, 0x01,                         /*113: and    $0x1,%eax */
0x85, 0xc0,                               /*116: test   %eax,%eax */
0x48, 0xc7, 0x83, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*118: movq   $0x0,0xab0800(%rbx) */
0xc7, 0x83, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*123: movl   $0x2,0xab0800(%rbx) */
0x74, 0x0d,                               /*12d: je     13c <op_eq+0x13c> */
0x48, 0xc7, 0x83, 0x00, 0x08, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*12f: movq   $0x4,0xab0800(%rbx) */
0xeb, 0x2a,                               /*13a: jmp    166 <op_eq+0x166> */
0x48, 0xc7, 0x83, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*13c: movq   $0x2,0xab0800(%rbx) */
0xeb, 0x1d,                               /*147: jmp    166 <op_eq+0x166> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*149: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*14e: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*153: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*158: mov    $0xcd0000,%r8d */
0x4c, 0x89, 0xf7,                         /*15e: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*161: callq  166 <op_eq+0x166> */
0x4c, 0x89, 0xf7,                         /*166: mov    %r14,%rdi */
0x5b,                                     /*169: pop    %rbx */
0x41, 0x5e,                               /*16a: pop    %r14 */

};
static void op_eq_link(uint8_t *op) {
  *((int32_t *)(op + 29)) = (uint32_t)(((uint8_t *)mrb_obj_eq) + (0) - (op + 29));
  *((int32_t *)(op + 354)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 354));
}

static void op_eq_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = a * 8 + 0;
  *((int32_t *)(op + 24)) = a * 8 + 8;
  *((int32_t *)(op + 40)) = a * 8 + 0;
  *((int32_t *)(op + 56)) = a * 8 + 0;
  *((int32_t *)(op + 122)) = a * 8 + 8;
  *((int32_t *)(op + 283)) = a * 8 + 0;
  *((int32_t *)(op + 293)) = a * 8 + 0;
  *((int32_t *)(op + 306)) = a * 8 + 0;
  *((int32_t *)(op + 319)) = a * 8 + 0;
  *((int32_t *)(op + 335)) = a * 1 + 0;
  *((int32_t *)(op + 340)) = b * 1 + 0;
  *((int32_t *)(op + 346)) = c * 1 + 0;
}

static void op_eq_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_eq_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 13..16], [8, 8, 80..83], [8, 0, 241..244], [8, 0, 251..254], [8, 0, 264..267], [8, 0, 277..280], [1, 0, 293..296]], "b"=>[[1, 0, 298..301]], "c"=>[[1, 0, 304..307]]} */
static uint8_t op_lt[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x46, 0x18,                   /*6: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x88, 0x00, 0x08, 0xab, 0x00, /*a: mov    0xab0800(%rax),%rcx */
0x48, 0x83, 0xf9, 0x07,                   /*11: cmp    $0x7,%rcx */
0x73, 0x13,                               /*15: jae    2a <op_lt+0x2a> */
0xba, 0x55, 0x00, 0x00, 0x00,             /*17: mov    $0x55,%edx */
0x0f, 0xa3, 0xca,                         /*1c: bt     %ecx,%edx */
0x73, 0x09,                               /*1f: jae    2a <op_lt+0x2a> */
0x8b, 0x34, 0x8d, 0x00, 0x00, 0x00, 0x00, /*21: mov    0x0(,%rcx,4),%esi */
0xeb, 0x1a,                               /*28: jmp    44 <op_lt+0x44> */
0xbe, 0x03, 0x00, 0x00, 0x00,             /*2a: mov    $0x3,%esi */
0xf6, 0xc1, 0x01,                         /*2f: test   $0x1,%cl */
0x75, 0x10,                               /*32: jne    44 <op_lt+0x44> */
0x0f, 0xb6, 0xd1,                         /*34: movzbl %cl,%edx */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*37: mov    $0x4,%esi */
0x83, 0xfa, 0x0e,                         /*3c: cmp    $0xe,%edx */
0x74, 0x03,                               /*3f: je     44 <op_lt+0x44> */
0x0f, 0xb6, 0x31,                         /*41: movzbl (%rcx),%esi */
0xc1, 0xe6, 0x08,                         /*44: shl    $0x8,%esi */
0x81, 0xe6, 0x00, 0xff, 0xff, 0x00,       /*47: and    $0xffff00,%esi */
0x48, 0x8b, 0x90, 0x08, 0x08, 0xab, 0x00, /*4d: mov    0xab0808(%rax),%rdx */
0x48, 0x83, 0xfa, 0x07,                   /*54: cmp    $0x7,%rdx */
0x73, 0x13,                               /*58: jae    6d <op_lt+0x6d> */
0xbf, 0x55, 0x00, 0x00, 0x00,             /*5a: mov    $0x55,%edi */
0x0f, 0xa3, 0xd7,                         /*5f: bt     %edx,%edi */
0x73, 0x09,                               /*62: jae    6d <op_lt+0x6d> */
0x8b, 0x3c, 0x95, 0x00, 0x00, 0x00, 0x00, /*64: mov    0x0(,%rdx,4),%edi */
0xeb, 0x1a,                               /*6b: jmp    87 <op_lt+0x87> */
0xbf, 0x03, 0x00, 0x00, 0x00,             /*6d: mov    $0x3,%edi */
0xf6, 0xc2, 0x01,                         /*72: test   $0x1,%dl */
0x75, 0x10,                               /*75: jne    87 <op_lt+0x87> */
0x0f, 0xb6, 0xda,                         /*77: movzbl %dl,%ebx */
0xbf, 0x04, 0x00, 0x00, 0x00,             /*7a: mov    $0x4,%edi */
0x83, 0xfb, 0x0e,                         /*7f: cmp    $0xe,%ebx */
0x74, 0x03,                               /*82: je     87 <op_lt+0x87> */
0x0f, 0xb6, 0x3a,                         /*84: movzbl (%rdx),%edi */
0x40, 0x0f, 0xb6, 0xff,                   /*87: movzbl %dil,%edi */
0x09, 0xf7,                               /*8b: or     %esi,%edi */
0x81, 0xff, 0x02, 0x06, 0x00, 0x00,       /*8d: cmp    $0x602,%edi */
0x7f, 0x13,                               /*93: jg     a8 <op_lt+0xa8> */
0x81, 0xff, 0x03, 0x03, 0x00, 0x00,       /*95: cmp    $0x303,%edi */
0x75, 0x1b,                               /*9b: jne    b8 <op_lt+0xb8> */
0xd1, 0xf9,                               /*9d: sar    %ecx */
0xd1, 0xfa,                               /*9f: sar    %edx */
0x39, 0xd1,                               /*a1: cmp    %edx,%ecx */
0x0f, 0x9c, 0xc1,                         /*a3: setl   %cl */
0xeb, 0x41,                               /*a6: jmp    e9 <op_lt+0xe9> */
0x81, 0xff, 0x03, 0x06, 0x00, 0x00,       /*a8: cmp    $0x603,%edi */
0x75, 0x24,                               /*ae: jne    d4 <op_lt+0xd4> */
0xd1, 0xfa,                               /*b0: sar    %edx */
0xf2, 0x0f, 0x2a, 0xc2,                   /*b2: cvtsi2sd %edx,%xmm0 */
0xeb, 0x29,                               /*b6: jmp    e1 <op_lt+0xe1> */
0x81, 0xff, 0x06, 0x03, 0x00, 0x00,       /*b8: cmp    $0x306,%edi */
0x75, 0x5f,                               /*be: jne    11f <op_lt+0x11f> */
0xd1, 0xf9,                               /*c0: sar    %ecx */
0xf2, 0x0f, 0x2a, 0xc1,                   /*c2: cvtsi2sd %ecx,%xmm0 */
0xf2, 0x0f, 0x10, 0x4a, 0x18,             /*c6: movsd  0x18(%rdx),%xmm1 */
0x66, 0x0f, 0x2e, 0xc8,                   /*cb: ucomisd %xmm0,%xmm1 */
0x0f, 0x97, 0xc1,                         /*cf: seta   %cl */
0xeb, 0x15,                               /*d2: jmp    e9 <op_lt+0xe9> */
0x81, 0xff, 0x06, 0x06, 0x00, 0x00,       /*d4: cmp    $0x606,%edi */
0x75, 0x43,                               /*da: jne    11f <op_lt+0x11f> */
0xf2, 0x0f, 0x10, 0x42, 0x18,             /*dc: movsd  0x18(%rdx),%xmm0 */
0x66, 0x0f, 0x2e, 0x41, 0x18,             /*e1: ucomisd 0x18(%rcx),%xmm0 */
0x0f, 0x97, 0xc1,                         /*e6: seta   %cl */
0x0f, 0xb6, 0xc9,                         /*e9: movzbl %cl,%ecx */
0x85, 0xc9,                               /*ec: test   %ecx,%ecx */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*ee: movq   $0x0,0xab0800(%rax) */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*f9: movl   $0x2,0xab0800(%rax) */
0x74, 0x0d,                               /*103: je     112 <op_lt+0x112> */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*105: movq   $0x4,0xab0800(%rax) */
0xeb, 0x2a,                               /*110: jmp    13c <op_lt+0x13c> */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*112: movq   $0x2,0xab0800(%rax) */
0xeb, 0x1d,                               /*11d: jmp    13c <op_lt+0x13c> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*11f: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*124: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*129: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*12e: mov    $0xcd0000,%r8d */
0x4c, 0x89, 0xf7,                         /*134: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*137: callq  13c <op_lt+0x13c> */
0x4c, 0x89, 0xf7,                         /*13c: mov    %r14,%rdi */
0x5b,                                     /*13f: pop    %rbx */
0x41, 0x5e,                               /*140: pop    %r14 */

};
static void op_lt_link(uint8_t *op) {
  *((int32_t *)(op + 312)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 312));
}

static void op_lt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = a * 8 + 0;
  *((int32_t *)(op + 80)) = a * 8 + 8;
  *((int32_t *)(op + 241)) = a * 8 + 0;
  *((int32_t *)(op + 251)) = a * 8 + 0;
  *((int32_t *)(op + 264)) = a * 8 + 0;
  *((int32_t *)(op + 277)) = a * 8 + 0;
  *((int32_t *)(op + 293)) = a * 1 + 0;
  *((int32_t *)(op + 298)) = b * 1 + 0;
  *((int32_t *)(op + 304)) = c * 1 + 0;
}

static void op_lt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_lt_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 13..16], [8, 8, 80..83], [8, 0, 241..244], [8, 0, 251..254], [8, 0, 264..267], [8, 0, 277..280], [1, 0, 293..296]], "b"=>[[1, 0, 298..301]], "c"=>[[1, 0, 304..307]]} */
static uint8_t op_le[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x46, 0x18,                   /*6: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x88, 0x00, 0x08, 0xab, 0x00, /*a: mov    0xab0800(%rax),%rcx */
0x48, 0x83, 0xf9, 0x07,                   /*11: cmp    $0x7,%rcx */
0x73, 0x13,                               /*15: jae    2a <op_le+0x2a> */
0xba, 0x55, 0x00, 0x00, 0x00,             /*17: mov    $0x55,%edx */
0x0f, 0xa3, 0xca,                         /*1c: bt     %ecx,%edx */
0x73, 0x09,                               /*1f: jae    2a <op_le+0x2a> */
0x8b, 0x34, 0x8d, 0x00, 0x00, 0x00, 0x00, /*21: mov    0x0(,%rcx,4),%esi */
0xeb, 0x1a,                               /*28: jmp    44 <op_le+0x44> */
0xbe, 0x03, 0x00, 0x00, 0x00,             /*2a: mov    $0x3,%esi */
0xf6, 0xc1, 0x01,                         /*2f: test   $0x1,%cl */
0x75, 0x10,                               /*32: jne    44 <op_le+0x44> */
0x0f, 0xb6, 0xd1,                         /*34: movzbl %cl,%edx */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*37: mov    $0x4,%esi */
0x83, 0xfa, 0x0e,                         /*3c: cmp    $0xe,%edx */
0x74, 0x03,                               /*3f: je     44 <op_le+0x44> */
0x0f, 0xb6, 0x31,                         /*41: movzbl (%rcx),%esi */
0xc1, 0xe6, 0x08,                         /*44: shl    $0x8,%esi */
0x81, 0xe6, 0x00, 0xff, 0xff, 0x00,       /*47: and    $0xffff00,%esi */
0x48, 0x8b, 0x90, 0x08, 0x08, 0xab, 0x00, /*4d: mov    0xab0808(%rax),%rdx */
0x48, 0x83, 0xfa, 0x07,                   /*54: cmp    $0x7,%rdx */
0x73, 0x13,                               /*58: jae    6d <op_le+0x6d> */
0xbf, 0x55, 0x00, 0x00, 0x00,             /*5a: mov    $0x55,%edi */
0x0f, 0xa3, 0xd7,                         /*5f: bt     %edx,%edi */
0x73, 0x09,                               /*62: jae    6d <op_le+0x6d> */
0x8b, 0x3c, 0x95, 0x00, 0x00, 0x00, 0x00, /*64: mov    0x0(,%rdx,4),%edi */
0xeb, 0x1a,                               /*6b: jmp    87 <op_le+0x87> */
0xbf, 0x03, 0x00, 0x00, 0x00,             /*6d: mov    $0x3,%edi */
0xf6, 0xc2, 0x01,                         /*72: test   $0x1,%dl */
0x75, 0x10,                               /*75: jne    87 <op_le+0x87> */
0x0f, 0xb6, 0xda,                         /*77: movzbl %dl,%ebx */
0xbf, 0x04, 0x00, 0x00, 0x00,             /*7a: mov    $0x4,%edi */
0x83, 0xfb, 0x0e,                         /*7f: cmp    $0xe,%ebx */
0x74, 0x03,                               /*82: je     87 <op_le+0x87> */
0x0f, 0xb6, 0x3a,                         /*84: movzbl (%rdx),%edi */
0x40, 0x0f, 0xb6, 0xff,                   /*87: movzbl %dil,%edi */
0x09, 0xf7,                               /*8b: or     %esi,%edi */
0x81, 0xff, 0x02, 0x06, 0x00, 0x00,       /*8d: cmp    $0x602,%edi */
0x7f, 0x13,                               /*93: jg     a8 <op_le+0xa8> */
0x81, 0xff, 0x03, 0x03, 0x00, 0x00,       /*95: cmp    $0x303,%edi */
0x75, 0x1b,                               /*9b: jne    b8 <op_le+0xb8> */
0xd1, 0xf9,                               /*9d: sar    %ecx */
0xd1, 0xfa,                               /*9f: sar    %edx */
0x39, 0xd1,                               /*a1: cmp    %edx,%ecx */
0x0f, 0x9e, 0xc1,                         /*a3: setle  %cl */
0xeb, 0x41,                               /*a6: jmp    e9 <op_le+0xe9> */
0x81, 0xff, 0x03, 0x06, 0x00, 0x00,       /*a8: cmp    $0x603,%edi */
0x75, 0x24,                               /*ae: jne    d4 <op_le+0xd4> */
0xd1, 0xfa,                               /*b0: sar    %edx */
0xf2, 0x0f, 0x2a, 0xc2,                   /*b2: cvtsi2sd %edx,%xmm0 */
0xeb, 0x29,                               /*b6: jmp    e1 <op_le+0xe1> */
0x81, 0xff, 0x06, 0x03, 0x00, 0x00,       /*b8: cmp    $0x306,%edi */
0x75, 0x5f,                               /*be: jne    11f <op_le+0x11f> */
0xd1, 0xf9,                               /*c0: sar    %ecx */
0xf2, 0x0f, 0x2a, 0xc1,                   /*c2: cvtsi2sd %ecx,%xmm0 */
0xf2, 0x0f, 0x10, 0x4a, 0x18,             /*c6: movsd  0x18(%rdx),%xmm1 */
0x66, 0x0f, 0x2e, 0xc8,                   /*cb: ucomisd %xmm0,%xmm1 */
0x0f, 0x93, 0xc1,                         /*cf: setae  %cl */
0xeb, 0x15,                               /*d2: jmp    e9 <op_le+0xe9> */
0x81, 0xff, 0x06, 0x06, 0x00, 0x00,       /*d4: cmp    $0x606,%edi */
0x75, 0x43,                               /*da: jne    11f <op_le+0x11f> */
0xf2, 0x0f, 0x10, 0x42, 0x18,             /*dc: movsd  0x18(%rdx),%xmm0 */
0x66, 0x0f, 0x2e, 0x41, 0x18,             /*e1: ucomisd 0x18(%rcx),%xmm0 */
0x0f, 0x93, 0xc1,                         /*e6: setae  %cl */
0x0f, 0xb6, 0xc9,                         /*e9: movzbl %cl,%ecx */
0x85, 0xc9,                               /*ec: test   %ecx,%ecx */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*ee: movq   $0x0,0xab0800(%rax) */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*f9: movl   $0x2,0xab0800(%rax) */
0x74, 0x0d,                               /*103: je     112 <op_le+0x112> */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*105: movq   $0x4,0xab0800(%rax) */
0xeb, 0x2a,                               /*110: jmp    13c <op_le+0x13c> */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*112: movq   $0x2,0xab0800(%rax) */
0xeb, 0x1d,                               /*11d: jmp    13c <op_le+0x13c> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*11f: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*124: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*129: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*12e: mov    $0xcd0000,%r8d */
0x4c, 0x89, 0xf7,                         /*134: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*137: callq  13c <op_le+0x13c> */
0x4c, 0x89, 0xf7,                         /*13c: mov    %r14,%rdi */
0x5b,                                     /*13f: pop    %rbx */
0x41, 0x5e,                               /*140: pop    %r14 */

};
static void op_le_link(uint8_t *op) {
  *((int32_t *)(op + 312)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 312));
}

static void op_le_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = a * 8 + 0;
  *((int32_t *)(op + 80)) = a * 8 + 8;
  *((int32_t *)(op + 241)) = a * 8 + 0;
  *((int32_t *)(op + 251)) = a * 8 + 0;
  *((int32_t *)(op + 264)) = a * 8 + 0;
  *((int32_t *)(op + 277)) = a * 8 + 0;
  *((int32_t *)(op + 293)) = a * 1 + 0;
  *((int32_t *)(op + 298)) = b * 1 + 0;
  *((int32_t *)(op + 304)) = c * 1 + 0;
}

static void op_le_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_le_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 13..16], [8, 8, 80..83], [8, 0, 241..244], [8, 0, 251..254], [8, 0, 264..267], [8, 0, 277..280], [1, 0, 293..296]], "b"=>[[1, 0, 298..301]], "c"=>[[1, 0, 304..307]]} */
static uint8_t op_gt[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x46, 0x18,                   /*6: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x88, 0x00, 0x08, 0xab, 0x00, /*a: mov    0xab0800(%rax),%rcx */
0x48, 0x83, 0xf9, 0x07,                   /*11: cmp    $0x7,%rcx */
0x73, 0x13,                               /*15: jae    2a <op_gt+0x2a> */
0xba, 0x55, 0x00, 0x00, 0x00,             /*17: mov    $0x55,%edx */
0x0f, 0xa3, 0xca,                         /*1c: bt     %ecx,%edx */
0x73, 0x09,                               /*1f: jae    2a <op_gt+0x2a> */
0x8b, 0x34, 0x8d, 0x00, 0x00, 0x00, 0x00, /*21: mov    0x0(,%rcx,4),%esi */
0xeb, 0x1a,                               /*28: jmp    44 <op_gt+0x44> */
0xbe, 0x03, 0x00, 0x00, 0x00,             /*2a: mov    $0x3,%esi */
0xf6, 0xc1, 0x01,                         /*2f: test   $0x1,%cl */
0x75, 0x10,                               /*32: jne    44 <op_gt+0x44> */
0x0f, 0xb6, 0xd1,                         /*34: movzbl %cl,%edx */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*37: mov    $0x4,%esi */
0x83, 0xfa, 0x0e,                         /*3c: cmp    $0xe,%edx */
0x74, 0x03,                               /*3f: je     44 <op_gt+0x44> */
0x0f, 0xb6, 0x31,                         /*41: movzbl (%rcx),%esi */
0xc1, 0xe6, 0x08,                         /*44: shl    $0x8,%esi */
0x81, 0xe6, 0x00, 0xff, 0xff, 0x00,       /*47: and    $0xffff00,%esi */
0x48, 0x8b, 0x90, 0x08, 0x08, 0xab, 0x00, /*4d: mov    0xab0808(%rax),%rdx */
0x48, 0x83, 0xfa, 0x07,                   /*54: cmp    $0x7,%rdx */
0x73, 0x13,                               /*58: jae    6d <op_gt+0x6d> */
0xbf, 0x55, 0x00, 0x00, 0x00,             /*5a: mov    $0x55,%edi */
0x0f, 0xa3, 0xd7,                         /*5f: bt     %edx,%edi */
0x73, 0x09,                               /*62: jae    6d <op_gt+0x6d> */
0x8b, 0x3c, 0x95, 0x00, 0x00, 0x00, 0x00, /*64: mov    0x0(,%rdx,4),%edi */
0xeb, 0x1a,                               /*6b: jmp    87 <op_gt+0x87> */
0xbf, 0x03, 0x00, 0x00, 0x00,             /*6d: mov    $0x3,%edi */
0xf6, 0xc2, 0x01,                         /*72: test   $0x1,%dl */
0x75, 0x10,                               /*75: jne    87 <op_gt+0x87> */
0x0f, 0xb6, 0xda,                         /*77: movzbl %dl,%ebx */
0xbf, 0x04, 0x00, 0x00, 0x00,             /*7a: mov    $0x4,%edi */
0x83, 0xfb, 0x0e,                         /*7f: cmp    $0xe,%ebx */
0x74, 0x03,                               /*82: je     87 <op_gt+0x87> */
0x0f, 0xb6, 0x3a,                         /*84: movzbl (%rdx),%edi */
0x40, 0x0f, 0xb6, 0xff,                   /*87: movzbl %dil,%edi */
0x09, 0xf7,                               /*8b: or     %esi,%edi */
0x81, 0xff, 0x02, 0x06, 0x00, 0x00,       /*8d: cmp    $0x602,%edi */
0x7f, 0x13,                               /*93: jg     a8 <op_gt+0xa8> */
0x81, 0xff, 0x03, 0x03, 0x00, 0x00,       /*95: cmp    $0x303,%edi */
0x75, 0x27,                               /*9b: jne    c4 <op_gt+0xc4> */
0xd1, 0xf9,                               /*9d: sar    %ecx */
0xd1, 0xfa,                               /*9f: sar    %edx */
0x39, 0xd1,                               /*a1: cmp    %edx,%ecx */
0x0f, 0x9f, 0xc1,                         /*a3: setg   %cl */
0xeb, 0x41,                               /*a6: jmp    e9 <op_gt+0xe9> */
0x81, 0xff, 0x03, 0x06, 0x00, 0x00,       /*a8: cmp    $0x603,%edi */
0x75, 0x24,                               /*ae: jne    d4 <op_gt+0xd4> */
0xf2, 0x0f, 0x10, 0x41, 0x18,             /*b0: movsd  0x18(%rcx),%xmm0 */
0xd1, 0xfa,                               /*b5: sar    %edx */
0xf2, 0x0f, 0x2a, 0xca,                   /*b7: cvtsi2sd %edx,%xmm1 */
0x66, 0x0f, 0x2e, 0xc1,                   /*bb: ucomisd %xmm1,%xmm0 */
0x0f, 0x97, 0xc1,                         /*bf: seta   %cl */
0xeb, 0x25,                               /*c2: jmp    e9 <op_gt+0xe9> */
0x81, 0xff, 0x06, 0x03, 0x00, 0x00,       /*c4: cmp    $0x306,%edi */
0x75, 0x53,                               /*ca: jne    11f <op_gt+0x11f> */
0xd1, 0xf9,                               /*cc: sar    %ecx */
0xf2, 0x0f, 0x2a, 0xc1,                   /*ce: cvtsi2sd %ecx,%xmm0 */
0xeb, 0x0d,                               /*d2: jmp    e1 <op_gt+0xe1> */
0x81, 0xff, 0x06, 0x06, 0x00, 0x00,       /*d4: cmp    $0x606,%edi */
0x75, 0x43,                               /*da: jne    11f <op_gt+0x11f> */
0xf2, 0x0f, 0x10, 0x41, 0x18,             /*dc: movsd  0x18(%rcx),%xmm0 */
0x66, 0x0f, 0x2e, 0x42, 0x18,             /*e1: ucomisd 0x18(%rdx),%xmm0 */
0x0f, 0x97, 0xc1,                         /*e6: seta   %cl */
0x0f, 0xb6, 0xc9,                         /*e9: movzbl %cl,%ecx */
0x85, 0xc9,                               /*ec: test   %ecx,%ecx */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*ee: movq   $0x0,0xab0800(%rax) */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*f9: movl   $0x2,0xab0800(%rax) */
0x74, 0x0d,                               /*103: je     112 <op_gt+0x112> */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*105: movq   $0x4,0xab0800(%rax) */
0xeb, 0x2a,                               /*110: jmp    13c <op_gt+0x13c> */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*112: movq   $0x2,0xab0800(%rax) */
0xeb, 0x1d,                               /*11d: jmp    13c <op_gt+0x13c> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*11f: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*124: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*129: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*12e: mov    $0xcd0000,%r8d */
0x4c, 0x89, 0xf7,                         /*134: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*137: callq  13c <op_gt+0x13c> */
0x4c, 0x89, 0xf7,                         /*13c: mov    %r14,%rdi */
0x5b,                                     /*13f: pop    %rbx */
0x41, 0x5e,                               /*140: pop    %r14 */

};
static void op_gt_link(uint8_t *op) {
  *((int32_t *)(op + 312)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 312));
}

static void op_gt_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = a * 8 + 0;
  *((int32_t *)(op + 80)) = a * 8 + 8;
  *((int32_t *)(op + 241)) = a * 8 + 0;
  *((int32_t *)(op + 251)) = a * 8 + 0;
  *((int32_t *)(op + 264)) = a * 8 + 0;
  *((int32_t *)(op + 277)) = a * 8 + 0;
  *((int32_t *)(op + 293)) = a * 1 + 0;
  *((int32_t *)(op + 298)) = b * 1 + 0;
  *((int32_t *)(op + 304)) = c * 1 + 0;
}

static void op_gt_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_gt_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 13..16], [8, 8, 80..83], [8, 0, 241..244], [8, 0, 251..254], [8, 0, 264..267], [8, 0, 277..280], [1, 0, 293..296]], "b"=>[[1, 0, 298..301]], "c"=>[[1, 0, 304..307]]} */
static uint8_t op_ge[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x49, 0x89, 0xfe,                         /*3: mov    %rdi,%r14 */
0x49, 0x8b, 0x46, 0x18,                   /*6: mov    0x18(%r14),%rax */
0x48, 0x8b, 0x88, 0x00, 0x08, 0xab, 0x00, /*a: mov    0xab0800(%rax),%rcx */
0x48, 0x83, 0xf9, 0x07,                   /*11: cmp    $0x7,%rcx */
0x73, 0x13,                               /*15: jae    2a <op_ge+0x2a> */
0xba, 0x55, 0x00, 0x00, 0x00,             /*17: mov    $0x55,%edx */
0x0f, 0xa3, 0xca,                         /*1c: bt     %ecx,%edx */
0x73, 0x09,                               /*1f: jae    2a <op_ge+0x2a> */
0x8b, 0x34, 0x8d, 0x00, 0x00, 0x00, 0x00, /*21: mov    0x0(,%rcx,4),%esi */
0xeb, 0x1a,                               /*28: jmp    44 <op_ge+0x44> */
0xbe, 0x03, 0x00, 0x00, 0x00,             /*2a: mov    $0x3,%esi */
0xf6, 0xc1, 0x01,                         /*2f: test   $0x1,%cl */
0x75, 0x10,                               /*32: jne    44 <op_ge+0x44> */
0x0f, 0xb6, 0xd1,                         /*34: movzbl %cl,%edx */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*37: mov    $0x4,%esi */
0x83, 0xfa, 0x0e,                         /*3c: cmp    $0xe,%edx */
0x74, 0x03,                               /*3f: je     44 <op_ge+0x44> */
0x0f, 0xb6, 0x31,                         /*41: movzbl (%rcx),%esi */
0xc1, 0xe6, 0x08,                         /*44: shl    $0x8,%esi */
0x81, 0xe6, 0x00, 0xff, 0xff, 0x00,       /*47: and    $0xffff00,%esi */
0x48, 0x8b, 0x90, 0x08, 0x08, 0xab, 0x00, /*4d: mov    0xab0808(%rax),%rdx */
0x48, 0x83, 0xfa, 0x07,                   /*54: cmp    $0x7,%rdx */
0x73, 0x13,                               /*58: jae    6d <op_ge+0x6d> */
0xbf, 0x55, 0x00, 0x00, 0x00,             /*5a: mov    $0x55,%edi */
0x0f, 0xa3, 0xd7,                         /*5f: bt     %edx,%edi */
0x73, 0x09,                               /*62: jae    6d <op_ge+0x6d> */
0x8b, 0x3c, 0x95, 0x00, 0x00, 0x00, 0x00, /*64: mov    0x0(,%rdx,4),%edi */
0xeb, 0x1a,                               /*6b: jmp    87 <op_ge+0x87> */
0xbf, 0x03, 0x00, 0x00, 0x00,             /*6d: mov    $0x3,%edi */
0xf6, 0xc2, 0x01,                         /*72: test   $0x1,%dl */
0x75, 0x10,                               /*75: jne    87 <op_ge+0x87> */
0x0f, 0xb6, 0xda,                         /*77: movzbl %dl,%ebx */
0xbf, 0x04, 0x00, 0x00, 0x00,             /*7a: mov    $0x4,%edi */
0x83, 0xfb, 0x0e,                         /*7f: cmp    $0xe,%ebx */
0x74, 0x03,                               /*82: je     87 <op_ge+0x87> */
0x0f, 0xb6, 0x3a,                         /*84: movzbl (%rdx),%edi */
0x40, 0x0f, 0xb6, 0xff,                   /*87: movzbl %dil,%edi */
0x09, 0xf7,                               /*8b: or     %esi,%edi */
0x81, 0xff, 0x02, 0x06, 0x00, 0x00,       /*8d: cmp    $0x602,%edi */
0x7f, 0x13,                               /*93: jg     a8 <op_ge+0xa8> */
0x81, 0xff, 0x03, 0x03, 0x00, 0x00,       /*95: cmp    $0x303,%edi */
0x75, 0x27,                               /*9b: jne    c4 <op_ge+0xc4> */
0xd1, 0xf9,                               /*9d: sar    %ecx */
0xd1, 0xfa,                               /*9f: sar    %edx */
0x39, 0xd1,                               /*a1: cmp    %edx,%ecx */
0x0f, 0x9d, 0xc1,                         /*a3: setge  %cl */
0xeb, 0x41,                               /*a6: jmp    e9 <op_ge+0xe9> */
0x81, 0xff, 0x03, 0x06, 0x00, 0x00,       /*a8: cmp    $0x603,%edi */
0x75, 0x24,                               /*ae: jne    d4 <op_ge+0xd4> */
0xf2, 0x0f, 0x10, 0x41, 0x18,             /*b0: movsd  0x18(%rcx),%xmm0 */
0xd1, 0xfa,                               /*b5: sar    %edx */
0xf2, 0x0f, 0x2a, 0xca,                   /*b7: cvtsi2sd %edx,%xmm1 */
0x66, 0x0f, 0x2e, 0xc1,                   /*bb: ucomisd %xmm1,%xmm0 */
0x0f, 0x93, 0xc1,                         /*bf: setae  %cl */
0xeb, 0x25,                               /*c2: jmp    e9 <op_ge+0xe9> */
0x81, 0xff, 0x06, 0x03, 0x00, 0x00,       /*c4: cmp    $0x306,%edi */
0x75, 0x53,                               /*ca: jne    11f <op_ge+0x11f> */
0xd1, 0xf9,                               /*cc: sar    %ecx */
0xf2, 0x0f, 0x2a, 0xc1,                   /*ce: cvtsi2sd %ecx,%xmm0 */
0xeb, 0x0d,                               /*d2: jmp    e1 <op_ge+0xe1> */
0x81, 0xff, 0x06, 0x06, 0x00, 0x00,       /*d4: cmp    $0x606,%edi */
0x75, 0x43,                               /*da: jne    11f <op_ge+0x11f> */
0xf2, 0x0f, 0x10, 0x41, 0x18,             /*dc: movsd  0x18(%rcx),%xmm0 */
0x66, 0x0f, 0x2e, 0x42, 0x18,             /*e1: ucomisd 0x18(%rdx),%xmm0 */
0x0f, 0x93, 0xc1,                         /*e6: setae  %cl */
0x0f, 0xb6, 0xc9,                         /*e9: movzbl %cl,%ecx */
0x85, 0xc9,                               /*ec: test   %ecx,%ecx */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*ee: movq   $0x0,0xab0800(%rax) */
0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*f9: movl   $0x2,0xab0800(%rax) */
0x74, 0x0d,                               /*103: je     112 <op_ge+0x112> */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x04, 0x00, 0x00, 0x00,/*105: movq   $0x4,0xab0800(%rax) */
0xeb, 0x2a,                               /*110: jmp    13c <op_ge+0x13c> */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x02, 0x00, 0x00, 0x00,/*112: movq   $0x2,0xab0800(%rax) */
0xeb, 0x1d,                               /*11d: jmp    13c <op_ge+0x13c> */
0xbe, 0x20, 0x00, 0x00, 0x00,             /*11f: mov    $0x20,%esi */
0xba, 0x00, 0x00, 0xab, 0x00,             /*124: mov    $0xab0000,%edx */
0xb9, 0x00, 0x00, 0xbc, 0x00,             /*129: mov    $0xbc0000,%ecx */
0x41, 0xb8, 0x00, 0x00, 0xcd, 0x00,       /*12e: mov    $0xcd0000,%r8d */
0x4c, 0x89, 0xf7,                         /*134: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*137: callq  13c <op_ge+0x13c> */
0x4c, 0x89, 0xf7,                         /*13c: mov    %r14,%rdi */
0x5b,                                     /*13f: pop    %rbx */
0x41, 0x5e,                               /*140: pop    %r14 */

};
static void op_ge_link(uint8_t *op) {
  *((int32_t *)(op + 312)) = (uint32_t)(((uint8_t *)_op_send) + (0) - (op + 312));
}

static void op_ge_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = a * 8 + 0;
  *((int32_t *)(op + 80)) = a * 8 + 8;
  *((int32_t *)(op + 241)) = a * 8 + 0;
  *((int32_t *)(op + 251)) = a * 8 + 0;
  *((int32_t *)(op + 264)) = a * 8 + 0;
  *((int32_t *)(op + 277)) = a * 8 + 0;
  *((int32_t *)(op + 293)) = a * 1 + 0;
  *((int32_t *)(op + 298)) = b * 1 + 0;
  *((int32_t *)(op + 304)) = c * 1 + 0;
}

static void op_ge_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_ge_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[8, 0, 17..20]], "c"=>[[1, 0, 22..25]], "a"=>[[8, 0, 34..37]]} */
static uint8_t op_array[] = {
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
static void op_array_link(uint8_t *op) {
  *((int32_t *)(op + 27)) = (uint32_t)(((uint8_t *)mrb_ary_new_from_values) + (0) - (op + 27));
}

static void op_array_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 8 + 0;
  *((int32_t *)(op + 22)) = c * 1 + 0;
  *((int32_t *)(op + 34)) = a * 8 + 0;
}

static void op_array_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_array_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[8, 0, 19..22]], "a"=>[[8, 0, 34..37]]} */
static uint8_t op_arycat[] = {
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
static void op_arycat_link(uint8_t *op) {
  *((int32_t *)(op + 27)) = (uint32_t)(((uint8_t *)mrb_ary_splat) + (0) - (op + 27));
  *((int32_t *)(op + 45)) = (uint32_t)(((uint8_t *)mrb_ary_concat) + (0) - (op + 45));
}

static void op_arycat_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = b * 8 + 0;
  *((int32_t *)(op + 34)) = a * 8 + 0;
}

static void op_arycat_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_arycat_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18]], "b"=>[[8, 0, 22..25]]} */
static uint8_t op_arypush[] = {
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
static void op_arypush_link(uint8_t *op) {
  *((int32_t *)(op + 27)) = (uint32_t)(((uint8_t *)mrb_ary_push) + (0) - (op + 27));
}

static void op_arypush_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
  *((int32_t *)(op + 22)) = b * 8 + 0;
}

static void op_arypush_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_arypush_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[8, 0, 13..16]], "c"=>[[1, 0, 62..65]], "a"=>[[8, 0, 74..77], [8, 0, 83..86], [8, 0, 97..100], [8, 0, 109..112]]} */
static uint8_t op_aref[] = {
0x41, 0x56,                               /*0: push   %r14 */
0x53,                                     /*2: push   %rbx */
0x48, 0x89, 0xfb,                         /*3: mov    %rdi,%rbx */
0x4c, 0x8b, 0x73, 0x18,                   /*6: mov    0x18(%rbx),%r14 */
0x49, 0x8b, 0xb6, 0x00, 0x08, 0xbc, 0x00, /*a: mov    0xbc0800(%r14),%rsi */
0x48, 0x83, 0xfe, 0x06,                   /*11: cmp    $0x6,%rsi */
0x77, 0x0b,                               /*15: ja     22 <op_aref+0x22> */
0xb8, 0x55, 0x00, 0x00, 0x00,             /*17: mov    $0x55,%eax */
0x48, 0x0f, 0xa3, 0xf0,                   /*1c: bt     %rsi,%rax */
0x72, 0x2e,                               /*20: jb     50 <op_aref+0x50> */
0x40, 0xf6, 0xc6, 0x01,                   /*22: test   $0x1,%sil */
0x75, 0x28,                               /*26: jne    50 <op_aref+0x50> */
0x40, 0x0f, 0xb6, 0xc6,                   /*28: movzbl %sil,%eax */
0x83, 0xf8, 0x0e,                         /*2c: cmp    $0xe,%eax */
0x74, 0x1f,                               /*2f: je     50 <op_aref+0x50> */
0x0f, 0xb6, 0x06,                         /*31: movzbl (%rsi),%eax */
0x83, 0xf8, 0x0e,                         /*34: cmp    $0xe,%eax */
0x75, 0x17,                               /*37: jne    50 <op_aref+0x50> */
0x48, 0x8b, 0x7b, 0x50,                   /*39: mov    0x50(%rbx),%rdi */
0xba, 0x00, 0x00, 0xcd, 0x00,             /*3d: mov    $0xcd0000,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*42: callq  47 <op_aref+0x47> */
0x49, 0x89, 0x86, 0x00, 0x08, 0xab, 0x00, /*47: mov    %rax,0xab0800(%r14) */
0xeb, 0x25,                               /*4e: jmp    75 <op_aref+0x75> */
0x49, 0xc7, 0x86, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*50: movq   $0x0,0xab0800(%r14) */
0x48, 0x8b, 0x43, 0x18,                   /*5b: mov    0x18(%rbx),%rax */
0x83, 0xa0, 0x00, 0x08, 0xab, 0x00, 0x01, /*5f: andl   $0x1,0xab0800(%rax) */
0x48, 0x8b, 0x43, 0x18,                   /*66: mov    0x18(%rbx),%rax */
0x48, 0xc7, 0x80, 0x00, 0x08, 0xab, 0x00, 0x00, 0x00, 0x00, 0x00,/*6a: movq   $0x0,0xab0800(%rax) */
0x48, 0x89, 0xdf,                         /*75: mov    %rbx,%rdi */
0x5b,                                     /*78: pop    %rbx */
0x41, 0x5e,                               /*79: pop    %r14 */

};
static void op_aref_link(uint8_t *op) {
  *((int32_t *)(op + 67)) = (uint32_t)(((uint8_t *)mrb_ary_ref) + (0) - (op + 67));
}

static void op_aref_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = b * 8 + 0;
  *((int32_t *)(op + 62)) = c * 1 + 0;
  *((int32_t *)(op + 74)) = a * 8 + 0;
  *((int32_t *)(op + 83)) = a * 8 + 0;
  *((int32_t *)(op + 97)) = a * 8 + 0;
  *((int32_t *)(op + 109)) = a * 8 + 0;
}

static void op_aref_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_aref_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[8, 0, 15..18]], "a"=>[[8, 0, 22..25]], "c"=>[[1, 0, 27..30]]} */
static uint8_t op_aset[] = {
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
static void op_aset_link(uint8_t *op) {
  *((int32_t *)(op + 32)) = (uint32_t)(((uint8_t *)mrb_ary_set) + (0) - (op + 32));
}

static void op_aset_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = b * 8 + 0;
  *((int32_t *)(op + 22)) = a * 8 + 0;
  *((int32_t *)(op + 27)) = c * 1 + 0;
}

static void op_aset_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_aset_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 20..23], [8, 0, 121..124], [8, 0, 203..206], [8, 0, 281..284], [1, 1, 375..378]], "b"=>[[8, 0, 109..112], [1, 1, 288..291]], "c"=>[[1, 0, 176..179], [1, 0, 257..260], [1, 0, 362..365]]} */
static uint8_t op_apost[] = {
0x55,                                     /*0: push   %rbp */
0x41, 0x57,                               /*1: push   %r15 */
0x41, 0x56,                               /*3: push   %r14 */
0x41, 0x55,                               /*5: push   %r13 */
0x41, 0x54,                               /*7: push   %r12 */
0x53,                                     /*9: push   %rbx */
0x48, 0x89, 0xfd,                         /*a: mov    %rdi,%rbp */
0x4c, 0x8b, 0x7d, 0x18,                   /*d: mov    0x18(%rbp),%r15 */
0x4d, 0x8b, 0xb7, 0x00, 0x08, 0xab, 0x00, /*11: mov    0xab0800(%r15),%r14 */
0x49, 0x83, 0xfe, 0x06,                   /*18: cmp    $0x6,%r14 */
0x77, 0x0f,                               /*1c: ja     2d <op_apost+0x2d> */
0xb8, 0x55, 0x00, 0x00, 0x00,             /*1e: mov    $0x55,%eax */
0x4c, 0x0f, 0xa3, 0xf0,                   /*23: bt     %r14,%rax */
0x0f, 0x82, 0x8e, 0x00, 0x00, 0x00,       /*27: jb     bb <op_apost+0xbb> */
0x41, 0xf6, 0xc6, 0x01,                   /*2d: test   $0x1,%r14b */
0x0f, 0x85, 0x84, 0x00, 0x00, 0x00,       /*31: jne    bb <op_apost+0xbb> */
0x41, 0x0f, 0xb6, 0xc6,                   /*37: movzbl %r14b,%eax */
0x83, 0xf8, 0x0e,                         /*3b: cmp    $0xe,%eax */
0x74, 0x7b,                               /*3e: je     bb <op_apost+0xbb> */
0x41, 0x0f, 0xb6, 0x06,                   /*40: movzbl (%r14),%eax */
0x83, 0xf8, 0x0e,                         /*44: cmp    $0xe,%eax */
0x75, 0x72,                               /*47: jne    bb <op_apost+0xbb> */
0x45, 0x8b, 0x66, 0x18,                   /*49: mov    0x18(%r14),%r12d */
0x48, 0x8b, 0x7d, 0x50,                   /*4d: mov    0x50(%rbp),%rdi */
0x41, 0x81, 0xfc, 0x01, 0x00, 0x89, 0x01, /*51: cmp    $0x1890001,%r12d */
0x0f, 0x8c, 0xae, 0x00, 0x00, 0x00,       /*58: jl     10c <op_apost+0x10c> */
0x41, 0x8d, 0xb4, 0x24, 0x00, 0x00, 0x77, 0xfe,/*5e: lea    -0x1890000(%r12),%esi */
0x49, 0x8b, 0x56, 0x28,                   /*66: mov    0x28(%r14),%rdx */
0x48, 0x81, 0xc2, 0x00, 0x08, 0xbc, 0x00, /*6a: add    $0xbc0800,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*71: callq  76 <op_apost+0x76> */
0x49, 0x89, 0x87, 0x00, 0x08, 0xab, 0x00, /*76: mov    %rax,0xab0800(%r15) */
0x41, 0x81, 0xc4, 0x00, 0x00, 0x33, 0xff, /*7d: add    $0xff330000,%r12d */
0x31, 0xc0,                               /*84: xor    %eax,%eax */
0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*86: nopw   %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x4d, 0x18,                   /*90: mov    0x18(%rbp),%rcx */
0x41, 0x8d, 0x14, 0x04,                   /*94: lea    (%r12,%rax,1),%edx */
0x48, 0x63, 0xd2,                         /*98: movslq %edx,%rdx */
0x49, 0x8b, 0x76, 0x28,                   /*9b: mov    0x28(%r14),%rsi */
0x48, 0x8b, 0x14, 0xd6,                   /*9f: mov    (%rsi,%rdx,8),%rdx */
0x48, 0x89, 0x94, 0xc1, 0x08, 0x00, 0x58, 0x05,/*a3: mov    %rdx,0x5580008(%rcx,%rax,8) */
0x48, 0xff, 0xc0,                         /*ab: inc    %rax */
0x48, 0x3d, 0x00, 0x00, 0xcd, 0x00,       /*ae: cmp    $0xcd0000,%rax */
0x75, 0xda,                               /*b4: jne    90 <op_apost+0x90> */
0xe9, 0xec, 0x00, 0x00, 0x00,             /*b6: jmpq   1a7 <op_apost+0x1a7> */
0x48, 0x8b, 0x7d, 0x50,                   /*bb: mov    0x50(%rbp),%rdi */
0x31, 0xdb,                               /*bf: xor    %ebx,%ebx */
0x31, 0xf6,                               /*c1: xor    %esi,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*c3: callq  c8 <op_apost+0xc8> */
0x49, 0x89, 0x87, 0x00, 0x08, 0xab, 0x00, /*c8: mov    %rax,0xab0800(%r15) */
0x90,                                     /*cf: nop */
0x48, 0x8b, 0x45, 0x18,                   /*d0: mov    0x18(%rbp),%rax */
0x48, 0xc7, 0x84, 0xd8, 0x08, 0x00, 0x58, 0x05, 0x00, 0x00, 0x00, 0x00,/*d4: movq   $0x0,0x5580008(%rax,%rbx,8) */
0x48, 0x8b, 0x45, 0x18,                   /*e0: mov    0x18(%rbp),%rax */
0x83, 0xa4, 0xd8, 0x08, 0x00, 0x58, 0x05, 0x01,/*e4: andl   $0x1,0x5580008(%rax,%rbx,8) */
0x48, 0x8b, 0x45, 0x18,                   /*ec: mov    0x18(%rbp),%rax */
0x48, 0xc7, 0x84, 0xd8, 0x08, 0x00, 0x58, 0x05, 0x00, 0x00, 0x00, 0x00,/*f0: movq   $0x0,0x5580008(%rax,%rbx,8) */
0x48, 0xff, 0xc3,                         /*fc: inc    %rbx */
0x81, 0xfb, 0x00, 0x00, 0xcd, 0x00,       /*ff: cmp    $0xcd0000,%ebx */
0x75, 0xc9,                               /*105: jne    d0 <op_apost+0xd0> */
0xe9, 0x9b, 0x00, 0x00, 0x00,             /*107: jmpq   1a7 <op_apost+0x1a7> */
0x45, 0x31, 0xed,                         /*10c: xor    %r13d,%r13d */
0x31, 0xf6,                               /*10f: xor    %esi,%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*111: callq  116 <op_apost+0x116> */
0x49, 0x89, 0x87, 0x00, 0x08, 0xab, 0x00, /*116: mov    %rax,0xab0800(%r15) */
0x41, 0x81, 0xfc, 0x01, 0x00, 0xbc, 0x00, /*11d: cmp    $0xbc0001,%r12d */
0x7c, 0x43,                               /*124: jl     169 <op_apost+0x169> */
0x45, 0x89, 0xe5,                         /*126: mov    %r12d,%r13d */
0x41, 0x81, 0xc5, 0x00, 0x00, 0x44, 0xff, /*129: add    $0xff440000,%r13d */
0x31, 0xc0,                               /*130: xor    %eax,%eax */
0x66, 0x66, 0x66, 0x66, 0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00,/*132: data16 data16 data16 data16 nopw %cs:0x0(%rax,%rax,1) */
0x48, 0x8b, 0x4d, 0x18,                   /*140: mov    0x18(%rbp),%rcx */
0x49, 0x8b, 0x56, 0x28,                   /*144: mov    0x28(%r14),%rdx */
0x48, 0x8b, 0x94, 0xc2, 0x00, 0x00, 0xe0, 0x05,/*148: mov    0x5e00000(%rdx,%rax,8),%rdx */
0x48, 0x89, 0x94, 0xc1, 0x08, 0x00, 0x58, 0x05,/*150: mov    %rdx,0x5580008(%rcx,%rax,8) */
0x48, 0xff, 0xc0,                         /*158: inc    %rax */
0x41, 0x39, 0xc5,                         /*15b: cmp    %eax,%r13d */
0x75, 0xe0,                               /*15e: jne    140 <op_apost+0x140> */
0x41, 0x81, 0xfd, 0xff, 0xff, 0xcc, 0x00, /*160: cmp    $0xccffff,%r13d */
0x7f, 0x3e,                               /*167: jg     1a7 <op_apost+0x1a7> */
0xb8, 0x00, 0x00, 0xcd, 0x00,             /*169: mov    $0xcd0000,%eax */
0x44, 0x29, 0xe8,                         /*16e: sub    %r13d,%eax */
0x49, 0x63, 0xcd,                         /*171: movslq %r13d,%rcx */
0x48, 0x81, 0xc1, 0x01, 0x00, 0xab, 0x00, /*174: add    $0xab0001,%rcx */
0x0f, 0x1f, 0x44, 0x00, 0x00,             /*17b: nopl   0x0(%rax,%rax,1) */
0x48, 0x8b, 0x55, 0x18,                   /*180: mov    0x18(%rbp),%rdx */
0x48, 0xc7, 0x04, 0xca, 0x00, 0x00, 0x00, 0x00,/*184: movq   $0x0,(%rdx,%rcx,8) */
0x48, 0x8b, 0x55, 0x18,                   /*18c: mov    0x18(%rbp),%rdx */
0x83, 0x24, 0xca, 0x01,                   /*190: andl   $0x1,(%rdx,%rcx,8) */
0x48, 0x8b, 0x55, 0x18,                   /*194: mov    0x18(%rbp),%rdx */
0x48, 0xc7, 0x04, 0xca, 0x00, 0x00, 0x00, 0x00,/*198: movq   $0x0,(%rdx,%rcx,8) */
0x48, 0xff, 0xc1,                         /*1a0: inc    %rcx */
0xff, 0xc8,                               /*1a3: dec    %eax */
0x75, 0xd9,                               /*1a5: jne    180 <op_apost+0x180> */
0x8b, 0x45, 0x48,                         /*1a7: mov    0x48(%rbp),%eax */
0x48, 0x8b, 0x4d, 0x50,                   /*1aa: mov    0x50(%rbp),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*1ae: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xef,                         /*1b4: mov    %rbp,%rdi */
0x5b,                                     /*1b7: pop    %rbx */
0x41, 0x5c,                               /*1b8: pop    %r12 */
0x41, 0x5d,                               /*1ba: pop    %r13 */
0x41, 0x5e,                               /*1bc: pop    %r14 */
0x41, 0x5f,                               /*1be: pop    %r15 */
0x5d,                                     /*1c0: pop    %rbp */

};
static void op_apost_link(uint8_t *op) {
  *((int32_t *)(op + 114)) = (uint32_t)(((uint8_t *)mrb_ary_new_from_values) + (0) - (op + 114));
  *((int32_t *)(op + 196)) = (uint32_t)(((uint8_t *)mrb_ary_new_capa) + (0) - (op + 196));
  *((int32_t *)(op + 274)) = (uint32_t)(((uint8_t *)mrb_ary_new_capa) + (0) - (op + 274));
}

static void op_apost_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 20)) = a * 8 + 0;
  *((int32_t *)(op + 121)) = a * 8 + 0;
  *((int32_t *)(op + 203)) = a * 8 + 0;
  *((int32_t *)(op + 281)) = a * 8 + 0;
  *((int32_t *)(op + 375)) = a * 1 + 1;
  *((int32_t *)(op + 109)) = b * 8 + 0;
  *((int32_t *)(op + 288)) = b * 1 + 1;
  *((int32_t *)(op + 176)) = c * 1 + 0;
  *((int32_t *)(op + 257)) = c * 1 + 0;
  *((int32_t *)(op + 362)) = c * 1 + 0;
}

static void op_apost_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_apost_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"b"=>[[8, 0, 21..24]], "a"=>[[8, 0, 33..36]]} */
static uint8_t op_string[] = {
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
static void op_string_link(uint8_t *op) {
  *((int32_t *)(op + 26)) = (uint32_t)(((uint8_t *)mrb_str_dup) + (0) - (op + 26));
}

static void op_string_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = b * 8 + 0;
  *((int32_t *)(op + 33)) = a * 8 + 0;
}

static void op_string_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_string_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 15..18]], "b"=>[[8, 0, 22..25]]} */
static uint8_t op_strcat[] = {
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
static void op_strcat_link(uint8_t *op) {
  *((int32_t *)(op + 27)) = (uint32_t)(((uint8_t *)mrb_str_concat) + (0) - (op + 27));
}

static void op_strcat_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 15)) = a * 8 + 0;
  *((int32_t *)(op + 22)) = b * 8 + 0;
}

static void op_strcat_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_strcat_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[1, 0, 13..16]], "c"=>[[1, 0, 20..23]], "a"=>[[8, 0, 124..127]]} */
static uint8_t op_hash[] = {
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
static void op_hash_link(uint8_t *op) {
  *((int32_t *)(op + 43)) = (uint32_t)(((uint8_t *)mrb_hash_new_capa) + (0) - (op + 43));
  *((int32_t *)(op + 99)) = (uint32_t)(((uint8_t *)mrb_hash_set) + (0) - (op + 99));
}

static void op_hash_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 13)) = b * 1 + 0;
  *((int32_t *)(op + 20)) = c * 1 + 0;
  *((int32_t *)(op + 124)) = a * 8 + 0;
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
0xe8, 0x00, 0x00, 0x00, 0x00,             /*13: callq  18 <op_lambda+0x18> */
0x48, 0x89, 0xdf,                         /*18: mov    %rbx,%rdi */
0x5b,                                     /*1b: pop    %rbx */

};
static void op_lambda_link(uint8_t *op) {
  *((int32_t *)(op + 20)) = (uint32_t)(((uint8_t *)_op_lambda) + (0) - (op + 20));
}

static void op_lambda_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 5)) = a * 1 + 0;
  *((int32_t *)(op + 10)) = b * 1 + 0;
  *((int32_t *)(op + 15)) = c * 1 + 0;
}

static void op_lambda_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_lambda_set_args(op, GETARG_A(c),GETARG_b(c),GETARG_c(c),op_idx);
}


/* args: {"b"=>[[8, 0, 17..20], [8, 8, 24..27]], "c"=>[[1, 0, 29..32]], "a"=>[[8, 0, 41..44]]} */
static uint8_t op_range[] = {
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
static void op_range_link(uint8_t *op) {
  *((int32_t *)(op + 34)) = (uint32_t)(((uint8_t *)mrb_range_new) + (0) - (op + 34));
}

static void op_range_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 8 + 0;
  *((int32_t *)(op + 24)) = b * 8 + 8;
  *((int32_t *)(op + 29)) = c * 1 + 0;
  *((int32_t *)(op + 41)) = a * 8 + 0;
}

static void op_range_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_range_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {"a"=>[[8, 0, 84..87]]} */
static uint8_t op_oclass[] = {
0x4c, 0x8b, 0x47, 0x18,                   /*0: mov    0x18(%rdi),%r8 */
0x48, 0x8b, 0x47, 0x50,                   /*4: mov    0x50(%rdi),%rax */
0x48, 0x8b, 0x48, 0x40,                   /*8: mov    0x40(%rax),%rcx */
0x8b, 0x31,                               /*c: mov    (%rcx),%esi */
0x40, 0x0f, 0xb6, 0xc6,                   /*e: movzbl %sil,%eax */
0xba, 0x04, 0x00, 0x00, 0x00,             /*12: mov    $0x4,%edx */
0x83, 0xf8, 0x05,                         /*17: cmp    $0x5,%eax */
0x77, 0x15,                               /*1a: ja     31 <op_oclass+0x31> */
0xff, 0x24, 0xc5, 0x00, 0x00, 0x00, 0x00, /*1c: jmpq   *0x0(,%rax,8) */
0x48, 0x85, 0xc9,                         /*23: test   %rcx,%rcx */
0x0f, 0x95, 0xc0,                         /*26: setne  %al */
0x0f, 0xb6, 0xd0,                         /*29: movzbl %al,%edx */
0x48, 0x01, 0xd2,                         /*2c: add    %rdx,%rdx */
0xeb, 0x20,                               /*2f: jmp    51 <op_oclass+0x51> */
0x48, 0x85, 0xc9,                         /*31: test   %rcx,%rcx */
0x48, 0x89, 0xca,                         /*34: mov    %rcx,%rdx */
0x74, 0x18,                               /*37: je     51 <op_oclass+0x51> */
0x89, 0x31,                               /*39: mov    %esi,(%rcx) */
0xeb, 0x11,                               /*3b: jmp    4e <op_oclass+0x4e> */
0x48, 0x83, 0xc9, 0x01,                   /*3d: or     $0x1,%rcx */
0xeb, 0x0b,                               /*41: jmp    4e <op_oclass+0x4e> */
0x48, 0x81, 0xe1, 0x00, 0xff, 0xff, 0xff, /*43: and    $0xffffffffffffff00,%rcx */
0x48, 0x83, 0xc9, 0x0e,                   /*4a: or     $0xe,%rcx */
0x48, 0x89, 0xca,                         /*4e: mov    %rcx,%rdx */
0x49, 0x89, 0x90, 0x00, 0x08, 0xab, 0x00, /*51: mov    %rdx,0xab0800(%r8) */
0xeb, 0x07,                               /*58: jmp    61 <op_oclass+0x61> */
0xba, 0x06, 0x00, 0x00, 0x00,             /*5a: mov    $0x6,%edx */
0xeb, 0xf0,                               /*5f: jmp    51 <op_oclass+0x51> */

};
static void op_oclass_link(uint8_t *op) {
}

static void op_oclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 84)) = a * 8 + 0;
}

static void op_oclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_oclass_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"b"=>[[4, 0, 29..32]], "a"=>[[8, 0, 36..39], [8, 8, 43..46], [8, 0, 222..225]]} */
static uint8_t op_class[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xbf, 0x00, 0x00, 0x00, 0x00,             /*4: mov    $0x0,%edi */
0x31, 0xc0,                               /*9: xor    %eax,%eax */
0x48, 0x89, 0xde,                         /*b: mov    %rbx,%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*e: callq  13 <op_class+0x13> */
0x48, 0x8b, 0x43, 0x18,                   /*13: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*17: mov    0x28(%rbx),%rcx */
0x8b, 0x89, 0x00, 0x04, 0xbc, 0x00,       /*1b: mov    0xbc0400(%rcx),%ecx */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xab, 0x00, /*21: mov    0xab0800(%rax),%rsi */
0x48, 0x8b, 0x90, 0x08, 0x08, 0xab, 0x00, /*28: mov    0xab0808(%rax),%rdx */
0x48, 0x85, 0xf6,                         /*2f: test   %rsi,%rsi */
0x75, 0x56,                               /*32: jne    8a <op_class+0x8a> */
0x48, 0x8b, 0x43, 0x50,                   /*34: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*38: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*3c: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x48,                   /*40: mov    0x48(%rax),%rax */
0x44, 0x8b, 0x00,                         /*44: mov    (%rax),%r8d */
0x41, 0x0f, 0xb6, 0xf8,                   /*47: movzbl %r8b,%edi */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*4b: mov    $0x4,%esi */
0x83, 0xff, 0x05,                         /*50: cmp    $0x5,%edi */
0x77, 0x15,                               /*53: ja     6a <op_class+0x6a> */
0xff, 0x24, 0xfd, 0x00, 0x00, 0x00, 0x00, /*55: jmpq   *0x0(,%rdi,8) */
0x48, 0x85, 0xc0,                         /*5c: test   %rax,%rax */
0x0f, 0x95, 0xc0,                         /*5f: setne  %al */
0x0f, 0xb6, 0xf0,                         /*62: movzbl %al,%esi */
0x48, 0x01, 0xf6,                         /*65: add    %rsi,%rsi */
0xeb, 0x20,                               /*68: jmp    8a <op_class+0x8a> */
0x48, 0x85, 0xc0,                         /*6a: test   %rax,%rax */
0x48, 0x89, 0xc6,                         /*6d: mov    %rax,%rsi */
0x74, 0x18,                               /*70: je     8a <op_class+0x8a> */
0x44, 0x89, 0x00,                         /*72: mov    %r8d,(%rax) */
0xeb, 0x10,                               /*75: jmp    87 <op_class+0x87> */
0x48, 0x83, 0xc8, 0x01,                   /*77: or     $0x1,%rax */
0xeb, 0x0a,                               /*7b: jmp    87 <op_class+0x87> */
0x48, 0x25, 0x00, 0xff, 0xff, 0xff,       /*7d: and    $0xffffffffffffff00,%rax */
0x48, 0x83, 0xc8, 0x0e,                   /*83: or     $0xe,%rax */
0x48, 0x89, 0xc6,                         /*87: mov    %rax,%rsi */
0x48, 0x8b, 0x7b, 0x50,                   /*8a: mov    0x50(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*8e: callq  93 <op_class+0x93> */
0x48, 0x8b, 0x4b, 0x18,                   /*93: mov    0x18(%rbx),%rcx */
0x8b, 0x30,                               /*97: mov    (%rax),%esi */
0x40, 0x0f, 0xb6, 0xfe,                   /*99: movzbl %sil,%edi */
0xba, 0x04, 0x00, 0x00, 0x00,             /*9d: mov    $0x4,%edx */
0x83, 0xff, 0x05,                         /*a2: cmp    $0x5,%edi */
0x77, 0x15,                               /*a5: ja     bc <op_class+0xbc> */
0xff, 0x24, 0xfd, 0x00, 0x00, 0x00, 0x00, /*a7: jmpq   *0x0(,%rdi,8) */
0x48, 0x85, 0xc0,                         /*ae: test   %rax,%rax */
0x0f, 0x95, 0xc0,                         /*b1: setne  %al */
0x0f, 0xb6, 0xd0,                         /*b4: movzbl %al,%edx */
0x48, 0x01, 0xd2,                         /*b7: add    %rdx,%rdx */
0xeb, 0x1f,                               /*ba: jmp    db <op_class+0xdb> */
0x48, 0x85, 0xc0,                         /*bc: test   %rax,%rax */
0x48, 0x89, 0xc2,                         /*bf: mov    %rax,%rdx */
0x74, 0x17,                               /*c2: je     db <op_class+0xdb> */
0x89, 0x30,                               /*c4: mov    %esi,(%rax) */
0xeb, 0x10,                               /*c6: jmp    d8 <op_class+0xd8> */
0x48, 0x83, 0xc8, 0x01,                   /*c8: or     $0x1,%rax */
0xeb, 0x0a,                               /*cc: jmp    d8 <op_class+0xd8> */
0x48, 0x25, 0x00, 0xff, 0xff, 0xff,       /*ce: and    $0xffffffffffffff00,%rax */
0x48, 0x83, 0xc8, 0x0e,                   /*d4: or     $0xe,%rax */
0x48, 0x89, 0xc2,                         /*d8: mov    %rax,%rdx */
0x48, 0x89, 0x91, 0x00, 0x08, 0xab, 0x00, /*db: mov    %rdx,0xab0800(%rcx) */
0x8b, 0x43, 0x48,                         /*e2: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*e5: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*e9: mov    %eax,0xdc(%rcx) */
0xbf, 0x00, 0x00, 0x00, 0x00,             /*ef: mov    $0x0,%edi */
0x31, 0xc0,                               /*f4: xor    %eax,%eax */
0x48, 0x89, 0xde,                         /*f6: mov    %rbx,%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*f9: callq  fe <op_class+0xfe> */
0x48, 0x89, 0xdf,                         /*fe: mov    %rbx,%rdi */
0x5b,                                     /*101: pop    %rbx */
0xeb, 0x11,                               /*102: jmp    115 <op_class+0x115> */
0xba, 0x06, 0x00, 0x00, 0x00,             /*104: mov    $0x6,%edx */
0xeb, 0xd0,                               /*109: jmp    db <op_class+0xdb> */
0xbe, 0x06, 0x00, 0x00, 0x00,             /*10b: mov    $0x6,%esi */
0xe9, 0x75, 0xff, 0xff, 0xff,             /*110: jmpq   8a <op_class+0x8a> */

};
static void op_class_link(uint8_t *op) {
  *((int32_t *)(op + 15)) = (uint32_t)(((uint8_t *)printf) + (0) - (op + 15));
  *((int32_t *)(op + 143)) = (uint32_t)(((uint8_t *)mrb_vm_define_class) + (0) - (op + 143));
  *((int32_t *)(op + 250)) = (uint32_t)(((uint8_t *)printf) + (0) - (op + 250));
}

static void op_class_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 29)) = b * 4 + 0;
  *((int32_t *)(op + 36)) = a * 8 + 0;
  *((int32_t *)(op + 43)) = a * 8 + 8;
  *((int32_t *)(op + 222)) = a * 8 + 0;
}

static void op_class_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_class_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 14..17]], "a"=>[[8, 0, 21..24], [8, 0, 197..200]]} */
static uint8_t op_module[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*4: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*8: mov    0x28(%rbx),%rcx */
0x8b, 0x91, 0x00, 0x04, 0xbc, 0x00,       /*c: mov    0xbc0400(%rcx),%edx */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xab, 0x00, /*12: mov    0xab0800(%rax),%rsi */
0x48, 0x85, 0xf6,                         /*19: test   %rsi,%rsi */
0x75, 0x53,                               /*1c: jne    71 <op_module+0x71> */
0x48, 0x8b, 0x43, 0x50,                   /*1e: mov    0x50(%rbx),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*22: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*26: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x40, 0x48,                   /*2a: mov    0x48(%rax),%rax */
0x8b, 0x08,                               /*2e: mov    (%rax),%ecx */
0x0f, 0xb6, 0xf9,                         /*30: movzbl %cl,%edi */
0xbe, 0x04, 0x00, 0x00, 0x00,             /*33: mov    $0x4,%esi */
0x83, 0xff, 0x05,                         /*38: cmp    $0x5,%edi */
0x77, 0x15,                               /*3b: ja     52 <op_module+0x52> */
0xff, 0x24, 0xfd, 0x00, 0x00, 0x00, 0x00, /*3d: jmpq   *0x0(,%rdi,8) */
0x48, 0x85, 0xc0,                         /*44: test   %rax,%rax */
0x0f, 0x95, 0xc0,                         /*47: setne  %al */
0x0f, 0xb6, 0xf0,                         /*4a: movzbl %al,%esi */
0x48, 0x01, 0xf6,                         /*4d: add    %rsi,%rsi */
0xeb, 0x1f,                               /*50: jmp    71 <op_module+0x71> */
0x48, 0x85, 0xc0,                         /*52: test   %rax,%rax */
0x48, 0x89, 0xc6,                         /*55: mov    %rax,%rsi */
0x74, 0x17,                               /*58: je     71 <op_module+0x71> */
0x89, 0x08,                               /*5a: mov    %ecx,(%rax) */
0xeb, 0x10,                               /*5c: jmp    6e <op_module+0x6e> */
0x48, 0x83, 0xc8, 0x01,                   /*5e: or     $0x1,%rax */
0xeb, 0x0a,                               /*62: jmp    6e <op_module+0x6e> */
0x48, 0x25, 0x00, 0xff, 0xff, 0xff,       /*64: and    $0xffffffffffffff00,%rax */
0x48, 0x83, 0xc8, 0x0e,                   /*6a: or     $0xe,%rax */
0x48, 0x89, 0xc6,                         /*6e: mov    %rax,%rsi */
0x48, 0x8b, 0x7b, 0x50,                   /*71: mov    0x50(%rbx),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*75: callq  7a <op_module+0x7a> */
0x48, 0x8b, 0x4b, 0x18,                   /*7a: mov    0x18(%rbx),%rcx */
0x8b, 0x30,                               /*7e: mov    (%rax),%esi */
0x40, 0x0f, 0xb6, 0xfe,                   /*80: movzbl %sil,%edi */
0xba, 0x04, 0x00, 0x00, 0x00,             /*84: mov    $0x4,%edx */
0x83, 0xff, 0x05,                         /*89: cmp    $0x5,%edi */
0x77, 0x15,                               /*8c: ja     a3 <op_module+0xa3> */
0xff, 0x24, 0xfd, 0x00, 0x00, 0x00, 0x00, /*8e: jmpq   *0x0(,%rdi,8) */
0x48, 0x85, 0xc0,                         /*95: test   %rax,%rax */
0x0f, 0x95, 0xc0,                         /*98: setne  %al */
0x0f, 0xb6, 0xd0,                         /*9b: movzbl %al,%edx */
0x48, 0x01, 0xd2,                         /*9e: add    %rdx,%rdx */
0xeb, 0x1f,                               /*a1: jmp    c2 <op_module+0xc2> */
0x48, 0x85, 0xc0,                         /*a3: test   %rax,%rax */
0x48, 0x89, 0xc2,                         /*a6: mov    %rax,%rdx */
0x74, 0x17,                               /*a9: je     c2 <op_module+0xc2> */
0x89, 0x30,                               /*ab: mov    %esi,(%rax) */
0xeb, 0x10,                               /*ad: jmp    bf <op_module+0xbf> */
0x48, 0x83, 0xc8, 0x01,                   /*af: or     $0x1,%rax */
0xeb, 0x0a,                               /*b3: jmp    bf <op_module+0xbf> */
0x48, 0x25, 0x00, 0xff, 0xff, 0xff,       /*b5: and    $0xffffffffffffff00,%rax */
0x48, 0x83, 0xc8, 0x0e,                   /*bb: or     $0xe,%rax */
0x48, 0x89, 0xc2,                         /*bf: mov    %rax,%rdx */
0x48, 0x89, 0x91, 0x00, 0x08, 0xab, 0x00, /*c2: mov    %rdx,0xab0800(%rcx) */
0x8b, 0x43, 0x48,                         /*c9: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*cc: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*d0: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*d6: mov    %rbx,%rdi */
0x5b,                                     /*d9: pop    %rbx */
0xeb, 0x0e,                               /*da: jmp    ea <op_module+0xea> */
0xba, 0x06, 0x00, 0x00, 0x00,             /*dc: mov    $0x6,%edx */
0xeb, 0xdf,                               /*e1: jmp    c2 <op_module+0xc2> */
0xbe, 0x06, 0x00, 0x00, 0x00,             /*e3: mov    $0x6,%esi */
0xeb, 0x87,                               /*e8: jmp    71 <op_module+0x71> */

};
static void op_module_link(uint8_t *op) {
  *((int32_t *)(op + 118)) = (uint32_t)(((uint8_t *)mrb_vm_define_module) + (0) - (op + 118));
}

static void op_module_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 14)) = b * 4 + 0;
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 197)) = a * 8 + 0;
}

static void op_module_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_module_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 21..24], [1, 0, 48..51], [8, 0, 97..100]], "b"=>[[8, 0, 116..119]]} */
static uint8_t op_exec[] = {
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
0x4c, 0x89, 0x7b, 0x48,                   /*51: mov    %r15,0x48(%rbx) */
0x49, 0x8b, 0x46, 0x50,                   /*55: mov    0x50(%r14),%rax */
0x48, 0x8b, 0x40, 0x18,                   /*59: mov    0x18(%rax),%rax */
0x48, 0x81, 0x40, 0x08, 0x00, 0x08, 0xab, 0x00,/*5d: addq   $0xab0800,0x8(%rax) */
0x49, 0x8b, 0x46, 0x08,                   /*65: mov    0x8(%r14),%rax */
0x49, 0x8b, 0x7e, 0x50,                   /*69: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x40, 0x20,                   /*6d: mov    0x20(%rax),%rax */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xbc, 0x00, /*71: mov    0xbc0800(%rax),%rsi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*78: callq  7d <op_exec+0x7d> */
0x49, 0x89, 0xc4,                         /*7d: mov    %rax,%r12 */
0x48, 0x8b, 0x43, 0x48,                   /*80: mov    0x48(%rbx),%rax */
0x49, 0x89, 0x44, 0x24, 0x20,             /*84: mov    %rax,0x20(%r12) */
0x4c, 0x89, 0x63, 0x08,                   /*89: mov    %r12,0x8(%rbx) */
0x41, 0xf6, 0x44, 0x24, 0x02, 0x04,       /*8d: testb  $0x4,0x2(%r12) */
0x74, 0x40,                               /*93: je     d5 <op_exec+0xd5> */
0xc7, 0x43, 0x18, 0x00, 0x00, 0x00, 0x00, /*95: movl   $0x0,0x18(%rbx) */
0x49, 0x8b, 0x7e, 0x50,                   /*9c: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*a0: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x58, 0x08,                   /*a4: mov    0x8(%rax),%rbx */
0x4c, 0x89, 0xfe,                         /*a8: mov    %r15,%rsi */
0x41, 0xff, 0x54, 0x24, 0x18,             /*ab: callq  *0x18(%r12) */
0x48, 0x89, 0x03,                         /*b0: mov    %rax,(%rbx) */
0x49, 0x8b, 0x7e, 0x50,                   /*b3: mov    0x50(%r14),%rdi */
0x41, 0x8b, 0x76, 0x48,                   /*b7: mov    0x48(%r14),%esi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*bb: callq  c0 <op_exec+0xc0> */
0x49, 0x8b, 0x46, 0x50,                   /*c0: mov    0x50(%r14),%rax */
0x48, 0x83, 0x78, 0x28, 0x00,             /*c4: cmpq   $0x0,0x28(%rax) */
0x74, 0x69,                               /*c9: je     134 <op_exec+0x134> */
0x4c, 0x89, 0xf7,                         /*cb: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*ce: callq  d3 <op_exec+0xd3> */
0xeb, 0x7c,                               /*d3: jmp    151 <op_exec+0x151> */
0x49, 0x8b, 0x44, 0x24, 0x18,             /*d5: mov    0x18(%r12),%rax */
0x49, 0x89, 0x46, 0x08,                   /*da: mov    %rax,0x8(%r14) */
0x48, 0x8b, 0x48, 0x10,                   /*de: mov    0x10(%rax),%rcx */
0x49, 0x89, 0x4e, 0x20,                   /*e2: mov    %rcx,0x20(%r14) */
0x48, 0x8b, 0x48, 0x18,                   /*e6: mov    0x18(%rax),%rcx */
0x49, 0x89, 0x4e, 0x28,                   /*ea: mov    %rcx,0x28(%r14) */
0x49, 0x8b, 0x7e, 0x50,                   /*ee: mov    0x50(%r14),%rdi */
0x0f, 0xb7, 0x70, 0x02,                   /*f2: movzwl 0x2(%rax),%esi */
0xba, 0x01, 0x00, 0x00, 0x00,             /*f6: mov    $0x1,%edx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*fb: callq  100 <op_exec+0x100> */
0x49, 0x8b, 0x46, 0x08,                   /*100: mov    0x8(%r14),%rax */
0x0f, 0xb7, 0x40, 0x02,                   /*104: movzwl 0x2(%rax),%eax */
0x89, 0x43, 0x18,                         /*108: mov    %eax,0x18(%rbx) */
0x49, 0x8b, 0x7e, 0x50,                   /*10b: mov    0x50(%r14),%rdi */
0x48, 0x8b, 0x47, 0x18,                   /*10f: mov    0x18(%rdi),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*113: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x18,                   /*117: mov    %rax,0x18(%r14) */
0x49, 0x8b, 0x46, 0x08,                   /*11b: mov    0x8(%r14),%rax */
0x48, 0x8b, 0x40, 0x08,                   /*11f: mov    0x8(%rax),%rax */
0x49, 0x89, 0x46, 0x10,                   /*123: mov    %rax,0x10(%r14) */
0x4c, 0x89, 0xe6,                         /*127: mov    %r12,%rsi */
0x4c, 0x89, 0xf2,                         /*12a: mov    %r14,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*12d: callq  132 <op_exec+0x132> */
0xeb, 0x1d,                               /*132: jmp    151 <op_exec+0x151> */
0x48, 0x8b, 0x40, 0x18,                   /*134: mov    0x18(%rax),%rax */
0x48, 0x8b, 0x48, 0x20,                   /*138: mov    0x20(%rax),%rcx */
0x48, 0x8b, 0x49, 0x10,                   /*13c: mov    0x10(%rcx),%rcx */
0x48, 0x89, 0x48, 0x08,                   /*140: mov    %rcx,0x8(%rax) */
0x49, 0x89, 0x4e, 0x18,                   /*144: mov    %rcx,0x18(%r14) */
0x49, 0x8b, 0x7e, 0x50,                   /*148: mov    0x50(%r14),%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*14c: callq  151 <op_exec+0x151> */
0x4c, 0x89, 0xf7,                         /*151: mov    %r14,%rdi */
0x5b,                                     /*154: pop    %rbx */
0x41, 0x5c,                               /*155: pop    %r12 */
0x41, 0x5e,                               /*157: pop    %r14 */
0x41, 0x5f,                               /*159: pop    %r15 */

};
static void op_exec_link(uint8_t *op) {
  *((int32_t *)(op + 26)) = (uint32_t)(((uint8_t *)cipush) + (0) - (op + 26));
  *((int32_t *)(op + 121)) = (uint32_t)(((uint8_t *)mrb_proc_new) + (0) - (op + 121));
  *((int32_t *)(op + 188)) = (uint32_t)(((uint8_t *)mrb_gc_arena_restore) + (0) - (op + 188));
  *((int32_t *)(op + 207)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 207));
  *((int32_t *)(op + 252)) = (uint32_t)(((uint8_t *)stack_extend) + (0) - (op + 252));
  *((int32_t *)(op + 302)) = (uint32_t)(((uint8_t *)mrb_proc_call_jit) + (0) - (op + 302));
  *((int32_t *)(op + 333)) = (uint32_t)(((uint8_t *)cipop) + (0) - (op + 333));
}

static void op_exec_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 21)) = a * 8 + 0;
  *((int32_t *)(op + 48)) = a * 1 + 0;
  *((int32_t *)(op + 97)) = a * 8 + 0;
  *((int32_t *)(op + 116)) = b * 8 + 0;
}

static void op_exec_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_exec_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}


/* args: {"b"=>[[4, 0, 18..21]], "a"=>[[8, 0, 25..28], [8, 8, 32..35]]} */
static uint8_t op_method[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0x48, 0x8b, 0x7b, 0x50,                   /*4: mov    0x50(%rbx),%rdi */
0x48, 0x8b, 0x43, 0x18,                   /*8: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x4b, 0x28,                   /*c: mov    0x28(%rbx),%rcx */
0x8b, 0x91, 0x00, 0x04, 0xbc, 0x00,       /*10: mov    0xbc0400(%rcx),%edx */
0x48, 0x8b, 0xb0, 0x00, 0x08, 0xab, 0x00, /*16: mov    0xab0800(%rax),%rsi */
0x48, 0x8b, 0x88, 0x08, 0x08, 0xab, 0x00, /*1d: mov    0xab0808(%rax),%rcx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*24: callq  29 <op_method+0x29> */
0x8b, 0x43, 0x48,                         /*29: mov    0x48(%rbx),%eax */
0x48, 0x8b, 0x4b, 0x50,                   /*2c: mov    0x50(%rbx),%rcx */
0x89, 0x81, 0xdc, 0x00, 0x00, 0x00,       /*30: mov    %eax,0xdc(%rcx) */
0x48, 0x89, 0xdf,                         /*36: mov    %rbx,%rdi */
0x5b,                                     /*39: pop    %rbx */

};
static void op_method_link(uint8_t *op) {
  *((int32_t *)(op + 37)) = (uint32_t)(((uint8_t *)mrb_define_method_vm) + (0) - (op + 37));
}

static void op_method_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 18)) = b * 4 + 0;
  *((int32_t *)(op + 25)) = a * 8 + 0;
  *((int32_t *)(op + 32)) = a * 8 + 8;
}

static void op_method_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_method_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"b"=>[[8, 0, 17..20]], "a"=>[[8, 0, 29..32]]} */
static uint8_t op_sclass[] = {
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
static void op_sclass_link(uint8_t *op) {
  *((int32_t *)(op + 22)) = (uint32_t)(((uint8_t *)mrb_singleton_class) + (0) - (op + 22));
}

static void op_sclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 17)) = b * 8 + 0;
  *((int32_t *)(op + 29)) = a * 8 + 0;
}

static void op_sclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_sclass_set_args(op, GETARG_A(c),GETARG_B(c),0,op_idx);
}


/* args: {"a"=>[[8, 0, 139..142]]} */
static uint8_t op_tclass[] = {
0x41, 0x57,                               /*0: push   %r15 */
0x41, 0x56,                               /*2: push   %r14 */
0x53,                                     /*4: push   %rbx */
0x49, 0x89, 0xfe,                         /*5: mov    %rdi,%r14 */
0x49, 0x8b, 0x5e, 0x50,                   /*8: mov    0x50(%r14),%rbx */
0x48, 0x8b, 0x43, 0x18,                   /*c: mov    0x18(%rbx),%rax */
0x48, 0x8b, 0x40, 0x20,                   /*10: mov    0x20(%rax),%rax */
0x48, 0x8b, 0x48, 0x48,                   /*14: mov    0x48(%rax),%rcx */
0x48, 0x85, 0xc9,                         /*18: test   %rcx,%rcx */
0x74, 0x22,                               /*1b: je     3f <op_tclass+0x3f> */
0x49, 0x8b, 0x46, 0x18,                   /*1d: mov    0x18(%r14),%rax */
0x8b, 0x31,                               /*21: mov    (%rcx),%esi */
0x40, 0x0f, 0xb6, 0xfe,                   /*23: movzbl %sil,%edi */
0xba, 0x04, 0x00, 0x00, 0x00,             /*27: mov    $0x4,%edx */
0x83, 0xff, 0x05,                         /*2c: cmp    $0x5,%edi */
0x77, 0x52,                               /*2f: ja     83 <op_tclass+0x83> */
0xff, 0x24, 0xfd, 0x00, 0x00, 0x00, 0x00, /*31: jmpq   *0x0(,%rdi,8) */
0xba, 0x02, 0x00, 0x00, 0x00,             /*38: mov    $0x2,%edx */
0xeb, 0x49,                               /*3d: jmp    88 <op_tclass+0x88> */
0x48, 0x8b, 0x35, 0x00, 0x00, 0x00, 0x00, /*3f: mov    0x0(%rip),%rsi        # 46 <op_tclass+0x46> */
0x48, 0x89, 0xdf,                         /*46: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*49: callq  4e <op_tclass+0x4e> */
0x49, 0x89, 0xc7,                         /*4e: mov    %rax,%r15 */
0xbe, 0x00, 0x00, 0x00, 0x00,             /*51: mov    $0x0,%esi */
0xba, 0x19, 0x00, 0x00, 0x00,             /*56: mov    $0x19,%edx */
0x48, 0x89, 0xdf,                         /*5b: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*5e: callq  63 <op_tclass+0x63> */
0x48, 0x89, 0xdf,                         /*63: mov    %rbx,%rdi */
0x4c, 0x89, 0xfe,                         /*66: mov    %r15,%rsi */
0x48, 0x89, 0xc2,                         /*69: mov    %rax,%rdx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*6c: callq  71 <op_tclass+0x71> */
0x49, 0x8b, 0x4e, 0x50,                   /*71: mov    0x50(%r14),%rcx */
0x48, 0x89, 0x41, 0x28,                   /*75: mov    %rax,0x28(%rcx) */
0x4c, 0x89, 0xf7,                         /*79: mov    %r14,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*7c: callq  81 <op_tclass+0x81> */
0xeb, 0x0c,                               /*81: jmp    8f <op_tclass+0x8f> */
0x89, 0x31,                               /*83: mov    %esi,(%rcx) */
0x48, 0x89, 0xca,                         /*85: mov    %rcx,%rdx */
0x48, 0x89, 0x90, 0x00, 0x08, 0xab, 0x00, /*88: mov    %rdx,0xab0800(%rax) */
0x4c, 0x89, 0xf7,                         /*8f: mov    %r14,%rdi */
0x5b,                                     /*92: pop    %rbx */
0x41, 0x5e,                               /*93: pop    %r14 */
0x41, 0x5f,                               /*95: pop    %r15 */
0xeb, 0x1a,                               /*97: jmp    b3 <_str_const_no_target_class+0x83> */
0x48, 0x83, 0xc9, 0x01,                   /*99: or     $0x1,%rcx */
0xeb, 0xe6,                               /*9d: jmp    85 <op_tclass+0x85> */
0x48, 0x81, 0xe1, 0x00, 0xff, 0xff, 0xff, /*9f: and    $0xffffffffffffff00,%rcx */
0x48, 0x83, 0xc9, 0x0e,                   /*a6: or     $0xe,%rcx */
0xeb, 0xd9,                               /*aa: jmp    85 <op_tclass+0x85> */
0xba, 0x06, 0x00, 0x00, 0x00,             /*ac: mov    $0x6,%edx */
0xeb, 0xd5,                               /*b1: jmp    88 <op_tclass+0x88> */

};
static void op_tclass_link(uint8_t *op) {
  *((int32_t *)(op + 66)) = (uint32_t)(((uint8_t *)_mrb_str_const_type_error) + (0) - (op + 66));
  *((int32_t *)(op + 74)) = (uint32_t)(((uint8_t *)mrb_class_get) + (0) - (op + 74));
  *((int32_t *)(op + 95)) = (uint32_t)(((uint8_t *)mrb_str_new_static) + (0) - (op + 95));
  *((int32_t *)(op + 109)) = (uint32_t)(((uint8_t *)mrb_exc_new_str) + (0) - (op + 109));
  *((int32_t *)(op + 125)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 125));
}

static void op_tclass_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 139)) = a * 8 + 0;
}

static void op_tclass_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_tclass_set_args(op, GETARG_A(c),0,0,op_idx);
}


/* args: {"a"=>[[1, 0, 10..13]], "b"=>[[1, 0, 15..18]], "c"=>[[1, 0, 20..23]]} */
static uint8_t op_debug[] = {
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
static void op_debug_link(uint8_t *op) {
  *((int32_t *)(op + 27)) = (uint32_t)(((uint8_t *)printf) + (0) - (op + 27));
}

static void op_debug_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 10)) = a * 1 + 0;
  *((int32_t *)(op + 15)) = b * 1 + 0;
  *((int32_t *)(op + 20)) = c * 1 + 0;
}

static void op_debug_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_debug_set_args(op, GETARG_A(c),GETARG_B(c),GETARG_C(c),op_idx);
}


/* args: {} */
static uint8_t op_stop[] = {
0x53,                                     /*0: push   %rbx */
0x48, 0x89, 0xfb,                         /*1: mov    %rdi,%rbx */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*4: callq  9 <op_stop+0x9> */
0x48, 0x89, 0xdf,                         /*9: mov    %rbx,%rdi */
0x5b,                                     /*c: pop    %rbx */

};
static void op_stop_link(uint8_t *op) {
  *((int32_t *)(op + 5)) = (uint32_t)(((uint8_t *)_op_stop) + (0) - (op + 5));
}

static void op_stop_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
}

static void op_stop_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_stop_set_args(op, 0,0,0,op_idx);
}


/* args: {"b"=>[[8, 0, 19..22]]} */
static uint8_t op_err[] = {
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
0x48, 0x8b, 0x4b, 0x50,                   /*3f: mov    0x50(%rbx),%rcx */
0x48, 0x89, 0x41, 0x28,                   /*43: mov    %rax,0x28(%rcx) */
0x48, 0x89, 0xdf,                         /*47: mov    %rbx,%rdi */
0xe8, 0x00, 0x00, 0x00, 0x00,             /*4a: callq  4f <op_err+0x4f> */
0x48, 0x89, 0xdf,                         /*4f: mov    %rbx,%rdi */
0x5b,                                     /*52: pop    %rbx */
0x41, 0x5e,                               /*53: pop    %r14 */
0x41, 0x5f,                               /*55: pop    %r15 */

};
static void op_err_link(uint8_t *op) {
  *((int32_t *)(op + 27)) = (uint32_t)(((uint8_t *)mrb_str_dup) + (0) - (op + 27));
  *((int32_t *)(op + 37)) = (uint32_t)(((uint8_t *)_mrb_str_const_localjump_error) + (0) - (op + 37));
  *((int32_t *)(op + 45)) = (uint32_t)(((uint8_t *)mrb_class_get) + (0) - (op + 45));
  *((int32_t *)(op + 59)) = (uint32_t)(((uint8_t *)mrb_exc_new_str) + (0) - (op + 59));
  *((int32_t *)(op + 75)) = (uint32_t)(((uint8_t *)_op_raise) + (0) - (op + 75));
}

static void op_err_set_args(uint8_t *op, uint32_t a, int32_t b, uint8_t c, uint32_t op_idx) {
  *((int32_t *)(op + 19)) = b * 8 + 0;
}

static void op_err_set_args_from_code(uint8_t *op, mrb_code c, uint32_t op_idx) {
  op_err_set_args(op, GETARG_A(c),GETARG_Bx(c),0,op_idx);
}

typedef void (*jit_args_func_t)(uint8_t *op, mrb_code c, uint32_t op_idx);
typedef void (*jit_link_func_t)(uint8_t *op);
static jit_args_func_t arg_funcs[78];
static jit_link_func_t link_funcs[78];
uint8_t* ops[78];
static char *op_names[78];

static size_t op_sizes[] = {
  sizeof(op_nop), /* 0 */
  sizeof(op_move), /* 18 */
  sizeof(op_loadl), /* 22 */
  sizeof(op_loadi), /* 51 */
  sizeof(op_loadsym), /* 46 */
  sizeof(op_loadnil), /* 41 */
  sizeof(op_loadself), /* 14 */
  sizeof(op_loadt), /* 52 */
  sizeof(op_loadf), /* 52 */
  sizeof(op_getglobal), /* 42 */
  sizeof(op_setglobal), /* 38 */
  sizeof(op_getspecial), /* 37 */
  sizeof(op_setspecial), /* 33 */
  sizeof(op_getiv), /* 42 */
  sizeof(op_setiv), /* 38 */
  sizeof(op_getcv), /* 82 */
  sizeof(op_setcv), /* 38 */
  sizeof(op_getconst), /* 90 */
  sizeof(op_setconst), /* 38 */
  sizeof(op_getmcnst), /* 101 */
  sizeof(op_setmcnst), /* 45 */
  sizeof(op_getupvar), /* 112 */
  sizeof(op_setupvar), /* 94 */
  sizeof(op_jmp), /* 0 */
  sizeof(op_jmpif), /* 19 */
  sizeof(op_jmpnot), /* 19 */
  sizeof(op_onerr), /* 145 */
  sizeof(op_rescue), /* 177 */
  sizeof(op_poperr), /* 35 */
  sizeof(op_raise), /* 32 */
  sizeof(op_epush), /* 167 */
  sizeof(op_epop), /* 84 */
  sizeof(op_send), /* 111 */
  sizeof(op_sendb), /* 56 */
  sizeof(op_fsend), /* 0 */
  sizeof(op_call), /* 419 */
  sizeof(op_super), /* 644 */
  sizeof(op_argary), /* 681 */
  sizeof(op_enter), /* 1570 */
  sizeof(op_enter_method_m), /* 437 */
  sizeof(op_karg), /* 0 */
  sizeof(op_kdict), /* 0 */
  sizeof(op_return), /* 38 */
  sizeof(op_break), /* 24 */
  sizeof(op_tailcall), /* 676 */
  sizeof(op_blkpush), /* 195 */
  sizeof(op_add), /* 344 */
  sizeof(op_addi), /* 208 */
  sizeof(op_sub), /* 302 */
  sizeof(op_subi), /* 192 */
  sizeof(op_mul), /* 344 */
  sizeof(op_div), /* 264 */
  sizeof(op_eq), /* 364 */
  sizeof(op_lt), /* 322 */
  sizeof(op_le), /* 322 */
  sizeof(op_gt), /* 322 */
  sizeof(op_ge), /* 322 */
  sizeof(op_array), /* 57 */
  sizeof(op_arycat), /* 70 */
  sizeof(op_arypush), /* 35 */
  sizeof(op_aref), /* 123 */
  sizeof(op_aset), /* 40 */
  sizeof(op_apost), /* 449 */
  sizeof(op_string), /* 56 */
  sizeof(op_strcat), /* 35 */
  sizeof(op_hash), /* 150 */
  sizeof(op_lambda), /* 28 */
  sizeof(op_range), /* 64 */
  sizeof(op_oclass), /* 97 */
  sizeof(op_class), /* 277 */
  sizeof(op_module), /* 234 */
  sizeof(op_exec), /* 347 */
  sizeof(op_method), /* 58 */
  sizeof(op_sclass), /* 52 */
  sizeof(op_tclass), /* 179 */
  sizeof(op_debug), /* 35 */
  sizeof(op_stop), /* 13 */
  sizeof(op_err), /* 87 */

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
    link_funcs[0] = op_nop_link;
    ops[1] = op_move;
    op_names[1] = "op_move";
    arg_funcs[1] = op_move_set_args_from_code;
    link_funcs[1] = op_move_link;
    ops[2] = op_loadl;
    op_names[2] = "op_loadl";
    arg_funcs[2] = op_loadl_set_args_from_code;
    link_funcs[2] = op_loadl_link;
    ops[3] = op_loadi;
    op_names[3] = "op_loadi";
    arg_funcs[3] = op_loadi_set_args_from_code;
    link_funcs[3] = op_loadi_link;
    ops[4] = op_loadsym;
    op_names[4] = "op_loadsym";
    arg_funcs[4] = op_loadsym_set_args_from_code;
    link_funcs[4] = op_loadsym_link;
    ops[5] = op_loadnil;
    op_names[5] = "op_loadnil";
    arg_funcs[5] = op_loadnil_set_args_from_code;
    link_funcs[5] = op_loadnil_link;
    ops[6] = op_loadself;
    op_names[6] = "op_loadself";
    arg_funcs[6] = op_loadself_set_args_from_code;
    link_funcs[6] = op_loadself_link;
    ops[7] = op_loadt;
    op_names[7] = "op_loadt";
    arg_funcs[7] = op_loadt_set_args_from_code;
    link_funcs[7] = op_loadt_link;
    ops[8] = op_loadf;
    op_names[8] = "op_loadf";
    arg_funcs[8] = op_loadf_set_args_from_code;
    link_funcs[8] = op_loadf_link;
    ops[9] = op_getglobal;
    op_names[9] = "op_getglobal";
    arg_funcs[9] = op_getglobal_set_args_from_code;
    link_funcs[9] = op_getglobal_link;
    ops[10] = op_setglobal;
    op_names[10] = "op_setglobal";
    arg_funcs[10] = op_setglobal_set_args_from_code;
    link_funcs[10] = op_setglobal_link;
    ops[11] = op_getspecial;
    op_names[11] = "op_getspecial";
    arg_funcs[11] = op_getspecial_set_args_from_code;
    link_funcs[11] = op_getspecial_link;
    ops[12] = op_setspecial;
    op_names[12] = "op_setspecial";
    arg_funcs[12] = op_setspecial_set_args_from_code;
    link_funcs[12] = op_setspecial_link;
    ops[13] = op_getiv;
    op_names[13] = "op_getiv";
    arg_funcs[13] = op_getiv_set_args_from_code;
    link_funcs[13] = op_getiv_link;
    ops[14] = op_setiv;
    op_names[14] = "op_setiv";
    arg_funcs[14] = op_setiv_set_args_from_code;
    link_funcs[14] = op_setiv_link;
    ops[15] = op_getcv;
    op_names[15] = "op_getcv";
    arg_funcs[15] = op_getcv_set_args_from_code;
    link_funcs[15] = op_getcv_link;
    ops[16] = op_setcv;
    op_names[16] = "op_setcv";
    arg_funcs[16] = op_setcv_set_args_from_code;
    link_funcs[16] = op_setcv_link;
    ops[17] = op_getconst;
    op_names[17] = "op_getconst";
    arg_funcs[17] = op_getconst_set_args_from_code;
    link_funcs[17] = op_getconst_link;
    ops[18] = op_setconst;
    op_names[18] = "op_setconst";
    arg_funcs[18] = op_setconst_set_args_from_code;
    link_funcs[18] = op_setconst_link;
    ops[19] = op_getmcnst;
    op_names[19] = "op_getmcnst";
    arg_funcs[19] = op_getmcnst_set_args_from_code;
    link_funcs[19] = op_getmcnst_link;
    ops[20] = op_setmcnst;
    op_names[20] = "op_setmcnst";
    arg_funcs[20] = op_setmcnst_set_args_from_code;
    link_funcs[20] = op_setmcnst_link;
    ops[21] = op_getupvar;
    op_names[21] = "op_getupvar";
    arg_funcs[21] = op_getupvar_set_args_from_code;
    link_funcs[21] = op_getupvar_link;
    ops[22] = op_setupvar;
    op_names[22] = "op_setupvar";
    arg_funcs[22] = op_setupvar_set_args_from_code;
    link_funcs[22] = op_setupvar_link;
    ops[23] = op_jmp;
    op_names[23] = "op_jmp";
    arg_funcs[23] = op_jmp_set_args_from_code;
    link_funcs[23] = op_jmp_link;
    ops[24] = op_jmpif;
    op_names[24] = "op_jmpif";
    arg_funcs[24] = op_jmpif_set_args_from_code;
    link_funcs[24] = op_jmpif_link;
    ops[25] = op_jmpnot;
    op_names[25] = "op_jmpnot";
    arg_funcs[25] = op_jmpnot_set_args_from_code;
    link_funcs[25] = op_jmpnot_link;
    ops[26] = op_onerr;
    op_names[26] = "op_onerr";
    arg_funcs[26] = op_onerr_set_args_from_code;
    link_funcs[26] = op_onerr_link;
    ops[27] = op_rescue;
    op_names[27] = "op_rescue";
    arg_funcs[27] = op_rescue_set_args_from_code;
    link_funcs[27] = op_rescue_link;
    ops[28] = op_poperr;
    op_names[28] = "op_poperr";
    arg_funcs[28] = op_poperr_set_args_from_code;
    link_funcs[28] = op_poperr_link;
    ops[29] = op_raise;
    op_names[29] = "op_raise";
    arg_funcs[29] = op_raise_set_args_from_code;
    link_funcs[29] = op_raise_link;
    ops[30] = op_epush;
    op_names[30] = "op_epush";
    arg_funcs[30] = op_epush_set_args_from_code;
    link_funcs[30] = op_epush_link;
    ops[31] = op_epop;
    op_names[31] = "op_epop";
    arg_funcs[31] = op_epop_set_args_from_code;
    link_funcs[31] = op_epop_link;
    ops[32] = op_send;
    op_names[32] = "op_send";
    arg_funcs[32] = op_send_set_args_from_code;
    link_funcs[32] = op_send_link;
    ops[33] = op_sendb;
    op_names[33] = "op_sendb";
    arg_funcs[33] = op_sendb_set_args_from_code;
    link_funcs[33] = op_sendb_link;
    ops[34] = op_fsend;
    op_names[34] = "op_fsend";
    arg_funcs[34] = op_fsend_set_args_from_code;
    link_funcs[34] = op_fsend_link;
    ops[35] = op_call;
    op_names[35] = "op_call";
    arg_funcs[35] = op_call_set_args_from_code;
    link_funcs[35] = op_call_link;
    ops[36] = op_super;
    op_names[36] = "op_super";
    arg_funcs[36] = op_super_set_args_from_code;
    link_funcs[36] = op_super_link;
    ops[37] = op_argary;
    op_names[37] = "op_argary";
    arg_funcs[37] = op_argary_set_args_from_code;
    link_funcs[37] = op_argary_link;
    ops[38] = op_enter;
    op_names[38] = "op_enter";
    arg_funcs[38] = op_enter_set_args_from_code;
    link_funcs[38] = op_enter_link;
    ops[39] = op_enter_method_m;
    op_names[39] = "op_enter_method_m";
    arg_funcs[39] = op_enter_method_m_set_args_from_code;
    link_funcs[39] = op_enter_method_m_link;
    ops[40] = op_karg;
    op_names[40] = "op_karg";
    arg_funcs[40] = op_karg_set_args_from_code;
    link_funcs[40] = op_karg_link;
    ops[41] = op_kdict;
    op_names[41] = "op_kdict";
    arg_funcs[41] = op_kdict_set_args_from_code;
    link_funcs[41] = op_kdict_link;
    ops[42] = op_return;
    op_names[42] = "op_return";
    arg_funcs[42] = op_return_set_args_from_code;
    link_funcs[42] = op_return_link;
    ops[43] = op_break;
    op_names[43] = "op_break";
    arg_funcs[43] = op_break_set_args_from_code;
    link_funcs[43] = op_break_link;
    ops[44] = op_tailcall;
    op_names[44] = "op_tailcall";
    arg_funcs[44] = op_tailcall_set_args_from_code;
    link_funcs[44] = op_tailcall_link;
    ops[45] = op_blkpush;
    op_names[45] = "op_blkpush";
    arg_funcs[45] = op_blkpush_set_args_from_code;
    link_funcs[45] = op_blkpush_link;
    ops[46] = op_add;
    op_names[46] = "op_add";
    arg_funcs[46] = op_add_set_args_from_code;
    link_funcs[46] = op_add_link;
    ops[47] = op_addi;
    op_names[47] = "op_addi";
    arg_funcs[47] = op_addi_set_args_from_code;
    link_funcs[47] = op_addi_link;
    ops[48] = op_sub;
    op_names[48] = "op_sub";
    arg_funcs[48] = op_sub_set_args_from_code;
    link_funcs[48] = op_sub_link;
    ops[49] = op_subi;
    op_names[49] = "op_subi";
    arg_funcs[49] = op_subi_set_args_from_code;
    link_funcs[49] = op_subi_link;
    ops[50] = op_mul;
    op_names[50] = "op_mul";
    arg_funcs[50] = op_mul_set_args_from_code;
    link_funcs[50] = op_mul_link;
    ops[51] = op_div;
    op_names[51] = "op_div";
    arg_funcs[51] = op_div_set_args_from_code;
    link_funcs[51] = op_div_link;
    ops[52] = op_eq;
    op_names[52] = "op_eq";
    arg_funcs[52] = op_eq_set_args_from_code;
    link_funcs[52] = op_eq_link;
    ops[53] = op_lt;
    op_names[53] = "op_lt";
    arg_funcs[53] = op_lt_set_args_from_code;
    link_funcs[53] = op_lt_link;
    ops[54] = op_le;
    op_names[54] = "op_le";
    arg_funcs[54] = op_le_set_args_from_code;
    link_funcs[54] = op_le_link;
    ops[55] = op_gt;
    op_names[55] = "op_gt";
    arg_funcs[55] = op_gt_set_args_from_code;
    link_funcs[55] = op_gt_link;
    ops[56] = op_ge;
    op_names[56] = "op_ge";
    arg_funcs[56] = op_ge_set_args_from_code;
    link_funcs[56] = op_ge_link;
    ops[57] = op_array;
    op_names[57] = "op_array";
    arg_funcs[57] = op_array_set_args_from_code;
    link_funcs[57] = op_array_link;
    ops[58] = op_arycat;
    op_names[58] = "op_arycat";
    arg_funcs[58] = op_arycat_set_args_from_code;
    link_funcs[58] = op_arycat_link;
    ops[59] = op_arypush;
    op_names[59] = "op_arypush";
    arg_funcs[59] = op_arypush_set_args_from_code;
    link_funcs[59] = op_arypush_link;
    ops[60] = op_aref;
    op_names[60] = "op_aref";
    arg_funcs[60] = op_aref_set_args_from_code;
    link_funcs[60] = op_aref_link;
    ops[61] = op_aset;
    op_names[61] = "op_aset";
    arg_funcs[61] = op_aset_set_args_from_code;
    link_funcs[61] = op_aset_link;
    ops[62] = op_apost;
    op_names[62] = "op_apost";
    arg_funcs[62] = op_apost_set_args_from_code;
    link_funcs[62] = op_apost_link;
    ops[63] = op_string;
    op_names[63] = "op_string";
    arg_funcs[63] = op_string_set_args_from_code;
    link_funcs[63] = op_string_link;
    ops[64] = op_strcat;
    op_names[64] = "op_strcat";
    arg_funcs[64] = op_strcat_set_args_from_code;
    link_funcs[64] = op_strcat_link;
    ops[65] = op_hash;
    op_names[65] = "op_hash";
    arg_funcs[65] = op_hash_set_args_from_code;
    link_funcs[65] = op_hash_link;
    ops[66] = op_lambda;
    op_names[66] = "op_lambda";
    arg_funcs[66] = op_lambda_set_args_from_code;
    link_funcs[66] = op_lambda_link;
    ops[67] = op_range;
    op_names[67] = "op_range";
    arg_funcs[67] = op_range_set_args_from_code;
    link_funcs[67] = op_range_link;
    ops[68] = op_oclass;
    op_names[68] = "op_oclass";
    arg_funcs[68] = op_oclass_set_args_from_code;
    link_funcs[68] = op_oclass_link;
    ops[69] = op_class;
    op_names[69] = "op_class";
    arg_funcs[69] = op_class_set_args_from_code;
    link_funcs[69] = op_class_link;
    ops[70] = op_module;
    op_names[70] = "op_module";
    arg_funcs[70] = op_module_set_args_from_code;
    link_funcs[70] = op_module_link;
    ops[71] = op_exec;
    op_names[71] = "op_exec";
    arg_funcs[71] = op_exec_set_args_from_code;
    link_funcs[71] = op_exec_link;
    ops[72] = op_method;
    op_names[72] = "op_method";
    arg_funcs[72] = op_method_set_args_from_code;
    link_funcs[72] = op_method_link;
    ops[73] = op_sclass;
    op_names[73] = "op_sclass";
    arg_funcs[73] = op_sclass_set_args_from_code;
    link_funcs[73] = op_sclass_link;
    ops[74] = op_tclass;
    op_names[74] = "op_tclass";
    arg_funcs[74] = op_tclass_set_args_from_code;
    link_funcs[74] = op_tclass_link;
    ops[75] = op_debug;
    op_names[75] = "op_debug";
    arg_funcs[75] = op_debug_set_args_from_code;
    link_funcs[75] = op_debug_link;
    ops[76] = op_stop;
    op_names[76] = "op_stop";
    arg_funcs[76] = op_stop_set_args_from_code;
    link_funcs[76] = op_stop_link;
    ops[77] = op_err;
    op_names[77] = "op_err";
    arg_funcs[77] = op_err_set_args_from_code;
    link_funcs[77] = op_err_link;
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
