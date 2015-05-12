typedef void (*jit_link_func_t)(uint8_t *op, uint8_t *data, mrb_code c);
jit_link_func_t link_funcs[78];
static void op_nop__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_nop__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_nop_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_nop__rodata__link(text, rodata, c);
  op_nop__text__link(text, rodata, c);
}
static void op_move__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_move__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_move_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_move__rodata__link(text, rodata, c);
  op_move__text__link(text, rodata, c);
}
static void op_loadl__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadl__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 14)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_loadl_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadl__rodata__link(text, rodata, c);
  op_loadl__text__link(text, rodata, c);
}
static void op_loadi__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadi__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 28)) = (uint32_t)(((uintptr_t)GETARG_sBx(c)) + (0));
}
static void op_loadi_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadi__rodata__link(text, rodata, c);
  op_loadi__text__link(text, rodata, c);
}
static void op_loadsym__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadsym__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 26)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
}
static void op_loadsym_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadsym__rodata__link(text, rodata, c);
  op_loadsym__text__link(text, rodata, c);
}
static void op_loadnil__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadnil__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_loadnil_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadnil__rodata__link(text, rodata, c);
  op_loadnil__text__link(text, rodata, c);
}
static void op_loadself__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadself__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_loadself_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadself__rodata__link(text, rodata, c);
  op_loadself__text__link(text, rodata, c);
}
static void op_loadt__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadt__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_loadt_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadt__rodata__link(text, rodata, c);
  op_loadt__text__link(text, rodata, c);
}
static void op_loadf__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadf__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_loadf_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadf__rodata__link(text, rodata, c);
  op_loadf__text__link(text, rodata, c);
}
static void op_getglobal__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getglobal__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 19)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 27)) = (int32_t)(((uintptr_t)mrb_gv_get) + (-4) - ((uintptr_t)(text + 27)));
*((uint32_t *)(text + 32)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_getglobal_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getglobal__rodata__link(text, rodata, c);
  op_getglobal__text__link(text, rodata, c);
}
static void op_setglobal__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setglobal__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 25)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 34)) = (int32_t)(((uintptr_t)mrb_gv_set) + (-4) - ((uintptr_t)(text + 34)));
}
static void op_setglobal_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setglobal__rodata__link(text, rodata, c);
  op_setglobal__text__link(text, rodata, c);
}
static void op_getspecial__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getspecial__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 20)) = (int32_t)(((uintptr_t)mrb_vm_special_get) + (-4) - ((uintptr_t)(text + 20)));
*((uint32_t *)(text + 25)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_getspecial_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getspecial__rodata__link(text, rodata, c);
  op_getspecial__text__link(text, rodata, c);
}
static void op_setspecial__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setspecial__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 22)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 27)) = (int32_t)(((uintptr_t)mrb_vm_special_set) + (-4) - ((uintptr_t)(text + 27)));
}
static void op_setspecial_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setspecial__rodata__link(text, rodata, c);
  op_setspecial__text__link(text, rodata, c);
}
static void op_getiv__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getiv__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 19)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 27)) = (int32_t)(((uintptr_t)mrb_vm_iv_get) + (-4) - ((uintptr_t)(text + 27)));
*((uint32_t *)(text + 32)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_getiv_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getiv__rodata__link(text, rodata, c);
  op_getiv__text__link(text, rodata, c);
}
static void op_setiv__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setiv__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 25)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 34)) = (int32_t)(((uintptr_t)mrb_vm_iv_set) + (-4) - ((uintptr_t)(text + 34)));
}
static void op_setiv_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setiv__rodata__link(text, rodata, c);
  op_setiv__text__link(text, rodata, c);
}
static void op_getcv__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getcv__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 39)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 47)) = (int32_t)(((uintptr_t)mrb_vm_cv_get) + (-4) - ((uintptr_t)(text + 47)));
*((uint32_t *)(text + 52)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_getcv_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getcv__rodata__link(text, rodata, c);
  op_getcv__text__link(text, rodata, c);
}
static void op_setcv__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setcv__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 25)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 34)) = (int32_t)(((uintptr_t)mrb_vm_cv_set) + (-4) - ((uintptr_t)(text + 34)));
}
static void op_setcv_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setcv__rodata__link(text, rodata, c);
  op_setcv__text__link(text, rodata, c);
}
static void op_getconst__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getconst__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 33)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 41)) = (int32_t)(((uintptr_t)mrb_vm_const_get) + (-4) - ((uintptr_t)(text + 41)));
*((uint32_t *)(text + 82)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_getconst_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getconst__rodata__link(text, rodata, c);
  op_getconst__text__link(text, rodata, c);
}
static void op_setconst__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setconst__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 25)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 34)) = (int32_t)(((uintptr_t)mrb_vm_const_set) + (-4) - ((uintptr_t)(text + 34)));
}
static void op_setconst_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setconst__rodata__link(text, rodata, c);
  op_setconst__text__link(text, rodata, c);
}
static void op_getmcnst__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getmcnst__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 39)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 47)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 59)) = (int32_t)(((uintptr_t)mrb_const_get) + (-4) - ((uintptr_t)(text + 59)));
}
static void op_getmcnst_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getmcnst__rodata__link(text, rodata, c);
  op_getmcnst__text__link(text, rodata, c);
}
static void op_setmcnst__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setmcnst__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 25)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 47)) = (int32_t)(((uintptr_t)mrb_const_set) + (-4) - ((uintptr_t)(text + 47)));
}
static void op_setmcnst_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setmcnst__rodata__link(text, rodata, c);
  op_setmcnst__text__link(text, rodata, c);
}
static void op_getupvar__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getupvar__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 31)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 72)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 91)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 103)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_getupvar_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getupvar__rodata__link(text, rodata, c);
  op_getupvar__text__link(text, rodata, c);
}
static void op_setupvar__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setupvar__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 31)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 79)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 84)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 104)) = (int32_t)(((uintptr_t)mrb_write_barrier) + (-4) - ((uintptr_t)(text + 104)));
}
static void op_setupvar_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setupvar__rodata__link(text, rodata, c);
  op_setupvar__text__link(text, rodata, c);
}
static void op_jmp__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_jmp__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_jmp_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_jmp__rodata__link(text, rodata, c);
  op_jmp__text__link(text, rodata, c);
}
static void op_jmpif__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_jmpif__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_jmpif_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_jmpif__rodata__link(text, rodata, c);
  op_jmpif__text__link(text, rodata, c);
}
static void op_jmpnot__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_jmpnot__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_jmpnot_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_jmpnot__rodata__link(text, rodata, c);
  op_jmpnot__text__link(text, rodata, c);
}
static void op_onerr__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_onerr__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  *((int32_t *)(text + 61)) = (int32_t)(((uintptr_t)mrb_realloc) + (-4) - ((uintptr_t)(text + 61)));
*((uint32_t *)(text + 101)) = (uint32_t)(((uintptr_t)GETARG_sBx(c)) + (0));
}
static void op_onerr_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_onerr__rodata__link(text, rodata, c);
  op_onerr__text__link(text, rodata, c);
}
static void op_rescue__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint64_t *)(rodata + 0)) = (uint64_t)(((uintptr_t)text) + (64));
*((uint64_t *)(rodata + 8)) = (uint64_t)(((uintptr_t)text) + (94));
*((uint64_t *)(rodata + 16)) = (uint64_t)(((uintptr_t)text) + (119));
*((uint64_t *)(rodata + 24)) = (uint64_t)(((uintptr_t)text) + (141));
*((uint64_t *)(rodata + 32)) = (uint64_t)(((uintptr_t)text) + (159));
*((uint64_t *)(rodata + 40)) = (uint64_t)(((uintptr_t)text) + (177));
}
static void op_rescue__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 60)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 81)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 99)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 124)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 146)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 164)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 182)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_rescue_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_rescue__rodata__link(text, rodata, c);
  op_rescue__text__link(text, rodata, c);
}
static void op_poperr__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_poperr__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 1)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_poperr_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_poperr__rodata__link(text, rodata, c);
  op_poperr__text__link(text, rodata, c);
}
static void op_raise__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_raise__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 26)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 26)));
}
static void op_raise_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_raise__rodata__link(text, rodata, c);
  op_raise__text__link(text, rodata, c);
}
static void op_epush__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_epush__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 19)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 28)) = (int32_t)(((uintptr_t)mrb_closure_new) + (-4) - ((uintptr_t)(text + 28)));
  *((int32_t *)(text + 94)) = (int32_t)(((uintptr_t)mrb_realloc) + (-4) - ((uintptr_t)(text + 94)));
}
static void op_epush_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_epush__rodata__link(text, rodata, c);
  op_epush__text__link(text, rodata, c);
}
static void op_epop__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_epop__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 12)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 40)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 63)) = (int32_t)(((uintptr_t)ecall) + (-4) - ((uintptr_t)(text + 63)));
}
static void op_epop_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_epop__rodata__link(text, rodata, c);
  op_epop__text__link(text, rodata, c);
}
static void op_send__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_send__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 49)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 54)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 60)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 65)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 65)));
}
static void op_send_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_send__rodata__link(text, rodata, c);
  op_send__text__link(text, rodata, c);
}
static void op_sendb__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_sendb__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 22)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 27)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 33)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 38)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 38)));
}
static void op_sendb_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_sendb__rodata__link(text, rodata, c);
  op_sendb__text__link(text, rodata, c);
}
static void op_fsend__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_fsend__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_fsend_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_fsend__rodata__link(text, rodata, c);
  op_fsend__text__link(text, rodata, c);
}
static void op_call__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_call__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  *((int32_t *)(text + 110)) = (int32_t)(((uintptr_t)mrb_gc_arena_restore) + (-4) - ((uintptr_t)(text + 110)));
  *((int32_t *)(text + 129)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 129)));
  *((int32_t *)(text + 253)) = (int32_t)(((uintptr_t)cipop) + (-4) - ((uintptr_t)(text + 253)));
*((uint32_t *)(text + 319)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 329)) = (int32_t)(((uintptr_t)_op_return) + (-4) - ((uintptr_t)(text + 329)));
  *((int32_t *)(text + 352)) = (int32_t)(((uintptr_t)stack_extend) + (-4) - ((uintptr_t)(text + 352)));
  *((int32_t *)(text + 409)) = (int32_t)(((uintptr_t)mrb_proc_call_jit) + (-4) - ((uintptr_t)(text + 409)));
}
static void op_call_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_call__rodata__link(text, rodata, c);
  op_call__text__link(text, rodata, c);
}
static void op_super__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_super__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  *((int32_t *)(text + 134)) = (int32_t)(((uintptr_t)_mrb_str_const_nomethod_error) + (-4) - ((uintptr_t)(text + 134)));
  *((int32_t *)(text + 142)) = (int32_t)(((uintptr_t)mrb_class_get) + (-4) - ((uintptr_t)(text + 142)));
*((uint32_t *)(text + 154)) = (uint32_t)(((uintptr_t)_str_const_super_outside_method) + (0));
  *((int32_t *)(text + 164)) = (int32_t)(((uintptr_t)mrb_str_new_static) + (-4) - ((uintptr_t)(text + 164)));
  *((int32_t *)(text + 178)) = (int32_t)(((uintptr_t)mrb_exc_new_str) + (-4) - ((uintptr_t)(text + 178)));
  *((int32_t *)(text + 202)) = (int32_t)(((uintptr_t)_mrb_method_search_vm) + (-4) - ((uintptr_t)(text + 202)));
*((uint32_t *)(text + 210)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 228)) = (uint32_t)(((uintptr_t)_str_const_method_missing) + (0));
  *((int32_t *)(text + 238)) = (int32_t)(((uintptr_t)mrb_intern_static) + (-4) - ((uintptr_t)(text + 238)));
  *((int32_t *)(text + 257)) = (int32_t)(((uintptr_t)mrb_method_search_vm) + (-4) - ((uintptr_t)(text + 257)));
*((uint32_t *)(text + 265)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 295)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 309)) = (int32_t)(((uintptr_t)mrb_ary_unshift) + (-4) - ((uintptr_t)(text + 309)));
*((uint32_t *)(text + 314)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 328)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 350)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 375)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 385)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 398)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 435)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 445)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 458)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 499)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 537)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 548)) = (int32_t)(((uintptr_t)cipush) + (-4) - ((uintptr_t)(text + 548)));
*((uint32_t *)(text + 618)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 703)) = (int32_t)(((uintptr_t)mrb_gc_arena_restore) + (-4) - ((uintptr_t)(text + 703)));
  *((int32_t *)(text + 722)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 722)));
*((uint32_t *)(text + 732)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 834)) = (int32_t)(((uintptr_t)cipop) + (-4) - ((uintptr_t)(text + 834)));
  *((int32_t *)(text + 847)) = (int32_t)(((uintptr_t)stack_extend) + (-4) - ((uintptr_t)(text + 847)));
  *((int32_t *)(text + 886)) = (int32_t)(((uintptr_t)mrb_proc_call_jit) + (-4) - ((uintptr_t)(text + 886)));
}
static void op_super_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_super__rodata__link(text, rodata, c);
  op_super__text__link(text, rodata, c);
}
static void op_argary__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_argary__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 18)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 122)) = (int32_t)(((uintptr_t)_mrb_str_const_nomethod_error) + (-4) - ((uintptr_t)(text + 122)));
  *((int32_t *)(text + 130)) = (int32_t)(((uintptr_t)mrb_class_get) + (-4) - ((uintptr_t)(text + 130)));
*((uint32_t *)(text + 142)) = (uint32_t)(((uintptr_t)_str_const_super_outside_method) + (0));
  *((int32_t *)(text + 152)) = (int32_t)(((uintptr_t)mrb_str_new_static) + (-4) - ((uintptr_t)(text + 152)));
  *((int32_t *)(text + 166)) = (int32_t)(((uintptr_t)mrb_exc_new_str) + (-4) - ((uintptr_t)(text + 166)));
  *((int32_t *)(text + 178)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 178)));
  *((int32_t *)(text + 355)) = (int32_t)(((uintptr_t)mrb_ary_new_from_values) + (-4) - ((uintptr_t)(text + 355)));
*((uint32_t *)(text + 366)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 418)) = (int32_t)(((uintptr_t)mrb_ary_new_capa) + (-4) - ((uintptr_t)(text + 418)));
*((uint32_t *)(text + 423)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 656)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_argary_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_argary__rodata__link(text, rodata, c);
  op_argary__text__link(text, rodata, c);
}
static void op_enter__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint64_t *)(rodata + 0)) = (uint64_t)(((uintptr_t)text) + (119));
*((uint64_t *)(rodata + 8)) = (uint64_t)(((uintptr_t)text) + (71));
*((uint64_t *)(rodata + 16)) = (uint64_t)(((uintptr_t)text) + (94));
*((uint64_t *)(rodata + 24)) = (uint64_t)(((uintptr_t)text) + (71));
*((uint64_t *)(rodata + 32)) = (uint64_t)(((uintptr_t)text) + (94));
*((uint64_t *)(rodata + 40)) = (uint64_t)(((uintptr_t)text) + (71));
*((uint64_t *)(rodata + 48)) = (uint64_t)(((uintptr_t)text) + (94));
}
static void op_enter__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((int32_t *)(text + 67)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 100)) = (uint32_t)(((uintptr_t)_str_const_proc) + (0));
*((uint32_t *)(text + 106)) = (uint32_t)(((uintptr_t)_str_const_to_proc) + (0));
  *((int32_t *)(text + 111)) = (int32_t)(((uintptr_t)mrb_convert_type) + (-4) - ((uintptr_t)(text + 111)));
  *((int32_t *)(text + 151)) = (int32_t)(((uintptr_t)mrb_gc_protect) + (-4) - ((uintptr_t)(text + 151)));
*((uint32_t *)(text + 199)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 204)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 215)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 233)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 280)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 285)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 304)) = (int32_t)(((uintptr_t)argnum_error) + (-4) - ((uintptr_t)(text + 304)));
  *((int32_t *)(text + 312)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 312)));
*((uint32_t *)(text + 331)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 336)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 347)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 360)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 434)) = (int32_t)(((uintptr_t)mrb_gc_protect) + (-4) - ((uintptr_t)(text + 434)));
*((uint32_t *)(text + 461)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 466)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 477)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 490)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 522)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 527)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 550)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 574)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 579)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 590)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 603)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 769)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 774)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 785)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 798)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 849)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 854)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 889)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 894)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 919)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 924)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1049)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1054)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1065)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1136)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1141)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1152)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1203)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1208)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1232)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1237)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1298)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1314)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1340)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1345)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1362)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 1424)) = (int32_t)(((uintptr_t)mrb_ary_new_from_values) + (-4) - ((uintptr_t)(text + 1424)));
*((uint32_t *)(text + 1461)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1466)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1498)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1518)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1523)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1534)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1581)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1609)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1626)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1631)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1642)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1655)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1748)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 1769)) = (int32_t)(((uintptr_t)mrb_ary_new_capa) + (-4) - ((uintptr_t)(text + 1769)));
*((uint32_t *)(text + 1774)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1779)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1808)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1813)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1851)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1868)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1873)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1910)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1923)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1983)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1988)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1999)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2012)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2057)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2070)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
}
static void op_enter_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_enter__rodata__link(text, rodata, c);
  op_enter__text__link(text, rodata, c);
}
static void op_enter_method_m__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_enter_method_m__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((int32_t *)(text + 77)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 131)) = (uint32_t)(((uintptr_t)_str_const_proc) + (0));
*((uint32_t *)(text + 137)) = (uint32_t)(((uintptr_t)_str_const_to_proc) + (0));
  *((int32_t *)(text + 142)) = (int32_t)(((uintptr_t)mrb_convert_type) + (-4) - ((uintptr_t)(text + 142)));
*((uint32_t *)(text + 159)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 181)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 192)) = (int32_t)(((uintptr_t)argnum_error) + (-4) - ((uintptr_t)(text + 192)));
  *((int32_t *)(text + 200)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 200)));
  *((int32_t *)(text + 226)) = (int32_t)(((uintptr_t)mrb_gc_protect) + (-4) - ((uintptr_t)(text + 226)));
*((uint32_t *)(text + 243)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 296)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 316)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 329)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 369)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 382)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
}
static void op_enter_method_m_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_enter_method_m__rodata__link(text, rodata, c);
  op_enter_method_m__text__link(text, rodata, c);
}
static void op_karg__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_karg__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_karg_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_karg__rodata__link(text, rodata, c);
  op_karg__text__link(text, rodata, c);
}
static void op_kdict__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_kdict__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_kdict_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_kdict__rodata__link(text, rodata, c);
  op_kdict__text__link(text, rodata, c);
}
static void op_return__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_return__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 24)) = (int32_t)(((uintptr_t)_op_return) + (-4) - ((uintptr_t)(text + 24)));
}
static void op_return_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_return__rodata__link(text, rodata, c);
  op_return__text__link(text, rodata, c);
}
static void op_break__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_break__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 15)) = (int32_t)(((uintptr_t)_op_return) + (-4) - ((uintptr_t)(text + 15)));
}
static void op_break_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_break__rodata__link(text, rodata, c);
  op_break__text__link(text, rodata, c);
}
static void op_tailcall__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint64_t *)(rodata + 0)) = (uint64_t)(((uintptr_t)text) + (98));
*((uint64_t *)(rodata + 8)) = (uint64_t)(((uintptr_t)text) + (63));
*((uint64_t *)(rodata + 16)) = (uint64_t)(((uintptr_t)text) + (98));
*((uint64_t *)(rodata + 24)) = (uint64_t)(((uintptr_t)text) + (63));
*((uint64_t *)(rodata + 32)) = (uint64_t)(((uintptr_t)text) + (130));
*((uint64_t *)(rodata + 40)) = (uint64_t)(((uintptr_t)text) + (63));
*((uint64_t *)(rodata + 48)) = (uint64_t)(((uintptr_t)text) + (153));
*((uint64_t *)(rodata + 56)) = (uint64_t)(((uintptr_t)text) + (98));
*((uint64_t *)(rodata + 64)) = (uint64_t)(((uintptr_t)text) + (153));
*((uint64_t *)(rodata + 72)) = (uint64_t)(((uintptr_t)text) + (130));
*((uint64_t *)(rodata + 80)) = (uint64_t)(((uintptr_t)text) + (112));
*((uint64_t *)(rodata + 88)) = (uint64_t)(((uintptr_t)text) + (121));
*((uint64_t *)(rodata + 96)) = (uint64_t)(((uintptr_t)text) + (153));
*((uint64_t *)(rodata + 104)) = (uint64_t)(((uintptr_t)text) + (920));
*((uint64_t *)(rodata + 112)) = (uint64_t)(((uintptr_t)text) + (929));
}
static void op_tailcall__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 30)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 39)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 59)) = (int32_t)(((uintptr_t)rodata) + (0));
*((int32_t *)(text + 94)) = (int32_t)(((uintptr_t)rodata) + (56));
  *((int32_t *)(text + 252)) = (int32_t)(((uintptr_t)_mrb_method_search_vm) + (-4) - ((uintptr_t)(text + 252)));
*((uint32_t *)(text + 260)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 291)) = (uint32_t)(((uintptr_t)_str_const_method_missing) + (0));
  *((int32_t *)(text + 304)) = (int32_t)(((uintptr_t)mrb_intern_static) + (-4) - ((uintptr_t)(text + 304)));
  *((int32_t *)(text + 323)) = (int32_t)(((uintptr_t)mrb_method_search_vm) + (-4) - ((uintptr_t)(text + 323)));
*((uint32_t *)(text + 335)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 350)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 370)) = (int32_t)(((uintptr_t)mrb_ary_unshift) + (-4) - ((uintptr_t)(text + 370)));
*((uint32_t *)(text + 375)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 385)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 407)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 437)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 447)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 460)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 504)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 514)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 527)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 563)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 577)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 630)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 652)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 676)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 777)) = (int32_t)(((uintptr_t)mrb_gc_arena_restore) + (-4) - ((uintptr_t)(text + 777)));
*((uint32_t *)(text + 782)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 792)) = (int32_t)(((uintptr_t)_op_return) + (-4) - ((uintptr_t)(text + 792)));
  *((int32_t *)(text + 858)) = (int32_t)(((uintptr_t)stack_extend) + (-4) - ((uintptr_t)(text + 858)));
  *((int32_t *)(text + 897)) = (int32_t)(((uintptr_t)mrb_proc_call_jit) + (-4) - ((uintptr_t)(text + 897)));
}
static void op_tailcall_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_tailcall__rodata__link(text, rodata, c);
  op_tailcall__text__link(text, rodata, c);
}
static void op_blkpush__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_blkpush__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 11)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 109)) = (int32_t)(((uintptr_t)localjump_error) + (-4) - ((uintptr_t)(text + 109)));
  *((int32_t *)(text + 117)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 117)));
*((uint32_t *)(text + 179)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_blkpush_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_blkpush__rodata__link(text, rodata, c);
  op_blkpush__text__link(text, rodata, c);
}
static void op_add__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_add__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 46)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 89)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 121)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 185)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 211)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 248)) = (int32_t)(((uintptr_t)mrb_str_plus) + (-4) - ((uintptr_t)(text + 248)));
  *((int32_t *)(text + 320)) = (int32_t)(((uintptr_t)op_send) + (-4) - ((uintptr_t)(text + 320)));
  *((int32_t *)(text + 334)) = (int32_t)(((uintptr_t)mrb_word_boxing_float_value) + (-4) - ((uintptr_t)(text + 334)));
*((uint32_t *)(text + 339)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_add_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_add__rodata__link(text, rodata, c);
  op_add__text__link(text, rodata, c);
}
static void op_addi__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_addi__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 63)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 86)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 106)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 131)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 148)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 167)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 181)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 181)));
*((uint32_t *)(text + 192)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 205)) = (int32_t)(((uintptr_t)mrb_word_boxing_float_value) + (-4) - ((uintptr_t)(text + 205)));
*((uint32_t *)(text + 210)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_addi_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_addi__rodata__link(text, rodata, c);
  op_addi__text__link(text, rodata, c);
}
static void op_sub__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_sub__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 46)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 87)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 119)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 192)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 278)) = (int32_t)(((uintptr_t)op_send) + (-4) - ((uintptr_t)(text + 278)));
  *((int32_t *)(text + 297)) = (int32_t)(((uintptr_t)mrb_word_boxing_float_value) + (-4) - ((uintptr_t)(text + 297)));
*((uint32_t *)(text + 302)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_sub_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_sub__rodata__link(text, rodata, c);
  op_sub__text__link(text, rodata, c);
}
static void op_subi__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_subi__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 77)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 100)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 125)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 142)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 161)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 175)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 175)));
*((uint32_t *)(text + 186)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 199)) = (int32_t)(((uintptr_t)mrb_word_boxing_float_value) + (-4) - ((uintptr_t)(text + 199)));
*((uint32_t *)(text + 204)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_subi_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_subi__rodata__link(text, rodata, c);
  op_subi__text__link(text, rodata, c);
}
static void op_mul__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_mul__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 46)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 89)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 121)) = (int32_t)(((uintptr_t)rodata) + (0));
  *((int32_t *)(text + 178)) = (int32_t)(((uintptr_t)mrb_fixnum_mul) + (-4) - ((uintptr_t)(text + 178)));
  *((int32_t *)(text + 298)) = (int32_t)(((uintptr_t)mrb_word_boxing_float_value) + (-4) - ((uintptr_t)(text + 298)));
*((uint32_t *)(text + 303)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 320)) = (int32_t)(((uintptr_t)op_send) + (-4) - ((uintptr_t)(text + 320)));
*((uint32_t *)(text + 332)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_mul_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_mul__rodata__link(text, rodata, c);
  op_mul__text__link(text, rodata, c);
}
static void op_div__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_div__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 44)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 85)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 117)) = (int32_t)(((uintptr_t)rodata) + (0));
  *((int32_t *)(text + 248)) = (int32_t)(((uintptr_t)mrb_word_boxing_float_value) + (-4) - ((uintptr_t)(text + 248)));
*((uint32_t *)(text + 253)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 270)) = (int32_t)(((uintptr_t)op_send) + (-4) - ((uintptr_t)(text + 270)));
}
static void op_div_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_div__rodata__link(text, rodata, c);
  op_div__text__link(text, rodata, c);
}
static void op_eq__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_eq__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 31)) = (int32_t)(((uintptr_t)mrb_obj_eq) + (-4) - ((uintptr_t)(text + 31)));
*((uint32_t *)(text + 44)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 73)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 114)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 146)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 282)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 306)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 329)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 351)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 356)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 362)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 370)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 370)));
}
static void op_eq_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_eq__rodata__link(text, rodata, c);
  op_eq__text__link(text, rodata, c);
}
static void op_lt__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_lt__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 11)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 41)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 83)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 115)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 251)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 274)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 309)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 314)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 320)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 328)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 328)));
}
static void op_lt_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_lt__rodata__link(text, rodata, c);
  op_lt__text__link(text, rodata, c);
}
static void op_le__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_le__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 11)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 41)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 83)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 115)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 251)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 274)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 309)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 314)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 320)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 328)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 328)));
}
static void op_le_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_le__rodata__link(text, rodata, c);
  op_le__text__link(text, rodata, c);
}
static void op_gt__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_gt__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 11)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 41)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 83)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 115)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 251)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 274)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 309)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 314)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 320)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 328)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 328)));
}
static void op_gt_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_gt__rodata__link(text, rodata, c);
  op_gt__text__link(text, rodata, c);
}
static void op_ge__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_ge__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 11)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 41)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 83)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 115)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 251)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 274)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 309)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 314)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 320)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 328)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 328)));
}
static void op_ge_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_ge__rodata__link(text, rodata, c);
  op_ge__text__link(text, rodata, c);
}
static void op_array__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_array__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 24)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 29)) = (int32_t)(((uintptr_t)mrb_ary_new_from_values) + (-4) - ((uintptr_t)(text + 29)));
*((uint32_t *)(text + 34)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_array_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_array__rodata__link(text, rodata, c);
  op_array__text__link(text, rodata, c);
}
static void op_arycat__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_arycat__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 29)) = (int32_t)(((uintptr_t)mrb_ary_splat) + (-4) - ((uintptr_t)(text + 29)));
*((uint32_t *)(text + 34)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 49)) = (int32_t)(((uintptr_t)mrb_ary_concat) + (-4) - ((uintptr_t)(text + 49)));
}
static void op_arycat_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_arycat__rodata__link(text, rodata, c);
  op_arycat__text__link(text, rodata, c);
}
static void op_arypush__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_arypush__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 22)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 31)) = (int32_t)(((uintptr_t)mrb_ary_push) + (-4) - ((uintptr_t)(text + 31)));
}
static void op_arypush_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_arypush__rodata__link(text, rodata, c);
  op_arypush__text__link(text, rodata, c);
}
static void op_aref__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_aref__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 11)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 64)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 69)) = (int32_t)(((uintptr_t)mrb_ary_ref) + (-4) - ((uintptr_t)(text + 69)));
*((uint32_t *)(text + 74)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 85)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 94)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 105)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_aref_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_aref__rodata__link(text, rodata, c);
  op_aref__text__link(text, rodata, c);
}
static void op_aset__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_aset__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 22)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 31)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 36)) = (int32_t)(((uintptr_t)mrb_ary_set) + (-4) - ((uintptr_t)(text + 36)));
}
static void op_aset_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_aset__rodata__link(text, rodata, c);
  op_aset__text__link(text, rodata, c);
}
static void op_apost__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_apost__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 16)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 88)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 93)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 113)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 118)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 141)) = (int32_t)(((uintptr_t)mrb_ary_new_from_values) + (-4) - ((uintptr_t)(text + 141)));
*((uint32_t *)(text + 146)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 219)) = (int32_t)(((uintptr_t)mrb_ary_new_capa) + (-4) - ((uintptr_t)(text + 219)));
*((uint32_t *)(text + 224)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 236)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 306)) = (int32_t)(((uintptr_t)mrb_ary_new_capa) + (-4) - ((uintptr_t)(text + 306)));
*((uint32_t *)(text + 311)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 323)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 337)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 353)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 400)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 409)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 419)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_apost_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_apost__rodata__link(text, rodata, c);
  op_apost__text__link(text, rodata, c);
}
static void op_string__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_string__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 19)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 28)) = (int32_t)(((uintptr_t)mrb_str_dup) + (-4) - ((uintptr_t)(text + 28)));
*((uint32_t *)(text + 33)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_string_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_string__rodata__link(text, rodata, c);
  op_string__text__link(text, rodata, c);
}
static void op_strcat__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_strcat__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 22)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 31)) = (int32_t)(((uintptr_t)mrb_str_concat) + (-4) - ((uintptr_t)(text + 31)));
}
static void op_strcat_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_strcat__rodata__link(text, rodata, c);
  op_strcat__text__link(text, rodata, c);
}
static void op_hash__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_hash__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 19)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 45)) = (int32_t)(((uintptr_t)mrb_hash_new_capa) + (-4) - ((uintptr_t)(text + 45)));
  *((int32_t *)(text + 99)) = (int32_t)(((uintptr_t)mrb_hash_set) + (-4) - ((uintptr_t)(text + 99)));
*((uint32_t *)(text + 122)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_hash_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_hash__rodata__link(text, rodata, c);
  op_hash__text__link(text, rodata, c);
}
static void op_lambda__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_lambda__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_b(c)) + (0));
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_c(c)) + (0));
  *((int32_t *)(text + 20)) = (int32_t)(((uintptr_t)_op_lambda) + (-4) - ((uintptr_t)(text + 20)));
}
static void op_lambda_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_lambda__rodata__link(text, rodata, c);
  op_lambda__text__link(text, rodata, c);
}
static void op_range__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_range__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 39)) = (int32_t)(((uintptr_t)mrb_range_new) + (-4) - ((uintptr_t)(text + 39)));
*((uint32_t *)(text + 44)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_range_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_range__rodata__link(text, rodata, c);
  op_range__text__link(text, rodata, c);
}
static void op_oclass__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint64_t *)(rodata + 0)) = (uint64_t)(((uintptr_t)text) + (35));
*((uint64_t *)(rodata + 8)) = (uint64_t)(((uintptr_t)text) + (49));
*((uint64_t *)(rodata + 16)) = (uint64_t)(((uintptr_t)text) + (81));
*((uint64_t *)(rodata + 24)) = (uint64_t)(((uintptr_t)text) + (61));
*((uint64_t *)(rodata + 32)) = (uint64_t)(((uintptr_t)text) + (67));
*((uint64_t *)(rodata + 40)) = (uint64_t)(((uintptr_t)text) + (92));
}
static void op_oclass__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((int32_t *)(text + 31)) = (int32_t)(((uintptr_t)rodata) + (0));
*((uint32_t *)(text + 82)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_oclass_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_oclass__rodata__link(text, rodata, c);
  op_oclass__text__link(text, rodata, c);
}
static void op_class__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint64_t *)(rodata + 0)) = (uint64_t)(((uintptr_t)text) + (86));
*((uint64_t *)(rodata + 8)) = (uint64_t)(((uintptr_t)text) + (100));
*((uint64_t *)(rodata + 16)) = (uint64_t)(((uintptr_t)text) + (132));
*((uint64_t *)(rodata + 24)) = (uint64_t)(((uintptr_t)text) + (113));
*((uint64_t *)(rodata + 32)) = (uint64_t)(((uintptr_t)text) + (119));
*((uint64_t *)(rodata + 40)) = (uint64_t)(((uintptr_t)text) + (250));
*((uint64_t *)(rodata + 48)) = (uint64_t)(((uintptr_t)text) + (168));
*((uint64_t *)(rodata + 56)) = (uint64_t)(((uintptr_t)text) + (182));
*((uint64_t *)(rodata + 64)) = (uint64_t)(((uintptr_t)text) + (213));
*((uint64_t *)(rodata + 72)) = (uint64_t)(((uintptr_t)text) + (194));
*((uint64_t *)(rodata + 80)) = (uint64_t)(((uintptr_t)text) + (200));
*((uint64_t *)(rodata + 88)) = (uint64_t)(((uintptr_t)text) + (243));
}
static void op_class__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 21)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 82)) = (int32_t)(((uintptr_t)rodata) + (0));
  *((int32_t *)(text + 137)) = (int32_t)(((uintptr_t)mrb_vm_define_class) + (-4) - ((uintptr_t)(text + 137)));
*((int32_t *)(text + 164)) = (int32_t)(((uintptr_t)rodata) + (48));
*((uint32_t *)(text + 214)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_class_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_class__rodata__link(text, rodata, c);
  op_class__text__link(text, rodata, c);
}
static void op_module__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint64_t *)(rodata + 0)) = (uint64_t)(((uintptr_t)text) + (75));
*((uint64_t *)(rodata + 8)) = (uint64_t)(((uintptr_t)text) + (89));
*((uint64_t *)(rodata + 16)) = (uint64_t)(((uintptr_t)text) + (120));
*((uint64_t *)(rodata + 24)) = (uint64_t)(((uintptr_t)text) + (101));
*((uint64_t *)(rodata + 32)) = (uint64_t)(((uintptr_t)text) + (107));
*((uint64_t *)(rodata + 40)) = (uint64_t)(((uintptr_t)text) + (238));
*((uint64_t *)(rodata + 48)) = (uint64_t)(((uintptr_t)text) + (156));
*((uint64_t *)(rodata + 56)) = (uint64_t)(((uintptr_t)text) + (170));
*((uint64_t *)(rodata + 64)) = (uint64_t)(((uintptr_t)text) + (201));
*((uint64_t *)(rodata + 72)) = (uint64_t)(((uintptr_t)text) + (182));
*((uint64_t *)(rodata + 80)) = (uint64_t)(((uintptr_t)text) + (188));
*((uint64_t *)(rodata + 88)) = (uint64_t)(((uintptr_t)text) + (231));
}
static void op_module__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 21)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 71)) = (int32_t)(((uintptr_t)rodata) + (0));
  *((int32_t *)(text + 125)) = (int32_t)(((uintptr_t)mrb_vm_define_module) + (-4) - ((uintptr_t)(text + 125)));
*((int32_t *)(text + 152)) = (int32_t)(((uintptr_t)rodata) + (48));
*((uint32_t *)(text + 202)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_module_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_module__rodata__link(text, rodata, c);
  op_module__text__link(text, rodata, c);
}
static void op_exec__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_exec__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 11)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 31)) = (int32_t)(((uintptr_t)cipush) + (-4) - ((uintptr_t)(text + 31)));
*((uint32_t *)(text + 120)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 129)) = (int32_t)(((uintptr_t)mrb_proc_new) + (-4) - ((uintptr_t)(text + 129)));
  *((int32_t *)(text + 196)) = (int32_t)(((uintptr_t)mrb_gc_arena_restore) + (-4) - ((uintptr_t)(text + 196)));
  *((int32_t *)(text + 215)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 215)));
  *((int32_t *)(text + 260)) = (int32_t)(((uintptr_t)stack_extend) + (-4) - ((uintptr_t)(text + 260)));
  *((int32_t *)(text + 310)) = (int32_t)(((uintptr_t)mrb_proc_call_jit) + (-4) - ((uintptr_t)(text + 310)));
  *((int32_t *)(text + 341)) = (int32_t)(((uintptr_t)cipop) + (-4) - ((uintptr_t)(text + 341)));
}
static void op_exec_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_exec__rodata__link(text, rodata, c);
  op_exec__text__link(text, rodata, c);
}
static void op_method__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_method__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 28)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 45)) = (int32_t)(((uintptr_t)mrb_define_method_vm) + (-4) - ((uintptr_t)(text + 45)));
}
static void op_method_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_method__rodata__link(text, rodata, c);
  op_method__text__link(text, rodata, c);
}
static void op_sclass__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_sclass__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 24)) = (int32_t)(((uintptr_t)mrb_singleton_class) + (-4) - ((uintptr_t)(text + 24)));
*((uint32_t *)(text + 29)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_sclass_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_sclass__rodata__link(text, rodata, c);
  op_sclass__text__link(text, rodata, c);
}
static void op_tclass__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint64_t *)(rodata + 0)) = (uint64_t)(((uintptr_t)text) + (56));
*((uint64_t *)(rodata + 8)) = (uint64_t)(((uintptr_t)text) + (131));
*((uint64_t *)(rodata + 16)) = (uint64_t)(((uintptr_t)text) + (136));
*((uint64_t *)(rodata + 24)) = (uint64_t)(((uintptr_t)text) + (155));
*((uint64_t *)(rodata + 32)) = (uint64_t)(((uintptr_t)text) + (161));
*((uint64_t *)(rodata + 40)) = (uint64_t)(((uintptr_t)text) + (174));
}
static void op_tclass__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((int32_t *)(text + 52)) = (int32_t)(((uintptr_t)rodata) + (0));
  *((int32_t *)(text + 66)) = (int32_t)(((uintptr_t)_mrb_str_const_type_error) + (-4) - ((uintptr_t)(text + 66)));
  *((int32_t *)(text + 74)) = (int32_t)(((uintptr_t)mrb_class_get) + (-4) - ((uintptr_t)(text + 74)));
*((uint32_t *)(text + 82)) = (uint32_t)(((uintptr_t)_str_const_no_target_class) + (0));
  *((int32_t *)(text + 95)) = (int32_t)(((uintptr_t)mrb_str_new_static) + (-4) - ((uintptr_t)(text + 95)));
  *((int32_t *)(text + 109)) = (int32_t)(((uintptr_t)mrb_exc_new_str) + (-4) - ((uintptr_t)(text + 109)));
  *((int32_t *)(text + 125)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 125)));
*((uint32_t *)(text + 137)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_tclass_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_tclass__rodata__link(text, rodata, c);
  op_tclass__text__link(text, rodata, c);
}
static void op_debug__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_debug__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)_str_const_op_debug_format) + (0));
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 20)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 27)) = (int32_t)(((uintptr_t)printf) + (-4) - ((uintptr_t)(text + 27)));
}
static void op_debug_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_debug__rodata__link(text, rodata, c);
  op_debug__text__link(text, rodata, c);
}
static void op_stop__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_stop__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  *((int32_t *)(text + 5)) = (int32_t)(((uintptr_t)_op_stop) + (-4) - ((uintptr_t)(text + 5)));
}
static void op_stop_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_stop__rodata__link(text, rodata, c);
  op_stop__text__link(text, rodata, c);
}
static void op_err__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_err__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 17)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 29)) = (int32_t)(((uintptr_t)mrb_str_dup) + (-4) - ((uintptr_t)(text + 29)));
  *((int32_t *)(text + 39)) = (int32_t)(((uintptr_t)_mrb_str_const_localjump_error) + (-4) - ((uintptr_t)(text + 39)));
  *((int32_t *)(text + 47)) = (int32_t)(((uintptr_t)mrb_class_get) + (-4) - ((uintptr_t)(text + 47)));
  *((int32_t *)(text + 61)) = (int32_t)(((uintptr_t)mrb_exc_new_str) + (-4) - ((uintptr_t)(text + 61)));
  *((int32_t *)(text + 77)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 77)));
}
static void op_err_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_err__rodata__link(text, rodata, c);
  op_err__text__link(text, rodata, c);
}
void init_linker() {
  static int init = 0;
  if(init == 0) {
    init = 1;
    link_funcs[0] = (jit_link_func_t) op_nop_link;
    link_funcs[1] = (jit_link_func_t) op_move_link;
    link_funcs[2] = (jit_link_func_t) op_loadl_link;
    link_funcs[3] = (jit_link_func_t) op_loadi_link;
    link_funcs[4] = (jit_link_func_t) op_loadsym_link;
    link_funcs[5] = (jit_link_func_t) op_loadnil_link;
    link_funcs[6] = (jit_link_func_t) op_loadself_link;
    link_funcs[7] = (jit_link_func_t) op_loadt_link;
    link_funcs[8] = (jit_link_func_t) op_loadf_link;
    link_funcs[9] = (jit_link_func_t) op_getglobal_link;
    link_funcs[10] = (jit_link_func_t) op_setglobal_link;
    link_funcs[11] = (jit_link_func_t) op_getspecial_link;
    link_funcs[12] = (jit_link_func_t) op_setspecial_link;
    link_funcs[13] = (jit_link_func_t) op_getiv_link;
    link_funcs[14] = (jit_link_func_t) op_setiv_link;
    link_funcs[15] = (jit_link_func_t) op_getcv_link;
    link_funcs[16] = (jit_link_func_t) op_setcv_link;
    link_funcs[17] = (jit_link_func_t) op_getconst_link;
    link_funcs[18] = (jit_link_func_t) op_setconst_link;
    link_funcs[19] = (jit_link_func_t) op_getmcnst_link;
    link_funcs[20] = (jit_link_func_t) op_setmcnst_link;
    link_funcs[21] = (jit_link_func_t) op_getupvar_link;
    link_funcs[22] = (jit_link_func_t) op_setupvar_link;
    link_funcs[23] = (jit_link_func_t) op_jmp_link;
    link_funcs[24] = (jit_link_func_t) op_jmpif_link;
    link_funcs[25] = (jit_link_func_t) op_jmpnot_link;
    link_funcs[26] = (jit_link_func_t) op_onerr_link;
    link_funcs[27] = (jit_link_func_t) op_rescue_link;
    link_funcs[28] = (jit_link_func_t) op_poperr_link;
    link_funcs[29] = (jit_link_func_t) op_raise_link;
    link_funcs[30] = (jit_link_func_t) op_epush_link;
    link_funcs[31] = (jit_link_func_t) op_epop_link;
    link_funcs[32] = (jit_link_func_t) op_send_link;
    link_funcs[33] = (jit_link_func_t) op_sendb_link;
    link_funcs[34] = (jit_link_func_t) op_fsend_link;
    link_funcs[35] = (jit_link_func_t) op_call_link;
    link_funcs[36] = (jit_link_func_t) op_super_link;
    link_funcs[37] = (jit_link_func_t) op_argary_link;
    link_funcs[38] = (jit_link_func_t) op_enter_link;
    link_funcs[39] = (jit_link_func_t) op_enter_method_m_link;
    link_funcs[40] = (jit_link_func_t) op_karg_link;
    link_funcs[41] = (jit_link_func_t) op_kdict_link;
    link_funcs[42] = (jit_link_func_t) op_return_link;
    link_funcs[43] = (jit_link_func_t) op_break_link;
    link_funcs[44] = (jit_link_func_t) op_tailcall_link;
    link_funcs[45] = (jit_link_func_t) op_blkpush_link;
    link_funcs[46] = (jit_link_func_t) op_add_link;
    link_funcs[47] = (jit_link_func_t) op_addi_link;
    link_funcs[48] = (jit_link_func_t) op_sub_link;
    link_funcs[49] = (jit_link_func_t) op_subi_link;
    link_funcs[50] = (jit_link_func_t) op_mul_link;
    link_funcs[51] = (jit_link_func_t) op_div_link;
    link_funcs[52] = (jit_link_func_t) op_eq_link;
    link_funcs[53] = (jit_link_func_t) op_lt_link;
    link_funcs[54] = (jit_link_func_t) op_le_link;
    link_funcs[55] = (jit_link_func_t) op_gt_link;
    link_funcs[56] = (jit_link_func_t) op_ge_link;
    link_funcs[57] = (jit_link_func_t) op_array_link;
    link_funcs[58] = (jit_link_func_t) op_arycat_link;
    link_funcs[59] = (jit_link_func_t) op_arypush_link;
    link_funcs[60] = (jit_link_func_t) op_aref_link;
    link_funcs[61] = (jit_link_func_t) op_aset_link;
    link_funcs[62] = (jit_link_func_t) op_apost_link;
    link_funcs[63] = (jit_link_func_t) op_string_link;
    link_funcs[64] = (jit_link_func_t) op_strcat_link;
    link_funcs[65] = (jit_link_func_t) op_hash_link;
    link_funcs[66] = (jit_link_func_t) op_lambda_link;
    link_funcs[67] = (jit_link_func_t) op_range_link;
    link_funcs[68] = (jit_link_func_t) op_oclass_link;
    link_funcs[69] = (jit_link_func_t) op_class_link;
    link_funcs[70] = (jit_link_func_t) op_module_link;
    link_funcs[71] = (jit_link_func_t) op_exec_link;
    link_funcs[72] = (jit_link_func_t) op_method_link;
    link_funcs[73] = (jit_link_func_t) op_sclass_link;
    link_funcs[74] = (jit_link_func_t) op_tclass_link;
    link_funcs[75] = (jit_link_func_t) op_debug_link;
    link_funcs[76] = (jit_link_func_t) op_stop_link;
    link_funcs[77] = (jit_link_func_t) op_err_link;
  }
}

