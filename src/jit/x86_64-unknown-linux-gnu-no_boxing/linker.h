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
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 14)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
}
static void op_move_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_move__rodata__link(text, rodata, c);
  op_move__text__link(text, rodata, c);
}
static void op_loadl__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadl__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 1)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 18)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
}
static void op_loadl_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadl__rodata__link(text, rodata, c);
  op_loadl__text__link(text, rodata, c);
}
static void op_loadi__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadi__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 26)) = (uint32_t)(((uintptr_t)GETARG_sBx(c)) + (0));
}
static void op_loadi_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_loadi__rodata__link(text, rodata, c);
  op_loadi__text__link(text, rodata, c);
}
static void op_loadsym__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_loadsym__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 30)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
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
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 30)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 38)) = (int32_t)(((uintptr_t)mrb_gv_get) + (-4) - ((uintptr_t)(text + 38)));
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
  *((int32_t *)(text + 42)) = (int32_t)(((uintptr_t)mrb_gv_set) + (-4) - ((uintptr_t)(text + 42)));
}
static void op_setglobal_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setglobal__rodata__link(text, rodata, c);
  op_setglobal__text__link(text, rodata, c);
}
static void op_getspecial__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getspecial__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 26)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 31)) = (int32_t)(((uintptr_t)mrb_vm_special_get) + (-4) - ((uintptr_t)(text + 31)));
}
static void op_getspecial_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getspecial__rodata__link(text, rodata, c);
  op_getspecial__text__link(text, rodata, c);
}
static void op_setspecial__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setspecial__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 30)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 35)) = (int32_t)(((uintptr_t)mrb_vm_special_set) + (-4) - ((uintptr_t)(text + 35)));
}
static void op_setspecial_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setspecial__rodata__link(text, rodata, c);
  op_setspecial__text__link(text, rodata, c);
}
static void op_getiv__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getiv__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 30)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 38)) = (int32_t)(((uintptr_t)mrb_vm_iv_get) + (-4) - ((uintptr_t)(text + 38)));
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
  *((int32_t *)(text + 42)) = (int32_t)(((uintptr_t)mrb_vm_iv_set) + (-4) - ((uintptr_t)(text + 42)));
}
static void op_setiv_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setiv__rodata__link(text, rodata, c);
  op_setiv__text__link(text, rodata, c);
}
static void op_getcv__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getcv__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 30)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 51)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 59)) = (int32_t)(((uintptr_t)mrb_vm_cv_get) + (-4) - ((uintptr_t)(text + 59)));
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
  *((int32_t *)(text + 42)) = (int32_t)(((uintptr_t)mrb_vm_cv_set) + (-4) - ((uintptr_t)(text + 42)));
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
  *((int32_t *)(text + 42)) = (int32_t)(((uintptr_t)mrb_vm_const_set) + (-4) - ((uintptr_t)(text + 42)));
}
static void op_setconst_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setconst__rodata__link(text, rodata, c);
  op_setconst__text__link(text, rodata, c);
}
static void op_getmcnst__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getmcnst__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 31)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 51)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 68)) = (int32_t)(((uintptr_t)mrb_const_get) + (-4) - ((uintptr_t)(text + 68)));
}
static void op_getmcnst_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getmcnst__rodata__link(text, rodata, c);
  op_getmcnst__text__link(text, rodata, c);
}
static void op_setmcnst__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setmcnst__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 12)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 36)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 68)) = (int32_t)(((uintptr_t)mrb_const_set) + (-4) - ((uintptr_t)(text + 68)));
}
static void op_setmcnst_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_setmcnst__rodata__link(text, rodata, c);
  op_setmcnst__text__link(text, rodata, c);
}
static void op_getupvar__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_getupvar__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 1)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 40)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 92)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
}
static void op_getupvar_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_getupvar__rodata__link(text, rodata, c);
  op_getupvar__text__link(text, rodata, c);
}
static void op_setupvar__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_setupvar__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 31)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 75)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 88)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 122)) = (int32_t)(((uintptr_t)mrb_write_barrier) + (-4) - ((uintptr_t)(text + 122)));
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
}
static void op_rescue__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 16)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
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
  *((int32_t *)(text + 30)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 30)));
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
  *((int32_t *)(text + 116)) = (int32_t)(((uintptr_t)mrb_gc_arena_restore) + (-4) - ((uintptr_t)(text + 116)));
  *((int32_t *)(text + 135)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 135)));
  *((int32_t *)(text + 267)) = (int32_t)(((uintptr_t)cipop) + (-4) - ((uintptr_t)(text + 267)));
*((uint32_t *)(text + 343)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 353)) = (int32_t)(((uintptr_t)_op_return) + (-4) - ((uintptr_t)(text + 353)));
  *((int32_t *)(text + 376)) = (int32_t)(((uintptr_t)stack_extend) + (-4) - ((uintptr_t)(text + 376)));
  *((int32_t *)(text + 441)) = (int32_t)(((uintptr_t)mrb_proc_call_jit) + (-4) - ((uintptr_t)(text + 441)));
}
static void op_call_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_call__rodata__link(text, rodata, c);
  op_call__text__link(text, rodata, c);
}
static void op_super__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_super__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  *((int32_t *)(text + 155)) = (int32_t)(((uintptr_t)_mrb_str_const_nomethod_error) + (-4) - ((uintptr_t)(text + 155)));
  *((int32_t *)(text + 163)) = (int32_t)(((uintptr_t)mrb_class_get) + (-4) - ((uintptr_t)(text + 163)));
*((uint32_t *)(text + 175)) = (uint32_t)(((uintptr_t)_str_const_super_outside_method) + (0));
  *((int32_t *)(text + 185)) = (int32_t)(((uintptr_t)mrb_str_new_static) + (-4) - ((uintptr_t)(text + 185)));
  *((int32_t *)(text + 201)) = (int32_t)(((uintptr_t)mrb_exc_new_str) + (-4) - ((uintptr_t)(text + 201)));
  *((int32_t *)(text + 226)) = (int32_t)(((uintptr_t)_mrb_method_search_vm) + (-4) - ((uintptr_t)(text + 226)));
*((uint32_t *)(text + 234)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 252)) = (uint32_t)(((uintptr_t)_str_const_method_missing) + (0));
  *((int32_t *)(text + 262)) = (int32_t)(((uintptr_t)mrb_intern_static) + (-4) - ((uintptr_t)(text + 262)));
  *((int32_t *)(text + 282)) = (int32_t)(((uintptr_t)mrb_method_search_vm) + (-4) - ((uintptr_t)(text + 282)));
*((uint32_t *)(text + 290)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 308)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 339)) = (int32_t)(((uintptr_t)mrb_ary_unshift) + (-4) - ((uintptr_t)(text + 339)));
*((uint32_t *)(text + 344)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 358)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 380)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 405)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 419)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 436)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 475)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 485)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 493)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 557)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 589)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 604)) = (int32_t)(((uintptr_t)cipush) + (-4) - ((uintptr_t)(text + 604)));
*((uint32_t *)(text + 675)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 777)) = (int32_t)(((uintptr_t)mrb_gc_arena_restore) + (-4) - ((uintptr_t)(text + 777)));
  *((int32_t *)(text + 796)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 796)));
*((uint32_t *)(text + 806)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 909)) = (int32_t)(((uintptr_t)cipop) + (-4) - ((uintptr_t)(text + 909)));
  *((int32_t *)(text + 922)) = (int32_t)(((uintptr_t)stack_extend) + (-4) - ((uintptr_t)(text + 922)));
  *((int32_t *)(text + 961)) = (int32_t)(((uintptr_t)mrb_proc_call_jit) + (-4) - ((uintptr_t)(text + 961)));
}
static void op_super_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_super__rodata__link(text, rodata, c);
  op_super__text__link(text, rodata, c);
}
static void op_argary__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_argary__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 18)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 58)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 122)) = (int32_t)(((uintptr_t)_mrb_str_const_nomethod_error) + (-4) - ((uintptr_t)(text + 122)));
  *((int32_t *)(text + 130)) = (int32_t)(((uintptr_t)mrb_class_get) + (-4) - ((uintptr_t)(text + 130)));
*((uint32_t *)(text + 143)) = (uint32_t)(((uintptr_t)_str_const_super_outside_method) + (0));
  *((int32_t *)(text + 153)) = (int32_t)(((uintptr_t)mrb_str_new_static) + (-4) - ((uintptr_t)(text + 153)));
  *((int32_t *)(text + 169)) = (int32_t)(((uintptr_t)mrb_exc_new_str) + (-4) - ((uintptr_t)(text + 169)));
  *((int32_t *)(text + 181)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 181)));
*((uint32_t *)(text + 202)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 213)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 235)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 240)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 262)) = (int32_t)(((uintptr_t)mrb_ary_new_from_values) + (-4) - ((uintptr_t)(text + 262)));
*((uint32_t *)(text + 281)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 311)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 343)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 375)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 381)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 411)) = (int32_t)(((uintptr_t)mrb_ary_new_capa) + (-4) - ((uintptr_t)(text + 411)));
*((uint32_t *)(text + 452)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 526)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 589)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 601)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 606)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 693)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 707)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 712)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 723)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
}
static void op_argary_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_argary__rodata__link(text, rodata, c);
  op_argary__text__link(text, rodata, c);
}
static void op_enter__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_enter__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 97)) = (uint32_t)(((uintptr_t)_str_const_proc) + (0));
*((uint32_t *)(text + 103)) = (uint32_t)(((uintptr_t)_str_const_to_proc) + (0));
  *((int32_t *)(text + 108)) = (int32_t)(((uintptr_t)mrb_convert_type) + (-4) - ((uintptr_t)(text + 108)));
  *((int32_t *)(text + 155)) = (int32_t)(((uintptr_t)mrb_gc_protect) + (-4) - ((uintptr_t)(text + 155)));
*((uint32_t *)(text + 200)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 205)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 216)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 231)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 276)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 281)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 300)) = (int32_t)(((uintptr_t)argnum_error) + (-4) - ((uintptr_t)(text + 300)));
  *((int32_t *)(text + 308)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 308)));
*((uint32_t *)(text + 324)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 329)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 340)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 353)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 402)) = (int32_t)(((uintptr_t)mrb_gc_protect) + (-4) - ((uintptr_t)(text + 402)));
*((uint32_t *)(text + 431)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 436)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 447)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 460)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 493)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 498)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 522)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 547)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 552)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 563)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 576)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 798)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 803)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 814)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 827)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 891)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 896)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 934)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 939)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 964)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 969)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1121)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1126)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1137)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1219)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1224)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1235)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1313)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1318)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1342)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1347)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1418)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1441)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1471)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1476)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1487)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 1557)) = (int32_t)(((uintptr_t)mrb_ary_new_from_values) + (-4) - ((uintptr_t)(text + 1557)));
*((uint32_t *)(text + 1577)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1582)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1619)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1639)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1644)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1655)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1710)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1740)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1757)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1762)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1773)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1786)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1916)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1927)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1932)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 1970)) = (int32_t)(((uintptr_t)mrb_ary_new_capa) + (-4) - ((uintptr_t)(text + 1970)));
*((uint32_t *)(text + 1984)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 1989)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2028)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2045)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2050)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2088)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2101)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2167)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2172)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2183)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2196)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2257)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 2270)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
}
static void op_enter_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_enter__rodata__link(text, rodata, c);
  op_enter__text__link(text, rodata, c);
}
static void op_enter_method_m__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_enter_method_m__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 102)) = (uint32_t)(((uintptr_t)_str_const_proc) + (0));
*((uint32_t *)(text + 108)) = (uint32_t)(((uintptr_t)_str_const_to_proc) + (0));
  *((int32_t *)(text + 113)) = (int32_t)(((uintptr_t)mrb_convert_type) + (-4) - ((uintptr_t)(text + 113)));
*((uint32_t *)(text + 134)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 156)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
  *((int32_t *)(text + 167)) = (int32_t)(((uintptr_t)argnum_error) + (-4) - ((uintptr_t)(text + 167)));
  *((int32_t *)(text + 175)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 175)));
  *((int32_t *)(text + 204)) = (int32_t)(((uintptr_t)mrb_gc_protect) + (-4) - ((uintptr_t)(text + 204)));
*((uint32_t *)(text + 221)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 288)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 311)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 328)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 398)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
*((uint32_t *)(text + 411)) = (uint32_t)(((uintptr_t)GETARG_Ax(c)) + (0));
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
*((uint64_t *)(rodata + 0)) = (uint64_t)(((uintptr_t)text) + (82));
*((uint64_t *)(rodata + 8)) = (uint64_t)(((uintptr_t)text) + (108));
*((uint64_t *)(rodata + 16)) = (uint64_t)(((uintptr_t)text) + (1001));
*((uint64_t *)(rodata + 24)) = (uint64_t)(((uintptr_t)text) + (1013));
*((uint64_t *)(rodata + 32)) = (uint64_t)(((uintptr_t)text) + (1025));
*((uint64_t *)(rodata + 40)) = (uint64_t)(((uintptr_t)text) + (108));
*((uint64_t *)(rodata + 48)) = (uint64_t)(((uintptr_t)text) + (1037));
*((uint64_t *)(rodata + 56)) = (uint64_t)(((uintptr_t)text) + (1046));
}
static void op_tailcall__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 30)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 38)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((int32_t *)(text + 78)) = (int32_t)(((uintptr_t)rodata) + (0));
  *((int32_t *)(text + 203)) = (int32_t)(((uintptr_t)_mrb_method_search_vm) + (-4) - ((uintptr_t)(text + 203)));
*((uint32_t *)(text + 211)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 233)) = (uint32_t)(((uintptr_t)_str_const_method_missing) + (0));
  *((int32_t *)(text + 246)) = (int32_t)(((uintptr_t)mrb_intern_static) + (-4) - ((uintptr_t)(text + 246)));
  *((int32_t *)(text + 265)) = (int32_t)(((uintptr_t)mrb_method_search_vm) + (-4) - ((uintptr_t)(text + 265)));
*((uint32_t *)(text + 277)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 291)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 325)) = (int32_t)(((uintptr_t)mrb_ary_unshift) + (-4) - ((uintptr_t)(text + 325)));
*((uint32_t *)(text + 330)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 340)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 362)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 391)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 405)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 422)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 479)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 489)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 497)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 557)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 583)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 636)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 661)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 700)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 857)) = (int32_t)(((uintptr_t)mrb_gc_arena_restore) + (-4) - ((uintptr_t)(text + 857)));
*((uint32_t *)(text + 862)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 872)) = (int32_t)(((uintptr_t)_op_return) + (-4) - ((uintptr_t)(text + 872)));
  *((int32_t *)(text + 939)) = (int32_t)(((uintptr_t)stack_extend) + (-4) - ((uintptr_t)(text + 939)));
  *((int32_t *)(text + 978)) = (int32_t)(((uintptr_t)mrb_proc_call_jit) + (-4) - ((uintptr_t)(text + 978)));
}
static void op_tailcall_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_tailcall__rodata__link(text, rodata, c);
  op_tailcall__text__link(text, rodata, c);
}
static void op_blkpush__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_blkpush__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 7)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 45)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 109)) = (int32_t)(((uintptr_t)localjump_error) + (-4) - ((uintptr_t)(text + 109)));
  *((int32_t *)(text + 117)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 117)));
*((uint32_t *)(text + 138)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 150)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 155)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
*((uint32_t *)(text + 166)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
}
static void op_blkpush_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_blkpush__rodata__link(text, rodata, c);
  op_blkpush__text__link(text, rodata, c);
}
static void op_add__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_add__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 21)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 103)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 175)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 208)) = (int32_t)(((uintptr_t)mrb_str_plus) + (-4) - ((uintptr_t)(text + 208)));
*((uint32_t *)(text + 233)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 282)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 336)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 378)) = (int32_t)(((uintptr_t)op_send) + (-4) - ((uintptr_t)(text + 378)));
*((uint32_t *)(text + 393)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_add_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_add__rodata__link(text, rodata, c);
  op_add__text__link(text, rodata, c);
}
static void op_addi__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_addi__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 34)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 65)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 84)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 103)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 126)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 140)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 154)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 154)));
}
static void op_addi_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_addi__rodata__link(text, rodata, c);
  op_addi__text__link(text, rodata, c);
}
static void op_sub__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_sub__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 78)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 137)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 167)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 220)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 276)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 320)) = (int32_t)(((uintptr_t)op_send) + (-4) - ((uintptr_t)(text + 320)));
}
static void op_sub_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_sub__rodata__link(text, rodata, c);
  op_sub__text__link(text, rodata, c);
}
static void op_subi__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_subi__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 52)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 73)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 92)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 115)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 129)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 143)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 143)));
}
static void op_subi_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_subi__rodata__link(text, rodata, c);
  op_subi__text__link(text, rodata, c);
}
static void op_mul__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_mul__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 88)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 118)) = (int32_t)(((uintptr_t)mrb_fixnum_mul) + (-4) - ((uintptr_t)(text + 118)));
*((uint32_t *)(text + 139)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 168)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 218)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 272)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 314)) = (int32_t)(((uintptr_t)op_send) + (-4) - ((uintptr_t)(text + 314)));
*((uint32_t *)(text + 333)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_mul_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_mul__rodata__link(text, rodata, c);
  op_mul__text__link(text, rodata, c);
}
static void op_div__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_div__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 74)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 136)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 189)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 244)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 288)) = (int32_t)(((uintptr_t)op_send) + (-4) - ((uintptr_t)(text + 288)));
}
static void op_div_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_div__rodata__link(text, rodata, c);
  op_div__text__link(text, rodata, c);
}
static void op_eq__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_eq__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 19)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 55)) = (int32_t)(((uintptr_t)mrb_obj_eq) + (-4) - ((uintptr_t)(text + 55)));
*((uint32_t *)(text + 99)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 134)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 179)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 223)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 252)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 315)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 341)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 346)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 352)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 360)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 360)));
}
static void op_eq_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_eq__rodata__link(text, rodata, c);
  op_eq__text__link(text, rodata, c);
}
static void op_lt__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_lt__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 69)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 115)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 158)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 209)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 272)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 298)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 303)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 309)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 317)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 317)));
}
static void op_lt_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_lt__rodata__link(text, rodata, c);
  op_lt__text__link(text, rodata, c);
}
static void op_le__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_le__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 69)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 115)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 158)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 209)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 272)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 298)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 303)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 309)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 317)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 317)));
}
static void op_le_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_le__rodata__link(text, rodata, c);
  op_le__text__link(text, rodata, c);
}
static void op_gt__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_gt__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 69)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 112)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 163)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 193)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 256)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 282)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 287)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 293)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 301)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 301)));
}
static void op_gt_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_gt__rodata__link(text, rodata, c);
  op_gt__text__link(text, rodata, c);
}
static void op_ge__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_ge__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 9)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 69)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 112)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 163)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 193)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 256)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 282)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 287)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 293)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 301)) = (int32_t)(((uintptr_t)_op_send) + (-4) - ((uintptr_t)(text + 301)));
}
static void op_ge_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_ge__rodata__link(text, rodata, c);
  op_ge__text__link(text, rodata, c);
}
static void op_array__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_array__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 27)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 39)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 44)) = (int32_t)(((uintptr_t)mrb_ary_new_from_values) + (-4) - ((uintptr_t)(text + 44)));
}
static void op_array_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_array__rodata__link(text, rodata, c);
  op_array__text__link(text, rodata, c);
}
static void op_arycat__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_arycat__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 19)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 28)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 49)) = (int32_t)(((uintptr_t)mrb_ary_splat) + (-4) - ((uintptr_t)(text + 49)));
  *((int32_t *)(text + 72)) = (int32_t)(((uintptr_t)mrb_ary_concat) + (-4) - ((uintptr_t)(text + 72)));
}
static void op_arycat_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_arycat__rodata__link(text, rodata, c);
  op_arycat__text__link(text, rodata, c);
}
static void op_arypush__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_arypush__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 24)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 50)) = (int32_t)(((uintptr_t)mrb_ary_push) + (-4) - ((uintptr_t)(text + 50)));
}
static void op_arypush_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_arypush__rodata__link(text, rodata, c);
  op_arypush__text__link(text, rodata, c);
}
static void op_aref__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_aref__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 36)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 54)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 59)) = (int32_t)(((uintptr_t)mrb_ary_ref) + (-4) - ((uintptr_t)(text + 59)));
*((uint32_t *)(text + 75)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 88)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 111)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_aref_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_aref__rodata__link(text, rodata, c);
  op_aref__text__link(text, rodata, c);
}
static void op_aset__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_aset__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 24)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 50)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 55)) = (int32_t)(((uintptr_t)mrb_ary_set) + (-4) - ((uintptr_t)(text + 55)));
}
static void op_aset_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_aset__rodata__link(text, rodata, c);
  op_aset__text__link(text, rodata, c);
}
static void op_apost__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_apost__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 18)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 58)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 63)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 82)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 87)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 109)) = (int32_t)(((uintptr_t)mrb_ary_new_from_values) + (-4) - ((uintptr_t)(text + 109)));
*((uint32_t *)(text + 122)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 216)) = (int32_t)(((uintptr_t)mrb_ary_new_capa) + (-4) - ((uintptr_t)(text + 216)));
*((uint32_t *)(text + 229)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 234)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 298)) = (int32_t)(((uintptr_t)mrb_ary_new_capa) + (-4) - ((uintptr_t)(text + 298)));
*((uint32_t *)(text + 311)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 323)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 338)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 393)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 402)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 412)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_apost_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_apost__rodata__link(text, rodata, c);
  op_apost__text__link(text, rodata, c);
}
static void op_string__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_string__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 31)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 48)) = (int32_t)(((uintptr_t)mrb_str_dup) + (-4) - ((uintptr_t)(text + 48)));
}
static void op_string_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_string__rodata__link(text, rodata, c);
  op_string__text__link(text, rodata, c);
}
static void op_strcat__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_strcat__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 15)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 24)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 50)) = (int32_t)(((uintptr_t)mrb_str_concat) + (-4) - ((uintptr_t)(text + 50)));
}
static void op_strcat_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_strcat__rodata__link(text, rodata, c);
  op_strcat__text__link(text, rodata, c);
}
static void op_hash__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_hash__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 23)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 28)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
  *((int32_t *)(text + 33)) = (int32_t)(((uintptr_t)mrb_hash_new_capa) + (-4) - ((uintptr_t)(text + 33)));
*((uint32_t *)(text + 44)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 81)) = (uint32_t)(((uintptr_t)GETARG_C(c)) + (0));
*((uint32_t *)(text + 86)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 156)) = (int32_t)(((uintptr_t)mrb_hash_set) + (-4) - ((uintptr_t)(text + 156)));
*((uint32_t *)(text + 185)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
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
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 27)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 72)) = (int32_t)(((uintptr_t)mrb_range_new) + (-4) - ((uintptr_t)(text + 72)));
}
static void op_range_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_range__rodata__link(text, rodata, c);
  op_range__text__link(text, rodata, c);
}
static void op_oclass__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_oclass__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 1)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_oclass_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_oclass__rodata__link(text, rodata, c);
  op_oclass__text__link(text, rodata, c);
}
static void op_class__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_class__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 22)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 122)) = (int32_t)(((uintptr_t)mrb_vm_define_class) + (-4) - ((uintptr_t)(text + 122)));
*((uint32_t *)(text + 131)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_class_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_class__rodata__link(text, rodata, c);
  op_class__text__link(text, rodata, c);
}
static void op_module__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_module__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 13)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
*((uint32_t *)(text + 21)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 97)) = (int32_t)(((uintptr_t)mrb_vm_define_module) + (-4) - ((uintptr_t)(text + 97)));
*((uint32_t *)(text + 106)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
}
static void op_module_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_module__rodata__link(text, rodata, c);
  op_module__text__link(text, rodata, c);
}
static void op_exec__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_exec__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 14)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 46)) = (int32_t)(((uintptr_t)cipush) + (-4) - ((uintptr_t)(text + 46)));
*((uint32_t *)(text + 127)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 136)) = (int32_t)(((uintptr_t)mrb_proc_new) + (-4) - ((uintptr_t)(text + 136)));
  *((int32_t *)(text + 204)) = (int32_t)(((uintptr_t)mrb_gc_arena_restore) + (-4) - ((uintptr_t)(text + 204)));
  *((int32_t *)(text + 223)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 223)));
  *((int32_t *)(text + 267)) = (int32_t)(((uintptr_t)stack_extend) + (-4) - ((uintptr_t)(text + 267)));
  *((int32_t *)(text + 317)) = (int32_t)(((uintptr_t)mrb_proc_call_jit) + (-4) - ((uintptr_t)(text + 317)));
  *((int32_t *)(text + 348)) = (int32_t)(((uintptr_t)cipop) + (-4) - ((uintptr_t)(text + 348)));
}
static void op_exec_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_exec__rodata__link(text, rodata, c);
  op_exec__text__link(text, rodata, c);
}
static void op_method__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_method__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 5)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 35)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 60)) = (int32_t)(((uintptr_t)mrb_define_method_vm) + (-4) - ((uintptr_t)(text + 60)));
}
static void op_method_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_method__rodata__link(text, rodata, c);
  op_method__text__link(text, rodata, c);
}
static void op_sclass__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_sclass__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 10)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
*((uint32_t *)(text + 27)) = (uint32_t)(((uintptr_t)GETARG_B(c)) + (0));
  *((int32_t *)(text + 44)) = (int32_t)(((uintptr_t)mrb_singleton_class) + (-4) - ((uintptr_t)(text + 44)));
}
static void op_sclass_link(uint8_t *text, uint8_t *rodata, mrb_code c) {
  op_sclass__rodata__link(text, rodata, c);
  op_sclass__text__link(text, rodata, c);
}
static void op_tclass__rodata__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
}
static void op_tclass__text__link(uint8_t *text, uint8_t *rodata, mrb_code c) {
*((uint32_t *)(text + 34)) = (uint32_t)(((uintptr_t)GETARG_A(c)) + (0));
  *((int32_t *)(text + 58)) = (int32_t)(((uintptr_t)_mrb_str_const_type_error) + (-4) - ((uintptr_t)(text + 58)));
  *((int32_t *)(text + 66)) = (int32_t)(((uintptr_t)mrb_class_get) + (-4) - ((uintptr_t)(text + 66)));
*((uint32_t *)(text + 74)) = (uint32_t)(((uintptr_t)_str_const_no_target_class) + (0));
  *((int32_t *)(text + 87)) = (int32_t)(((uintptr_t)mrb_str_new_static) + (-4) - ((uintptr_t)(text + 87)));
  *((int32_t *)(text + 103)) = (int32_t)(((uintptr_t)mrb_exc_new_str) + (-4) - ((uintptr_t)(text + 103)));
  *((int32_t *)(text + 119)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 119)));
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
*((uint32_t *)(text + 18)) = (uint32_t)(((uintptr_t)GETARG_Bx(c)) + (0));
  *((int32_t *)(text + 38)) = (int32_t)(((uintptr_t)mrb_str_dup) + (-4) - ((uintptr_t)(text + 38)));
  *((int32_t *)(text + 50)) = (int32_t)(((uintptr_t)_mrb_str_const_localjump_error) + (-4) - ((uintptr_t)(text + 50)));
  *((int32_t *)(text + 58)) = (int32_t)(((uintptr_t)mrb_class_get) + (-4) - ((uintptr_t)(text + 58)));
  *((int32_t *)(text + 74)) = (int32_t)(((uintptr_t)mrb_exc_new_str) + (-4) - ((uintptr_t)(text + 74)));
  *((int32_t *)(text + 90)) = (int32_t)(((uintptr_t)_op_raise) + (-4) - ((uintptr_t)(text + 90)));
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

