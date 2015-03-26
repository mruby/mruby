#define SYMTBL_SIZE 82
static void* symtbl[SYMTBL_SIZE];
void init_symtbl() {
  static int init = 0;
  if(init == 0) {
    init = 1;
    symtbl[0] = (void *) _mrb_str_const_nomethod_error;
    symtbl[1] = (void *) mrb_closure_new;
    symtbl[2] = (void *) mrb_str_new_static;
    symtbl[3] = (void *) _mrb_str_const_runtime_error;
    symtbl[4] = (void *) _op_return;
    symtbl[5] = (void *) mrb_vm_const_get;
    symtbl[6] = (void *) mrb_gv_set;
    symtbl[7] = (void *) mrb_obj_eq;
    symtbl[8] = (void *) value_move;
    symtbl[9] = (void *) mrb_vm_define_module;
    symtbl[10] = (void *) mrb_nil_value;
    symtbl[11] = (void *) mrb_vm_iv_get;
    symtbl[12] = (void *) mrb_singleton_class;
    symtbl[13] = (void *) mrb_method_search_vm;
    symtbl[14] = (void *) mrb_ary_new_capa;
    symtbl[15] = (void *) mrb_vm_cv_get;
    symtbl[16] = (void *) cipush;
    symtbl[17] = (void *) mrb_define_method_vm;
    symtbl[18] = (void *) mrb_ary_splat;
    symtbl[19] = (void *) mrb_write_barrier;
    symtbl[20] = (void *) mrb_const_set;
    symtbl[21] = (void *) mrb_gc_arena_restore;
    symtbl[22] = (void *) mrb_class_get;
    symtbl[23] = (void *) mrb_convert_type;
    symtbl[24] = (void *) mrb_realloc;
    symtbl[25] = (void *) mrb_vm_special_set;
    symtbl[26] = (void *) mrb_str_plus;
    symtbl[27] = (void *) stack_extend;
    symtbl[28] = (void *) mrb_proc_new;
    symtbl[29] = (void *) mrb_symbol_value;
    symtbl[30] = (void *) mrb_obj_iv_get;
    symtbl[31] = (void *) cipop;
    symtbl[32] = (void *) mrb_hash_new_capa;
    symtbl[33] = (void *) mrb_int_sub_overflow;
    symtbl[34] = (void *) printf;
    symtbl[35] = (void *) mrb_range_new;
    symtbl[36] = (void *) mrb_vm_define_class;
    symtbl[37] = (void *) mrb_int_add_overflow;
    symtbl[38] = (void *) longjmp;
    symtbl[39] = (void *) mrb_gv_get;
    symtbl[40] = (void *) mrb_ary_concat;
    symtbl[41] = (void *) _mrb_str_const_type_error;
    symtbl[42] = (void *) mrb_hash_set;
    symtbl[43] = (void *) localjump_error;
    symtbl[44] = (void *) _op_raise;
    symtbl[45] = (void *) mrb_vm_const_set;
    symtbl[46] = (void *) mrb_exc_new_str;
    symtbl[47] = (void *) mrb_str_dup;
    symtbl[48] = (void *) _op_stop;
    symtbl[49] = (void *) _op_send;
    symtbl[50] = (void *) mrb_gc_protect;
    symtbl[51] = (void *) mrb_vm_iv_set;
    symtbl[52] = (void *) _mrb_str_const_fiber_error;
    symtbl[53] = (void *) mrb_str_concat;
    symtbl[54] = (void *) ecall;
    symtbl[55] = (void *) mrb_ary_ref;
    symtbl[56] = (void *) mrb_const_get;
    symtbl[57] = (void *) mrb_vm_cv_set;
    symtbl[58] = (void *) mrb_ary_unshift;
    symtbl[59] = (void *) mrb_obj_value;
    symtbl[60] = (void *) mrb_ary_new_from_values;
    symtbl[61] = (void *) mrb_vm_special_get;
    symtbl[62] = (void *) mrb_ary_set;
    symtbl[63] = (void *) op_send;
    symtbl[64] = (void *) mrb_ary_push;
    symtbl[65] = (void *) mrb_fixnum_mul;
    symtbl[66] = (void *) _mrb_str_const_localjump_error;
    symtbl[67] = (void *) stack_copy;
    symtbl[68] = (void *) argnum_error;
    symtbl[69] = (void *) uvenv;
    symtbl[70] = (void *) mrb_class;
    symtbl[71] = (void *) mrb_intern_static;
    symtbl[72] = (void *) top_env;
    symtbl[73] = (void *) _str_const_method_missing;
    symtbl[74] = (void *) _str_const_loadi;
    symtbl[75] = (void *) _str_const_super_outside_method;
    symtbl[76] = (void *) _str_const_proc;
    symtbl[77] = (void *) _str_const_to_proc;
    symtbl[78] = (void *) _str_const_double_resume;
    symtbl[79] = (void *) _str_const_attached;
    symtbl[80] = (void *) _str_const_no_target_class;
    symtbl[81] = (void *) _str_const_op_debug_format;
  }
}
