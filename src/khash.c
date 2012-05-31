#include "mruby/khash.h"
#include "stdio.h"

KHASH_DEF(mt,  mrb_sym,   struct RProc*, 1, kh_mrbsym_hash_func, kh_mrbsym_hash_equal);
KHASH_DEF(iv,  mrb_sym,       mrb_value, 1, kh_mrbsym_hash_func, kh_mrbsym_hash_equal);
KHASH_DEF(s2n, mrb_sym,       kh_cstr_t, 1, kh_mrbsym_hash_func, kh_mrbsym_hash_equal);
KHASH_DEF(n2s, kh_cstr_t,       mrb_sym, 1, kh_str_hash_func,    kh_str_hash_equal);

