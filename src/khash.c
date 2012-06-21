#include "mruby/khash.h"

KHASH_DEFINE(mt, mrb_sym,   struct RProc*, 1, kh_int_hash_func, kh_int_hash_equal)
KHASH_DEFINE(iv, mrb_sym,   mrb_value,     1, kh_int_hash_func, kh_int_hash_equal)

