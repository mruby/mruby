# API Changes

Created from `git log --date=iso --format=fuller -U 1.0.0...mruby/master include/`.

## From 1.0.0

2014/07/14
Return type of `mrb_const_defined_at` is changed to `mrb_bool`.

2014/07/12
`SystemStackError` is defined to raise stack error with it instead.
Macro `E_SYSSTACK_ERROR` is defined too.

2014/07/09
Compatibility macro `MRB_TT_VOIDP`, `mrb_voidp_value`, `mrb_voidp`, `mrb_voidp_p` is removed.
Some of the header is split to **mruby/boxing_*.h**

2014/07/06
API `mrb_ary_resize` is added.

2014/06/23
API `mrb_open_core`, `mrb_default_allocf` is added.

2014/06/21
`mrb_context_run` clears local variables on first run.

2014/06/16
mrbconf `MRB_FIXED_STATE_ATEXIT_STACK` and `MRB_FIXED_STATE_ATEXIT_STACK_SIZE` is added.

2014/06/10
API `mrb_state_atexit` is added.

2014/06/09
Macro `mrb_static_assert` is added.

2014/06/04
Field `nomem_err` is added to `mrb_state`.
Now when memory ran out mruby will raise pre-allocated exception.

2014/06/02
API `mrb_no_method_error` is added.

2014/05/30
Make `mrb_toplevel_run_keep` public.

2014/05/20
`mrb_format` is moved to **mruby.h**

2014/05/19
Macro `RITE_LV_NULL_MARK` is added.

2014/05/14
Macro `RITE_SECTION_LV_IDENTIFIER` and `struct rite_section_lv_header` is added.
Now mruby supports local variables information dumping.
**src/opcode.h** is moved to **include/mruby/opcode.h**.

2014/05/05
API `mrb_int_add_overflow`, `mrb_int_sub_overflow` is added.

2014/05/03
API `mrb_get_values_at` is added.
Macro `mrb_int` is added.

2014/05/02
API `mrb_dump_irep` is public.

2014/04/29
`mrb_str_offset` is removed.

2014/04/28
`mrb_str_buf_append` is renamed to `mrb_str_cat_str`.
`mrb_str_buf_append` is now a inline function instead.
`struct mrb_locals` is added.

2014/04/25
2nd argument type of 's' and '*' format specifier in `mrb_get_args` is not `mrb_int`.
Some type in declaration of `mrb_obj_new`, `mrb_get_args`, `mrb_funcall`, `mrb_funcall_argv`,
`mrb_funcall_with_block`, `mrb_yield_argv`, `mrb_yield_with_class`, `mrb_fiber_yield`, `mrb_str_to_inum`
is now `mrb_int`.
Macro `mrb_assert_int_fit` is added.
4th argument of `mrb_range_new` is not `mrb_bool`.

2014/04/21
`mrb_str_buf_cat` is now inline function.

2014/04/17
API `mrb_regexp_p` is added.

2014/04/16
**src/re.h** is moved to **include/mruby/re.h**.
Macro `mrb_noreturn` is added.

2014/04/13
Macro `kh_put2` is added.

2014/04/11
Some argument of `mrb_obj_new`, `mrb_funcall_argv`, `mrb_funcall_with_block`,
`mrb_make_exception`, `mrb_fiber_yield` is const qualified.

2014/04/10
`argv` argument of `mrb_yield_with_class` and `mrb_yield_argv` is const qualified.

2014/04/09
Macro `MRB_TT_HAS_BASIC_P` is added.

2014/04/01
Macro `MRB_ENV_STACK_LEN`, `MRB_ENV_UNSHARE_STACK`, `MRB_ENV_STACK_SHARED_P` is added.

2014/03/31
API `mrb_proc_new_cfunc_with_env` and `mrb_cfunc_env_get` is added.

2014/03/28
`mrb_get_args` won't use `to_f` method.
API `mrb_to_flo` is added.

2014/03/26
API `mrb_toplevel_run` is added.

2014/03/25
`mrb_class` now returns `NULL` if `v` is `REnv`.

2014/03/19
`mrb_yield_internal` is renamed to `mrb_yield_with_class`.

2014/03/15
Symbol and string length's type is now `mrb_int`.
3rd argument of `mrb_sym2name_len` is now `mrb_int*`.

2014/03/13
Macro `mrb_strlen_lit` is added to make string literal passing safer check.
**mruby.h** now includes **limits.h**.
Macro `MRB_INT_BIT` is added.

2014/03/09
`INFINITY` and `NAN` macro is defined for MSVC.

2014/03/06
Embed string support is added.

2014/03/05
**mruby/error.h** is able to use in C++ without `extern "C"`.

2014/03/04
Macro `mrb_exc_new_str_lit` is added.

2014/03/01
API `mrb_fiber_yield` is added.
Declaration of `mrb_f_send` is added to **mruby/proc.h**.

2014/02/27
API `mrb_get_backtrace`, `mrb_print_backtrace`, `mrb_exc_backtrace` is added.

2014/02/26
API `mrb_str_cat_lit` is added.

2014/02/24
`STRINGIZE` macro is renamed to `MRB_STRINGIZE`.

2014/02/20
**src/error.h** is moved to **include/mruby/error.h**.
**src/error.h** is now a alias to **include/mruby/error.h**.

2014/02/18
Declaration of `mrb_flo_to_str` is removed.

2014/02/14
First 'v' of `MRUBY_VERSION` is removed.

2014/02/12
Declaration of `mrb_str_literal` is removed.
**mruby/version.h** is included in **mruby.h**

2014/02/12
API `mrb_ary_aget` is now private.
API `mrb_ary_len` is now a inline function.

2014/02/11
`MRUBY_RUBY_ENGINE` macro is added.

2014/02/08
API `mrb_module_get` and `mrb_module_get_under` is added.
To get modules use these APIs instead of `mrb_class_get` and `mrb_class_get_under`.

2014/02/07
Header **mruby/version.h** is added.

2014/02/06
Macro `mrb_str_new_lit` is added.

2014/01/31
Some type of `mrb_range_beg_len`, `mrb_str_to_inum`, `mrb_str_to_dbl` is replaced to `mrb_bool`.
API `objspace_each_objects` and function pointer type `each_object_callback`
now have `mrb_` prefix.
**mruby/gc.h** is now able to use in C++ without `extern "C"`.

2014/01/30
Removed `mrb_str_format` since it's part of mruby-sprintf gem.
Removed `mrb_block_proc` since it only returned `nil`.

2014/01/23
`debug_op_hook` field in `mrb_state` is added.
