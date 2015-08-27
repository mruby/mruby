#define MRB_DISABLE_LITERAL_INTERN

#include <mruby.h>
#include <mruby/khash.h>
#include <mruby/array.h>

#include <stdarg.h>

MRB_API struct RClass*
mrb_define_module(mrb_state *mrb, const char *name)
{
  return mrb_define_module_id(mrb, mrb_intern_cstr(mrb, name));
}

MRB_API struct RClass*
mrb_define_module_under(mrb_state *mrb, struct RClass *outer, const char *name)
{
  return mrb_define_module_under_id(mrb, outer, mrb_intern_cstr(mrb, name));
}

MRB_API struct RClass*
mrb_define_class(mrb_state *mrb, const char *name, struct RClass *super)
{
  return mrb_define_class_id(mrb, mrb_intern_cstr(mrb, name), super);
}

MRB_API mrb_bool
mrb_class_defined(mrb_state *mrb, const char *name)
{
  return mrb_class_defined_id(mrb, mrb_intern_cstr(mrb, name));
}

MRB_API struct RClass *
mrb_class_get_under(mrb_state *mrb, struct RClass *outer, const char *name)
{
  return mrb_class_get_under_id(mrb, outer, mrb_intern_cstr(mrb, name));
}

MRB_API struct RClass *
mrb_class_get(mrb_state *mrb, const char *name)
{
  return mrb_class_get_id(mrb, mrb_intern_cstr(mrb, name));
}

MRB_API struct RClass *
mrb_module_get_under(mrb_state *mrb, struct RClass *outer, const char *name)
{
  return mrb_module_get_under_id(mrb, outer, mrb_intern_cstr(mrb, name));
}

MRB_API struct RClass *
mrb_module_get(mrb_state *mrb, const char *name)
{
  return mrb_module_get_id(mrb, mrb_intern_cstr(mrb, name));
}

MRB_API struct RClass *
mrb_define_classunder(mrb_state *mrb, struct RClass *outer, const char *name, struct RClass *super)
{
  return mrb_define_class_under_id(mrb, outer, mrb_intern_cstr(mrb, name), super);
}

MRB_API void
mrb_define_method(mrb_state *mrb, struct RClass *c, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_method_id(mrb, c, mrb_intern_cstr(mrb, name), func, aspec);
}

MRB_API void
mrb_define_singleton_method(mrb_state *mrb, struct RObject *o, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_singleton_method_id(mrb, o, mrb_intern_cstr(mrb, name), func, aspec);
}

MRB_API void
mrb_define_class_method(mrb_state *mrb, struct RClass *c, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_class_method_id(mrb, c, mrb_intern_cstr(mrb, name), func, aspec);
}

MRB_API void
mrb_define_module_function(mrb_state *mrb, struct RClass *c, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_module_function_id(mrb, c, mrb_intern_cstr(mrb, name), func, aspec);
}

MRB_API void
mrb_undef_method(mrb_state *mrb, struct RClass *c, const char *name)
{
  mrb_undef_method_id(mrb, c, mrb_intern_cstr(mrb, name));
}

MRB_API void
mrb_undef_class_method(mrb_state *mrb, struct RClass *c, const char *name)
{
  mrb_undef_class_method_id(mrb, c, mrb_intern_cstr(mrb, name));
}

MRB_API void
mrb_define_const(mrb_state *mrb, struct RClass *mod, const char *name, mrb_value v)
{
  mrb_define_const_id(mrb, mod, mrb_intern_cstr(mrb, name), v);
}

MRB_API void
mrb_define_global_const(mrb_state *mrb, const char *name, mrb_value val)
{
  mrb_define_global_const_id(mrb, mrb_intern_cstr(mrb, name), val);
}

MRB_API mrb_value
mrb_funcall(mrb_state *mrb, mrb_value self, const char *name, mrb_int argc, ...)
{
  mrb_value argv[MRB_FUNCALL_ARGC_MAX];
  va_list ap;
  mrb_int i;
  mrb_sym mid = mrb_intern_cstr(mrb, name);

  if (argc > MRB_FUNCALL_ARGC_MAX) {
    mrb_raise(mrb, mrb_exc_get_id(mrb, mrb_intern_lit(mrb, "ArgumentError")), "Too long arguments. (limit=" MRB_STRINGIZE(MRB_FUNCALL_ARGC_MAX) ")");
  }

  va_start(ap, argc);
  for (i = 0; i < argc; i++) {
    argv[i] = va_arg(ap, mrb_value);
  }
  va_end(ap);
  return mrb_funcall_argv(mrb, self, mid, argc, argv);
}

typedef struct symbol_name {
  size_t len;
  const char *name;
} symbol_name;

/*
 *  call-seq:
 *     Symbol.all_symbols    => array
 *
 *  Returns an array of all the symbols currently in Ruby's symbol
 *  table.
 *
 *     Symbol.all_symbols.size    #=> 903
 *     Symbol.all_symbols[1,20]   #=> [:floor, :ARGV, :Binding, :symlink,
 *                                     :chown, :EOFError, :$;, :String,
 *                                     :LOCK_SH, :"setuid?", :$<,
 *                                     :default_proc, :compact, :extend,
 *                                     :Tms, :getwd, :$=, :ThreadGroup,
 *                                     :wait2, :$>]
 */
static mrb_value
mrb_sym_all_symbols(mrb_state *mrb, mrb_value self)
{
  mrb_sym i, lim;
  mrb_value ary = mrb_ary_new_capa(mrb, mrb->symidx);

  for (i=1, lim=mrb->symidx+1; i<lim; i++) {
    mrb_ary_push(mrb, ary, mrb_symbol_value(i));
  }

  return ary;
}

/*
 * call-seq:
 *   sym.length    -> integer
 *
 * Same as <code>sym.to_s.length</code>.
 */
static mrb_value
mrb_sym_length(mrb_state *mrb, mrb_value self)
{
  mrb_int len;
  mrb_sym2name_len(mrb, mrb_symbol(self), &len);
  return mrb_fixnum_value(len);
}

void
mrb_mruby_symbol_ext_gem_init(mrb_state* mrb)
{
  struct RClass *s = mrb->symbol_class;
  mrb_define_class_method_id(mrb, s, mrb_intern_lit(mrb, "all_symbols"), mrb_sym_all_symbols, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, s, mrb_intern_lit(mrb, "length"), mrb_sym_length, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, s, mrb_intern_lit(mrb, "size"), mrb_sym_length, MRB_ARGS_NONE());
}

void
mrb_mruby_symbol_ext_gem_final(mrb_state* mrb)
{
}
