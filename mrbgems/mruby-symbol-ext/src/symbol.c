#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/internal.h>

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
#ifdef MRB_USE_ALL_SYMBOLS
static mrb_value
mrb_sym_all_symbols(mrb_state *mrb, mrb_value self)
{
  mrb_value ary = mrb_ary_new_capa(mrb, mrb->symidx);

  for (mrb_sym i=1; i<=MRB_PRESYM_MAX; i++) {
    mrb_ary_push(mrb, ary, mrb_symbol_value(i));
  }
  mrb_sym lim = mrb->symidx + 1;
  for (mrb_sym i=1; i<lim; i++) {
    mrb_ary_push(mrb, ary, mrb_symbol_value(i+MRB_PRESYM_MAX));
  }

  return ary;
}
#endif

/*
 * call-seq:
 *   sym.length    -> integer
 *
 * Same as `sym.to_s.length`.
 */
static mrb_value
mrb_sym_length(mrb_state *mrb, mrb_value self)
{
  mrb_int len;
#ifdef MRB_UTF8_STRING
  mrb_int byte_len;
  const char *name = mrb_sym_name_len(mrb, mrb_symbol(self), &byte_len);
  len = mrb_utf8_strlen(name, byte_len);
#else
  mrb_sym_name_len(mrb, mrb_symbol(self), &len);
#endif
  return mrb_fixnum_value(len);
}

/*
 * call-seq:
 *   sym.slice(index)          -> string or nil
 *   sym.slice(start, length)  -> string or nil
 *   sym.slice(range)          -> string or nil
 *   sym[index]                -> string or nil
 *
 * Slices the symbol's name the way `String#slice` slices a string, and
 * answers a String rather than a Symbol. The name comes from the symbol
 * itself rather than through `to_s`, which is where CRuby's sym_aref() takes
 * it from as well, so redefining `Symbol#to_s` does not move these two.
 */
static mrb_value
mrb_sym_slice(mrb_state *mrb, mrb_value self)
{
  mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  /* The name is handed to `String#slice` rather than to the core function
     that answers it, because mruby-regexp registers a `String#slice` of its
     own to answer a Regexp index and this gem does not depend on it. Sending
     is what keeps `sym[re]` answering wherever that gem is built in, which is
     what the Ruby definition this replaces did, and it leaves the argument
     checking in the one place that already spells it out. What comes back is
     the answer, so nothing is read from it here and nothing is held across
     the call. */
  return mrb_funcall_argv(mrb, mrb_sym_str(mrb, mrb_symbol(self)),
                          MRB_SYM(slice), argc, argv);
}

static const mrb_mt_entry symbol_ext_rom_entries[] = {
  MRB_MT_ENTRY(mrb_sym_length, MRB_SYM(length), MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mrb_sym_length, MRB_SYM(size), MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mrb_sym_slice,  MRB_SYM(slice), MRB_ARGS_ANY()),
  MRB_MT_ENTRY(mrb_sym_slice,  MRB_OPSYM(aref), MRB_ARGS_ANY()),
};

void
mrb_mruby_symbol_ext_gem_init(mrb_state* mrb)
{
  struct RClass *s = mrb->symbol_class;
#ifdef MRB_USE_ALL_SYMBOLS
  mrb_define_class_method_id(mrb, s, MRB_SYM(all_symbols), mrb_sym_all_symbols, MRB_ARGS_NONE());
#endif
  MRB_MT_INIT_ROM(mrb, s, symbol_ext_rom_entries);
}

void
mrb_mruby_symbol_ext_gem_final(mrb_state* mrb)
{
}
