#include <mruby.h>
#include <mruby/error.h>
#include <mruby/array.h>
#include <mruby/hash.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/numeric.h>
#include <mruby/proc.h>
#include <mruby/class.h>
#include <mruby/variable.h>
#include <mruby/internal.h>

/* The entries `caller` and `caller_locations` answer with, for the
   arguments they are given: FALSE when the answer is nil, else `*levp`
   entries are skipped and `*np` taken after the first of `bt_len`, which is
   the method itself. */
static mrb_bool
caller_range(mrb_state *mrb, mrb_int bt_len, mrb_int *levp, mrb_int *np)
{
  mrb_value v;
  mrb_int argc, lev, n;

  argc = mrb_get_args(mrb, "|oi", &v, &n);

  switch (argc) {
  case 0:
    lev = 1;
    n = bt_len - 1;
    break;
  case 1:
    if (mrb_range_p(v)) {
      mrb_int beg, len;
      if (mrb_range_beg_len(mrb, v, &beg, &len, bt_len, TRUE) == MRB_RANGE_OK) {
        lev = beg;
        n = len;
      }
      else {
        return FALSE;
      }
    }
    else {
      lev = mrb_as_int(mrb, v);
      if (lev < 0) {
        mrb_raisef(mrb, E_ARGUMENT_ERROR, "negative level (%v)", v);
      }
      n = bt_len - lev;
    }
    break;
  case 2:
    lev = mrb_as_int(mrb, v);
    break;
  default:
    /* not reached */
    lev = n = 0;
    break;
  }
  if (lev >= bt_len) return FALSE;
  if (lev < 0) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "negative level (%v)", v);
  }
  if (n < 0) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "negative size (%d)", n);
  }
  if (bt_len <= n + lev) n = bt_len - lev - 1;
  *levp = lev;
  *np = n;
  return TRUE;
}

/*
 * call-seq:
 *   caller(start = 1)          -> array or nil
 *   caller(range)              -> array or nil
 *   caller(start, length)      -> array or nil
 *
 * Returns the current execution stack as an array of strings in the form
 * "file:line" or "file:line:in `method'". The optional start parameter
 * determines the number of initial stack entries to omit from the top of the stack.
 *
 *   def a(skip)
 *     caller(skip)
 *   end
 *   def b
 *     a(0)
 *   end
 *   def c
 *     b
 *   end
 *   c  #=> ["prog:2:in `a'", "prog:5:in `b'", "prog:8:in `c'", "prog:10:in `<main>'"]
 */
static mrb_value
mrb_f_caller(mrb_state *mrb, mrb_value self)
{
  mrb_value bt = mrb_get_backtrace(mrb);
  mrb_int lev, n;

  if (!caller_range(mrb, RARRAY_LEN(bt), &lev, &n)) return mrb_nil_value();
  if (n == 0) return mrb_ary_new(mrb);
  return mrb_ary_new_from_values(mrb, n, RARRAY_PTR(bt)+lev+1);
}

/* A Thread::Backtrace::Location is entry `__idx__` of the packed backtrace
   `__bt__`, read when it is asked for. */
static mrb_value
location_new(mrb_state *mrb, struct RClass *location, mrb_value bt, mrb_int i)
{
  struct RObject *loc = MRB_OBJ_ALLOC(mrb, MRB_TT_OBJECT, location);

  mrb_obj_iv_set_force(mrb, loc, MRB_SYM(__bt__), bt);
  mrb_obj_iv_set_force(mrb, loc, MRB_SYM(__idx__), mrb_int_value(mrb, i));
  return mrb_obj_value(loc);
}

static mrb_value
locations_new(mrb_state *mrb, mrb_value bt, mrb_int lev, mrb_int n)
{
  struct RClass *thread = mrb_class_get_id(mrb, MRB_SYM(Thread));
  struct RClass *backtrace = mrb_class_get_under_id(mrb, thread, MRB_SYM(Backtrace));
  struct RClass *location = mrb_class_get_under_id(mrb, backtrace, MRB_SYM(Location));
  mrb_value ary = mrb_ary_new_capa(mrb, n);
  int ai = mrb_gc_arena_save(mrb);

  for (mrb_int i = 0; i < n; i++) {
    mrb_ary_push(mrb, ary, location_new(mrb, location, bt, lev + i));
    mrb_gc_arena_restore(mrb, ai);
  }
  return ary;
}

/*
 * call-seq:
 *   caller_locations(start = 1)          -> array or nil
 *   caller_locations(range)              -> array or nil
 *   caller_locations(start, length)      -> array or nil
 *
 * Returns the current execution stack as Thread::Backtrace::Location
 * objects, one for each string `caller` gives for the same arguments.
 *
 *   def a
 *     caller_locations(0, 1)[0]
 *   end
 *   loc = a
 *   loc.path    #=> "prog"
 *   loc.lineno  #=> 2
 *   loc.label   #=> "a"
 */
static mrb_value
mrb_f_caller_locations(mrb_state *mrb, mrb_value self)
{
  mrb_value bt = mrb_get_packed_backtrace(mrb);
  mrb_int lev, n;

  if (!caller_range(mrb, mrb_backtrace_len(bt), &lev, &n)) return mrb_nil_value();
  return locations_new(mrb, bt, lev+1, n);
}

/*
 * call-seq:
 *   exc.backtrace_locations -> array or nil
 *
 * Returns the backtrace of the exception as Thread::Backtrace::Location
 * objects; nil before the exception is raised, or once its backtrace has
 * been set with set_backtrace.
 */
static mrb_value
mrb_exc_backtrace_locations(mrb_state *mrb, mrb_value exc)
{
  mrb_value bt = mrb_exc_packed_backtrace(mrb, exc);

  if (mrb_nil_p(bt)) return bt;
  return locations_new(mrb, bt, 0, mrb_backtrace_len(bt));
}

static mrb_bool
location_get(mrb_state *mrb, mrb_value self, const char **file, int32_t *lineno, mrb_sym *mid, mrb_value *str)
{
  struct RObject *loc = mrb_obj_ptr(self);
  mrb_value bt = mrb_obj_iv_get(mrb, loc, MRB_SYM(__bt__));
  mrb_int i = mrb_integer(mrb_obj_iv_get(mrb, loc, MRB_SYM(__idx__)));

  return mrb_backtrace_location(mrb, bt, i, file, lineno, mid, str);
}

/*
 * call-seq:
 *   loc.path -> string
 *
 * The file of the location; "(unknown)" when there is no debug information.
 */
static mrb_value
location_path(mrb_state *mrb, mrb_value self)
{
  const char *file = NULL;
  int32_t lineno;

  if (!location_get(mrb, self, &file, &lineno, NULL, NULL) || !file) {
    return mrb_str_new_lit(mrb, "(unknown)");
  }
  return mrb_str_new_cstr(mrb, file);
}

/*
 * call-seq:
 *   loc.lineno -> integer
 *
 * The line of the location; 0 when it is unknown.
 */
static mrb_value
location_lineno(mrb_state *mrb, mrb_value self)
{
  const char *file;
  int32_t lineno = 0;

  if (!location_get(mrb, self, &file, &lineno, NULL, NULL) || lineno < 0) {
    return mrb_fixnum_value(0);
  }
  return mrb_fixnum_value(lineno);
}

/*
 * call-seq:
 *   loc.label -> string or nil
 *
 * The method of the location, as `caller` names it; nil for none.
 */
static mrb_value
location_label(mrb_state *mrb, mrb_value self)
{
  const char *file;
  int32_t lineno;
  mrb_sym mid;

  location_get(mrb, self, &file, &lineno, &mid, NULL);
  if (mid == 0) return mrb_nil_value();
  return mrb_sym_str(mrb, mid);
}

/*
 * call-seq:
 *   loc.to_s -> string
 *
 * The location as the string `caller` gives for it.
 */
static mrb_value
location_to_s(mrb_state *mrb, mrb_value self)
{
  const char *file;
  int32_t lineno;
  mrb_value str;

  location_get(mrb, self, &file, &lineno, NULL, &str);
  return str;
}

static mrb_value
location_inspect(mrb_state *mrb, mrb_value self)
{
  return mrb_str_inspect(mrb, location_to_s(mrb, self));
}

/*
 *  call-seq:
 *     __method__         -> symbol
 *
 *  Returns the called name of the current method as a Symbol.
 *  If called outside of a method, it returns `nil`.
 *
 */
static mrb_value
mrb_f_method(mrb_state *mrb, mrb_value self)
{
  mrb_callinfo *ci = mrb->c->ci;
  ci--;
  if (ci->proc && ci->proc->e.env && ci->proc->e.env->tt == MRB_TT_ENV && ci->proc->e.env->mid)
    return mrb_symbol_value(ci->proc->e.env->mid);
  else if (ci->mid)
    return mrb_symbol_value(ci->mid);
  else
    return mrb_nil_value();
}

/*
 *  call-seq:
 *     __callee__         -> symbol
 *
 *  Returns the called name of the current method as a Symbol.
 *  If called outside of a method, it returns `nil`.
 *
 */
static mrb_value
mrb_f_callee(mrb_state *mrb, mrb_value self)
{
  mrb_callinfo *ci = mrb->c->ci;
  ci--;
  if (ci->mid)
    return mrb_symbol_value(ci->mid);
  else
    return mrb_nil_value();
}

/*
 *  call-seq:
 *     Integer(arg,base=0)    -> integer
 *
 *  Converts *arg* to a `Integer`.
 *  Numeric types are converted directly (with floating-point numbers
 *  being truncated).    *base* (0, or between 2 and 36) is a base for
 *  integer string representation.  If *arg* is a `String`,
 *  when *base* is omitted or equals to zero, radix indicators
 *  (`0`, `0b`, and `0x`) are honored.
 *  In any case, strings should be strictly conformed to numeric
 *  representation. This behavior is different from that of
 *  `String#to_i`.  Non string values will be treated as integers.
 *  Passing `nil` raises a TypeError.
 *
 *     Integer(123.999)    #=> 123
 *     Integer("0x1a")     #=> 26
 *     Integer(Time.new)   #=> 1204973019
 *     Integer("0930", 10) #=> 930
 *     Integer("111", 2)   #=> 7
 *     Integer(nil)        #=> TypeError
 */
static mrb_noreturn void
arg_error(mrb_state *mrb)
{
  mrb_raise(mrb, E_ARGUMENT_ERROR, "base specified for non string value");
}

/* the conversion itself, so exception: false can run it under a guard */
static mrb_value
integer_convert(mrb_state *mrb, mrb_value val, mrb_int base)
{
  mrb_value tmp;

  if (mrb_nil_p(val)) {
    if (base != 0) arg_error(mrb);
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert nil into Integer");
  }
  switch (mrb_type(val)) {
#ifndef MRB_NO_FLOAT
    case MRB_TT_FLOAT:
      if (base != 0) arg_error(mrb);
      return mrb_float_to_integer(mrb, val);
#endif

    case MRB_TT_INTEGER:
      if (base != 0) arg_error(mrb);
      return val;

    case MRB_TT_STRING:
    string_conv:
      return mrb_str_to_integer(mrb, val, base, TRUE);

    default:
      break;
  }
  if (base != 0) {
    tmp = mrb_obj_as_string(mrb, val);
    if (mrb_string_p(tmp)) {
      val = tmp;
      goto string_conv;
    }
    arg_error(mrb);
  }
  /* to raise TypeError */
  return mrb_ensure_integer_type(mrb, val);
}

struct integer_args {
  mrb_value val;
  mrb_int base;
};

static mrb_value
integer_body(mrb_state *mrb, void *data)
{
  struct integer_args *a = (struct integer_args *)data;
  return integer_convert(mrb, a->val, a->base);
}

/* exception: false in the keywords: nil instead of a raise */
static mrb_bool
no_exception_p(mrb_state *mrb, const mrb_value *kw_values)
{
  return !mrb_undef_p(kw_values[0]) && !mrb_test(kw_values[0]);
}

/* An illegal radix is a mistake in the call, not a conversion that failed,
   so exception: false does not turn it into nil: the same error the string
   conversion would raise, raised before the guard.  What that conversion
   takes: 0 and -1 let the string's prefix choose the radix, and a negative
   radix is the positive one with no prefix allowed. */
static void
check_radix(mrb_state *mrb, mrb_int base)
{
  mrb_int b = base;

  if (b == 0 || b == -1) return;
  if (b < -MRB_INT_MAX) {
    /* MRB_INT_MIN: negating it would overflow mrb_int */
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "illegal radix %i", base);
  }
  if (b < 0) b = -b;
  if (b < 2 || 36 < b) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "illegal radix %i", b);
  }
}

static mrb_value
mrb_f_integer(mrb_state *mrb, mrb_value self)
{
  struct integer_args a;
  const mrb_sym kw_names[1] = { MRB_SYM(exception) };
  mrb_value kw_values[1];
  const mrb_kwargs kwargs = { 1, 0, kw_names, kw_values, NULL };

  a.base = 0;
  mrb_get_args(mrb, "o|i:", &a.val, &a.base, &kwargs);
  if (no_exception_p(mrb, kw_values)) {
    mrb_bool error;

    if (mrb_string_p(a.val)) check_radix(mrb, a.base);
    mrb_value ret = mrb_protect_error(mrb, integer_body, &a, &error);
    if (error) {
      mrb->exc = NULL;
      return mrb_nil_value();
    }
    return ret;
  }
  return integer_convert(mrb, a.val, a.base);
}

#ifndef MRB_NO_FLOAT
/*
 *  call-seq:
 *     Float(arg)    -> float
 *
 *  Returns *arg* converted to a float. Numeric types are converted
 *  directly, the rest are converted using *arg*.to_f.
 *
 *     Float(1)           #=> 1.0
 *     Float(123.456)     #=> 123.456
 *     Float("123.456")   #=> 123.456
 *     Float(nil)         #=> TypeError
 */
static mrb_value
float_body(mrb_state *mrb, void *data)
{
  mrb_value arg = *(mrb_value *)data;
  if (mrb_string_p(arg)) {
    return mrb_float_value(mrb, mrb_str_to_dbl(mrb, arg, TRUE));
  }
  return mrb_ensure_float_type(mrb, arg);
}

static mrb_value
mrb_f_float(mrb_state *mrb, mrb_value self)
{
  mrb_value arg;
  const mrb_sym kw_names[1] = { MRB_SYM(exception) };
  mrb_value kw_values[1];
  const mrb_kwargs kwargs = { 1, 0, kw_names, kw_values, NULL };

  mrb_get_args(mrb, "o:", &arg, &kwargs);
  if (no_exception_p(mrb, kw_values)) {
    mrb_bool error;
    mrb_value ret = mrb_protect_error(mrb, float_body, &arg, &error);
    if (error) {
      mrb->exc = NULL;
      return mrb_nil_value();
    }
    return ret;
  }
  return float_body(mrb, &arg);
}
#endif

/*
 *  call-seq:
 *     String(arg)   -> string
 *
 *  Returns *arg* as an `String`.
 *  converted using `to_s` method.
 *
 *     String(self)        #=> "main"
 *     String(self.class)  #=> "Object"
 *     String(123456)      #=> "123456"
 */
static mrb_value
mrb_f_string(mrb_state *mrb, mrb_value self)
{
  mrb_value arg = mrb_get_arg1(mrb);
  mrb_value tmp = mrb_type_convert(mrb, arg, MRB_TT_STRING, MRB_SYM(to_s));
  return tmp;
}

/*
 *  call-seq:
 *     Array(arg)    -> array
 *
 *  Returns `arg` as an Array using to_a method.
 *
 *     Array(1..5)   #=> [1, 2, 3, 4, 5]
 *
 */
static mrb_value
mrb_f_array(mrb_state *mrb, mrb_value self)
{
  mrb_value arg = mrb_get_arg1(mrb);
  mrb_value tmp = mrb_type_convert_check(mrb, arg, MRB_TT_ARRAY, MRB_SYM(to_a));
  if (mrb_nil_p(tmp)) {
    return mrb_ary_new_from_values(mrb, 1, &arg);
  }

  return tmp;
}

/*
 *  call-seq:
 *     Hash(arg)    -> hash
 *
 *  Returns a `Hash` if *arg* is a `Hash`.
 *  Returns an empty `Hash` when *arg* is `nil`
 *  or `[]`.
 *
 *      Hash([])          #=> {}
 *      Hash(nil)         #=> {}
 *      Hash(key: :value) #=> {:key => :value}
 *      Hash([1, 2, 3])   #=> TypeError
 *
 */
static mrb_value
mrb_f_hash(mrb_state *mrb, mrb_value self)
{
  mrb_value arg = mrb_get_arg1(mrb);

  if (mrb_nil_p(arg) || (mrb_array_p(arg) && RARRAY_LEN(arg) == 0)) {
    return mrb_hash_new(mrb);
  }
  mrb_ensure_hash_type(mrb, arg);
  return arg;
}

static const mrb_mt_entry kernel_ext_rom_entries[] = {
  MRB_MT_ENTRY(mrb_f_raise, MRB_SYM(fail),      MRB_ARGS_OPT(2) | MRB_ARGS_KEY(1,1) | MRB_MT_PRIVATE),
  MRB_MT_ENTRY(mrb_f_caller, MRB_SYM(caller),    MRB_ARGS_OPT(2) | MRB_MT_PRIVATE),
  MRB_MT_ENTRY(mrb_f_caller_locations, MRB_SYM(caller_locations), MRB_ARGS_OPT(2) | MRB_MT_PRIVATE),
  MRB_MT_ENTRY(mrb_f_method, MRB_SYM(__method__),             MRB_ARGS_NONE() | MRB_MT_PRIVATE),
  MRB_MT_ENTRY(mrb_f_callee, MRB_SYM(__callee__),             MRB_ARGS_NONE() | MRB_MT_PRIVATE),
  MRB_MT_ENTRY(mrb_f_integer, MRB_SYM(Integer), MRB_ARGS_ARG(1,1) | MRB_ARGS_KEY(1,0) | MRB_MT_PRIVATE),
  MRB_MT_ENTRY(mrb_f_string, MRB_SYM(String),    MRB_ARGS_REQ(1) | MRB_MT_PRIVATE),
  MRB_MT_ENTRY(mrb_f_array, MRB_SYM(Array),     MRB_ARGS_REQ(1) | MRB_MT_PRIVATE),
#ifndef MRB_NO_FLOAT
  MRB_MT_ENTRY(mrb_f_float, MRB_SYM(Float),     MRB_ARGS_REQ(1) | MRB_ARGS_KEY(1,0) | MRB_MT_PRIVATE),
#endif
  MRB_MT_ENTRY(mrb_f_hash, MRB_SYM(Hash),      MRB_ARGS_REQ(1) | MRB_MT_PRIVATE),
};

/* public counterparts on `Kernel` itself; see `kernel_module_function_entries`
   in src/kernel.c */
static const mrb_mt_entry kernel_ext_module_function_entries[] = {
  MRB_MT_ENTRY(mrb_f_raise, MRB_SYM(fail),      MRB_ARGS_OPT(2) | MRB_ARGS_KEY(1,1)),
  MRB_MT_ENTRY(mrb_f_caller, MRB_SYM(caller),    MRB_ARGS_OPT(2)),
  MRB_MT_ENTRY(mrb_f_caller_locations, MRB_SYM(caller_locations), MRB_ARGS_OPT(2)),
  MRB_MT_ENTRY(mrb_f_method, MRB_SYM(__method__),             MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mrb_f_callee, MRB_SYM(__callee__),             MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mrb_f_integer, MRB_SYM(Integer), MRB_ARGS_ARG(1,1) | MRB_ARGS_KEY(1,0)),
  MRB_MT_ENTRY(mrb_f_string, MRB_SYM(String),    MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(mrb_f_array, MRB_SYM(Array),     MRB_ARGS_REQ(1)),
#ifndef MRB_NO_FLOAT
  MRB_MT_ENTRY(mrb_f_float, MRB_SYM(Float),     MRB_ARGS_REQ(1) | MRB_ARGS_KEY(1,0)),
#endif
  MRB_MT_ENTRY(mrb_f_hash, MRB_SYM(Hash),      MRB_ARGS_REQ(1)),
};

static const mrb_mt_entry location_rom_entries[] = {
  MRB_MT_ENTRY(location_path,    MRB_SYM(path),          MRB_ARGS_NONE()),
  MRB_MT_ENTRY(location_path,    MRB_SYM(absolute_path), MRB_ARGS_NONE()),
  MRB_MT_ENTRY(location_lineno,  MRB_SYM(lineno),        MRB_ARGS_NONE()),
  MRB_MT_ENTRY(location_label,   MRB_SYM(label),         MRB_ARGS_NONE()),
  MRB_MT_ENTRY(location_label,   MRB_SYM(base_label),    MRB_ARGS_NONE()),
  MRB_MT_ENTRY(location_to_s,    MRB_SYM(to_s),          MRB_ARGS_NONE()),
  MRB_MT_ENTRY(location_inspect, MRB_SYM(inspect),       MRB_ARGS_NONE()),
};

static const mrb_mt_entry exception_ext_rom_entries[] = {
  MRB_MT_ENTRY(mrb_exc_backtrace_locations, MRB_SYM(backtrace_locations), MRB_ARGS_NONE()),
};

void
mrb_mruby_kernel_ext_gem_init(mrb_state *mrb)
{
  struct RClass *krn = mrb->kernel_module;

  MRB_MT_INIT_ROM(mrb, krn, kernel_ext_rom_entries);
  MRB_MT_INIT_ROM(mrb, mrb_singleton_class_ptr(mrb, mrb_obj_value(krn)),
                  kernel_ext_module_function_entries);

  /* CRuby's name for it; `Thread` is only its namespace here */
  struct RClass *thread = mrb_define_class_id(mrb, MRB_SYM(Thread), mrb->object_class);
  struct RClass *backtrace = mrb_define_class_under_id(mrb, thread, MRB_SYM(Backtrace), mrb->object_class);
  struct RClass *location = mrb_define_class_under_id(mrb, backtrace, MRB_SYM(Location), mrb->object_class);
  MRB_UNDEF_ALLOCATOR(location);
  mrb_undef_class_method_id(mrb, location, MRB_SYM(new));
  MRB_MT_INIT_ROM(mrb, location, location_rom_entries);
  MRB_MT_INIT_ROM(mrb, mrb->eException_class, exception_ext_rom_entries);
}

void
mrb_mruby_kernel_ext_gem_final(mrb_state *mrb)
{
}
