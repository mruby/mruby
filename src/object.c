/*
** object.c - Object, NilClass, TrueClass, FalseClass class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/numeric.h>
#include <mruby/string.h>
#include <mruby/class.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

/*
 * Checks if two mruby values, `v1` and `v2`, are identical.
 * For most types, this is equivalent to pointer equality.
 * For immediate values (integers, symbols, true, false, nil),
 * it compares their actual values.
 *
 * Behavior under different boxing configurations:
 * - MRB_NAN_BOXING: Compares the raw `uint64_t` values.
 * - MRB_WORD_BOXING: Compares the raw `mrb_word` values.
 * - MRB_NO_BOXING: Checks if types are equal. If so, performs
 *   type-specific comparisons (value for immediates, pointer for others).
 */
MRB_API mrb_bool
mrb_obj_eq(mrb_state *mrb, mrb_value v1, mrb_value v2)
{
#if defined(MRB_NAN_BOXING)
  return v1.u == v2.u;
#elif defined(MRB_WORD_BOXING)
  return v1.w == v2.w;
#else  /* MRB_NO_BOXING */
  if (mrb_type(v1) != mrb_type(v2)) return FALSE;
  switch (mrb_type(v1)) {
  case MRB_TT_TRUE:
    return TRUE;

  case MRB_TT_FALSE:
    return (mrb_fixnum(v1) == mrb_fixnum(v2));
  case MRB_TT_INTEGER:
    return (mrb_integer(v1) == mrb_integer(v2));
  case MRB_TT_SYMBOL:
    return (mrb_symbol(v1) == mrb_symbol(v2));

#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
    return (mrb_float(v1) == mrb_float(v2));
#endif

  default:
    return (mrb_ptr(v1) == mrb_ptr(v2));
  }
#endif
}

/*
 * Checks if two mruby values, `v1` and `v2`, are equal.
 * This function currently calls mrb_obj_eq to perform the comparison.
 */
MRB_API mrb_bool
mrb_obj_equal(mrb_state *mrb, mrb_value v1, mrb_value v2)
{
  return mrb_obj_eq(mrb, v1, v2);
}

/*
 * Checks for equality between `obj1` and `obj2`.
 *
 * It first uses `mrb_obj_eq` for an identity check. If that fails,
 * it handles cases like mixed integer/float comparisons.
 * If `MRB_USE_BIGINT` is defined, it also considers comparisons
 * involving BigInts against Integers, other BigInts, or Floats.
 * Finally, if none of the above apply, it attempts to call the
 * `==` operator (MRB_OPSYM(eq)) on `obj1` with `obj2` as an argument,
 * unless `obj1`'s `==` method is the default `mrb_obj_equal_m`.
 */
MRB_API mrb_bool
mrb_equal(mrb_state *mrb, mrb_value obj1, mrb_value obj2)
{
  if (mrb_obj_eq(mrb, obj1, obj2)) return TRUE;
#ifndef MRB_NO_FLOAT
  /* value mixing with integer and float */
  else if (mrb_integer_p(obj1) && mrb_float_p(obj2)) {
    if ((mrb_float)mrb_integer(obj1) == mrb_float(obj2))
      return TRUE;
  }
  else if (mrb_float_p(obj1) && mrb_integer_p(obj2)) {
    if (mrb_float(obj1) == (mrb_float)mrb_integer(obj2))
      return TRUE;
  }
#endif
#ifdef MRB_USE_BIGINT
  else if (mrb_bigint_p(obj1) &&
      (mrb_integer_p(obj2) || mrb_bigint_p(obj2) || mrb_float_p(obj2))) {
    if (mrb_bint_cmp(mrb, obj1, obj2) == 0)
      return TRUE;
  }
#endif
  else if (!mrb_func_basic_p(mrb, obj1, MRB_OPSYM(eq), mrb_obj_equal_m)) {
    mrb_value result = mrb_funcall_argv(mrb, obj1, MRB_OPSYM(eq), 1, &obj2);
    if (mrb_test(result)) return TRUE;
  }
  return FALSE;
}

/*
 * Document-class: NilClass
 *
 *  The class of the singleton object `nil`.
 */

/* 15.2.4.3.4  */
/*
 * call_seq:
 *   nil.nil?               -> true
 *
 * Only the object *nil* responds `true` to `nil?`.
 */

static mrb_value
mrb_true(mrb_state *mrb, mrb_value obj)
{
  return mrb_true_value();
}

/* 15.2.4.3.5  */
/*
 *  call-seq:
 *     nil.to_s    -> ""
 *
 *  Always returns the empty string.
 */

static mrb_value
nil_to_s(mrb_state *mrb, mrb_value obj)
{
  mrb_value str = mrb_str_new_frozen(mrb, NULL, 0);
  RSTR_SET_ASCII_FLAG(mrb_str_ptr(str));
  return str;
}

static mrb_value
nil_inspect(mrb_state *mrb, mrb_value obj)
{
  mrb_value str = mrb_str_new_lit_frozen(mrb, "nil");
  RSTR_SET_ASCII_FLAG(mrb_str_ptr(str));
  return str;
}

/***********************************************************************
 *  Document-class: TrueClass
 *
 *  The global value `true` is the only instance of class
 *  `TrueClass` and represents a logically true value in
 *  boolean expressions. The class provides operators allowing
 *  `true` to be used in logical expressions.
 */

/* 15.2.5.3.1  */
/*
 *  call-seq:
 *     true & obj    -> true or false
 *
 *  And---Returns `false` if *obj* is
 *  `nil` or `false`, `true` otherwise.
 */

static mrb_value
true_and(mrb_state *mrb, mrb_value obj)
{
  mrb_bool obj2;

  mrb_get_args(mrb, "b", &obj2);

  return mrb_bool_value(obj2);
}

/* 15.2.5.3.2  */
/*
 *  call-seq:
 *     true ^ obj   -> !obj
 *
 *  Exclusive Or---Returns `true` if *obj* is
 *  `nil` or `false`, `false`
 *  otherwise.
 */

static mrb_value
true_xor(mrb_state *mrb, mrb_value obj)
{
  mrb_bool obj2;

  mrb_get_args(mrb, "b", &obj2);
  return mrb_bool_value(!obj2);
}

/* 15.2.5.3.3  */
/*
 * call-seq:
 *   true.to_s   ->  "true"
 *
 * The string representation of `true` is "true".
 */

static mrb_value
true_to_s(mrb_state *mrb, mrb_value obj)
{
  mrb_value str = mrb_str_new_lit_frozen(mrb, "true");
  RSTR_SET_ASCII_FLAG(mrb_str_ptr(str));
  return str;
}

/* 15.2.5.3.4  */
/*
 *  call-seq:
 *     true | obj   -> true
 *
 *  Or---Returns `true`. As *anObject* is an argument to
 *  a method call, it is always evaluated; there is no short-circuit
 *  evaluation in this case.
 *
 *     true |  puts("or")
 *     true || puts("logical or")
 *
 *  <em>produces:</em>
 *
 *     or
 */

static mrb_value
true_or(mrb_state *mrb, mrb_value obj)
{
  return mrb_true_value();
}

/*
 *  Document-class: FalseClass
 *
 *  The global value `false` is the only instance of class
 *  `FalseClass` and represents a logically false value in
 *  boolean expressions. The class provides operators allowing
 *  `false` to participate correctly in logical expressions.
 *
 */

/* 15.2.4.3.1  */
/* 15.2.6.3.1  */
/*
 *  call-seq:
 *     false & obj   -> false
 *     nil & obj     -> false
 *
 *  And---Returns `false`. *obj* is always
 *  evaluated as it is the argument to a method call---there is no
 *  short-circuit evaluation in this case.
 */

static mrb_value
false_and(mrb_state *mrb, mrb_value obj)
{
  return mrb_false_value();
}

/* 15.2.4.3.2  */
/* 15.2.6.3.2  */
/*
 *  call-seq:
 *     false ^ obj    -> true or false
 *     nil   ^ obj    -> true or false
 *
 *  Exclusive Or---If *obj* is `nil` or
 *  `false`, returns `false`; otherwise, returns
 *  `true`.
 *
 */

static mrb_value
false_xor(mrb_state *mrb, mrb_value obj)
{
  mrb_bool obj2;

  mrb_get_args(mrb, "b", &obj2);
  return mrb_bool_value(obj2);
}

/* 15.2.4.3.3  */
/* 15.2.6.3.4  */
/*
 *  call-seq:
 *     false | obj   ->   true or false
 *     nil   | obj   ->   true or false
 *
 *  Or---Returns `false` if *obj* is
 *  `nil` or `false`; `true` otherwise.
 */

static mrb_value
false_or(mrb_state *mrb, mrb_value obj)
{
  mrb_bool obj2;

  mrb_get_args(mrb, "b", &obj2);
  return mrb_bool_value(obj2);
}

/* 15.2.6.3.3  */
/*
 * call-seq:
 *   false.to_s   ->  "false"
 *
 * 'nuf said...
 */

static mrb_value
false_to_s(mrb_state *mrb, mrb_value obj)
{
  mrb_value str = mrb_str_new_lit_frozen(mrb, "false");
  RSTR_SET_ASCII_FLAG(mrb_str_ptr(str));
  return str;
}

void
mrb_init_object(mrb_state *mrb)
{
  struct RClass *n;
  struct RClass *t;
  struct RClass *f;

  mrb->nil_class = n = mrb_define_class_id(mrb, MRB_SYM(NilClass), mrb->object_class);
  MRB_SET_INSTANCE_TT(n, MRB_TT_FALSE);
  mrb_undef_class_method_id(mrb, n, MRB_SYM(new));
  mrb_define_method_id(mrb, n, MRB_OPSYM(and),  false_and,      MRB_ARGS_REQ(1));  /* 15.2.4.3.1  */
  mrb_define_method_id(mrb, n, MRB_OPSYM(or),   false_or,       MRB_ARGS_REQ(1));  /* 15.2.4.3.2  */
  mrb_define_method_id(mrb, n, MRB_OPSYM(xor),  false_xor,      MRB_ARGS_REQ(1));  /* 15.2.4.3.3  */
  mrb_define_method_id(mrb, n, MRB_SYM_Q(nil),  mrb_true,       MRB_ARGS_NONE());  /* 15.2.4.3.4  */
  mrb_define_method_id(mrb, n, MRB_SYM(to_s),   nil_to_s,       MRB_ARGS_NONE());  /* 15.2.4.3.5  */
  mrb_define_method_id(mrb, n, MRB_SYM(inspect), nil_inspect, MRB_ARGS_NONE());

  mrb->true_class = t = mrb_define_class_id(mrb, MRB_SYM(TrueClass), mrb->object_class);
  MRB_SET_INSTANCE_TT(t, MRB_TT_TRUE);
  mrb_undef_class_method_id(mrb, t, MRB_SYM(new));
  mrb_define_method_id(mrb, t, MRB_OPSYM(and),  true_and,       MRB_ARGS_REQ(1));  /* 15.2.5.3.1  */
  mrb_define_method_id(mrb, t, MRB_OPSYM(or),   true_or,        MRB_ARGS_REQ(1));  /* 15.2.5.3.2  */
  mrb_define_method_id(mrb, t, MRB_OPSYM(xor),  true_xor,       MRB_ARGS_REQ(1));  /* 15.2.5.3.3  */
  mrb_define_method_id(mrb, t, MRB_SYM(to_s),   true_to_s,      MRB_ARGS_NONE());  /* 15.2.5.3.4  */
  mrb_define_method_id(mrb, t, MRB_SYM(inspect), true_to_s,   MRB_ARGS_NONE());

  mrb->false_class = f = mrb_define_class_id(mrb, MRB_SYM(FalseClass), mrb->object_class);
  MRB_SET_INSTANCE_TT(f, MRB_TT_FALSE);
  mrb_undef_class_method_id(mrb, f, MRB_SYM(new));
  mrb_define_method_id(mrb, f, MRB_OPSYM(and),  false_and,      MRB_ARGS_REQ(1));  /* 15.2.6.3.1  */
  mrb_define_method_id(mrb, f, MRB_OPSYM(or),   false_or,       MRB_ARGS_REQ(1));  /* 15.2.6.3.2  */
  mrb_define_method_id(mrb, f, MRB_OPSYM(xor),  false_xor,      MRB_ARGS_REQ(1));  /* 15.2.6.3.3  */
  mrb_define_method_id(mrb, f, MRB_SYM(to_s),   false_to_s,     MRB_ARGS_NONE());  /* 15.2.6.3.4  */
  mrb_define_method_id(mrb, f, MRB_SYM(inspect), false_to_s,  MRB_ARGS_NONE());
}

static const char*
type_name(enum mrb_vtype t)
{
  switch (t) {
#define MRB_VTYPE_NAME(tt, type, name) case tt: return name;
    MRB_VTYPE_FOREACH(MRB_VTYPE_NAME)
#undef MRB_VTYPE_NAME
    default: return NULL;
  }
}

static mrb_value
convert_type(mrb_state *mrb, mrb_value val, const char *tname, mrb_sym method, mrb_bool raise)
{
  if (!mrb_respond_to(mrb, val, method)) {
    if (raise) {
      if (tname) mrb_raisef(mrb, E_TYPE_ERROR, "can't convert %Y into %s", val, tname);
      mrb_raisef(mrb, E_TYPE_ERROR, "can't convert %Y", val);
    }
    return mrb_nil_value();
  }
  return mrb_funcall_argv(mrb, val, method, 0, 0);
}

/*
 * Attempts to convert the mruby value `val` to the specified `type`
 * by calling the given `method` (a symbol) on `val`.
 *
 * It first checks if `val` is already of the target `type`. If not, it
 * proceeds to call the conversion `method` on `val`.
 *
 * If the conversion method does not return a value of the target `type`,
 * a `TypeError` is raised. However, as a special case, if the target
 * `type` is `MRB_TT_STRING`, and the initial conversion fails to produce
 * a string, this function will then attempt to call `mrb_any_to_s` on
 * the original `val` as a fallback mechanism.
 *
 * Returns the converted value if successful.
 */
MRB_API mrb_value
mrb_type_convert(mrb_state *mrb, mrb_value val, enum mrb_vtype type, mrb_sym method)
{
  mrb_value v;
  const char *tname;

  if (mrb_type(val) == type) return val;
  tname = type_name(type);
  v = convert_type(mrb, val, tname, method, TRUE);
  if (mrb_type(v) != type) {
    if (type == MRB_TT_STRING) return mrb_any_to_s(mrb, val);
    mrb_raisef(mrb, E_TYPE_ERROR, "%v cannot be converted to %s by #%n", val, tname, method);
  }
  return v;
}

/*
 * Attempts to convert the mruby value `val` to the specified `type`
 * by calling the given `method` (a symbol) on `val`.
 *
 * This function first checks if `val` is already of the target `type`.
 * An exception to this initial check is if the target `type` is
 * `MRB_TT_CDATA` or `MRB_TT_ISTRUCT`; in these cases, the conversion
 * attempt proceeds regardless of `val`'s current type.
 *
 * If `val` is not already of the target `type` (or if it's `MRB_TT_CDATA`
 * or `MRB_TT_ISTRUCT`), the specified `method` is called on `val` to
 * perform the conversion. Unlike `mrb_type_convert`, this function
 * does *not* raise an error if the conversion fails or if the returned
 * value is not of the target `type`.
 *
 * Returns the converted value if the conversion was successful and the
 * resulting value is of the target `type`. Otherwise, it returns
 * `mrb_nil_value()`.
 */
MRB_API mrb_value
mrb_type_convert_check(mrb_state *mrb, mrb_value val, enum mrb_vtype type, mrb_sym method)
{
  mrb_value v;

  if (mrb_type(val) == type && type != MRB_TT_CDATA && type != MRB_TT_ISTRUCT) return val;
  v = convert_type(mrb, val, type_name(type), method, FALSE);
  if (mrb_nil_p(v) || mrb_type(v) != type) return mrb_nil_value();
  return v;
}

/*
 * Checks if the mruby value `x` is of the specified type `t`.
 *
 * If the type of `x` does not match `t`, this function raises
 * a `TypeError`. The error message provides details about the
 * actual type of `x` and the expected type `t`.
 */
MRB_API void
mrb_check_type(mrb_state *mrb, mrb_value x, enum mrb_vtype t)
{
  enum mrb_vtype xt = mrb_type(x);
  const char *tname, *ename;

  if (t == xt) return;

  tname = type_name(t);
  if (mrb_nil_p(x)) {
    ename = "nil";
  }
  else if (mrb_integer_p(x)) {
    ename = "Integer";
  }
  else if (mrb_symbol_p(x)) {
    ename = "Symbol";
  }
  else if (mrb_immediate_p(x)) {
    ename = RSTRING_PTR(mrb_obj_as_string(mrb, x));
  }
  else {
    ename = mrb_obj_classname(mrb, x);
  }
  if (tname) {
    mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected %s)",
               ename, tname);
  }
  mrb_raisef(mrb, E_TYPE_ERROR, "unknown type %d (%s given)", t, ename);
}

/* 15.3.1.3.46 */
/*
 *  call-seq:
 *     obj.to_s    => string
 *
 *  Returns a string representing *obj*. The default
 *  `to_s` prints the object's class and an encoding of the
 *  object id. As a special case, the top-level object that is the
 *  initial execution context of Ruby programs returns "main."
 */

MRB_API mrb_value
mrb_any_to_s(mrb_state *mrb, mrb_value obj)
{
  mrb_value str = mrb_str_new_capa(mrb, 20);
  const char *cname = mrb_obj_classname(mrb, obj);

  mrb_str_cat_lit(mrb, str, "#<");
  mrb_str_cat_cstr(mrb, str, cname);
  if (!mrb_immediate_p(obj)) {
    mrb_str_cat_lit(mrb, str, ":");
    mrb_str_cat_str(mrb, str, mrb_ptr_to_str(mrb, mrb_ptr(obj)));
  }
  mrb_str_cat_lit(mrb, str, ">");

  return str;
}

/*
 *  call-seq:
 *     obj.is_a?(class)       => true or false
 *     obj.kind_of?(class)    => true or false
 *
 *  Checks if the mruby object `obj` is an instance of class `c`,
 *  or an instance of a class that inherits from `c`, or an instance
 *  of a class that includes `c` if `c` is a module.
 *
 *  This function traverses the class hierarchy of `obj` upwards.
 *  It returns `TRUE` if `c` is found in the ancestry. Otherwise,
 *  it returns `FALSE`.
 *
 *  If `c` is not a class or module, a `TypeError` is raised.
 *
 *     module M;    end
 *     class A
 *       include M
 *     end
 *     class B < A; end
 *     class C < B; end
 *     b = B.new
 *     b.instance_of? A   #=> false (mrb_obj_is_instance_of)
 *     b.instance_of? B   #=> true  (mrb_obj_is_instance_of)
 *     b.kind_of? A       #=> true
 *     b.kind_of? B       #=> true
 *     b.kind_of? C       #=> false
 *     b.kind_of? M       #=> true
 */
MRB_API mrb_bool
mrb_obj_is_kind_of(mrb_state *mrb, mrb_value obj, struct RClass *c)
{
  struct RClass *cl = mrb_class(mrb, obj);

  switch (c->tt) {
    case MRB_TT_MODULE:
    case MRB_TT_CLASS:
    case MRB_TT_ICLASS:
    case MRB_TT_SCLASS:
      break;

    default:
      mrb_raise(mrb, E_TYPE_ERROR, "class or module required");
  }

  MRB_CLASS_ORIGIN(c);
  while (cl) {
    if (cl == c || cl->mt == c->mt)
      return TRUE;
    cl = cl->super;
  }
  return FALSE;
}

#ifdef MRB_USE_RATIONAL
// provided by mruby-rational with MRB_USE_RATIONAL
mrb_value mrb_rational_to_i(mrb_state *mrb, mrb_value rat);
mrb_value mrb_rational_to_f(mrb_state *mrb, mrb_value rat);
#endif
#ifdef MRB_USE_COMPLEX
// provided by mruby-complex with MRB_USE_COMPLEX
mrb_value mrb_complex_to_f(mrb_state *mrb, mrb_value comp);
mrb_value mrb_complex_to_i(mrb_state *mrb, mrb_value comp);
#endif

/*
 * Ensures that the given mruby value `val` is an Integer.
 *
 * If `val` is already an `MRB_TT_INTEGER`, it is returned directly.
 *
 * If `val` is an `MRB_TT_FLOAT` (and `MRB_NO_FLOAT` is not defined),
 * it is converted to an integer using `mrb_float_to_integer`.
 *
 * The function also handles conversions from other numeric types if
 * the respective modules are enabled:
 * - `MRB_TT_BIGINT` (if `MRB_USE_BIGINT` is defined): `val` is returned as is,
 *   as BigInts are considered integers.
 * - `MRB_TT_RATIONAL` (if `MRB_USE_RATIONAL` is defined): Converted using
 *   `mrb_rational_to_i`.
 * - `MRB_TT_COMPLEX` (if `MRB_USE_COMPLEX` is defined): Converted using
 *   `mrb_complex_to_i` (typically if the imaginary part is zero).
 *
 * If `val` cannot be converted to an Integer (e.g., it's a String or Array,
 * or a Complex with a non-zero imaginary part), a `TypeError` is raised.
 *
 * Returns the (potentially converted) integer value.
 */
MRB_API mrb_value
mrb_ensure_integer_type(mrb_state *mrb, mrb_value val)
{
  if (!mrb_integer_p(val)) {
#ifndef MRB_NO_FLOAT
    if (mrb_float_p(val)) {
      return mrb_float_to_integer(mrb, val);
    }
    else {
      switch (mrb_type(val)) {
#ifdef MRB_USE_BIGINT
      case MRB_TT_BIGINT:
        return val;
#endif
#ifdef MRB_USE_RATIONAL
      case MRB_TT_RATIONAL:
        return mrb_rational_to_i(mrb, val);
#endif
#ifdef MRB_USE_COMPLEX
      case MRB_TT_COMPLEX:
        return mrb_complex_to_i(mrb, val);
#endif
      default:
        break;
      }
    }
#endif
    mrb_raisef(mrb, E_TYPE_ERROR, "%Y cannot be converted to Integer", val);
  }
  return val;
}

/*
 * Ensures that the given mruby value `val` is a C `mrb_int` (fixed-size integer).
 *
 * This function first calls `mrb_ensure_integer_type` to convert `val`
 * to a generic mruby Integer if it's not already. This step might result
 * in `val` being a Fixnum or a BigInt (if `MRB_USE_BIGINT` is enabled).
 *
 * If `mrb_ensure_integer_type` returns a BigInt (and `MRB_USE_BIGINT`
 * is defined), this function then attempts to convert the BigInt to a C
 * `mrb_int` using `mrb_bint_as_int`. This conversion may involve truncation
 * if the BigInt's value is outside the representable range of `mrb_int`,
 * or it could raise an error (e.g., RangeError) depending on the
 * `mrb_bint_as_int` implementation if the value is too large to truncate.
 *
 * If `val` is already a standard Integer (Fixnum) after the call to
 * `mrb_ensure_integer_type`, it is returned directly as it fits `mrb_int`.
 *
 * Returns an `mrb_value` that represents a C `mrb_int`.
 */
MRB_API mrb_value
mrb_ensure_int_type(mrb_state *mrb, mrb_value val)
{
  val = mrb_ensure_integer_type(mrb, val);
#ifdef MRB_USE_BIGINT
  if (mrb_bigint_p(val)) {
    return mrb_int_value(mrb, mrb_bint_as_int(mrb, val));
  }
#endif
  return val;
}

#ifndef MRB_NO_FLOAT
/*
 * Ensures that the given mruby value `val` is a Float.
 *
 * If `val` is `nil`, this function raises a `TypeError`.
 *
 * If `val` is an `MRB_TT_INTEGER`, it is converted to an `mrb_float`.
 * If `val` is already an `MRB_TT_FLOAT`, it is returned directly.
 *
 * The function also handles conversions from other numeric types if the
 * respective mruby modules are enabled:
 * - `MRB_TT_RATIONAL` (if `MRB_USE_RATIONAL` is defined): Converted to Float
 *   using `mrb_rational_to_f`.
 * - `MRB_TT_COMPLEX` (if `MRB_USE_COMPLEX` is defined): Converted to Float
 *   using `mrb_complex_to_f` (typically requires the imaginary part to be zero).
 * - `MRB_TT_BIGINT` (if `MRB_USE_BIGINT` is defined): Converted to Float
 *   using `mrb_bint_as_float`.
 *
 * If `val` cannot be converted to a Float (e.g., it's a String, Array, or
 * an incompatible Complex number), a `TypeError` is raised.
 *
 * Returns an `mrb_value` representing an `mrb_float`.
 */
MRB_API mrb_value
mrb_ensure_float_type(mrb_state *mrb, mrb_value val)
{
  if (mrb_nil_p(val)) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert nil into Float");
  }
  switch (mrb_type(val)) {
    case MRB_TT_INTEGER:
      return mrb_float_value(mrb, (mrb_float)mrb_integer(val));

    case MRB_TT_FLOAT:
      return val;

#ifdef MRB_USE_RATIONAL
    case MRB_TT_RATIONAL:
      return mrb_rational_to_f(mrb, val);
#endif

#ifdef MRB_USE_COMPLEX
    case MRB_TT_COMPLEX:
      return mrb_complex_to_f(mrb, val);
#endif

#ifdef MRB_USE_BIGINT
    case MRB_TT_BIGINT:
      return mrb_float_value(mrb, mrb_bint_as_float(mrb, val));
#endif

    default:
      mrb_raisef(mrb, E_TYPE_ERROR, "%Y cannot be converted to Float", val);
      /* not reached */
      return val;
  }
}
#endif

/*
 * Ensures that the given mruby value `str` is a String.
 *
 * If `str` is not of type `MRB_TT_STRING`, this function raises
 * a `TypeError`.
 * Otherwise, `str` itself is returned.
 */
MRB_API mrb_value
mrb_ensure_string_type(mrb_state *mrb, mrb_value str)
{
  if (!mrb_string_p(str)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "%Y cannot be converted to String", str);
  }
  return str;
}

/*
 * Checks if the given mruby value `str` is a String.
 *
 * If `str` is of type `MRB_TT_STRING`, this function returns `str`.
 * Otherwise (if `str` is not a String), it returns `mrb_nil_value()`
 * without raising an error. This allows for type checking without
 * forcing error handling.
 */
MRB_API mrb_value
mrb_check_string_type(mrb_state *mrb, mrb_value str)
{
  if (!mrb_string_p(str)) return mrb_nil_value();
  return str;
}

/*
 * Ensures that the given mruby value `ary` is an Array.
 *
 * If `ary` is not of type `MRB_TT_ARRAY`, this function raises
 * a `TypeError`.
 * Otherwise, `ary` itself is returned.
 */
MRB_API mrb_value
mrb_ensure_array_type(mrb_state *mrb, mrb_value ary)
{
  if (!mrb_array_p(ary)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "%Y cannot be converted to Array", ary);
  }
  return ary;
}

/*
 * Checks if the given mruby value `ary` is an Array.
 *
 * If `ary` is of type `MRB_TT_ARRAY`, this function returns `ary`.
 * Otherwise (if `ary` is not an Array), it returns `mrb_nil_value()`
 * without raising an error. This allows for type checking without
 * forcing error handling.
 */
MRB_API mrb_value
mrb_check_array_type(mrb_state *mrb, mrb_value ary)
{
  if (!mrb_array_p(ary)) return mrb_nil_value();
  return ary;
}

/*
 * Ensures that the given mruby value `hash` is a Hash.
 *
 * If `hash` is not of type `MRB_TT_HASH`, this function raises
 * a `TypeError`.
 * Otherwise, `hash` itself is returned.
 */
MRB_API mrb_value
mrb_ensure_hash_type(mrb_state *mrb, mrb_value hash)
{
  if (mrb_hash_p(hash)) {
    return hash;
  }

  if (!mrb_respond_to(mrb, hash, MRB_SYM(to_hash))) {
    mrb_raisef(mrb, E_TYPE_ERROR, "%Y cannot be converted to Hash", hash);
  }

  mrb_value conv = mrb_funcall_argv(mrb, hash, MRB_SYM(to_hash), 0, NULL);

  if (!mrb_hash_p(conv)) {
    mrb_raisef(mrb, E_TYPE_ERROR,
               "can't convert %Y to Hash (%Y#to_hash gives %Y)", hash, hash,
               conv);
    return mrb_nil_value(); // unreachable
  }
  return conv;
}

/*
 * Checks if the given mruby value `hash` is a Hash.
 *
 * If `hash` is of type `MRB_TT_HASH`, this function returns `hash`.
 * Otherwise (if `hash` is not a Hash), it returns `mrb_nil_value()`
 * without raising an error. This allows for type checking without
 * forcing error handling.
 */
MRB_API mrb_value
mrb_check_hash_type(mrb_state *mrb, mrb_value hash)
{
  if (!mrb_hash_p(hash)) return mrb_nil_value();
  return hash;
}

/*
 * Returns a human-readable string representation of the mruby object `obj`.
 *
 * This function calls the `inspect` method (identified by `MRB_SYM(inspect)`)
 * on the given `obj`. The `inspect` method is expected to return a string
 * that is suitable for debugging and inspection.
 *
 * If the object's `inspect` method does not return a String value (e.g., it
 * returns `nil` or another type, or if the class doesn't define `inspect`
 * appropriately), this function falls back to calling `mrb_obj_as_string`.
 * `mrb_obj_as_string` typically provides a basic string representation,
 * such as "#<ClassName:0xPointer>" if `inspect` is unavailable or
 * misbehaves by not returning a string.
 *
 * The function ultimately returns the resulting string `mrb_value`.
 */
MRB_API mrb_value
mrb_inspect(mrb_state *mrb, mrb_value obj)
{
  mrb_value v = mrb_funcall_argv(mrb, obj, MRB_SYM(inspect), 0, NULL);
  if (!mrb_string_p(v)) {
    v = mrb_obj_as_string(mrb, obj);
  }
  return v;
}

/*
 * Checks if two mruby values, `obj1` and `obj2`, are equal using
 * `eql?` semantics.
 *
 * This function first performs an identity check on `obj1` and `obj2`
 * using `mrb_obj_eq`. If they are identical (i.e., the same object),
 * it returns `TRUE` immediately.
 *
 * Otherwise, it calls the `eql?` method on `obj1`, passing `obj2` as
 * an argument. The symbol for the `eql?` method is `MRB_SYM_Q(eql)`.
 *
 * The function returns `TRUE` if the `eql?` method call returns a truthy
 * value (any value other than `false` or `nil`). Otherwise, it returns
 * `FALSE`. This is determined by `mrb_test` on the result of the
 * method call.
 */
MRB_API mrb_bool
mrb_eql(mrb_state *mrb, mrb_value obj1, mrb_value obj2)
{
  if (mrb_obj_eq(mrb, obj1, obj2)) return TRUE;
  return mrb_test(mrb_funcall_argv(mrb, obj1, MRB_SYM_Q(eql), 1, &obj2));
}

/*
 * Returns the receiver object itself.
 *
 * This function simply returns the mruby value `self` that it was passed.
 * It corresponds to the `Object#itself` method in Ruby, which is useful
 * in some functional programming patterns or for obtaining the object
 * itself in a chain of method calls.
 */
MRB_API mrb_value
mrb_obj_itself(mrb_state *mrb, mrb_value self)
{
  return self;
}
