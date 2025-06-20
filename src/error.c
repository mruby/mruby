/*
** error.c - Exception class
**
** See Copyright Notice in mruby.h
*/

#include <errno.h>
#include <stdlib.h>
#include <mruby.h>
#include <mruby/array.h>
#include <mruby/irep.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/class.h>
#include <mruby/throw.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

void
mrb_exc_mesg_set(mrb_state *mrb, struct RException *exc, mrb_value mesg)
{
  if (!mrb_string_p(mesg)) {
    mesg = mrb_obj_as_string(mrb, mesg);
  }
  exc->mesg = mrb_basic_ptr(mesg);
  mrb_field_write_barrier_value(mrb, (struct RBasic*)exc, mesg);
}

mrb_value
mrb_exc_mesg_get(mrb_state *mrb, struct RException *exc)
{
  if (exc->mesg == NULL) return mrb_nil_value();
  return mrb_obj_value(exc->mesg);
}

MRB_API mrb_value
mrb_exc_new_str(mrb_state *mrb, struct RClass* c, mrb_value str)
{
  mrb_ensure_string_type(mrb, str);

  struct RException *e = MRB_OBJ_ALLOC(mrb, MRB_TT_EXCEPTION, c);
  mrb_value exc = mrb_obj_value(e);
  mrb_exc_mesg_set(mrb, e, str);
  return exc;
}

MRB_API mrb_value
mrb_exc_new(mrb_state *mrb, struct RClass *c, const char *ptr, mrb_int len)
{
  return mrb_exc_new_str(mrb, c, mrb_str_new(mrb, ptr, len));
}

/*
 * call-seq:
 *    Exception.new(msg = nil)   ->  exception
 *
 *  Construct a new Exception object, optionally passing in
 *  a message.
 */

static mrb_value
exc_initialize(mrb_state *mrb, mrb_value exc)
{
  mrb_value mesg;

  if (mrb_get_args(mrb, "|o", &mesg) == 1) {
    mrb_exc_mesg_set(mrb, mrb_exc_ptr(exc), mesg);
  }
  return exc;
}

/*
 *  Document-method: exception
 *
 *  call-seq:
 *     exc.exception(string)  ->  an_exception or exc
 *
 *  With no argument, or if the argument is the same as the receiver,
 *  return the receiver. Otherwise, create a new
 *  exception object of the same class as the receiver, but with a
 *  message equal to <code>string</code>.
 *
 */

static mrb_value
exc_exception(mrb_state *mrb, mrb_value self)
{
  mrb_value a;
  mrb_int argc = mrb_get_args(mrb, "|o", &a);

  if (argc == 0) return self;
  if (mrb_obj_equal(mrb, self, a)) return self;

  mrb_value exc = mrb_obj_clone(mrb, self);
  mrb_exc_mesg_set(mrb, mrb_exc_ptr(exc), a);

  return exc;
}

/*
 * call-seq:
 *   exception.to_s   ->  string
 *
 * Returns exception's message (or the name of the exception if
 * no message is set).
 */

static mrb_value
exc_to_s(mrb_state *mrb, mrb_value exc)
{
  mrb_value mesg = mrb_exc_mesg_get(mrb, mrb_exc_ptr(exc));

  if (!mrb_string_p(mesg)) {
    return mrb_str_new_cstr(mrb, mrb_obj_classname(mrb, exc));
  }
  struct RObject *p = mrb_obj_ptr(mesg);
  if (!p->c) {
    p->c = mrb->string_class;
  }
  return mesg;
}

/*
 * call-seq:
 *   exception.inspect   -> string
 *
 * Returns this exception's filename, line number,
 * message and class name.
 * If filename or line number is not set,
 * returns message and class name.
 */

mrb_value
mrb_exc_inspect(mrb_state *mrb, mrb_value exc)
{
  mrb_value cname = mrb_mod_to_s(mrb, mrb_obj_value(mrb_obj_class(mrb, exc)));
  mrb_value mesg = mrb_exc_mesg_get(mrb, mrb_exc_ptr(exc)); /* string or nil */
  return (mrb_nil_p(mesg)||RSTRING_LEN(mesg)==0) ? cname : mrb_format(mrb, "#<%v: %v>", cname, mesg);
}

mrb_value
mrb_exc_get_output(mrb_state *mrb, struct RObject *exc)
{
  mrb_value cname = mrb_mod_to_s(mrb, mrb_obj_value(mrb_class_real(exc->c)));
  mrb_value mesg = mrb_exc_mesg_get(mrb, (struct RException*)exc); /* string or nil */
  return (mrb_nil_p(mesg)||RSTRING_LEN(mesg)==0) ? cname : mrb_format(mrb, "%v (%v)", mesg, cname);
}

void mrb_keep_backtrace(mrb_state *mrb, mrb_value exc);

static void
set_backtrace(mrb_state *mrb, mrb_value exc, mrb_value backtrace)
{
  if (!mrb_array_p(backtrace)) {
  type_err:
    mrb_raise(mrb, E_TYPE_ERROR, "backtrace must be Array of String");
  }
  else {
    const mrb_value *p = RARRAY_PTR(backtrace);
    const mrb_value *pend = p + RARRAY_LEN(backtrace);

    while (p < pend) {
      if (!mrb_string_p(*p)) goto type_err;
      p++;
    }
  }
  mrb_exc_ptr(exc)->backtrace = mrb_basic_ptr(backtrace);
  mrb_field_write_barrier_value(mrb, mrb_basic_ptr(exc), backtrace);
}

static mrb_value
exc_set_backtrace(mrb_state *mrb, mrb_value exc)
{
  mrb_value backtrace = mrb_get_arg1(mrb);

  set_backtrace(mrb, exc, backtrace);
  return backtrace;
}

void
mrb_exc_set(mrb_state *mrb, mrb_value exc)
{
  if (mrb_nil_p(exc)) {
    mrb->exc = 0;
  }
  else {
    mrb->exc = mrb_obj_ptr(exc);
    if (mrb->gc.arena_idx > 0 &&
        (struct RBasic*)mrb->exc == mrb->gc.arena[mrb->gc.arena_idx-1]) {
      mrb->gc.arena_idx--;
    }
    if (!mrb->gc.out_of_memory && !mrb_frozen_p(mrb->exc)) {
      mrb_keep_backtrace(mrb, exc);
    }
  }
}

static mrb_noreturn void
exc_throw(mrb_state *mrb, mrb_value exc)
{
  if (!mrb->jmp) {
    mrb_print_error(mrb);
    abort();
  }
  MRB_THROW(mrb->jmp);
}

/*
 * Raises the given exception object.
 *
 * This function sets the provided exception object as the current
 * exception in the mruby state and then triggers the exception
 * handling mechanism (longjmp).
 *
 * If the provided object is a 'break' object, it's handled specially.
 * If it's not an exception object, a TypeError is raised.
 *
 * mrb: The mruby state.
 * exc: The exception object to raise.
 */
MRB_API mrb_noreturn void
mrb_exc_raise(mrb_state *mrb, mrb_value exc)
{
  if (mrb_break_p(exc)) {
    mrb->exc = mrb_obj_ptr(exc);
  }
  else {
    if (mrb_type(exc) != MRB_TT_EXCEPTION) {
      mrb_raise(mrb, E_TYPE_ERROR, "exception object expected");
    }
    mrb_exc_set(mrb, exc);
  }
  exc_throw(mrb, exc);
}

/*
 * Creates a new exception of class `c` with the message `msg` and raises it.
 *
 * This is a convenience function that combines creating an exception
 * from a C string and then raising it.
 *
 * mrb: The mruby state.
 * c:   The exception class to instantiate.
 * msg: The C string message for the exception.
 */
MRB_API mrb_noreturn void
mrb_raise(mrb_state *mrb, struct RClass *c, const char *msg)
{
  mrb_exc_raise(mrb, mrb_exc_new_str(mrb, c, mrb_str_new_cstr(mrb, msg)));
}

/*
 * Formats arguments according to a format string, similar to vsprintf.
 * This function is the core of mruby's string formatting capabilities.
 * It takes a format string and a va_list of arguments and returns a
 * new mruby string with the formatted result.
 *
 * The format string supports various specifiers to control how arguments
 * are converted to strings.
 *
 * Format Sequence Syntax:
 *   %[modifier]specifier
 *
 * Modifier:
 *   !  : Use the 'inspect' method for conversion instead of 'to_s'.
 *
 * Specifiers:
 *   c  : char
 *   d  : int (decimal)
 *   i  : mrb_int (decimal)
 *   f  : mrb_float
 *   l  : char* and size_t (string with length)
 *   n  : mrb_sym (symbol name)
 *   s  : char* (NUL-terminated C string)
 *   t  : mrb_value (type/class of the object)
 *   v,S: mrb_value (converted using to_s or inspect based on '!')
 *   C  : struct RClass* (class name)
 *   T  : mrb_value (real type/class of the object)
 *   Y  : mrb_value (uses 'inspect' if true, false, or nil, otherwise same as 'T')
 *   %  : Literal '%' character (no argument consumed)
 *
 * mrb: The mruby state.
 * format: The format string.
 * ap: The va_list of arguments.
 *
 * Returns a new mrb_value string containing the formatted output.
 * Raises ArgumentError if the format string is malformed.
 */
MRB_API mrb_value
mrb_vformat(mrb_state *mrb, const char *format, va_list ap)
{
  const char *chars, *p = format, *b = format, *e;
  char ch;
  size_t len;
  mrb_int i;
  struct RClass *cls;
  mrb_bool inspect = FALSE;
  mrb_value result = mrb_str_new_capa(mrb, 128), obj, str;
  int ai = mrb_gc_arena_save(mrb);

  while (*p) {
    const char c = *p++;
    e = p;
    if (c == '%') {
      if (*p == '!') {
        inspect = TRUE;
        p++;
      }
      if (!*p) break;
      switch (*p) {
        case 'c':
          ch = (char)va_arg(ap, int);
          chars = &ch;
          len = 1;
          goto L_cat;
        case 'd': case 'i':
#if MRB_INT_MAX < INT_MAX
          i = (mrb_int)va_arg(ap, int);
#else
          i = *p == 'd' ? (mrb_int)va_arg(ap, int) : va_arg(ap, mrb_int);
#endif
          obj = mrb_int_value(mrb, i);
          goto L_cat_obj;
#ifndef MRB_NO_FLOAT
        case 'f':
          obj = mrb_float_value(mrb, (mrb_float)va_arg(ap, double));
          goto L_cat_obj;
#endif
        case 'l':
          chars = va_arg(ap, char*);
          len = va_arg(ap, size_t);
        L_cat:
          if (inspect) {
            obj = mrb_str_new(mrb, chars, len);
            goto L_cat_obj;
          }
        L_cat_plain:
          mrb_str_cat(mrb, result, b,  e - b - 1);
          mrb_str_cat(mrb, result, chars, len);
          b = ++p;
          mrb_gc_arena_restore(mrb, ai);
          break;
        case 'n':
#if UINT32_MAX < INT_MAX
          obj = mrb_symbol_value((mrb_sym)va_arg(ap, int));
#else
          obj = mrb_symbol_value(va_arg(ap, mrb_sym));
#endif
          goto L_cat_obj;
        case 's':
          chars = va_arg(ap, char*);
          if (chars == NULL) chars = "(null)";
          len = strlen(chars);
          goto L_cat;
        case 't':
          cls = mrb_class(mrb, va_arg(ap, mrb_value));
          goto L_cat_class;
        case 'v': case 'S':
          obj = va_arg(ap, mrb_value);
        L_cat_obj:
          str = (inspect ? mrb_inspect : mrb_obj_as_string)(mrb, obj);
          if (mrb_type(str) != MRB_TT_STRING) {
            chars = "void (no string conversion)";
            len = strlen(chars);
          }
          else {
            chars = RSTRING_PTR(str);
            len = RSTRING_LEN(str);
          }
          goto L_cat_plain;
        case 'C':
          cls = va_arg(ap, struct RClass*);
        L_cat_class:
          obj = mrb_obj_value(cls);
          goto L_cat_obj;
        case 'T':
          obj = va_arg(ap, mrb_value);
        L_cat_real_class_of:
          cls = mrb_obj_class(mrb, obj);
          goto L_cat_class;
        case 'Y':
          obj = va_arg(ap, mrb_value);
          if (!mrb_test(obj) || mrb_true_p(obj)) {
            inspect = TRUE;
            goto L_cat_obj;
          }
          else {
            goto L_cat_real_class_of;
          }
        case '%':
        L_cat_current:
          chars = p;
          len = 1;
          goto L_cat_plain;
        default:
          mrb_raisef(mrb, E_ARGUMENT_ERROR, "malformed format string - %%%c", *p);
      }
    }
    else if (c == '\\') {
      if (!*p) break;
      goto L_cat_current;

    }
  }

  mrb_str_cat(mrb, result, b, p - b);
  return result;
}

/*
 * Formats arguments according to a format string, similar to sprintf.
 *
 * This function takes a format string and a variable number of arguments,
 * then calls mrb_vformat to perform the actual formatting.
 * See mrb_vformat for details on the format string specifiers.
 *
 * mrb: The mruby state.
 * format: The format string.
 * ...: Variable arguments to be formatted.
 *
 * Returns a new mrb_value string containing the formatted output.
 */
MRB_API mrb_value
mrb_format(mrb_state *mrb, const char *format, ...)
{
  va_list ap;

  va_start(ap, format);

  mrb_value str = mrb_vformat(mrb, format, ap);
  va_end(ap);

  return str;
}

static mrb_value
error_va(mrb_state *mrb, struct RClass *c, const char *fmt, va_list ap)
{
  return mrb_exc_new_str(mrb, c, mrb_vformat(mrb, fmt, ap));
}

/*
 * Creates a new exception of class `c` with a formatted message and raises it.
 *
 * This function formats a message string using `fmt` and the subsequent
 * variable arguments, then creates an exception of class `c` with this
 * message, and finally raises the exception.
 * See mrb_vformat for details on the format string specifiers.
 *
 * mrb: The mruby state.
 * c:   The exception class to instantiate.
 * fmt: The format string for the exception message.
 * ...: Variable arguments for the format string.
 */
MRB_API mrb_noreturn void
mrb_raisef(mrb_state *mrb, struct RClass *c, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);

  mrb_value exc = error_va(mrb, c, fmt, ap);
  va_end(ap);

  mrb_exc_raise(mrb, exc);
}

/*
 * Raises a NameError exception with a formatted message.
 *
 * This function creates a NameError exception. The message is generated
 * from `fmt` and the variable arguments. The symbol `id` (e.g., the name
 * of a missing constant or variable) is associated with the exception object
 * via an instance variable named 'name'.
 * See mrb_vformat for details on the format string specifiers.
 *
 * mrb: The mruby state.
 * id:  The symbol representing the name that caused the error.
 * fmt: The format string for the exception message.
 * ...: Variable arguments for the format string.
 */
MRB_API mrb_noreturn void
mrb_name_error(mrb_state *mrb, mrb_sym id, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);

  mrb_value exc = error_va(mrb, E_NAME_ERROR, fmt, ap);
  va_end(ap);
  mrb_iv_set(mrb, exc, MRB_IVSYM(name), mrb_symbol_value(id));
  mrb_exc_raise(mrb, exc);
}

/*
 * Prints a warning message to stderr.
 *
 * The message is formatted using `fmt` and the subsequent variable arguments.
 * The output is prefixed with "warning: " and followed by a newline.
 * This function does nothing if MRB_NO_STDIO is defined.
 * See mrb_vformat for details on the format string specifiers.
 *
 * mrb: The mruby state.
 * fmt: The format string for the warning message.
 * ...: Variable arguments for the format string.
 */
MRB_API void
mrb_warn(mrb_state *mrb, const char *fmt, ...)
{
#ifndef MRB_NO_STDIO
  va_list ap;

  va_start(ap, fmt);

  mrb_value str = mrb_vformat(mrb, fmt, ap);
  fputs("warning: ", stderr);
  fwrite(RSTRING_PTR(str), RSTRING_LEN(str), 1, stderr);
  putc('\n', stderr);
  va_end(ap);
#endif
}

/*
 * Reports an internal mruby bug, prints a message to stderr, and terminates the program.
 *
 * This function is called when an unexpected internal error occurs within mruby.
 * It prints the given message prefixed with "bug: " to stderr and then
 * calls exit(EXIT_FAILURE).
 * If MRB_NO_STDIO is defined, the message is not printed, but the program still exits.
 *
 * mrb: The mruby state (currently unused in the function body but part of the API).
 * mesg: The C string message describing the bug.
 */
MRB_API mrb_noreturn void
mrb_bug(mrb_state *mrb, const char *mesg)
{
#ifndef MRB_NO_STDIO
  fputs("bug: ", stderr);
  fputs(mesg, stderr);
  fputs("\n", stderr);
#endif
  exit(EXIT_FAILURE);
}

mrb_value
mrb_make_exception(mrb_state *mrb, mrb_value exc, mrb_value mesg)
{
  mrb_int n = 1;

  if (mrb_nil_p(mesg)) {
    n = 0;
  }
  if (mrb_class_p(exc)) {
    exc = mrb_funcall_argv(mrb, exc, MRB_SYM(new), n, &mesg);
  }
  else if (mrb_exception_p(exc)) {
    if (n > 0) {
      exc = mrb_obj_clone(mrb, exc);
      mrb_exc_mesg_set(mrb, mrb_exc_ptr(exc), mesg);
    }
  }
  else {
    mrb_raise(mrb, E_TYPE_ERROR, "exception class/object expected");
  }
  if (mrb_type(exc) != MRB_TT_EXCEPTION) {
    mrb_raise(mrb, E_EXCEPTION, "exception object expected");
  }
  return exc;
}

/*
 * Raises a SystemCallError if available, otherwise a RuntimeError,
 * based on the current `errno` value.
 *
 * If the SystemCallError class is defined, this function attempts to call
 * its `_sys_fail` method with the current `errno` and an optional
 * message. This typically results in a SystemCallError being raised.
 *
 * If SystemCallError is not defined, or if the call to `_sys_fail`
 * itself fails (which shouldn't happen in normal circumstances but leads
 * to mrb_raise), it falls back to raising a RuntimeError with the
 * given message (or a default message if `mesg` is NULL, though the
 * current implementation would pass NULL to mrb_raise which might be
 * an issue).
 *
 * mrb: The mruby state.
 * mesg: An optional C string message to append to the error. If NULL,
 *       a default message or no message might be used depending on the
 *       error path.
 */
MRB_API mrb_noreturn void
mrb_sys_fail(mrb_state *mrb, const char *mesg)
{
  if (mrb_class_defined_id(mrb, MRB_SYM(SystemCallError))) {
    struct RClass *sce = mrb_class_get_id(mrb, MRB_SYM(SystemCallError));
    mrb_int no = (mrb_int)errno;
    if (mesg != NULL) {
      mrb_funcall_id(mrb, mrb_obj_value(sce), MRB_SYM(_sys_fail), 2, mrb_fixnum_value(no), mrb_str_new_cstr(mrb, mesg));
    }
    else {
      mrb_funcall_id(mrb, mrb_obj_value(sce), MRB_SYM(_sys_fail), 1, mrb_fixnum_value(no));
    }
  }

  mrb_raise(mrb, E_RUNTIME_ERROR, mesg);
}

/*
 * Raises a NoMethodError exception with a formatted message.
 *
 * This function creates a NoMethodError. The message is generated from
 * `fmt` and the variable arguments. The symbol `id` (the name of the
 * missing method) and `args` (the arguments passed to the method)
 * are associated with the exception object via instance variables
 * named 'name' and 'args', respectively.
 * See mrb_vformat for details on the format string specifiers.
 *
 * mrb:  The mruby state.
 * id:   The symbol representing the name of the undefined method.
 * args: The arguments that were passed to the method call.
 * fmt:  The format string for the exception message.
 * ...:  Variable arguments for the format string.
 */
MRB_API mrb_noreturn void
mrb_no_method_error(mrb_state *mrb, mrb_sym id, mrb_value args, char const* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);

  mrb_value exc = error_va(mrb, E_NOMETHOD_ERROR, fmt, ap);
  va_end(ap);
  mrb_iv_set(mrb, exc, MRB_IVSYM(name), mrb_symbol_value(id));
  mrb_iv_set(mrb, exc, MRB_IVSYM(args), args);
  mrb_exc_raise(mrb, exc);
}

static mrb_noreturn void
frozen_error(mrb_state *mrb, mrb_value v)
{
  mrb_raisef(mrb, E_FROZEN_ERROR, "can't modify frozen %T", v);
}

/*
 * Raises a FrozenError for the given frozen object.
 *
 * This function is called when an attempt is made to modify an object
 * that has been frozen. It constructs and raises a FrozenError,
 * indicating the specific object that could not be modified.
 *
 * mrb:        The mruby state.
 * frozen_obj: A pointer to the RBasic structure of the frozen object.
 */
MRB_API mrb_noreturn void
mrb_frozen_error(mrb_state *mrb, void *frozen_obj)
{
  frozen_error(mrb, mrb_obj_value(frozen_obj));
}

/*
 * Checks if the given object is frozen. If it is, raises a FrozenError.
 *
 * This utility function is used before attempting an operation that
 * would modify an object, to ensure that the operation is allowed.
 *
 * mrb: The mruby state.
 * o:   A pointer to the RBasic structure of the object to check.
 */
MRB_API void
mrb_check_frozen(mrb_state *mrb, void *o)
{
  if (mrb_frozen_p((struct RBasic*)o)) {
    mrb_frozen_error(mrb, o);
  }
}

/*
 * Checks if the given mrb_value refers to a frozen object.
 * If it is frozen, or if it's an immediate value (which are implicitly
 * unmodifiable in a way that would trigger a FrozenError for heap objects),
 * this function raises a FrozenError.
 *
 * Note: The check `mrb_immediate_p(v)` combined with `frozen_error`
 * might be misleading. Immediate values are not "frozen" in the same
 * sense as heap objects. This function effectively raises a FrozenError
 * if an attempt is made to modify an immediate value or a
 * heap-allocated object that is explicitly frozen.
 *
 * mrb: The mruby state.
 * v:   The mrb_value to check.
 */
MRB_API void
mrb_check_frozen_value(mrb_state *mrb, mrb_value v)
{
  if (mrb_immediate_p(v) || mrb_frozen_p(mrb_basic_ptr(v))) {
    frozen_error(mrb, v);
  }
}

/*
 * Raises an ArgumentError indicating a mismatch in the number of arguments.
 *
 * This function is used to report errors when a method receives an
 * incorrect number of arguments. It formats a message specifying the
 * number of arguments received (`argc`) and the expected number,
 * which can be an exact number (`min` == `max`), a minimum (`max` < 0),
 * or a range (`min` to `max`).
 *
 * mrb: The mruby state.
 * argc: The number of arguments actually received.
 * min: The minimum number of arguments expected.
 * max: The maximum number of arguments expected. If negative, it means
 *      `min` or more arguments are expected.
 */
MRB_API mrb_noreturn void
mrb_argnum_error(mrb_state *mrb, mrb_int argc, int min, int max)
{
#define FMT(exp) "wrong number of arguments (given %i, expected " exp ")"
  if (min == max)
    mrb_raisef(mrb, E_ARGUMENT_ERROR, FMT("%d"), argc, min);
  else if (max < 0)
    mrb_raisef(mrb, E_ARGUMENT_ERROR, FMT("%d+"), argc, min);
  else
    mrb_raisef(mrb, E_ARGUMENT_ERROR, FMT("%d..%d"), argc, min, max);
#undef FMT
}

void mrb_core_init_printabort(mrb_state *mrb);

int
mrb_core_init_protect(mrb_state *mrb, void (*body)(mrb_state*, void*), void *opaque)
{
  struct mrb_jmpbuf *prev_jmp = mrb->jmp;
  struct mrb_jmpbuf c_jmp;
  volatile int err = 1;

  MRB_TRY(&c_jmp) {
    mrb->jmp = &c_jmp;
    body(mrb, opaque);
    err = 0;
  } MRB_CATCH(&c_jmp) {
    if (mrb->exc) {
      mrb_print_error(mrb);
      mrb->exc = NULL;
    }
    else {
      mrb_core_init_printabort(mrb);
    }
  } MRB_END_EXC(&c_jmp);

  mrb->jmp = prev_jmp;

  return err;
}

mrb_noreturn void
mrb_core_init_abort(mrb_state *mrb)
{
  mrb->exc = NULL;
  exc_throw(mrb, mrb_nil_value());
}

void
mrb_protect_atexit(mrb_state *mrb)
{
  if (mrb->atexit_stack_len > 0) {
    if (mrb->c && mrb->c->ci) {
      // Even if the call stack is incomplete due to some fault, atexit to be executed at the top level is desirable.
      // Clean-up also makes it easier to collect unnecessary objects.
      mrb_callinfo zero = { 0 };
      struct mrb_context *c = mrb->c = mrb->root_c;
      mrb_gc_arena_restore(mrb, 0);

      if (c->ci == c->cibase) {
        // Since there is no problem with the ci, the env object is detached normally.
        struct REnv *e = mrb_vm_ci_env(c->ci);
        *c->ci = zero;
        c->ci->stack = c->stbase;
        if (e) {
          c->ci->u.env = NULL;
          mrb_env_unshare(mrb, e, TRUE);
        }
      }
      else {
        // Any env objects on the ci that are in the process of being executed are destroyed.
        do {
          struct REnv *e = mrb_vm_ci_env(c->ci);
          if (e) {
            e->stack = NULL;
            MRB_ENV_SET_LEN(e, 0);
            MRB_ENV_SET_BIDX(e, 0);
            MRB_ENV_CLOSE(e);
          }
        } while (c->ci-- > c->cibase);
        c->ci = c->cibase;
        *c->ci = zero;
        c->ci->stack = c->stbase;
      }
    }

    struct mrb_jmpbuf *prev_jmp = mrb->jmp;
    struct mrb_jmpbuf c_jmp;
    int i = mrb->atexit_stack_len;
    while (i > 0) {
      MRB_TRY(&c_jmp) {
        mrb->jmp = &c_jmp;
        do {
          mrb->atexit_stack[--i](mrb);
          mrb_gc_arena_restore(mrb, 0);
        } while (i > 0);
        mrb->jmp = prev_jmp;
      } MRB_CATCH(&c_jmp) {
        mrb->jmp = prev_jmp;
        /* ignore atexit errors */
        mrb_gc_arena_restore(mrb, 0);
      } MRB_END_EXC(&c_jmp);
    }
#ifndef MRB_FIXED_STATE_ATEXIT_STACK
    mrb_free(mrb, mrb->atexit_stack);
#endif
  }
}

mrb_noreturn void
mrb_raise_nomemory(mrb_state *mrb)
{
  if (mrb->nomem_err) {
    mrb_exc_raise(mrb, mrb_obj_value(mrb->nomem_err));
  }
  else {
    mrb_core_init_abort(mrb);
  }
}

/*
 * Prints the current exception and its backtrace to stderr.
 *
 * If an exception is set in the mruby state (`mrb->exc`), this function
 * attempts to print its details, including the class name, message,
 * and backtrace.
 * It takes precautions to handle potential errors during the backtrace
 * printing itself, especially if called from a context without an active
 * jump buffer (e.g., top-level error).
 * This function does nothing if MRB_NO_STDIO is defined.
 *
 * mrb: The mruby state.
 */
MRB_API void
mrb_print_error(mrb_state *mrb)
{
#ifndef MRB_NO_STDIO
  if (mrb->jmp == NULL) {
    struct mrb_jmpbuf c_jmp;
    MRB_TRY(&c_jmp) {
      mrb->jmp = &c_jmp;
      mrb_print_backtrace(mrb);
    } MRB_CATCH(&c_jmp) {
      /* ignore exception during print_backtrace() */
    } MRB_END_EXC(&c_jmp);
    mrb->jmp = NULL;
  }
  else {
    mrb_print_backtrace(mrb);
  }
#endif
}

/*
 * Clears the current exception status in the mruby state.
 *
 * After this function is called, `mrb->exc` will be NULL, indicating
 * that there is no pending exception.
 *
 * mrb: The mruby state.
 */
MRB_API void
mrb_clear_error(mrb_state *mrb)
{
  mrb->exc = NULL;
}

/* returns TRUE if error in the previous call; internally calls mrb_clear_error() */
MRB_API mrb_bool
mrb_check_error(mrb_state *mrb)
{
  if (mrb->exc) {
    mrb_clear_error(mrb);
    return TRUE;
  }
  return FALSE;
}

void
mrb_init_exception(mrb_state *mrb)
{
  struct RClass *exception = mrb->eException_class = mrb_define_class_id(mrb, MRB_SYM(Exception), mrb->object_class); /* 15.2.22 */
  MRB_SET_INSTANCE_TT(exception, MRB_TT_EXCEPTION);
  mrb_define_class_method_id(mrb, exception, MRB_SYM(exception), mrb_instance_new,  MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, exception, MRB_SYM(exception),       exc_exception,     MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, exception, MRB_SYM(initialize),      exc_initialize,    MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, exception, MRB_SYM(to_s),            exc_to_s,          MRB_ARGS_NONE());
  mrb_define_method_id(mrb, exception, MRB_SYM(message),         exc_to_s,          MRB_ARGS_NONE());
  mrb_define_method_id(mrb, exception, MRB_SYM(inspect),         mrb_exc_inspect,   MRB_ARGS_NONE());
  mrb_define_method_id(mrb, exception, MRB_SYM(backtrace),       mrb_exc_backtrace, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, exception, MRB_SYM(set_backtrace),   exc_set_backtrace, MRB_ARGS_REQ(1));

  mrb->eStandardError_class = mrb_define_class_id(mrb, MRB_SYM(StandardError), mrb->eException_class); /* 15.2.23 */
  mrb_define_class_id(mrb, MRB_SYM(ArgumentError), E_STANDARD_ERROR);                                  /* 15.2.24 */
  mrb_define_class_id(mrb, MRB_SYM(LocalJumpError), E_STANDARD_ERROR);                                 /* 15.2.25 */
  struct RClass *range_error = mrb_define_class_id(mrb, MRB_SYM(RangeError), E_STANDARD_ERROR);        /* 15.2.26 */
  mrb_define_class_id(mrb, MRB_SYM(FloatDomainError), range_error);
  mrb_define_class_id(mrb, MRB_SYM(RegexpError), E_STANDARD_ERROR);                                    /* 15.2.27 */
  struct RClass *runtime_error = mrb_define_class_id(mrb, MRB_SYM(RuntimeError), E_STANDARD_ERROR);    /* 15.2.28 */
  mrb_define_class_id(mrb, MRB_SYM(FrozenError), runtime_error);
  mrb_define_class_id(mrb, MRB_SYM(TypeError), E_STANDARD_ERROR);                                      /* 15.2.29 */
  mrb_define_class_id(mrb, MRB_SYM(ZeroDivisionError), E_STANDARD_ERROR);                              /* 15.2.30 */
  struct RClass *script_error = mrb_define_class_id(mrb, MRB_SYM(ScriptError), exception);             /* 15.2.37 */
  mrb_define_class_id(mrb, MRB_SYM(NotImplementedError), script_error);
  mrb_define_class_id(mrb, MRB_SYM(SyntaxError), script_error);                                        /* 15.2.38 */
  struct RClass *index_error = mrb_define_class_id(mrb, MRB_SYM(IndexError), E_STANDARD_ERROR);        /* 15.2.33 */
  mrb_define_class_id(mrb, MRB_SYM(KeyError), index_error);
  struct RClass *stack_error = mrb_define_class_id(mrb, MRB_SYM(SystemStackError), exception);
  mrb->stack_err = mrb_obj_ptr(mrb_exc_new_lit(mrb, stack_error, "stack level too deep"));

  struct RClass *nomem_error = mrb_define_class_id(mrb, MRB_SYM(NoMemoryError), exception);
  mrb->nomem_err = mrb_obj_ptr(mrb_exc_new_lit(mrb, nomem_error, "Out of memory"));
#ifdef MRB_GC_FIXED_ARENA
  mrb->arena_err = mrb_obj_ptr(mrb_exc_new_lit(mrb, nomem_error, "arena overflow error"));
#endif
}
