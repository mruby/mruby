/*
** complex.c - Complex class
**
** See Copyright Notice in mruby.h
*/


#include <float.h>
#include <math.h>
#include <stdlib.h>

#include "mruby.h"
#include "mruby/string.h"
#include "mruby/numeric.h"

#ifdef MRB_COMPLEX

mrb_noreturn static void
cpx_raise_not_numeric_error(mrb_state *mrb)
{
  mrb_raise(mrb, E_TYPE_ERROR, "non complex value");
}

static void
cpx_print_to_string(mrb_state *mrb, mrb_value str, mrb_value self)
{
  mrb_str_concat(mrb, str, mrb_float_value(mrb, mrb_real(self)));
  if (mrb_imag(self) >= 0 || isnan(mrb_imag(self)))
    mrb_str_cat_lit(mrb, str, "+");
  mrb_str_concat(mrb, str, mrb_float_value(mrb, mrb_imag(self)));
  if (!isfinite(mrb_imag(self)))
    mrb_str_cat_lit(mrb, str, "*");
  mrb_str_cat_lit(mrb, str, "i");
}

static mrb_value
cpx_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_value str = mrb_str_buf_new(mrb, 48);

  cpx_print_to_string(mrb, str, self);
  return str;
}

static mrb_value
cpx_inspect(mrb_state *mrb, mrb_value self)
{
  mrb_value str = mrb_str_buf_new(mrb, 48);

  mrb_str_cat_lit(mrb, str, "(");
  cpx_print_to_string(mrb, str, self);
  mrb_str_cat_lit(mrb, str, ")");
  return str;
}

static mrb_value
cpx_new(mrb_state *mrb, mrb_value self)
{
  mrb_float real, imag;
  int argc = mrb_get_args(mrb, "f|f", &real, &imag);
  if (argc == 1) imag = 0.0;
  return mrb_complex_value(mrb, real, imag);
}

static mrb_value
cpx_real(mrb_state *mrb, mrb_value self)
{
  return mrb_float_value(mrb, mrb_real(self));
}

static mrb_value
cpx_imag(mrb_state *mrb, mrb_value self)
{
  return mrb_float_value(mrb, mrb_imag(self));
}

static mrb_value
cpx_equal(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_float oreal, oimag;

  mrb_get_args(mrb, "o", &other);
  switch (mrb_type(other)) {
  case MRB_TT_COMPLEX:
    oreal = mrb_real(other);
    oimag = mrb_imag(other);
    break;
  case MRB_TT_FIXNUM:
    oreal = (mrb_float)mrb_fixnum(other);
    oimag = 0;
    break;
  case MRB_TT_FLOAT:
    oreal = mrb_float(other);
    oimag = 0;
    break;
  default:
    return mrb_false_value();
  }
  return mrb_bool_value(mrb_imag(self) == oimag && mrb_real(self) == oreal);
}

static mrb_value
cpx_eql(mrb_state *mrb, mrb_value self)
{
  mrb_value other;

  mrb_get_args(mrb, "o", &other);
  if (!mrb_complex_p(other)) return mrb_false_value();
  return mrb_bool_value(mrb_imag(self) == mrb_imag(other) && mrb_real(self) == mrb_real(other));
}

static mrb_value
cpx_plus(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_float oreal;

  mrb_get_args(mrb, "o", &other);
  switch (mrb_type(other)) {
  case MRB_TT_COMPLEX:
    return mrb_complex_value(mrb,
      mrb_real(self) + mrb_real(other),
      mrb_imag(self) + mrb_imag(other)
    );
    break;
  case MRB_TT_FIXNUM:
    oreal = (mrb_float)mrb_fixnum(other);
    break;
  case MRB_TT_FLOAT:
    oreal = mrb_float(other);
    break;
  default:
    cpx_raise_not_numeric_error(mrb);
  }
  return mrb_complex_value(mrb,
    mrb_real(self) + oreal,
    mrb_imag(self)
  );
}

static mrb_value
cpx_minus(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_float oreal;

  mrb_get_args(mrb, "o", &other);
  switch (mrb_type(other)) {
  case MRB_TT_COMPLEX:
    return mrb_complex_value(mrb,
      mrb_real(self) - mrb_real(other),
      mrb_imag(self) - mrb_imag(other)
    );
    break;
  case MRB_TT_FIXNUM:
    oreal = (mrb_float)mrb_fixnum(other);
    break;
  case MRB_TT_FLOAT:
    oreal = mrb_float(other);
    break;
  default:
    cpx_raise_not_numeric_error(mrb);
  }
  return mrb_complex_value(mrb,
    mrb_real(self) - oreal,
    mrb_imag(self)
  );
}

static mrb_value
cpx_mul(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_float oreal;

  mrb_get_args(mrb, "o", &other);
  switch (mrb_type(other)) {
  case MRB_TT_COMPLEX:
    return mrb_complex_value(mrb,
      mrb_real(self) * mrb_real(other) - mrb_imag(self) * mrb_imag(other),
      mrb_imag(self) * mrb_real(other) + mrb_real(self) * mrb_imag(other)
      );
    break;
  case MRB_TT_FIXNUM:
    oreal = (mrb_float)mrb_fixnum(other);
    break;
  case MRB_TT_FLOAT:
    oreal = mrb_float(other);
    break;
  default:
    cpx_raise_not_numeric_error(mrb);
  }
  return mrb_complex_value(mrb,
    mrb_real(self) * oreal,
    mrb_imag(self) * oreal
  );
}

static mrb_value
cpx_div(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_float oreal;

  mrb_get_args(mrb, "o", &other);
  switch (mrb_type(other)) {
  case MRB_TT_COMPLEX:
    if (fabs(mrb_real(other)) > fabs(mrb_imag(other))) {
      mrb_float r, n;
      r = mrb_imag(other) / mrb_real(other);
      n = mrb_real(other) * (r*r+1);
      return mrb_complex_value(mrb,
        (mrb_real(self) + (mrb_imag(self) * r)) / n,
        (mrb_imag(self) - (mrb_real(self) * r)) / n
      );
    } else {
      mrb_float r, n;
      r = mrb_real(other) / mrb_imag(other);
      n = mrb_imag(other) * (r*r+1);
      return mrb_complex_value(mrb,
        ((mrb_real(self) * r) + mrb_imag(self)) / n,
        ((mrb_imag(self) * r) - mrb_real(self)) / n
      );
    }
  case MRB_TT_FIXNUM:
    oreal = (mrb_float)mrb_fixnum(other);
    break;
  case MRB_TT_FLOAT:
    oreal = mrb_float(other);
    break;
  default:
    cpx_raise_not_numeric_error(mrb);
  }
  return mrb_complex_value(mrb,
    mrb_real(self) / oreal,
    mrb_imag(self) / oreal
  );
}
#endif

/* ------------------------------------------------------------------------*/
void
mrb_init_complex(mrb_state *mrb)
{
#ifdef MRB_COMPLEX
  struct RClass *complex, *numeric;
  
#ifndef MRB_WORD_BOXING
  /* When not word boxing, make sure the binary representation of
   * mrb_value is identical to C99 and C++ complex numbers.
   */
#ifdef MRB_USE_FLOAT
  mrb_static_assert(sizeof(void*) == 4, "when using MRB_FLOAT and MRB_COMPLEX, sizeof pointer must be 4 bytes");
  mrb_static_assert(sizeof(mrb_value) == 8, "sizeof mrb_value is not 8");
#else
  mrb_static_assert(sizeof(mrb_value) == 16, "sizeof mrb_value is not 16");
#endif
#endif
  
  /* Complex() initializer */
  mrb_define_module_function(mrb, mrb->kernel_module, "Complex", cpx_new, MRB_ARGS_ARG(1,1));
  /* Complex Class */
  numeric = mrb_class_get(mrb, "Numeric");
  complex = mrb->complex_class = mrb_define_class(mrb, "Complex", numeric);
  mrb_undef_class_method(mrb,  complex, "new");
  mrb_undef_method(mrb, complex, "<=>");
  mrb_undef_method(mrb, complex, "**");
  mrb_define_method(mrb, complex, "to_s", cpx_to_s, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, complex, "inspect", cpx_inspect, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, complex, "==", cpx_equal, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, complex, "eql?", cpx_eql, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, complex, "real", cpx_real, MRB_ARGS_NONE());
  mrb_define_method(mrb, complex, "imag", cpx_imag, MRB_ARGS_NONE());
  mrb_define_method(mrb, complex, "+", cpx_plus, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, complex, "-", cpx_minus, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, complex, "*", cpx_mul, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, complex, "/", cpx_div, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, complex, "quo", cpx_div, MRB_ARGS_REQ(1));
#endif
}
