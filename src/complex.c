/*
** numeric.c - Numeric, Integer, Float, Fixnum class
**
** See Copyright Notice in mruby.h
*/


#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>

#include "mruby.h"

#ifdef MRB_COMPLEX

static mrb_value
cpx_new(mrb_state *mrb, mrb_value self)
{
  mrb_float real, imag;
  int argc = mrb_get_args(mrb, "|ff", &real, &imag);
  if (argc == 0) real = 0.0;
  if (argc < 2) imag = 0.0;
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



#endif /* MRB_COMPLEX */

/* ------------------------------------------------------------------------*/
void
mrb_init_complex(mrb_state *mrb)
{
#ifdef MRB_COMPLEX
  struct RClass *complex;

  /* Complex Class */
  mrb_define_module_function(mrb, mrb->kernel_module, "Complex", cpx_new, MRB_ARGS_OPT(2));
  complex = mrb->complex_class = mrb_define_class(mrb, "Complex", mrb->numeric_class);
  mrb_undef_class_method(mrb,  complex, "new");
  mrb_define_method(mrb, complex,  "real",     cpx_real,          MRB_ARGS_NONE());
  mrb_define_method(mrb, complex,  "imag",     cpx_imag,          MRB_ARGS_NONE());

#endif /* MRB_COMPLEX */
}

