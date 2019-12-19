#include <string.h>
#include <mruby.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/string.h>

/* no argument */
static mrb_value
vf_s_format_0(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str;
  const char *fmt;

  mrb_get_args(mrb, "S", &fmt_str);
  fmt = RSTRING_CSTR(mrb, fmt_str);

  return mrb_format(mrb, fmt);
}

/* c char */
static mrb_value
vf_s_format_c(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_str;
  const char *fmt;
  char c;
  
  mrb_get_args(mrb, "SS", &fmt_str, &arg_str);
  fmt = RSTRING_CSTR(mrb, fmt_str);
  c = RSTRING_CSTR(mrb, arg_str)[0];

  return mrb_format(mrb, fmt, c);
}

/* d int */
static mrb_value
vf_s_format_d(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_int;
  const char *fmt;
  int d;

  mrb_get_args(mrb, "Si", &fmt_str, &arg_int);
  fmt = RSTRING_CSTR(mrb, fmt_str);
  d = mrb_fixnum(arg_int);

  return mrb_format(mrb, fmt, d);
}

#ifndef MRB_WITHOUT_FLOAT
/* f float */
static mrb_value
vf_s_format_f(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_flt;
  const char *fmt;
  mrb_float f;

  mrb_get_args(mrb, "Sf", &fmt_str, &arg_flt);
  fmt = RSTRING_CSTR(mrb, fmt_str);
  f = mrb_float(arg_flt);

  return mrb_format(mrb, fmt, f);
}
#endif

/* i fixnum */
static mrb_value
vf_s_format_i(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_int;
  const char *fmt;
  mrb_int i;

  mrb_get_args(mrb, "Si", &fmt_str, &arg_int);
  fmt = RSTRING_CSTR(mrb, fmt_str);
  i = mrb_fixnum(arg_int);

  return mrb_format(mrb, fmt, i);
}

/* l char*, size_t */
static mrb_value
vf_s_format_l(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_str, arg_int;
  const char *fmt;
  const char *s;
  size_t len;

  mrb_get_args(mrb, "SSi", &fmt_str, &arg_str, &arg_int);
  fmt = RSTRING_CSTR(mrb, fmt_str);
  s = RSTRING_PTR(arg_str);
  len = (size_t)mrb_fixnum(arg_int);
  if (len > (size_t)RSTRING_LEN(arg_str)) len = (size_t)RSTRING_LEN(arg_str);

  return mrb_format(mrb, fmt, s, len);
}

/* n symbol */
static mrb_value
vf_s_format_n(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_sym;
  const char *fmt;
  mrb_sym n;

  mrb_get_args(mrb, "Sn", &fmt_str, &arg_sym);
  fmt = RSTRING_CSTR(mrb, fmt_str);
  n = mrb_symbol(arg_sym);

  return mrb_format(mrb, fmt, n);
}

/* s char* */
static mrb_value
vf_s_format_s(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_str;
  const char *fmt;
  const char *s;

  mrb_get_args(mrb, "SS", &fmt_str, &arg_str);
  fmt = RSTRING_CSTR(mrb, fmt_str);
  s = RSTRING_CSTR(mrb, arg_str);

  return mrb_format(mrb, fmt, s);
}

/* C RClass */
static mrb_value
vf_s_format_C(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_cls;
  const char *fmt;
  struct RClass *c;

  mrb_get_args(mrb, "SC", &fmt_str, &arg_cls);
  fmt = RSTRING_CSTR(mrb, fmt_str);
  c = mrb_class_ptr(arg_cls);

  return mrb_format(mrb, fmt, c);
}

/* v value */
static mrb_value
vf_s_format_v(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, arg_v;
  const char *fmt;

  mrb_get_args(mrb, "So", &fmt_str, &arg_v);
  fmt = RSTRING_CSTR(mrb, fmt_str);

  return mrb_format(mrb, fmt, arg_v);
}

void
mrb_init_test_vformat(mrb_state *mrb)
{
  struct RClass *vf;

  vf = mrb_define_module(mrb, "TestVFormat");
  mrb_define_class_method(mrb, vf, "z", vf_s_format_0, MRB_ARGS_REQ(1));

#define VF_DEFINE_FORMAT_METHOD(t) VF_DEFINE_FORMAT_METHOD_n(t,2)
#define VF_DEFINE_FORMAT_METHOD_n(t,n) mrb_define_class_method(mrb, vf, #t, vf_s_format_##t, MRB_ARGS_REQ(n));
  
  VF_DEFINE_FORMAT_METHOD(c);
  VF_DEFINE_FORMAT_METHOD(d);
#ifndef MRB_WITHOUT_FLOAT
  VF_DEFINE_FORMAT_METHOD(f);
#endif
  VF_DEFINE_FORMAT_METHOD(i);
  VF_DEFINE_FORMAT_METHOD_n(l,3);
  VF_DEFINE_FORMAT_METHOD(n);
  VF_DEFINE_FORMAT_METHOD(s);
  VF_DEFINE_FORMAT_METHOD(C);
  VF_DEFINE_FORMAT_METHOD(v);
}
