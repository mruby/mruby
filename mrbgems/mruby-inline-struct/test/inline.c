#include <mruby.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/inline.h>

static mrb_value
inline_test_initialize(mrb_state *mrb, mrb_value self)
{
  char *string = mrb_inline_ptr(self);
  mrb_int size = mrb_inline_size();
  mrb_value object;
  mrb_get_args(mrb, "o", &object);

  if (mrb_float_p(object))
  {
    snprintf(string, size, "float(%.3f)", mrb_float(object));
  }
  else if (mrb_fixnum_p(object))
  {
    snprintf(string, size, "fixnum(%d)", mrb_fixnum(object));
  }
  else if (mrb_string_p(object))
  {
    snprintf(string, size, "string(%s)", mrb_string_value_cstr(mrb, &object));
  }

  string[size - 1] = 0; // force NULL at the end
  return self;
}

static mrb_value
inline_test_to_s(mrb_state *mrb, mrb_value self)
{
  return mrb_str_new_cstr(mrb, mrb_inline_ptr(self));
}

static mrb_value
inline_test_length(mrb_state *mrb, mrb_value self)
{
  return mrb_fixnum_value(mrb_inline_size());
}

static mrb_value
inline_test_test_receive(mrb_state *mrb, mrb_value self)
{
  mrb_value object;
  mrb_get_args(mrb, "o", &object);
  if (mrb_obj_class(mrb, object) != mrb_class_get(mrb, "InlineStructTest"))
  {
    mrb_raisef(mrb, E_TYPE_ERROR, "Expected InlineStructTest");
  }
  return mrb_bool_value(((char*)mrb_inline_ptr(object))[0] == 's');
}

static mrb_value
inline_test_test_receive_direct(mrb_state *mrb, mrb_value self)
{
  char *ptr;
  mrb_get_args(mrb, "I", &ptr);
  return mrb_bool_value(ptr[0] == 's');
}

static mrb_value
inline_test_mutate(mrb_state *mrb, mrb_value self)
{
  char *ptr = mrb_inline_ptr(self);
  memcpy(ptr, "mutate", 6);
  return mrb_nil_value();
}

void mrb_mruby_inline_struct_gem_test(mrb_state *mrb)
{
  struct RClass *cls;

  cls = mrb_define_class(mrb, "InlineStructTest", mrb->object_class);
  MRB_SET_INSTANCE_TT(cls, MRB_TT_INLINE);
  mrb_define_method(mrb, cls, "initialize", inline_test_initialize, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, cls, "to_s", inline_test_to_s, MRB_ARGS_NONE());
  mrb_define_method(mrb, cls, "mutate", inline_test_mutate, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, cls, "length", inline_test_length, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, cls, "test_receive", inline_test_test_receive, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, cls, "test_receive_direct", inline_test_test_receive_direct, MRB_ARGS_REQ(1));
}
