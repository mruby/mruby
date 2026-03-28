#include <mruby.h>
#include <mruby/presym.h>
#include <mruby/variable.h>
#include <mruby/adc.h>

/* ADC#__init(pin) */
static mrb_value
mrb_adc_m_init(mrb_state *mrb, mrb_value self)
{
  mrb_int pin;
  mrb_get_args(mrb, "i", &pin);
  int input = mrb_adc_init((uint8_t)pin);
  if (input < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid ADC pin");
  }
  return mrb_fixnum_value(input);
}

/* ADC#read_raw */
static mrb_value
mrb_adc_m_read_raw(mrb_state *mrb, mrb_value self)
{
  mrb_int input = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(input)));
  return mrb_fixnum_value(mrb_adc_read_raw((uint8_t)input));
}

/* ADC#read_voltage (also aliased as read) */
static mrb_value
mrb_adc_m_read_voltage(mrb_state *mrb, mrb_value self)
{
  mrb_int input = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(input)));
  return mrb_float_value(mrb, (mrb_float)mrb_adc_read_voltage((uint8_t)input));
}

void
mrb_hw_adc_gem_init(mrb_state *mrb)
{
  struct RClass *cls = mrb_define_class_id(mrb, MRB_SYM(ADC), mrb->object_class);
  mrb_define_method_id(mrb, cls, MRB_SYM(__init), mrb_adc_m_init, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(read_raw), mrb_adc_m_read_raw, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(read_voltage), mrb_adc_m_read_voltage, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(read), mrb_adc_m_read_voltage, MRB_ARGS_NONE());
}

void
mrb_hw_adc_gem_final(mrb_state *mrb)
{
}
