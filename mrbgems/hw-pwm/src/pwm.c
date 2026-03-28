#include <mruby.h>
#include <mruby/presym.h>
#include <mruby/variable.h>
#include <mruby/pwm.h>

static mrb_value
mrb_pwm_m_init(mrb_state *mrb, mrb_value self)
{
  mrb_int pin;
  mrb_get_args(mrb, "i", &pin);
  mrb_pwm_init((uint32_t)pin);
  return mrb_nil_value();
}

static void
apply_freq_duty(mrb_state *mrb, mrb_value self)
{
  uint32_t pin = (uint32_t)mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(pin)));
  mrb_float freq = mrb_as_float(mrb, mrb_iv_get(mrb, self, MRB_IVSYM(frequency)));
  mrb_float duty = mrb_as_float(mrb, mrb_iv_get(mrb, self, MRB_IVSYM(duty)));
  mrb_pwm_set_freq_duty(pin, (float)freq, (float)duty);
  mrb_pwm_set_enabled(pin, freq > 0);
}

/* PWM#frequency(freq) */
static mrb_value
mrb_pwm_m_frequency(mrb_state *mrb, mrb_value self)
{
  mrb_float freq;
  mrb_get_args(mrb, "f", &freq);
  mrb_iv_set(mrb, self, MRB_IVSYM(frequency), mrb_float_value(mrb, freq));
  apply_freq_duty(mrb, self);
  return mrb_float_value(mrb, freq);
}

/* PWM#period_us(us) */
static mrb_value
mrb_pwm_m_period_us(mrb_state *mrb, mrb_value self)
{
  mrb_int us;
  mrb_get_args(mrb, "i", &us);
  mrb_float freq = 1000000.0 / us;
  mrb_iv_set(mrb, self, MRB_IVSYM(frequency), mrb_float_value(mrb, freq));
  apply_freq_duty(mrb, self);
  return mrb_float_value(mrb, freq);
}

/* PWM#duty(pct) */
static mrb_value
mrb_pwm_m_duty(mrb_state *mrb, mrb_value self)
{
  mrb_float duty;
  mrb_get_args(mrb, "f", &duty);
  if (duty < 0.0) duty = 0.0;
  if (duty > 100.0) duty = 100.0;
  mrb_iv_set(mrb, self, MRB_IVSYM(duty), mrb_float_value(mrb, duty));
  apply_freq_duty(mrb, self);
  return mrb_float_value(mrb, duty);
}

/* PWM#pulse_width_us(us) */
static mrb_value
mrb_pwm_m_pulse_width_us(mrb_state *mrb, mrb_value self)
{
  mrb_int pw;
  mrb_get_args(mrb, "i", &pw);
  mrb_float freq = mrb_as_float(mrb, mrb_iv_get(mrb, self, MRB_IVSYM(frequency)));
  mrb_float duty = (mrb_float)pw / 10000.0 * freq;
  if (duty < 0.0) duty = 0.0;
  if (duty > 100.0) duty = 100.0;
  mrb_iv_set(mrb, self, MRB_IVSYM(duty), mrb_float_value(mrb, duty));
  apply_freq_duty(mrb, self);
  return mrb_float_value(mrb, duty);
}

void
mrb_hw_pwm_gem_init(mrb_state *mrb)
{
  struct RClass *cls = mrb_define_class_id(mrb, MRB_SYM(PWM), mrb->object_class);
  mrb_define_method_id(mrb, cls, MRB_SYM(__init), mrb_pwm_m_init, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(frequency), mrb_pwm_m_frequency, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(period_us), mrb_pwm_m_period_us, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(duty), mrb_pwm_m_duty, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(pulse_width_us), mrb_pwm_m_pulse_width_us, MRB_ARGS_REQ(1));
}

void
mrb_hw_pwm_gem_final(mrb_state *mrb)
{
}
