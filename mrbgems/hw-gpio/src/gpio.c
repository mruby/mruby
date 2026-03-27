#include <mruby.h>
#include <mruby/presym.h>
#include <mruby/variable.h>
#include <mruby/gpio.h>

static mrb_value
mrb_gpio_m_init(mrb_state *mrb, mrb_value self)
{
  mrb_int pin;
  mrb_get_args(mrb, "i", &pin);
  mrb_gpio_init((uint8_t)pin);
  return mrb_nil_value();
}

/* GPIO.set_dir_at(pin, flags) */
static mrb_value
mrb_gpio_s_set_dir_at(mrb_state *mrb, mrb_value klass)
{
  mrb_int pin, flags;
  mrb_get_args(mrb, "ii", &pin, &flags);
  mrb_gpio_set_dir((uint8_t)pin, (uint8_t)flags);
  return mrb_nil_value();
}

/* GPIO.pull_up_at(pin) */
static mrb_value
mrb_gpio_s_pull_up_at(mrb_state *mrb, mrb_value klass)
{
  mrb_int pin;
  mrb_get_args(mrb, "i", &pin);
  mrb_gpio_pull_up((uint8_t)pin);
  return mrb_nil_value();
}

/* GPIO.pull_down_at(pin) */
static mrb_value
mrb_gpio_s_pull_down_at(mrb_state *mrb, mrb_value klass)
{
  mrb_int pin;
  mrb_get_args(mrb, "i", &pin);
  mrb_gpio_pull_down((uint8_t)pin);
  return mrb_nil_value();
}

/* GPIO.open_drain_at(pin) */
static mrb_value
mrb_gpio_s_open_drain_at(mrb_state *mrb, mrb_value klass)
{
  mrb_int pin;
  mrb_get_args(mrb, "i", &pin);
  mrb_gpio_open_drain((uint8_t)pin);
  return mrb_nil_value();
}

/* GPIO.read_at(pin) */
static mrb_value
mrb_gpio_s_read_at(mrb_state *mrb, mrb_value klass)
{
  mrb_int pin;
  mrb_get_args(mrb, "i", &pin);
  return mrb_fixnum_value(mrb_gpio_read((uint8_t)pin));
}

/* GPIO.write_at(pin, val) */
static mrb_value
mrb_gpio_s_write_at(mrb_state *mrb, mrb_value klass)
{
  mrb_int pin, val;
  mrb_get_args(mrb, "ii", &pin, &val);
  if (val != 0 && val != 1) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "value must be 0 or 1");
  }
  mrb_gpio_write((uint8_t)pin, (uint8_t)val);
  return mrb_nil_value();
}

/* GPIO#read */
static mrb_value
mrb_gpio_m_read(mrb_state *mrb, mrb_value self)
{
  mrb_int pin = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(pin)));
  return mrb_fixnum_value(mrb_gpio_read((uint8_t)pin));
}

/* GPIO#write(val) */
static mrb_value
mrb_gpio_m_write(mrb_state *mrb, mrb_value self)
{
  mrb_int val;
  mrb_get_args(mrb, "i", &val);
  if (val != 0 && val != 1) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "value must be 0 or 1");
  }
  mrb_int pin = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(pin)));
  mrb_gpio_write((uint8_t)pin, (uint8_t)val);
  return mrb_nil_value();
}

void
mrb_hw_gpio_gem_init(mrb_state *mrb)
{
  struct RClass *cls = mrb_define_class_id(mrb, MRB_SYM(GPIO), mrb->object_class);

  mrb_define_method_id(mrb, cls, MRB_SYM(__init), mrb_gpio_m_init, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(read), mrb_gpio_m_read, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(write), mrb_gpio_m_write, MRB_ARGS_REQ(1));

  mrb_define_class_method_id(mrb, cls, MRB_SYM(set_dir_at), mrb_gpio_s_set_dir_at, MRB_ARGS_REQ(2));
  mrb_define_class_method_id(mrb, cls, MRB_SYM(pull_up_at), mrb_gpio_s_pull_up_at, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, cls, MRB_SYM(pull_down_at), mrb_gpio_s_pull_down_at, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, cls, MRB_SYM(open_drain_at), mrb_gpio_s_open_drain_at, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, cls, MRB_SYM(read_at), mrb_gpio_s_read_at, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, cls, MRB_SYM(write_at), mrb_gpio_s_write_at, MRB_ARGS_REQ(2));
}

void
mrb_hw_gpio_gem_final(mrb_state *mrb)
{
}
