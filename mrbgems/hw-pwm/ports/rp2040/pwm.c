#include "pico/stdlib.h"
#include "hardware/pwm.h"
#include <mruby/pwm.h>

#define APB_CLK_FREQ 125000000
#define CLK_DIV      100.0f

void
mrb_pwm_init(uint32_t pin)
{
  gpio_set_function(pin, GPIO_FUNC_PWM);
  uint slice = pwm_gpio_to_slice_num(pin);
  pwm_set_clkdiv(slice, CLK_DIV);
}

void
mrb_pwm_set_freq_duty(uint32_t pin, float frequency, float duty)
{
  uint slice = pwm_gpio_to_slice_num(pin);
  uint channel = pwm_gpio_to_channel(pin);
  float period = 1.0f / frequency;
  uint16_t wrap = (uint16_t)(period * APB_CLK_FREQ / CLK_DIV);
  pwm_set_wrap(slice, wrap);
  uint16_t level = (uint16_t)(wrap * duty / 100.0f);
  pwm_set_chan_level(slice, channel, level);
}

void
mrb_pwm_set_enabled(uint32_t pin, bool enabled)
{
  uint slice = pwm_gpio_to_slice_num(pin);
  pwm_set_enabled(slice, enabled);
}
