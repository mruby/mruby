#include "driver/ledc.h"
#include <mruby/pwm.h>

#define DUTY_RESOLUTION LEDC_TIMER_14_BIT

static int8_t channel_for_gpio[GPIO_NUM_MAX];
static int next_channel;

void
mrb_pwm_init(uint32_t gpio)
{
  if (gpio >= GPIO_NUM_MAX) return;
  if (next_channel >= LEDC_CHANNEL_MAX) return;

  ledc_timer_config_t timer_cfg = {
    .speed_mode = LEDC_LOW_SPEED_MODE,
    .timer_num = LEDC_TIMER_0,
    .duty_resolution = DUTY_RESOLUTION,
    .freq_hz = 1000,
    .clk_cfg = LEDC_AUTO_CLK,
  };
  ledc_timer_config(&timer_cfg);

  ledc_channel_config_t ch_cfg = {
    .gpio_num = gpio,
    .speed_mode = LEDC_LOW_SPEED_MODE,
    .channel = next_channel,
    .timer_sel = LEDC_TIMER_0,
    .intr_type = LEDC_INTR_DISABLE,
    .duty = 0,
    .hpoint = 0,
  };
  ledc_channel_config(&ch_cfg);

  channel_for_gpio[gpio] = next_channel++;
}

void
mrb_pwm_set_freq_duty(uint32_t gpio, float frequency, float duty)
{
  if (gpio >= GPIO_NUM_MAX) return;

  ledc_set_freq(LEDC_LOW_SPEED_MODE, LEDC_TIMER_0, (uint32_t)frequency);

  int8_t ch = channel_for_gpio[gpio];
  uint32_t max_duty = (1 << DUTY_RESOLUTION) - 1;
  uint32_t d = (uint32_t)(duty * max_duty / 100.0f);
  ledc_set_duty(LEDC_LOW_SPEED_MODE, ch, d);
  ledc_update_duty(LEDC_LOW_SPEED_MODE, ch);
}

void
mrb_pwm_set_enabled(uint32_t gpio, bool enabled)
{
  if (!enabled) {
    int8_t ch = channel_for_gpio[gpio];
    ledc_stop(LEDC_LOW_SPEED_MODE, ch, 0);
  }
}
