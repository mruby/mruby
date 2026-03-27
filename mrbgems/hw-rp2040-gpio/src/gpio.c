#include <stdbool.h>
#include "hardware/gpio.h"
#include <mruby/gpio.h>

void
mrb_gpio_init(uint8_t pin)
{
  gpio_init(pin);
}

void
mrb_gpio_set_dir(uint8_t pin, uint8_t flags)
{
  if (flags & MRB_GPIO_IN) {
    gpio_set_dir(pin, false);
  }
  else if (flags & MRB_GPIO_OUT) {
    gpio_set_dir(pin, true);
  }
  /* HIGH_Z: not yet implemented */
}

void
mrb_gpio_pull_up(uint8_t pin)
{
  gpio_pull_up(pin);
}

void
mrb_gpio_pull_down(uint8_t pin)
{
  gpio_pull_down(pin);
}

void
mrb_gpio_open_drain(uint8_t pin)
{
  /* not yet implemented */
}

int
mrb_gpio_read(uint8_t pin)
{
  return gpio_get(pin);
}

void
mrb_gpio_write(uint8_t pin, uint8_t val)
{
  gpio_put(pin, val == 1);
}

#include <mruby.h>

void mrb_hw_rp2040_gpio_gem_init(mrb_state *mrb) {}
void mrb_hw_rp2040_gpio_gem_final(mrb_state *mrb) {}
