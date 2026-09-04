#include <stdbool.h>
#include "hardware/adc.h"
#include <mruby/adc.h>

#define VOLTAGE_MAX 3.3f
#define RESOLUTION  4095
#define TEMP_INPUT  4

static bool adc_initialized;

int
mrb_adc_init(uint8_t pin)
{
  if (!adc_initialized) {
    adc_init();
    adc_initialized = true;
  }

  uint input;
  switch (pin) {
  case 26: input = 0; break;
  case 27: input = 1; break;
  case 28: input = 2; break;
  case 29: input = 3; break;
  default: return -1;
  }
  adc_gpio_init(pin);
  return (int)input;
}

uint32_t
mrb_adc_read_raw(uint8_t input)
{
  adc_select_input(input);
  return (uint32_t)adc_read();
}

float
mrb_adc_read_voltage(uint8_t input)
{
  adc_select_input(input);
  return (float)adc_read() * VOLTAGE_MAX / RESOLUTION;
}
