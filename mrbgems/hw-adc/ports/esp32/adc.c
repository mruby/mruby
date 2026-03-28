#include <stdbool.h>
#include "esp_adc/adc_oneshot.h"
#include <mruby/adc.h>

#define VOLTAGE_MAX 3.3f
#define RESOLUTION  4095
#define UNIT_NUM    2

static adc_oneshot_unit_handle_t adc_handles[UNIT_NUM];
static bool adc_initialized;

static adc_unit_t
pin_to_unit(uint8_t pin)
{
  switch (pin) {
#if CONFIG_IDF_TARGET_ESP32C3
  case 0: case 1: case 2: case 3: case 4:
    return ADC_UNIT_1;
  case 5:
    return ADC_UNIT_2;
#elif CONFIG_IDF_TARGET_ESP32
  case 32: case 33: case 34: case 35: case 36: case 39:
    return ADC_UNIT_1;
  case 0: case 2: case 4: case 12: case 13: case 14: case 15:
  case 25: case 26: case 27:
    return ADC_UNIT_2;
#elif (CONFIG_IDF_TARGET_ESP32S3 || CONFIG_IDF_TARGET_ESP32S2)
  case 1: case 2: case 3: case 4: case 5:
  case 6: case 7: case 8: case 9: case 10:
    return ADC_UNIT_1;
  case 11: case 12: case 13: case 14: case 15:
  case 16: case 17: case 18: case 19: case 20:
    return ADC_UNIT_2;
#endif
  }
  return -1;
}

static adc_channel_t
pin_to_channel(uint8_t pin)
{
  switch (pin) {
#if CONFIG_IDF_TARGET_ESP32C3
  case 0: return ADC_CHANNEL_0;
  case 1: return ADC_CHANNEL_1;
  case 2: return ADC_CHANNEL_2;
  case 3: return ADC_CHANNEL_3;
  case 4: return ADC_CHANNEL_4;
  case 5: return ADC_CHANNEL_0;
#elif CONFIG_IDF_TARGET_ESP32
  case 36: return ADC_CHANNEL_0;
  case 39: return ADC_CHANNEL_3;
  case 32: return ADC_CHANNEL_4;
  case 33: return ADC_CHANNEL_5;
  case 34: return ADC_CHANNEL_6;
  case 35: return ADC_CHANNEL_7;
  case 4:  return ADC_CHANNEL_0;
  case 0:  return ADC_CHANNEL_1;
  case 2:  return ADC_CHANNEL_2;
  case 15: return ADC_CHANNEL_3;
  case 13: return ADC_CHANNEL_4;
  case 12: return ADC_CHANNEL_5;
  case 14: return ADC_CHANNEL_6;
  case 27: return ADC_CHANNEL_7;
  case 25: return ADC_CHANNEL_8;
  case 26: return ADC_CHANNEL_9;
#elif (CONFIG_IDF_TARGET_ESP32S3 || CONFIG_IDF_TARGET_ESP32S2)
  case 1:  return ADC_CHANNEL_0;
  case 2:  return ADC_CHANNEL_1;
  case 3:  return ADC_CHANNEL_2;
  case 4:  return ADC_CHANNEL_3;
  case 5:  return ADC_CHANNEL_4;
  case 6:  return ADC_CHANNEL_5;
  case 7:  return ADC_CHANNEL_6;
  case 8:  return ADC_CHANNEL_7;
  case 9:  return ADC_CHANNEL_8;
  case 10: return ADC_CHANNEL_9;
  case 11: return ADC_CHANNEL_0;
  case 12: return ADC_CHANNEL_1;
  case 13: return ADC_CHANNEL_2;
  case 14: return ADC_CHANNEL_3;
  case 15: return ADC_CHANNEL_4;
  case 16: return ADC_CHANNEL_5;
  case 17: return ADC_CHANNEL_6;
  case 18: return ADC_CHANNEL_7;
  case 19: return ADC_CHANNEL_8;
  case 20: return ADC_CHANNEL_9;
#endif
  }
  return -1;
}

static int
init_units(void)
{
  adc_unit_t units[] = { ADC_UNIT_1, ADC_UNIT_2 };
  for (int i = 0; i < UNIT_NUM; i++) {
    adc_oneshot_unit_init_cfg_t cfg = {
      .unit_id = units[i],
      .ulp_mode = ADC_ULP_MODE_DISABLE,
    };
    if (adc_oneshot_new_unit(&cfg, &adc_handles[i]) != ESP_OK)
      return -1;
  }
  return 0;
}

int
mrb_adc_init(uint8_t pin)
{
  if (!adc_initialized) {
    if (init_units() != 0) return -1;
    adc_initialized = true;
  }

  int ch = pin_to_channel(pin);
  if (ch < 0) return -1;

  adc_unit_t unit = pin_to_unit(pin);
  if (unit < 0 || unit >= UNIT_NUM) return -1;

  adc_oneshot_chan_cfg_t cfg = {
    .atten = ADC_ATTEN_DB_12,
    .bitwidth = ADC_BITWIDTH_DEFAULT,
  };
  if (adc_oneshot_config_channel(adc_handles[unit], ch, &cfg) != ESP_OK)
    return -1;

  return (int)pin;
}

uint32_t
mrb_adc_read_raw(uint8_t input)
{
  adc_unit_t unit = pin_to_unit(input);
  int ch = pin_to_channel(input);
  int raw = 0;
  if (unit >= 0 && unit < UNIT_NUM)
    adc_oneshot_read(adc_handles[unit], ch, &raw);
  return (uint32_t)raw;
}

float
mrb_adc_read_voltage(uint8_t input)
{
  return (float)mrb_adc_read_raw(input) * VOLTAGE_MAX / RESOLUTION;
}
