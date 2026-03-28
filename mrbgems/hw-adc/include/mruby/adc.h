#ifndef MRUBY_ADC_H
#define MRUBY_ADC_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* HAL functions - implemented in ports/<platform>/adc.c */
int      mrb_adc_init(uint8_t pin);
uint32_t mrb_adc_read_raw(uint8_t input);
float    mrb_adc_read_voltage(uint8_t input);

#ifdef __cplusplus
}
#endif

#endif /* MRUBY_ADC_H */
