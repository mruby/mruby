#ifndef MRUBY_ADC_H
#define MRUBY_ADC_H

#include <stdint.h>
#include <mruby/common.h>

MRB_BEGIN_DECL

/* HAL functions - implemented in ports/<platform>/adc.c */
int      mrb_adc_init(uint8_t pin);
uint32_t mrb_adc_read_raw(uint8_t input);
float    mrb_adc_read_voltage(uint8_t input);

MRB_END_DECL

#endif /* MRUBY_ADC_H */
