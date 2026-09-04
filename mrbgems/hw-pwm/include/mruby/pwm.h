#ifndef MRUBY_PWM_H
#define MRUBY_PWM_H

#include <stdint.h>
#include <stdbool.h>
#include <mruby/common.h>

MRB_BEGIN_DECL

/* HAL functions - implemented in ports/<platform>/pwm.c */
void mrb_pwm_init(uint32_t pin);
void mrb_pwm_set_freq_duty(uint32_t pin, float frequency, float duty);
void mrb_pwm_set_enabled(uint32_t pin, bool enabled);

MRB_END_DECL

#endif /* MRUBY_PWM_H */
