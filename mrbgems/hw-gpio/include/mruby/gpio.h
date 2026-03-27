#ifndef MRUBY_GPIO_H
#define MRUBY_GPIO_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* direction/mode flags (bitmask) */
#define MRB_GPIO_IN         0x01
#define MRB_GPIO_OUT        0x02
#define MRB_GPIO_HIGH_Z     0x04
#define MRB_GPIO_PULL_UP    0x08
#define MRB_GPIO_PULL_DOWN  0x10
#define MRB_GPIO_OPEN_DRAIN 0x20

/* HAL functions - implemented by hw-<platform>-gpio gems */
void mrb_gpio_init(uint8_t pin);
void mrb_gpio_set_dir(uint8_t pin, uint8_t flags);
void mrb_gpio_pull_up(uint8_t pin);
void mrb_gpio_pull_down(uint8_t pin);
void mrb_gpio_open_drain(uint8_t pin);
int  mrb_gpio_read(uint8_t pin);
void mrb_gpio_write(uint8_t pin, uint8_t val);

#ifdef __cplusplus
}
#endif

#endif /* MRUBY_GPIO_H */
