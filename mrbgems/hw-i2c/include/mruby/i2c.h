#ifndef MRUBY_I2C_H
#define MRUBY_I2C_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  MRB_I2C_OK             =  0,
  MRB_I2C_ERROR_UNIT     = -1,
  MRB_I2C_ERROR_TIMEOUT  = -2,
  MRB_I2C_ERROR_NACK     = -3,
} mrb_i2c_status;

/* HAL functions - implemented by hw-<platform>-i2c gems */
mrb_i2c_status mrb_i2c_init(int unit, uint32_t freq, int8_t sda, int8_t scl);
int mrb_i2c_read(int unit, uint8_t addr, uint8_t *dst, size_t len, uint32_t timeout_us);
int mrb_i2c_write(int unit, uint8_t addr, const uint8_t *src, size_t len, uint32_t timeout_us);
int mrb_i2c_write_read(int unit, uint8_t addr, const uint8_t *src, size_t wlen,
                       uint8_t *dst, size_t rlen, uint32_t timeout_us);
int mrb_i2c_unit_name_to_num(const char *name);

#ifdef __cplusplus
}
#endif

#endif /* MRUBY_I2C_H */
