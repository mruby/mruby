#include <string.h>
#include "pico/stdlib.h"
#include "hardware/i2c.h"
#include <mruby/i2c.h>

#define UNIT_SELECT(u) \
  i2c_inst_t *inst; \
  switch (u) { \
  case 0: inst = i2c0; break; \
  case 1: inst = i2c1; break; \
  default: return MRB_I2C_ERROR_UNIT; \
  }

int
mrb_i2c_unit_name_to_num(const char *name)
{
  if (strcmp(name, "RP2040_I2C0") == 0) return 0;
  if (strcmp(name, "RP2040_I2C1") == 0) return 1;
  return MRB_I2C_ERROR_UNIT;
}

mrb_i2c_status
mrb_i2c_init(int unit, uint32_t freq, int8_t sda, int8_t scl)
{
  UNIT_SELECT(unit);
  i2c_init(inst, freq);

  if (sda < 0) sda = PICO_DEFAULT_I2C_SDA_PIN;
  if (scl < 0) scl = PICO_DEFAULT_I2C_SCL_PIN;
  gpio_set_function(sda, GPIO_FUNC_I2C);
  gpio_set_function(scl, GPIO_FUNC_I2C);
  gpio_pull_up(sda);
  gpio_pull_up(scl);

  return MRB_I2C_OK;
}

int
mrb_i2c_read(int unit, uint8_t addr, uint8_t *dst, size_t len,
             uint32_t timeout_us)
{
  UNIT_SELECT(unit);
  return i2c_read_timeout_us(inst, addr, dst, len, false, timeout_us);
}

int
mrb_i2c_write(int unit, uint8_t addr, const uint8_t *src, size_t len,
              uint32_t timeout_us)
{
  UNIT_SELECT(unit);
  return i2c_write_timeout_us(inst, addr, src, len, false, timeout_us);
}

int
mrb_i2c_write_read(int unit, uint8_t addr, const uint8_t *src, size_t wlen,
                   uint8_t *dst, size_t rlen, uint32_t timeout_us)
{
  UNIT_SELECT(unit);
  /* write with nostop=true (no STOP, keeps bus for repeated START) */
  int ret = i2c_write_timeout_us(inst, addr, src, wlen, true, timeout_us);
  if (ret < 0) return ret;
  /* read with nostop=false (STOP after read) */
  return i2c_read_timeout_us(inst, addr, dst, rlen, false, timeout_us);
}

#include <mruby.h>

void mrb_hw_rp2040_i2c_gem_init(mrb_state *mrb) {}
void mrb_hw_rp2040_i2c_gem_final(mrb_state *mrb) {}
