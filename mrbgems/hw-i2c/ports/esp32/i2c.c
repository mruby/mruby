#include <string.h>
#include "driver/i2c_master.h"
#include <mruby/i2c.h>

typedef struct {
  i2c_master_bus_handle_t bus;
  uint32_t freq;
  bool initialized;
} i2c_ctx;

/* ESP32 supports up to 2 I2C ports */
static i2c_ctx ctx[2];

static bool
valid_unit(int unit)
{
  return unit >= 0 && unit <= 1 && ctx[unit].initialized;
}

static uint32_t
us_to_ms(uint32_t timeout_us)
{
  uint32_t ms = (timeout_us + 999) / 1000;
  return (ms < 10) ? 10 : ms;
}

static i2c_master_dev_handle_t
add_device(int unit, uint8_t addr)
{
  i2c_device_config_t cfg = {
    .dev_addr_length = I2C_ADDR_BIT_LEN_7,
    .device_address = addr,
    .scl_speed_hz = ctx[unit].freq,
  };
  i2c_master_dev_handle_t dev;
  if (i2c_master_bus_add_device(ctx[unit].bus, &cfg, &dev) != ESP_OK)
    return NULL;
  return dev;
}

int
mrb_i2c_unit_name_to_num(const char *name)
{
  if (strcmp(name, "ESP32_I2C0") == 0) return 0;
  if (strcmp(name, "ESP32_I2C1") == 0) return 1;
  return MRB_I2C_ERROR_UNIT;
}

mrb_i2c_status
mrb_i2c_init(int unit, uint32_t freq, int8_t sda, int8_t scl)
{
  if (unit < 0 || unit > 1) return MRB_I2C_ERROR_UNIT;

  if (ctx[unit].initialized) {
    i2c_del_master_bus(ctx[unit].bus);
    ctx[unit].initialized = false;
  }

  i2c_master_bus_config_t cfg = {
    .clk_source = I2C_CLK_SRC_DEFAULT,
    .i2c_port = unit,
    .scl_io_num = scl,
    .sda_io_num = sda,
    .glitch_ignore_cnt = 7,
    .flags.enable_internal_pullup = true,
  };

  esp_err_t err = i2c_new_master_bus(&cfg, &ctx[unit].bus);
  if (err != ESP_OK) return MRB_I2C_ERROR_UNIT;

  ctx[unit].initialized = true;
  ctx[unit].freq = freq;
  return MRB_I2C_OK;
}

int
mrb_i2c_read(int unit, uint8_t addr, uint8_t *dst, size_t len,
             uint32_t timeout_us)
{
  if (!valid_unit(unit)) return MRB_I2C_ERROR_UNIT;

  i2c_master_dev_handle_t dev = add_device(unit, addr);
  if (!dev) return -1;

  esp_err_t err = i2c_master_receive(dev, dst, len, us_to_ms(timeout_us));
  i2c_master_bus_rm_device(dev);
  return (err == ESP_OK) ? (int)len : -1;
}

int
mrb_i2c_write(int unit, uint8_t addr, const uint8_t *src, size_t len,
              uint32_t timeout_us)
{
  if (!valid_unit(unit)) return MRB_I2C_ERROR_UNIT;

  i2c_master_dev_handle_t dev = add_device(unit, addr);
  if (!dev) return -1;

  esp_err_t err = i2c_master_transmit(dev, src, len, us_to_ms(timeout_us));
  i2c_master_bus_rm_device(dev);
  return (err == ESP_OK) ? (int)len : -1;
}

int
mrb_i2c_write_read(int unit, uint8_t addr, const uint8_t *src, size_t wlen,
                   uint8_t *dst, size_t rlen, uint32_t timeout_us)
{
  if (!valid_unit(unit)) return MRB_I2C_ERROR_UNIT;

  i2c_master_dev_handle_t dev = add_device(unit, addr);
  if (!dev) return -1;

  esp_err_t err = i2c_master_transmit_receive(dev, src, wlen, dst, rlen,
                                               us_to_ms(timeout_us));
  i2c_master_bus_rm_device(dev);
  return (err == ESP_OK) ? (int)rlen : -1;
}
