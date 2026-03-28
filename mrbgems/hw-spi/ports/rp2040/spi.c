#include <string.h>
#include "pico/stdlib.h"
#include "hardware/spi.h"
#include <mruby/spi.h>

#define UNIT_SELECT(info) \
  spi_inst_t *inst; \
  switch ((info)->unit_num) { \
  case 0: inst = spi0; break; \
  case 1: inst = spi1; break; \
  default: return MRB_SPI_ERROR_UNIT; \
  }

int
mrb_spi_unit_name_to_num(const char *name)
{
  if (strcmp(name, "RP2040_SPI0") == 0) return 0;
  if (strcmp(name, "RP2040_SPI1") == 0) return 1;
  return MRB_SPI_ERROR_UNIT;
}

mrb_spi_status
mrb_spi_init(mrb_spi_info *info)
{
  UNIT_SELECT(info);
  spi_init(inst, info->frequency);

  if (info->sck_pin < 0)  info->sck_pin  = PICO_DEFAULT_SPI_SCK_PIN;
  if (info->cipo_pin < 0) info->cipo_pin = PICO_DEFAULT_SPI_RX_PIN;
  if (info->copi_pin < 0) info->copi_pin = PICO_DEFAULT_SPI_TX_PIN;

  gpio_set_function(info->sck_pin, GPIO_FUNC_SPI);
  gpio_set_function(info->cipo_pin, GPIO_FUNC_SPI);
  gpio_set_function(info->copi_pin, GPIO_FUNC_SPI);

  if (info->first_bit != MRB_SPI_MSB_FIRST) {
    return MRB_SPI_ERROR_FIRST_BIT;
  }

  spi_cpol_t cpol;
  spi_cpha_t cpha;
  switch (info->mode) {
  case 0: cpol = 0; cpha = 0; break;
  case 1: cpol = 0; cpha = 1; break;
  case 2: cpol = 1; cpha = 0; break;
  case 3: cpol = 1; cpha = 1; break;
  default: return MRB_SPI_ERROR_MODE;
  }
  spi_set_format(inst, 8, cpol, cpha, info->first_bit);

  return MRB_SPI_OK;
}

int
mrb_spi_read(mrb_spi_info *info, uint8_t *dst, size_t len, uint8_t tx_val)
{
  UNIT_SELECT(info);
  return spi_read_blocking(inst, tx_val, dst, len);
}

int
mrb_spi_write(mrb_spi_info *info, const uint8_t *src, size_t len)
{
  UNIT_SELECT(info);
  return spi_write_blocking(inst, src, len);
}

int
mrb_spi_transfer(mrb_spi_info *info, const uint8_t *tx, uint8_t *rx, size_t len)
{
  UNIT_SELECT(info);
  return spi_write_read_blocking(inst, tx, rx, len);
}
