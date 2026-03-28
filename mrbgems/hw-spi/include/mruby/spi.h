#ifndef MRUBY_SPI_H
#define MRUBY_SPI_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MRB_SPI_MSB_FIRST 1
#define MRB_SPI_LSB_FIRST 0

typedef enum {
  MRB_SPI_OK              =  0,
  MRB_SPI_ERROR_UNIT      = -1,
  MRB_SPI_ERROR_MODE      = -2,
  MRB_SPI_ERROR_FIRST_BIT = -3,
  MRB_SPI_ERROR_INIT      = -4,
} mrb_spi_status;

typedef struct {
  uint32_t frequency;
  uint8_t  unit_num;
  int8_t   sck_pin;
  int8_t   copi_pin;
  int8_t   cipo_pin;
  int8_t   cs_pin;
  uint8_t  mode;
  uint8_t  first_bit;
} mrb_spi_info;

/* HAL functions - implemented in ports/<platform>/spi.c */
int mrb_spi_unit_name_to_num(const char *name);
mrb_spi_status mrb_spi_init(mrb_spi_info *info);
int mrb_spi_read(mrb_spi_info *info, uint8_t *dst, size_t len, uint8_t tx_val);
int mrb_spi_write(mrb_spi_info *info, const uint8_t *src, size_t len);
int mrb_spi_transfer(mrb_spi_info *info, const uint8_t *tx, uint8_t *rx, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* MRUBY_SPI_H */
