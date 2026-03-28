#include <string.h>
#include "driver/spi_master.h"
#include <mruby/spi.h>

static spi_device_handle_t handles[SPI_HOST_MAX];

int
mrb_spi_unit_name_to_num(const char *name)
{
  if (strcmp(name, "ESP32_SPI2_HOST") == 0) return SPI2_HOST;
  if (strcmp(name, "ESP32_HSPI_HOST") == 0) return SPI2_HOST;
#if (SOC_SPI_PERIPH_NUM == 3)
  if (strcmp(name, "ESP32_SPI3_HOST") == 0) return SPI3_HOST;
  if (strcmp(name, "ESP32_VSPI_HOST") == 0) return SPI3_HOST;
#endif
  return MRB_SPI_ERROR_UNIT;
}

mrb_spi_status
mrb_spi_init(mrb_spi_info *info)
{
  if (handles[info->unit_num] != NULL) return MRB_SPI_OK;

  spi_bus_config_t buscfg = {
    .mosi_io_num = info->copi_pin,
    .miso_io_num = info->cipo_pin,
    .sclk_io_num = info->sck_pin,
    .quadwp_io_num = -1,
    .quadhd_io_num = -1,
  };

  esp_err_t err = spi_bus_initialize(info->unit_num, &buscfg, SPI_DMA_CH_AUTO);
  if (err != ESP_OK) return MRB_SPI_ERROR_INIT;

  spi_device_interface_config_t devcfg = {
    .clock_speed_hz = info->frequency,
    .mode = info->mode,
    .spics_io_num = info->cs_pin,
    .queue_size = 7,
  };

  err = spi_bus_add_device(info->unit_num, &devcfg, &handles[info->unit_num]);
  if (err != ESP_OK) {
    spi_bus_free(info->unit_num);
    handles[info->unit_num] = NULL;
    return MRB_SPI_ERROR_INIT;
  }
  return MRB_SPI_OK;
}

int
mrb_spi_read(mrb_spi_info *info, uint8_t *dst, size_t len, uint8_t tx_val)
{
  spi_transaction_t t = {
    .length = len * 8,
    .tx_buffer = NULL,
    .rx_buffer = dst,
  };
  esp_err_t err = spi_device_polling_transmit(handles[info->unit_num], &t);
  return (err == ESP_OK) ? (int)len : -1;
}

int
mrb_spi_write(mrb_spi_info *info, const uint8_t *src, size_t len)
{
  spi_transaction_t t = {
    .length = len * 8,
    .tx_buffer = src,
    .rx_buffer = NULL,
  };
  esp_err_t err = spi_device_polling_transmit(handles[info->unit_num], &t);
  return (err == ESP_OK) ? (int)len : -1;
}

int
mrb_spi_transfer(mrb_spi_info *info, const uint8_t *tx, uint8_t *rx, size_t len)
{
  spi_transaction_t t = {
    .length = len * 8,
    .tx_buffer = tx,
    .rx_buffer = rx,
  };
  esp_err_t err = spi_device_polling_transmit(handles[info->unit_num], &t);
  return (err == ESP_OK) ? (int)len : -1;
}
