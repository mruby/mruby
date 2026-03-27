#include <stdio.h>
#include <string.h>
#include "freertos/FreeRTOS.h"
#include "driver/uart.h"
#include <mruby/uart.h>

#define RX_TASK_BUF_SIZE 128
#define QUEUE_LENGTH     20
#define TASK_STACK_SIZE  4096
#define TASK_PRIORITY    12

typedef struct {
  int unit;
  QueueHandle_t queue;
  mrb_uart_ringbuf *rxbuf;
} uart_ctx;

static uart_ctx ctx[UART_NUM_MAX];

static void
rx_task(void *arg)
{
  uart_ctx *c = (uart_ctx*)arg;
  uart_event_t event;
  uint8_t buf[RX_TASK_BUF_SIZE];

  for (;;) {
    if (xQueueReceive(c->queue, &event, portMAX_DELAY)) {
      if (event.type == UART_DATA) {
        size_t n = event.size > RX_TASK_BUF_SIZE ? RX_TASK_BUF_SIZE : event.size;
        uart_read_bytes(c->unit, buf, n, portMAX_DELAY);
        for (size_t i = 0; i < n; i++) {
          mrb_uart_ringbuf_push(c->rxbuf, buf[i]);
        }
      }
    }
  }
}

int
mrb_uart_unit_name_to_num(const char *name)
{
  if (strcmp(name, "ESP32_UART0") == 0) return UART_NUM_0;
  if (strcmp(name, "ESP32_UART1") == 0) return UART_NUM_1;
#ifdef UART_NUM_2
  if (strcmp(name, "ESP32_UART2") == 0) return UART_NUM_2;
#endif
  return MRB_UART_ERROR_UNIT;
}

mrb_uart_status
mrb_uart_init(int unit, uint32_t tx_pin, uint32_t rx_pin,
              mrb_uart_ringbuf *rxbuf)
{
  if (unit < 0 || unit >= UART_NUM_MAX) return MRB_UART_ERROR_UNIT;

  uart_config_t cfg = {
    .baud_rate = 9600,
    .data_bits = UART_DATA_8_BITS,
    .parity    = UART_PARITY_DISABLE,
    .stop_bits = UART_STOP_BITS_1,
    .flow_ctrl = UART_HW_FLOWCTRL_DISABLE,
    .source_clk = UART_SCLK_DEFAULT,
  };

  int bufsize = (rxbuf->mask + 1);
  uart_driver_install(unit, bufsize, 0, QUEUE_LENGTH, &ctx[unit].queue, 0);
  uart_param_config(unit, &cfg);
  uart_set_pin(unit, tx_pin, rx_pin, UART_PIN_NO_CHANGE, UART_PIN_NO_CHANGE);

  ctx[unit].unit = unit;
  ctx[unit].rxbuf = rxbuf;

  char name[32];
  snprintf(name, sizeof(name), "uart_rx_%d", unit);
  xTaskCreate(rx_task, name, TASK_STACK_SIZE, &ctx[unit], TASK_PRIORITY, NULL);

  return MRB_UART_OK;
}

uint32_t
mrb_uart_set_baudrate(int unit, uint32_t baudrate)
{
  uart_set_baudrate(unit, baudrate);
  return baudrate;
}

void
mrb_uart_set_format(int unit, uint32_t data_bits, uint32_t stop_bits,
                     uint8_t parity)
{
  static const uart_word_length_t wl[] = {
    UART_DATA_5_BITS, UART_DATA_6_BITS, UART_DATA_7_BITS, UART_DATA_8_BITS
  };
  static const uart_stop_bits_t sb[] = {
    UART_STOP_BITS_1, UART_STOP_BITS_2
  };
  static const uart_parity_t pr[] = {
    UART_PARITY_DISABLE, UART_PARITY_EVEN, UART_PARITY_ODD
  };
  if (data_bits >= 5 && data_bits <= 8)
    uart_set_word_length(unit, wl[data_bits - 5]);
  if (stop_bits >= 1 && stop_bits <= 2)
    uart_set_stop_bits(unit, sb[stop_bits - 1]);
  if (parity <= 2)
    uart_set_parity(unit, pr[parity]);
}

void
mrb_uart_set_flow_control(int unit, bool cts, bool rts)
{
  uart_hw_flowcontrol_t mode = UART_HW_FLOWCTRL_DISABLE;
  if (cts && rts) mode = UART_HW_FLOWCTRL_CTS_RTS;
  else if (cts)   mode = UART_HW_FLOWCTRL_CTS;
  else if (rts)   mode = UART_HW_FLOWCTRL_RTS;
  uart_set_hw_flow_ctrl(unit, mode, 122);
}

void
mrb_uart_write(int unit, const uint8_t *src, size_t len)
{
  uart_write_bytes(unit, (const char*)src, len);
}

void
mrb_uart_flush(int unit)
{
  uart_wait_tx_done(unit, 100);
}

void
mrb_uart_send_break(int unit, uint32_t duration_ms)
{
  uart_write_bytes_with_break(unit, NULL, 0, duration_ms);
}

void
mrb_uart_clear_rx(int unit)
{
  uart_flush_input(unit);
}

void
mrb_uart_clear_tx(int unit)
{
  /* not supported on ESP-IDF */
}

#include <mruby.h>

void mrb_hw_esp32_uart_gem_init(mrb_state *mrb) {}
void mrb_hw_esp32_uart_gem_final(mrb_state *mrb) {}
