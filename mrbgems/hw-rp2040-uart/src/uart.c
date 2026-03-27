#include <string.h>
#include "pico/stdlib.h"
#include "hardware/gpio.h"
#include "hardware/uart.h"
#include "hardware/irq.h"
#include <mruby/uart.h>

#define UNIT_SELECT(u) \
  uart_inst_t *inst; \
  switch (u) { \
  case 0: inst = uart0; break; \
  case 1: inst = uart1; break; \
  default: return MRB_UART_ERROR_UNIT; \
  }

/* void-returning variant for functions that can't return error */
#define UNIT_SELECT_V(u) \
  uart_inst_t *inst; \
  switch (u) { \
  case 0: inst = uart0; break; \
  case 1: inst = uart1; break; \
  default: return; \
  }

static mrb_uart_ringbuf *rx_bufs[2];

static void
on_uart0_rx(void)
{
  while (uart_is_readable(uart0)) {
    mrb_uart_ringbuf_push(rx_bufs[0], uart_getc(uart0));
  }
}

static void
on_uart1_rx(void)
{
  while (uart_is_readable(uart1)) {
    mrb_uart_ringbuf_push(rx_bufs[1], uart_getc(uart1));
  }
}

int
mrb_uart_unit_name_to_num(const char *name)
{
  if (strcmp(name, "RP2040_UART0") == 0) return 0;
  if (strcmp(name, "RP2040_UART1") == 0) return 1;
  return MRB_UART_ERROR_UNIT;
}

mrb_uart_status
mrb_uart_init(int unit, uint32_t tx_pin, uint32_t rx_pin,
              mrb_uart_ringbuf *rxbuf)
{
  UNIT_SELECT(unit);
  uart_init(inst, 9600);

  gpio_set_function(tx_pin, GPIO_FUNC_UART);
  gpio_set_function(rx_pin, GPIO_FUNC_UART);

  rx_bufs[unit] = rxbuf;

  uint irq;
  if (unit == 0) {
    irq = UART0_IRQ;
    irq_set_exclusive_handler(irq, on_uart0_rx);
  }
  else {
    irq = UART1_IRQ;
    irq_set_exclusive_handler(irq, on_uart1_rx);
  }
  irq_set_enabled(irq, true);
  uart_set_irq_enables(inst, true, false);

  return MRB_UART_OK;
}

uint32_t
mrb_uart_set_baudrate(int unit, uint32_t baudrate)
{
  UNIT_SELECT(unit);
  return uart_set_baudrate(inst, baudrate);
}

void
mrb_uart_set_format(int unit, uint32_t data_bits, uint32_t stop_bits,
                     uint8_t parity)
{
  UNIT_SELECT_V(unit);
  uart_set_format(inst, data_bits, stop_bits, (uart_parity_t)parity);
}

void
mrb_uart_set_flow_control(int unit, bool cts, bool rts)
{
  UNIT_SELECT_V(unit);
  uart_set_hw_flow(inst, cts, rts);
}

void
mrb_uart_write(int unit, const uint8_t *src, size_t len)
{
  UNIT_SELECT_V(unit);
  uart_write_blocking(inst, src, len);
}

void
mrb_uart_flush(int unit)
{
  UNIT_SELECT_V(unit);
  uart_tx_wait_blocking(inst);
}

void
mrb_uart_send_break(int unit, uint32_t duration_ms)
{
  UNIT_SELECT_V(unit);
  uart_set_break(inst, true);
  sleep_ms(duration_ms);
  uart_set_break(inst, false);
}

void
mrb_uart_clear_rx(int unit)
{
  UNIT_SELECT_V(unit);
  while (uart_is_readable(inst)) {
    uart_getc(inst);
  }
}

void
mrb_uart_clear_tx(int unit)
{
  /* not supported on RP2040 */
}

#include <mruby.h>

void mrb_hw_rp2040_uart_gem_init(mrb_state *mrb) {}
void mrb_hw_rp2040_uart_gem_final(mrb_state *mrb) {}
