#ifndef MRUBY_UART_H
#define MRUBY_UART_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MRB_UART_PARITY_NONE 0
#define MRB_UART_PARITY_EVEN 1
#define MRB_UART_PARITY_ODD  2

#define MRB_UART_FLOW_NONE    0
#define MRB_UART_FLOW_RTS_CTS 1

typedef enum {
  MRB_UART_OK           =  0,
  MRB_UART_ERROR_UNIT   = -1,
} mrb_uart_status;

/* Ring buffer for interrupt-driven RX.
   Allocated by common gem, populated by platform interrupt handler.
   size must be a power of two. */
typedef struct {
  volatile int head;
  volatile int tail;
  int mask;
  uint8_t data[];
} mrb_uart_ringbuf;

/* Ring buffer helpers (implemented in hw-uart/src/ringbuf.c) */
bool mrb_uart_ringbuf_init(mrb_uart_ringbuf *rb, int size);
bool mrb_uart_ringbuf_push(mrb_uart_ringbuf *rb, uint8_t ch);
int  mrb_uart_ringbuf_pop(mrb_uart_ringbuf *rb, uint8_t *dst, int len);
int  mrb_uart_ringbuf_available(const mrb_uart_ringbuf *rb);
void mrb_uart_ringbuf_clear(mrb_uart_ringbuf *rb);
int  mrb_uart_ringbuf_search(const mrb_uart_ringbuf *rb, uint8_t ch);

/* HAL functions - implemented by hw-<platform>-uart gems */
int  mrb_uart_unit_name_to_num(const char *name);
mrb_uart_status mrb_uart_init(int unit, uint32_t tx_pin, uint32_t rx_pin,
                               mrb_uart_ringbuf *rxbuf);
uint32_t mrb_uart_set_baudrate(int unit, uint32_t baudrate);
void mrb_uart_set_format(int unit, uint32_t data_bits, uint32_t stop_bits,
                          uint8_t parity);
void mrb_uart_set_flow_control(int unit, bool cts, bool rts);
void mrb_uart_write(int unit, const uint8_t *src, size_t len);
void mrb_uart_flush(int unit);
void mrb_uart_send_break(int unit, uint32_t duration_ms);
void mrb_uart_clear_rx(int unit);
void mrb_uart_clear_tx(int unit);

#ifdef __cplusplus
}
#endif

#endif /* MRUBY_UART_H */
