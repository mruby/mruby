#include <mruby/uart.h>

bool
mrb_uart_ringbuf_init(mrb_uart_ringbuf *rb, int size)
{
  /* size must be a power of two */
  if (size <= 0 || (size & (size - 1)) != 0) return false;
  rb->head = 0;
  rb->tail = 0;
  rb->mask = size - 1;
  return true;
}

bool
mrb_uart_ringbuf_push(mrb_uart_ringbuf *rb, uint8_t ch)
{
  int next = (rb->head + 1) & rb->mask;
  if (next == rb->tail) return false; /* full */
  rb->data[rb->head] = ch;
  rb->head = next;
  return true;
}

int
mrb_uart_ringbuf_pop(mrb_uart_ringbuf *rb, uint8_t *dst, int len)
{
  int i;
  for (i = 0; i < len; i++) {
    if (rb->tail == rb->head) break; /* empty */
    dst[i] = rb->data[rb->tail];
    rb->tail = (rb->tail + 1) & rb->mask;
  }
  return i;
}

int
mrb_uart_ringbuf_available(const mrb_uart_ringbuf *rb)
{
  return (rb->head - rb->tail) & rb->mask;
}

void
mrb_uart_ringbuf_clear(mrb_uart_ringbuf *rb)
{
  rb->tail = rb->head;
}

int
mrb_uart_ringbuf_search(const mrb_uart_ringbuf *rb, uint8_t ch)
{
  int pos = rb->tail;
  int i = 0;
  while (pos != rb->head) {
    if (rb->data[pos] == ch) return i;
    pos = (pos + 1) & rb->mask;
    i++;
  }
  return -1;
}
