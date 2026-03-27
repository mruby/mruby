#include <mruby.h>
#include <mruby/presym.h>
#include <mruby/variable.h>
#include <mruby/string.h>
#include <mruby/data.h>
#include <mruby/class.h>
#include <mruby/uart.h>

#define E_IO_ERROR mrb_exc_get_id(mrb, MRB_SYM(IOError))

#define DEFAULT_RX_BUF_SIZE 256

static void
rxbuf_free(mrb_state *mrb, void *ptr)
{
  mrb_free(mrb, ptr);
}

static const struct mrb_data_type rxbuf_type = { "UART", rxbuf_free };

/* UART#__open_rx_buffer(size) */
static mrb_value
mrb_uart_m_open_rxbuf(mrb_state *mrb, mrb_value self)
{
  mrb_int size;
  mrb_get_args(mrb, "i", &size);
  if (size <= 0) size = DEFAULT_RX_BUF_SIZE;

  mrb_uart_ringbuf *rb = (mrb_uart_ringbuf*)mrb_malloc(mrb,
      sizeof(mrb_uart_ringbuf) + sizeof(uint8_t) * size);
  if (!mrb_uart_ringbuf_init(rb, (int)size)) {
    mrb_free(mrb, rb);
    mrb_raise(mrb, E_ARGUMENT_ERROR, "rx_buffer_size must be a power of two");
  }
  DATA_PTR(self) = rb;
  DATA_TYPE(self) = &rxbuf_type;
  return mrb_nil_value();
}

/* UART#__open_connection(unit_name, tx_pin, rx_pin) */
static mrb_value
mrb_uart_m_open_conn(mrb_state *mrb, mrb_value self)
{
  const char *name;
  mrb_int tx_pin, rx_pin;
  mrb_get_args(mrb, "zii", &name, &tx_pin, &rx_pin);

  int num = mrb_uart_unit_name_to_num(name);
  if (num < 0) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unknown UART unit: %s", name);
  }

  mrb_uart_ringbuf *rb = (mrb_uart_ringbuf*)mrb_data_get_ptr(mrb, self, &rxbuf_type);
  mrb_uart_status st = mrb_uart_init(num, (uint32_t)tx_pin, (uint32_t)rx_pin, rb);
  if (st != MRB_UART_OK) {
    mrb_raise(mrb, E_IO_ERROR, "UART init failed");
  }
  return mrb_fixnum_value(num);
}

/* UART#__set_baudrate(baud) */
static mrb_value
mrb_uart_m_set_baudrate(mrb_state *mrb, mrb_value self)
{
  mrb_int baud;
  mrb_get_args(mrb, "i", &baud);
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  uint32_t actual = mrb_uart_set_baudrate((int)unit, (uint32_t)baud);
  return mrb_fixnum_value(actual);
}

/* UART#__set_format(data_bits, stop_bits, parity) */
static mrb_value
mrb_uart_m_set_format(mrb_state *mrb, mrb_value self)
{
  mrb_int data_bits, stop_bits, parity;
  mrb_get_args(mrb, "iii", &data_bits, &stop_bits, &parity);
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  mrb_uart_set_format((int)unit, (uint32_t)data_bits, (uint32_t)stop_bits, (uint8_t)parity);
  return mrb_nil_value();
}

/* UART#__set_flow_control(cts, rts) */
static mrb_value
mrb_uart_m_set_flow(mrb_state *mrb, mrb_value self)
{
  mrb_bool cts, rts;
  mrb_get_args(mrb, "bb", &cts, &rts);
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  mrb_uart_set_flow_control((int)unit, cts, rts);
  return mrb_nil_value();
}

/* UART#write(str) */
static mrb_value
mrb_uart_m_write(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  size_t len = RSTRING_LEN(str);
  mrb_uart_write((int)unit, (const uint8_t*)RSTRING_PTR(str), len);
  return mrb_fixnum_value(len);
}

/* UART#read(len=nil) */
static mrb_value
mrb_uart_m_read(mrb_state *mrb, mrb_value self)
{
  mrb_int len = -1;
  mrb_get_args(mrb, "|i", &len);

  mrb_uart_ringbuf *rb = (mrb_uart_ringbuf*)mrb_data_get_ptr(mrb, self, &rxbuf_type);
  int avail = mrb_uart_ringbuf_available(rb);
  if (avail == 0) return mrb_nil_value();

  if (len >= 0) {
    if (avail < len) return mrb_nil_value();
    avail = (int)len;
  }

  uint8_t *buf = (uint8_t*)mrb_malloc(mrb, avail);
  int n = mrb_uart_ringbuf_pop(rb, buf, avail);
  mrb_value str = mrb_str_new(mrb, (const char*)buf, n);
  mrb_free(mrb, buf);
  return str;
}

/* UART#readpartial(maxlen) */
static mrb_value
mrb_uart_m_readpartial(mrb_state *mrb, mrb_value self)
{
  mrb_int maxlen;
  mrb_get_args(mrb, "i", &maxlen);

  mrb_uart_ringbuf *rb = (mrb_uart_ringbuf*)mrb_data_get_ptr(mrb, self, &rxbuf_type);
  int avail = mrb_uart_ringbuf_available(rb);
  if (avail == 0) return mrb_nil_value();
  if (avail > maxlen) avail = (int)maxlen;

  uint8_t *buf = (uint8_t*)mrb_malloc(mrb, avail);
  int n = mrb_uart_ringbuf_pop(rb, buf, avail);
  mrb_value str = mrb_str_new(mrb, (const char*)buf, n);
  mrb_free(mrb, buf);
  return str;
}

/* UART#bytes_available */
static mrb_value
mrb_uart_m_bytes_available(mrb_state *mrb, mrb_value self)
{
  mrb_uart_ringbuf *rb = (mrb_uart_ringbuf*)mrb_data_get_ptr(mrb, self, &rxbuf_type);
  return mrb_fixnum_value(mrb_uart_ringbuf_available(rb));
}

/* UART#gets */
static mrb_value
mrb_uart_m_gets(mrb_state *mrb, mrb_value self)
{
  mrb_uart_ringbuf *rb = (mrb_uart_ringbuf*)mrb_data_get_ptr(mrb, self, &rxbuf_type);
  int pos = mrb_uart_ringbuf_search(rb, (uint8_t)'\n');
  if (pos < 0) return mrb_nil_value();
  int len = pos + 1;
  uint8_t *buf = (uint8_t*)mrb_malloc(mrb, len);
  mrb_uart_ringbuf_pop(rb, buf, len);
  mrb_value str = mrb_str_new(mrb, (const char*)buf, len);
  mrb_free(mrb, buf);
  return str;
}

/* UART#flush */
static mrb_value
mrb_uart_m_flush(mrb_state *mrb, mrb_value self)
{
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  mrb_uart_flush((int)unit);
  return self;
}

/* UART#clear_tx_buffer */
static mrb_value
mrb_uart_m_clear_tx(mrb_state *mrb, mrb_value self)
{
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  mrb_uart_clear_tx((int)unit);
  return self;
}

/* UART#clear_rx_buffer */
static mrb_value
mrb_uart_m_clear_rx(mrb_state *mrb, mrb_value self)
{
  mrb_uart_ringbuf *rb = (mrb_uart_ringbuf*)mrb_data_get_ptr(mrb, self, &rxbuf_type);
  mrb_uart_ringbuf_clear(rb);
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  mrb_uart_clear_rx((int)unit);
  return self;
}

/* UART#send_break(duration_ms=100) */
static mrb_value
mrb_uart_m_send_break(mrb_state *mrb, mrb_value self)
{
  mrb_int ms = 100;
  mrb_get_args(mrb, "|i", &ms);
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  mrb_uart_send_break((int)unit, (uint32_t)ms);
  return self;
}

void
mrb_hw_uart_gem_init(mrb_state *mrb)
{
  struct RClass *cls = mrb_define_class_id(mrb, MRB_SYM(UART), mrb->object_class);
  MRB_SET_INSTANCE_TT(cls, MRB_TT_CDATA);

  mrb_define_method_id(mrb, cls, MRB_SYM(__open_rx_buffer), mrb_uart_m_open_rxbuf, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(__open_connection), mrb_uart_m_open_conn, MRB_ARGS_REQ(3));
  mrb_define_method_id(mrb, cls, MRB_SYM(__set_baudrate), mrb_uart_m_set_baudrate, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(__set_format), mrb_uart_m_set_format, MRB_ARGS_REQ(3));
  mrb_define_method_id(mrb, cls, MRB_SYM(__set_flow_control), mrb_uart_m_set_flow, MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, cls, MRB_SYM(write), mrb_uart_m_write, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(read), mrb_uart_m_read, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(readpartial), mrb_uart_m_readpartial, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, cls, MRB_SYM(bytes_available), mrb_uart_m_bytes_available, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(gets), mrb_uart_m_gets, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(flush), mrb_uart_m_flush, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(clear_tx_buffer), mrb_uart_m_clear_tx, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(clear_rx_buffer), mrb_uart_m_clear_rx, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(send_break), mrb_uart_m_send_break, MRB_ARGS_OPT(1));
}

void
mrb_hw_uart_gem_final(mrb_state *mrb)
{
}
