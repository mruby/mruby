#include <string.h>
#include <mruby.h>
#include <mruby/presym.h>
#include <mruby/variable.h>
#include <mruby/array.h>
#include <mruby/string.h>
#include <mruby/i2c.h>

#define STACK_BUF_SIZE 256
#define E_IO_ERROR mrb_exc_get_id(mrb, MRB_SYM(IOError))

static size_t
i2c_fill_buf(mrb_state *mrb, uint8_t *buf, mrb_value *args, mrb_int argc)
{
  size_t pos = 0;
  for (mrb_int i = 0; i < argc; i++) {
    switch (mrb_type(args[i])) {
    case MRB_TT_ARRAY: {
      mrb_int alen = RARRAY_LEN(args[i]);
      const mrb_value *aptr = RARRAY_PTR(args[i]);
      for (mrb_int j = 0; j < alen; j++) {
        if (!mrb_integer_p(aptr[j])) {
          mrb_raise(mrb, E_TYPE_ERROR, "array element must be Integer");
        }
        buf[pos++] = (uint8_t)mrb_integer(aptr[j]);
      }
      break;
    }
    case MRB_TT_INTEGER:
      buf[pos++] = (uint8_t)mrb_integer(args[i]);
      break;
    case MRB_TT_STRING:
      memcpy(&buf[pos], RSTRING_PTR(args[i]), RSTRING_LEN(args[i]));
      pos += RSTRING_LEN(args[i]);
      break;
    default:
      break;
    }
  }
  return pos;
}

static size_t
i2c_calc_size(mrb_state *mrb, mrb_value *args, mrb_int argc)
{
  size_t total = 0;
  for (mrb_int i = 0; i < argc; i++) {
    switch (mrb_type(args[i])) {
    case MRB_TT_ARRAY:
      total += RARRAY_LEN(args[i]);
      break;
    case MRB_TT_INTEGER:
      total += 1;
      break;
    case MRB_TT_STRING:
      total += RSTRING_LEN(args[i]);
      break;
    default:
      mrb_raise(mrb, E_TYPE_ERROR, "Integer, Array, or String expected");
    }
  }
  return total;
}

/* Allocate write buffer, fill it, return pointer and size.
   Caller must free if need_free is set. */
static uint8_t*
i2c_build_buf(mrb_state *mrb, mrb_value *args, mrb_int argc,
              size_t *out_len, uint8_t *sbuf, mrb_bool *need_free)
{
  size_t total = i2c_calc_size(mrb, args, argc);
  uint8_t *buf;
  if (total <= STACK_BUF_SIZE) {
    buf = sbuf;
    *need_free = FALSE;
  }
  else {
    buf = (uint8_t*)mrb_malloc(mrb, total);
    *need_free = TRUE;
  }
  i2c_fill_buf(mrb, buf, args, argc);
  *out_len = total;
  return buf;
}

static mrb_int
get_timeout(mrb_state *mrb, mrb_value self, mrb_value kw)
{
  if (mrb_undef_p(kw)) {
    return mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(timeout)));
  }
  return mrb_integer(kw);
}

static mrb_value
mrb_i2c_m_write(mrb_state *mrb, mrb_value self)
{
  mrb_value *args;
  mrb_int argc, addr;
  const mrb_sym kw_names[] = { MRB_SYM(timeout) };
  mrb_value kw_values[1];
  mrb_kwargs kwargs = { 1, 0, kw_names, kw_values, NULL };

  mrb_get_args(mrb, "i*:", &addr, &args, &argc, &kwargs);

  mrb_int timeout_ms = get_timeout(mrb, self, kw_values[0]);
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));

  uint8_t sbuf[STACK_BUF_SIZE];
  size_t wlen;
  mrb_bool need_free;
  uint8_t *buf = i2c_build_buf(mrb, args, argc, &wlen, sbuf, &need_free);

  int ret = mrb_i2c_write((int)unit, (uint8_t)addr, buf, wlen,
                           (uint32_t)timeout_ms * 1000);
  if (need_free) mrb_free(mrb, buf);
  if (ret < 0) {
    mrb_raise(mrb, E_IO_ERROR, "I2C write failed");
  }
  return mrb_fixnum_value(ret);
}

static mrb_value
mrb_i2c_m_read(mrb_state *mrb, mrb_value self)
{
  mrb_value *args;
  mrb_int argc, addr, len;
  const mrb_sym kw_names[] = { MRB_SYM(timeout) };
  mrb_value kw_values[1];
  mrb_kwargs kwargs = { 1, 0, kw_names, kw_values, NULL };

  mrb_get_args(mrb, "ii*:", &addr, &len, &args, &argc, &kwargs);

  if (len <= 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "read length must be positive");
  }

  mrb_int timeout_ms = get_timeout(mrb, self, kw_values[0]);
  mrb_int unit = mrb_integer(mrb_iv_get(mrb, self, MRB_IVSYM(unit_num)));
  uint32_t timeout_us = (uint32_t)timeout_ms * 1000;

  uint8_t *rxbuf = (uint8_t*)mrb_malloc(mrb, len);
  int ret;

  if (argc > 0) {
    /* write-then-read (repeated START) */
    uint8_t sbuf[STACK_BUF_SIZE];
    size_t wlen;
    mrb_bool need_free;
    uint8_t *wbuf = i2c_build_buf(mrb, args, argc, &wlen, sbuf, &need_free);

    ret = mrb_i2c_write_read((int)unit, (uint8_t)addr,
                              wbuf, wlen, rxbuf, (size_t)len, timeout_us);
    if (need_free) mrb_free(mrb, wbuf);
  }
  else {
    ret = mrb_i2c_read((int)unit, (uint8_t)addr, rxbuf, (size_t)len, timeout_us);
  }

  if (ret < 0) {
    mrb_free(mrb, rxbuf);
    mrb_raise(mrb, E_IO_ERROR, "I2C read failed");
  }
  mrb_value str = mrb_str_new(mrb, (const char*)rxbuf, ret);
  mrb_free(mrb, rxbuf);
  return str;
}

static mrb_value
mrb_i2c_m_init(mrb_state *mrb, mrb_value self)
{
  const char *unit;
  mrb_int freq, sda, scl;

  mrb_get_args(mrb, "ziii", &unit, &freq, &sda, &scl);

  int num = mrb_i2c_unit_name_to_num(unit);
  if (num < 0) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unknown I2C unit: %s", unit);
  }
  mrb_i2c_status st = mrb_i2c_init(num, (uint32_t)freq, (int8_t)sda, (int8_t)scl);
  if (st != MRB_I2C_OK) {
    mrb_raise(mrb, E_IO_ERROR, "I2C init failed");
  }
  return mrb_fixnum_value(num);
}

void
mrb_hw_i2c_gem_init(mrb_state *mrb)
{
  struct RClass *cls = mrb_define_class_id(mrb, MRB_SYM(I2C), mrb->object_class);
  mrb_define_method_id(mrb, cls, MRB_SYM(__init), mrb_i2c_m_init, MRB_ARGS_REQ(4));
  mrb_define_method_id(mrb, cls, MRB_SYM(write), mrb_i2c_m_write, MRB_ARGS_REQ(1)|MRB_ARGS_REST()|MRB_ARGS_KEY(1, 0));
  mrb_define_method_id(mrb, cls, MRB_SYM(read), mrb_i2c_m_read, MRB_ARGS_REQ(2)|MRB_ARGS_REST()|MRB_ARGS_KEY(1, 0));
}

void
mrb_hw_i2c_gem_final(mrb_state *mrb)
{
}
