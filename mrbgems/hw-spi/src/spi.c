#include <string.h>
#include <mruby.h>
#include <mruby/presym.h>
#include <mruby/variable.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/data.h>
#include <mruby/class.h>
#include <mruby/spi.h>

#define STACK_BUF_SIZE 256
#define E_IO_ERROR mrb_exc_get_id(mrb, MRB_SYM(IOError))

static void
spi_info_free(mrb_state *mrb, void *ptr)
{
  mrb_free(mrb, ptr);
}

static const struct mrb_data_type spi_info_type = { "SPI", spi_info_free };

#define SPI_INFO(self) \
  ((mrb_spi_info*)mrb_data_get_ptr(mrb, self, &spi_info_type))

static size_t
spi_calc_size(mrb_state *mrb, mrb_value *args, mrb_int argc)
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

static void
spi_fill_buf(mrb_state *mrb, uint8_t *buf, mrb_value *args, mrb_int argc,
             size_t pad)
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
  memset(&buf[pos], 0, pad);
}

/* SPI.new(unit:, frequency:, sck_pin:, cipo_pin:, copi_pin:,
           cs_pin:, mode:, first_bit:) */
static mrb_value
mrb_spi_s_new(mrb_state *mrb, mrb_value klass)
{
  const char *unit_name;
  mrb_int freq = 100000, sck = -1, cipo = -1, copi = -1, cs = -1;
  mrb_int mode = 0, first_bit = MRB_SPI_MSB_FIRST;

  const mrb_sym kw_names[] = {
    MRB_SYM(unit), MRB_SYM(frequency), MRB_SYM(sck_pin),
    MRB_SYM(cipo_pin), MRB_SYM(copi_pin), MRB_SYM(cs_pin),
    MRB_SYM(mode), MRB_SYM(first_bit)
  };
  mrb_value kw_values[8];
  mrb_kwargs kwargs = { 8, 7, kw_names, kw_values, NULL };
  mrb_get_args(mrb, ":", &kwargs);

  /* unit is required */
  if (mrb_undef_p(kw_values[0])) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "unit: is required");
  }
  unit_name = mrb_str_to_cstr(mrb, mrb_sym_str(mrb, mrb_symbol(kw_values[0])));

  if (!mrb_undef_p(kw_values[1])) freq = mrb_integer(kw_values[1]);
  if (!mrb_undef_p(kw_values[2])) sck = mrb_integer(kw_values[2]);
  if (!mrb_undef_p(kw_values[3])) cipo = mrb_integer(kw_values[3]);
  if (!mrb_undef_p(kw_values[4])) copi = mrb_integer(kw_values[4]);
  if (!mrb_undef_p(kw_values[5])) cs = mrb_integer(kw_values[5]);
  if (!mrb_undef_p(kw_values[6])) mode = mrb_integer(kw_values[6]);
  if (!mrb_undef_p(kw_values[7])) first_bit = mrb_integer(kw_values[7]);

  int num = mrb_spi_unit_name_to_num(unit_name);
  if (num < 0) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unknown SPI unit: %s", unit_name);
  }

  mrb_spi_info *info = (mrb_spi_info*)mrb_malloc(mrb, sizeof(mrb_spi_info));
  info->unit_num  = (uint8_t)num;
  info->frequency = (uint32_t)freq;
  info->sck_pin   = (int8_t)sck;
  info->cipo_pin  = (int8_t)cipo;
  info->copi_pin  = (int8_t)copi;
  info->cs_pin    = (int8_t)cs;
  info->mode      = (uint8_t)mode;
  info->first_bit = (uint8_t)first_bit;

  mrb_value self = mrb_obj_value(
    Data_Wrap_Struct(mrb, mrb_class_ptr(klass), &spi_info_type, info));

  mrb_spi_status st = mrb_spi_init(info);
  if (st != MRB_SPI_OK) {
    mrb_raise(mrb, E_IO_ERROR, "SPI init failed");
  }
  return self;
}

/* SPI#write(*data) */
static mrb_value
mrb_spi_m_write(mrb_state *mrb, mrb_value self)
{
  mrb_value *args;
  mrb_int argc;
  mrb_get_args(mrb, "*", &args, &argc);

  mrb_spi_info *info = SPI_INFO(self);
  size_t total = spi_calc_size(mrb, args, argc);
  if (total == 0) return mrb_fixnum_value(0);

  uint8_t sbuf[STACK_BUF_SIZE];
  uint8_t *buf = sbuf;
  mrb_bool need_free = FALSE;
  if (total > STACK_BUF_SIZE) {
    buf = (uint8_t*)mrb_malloc(mrb, total);
    need_free = TRUE;
  }
  spi_fill_buf(mrb, buf, args, argc, 0);

  int ret = mrb_spi_write(info, buf, total);
  if (need_free) mrb_free(mrb, buf);
  if (ret < 0) mrb_raise(mrb, E_IO_ERROR, "SPI write failed");
  return mrb_fixnum_value(ret);
}

/* SPI#read(len, tx_value=0) */
static mrb_value
mrb_spi_m_read(mrb_state *mrb, mrb_value self)
{
  mrb_int len, tx_val = 0;
  mrb_get_args(mrb, "i|i", &len, &tx_val);
  if (len <= 0) mrb_raise(mrb, E_ARGUMENT_ERROR, "length must be positive");

  mrb_spi_info *info = SPI_INFO(self);
  uint8_t *buf = (uint8_t*)mrb_malloc(mrb, len);
  int ret = mrb_spi_read(info, buf, (size_t)len, (uint8_t)tx_val);
  if (ret < 0) {
    mrb_free(mrb, buf);
    mrb_raise(mrb, E_IO_ERROR, "SPI read failed");
  }
  mrb_value str = mrb_str_new(mrb, (const char*)buf, ret);
  mrb_free(mrb, buf);
  return str;
}

/* SPI#transfer(*data, additional_read_bytes: 0) */
static mrb_value
mrb_spi_m_transfer(mrb_state *mrb, mrb_value self)
{
  mrb_value *args;
  mrb_int argc, extra = 0;
  const mrb_sym kw_names[] = { MRB_SYM(additional_read_bytes) };
  mrb_value kw_values[1];
  mrb_kwargs kwargs = { 1, 0, kw_names, kw_values, NULL };
  mrb_get_args(mrb, "*:", &args, &argc, &kwargs);

  if (!mrb_undef_p(kw_values[0])) extra = mrb_integer(kw_values[0]);

  mrb_spi_info *info = SPI_INFO(self);
  size_t total = spi_calc_size(mrb, args, argc) + (size_t)extra;
  if (total == 0) return mrb_str_new(mrb, "", 0);

  uint8_t sbuf_tx[STACK_BUF_SIZE], sbuf_rx[STACK_BUF_SIZE];
  uint8_t *tx = sbuf_tx, *rx = sbuf_rx;
  mrb_bool need_free = FALSE;
  if (total > STACK_BUF_SIZE) {
    tx = (uint8_t*)mrb_malloc(mrb, total * 2);
    rx = tx + total;
    need_free = TRUE;
  }
  spi_fill_buf(mrb, tx, args, argc, (size_t)extra);

  int ret = mrb_spi_transfer(info, tx, rx, total);
  if (ret < 0) {
    if (need_free) mrb_free(mrb, tx);
    mrb_raise(mrb, E_IO_ERROR, "SPI transfer failed");
  }
  mrb_value str = mrb_str_new(mrb, (const char*)rx, ret);
  if (need_free) mrb_free(mrb, tx);
  return str;
}

void
mrb_hw_spi_gem_init(mrb_state *mrb)
{
  struct RClass *cls = mrb_define_class_id(mrb, MRB_SYM(SPI), mrb->object_class);
  MRB_SET_INSTANCE_TT(cls, MRB_TT_CDATA);

  mrb_define_class_method_id(mrb, cls, MRB_SYM(new), mrb_spi_s_new, MRB_ARGS_KEY(8, 1));
  mrb_define_method_id(mrb, cls, MRB_SYM(write), mrb_spi_m_write, MRB_ARGS_REST());
  mrb_define_method_id(mrb, cls, MRB_SYM(read), mrb_spi_m_read, MRB_ARGS_ARG(1, 1));
  mrb_define_method_id(mrb, cls, MRB_SYM(transfer), mrb_spi_m_transfer, MRB_ARGS_REST()|MRB_ARGS_KEY(1, 0));
}

void
mrb_hw_spi_gem_final(mrb_state *mrb)
{
}
