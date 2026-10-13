#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/numeric.h>
#include <string.h>

/* BigintTest.int64_roundtrip(integer) -> Integer
 *
 * Sends an Integer through the int64_t conversion a C extension uses to read
 * one, then builds an Integer back from the result.  A value the conversion
 * carries intact therefore comes back equal to itself, and one it refuses
 * raises RangeError instead.  Whether the argument arrives as a Fixnum or as
 * a Bignum depends on the width of mrb_int, and neither the reading nor the
 * writing call here has to know which it is.
 */
static mrb_value
test_int64_roundtrip(mrb_state *mrb, mrb_value self)
{
  mrb_value v;

  mrb_get_args(mrb, "o", &v);
  return mrb_int64_value(mrb, mrb_as_int64(mrb, v));
}

/* BigintTest.uint64_roundtrip(integer) -> Integer
 *
 * The unsigned counterpart.  The range it carries reaches one bit further up
 * and stops at zero, so a negative value raises RangeError however narrow it
 * is.
 */
static mrb_value
test_uint64_roundtrip(mrb_state *mrb, mrb_value self)
{
  mrb_value v;

  mrb_get_args(mrb, "o", &v);
  return mrb_uint64_value(mrb, mrb_as_uint64(mrb, v));
}


/* BigintTest.to_bytes(integer) -> [sign, [byte, ...]]
 *
 * The magnitude an extension writing a wire format would write, asked for
 * twice the way such a caller asks: once with no buffer, to learn the
 * length, and once with one.  Both answers are returned so a test can hold
 * them to each other.
 */
static mrb_value
test_to_bytes(mrb_state *mrb, mrb_value self)
{
  mrb_value v, ary;
  uint8_t *buf;
  size_t len, got, i;
  int sign = 0;

  mrb_get_args(mrb, "o", &v);
  len = mrb_integer_to_bytes(mrb, v, NULL, 0, NULL);
  buf = (uint8_t*)mrb_malloc(mrb, len ? len : 1);
  got = mrb_integer_to_bytes(mrb, v, buf, len, &sign);
  if (got != len) {
    mrb_free(mrb, buf);
    mrb_raise(mrb, E_RUNTIME_ERROR, "length asked for and length written differ");
  }
  ary = mrb_ary_new_capa(mrb, (mrb_int)len);
  for (i = 0; i < len; i++) {
    mrb_ary_push(mrb, ary, mrb_fixnum_value(buf[i]));
  }
  mrb_free(mrb, buf);
  return mrb_assoc_new(mrb, mrb_fixnum_value(sign), ary);
}

/* BigintTest.from_bytes(sign, [byte, ...]) -> Integer */
static mrb_value
test_from_bytes(mrb_state *mrb, mrb_value self)
{
  mrb_value ary, ret;
  mrb_int sign;
  uint8_t *buf;
  mrb_int len, i;

  mrb_get_args(mrb, "iA", &sign, &ary);
  len = RARRAY_LEN(ary);
  buf = (uint8_t*)mrb_malloc(mrb, len ? (size_t)len : 1);
  for (i = 0; i < len; i++) {
    buf[i] = (uint8_t)mrb_integer(mrb_ary_entry(ary, i));
  }
  ret = mrb_integer_from_bytes(mrb, buf, (size_t)len, (int)sign);
  mrb_free(mrb, buf);
  return ret;
}

/* BigintTest.to_bytes_short(integer) -> Integer
 *
 * What a buffer one byte too small answers: the length it needed, with
 * nothing written.
 */
static mrb_value
test_to_bytes_short(mrb_state *mrb, mrb_value self)
{
  mrb_value v;
  uint8_t buf[64];
  size_t len;

  mrb_get_args(mrb, "o", &v);
  len = mrb_integer_to_bytes(mrb, v, NULL, 0, NULL);
  if (len == 0 || len > sizeof(buf)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "value does not suit this probe");
  }
  memset(buf, 0xff, sizeof(buf));
  if (mrb_integer_to_bytes(mrb, v, buf, len - 1, NULL) != len) return mrb_fixnum_value(-1);
  for (size_t i = 0; i < sizeof(buf); i++) {
    if (buf[i] != 0xff) return mrb_fixnum_value(-2);   /* it wrote something */
  }
  return mrb_fixnum_value((mrb_int)len);
}

/* BigintTest.unnormalized(integer) -> Integer
 *
 * Builds a Bignum that holds a value an mrb_int can hold, as C code using
 * mrb_bint_new_int() can, so that a method reached with it sees a Bignum
 * where Ruby code would only ever pass a Fixnum.
 */
mrb_value mrb_bint_new_int(mrb_state *mrb, mrb_int x);

static mrb_value
test_unnormalized(mrb_state *mrb, mrb_value self)
{
  mrb_int i;

  mrb_get_args(mrb, "i", &i);
  return mrb_bint_new_int(mrb, i);
}

void
mrb_mruby_bigint_gem_test(mrb_state *mrb)
{
  struct RClass *test = mrb_define_module(mrb, "BigintTest");

  mrb_define_module_function(mrb, test, "int64_roundtrip", test_int64_roundtrip,
                             MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, test, "uint64_roundtrip", test_uint64_roundtrip,
                             MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, test, "to_bytes", test_to_bytes, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, test, "from_bytes", test_from_bytes, MRB_ARGS_REQ(2));
  mrb_define_module_function(mrb, test, "to_bytes_short", test_to_bytes_short,
                             MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, test, "unnormalized", test_unnormalized,
                             MRB_ARGS_REQ(1));
}
