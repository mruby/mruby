#include <mruby.h>
#include <mruby/class.h>
#include <mruby/internal.h>

/* BigintTest.int64_roundtrip(integer) -> Integer
 *
 * Sends an Integer through the int64_t conversion a C extension uses to read
 * one, then builds an Integer back from the result.  A value the conversion
 * carries intact therefore comes back equal to itself, and one it refuses
 * raises RangeError instead.  Whether the argument arrives as a Bignum
 * depends on the width of mrb_int, so both representations are accepted.
 */
static mrb_value
test_int64_roundtrip(mrb_state *mrb, mrb_value self)
{
  mrb_value v;
  int64_t n;

  mrb_get_args(mrb, "o", &v);
  if (mrb_integer_p(v)) {
    n = (int64_t)mrb_integer(v);
  }
  else if (mrb_bigint_p(v)) {
    n = mrb_bint_as_int64(mrb, v);
  }
  else {
    mrb_raisef(mrb, E_TYPE_ERROR, "%Y is not an Integer", v);
    return mrb_nil_value();
  }

#ifdef MRB_INT32
  if (n < MRB_INT_MIN || n > MRB_INT_MAX) {
    return mrb_bint_new_int64(mrb, n);
  }
#endif
  return mrb_int_value(mrb, (mrb_int)n);
}

void
mrb_mruby_bigint_gem_test(mrb_state *mrb)
{
  struct RClass *test = mrb_define_module(mrb, "BigintTest");

  mrb_define_module_function(mrb, test, "int64_roundtrip", test_int64_roundtrip,
                             MRB_ARGS_REQ(1));
}
