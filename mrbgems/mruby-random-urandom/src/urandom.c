/*
** urandom.c - Random.urandom
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/internal.h>
#include "urandom_hal.h"

/*
 * call-seq:
 *   Random.urandom(size) -> string
 *
 * Returns a binary String of size random bytes from the platform, read from
 * /dev/urandom on POSIX and BCryptGenRandom on Windows. Unlike Random.bytes,
 * these do not come from a seeded generator, and suit keys and nonces.
 * Raises RuntimeError when the platform cannot give them.
 *
 *   Random.urandom(4)  #=> "\x8F\x12\xA3\x7C"
 */
static mrb_value
random_s_urandom(mrb_state *mrb, mrb_value self)
{
  mrb_int len = mrb_as_int(mrb, mrb_get_arg1(mrb));
  if (len < 0) mrb_raise(mrb, E_ARGUMENT_ERROR, "negative string size");

  mrb_value str = mrb_str_new(mrb, NULL, len);
  RSTR_ENCODING_SET(mrb_str_ptr(str), MRB_STR_ENCODING_BINARY);
  if (len > 0 && mrb_hal_urandom(mrb, RSTRING_PTR(str), (size_t)len) != 0) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "failed to get urandom");
  }
  return str;
}

void
mrb_mruby_random_urandom_gem_init(mrb_state *mrb)
{
  struct RClass *random = mrb_class_get_id(mrb, MRB_SYM(Random));
  mrb_define_class_method_id(mrb, random, MRB_SYM(urandom), random_s_urandom, MRB_ARGS_REQ(1));
}

void
mrb_mruby_random_urandom_gem_final(mrb_state *mrb)
{
}
