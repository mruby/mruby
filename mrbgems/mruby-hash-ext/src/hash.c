#include "mruby.h"
#include "mruby/hash.h"

/*
 *  call-seq:
 *     Hash.try_convert(obj) -> hash or nil
 *
 *  Try to convert <i>obj</i> into a hash, using to_hash method.
 *  Returns converted hash or nil if <i>obj</i> cannot be converted
 *  for any reason.
 *
 *     Hash.try_convert({1=>2})   # => {1=>2}
 *     Hash.try_convert("1=>2")   # => nil
 */

static mrb_value
mrb_hash_s_try_convert(mrb_state *mrb, mrb_value self)
{
  mrb_value hash;

  mrb_get_args(mrb, "o", &hash);
  return mrb_check_hash_type(mrb, hash);
}

void
mrb_mruby_hash_ext_gem_init(mrb_state* mrb)
{
  struct RClass * h = mrb->hash_class;

  mrb_define_class_method(mrb, h, "try_convert", mrb_hash_s_try_convert, ARGS_REQ(1));
}

void
mrb_mruby_hash_ext_gem_final(mrb_state* mrb)
{
}

