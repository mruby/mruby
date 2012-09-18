#include <mruby.h>
#include <mruby/string.h>
#include "md5.h"

static struct RClass *_class_md5;

/*********************************************************
 * main
 *********************************************************/

static char
nr2char(int n) {
  if (0 <= n && n <= 9) {
    return '0' + n;
  } else {
    return 'a' + n - 10;
  }
}

static mrb_value
mrb_md5_hex(mrb_state *mrb, mrb_value self)
{
  md5_state_t pms;
  md5_byte_t digest[16];
  md5_byte_t digest_hex[33];
  mrb_value arg;
  int i;

  mrb_get_args(mrb, "o", &arg);
  if (mrb_nil_p(arg) || mrb_type(arg) != MRB_TT_STRING) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid argument");
  }

  md5_init(&pms);
  md5_append(&pms, (const md5_byte_t*) RSTRING_PTR(arg), RSTRING_CAPA(arg));
  md5_finish(&pms, digest);


  for (i = 0; i < 16; i++) {
    digest_hex[i*2+0] = nr2char((digest[i] >> 4) & 0xf);
    digest_hex[i*2+1] = nr2char(digest[i] & 0x0f);
  }
  digest_hex[32] = 0;

  return mrb_str_new(mrb, (char*) digest_hex, 32);
}

/*********************************************************
 * register
 *********************************************************/

void
mrb_md5_gem_init(mrb_state* mrb) {
  _class_md5 = mrb_define_module(mrb, "MD5");
  mrb_define_class_method(mrb, _class_md5, "md5_hex", mrb_md5_hex, ARGS_REQ(1));
}
