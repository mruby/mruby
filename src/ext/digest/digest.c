/*
** digest.c - Digest and subclasses
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <string.h>
#include "mruby/array.h"
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/string.h"
#include "mruby/variable.h"

#include <err.h>

#define NAMESYM "__name__"

static mrb_value mrb_digest_hexdigest(mrb_state *, mrb_value);

struct mrb_evp {
  EVP_MD_CTX ctx;
  const EVP_MD *md;
};

struct mrb_hmac {
  HMAC_CTX ctx;
  const EVP_MD *md;
  char *key;
  int keylen;
};

static void
mrb_evp_free(mrb_state *mrb, void *ptr)
{
  struct mrb_evp *evp = ptr;

  EVP_MD_CTX_cleanup(&evp->ctx);
  mrb_free(mrb, evp);
}

static void
mrb_hmac_free(mrb_state *mrb, void *ptr)
{
  struct mrb_hmac *hmac = ptr;

  HMAC_CTX_cleanup(&hmac->ctx);
  memset(hmac->key, 0, hmac->keylen);
  mrb_free(mrb, hmac->key);
  mrb_free(mrb, hmac);
}

static struct mrb_data_type mrb_evp_type = { "EVP", mrb_evp_free };
static struct mrb_data_type mrb_hmac_type = { "HMAC", mrb_hmac_free };


static void
basecheck(mrb_state *mrb, mrb_value self, const char **namep, struct mrb_evp **evpp)
{
  struct RClass *c;
  struct mrb_evp *evp;
  mrb_value name;

  c = mrb_obj_class(mrb, self);
  name = mrb_const_get(mrb, mrb_obj_value(c), mrb_intern(mrb, NAMESYM));
  if (mrb_nil_p(name)) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "Digest::Base is an abstract class");
  }
  evp = (struct mrb_evp *)mrb_get_datatype(mrb, self, &mrb_evp_type);
  if (!evp) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "no evp found (BUG?)");
  }
  if (namep) *namep = RSTRING_PTR(name);
  if (evpp) *evpp = evp;
}

static mrb_value
mrb_evp_wrap(mrb_state *mrb, struct RClass *c, struct mrb_evp *evp)
{
  return mrb_obj_value(Data_Wrap_Struct(mrb, c, &mrb_evp_type, evp));
}

static mrb_value
mrb_digest_block_length(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *evp;

  basecheck(mrb, self, NULL, &evp);
  return mrb_fixnum_value(EVP_MD_block_size(evp->md));
}

static mrb_value
mrb_digest_digest(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *evp;
  EVP_MD_CTX ctx;
  unsigned int mdlen;
  unsigned char mdstr[EVP_MAX_MD_SIZE];

  evp = (struct mrb_evp *)mrb_get_datatype(mrb, self, &mrb_evp_type);
  if (!evp) return mrb_nil_value();
  EVP_MD_CTX_copy(&ctx, &evp->ctx);
  EVP_DigestFinal_ex(&ctx, mdstr, &mdlen);
  return mrb_str_new(mrb, mdstr, mdlen);
}

static mrb_value
mrb_digest_digest_bang(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *evp;
  unsigned int mdlen;
  unsigned char mdstr[EVP_MAX_MD_SIZE];

  evp = (struct mrb_evp *)mrb_get_datatype(mrb, self, &mrb_evp_type);
  if (!evp) return mrb_nil_value();
  EVP_DigestFinal_ex(&evp->ctx, mdstr, &mdlen);
  EVP_DigestInit_ex(&evp->ctx, evp->md, NULL);
  return mrb_str_new(mrb, mdstr, mdlen);
}

static mrb_value
mrb_digest_eq(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *e1, *e2;
  EVP_MD_CTX c1, c2;
  mrb_value h, other;
  unsigned l1, l2;
  unsigned char d1[EVP_MAX_MD_SIZE], d2[EVP_MAX_MD_SIZE];

  basecheck(mrb, self, NULL, &e1);
  mrb_get_args(mrb, "o", &other);
  if (mrb_type(other) == MRB_TT_STRING) {
    h = mrb_digest_hexdigest(mrb, self);
    if (mrb_nil_p(h)) return mrb_false_value();
    if (mrb_str_cmp(mrb, h, other) != 0) return mrb_false_value();
    return mrb_true_value();
  } else {
    e2 = (struct mrb_evp *)mrb_get_datatype(mrb, other, &mrb_evp_type);
    if (!e2) return mrb_false_value();

    EVP_MD_CTX_copy(&c1, &e1->ctx);
    EVP_MD_CTX_copy(&c2, &e2->ctx);
    EVP_DigestFinal_ex(&c1, d1, &l1);
    EVP_DigestFinal_ex(&c2, d2, &l2);
    if (l1 != l2)
      return mrb_false_value();
    if (memcmp(d1, d2, l1) != 0)
      return mrb_false_value();
    return mrb_true_value();
  }
}

static mrb_value
mrb_digest_hexdigest(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *evp;
  EVP_MD_CTX ctx;
  unsigned int mdlen;
  int i;
  unsigned char mdstr[EVP_MAX_MD_SIZE];
  unsigned char distr[EVP_MAX_MD_SIZE * 2 + 1];
  unsigned char hex[] = "0123456789abcdef";

  evp = (struct mrb_evp *)mrb_get_datatype(mrb, self, &mrb_evp_type);
  if (!evp) return mrb_nil_value();
  EVP_MD_CTX_copy(&ctx, &evp->ctx);
  EVP_DigestFinal_ex(&ctx, mdstr, &mdlen);
  for (i = 0; i < mdlen; i++) {
    distr[i*2] = hex[(mdstr[i] / 16) % 16];
    distr[i*2+1] = hex[mdstr[i] % 16];
  }
  distr[i*2] = '\0';
  return mrb_str_new2(mrb, distr);
}

static mrb_value
mrb_digest_hexdigest_bang(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *evp;
  unsigned int mdlen;
  int i;
  unsigned char mdstr[EVP_MAX_MD_SIZE];
  unsigned char distr[EVP_MAX_MD_SIZE * 2 + 1];
  unsigned char hex[] = "0123456789abcdef";

  evp = (struct mrb_evp *)mrb_get_datatype(mrb, self, &mrb_evp_type);
  if (!evp) return mrb_nil_value();
  EVP_DigestFinal_ex(&evp->ctx, mdstr, &mdlen);
  EVP_DigestInit_ex(&evp->ctx, evp->md, NULL);
  for (i = 0; i < mdlen; i++) {
    distr[i*2] = hex[(mdstr[i] / 16) % 16];
    distr[i*2+1] = hex[mdstr[i] % 16];
  }
  distr[i*2] = '\0';
  return mrb_str_new2(mrb, distr);
}


static mrb_value
mrb_digest_init(mrb_state *mrb, mrb_value self)
{
  struct RClass *c;
  struct mrb_evp *evp;
  mrb_value name;

  c = mrb_obj_class(mrb, self);
  name = mrb_const_get(mrb, mrb_obj_value(c), mrb_intern(mrb, NAMESYM));
  if (mrb_nil_p(name)) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "Digest::Base is an abstract class");
  }

  evp = (struct mrb_evp *)mrb_get_datatype(mrb, self, &mrb_evp_type);
  if (evp) {
    mrb_evp_free(mrb, evp);
  }

  evp = (struct mrb_evp *)mrb_malloc(mrb, sizeof(*evp));
  evp->md = EVP_get_digestbyname(RSTRING_PTR(name));
  if (evp->md == NULL)
    mrb_raise(mrb, E_RUNTIME_ERROR, "no md5");

  EVP_MD_CTX_init(&evp->ctx);
  EVP_DigestInit_ex(&evp->ctx, evp->md, NULL);

  DATA_PTR(self) = evp;
  DATA_TYPE(self) = &mrb_evp_type;
  return self;
}

static mrb_value
mrb_digest_init_copy(mrb_state *mrb, mrb_value copy)
{
  struct mrb_evp *e1, *e2;
  mrb_value src;

  mrb_get_args(mrb, "o", &src);
  if (mrb_obj_equal(mrb, copy, src)) return copy;
  if (!mrb_obj_is_instance_of(mrb, src, mrb_obj_class(mrb, copy))) {
    mrb_raise(mrb, E_TYPE_ERROR, "wrong argument class");
  }
  if (!DATA_PTR(copy)) {
    DATA_PTR(copy) = mrb_malloc(mrb, sizeof(struct mrb_evp));
    DATA_TYPE(copy) = &mrb_evp_type;
  }
  e1 = DATA_PTR(src);
  e2 = DATA_PTR(copy);
  e2->md = e1->md;
  EVP_MD_CTX_init(&e2->ctx);
  EVP_DigestInit_ex(&e2->ctx, e2->md, NULL);
  return copy;
}

static mrb_value
mrb_digest_reset(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *evp;
  const char *name;

  basecheck(mrb, self, &name, &evp);
  EVP_MD_CTX_init(&evp->ctx);
  EVP_DigestInit_ex(&evp->ctx, evp->md, NULL);
  return self;
}

static mrb_value
mrb_digest_size(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *evp;
  const char *name;

  basecheck(mrb, self, &name, &evp);
  return mrb_fixnum_value(EVP_MD_size(evp->md));
}

static mrb_value
mrb_digest_update(mrb_state *mrb, mrb_value self)
{
  struct mrb_evp *evp;
  int len;
  char *str;

  evp = (struct mrb_evp *)mrb_get_datatype(mrb, self, &mrb_evp_type);
  if (!evp) return mrb_nil_value();
  mrb_get_args(mrb, "s", &str, &len);
  EVP_DigestUpdate(&evp->ctx, str, len);
  return self;
}

static mrb_value
mrb_digest_s_digest(mrb_state *mrb, mrb_value klass)
{
  struct mrb_evp *evp;
  mrb_value d, name;

  name = mrb_const_get(mrb, klass, mrb_intern(mrb, NAMESYM));
  if (mrb_nil_p(name)) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "Digest::Base is an abstract class");
  }

  evp = (struct mrb_evp *)mrb_malloc(mrb, sizeof(struct mrb_evp));
  evp->md = EVP_get_digestbyname(RSTRING_PTR(name));
  if (evp->md == NULL)
    mrb_raise(mrb, E_RUNTIME_ERROR, "no md5");

  EVP_MD_CTX_init(&evp->ctx);
  EVP_DigestInit_ex(&evp->ctx, evp->md, NULL);
  d = mrb_evp_wrap(mrb, mrb_class_ptr(klass), evp);
  mrb_digest_update(mrb, d);
  return mrb_digest_digest(mrb, d);
}

static mrb_value
mrb_digest_s_hexdigest(mrb_state *mrb, mrb_value klass)
{
  struct mrb_evp *evp;
  mrb_value d, name;

  name = mrb_const_get(mrb, klass, mrb_intern(mrb, NAMESYM));
  if (mrb_nil_p(name)) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "Digest::Base is an abstract class");
  }

  evp = (struct mrb_evp *)mrb_malloc(mrb, sizeof(struct mrb_evp));
  evp->md = EVP_get_digestbyname(RSTRING_PTR(name));
  if (evp->md == NULL)
    mrb_raise(mrb, E_RUNTIME_ERROR, "no %s", RSTRING_PTR(name));

  EVP_MD_CTX_init(&evp->ctx);
  EVP_DigestInit_ex(&evp->ctx, evp->md, NULL);
  d = mrb_evp_wrap(mrb, mrb_class_ptr(klass), evp);
  mrb_digest_update(mrb, d);
  return mrb_digest_hexdigest(mrb, d);
}

static mrb_value
mrb_hmac_block_length(mrb_state *mrb, mrb_value self)
{
  struct mrb_hmac *hmac;

  hmac = (struct mrb_hmac *)mrb_get_datatype(mrb, self, &mrb_hmac_type);
  if (!hmac) return mrb_nil_value();
  return mrb_fixnum_value(EVP_MD_block_size(hmac->md));
}

static mrb_value
mrb_hmac_digest(mrb_state *mrb, mrb_value self)
{
  struct mrb_hmac *hmac;
  HMAC_CTX ctx;
  unsigned int mdlen;
  unsigned char mdstr[EVP_MAX_MD_SIZE];

  hmac = (struct mrb_hmac *)mrb_get_datatype(mrb, self, &mrb_hmac_type);
  if (!hmac) return mrb_nil_value();
  HMAC_CTX_copy(&ctx, &hmac->ctx);
  HMAC_Final(&ctx, mdstr, &mdlen);
  return mrb_str_new(mrb, mdstr, mdlen);
}

static mrb_value
mrb_hmac_digest_length(mrb_state *mrb, mrb_value self)
{
  struct mrb_hmac *hmac;

  hmac = (struct mrb_hmac *)mrb_get_datatype(mrb, self, &mrb_hmac_type);
  if (!hmac) return mrb_nil_value();
  return mrb_fixnum_value(EVP_MD_size(hmac->md));
}

static mrb_value
mrb_hmac_hexdigest(mrb_state *mrb, mrb_value self)
{
  struct mrb_hmac *hmac;
  HMAC_CTX ctx;
  unsigned int mdlen;
  int i;
  unsigned char mdstr[EVP_MAX_MD_SIZE];
  unsigned char distr[EVP_MAX_MD_SIZE * 2 + 1];
  unsigned char hex[] = "0123456789abcdef";

  hmac = (struct mrb_hmac *)mrb_get_datatype(mrb, self, &mrb_hmac_type);
  if (!hmac) return mrb_nil_value();
  HMAC_CTX_copy(&ctx, &hmac->ctx);
  HMAC_Final(&ctx, mdstr, &mdlen);
  for (i = 0; i < mdlen; i++) {
    distr[i*2] = hex[(mdstr[i] / 16) % 16];
    distr[i*2+1] = hex[mdstr[i] % 16];
  }
  distr[i*2] = '\0';
  return mrb_str_new2(mrb, distr);
}

static mrb_value
mrb_hmac_init(mrb_state *mrb, mrb_value self)
{
  struct mrb_hmac *hmac;
  mrb_value digest, name;
  int keylen;
  char *key;

  mrb_get_args(mrb, "so", &key, &keylen, &digest);
  name = mrb_const_get(mrb, digest, mrb_intern(mrb, NAMESYM));
  if (mrb_nil_p(name)) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "not a digester");
  }

  hmac = (struct mrb_hmac *)mrb_get_datatype(mrb, self, &mrb_hmac_type);
  if (hmac) {
    mrb_hmac_free(mrb, hmac);
  }

  hmac = (struct mrb_hmac *)mrb_malloc(mrb, sizeof(*hmac));
  hmac->md = EVP_get_digestbyname(RSTRING_PTR(name));
  hmac->key = mrb_malloc(mrb, keylen);
  memcpy(hmac->key, key, keylen);
  hmac->keylen = keylen;
  HMAC_CTX_init(&hmac->ctx);
  HMAC_Init_ex(&hmac->ctx, key, keylen, hmac->md, NULL);

  DATA_PTR(self) = hmac;
  DATA_TYPE(self) = &mrb_hmac_type;
  return self;
}

static mrb_value
mrb_hmac_init_copy(mrb_state *mrb, mrb_value copy)
{
  mrb_raise(mrb, E_RUNTIME_ERROR, "cannot duplicate HMAC");
  return copy;
}

static mrb_value
mrb_hmac_reset(mrb_state *mrb, mrb_value self)
{
  struct mrb_hmac *hmac;

  hmac = (struct mrb_hmac *)mrb_get_datatype(mrb, self, &mrb_hmac_type);
  if (!hmac) return mrb_nil_value();
  HMAC_CTX_cleanup(&hmac->ctx);
  HMAC_CTX_init(&hmac->ctx);
  HMAC_Init_ex(&hmac->ctx, hmac->key, hmac->keylen, hmac->md, NULL);
  return self;
}

static mrb_value
mrb_hmac_update(mrb_state *mrb, mrb_value self)
{
  struct mrb_hmac *hmac;
  int len;
  char *str;

  hmac = (struct mrb_hmac *)mrb_get_datatype(mrb, self, &mrb_hmac_type);
  if (!hmac) return mrb_nil_value();
  mrb_get_args(mrb, "s", &str, &len);
  HMAC_Update(&hmac->ctx, str, len);
  return self;
}


void
mrb_init_digest(mrb_state *mrb)
{
  struct RClass *b, *d, *h;
  struct RClass *md5c, *rmd160c, *sha1c, *sha256c, *sha384c, *sha512c;

  OpenSSL_add_all_digests();

  d = mrb_define_module(mrb, "Digest");

  b = mrb_define_class_under(mrb, d, "Base", mrb->object_class);
  mrb_define_class_method(mrb, b, "digest", mrb_digest_s_digest, ARGS_REQ(1));
  //mrb_define_class_method(mrb, b, "file", mrb_digest_s_file, ARGS_REQ(1));
  mrb_define_class_method(mrb, b, "hexdigest", mrb_digest_s_hexdigest, ARGS_REQ(1));
  mrb_define_method(mrb, b, "block_length", mrb_digest_block_length, ARGS_NONE());
  mrb_define_method(mrb, b, "digest", mrb_digest_digest, ARGS_NONE());
  mrb_define_method(mrb, b, "digest!", mrb_digest_digest_bang, ARGS_NONE()); /* XXX: can be defined in mrblib... */
  mrb_define_method(mrb, b, "digest_length", mrb_digest_size, ARGS_NONE());
  //mrb_define_method(mrb, b, "file", mrb_digest_file, ARGS_REQ(1));
  mrb_define_method(mrb, b, "hexdigest", mrb_digest_hexdigest, ARGS_NONE());
  mrb_define_method(mrb, b, "hexdigest!", mrb_digest_hexdigest_bang, ARGS_NONE()); /* XXX: can be defined in mrblib... */
  mrb_define_method(mrb, b, "initialize", mrb_digest_init, ARGS_NONE());
  mrb_define_method(mrb, b, "initialize_copy", mrb_digest_init_copy, ARGS_REQ(1));
  mrb_define_method(mrb, b, "length", mrb_digest_size, ARGS_NONE());
  mrb_define_method(mrb, b, "reset", mrb_digest_reset, ARGS_NONE());
  mrb_define_method(mrb, b, "size", mrb_digest_size, ARGS_NONE());
  mrb_define_method(mrb, b, "to_s", mrb_digest_hexdigest, ARGS_NONE());
  mrb_define_method(mrb, b, "update", mrb_digest_update, ARGS_REQ(1));
  mrb_define_method(mrb, b, "==", mrb_digest_eq, ARGS_REQ(1));
  mrb_define_method(mrb, b, "<<", mrb_digest_update, ARGS_REQ(1));

  md5c = mrb_define_class_under(mrb, d, "MD5", b);
  MRB_SET_INSTANCE_TT(md5c, MRB_TT_DATA);
  mrb_define_const(mrb, md5c, NAMESYM, mrb_str_new2(mrb, "md5"));

  rmd160c = mrb_define_class_under(mrb, d, "RMD160", b);
  MRB_SET_INSTANCE_TT(rmd160c, MRB_TT_DATA);
  mrb_define_const(mrb, rmd160c, NAMESYM, mrb_str_new2(mrb, "ripemd160"));

  sha1c = mrb_define_class_under(mrb, d, "SHA1", b);
  MRB_SET_INSTANCE_TT(sha1c, MRB_TT_DATA);
  mrb_define_const(mrb, sha1c, NAMESYM, mrb_str_new2(mrb, "sha1"));

  sha256c = mrb_define_class_under(mrb, d, "SHA256", b);
  MRB_SET_INSTANCE_TT(sha256c, MRB_TT_DATA);
  mrb_define_const(mrb, sha256c, NAMESYM, mrb_str_new2(mrb, "sha256"));

  sha384c = mrb_define_class_under(mrb, d, "SHA384", b);
  MRB_SET_INSTANCE_TT(sha384c, MRB_TT_DATA);
  mrb_define_const(mrb, sha384c, NAMESYM, mrb_str_new2(mrb, "sha384"));

  sha512c = mrb_define_class_under(mrb, d, "SHA512", b);
  MRB_SET_INSTANCE_TT(sha512c, MRB_TT_DATA);
  mrb_define_const(mrb, sha512c, NAMESYM, mrb_str_new2(mrb, "sha512"));

  h = mrb_define_class_under(mrb, d, "HMAC", mrb->object_class);
  MRB_SET_INSTANCE_TT(h, MRB_TT_DATA);
  mrb_define_method(mrb, h, "block_length", mrb_hmac_block_length, ARGS_NONE());
  mrb_define_method(mrb, h, "digest", mrb_hmac_digest, ARGS_NONE());
  mrb_define_method(mrb, h, "digest_length", mrb_hmac_digest_length, ARGS_NONE());
  mrb_define_method(mrb, h, "hexdigest", mrb_hmac_hexdigest, ARGS_NONE());
  mrb_define_method(mrb, h, "initialize", mrb_hmac_init, ARGS_REQ(2));
  mrb_define_method(mrb, h, "initialize_copy", mrb_hmac_init_copy, ARGS_REQ(1));
  mrb_define_method(mrb, h, "reset", mrb_hmac_reset, ARGS_NONE());
  mrb_define_method(mrb, h, "update", mrb_hmac_update, ARGS_REQ(1));
}
