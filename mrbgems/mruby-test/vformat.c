#include <string.h>
#include <mruby.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/string.h>

#define NATIVE_TYPES                                                        \
  char c;                                                                   \
  mrb_float f;                                                              \
  mrb_int i;                                                                \
  mrb_sym n;                                                                \
  char *s;                                                                  \
  struct RClass *C

typedef enum {
  ARG_c,
  ARG_f,
  ARG_i,
  ARG_n,
  ARG_s,
  ARG_C,
  ARG_v,
} VFArgumentType;

typedef struct {
  VFArgumentType type;
  union { NATIVE_TYPES; };
} VFNative;

typedef struct {
  VFArgumentType type;
  union {
    NATIVE_TYPES;
    mrb_value v;
  };
} VFArgument;

static void
native_free(mrb_state *mrb, void *data)
{
  VFNative *native = (VFNative*)data;
  if (native->type == ARG_s) mrb_free(mrb, native->s);
  mrb_free(mrb, native);
}

static const struct mrb_data_type native_data_type = {
  "TestVFormat::Native", native_free
};

static mrb_value
native_initialize(mrb_state *mrb, mrb_value self)
{
  VFNative data, *datap;
  mrb_value obj;

  mrb_get_args(mrb, "o", &obj);
  switch (mrb_type(obj)) {
    case MRB_TT_FLOAT:
      data.f = mrb_float(obj);
      data.type = ARG_f;
      break;
    case MRB_TT_FIXNUM:
      data.i = mrb_fixnum(obj);
      data.type = ARG_i;
      break;
    case MRB_TT_SYMBOL:
      data.n = mrb_symbol(obj);
      data.type = ARG_n;
      break;
    case MRB_TT_CLASS: case MRB_TT_SCLASS: case MRB_TT_MODULE:
      data.C = mrb_class_ptr(obj);
      data.type = ARG_C;
      break;
    case MRB_TT_STRING: {
      mrb_int len = RSTRING_LEN(obj);
      const char *s = RSTRING_PTR(obj);
      if (len == 1) {
        /* one byte string is considered char */
        data.c = s[0];
        data.type = ARG_c;
      }
      else {
        data.s = (char*)mrb_malloc(mrb, len + 1);
        memcpy(data.s, s, len);
        data.s[len] = '\0';
        data.type = ARG_s;
      }
      break;
    }
    default: {
      mrb_value msg = mrb_str_new_cstr(mrb, "native type for ");
      mrb_str_cat_cstr(mrb, msg, mrb_class_name(mrb, mrb_class(mrb, obj)));
      mrb_str_cat_cstr(mrb, msg, " is unknown");
      mrb_raise(mrb, E_ARGUMENT_ERROR, RSTRING_PTR(msg));
    }
  }

  datap = (VFNative*)mrb_malloc(mrb, sizeof(VFNative));
  *datap = data;
  mrb_data_init(self, datap, &native_data_type);
  return self;
}

static VFArgument*
arg_from_obj(mrb_state *mrb, mrb_value obj, struct RClass *native_class,
             VFArgument *vf_arg)
{
  if (mrb_obj_is_instance_of(mrb, obj, native_class)) {
    const VFNative *native = (VFNative*)DATA_PTR(obj);
    *(VFNative*)vf_arg = *native;
  }
  else {
    vf_arg->v = obj;
    vf_arg->type = ARG_v;
  }
  return vf_arg;
}

#define VF_FORMAT_INIT(klass)                                               \
  struct RClass *vf_native_class =                                          \
    mrb_class_get_under(mrb, mrb_class_ptr(klass), "Native");               \
  VFArgument vf_args[2];

#define VF_ARG(args, idx) \
  arg_from_obj(mrb, args[idx], vf_native_class, &vf_args[idx])

#define VF_FORMAT0(fmt) mrb_format(mrb, fmt);
#define VF_FORMAT1(fmt, args) \
  (VF_ARG(args, 0), VF_FORMAT_TYPED(fmt, 1, vf_args, NULL))
#define VF_FORMAT2(fmt, args) (                                             \
  VF_ARG(args, 0), VF_ARG(args, 1),                                         \
  VF_FORMAT2_COND_EXPR(fmt, vf_args, vf_args+1, c) :                        \
  VF_FORMAT2_COND_EXPR(fmt, vf_args, vf_args+1, f) :                        \
  VF_FORMAT2_COND_EXPR(fmt, vf_args, vf_args+1, i) :                        \
  VF_FORMAT2_COND_EXPR(fmt, vf_args, vf_args+1, n) :                        \
  VF_FORMAT2_COND_EXPR(fmt, vf_args, vf_args+1, s) :                        \
  VF_FORMAT2_COND_EXPR(fmt, vf_args, vf_args+1, C) :                        \
  VF_FORMAT2_COND_EXPR(fmt, vf_args, vf_args+1, v) :                        \
  mrb_nil_value()  /* not reached */                                        \
)
#define VF_FORMAT2_COND_EXPR(fmt, a1, a2, t) \
  a1->type == ARG_##t ? VF_FORMAT_TYPED(fmt, 2, a2, (a1)->t)
#define VF_FORMAT_TYPED(fmt, n_arg, type_a, v1)                             \
  VF_FORMAT_TYPED_COND_EXPR(fmt, n_arg, type_a, v1, c) :                    \
  VF_FORMAT_TYPED_COND_EXPR(fmt, n_arg, type_a, v1, f) :                    \
  VF_FORMAT_TYPED_COND_EXPR(fmt, n_arg, type_a, v1, i) :                    \
  VF_FORMAT_TYPED_COND_EXPR(fmt, n_arg, type_a, v1, n) :                    \
  VF_FORMAT_TYPED_COND_EXPR(fmt, n_arg, type_a, v1, s) :                    \
  VF_FORMAT_TYPED_COND_EXPR(fmt, n_arg, type_a, v1, C) :                    \
  VF_FORMAT_TYPED_COND_EXPR(fmt, n_arg, type_a, v1, v) :                    \
  mrb_nil_value()  /* not reached */
#define VF_FORMAT_TYPED_COND_EXPR(fmt, n_arg, type_a, v1, t)                \
  (type_a)->type == ARG_##t ? n_arg == 1 ?                                  \
    mrb_format(mrb, fmt, (type_a)->t) : mrb_format(mrb, fmt, v1, (type_a)->t)

static mrb_value
vf_s_format(mrb_state *mrb, mrb_value klass)
{
  mrb_value fmt_str, args[2];
  mrb_int argc = mrb_get_args(mrb, "S|oo", &fmt_str, args, args+1);
  const char *fmt = mrb_string_value_cstr(mrb, &fmt_str);

  VF_FORMAT_INIT(klass);

  switch (argc) {
    case 1: return VF_FORMAT0(fmt);
    case 2: return VF_FORMAT1(fmt, args);
    case 3: return VF_FORMAT2(fmt, args);
    default: return mrb_nil_value();  /* not reached */
  }
}

void
mrb_init_test_vformat(mrb_state *mrb)
{
  struct RClass *vf, *n;

  vf = mrb_define_module(mrb, "TestVFormat");
  mrb_define_class_method(mrb, vf, "format", vf_s_format, MRB_ARGS_ARG(1,2));

  n = mrb_define_class_under(mrb, vf, "Native", mrb->object_class);
  MRB_SET_INSTANCE_TT(n, MRB_TT_DATA);
  mrb_define_method(mrb, n, "initialize", native_initialize, MRB_ARGS_REQ(1));
  mrb_singleton_class(mrb, mrb_obj_value(n));
  mrb_define_alias(mrb, n->c, "[]", "new");
}
