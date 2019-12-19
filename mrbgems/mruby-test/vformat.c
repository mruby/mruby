#include <string.h>
#include <mruby.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/string.h>

#ifdef MRB_WITHOUT_FLOAT
typedef mrb_int mrb_float;
#define mrb_float(o) mrb_fixnum(o)
#endif

#define NATIVE_TYPES                                                        \
  char c;                                                                   \
  int d;                                                                    \
  mrb_float f;                                                              \
  mrb_int i;                                                                \
  size_t l;                                                                 \
  mrb_sym n;                                                                \
  const char* s;                                                            \
  struct RClass* C

#define NATIVE_DEFINE_TYPE_FUNC(t)                                          \
  static mrb_value                                                          \
  native_s_##t(mrb_state *mrb, mrb_value klass)                             \
  {                                                                         \
    mrb_value obj, type = mrb_fixnum_value(ARG_##t);                        \
    mrb_get_args(mrb, "o", &obj);                                           \
    return mrb_funcall(mrb, klass, "new", 2, type, obj);                    \
  }

#define NATIVE_DEFINE_TYPE_METHOD(t) \
  mrb_define_class_method(mrb, n, #t, native_s_##t, MRB_ARGS_REQ(1));

typedef enum {
  ARG_none = 0,
  ARG_c,
  ARG_d,
  ARG_f,
  ARG_i,
  ARG_l,
  ARG_n,
  ARG_s,
  ARG_C,
  ARG_v,
} VFArgumentType;

typedef struct {
  VFArgumentType type;
  union { NATIVE_TYPES; };
  mrb_value obj;
} VFNative;

typedef struct {
  VFArgumentType type;
  union {
    NATIVE_TYPES;
    mrb_value v;
    int none;  /* use for `VF_FORMAT_CASE2(t1, none)` */
  };
} VFArgument;

static const char*
str_ensure_null_termination(mrb_state *mrb, mrb_value *strp)
{
  const char *s = RSTRING_PTR(*strp);
  mrb_int len = RSTRING_LEN(*strp);
  if (s[len]) {
    *strp = mrb_str_new(mrb, s, len);
    return RSTRING_PTR(*strp);
  }
  else {
    return s;
  }
}

static void
native_free(mrb_state *mrb, void *data)
{
  VFNative *native = (VFNative*)data;
  mrb_gc_unregister(mrb, native->obj);
  mrb_free(mrb, native);
}

static const struct mrb_data_type native_data_type = {
  "TestVFormat::Native", native_free
};

static mrb_value
native_initialize(mrb_state *mrb, mrb_value self)
{
  VFNative data, *datap;
  mrb_int type;
  mrb_value obj;

  mrb_get_args(mrb, "io", &type, &obj);
  data.type = (VFArgumentType)type;
  switch (data.type) {
    case ARG_c: data.c = RSTRING_PTR(obj)[0]; break;
    case ARG_d: data.d = (int)mrb_fixnum(obj); break;
    case ARG_f: data.f = mrb_float(obj); break;
    case ARG_i: data.i = mrb_fixnum(obj); break;
    case ARG_l: data.l = (size_t)mrb_fixnum(obj); break;
    case ARG_n: data.n = mrb_symbol(obj); break;
    case ARG_s: data.s = str_ensure_null_termination(mrb, &obj); break;
    case ARG_C: data.C = mrb_class_ptr(obj); break;
    default: mrb_raisef(mrb, E_ARGUMENT_ERROR, "unknown type: %d", data.type);
  }
  data.obj = obj;
  mrb_gc_register(mrb, data.obj);
  datap = (VFNative*)mrb_malloc(mrb, sizeof(VFNative));
  *datap = data;
  mrb_data_init(self, datap, &native_data_type);
  return self;
}

NATIVE_DEFINE_TYPE_FUNC(c)
NATIVE_DEFINE_TYPE_FUNC(d)
NATIVE_DEFINE_TYPE_FUNC(f)
NATIVE_DEFINE_TYPE_FUNC(i)
NATIVE_DEFINE_TYPE_FUNC(l)
NATIVE_DEFINE_TYPE_FUNC(n)
NATIVE_DEFINE_TYPE_FUNC(s)
NATIVE_DEFINE_TYPE_FUNC(C)

static void
args_from_objs(mrb_state *mrb, struct RClass *native_class, mrb_int vf_argc,
               mrb_value *objs, VFArgument *vf_args)
{
  int i;
  for (i = 0; i < vf_argc; i++) {
    if (mrb_obj_is_instance_of(mrb, objs[i], native_class)) {
      const VFNative *native = (VFNative*)DATA_PTR(objs[i]);
      vf_args[i] = *(VFArgument*)native;
    }
    else {
      vf_args[i].v = objs[i];
      vf_args[i].type = ARG_v;
    }
  }
}

#define ARG_TYPE2(type1, type2) (((uint32_t)(type2) << 15) | (type1))

#define VF_FORMAT_CASE(fmt, vf_args, t2)                                    \
  VF_FORMAT_CASE2(fmt, vf_args, c, t2);                                     \
  VF_FORMAT_CASE2(fmt, vf_args, d, t2);                                     \
  VF_FORMAT_CASE2(fmt, vf_args, f, t2);                                     \
  VF_FORMAT_CASE2(fmt, vf_args, i, t2);                                     \
  VF_FORMAT_CASE2(fmt, vf_args, l, t2);                                     \
  VF_FORMAT_CASE2(fmt, vf_args, n, t2);                                     \
  VF_FORMAT_CASE2(fmt, vf_args, s, t2);                                     \
  VF_FORMAT_CASE2(fmt, vf_args, C, t2);                                     \
  VF_FORMAT_CASE2(fmt, vf_args, v, t2);

#define VF_FORMAT_CASE2(fmt, vf_args, t1, t2)                               \
  case ARG_TYPE2(ARG_##t1, ARG_##t2):                                       \
    return ARG_##t2 == ARG_none ?                                           \
      mrb_format(mrb, fmt, vf_args[0].t1) :                                 \
      mrb_format(mrb, fmt, vf_args[0].t1, vf_args[1].t2)

static mrb_value
vf_format0(mrb_state *mrb, const char *fmt)
{
  return mrb_format(mrb, fmt);
}

static mrb_value
vf_format1(mrb_state *mrb, const char *fmt, VFArgument *vf_args)
{
  switch (ARG_TYPE2(vf_args[0].type, ARG_none)) {
    VF_FORMAT_CASE(fmt, vf_args, none);
    default: return mrb_nil_value();  /* not reached */
  }
}

static mrb_value
vf_format2(mrb_state *mrb, const char *fmt, VFArgument *vf_args)
{
  switch (ARG_TYPE2(vf_args[0].type, vf_args[1].type)) {
    VF_FORMAT_CASE(fmt, vf_args, c);
    VF_FORMAT_CASE(fmt, vf_args, d);
    VF_FORMAT_CASE(fmt, vf_args, f);
    VF_FORMAT_CASE(fmt, vf_args, i);
    VF_FORMAT_CASE(fmt, vf_args, l);
    VF_FORMAT_CASE(fmt, vf_args, n);
    VF_FORMAT_CASE(fmt, vf_args, s);
    VF_FORMAT_CASE(fmt, vf_args, C);
    VF_FORMAT_CASE(fmt, vf_args, v);
    default: return mrb_nil_value();  /* not reached */
  }
}

static mrb_value
vf_s_format(mrb_state *mrb, mrb_value klass)
{
  const char *fmt;
  mrb_value args[2];
  VFArgument vf_args[2];
  mrb_int argc = mrb_get_args(mrb, "z|oo", &fmt, args, args+1);

  args_from_objs(mrb, mrb_class_get_under(mrb, mrb_class_ptr(klass), "Native"),
                 argc-1, args, vf_args);
  switch (argc) {
    case 1: return vf_format0(mrb, fmt);
    case 2: return vf_format1(mrb, fmt, vf_args);
    case 3: return vf_format2(mrb, fmt, vf_args);
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
  NATIVE_DEFINE_TYPE_METHOD(c);
  NATIVE_DEFINE_TYPE_METHOD(d);
  NATIVE_DEFINE_TYPE_METHOD(f);
  NATIVE_DEFINE_TYPE_METHOD(i);
  NATIVE_DEFINE_TYPE_METHOD(l);
  NATIVE_DEFINE_TYPE_METHOD(n);
  NATIVE_DEFINE_TYPE_METHOD(s);
  NATIVE_DEFINE_TYPE_METHOD(C);
  mrb_define_method(mrb, n, "initialize", native_initialize, MRB_ARGS_REQ(2));
}
