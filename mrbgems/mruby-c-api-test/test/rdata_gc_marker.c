#include <mruby.h>
#include <mruby/class.h>
#include <mruby/data.h>


int released = 0;

struct test_obj {
  mrb_value value;
};

static void
free_test_obj(mrb_state* mrb, void* p)
{
  mrb_free(mrb, p);
}

mrb_data_type const test_obj_type = {
  "test_obj", &free_test_obj
};

static void
free_release_obj(mrb_state* mrb, void* p)
{
  released = 0;
}

mrb_data_type const release_obj_type = {
  "release_obj", &free_release_obj
};

static void
gc_marker(mrb_state* mrb, mrb_value self)
{
  mrb_gc_mark_value(mrb, ((struct test_obj*)DATA_PTR(self))->value);
}

static mrb_value
test_obj_init(mrb_state* mrb, mrb_value self)
{
  released = 0;
  DATA_PTR(self) = mrb_malloc(mrb, sizeof(struct test_obj));
  ((struct test_obj*)DATA_PTR(self))->value = mrb_nil_value();
  DATA_TYPE(self) = &test_obj_type;
  return self;
}

static mrb_value
test_obj_set_value(mrb_state* mrb, mrb_value self)
{
  mrb_value v;
  mrb_get_args(mrb, "o", &v);
  ((struct test_obj*)DATA_PTR(self))->value = v;
  return self;
}

static mrb_value
test_obj_released(mrb_state* mrb, mrb_value self)
{
  (void)mrb;
  (void)self;
  return mrb_bool_value(!released);
}

static mrb_value
release_obj_init(mrb_state* mrb, mrb_value self)
{
  (void)mrb;
  DATA_TYPE(self) = &release_obj_type;
  released = 1;
  return self;
}

void
mrb_mruby_c_api_test_gem_test(mrb_state* mrb)
{
  struct RClass* without = mrb_define_class(mrb, "RDataWithoutMarker", mrb->object_class);
  MRB_SET_INSTANCE_TT(without, MRB_TT_DATA);
  mrb_define_method(mrb, without, "initialize", &test_obj_init, MRB_ARGS_NONE());
  mrb_define_method(mrb, without, "value=", &test_obj_set_value, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, without, "released?", &test_obj_released, MRB_ARGS_NONE());

  struct RClass* with = mrb_define_class(mrb, "RDataWithMarker", without);
  mrb_data_set_gc_marker(mrb, with, gc_marker);

  struct RClass* release_cls = mrb_define_class(mrb, "ReleaseObj", mrb->object_class);
  MRB_SET_INSTANCE_TT(release_cls, MRB_TT_DATA);
  mrb_define_method(mrb, release_cls, "initialize", &release_obj_init, MRB_ARGS_NONE());
}
