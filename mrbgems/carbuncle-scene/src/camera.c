#include <carbuncle/core.h>
#include <carbuncle/point.h>
#include <carbuncle/camera.h>

#include <mruby/data.h>
#include <mruby/error.h>
#include <mruby/class.h>
#include <mruby/variable.h>

#define OFFSET_SYMBOL   mrb_intern_cstr(mrb, "#offset")
#define TARGET_SYMBOL   mrb_intern_cstr(mrb, "#target")

static const struct mrb_data_type camera_data_type = {
  "Carbuncle::Camera", mrb_free
};

static mrb_value
mrb_camera_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value offset, rotation, target;
  mrb_int arena;
  struct mrb_Camera *cam = mrb_malloc(mrb, sizeof *cam);
  DATA_PTR(self) = cam;
  DATA_TYPE(self) = &camera_data_type;
  arena = mrb_gc_arena_save(mrb);
  offset = mrb_carbuncle_point_new(mrb, 0, 0);
  target = mrb_carbuncle_point_new(mrb, 0, 0);
  mrb_iv_set(mrb, self, OFFSET_SYMBOL, offset);
  mrb_iv_set(mrb, self, TARGET_SYMBOL, target);
  cam->offset = mrb_carbuncle_get_point(mrb, offset);
  cam->target = mrb_carbuncle_get_point(mrb, target);
  cam->angle = 0;
  cam->scale = 1;
  mrb_gc_arena_restore(mrb, arena);
  return self;
}

static mrb_value
mrb_camera_get_target(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, TARGET_SYMBOL);
}

static mrb_value
mrb_camera_get_offset(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, OFFSET_SYMBOL);
}

static mrb_value
mrb_camera_get_angle(mrb_state *mrb, mrb_value self)
{
  struct mrb_Camera *cam = mrb_carbuncle_get_camera(mrb, self);
  return mrb_float_value(mrb, cam->angle);
}

static mrb_value
mrb_camera_get_scale(mrb_state *mrb, mrb_value self)
{
  struct mrb_Camera *cam = mrb_carbuncle_get_camera(mrb, self);
  return mrb_float_value(mrb, cam->scale);
}

static mrb_value
mrb_camera_set_target(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_get_args(mrb, "o", &obj);
  Vector2 *point = mrb_carbuncle_get_point(mrb, obj);
  struct mrb_Camera *cam = mrb_carbuncle_get_camera(mrb, self);
  cam->target = point;
  mrb_iv_set(mrb, self, TARGET_SYMBOL, obj);
  return obj;
}

static mrb_value
mrb_camera_set_offset(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_get_args(mrb, "o", &obj);
  Vector2 *point = mrb_carbuncle_get_point(mrb, obj);
  struct mrb_Camera *cam = mrb_carbuncle_get_camera(mrb, self);
  cam->offset = point;
  mrb_iv_set(mrb, self, OFFSET_SYMBOL, obj);
  return obj;
}

static mrb_value
mrb_camera_set_angle(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  struct mrb_Camera *cam = mrb_carbuncle_get_camera(mrb, self);
  mrb_get_args(mrb, "f", &value);
  cam->angle = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_camera_set_scale(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  struct mrb_Camera *cam = mrb_carbuncle_get_camera(mrb, self);
  mrb_get_args(mrb, "f", &value);
  cam->scale = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_camera_begin_draw(mrb_state *mrb, mrb_value self)
{
  struct mrb_Camera *data = mrb_carbuncle_get_camera(mrb, self);
  Camera2D camera = (Camera2D){
    .offset = *(data->offset),
    .target = *(data->target),
    .rotation = data->angle,
    .zoom = data->scale
  };
  mrb_value block = mrb_nil_value();
  mrb_get_args(mrb, "|&", &block);
  BeginMode2D(camera);
  if (!mrb_nil_p(block))
  {
    mrb_bool raised = FALSE;
    mrb_value result = mrb_protect(mrb, mrb_carbuncle_call_block, block, &raised);
    EndMode2D();
    if (raised) { mrb_exc_raise(mrb, result); }
  }
  return self;
}

static mrb_value
mrb_camera_end_draw(mrb_state *mrb, mrb_value self)
{
  EndMode2D();
  return self;
}

void
mrb_init_carbuncle_camera(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *camera = mrb_define_class_under(mrb, carbuncle, "Camera", mrb->object_class);
  MRB_SET_INSTANCE_TT(camera, MRB_TT_DATA);

  mrb_define_method(mrb, camera, "initialize", mrb_camera_initialize, MRB_ARGS_NONE());

  mrb_define_method(mrb, camera, "target", mrb_camera_get_target, MRB_ARGS_NONE());
  mrb_define_method(mrb, camera, "offset", mrb_camera_get_offset, MRB_ARGS_NONE());
  mrb_define_method(mrb, camera, "angle",  mrb_camera_get_angle, MRB_ARGS_NONE());
  mrb_define_method(mrb, camera, "scale",  mrb_camera_get_scale, MRB_ARGS_NONE());

  mrb_define_method(mrb, camera, "target=", mrb_camera_set_target, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, camera, "offset=", mrb_camera_set_offset, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, camera, "angle=",  mrb_camera_set_angle, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, camera, "scale=",  mrb_camera_set_scale, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, camera, "draw",  mrb_camera_begin_draw, MRB_ARGS_BLOCK());
  mrb_define_method(mrb, camera, "begin_draw",  mrb_camera_begin_draw, MRB_ARGS_BLOCK());
  mrb_define_method(mrb, camera, "end_draw",  mrb_camera_end_draw, MRB_ARGS_NONE());
}

struct mrb_Camera *
mrb_carbuncle_get_camera(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &camera_data_type, struct mrb_Camera);
}

mrb_bool
mrb_carbuncle_camera_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &camera_data_type);
}
