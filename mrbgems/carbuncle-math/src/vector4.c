#include "carbuncle/core.h"
#include "carbuncle/vector4.h"

#include <mruby/numeric.h>

static const struct mrb_data_type vector4_data_type = {
  "Carbuncle::Vector4", mrb_free
};

static void
set_vector_values(mrb_state *mrb, mrb_value self, mrb_bool from_initialize)
{
  mrb_value other;
  mrb_float y, z, w;
  mrb_int argc = mrb_get_args(mrb, "|offf", &other, &y, &z, &w);
  Vector4 *data = mrb_carbuncle_get_vector4(mrb, self);
  switch (argc)
  {
    case 0:
    {
      if (!from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "1 or 4", argc);
      }
      data->x = data->y = data->z = data->w = 0;
      break;
    }
    case 1:
    {
      Vector4 *vector = mrb_carbuncle_get_vector4(mrb, other);
      *data = *vector;
      break;
    }
    case 4:
    {
      *data = (Vector4){mrb_to_flo(mrb, other), y, z, w};
      break;
    }
    default:
    {
      if (from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "0, 1 or 4", argc);
      }
      else
      {
        mrb_carbuncle_arg_error(mrb, "1 or 4", argc);
      }
      break;
    }
  }
}

static mrb_value
mrb_vector4_initialize(mrb_state *mrb, mrb_value self)
{
  DATA_PTR(self) = mrb_malloc(mrb, sizeof(Vector4));
  DATA_TYPE(self) = &vector4_data_type;
  set_vector_values(mrb, self, TRUE);
  return self;
}


static mrb_value
mrb_vector4_get_x(mrb_state *mrb, mrb_value self)
{
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, self);
  return mrb_float_value(mrb, vector->x);
}

static mrb_value
mrb_vector4_get_y(mrb_state *mrb, mrb_value self)
{
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, self);
  return mrb_float_value(mrb, vector->y);
}

static mrb_value
mrb_vector4_get_z(mrb_state *mrb, mrb_value self)
{
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, self);
  return mrb_float_value(mrb, vector->z);
}

static mrb_value
mrb_vector4_get_w(mrb_state *mrb, mrb_value self)
{
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, self);
  return mrb_float_value(mrb, vector->w);
}

static mrb_value
mrb_vector4_set_x(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, self);
  vector->x = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_vector4_set_y(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, self);
  vector->y = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_vector4_set_z(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, self);
  vector->z = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_vector4_set_w(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, self);
  vector->w = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_vector4_set(mrb_state *mrb, mrb_value self)
{
  set_vector_values(mrb, self, FALSE);
  return self;
}

static mrb_value
mrb_vector4_equal(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  if (!mrb_carbuncle_vector4_p(other))
  {
    return mrb_false_value();
  }
  Vector4 *data   = mrb_carbuncle_get_vector4(mrb, self);
  Vector4 *vector = mrb_carbuncle_get_vector4(mrb, other);
  return mrb_bool_value(
    data->x == vector->x && data->y == vector->y &&
    data->z == vector->z && data->w == vector->w
  );
}

void
mrb_carbuncle_vector4_init(mrb_state *mrb)
{
  struct RClass *vector = mrb_carbuncle_define_data_class(mrb, "Vector4", mrb->object_class);

  mrb_define_method(mrb, vector, "initialize", mrb_vector4_initialize, MRB_ARGS_OPT(2));
  mrb_define_method(mrb, vector, "initialize_copy", mrb_vector4_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, vector, "x", mrb_vector4_get_x, MRB_ARGS_NONE());
  mrb_define_method(mrb, vector, "y", mrb_vector4_get_y, MRB_ARGS_NONE());
  mrb_define_method(mrb, vector, "z", mrb_vector4_get_z, MRB_ARGS_NONE());
  mrb_define_method(mrb, vector, "w", mrb_vector4_get_w, MRB_ARGS_NONE());

  mrb_define_method(mrb, vector, "x=", mrb_vector4_set_x, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, vector, "y=", mrb_vector4_set_y, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, vector, "z=", mrb_vector4_set_z, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, vector, "w=", mrb_vector4_set_w, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, vector, "set", mrb_vector4_set, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(3));

  mrb_define_method(mrb, vector, "==", mrb_vector4_equal, MRB_ARGS_REQ(1));

  mrb_value empty_vector = mrb_obj_freeze(mrb, mrb_obj_new(mrb, vector, 0, NULL));
  mrb_define_const(mrb, vector, "EMPTY", empty_vector);
}

Vector4 *
mrb_carbuncle_get_vector4(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &vector4_data_type, Vector4);
}

mrb_bool
mrb_carbuncle_vector4_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &vector4_data_type);
}
