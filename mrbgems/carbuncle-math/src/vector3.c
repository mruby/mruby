#include "carbuncle/core.h"
#include "carbuncle/vector3.h"

#include <mruby/numeric.h>
#include <mruby/class.h>

static const struct mrb_data_type vector3_data_type = {
  "Carbuncle::Vector3", mrb_free
};

static void
set_vector_values(mrb_state *mrb, mrb_value self, mrb_bool from_initialize)
{
  mrb_value other;
  mrb_float y, z;
  mrb_int argc = mrb_get_args(mrb, "|off", &other, &y, &z);
  Vector3 *data = mrb_carbuncle_get_vector3(mrb, self);
  switch (argc)
  {
    case 0:
    {
      if (!from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "1 or 3", argc);
      }
      data->x = data->y = data->z = 0;
      break;
    }
    case 1:
    {
      Vector3 *vector = mrb_carbuncle_get_vector3(mrb, other);
      *data = *vector;
      break;
    }
    case 3:
    {
      *data = (Vector3){mrb_to_flo(mrb, other), y, z};
      break;
    }
    default:
    {
      if (from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "0, 1 or 3", argc);
      }
      else
      {
        mrb_carbuncle_arg_error(mrb, "1 or 3", argc);
      }
      break;
    }
  }
}

static mrb_value
mrb_vector3_initialize(mrb_state *mrb, mrb_value self)
{
  DATA_PTR(self) = mrb_malloc(mrb, sizeof(Vector3));
  DATA_TYPE(self) = &vector3_data_type;
  set_vector_values(mrb, self, TRUE);
  return self;
}


static mrb_value
mrb_vector3_get_x(mrb_state *mrb, mrb_value self)
{
  Vector3 *vector = mrb_carbuncle_get_vector3(mrb, self);
  return mrb_float_value(mrb, vector->x);
}

static mrb_value
mrb_vector3_get_y(mrb_state *mrb, mrb_value self)
{
  Vector3 *vector = mrb_carbuncle_get_vector3(mrb, self);
  return mrb_float_value(mrb, vector->y);
}

static mrb_value
mrb_vector3_get_z(mrb_state *mrb, mrb_value self)
{
  Vector3 *vector = mrb_carbuncle_get_vector3(mrb, self);
  return mrb_float_value(mrb, vector->z);
}

static mrb_value
mrb_vector3_set_x(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  Vector3 *vector = mrb_carbuncle_get_vector3(mrb, self);
  vector->x = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_vector3_set_y(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  Vector3 *vector = mrb_carbuncle_get_vector3(mrb, self);
  vector->y = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_vector3_set_z(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  Vector3 *vector = mrb_carbuncle_get_vector3(mrb, self);
  vector->z = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_vector3_set(mrb_state *mrb, mrb_value self)
{
  set_vector_values(mrb, self, FALSE);
  return self;
}

static mrb_value
mrb_vector3_equal(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  if (!mrb_carbuncle_vector3_p(other))
  {
    return mrb_false_value();
  }
  Vector3 *data   = mrb_carbuncle_get_vector3(mrb, self);
  Vector3 *vector = mrb_carbuncle_get_vector3(mrb, other);
  return mrb_bool_value(
    data->x == vector->x && data->y == vector->y && data->z == vector->z
  );
}

void
mrb_init_carbuncle_vector3(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *vector = mrb_define_class_under(mrb, carbuncle, "Vector3", mrb->object_class);
  MRB_SET_INSTANCE_TT(vector, MRB_TT_DATA);

  mrb_define_method(mrb, vector, "initialize", mrb_vector3_initialize, MRB_ARGS_OPT(2));
  mrb_define_method(mrb, vector, "initialize_copy", mrb_vector3_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, vector, "x", mrb_vector3_get_x, MRB_ARGS_NONE());
  mrb_define_method(mrb, vector, "y", mrb_vector3_get_y, MRB_ARGS_NONE());
  mrb_define_method(mrb, vector, "z", mrb_vector3_get_z, MRB_ARGS_NONE());

  mrb_define_method(mrb, vector, "x=", mrb_vector3_set_x, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, vector, "y=", mrb_vector3_set_y, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, vector, "z=", mrb_vector3_set_z, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, vector, "set", mrb_vector3_set, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(2));

  mrb_define_method(mrb, vector, "==", mrb_vector3_equal, MRB_ARGS_REQ(1));

  mrb_value empty_vector = mrb_obj_freeze(mrb, mrb_obj_new(mrb, vector, 0, NULL));
  mrb_define_const(mrb, vector, "EMPTY", empty_vector);
}

Vector3 *
mrb_carbuncle_get_vector3(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &vector3_data_type, Vector3);
}

mrb_bool
mrb_carbuncle_vector3_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &vector3_data_type);
}
