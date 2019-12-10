#include "carbuncle/core.h"
#include "carbuncle/vector3.h"

static const struct mrb_data_type vector3_data_type = {
  "Carbuncle::Vector3", mrb_free
};

void
mrb_carbuncle_vector3_init(mrb_state *mrb)
{
  struct RClass *vector = mrb_carbuncle_define_data_class(mrb, "Vector3", mrb->object_class);
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
