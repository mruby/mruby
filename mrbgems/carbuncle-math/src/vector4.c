#include "carbuncle/core.h"
#include "carbuncle/vector4.h"

static const struct mrb_data_type vector4_data_type = {
  "Carbuncle::Vector4", mrb_free
};

void
mrb_carbuncle_vector4_init(mrb_state *mrb)
{
  struct RClass *vector = mrb_carbuncle_define_data_class(mrb, "Vector4", mrb->object_class);
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
