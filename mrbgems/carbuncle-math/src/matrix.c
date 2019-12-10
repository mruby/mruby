#include "carbuncle/core.h"
#include "carbuncle/matrix.h"

static const struct mrb_data_type matrix_data_type = {
  "Carbuncle::Matrix", mrb_free
};

void
mrb_carbuncle_matrix_init(mrb_state *mrb)
{
  struct RClass *matrix = mrb_carbuncle_define_data_class(mrb, "Matrix", mrb->object_class);
}

Matrix *
mrb_carbuncle_get_matrix(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &matrix_data_type, Matrix);
}

mrb_bool
mrb_carbuncle_matrix_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &matrix_data_type);
}
