#include "carbuncle/core.h"
#include "carbuncle/matrix.h"

static const struct mrb_data_type matrix_data_type = {
  "Carbuncle::Matrix", mrb_free
};

static mrb_value
mrb_matrix_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  Matrix *data = mrb_malloc(mrb, sizeof *data);
  if (mrb_get_args(mrb, "|o", &value))
  {
    Matrix *matrix = mrb_carbuncle_get_matrix(mrb, value);
    *data = *matrix;
  }
  else
  {
    *data = (Matrix){
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    };
  }
  DATA_PTR(self) = data;
  DATA_TYPE(self) = &matrix_data_type;
  return self;
}

static mrb_value
mrb_matrix_get_subscript(mrb_state *mrb, mrb_value self)
{
  mrb_int i, j;
  Matrix *matrix = mrb_carbuncle_get_matrix(mrb, self);
  mrb_get_args(mrb, "ii", &i, &j);
  switch (i)
  {
    case 0:
    {
      switch(j)
      {
        case 0: { return mrb_float_value(mrb, matrix->m0); }
        case 1: { return mrb_float_value(mrb, matrix->m1); }
        case 2: { return mrb_float_value(mrb, matrix->m2); }
        case 3: { return mrb_float_value(mrb, matrix->m3); }
        default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index j (%d) out of range", j); }
      }
    }
    case 1:
    {
      switch(j)
      {
        case 0: { return mrb_float_value(mrb, matrix->m4); }
        case 1: { return mrb_float_value(mrb, matrix->m5); }
        case 2: { return mrb_float_value(mrb, matrix->m6); }
        case 3: { return mrb_float_value(mrb, matrix->m7); }
        default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index j (%d) out of range", j); }
      }
    }
    case 2:
    {
      switch(j)
      {
        case 0: { return mrb_float_value(mrb, matrix->m8); }
        case 1: { return mrb_float_value(mrb, matrix->m9); }
        case 2: { return mrb_float_value(mrb, matrix->m10); }
        case 3: { return mrb_float_value(mrb, matrix->m11); }
        default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index j (%d) out of range", j); }
      }
    }
    case 3:
    {
      switch(j)
      {
        case 0: { return mrb_float_value(mrb, matrix->m12); }
        case 1: { return mrb_float_value(mrb, matrix->m13); }
        case 2: { return mrb_float_value(mrb, matrix->m14); }
        case 3: { return mrb_float_value(mrb, matrix->m15); }
        default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index j (%d) out of range", j); }
      }
    }
    default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index i (%d) out of range", i); }
  }
  return self;
}

static mrb_value
mrb_matrix_set_subscript(mrb_state *mrb, mrb_value self)
{
  mrb_int i, j;
  mrb_float value;
  mrb_carbuncle_check_frozen(mrb, self);
  Matrix *matrix = mrb_carbuncle_get_matrix(mrb, self);
  mrb_get_args(mrb, "iif", &i, &j, &value);
  switch (i)
  {
    case 0:
    {
      switch(j)
      {
        case 0: { matrix->m0 = value; break; }
        case 1: { matrix->m1 = value; break; }
        case 2: { matrix->m2 = value; break; }
        case 3: { matrix->m3 = value; break; }
        default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index j (%d) out of range", j); }
      }
      break;
    }
    case 1:
    {
      switch(j)
      {
        case 0: { matrix->m4 = value; break; }
        case 1: { matrix->m5 = value; break; }
        case 2: { matrix->m6 = value; break; }
        case 3: { matrix->m7 = value; break; }
        default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index j (%d) out of range", j); }
      }
      break;
    }
    case 2:
    {
      switch(j)
      {
        case 0: { matrix->m8  = value; break; }
        case 1: { matrix->m9  = value; break; }
        case 2: { matrix->m10 = value; break; }
        case 3: { matrix->m11 = value; break; }
        default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index j (%d) out of range", j); }
      }
      break;
    }
    case 3:
    {
      switch(j)
      {
        case 0: { matrix->m12 = value; break; }
        case 1: { matrix->m13 = value; break; }
        case 2: { matrix->m14 = value; break; }
        case 3: { matrix->m15 = value; break; }
        default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index j (%d) out of range", j); }
      }
      break;
    }
    default: { mrb_raisef(mrb, E_ARGUMENT_ERROR, "Matrix index i (%d) out of range", i); }
  }
  return mrb_float_value(mrb, value);
}

void
mrb_carbuncle_matrix_init(mrb_state *mrb)
{
  struct RClass *matrix = mrb_carbuncle_define_data_class(mrb, "Matrix", mrb->object_class);

  mrb_define_method(mrb, matrix, "initialize", mrb_matrix_initialize, MRB_ARGS_OPT(1));
  mrb_define_method(mrb, matrix, "initialize_copy", mrb_matrix_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, matrix, "[]", mrb_matrix_get_subscript, MRB_ARGS_REQ(2));

  mrb_define_method(mrb, matrix, "[]=", mrb_matrix_set_subscript, MRB_ARGS_REQ(3));

  mrb_define_const(mrb, matrix, "IDENTITY", mrb_obj_freeze(mrb, mrb_obj_new(mrb, matrix, 0, NULL)));
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
