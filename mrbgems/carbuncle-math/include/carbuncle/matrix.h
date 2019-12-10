#ifndef CARBUNCLE_MATRIX_H
#define CARBUNCLE_MATRIX_H

#include <mruby.h>

#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_matrix_init(mrb_state *mrb);

Matrix *
mrb_carbuncle_get_matrix(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_matrix_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
