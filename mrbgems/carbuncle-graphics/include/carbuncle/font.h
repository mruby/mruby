#ifndef CARBUNCLE_FONT_H
#define CARBUNCLE_FONT_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

struct mrb_Font
{
  struct {
    size_t size;
    size_t capa;
    int *list;
  } glyphs;
  Font data;
  mrb_int size;
};

void
mrb_carbuncle_font_init(mrb_state *mrb);

struct mrb_Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_font_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
