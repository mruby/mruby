#ifndef CARBUNCLE_FONT_H
#define CARBUNCLE_FONT_H

#include <mruby.h>
#include "raylib.h"
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#ifdef __cplusplus
extern "C" {
#endif

struct mrb_Font
{
  FT_Face face;
  Font font;
  mrb_int size;
  char *filename;
  uint32_t *chars;
  size_t count;
  mrb_bool dirty;
};

void
mrb_carbuncle_font_init(mrb_state *mrb);

struct mrb_Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_font_p(mrb_value obj);

void
mrb_carbuncle_font_check_data(mrb_state *mrb, struct mrb_Font *font, uint32_t codepoint);

#ifdef __cplusplus
}
#endif

#endif
