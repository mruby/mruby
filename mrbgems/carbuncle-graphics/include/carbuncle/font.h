#ifndef CARBUNCLE_FONT_H
#define CARBUNCLE_FONT_H

#include <mruby.h>
#include "raylib.h"
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include <carbuncle/avl.h>

#ifdef __cplusplus
extern "C" {
#endif

struct mrb_Font
{
  FT_Face face;
  mrb_int size;
  struct mrb_AVL *glyphs;
};

struct mrb_Glyph
{
  FT_BitmapGlyph glyph;
  Texture2D      texture;
  struct { mrb_int x, y; } advance;
  mrb_int top, left;
};

void
mrb_carbuncle_font_init(mrb_state *mrb);

struct mrb_Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_font_p(mrb_value obj);

struct mrb_Glyph *
mrb_carbuncle_font_glyph(mrb_state *mrb, struct mrb_Font *font, FT_ULong charcode);

#ifdef __cplusplus
}
#endif

#endif
