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

struct mrb_Font;

struct mrb_Glyph
{
  FT_BitmapGlyph bmp;
  Texture2D texture;
  Rectangle rect;
};

void
mrb_carbuncle_font_init(mrb_state *mrb);

struct mrb_Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_font_p(mrb_value obj);

struct mrb_Glyph
mrb_carbuncle_font_get_glyph(mrb_state *mrb, struct mrb_Font *font, FT_UInt codepoint);

FT_Face
mrb_carbuncle_font_get_face(struct mrb_Font *font);

void
mrb_carbuncle_font_unload_glyph(struct mrb_Font *font, FT_UInt codepoint);

#ifdef __cplusplus
}
#endif

#endif
