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

struct mrb_Glyph
{
  FT_ULong codepoint;
  FT_BitmapGlyph bmp;
  Rectangle rect;
  Vector2 advance;
  Vector2 margin;
  struct mrb_Glyph *left;
  struct mrb_Glyph *right;
  mrb_int height;
};

struct mrb_Font
{
  FT_Face face;
  mrb_int size;
  struct 
  {
    size_t count;
    struct mrb_Glyph *root;
  } glyphs;
  struct
  {
    size_t max_width;
    size_t min_height;
    size_t max_height;
    size_t total_width;
  } metrics;
  struct 
  {
    size_t width;
    size_t height;
    Texture2D texture;
  } atlas;
};


void
mrb_carbuncle_font_init(mrb_state *mrb);

struct mrb_Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_font_p(mrb_value obj);

FT_Face
mrb_carbuncle_font_get_face(struct mrb_Font *font);

FT_BitmapGlyph *
mrb_carbuncle_font_load_glyphs(mrb_state *mrb, FT_Face face, size_t len, const char *message);

Vector2
mrb_carbuncle_font_calculate_size(size_t len, FT_BitmapGlyph *bmps);

void
mrb_carbuncle_font_destroy_glyphs(mrb_state *mrb, size_t len, FT_BitmapGlyph *bmps);

struct mrb_Glyph *
mrb_carbuncle_font_get_glyph(struct mrb_Font *font, FT_UInt codepoint);

#ifdef __cplusplus
}
#endif

#endif
