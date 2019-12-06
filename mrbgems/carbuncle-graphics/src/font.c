#include "carbuncle/core.h"
#include "carbuncle/font.h"
#include "carbuncle/point.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/string.h>
#include <mruby/array.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include "microutf8.h"
#include "microutf8.h"

#include <stdio.h>
#include <string.h>

struct mrb_Font
{
  FT_Face face;
  mrb_int size;
};

static void
mrb_font_free(mrb_state *mrb, void *ptr)
{
  struct mrb_Font *font = ptr;
  if (font)
  {
    if (font->face)
    {
      FT_Done_Face(font->face);
    }
    mrb_free(mrb, font);
  }
}

static const struct mrb_data_type font_data_type = {
  "Carbuncle::Font", mrb_font_free
};

static FT_Library carbuncle_freetype;

static void
open_font(mrb_state *mrb, struct mrb_Font *font, const char *filename, size_t size)
{
  if (FT_New_Face(carbuncle_freetype, filename, 0, &(font->face)))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot load font '%s'.", filename);
  }
  if (FT_Select_Charmap(font->face, FT_ENCODING_UNICODE))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot load font '%s' as unicode.", filename);
  }
  if (FT_Set_Pixel_Sizes(font->face, 0, size))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot set font size for font %s.", filename);
  }
}

static mrb_value
mrb_font_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_int size, argc;
  const char *name = NULL;
  mrb_value font_class = mrb_obj_value(mrb_carbuncle_class_get(mrb, "Font"));
  argc = mrb_get_args(mrb, "|zi", &name, &size);
  if (argc < 1)
  {
    mrb_value default_name = mrb_funcall(mrb, font_class, "default_name", 0);
    if (mrb_nil_p(default_name))
    {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "Cannot load default font when default font is null.");
    }
    name = mrb_str_to_cstr(mrb, default_name);
  }
  if (argc < 2)
  {
    size = mrb_fixnum(mrb_to_int(mrb, mrb_funcall(mrb, font_class, "default_size", 0)));
  }
  struct mrb_Font *font = mrb_malloc(mrb, sizeof *font);
  DATA_PTR(self) = font;
  DATA_TYPE(self) = &font_data_type;
  size_t str_size = strlen(name) + 1;
  font->face = NULL;
  mrb_carbuncle_check_file(mrb, name);
  font->size = size;
  open_font(mrb, font, name, size);
  return self;
}

static mrb_value
mrb_font_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

static mrb_value
mrb_font_dispose(mrb_state *mrb, mrb_value self)
{
  struct mrb_Font *font = mrb_carbuncle_get_font(mrb, self);
  mrb_font_free(mrb, font);
  DATA_PTR(self) = NULL;
  return self;
}

static mrb_value
mrb_font_measure_text(mrb_state *mrb, mrb_value self)
{
  const char *text;
  struct mrb_Font *data = mrb_carbuncle_get_font(mrb, self);
  mrb_get_args(mrb, "z", &text);
  size_t len = utf8_strlen(text);
  FT_BitmapGlyph *bmps = mrb_carbuncle_font_load_glyphs(mrb, data->face, len, text);
	Vector2 size = mrb_carbuncle_font_calculate_size(len, bmps);
  mrb_carbuncle_font_destroy_glyphs(mrb, len, bmps);
  return mrb_carbuncle_point_new(mrb, size.x, size.y);
}

void
mrb_carbuncle_font_init(mrb_state *mrb)
{
  if (FT_Init_FreeType( &carbuncle_freetype ))
  {
    mrb_raise(mrb, mrb->eStandardError_class, "Unable to initialize freetype.");
  }
  struct RClass *font = mrb_carbuncle_define_data_class(mrb, "Font", mrb->object_class);

  mrb_define_method(mrb, font, "initialize", mrb_font_initialize, MRB_ARGS_OPT(2));

  mrb_define_method(mrb, font, "disposed?", mrb_font_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, font, "dispose", mrb_font_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, font, "measure_text", mrb_font_measure_text, MRB_ARGS_REQ(1));
}

struct mrb_Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &font_data_type, struct mrb_Font);
}

mrb_bool
mrb_carbuncle_font_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &font_data_type);
}

FT_Face
mrb_carbuncle_font_get_face(struct mrb_Font *font)
{
  return font->face;
}

static void
glyph_error(mrb_state *mrb)
{
  mrb_raise(mrb, mrb->eStandardError_class, "Unable to load font glyphs.");
}

FT_BitmapGlyph *
mrb_carbuncle_font_load_glyphs(mrb_state *mrb, FT_Face face, size_t len, const char *message)
{
  FT_UInt codepoint;
  FT_Glyph glyph;
  FT_Matrix matrix = (FT_Matrix){ .xx = 0x10000, .xy = 0, .yx = 0, .yy = 0x10000 };
	FT_Vector pen = (FT_Vector){ .x = 0, .y = 0};  
  FT_BitmapGlyph *bmps = mrb_malloc(mrb, len * sizeof(*bmps));
  for (size_t i = 0; i < len; ++i)
  {
    FT_Set_Transform(face, &matrix, &pen);
    message = utf8_decode(message, &codepoint);
    if (FT_Load_Char(face, codepoint, FT_LOAD_TARGET_NORMAL)) { glyph_error(mrb); }
    if (FT_Get_Glyph(face->glyph, &glyph)) { glyph_error(mrb); }
    FT_Glyph_To_Bitmap(&glyph, FT_RENDER_MODE_NORMAL, 0, 1);
    bmps[i] = (FT_BitmapGlyph)glyph;
    pen.x += face->glyph->advance.x;
    pen.y += face->glyph->advance.y;
  }
  return bmps;
}

Vector2
mrb_carbuncle_font_calculate_size(size_t len, FT_BitmapGlyph *bmps)
{
  Vector2 result = (Vector2){0, 0};
	for (size_t i = 0; i < len; ++i)
  {
    FT_BitmapGlyph bmp = bmps[i];
    float new_width = bmp->bitmap.width + bmp->left;
    float new_height = bmp->bitmap.rows + bmp->top;
		if (result.x < new_width) { result.x = new_width; }
		if (result.y < new_height) { result.y = new_height; }
	}  
  return result;
}

void
mrb_carbuncle_font_destroy_glyphs(mrb_state *mrb, size_t len, FT_BitmapGlyph *bmps)
{
  for (size_t i = 0; i < len; ++i)
  {
    FT_Done_Glyph((FT_Glyph)bmps[i]);
  }
  mrb_free(mrb, bmps);
}
