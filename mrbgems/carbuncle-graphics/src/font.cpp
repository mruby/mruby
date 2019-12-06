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

#include <stdio.h>
#include <string.h>

#include <map>

struct mrb_Font
{
  FT_Face face;
  mrb_int size;
  std::map<FT_UInt, struct mrb_Glyph> *glyphs;
};

static void
mrb_font_free(mrb_state *mrb, void *ptr)
{
  auto font = reinterpret_cast<struct mrb_Font *>(ptr);
  if (font)
  {
    if (font->face)
    {
      FT_Done_Face(font->face);
    }
    for (auto it = font->glyphs->begin(); it != font->glyphs->end(); ++it)
    {
      UnloadTexture(it->second.texture);
    }
    delete font->glyphs;
    mrb_free(mrb, font);
  }
}

static const struct mrb_data_type font_data_type = {
  "Carbuncle::Font", mrb_font_free
};

namespace
{
  FT_Library carbuncle_freetype;

  void
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
    font->glyphs = new std::map<FT_UInt, struct mrb_Glyph>();
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
  struct mrb_Font *font = reinterpret_cast<struct mrb_Font *>(mrb_malloc(mrb, sizeof *font));
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
  Rectangle rect;
  const char *text;
  struct mrb_Font *data = mrb_carbuncle_get_font(mrb, self);
  mrb_get_args(mrb, "z", &text);
  return mrb_carbuncle_point_new(mrb, rect.x, rect.y);
}

extern "C" void
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

extern "C" struct mrb_Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &font_data_type, struct mrb_Font);
}

extern "C" mrb_bool
mrb_carbuncle_font_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &font_data_type);
}

static void
create_texture(mrb_state *mrb, FT_Face face, FT_BitmapGlyph glyph, struct mrb_Glyph *data)
{
	size_t width = glyph->bitmap.width;
	size_t height = glyph->bitmap.rows;
  Color *pixels = reinterpret_cast<Color *>(mrb_malloc(mrb, width * height * sizeof(*pixels)));
  for (size_t y = 0; y < height; ++y)
  {
    for (size_t x = 0; x < width; ++x)
    {
      size_t i = x + y * width;
      pixels[i] = (Color){
        255, 255, 255, glyph->bitmap.buffer[i]
      };
    }
  }
  Image img = LoadImageEx(pixels, width, height);
  data->texture = LoadTextureFromImage(img);
  data->rect = (Rectangle){0, 0, (float)width, (float)height};
  UnloadImage(img);
  mrb_free(mrb, pixels);
}

extern "C" struct mrb_Glyph
mrb_carbuncle_font_get_glyph(mrb_state *mrb, struct mrb_Font *font, FT_UInt codepoint)
{
  FT_Error err;
  auto pair = font->glyphs->find(codepoint);
  struct mrb_Glyph result;

  FT_Glyph glyph;

  err = FT_Load_Char(font->face, codepoint, FT_LOAD_TARGET_NORMAL);
  if (err)
  {
    mrb_raisef(mrb, mrb->eStandardError_class, "Cannot load character %du", codepoint);
  }
  err = FT_Get_Glyph(font->face->glyph, &glyph);
  if (err)
  {
    mrb_raisef(mrb, mrb->eStandardError_class, "Cannot load character %du", codepoint);
  }
  FT_Glyph_To_Bitmap(&glyph, FT_RENDER_MODE_NORMAL, 0, 1);
  if (pair != font->glyphs->end())
  {
    result = pair->second;
  }
  result.bmp = reinterpret_cast<FT_BitmapGlyph>(glyph);
  create_texture(mrb, font->face, result.bmp, &result);
  if (pair != font->glyphs->end())
  {
    font->glyphs->insert(std::pair<FT_UInt, struct mrb_Glyph>(codepoint, result));
  }
  return result;
}

extern "C" FT_Face
mrb_carbuncle_font_get_face(struct mrb_Font *font)
{
  return font->face;
}

extern "C" void
mrb_carbuncle_font_unload_glyph(struct mrb_Font *font, FT_UInt codepoint)
{
  auto pair = font->glyphs->find(codepoint);
  if (pair != font->glyphs->end())
  {
    FT_Done_Glyph(reinterpret_cast<FT_Glyph>(pair->second.bmp));
  }
}
