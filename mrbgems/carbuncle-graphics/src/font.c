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

#include <assert.h>

#define DEFAULT_FONT_CAPA 63

static FT_Library carbuncle_freetype;

static void
free_glyph(mrb_state *mrb, void *ptr)
{
  if (ptr)
  {
    struct mrb_Glyph *data = ptr;
    FT_Done_Glyph((FT_Glyph)data->glyph);
    UnloadTexture(data->texture);
    mrb_free(mrb, data);
  }
}

static void
create_glyph_texture(mrb_state *mrb, struct mrb_Glyph *data, FT_BitmapGlyph glyph)
{
  size_t width = glyph->bitmap.width;
  size_t height = glyph->bitmap.rows + data->glyph->top;
  size_t size = width * height;
  Color *pixels = mrb_malloc(mrb, size * sizeof *data);
	size_t left = glyph->left;
	size_t top = height - glyph->top;

	for (size_t y = 0; y < glyph->bitmap.rows; y++) {
		for (size_t x = 0; x < glyph->bitmap.width; x++) {
      size_t i = x + y * width;
      pixels[i] = (Color){
        255, 255, 255, glyph->bitmap.buffer[i]
      };
    }
  }
  Image image = LoadImageEx(pixels, width, height);
  data->texture = LoadTextureFromImage(image);
  UnloadImage(image);
  mrb_free(mrb, pixels);
}

static struct mrb_Glyph *
load_glyph(mrb_state *mrb, struct mrb_Font *font, FT_ULong charcode)
{
  FT_Load_Char(font->face, charcode, FT_LOAD_TARGET_NORMAL);
  FT_Glyph glyph;
  FT_BBox box;
	if (FT_Get_Glyph(font->face->glyph, &glyph))
  {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "cannot load glyph.");
  }
	FT_Glyph_Get_CBox(glyph, FT_GLYPH_BBOX_PIXELS, &box);
  if (FT_Glyph_To_Bitmap(&glyph, FT_RENDER_MODE_NORMAL, 0, 1))
  {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "cannot load glyph bitmap.");
  }
  struct mrb_Glyph *data = mrb_malloc(mrb, sizeof *glyph);
  assert(data);
  data->glyph = glyph;
  data->advance.x = box.xMax - box.xMin;
  data->advance.y = 0;
  data->top = data->glyph->left;
  data->left = data->glyph->top;
  create_glyph_texture(mrb, data, glyph);
  mrb_carbuncle_avl_insert(mrb, font->glyphs, charcode, data);
  assert(font->glyphs);
  return data;
}

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
  if (FT_Set_Pixel_Sizes(font->face, size, size))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot set font size for font %s.", filename);
  }
}


static void
mrb_font_free(mrb_state *mrb, void *ptr)
{
  struct mrb_Font *font = ptr;
  if (font)
  {
    if (font->glyphs)
    {
      mrb_carbuncle_avl_free(mrb, font->glyphs);
    }
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
  font->face = NULL;
  font->glyphs = NULL;
  mrb_carbuncle_check_file(mrb, name);
  font->glyphs = mrb_carbuncle_avl_new(mrb, free_glyph);
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
  uint32_t codepoint;
  const char *text;
  mrb_get_args(mrb, "z", &text);
  struct mrb_Font *font = mrb_carbuncle_get_font(mrb, self);
  rect.x = 0;
  rect.y = 0;
  size_t len = utf8_strlen(text);
  for (size_t i = 0; i < len; ++i)
  {
    text = utf8_decode(text, &codepoint);
    if(FT_Load_Char(font->face, codepoint, FT_LOAD_TARGET_NORMAL))
    {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "cannot load font character.");
    }
    FT_Glyph glyph;
		if (FT_Get_Glyph(font->face->glyph, &glyph))
    {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "cannot load font character.");
    }

		FT_BBox bbox;
		FT_Glyph_Get_CBox(glyph, FT_GLYPH_BBOX_PIXELS, &bbox);

		rect.x += bbox.xMax - bbox.xMin;
		if (rect.y < bbox.yMax - bbox.yMin) rect.y = bbox.yMax - bbox.yMin;

    FT_Done_Glyph(glyph);
  }
  return mrb_carbuncle_point_new(mrb, rect.x, rect.y);
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

struct mrb_Glyph *
mrb_carbuncle_font_glyph(mrb_state *mrb, struct mrb_Font *font, FT_ULong charcode)
{
  struct mrb_Glyph *glyph = mrb_carbuncle_avl_data(font->glyphs, charcode);
  if (!glyph) { glyph = load_glyph(mrb, font, charcode); }
  return glyph;
}
