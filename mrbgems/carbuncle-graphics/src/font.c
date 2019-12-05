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

#define DEFAULT_FONT_CAPA 95

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
  if (FT_Set_Pixel_Sizes(font->face, size, size))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot set font size for font %s.", filename);
  }
  font->chars = mrb_malloc(mrb, DEFAULT_FONT_CAPA * sizeof(int));
  for (size_t i = 0; i < DEFAULT_FONT_CAPA; ++i)
  {
    font->chars[i] = ' ' + i;
  }
  font->count = DEFAULT_FONT_CAPA;
  font->font = LoadFontEx(filename, size, font->chars, font->count);
  font->dirty = FALSE;
}


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
    UnloadFont(font->font);
    mrb_free(mrb, font->chars);
    mrb_free(mrb, font->filename);
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
  size_t str_size = strlen(name) + 1;
  font->filename = mrb_malloc(mrb, str_size);
  memset(font->filename, '\0', str_size);
  strcpy(font->filename, name);
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
  MeasureTextEx(data->font, text, data->size, 0);
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

static int
reorder_codepoints(mrb_state *mrb, struct mrb_Font *font, uint32_t codepoint)
{
  uint32_t min = ' ';
  uint32_t max = codepoint;
  size_t new_count;
  if ((codepoint - min) < font->count)
  {
    return 0;
  }
  for (size_t i = 0; i < font->count; ++i)
  {
    if (min > font->chars[i])
    {
      min = font->chars[i];
    }
    if (max < font->chars[i])
    {
      max = font->chars[i];
    }
  }
  new_count = max - min + 1;
  if (new_count < font->count)
  {
    return 0;
  }
  font->chars = mrb_realloc(mrb, font->chars, new_count * sizeof *(font->chars));
  font->count = new_count;
  for (size_t i = 0; i < font->count; ++i)
  {
    font->chars[i] = min + i;
  }
  return 1;
}

void
mrb_carbuncle_font_check_data(mrb_state *mrb, struct mrb_Font *font, uint32_t codepoint)
{
  if (reorder_codepoints(mrb, font, codepoint))
  {
    font->dirty = TRUE;
  }
}
