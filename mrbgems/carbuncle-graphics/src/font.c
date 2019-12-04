#include "carbuncle/core.h"
#include "carbuncle/font.h"
#include "carbuncle/point.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/string.h>
#include <mruby/array.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#define DEFAULT_FONT_CAPA 63

static FT_Library carbuncle_freetype;

static void
add_char(mrb_state *mrb, struct mrb_Font *font, FT_ULong character)
{
  if (font->glyphs.capa <= font->glyphs.size)
  {
    size_t new_capa = (font->glyphs.capa * 2) + 1;
    font->glyphs.list = mrb_realloc(mrb, font->glyphs.list, new_capa * sizeof(int));
    font->glyphs.capa = new_capa;
  }
  font->glyphs.list[font->glyphs.size] = character;
  font->glyphs.size += 1;
}

static void
load_characters(mrb_state *mrb, struct mrb_Font *font, const char *filename)
{
  FT_Face face;
  FT_UInt index;
  if (FT_New_Face(carbuncle_freetype, filename, 0, &face))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot load font '%s'.", filename);
  }
  if (FT_Select_Charmap(face, FT_ENCODING_UNICODE))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot load font '%s' as unicode.", filename);
  }
  FT_ULong character = FT_Get_First_Char(face, &index);
  while(index)
  {
    add_char(mrb, font, character);
    character = FT_Get_Next_Char(face, character, &index);
  }
  FT_Done_Face(face);
}


static void
mrb_font_free(mrb_state *mrb, void *ptr)
{
  struct mrb_Font *font = ptr;
  if (font)
  {
    UnloadFont(font->data);
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
    if (!mrb_nil_p(default_name))
    {
      name = mrb_str_to_cstr(mrb, default_name);
    }
  }
  if (argc < 2)
  {
    size = mrb_fixnum(mrb_to_int(mrb, mrb_funcall(mrb, font_class, "default_size", 0)));
  }
  struct mrb_Font *font = mrb_malloc(mrb, sizeof *font);
  if (name)
  {
    mrb_carbuncle_check_file(mrb, name);
    font->glyphs.capa = DEFAULT_FONT_CAPA;
    font->glyphs.size = 0;
    font->glyphs.list = mrb_malloc(mrb, DEFAULT_FONT_CAPA * sizeof(int));
    load_characters(mrb, font, name);
    font->data = LoadFontEx(name, size, font->glyphs.list, font->glyphs.size);
  }
  else
  {
    font->data = GetFontDefault();
  }
  font->size = size;
  DATA_PTR(self) = font;
  DATA_TYPE(self) = &font_data_type;
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
  mrb_get_args(mrb, "z", &text);
  struct mrb_Font *font = mrb_carbuncle_get_font(mrb, self);
  Vector2 point = MeasureTextEx(font->data, text, font->size, 0);
  return mrb_carbuncle_point_new(mrb, point.x, point.y);
}

static mrb_value
mrb_font_get_characters(mrb_state *mrb, mrb_value self)
{
  struct mrb_Font *font = mrb_carbuncle_get_font(mrb, self);
  int arena = mrb_gc_arena_save(mrb);
  mrb_value result = mrb_ary_new_capa(mrb, font->glyphs.size);
  for (size_t i = 0; i < font->glyphs.size; ++i)
  {
    char str[5];
    carbuncle_utf8_encode(str, font->glyphs.list[i]);
    mrb_ary_push(mrb, result, mrb_str_new_cstr(mrb, str));
  }
  mrb_gc_protect(mrb, result);
  mrb_gc_arena_restore(mrb, arena);
  return result;
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

  mrb_define_method(mrb, font, "characters", mrb_font_get_characters, MRB_ARGS_NONE());
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