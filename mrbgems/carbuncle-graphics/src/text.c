#include "carbuncle/core.h"
#include "carbuncle/color.h"
#include "carbuncle/font.h"
#include "carbuncle/text.h"

#include <assert.h>

#include "carbuncle/point.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/string.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include "microutf8.h"

const char *FONT_SYM = "#font";
const char *COLOR_SYM = "#color";
const char *POSITION_SYM = "#position";

#define FONT_SYMBOL mrb_intern_cstr(mrb, FONT_SYM)
#define COLOR_SYMBOL mrb_intern_cstr(mrb, COLOR_SYM)
#define POSITION_SYMBOL mrb_intern_cstr(mrb, POSITION_SYM)
#define VALUE_SYMBOL mrb_intern_cstr(mrb, "#value")

struct mrb_Text
{
  struct mrb_Font  *font;
  Color            *color;
  Vector2          *position;
  Texture2D         texture;
  mrb_bool          empty;
};

static const struct mrb_data_type text_data_type = {
  "Carbuncle::Text", mrb_free
};

static struct mrb_Text *
get_text(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &text_data_type, struct mrb_Text);
}

static float
get_min_height(size_t len, FT_BitmapGlyph *bmps, float size)
{
  float result = size;
  for (size_t i = 0; i < len; ++i)
  {
    float new_height = size - bmps[i]->top;
    if (result > new_height) { result = new_height; }
  }
  return result;
}

static void
draw_glyph(Color *pixels, FT_BitmapGlyph bmp, Vector2 size, float min_y)
{
	size_t left = bmp->left;
	size_t top = size.y - bmp->top;
	for (size_t y = 0; y < bmp->bitmap.rows; y++)
  {
		for (size_t x = 0; x < bmp->bitmap.width; x++)
    {
      size_t i = ((left + x) + (top + y - min_y) * size.x);
      unsigned char alpha = bmp->bitmap.buffer[x + y * bmp->bitmap.width];
      pixels[i] = (Color){ 255, 255, 255, alpha };
    }
  }
}

static Texture2D
create_texture(mrb_state *mrb, size_t len, FT_BitmapGlyph *bmps, Vector2 size)
{
  Texture2D texture;
  size_t byte_size = size.x * size.y * sizeof(Color);
  Color *pixels = mrb_malloc(mrb, byte_size);
  memset(pixels, 0, byte_size);
  float min_y = get_min_height(len, bmps, size.y);
  for (size_t i = 0; i  < len; ++i)
  {
    draw_glyph(pixels, bmps[i], size, min_y);
  }
  Image img = LoadImageEx(pixels, size.x, size.y);
  texture = LoadTextureFromImage(img);
  UnloadImage(img);
  mrb_free(mrb, pixels);
  return texture;
}

static void
update_text(mrb_state *mrb, struct mrb_Text *text, const char *message)
{
	text->empty = FALSE;
  size_t len = utf8_strlen(message);
  if (len <= 0)
  {
    text->empty = TRUE;
    return;
  }
  FT_Face face = mrb_carbuncle_font_get_face(text->font);
  FT_BitmapGlyph *bmps = mrb_carbuncle_font_load_glyphs(mrb, face, len, message);
	Vector2 size = mrb_carbuncle_font_calculate_size(len, bmps);
  text->texture = create_texture(mrb, len, bmps, size);
  mrb_carbuncle_font_destroy_glyphs(mrb, len, bmps);
}

static struct mrb_value
mrb_text_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value font = mrb_obj_new(mrb, mrb_carbuncle_class_get(mrb, "Font"), 0, NULL);
  mrb_value color = mrb_carbuncle_color_new(mrb, 255, 255, 255, 255);
  mrb_value position = mrb_carbuncle_point_new(mrb, 0, 0);
  mrb_iv_set(mrb, self, FONT_SYMBOL, font);
  mrb_iv_set(mrb, self, COLOR_SYMBOL, color);
  mrb_iv_set(mrb, self, POSITION_SYMBOL, position);
  mrb_iv_set(mrb, self, VALUE_SYMBOL, mrb_str_new_cstr(mrb, ""));
  struct mrb_Text *text = mrb_malloc(mrb, sizeof *text);
  text->font = mrb_carbuncle_get_font(mrb, font);
  text->color = mrb_carbuncle_get_color(mrb, color);
  text->position = mrb_carbuncle_get_point(mrb, position);
  DATA_PTR(self) = text;
  DATA_TYPE(self) = &text_data_type;
  update_text(mrb, text, "");
  return self;
}

static struct mrb_value
mrb_text_get_value(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, VALUE_SYMBOL);
}

static struct mrb_value
mrb_text_get_font(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, FONT_SYMBOL);
}

static struct mrb_value
mrb_text_get_color(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, COLOR_SYMBOL);
}

static mrb_value
mrb_text_get_position(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, POSITION_SYMBOL);
}

static mrb_value
mrb_text_set_value(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "S", &value);
  struct mrb_Text *text = get_text(mrb, self);
  mrb_iv_set(mrb, self, VALUE_SYMBOL, value);
  const char *message = mrb_str_to_cstr(mrb, value);
  update_text(mrb, text, message);
  return value;
}

static mrb_value
mrb_text_set_font(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Text *text = get_text(mrb, self);
  text->font = mrb_carbuncle_get_font(mrb, value);
  mrb_iv_set(mrb, self, FONT_SYMBOL, value);
  const char *message = mrb_str_to_cstr(mrb, mrb_iv_get(mrb, self, VALUE_SYMBOL));
  update_text(mrb, text, message);
  return value;
}

static mrb_value
mrb_text_set_color(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Text *text = get_text(mrb, self);
  text->color = mrb_carbuncle_get_color(mrb, value);
  mrb_iv_set(mrb, self, COLOR_SYMBOL, value);
  return value;
}

static mrb_value
mrb_text_set_position(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Text *text = get_text(mrb, self);
  text->position = mrb_carbuncle_get_point(mrb, value);
  mrb_iv_set(mrb, self, POSITION_SYMBOL, value);
  return value;
}

static mrb_value
mrb_text_draw(mrb_state *mrb, mrb_value self)
{
  struct mrb_Text *text = get_text(mrb, self);
  if (text->empty) { return self; }
  DrawTextureV(text->texture, *(text->position), *(text->color));
  return self;
}

void
mrb_carbuncle_text_init(mrb_state *mrb)
{
  struct RClass *text = mrb_carbuncle_define_data_class(mrb, "Text", mrb->object_class);

  mrb_define_method(mrb, text, "initialize", mrb_text_initialize, MRB_ARGS_NONE());

  mrb_define_method(mrb, text, "font", mrb_text_get_font, MRB_ARGS_NONE());
  mrb_define_method(mrb, text, "color", mrb_text_get_color, MRB_ARGS_NONE());
  mrb_define_method(mrb, text, "position", mrb_text_get_position, MRB_ARGS_NONE());
  mrb_define_method(mrb, text, "value", mrb_text_get_value, MRB_ARGS_NONE());

  mrb_define_method(mrb, text, "value=", mrb_text_set_value, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, text, "font=", mrb_text_set_font, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, text, "color=", mrb_text_set_color, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, text, "position=", mrb_text_set_position, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, text, "draw", mrb_text_draw, MRB_ARGS_NONE());
}
