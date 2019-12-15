#include "carbuncle/core.h"
#include "carbuncle/color.h"
#include "carbuncle/font.h"
#include "carbuncle/text.h"

#include <assert.h>

#include "carbuncle/point.h"

#include <mruby/class.h>
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

#define DEFAULT_CAPA 32

struct mrb_Text
{
  struct mrb_Font   *font;
  Color             *color;
  Vector2           *position;
  Texture2D          texture;
  size_t             len;
  size_t             capa;
  struct mrb_Glyph **glyphs;
  mrb_int            min_y;
};

static void
mrb_free_text(mrb_state *mrb, void *ptr)
{
  if (ptr)
  {
    struct mrb_Text *text = ptr;
    if (text->glyphs)
    {
      mrb_free(mrb, text->glyphs);
    }
    mrb_free(mrb, ptr);
  }
}

static const struct mrb_data_type text_data_type = {
  "Carbuncle::Text", mrb_free_text
};

static struct mrb_Text *
get_text(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &text_data_type, struct mrb_Text);
}

static void
update_glyph_capacity(mrb_state *mrb, struct mrb_Text *text, size_t len)
{
  mrb_bool change = FALSE;
  while (text->capa <= len)
  {
    text->capa = text->capa * 2 + 1;
    change = TRUE;
  }
  if (change)
  {
    text->glyphs = mrb_realloc(mrb, text->glyphs, text->capa * sizeof(struct mrb_Glyph *));
  }
}

static void
load_glyphs(mrb_state *mrb, struct mrb_Text *text, size_t len, const char *message)
{
  uint32_t codepoint;
  text->min_y = text->font->metrics.max_height;
  for (size_t i = 0; i < len; ++i)
  {
    message = utf8_decode(message, &codepoint);
    struct mrb_Glyph *glyph = mrb_carbuncle_font_get_glyph(text->font, codepoint);
    if (!glyph) {glyph = mrb_carbuncle_font_get_glyph(text->font, 0xFFFD); }
    if (glyph)
    {
      text->glyphs[i] = glyph;
      mrb_int min_y = text->font->metrics.max_height - glyph->margin.y;
      if (min_y < text->min_y) { text->min_y = min_y; }
    }
  }
}

static void
update_text(mrb_state *mrb, struct mrb_Text *text, const char *message)
{
  size_t len = utf8_strlen(message);
  text->len = len;
  update_glyph_capacity(mrb, text, len);
  load_glyphs(mrb, text, len, message);
}

static struct mrb_value
mrb_text_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value font = mrb_nil_value();
  mrb_get_args(mrb, "|o", &font);
  if (mrb_nil_p(font))
  {
    font = mrb_obj_new(mrb, mrb_carbuncle_class_get(mrb, "Font"), 0, NULL);
  }
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
  text->glyphs = mrb_malloc(mrb, DEFAULT_CAPA * sizeof(struct mrb_Glyph *));
  text->capa = DEFAULT_CAPA;
  text->len = 0;
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
  Vector2 position = *(text->position);
  Color color = *(text->color);
  for (size_t i = 0; i < text->len; ++i)
  {
    struct mrb_Glyph *glyph = text->glyphs[i];
    if (glyph)
    {
      Vector2 pos = (Vector2){
        position.x + glyph->margin.x,
        position.y + text->min_y - glyph->margin.y
      };
      DrawTextureRec(text->font->texture, glyph->rect, pos, color);
      position.x += glyph->advance.x;
      position.y += glyph->advance.y;
    }
  }
  return self;
}

void
mrb_init_carbuncle_text(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *text = mrb_define_class_under(mrb, carbuncle, "Text", mrb->object_class);
  MRB_SET_INSTANCE_TT(text, MRB_TT_DATA);

  mrb_define_method(mrb, text, "initialize", mrb_text_initialize, MRB_ARGS_OPT(1));

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
