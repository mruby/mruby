#include "carbuncle/core.h"
#include "carbuncle/color.h"
#include "carbuncle/rect.h"
#include "carbuncle/font.h"
#include "carbuncle/point.h"

#include "carbuncle/bitmap.h"

#include "microutf8.h"

#include <string.h>

#include <mruby/string.h>
#include <mruby/numeric.h>
#include <mruby/variable.h>
#include <mruby/class.h>

#define FONT_SYMBOL mrb_intern_cstr(mrb, "#font")

static void
mrb_bitmap_free(mrb_state *mrb, void *ptr)
{
  Image *img = ptr;
  if (img)
  {
    UnloadImage(*img);
    mrb_free(mrb, img);
  }
}
static const struct mrb_data_type bitmap_data_type = {
  "Carbuncle::Bitmap", mrb_bitmap_free
};

static Color
get_blt_color(mrb_state *mrb, mrb_value obj)
{
  if (mrb_carbuncle_color_p(obj))
  {
    return *mrb_carbuncle_get_color(mrb, obj);
  }
  mrb_int opacity = mrb_fixnum(mrb_to_int(mrb, obj));
  if (opacity > 255) { opacity = 255; }
  if (opacity < 0) { opacity = 0; }
  return (Color){255, 255, 255, opacity };
}

static void
draw_text(Image *dst, Rectangle dst_rect, struct mrb_Font *font, const char *msg, Color color)
{
  const char *text;
  uint32_t codepoint;
  mrb_int min_height = font->metrics.max_height;
  Vector2 size = mrb_carbuncle_font_measure_text(font, text);
  Image text_img = GenImageColor(size.x, size.y, BLANK);
  size_t len = utf8_strlen(text);
  Vector2 position = { 0, 0 };
  Rectangle src_rect = (Rectangle){ 0, 0, size.x, size.y };
  text = msg;
  for (size_t i = 0; i < len; ++i)
  {
    text = utf8_decode(text, &codepoint);
    struct mrb_Glyph *glyph = mrb_carbuncle_font_get_glyph(font, codepoint);
    if (glyph)
    {
      mrb_int min_y = font->metrics.max_height - glyph->margin.y;
      if (min_y < min_height) { min_height = min_y; }
    }
  }
  text = msg;
  for (size_t i = 0; i < len; ++i)
  {
    text = utf8_decode(text, &codepoint);
    struct mrb_Glyph *glyph = mrb_carbuncle_font_get_glyph(font, codepoint);
    if (glyph)
    {
      Rectangle glyph_dst = (Rectangle){
        position.x + glyph->margin.x,
        position.y + min_height - glyph->margin.y,
        glyph->rect.width,
        glyph->rect.height
      };
      ImageDraw(&text_img, font->image, glyph->rect, glyph_dst, color);
      position.x += glyph->advance.x;
      position.y += glyph->advance.y;
    }
  }
  ImageDraw(dst, text_img, src_rect, dst_rect, WHITE);
  UnloadImage(text_img);
}

static mrb_value
mrb_bitmap_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_int width, height, argc;
  argc = mrb_get_args(mrb, "o|i", &obj, &height);
  Image *img = mrb_malloc(mrb, sizeof *img);
  if (argc == 1)
  {
    if (mrb_carbuncle_bitmap_p(obj))
    {
      Image *src = mrb_carbuncle_get_bitmap(mrb, obj);
      *img = ImageCopy(*src);
    }
    else
    {
      const char *filename = mrb_str_to_cstr(mrb, obj);
      mrb_carbuncle_check_file(mrb, filename);
      *img = LoadImage(filename);
    }
  }
  else
  {
    width = mrb_fixnum(mrb_to_int(mrb, obj));
    if (width <= 0) { width = 1; }
    if (height <= 0) { height = 1; }
    Color *colors = mrb_malloc(mrb, width * height);
    memset(colors, 0, width * height);
    *img = LoadImageEx(colors, width, height);
    mrb_free(mrb, colors);
  }
  ImageFormat(img, UNCOMPRESSED_R8G8B8A8);
  DATA_PTR(self) = img;
  DATA_TYPE(self) = &bitmap_data_type;
  mrb_iv_set(mrb, self, FONT_SYMBOL, mrb_nil_value());
  return self;
}

static mrb_value
mrb_bitmap_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

static mrb_value
mrb_bitmap_dispose(mrb_state *mrb, mrb_value self)
{
  Image *image = mrb_carbuncle_get_bitmap(mrb, self);
  mrb_bitmap_free(mrb, image);
  DATA_PTR(self) = NULL;
  return self;
}

static mrb_value
mrb_bitmap_save(mrb_state *mrb, mrb_value self)
{
  const char *filename;
  mrb_get_args(mrb, "z", &filename);
  Image *image = mrb_carbuncle_get_bitmap(mrb, self);
  ExportImage(*image, filename);
  return self;
}

static mrb_value
mrb_bitmap_get_font(mrb_state *mrb, mrb_value self)
{
  mrb_value font = mrb_iv_get(mrb, self, FONT_SYMBOL);
  if (mrb_nil_p(font))
  {
    font = mrb_obj_new(mrb, mrb_carbuncle_class_get(mrb, "Font"), 0, NULL);
    mrb_iv_set(mrb, self, FONT_SYMBOL, font);
  }
  return font;
}

static mrb_value
mrb_bitmap_get_rect(mrb_state *mrb, mrb_value self)
{
  Image *image = mrb_carbuncle_get_bitmap(mrb, self);
  return mrb_carbuncle_rect_new(mrb, 0, 0, image->width, image->height);
}

static mrb_value
mrb_bitmap_get_width(mrb_state *mrb, mrb_value self)
{
  Image *image = mrb_carbuncle_get_bitmap(mrb, self);
  return mrb_fixnum_value(image->width);
}

static mrb_value
mrb_bitmap_get_height(mrb_state *mrb, mrb_value self)
{
  Image *image = mrb_carbuncle_get_bitmap(mrb, self);
  return mrb_fixnum_value(image->height);
}

static mrb_value
mrb_bitmap_get_subscript(mrb_state *mrb, mrb_value self)
{
  Image *img = mrb_carbuncle_get_bitmap(mrb, self);
  mrb_int x, y;
  mrb_get_args(mrb, "ii", &x, &y);
  if (x < 0 || x >= img->width || y < 0 || y <= img->height)
  {
    return mrb_carbuncle_color_new(mrb, 0, 0, 0, 0);
  }
  size_t index =  (x + y * img->width) * 4;
  uint8_t *pixels = (uint8_t*)img->data;
  Color color = {
    pixels[index + 0],
    pixels[index + 1],
    pixels[index + 2],
    pixels[index + 3]
  };
  return mrb_carbuncle_color_new(mrb, color.r, color.g, color.b, color.a);
}

static mrb_value
mrb_bitmap_set_font(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  mrb_carbuncle_get_font(mrb, value);
  mrb_iv_set(mrb, self, FONT_SYMBOL, value);
  return value;
}

static mrb_value
mrb_bitmap_set_subscript(mrb_state *mrb, mrb_value self)
{
  mrb_int x, y;
  mrb_value value;
  mrb_get_args(mrb, "iio", &x, &y, &value);
  Image *img = mrb_carbuncle_get_bitmap(mrb, self);
  Color *color = mrb_carbuncle_get_color(mrb, value);
  if (x < 0 || x >= img->width || y < 0 || y <= img->height)
  {
    return value;
  }
  size_t index =  (x + y * img->width) * 4;
  uint8_t *pixels = (uint8_t*)img->data;
  pixels[index + 0] = color->r;
  pixels[index + 1] = color->g;
  pixels[index + 2] = color->b;
  pixels[index + 3] = color->a;
  return value;
}

static mrb_value
mrb_bitmap_blt(mrb_state *mrb, mrb_value self)
{
  Image src;
  Rectangle src_rect, dst_rect;
  Color color;
  mrb_int argc = mrb_get_argc(mrb);
  Image *dst = mrb_carbuncle_get_bitmap(mrb, self);
  switch (argc)
  {
    case 2:
    {
      mrb_value arg1, arg2;
      mrb_get_args(mrb, "oo", &arg1, &arg2);
      src = *mrb_carbuncle_get_bitmap(mrb, arg1);
      color = WHITE;
      if (mrb_carbuncle_point_p(arg2))
      {
        Vector2 *p = mrb_carbuncle_get_point(mrb, arg2);
        src_rect = (Rectangle) { 0, 0, src.width, src.height };
        dst_rect = (Rectangle) { p->x, p->y, src.width, src.height };
      }
      else
      {
        src_rect = (Rectangle) { 0, 0, src.width, src.height };
        dst_rect = *mrb_carbuncle_get_rect(mrb, arg2);
      }
      break;
    }
    case 3:
    {
      mrb_value arg1, arg2, arg3;
      mrb_get_args(mrb, "ooo", &arg1, &arg2, &arg3);
      src = *mrb_carbuncle_get_bitmap(mrb, arg1);
      if (mrb_fixnum_p(arg3) || mrb_float_p(arg3) || mrb_carbuncle_color_p(arg3))
      {
        src_rect = (Rectangle) { 0, 0, src.width, src.height };
        dst_rect = *mrb_carbuncle_get_rect(mrb, arg2);
        color    = get_blt_color(mrb, arg3);
      }
      else
      {
        dst_rect = *mrb_carbuncle_get_rect(mrb, arg2);
        src_rect = *mrb_carbuncle_get_rect(mrb, arg3);
        color = WHITE;
      }
      break;
    }
    case 4:
    {
      mrb_value arg1, arg2, arg3, arg4;
      mrb_get_args(mrb, "oooo", &arg1, &arg2, &arg3, &arg4);
      src = *mrb_carbuncle_get_bitmap(mrb, arg1);
      dst_rect = *mrb_carbuncle_get_rect(mrb, arg2);
      src_rect = *mrb_carbuncle_get_rect(mrb, arg3);
      color = get_blt_color(mrb, arg4);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "2, 3, or 4", argc); }
  }
  ImageDraw(dst, src, src_rect, dst_rect, color);
  return self;
}

static mrb_value
mrb_bitmap_resize(mrb_state *mrb, mrb_value self)
{
  mrb_int width, height;
  mrb_bool bicubic;
  Image *dst = mrb_carbuncle_get_bitmap(mrb, self);
  mrb_get_args(mrb, "ii|b", &width, &height, &bicubic);
  if (width < 1) { mrb_raise(mrb, E_ARGUMENT_ERROR, "width should be positive"); }
  if (height < 1) { mrb_raise(mrb, E_ARGUMENT_ERROR, "height should be positive"); }
  if (bicubic) { ImageResize(dst, width, height); }
  else { ImageResizeNN(dst, width, height); }
  return self;
}

static mrb_value
mrb_bitmap_fill_rect(mrb_state *mrb, mrb_value self)
{
  Rectangle rect;
  Color color;
  Image *dst = mrb_carbuncle_get_bitmap(mrb, self);
  mrb_int argc = mrb_get_argc(mrb);
  switch (argc)
  {
    case 2:
    {
      mrb_value arg1, arg2;
      mrb_get_args(mrb, "oo", &arg1, &arg2);
      rect  = *mrb_carbuncle_get_rect(mrb, arg1);
      color = *mrb_carbuncle_get_color(mrb, arg2);
      break;
    }
    case 5:
    {
      mrb_int x, y, w, h;
      mrb_value arg5;
      mrb_get_args(mrb, "iiiio", &x, &y, &w, &h, &arg5);
      rect = (Rectangle){x, y, w, h};
      color = *mrb_carbuncle_get_color(mrb, arg5);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "2, or 5", argc); }
  }
  ImageDrawRectangle(dst, rect, color);
  return self;
}

static mrb_value
mrb_bitmap_stroke_rect(mrb_state *mrb, mrb_value self)
{
  Rectangle rect;
  Color color;
  Image *dst = mrb_carbuncle_get_bitmap(mrb, self);
  mrb_int argc = mrb_get_argc(mrb);
  mrb_int thick = 1;
  switch (argc)
  {
    case 2:
    {
      mrb_value arg1, arg2;
      mrb_get_args(mrb, "oo", &arg1, &arg2);
      rect  = *mrb_carbuncle_get_rect(mrb, arg1);
      color = *mrb_carbuncle_get_color(mrb, arg2);
      break;
    }
    case 3:
    {
      mrb_value arg1, arg2;
      mrb_get_args(mrb, "ooi", &arg1, &arg2, &thick);
      rect  = *mrb_carbuncle_get_rect(mrb, arg1);
      color = *mrb_carbuncle_get_color(mrb, arg2);
      break;
    }
    case 5:
    {
      mrb_int x, y, w, h;
      mrb_value arg5;
      mrb_get_args(mrb, "iiiio", &x, &y, &w, &h, &arg5);
      rect = (Rectangle){x, y, w, h};
      color = *mrb_carbuncle_get_color(mrb, arg5);
      break;
    }
    case 6:
    {
      mrb_int x, y, w, h;
      mrb_value arg5;
      mrb_get_args(mrb, "iiiioi", &x, &y, &w, &h, &arg5, &thick);
      rect = (Rectangle){x, y, w, h};
      color = *mrb_carbuncle_get_color(mrb, arg5);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "2, 3, 5 or 6", argc); }
  }
  ImageDrawRectangleLines(dst, rect, thick, color);
  return self;
}

static mrb_value
mrb_bitmap_flip_x(mrb_state *mrb, mrb_value self)
{
  Image *dst = mrb_carbuncle_get_bitmap(mrb, self);
  ImageFlipHorizontal(dst);
  return self;
}

static mrb_value
mrb_bitmap_flip_y(mrb_state *mrb, mrb_value self)
{
  Image *dst = mrb_carbuncle_get_bitmap(mrb, self);
  ImageFlipVertical(dst);
  return self;
}

static mrb_value
mrb_bitmap_draw_text(mrb_state *mrb, mrb_value self)
{
  const char *text;
  Rectangle dst_rect;
  Color color;
  Image *dst = mrb_carbuncle_get_bitmap(mrb, self);
  struct mrb_Font *font = mrb_carbuncle_get_font(mrb, mrb_bitmap_get_font(mrb, self));
  mrb_int argc = mrb_get_argc(mrb);
  switch (argc)
  {
    case 3:
    {
      mrb_value arg1, arg3;
      mrb_get_args(mrb, "ozo", &arg1, &text, &arg3);
      dst_rect = *mrb_carbuncle_get_rect(mrb, arg1);
      color    = *mrb_carbuncle_get_color(mrb, arg3);
      break;
    }
    case 6:
    {
      mrb_float x, y, w, h;
      mrb_value arg6;
      mrb_get_args(mrb, "ffffzo", &x, &y, &w, &h, &text, &arg6);
      dst_rect = (Rectangle){x, y, w, h};
      color    = *mrb_carbuncle_get_color(mrb, arg6);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "3 or 6", argc); }
  }
  draw_text(dst, dst_rect, font, text, color);
  return self;
}

static mrb_value
mrb_s_bitmap_get_screenshot(mrb_state *mrb, mrb_value self)
{
  mrb_value values[2] = {
    mrb_fixnum_value(1), mrb_fixnum_value(1)
  };
  mrb_value img = mrb_obj_new(mrb, mrb_carbuncle_class_get(mrb, "Bitmap"), 2, values);
  Image *data = DATA_PTR(img);
  UnloadImage(*data);
  *data = GetScreenData();
  ImageFormat(data, UNCOMPRESSED_R8G8B8A8);
  return img;
}

void
mrb_init_carbuncle_bitmap(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *bitmap = mrb_define_class_under(mrb, carbuncle, "Bitmap", mrb->object_class);
  MRB_SET_INSTANCE_TT(bitmap, MRB_TT_DATA);

  mrb_define_method(mrb, bitmap, "initialize", mrb_bitmap_initialize, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_method(mrb, bitmap, "initialize_copy", mrb_bitmap_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, bitmap, "disposed?", mrb_bitmap_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, bitmap, "dispose", mrb_bitmap_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, bitmap, "save", mrb_bitmap_save, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, bitmap, "font", mrb_bitmap_get_font, MRB_ARGS_NONE());
  mrb_define_method(mrb, bitmap, "rect", mrb_bitmap_get_rect, MRB_ARGS_NONE());
  mrb_define_method(mrb, bitmap, "width", mrb_bitmap_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, bitmap, "height", mrb_bitmap_get_height, MRB_ARGS_NONE());
  mrb_define_method(mrb, bitmap, "[]", mrb_bitmap_get_subscript, MRB_ARGS_REQ(2));

  mrb_define_method(mrb, bitmap, "font=", mrb_bitmap_set_font, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, bitmap, "[]=", mrb_bitmap_set_subscript, MRB_ARGS_REQ(3));

  mrb_define_method(mrb, bitmap, "blt", mrb_bitmap_blt, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(2));
  mrb_define_method(mrb, bitmap, "draw_bitmap", mrb_bitmap_blt, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(1));
  mrb_define_method(mrb, bitmap, "resize", mrb_bitmap_resize, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(1));
  mrb_define_method(mrb, bitmap, "fill_rect", mrb_bitmap_fill_rect, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(3));
  mrb_define_method(mrb, bitmap, "stroke_rect", mrb_bitmap_stroke_rect, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(4));
  mrb_define_method(mrb, bitmap, "flip_x", mrb_bitmap_flip_x, MRB_ARGS_NONE());
  mrb_define_method(mrb, bitmap, "flip_y", mrb_bitmap_flip_y, MRB_ARGS_NONE());
  mrb_define_method(mrb, bitmap, "draw_text", mrb_bitmap_draw_text, MRB_ARGS_REQ(3)|MRB_ARGS_OPT(4));

  mrb_define_class_method(mrb, bitmap, "screenshot", mrb_s_bitmap_get_screenshot, MRB_ARGS_NONE());
}

Image *
mrb_carbuncle_get_bitmap(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &bitmap_data_type, Image);
}

mrb_bool
mrb_carbuncle_bitmap_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &bitmap_data_type);
}
