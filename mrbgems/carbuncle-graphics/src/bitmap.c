#include "carbuncle/core.h"
#include "carbuncle/color.h"

#include "carbuncle/bitmap.h"

#include <string.h>

#include <mruby/string.h>

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

static mrb_value
mrb_bitmap_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_int width, height, argc;
  argc = mrb_get_args(mrb, "o|i", &obj, &height);
  Image *img = mrb_malloc(mrb, sizeof *img);
  if (argc == 1)
  {
    const char *filename = mrb_str_to_cstr(mrb, obj);
    mrb_carbuncle_check_file(mrb, filename);
    *img = LoadImage(filename);
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
mrb_carbuncle_bitmap_init(mrb_state *mrb)
{
  struct RClass *bitmap = mrb_carbuncle_define_data_class(mrb, "Bitmap", mrb->object_class);

  mrb_define_method(mrb, bitmap, "initialize", mrb_bitmap_initialize, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));

  mrb_define_method(mrb, bitmap, "disposed?", mrb_bitmap_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, bitmap, "dispose", mrb_bitmap_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, bitmap, "save", mrb_bitmap_save, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, bitmap, "[]", mrb_bitmap_get_subscript, MRB_ARGS_REQ(2));

  mrb_define_method(mrb, bitmap, "[]=", mrb_bitmap_set_subscript, MRB_ARGS_REQ(3));

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
