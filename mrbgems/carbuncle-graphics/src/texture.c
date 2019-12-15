#include "carbuncle/core.h"
#include "carbuncle/texture.h"
#include "carbuncle/bitmap.h"

#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/string.h>

const char *TEXTURE_INITIALIZE_ERROR_MSG = "invalid new texture, it can only be created from another texture or a String as filename but %s was given.";

struct mrb_Texture
{
  Texture2D base;
  mrb_int ref_count;
};

static void
mrb_texture_free(mrb_state *mrb, void *ptr)
{
  struct mrb_Texture *texture = (struct mrb_Texture *)ptr;
  if (texture)
  {
    --(texture->ref_count);
    if (texture->ref_count <= 0)
    {
      UnloadTexture(texture->base);
      mrb_free(mrb, texture);
    }
  }
}

static const struct mrb_data_type texture_data_type = {
  "Carbuncle::Texture", mrb_texture_free
};

void
load_from_filename(mrb_state *mrb, mrb_value self, const char *filename)
{
  struct mrb_Texture *texture = mrb_malloc(mrb, sizeof *texture);
  mrb_carbuncle_check_file(mrb, filename);
  texture->base = LoadTexture(filename);
  ++(texture->ref_count);
  DATA_PTR(self) = texture;
  DATA_TYPE(self) = &texture_data_type;
}

static void
copy_texture(mrb_state *mrb, mrb_value self, mrb_value obj)
{
  mrb_carbuncle_get_texture(mrb, obj);
  struct mrb_Texture *texture = DATA_PTR(obj);
  ++(texture->ref_count);
  DATA_PTR(self) = texture;
  DATA_TYPE(self) = &texture_data_type;    
}

static void
load_image(mrb_state *mrb, mrb_value self, mrb_value obj)
{
  Image *img = mrb_carbuncle_get_bitmap(mrb, obj);
  struct mrb_Texture *texture = mrb_malloc(mrb, sizeof *texture);
  texture->base = LoadTextureFromImage(*img);
  ++(texture->ref_count);
  DATA_PTR(self) = texture;
  DATA_TYPE(self) = &texture_data_type;
}

static mrb_value
mrb_texture_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_get_args(mrb, "o", &obj);
  if (mrb_string_p(obj))
  {
    const char *filename = mrb_str_to_cstr(mrb, obj);
    load_from_filename(mrb, self, filename);
  }
  else if (mrb_carbuncle_texture_p(obj))
  {
    copy_texture(mrb, self, obj);
  }
  else if (mrb_carbuncle_bitmap_p(obj))
  {
    load_image(mrb, self, obj);
  }
  else
  {
    const char *class_name = mrb_class_name(mrb, mrb_obj_class(mrb, obj));
    mrb_raisef(mrb, E_ARGUMENT_ERROR, TEXTURE_INITIALIZE_ERROR_MSG, class_name);
  }
  return self;
}

static mrb_value
mrb_texture_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

static mrb_value
mrb_texture_get_width(mrb_state *mrb, mrb_value self)
{
  Texture2D *data = mrb_carbuncle_get_texture(mrb, self);
  return mrb_float_value(mrb, data->width);
}

static mrb_value
mrb_texture_get_height(mrb_state *mrb, mrb_value self)
{
  struct Texture2D *data = mrb_carbuncle_get_texture(mrb, self);
  return mrb_float_value(mrb, data->height);
}

static mrb_value
mrb_texture_dispose(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_texture(mrb, self);
  mrb_texture_free(mrb, DATA_PTR(self));
  DATA_PTR(self) = NULL;
  return self;
}

static mrb_value
mrb_texture_to_bitmap(mrb_state *mrb, mrb_value self)
{
  mrb_value values[2] = { mrb_fixnum_value(1), mrb_fixnum_value(1) };
  Texture2D *texture = mrb_carbuncle_get_texture(mrb, self);
  mrb_value img = mrb_obj_new(mrb, mrb_carbuncle_class_get(mrb, "Bitmap"), 2, values);
  Image *ptr = mrb_carbuncle_get_bitmap(mrb, img);
  UnloadImage(*ptr);
  *ptr = GetTextureData(*texture);
  ImageFormat(ptr, UNCOMPRESSED_R8G8B8A8);
  return img;
}

void
mrb_init_carbuncle_texture(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *texture = mrb_define_class_under(mrb, carbuncle, "Texture", mrb->object_class);
  MRB_SET_INSTANCE_TT(texture, MRB_TT_DATA);
  
  mrb_define_method(mrb, texture, "initialize", mrb_texture_initialize, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, texture, "initialize_copy", mrb_texture_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, texture, "width", mrb_texture_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "height", mrb_texture_get_height, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "w", mrb_texture_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "h", mrb_texture_get_height, MRB_ARGS_NONE());

  mrb_define_method(mrb, texture, "disposed?", mrb_texture_disposedQ, MRB_ARGS_NONE());

  mrb_define_method(mrb, texture, "dispose", mrb_texture_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, texture, "to_bitmap", mrb_texture_to_bitmap, MRB_ARGS_NONE());
}

Texture2D *
mrb_carbuncle_get_texture(mrb_state *mrb, mrb_value obj)
{
  struct mrb_Texture *texture = DATA_GET_DISPOSABLE_PTR(mrb, obj, &texture_data_type, struct mrb_Texture);
  return &(texture->base);
}

mrb_bool
mrb_carbuncle_texture_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &texture_data_type);
}
