#include "carbuncle/core.h"
#include "carbuncle/font.h"
#include "carbuncle/point.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/string.h>

#define MAX_FONT_SIZE 32767

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
    font->data = LoadFontEx(name, size, NULL, MAX_FONT_SIZE);
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

void
mrb_carbuncle_font_init(mrb_state *mrb)
{
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