#include "carbuncle/core.h"
#include "carbuncle/font.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/string.h>

static void
mrb_font_free(mrb_state *mrb, void *ptr)
{
  Font *font = (Font *)ptr;
  if (font)
  {
    mrb_free(mrb, font);
    UnloadFont(*font);
  }
}

static const struct mrb_data_type font_data_type = {
  "Carbuncle::Font", mrb_font_free
};

static mrb_value
mrb_font_initialize(mrb_state *mrb, mrb_value self)
{
  const char *name;
  mrb_int size, argc;
  Font *font = mrb_malloc(mrb, sizeof *font);
  mrb_value font_class = mrb_obj_value(mrb_obj_class(mrb, self));
  argc = mrb_get_args(mrb, "|zi", &name, &size);
  if (argc < 1)
  {
    name = mrb_str_ptr(mrb_funcall(mrb, font_class, "default_font", 0));
  }
  if (argc < 2)
  {
    size = mrb_fixnum(mrb_to_int(mrb, mrb_funcall(mrb, font_class, "default_size", 0)));
  }
  return self;
}

void
mrb_carbuncle_font_init(mrb_state *mrb)
{
  struct RClass *font = mrb_carbuncle_define_data_class(mrb, "Font", mrb->object_class);

  mrb_define_method(mrb, font, "initialize", mrb_font_initialize, MRB_ARGS_OPT(2));

  /*
  mrb_define_class_method(mrb, font, "default_name", mrb_s_font_get_default_name, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, font, "default_size", mrb_s_font_get_default_size, MRB_ARGS_NONE());

  mrb_define_class_method(mrb, font, "default_name=", mrb_s_font_set_default_name, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, font, "default_size=", mrb_s_font_set_default_size, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, font, "measure_text", mrb_font_measure_text, MRB_ARGS_REQ(1));
  */
}

Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &font_data_type, Font);
}

mrb_bool
mrb_carbuncle_font_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &font_data_type);
}