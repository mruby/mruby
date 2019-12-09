#include "carbuncle/core.h"
#include "carbuncle/shader.h"

#include "raylib.h"
#include "rlgl.h"

#include <mruby/class.h>

#define UNIFORM_RECT   997
#define UNIFORM_COLOR  998
#define UNIFORM_MATRIX 999

#define UNIFORM_SYMBOL mrb_intern_cstr(mrb, "@uniform_values")
#define UNIFORM_TYPES  mrb_intern_cst(mrb, "@uniform_types")
struct mrb_Shader
{
  mrb_bool default_shader;
  Shader shader;
};

static void
mrb_shader_free(mrb_state *mrb, void *ptr)
{
  if (ptr)
  {
    struct mrb_Shader *shader = ptr;
    if (!shader->default_shader)
    {
      UnloadShader(shader->shader);
    }
    mrb_free(mrb, shader);
  }
}

static const struct mrb_data_type shader_data_type = {
  "Carbuncle::Shader", mrb_shader_free
};

static inline struct mrb_Shader *
get_shader(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &shader_data_type, struct mrb_Shader);
}

static void
load_shader(mrb_state *mrb, struct mrb_Shader *shader, const char *vertex, const char *fragment)
{
  mrb_bool free_vertex, free_fragment;
  free_vertex = FALSE;
  free_fragment = FALSE;
  if (vertex && FileExists(vertex)) { vertex = LoadText(vertex); free_vertex = TRUE; }
  if (fragment && FileExists(fragment)) { fragment = LoadText(fragment); free_fragment = TRUE; }
  shader->shader = LoadShaderCode(vertex, fragment);
  if (free_vertex) { RL_FREE(vertex); }
  if (free_fragment) { RL_FREE(fragment); }
}

static mrb_value
symbol_for(mrb_state *mrb, const char *value)
{
  return mrb_symbol_value(mrb_intern_cstr(mrb, value));
}

static void
add_uniform(mrb_state *mrb, mrb_value self, struct mrb_Shader *shader, const char *name, mrb_value class_value)
{
  mrb_funcall(mrb, self, "define_uniform", 2, symbol_for(mrb, name), class_value);
}

static void
set_default_shader_locations(mrb_state *mrb, mrb_value self)
{
  struct mrb_Shader *shader = get_shader(mrb, self);
  add_uniform(mrb, self, shader, "mvp", mrb_obj_value(mrb_class_get(mrb, "Matrix")));
  add_uniform(mrb, self, shader, "proyection",  mrb_obj_value(mrb_class_get(mrb, "Matrix")));
  add_uniform(mrb, self, shader, "view", mrb_obj_value(mrb_class_get(mrb, "Matrix")));
  add_uniform(mrb, self, shader, "colDiffuse", mrb_obj_value(mrb_class_get(mrb, "Color")));
  add_uniform(mrb, self, shader, "texture0", mrb_obj_value(mrb_class_get(mrb, "Texture")));
  add_uniform(mrb, self, shader, "texture1", mrb_obj_value(mrb_class_get(mrb, "Texture")));
  add_uniform(mrb, self, shader, "texture2", mrb_obj_value(mrb_class_get(mrb, "Texture")));
}

mrb_value
mrb_shader_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_int argc;
  const char *first, *second;
  struct mrb_Shader *shader = mrb_malloc(mrb, sizeof *shader);
  shader->default_shader = FALSE;
  argc = mrb_get_args(mrb, "|zz", &first, &second);
  DATA_PTR(self) = shader;
  DATA_TYPE(self) = &shader_data_type;
  switch (argc)
  {
    case 0:
    {
      shader->default_shader = TRUE;
      shader->shader = GetShaderDefault();
      break;
    }
    case 1:
    {
      if (FileExists(first) && (IsFileExtension(first, ".vert") || IsFileExtension(first, ".vs")))
      {
        load_shader(mrb, shader, first, NULL);
      }
      else
      {
        load_shader(mrb, shader, NULL, first);
      }
      break;
    }
    case 2:
    {
      load_shader(mrb, shader, first, second);
      break;
    }
  }
  set_default_shader_locations(mrb, self);
  return self;
}

mrb_value
mrb_s_shader_get_default(mrb_state *mrb, mrb_value self)
{
  return mrb_obj_new(mrb, mrb_carbuncle_class_get(mrb, "Shader"), 0, NULL);
}

mrb_value
mrb_s_shader_compile(mrb_state *mrb, mrb_value self)
{
  mrb_value *argv;
  mrb_int argc;
  struct RClass *shader = mrb_class_ptr(self);
  mrb_get_args(mrb, "*", &argv, &argc);
  return mrb_obj_new(mrb, shader, argc, argv);
}

mrb_value
mrb_shader_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

mrb_value
mrb_shader_dispose(mrb_state *mrb, mrb_value self)
{
  struct mrb_Shader *shader = get_shader(mrb, self);
  mrb_shader_free(mrb, shader);
  DATA_PTR(self) = NULL;
  return self;
}

void
mrb_carbuncle_shader_init(mrb_state *mrb)
{
  struct RClass *shader = mrb_carbuncle_define_data_class(mrb, "Shader", mrb->object_class);

  mrb_define_method(mrb, shader, "initialize", mrb_shader_initialize, MRB_ARGS_OPT(2));

  mrb_define_class_method(mrb, shader, "default", mrb_s_shader_get_default, MRB_ARGS_NONE());

  mrb_define_class_method(mrb, shader, "compile", mrb_s_shader_compile, MRB_ARGS_OPT(2));
  mrb_define_class_method(mrb, shader, "load", mrb_s_shader_compile, MRB_ARGS_OPT(2));

  mrb_define_method(mrb, shader, "disposed?", mrb_shader_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, shader, "dispose", mrb_shader_dispose, MRB_ARGS_NONE());
}
