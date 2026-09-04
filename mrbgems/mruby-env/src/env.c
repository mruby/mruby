/*
** env.c - ENV object for environment variable access
*/

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/error.h>

#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <stdlib.h>
#define environ _environ
#else
#ifdef __APPLE__
#include <crt_externs.h>
#define environ (*_NSGetEnviron())
#else
extern char **environ;
#endif
#endif

static void
env_check_key(mrb_state *mrb, mrb_value key)
{
  mrb_ensure_string_type(mrb, key);
  if (memchr(RSTRING_PTR(key), '=', RSTRING_LEN(key))) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "bad environment variable name: contains '='");
  }
  if (memchr(RSTRING_PTR(key), '\0', RSTRING_LEN(key))) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "bad environment variable name: contains null byte");
  }
}

/* ENV[] */
static mrb_value
mrb_env_aref(mrb_state *mrb, mrb_value self)
{
  mrb_value key;
  const char *val;

  mrb_get_args(mrb, "o", &key);
  env_check_key(mrb, key);
  val = getenv(RSTRING_PTR(key));
  if (val == NULL) return mrb_nil_value();
  return mrb_str_new_cstr(mrb, val);
}

/* ENV[]= */
static mrb_value
mrb_env_aset(mrb_state *mrb, mrb_value self)
{
  mrb_value key, val;

  mrb_get_args(mrb, "oo", &key, &val);
  env_check_key(mrb, key);

  if (mrb_nil_p(val)) {
#ifdef _WIN32
    _putenv_s(RSTRING_PTR(key), "");
#else
    unsetenv(RSTRING_PTR(key));
#endif
    return mrb_nil_value();
  }

  mrb_ensure_string_type(mrb, val);
  if (memchr(RSTRING_PTR(val), '\0', RSTRING_LEN(val))) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "bad environment variable value: contains null byte");
  }
#ifdef _WIN32
  _putenv_s(RSTRING_PTR(key), RSTRING_PTR(val));
#else
  setenv(RSTRING_PTR(key), RSTRING_PTR(val), 1);
#endif
  return val;
}

/* ENV.__delete(key) - returns old value or nil */
static mrb_value
mrb_env_delete(mrb_state *mrb, mrb_value self)
{
  mrb_value key;
  const char *val;
  mrb_value result;

  mrb_get_args(mrb, "o", &key);
  env_check_key(mrb, key);
  val = getenv(RSTRING_PTR(key));
  if (val == NULL) return mrb_nil_value();
  result = mrb_str_new_cstr(mrb, val);
#ifdef _WIN32
  _putenv_s(RSTRING_PTR(key), "");
#else
  unsetenv(RSTRING_PTR(key));
#endif
  return result;
}

/* ENV.keys */
static mrb_value
mrb_env_keys(mrb_state *mrb, mrb_value self)
{
  mrb_value ary = mrb_ary_new(mrb);
  char **env;
  int ai = mrb_gc_arena_save(mrb);

  for (env = environ; *env != NULL; env++) {
    char *eq = strchr(*env, '=');
    if (eq) {
      mrb_ary_push(mrb, ary, mrb_str_new(mrb, *env, (mrb_int)(eq - *env)));
      mrb_gc_arena_restore(mrb, ai);
    }
  }
  return ary;
}

/* ENV.__values */
static mrb_value
mrb_env_values(mrb_state *mrb, mrb_value self)
{
  mrb_value ary = mrb_ary_new(mrb);
  char **env;
  int ai = mrb_gc_arena_save(mrb);

  for (env = environ; *env != NULL; env++) {
    char *eq = strchr(*env, '=');
    if (eq) {
      mrb_ary_push(mrb, ary, mrb_str_new_cstr(mrb, eq + 1));
      mrb_gc_arena_restore(mrb, ai);
    }
  }
  return ary;
}

/* ENV.size */
static mrb_value
mrb_env_size(mrb_state *mrb, mrb_value self)
{
  mrb_int count = 0;
  char **env;

  for (env = environ; *env != NULL; env++) {
    count++;
  }
  return mrb_fixnum_value(count);
}

/* ENV.key? */
static mrb_value
mrb_env_has_key(mrb_state *mrb, mrb_value self)
{
  mrb_value key;

  mrb_get_args(mrb, "o", &key);
  env_check_key(mrb, key);
  return mrb_bool_value(getenv(RSTRING_PTR(key)) != NULL);
}

/* ENV.to_s */
static mrb_value
mrb_env_to_s(mrb_state *mrb, mrb_value self)
{
  return mrb_str_new_lit(mrb, "ENV");
}

/* ENV.__clear */
static mrb_value
mrb_env_clear(mrb_state *mrb, mrb_value self)
{
  while (*environ) {
    char *eq = strchr(*environ, '=');
    if (eq) {
      mrb_int len = (mrb_int)(eq - *environ);
      char *key = (char*)mrb_malloc(mrb, len + 1);
      memcpy(key, *environ, len);
      key[len] = '\0';
#ifdef _WIN32
      _putenv_s(key, "");
#else
      unsetenv(key);
#endif
      mrb_free(mrb, key);
    }
    else {
      break;
    }
  }
  return self;
}

void
mrb_mruby_env_gem_init(mrb_state *mrb)
{
  mrb_value env = mrb_obj_new(mrb, mrb->object_class, 0, NULL);
  struct RObject *eobj = mrb_obj_ptr(env);

  mrb_define_global_const(mrb, "ENV", env);

  mrb_define_singleton_method_id(mrb, eobj, MRB_OPSYM(aref), mrb_env_aref, MRB_ARGS_REQ(1));
  mrb_define_singleton_method_id(mrb, eobj, MRB_OPSYM(aset), mrb_env_aset, MRB_ARGS_REQ(2));
  mrb_define_singleton_method_id(mrb, eobj, MRB_SYM(__delete), mrb_env_delete, MRB_ARGS_REQ(1));
  mrb_define_singleton_method_id(mrb, eobj, MRB_SYM(keys), mrb_env_keys, MRB_ARGS_NONE());
  mrb_define_singleton_method_id(mrb, eobj, MRB_SYM(__values), mrb_env_values, MRB_ARGS_NONE());
  mrb_define_singleton_method_id(mrb, eobj, MRB_SYM(size), mrb_env_size, MRB_ARGS_NONE());
  mrb_define_singleton_method_id(mrb, eobj, MRB_SYM_Q(key), mrb_env_has_key, MRB_ARGS_REQ(1));
  mrb_define_singleton_method_id(mrb, eobj, MRB_SYM(to_s), mrb_env_to_s, MRB_ARGS_NONE());
  mrb_define_singleton_method_id(mrb, eobj, MRB_SYM(__clear), mrb_env_clear, MRB_ARGS_NONE());

  /* include Enumerable in ENV's singleton class */
  {
    struct RClass *sc = mrb_singleton_class_ptr(mrb, env);
    mrb_include_module(mrb, sc, mrb_module_get_id(mrb, MRB_SYM(Enumerable)));
  }
}

void
mrb_mruby_env_gem_final(mrb_state *mrb)
{
}
