/*
** env.c - ENV is a Hash-like accessor for environment variables.
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/hash.h"
#include "mruby/khash.h"
#include "mruby/class.h"
#include "mruby/array.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include <string.h>
#include <stdio.h>

#ifdef ENABLE_ENV
static char **origenviron;

extern char **environ;

static mrb_value
mrb_env_getenv(mrb_state *mrb, mrb_value name)
{
  if (mrb_type(name) != MRB_TT_STRING) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, name));
    return mrb_nil_value();
  }

  char *nam = mrb_string_value_ptr(mrb, name);
  char *env = getenv(nam);
  if (env == NULL) {
    return mrb_nil_value();
  }
  return mrb_str_new2(mrb, env);
}

static mrb_value
mrb_env_unsetenv(mrb_state *mrb, mrb_value name)
{
  mrb_value val = mrb_env_getenv(mrb, name);
  if (mrb_nil_p(val)) {
    return mrb_nil_value();
  }

  char *nam = mrb_string_value_ptr(mrb, name);
  if (unsetenv(nam) != 0) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "can't delete environment variable");
    return mrb_nil_value();
  }
  return val;
}

static mrb_value
mrb_env_setenv(mrb_state *mrb, mrb_value name, mrb_value value)
{
  if (mrb_type(name) != MRB_TT_STRING) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, name));
    return mrb_nil_value();
  }

  if (mrb_nil_p(value)) {
    return mrb_env_unsetenv(mrb, name);
  }

  if (mrb_type(value) != MRB_TT_STRING) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't convert %s into String", mrb_obj_classname(mrb, value));
    return mrb_nil_value();
  }

  char *nam = mrb_string_value_ptr(mrb, name);
  char *val = mrb_string_value_ptr(mrb, value);
  if (setenv(nam, val, 1) != 0) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "can't change environment variable");
    return mrb_nil_value();;
  }
  return value;
}

mrb_value
mrb_env_aget(mrb_state *mrb, mrb_value self)
{
  mrb_value key;

  mrb_get_args(mrb, "o", &key);
  return mrb_env_getenv(mrb, key);
}

mrb_value
mrb_env_keys(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_value ary;

  ary = mrb_ary_new(mrb);
  for (i = 0; environ[i] != NULL; i++) {
    char *str = strchr(environ[i], '=');
    if (str != NULL) {
      int len = str - environ[i];
      mrb_ary_push(mrb, ary, mrb_str_new(mrb, environ[i], len));
    }
  }

  return ary;
}

mrb_value
mrb_env_values(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_value ary;

  ary = mrb_ary_new(mrb);
  for (i = 0; environ[i] != NULL; i++) {
    char *str = strchr(environ[i], '=');
    if (str) {
      str++;
      int len = strlen(str);
      mrb_ary_push(mrb, ary, mrb_str_new(mrb, str, len));
    }
  }

  return ary;
}

static mrb_value
mrb_env_size(mrb_state *mrb, mrb_value self)
{
  int i;

  for (i = 0; environ[i] != NULL; i++)
    ;

  return mrb_fixnum_value(i);
}

static mrb_value
mrb_env_to_hash(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_value hash;

  hash = mrb_hash_new(mrb);
  for (i = 0; environ[i] != NULL; i++) {
    char *str = strchr(environ[i], '=');
    if (str != NULL) {
      int len = str - environ[i];
      mrb_value key = mrb_str_new(mrb, environ[i], len);
      str++;
      mrb_value val = mrb_str_new(mrb, str, strlen(str));
      mrb_hash_set(mrb, hash, key, val);
    }
  }

  return hash;
}

static mrb_value
mrb_env_to_a(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_value ary;

  ary = mrb_ary_new(mrb);
  for (i = 0; environ[i] != NULL; i++) {
    char *str = strchr(environ[i], '=');
    if (str != NULL) {
      mrb_value elem = mrb_ary_new(mrb);
      int len = str - environ[i];
      mrb_ary_push(mrb, elem, mrb_str_new(mrb, environ[i], len));
      str++;
      mrb_ary_push(mrb, elem, mrb_str_new(mrb, str, strlen(str)));
      mrb_ary_push(mrb, ary, elem);
    }
  }

  return ary;
}

static mrb_value
mrb_env_inspect(mrb_state *mrb, mrb_value self)
{
  mrb_value hash = mrb_env_to_hash(mrb, self);
  return mrb_funcall(mrb, hash, "inspect", 0);
}

static mrb_value
mrb_env_to_s(mrb_state *mrb, mrb_value self)
{
  return mrb_str_new2(mrb, "ENV");
}

static mrb_value
mrb_env_aset(mrb_state *mrb, mrb_value self)
{
  mrb_value name, value;

  mrb_get_args(mrb, "oo", &name, &value);
  if (mrb_nil_p(value)) {
    mrb_env_unsetenv(mrb, name);
  } else {
    mrb_env_setenv(mrb, name, value);
  }
  return value;
}

static mrb_value
mrb_env_delete(mrb_state *mrb, mrb_value self)
{
  mrb_value name;

  mrb_get_args(mrb, "o", &name);
  mrb_env_unsetenv(mrb, name);
  return mrb_nil_value();
}

static mrb_value
mrb_env_clear(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_value keys = mrb_env_keys(mrb, self);

  for (i = 0; i < RARRAY_LEN(keys); i++) {
    mrb_env_unsetenv(mrb, mrb_ary_ref(mrb, keys, i));
  }

  return self;
}

void
mrb_init_env(mrb_state *mrb)
{
  struct RObject *e;

  origenviron = environ;
  e = (struct RObject*) mrb_obj_alloc(mrb, MRB_TT_OBJECT, mrb->object_class);
  mrb_include_module(mrb, (struct RClass*)e, mrb_class_get(mrb, "Enumerable"));

  mrb_define_singleton_method(mrb, e,"[]",       mrb_env_aget,       ARGS_REQ(1));
  mrb_define_singleton_method(mrb, e,"[]=",      mrb_env_aset,       ARGS_REQ(2));
  mrb_define_singleton_method(mrb, e,"store",    mrb_env_aset,       ARGS_REQ(2));
  mrb_define_singleton_method(mrb, e,"clear",    mrb_env_clear,      ARGS_NONE());
  mrb_define_singleton_method(mrb, e,"delete",   mrb_env_delete,     ARGS_REQ(1));
  mrb_define_singleton_method(mrb, e,"keys",     mrb_env_keys,       ARGS_NONE());
  mrb_define_singleton_method(mrb, e,"values",   mrb_env_values,     ARGS_NONE());
  mrb_define_singleton_method(mrb, e,"size",     mrb_env_size,       ARGS_NONE());

  mrb_define_singleton_method(mrb, e,"to_hash",  mrb_env_to_hash,    ARGS_NONE());
  mrb_define_singleton_method(mrb, e,"to_a",     mrb_env_to_a,       ARGS_NONE());
  mrb_define_singleton_method(mrb, e,"inspect",  mrb_env_inspect,    ARGS_NONE());
  mrb_define_singleton_method(mrb, e,"to_s",     mrb_env_to_s,       ARGS_NONE());

  mrb_define_global_const(mrb, "ENV", mrb_obj_value(e));
}

#endif /* ENABLE_ENV */
