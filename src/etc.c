/*
** etc.c -
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/string.h"
#include "error.h"
#include "mruby/numeric.h"
#include "mruby/data.h"
#include "mruby/class.h"

struct RData*
mrb_data_object_alloc(mrb_state *mrb, struct RClass *klass, void *ptr, const mrb_data_type *type)
{
  struct RData *data;

  data = (struct RData*)mrb_obj_alloc(mrb, MRB_TT_DATA, klass);
  data->data = ptr;
  data->type = (mrb_data_type*) type;

  return data;
}

void
mrb_data_check_type(mrb_state *mrb, mrb_value obj, const mrb_data_type *type)
{
  if (mrb_special_const_p(obj) || (mrb_type(obj) != MRB_TT_DATA)) {
    mrb_check_type(mrb, obj, MRB_TT_DATA);
  }
  if (DATA_TYPE(obj) != type) {
    const mrb_data_type *t2 = DATA_TYPE(obj);

    if (t2) {
      mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %S (expected %S)",
                 mrb_str_new_cstr(mrb, t2->struct_name), mrb_str_new_cstr(mrb, type->struct_name));
    }
    else {
      struct RClass *c = mrb_class(mrb, obj);

      mrb_raisef(mrb, E_TYPE_ERROR, "uninitialized %S (expected %S)",
                 mrb_obj_value(c), mrb_str_new_cstr(mrb, type->struct_name));
    }
  }
}

void *
mrb_data_get_unchecked_ptr(mrb_state *mrb, mrb_value obj, const mrb_data_type *type)
{
  if (mrb_special_const_p(obj) || (mrb_type(obj) != MRB_TT_DATA)) {
    return NULL;
  }
  if (DATA_TYPE(obj) != type) {
    return NULL;
  }
  return DATA_PTR(obj);
}

void *
mrb_data_get_checked_ptr(mrb_state *mrb, mrb_value obj, const mrb_data_type *type)
{
  mrb_data_check_type(mrb, obj, type);
  return DATA_PTR(obj);
}

mrb_value
mrb_lastline_get(mrb_state *mrb)
{
  mrb_value *argv;
  int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc < 1) {
    return mrb_nil_value();
  }
  else
  {
    return argv[0];
  }
}

/* ------------------------------------------------ */
/*
 * Calls func(obj, arg, recursive), where recursive is non-zero if the
 * current method is called recursively on obj
 */

mrb_value
mrb_exec_recursive(mrb_state *mrb, mrb_value (*func) (mrb_state *, mrb_value, mrb_value, int), mrb_value obj, void *arg)
{
  return func(mrb, obj, *(mrb_value*)arg, 0);
}

mrb_sym
mrb_obj_to_sym(mrb_state *mrb, mrb_value name)
{
  mrb_value tmp;
  mrb_sym id;

  switch (mrb_type(name)) {
    default:
      tmp = mrb_check_string_type(mrb, name);
      if (mrb_nil_p(tmp)) {
        tmp = mrb_inspect(mrb, name);
        mrb_raisef(mrb, E_TYPE_ERROR, "%S is not a symbol", tmp);
      }
      name = tmp;
      /* fall through */
    case MRB_TT_STRING:
      name = mrb_str_intern(mrb, name);
      /* fall through */
    case MRB_TT_SYMBOL:
      return mrb_symbol(name);
  }
  return id;
}

/*
 * call-seq:
 *   proc   { |...| block }  -> a_proc
 *
 * Equivalent to <code>Proc.new</code>.
 */

mrb_value
mrb_block_proc(void)
{
  return mrb_nil_value();
}

static mrb_int
float_id(mrb_float f)
{
  const char *p = (const char*)&f;
  int len = sizeof(f);
  mrb_int id = 0;

  while (len--) {
    id = id*65599 + *p;
    p++;
  }
  id = id + (id>>5);

  return id;
}

mrb_int
mrb_obj_id(mrb_value obj)
{
  mrb_int tt = mrb_type(obj);

#define MakeID2(p,t) (((intptr_t)(p))^(t))
#define MakeID(p)    MakeID2(p,tt)

  switch (tt) {
  case  MRB_TT_FREE:
  case  MRB_TT_UNDEF:
    return MakeID(0); /* not define */
  case  MRB_TT_FALSE:
    if (mrb_nil_p(obj))
      return MakeID(1);
    return MakeID(0);
  case  MRB_TT_TRUE:
    return MakeID(1);
  case  MRB_TT_SYMBOL:
    return MakeID(mrb_symbol(obj));
  case  MRB_TT_FIXNUM:
    return MakeID2(float_id((mrb_float)mrb_fixnum(obj)), MRB_TT_FLOAT);
  case  MRB_TT_FLOAT:
    return MakeID(float_id(mrb_float(obj)));
  case  MRB_TT_STRING:
  case  MRB_TT_OBJECT:
  case  MRB_TT_CLASS:
  case  MRB_TT_MODULE:
  case  MRB_TT_ICLASS:
  case  MRB_TT_SCLASS:
  case  MRB_TT_PROC:
  case  MRB_TT_ARRAY:
  case  MRB_TT_HASH:
  case  MRB_TT_RANGE:
  case  MRB_TT_EXCEPTION:
  case  MRB_TT_FILE:
  case  MRB_TT_DATA:
  default:
    return MakeID(mrb_ptr(obj));
  }
}

#ifdef MRB_WORD_BOXING
mrb_value
mrb_float_value(mrb_state *mrb, mrb_float f)
{
  mrb_value v;

  v.value.p = mrb_obj_alloc(mrb, MRB_TT_FLOAT, mrb->float_class);
  v.value.fp->f = f;
  return v;
}

mrb_value
mrb_voidp_value(mrb_state *mrb, void *p)
{
  mrb_value v;

  v.value.p = mrb_obj_alloc(mrb, MRB_TT_VOIDP, mrb->object_class);
  v.value.vp->p = p;
  return v;
}
#endif  /* MRB_WORD_BOXING */

