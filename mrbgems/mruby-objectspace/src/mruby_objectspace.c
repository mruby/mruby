#include <mruby.h>
#include <mruby/gc.h>
#include <mruby/hash.h>
#include <mruby/class.h>

struct os_count_struct {
  mrb_int total;
  mrb_int freed;
  mrb_int counts[MRB_TT_MAXDEFINE+1];
};

static void
os_count_object_type(mrb_state *mrb, struct RBasic *obj, void *data)
{
  struct os_count_struct *obj_count;
  obj_count = (struct os_count_struct*)data;

  if (is_dead(mrb, obj)) {
    obj_count->freed++;
  }
  else {
    obj_count->counts[obj->tt]++;
    obj_count->total++;
  }
}

/*
 *  call-seq:
 *     ObjectSpace.count_objects([result_hash]) -> hash
 *
 *  Counts objects for each type.
 *
 *  It returns a hash, such as:
 *  {
 *    :TOTAL=>10000,
 *    :FREE=>3011,
 *    :MRB_TT_OBJECT=>6,
 *    :MRB_TT_CLASS=>404,
 *    # ...
 *  }
 *
 *  If the optional argument +result_hash+ is given,
 *  it is overwritten and returned. This is intended to avoid probe effect.
 *
 */

static mrb_value
os_count_objects(mrb_state *mrb, mrb_value self)
{
  struct os_count_struct obj_count = { 0 };
  enum mrb_vtype i;
  mrb_value hash;

  if (mrb_get_args(mrb, "|H", &hash) == 0) {
    hash = mrb_hash_new(mrb);
  }

  if (!mrb_test(mrb_hash_empty_p(mrb, hash))) {
    mrb_hash_clear(mrb, hash);
  }

  mrb_objspace_each_objects(mrb, os_count_object_type, &obj_count);

  mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_lit(mrb, "TOTAL")), mrb_fixnum_value(obj_count.total));
  mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_lit(mrb, "FREE")), mrb_fixnum_value(obj_count.freed));

  for (i = MRB_TT_FALSE; i < MRB_TT_MAXDEFINE; i++) {
    mrb_value type;
    switch (i) {
#define COUNT_TYPE(t) case (t): type = mrb_symbol_value(mrb_intern_lit(mrb, #t)); break;
      COUNT_TYPE(MRB_TT_FALSE);
      COUNT_TYPE(MRB_TT_FREE);
      COUNT_TYPE(MRB_TT_TRUE);
      COUNT_TYPE(MRB_TT_FIXNUM);
      COUNT_TYPE(MRB_TT_SYMBOL);
      COUNT_TYPE(MRB_TT_UNDEF);
      COUNT_TYPE(MRB_TT_FLOAT);
      COUNT_TYPE(MRB_TT_CPTR);
      COUNT_TYPE(MRB_TT_OBJECT);
      COUNT_TYPE(MRB_TT_CLASS);
      COUNT_TYPE(MRB_TT_MODULE);
      COUNT_TYPE(MRB_TT_ICLASS);
      COUNT_TYPE(MRB_TT_SCLASS);
      COUNT_TYPE(MRB_TT_PROC);
      COUNT_TYPE(MRB_TT_ARRAY);
      COUNT_TYPE(MRB_TT_HASH);
      COUNT_TYPE(MRB_TT_STRING);
      COUNT_TYPE(MRB_TT_RANGE);
      COUNT_TYPE(MRB_TT_EXCEPTION);
      COUNT_TYPE(MRB_TT_FILE);
      COUNT_TYPE(MRB_TT_ENV);
      COUNT_TYPE(MRB_TT_DATA);
#undef COUNT_TYPE
    default:
      type = mrb_fixnum_value(i); break;
    }
    if (obj_count.counts[i])
      mrb_hash_set(mrb, hash, type, mrb_fixnum_value(obj_count.counts[i]));
  }

  return hash;
}

struct os_each_object_data {
  mrb_value block;
  struct RClass *target_module;
  mrb_int count;
};

static void
os_each_object_cb(mrb_state *mrb, struct RBasic *obj, void *ud)
{
  struct os_each_object_data *d = (struct os_each_object_data*)ud;

  /* filter dead objects */
  if (is_dead(mrb, obj)) {
    return;
  }

  /* filter class kind if target module defined */
  if (d->target_module && !mrb_obj_is_kind_of(mrb, mrb_obj_value(obj), d->target_module)) {
    return;
  }

  mrb_yield(mrb, d->block, mrb_obj_value(obj));
  ++d->count;
}

static mrb_value
os_each_object(mrb_state *mrb, mrb_value self)
{
  mrb_value cls = mrb_nil_value();
  struct os_each_object_data d;
  mrb_get_args(mrb, "&|C", &d.block, &cls);

  if (mrb_nil_p(d.block)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Expected block in ObjectSpace.each_object.");
  }

  d.target_module = mrb_nil_p(cls) ? NULL : mrb_class_ptr(cls);
  d.count = 0;
  mrb_objspace_each_objects(mrb, os_each_object_cb, &d);
  return mrb_fixnum_value(d.count);
}

void
mrb_mruby_objectspace_gem_init(mrb_state *mrb)
{
  struct RClass *os = mrb_define_module(mrb, "ObjectSpace");
  mrb_define_class_method(mrb, os, "count_objects", os_count_objects, MRB_ARGS_OPT(1));
  mrb_define_class_method(mrb, os, "each_object", os_each_object, MRB_ARGS_OPT(1));
}

void
mrb_mruby_objectspace_gem_final(mrb_state *mrb)
{
}
