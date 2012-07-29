/*
** variable.c - mruby variables
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/class.h"
#include "mruby/khash.h"
#include "mruby/variable.h"
#include "error.h"
#include "mruby/array.h"
#include "mruby/proc.h"

#ifdef ENABLE_REGEXP
#include "re.h"
#endif

static void
mark_tbl(mrb_state *mrb, struct kh_iv *h)
{
  khiter_t k;

  if (!h) return;
  for (k = kh_begin(h); k != kh_end(h); k++)
    if (kh_exist(h, k))
      mrb_gc_mark_value(mrb, kh_value(h, k));
}

void
mrb_gc_mark_gv(mrb_state *mrb)
{
  mark_tbl(mrb, mrb->globals);
}

void
mrb_gc_free_gv(mrb_state *mrb)
{
  kh_destroy(iv, mrb->globals);
}

void
mrb_gc_mark_iv(mrb_state *mrb, struct RObject *obj)
{
  mark_tbl(mrb, obj->iv);
}

size_t
mrb_gc_mark_iv_size(mrb_state *mrb, struct RObject *obj)
{
  struct kh_iv *h = obj->iv;

  if (!h) return 0;
  return kh_size(h);
}

void
mrb_gc_free_iv(mrb_state *mrb, struct RObject *obj)
{
  kh_destroy(iv, obj->iv);
}

mrb_value
mrb_vm_special_get(mrb_state *mrb, mrb_sym i)
{
  return mrb_fixnum_value(0);
}

void
mrb_vm_special_set(mrb_state *mrb, mrb_sym i, mrb_value v)
{
}

static mrb_value
ivget(mrb_state *mrb, struct kh_iv *h, mrb_sym sym)
{
  khiter_t k;

  k = kh_get(iv, h, sym);
  if (k != kh_end(h))
    return kh_value(h, k);
  return mrb_nil_value();
}

mrb_value
mrb_obj_iv_get(mrb_state *mrb, struct RObject *obj, mrb_sym sym)
{
  if (!obj->iv) {
    return mrb_nil_value();
  }
  return ivget(mrb, obj->iv, sym);
}

static int
obj_iv_p(mrb_value obj)
{
  switch (mrb_type(obj)) {
    case MRB_TT_OBJECT:
    case MRB_TT_CLASS:
    case MRB_TT_MODULE:
    case MRB_TT_HASH:
    case MRB_TT_DATA:
      return TRUE;
    default:
      return FALSE;
  }
}

mrb_value
mrb_iv_get(mrb_state *mrb, mrb_value obj, mrb_sym sym)
{
  if (obj_iv_p(obj)) {
    return mrb_obj_iv_get(mrb, mrb_obj_ptr(obj), sym);
  }
  return mrb_nil_value();
}

static void
ivset(mrb_state *mrb, struct kh_iv *h, mrb_sym sym, mrb_value v)
{
  khiter_t k;

  k = kh_put(iv, h, sym);
  kh_value(h, k) = v;
}

void
mrb_obj_iv_set(mrb_state *mrb, struct RObject *obj, mrb_sym sym, mrb_value v)
{
  khash_t(iv) *h;

  if (!obj->iv) {
    h = obj->iv = kh_init(iv, mrb);
  }
  else {
    h = obj->iv;
  }
  mrb_write_barrier(mrb, (struct RBasic*)obj);
  ivset(mrb, h, sym, v);
}

void
mrb_iv_set(mrb_state *mrb, mrb_value obj, mrb_sym sym, mrb_value v) /* mrb_ivar_set */
{
  if (obj_iv_p(obj)) {
    mrb_obj_iv_set(mrb, mrb_obj_ptr(obj), sym, v);
  }
}

mrb_value
mrb_iv_remove(mrb_state *mrb, mrb_value obj, mrb_sym sym)
{
  mrb_value val;

  if (obj_iv_p(obj)) {
    khash_t(iv) *h = mrb_obj_ptr(obj)->iv;
    khiter_t k;

    if (h) {
      k = kh_get(iv, h, sym);
      if (k != kh_end(h)) {
	val = kh_value(h, k);
	kh_del(iv, h, k);
	return val;
      }
    }
  }
  return mrb_undef_value();
}

mrb_value
mrb_vm_iv_get(mrb_state *mrb, mrb_sym sym)
{
  /* get self */
  return mrb_iv_get(mrb, mrb->stack[0], sym);
}

void
mrb_vm_iv_set(mrb_state *mrb, mrb_sym sym, mrb_value v)
{
  /* get self */
  mrb_iv_set(mrb, mrb->stack[0], sym, v);
}

/* 15.3.1.3.23 */
/*
 *  call-seq:
 *     obj.instance_variables    -> array
 *
 *  Returns an array of instance variable names for the receiver. Note
 *  that simply defining an accessor does not create the corresponding
 *  instance variable.
 *
 *     class Fred
 *       attr_accessor :a1
 *       def initialize
 *         @iv = 3
 *       end
 *     end
 *     Fred.new.instance_variables   #=> [:@iv]
 */
mrb_value
mrb_obj_instance_variables(mrb_state *mrb, mrb_value self)
{
  mrb_value ary;
  kh_iv_t *h;
  khint_t i;
  int len;
  const char* p;

  ary = mrb_ary_new(mrb);
  if (obj_iv_p(self)) {
    h = ROBJECT_IVPTR(self);
    if (h) {
      for (i=0;i<kh_end(h);i++) {
        if (kh_exist(h, i)) {
          p = mrb_sym2name_len(mrb, kh_key(h,i), &len);
          if (len > 1 && *p == '@') {
            if (mrb_type(kh_value(h, i)) != MRB_TT_UNDEF)
              mrb_ary_push(mrb, ary, mrb_str_new(mrb, p, len));
          }
        }
      }
    }
  }
  return ary;
}

mrb_value
mrb_vm_cv_get(mrb_state *mrb, mrb_sym sym)
{
  struct RClass *c = mrb->ci->target_class;

  while (c) {
    if (c->iv) {
      khash_t(iv) *h = c->iv;
      khiter_t k = kh_get(iv, h, sym);

      if (k != kh_end(h))
        return kh_value(h, k);
    }
    c = c->super;
  }
  return mrb_nil_value();
}

void
mrb_vm_cv_set(mrb_state *mrb, mrb_sym sym, mrb_value v)
{
  struct RClass *c = mrb->ci->target_class;
  khash_t(iv) *h;
  khiter_t k;

  while (c) {
    if (c->iv) {
      h = c->iv;
      k = kh_get(iv, h, sym);
      if (k != kh_end(h)) {
        k = kh_put(iv, h, sym);
        kh_value(h, k) = v;
        return;
      }
    }
    c = c->super;
  }
  c = mrb->ci->target_class;
  h = c->iv;
  if (!h) {
    c->iv = h = kh_init(iv, mrb);
  }
  k = kh_put(iv, h, sym);
  kh_value(h, k) = v;
}

int
mrb_const_defined(mrb_state *mrb, mrb_value mod, mrb_sym sym)
{
  khiter_t k;
  struct RClass *m = mrb_class_ptr(mod);
  struct kh_iv *h = m->iv;

  if (!h) return 0;
  k = kh_get(iv, h, sym);
  if (k != kh_end(h))
    return 1;
  return 0;
}

static void
mod_const_check(mrb_state *mrb, mrb_value mod)
{
  switch (mod.tt) {
  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
    break;
  default:
    mrb_raise(mrb, E_TYPE_ERROR, "constant look-up for non class/module");
    break;
  }
}

static mrb_value
const_get(mrb_state *mrb, struct RClass *base, mrb_sym sym)
{
  struct RClass *c = base;
  khash_t(iv) *h;
  khiter_t k;
  mrb_sym cm = mrb_intern(mrb, "const_missing");

 L_RETRY:
  while (c) {
    if (c->iv) {
      h = c->iv;
      k = kh_get(iv, h, sym);
      if (k != kh_end(h)) {
        return kh_value(h, k);
      }
      if (mrb_respond_to(mrb, mrb_obj_value(c), cm)) {
        mrb_value argv = mrb_symbol_value(sym);
        return mrb_funcall_argv(mrb, mrb_obj_value(c), "const_missing", 1, &argv);
      }
    }
    c = c->super;
  }

  if (base->tt == MRB_TT_MODULE) {
    c = base = mrb->object_class;
    goto L_RETRY;
  }
  mrb_raise(mrb, E_NAME_ERROR, "uninitialized constant %s",
            mrb_sym2name(mrb, sym));
  /* not reached */
  return mrb_nil_value();
}

mrb_value
mrb_const_get(mrb_state *mrb, mrb_value mod, mrb_sym sym)
{
  mod_const_check(mrb, mod);
  return const_get(mrb, mrb_class_ptr(mod), sym);
}

mrb_value
mrb_vm_const_get(mrb_state *mrb, mrb_sym sym)
{
  struct RClass *c = mrb->ci->proc->target_class;

  if (!c) c = mrb->ci->target_class;
  return const_get(mrb, c, sym);
}

void
mrb_const_set(mrb_state *mrb, mrb_value mod, mrb_sym sym, mrb_value v)
{
  mod_const_check(mrb, mod);
  mrb_iv_set(mrb, mod, sym, v);
}

void
mrb_vm_const_set(mrb_state *mrb, mrb_sym sym, mrb_value v)
{
  struct RClass *c = mrb->ci->proc->target_class;

  if (!c) c = mrb->ci->target_class;
  mrb_obj_iv_set(mrb, (struct RObject*)c, sym, v);
}

void
mrb_define_const(mrb_state *mrb, struct RClass *mod, const char *name, mrb_value v)
{
  mrb_obj_iv_set(mrb, (struct RObject*)mod, mrb_intern(mrb, name), v);
}

void
mrb_define_global_const(mrb_state *mrb, const char *name, mrb_value val)
{
  mrb_define_const(mrb, mrb->object_class, name, val);
}

mrb_value
mrb_gv_get(mrb_state *mrb, mrb_sym sym)
{
  if (!mrb->globals) {
    return mrb_nil_value();
  }
  return ivget(mrb, mrb->globals, sym);
}

void
mrb_gv_set(mrb_state *mrb, mrb_sym sym, mrb_value v)
{
  khash_t(iv) *h;

  if (!mrb->globals) {
    h = mrb->globals = kh_init(iv, mrb);
  }
  else {
    h = mrb->globals;
  }
  ivset(mrb, h, sym, v);
}

/* 15.3.1.2.4  */
/* 15.3.1.3.14 */
/*
 *  call-seq:
 *     global_variables    -> array
 *
 *  Returns an array of the names of global variables.
 *
 *     global_variables.grep /std/   #=> [:$stdin, :$stdout, :$stderr]
 */
mrb_value
mrb_f_global_variables(mrb_state *mrb, mrb_value self)
{
  char buf[3];
  khint_t i;
  struct kh_iv *h = mrb->globals;
  mrb_value ary = mrb_ary_new(mrb);

  if (h) {
    for (i=0;i < kh_end(h);i++) {
      if (kh_exist(h, i)) {
        mrb_ary_push(mrb, ary, mrb_symbol_value(kh_key(h,i)));
      }
    }
  }
  buf[0] = '$';
  buf[2] = 0;
  for (i = 1; i <= 9; ++i) {
      buf[1] = (char)(i + '0');
      mrb_ary_push(mrb, ary, mrb_symbol_value(mrb_intern(mrb, buf)));
  }
  return ary;
}

int
mrb_st_lookup(struct kh_iv *table, mrb_sym id, khiter_t *value)
{
  khash_t(iv) *h;
  khiter_t k;

  if (table) {
    h = (khash_t(iv)*)table;
    k = kh_get(iv, h, id);
    if (k != kh_end(h)) {
      if (value != 0)  *value = k;//kh_value(h, k);
      return 1;/* TRUE */
    }
    return 0;/* FALSE */
  }
  else {
    return 0;/* FALSE */
  }
}

static int
kiv_lookup(khash_t(iv)* table, mrb_sym key, mrb_value *value)
{
  khash_t(iv) *h=table;
  khiter_t k;

  k = kh_get(iv, h, key);
  if (k != kh_end(h)) {
    *value = kh_value(h, k);
    return 1;
  }
  return 0;
}

static int
mrb_const_defined_0(mrb_state *mrb, struct RClass *klass, mrb_sym id, int exclude, int recurse)
{
  mrb_value value;
  struct RClass * tmp;
  int mod_retry = 0;

  tmp = klass;
retry:
  while (tmp) {
    if (tmp->iv && kiv_lookup(tmp->iv, id, &value)) {
      return (int)1/*Qtrue*/;
    }
    if (!recurse && (klass != mrb->object_class)) break;
    tmp = tmp->super;
  }
  if (!exclude && !mod_retry && (klass->tt == MRB_TT_MODULE)) {
    mod_retry = 1;
    tmp = mrb->object_class;
    goto retry;
  }
  return (int)0/*Qfalse*/;
}

int
mrb_const_defined_at(mrb_state *mrb, struct RClass *klass, mrb_sym id)
{
  return mrb_const_defined_0(mrb, klass, id, TRUE, FALSE);
}

mrb_value
mrb_attr_get(mrb_state *mrb, mrb_value obj, mrb_sym id)
{
  return mrb_iv_get(mrb, obj, id);
}

struct RClass *
mrb_class_obj_get(mrb_state *mrb, const char *name)
{
  mrb_value mod = mrb_obj_value(mrb->object_class);
  mrb_sym sym = mrb_intern(mrb, name);

  return mrb_class_ptr(mrb_const_get(mrb, mod, sym));
}

