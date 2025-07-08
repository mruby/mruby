/*
** class.c - Class class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/hash.h>
#include <mruby/class.h>
#include <mruby/numeric.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/data.h>
#include <mruby/istruct.h>
#include <mruby/opcode.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#define METHOD_MID(m) MT_KEY_SYM((m).flags)

union mt_ptr {
  const struct RProc *proc;
  mrb_func_t func;
};

#define MT_KEY_SHIFT 4
#define MT_KEY_MASK  ((1<<MT_KEY_SHIFT)-1)
#define MT_KEY_P(k) (((k)>>MT_KEY_SHIFT) != 0)
#define MT_FUNC MRB_METHOD_FUNC_FL
#define MT_NOARG MRB_METHOD_NOARG_FL
#define MT_PUBLIC MRB_METHOD_PUBLIC_FL
#define MT_PRIVATE MRB_METHOD_PRIVATE_FL
#define MT_PROTECTED MRB_METHOD_PROTECTED_FL
#define MT_VDEFAULT MRB_METHOD_VDEFAULT_FL
#define MT_VMASK MRB_METHOD_VISIBILITY_MASK
#define MT_EMPTY 0
#define MT_DELETED 1

#define MT_KEY(sym, flags) ((sym)<<MT_KEY_SHIFT|(flags))
#define MT_KEY_SYM(k) ((k)>>MT_KEY_SHIFT)
#define MT_KEY_FLG(k) ((k)&MT_KEY_MASK)

/* method table structure */
typedef struct mt_tbl {
  int             size;  /* # of used entries */
  int             alloc; /* capacity */
  union mt_ptr   *ptr;   /* block: [ ptr[0...alloc] | keys[0...alloc] ] */
} mt_tbl;

/* helper to get keys array */
static inline mrb_sym*
mt_keys(mt_tbl *t) {
  return (mrb_sym*)&t->ptr[t->alloc];
}

static union mt_ptr*
mt_vals(mt_tbl *t) {
  return t->ptr;
}

/* allocate or grow the block to exactly alloc entries */
static void
mt_grow(mrb_state *mrb, mt_tbl *t, int new_alloc)
{
  int old_alloc = t->alloc;
  size_t new_block = new_alloc * (sizeof(union mt_ptr) + sizeof(mrb_sym));

  t->ptr = (union mt_ptr*)mrb_realloc(mrb, t->ptr, new_block);
  if (old_alloc > 0) {
    /* keys used to live at &ptr[old_alloc] */
    mrb_sym *old_keys = (mrb_sym*)&t->ptr[old_alloc];
    /* keys must now live at &ptr[new_alloc] */
    mrb_sym *new_keys = (mrb_sym*)&t->ptr[new_alloc];

    /* move the old key array up to its new position */
    memmove(new_keys, old_keys, old_alloc * sizeof(mrb_sym));
  }
  t->alloc = new_alloc;
}

/* Creates the method table. */
static mt_tbl*
mt_new(mrb_state *mrb)
{
  mt_tbl *t;

  t = (mt_tbl*)mrb_malloc(mrb, sizeof(mt_tbl));
  t->size = 0;
  t->alloc = 0;
  t->ptr = NULL;

  return t;
}

/* Branch-free binary search helper: returns the index where `target` should be inserted/found. */
static inline int
bsearch_idx(mrb_sym *keys, int size, mrb_sym target)
{
  if (size == 0) return 0;
  int n = size;
  mrb_sym *p = keys;
  /* While more than one element remains, halve the range each iteration */
  while (n > 1) {
    int half = n >> 1;
    MRB_MEM_PREFETCH(p + (half >> 1));
    MRB_MEM_PREFETCH(p + half + (half >> 1));
    mrb_sym mid_sym = MT_KEY_SYM(p[half]);
    /*
     * Update pointer p without a branch:
     * If mid_sym < target, move p forward by half; otherwise keep p unchanged.
     * Compiler will emit a CMOV or equivalent.
     */
    p = (mid_sym < target) ? p + half : p;
    n -= half;
  }
  /* Final adjustment: if the remaining element is still less than target, advance by one */
  int offset = (MT_KEY_SYM(*p) < target);
  return (int)(p - keys) + offset;
}

/* Insert or update an entry in the method table using branch-free binary search */
static void
mt_put(mrb_state *mrb, mt_tbl *t, mrb_sym sym, mrb_sym flags, union mt_ptr ptrval)
{
  mrb_sym key = MT_KEY(sym, flags);

  /* Ensure there is capacity */
  if (t->alloc == 0) {
    mt_grow(mrb, t, 8);
  }
  else if (t->size == t->alloc) {
    mt_grow(mrb, t, t->alloc * 2);
  }

  mrb_sym      *keys = mt_keys(t);
  union mt_ptr *vals = mt_vals(t);

  /*
   * If table is empty, insertion index is 0.
   * Otherwise, find the insertion/update position branch-free.
   */
  int lo = bsearch_idx(keys, t->size, sym);

  /* If the key already exists, update its value and return */
  if (lo < t->size && MT_KEY_SYM(keys[lo]) == sym) {
    keys[lo] = key;
    vals[lo] = ptrval;
    return;
  }

  /* Shift existing entries to make room at index lo */
  if (t->size > lo) {
    int move_count = t->size - lo;
    memmove(&vals[lo+1], &vals[lo], move_count * sizeof(union mt_ptr));
    memmove(&keys[lo+1], &keys[lo], move_count * sizeof(mrb_sym));
  }

  /* Insert the new key and value */
  keys[lo] = key;
  vals[lo] = ptrval;
  t->size++;
}

/* Retrieve a value from the method table using branch-free binary search */
static mrb_sym
mt_get(mrb_state *mrb, mt_tbl *t, mrb_sym sym, union mt_ptr *pp)
{
  /* Return 0 if table is empty or null */
  if (!t || t->size == 0) return 0;

  mrb_sym      *keys = mt_keys(t);
  union mt_ptr *vals = mt_vals(t);

  /* Find the position in a branch-free manner */
  int lo = bsearch_idx(keys, t->size, sym);

  /* If found, set *pp to the value and return the full key */
  if (lo < t->size && MT_KEY_SYM(keys[lo]) == sym) {
    *pp = vals[lo];
    return keys[lo];
  }

  /* Not found */
  return 0;
}

/* Deletes the entry for `sym` from the method table using branch-free search. */
static mrb_bool
mt_del(mrb_state *mrb, mt_tbl *t, mrb_sym sym)
{
  /* Return FALSE if table is null or empty */
  if (!t || t->size == 0) return FALSE;

  mrb_sym      *keys = mt_keys(t);
  union mt_ptr *vals = mt_vals(t);

  /* Find the index of `sym` in a branch-free manner */
  int lo = bsearch_idx(keys, t->size, sym);

  /* If the key exists at index lo, remove it by shifting left */
  if (lo < t->size && MT_KEY_SYM(keys[lo]) == sym) {
    int move_count = t->size - lo - 1;
    /* shift left to remove entry */
    memmove(&vals[lo],     &vals[lo + 1], move_count * sizeof(union mt_ptr));
    memmove(&keys[lo],     &keys[lo + 1], move_count * sizeof(mrb_sym));
    t->size--;
    return TRUE;
  }

  /* Key not found */
  return FALSE;
}

/* Copy the method table. */
static mt_tbl*
mt_copy(mrb_state *mrb, mt_tbl *t)
{
  if (!t || t->size == 0) return NULL;
  mt_tbl *t2 = mt_new(mrb);
  mt_grow(mrb, t2, t->size);
  /* copy used entries */
  memcpy(mt_vals(t2), mt_vals(t), t->size * sizeof(union mt_ptr));
  memcpy(mt_keys(t2), mt_keys(t), t->size * sizeof(mrb_sym));
  t2->size = t->size;
  return t2;
}

/* Free memory of the method table. */
static void
mt_free(mrb_state *mrb, mt_tbl *t)
{
  mrb_free(mrb, t->ptr);
  mrb_free(mrb, t);
}

static inline mrb_method_t
create_method_value(mrb_state *mrb, mrb_sym key, union mt_ptr val)
{
  mrb_method_t m = { key, { val.proc } };
  return m;
}

/*
 * Iterates over the methods in a class's method table.
 *
 * @param mrb The mruby state.
 * @param c The class whose method table is to be iterated.
 * @param fn The callback function to be called for each method.
 *   The function receives the mruby state, the method symbol, the method itself, and user data.
 *   It should return 0 to continue iteration, or a non-zero value to stop.
 * @param p User data to be passed to the callback function.
 */
MRB_API void
mrb_mt_foreach(mrb_state *mrb, struct RClass *c, mrb_mt_foreach_func *fn, void *p)
{
  mt_tbl *t = c->mt;
  if (!t || t->size == 0) return;

  for (int i=0; i<t->size; i++) {
    union mt_ptr *vals = mt_vals(t);
    mrb_sym      *keys = mt_keys(t);
    mrb_sym key = keys[i];
    if (fn(mrb, MT_KEY_SYM(key), create_method_value(mrb, key, vals[i]), p) != 0) {
      return;
    }
  }
}

size_t
mrb_gc_mark_mt(mrb_state *mrb, struct RClass *c)
{
  mt_tbl *t = c->mt;

  if (!t || t->size == 0) return 0;

  mrb_sym *keys = mt_keys(t);
  union mt_ptr *vals = mt_vals(t);
  for (int i=0; i<t->size; i++) {
    if (MT_KEY_P(keys[i]) && (keys[i] & MT_FUNC) == 0) { /* Proc pointer */
      const struct RProc *p = vals[i].proc;
      mrb_gc_mark(mrb, (struct RBasic*)p);
    }
  }
  if (!t) return 0;
  return (size_t)t->size;
}

size_t
mrb_class_mt_memsize(mrb_state *mrb, struct RClass *c)
{
  struct mt_tbl *h = c->mt;

  if (!h) return 0;
  return sizeof(struct mt_tbl) + (size_t)h->size * sizeof(mrb_method_t);
}

void
mrb_gc_free_mt(mrb_state *mrb, struct RClass *c)
{
  if (c->mt) mt_free(mrb, c->mt);
}

void
mrb_class_name_class(mrb_state *mrb, struct RClass *outer, struct RClass *c, mrb_sym id)
{
  mrb_value name;
  mrb_sym nsym = MRB_SYM(__classname__);

  if (mrb_obj_iv_defined(mrb, (struct RObject*)c, nsym)) return;
  if (outer == NULL || outer == mrb->object_class) {
    name = mrb_symbol_value(id);
  }
  else {
    name = mrb_class_path(mrb, outer);
    if (mrb_nil_p(name)) {      /* unnamed outer class */
      if (outer != mrb->object_class && outer != c) {
        mrb_obj_iv_set_force(mrb, (struct RObject*)c, MRB_SYM(__outer__),
                             mrb_obj_value(outer));
      }
      return;
    }
    else {
      mrb_int len;
      const char *n = mrb_sym_name_len(mrb, id, &len);

      mrb_str_cat_lit(mrb, name, "::");
      mrb_str_cat(mrb, name, n, len);
    }
  }
  mrb_obj_iv_set_force(mrb, (struct RObject*)c, nsym, name);
}

mrb_bool
mrb_const_name_p(mrb_state *mrb, const char *name, mrb_int len)
{
  return len > 0 && ISUPPER(name[0]) && mrb_ident_p(name+1, len-1);
}

static void
setup_class(mrb_state *mrb, struct RClass *outer, struct RClass *c, mrb_sym id)
{
  mrb_const_set(mrb, mrb_obj_value(outer), id, mrb_obj_value(c));
}

#define make_metaclass(mrb, c) prepare_singleton_class((mrb), (struct RBasic*)(c))

static void
prepare_singleton_class(mrb_state *mrb, struct RBasic *o)
{
  struct RClass *c;

  mrb_assert(o->c);
  if (o->c->tt == MRB_TT_SCLASS) return;
  struct RClass *sc = MRB_OBJ_ALLOC(mrb, MRB_TT_SCLASS, mrb->class_class);
  sc->flags |= MRB_FL_CLASS_IS_INHERITED;
  sc->mt = NULL;
  sc->iv = NULL;
  if (o->tt == MRB_TT_CLASS) {
    c = (struct RClass*)o;
    if (!c->super) {
      sc->super = mrb->class_class;
    }
    else {
      sc->super = c->super->c;
    }
  }
  else if (o->tt == MRB_TT_SCLASS) {
    c = (struct RClass*)o;
    while (c->super->tt == MRB_TT_ICLASS)
      c = c->super;
    make_metaclass(mrb, c->super);
    sc->super = c->super->c;
  }
  else {
    sc->super = o->c;
    prepare_singleton_class(mrb, (struct RBasic*)sc);
  }
  o->c = sc;
  mrb_field_write_barrier(mrb, (struct RBasic*)o, (struct RBasic*)sc);
  mrb_obj_iv_set(mrb, (struct RObject*)sc, MRB_SYM(__attached__), mrb_obj_value(o));
  sc->frozen = o->frozen;
}

static mrb_value
class_name_str(mrb_state *mrb, struct RClass* c)
{
  mrb_value path = mrb_class_path(mrb, c);
  if (mrb_nil_p(path)) {
    path = c->tt == MRB_TT_MODULE ? mrb_str_new_lit(mrb, "#<Module:") :
                                    mrb_str_new_lit(mrb, "#<Class:");
    mrb_str_cat_str(mrb, path, mrb_ptr_to_str(mrb, c));
    mrb_str_cat_lit(mrb, path, ">");
  }
  return path;
}

static struct RClass*
class_from_sym(mrb_state *mrb, struct RClass *klass, mrb_sym id)
{
  mrb_value c = mrb_const_get(mrb, mrb_obj_value(klass), id);

  mrb_check_type(mrb, c, MRB_TT_CLASS);
  return mrb_class_ptr(c);
}

static struct RClass*
module_from_sym(mrb_state *mrb, struct RClass *klass, mrb_sym id)
{
  mrb_value c = mrb_const_get(mrb, mrb_obj_value(klass), id);

  mrb_check_type(mrb, c, MRB_TT_MODULE);
  return mrb_class_ptr(c);
}

static mrb_bool
class_ptr_p(mrb_value obj)
{
  switch (mrb_type(obj)) {
  case MRB_TT_CLASS:
  case MRB_TT_SCLASS:
  case MRB_TT_MODULE:
    return TRUE;
  default:
    return FALSE;
  }
}

static void
check_if_class_or_module(mrb_state *mrb, mrb_value obj)
{
  if (!class_ptr_p(obj)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "%!v is not a class/module", obj);
  }
}

static struct RClass*
define_module(mrb_state *mrb, mrb_sym name, struct RClass *outer)
{
  if (mrb_const_defined_at(mrb, mrb_obj_value(outer), name)) {
    return module_from_sym(mrb, outer, name);
  }
  struct RClass *m = mrb_module_new(mrb);
  setup_class(mrb, outer, m, name);

  return m;
}

/*
 * Defines a new module in the top-level scope (Object) using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param name The symbol representing the name of the module to define.
 * @return A pointer to the newly defined or existing RClass structure for the module.
 * @sideeffect Creates a new module or returns an existing one if already defined.
 *             The module is set as a constant in Object.
 */
MRB_API struct RClass*
mrb_define_module_id(mrb_state *mrb, mrb_sym name)
{
  return define_module(mrb, name, mrb->object_class);
}

/*
 * Defines a new module in the top-level scope (Object).
 *
 * @param mrb The mruby state.
 * @param name The name of the module to define.
 * @return A pointer to the newly defined or existing RClass structure for the module.
 * @sideeffect Creates a new module or returns an existing one if already defined.
 *             The module is set as a constant in Object.
 */
MRB_API struct RClass*
mrb_define_module(mrb_state *mrb, const char *name)
{
  return define_module(mrb, mrb_intern_cstr(mrb, name), mrb->object_class);
}

struct RClass*
mrb_vm_define_module(mrb_state *mrb, mrb_value outer, mrb_sym id)
{
  check_if_class_or_module(mrb, outer);
  if (mrb_const_defined_at(mrb, outer, id)) {
    mrb_value old = mrb_const_get(mrb, outer, id);

    if (!mrb_module_p(old)) {
      mrb_raisef(mrb, E_TYPE_ERROR, "%!v is not a module", old);
    }
    return mrb_class_ptr(old);
  }
  return define_module(mrb, id, mrb_class_ptr(outer));
}

/*
 * Defines a new module under the given outer module/class using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 * @param name The symbol representing the name of the module to define.
 * @return A pointer to the newly defined or existing RClass structure for the module.
 * @sideeffect Creates a new module or returns an existing one if already defined under `outer`.
 *             The module is set as a constant in `outer`.
 */
MRB_API struct RClass*
mrb_define_module_under_id(mrb_state *mrb, struct RClass *outer, mrb_sym name)
{
  struct RClass * c = define_module(mrb, name, outer);

  setup_class(mrb, outer, c, name);
  return c;
}

/*
 * Defines a new module under the given outer module/class using a C string for the name.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 * @param name The C string representing the name of the module to define.
 * @return A pointer to the newly defined or existing RClass structure for the module.
 * @sideeffect Creates a new module or returns an existing one if already defined under `outer`.
 *             The module is set as a constant in `outer`.
 */
MRB_API struct RClass*
mrb_define_module_under(mrb_state *mrb, struct RClass *outer, const char *name)
{
  mrb_sym id = mrb_intern_cstr(mrb, name);
  struct RClass * c = define_module(mrb, id, outer);

  setup_class(mrb, outer, c, id);
  return c;
}

static struct RClass*
find_origin(struct RClass *c)
{
  MRB_CLASS_ORIGIN(c);
  return c;
}

static struct RClass*
define_class(mrb_state *mrb, mrb_sym name, struct RClass *super, struct RClass *outer)
{
  struct RClass * c;

  if (mrb_const_defined_at(mrb, mrb_obj_value(outer), name)) {
    c = class_from_sym(mrb, outer, name);
    MRB_CLASS_ORIGIN(c);
    if (super && mrb_class_real(c->super) != super) {
      mrb_raisef(mrb, E_TYPE_ERROR, "superclass mismatch for Class %n (%C not %C)",
                 name, c->super, super);
    }
    return c;
  }

  c = mrb_class_new(mrb, super);
  setup_class(mrb, outer, c, name);

  return c;
}

/*
 * Defines a new class in the top-level scope (Object) using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param name The symbol representing the name of the class to define.
 * @param super A pointer to the RClass structure of the superclass.
 *              If NULL, Object is assumed as the superclass, and a warning is issued.
 * @return A pointer to the newly defined or existing RClass structure for the class.
 * @sideeffect Creates a new class or returns an existing one if already defined.
 *             The class is set as a constant in Object.
 *             Issues a warning if `super` is NULL.
 */
MRB_API struct RClass*
mrb_define_class_id(mrb_state *mrb, mrb_sym name, struct RClass *super)
{
  if (!super) {
    mrb_warn(mrb, "no super class for '%n', Object assumed", name);
  }
  return define_class(mrb, name, super, mrb->object_class);
}

/*
 * Defines a new class in the top-level scope (Object).
 *
 * @param mrb The mruby state.
 * @param name The name of the class to define.
 * @param super A pointer to the RClass structure of the superclass.
 *              If NULL, Object is assumed as the superclass.
 * @return A pointer to the newly defined or existing RClass structure for the class.
 * @sideeffect Creates a new class or returns an existing one if already defined.
 *             The class is set as a constant in Object.
 */
MRB_API struct RClass*
mrb_define_class(mrb_state *mrb, const char *name, struct RClass *super)
{
  return mrb_define_class_id(mrb, mrb_intern_cstr(mrb, name), super);
}

static mrb_value mrb_do_nothing(mrb_state *mrb, mrb_value);
#ifndef MRB_NO_METHOD_CACHE
static void mc_clear_by_id(mrb_state *mrb, mrb_sym mid);
#else
#define mc_clear(mrb)
#define mc_clear_by_id(mrb,mid)
#endif

static void
mrb_class_inherited(mrb_state *mrb, struct RClass *super, struct RClass *klass)
{

  if (!super)
    super = mrb->object_class;
  super->flags |= MRB_FL_CLASS_IS_INHERITED;

  mrb_value s = mrb_obj_value(super);
  mrb_sym mid = MRB_SYM(inherited);

  if (!mrb_func_basic_p(mrb, s, mid, mrb_do_nothing)) {
    mrb_value c = mrb_obj_value(klass);
    mrb_funcall_argv(mrb, s, mid, 1, &c);
  }
}

struct RClass*
mrb_vm_define_class(mrb_state *mrb, mrb_value outer, mrb_value super, mrb_sym id)
{
  struct RClass *s;
  struct RClass *c;

  if (!mrb_nil_p(super)) {
    if (!mrb_class_p(super)) {
      mrb_raisef(mrb, E_TYPE_ERROR, "superclass must be a Class (%!v given)", super);
    }
    s = mrb_class_ptr(super);
  }
  else {
    s = NULL;
  }
  check_if_class_or_module(mrb, outer);
  if (mrb_const_defined_at(mrb, outer, id)) {
    mrb_value old = mrb_const_get(mrb, outer, id);

    if (!mrb_class_p(old)) {
      mrb_raisef(mrb, E_TYPE_ERROR, "%!v is not a class", old);
    }
    c = mrb_class_ptr(old);
    if (s) {
      /* check super class */
      if (mrb_class_real(c->super) != s) {
        mrb_raisef(mrb, E_TYPE_ERROR, "superclass mismatch for %v", old);
      }
    }
    return c;
  }
  c = define_class(mrb, id, s, mrb_class_ptr(outer));
  mrb_class_inherited(mrb, mrb_class_real(c->super), c);

  return c;
}

/*
 * Checks if a class is defined in the top-level scope (Object).
 *
 * @param mrb The mruby state.
 * @param name The name of the class to check.
 * @return TRUE if the class is defined, FALSE otherwise.
 *         Returns FALSE if the name is not a valid symbol.
 */
MRB_API mrb_bool
mrb_class_defined(mrb_state *mrb, const char *name)
{
  mrb_sym sym = mrb_intern_check_cstr(mrb, name);
  if (!sym) return FALSE;
  return mrb_const_defined(mrb, mrb_obj_value(mrb->object_class), sym);
}

/*
 * Checks if a class is defined in the top-level scope (Object) using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param name The symbol representing the name of the class to check.
 * @return TRUE if the class is defined, FALSE otherwise.
 */
MRB_API mrb_bool
mrb_class_defined_id(mrb_state *mrb, mrb_sym name)
{
  return mrb_const_defined(mrb, mrb_obj_value(mrb->object_class), name);
}

/*
 * Checks if a class is defined under the given outer module/class.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 * @param name The name of the class to check.
 * @return TRUE if the class is defined under `outer`, FALSE otherwise.
 *         Returns FALSE if the name is not a valid symbol.
 */
MRB_API mrb_bool
mrb_class_defined_under(mrb_state *mrb, struct RClass *outer, const char *name)
{
  mrb_sym sym = mrb_intern_check_cstr(mrb, name);
  if (!sym) return FALSE;
  return mrb_const_defined_at(mrb, mrb_obj_value(outer), sym);
}

/*
 * Checks if a class is defined under the given outer module/class using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 * @param name The symbol representing the name of the class to check.
 * @return TRUE if the class is defined under `outer`, FALSE otherwise.
 */
MRB_API mrb_bool
mrb_class_defined_under_id(mrb_state *mrb, struct RClass *outer, mrb_sym name)
{
  return mrb_const_defined_at(mrb, mrb_obj_value(outer), name);
}

/*
 * Retrieves a class defined under an outer module/class.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 *              If NULL, Object is assumed.
 * @param name The name of the class to retrieve.
 * @return A pointer to the RClass structure of the found class.
 * @raise TypeError if the constant found is not a class.
 * @raise NameError if the constant is not found.
 */
MRB_API struct RClass*
mrb_class_get_under(mrb_state *mrb, struct RClass *outer, const char *name)
{
  return class_from_sym(mrb, outer, mrb_intern_cstr(mrb, name));
}

/*
 * Retrieves a class defined under an outer module/class using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 *              If NULL, Object is assumed.
 * @param name The symbol representing the name of the class to retrieve.
 * @return A pointer to the RClass structure of the found class.
 * @raise TypeError if the constant found is not a class.
 * @raise NameError if the constant is not found.
 */
MRB_API struct RClass*
mrb_class_get_under_id(mrb_state *mrb, struct RClass *outer, mrb_sym name)
{
  return class_from_sym(mrb, outer, name);
}

/*
 * Retrieves a class defined in the top-level scope (Object).
 *
 * @param mrb The mruby state.
 * @param name The name of the class to retrieve.
 * @return A pointer to the RClass structure of the found class.
 * @raise TypeError if the constant found is not a class.
 * @raise NameError if the constant is not found.
 */
MRB_API struct RClass*
mrb_class_get(mrb_state *mrb, const char *name)
{
  return mrb_class_get_under(mrb, mrb->object_class, name);
}

/*
 * Retrieves a class defined in the top-level scope (Object) using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param name The symbol representing the name of the class to retrieve.
 * @return A pointer to the RClass structure of the found class.
 * @raise TypeError if the constant found is not a class.
 * @raise NameError if the constant is not found.
 */
MRB_API struct RClass*
mrb_class_get_id(mrb_state *mrb, mrb_sym name)
{
  return mrb_class_get_under_id(mrb, mrb->object_class, name);
}

/*
 * Retrieves an exception class by its symbol name.
 * This function specifically searches for exception classes.
 *
 * @param mrb The mruby state.
 * @param name The symbol representing the name of the exception class.
 * @return A pointer to the RClass structure of the found exception class.
 * @raise TypeError if the constant found is not a class.
 * @raise NameError if the constant is not found.
 * @raise Exception if the found class is not an exception (does not inherit from E_EXCEPTION).
 * @raise Exception if the exception system is corrupted.
 */
MRB_API struct RClass*
mrb_exc_get_id(mrb_state *mrb, mrb_sym name)
{
  mrb_value c = mrb_exc_const_get(mrb, name);

  if (!mrb_class_p(c)) {
    mrb_raise(mrb, E_EXCEPTION, "exception corrupted");
  }

  struct RClass *exc = mrb_class_ptr(c);
  for (struct RClass *e = exc; e; e = e->super) {
    if (e == E_EXCEPTION)
      return exc;
  }
  mrb_raise(mrb, E_EXCEPTION, "non-exception raised");
  /* not reached */
  return NULL;
}

/*
 * Retrieves a module defined under an outer module/class.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 * @param name The name of the module to retrieve.
 * @return A pointer to the RClass structure of the found module.
 * @raise TypeError if the constant found is not a module.
 * @raise NameError if the constant is not found.
 */
MRB_API struct RClass*
mrb_module_get_under(mrb_state *mrb, struct RClass *outer, const char *name)
{
  return module_from_sym(mrb, outer, mrb_intern_cstr(mrb, name));
}

/*
 * Retrieves a module defined under an outer module/class using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 * @param name The symbol representing the name of the module to retrieve.
 * @return A pointer to the RClass structure of the found module.
 * @raise TypeError if the constant found is not a module.
 * @raise NameError if the constant is not found.
 */
MRB_API struct RClass*
mrb_module_get_under_id(mrb_state *mrb, struct RClass *outer, mrb_sym name)
{
  return module_from_sym(mrb, outer, name);
}

/*
 * Retrieves a module defined in the top-level scope (Object).
 *
 * @param mrb The mruby state.
 * @param name The name of the module to retrieve.
 * @return A pointer to the RClass structure of the found module.
 * @raise TypeError if the constant found is not a module.
 * @raise NameError if the constant is not found.
 */
MRB_API struct RClass*
mrb_module_get(mrb_state *mrb, const char *name)
{
  return mrb_module_get_under(mrb, mrb->object_class, name);
}

/*
 * Retrieves a module defined in the top-level scope (Object) using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param name The symbol representing the name of the module to retrieve.
 * @return A pointer to the RClass structure of the found module.
 * @raise TypeError if the constant found is not a module.
 * @raise NameError if the constant is not found.
 */
MRB_API struct RClass*
mrb_module_get_id(mrb_state *mrb, mrb_sym name)
{
  return mrb_module_get_under_id(mrb, mrb->object_class, name);
}

/*
 * Defines a class under the namespace of outer.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 * @param name The symbol representing the name of the class to define.
 * @param super A pointer to the RClass structure of the superclass.
 *              If NULL, Object is assumed as the superclass.
 * @return A pointer to the newly defined or existing RClass structure for the class.
 * @raise TypeError if a constant with the same name exists but is not a class.
 * @raise NameError if the class is already defined but with a different superclass.
 * @sideeffect Creates a new class or returns an existing one if compatible.
 *             The class is set as a constant in `outer`.
 *             If a class with the same name is already defined and its superclass
 *             matches `super`, the existing class is returned.
 */
MRB_API struct RClass*
mrb_define_class_under_id(mrb_state *mrb, struct RClass *outer, mrb_sym name, struct RClass *super)
{
  struct RClass * c;

#if 0  /* Warning is disabled by default, but can be enabled for debugging. */
  if (!super) {
    /* Emits a warning if no superclass is provided, assuming Object. */
    mrb_warn(mrb, "no super class for '%C::%n', Object assumed", outer, name);
  }
#endif
  c = define_class(mrb, name, super, outer);
  setup_class(mrb, outer, c, name); /* This sets the constant in outer */
  return c;
}

/*
 * Defines a class under the namespace of outer using a C string for the name.
 *
 * @param mrb The mruby state.
 * @param outer A pointer to the RClass structure of the outer module/class.
 * @param name The C string representing the name of the class to define.
 * @param super A pointer to the RClass structure of the superclass.
 *              If NULL, Object is assumed as the superclass.
 * @return A pointer to the newly defined or existing RClass structure for the class.
 * @raise TypeError if a constant with the same name exists but is not a class.
 * @raise NameError if the class is already defined but with a different superclass.
 * @sideeffect Creates a new class or returns an existing one if compatible.
 *             The class is set as a constant in `outer`.
 */
MRB_API struct RClass*
mrb_define_class_under(mrb_state *mrb, struct RClass *outer, const char *name, struct RClass *super)
{
  return mrb_define_class_under_id(mrb, outer, mrb_intern_cstr(mrb, name), super);
}

static mrb_bool
check_visibility_break(const struct RProc *p, const struct RClass *c, mrb_callinfo *ci, struct REnv *env)
{
  if (!p || p->upper == NULL || MRB_PROC_SCOPE_P(p) || p->e.env == NULL || !MRB_PROC_ENV_P(p)) {
    return TRUE;
  }
  if (env) {
    return p->e.env->c != c || MRB_ENV_VISIBILITY_BREAK_P(env);
  }
  return mrb_vm_ci_target_class(ci) != c || MRB_CI_VISIBILITY_BREAK_P(ci);
}

static void
find_visibility_scope(mrb_state *mrb, const struct RClass *c, int n, mrb_callinfo **cp, struct REnv **ep)
{
  const struct mrb_context *ec = mrb->c;
  mrb_callinfo *ci = ec->ci - n;
  const struct RProc *p = ci->proc;

  if (c == NULL) c = mrb_vm_ci_target_class(ci);

  if (check_visibility_break(p, c, ci, NULL)) {
    *ep = (ci->u.env && ci->u.env->tt == MRB_TT_ENV) ? ci->u.env : NULL;
    *cp = ci;
    return;
  }

  for (;;) {
    struct REnv *env = p->e.env;
    p = p->upper;
    if (check_visibility_break(p, c, ci, env)) {
      *ep = env;
      *cp = NULL;
      return;
    }
  }
}

/*
 * Defines a method with raw mrb_method_t structure.
 * This is a low-level function for method definition.
 *
 * @param mrb The mruby state.
 * @param c The class/module in which to define the method.
 * @param mid The symbol ID of the method name.
 * @param m The mrb_method_t structure representing the method.
 * @sideeffect Modifies the method table of the class/module `c`.
 *             Clears the method cache for `mid`.
 *             If `mid` is `initialize`, the method is automatically set to private.
 *             If the method visibility is default, it's determined by the current scope.
 * @raise TypeError if the class/module or its attached object (for singleton classes) is frozen.
 */
MRB_API void
mrb_define_method_raw(mrb_state *mrb, struct RClass *c, mrb_sym mid, mrb_method_t m)
{
  union mt_ptr ptr;

  MRB_CLASS_ORIGIN(c);

  mt_tbl *h = c->mt;
  if (c->tt == MRB_TT_SCLASS && mrb_frozen_p(c)) {
    mrb_value v = mrb_iv_get(mrb, mrb_obj_value(c), MRB_SYM(__attached__));
    mrb_check_frozen_value(mrb, v);
  }
  else {
    mrb_check_frozen(mrb, c);
  }
  if (!h) h = c->mt = mt_new(mrb);
  if (MRB_METHOD_PROC_P(m)) {
    struct RProc *p = (struct RProc*)MRB_METHOD_PROC(m);

    ptr.proc = p;
    if (p) {
      if (p->gc_color != MRB_GC_RED) {
        p->flags |= MRB_PROC_SCOPE;
        p->c = NULL;
        mrb_field_write_barrier(mrb, (struct RBasic*)c, (struct RBasic*)p);
        if (!MRB_PROC_ENV_P(p)) {
          MRB_PROC_SET_TARGET_CLASS(p, c);
        }
      }
      else {
        mrb_assert(mrb_frozen_p(p) && MRB_PROC_SCOPE_P(p));
        mrb_assert(p->c == NULL && p->upper == NULL && p->e.target_class == NULL);
      }
    }
  }
  else {
    ptr.func = MRB_METHOD_FUNC(m);
  }

  int flags = MT_KEY_FLG(m.flags);
  if (mid == MRB_SYM(initialize)) {
    MRB_SET_VISIBILITY_FLAGS(flags, MT_PRIVATE);
  }
  else if ((flags & MT_VMASK) == MT_VDEFAULT) {
    mrb_callinfo *ci;
    struct REnv *e;
    find_visibility_scope(mrb, c, 0, &ci, &e);
    mrb_assert(ci || e);
    MRB_SET_VISIBILITY_FLAGS(flags, (e ? MRB_ENV_VISIBILITY(e) : MRB_CI_VISIBILITY(ci)));
  }
  mt_put(mrb, h, mid, flags, ptr);
  if (!mrb->bootstrapping) mc_clear_by_id(mrb, mid);
}

static void
define_method_id(mrb_state *mrb, struct RClass *c, mrb_sym mid, mrb_func_t func, mrb_aspec aspec, int vis)
{
  mrb_method_t m;
  int ai = mrb_gc_arena_save(mrb);

  MRB_METHOD_FROM_FUNC(m, func);
  if (aspec == MRB_ARGS_NONE()) {
    MRB_METHOD_NOARG_SET(m);
  }
  MRB_METHOD_SET_VISIBILITY(m, vis);
  mrb_define_method_raw(mrb, c, mid, m);
  mrb_gc_arena_restore(mrb, ai);
}

/*
 * Defines a public C function as a method for a class/module using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param c The class/module in which to define the method.
 * @param mid The symbol ID of the method name.
 * @param func The C function pointer (mrb_func_t) for the method body.
 * @param aspec The argument specification for the method (e.g., MRB_ARGS_REQ(1)).
 * @sideeffect Modifies the method table of the class/module `c`.
 *             Clears the method cache for `mid`.
 */
MRB_API void
mrb_define_method_id(mrb_state *mrb, struct RClass *c, mrb_sym mid, mrb_func_t func, mrb_aspec aspec)
{
  define_method_id(mrb, c, mid, func, aspec, MT_PUBLIC);
}

/*
 * Defines a public C function as a method for a class/module.
 *
 * @param mrb The mruby state.
 * @param c The class/module in which to define the method.
 * @param name The C string name of the method.
 * @param func The C function pointer (mrb_func_t) for the method body.
 * @param aspec The argument specification for the method (e.g., MRB_ARGS_REQ(1)).
 * @sideeffect Modifies the method table of the class/module `c`.
 *             Interns the method name string.
 *             Clears the method cache for the interned method name.
 */
MRB_API void
mrb_define_method(mrb_state *mrb, struct RClass *c, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_method_id(mrb, c, mrb_intern_cstr(mrb, name), func, aspec);
}

/*
 * Defines a private C function as a method for a class/module using a symbol for the name.
 *
 * @param mrb The mruby state.
 * @param c The class/module in which to define the method.
 * @param mid The symbol ID of the method name.
 * @param func The C function pointer (mrb_func_t) for the method body.
 * @param aspec The argument specification for the method (e.g., MRB_ARGS_REQ(1)).
 * @sideeffect Modifies the method table of the class/module `c`.
 *             Clears the method cache for `mid`.
 */
MRB_API void
mrb_define_private_method_id(mrb_state *mrb, struct RClass *c, mrb_sym mid, mrb_func_t func, mrb_aspec aspec)
{
  define_method_id(mrb, c, mid, func, aspec, MT_PRIVATE);
}

/*
 * Defines a private C function as a method for a class/module.
 *
 * @param mrb The mruby state.
 * @param c The class/module in which to define the method.
 * @param name The C string name of the method.
 * @param func The C function pointer (mrb_func_t) for the method body.
 * @param aspec The argument specification for the method (e.g., MRB_ARGS_REQ(1)).
 * @sideeffect Modifies the method table of the class/module `c`.
 *             Interns the method name string.
 *             Clears the method cache for the interned method name.
 */
MRB_API void
mrb_define_private_method(mrb_state *mrb, struct RClass *c, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_private_method_id(mrb, c, mrb_intern_cstr(mrb, name), func, aspec);
}

/*
 * Raises a NotImplementedError, typically indicating that the C function
 * called by Ruby is not implemented for the current platform or build.
 * The error message will include the name of the Ruby method that called this C function.
 *
 * @param mrb The mruby state.
 * @sideeffect Raises a NotImplementedError exception. This function does not return.
 *             If a method name is available from the callinfo, it's included
 *             in the error message (e.g., "foo() function is unimplemented on this machine").
 */
MRB_API void
mrb_notimplement(mrb_state *mrb)
{
  mrb_callinfo *ci = mrb->c->ci;

  if (ci->mid) {
    mrb_raisef(mrb, E_NOTIMP_ERROR, "%n() function is unimplemented on this machine", ci->mid);
  }
}

/*
 * A C function suitable for use as a method body (mrb_func_t)
 * that raises a NotImplementedError.
 *
 * @param mrb The mruby state.
 * @param self The receiver of the method call (unused).
 * @return This function does not return, as it raises an exception.
 * @sideeffect Raises a NotImplementedError exception via `mrb_notimplement`.
 */
MRB_API mrb_value
mrb_notimplement_m(mrb_state *mrb, mrb_value self)
{
  mrb_notimplement(mrb);
  /* not reached */
  return mrb_nil_value();
}

static void
ensure_class_type(mrb_state *mrb, mrb_value val)
{
  if (!class_ptr_p(val)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "%v is not class/module", val);
  }
}

#define to_sym(mrb, ss) mrb_obj_to_sym(mrb, ss)

/*
 * Gets the number of arguments passed to the current C function call.
 *
 * This function retrieves the argument count from the current callinfo (`ci`)
 * in the mruby state. It correctly handles the case where arguments might be
 * packed into an array by the caller (indicated by `ci->n == 15`), in which
 * case it gets the length of that array.
 *
 * @param mrb The mruby state.
 * @return The number of arguments passed to the C function.
 */
MRB_API mrb_int
mrb_get_argc(mrb_state *mrb)
{
  mrb_int argc = mrb->c->ci->n;

  if (argc == 15) {
    struct RArray *a = mrb_ary_ptr(mrb->c->ci->stack[1]);

    a->c = NULL; /* hide from ObjectSpace.each_object */
    argc = ARY_LEN(a);
  }
  return argc;
}

/*
 * Gets a pointer to the array of arguments passed to the current C function call.
 *
 * This function retrieves the arguments from the current callinfo stack.
 * It handles the case where arguments might be packed into an array
 * (when `ci->n == 15`), returning a pointer to the elements of that array.
 * Otherwise, it returns a pointer to the arguments on the stack.
 *
 * @param mrb The mruby state.
 * @return A const pointer to the array of mrb_value arguments.
 *         The caller should not modify the contents of this array.
 * @note If arguments were packed, the RArray object on stack has its class pointer
 *       temporarily set to NULL to hide it from `ObjectSpace.each_object`.
 */
MRB_API const mrb_value*
mrb_get_argv(mrb_state *mrb)
{
  mrb_int argc = mrb->c->ci->n;
  mrb_value *array_argv = mrb->c->ci->stack + 1;
  if (argc == 15) {
    struct RArray *a = mrb_ary_ptr(*array_argv);

    a->c = NULL; /* hide from ObjectSpace.each_object */
    array_argv = ARY_PTR(a);
  }
  return array_argv;
}

/*
 * Gets the first argument passed to the current C function call.
 *
 * This is a convenience function for directly accessing the first argument.
 * It handles cases where arguments might be packed into an array or
 * if the first argument is a keyword hash.
 *
 * @param mrb The mruby state.
 * @return The first mrb_value argument.
 * @raise ArgumentError if the number of positional arguments is not 1,
 *        unless there are no positional arguments but a keyword hash is present,
 *        in which case the keyword hash is returned.
 */
MRB_API mrb_value
mrb_get_arg1(mrb_state *mrb)
{
  mrb_callinfo *ci = mrb->c->ci;
  mrb_int argc = ci->n;
  mrb_value *array_argv = ci->stack + 1;
  if (argc == 15) {
    struct RArray *a = mrb_ary_ptr(*array_argv);

    argc = ARY_LEN(a);
    array_argv = ARY_PTR(a);
  }
  if (argc == 0 && ci->nk == 15) {
    mrb_int n = ci->n;
    if (n == 15) n = 1;
    return ci->stack[n+1];      /* kwhash next to positional arguments */
  }
  if (argc != 1) {
    mrb_argnum_error(mrb, argc, 1, 1);
  }
  return array_argv[0];
}

/*
 * Checks if a block was passed to the current C function call.
 *
 * It inspects the current callinfo stack for a block argument.
 *
 * @param mrb The mruby state.
 * @return TRUE if a block is present (i.e., not nil), FALSE otherwise.
 */
MRB_API mrb_bool
mrb_block_given_p(mrb_state *mrb)
{
  mrb_callinfo *ci = mrb->c->ci;
  mrb_value b = ci->stack[mrb_ci_bidx(ci)];

  return !mrb_nil_p(b);
}

#define GET_ARG(_type) (ptr ? ((_type)(*ptr++)) : va_arg((*ap), _type))

static mrb_int
get_args_v(mrb_state *mrb, mrb_args_format format, void** ptr, va_list *ap)
{
  const char *fmt = format;
  char c;
  mrb_int i = 0;
  mrb_callinfo *ci = mrb->c->ci;
  mrb_int argc = ci->n;
  const mrb_value *argv = ci->stack+1;
  mrb_bool argv_on_stack;
  mrb_bool opt = FALSE;
  mrb_bool opt_skip = TRUE;
  const mrb_value *pickarg = NULL; /* arguments currently being processed */
  mrb_value kdict = mrb_nil_value();
  mrb_bool reqkarg = FALSE;
  int argc_min = 0, argc_max = 0;

  while ((c = *fmt++)) {
    switch (c) {
    case '|':
      opt = TRUE;
      break;
    case '*':
      opt_skip = FALSE;
      argc_max = -1;
      if (!reqkarg) reqkarg = strchr(fmt, ':') ? TRUE : FALSE;
      goto check_exit;
    case '!':
    case '+':
      break;
    case ':':
      reqkarg = TRUE;
      /* fall through */
    case '&': case '?':
      if (opt) opt_skip = FALSE;
      break;
    default:
      if (!opt) argc_min++;
      argc_max++;
      break;
    }
  }

 check_exit:
  if (!reqkarg && ci->nk > 0) {
    mrb_assert(ci->nk == 15);
    kdict = ci->stack[mrb_ci_bidx(ci)-1];
    if (mrb_hash_p(kdict) && mrb_hash_size(mrb, kdict) > 0) {
      if (argc < 14) {
        ci->n++;
        argc++;    /* include kdict in normal arguments */
      }
      else {
        /* 14+1 == 15 so pack first */
        if (argc == 14) {
          /* pack arguments and kdict */
          ci->stack[1] = mrb_ary_new_from_values(mrb, argc+1, &ci->stack[1]);
          argc = ci->n = 15;
        }
        else {
          /* push kdict to packed arguments */
          mrb_ary_push(mrb, ci->stack[1], kdict);
        }
        ci->stack[2] = ci->stack[mrb_ci_bidx(ci)];
      }
      ci->nk = 0;
    }
  }
  if (reqkarg && ci->nk > 0) {
    kdict = ci->stack[mrb_ci_bidx(ci)-1];
    mrb_assert(ci->nk == 15);
    mrb_assert(mrb_hash_p(kdict));
  }

  argv_on_stack = argc < 15;
  if (!argv_on_stack) {
    struct RArray *a = mrb_ary_ptr(*argv);
    argv = ARY_PTR(a);
    argc = ARY_LEN(a);
    a->c = NULL; /* hide from ObjectSpace.each_object */
  }

  opt = FALSE;
  i = 0;
  while ((c = *format++)) {
    mrb_bool altmode = FALSE;
    mrb_bool needmodify = FALSE;

    for (; *format; format++) {
      switch (*format) {
      case '!':
        if (altmode) goto modifier_exit; /* not accept for multiple '!' */
        altmode = TRUE;
        break;
      case '+':
        if (needmodify) goto modifier_exit; /* not accept for multiple '+' */
        needmodify = TRUE;
        break;
      default:
        goto modifier_exit;
      }
    }

  modifier_exit:
    switch (c) {
    case '|': case '*': case '&': case '?': case ':':
      if (needmodify) {
      bad_needmodify:
        mrb_raisef(mrb, E_ARGUMENT_ERROR, "wrong `%c+` modified specifier`", c);
      }
      break;
    default:
      if (i < argc) {
        pickarg = &argv[i++];
        if (needmodify && !mrb_nil_p(*pickarg)) {
          mrb_check_frozen_value(mrb, *pickarg);
        }
      }
      else {
        if (opt) {
          pickarg = NULL;
        }
        else {
          mrb_argnum_error(mrb, argc, argc_min, argc_max);
        }
      }
      break;
    }

    switch (c) {
    case 'o':
    case 'C':
    case 'S':
    case 'A':
    case 'H':
      {
        mrb_value *p;

        p = GET_ARG(mrb_value*);
        if (pickarg) {
          if (!(altmode && mrb_nil_p(*pickarg))) {
            switch (c) {
            case 'C': ensure_class_type(mrb, *pickarg); break;
            case 'S': mrb_ensure_string_type(mrb, *pickarg); break;
            case 'A': mrb_ensure_array_type(mrb, *pickarg); break;
            case 'H': mrb_ensure_hash_type(mrb, *pickarg); break;
            }
          }
          *p = *pickarg;
        }
      }
      break;
    case 'c':
      {
        struct RClass **p;

        p = GET_ARG(struct RClass**);
        if (pickarg) {
          if (altmode && mrb_nil_p(*pickarg)) {
            *p = NULL;
          }
          else {
            ensure_class_type(mrb, *pickarg);
            *p = mrb_class_ptr(*pickarg);
          }
        }
      }
      break;
    case 's':
      {
        const char **ps = NULL;
        mrb_int *pl = NULL;

        ps = GET_ARG(const char**);
        pl = GET_ARG(mrb_int*);
        if (needmodify) goto bad_needmodify;
        if (pickarg) {
          if (altmode && mrb_nil_p(*pickarg)) {
            *ps = NULL;
            *pl = 0;
          }
          else {
            mrb_ensure_string_type(mrb, *pickarg);
            *ps = RSTRING_PTR(*pickarg);
            *pl = RSTRING_LEN(*pickarg);
          }
        }
      }
      break;
    case 'z':
      {
        const char **ps;

        ps = GET_ARG(const char**);
        if (needmodify) goto bad_needmodify;
        if (pickarg) {
          if (altmode && mrb_nil_p(*pickarg)) {
            *ps = NULL;
          }
          else {
            mrb_ensure_string_type(mrb, *pickarg);
            *ps = RSTRING_CSTR(mrb, *pickarg);
          }
        }
      }
      break;
    case 'a':
      {
        struct RArray *a;
        const mrb_value **pb;
        mrb_int *pl;

        pb = GET_ARG(const mrb_value**);
        pl = GET_ARG(mrb_int*);
        if (needmodify) goto bad_needmodify;
        if (pickarg) {
          if (altmode && mrb_nil_p(*pickarg)) {
            *pb = NULL;
            *pl = 0;
          }
          else {
            mrb_ensure_array_type(mrb, *pickarg);
            a = mrb_ary_ptr(*pickarg);
            *pb = ARY_PTR(a);
            *pl = ARY_LEN(a);
          }
        }
      }
      break;
#ifndef MRB_NO_FLOAT
    case 'f':
      {
        mrb_float *p;

        p = GET_ARG(mrb_float*);
        if (pickarg) {
          *p = mrb_as_float(mrb, *pickarg);
        }
      }
      break;
#endif
    case 'i':
      {
        mrb_int *p;

        p = GET_ARG(mrb_int*);
        if (pickarg) {
          *p = mrb_as_int(mrb, *pickarg);
        }
      }
      break;
    case 'b':
      {
        mrb_bool *boolp = GET_ARG(mrb_bool*);

        if (pickarg) {
          *boolp = mrb_test(*pickarg);
        }
      }
      break;
    case 'n':
      {
        mrb_sym *symp;

        symp = GET_ARG(mrb_sym*);
        if (pickarg) {
          *symp = to_sym(mrb, *pickarg);
        }
      }
      break;
    case 'd':
      {
        void** datap;
        struct mrb_data_type const* type;

        datap = GET_ARG(void**);
        type = GET_ARG(struct mrb_data_type const*);
        if (pickarg) {
          if (altmode && mrb_nil_p(*pickarg)) {
            *datap = NULL;
          }
          else {
            *datap = mrb_data_get_ptr(mrb, *pickarg, type);
          }
        }
      }
      break;

    case '&':
      {
        mrb_value *p, *bp;

        p = GET_ARG(mrb_value*);
        bp = ci->stack + mrb_ci_bidx(ci);
        if (altmode && mrb_nil_p(*bp)) {
          mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
        }
        *p = *bp;
      }
      break;
    case '|':
      if (opt_skip && i == argc) goto finish;
      opt = TRUE;
      break;
    case '?':
      {
        mrb_bool *p;

        p = GET_ARG(mrb_bool*);
        *p = pickarg ? TRUE : FALSE;
      }
      break;

    case '*':
      {
        const mrb_value **var;
        mrb_int *pl;
        mrb_bool nocopy = (altmode || !argv_on_stack) ? TRUE : FALSE;

        var = GET_ARG(const mrb_value**);
        pl = GET_ARG(mrb_int*);
        if (argc > i) {
          *pl = argc-i;
          if (*pl > 0) {
            if (nocopy) {
              *var = argv+i;
            }
            else {
              mrb_value args = mrb_ary_new_from_values(mrb, *pl, argv+i);
              RARRAY(args)->c = NULL;
              *var = RARRAY_PTR(args);
            }
          }
          i = argc;
        }
        else {
          *pl = 0;
          *var = NULL;
        }
      }
      break;

    case ':':
      {
        mrb_value ksrc = mrb_hash_p(kdict) ? mrb_hash_dup(mrb, kdict) : mrb_hash_new(mrb);
        const mrb_kwargs *kwargs = GET_ARG(const mrb_kwargs*);
        mrb_value *rest;

        if (kwargs == NULL) {
          rest = NULL;
        }
        else {
          mrb_int kwnum = kwargs->num;
          mrb_int required = kwargs->required;
          const mrb_sym *kname = kwargs->table;
          mrb_value *values = kwargs->values;
          mrb_int j;
          const mrb_int keyword_max = 40;

          mrb_assert(kwnum >= 0);
          mrb_assert(required >= 0);
          if (kwnum > keyword_max || required > kwnum) {
            mrb_raise(mrb, E_ARGUMENT_ERROR, "keyword number is too large");
          }

          for (j = required; j > 0; j--, kname++, values++) {
            mrb_value k = mrb_symbol_value(*kname);
            if (!mrb_hash_key_p(mrb, ksrc, k)) {
              mrb_raisef(mrb, E_ARGUMENT_ERROR, "missing keyword: %n", *kname);
            }
            *values = mrb_hash_delete_key(mrb, ksrc, k);
            mrb_gc_protect(mrb, *values);
          }

          for (j = kwnum - required; j > 0; j--, kname++, values++) {
            mrb_value k = mrb_symbol_value(*kname);
            if (mrb_hash_key_p(mrb, ksrc, k)) {
              *values = mrb_hash_delete_key(mrb, ksrc, k);
              mrb_gc_protect(mrb, *values);
            }
            else {
              *values = mrb_undef_value();
            }
          }

          rest = kwargs->rest;
        }

        if (rest) {
          *rest = ksrc;
        }
        else if (!mrb_hash_empty_p(mrb, ksrc)) {
          ksrc = mrb_hash_first_key(mrb, ksrc);
          mrb_raisef(mrb, E_ARGUMENT_ERROR, "unknown keyword: %v", ksrc);
        }
      }
      break;

    default:
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "invalid argument specifier %c", c);
      break;
    }
  }

  if (!c && argc > i) {
    mrb_argnum_error(mrb, argc, argc_min, argc_max);
  }

finish:
  return i;
}

/*
  retrieve arguments from mrb_state.

  mrb_get_args(mrb, format, ...)

  returns number of arguments parsed.

  format specifiers:

    string  mruby type     C type                 note
    ----------------------------------------------------------------------------------------------
    o:      Object         [mrb_value]
    C:      Class/Module   [mrb_value]            when ! follows, the value may be nil
    S:      String         [mrb_value]            when ! follows, the value may be nil
    A:      Array          [mrb_value]            when ! follows, the value may be nil
    H:      Hash           [mrb_value]            when ! follows, the value may be nil
    s:      String         [const char*,mrb_int]  Receive two arguments; s! gives (NULL,0) for nil
    z:      String         [const char*]          NUL terminated string; z! gives NULL for nil
    a:      Array          [const mrb_value*,mrb_int] Receive two arguments; a! gives (NULL,0) for nil
    c:      Class/Module   [struct RClass*]       c! gives NULL for nil
    f:      Integer/Float  [mrb_float]
    i:      Integer/Float  [mrb_int]
    b:      boolean        [mrb_bool]
    n:      String/Symbol  [mrb_sym]
    d:      data           [void*,mrb_data_type const] 2nd argument will be used to check data type so it won't be modified; when ! follows, the value may be nil
    &:      block          [mrb_value]            &! raises exception if no block given
    *:      rest argument  [const mrb_value*,mrb_int] The rest of the arguments as an array; *! avoid copy of the stack
    |:      optional                              Following arguments are optional
    ?:      optional given [mrb_bool]             true if preceding argument (optional) is given
    ':':    keyword args   [mrb_kwargs const]     Get keyword arguments

  format modifiers:

    string  note
    ----------------------------------------------------------------------------------------------
    !:      Switch to the alternate mode; The behaviour changes depending on the specifier
    +:      Request a not frozen object; However, except nil value
 */
/*
 * Retrieves and parses arguments passed to a C function based on a given format string.
 * This is the primary and most flexible way for C extensions to handle arguments
 * passed from Ruby method calls.
 *
 * @param mrb The mruby state.
 * @param format A C string that specifies the expected arguments and their types.
 *        See below for detailed format specifiers and modifiers.
 * @param ... A variable number of pointer arguments, corresponding to the types
 *        specified in the format string, where the parsed values will be stored.
 * @return The number of arguments successfully parsed and assigned from the Ruby stack
 *         to the C variables.
 * @raise ArgumentError if the passed arguments do not match the format string,
 *        if there are type mismatches, or if required arguments are missing.
 * @sideeffect Arguments from the mruby stack are converted and stored in the C variables
 *             provided via `...`. The mruby garbage collector arena might be saved
 *             and restored during this process. Keyword argument processing might
 *             involve hash duplication or key deletion.
 *
 * Format Specifiers (within the `format` string):
 *   'o': Object (expects mrb_value*)
 *   'C': Class/Module (expects mrb_value*). Use 'c' for `struct RClass*`.
 *   'S': String (expects mrb_value*)
 *   'A': Array (expects mrb_value*)
 *   'H': Hash (expects mrb_value*)
 *   's': String (expects const char**, mrb_int* for pointer and length)
 *   'z': String (expects const char** for a NUL-terminated string)
 *   'a': Array (expects const mrb_value**, mrb_int* for pointer and length)
 *   'c': Class/Module (expects struct RClass**)
 *   'f': Float (expects mrb_float*) - available if MRB_NO_FLOAT is not defined.
 *   'i': Integer (expects mrb_int*)
 *   'b': Boolean (expects mrb_bool*)
 *   'n': Symbol (expects mrb_sym*) - converts from String or Symbol argument.
 *   'd': Data (expects void**, const struct mrb_data_type*). The second argument is used
 *        for type checking and is not modified.
 *   '&': Block (expects mrb_value*) - retrieves the block passed to the method.
 *   '*': Rest arguments (expects const mrb_value**, mrb_int*) - captures all remaining
 *        positional arguments into an array.
 *   '|': Optional arguments separator. Arguments following this are optional.
 *   '?': Optional given (expects mrb_bool*) - sets to TRUE if the preceding optional
 *        argument was provided, FALSE otherwise.
 *   ':': Keyword arguments (expects const mrb_kwargs*). Used to retrieve keyword arguments.
 *        See mrb_kwargs structure for details.
 *
 * Format Modifiers (prefix the specifier, e.g., "!s" or "c!"):
 *   '!': Alternate mode. Behavior changes depending on the specifier.
 *        For example, 's!' gives (NULL, 0) for a nil string. 'c!' gives NULL for nil.
 *        '&!' raises an ArgumentError if no block is given.
 *        '*!' avoids copying the rest arguments from the stack if possible.
 *   '+': Request a modifiable (not frozen) object. Raises a FrozenError if the
 *        retrieved object is frozen (this check does not apply to nil values).
 */
MRB_API mrb_int
mrb_get_args(mrb_state *mrb, mrb_args_format format, ...)
{
  va_list ap;
  va_start(ap, format);
  mrb_int rc = get_args_v(mrb, format, NULL, &ap);
  va_end(ap);
  return rc;
}

/*
 * Retrieves and parses arguments passed to a C function according to a format string,
 * taking a `void**` array for the output variables instead of `va_list`.
 * This version is useful when the argument parsing needs to be done in a more
 * programmatic way, or when wrapping `mrb_get_args`.
 *
 * @param mrb The mruby state.
 * @param format A C string specifying the expected arguments. See `mrb_get_args`
 *        documentation for format specifiers and modifiers.
 * @param args An array of `void*` pointers to variables where the parsed arguments
 *        will be stored. The types of these variables must correspond to the
 *        specifiers in the `format` string.
 * @return The number of arguments successfully parsed and assigned.
 * @raise ArgumentError if arguments do not match the format string, or if there are
 *        type mismatches.
 * @sideeffect Arguments from the mruby stack are converted and stored in the C variables
 *             pointed to by the elements of the `args` array.
 *             (See `mrb_get_args` for more details on side effects like GC arena handling
 *             and keyword argument processing).
 */
MRB_API mrb_int
mrb_get_args_a(mrb_state *mrb, mrb_args_format format, void **args)
{
  return get_args_v(mrb, format, args, NULL);
}

static struct RClass*
boot_defclass(mrb_state *mrb, struct RClass *super)
{
  struct RClass *c = MRB_OBJ_ALLOC(mrb, MRB_TT_CLASS, mrb->class_class);

  if (super) {
    c->super = super;
    mrb_field_write_barrier(mrb, (struct RBasic*)c, (struct RBasic*)super);
    c->flags |= MRB_FL_CLASS_IS_INHERITED;
  }
  else {
    c->super = mrb->object_class;
  }
  c->mt = mt_new(mrb);
  return c;
}

static void
boot_initmod(mrb_state *mrb, struct RClass *mod)
{
  if (!mod->mt) {
    mod->mt = mt_new(mrb);
  }
}

static struct RClass*
include_class_new(mrb_state *mrb, struct RClass *m, struct RClass *super)
{
  struct RClass *ic = MRB_OBJ_ALLOC(mrb, MRB_TT_ICLASS, mrb->class_class);
  if (m->tt == MRB_TT_ICLASS) {
    m = m->c;
  }
  MRB_CLASS_ORIGIN(m);
  ic->mt = m->mt;
  ic->super = super;
  if (m->tt == MRB_TT_ICLASS) {
    ic->c = m->c;
  }
  else {
    ic->c = m;
  }
  return ic;
}

static int
include_module_at(mrb_state *mrb, struct RClass *c, struct RClass *ins_pos, struct RClass *m, int search_super)
{
  struct RClass *ic;
  void *klass_mt = find_origin(c)->mt;

  while (m) {
    struct RClass *p = c->super;
    int original_seen = FALSE;
    int superclass_seen = FALSE;

    if (c == ins_pos) original_seen = TRUE;
    if (m->flags & MRB_FL_CLASS_IS_PREPENDED)
      goto skip;
    if (klass_mt && klass_mt == m->mt)
      return -1;

    while (p) {
      if (c == p) original_seen = TRUE;
      if (p->tt == MRB_TT_ICLASS) {
        if (p->mt == m->mt) {
          if (!superclass_seen && original_seen) {
            ins_pos = p; /* move insert point */
          }
          goto skip;
        }
      }
      else if (p->tt == MRB_TT_CLASS) {
        if (!search_super) break;
        superclass_seen = TRUE;
      }
      p = p->super;
    }

    ic = include_class_new(mrb, m, ins_pos->super);
    m->flags |= MRB_FL_CLASS_IS_INHERITED;
    ins_pos->super = ic;
    mrb_field_write_barrier(mrb, (struct RBasic*)ins_pos, (struct RBasic*)ic);
    ins_pos = ic;
  skip:
    m = m->super;
  }
  if (!mrb->bootstrapping) mrb_method_cache_clear(mrb);
  return 0;
}

static int
fix_include_module(mrb_state *mrb, struct RBasic *obj, void *data)
{
  struct RClass **m = (struct RClass**)data;

  if (obj->tt == MRB_TT_ICLASS && obj->c == m[0] && !MRB_FLAG_TEST(obj, MRB_FL_CLASS_IS_ORIGIN)) {
    struct RClass *ic = (struct RClass*)obj;
    include_module_at(mrb, ic, ic, m[1], 1);
  }
  return MRB_EACH_OBJ_OK;
}

/*
 * Includes a module into a class or another module.
 * This adds the methods and constants of module `m` to class `c` (or module `c`).
 * The included module's instance methods become instance methods of `c`.
 *
 * @param mrb The mruby state.
 * @param c The target class or module into which module `m` will be included.
 * @param m The module to include. Must be a module (MRB_TT_MODULE).
 * @raise ArgumentError if `m` is not a module or if a cyclic include is detected.
 * @raise FrozenError if class/module `c` is frozen.
 * @sideeffect Modifies the ancestor chain of `c` by inserting an ICLASS (inclusion class)
 *             that references `m`'s method table.
 *             Clears the method cache.
 *             If `m` defines an `included` hook, it will be called with `c` as an argument.
 *             If `c` is a module that has itself been included in other classes/modules,
 *             this operation will also propagate the inclusion of `m` to those descendants.
 */
MRB_API void
mrb_include_module(mrb_state *mrb, struct RClass *c, struct RClass *m)
{
  mrb_check_frozen(mrb, c);
  if (include_module_at(mrb, c, find_origin(c), m, 1) < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "cyclic include detected");
  }
  if (c->tt == MRB_TT_MODULE && (c->flags & MRB_FL_CLASS_IS_INHERITED)) {
    struct RClass *data[2];
    data[0] = c;
    data[1] = m;
    mrb_objspace_each_objects(mrb, fix_include_module, data);
  }
}

static int
fix_prepend_module(mrb_state *mrb, struct RBasic *obj, void *data)
{
  struct RClass **m = (struct RClass**)data;
  struct RClass *c = (struct RClass*)obj;

  if (c->tt == MRB_TT_CLASS || c->tt == MRB_TT_MODULE) {
    struct RClass *p = c->super;
    struct RClass *ins_pos = c;
    while (p) {
      if (c == m[0]) break;
      if (p == m[0]->super->c) {
        ins_pos = c;
      }
      if (p->tt == MRB_TT_CLASS) break;
      if (p->c == m[0]) {
        include_module_at(mrb, ins_pos, ins_pos, m[1], 0);
        break;
      }
      c = p;
      p = p->super;
    }
  }
  return MRB_EACH_OBJ_OK;
}

/*
 * Prepends a module to a class or another module.
 * Methods in the prepended module `m` will override methods of the same name in `c`.
 * In the ancestor chain, the prepended module appears before the class/module itself.
 *
 * @param mrb The mruby state.
 * @param c The target class or module to which module `m` will be prepended.
 * @param m The module to prepend. Must be a module (MRB_TT_MODULE).
 * @raise ArgumentError if `m` is not a module or if a cyclic prepend is detected.
 * @raise FrozenError if class/module `c` is frozen.
 * @sideeffect Modifies the ancestor chain of `c`. If `c` hasn't been prepended before,
 *             an "origin" ICLASS is created to hold `c`'s original methods, and `c`'s
 *             method table is cleared. Then, an ICLASS for `m` is inserted above `c`.
 *             Clears the method cache.
 *             If `m` defines a `prepended` hook, it will be called with `c` as an argument.
 *             If `c` is a module that has been included/prepended elsewhere, this
 *             operation propagates the prepending of `m` to those descendants.
 */
MRB_API void
mrb_prepend_module(mrb_state *mrb, struct RClass *c, struct RClass *m)
{
  mrb_check_frozen(mrb, c);
  if (!(c->flags & MRB_FL_CLASS_IS_PREPENDED)) {
    struct RClass *origin = MRB_OBJ_ALLOC(mrb, MRB_TT_ICLASS, c);
    origin->flags |= MRB_FL_CLASS_IS_ORIGIN | MRB_FL_CLASS_IS_INHERITED;
    origin->super = c->super;
    c->super = origin;
    origin->mt = c->mt;
    c->mt = NULL;
    mrb_field_write_barrier(mrb, (struct RBasic*)c, (struct RBasic*)origin);
    c->flags |= MRB_FL_CLASS_IS_PREPENDED;
  }
  if (include_module_at(mrb, c, c, m, 0) < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "cyclic prepend detected");
  }
  if (c->tt == MRB_TT_MODULE &&
      (c->flags & (MRB_FL_CLASS_IS_INHERITED|MRB_FL_CLASS_IS_PREPENDED))) {
    struct RClass *data[2];
    data[0] = c;
    data[1] = m;
    mrb_objspace_each_objects(mrb, fix_prepend_module, data);
  }
}

static mrb_value
mrb_mod_prepend(mrb_state *mrb, mrb_value mod)
{
  struct RClass *c = mrb_class_ptr(mod);
  mrb_int argc;
  mrb_value *argv;
  mrb_sym prepended = MRB_SYM(prepended);

  mrb_get_args(mrb, "*", &argv, &argc);
  while (argc--) {
    mrb_value m = argv[argc];
    mrb_check_type(mrb, m, MRB_TT_MODULE);
    mrb_prepend_module(mrb, c, mrb_class_ptr(m));
    if (!mrb_func_basic_p(mrb, m, prepended, mrb_do_nothing)) {
      mrb_funcall_argv(mrb, m, prepended, 1, &mod);
    }
  }
  return mod;
}

static mrb_value
mrb_mod_include(mrb_state *mrb, mrb_value mod)
{
  struct RClass *c = mrb_class_ptr(mod);
  mrb_int argc;
  mrb_value *argv;
  mrb_sym included = MRB_SYM(included);

  mrb_get_args(mrb, "*", &argv, &argc);
  while (argc--) {
    mrb_value m = argv[argc];
    mrb_check_type(mrb, m, MRB_TT_MODULE);
    mrb_include_module(mrb, c, mrb_class_ptr(m));
    if (!mrb_func_basic_p(mrb, m, included, mrb_do_nothing)) {
      mrb_funcall_argv(mrb, m, included, 1, &mod);
    }
  }
  return mod;
}

/* 15.3.1.3.13 */
/*
 *  call-seq:
 *    obj.extend(module, ...)    -> obj
 *
 *  Adds to _obj_ the instance methods from each module given as a
 *  parameter.
 *
 *     module Mod
 *      def hello
 *         "Hello from Mod.\n"
 *      end
 *     end
 *
 *     class Klass
 *       def hello
 *         "Hello from Klass.\n"
 *       end
 *     end
 *
 *     k = Klass.new
 *     k.hello         #=> "Hello from Klass.\n"
 *     k.extend(Mod)   #=> #<Klass:0x401b3bc8>
 *     k.hello         #=> "Hello from Mod.\n"
 *
 */
/*
 * Adds the instance methods from one or more modules to the given object `obj`.
 * This is achieved by including the specified modules into `obj`'s singleton class.
 *
 * @param mrb The mruby state.
 * @param obj The object to extend.
 * @return The extended object `obj`.
 * @raise TypeError if any of the arguments passed for extension are not modules.
 * @sideeffect Modifies the singleton class of `obj`. If the singleton class doesn't exist,
 *             it is created. The given modules are included into this singleton class.
 *             If any of the included modules define an `extended` hook, it is called
 *             with `obj` as an argument.
 */
mrb_value
mrb_obj_extend(mrb_state *mrb, mrb_value obj)
{
  mrb_int argc;
  mrb_value *argv;
  mrb_sym extended = MRB_SYM(extended);

  mrb_get_args(mrb, "*", &argv, &argc);

  mrb_value cc = mrb_singleton_class(mrb, obj);
  while (argc--) {
    mrb_value mod = argv[argc];
    mrb_check_type(mrb, mod, MRB_TT_MODULE);
    mrb_include_module(mrb, mrb_class_ptr(cc), mrb_class_ptr(mod));
    if (!mrb_func_basic_p(mrb, mod, extended, mrb_do_nothing)) {
      mrb_funcall_argv(mrb, mod, extended, 1, &obj);
    }
  }
  return obj;
}

/* 15.2.2.4.28 */
/*
 *  call-seq:
 *     mod.include?(module)    -> true or false
 *
 *  Returns <code>true</code> if <i>module</i> is included in
 *  <i>mod</i> or one of <i>mod</i>'s ancestors.
 *
 *     module A
 *     end
 *     class B
 *       include A
 *     end
 *     class C < B
 *     end
 *     B.include?(A)   #=> true
 *     C.include?(A)   #=> true
 *     A.include?(A)   #=> false
 */
static mrb_value
mrb_mod_include_p(mrb_state *mrb, mrb_value mod)
{
  mrb_value mod2;
  struct RClass *c = mrb_class_ptr(mod);

  mrb_get_args(mrb, "C", &mod2);
  mrb_check_type(mrb, mod2, MRB_TT_MODULE);

  while (c) {
    if (c->tt == MRB_TT_ICLASS) {
      if (c->c == mrb_class_ptr(mod2)) return mrb_true_value();
    }
    c = c->super;
  }
  return mrb_false_value();
}

static mrb_value
mrb_mod_ancestors(mrb_state *mrb, mrb_value self)
{
  struct RClass *c = mrb_class_ptr(self);
  mrb_value result = mrb_ary_new(mrb);

  while (c) {
    if (c->tt == MRB_TT_ICLASS) {
      mrb_ary_push(mrb, result, mrb_obj_value(c->c));
    }
    else if (!(c->flags & MRB_FL_CLASS_IS_PREPENDED)) {
      mrb_ary_push(mrb, result, mrb_obj_value(c));
    }
    c = c->super;
  }

  return result;
}

static mrb_value
mrb_mod_initialize(mrb_state *mrb, mrb_value mod)
{
  mrb_value b;
  struct RClass *m = mrb_class_ptr(mod);
  boot_initmod(mrb, m); /* bootstrap a newly initialized module */
  mrb_get_args(mrb, "|&", &b);
  if (!mrb_nil_p(b)) {
    mrb_yield_with_class(mrb, b, 1, &mod, mod, m);
  }
  return mod;
}

static void
mrb_mod_visibility(mrb_state *mrb, mrb_value mod, int vis)
{
  mrb_assert((vis&MT_VMASK)==vis);
  mrb_int argc;
  mrb_value *argv;
  struct RClass *c = mrb_class_ptr(mod);

  mrb_get_args(mrb, "*!", &argv, &argc);
  if (argc == 0) {
    mrb_callinfo *ci;
    struct REnv *e;
    find_visibility_scope(mrb, NULL, 1, &ci, &e);
    if (e) {
      MRB_ENV_SET_VISIBILITY(e, vis);
    }
    else {
      MRB_CI_SET_VISIBILITY(ci, vis);
    }
  }
  else {
    mt_tbl *h = c->mt;
    for (int i=0; i<argc; i++) {
      mrb_check_type(mrb, argv[i], MRB_TT_SYMBOL);
      mrb_sym mid = mrb_symbol(argv[i]);
      mrb_method_t m = mrb_method_search(mrb, c, mid);
      MRB_METHOD_SET_VISIBILITY(m, vis);
      union mt_ptr ptr;
      if (MRB_METHOD_PROC_P(m)) {
        ptr.proc = MRB_METHOD_PROC(m);
      }
      else {
        ptr.func = MRB_METHOD_FUNC(m);
      }
      mt_put(mrb, h, mid, MT_KEY_FLG(m.flags), ptr);
      mc_clear_by_id(mrb, mid);
    }
  }
}

static mrb_value
mrb_mod_public(mrb_state *mrb, mrb_value mod)
{
  mrb_mod_visibility(mrb, mod, MT_PUBLIC);
  return mod;
}

static mrb_value
mrb_mod_private(mrb_state *mrb, mrb_value mod)
{
  mrb_mod_visibility(mrb, mod, MT_PRIVATE);
  return mod;
}

static mrb_value
mrb_mod_protected(mrb_state *mrb, mrb_value mod)
{
  mrb_mod_visibility(mrb, mod, MT_PROTECTED);
  return mod;
}

static mrb_value
top_public(mrb_state *mrb, mrb_value self)
{
  self = mrb_obj_value(mrb->object_class);
  mrb_mod_visibility(mrb, self, MT_PUBLIC);
  return self;
}

static mrb_value
top_private(mrb_state *mrb, mrb_value self)
{
  self = mrb_obj_value(mrb->object_class);
  mrb_mod_visibility(mrb, self, MT_PRIVATE);
  return self;
}

static mrb_value
top_protected(mrb_state *mrb, mrb_value self)
{
  self = mrb_obj_value(mrb->object_class);
  mrb_mod_visibility(mrb, self, MT_PROTECTED);
  return self;
}

/*
 * Retrieves a pointer to the singleton class (also known as metaclass or eigenclass)
 * of a given object `v`. If the singleton class does not yet exist, it is created.
 *
 * Singleton classes are anonymous classes associated with a specific object,
 * allowing that object to have its own unique methods.
 *
 * @param mrb The mruby state.
 * @param v The `mrb_value` for which to get the singleton class.
 * @return A pointer to the `RClass` structure of the singleton class.
 *         Returns `NULL` for immediate values (e.g., Symbols, Integers,
 *         Floats if not word-boxed, C pointers) as they cannot have singleton classes.
 *         For `nil`, `true`, and `false`, it returns their respective predefined
 *         classes (`mrb->nil_class`, `mrb->true_class`, `mrb->false_class`),
 *         which effectively act as their singleton classes.
 * @sideeffect If the singleton class doesn't exist for `v` (and `v` can have one,
 *             i.e., it's not an immediate value or one of the special singletons),
 *             this function will:
 *             1. Allocate a new `RClass` of type `MRB_TT_SCLASS`.
 *             2. Set its superclass appropriately (e.g., the object's original class,
 *                or the class of the superclass for class singletons).
 *             3. Link this new singleton class to the object `v`.
 *             4. Set an internal `__attached__` instance variable on the singleton
 *                class to point back to `v`.
 *             5. The `MRB_FL_CLASS_IS_INHERITED` flag is set on the new singleton class.
 */
MRB_API struct RClass*
mrb_singleton_class_ptr(mrb_state *mrb, mrb_value v)
{
  struct RBasic *obj;

  switch (mrb_type(v)) {
  case MRB_TT_FALSE:
    if (mrb_nil_p(v))
      return mrb->nil_class;
    return mrb->false_class;
  case MRB_TT_TRUE:
    return mrb->true_class;
  case MRB_TT_CPTR:
  case MRB_TT_SYMBOL:
  case MRB_TT_INTEGER:
#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
#endif
    return NULL;
  default:
    break;
  }
  obj = mrb_basic_ptr(v);
  if (obj->c == NULL) return NULL;
  prepare_singleton_class(mrb, obj);
  return obj->c;
}

/*
 * Retrieves the singleton class (also known as metaclass or eigenclass) of a given object
 * as an mrb_value. If the singleton class does not exist, it is created.
 *
 * This function is a wrapper around `mrb_singleton_class_ptr` that returns the
 * singleton class as an `mrb_value`.
 *
 * @param mrb The mruby state.
 * @param v The `mrb_value` for which to get the singleton class.
 * @return An `mrb_value` representing the singleton class.
 * @raise TypeError if `v` is an object that cannot have a singleton class
 *        (e.g., immediate values like Symbols or Integers under certain configurations,
 *        or C pointers). This exception is raised by the underlying
 *        `mrb_singleton_class_ptr` if it returns NULL.
 * @sideeffect If the singleton class doesn't exist for `v` (and `v` can have one),
 *             it will be created via `mrb_singleton_class_ptr`, which involves
 *             memory allocation and modification of the object's class pointer.
 */
MRB_API mrb_value
mrb_singleton_class(mrb_state *mrb, mrb_value v)
{
  struct RClass *c = mrb_singleton_class_ptr(mrb, v);

  if (c == NULL) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't define singleton");
  }
  return mrb_obj_value(c);
}

/*
 * Defines a singleton method for a specific object `o`.
 * A singleton method is a method that belongs only to a single object,
 * not to all instances of its class. It's defined in the object's singleton class.
 *
 * @param mrb The mruby state.
 * @param o A pointer to the RObject for which the singleton method is being defined.
 * @param name The C string name of the method.
 * @param func The C function (mrb_func_t) that implements the method.
 * @param aspec The argument specification for the method (e.g., MRB_ARGS_REQ(1)).
 * @sideeffect
 *   1. Ensures that the singleton class for object `o` exists, creating it if necessary.
 *      This might involve memory allocation.
 *   2. Defines the method specified by `name`, `func`, and `aspec` into this
 *      singleton class.
 *   3. The method name `name` is interned into a symbol.
 *   4. The method cache for the newly defined method is cleared.
 * @raise TypeError if `o` is an object that cannot have a singleton class (e.g., immediate values).
 */
MRB_API void
mrb_define_singleton_method(mrb_state *mrb, struct RObject *o, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  prepare_singleton_class(mrb, (struct RBasic*)o);
  mrb_define_method_id(mrb, o->c, mrb_intern_cstr(mrb, name), func, aspec);
}

/*
 * Defines a singleton method for a specific object `o` using a symbol for the method name.
 * A singleton method is a method that belongs only to a single object,
 * not to all instances of its class. It's defined in the object's singleton class.
 *
 * @param mrb The mruby state.
 * @param o A pointer to the RObject for which the singleton method is being defined.
 * @param name The symbol ID (`mrb_sym`) of the method name.
 * @param func The C function (mrb_func_t) that implements the method.
 * @param aspec The argument specification for the method (e.g., MRB_ARGS_REQ(1)).
 * @sideeffect
 *   1. Ensures that the singleton class for object `o` exists, creating it if necessary.
 *      This might involve memory allocation.
 *   2. Defines the method specified by `name`, `func`, and `aspec` into this
 *      singleton class.
 *   3. The method cache for the method `name` is cleared.
 * @raise TypeError if `o` is an object that cannot have a singleton class (e.g., immediate values).
 */
MRB_API void
mrb_define_singleton_method_id(mrb_state *mrb, struct RObject *o, mrb_sym name, mrb_func_t func, mrb_aspec aspec)
{
  prepare_singleton_class(mrb, (struct RBasic*)o);
  mrb_define_method_id(mrb, o->c, name, func, aspec);
}

/*
 * Defines a class method for a class/module `c`.
 * Class methods are effectively singleton methods defined on the class object itself.
 *
 * @param mrb The mruby state.
 * @param c The class/module (`RClass*`) for which to define the class method.
 * @param name The C string name of the class method.
 * @param func The C function (mrb_func_t) that implements the method.
 * @param aspec The argument specification for the method.
 * @sideeffect This function internally calls `mrb_define_singleton_method` on the
 *             class object `c`. This involves:
 *             1. Ensuring `c`'s singleton class exists (creating it if needed).
 *             2. Defining the method in `c`'s singleton class.
 *             3. Interning the `name` string.
 *             4. Clearing the relevant method cache.
 * @raise TypeError if `c` itself is an object that cannot have a singleton class (highly unlikely for RClass).
 */
MRB_API void
mrb_define_class_method(mrb_state *mrb, struct RClass *c, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_singleton_method(mrb, (struct RObject*)c, name, func, aspec);
}

/*
 * Defines a class method for a class/module `c` using a symbol for the method name.
 * Class methods are effectively singleton methods defined on the class object itself.
 *
 * @param mrb The mruby state.
 * @param c The class/module (`RClass*`) for which to define the class method.
 * @param name The symbol ID (`mrb_sym`) of the class method name.
 * @param func The C function (mrb_func_t) that implements the method.
 * @param aspec The argument specification for the method.
 * @sideeffect This function internally calls `mrb_define_singleton_method_id` on the
 *             class object `c`. This involves:
 *             1. Ensuring `c`'s singleton class exists (creating it if needed).
 *             2. Defining the method in `c`'s singleton class.
 *             3. Clearing the relevant method cache.
 * @raise TypeError if `c` itself is an object that cannot have a singleton class (highly unlikely for RClass).
 */
MRB_API void
mrb_define_class_method_id(mrb_state *mrb, struct RClass *c, mrb_sym name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_singleton_method_id(mrb, (struct RObject*)c, name, func, aspec);
}

/*
 * Defines a module function for a module `c` using a symbol for the name.
 * A module function is a shorthand for defining a method that is both a
 * public class method (callable on the module itself) and a private
 * instance method (callable within the context of classes that include the module).
 *
 * @param mrb The mruby state.
 * @param c The module (`RClass*` where `c->tt` should be `MRB_TT_MODULE`)
 *          for which to define the module function.
 * @param name The symbol ID (`mrb_sym`) of the module function name.
 * @param func The C function (mrb_func_t) that implements the function.
 * @param aspec The argument specification for the function.
 * @sideeffect
 *   1. Defines a public class method on `c` with the given `name`, `func`, and `aspec`.
 *      This involves creating/accessing `c`'s singleton class.
 *   2. Defines a private instance method on `c` with the same `name`, `func`, and `aspec`.
 *   3. Clears the method cache for `name` in both contexts.
 */
MRB_API void
mrb_define_module_function_id(mrb_state *mrb, struct RClass *c, mrb_sym name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_class_method_id(mrb, c, name, func, aspec);
  mrb_define_private_method_id(mrb, c, name, func, aspec);
}

/*
 * Defines a module function for a module `c` using a C string for the name.
 * A module function is a shorthand for defining a method that is both a
 * public class method (callable on the module itself) and a private
 * instance method (callable within the context of classes that include the module).
 *
 * @param mrb The mruby state.
 * @param c The module (`RClass*` where `c->tt` should be `MRB_TT_MODULE`)
 *          for which to define the module function.
 * @param name The C string name of the module function. This name will be interned.
 * @param func The C function (mrb_func_t) that implements the function.
 * @param aspec The argument specification for the function.
 * @sideeffect
 *   1. Interns the `name` string to a symbol.
 *   2. Calls `mrb_define_module_function_id` with the interned symbol, which in turn:
 *      a. Defines a public class method on `c`.
 *      b. Defines a private instance method on `c`.
 *   3. Clears the method cache for the method name in both contexts.
 */
MRB_API void
mrb_define_module_function(mrb_state *mrb, struct RClass *c, const char *name, mrb_func_t func, mrb_aspec aspec)
{
  mrb_define_module_function_id(mrb, c, mrb_intern_cstr(mrb, name), func, aspec);
}

#ifndef MRB_NO_METHOD_CACHE
/* clear whole method cache table */
MRB_API void
mrb_method_cache_clear(mrb_state *mrb)
{
  static const struct mrb_cache_entry ce_zero ={0};

  for (int i=0; i<MRB_METHOD_CACHE_SIZE; i++) {
    mrb->cache[i] = ce_zero;
  }
}

/* clear method cache for a class */
void
mrb_mc_clear_by_class(mrb_state *mrb, struct RClass *c)
{
  struct mrb_cache_entry *mc = mrb->cache;

  for (int i=0; i<MRB_METHOD_CACHE_SIZE; mc++,i++) {
    if (mc->c == c || mc->c0 == c) mc->c = NULL;
  }
}

static void
mc_clear_by_id(mrb_state *mrb, mrb_sym id)
{
  struct mrb_cache_entry *mc = mrb->cache;

  for (int i=0; i<MRB_METHOD_CACHE_SIZE; mc++,i++) {
    if (METHOD_MID(mc->m) == id) mc->c = NULL;
  }
}
#endif // MRB_NO_METHOD_CACHE

mrb_method_t
mrb_vm_find_method(mrb_state *mrb, struct RClass *c, struct RClass **cp, mrb_sym mid)
{
  mrb_method_t m;
#ifndef MRB_NO_METHOD_CACHE
  struct RClass *oc = c;
  int h = mrb_int_hash_func(mrb, ((intptr_t)oc) ^ mid) & (MRB_METHOD_CACHE_SIZE-1);
  struct mrb_cache_entry *mc = &mrb->cache[h];

  if (mc->c == c && METHOD_MID(mc->m) == mid) {
    *cp = mc->c0;
    return mc->m;
  }
#endif

  while (c) {
    mt_tbl *h = c->mt;

    if (h) {
      union mt_ptr ptr;
      mrb_sym ret = mt_get(mrb, h, mid, &ptr);
      if (ret) {
        if (ptr.proc == 0) break;
        *cp = c;
        m = create_method_value(mrb, ret, ptr);
#ifndef MRB_NO_METHOD_CACHE
        mc->c = oc;
        mc->c0 = c;
        mc->m = m;
#endif
        return m;
      }
    }
    c = c->super;
  }
  MRB_METHOD_FROM_PROC(m, NULL);
  return m;                  /* no method */
}

/*
 * Searches for a method in the method table of a class and its ancestors
 * within the context of the current virtual machine.
 * This function is primarily used internally by the VM and dispatch mechanism.
 *
 * @param mrb The mruby state.
 * @param cp  A pointer to a pointer to the class (`RClass*`) from which to start
 *            the method search. On successful find, the `RClass*` pointed to by `cp`
 *            is updated to the class where the method was actually found.
 * @param mid The symbol ID of the method name to search for.
 * @return An `mrb_method_t` structure representing the found method.
 *         If the method is not found or is undefined, the returned `mrb_method_t`
 *         will have its `proc` field set to NULL (use `MRB_METHOD_UNDEF_P` to check).
 * @sideeffect If the method is found and method caching is enabled (i.e.,
 *             `MRB_NO_METHOD_CACHE` is not defined), this function will update
 *             the VM's method cache with the found method for the original class
 *             in `*cp` and the method ID `mid`.
 */
MRB_API mrb_method_t
mrb_method_search_vm(mrb_state *mrb, struct RClass **cp, mrb_sym mid)
{
  return mrb_vm_find_method(mrb, *cp, cp, mid);
}

/*
 * Searches for a method in a class `c` and its ancestors.
 * This is a higher-level wrapper around `mrb_method_search_vm`.
 *
 * @param mrb The mruby state.
 * @param c The class (`RClass*`) in which to start the search.
 * @param mid The symbol ID (`mrb_sym`) of the method name.
 * @return An `mrb_method_t` structure for the found method.
 * @raise NameError if the method specified by `mid` is not found or is undefined
 *        in class `c` or its ancestors.
 * @sideeffect May update the method cache if the method is found (via the
 *             internal call to `mrb_method_search_vm`).
 */
MRB_API mrb_method_t
mrb_method_search(mrb_state *mrb, struct RClass *c, mrb_sym mid)
{
  mrb_method_t m;

  m = mrb_method_search_vm(mrb, &c, mid);
  if (MRB_METHOD_UNDEF_P(m)) {
    mrb_name_error(mrb, mid, "undefined method '%n' for class %C", mid, c);
  }
  return m;
}

#define ONSTACK_ALLOC_MAX 32

static mrb_sym
prepare_name_common(mrb_state *mrb, mrb_sym sym, const char *prefix, const char *suffix)
{
  char onstack[ONSTACK_ALLOC_MAX];
  mrb_int sym_len;
  const char *sym_str = mrb_sym_name_len(mrb, sym, &sym_len);
  size_t prefix_len = prefix ? strlen(prefix) : 0;
  size_t suffix_len = suffix ? strlen(suffix) : 0;
  size_t name_len = sym_len + prefix_len + suffix_len;
  char *buf = name_len > sizeof(onstack) ? (char*)mrb_alloca(mrb, name_len) : onstack;
  char *p = buf;

  if (prefix_len > 0) {
    memcpy(p, prefix, prefix_len);
    p += prefix_len;
  }

  memcpy(p, sym_str, sym_len);
  p += sym_len;

  if (suffix_len > 0) {
    memcpy(p, suffix, suffix_len);
  }

  return mrb_intern(mrb, buf, name_len);
}

static mrb_value
prepare_ivar_name(mrb_state *mrb, mrb_sym sym)
{
  sym = prepare_name_common(mrb, sym, "@", NULL);
  mrb_iv_name_sym_check(mrb, sym);
  return mrb_symbol_value(sym);
}

static mrb_sym
prepare_writer_name(mrb_state *mrb, mrb_sym sym)
{
  return prepare_name_common(mrb, sym, NULL, "=");
}

static mrb_value
mod_attr_define(mrb_state *mrb, mrb_value mod, mrb_value (*accessor)(mrb_state*, mrb_value), mrb_sym (*access_name)(mrb_state*, mrb_sym))
{
  struct RClass *c = mrb_class_ptr(mod);
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);

  int ai = mrb_gc_arena_save(mrb);
  for (int i=0; i<argc; i++) {
    mrb_value name;
    mrb_sym method;
    struct RProc *p;
    mrb_method_t m;

    method = to_sym(mrb, argv[i]);
    name = prepare_ivar_name(mrb, method);
    if (access_name) {
      method = access_name(mrb, method);
    }

    p = mrb_proc_new_cfunc_with_env(mrb, accessor, 1, &name);
    MRB_METHOD_FROM_PROC(m, p);
    mrb_define_method_raw(mrb, c, method, m);
    mrb_gc_arena_restore(mrb, ai);
  }
  return mrb_nil_value();
}

static mrb_value
attr_reader(mrb_state *mrb, mrb_value obj)
{
  mrb_value name = mrb_proc_cfunc_env_get(mrb, 0);
  return mrb_iv_get(mrb, obj, to_sym(mrb, name));
}

static mrb_value
mrb_mod_attr_reader(mrb_state *mrb, mrb_value mod)
{
  return mod_attr_define(mrb, mod, attr_reader, NULL);
}

static mrb_value
attr_writer(mrb_state *mrb, mrb_value obj)
{
  mrb_value name = mrb_proc_cfunc_env_get(mrb, 0);
  mrb_value val = mrb_get_arg1(mrb);

  mrb_iv_set(mrb, obj, to_sym(mrb, name), val);
  return val;
}

static mrb_value
mrb_mod_attr_writer(mrb_state *mrb, mrb_value mod)
{
  return mod_attr_define(mrb, mod, attr_writer, prepare_writer_name);
}

static mrb_value
mrb_mod_attr_accessor(mrb_state *mrb, mrb_value mod)
{
  mrb_mod_attr_reader(mrb, mod);
  return mrb_mod_attr_writer(mrb, mod);
}

static mrb_value
mrb_instance_alloc(mrb_state *mrb, mrb_value cv)
{
  struct RClass *c = mrb_class_ptr(cv);
  enum mrb_vtype ttype = MRB_INSTANCE_TT(c);

  if (c->tt == MRB_TT_SCLASS)
    mrb_raise(mrb, E_TYPE_ERROR, "can't create instance of singleton class");

  if (c == mrb->nil_class || c == mrb->false_class) {
    mrb_assert(ttype == 0);
  }
  else if (ttype == 0) {
    ttype = MRB_TT_OBJECT;
  }
  if (MRB_UNDEF_ALLOCATOR_P(c)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "allocator undefined for %v", cv);
  }
  if (ttype <= MRB_TT_CPTR) {
    mrb_raisef(mrb, E_TYPE_ERROR, "can't create instance of %v", cv);
  }

  struct RObject *o = (struct RObject*)mrb_obj_alloc(mrb, ttype, c);
  return mrb_obj_value(o);
}

/*
 *  call-seq:
 *     class.new(args, ...)    ->  obj
 *
 *  Creates a new object of <i>class</i>'s class, then
 *  invokes that object's <code>initialize</code> method,
 *  passing it <i>args</i>. This is the method that ends
 *  up getting called whenever an object is constructed using
 *  `.new`.
 *
 */

mrb_value
mrb_instance_new(mrb_state *mrb, mrb_value cv)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_value blk;
  mrb_sym init;

  mrb_get_args(mrb, "*!&", &argv, &argc, &blk);
  mrb_value obj = mrb_instance_alloc(mrb, cv);
  init = MRB_SYM(initialize);
  if (!mrb_func_basic_p(mrb, obj, init, mrb_do_nothing)) {
    mrb_funcall_with_block(mrb, obj, init, argc, argv, blk);
  }
  return obj;
}

/*
 * Creates a new instance of class `c` and initializes it by calling its `initialize` method.
 *
 * This function first allocates a new object of the given class `c`.
 * Then, it calls the `initialize` method on this new object, passing
 * `argc` and `argv` as arguments. If the `initialize` method is not
 * explicitly defined or is the default (empty) one, it is not called.
 *
 * @param mrb The mruby state.
 * @param c A pointer to the `RClass` structure of the class to instantiate.
 * @param argc The number of arguments to pass to the `initialize` method.
 * @param argv A pointer to an array of `mrb_value` arguments for `initialize`.
 * @return The newly created and initialized `mrb_value` object.
 * @raise TypeError if `c` is a singleton class, or if its allocator is undefined,
 *                  or if it's a built-in type that cannot be instantiated this way
 *                  (e.g., `MRB_TT_CPTR`). This check occurs in `mrb_instance_alloc`.
 * @sideeffect
 *   1. Allocates a new object on the mruby heap.
 *   2. Calls the `initialize` method of the new object if it's a user-defined one.
 *      This `initialize` call can have arbitrary side effects.
 */
MRB_API mrb_value
mrb_obj_new(mrb_state *mrb, struct RClass *c, mrb_int argc, const mrb_value *argv)
{
  mrb_value obj = mrb_instance_alloc(mrb, mrb_obj_value(c));
  mrb_sym mid = MRB_SYM(initialize);

  if (!mrb_func_basic_p(mrb, obj, mid, mrb_do_nothing)) {
    mrb_funcall_argv(mrb, obj, mid, argc, argv);
  }
  return obj;
}

static mrb_value
mrb_class_initialize(mrb_state *mrb, mrb_value obj)
{
  struct RClass *c = mrb_class_ptr(obj);

  if (c->iv) {
    mrb_raise(mrb, E_TYPE_ERROR, "already initialized class");
  }

  mrb_value a, b;
  mrb_get_args(mrb, "|C&", &a, &b);
  if (!mrb_nil_p(b)) {
    mrb_yield_with_class(mrb, b, 1, &obj, obj, c);
  }
  return obj;
}

static mrb_value
mrb_class_new_class(mrb_state *mrb, mrb_value cv)
{
  mrb_value super, blk;
  mrb_int n = mrb_get_args(mrb, "|C&", &super, &blk);

  if (n == 0) {
    super = mrb_obj_value(mrb->object_class);
  }
  mrb_value new_class = mrb_obj_value(mrb_class_new(mrb, mrb_class_ptr(super)));
  mrb_sym mid = MRB_SYM(initialize);
  if (mrb_func_basic_p(mrb, new_class, mid, mrb_class_initialize)) {
    mrb_class_initialize(mrb, new_class);
  }
  else {
    mrb_funcall_with_block(mrb, new_class, mid, n, &super, blk);
  }
  mrb_class_inherited(mrb, mrb_class_ptr(super), mrb_class_ptr(new_class));
  return new_class;
}

static mrb_value
mrb_class_superclass(mrb_state *mrb, mrb_value klass)
{
  struct RClass *c = mrb_class_ptr(klass);

  c = find_origin(c)->super;
  while (c && c->tt == MRB_TT_ICLASS) {
    c = find_origin(c)->super;
  }
  if (!c) return mrb_nil_value();
  return mrb_obj_value(c);
}

static mrb_value
mrb_do_nothing(mrb_state *mrb, mrb_value cv)
{
  return mrb_nil_value();
}

static mrb_value
mrb_bob_not(mrb_state *mrb, mrb_value cv)
{
  return mrb_bool_value(!mrb_test(cv));
}

/* 15.3.1.3.1  */
/* 15.3.1.3.10 */
/* 15.3.1.3.11 */
/*
 *  call-seq:
 *     obj == other        -> true or false
 *     obj.equal?(other)   -> true or false
 *     obj.eql?(other)     -> true or false
 *
 *  Equality---At the <code>Object</code> level, <code>==</code> returns
 *  <code>true</code> only if <i>obj</i> and <i>other</i> are the
 *  same object. Typically, this method is overridden in descendant
 *  classes to provide class-specific meaning.
 *
 *  Unlike <code>==</code>, the <code>equal?</code> method should never be
 *  overridden by subclasses: it is used to determine object identity
 *  (that is, <code>a.equal?(b)</code> iff <code>a</code> is the same
 *  object as <code>b</code>).
 *
 *  The <code>eql?</code> method returns <code>true</code> if
 *  <i>obj</i> and <i>anObject</i> have the same value. Used by
 *  <code>Hash</code> to test members for equality.  For objects of
 *  class <code>Object</code>, <code>eql?</code> is synonymous with
 *  <code>==</code>. Subclasses normally continue this tradition, but
 *  there are exceptions. <code>Numeric</code> types, for example,
 *  perform type conversion across <code>==</code>, but not across
 *  <code>eql?</code>, so:
 *
 *     1 == 1.0     #=> true
 *     1.eql? 1.0   #=> false
 */
mrb_value
mrb_obj_equal_m(mrb_state *mrb, mrb_value self)
{
  mrb_value arg = mrb_get_arg1(mrb);

  return mrb_bool_value(mrb_obj_equal(mrb, self, arg));
}

/*
 * Checks if instances of a class `c` (or its ancestors) respond to a given method.
 *
 * This function searches for the method `mid` in the method table of class `c`
 * and its ancestor classes and included modules.
 *
 * @param mrb The mruby state.
 * @param c The `RClass*` representing the class of the object.
 * @param mid The symbol ID (`mrb_sym`) of the method name.
 * @return `TRUE` if an object of class `c` would respond to the method `mid`
 *         (i.e., the method is found and not undefined).
 *         `FALSE` otherwise.
 * @sideeffect May update the method cache if the method is found (due to the
 *             internal call to `mrb_method_search_vm`).
 */
MRB_API mrb_bool
mrb_obj_respond_to(mrb_state *mrb, struct RClass* c, mrb_sym mid)
{
  mrb_method_t m = mrb_method_search_vm(mrb, &c, mid);

  if (MRB_METHOD_UNDEF_P(m)) {
    return FALSE;
  }
  return TRUE;
}

/*
 * Checks if a given mruby object `obj` responds to a method specified by `mid`.
 *
 * This function first determines the class of `obj` and then calls
 * `mrb_obj_respond_to` to perform the method lookup.
 *
 * @param mrb The mruby state.
 * @param obj The `mrb_value` object to check.
 * @param mid The symbol ID (`mrb_sym`) of the method name.
 * @return `TRUE` if the object `obj` responds to the method `mid`, `FALSE` otherwise.
 * @sideeffect This function calls `mrb_class(mrb, obj)` which might have side effects
 *             if `obj` is a proxy object or has unusual class resolution.
 *             It also has the side effects of `mrb_obj_respond_to` (e.g., method
 *             cache updates).
 */
MRB_API mrb_bool
mrb_respond_to(mrb_state *mrb, mrb_value obj, mrb_sym mid)
{
  return mrb_obj_respond_to(mrb, mrb_class(mrb, obj), mid);
}

/*
 * Returns the name (path) of a class or module `c`.
 *
 * If the class/module has a cached name (typically set when it's assigned to a
 * constant), that name is returned.
 * For top-level classes/modules, this is their direct name.
 * For nested classes/modules, it's the fully qualified name (e.g., `Outer::Inner`).
 * If no name is cached (e.g., for anonymous classes/modules), this function
 * attempts to find or construct a path representation (e.g., `#<Class:0xPTR>`).
 *
 * @param mrb The mruby state.
 * @param c The `RClass*` structure of the class or module.
 * @return An `mrb_value` (String) representing the path of the class/module.
 *         - If a name is cached as a symbol (toplevel), it returns the symbol's string representation.
 *         - If a name is cached as a string (nested), it returns a duplicate of that string.
 *         - If no name is cached, it calls `mrb_class_find_path` to get or construct one.
 *         The returned string is suitable for modification by the caller as it's either
 *         a new string or a duplicate of an internal one.
 * @sideeffect May allocate a new string on the mruby heap if duplication or construction
 *             of the path string is necessary.
 */
MRB_API mrb_value
mrb_class_path(mrb_state *mrb, struct RClass *c)
{
  mrb_sym nsym = MRB_SYM(__classname__);
  mrb_value path = mrb_obj_iv_get(mrb, (struct RObject*)c, nsym);

  if (mrb_nil_p(path)) {
    /* no name (yet) */
    return mrb_class_find_path(mrb, c);
  }
  else if (mrb_symbol_p(path)) {
    /* toplevel class/module */
    return mrb_sym_str(mrb, mrb_symbol(path));
  }
  return mrb_str_dup(mrb, path);
}

/*
 * Returns the "real" class of a given class pointer `cl`.
 *
 * The "real" class is the underlying, non-singleton, non-iclass `RClass`.
 * This function traverses up the superclass chain, skipping any `MRB_TT_SCLASS`
 * (singleton class) or `MRB_TT_ICLASS` (module inclusion class / i-class)
 * encountered, until it finds an `RClass` that is a `MRB_TT_CLASS` or
 * `MRB_TT_MODULE`.
 *
 * @param cl A pointer to an `RClass` structure.
 * @return A pointer to the "real" `RClass` structure.
 *         Returns `NULL` if the input `cl` is `NULL` or if the superclass
 *         chain leads to `NULL` before a real class is found (which typically
 *         should not happen for valid class structures).
 */
MRB_API struct RClass*
mrb_class_real(struct RClass* cl)
{
  if (cl == 0) return NULL;
  while ((cl->tt == MRB_TT_SCLASS) || (cl->tt == MRB_TT_ICLASS)) {
    cl = cl->super;
    if (cl == 0) return NULL;
  }
  return cl;
}

/*
 * Returns the name of a class/module `c` as a C string.
 *
 * This function provides a C string representation of the class/module name.
 * It typically calls `mrb_class_path` internally and then returns a pointer
 * to the string data of the resulting `mrb_value`.
 *
 * @param mrb The mruby state.
 * @param c The `RClass*` structure of the class or module.
 * @return A `const char*` pointing to the name of the class/module.
 *         This could be the class name, a fully qualified name for nested
 *         modules/classes, or a representation like "#<Class:0xPTR>" for
 *         anonymous ones. Returns `NULL` if `c` is `NULL`.
 * @sideeffect This function may allocate memory on the mruby heap if `mrb_class_path`
 *             needs to construct the name string (e.g., for anonymous classes or
 *             if the name is not cached). The returned pointer is to the internal
 *             buffer of an `mrb_value` string; its validity is tied to the lifetime
 *             of that string value, which is subject to garbage collection unless
 *             explicitly protected.
 */
MRB_API const char*
mrb_class_name(mrb_state *mrb, struct RClass* c)
{
  if (c == NULL) return NULL;

  mrb_value name = class_name_str(mrb, c);
  return RSTRING_PTR(name);
}

/*
 * Returns the class name of a given mruby object `obj` as a C string.
 *
 * This function first retrieves the class of the object using `mrb_obj_class`
 * (which gets the "real" class, traversing SCLASS/ICLASS), and then
 * gets the name of that class using `mrb_class_name`.
 *
 * @param mrb The mruby state.
 * @param obj The `mrb_value` object whose class name is to be retrieved.
 * @return A `const char*` pointing to the name of the object's class.
 *         See `mrb_class_name` for details on the format of the name.
 * @sideeffect This function has the combined side effects of `mrb_obj_class`
 *             and `mrb_class_name`. This may include memory allocation on the
 *             mruby heap for constructing the class name string or for class
 *             structure creation if the object's class or metaclass components
 *             are not yet fully initialized. The lifetime of the returned pointer
 *             is tied to the underlying string `mrb_value`.
 */
MRB_API const char*
mrb_obj_classname(mrb_state *mrb, mrb_value obj)
{
  return mrb_class_name(mrb, mrb_obj_class(mrb, obj));
}

/*
 * Ensures a class can be derived from super.
 *
 * \param super a reference to an object.
 * \exception TypeError if \a super is not a Class or \a super is a singleton class.
 */
static void
mrb_check_inheritable(mrb_state *mrb, struct RClass *super)
{
  if (super->tt != MRB_TT_CLASS) {
    mrb_raisef(mrb, E_TYPE_ERROR, "superclass must be a Class (%C given)", super);
  }
  if (super->tt == MRB_TT_SCLASS) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't make subclass of singleton class");
  }
  if (super == mrb->class_class) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't make subclass of Class");
  }
}

/*
 * Creates a new, unnamed class.
 *
 * This function is the core mechanism for creating new classes in mruby.
 * The created class will not have a name (i.e., it's anonymous) until it is
 * assigned to a constant.
 *
 * @param mrb The mruby state.
 * @param super A pointer to the `RClass` structure of the superclass.
 *              If `super` is `NULL`, `Object` (mrb->object_class) will be used
 *              as the superclass by default (though `boot_defclass` handles this).
 * @return A pointer to the `RClass` structure of the newly created class.
 * @raise TypeError if `super` is not a valid class to inherit from (e.g., it's a
 *                  singleton class, or it's the `Class` class itself).
 *                  This check is performed by `mrb_check_inheritable`.
 * @sideeffect
 *   1. Allocates a new `RClass` object on the mruby heap.
 *   2. Initializes its method table (`mt`).
 *   3. Sets its superclass to the provided `super` (or `Object` if `super` is `NULL`).
 *   4. Copies instance type information (`MRB_INSTANCE_TT`) and the
 *      `MRB_FL_UNDEF_ALLOCATE` flag from the superclass if `super` is provided.
 *   5. Creates and attaches a metaclass (singleton class) to the new class.
 */
MRB_API struct RClass*
mrb_class_new(mrb_state *mrb, struct RClass *super)
{
  if (super) {
    mrb_check_inheritable(mrb, super);
  }

  struct RClass *c = boot_defclass(mrb, super);
  if (super) {
    MRB_SET_INSTANCE_TT(c, MRB_INSTANCE_TT(super));
    c->flags |= super->flags & MRB_FL_UNDEF_ALLOCATE;
  }
  make_metaclass(mrb, c);

  return c;
}

/*
 * Creates a new, unnamed module.
 *
 * This function is the core mechanism for creating new modules in mruby.
 * The created module will not have a name (i.e., it's anonymous) until it is
 * assigned to a constant.
 *
 * @param mrb The mruby state.
 * @return A pointer to the `RClass` structure of the newly created module.
 *         The `tt` field of this `RClass` will be `MRB_TT_MODULE`.
 * @sideeffect
 *   1. Allocates a new `RClass` object on the mruby heap.
 *   2. Sets its class to `mrb->module_class`.
 *   3. Initializes its method table (`mt`).
 */
MRB_API struct RClass*
mrb_module_new(mrb_state *mrb)
{
  struct RClass *m = MRB_OBJ_ALLOC(mrb, MRB_TT_MODULE, mrb->module_class);
  boot_initmod(mrb, m);
  return m;
}

/*
 *  call-seq:
 *     obj.class    => class
 *
 *  Returns the class of <i>obj</i>, now preferred over
 *  <code>Object#type</code>, as an object's type in Ruby is only
 *  loosely tied to that object's class. This method must always be
 *  called with an explicit receiver, as <code>class</code> is also a
 *  reserved word in Ruby.
 *
 *     1.class      #=> Integer
 *     self.class   #=> Object
 */

/*
 * Returns the "real" class of an object.
 * This is the preferred way to get the class of an object in C extension code.
 * It correctly handles various mruby internal object structures by first calling
 * `mrb_class` (which gets the direct class, potentially a singleton or i-class)
 * and then `mrb_class_real` to resolve it to the actual user-facing class.
 *
 * @param mrb The mruby state.
 * @param obj The `mrb_value` object whose class is to be retrieved.
 * @return A pointer to the `RClass` structure of the object's "real" class.
 *         For example, for an instance of a regular class, it returns the class itself.
 *         For an instance of a class that includes modules, it still returns the class itself,
 *         not the i-classes. For a class object, it returns `Class`.
 * @sideeffect This function itself has minimal side effects, but the underlying
 *             `mrb_class` and `mrb_class_real` might perform lookups or traversals.
 */
MRB_API struct RClass*
mrb_obj_class(mrb_state *mrb, mrb_value obj)
{
  return mrb_class_real(mrb_class(mrb, obj));
}

/*
 * Defines an alias for an existing method within a class or module `c`.
 * The new method `a` will be an alias of the old method `b`.
 *
 * @param mrb The mruby state.
 * @param c The class or module (`RClass*`) in which to define the alias.
 * @param a The symbol ID (`mrb_sym`) for the new method name (the alias).
 * @param b The symbol ID (`mrb_sym`) for the original method name to be aliased.
 * @return This function does not return a value.
 * @raise NameError if the original method `b` is not found in class `c` or its ancestors.
 * @sideeffect
 *   1. Searches for the original method `b` in class `c` and its ancestors.
 *   2. If `b` is found:
 *      a. If `b` is a C function (`MRB_METHOD_CFUNC_P` is true), or if `b` is already
 *         an alias proc (`MRB_PROC_ALIAS_P` is true for the proc body), the method `m`
 *         (representing `b`) is directly used for the new alias `a`.
 *      b. If `b` is a Ruby-defined method (a non-CFUNC, non-alias `RProc`), a new `RProc`
 *         of type `MRB_PROC_ALIAS` is created. This new proc stores the original
 *         method's symbol `b` and its `RProc` as its `upper`. This ensures that
 *         the alias `a` continues to point to the definition of `b` at the time of
 *         aliasing, even if `b` is later redefined. The visibility of `b` is copied
 *         to this new alias proc.
 *      c. The method (either the original `m` or the new alias proc) is then defined
 *         in class `c` under the new name `a` using `mrb_define_method_raw`.
 *   3. The method cache for the new alias name `a` is cleared.
 *   4. If `a` and `b` are the same, the function does nothing and returns early.
 */
MRB_API void
mrb_alias_method(mrb_state *mrb, struct RClass *c, mrb_sym a, mrb_sym b)
{
  if (a == b) return;
  mrb_method_t m = mrb_method_search(mrb, c, b);

  if (!MRB_METHOD_CFUNC_P(m)) {
    const struct RProc *p = MRB_METHOD_PROC(m);
    if (!MRB_PROC_CFUNC_P(p) && !MRB_PROC_ALIAS_P(p)) {
      struct RProc *pnew = MRB_OBJ_ALLOC(mrb, MRB_TT_PROC, mrb->proc_class);
      int vis = MRB_METHOD_VISIBILITY(m);

      pnew->body.mid = b;
      pnew->upper = p;
      pnew->e.env = NULL;
      pnew->flags |= MRB_PROC_ALIAS;
      MRB_METHOD_FROM_PROC(m, pnew);
      MRB_METHOD_SET_VISIBILITY(m, vis);
    }
  }
  mrb_define_method_raw(mrb, c, a, m);
}

/*
 * Defines an alias for an existing method within a class or module `klass`.
 * This version takes C string names for both the new alias and the original method.
 *
 * @param mrb The mruby state.
 * @param klass The class or module (`RClass*`) in which to define the alias.
 * @param name1 The C string for the new method name (the alias).
 * @param name2 The C string for the original method name to be aliased.
 * @return This function does not return a value.
 * @raise NameError if the original method `name2` (after being interned) is not found.
 * @sideeffect
 *   1. Interns both `name1` and `name2` to get their `mrb_sym` IDs.
 *   2. Calls `mrb_alias_method` with the class `klass` and the obtained symbols.
 *   (See `mrb_alias_method` for further side effects like method table modification
 *   and cache clearing).
 */
MRB_API void
mrb_define_alias(mrb_state *mrb, struct RClass *klass, const char *name1, const char *name2)
{
  mrb_alias_method(mrb, klass, mrb_intern_cstr(mrb, name1), mrb_intern_cstr(mrb, name2));
}

/*
 * Defines an alias for an existing method within a class or module `klass`,
 * using symbol IDs for both names.
 *
 * This function is a direct call to `mrb_alias_method`.
 *
 * @param mrb The mruby state.
 * @param klass The class or module (`RClass*`) in which to define the alias.
 * @param a The symbol ID (`mrb_sym`) for the new method name (the alias).
 * @param b The symbol ID (`mrb_sym`) for the original method name to be aliased.
 * @return This function does not return a value.
 * @raise NameError if the original method `b` is not found in `klass` or its ancestors.
 * @sideeffect Calls `mrb_alias_method`, which modifies the method table of `klass`
 *             and clears the method cache for the new alias `a`.
 *             (See `mrb_alias_method` for more detailed side effects).
 */
MRB_API void
mrb_define_alias_id(mrb_state *mrb, struct RClass *klass, mrb_sym a, mrb_sym b)
{
  mrb_alias_method(mrb, klass, a, b);
}

/*
 * call-seq:
 *   mod.to_s   -> string
 *
 * Return a string representing this module or class. For basic
 * classes and modules, this is the name. For singletons, we
 * show information on the thing we're attached to as well.
 */

mrb_value
mrb_mod_to_s(mrb_state *mrb, mrb_value klass)
{
  if (mrb_sclass_p(klass)) {
    mrb_value v = mrb_iv_get(mrb, klass, MRB_SYM(__attached__));
    mrb_value str = mrb_str_new_lit(mrb, "#<Class:");

    if (class_ptr_p(v)) {
      mrb_str_cat_str(mrb, str, mrb_inspect(mrb, v));
    }
    else {
      mrb_str_cat_str(mrb, str, mrb_any_to_s(mrb, v));
    }
    return mrb_str_cat_lit(mrb, str, ">");
  }
  else {
    return class_name_str(mrb, mrb_class_ptr(klass));
  }
}

static mrb_value
mrb_mod_alias(mrb_state *mrb, mrb_value mod)
{
  struct RClass *c = mrb_class_ptr(mod);
  mrb_sym new_name, old_name;

  mrb_get_args(mrb, "nn", &new_name, &old_name);
  mrb_alias_method(mrb, c, new_name, old_name);
  mrb_method_added(mrb, c, new_name);
  return mod;
}

static void
undef_method(mrb_state *mrb, struct RClass *c, mrb_sym a)
{
  mrb_method_t m;
  mrb_sym undefined;
  mrb_value recv;

  MRB_METHOD_FROM_PROC(m, NULL);
  mrb_define_method_raw(mrb, c, a, m);
  if (c->tt == MRB_TT_SCLASS) {
    undefined = MRB_SYM(singleton_method_undefined);
    recv = mrb_iv_get(mrb, mrb_obj_value(c), MRB_SYM(__attached__));
  }
  else {
    undefined = MRB_SYM(method_undefined);
    recv = mrb_obj_value(c);
  }
  if (!mrb_func_basic_p(mrb, recv, undefined, mrb_do_nothing)) {
    mrb_value sym = mrb_symbol_value(a);
    mrb_funcall_argv(mrb, recv, undefined, 1, &sym);
  }
}

/*
 * Undefines a method specified by symbol `a` in class/module `c`.
 *
 * This action prevents objects of class `c` (or classes including module `c`)
 * from responding to the method `a`. If the method was inherited, the version
 * in the superclass will no longer be accessible through `c`.
 * A special "undefined" entry is added to `c`'s method table for `a`.
 *
 * @param mrb The mruby state.
 * @param c The class or module (`RClass*`) in which to undefine the method.
 * @param a The symbol ID (`mrb_sym`) of the method to undefine.
 * @return This function does not return a value.
 * @raise NameError if the method `a` is not defined in `c` or its ancestors
 *        (i.e., if `c` does not respond to `a` before undefinition).
 * @sideeffect
 *   1. Modifies the method table of `c` by adding an entry that marks `a` as undefined.
 *   2. Triggers `method_undefined` (for regular classes/modules) or
 *      `singleton_method_undefined` (for singleton classes) callbacks on `c`
 *      or its attached object, if these hooks are defined.
 *   3. Clears the method cache for the method symbol `a`.
 */
MRB_API void
mrb_undef_method_id(mrb_state *mrb, struct RClass *c, mrb_sym a)
{
  if (!mrb_obj_respond_to(mrb, c, a)) {
    mrb_name_error(mrb, a, "undefined method '%n' for class '%C'", a, c);
  }
  undef_method(mrb, c, a);
}

/*
 * Undefines a method specified by a C string `name` in class/module `c`.
 *
 * This function interns the C string `name` to a symbol and then calls
 * `mrb_undef_method_id` to perform the undefinition.
 *
 * @param mrb The mruby state.
 * @param c The class or module (`RClass*`) in which to undefine the method.
 * @param name The C string name of the method to undefine.
 * @return This function does not return a value.
 * @raise NameError if the method `name` (after interned to a symbol) is not
 *        defined in `c` or its ancestors.
 * @sideeffect
 *   1. Interns the `name` string.
 *   2. All side effects of `mrb_undef_method_id` apply (method table modification,
 *      callback triggering, cache clearing).
 */
MRB_API void
mrb_undef_method(mrb_state *mrb, struct RClass *c, const char *name)
{
  undef_method(mrb, c, mrb_intern_cstr(mrb, name));
}

/*
 * Undefines a class method specified by symbol `name` for class/module `c`.
 *
 * Class methods are singleton methods of the class object. This function
 * retrieves the singleton class of `c` and then undefines the method there.
 *
 * @param mrb The mruby state.
 * @param c The class or module (`RClass*`) whose class method is to be undefined.
 * @param name The symbol ID (`mrb_sym`) of the class method to undefine.
 * @return This function does not return a value.
 * @raise TypeError if `c` cannot have a singleton class (e.g., if it's an
 *        immediate value, though highly unlikely for an `RClass*`).
 * @raise NameError if the class method `name` is not defined on `c`.
 * @sideeffect
 *   1. Retrieves or creates the singleton class of `c`.
 *   2. All side effects of `mrb_undef_method_id` apply to this singleton class
 *      (method table modification, callback triggering, cache clearing).
 */
MRB_API void
mrb_undef_class_method_id(mrb_state *mrb, struct RClass *c, mrb_sym name)
{
  mrb_undef_method_id(mrb,  mrb_class_ptr(mrb_singleton_class(mrb, mrb_obj_value(c))), name);
}

/*
 * Undefines a class method specified by a C string `name` for class/module `c`.
 *
 * This function interns the C string `name` to a symbol and then calls
 * `mrb_undef_class_method_id` (which undefines the method on `c`'s singleton class).
 *
 * @param mrb The mruby state.
 * @param c The class or module (`RClass*`) whose class method is to be undefined.
 * @param name The C string name of the class method to undefine.
 * @return This function does not return a value.
 * @raise TypeError if `c` cannot have a singleton class.
 * @raise NameError if the class method `name` (after interned) is not defined on `c`.
 * @sideeffect
 *   1. Interns the `name` string.
 *   2. Retrieves or creates the singleton class of `c`.
 *   3. All side effects of `mrb_undef_method_id` apply to this singleton class.
 */
MRB_API void
mrb_undef_class_method(mrb_state *mrb, struct RClass *c, const char *name)
{
  mrb_undef_method(mrb,  mrb_class_ptr(mrb_singleton_class(mrb, mrb_obj_value(c))), name);
}

/*
 * Removes a method specified by symbol `mid` directly from class/module `c0`.
 *
 * Unlike `mrb_undef_method_id`, this function only removes the method definition
 * from the specified class/module `c0`. If the method is defined in an ancestor,
 * that inherited method will become active after the removal from `c0`.
 *
 * @param mrb The mruby state.
 * @param c0 The class or module (`RClass*`) from which to remove the method.
 *           The method is removed from the "origin" of this class if it's an ICLASS/SCLASS.
 * @param mid The symbol ID (`mrb_sym`) of the method to remove.
 * @return This function does not return a value.
 * @raise NameError if the method `mid` is not defined directly in the method
 *        table of `c0` (or its origin).
 * @sideeffect
 *   1. Removes the method entry for `mid` from `c0`'s (or its origin's) method table.
 *   2. Triggers `method_removed` (for regular classes/modules) or
 *      `singleton_method_removed` (for singleton classes) callbacks on `c0`
 *      or its attached object, if these hooks are defined.
 *   3. Clears the method cache for the method symbol `mid`.
 */
MRB_API void
mrb_remove_method(mrb_state *mrb, struct RClass *c0, mrb_sym mid)
{
  struct RClass *c = c0;
  MRB_CLASS_ORIGIN(c);
  mt_tbl *h = c->mt;

  if (h && mt_del(mrb, h, mid)) {
    mrb_sym removed;
    mrb_value recv;

    mc_clear_by_id(mrb, mid);
    if (c0->tt == MRB_TT_SCLASS) {
      removed = MRB_SYM(singleton_method_removed);
      recv = mrb_iv_get(mrb, mrb_obj_value(c0), MRB_SYM(__attached__));
    }
    else {
      removed = MRB_SYM(method_removed);
      recv = mrb_obj_value(c0);
    }
    if (!mrb_func_basic_p(mrb, recv, removed, mrb_do_nothing)) {
      mrb_value sym = mrb_symbol_value(mid);
      mrb_funcall_argv(mrb, recv, removed, 1, &sym);
    }
    return;
  }
  mrb_name_error(mrb, mid, "method '%n' not defined in %C", mid, c);
}

static mrb_value
mrb_mod_undef(mrb_state *mrb, mrb_value mod)
{
  struct RClass *c = mrb_class_ptr(mod);
  mrb_int argc;
  const mrb_value *argv;

  mrb_get_args(mrb, "*", &argv, &argc);
  while (argc--) {
    mrb_undef_method_id(mrb, c, to_sym(mrb, *argv));
    argv++;
  }
  return mrb_nil_value();
}

static void
check_const_name_sym(mrb_state *mrb, mrb_sym id)
{
  mrb_int len;
  const char *name = mrb_sym_name_len(mrb, id, &len);
  if (!mrb_const_name_p(mrb, name, len)) {
    mrb_name_error(mrb, id, "wrong constant name %n", id);
  }
}

static mrb_value
mrb_mod_const_defined(mrb_state *mrb, mrb_value mod)
{
  mrb_sym id;
  mrb_bool inherit = TRUE;

  mrb_get_args(mrb, "n|b", &id, &inherit);
  check_const_name_sym(mrb, id);
  if (inherit) {
    return mrb_bool_value(mrb_const_defined(mrb, mod, id));
  }
  return mrb_bool_value(mrb_const_defined_at(mrb, mod, id));
}

static mrb_value
mrb_const_get_sym(mrb_state *mrb, mrb_value mod, mrb_sym id)
{
  check_const_name_sym(mrb, id);
  return mrb_const_get(mrb, mod, id);
}

static mrb_value
mrb_mod_const_get(mrb_state *mrb, mrb_value mod)
{
  mrb_value path = mrb_get_arg1(mrb);

  if (mrb_symbol_p(path)) {
    /* const get with symbol */
    return mrb_const_get_sym(mrb, mod, mrb_symbol(path));
  }

  /* const get with class path string */
  mrb_ensure_string_type(mrb, path);

  char *ptr = RSTRING_PTR(path);
  mrb_int len = RSTRING_LEN(path);
  mrb_int off = 0;

  while (off < len) {
    mrb_int end = mrb_str_index_lit(mrb, path, "::", off);
    if (end == -1) end = len;
    mrb_sym id = mrb_intern(mrb, ptr+off, end-off);
    mod = mrb_const_get_sym(mrb, mod, id);
    if (end == len)
      off = end;
    else {
      off = end + 2;
      if (off == len) {         /* trailing "::" */
        mrb_name_error(mrb, id, "wrong constant name '%v'", path);
      }
    }
  }

  return mod;
}

static mrb_value
mrb_mod_const_set(mrb_state *mrb, mrb_value mod)
{
  mrb_sym id;
  mrb_value value;

  mrb_get_args(mrb, "no", &id, &value);
  check_const_name_sym(mrb, id);
  mrb_const_set(mrb, mod, id, value);
  return value;
}

static mrb_value
mrb_mod_remove_const(mrb_state *mrb, mrb_value mod)
{
  mrb_sym id;

  mrb_get_args(mrb, "n", &id);
  check_const_name_sym(mrb, id);

  mrb_value val = mrb_iv_remove(mrb, mod, id);
  if (mrb_undef_p(val)) {
    mrb_name_error(mrb, id, "constant %n not defined", id);
  }
  return val;
}

mrb_value
mrb_const_missing(mrb_state *mrb, mrb_value mod, mrb_sym sym)
{
  if (mrb_class_real(mrb_class_ptr(mod)) != mrb->object_class) {
    mrb_name_error(mrb, sym, "uninitialized constant %v::%n", mod, sym);
  }
  else {
    mrb_name_error(mrb, sym, "uninitialized constant %n", sym);
  }
  /* not reached */
  return mrb_nil_value();
}

mrb_value
mrb_mod_const_missing(mrb_state *mrb, mrb_value mod)
{
  mrb_sym sym;

  mrb_get_args(mrb, "n", &sym);
  mrb->c->ci->mid = 0;
  return mrb_const_missing(mrb, mod, sym);
}

/* 15.2.2.4.34 */
/*
 *  call-seq:
 *     mod.method_defined?(symbol)    -> true or false
 *
 *  Returns +true+ if the named method is defined by
 *  _mod_ (or its included modules and, if _mod_ is a class,
 *  its ancestors). Public and protected methods are matched.
 *
 *     module A
 *       def method1()  end
 *     end
 *     class B
 *       def method2()  end
 *     end
 *     class C < B
 *       include A
 *       def method3()  end
 *     end
 *
 *     A.method_defined? :method1    #=> true
 *     C.method_defined? "method1"   #=> true
 *     C.method_defined? "method2"   #=> true
 *     C.method_defined? "method3"   #=> true
 *     C.method_defined? "method4"   #=> false
 */

static mrb_value
mrb_mod_method_defined(mrb_state *mrb, mrb_value mod)
{
  mrb_sym id;

  mrb_get_args(mrb, "n", &id);
  return mrb_bool_value(mrb_obj_respond_to(mrb, mrb_class_ptr(mod), id));
}

void
mrb_method_added(mrb_state *mrb, struct RClass *c, mrb_sym mid)
{
  mrb_sym added;
  mrb_value recv = mrb_obj_value(c);

  if (c->tt == MRB_TT_SCLASS) {
    added = MRB_SYM(singleton_method_added);
    recv = mrb_iv_get(mrb, recv, MRB_SYM(__attached__));
  }
  else {
    added = MRB_SYM(method_added);
  }
  if (!mrb_func_basic_p(mrb, recv, added, mrb_do_nothing)) {
    mrb_value sym = mrb_symbol_value(mid);
    mrb_funcall_argv(mrb, recv, added, 1, &sym);
  }
}

mrb_value
define_method_m(mrb_state *mrb, struct RClass *c, int vis)
{
  mrb_sym mid;
  mrb_value proc = mrb_undef_value();
  mrb_value blk;

  mrb_get_args(mrb, "n|o&", &mid, &proc, &blk);
  switch (mrb_type(proc)) {
    case MRB_TT_PROC:
      blk = proc;
      break;
    case MRB_TT_UNDEF:
      /* ignored */
      break;
    default:
      mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %T (expected Proc)", proc);
      break;
  }
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }
  struct RProc *p = MRB_OBJ_ALLOC(mrb, MRB_TT_PROC, mrb->proc_class);
  mrb_proc_copy(mrb, p, mrb_proc_ptr(blk));
  p->flags |= MRB_PROC_STRICT;

  mrb_method_t m;
  MRB_METHOD_FROM_PROC(m, p);
  MRB_METHOD_SET_VISIBILITY(m, vis);
  mrb_define_method_raw(mrb, c, mid, m);
  mrb_method_added(mrb, c, mid);
  return mrb_symbol_value(mid);
}

mrb_value
mrb_mod_define_method_m(mrb_state *mrb, struct RClass *c)
{
  return define_method_m(mrb, c, MT_PUBLIC);
}

static mrb_value
mod_define_method(mrb_state *mrb, mrb_value self)
{
  return mrb_mod_define_method_m(mrb, mrb_class_ptr(self));
}

static mrb_value
top_define_method(mrb_state *mrb, mrb_value self)
{
  return define_method_m(mrb, mrb->object_class, MT_PRIVATE);
}

static mrb_value
mrb_mod_eqq(mrb_state *mrb, mrb_value mod)
{
  mrb_value obj = mrb_get_arg1(mrb);
  mrb_bool eqq = mrb_obj_is_kind_of(mrb, obj, mrb_class_ptr(mod));

  return mrb_bool_value(eqq);
}

static mrb_value
mrb_mod_dup(mrb_state *mrb, mrb_value self)
{
  mrb_value mod = mrb_obj_clone(mrb, self);
  mrb_obj_ptr(mod)->frozen = 0;
  return mod;
}

static mrb_value
mrb_mod_module_function(mrb_state *mrb, mrb_value mod)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_check_type(mrb, mod, MRB_TT_MODULE);

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0) {
    /* set MODFUNC SCOPE if implemented */
    return mod;
  }

  /* set PRIVATE method visibility if implemented */
  /* mrb_mod_dummy_visibility(mrb, mod); */

  struct RClass *rclass = mrb_class_ptr(mod);
  int ai = mrb_gc_arena_save(mrb);
  for (int i=0; i<argc; i++) {
    mrb_check_type(mrb, argv[i], MRB_TT_SYMBOL);

    mrb_sym mid = mrb_symbol(argv[i]);
    mrb_method_t m = mrb_method_search(mrb, rclass, mid);

    prepare_singleton_class(mrb, (struct RBasic*)rclass);
    MRB_METHOD_SET_VISIBILITY(m, MT_PUBLIC);
    mrb_define_method_raw(mrb, rclass->c, mid, m);
    mrb_gc_arena_restore(mrb, ai);
  }

  return mod;
}

static struct RClass*
mrb_singleton_class_clone(mrb_state *mrb, mrb_value obj)
{
  struct RClass *klass = mrb_basic_ptr(obj)->c;

  if (klass->tt != MRB_TT_SCLASS)
    return klass;
  else {
    /* copy singleton(unnamed) class */
    struct RClass *clone = (struct RClass*)mrb_obj_alloc(mrb, klass->tt, mrb->class_class);

    switch (mrb_type(obj)) {
    case MRB_TT_CLASS:
    case MRB_TT_SCLASS:
      break;
    default:
      clone->c = mrb_singleton_class_clone(mrb, mrb_obj_value(klass));
      break;
    }
    clone->super = klass->super;
    if (klass->iv) {
      mrb_iv_copy(mrb, mrb_obj_value(clone), mrb_obj_value(klass));
      mrb_obj_iv_set(mrb, (struct RObject*)clone, MRB_SYM(__attached__), obj);
    }
    if (klass->mt) {
      clone->mt = mt_copy(mrb, klass->mt);
    }
    else {
      clone->mt = mt_new(mrb);
    }
    clone->tt = MRB_TT_SCLASS;
    return clone;
  }
}

static void
copy_class(mrb_state *mrb, mrb_value dst, mrb_value src)
{
  struct RClass *dc = mrb_class_ptr(dst);
  struct RClass *sc = mrb_class_ptr(src);
  /* if the origin is not the same as the class, then the origin and
     the current class need to be copied */
  if (sc->flags & MRB_FL_CLASS_IS_PREPENDED) {
    struct RClass *c0 = sc->super;
    struct RClass *c1 = dc;

    /* copy prepended iclasses */
    while (!(c0->flags & MRB_FL_CLASS_IS_ORIGIN)) {
      c1->super = mrb_class_ptr(mrb_obj_dup(mrb, mrb_obj_value(c0)));
      c1 = c1->super;
      c0 = c0->super;
    }
    c1->super = mrb_class_ptr(mrb_obj_dup(mrb, mrb_obj_value(c0)));
    c1->super->flags |= MRB_FL_CLASS_IS_ORIGIN;
  }
  if (sc->mt) {
    if (sc->tt == MRB_TT_ICLASS && !(sc->flags & MRB_FL_CLASS_IS_ORIGIN)) {
      dc->mt = sc->mt;
    }
    else {
      dc->mt = mt_copy(mrb, sc->mt);
    }
  }
  dc->super = sc->super;
  dc->flags = sc->flags;
  dc->frozen = 0;
}

/* 15.3.1.3.16 */
mrb_value mrb_obj_init_copy(mrb_state *mrb, mrb_value self);

static void
init_copy(mrb_state *mrb, mrb_value dest, mrb_value obj)
{
  mrb_assert((mrb_type(dest) == mrb_type(obj)));
  switch (mrb_unboxed_type(obj)) {
    case MRB_TT_ICLASS:
      copy_class(mrb, dest, obj);
      return;
    case MRB_TT_CLASS:
    case MRB_TT_MODULE:
      copy_class(mrb, dest, obj);
      mrb_iv_copy(mrb, dest, obj);
      mrb_iv_remove(mrb, dest, MRB_SYM(__classname__));
      break;
    case MRB_TT_OBJECT:
    case MRB_TT_SCLASS:
    case MRB_TT_HASH:
    case MRB_TT_CDATA:
    case MRB_TT_EXCEPTION:
      mrb_iv_copy(mrb, dest, obj);
      break;
    case MRB_TT_ISTRUCT:
      mrb_istruct_copy(dest, obj);
      break;
#if !defined(MRB_NO_FLOAT) && defined(MRB_WORDBOX_NO_FLOAT_TRUNCATE)
    case MRB_TT_FLOAT:
      {
        struct RFloat *f = (struct RFloat*)mrb_obj_ptr(dest);
        f->f = mrb_float(obj);
      }
      break;
#endif
#ifdef MRB_USE_BIGINT
    case MRB_TT_BIGINT:
      mrb_bint_copy(mrb, dest, obj);
      break;
#endif
#ifdef MRB_USE_RATIONAL
    case MRB_TT_RATIONAL:
      mrb_rational_copy(mrb, dest, obj);
      break;
#endif
#ifdef MRB_USE_COMPLEX
    case MRB_TT_COMPLEX:
      mrb_complex_copy(mrb, dest, obj);
      break;
#endif

    default:
      break;
  }
  if (!mrb_func_basic_p(mrb, dest, MRB_SYM(initialize_copy), mrb_obj_init_copy)) {
    mrb_funcall_argv(mrb, dest, MRB_SYM(initialize_copy), 1, &obj);
  }
}

/* 15.3.1.3.8  */
/*
 *  call-seq:
 *     obj.clone -> an_object
 *
 *  Produces a shallow copy of <i>obj</i>---the instance variables of
 *  <i>obj</i> are copied, but not the objects they reference. Copies
 *  the frozen state of <i>obj</i>. See also the discussion
 *  under <code>Object#dup</code>.
 *
 *     class Klass
 *        attr_accessor :str
 *     end
 *     s1 = Klass.new      #=> #<Klass:0x401b3a38>
 *     s1.str = "Hello"    #=> "Hello"
 *     s2 = s1.clone       #=> #<Klass:0x401b3998 @str="Hello">
 *     s2.str[1,4] = "i"   #=> "i"
 *     s1.inspect          #=> "#<Klass:0x401b3a38 @str=\"Hi\">"
 *     s2.inspect          #=> "#<Klass:0x401b3998 @str=\"Hi\">"
 *
 *  This method may have class-specific behavior.  If so, that
 *  behavior will be documented under the #+initialize_copy+ method of
 *  the class.
 *
 *  Some Class(True False Nil Symbol Integer Float) Object  cannot clone.
 */
/*
 * Creates a shallow copy of the given object `self`.
 *
 * This function performs a shallow copy, meaning instance variables are copied,
 * but the objects they refer to are not duplicated. The frozen state of the
 * original object is also copied to the clone. If the object has a singleton
 * class, that singleton class is also cloned and associated with the new object.
 * After the new object is created and its basic state is copied, its
 * `initialize_copy` method is called with the original object as an argument,
 * allowing for class-specific adjustments to the cloning process.
 *
 * @param mrb The mruby state.
 * @param self The `mrb_value` object to clone.
 * @return A new `mrb_value` which is a clone of `self`.
 * @raise TypeError if `self` is an immediate value (e.g., Fixnum, Symbol in some
 *                  configurations) or if `self` is a singleton class itself, as these
 *                  cannot be cloned.
 * @sideeffect
 *   1. Allocates a new object on the mruby heap.
 *   2. Copies instance variables from `self` to the new object.
 *   3. If `self` has a singleton class, it is cloned and assigned to the new object.
 *      This involves further allocations and setup for the new singleton class.
 *   4. The `frozen` state of `self` is propagated to the clone.
 *   5. Calls the `initialize_copy` method on the newly created clone, passing `self`
 *      as an argument. This method can have arbitrary side effects.
 */
MRB_API mrb_value
mrb_obj_clone(mrb_state *mrb, mrb_value self)
{
  if (mrb_immediate_p(self)) {
    return self;
  }
  if (mrb_sclass_p(self)) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't clone singleton class");
  }
  struct RObject *p = (struct RObject*)mrb_obj_alloc(mrb, mrb_unboxed_type(self), mrb_obj_class(mrb, self));
  p->c = mrb_singleton_class_clone(mrb, self);
  mrb_field_write_barrier(mrb, (struct RBasic*)p, (struct RBasic*)p->c);

  mrb_value clone = mrb_obj_value(p);
  init_copy(mrb, clone, self);
  p->frozen = mrb_obj_ptr(self)->frozen;

  return clone;
}

/* 15.3.1.3.9  */
/*
 *  call-seq:
 *     obj.dup -> an_object
 *
 *  Produces a shallow copy of <i>obj</i>---the instance variables of
 *  <i>obj</i> are copied, but not the objects they reference.
 *  <code>dup</code> copies the frozen state of <i>obj</i>. See also
 *  the discussion under <code>Object#clone</code>. In general,
 *  <code>clone</code> and <code>dup</code> may have different semantics
 *  in descendant classes. While <code>clone</code> is used to duplicate
 *  an object, including its internal state, <code>dup</code> typically
 *  uses the class of the descendant object to create the new instance.
 *
 *  This method may have class-specific behavior.  If so, that
 *  behavior will be documented under the #+initialize_copy+ method of
 *  the class.
 */

/*
 * Creates a shallow copy of the given object `obj`.
 *
 * This function performs a shallow copy, meaning instance variables are copied,
 * but the objects they refer to are not duplicated. Unlike `mrb_obj_clone`,
 * `mrb_obj_dup` does *not* copy the frozen state of the original object; the
 * duplicated object is always unfrozen. Also, it does not copy the singleton class.
 * After the new object is created and its basic state is copied, its
 * `initialize_copy` method is called with the original object as an argument.
 *
 * @param mrb The mruby state.
 * @param obj The `mrb_value` object to duplicate.
 * @return A new `mrb_value` which is a duplicate of `obj`.
 * @raise TypeError if `obj` is an immediate value (e.g., Fixnum, Symbol in some
 *                  configurations) or if `obj` is a singleton class itself, as these
 *                  cannot be duplicated.
 * @sideeffect
 *   1. Allocates a new object on the mruby heap with the same class as `obj`.
 *   2. Copies instance variables from `obj` to the new object.
 *   3. The new object is *not* frozen, regardless of `obj`'s frozen state.
 *   4. The singleton class of `obj` (if any) is *not* copied.
 *   5. Calls the `initialize_copy` method on the newly created duplicate, passing `obj`
 *      as an argument. This method can have arbitrary side effects.
 */
MRB_API mrb_value
mrb_obj_dup(mrb_state *mrb, mrb_value obj)
{
  if (mrb_immediate_p(obj)) {
    return obj;
  }
  if (mrb_sclass_p(obj)) {
    mrb_raise(mrb, E_TYPE_ERROR, "can't dup singleton class");
  }

  struct RBasic *p = mrb_obj_alloc(mrb, mrb_type(obj), mrb_obj_class(mrb, obj));
  mrb_value dup = mrb_obj_value(p);
  init_copy(mrb, dup, obj);

  return dup;
}

/* implementation of __id__ */
mrb_value mrb_obj_id_m(mrb_state *mrb, mrb_value self);

mrb_noreturn void
mrb_method_missing(mrb_state *mrb, mrb_sym name, mrb_value self, mrb_value args)
{
  mrb_no_method_error(mrb, name, args, "undefined method '%n' for %T", name, self);
}

/* 15.3.1.3.30 */
/*
 *  call-seq:
 *     obj.method_missing(symbol [, *args] )   -> result
 *
 *  Invoked by Ruby when <i>obj</i> is sent a message it cannot handle.
 *  <i>symbol</i> is the symbol for the method called, and <i>args</i>
 *  are any arguments that were passed to it. By default, the interpreter
 *  raises an error when this method is called. However, it is possible
 *  to override the method to provide more dynamic behavior.
 *  If it is decided that a particular method should not be handled, then
 *  <i>super</i> should be called, so that ancestors can pick up the
 *  missing method.
 *  The example below creates
 *  a class <code>Roman</code>, which responds to methods with names
 *  consisting of roman numerals, returning the corresponding integer
 *  values.
 *
 *     class Roman
 *       def romanToInt(str)
 *         # ...
 *       end
 *       def method_missing(sym)
 *         str = sym.to_s
 *         romanToInt(str)
 *       end
 *     end
 *
 *     r = Roman.new
 *     r.iv      #=> 4
 *     r.xxiii   #=> 23
 *     r.mm      #=> 2000
 */
mrb_value
mrb_obj_missing(mrb_state *mrb, mrb_value mod)
{
  mrb_sym name;
  const mrb_value *a;
  mrb_int alen;

  mrb->c->ci->mid = 0;
  mrb_get_args(mrb, "n*!", &name, &a, &alen);
  mrb_method_missing(mrb, name, mod, mrb_ary_new_from_values(mrb, alen, a));
  /* not reached */
  return mrb_nil_value();
}

static mrb_value
inspect_main(mrb_state *mrb, mrb_value mod)
{
  return mrb_str_new_lit(mrb, "main");
}

static const mrb_code new_iseq[] = {
  OP_ENTER, 0x0, 0x10, 0x3,  // OP_ENTER     0:0:1:0:0:1:1
  OP_SSEND, 4, 0, 0,         // OP_SSEND     R4  :allocate  n=0
  OP_MOVE, 0, 4,             // OP_MOVE      R0  R4
  OP_MOVE, 4, 3,             // OP_MOVE      R4  R3 (&)
  OP_MOVE, 3, 2,             // OP_MOVE      R3  R2 (**)
  OP_MOVE, 2, 1,             // OP_MOVE      R2  R1 (*)
  OP_SSENDB, 1, 1, 255,      // OP_SSENDB    R1  :initialize n=*|nk=*
  OP_RETURN, 0               // OP_RETURN    R0
};

MRB_PRESYM_DEFINE_VAR_AND_INITER(new_syms, 2, MRB_SYM(allocate), MRB_SYM(initialize))

static const mrb_irep new_irep = {
  4, 6, 0, MRB_IREP_STATIC,
  new_iseq, NULL, new_syms, NULL, NULL, NULL,
  sizeof(new_iseq), 0, 2, 0, 0,
};

mrb_alignas(8)
static const struct RProc new_proc = {
  NULL, NULL, MRB_TT_PROC, MRB_GC_RED, MRB_OBJ_IS_FROZEN, MRB_PROC_SCOPE | MRB_PROC_STRICT,
  { &new_irep }, NULL, { NULL }
};

static void
init_class_new(mrb_state *mrb, struct RClass *cls)
{
  mrb_method_t m;

  MRB_PRESYM_INIT_SYMBOLS(mrb, new_syms);
  MRB_METHOD_FROM_PROC(m, &new_proc);
  mrb_define_method_raw(mrb, cls, MRB_SYM(new), m);
}

static const mrb_code neq_iseq[] = {
  OP_ENTER, 0x4, 0, 0,       // OP_ENTER     1:0:0:0:0:0:0
  OP_EQ, 0,                  // OP_EQ        R0  (R1)
  OP_JMPNOT, 0, 0, 5,        // OP_JMPNOT    R3  016
  OP_LOADF, 0,               // OP_LOADF     R0  (true)
  OP_JMP, 0, 2,              // OP_JMP       R1  018
  OP_LOADT, 0,               // OP_LOADT     R3  (true)
  OP_RETURN, 0               // OP_RETURN    R0
};

static const mrb_irep neq_irep = {
  4, 6, 0, MRB_IREP_STATIC,
  neq_iseq, NULL, NULL, NULL, NULL, NULL,
  sizeof(neq_iseq), 0, 2, 0, 0,
};

mrb_alignas(8)
static const struct RProc neq_proc = {
  NULL, NULL, MRB_TT_PROC, MRB_GC_RED, MRB_OBJ_IS_FROZEN, MRB_PROC_SCOPE | MRB_PROC_STRICT,
  { &neq_irep }, NULL, { NULL }
};

void
mrb_init_class(mrb_state *mrb)
{
  struct RClass *bob;           /* BasicObject */
  struct RClass *obj;           /* Object */
  struct RClass *mod;           /* Module */
  struct RClass *cls;           /* Class */

  /* boot class hierarchy */
  bob = boot_defclass(mrb, 0);
  obj = boot_defclass(mrb, bob); mrb->object_class = obj;
  mod = boot_defclass(mrb, obj); mrb->module_class = mod;/* obj -> mod */
  cls = boot_defclass(mrb, mod); mrb->class_class = cls; /* obj -> cls */
  /* fix-up loose ends */
  bob->c = obj->c = mod->c = cls->c = cls;
  make_metaclass(mrb, bob);
  make_metaclass(mrb, obj);
  make_metaclass(mrb, mod);
  make_metaclass(mrb, cls);

  /* name basic classes */
  mrb_define_const_id(mrb, bob, MRB_SYM(BasicObject), mrb_obj_value(bob));
  mrb_define_const_id(mrb, obj, MRB_SYM(Object),      mrb_obj_value(obj));
  mrb_define_const_id(mrb, obj, MRB_SYM(Module),      mrb_obj_value(mod));
  mrb_define_const_id(mrb, obj, MRB_SYM(Class),       mrb_obj_value(cls));

  /* name each classes */
  mrb_class_name_class(mrb, NULL, bob, MRB_SYM(BasicObject));
  mrb_class_name_class(mrb, NULL, obj, MRB_SYM(Object)); /* 15.2.1 */
  mrb_class_name_class(mrb, NULL, mod, MRB_SYM(Module)); /* 15.2.2 */
  mrb_class_name_class(mrb, NULL, cls, MRB_SYM(Class));  /* 15.2.3 */

  MRB_SET_INSTANCE_TT(cls, MRB_TT_CLASS);
  mrb_define_method_id(mrb, bob, MRB_SYM(initialize),                      mrb_do_nothing,           MRB_ARGS_NONE());
  mrb_define_method_id(mrb, bob, MRB_OPSYM(not),                           mrb_bob_not,              MRB_ARGS_NONE());
  mrb_define_method_id(mrb, bob, MRB_OPSYM(eq),                            mrb_obj_equal_m,          MRB_ARGS_REQ(1)); /* 15.3.1.3.1  */
  mrb_define_method_id(mrb, bob, MRB_SYM(__id__),                          mrb_obj_id_m,             MRB_ARGS_NONE()); /* 15.3.1.3.4  */
  mrb_define_method_id(mrb, bob, MRB_SYM(__send__),                        mrb_f_send,               MRB_ARGS_REQ(1)|MRB_ARGS_REST()|MRB_ARGS_BLOCK());  /* 15.3.1.3.5  */
  mrb_define_method_id(mrb, bob, MRB_SYM_Q(equal),                         mrb_obj_equal_m,          MRB_ARGS_REQ(1)); /* 15.3.1.3.11 */
  mrb_define_method_id(mrb, bob, MRB_SYM(instance_eval),                   mrb_obj_instance_eval,    MRB_ARGS_OPT(1)|MRB_ARGS_BLOCK());  /* 15.3.1.3.18 */
  mrb_define_private_method_id(mrb, bob, MRB_SYM(singleton_method_added),  mrb_do_nothing,           MRB_ARGS_REQ(1));
  mrb_define_private_method_id(mrb, bob, MRB_SYM(singleton_method_removed),mrb_do_nothing,           MRB_ARGS_REQ(1));
  mrb_define_private_method_id(mrb, bob, MRB_SYM(singleton_method_undefined),mrb_do_nothing,         MRB_ARGS_REQ(1));
  mrb_define_private_method_id(mrb, bob, MRB_SYM(method_missing),          mrb_obj_missing,          MRB_ARGS_ANY());  /* 15.3.1.3.30 */

  mrb_method_t m;
  MRB_METHOD_FROM_PROC(m, &neq_proc);
  mrb_define_method_raw(mrb, bob, MRB_OPSYM(neq), m);

  mrb_define_class_method_id(mrb, cls, MRB_SYM(new),                       mrb_class_new_class,      MRB_ARGS_OPT(1)|MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, cls, MRB_SYM(allocate),                        mrb_instance_alloc,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, cls, MRB_SYM(superclass),                      mrb_class_superclass,     MRB_ARGS_NONE()); /* 15.2.3.3.4 */
  mrb_define_method_id(mrb, cls, MRB_SYM(initialize),                      mrb_class_initialize,     MRB_ARGS_OPT(1)); /* 15.2.3.3.1 */
  mrb_define_private_method_id(mrb, cls, MRB_SYM(inherited),               mrb_do_nothing,           MRB_ARGS_REQ(1));

  init_class_new(mrb, cls);

  MRB_SET_INSTANCE_TT(mod, MRB_TT_MODULE);
  mrb_define_private_method_id(mrb, mod, MRB_SYM(extended),                mrb_do_nothing,           MRB_ARGS_REQ(1)); /* 15.2.2.4.26 */
  mrb_define_private_method_id(mrb, mod, MRB_SYM(prepended),               mrb_do_nothing,           MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, mod, MRB_SYM_Q(include),                       mrb_mod_include_p,        MRB_ARGS_REQ(1)); /* 15.2.2.4.28 */

  mrb_define_method_id(mrb, mod, MRB_SYM(include),                         mrb_mod_include,          MRB_ARGS_REQ(1)); /* 15.2.2.4.27 */
  mrb_define_method_id(mrb, mod, MRB_SYM(prepend),                         mrb_mod_prepend, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, mod, MRB_SYM(class_eval),                      mrb_mod_module_eval,      MRB_ARGS_ANY());  /* 15.2.2.4.15 */
  mrb_define_private_method_id(mrb, mod, MRB_SYM(included),                mrb_do_nothing,           MRB_ARGS_REQ(1)); /* 15.2.2.4.29 */
  mrb_define_method_id(mrb, mod, MRB_SYM(initialize),                      mrb_mod_initialize,       MRB_ARGS_NONE()); /* 15.2.2.4.31 */
  mrb_define_method_id(mrb, mod, MRB_SYM(module_eval),                     mrb_mod_module_eval,      MRB_ARGS_ANY());  /* 15.2.2.4.35 */
  mrb_define_private_method_id(mrb, mod, MRB_SYM(module_function),         mrb_mod_module_function,  MRB_ARGS_ANY());
  mrb_define_private_method_id(mrb, mod, MRB_SYM(private),                 mrb_mod_private,          MRB_ARGS_ANY());  /* 15.2.2.4.36 */
  mrb_define_private_method_id(mrb, mod, MRB_SYM(protected),               mrb_mod_protected,        MRB_ARGS_ANY());  /* 15.2.2.4.37 */
  mrb_define_private_method_id(mrb, mod, MRB_SYM(public),                  mrb_mod_public,           MRB_ARGS_ANY());  /* 15.2.2.4.38 */
  mrb_define_method_id(mrb, mod, MRB_SYM(attr_accessor),                   mrb_mod_attr_accessor,    MRB_ARGS_ANY());  /* 15.2.2.4.12 */
  mrb_define_method_id(mrb, mod, MRB_SYM(attr_reader),                     mrb_mod_attr_reader,      MRB_ARGS_ANY());  /* 15.2.2.4.13 */
  mrb_define_method_id(mrb, mod, MRB_SYM(attr_writer),                     mrb_mod_attr_writer,      MRB_ARGS_ANY());  /* 15.2.2.4.14 */
  mrb_define_alias_id(mrb, mod, MRB_SYM(attr), MRB_SYM(attr_reader));                                                  /* 15.2.2.4.11 */
  mrb_define_method_id(mrb, mod, MRB_SYM(to_s),                            mrb_mod_to_s,             MRB_ARGS_NONE());
  mrb_define_method_id(mrb, mod, MRB_SYM(inspect),                         mrb_mod_to_s,             MRB_ARGS_NONE());
  mrb_define_method_id(mrb, mod, MRB_SYM(alias_method),                    mrb_mod_alias,            MRB_ARGS_ANY());  /* 15.2.2.4.8 */
  mrb_define_method_id(mrb, mod, MRB_SYM(ancestors),                       mrb_mod_ancestors,        MRB_ARGS_NONE()); /* 15.2.2.4.9 */
  mrb_define_method_id(mrb, mod, MRB_SYM(undef_method),                    mrb_mod_undef,            MRB_ARGS_ANY());  /* 15.2.2.4.41 */
  mrb_define_method_id(mrb, mod, MRB_SYM_Q(const_defined),                 mrb_mod_const_defined,    MRB_ARGS_ARG(1,1)); /* 15.2.2.4.20 */
  mrb_define_method_id(mrb, mod, MRB_SYM(const_get),                       mrb_mod_const_get,        MRB_ARGS_REQ(1)); /* 15.2.2.4.21 */
  mrb_define_method_id(mrb, mod, MRB_SYM(const_set),                       mrb_mod_const_set,        MRB_ARGS_REQ(2)); /* 15.2.2.4.23 */
  mrb_define_private_method_id(mrb, mod, MRB_SYM(remove_const),            mrb_mod_remove_const,     MRB_ARGS_REQ(1)); /* 15.2.2.4.40 */
  mrb_define_method_id(mrb, mod, MRB_SYM(const_missing),                   mrb_mod_const_missing,    MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, mod, MRB_SYM_Q(method_defined),                mrb_mod_method_defined,   MRB_ARGS_REQ(1)); /* 15.2.2.4.34 */
  mrb_define_method_id(mrb, mod, MRB_SYM(define_method),                   mod_define_method,        MRB_ARGS_ARG(1,1));
  mrb_define_method_id(mrb, mod, MRB_OPSYM(eqq),                           mrb_mod_eqq,              MRB_ARGS_REQ(1)); /* 15.2.2.4.7 */
  mrb_define_method_id(mrb, mod, MRB_SYM(dup),                             mrb_mod_dup,              MRB_ARGS_NONE());
  mrb_define_private_method_id(mrb, mod, MRB_SYM(method_added),            mrb_do_nothing,           MRB_ARGS_REQ(1));
  mrb_define_private_method_id(mrb, mod, MRB_SYM(method_removed),          mrb_do_nothing,           MRB_ARGS_REQ(1));
  mrb_define_private_method_id(mrb, mod, MRB_SYM(method_undefined),        mrb_do_nothing,           MRB_ARGS_REQ(1));
  mrb_define_private_method_id(mrb, mod, MRB_SYM(const_added),             mrb_do_nothing,           MRB_ARGS_REQ(1));

  mrb_undef_method_id(mrb, cls, MRB_SYM(module_function));

  mrb->top_self = MRB_OBJ_ALLOC(mrb, MRB_TT_OBJECT, mrb->object_class);
  mrb_define_singleton_method_id(mrb, mrb->top_self, MRB_SYM(inspect), inspect_main, MRB_ARGS_NONE());
  mrb_define_singleton_method_id(mrb, mrb->top_self, MRB_SYM(to_s), inspect_main, MRB_ARGS_NONE());
  mrb_define_singleton_method_id(mrb, mrb->top_self, MRB_SYM(define_method), top_define_method, MRB_ARGS_ARG(1,1));
  mrb_define_singleton_method_id(mrb, mrb->top_self, MRB_SYM(public), top_public, MRB_ARGS_ANY());
  mrb_define_singleton_method_id(mrb, mrb->top_self, MRB_SYM(private), top_private, MRB_ARGS_ANY());
  mrb_define_singleton_method_id(mrb, mrb->top_self, MRB_SYM(protected), top_protected, MRB_ARGS_ANY());
}
