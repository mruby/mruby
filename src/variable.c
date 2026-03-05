/*
** variable.c - mruby variables
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

/* Instance variable table structure */
typedef struct iv_tbl {
  int size, alloc;
  mrb_value *ptr;
} iv_tbl;



/* Creates the instance variable table. */
static iv_tbl*
iv_new(mrb_state *mrb)
{
  iv_tbl *t;

  t = (iv_tbl*)mrb_malloc(mrb, sizeof(iv_tbl));
  t->size = 0;
  t->alloc = 0;
  t->ptr = NULL;

  return t;
}

static void iv_put(mrb_state *mrb, iv_tbl *t, mrb_sym sym, mrb_value val);

#define IV_INITIAL_SIZE 2

static void
iv_rehash(mrb_state *mrb, iv_tbl *t)
{
  int old_alloc = t->alloc;
  int new_alloc = old_alloc > 0 ? old_alloc << 1 : IV_INITIAL_SIZE;

  if (old_alloc == 0) {
    /* first-time init */
    t->ptr = (mrb_value*)mrb_calloc(mrb, new_alloc, sizeof(mrb_value)+sizeof(mrb_sym));
    t->alloc = new_alloc;
    return;
  }

  /* realloc may extend in place, avoiding malloc+memcpy+free */
  size_t new_size = (size_t)new_alloc * (sizeof(mrb_value) + sizeof(mrb_sym));
  t->ptr = (mrb_value*)mrb_realloc(mrb, t->ptr, new_size);

  /* move keys from old position to new position */
  mrb_sym *old_keys = (mrb_sym*)&t->ptr[old_alloc];
  mrb_sym *new_keys = (mrb_sym*)&t->ptr[new_alloc];
  memmove(new_keys, old_keys, sizeof(mrb_sym) * t->size);

  /* clear extended value region (where old keys were + new slots) */
  memset(&t->ptr[old_alloc], 0, sizeof(mrb_value) * (new_alloc - old_alloc));

  /* clear extended key region */
  memset(&new_keys[t->size], 0, sizeof(mrb_sym) * (new_alloc - t->size));

  t->alloc = new_alloc;
}

/* Branch-free binary search helper: returns the index where `target` should be inserted/found. */
static inline int
iv_bsearch_idx(mrb_sym *keys, int size, mrb_sym target) {
  if (size == 0) return 0;
  int n = size;
  mrb_sym *p = keys;
  /* While more than one element remains, halve the range each iteration */
  while (n > 1) {
    int half = n >> 1;
    MRB_MEM_PREFETCH(p + (half >> 1));
    MRB_MEM_PREFETCH(p + half + (half >> 1));
    mrb_sym mid_sym = p[half];
    /*
     * Update pointer p without a branch:
     * If mid_sym < target, move p forward by half; otherwise keep p unchanged.
     * Compiler will emit a CMOV or equivalent.
     */
    p = (mid_sym < target) ? p + half : p;
    n -= half;
  }
  /* Final adjustment: if the remaining element is still less than target, advance by one */
  return (int)(p - keys) + (p[0] < target);
}

/* Set (insert or update) the value for `sym` in the instance variable table using branch-free search. */
static void
iv_put(mrb_state *mrb, iv_tbl *t, mrb_sym sym, mrb_value val)
{
  /* If table is uninitialized, allocate and initialize */
  if (t->alloc == 0) {
    iv_rehash(mrb, t);
  }

  /* Obtain pointers to keys and values arrays */
  mrb_sym   *keys = (mrb_sym*)&t->ptr[t->alloc];
  mrb_value *vals =  t->ptr;

  /* Determine insertion/update index:
   * If table has entries, use branch-free search; otherwise index = 0.
   */
  int lo = iv_bsearch_idx(keys, t->size, sym);

  /* If the key already exists, update its value and return */
  if (lo < t->size && keys[lo] == sym) {
    vals[lo] = val;
    return;
  }

  /* Grow table if full, then recompute position */
  if (t->size == t->alloc) {
    iv_rehash(mrb, t);
    keys = (mrb_sym*)&t->ptr[t->alloc];
    vals =  t->ptr;
    lo = iv_bsearch_idx(keys, t->size, sym);
  }

  /* Shift existing entries right to make room at index lo */
  int move_count = t->size - lo;
  if (move_count > 0) {
    memmove(&keys[lo + 1], &keys[lo],     move_count * sizeof(mrb_sym));
    memmove(&vals[lo + 1], &vals[lo],     move_count * sizeof(mrb_value));
  }

  /* Insert the new key and value */
  keys[lo] = sym;
  vals[lo] = val;
  t->size++;
}

/* Get a value for `sym` from the instance variable table using branch-free search. */
static int
iv_get(mrb_state *mrb, iv_tbl *t, mrb_sym sym, mrb_value *vp)
{
  /* Return 0 if table is null, uninitialized, or empty */
  if (t == NULL || t->alloc == 0 || t->size == 0) return 0;

  mrb_sym   *keys = (mrb_sym*)&t->ptr[t->alloc];
  mrb_value *vals =  t->ptr;

  /* Find index in a branch-free manner */
  int lo = iv_bsearch_idx(keys, t->size, sym);

  /* If found, store value (if vp provided) and return 1-based position */
  if (lo < t->size && keys[lo] == sym) {
    if (vp) *vp = vals[lo];
    return lo + 1;
  }

  /* Not found */
  return 0;
}

/* Delete the entry for `sym` from the instance variable table using branch-free search. */
static mrb_bool
iv_del(mrb_state *mrb, iv_tbl *t, mrb_sym sym, mrb_value *vp)
{
  /* Return FALSE if table is null, uninitialized, or empty */
  if (t == NULL || t->alloc == 0 || t->size == 0) return FALSE;

  mrb_sym   *keys = (mrb_sym*)&t->ptr[t->alloc];
  mrb_value *vals =  t->ptr;

  /* Find index in a branch-free manner */
  int lo = iv_bsearch_idx(keys, t->size, sym);

  /* If found, optionally return value and shift entries left to delete */
  if (lo < t->size && keys[lo] == sym) {
    if (vp) *vp = vals[lo];
    int move_count = t->size - lo - 1;
    if (move_count > 0) {
      memmove(&keys[lo],     &keys[lo + 1],     move_count * sizeof(mrb_sym));
      memmove(&vals[lo],     &vals[lo + 1],     move_count * sizeof(mrb_value));
    }
    t->size--;
    return TRUE;
  }

  /* Not found */
  return FALSE;
}

/* Iterates over the instance variable table. */
static void
iv_foreach(mrb_state *mrb, iv_tbl *t, mrb_iv_foreach_func *func, void *p)
{
  if (t == NULL || t->alloc == 0 || t->size == 0) return;
  for (int i = 0; i < t->size; i++) {
    mrb_sym   *keys = (mrb_sym*)&t->ptr[t->alloc];
    mrb_value *vals = t->ptr;
    if ((*func)(mrb, keys[i], vals[i], p) != 0) return;
  }
}

/* Get the size of the instance variable table. */
static size_t
iv_size(mrb_state *mrb, iv_tbl *t)
{
  return t ? t->size : 0;
}

/* Copy the sorted table */
static iv_tbl*
iv_copy(mrb_state *mrb, iv_tbl *t)
{
  if (t == NULL || t->alloc == 0 || t->size == 0) return NULL;

  /* create new table and mirror alloc/size */
  iv_tbl *t2 = iv_new(mrb);
  t2->alloc = t->alloc;
  t2->size  = t->size;

  /* allocate the same block shape */
  t2->ptr = (mrb_value*)mrb_calloc(mrb, t2->alloc, sizeof(mrb_value)+sizeof(mrb_sym));

  /* copy values[0...size] and keys[0...size] */
  memcpy(t2->ptr, t->ptr, sizeof(mrb_value)*t2->size);
  memcpy(&t2->ptr[t2->alloc], &t->ptr[t->alloc], sizeof(mrb_sym)*t2->size);

  return t2;
}

/* Free memory of the instance variable table. */
static void
iv_free(mrb_state *mrb, iv_tbl *t)
{
  mrb_free(mrb, t->ptr);
  mrb_free(mrb, t);
}

/*
 * Object Shape (Hidden Class) structures.
 *
 * A shape describes the IV layout of an object: which syms are stored
 * at which indices. Shapes form a tree rooted at the empty root shape.
 * Each child adds one IV (its "edge" sym). Objects sharing the same
 * set of IVs (assigned in the same order) share the same shape,
 * eliminating per-object key storage.
 *
 * Only MRB_TT_OBJECT instances are shaped. RClass, RHash, etc. keep
 * traditional iv_tbl.
 */

/* Maximum IV count before de-shaping to iv_tbl */
#define MRB_SHAPE_MAX_IVS 16

/* Shape descriptor -- shared across objects with same IV layout */
typedef struct mrb_iv_shape {
  struct mrb_iv_shape *parent;    /* parent shape (one fewer IV) */
  struct mrb_iv_shape *children;  /* linked list of child shapes */
  struct mrb_iv_shape *sibling;   /* next child of same parent */
  mrb_sym edge;                   /* IV sym added from parent */
  uint16_t count;                 /* number of IV slots */
} mrb_iv_shape;

/* Per-object shaped IV storage (allocated via struct hack) */
typedef struct mrb_shaped_iv {
  mrb_iv_shape *shape;
  mrb_value values[1];  /* shape->count elements */
} mrb_shaped_iv;

/* Create the empty root shape */
static mrb_iv_shape*
shape_root(mrb_state *mrb)
{
  mrb_iv_shape *s = (mrb_iv_shape*)mrb_calloc(mrb, 1, sizeof(mrb_iv_shape));
  return s;
}

/* Find a child shape with the given edge sym */
static mrb_iv_shape*
shape_find_child(mrb_iv_shape *shape, mrb_sym sym)
{
  mrb_iv_shape *c = shape->children;
  while (c) {
    if (c->edge == sym) return c;
    c = c->sibling;
  }
  return NULL;
}

/* Find or create a child shape for adding sym */
static mrb_iv_shape*
shape_transition(mrb_state *mrb, mrb_iv_shape *shape, mrb_sym sym)
{
  mrb_iv_shape *child = shape_find_child(shape, sym);
  if (child) return child;

  /* create new child shape */
  child = (mrb_iv_shape*)mrb_malloc(mrb, sizeof(mrb_iv_shape));
  child->parent = shape;
  child->children = NULL;
  child->sibling = shape->children;
  child->edge = sym;
  child->count = shape->count + 1;
  shape->children = child;
  return child;
}

/*
 * Look up sym in shape by walking the parent chain.
 * Returns the value index (0-based), or -1 if not found.
 */
static int
shape_lookup(mrb_iv_shape *shape, mrb_sym sym)
{
  mrb_iv_shape *s = shape;
  while (s->count > 0) {
    if (s->edge == sym) return s->count - 1;
    s = s->parent;
  }
  return -1;
}

/* Recursively free all shapes in the tree */
static void
shape_free_tree(mrb_state *mrb, mrb_iv_shape *shape)
{
  mrb_iv_shape *c = shape->children;
  while (c) {
    mrb_iv_shape *next = c->sibling;
    shape_free_tree(mrb, c);
    c = next;
  }
  mrb_free(mrb, shape);
}

/* Allocate a mrb_shaped_iv with room for count values */
static mrb_shaped_iv*
shaped_iv_alloc(mrb_state *mrb, mrb_iv_shape *shape)
{
  size_t sz = offsetof(mrb_shaped_iv, values) +
              sizeof(mrb_value) * shape->count;
  mrb_shaped_iv *siv = (mrb_shaped_iv*)mrb_malloc(mrb, sz);
  siv->shape = shape;
  return siv;
}

/* Convert a shaped object back to traditional iv_tbl (de-shape) */
static void
shaped_to_iv_tbl(mrb_state *mrb, struct RObject *obj)
{
  mrb_shaped_iv *siv = (mrb_shaped_iv*)obj->iv;
  iv_tbl *t = NULL;

  if (siv) {
    mrb_iv_shape *shape = siv->shape;

    if (shape->count > 0) {
      /* reconstruct keys from parent chain */
      mrb_sym keys[MRB_SHAPE_MAX_IVS];
      mrb_iv_shape *s = shape;
      while (s->count > 0) {
        keys[s->count - 1] = s->edge;
        s = s->parent;
      }

      t = iv_new(mrb);
      for (int i = 0; i < shape->count; i++) {
        if (!mrb_undef_p(siv->values[i])) {
          iv_put(mrb, t, keys[i], siv->values[i]);
        }
      }
    }
    mrb_free(mrb, siv);
  }
  obj->iv = t;
  obj->flags &= ~MRB_FL_OBJ_SHAPED;
}

/* --- Shaped IV operations --- */

static void
shaped_iv_set(mrb_state *mrb, struct RObject *obj, mrb_sym sym, mrb_value v)
{
  mrb_shaped_iv *siv = (mrb_shaped_iv*)obj->iv;
  mrb_iv_shape *shape = siv ? siv->shape : mrb->root_shape;

  /* check if sym already exists in current shape */
  int idx = shape_lookup(shape, sym);
  if (idx >= 0) {
    siv->values[idx] = v;
    return;
  }

  /* transition to new shape */
  mrb_iv_shape *new_shape = shape_transition(mrb, shape, sym);

  /* de-shape if too many IVs */
  if (new_shape->count > MRB_SHAPE_MAX_IVS) {
    shaped_to_iv_tbl(mrb, obj);
    if (!obj->iv) {
      obj->iv = iv_new(mrb);
    }
    iv_put(mrb, obj->iv, sym, v);
    return;
  }

  /* allocate new shaped_iv with room for new shape */
  mrb_shaped_iv *new_siv = shaped_iv_alloc(mrb, new_shape);

  /* copy old values (they are a prefix) */
  if (siv) {
    memcpy(new_siv->values, siv->values,
           sizeof(mrb_value) * shape->count);
    mrb_free(mrb, siv);
  }
  /* new IV goes in the last slot */
  new_siv->values[new_shape->count - 1] = v;

  obj->iv = (iv_tbl*)new_siv;
}

static mrb_value
shaped_iv_get(struct RObject *obj, mrb_sym sym)
{
  mrb_shaped_iv *siv = (mrb_shaped_iv*)obj->iv;
  if (!siv) return mrb_nil_value();
  int idx = shape_lookup(siv->shape, sym);
  if (idx >= 0 && !mrb_undef_p(siv->values[idx]))
    return siv->values[idx];
  return mrb_nil_value();
}

static mrb_bool
shaped_iv_defined(struct RObject *obj, mrb_sym sym)
{
  mrb_shaped_iv *siv = (mrb_shaped_iv*)obj->iv;
  if (!siv) return FALSE;
  int idx = shape_lookup(siv->shape, sym);
  if (idx >= 0 && !mrb_undef_p(siv->values[idx]))
    return TRUE;
  return FALSE;
}

static void
shaped_iv_foreach(mrb_state *mrb, struct RObject *obj,
                  mrb_iv_foreach_func *func, void *p)
{
  mrb_shaped_iv *siv = (mrb_shaped_iv*)obj->iv;
  if (!siv) return;
  mrb_iv_shape *shape = siv->shape;
  if (shape->count == 0) return;

  /* reconstruct keys from parent chain */
  mrb_sym keys[MRB_SHAPE_MAX_IVS];
  mrb_iv_shape *s = shape;
  while (s->count > 0) {
    keys[s->count - 1] = s->edge;
    s = s->parent;
  }

  for (int i = 0; i < shape->count; i++) {
    if (!mrb_undef_p(siv->values[i])) {
      if ((*func)(mrb, keys[i], siv->values[i], p) != 0) return;
    }
  }
}

static size_t
shaped_iv_mark(mrb_state *mrb, struct RObject *obj)
{
  mrb_shaped_iv *siv = (mrb_shaped_iv*)obj->iv;
  if (!siv) return 0;
  mrb_iv_shape *shape = siv->shape;
  for (int i = 0; i < shape->count; i++) {
    if (!mrb_undef_p(siv->values[i])) {
      mrb_gc_mark_value(mrb, siv->values[i]);
    }
  }
  return shape->count;
}

static void
shaped_iv_free(mrb_state *mrb, struct RObject *obj)
{
  if (obj->iv) {
    mrb_free(mrb, obj->iv);
  }
}

static void
shaped_iv_copy(mrb_state *mrb, struct RObject *dst, struct RObject *src)
{
  mrb_shaped_iv *ssiv = (mrb_shaped_iv*)src->iv;
  if (!ssiv) {
    dst->iv = NULL;
    return;
  }
  mrb_iv_shape *shape = ssiv->shape;
  mrb_shaped_iv *dsiv = shaped_iv_alloc(mrb, shape);
  memcpy(dsiv->values, ssiv->values, sizeof(mrb_value) * shape->count);
  dst->iv = (iv_tbl*)dsiv;
}

/* Public init/free for shape tree (called from state.c) */
void
mrb_init_shape(mrb_state *mrb)
{
  mrb->root_shape = shape_root(mrb);
}

void
mrb_free_shape(mrb_state *mrb)
{
  if (mrb->root_shape) {
    shape_free_tree(mrb, mrb->root_shape);
    mrb->root_shape = NULL;
  }
}

static int
iv_mark_i(mrb_state *mrb, mrb_sym sym, mrb_value v, void *p)
{
  mrb_gc_mark_value(mrb, v);
  return 0;
}

static void
mark_tbl(mrb_state *mrb, iv_tbl *t)
{
  iv_foreach(mrb, t, iv_mark_i, 0);
}

void
mrb_gc_mark_gv(mrb_state *mrb)
{
  mark_tbl(mrb, mrb->globals);
}

void
mrb_gc_free_gv(mrb_state *mrb)
{
  if (mrb->globals) {
    iv_free(mrb, mrb->globals);
    mrb->globals = NULL;
  }
}

size_t
mrb_gc_mark_iv(mrb_state *mrb, struct RObject *obj)
{
  if (MRB_OBJ_SHAPED_P(obj)) {
    return shaped_iv_mark(mrb, obj);
  }
  mark_tbl(mrb, obj->iv);
  return iv_size(mrb, obj->iv);
}

void
mrb_gc_free_iv(mrb_state *mrb, struct RObject *obj)
{
  if (MRB_OBJ_SHAPED_P(obj)) {
    shaped_iv_free(mrb, obj);
    return;
  }
  if (obj->iv) {
    iv_free(mrb, obj->iv);
  }
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

static mrb_bool
obj_iv_p(mrb_value obj)
{
  switch (mrb_unboxed_type(obj)) {
    case MRB_TT_OBJECT:
    case MRB_TT_CLASS:
    case MRB_TT_MODULE:
    case MRB_TT_SCLASS:
    case MRB_TT_HASH:
    case MRB_TT_CDATA:
    case MRB_TT_EXCEPTION:
      return TRUE;
    default:
      return FALSE;
  }
}

static iv_tbl*
class_iv_ptr(struct RClass *c)
{
  return c->tt == MRB_TT_ICLASS ? c->c->iv : c->iv;
}

/*
 * Retrieves an instance variable from an object.
 *
 * Args:
 *   mrb: The mruby state.
 *   obj: The object from which to retrieve the instance variable.
 *   sym: The symbol representing the name of the instance variable.
 *
 * Returns:
 *   The value of the instance variable, or mrb_nil_value() if the
 *   instance variable is not defined.
 */
MRB_API mrb_value
mrb_obj_iv_get(mrb_state *mrb, struct RObject *obj, mrb_sym sym)
{
  if (MRB_OBJ_SHAPED_P(obj)) {
    return shaped_iv_get(obj, sym);
  }

  mrb_value v;
  if (obj->iv && iv_get(mrb, obj->iv, sym, &v))
    return v;
  return mrb_nil_value();
}

/*
 * Retrieves an instance variable from an mrb_value.
 *
 * This function is a wrapper around mrb_obj_iv_get. It checks if the
 * given mrb_value is an object that can have instance variables before
 * attempting to retrieve the variable.
 *
 * Args:
 *   mrb: The mruby state.
 *   obj: The mrb_value from which to retrieve the instance variable.
 *   sym: The symbol representing the name of the instance variable.
 *
 * Returns:
 *   The value of the instance variable, or mrb_nil_value() if the
 *   instance variable is not defined or if the object cannot have
 *   instance variables.
 */
MRB_API mrb_value
mrb_iv_get(mrb_state *mrb, mrb_value obj, mrb_sym sym)
{
  if (obj_iv_p(obj)) {
    return mrb_obj_iv_get(mrb, mrb_obj_ptr(obj), sym);
  }
  return mrb_nil_value();
}

static inline mrb_bool
namespace_p(enum mrb_vtype tt)
{
  return tt == MRB_TT_CLASS || tt == MRB_TT_MODULE ? TRUE : FALSE;
}

static inline void
assign_class_name(mrb_state *mrb, struct RObject *obj, mrb_sym sym, mrb_value v)
{
  if (namespace_p(mrb_type(v))) {
    struct RObject *c = mrb_obj_ptr(v);
    if (obj != c && ISUPPER(mrb_sym_name_len(mrb, sym, NULL)[0])) {
      mrb_sym id_classname = MRB_SYM(__classname__);
      mrb_value o = mrb_obj_iv_get(mrb, c, id_classname);

      if (mrb_nil_p(o)) {
        mrb_sym id_outer = MRB_SYM(__outer__);
        o = mrb_obj_iv_get(mrb, c, id_outer);

        if (mrb_nil_p(o)) {
          if ((struct RClass*)obj == mrb->object_class) {
            mrb_obj_iv_set_force(mrb, c, id_classname, mrb_symbol_value(sym));
          }
          else {
            mrb_obj_iv_set_force(mrb, c, id_outer, mrb_obj_value(obj));
          }
        }
      }
    }
  }
}

void
mrb_obj_iv_set_force(mrb_state *mrb, struct RObject *obj, mrb_sym sym, mrb_value v)
{
  if (namespace_p(obj->tt)) {
    assign_class_name(mrb, obj, sym, v);
  }
  if (MRB_OBJ_SHAPED_P(obj)) {
    shaped_iv_set(mrb, obj, sym, v);
    mrb_field_write_barrier_value(mrb, (struct RBasic*)obj, v);
    return;
  }
  if (!obj->iv) {
    obj->iv = iv_new(mrb);
  }
  iv_put(mrb, obj->iv, sym, v);
  mrb_field_write_barrier_value(mrb, (struct RBasic*)obj, v);
}

/*
 * Sets an instance variable on an object.
 *
 * This function checks if the object is frozen before setting the variable.
 * It then calls mrb_obj_iv_set_force to actually set the variable.
 *
 * Args:
 *   mrb: The mruby state.
 *   obj: The object on which to set the instance variable.
 *   sym: The symbol representing the name of the instance variable.
 *   v:   The value to set for the instance variable.
 */
MRB_API void
mrb_obj_iv_set(mrb_state *mrb, struct RObject *obj, mrb_sym sym, mrb_value v)
{
  mrb_check_frozen(mrb, obj);
  mrb_obj_iv_set_force(mrb, obj, sym, v);
}

/*
 * Iterates over the instance variables of an object.
 *
 * This function calls the provided callback function for each instance
 * variable in the object.
 *
 * Args:
 *   mrb:  The mruby state.
 *   obj:  The mrb_value whose instance variables to iterate over.
 *   func: The callback function to call for each instance variable.
 *         The function should take mrb_state*, mrb_sym, mrb_value, and void*
 *         as arguments and return an int. If the callback returns a non-zero
 *         value, iteration stops.
 *   p:    A pointer to user data that will be passed to the callback function.
 */
MRB_API void
mrb_iv_foreach(mrb_state *mrb, mrb_value obj, mrb_iv_foreach_func *func, void *p)
{
  if (!obj_iv_p(obj)) return;
  if (MRB_OBJ_SHAPED_P(mrb_obj_ptr(obj))) {
    shaped_iv_foreach(mrb, mrb_obj_ptr(obj), func, p);
    return;
  }
  iv_foreach(mrb, mrb_obj_ptr(obj)->iv, func, p);
}

/*
 * Sets an instance variable on an mrb_value.
 *
 * This function is a wrapper around mrb_obj_iv_set. It checks if the
 * given mrb_value is an object that can have instance variables before
 * attempting to set the variable. If the object cannot have instance
 * variables, it raises an E_ARGUMENT_ERROR.
 *
 * Args:
 *   mrb: The mruby state.
 *   obj: The mrb_value on which to set the instance variable.
 *   sym: The symbol representing the name of the instance variable.
 *   v:   The value to set for the instance variable.
 *
 * Raises:
 *   E_ARGUMENT_ERROR: If the object cannot have instance variables.
 */
MRB_API void
mrb_iv_set(mrb_state *mrb, mrb_value obj, mrb_sym sym, mrb_value v)
{
  if (obj_iv_p(obj)) {
    mrb_obj_iv_set(mrb, mrb_obj_ptr(obj), sym, v);
  }
  else {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "cannot set instance variable");
  }
}

/*
 * Checks if an instance variable is defined on an object.
 *
 * Args:
 *   mrb: The mruby state.
 *   obj: The object to check.
 *   sym: The symbol representing the name of the instance variable.
 *
 * Returns:
 *   TRUE if the instance variable is defined, FALSE otherwise.
 */
MRB_API mrb_bool
mrb_obj_iv_defined(mrb_state *mrb, struct RObject *obj, mrb_sym sym)
{
  if (MRB_OBJ_SHAPED_P(obj)) {
    return shaped_iv_defined(obj, sym);
  }

  iv_tbl *t = obj->iv;
  if (t && iv_get(mrb, t, sym, NULL)) return TRUE;
  return FALSE;
}

/*
 * Checks if an instance variable is defined on an mrb_value.
 *
 * This function is a wrapper around mrb_obj_iv_defined. It checks if the
 * given mrb_value is an object that can have instance variables before
 * attempting to check for the variable.
 *
 * Args:
 *   mrb: The mruby state.
 *   obj: The mrb_value to check.
 *   sym: The symbol representing the name of the instance variable.
 *
 * Returns:
 *   TRUE if the instance variable is defined and the object can have
 *   instance variables, FALSE otherwise.
 */
MRB_API mrb_bool
mrb_iv_defined(mrb_state *mrb, mrb_value obj, mrb_sym sym)
{
  if (!obj_iv_p(obj)) return FALSE;
  return mrb_obj_iv_defined(mrb, mrb_obj_ptr(obj), sym);
}

/*
 * Checks if a symbol is a valid instance variable name.
 *
 * A valid instance variable name must:
 *   - Be at least 2 characters long.
 *   - Start with '@'.
 *   - Not have a digit as the second character.
 *   - The rest of the name must be a valid identifier.
 *
 * Args:
 *   mrb:     The mruby state.
 *   iv_name: The symbol to check.
 *
 * Returns:
 *   TRUE if the symbol is a valid instance variable name, FALSE otherwise.
 */
MRB_API mrb_bool
mrb_iv_name_sym_p(mrb_state *mrb, mrb_sym iv_name)
{
  mrb_int len;
  const char *s = mrb_sym_name_len(mrb, iv_name, &len);

  if (len < 2) return FALSE;
  if (s[0] != '@') return FALSE;
  if (ISDIGIT(s[1])) return FALSE;
  return mrb_ident_p(s+1, len-1);
}

/*
 * Checks if a symbol is a valid instance variable name and raises a
 * name error if it's not.
 *
 * Args:
 *   mrb:     The mruby state.
 *   iv_name: The symbol to check.
 *
 * Raises:
 *   E_NAME_ERROR: If the symbol is not a valid instance variable name.
 */
MRB_API void
mrb_iv_name_sym_check(mrb_state *mrb, mrb_sym iv_name)
{
  if (!mrb_iv_name_sym_p(mrb, iv_name)) {
    mrb_name_error(mrb, iv_name, "'%n' is not allowed as an instance variable name", iv_name);
  }
}

/*
 * Copies instance variables from one object to another.
 *
 * If the destination object already has instance variables, they are freed
 * before copying.
 *
 * Args:
 *   mrb:  The mruby state.
 *   dest: The destination object (mrb_value).
 *   src:  The source object (mrb_value).
 */
MRB_API void
mrb_iv_copy(mrb_state *mrb, mrb_value dest, mrb_value src)
{
  struct RObject *d = mrb_obj_ptr(dest);
  struct RObject *s = mrb_obj_ptr(src);

  /* free dest's existing IVs */
  if (MRB_OBJ_SHAPED_P(d)) {
    shaped_iv_free(mrb, d);
    d->iv = NULL;
  }
  else if (d->iv) {
    iv_free(mrb, d->iv);
    d->iv = NULL;
  }

  if (MRB_OBJ_SHAPED_P(s) && MRB_OBJ_SHAPED_P(d)) {
    /* both shaped: share shape, memcpy values */
    if (s->iv) {
      mrb_write_barrier(mrb, (struct RBasic*)d);
      shaped_iv_copy(mrb, d, s);
    }
  }
  else if (MRB_OBJ_SHAPED_P(s)) {
    /* src shaped, dest unshaped: convert src to iv_tbl copy */
    mrb_shaped_iv *ssiv = (mrb_shaped_iv*)s->iv;
    if (ssiv) {
      mrb_iv_shape *shape = ssiv->shape;
      if (shape->count > 0) {
        mrb_sym keys[MRB_SHAPE_MAX_IVS];
        mrb_iv_shape *sh = shape;
        while (sh->count > 0) {
          keys[sh->count - 1] = sh->edge;
          sh = sh->parent;
        }
        iv_tbl *t = iv_new(mrb);
        for (int i = 0; i < shape->count; i++) {
          if (!mrb_undef_p(ssiv->values[i])) {
            iv_put(mrb, t, keys[i], ssiv->values[i]);
          }
        }
        mrb_write_barrier(mrb, (struct RBasic*)d);
        d->iv = t;
      }
    }
  }
  else {
    /* both unshaped or dest shaped but src unshaped */
    if (MRB_OBJ_SHAPED_P(d)) {
      d->flags &= ~MRB_FL_OBJ_SHAPED;
    }
    if (s->iv) {
      mrb_write_barrier(mrb, (struct RBasic*)d);
      d->iv = iv_copy(mrb, s->iv);
    }
  }
}

/*
 * Removes an instance variable from an object.
 *
 * Args:
 *   mrb: The mruby state.
 *   obj: The object (mrb_value) from which to remove the instance variable.
 *   sym: The symbol representing the name of the instance variable.
 *
 * Returns:
 *   The value of the removed instance variable, or mrb_undef_value() if
 *   the instance variable was not defined or if the object cannot have
 *   instance variables.
 */
MRB_API mrb_value
mrb_iv_remove(mrb_state *mrb, mrb_value obj, mrb_sym sym)
{
  if (obj_iv_p(obj)) {
    struct RObject *o = mrb_obj_ptr(obj);
    mrb_check_frozen(mrb, o);

    if (MRB_OBJ_SHAPED_P(o)) {
      mrb_shaped_iv *siv = (mrb_shaped_iv*)o->iv;
      if (siv) {
        int idx = shape_lookup(siv->shape, sym);
        if (idx >= 0 && !mrb_undef_p(siv->values[idx])) {
          mrb_value val = siv->values[idx];
          /* de-shape, then remove the key */
          shaped_to_iv_tbl(mrb, o);
          iv_del(mrb, o->iv, sym, NULL);
          return val;
        }
      }
      return mrb_undef_value();
    }

    iv_tbl *t = o->iv;
    mrb_value val;
    if (iv_del(mrb, t, sym, &val)) {
      return val;
    }
  }
  return mrb_undef_value();
}

static int
iv_i(mrb_state *mrb, mrb_sym sym, mrb_value v, void *p)
{
  mrb_value ary = *(mrb_value*)p;
  mrb_int len;
  const char* s = mrb_sym_name_len(mrb, sym, &len);

  if (len > 1 && s[0] == '@' && s[1] != '@') {
    mrb_ary_push(mrb, ary, mrb_symbol_value(sym));
  }
  return 0;
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
  mrb_value ary = mrb_ary_new(mrb);

  if (obj_iv_p(self)) {
    struct RObject *obj = mrb_obj_ptr(self);
    if (MRB_OBJ_SHAPED_P(obj)) {
      shaped_iv_foreach(mrb, obj, iv_i, &ary);
    }
    else {
      iv_foreach(mrb, obj->iv, iv_i, &ary);
    }
  }
  return ary;
}

static int
cv_i(mrb_state *mrb, mrb_sym sym, mrb_value v, void *p)
{
  mrb_value ary = *(mrb_value*)p;
  mrb_int len;
  const char* s = mrb_sym_name_len(mrb, sym, &len);

  if (len > 2 && s[0] == '@' && s[1] == '@') {
    mrb_ary_push(mrb, ary, mrb_symbol_value(sym));
  }
  return 0;
}

/* 15.2.2.4.19 */
/*
 *  call-seq:
 *     mod.class_variables(inherit=true)   -> array
 *
 *  Returns an array of the names of class variables in *mod*.
 *
 *     class One
 *       @@var1 = 1
 *     end
 *     class Two < One
 *       @@var2 = 2
 *     end
 *     One.class_variables   #=> [:@@var1]
 *     Two.class_variables   #=> [:@@var2]
 */
mrb_value
mrb_mod_class_variables(mrb_state *mrb, mrb_value mod)
{
  mrb_bool inherit = TRUE;

  mrb_get_args(mrb, "|b", &inherit);
  mrb_value ary = mrb_ary_new(mrb);
  struct RClass *c = mrb_class_ptr(mod);
  while (c) {
    iv_foreach(mrb, class_iv_ptr(c), cv_i, &ary);
    if (!inherit) break;
    c = c->super;
  }
  return ary;
}

mrb_value
mrb_mod_cv_get(mrb_state *mrb, struct RClass *c, mrb_sym sym)
{
  struct RClass * cls = c;
  mrb_value v = mrb_nil_value();
  mrb_bool given = FALSE;

  while (c) {
    if (iv_get(mrb, class_iv_ptr(c), sym, &v)) {
      given = TRUE;
    }
    c = c->super;
  }
  if (given) return v;
  if (cls->tt == MRB_TT_SCLASS) {
    mrb_value klass = mrb_obj_iv_get(mrb, (struct RObject*)cls, MRB_SYM(__attached__));

    c = mrb_class_ptr(klass);
    if (c->tt == MRB_TT_CLASS || c->tt == MRB_TT_MODULE) {
      given = FALSE;
      while (c) {
        if (iv_get(mrb, class_iv_ptr(c), sym, &v)) {
          given = TRUE;
        }
        c = c->super;
      }
      if (given) return v;
    }
  }
  mrb_name_error(mrb, sym, "uninitialized class variable %n in %C", sym, cls);
  /* not reached */
  return mrb_nil_value();
}

/*
 * Retrieves a class variable from a module or class.
 *
 * This function is a wrapper around mrb_mod_cv_get.
 *
 * Args:
 *   mrb: The mruby state.
 *   mod: The module or class (mrb_value) from which to retrieve the class variable.
 *   sym: The symbol representing the name of the class variable.
 *
 * Returns:
 *   The value of the class variable.
 *
 * Raises:
 *   E_NAME_ERROR: If the class variable is not defined.
 */
MRB_API mrb_value
mrb_cv_get(mrb_state *mrb, mrb_value mod, mrb_sym sym)
{
  return mrb_mod_cv_get(mrb, mrb_class_ptr(mod), sym);
}

/*
 * Sets a class variable in a module or class.
 *
 * This function searches for the class variable in the superclass chain.
 * If found, it updates the value in the class where it's defined.
 * Otherwise, it sets the variable in the given class `c`.
 *
 * Args:
 *   mrb: The mruby state.
 *   c:   The class or module (struct RClass*) in which to set the class variable.
 *   sym: The symbol representing the name of the class variable.
 *   v:   The value to set for the class variable.
 */
MRB_API void
mrb_mod_cv_set(mrb_state *mrb, struct RClass *c, mrb_sym sym, mrb_value v)
{
  struct RClass * cls = c;

  while (c) {
    iv_tbl *t = class_iv_ptr(c);
    int pos = iv_get(mrb, t, sym, NULL);

    if (pos) {
      mrb_check_frozen(mrb, c);
      t->ptr[pos-1] = v;        /* iv_get returns pos+1 to put */
      mrb_field_write_barrier_value(mrb, (struct RBasic*)c, v);
      return;
    }
    c = c->super;
  }

  if (cls->tt == MRB_TT_SCLASS) {
    mrb_value klass = mrb_obj_iv_get(mrb, (struct RObject*)cls, MRB_SYM(__attached__));

    switch (mrb_type(klass)) {
    case MRB_TT_CLASS:
    case MRB_TT_MODULE:
    case MRB_TT_SCLASS:
      c = mrb_class_ptr(klass);
      break;
    default:
      c = cls;
      break;
    }
  }
  else if (cls->tt == MRB_TT_ICLASS) {
    c = cls->c;
  }
  else {
    c = cls;
  }

  mrb_check_frozen(mrb, c);
  if (!c->iv) {
    c->iv = iv_new(mrb);
  }

  iv_put(mrb, c->iv, sym, v);
  mrb_field_write_barrier_value(mrb, (struct RBasic*)c, v);
}

/*
 * Sets a class variable in a module or class.
 *
 * This function is a wrapper around mrb_mod_cv_set.
 *
 * Args:
 *   mrb: The mruby state.
 *   mod: The module or class (mrb_value) in which to set the class variable.
 *   sym: The symbol representing the name of the class variable.
 *   v:   The value to set for the class variable.
 */
MRB_API void
mrb_cv_set(mrb_state *mrb, mrb_value mod, mrb_sym sym, mrb_value v)
{
  mrb_mod_cv_set(mrb, mrb_class_ptr(mod), sym, v);
}

/*
 * Checks if a class variable is defined in a module or class or its ancestors.
 *
 * Args:
 *   mrb: The mruby state.
 *   c:   The class or module (struct RClass*) to check.
 *   sym: The symbol representing the name of the class variable.
 *
 * Returns:
 *   TRUE if the class variable is defined, FALSE otherwise.
 */
mrb_bool
mrb_mod_cv_defined(mrb_state *mrb, struct RClass * c, mrb_sym sym)
{
  while (c) {
    iv_tbl *t = class_iv_ptr(c);
    if (iv_get(mrb, t, sym, NULL)) return TRUE;
    c = c->super;
  }

  return FALSE;
}

/*
 * Checks if a class variable is defined in a module or class or its ancestors.
 *
 * This function is a wrapper around mrb_mod_cv_defined.
 *
 * Args:
 *   mrb: The mruby state.
 *   mod: The module or class (mrb_value) to check.
 *   sym: The symbol representing the name of the class variable.
 *
 * Returns:
 *   TRUE if the class variable is defined, FALSE otherwise.
 */
MRB_API mrb_bool
mrb_cv_defined(mrb_state *mrb, mrb_value mod, mrb_sym sym)
{
  return mrb_mod_cv_defined(mrb, mrb_class_ptr(mod), sym);
}

mrb_value
mrb_vm_cv_get(mrb_state *mrb, mrb_sym sym)
{
  struct RClass *c;

  const struct RProc *p = mrb->c->ci->proc;

  for (;;) {
    c = MRB_PROC_TARGET_CLASS(p);
    if (c && c->tt != MRB_TT_SCLASS) break;
    p = p->upper;
  }
  return mrb_mod_cv_get(mrb, c, sym);
}

void
mrb_vm_cv_set(mrb_state *mrb, mrb_sym sym, mrb_value v)
{
  struct RClass *c;
  const struct RProc *p = mrb->c->ci->proc;

  for (;;) {
    c = MRB_PROC_TARGET_CLASS(p);
    if (c && c->tt != MRB_TT_SCLASS) break;
    p = p->upper;
  }
  mrb_mod_cv_set(mrb, c, sym, v);
}

static void
mod_const_check(mrb_state *mrb, mrb_value mod)
{
  switch (mrb_type(mod)) {
  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
  case MRB_TT_SCLASS:
    break;
  default:
    mrb_raise(mrb, E_TYPE_ERROR, "constant look-up for non class/module");
    break;
  }
}

static mrb_value
const_get_nohook(mrb_state *mrb, struct RClass *base, mrb_sym sym, mrb_bool skip)
{
  struct RClass *c = base;
  mrb_value v;
  mrb_bool retry = FALSE;

  /* if skip then skip the current class (already searched) */
  if (skip) c = c->super;
L_RETRY:
  while (c) {
    if (!MRB_FLAG_TEST(c, MRB_FL_CLASS_IS_PREPENDED) && iv_get(mrb, class_iv_ptr(c), sym, &v)) {
      return v;
    }
    c = c->super;
    if (!skip && c == mrb->object_class) break;
  }
  if (!retry && base->tt == MRB_TT_MODULE && skip) {
    c = mrb->object_class;
    retry = TRUE;
    goto L_RETRY;
  }
  return mrb_undef_value();
}

static mrb_value
const_get(mrb_state *mrb, struct RClass *base, mrb_sym sym, mrb_bool skip)
{
  mrb_value v = const_get_nohook(mrb, base, sym, skip);

  /* call const_missing hook */
  if (mrb_undef_p(v)) {
    mrb_value mod = mrb_obj_value(base);
    if (mrb_func_basic_p(mrb, mod, MRB_SYM(const_missing), mrb_mod_const_missing)) {
      return mrb_const_missing(mrb, mod, sym);
    }
    mrb_value name = mrb_symbol_value(sym);
    return mrb_funcall_argv(mrb, mod, MRB_SYM(const_missing), 1, &name);
  }
  return v;
}

mrb_value
mrb_exc_const_get(mrb_state *mrb, mrb_sym sym)
{
  return const_get_nohook(mrb, mrb->object_class, sym, FALSE);
}

/*
 * Retrieves a constant from a module or class.
 *
 * It first checks if `mod` is a class or module, then calls the
 * internal `const_get` function to retrieve the constant.
 * This function will also call the `const_missing` hook if the
 * constant is not found.
 *
 * Args:
 *   mrb: The mruby state.
 *   mod: The module or class (mrb_value) from which to retrieve the constant.
 *   sym: The symbol representing the name of the constant.
 *
 * Returns:
 *   The value of the constant.
 *
 * Raises:
 *   E_TYPE_ERROR: If `mod` is not a class or module.
 *   E_NAME_ERROR: If the constant is not defined and `const_missing` is not defined or also raises an error.
 */
MRB_API mrb_value
mrb_const_get(mrb_state *mrb, mrb_value mod, mrb_sym sym)
{
  mod_const_check(mrb, mod);
  return const_get(mrb, mrb_class_ptr(mod), sym, FALSE);
}

mrb_value
mrb_vm_const_get(mrb_state *mrb, mrb_sym sym)
{
  const struct RProc *proc = mrb->c->ci->proc;
  struct RClass *c = MRB_PROC_TARGET_CLASS(proc), *c2;
  mrb_value v;

  if (!c) c = mrb->object_class;
  if (iv_get(mrb, class_iv_ptr(c), sym, &v)) {
    return v;
  }
  for (proc = proc->upper; proc; proc = proc->upper) {
    c2 = MRB_PROC_TARGET_CLASS(proc);
    if (!c2) c2 = mrb->object_class;
    if (iv_get(mrb, class_iv_ptr(c2), sym, &v)) {
      return v;
    }
  }
  if (c->tt == MRB_TT_SCLASS) {
    v = const_get_nohook(mrb, c, sym, TRUE);
    if (!mrb_undef_p(v)) {
      return v;
    }

    mrb_value klass;
    for (c2 = c; c2 && c2->tt == MRB_TT_SCLASS; c2 = mrb_class_ptr(klass)) {
      if (!iv_get(mrb, class_iv_ptr(c2), MRB_SYM(__attached__), &klass)) {
        c2 = NULL;
        break;
      }
    }
    if (c2 && (c2->tt == MRB_TT_CLASS || c2->tt == MRB_TT_MODULE)) c = c2;
  }
  return const_get(mrb, c, sym, TRUE);
}

/*
 * Sets a constant in a module or class.
 *
 * It first checks if `mod` is a class or module.
 * If the value `v` being set is a class or module, it calls
 * `mrb_class_name_class` to set up the class/module name.
 * Then, it sets the constant using `mrb_obj_iv_set` and calls
 * the `const_added` hook.
 *
 * Args:
 *   mrb: The mruby state.
 *   mod: The module or class (mrb_value) in which to set the constant.
 *   sym: The symbol representing the name of the constant.
 *   v:   The value to set for the constant.
 *
 * Raises:
 *   E_TYPE_ERROR: If `mod` is not a class or module.
 */
MRB_API void
mrb_const_set(mrb_state *mrb, mrb_value mod, mrb_sym sym, mrb_value v)
{
  mod_const_check(mrb, mod);
  if (mrb_type(v) == MRB_TT_CLASS || mrb_type(v) == MRB_TT_MODULE) {
    mrb_class_name_class(mrb, mrb_class_ptr(mod), mrb_class_ptr(v), sym);
  }
  mrb_obj_iv_set(mrb, mrb_obj_ptr(mod), sym, v);

  if (!mrb->bootstrapping) {
    mrb_value name = mrb_symbol_value(sym);
    mrb_funcall_argv(mrb, mod, MRB_SYM(const_added), 1, &name);
  }
}

/*
 * Removes a constant from a module or class.
 *
 * It first checks if `mod` is a class or module, then removes the
 * constant using `mrb_iv_remove`.
 *
 * Args:
 *   mrb: The mruby state.
 *   mod: The module or class (mrb_value) from which to remove the constant.
 *   sym: The symbol representing the name of the constant.
 *
 * Raises:
 *   E_TYPE_ERROR: If `mod` is not a class or module.
 */
MRB_API void
mrb_const_remove(mrb_state *mrb, mrb_value mod, mrb_sym sym)
{
  mod_const_check(mrb, mod);
  mrb_iv_remove(mrb, mod, sym);
}

/*
 * Defines a constant in a module or class using a symbol for the name.
 *
 * This is a direct way to set a constant without triggering `const_added` hook.
 *
 * Args:
 *   mrb:  The mruby state.
 *   mod:  The module or class (struct RClass*) in which to define the constant.
 *   name: The symbol representing the name of the constant.
 *   v:    The value to set for the constant.
 */
MRB_API void
mrb_define_const_id(mrb_state *mrb, struct RClass *mod, mrb_sym name, mrb_value v)
{
  mrb_obj_iv_set(mrb, (struct RObject*)mod, name, v);
}

/*
 * Defines a constant in a module or class using a C string for the name.
 *
 * This is a direct way to set a constant without triggering `const_added` hook.
 * The C string name is interned into a symbol.
 *
 * Args:
 *   mrb:  The mruby state.
 *   mod:  The module or class (struct RClass*) in which to define the constant.
 *   name: The C string representing the name of the constant.
 *   v:    The value to set for the constant.
 */
MRB_API void
mrb_define_const(mrb_state *mrb, struct RClass *mod, const char *name, mrb_value v)
{
  mrb_obj_iv_set(mrb, (struct RObject*)mod, mrb_intern_cstr(mrb, name), v);
}

/*
 * Defines a global constant.
 *
 * Global constants are defined in the `Object` class.
 * This function is a convenience wrapper around `mrb_define_const`.
 *
 * Args:
 *   mrb:  The mruby state.
 *   name: The C string representing the name of the global constant.
 *   val:  The value to set for the global constant.
 */
MRB_API void
mrb_define_global_const(mrb_state *mrb, const char *name, mrb_value val)
{
  mrb_define_const(mrb, mrb->object_class, name, val);
}

static int
const_i(mrb_state *mrb, mrb_sym sym, mrb_value v, void *p)
{
  mrb_value ary = *(mrb_value*)p;
  mrb_int len;
  const char* s = mrb_sym_name_len(mrb, sym, &len);

  if (len >= 1 && ISUPPER(s[0])) {
    mrb_int i, alen = RARRAY_LEN(ary);

    for (i=0; i<alen; i++) {
      if (mrb_symbol(RARRAY_PTR(ary)[i]) == sym)
        break;
    }
    if (i==alen) {
      mrb_ary_push(mrb, ary, mrb_symbol_value(sym));
    }
  }
  return 0;
}

mrb_value
mrb_mod_const_at(mrb_state *mrb, struct RClass *c, mrb_value ary)
{
  iv_foreach(mrb, class_iv_ptr(c), const_i, &ary);
  return ary;
}

/* 15.2.2.4.24 */
/*
 *  call-seq:
 *     mod.constants    -> array
 *
 *  Returns an array of all names of constants defined in the receiver.
 */
mrb_value
mrb_mod_constants(mrb_state *mrb, mrb_value mod)
{
  mrb_bool inherit = TRUE;
  struct RClass *c = mrb_class_ptr(mod);

  mrb_get_args(mrb, "|b", &inherit);
  mrb_value ary = mrb_ary_new(mrb);
  while (c) {
    mrb_mod_const_at(mrb, c, ary);
    if (!inherit) break;
    c = c->super;
    if (c == mrb->object_class) break;
  }
  return ary;
}

/*
 * Retrieves a global variable.
 *
 * Args:
 *   mrb: The mruby state.
 *   sym: The symbol representing the name of the global variable.
 *
 * Returns:
 *   The value of the global variable, or mrb_nil_value() if the
 *   global variable is not defined.
 */
MRB_API mrb_value
mrb_gv_get(mrb_state *mrb, mrb_sym sym)
{
  mrb_value v;

  if (iv_get(mrb, mrb->globals, sym, &v))
    return v;
  return mrb_nil_value();
}

/*
 * Sets a global variable.
 *
 * If the global variable table (`mrb->globals`) does not exist, it is created.
 *
 * Args:
 *   mrb: The mruby state.
 *   sym: The symbol representing the name of the global variable.
 *   v:   The value to set for the global variable.
 */
MRB_API void
mrb_gv_set(mrb_state *mrb, mrb_sym sym, mrb_value v)
{
  iv_tbl *t;

  if (!mrb->globals) {
    mrb->globals = iv_new(mrb);
  }
  t = mrb->globals;
  iv_put(mrb, t, sym, v);
}

/*
 * Removes a global variable.
 *
 * Args:
 *   mrb: The mruby state.
 *   sym: The symbol representing the name of the global variable to remove.
 */
MRB_API void
mrb_gv_remove(mrb_state *mrb, mrb_sym sym)
{
  iv_del(mrb, mrb->globals, sym, NULL);
}

static int
gv_i(mrb_state *mrb, mrb_sym sym, mrb_value v, void *p)
{
  mrb_value ary = *(mrb_value*)p;
  mrb_ary_push(mrb, ary, mrb_symbol_value(sym));
  return 0;
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
  iv_tbl *t = mrb->globals;
  mrb_value ary = mrb_ary_new(mrb);

  iv_foreach(mrb, t, gv_i, &ary);
  return ary;
}

static mrb_bool
const_defined_0(mrb_state *mrb, mrb_value mod, mrb_sym id, mrb_bool exclude, mrb_bool recurse)
{
  struct RClass *klass = mrb_class_ptr(mod);
  struct RClass *tmp;
  mrb_bool mod_retry = FALSE;

  tmp = klass;
retry:
  while (tmp) {
    if (iv_get(mrb, class_iv_ptr(tmp), id, NULL)) {
      return TRUE;
    }
    if (!recurse && (klass != mrb->object_class)) break;
    tmp = tmp->super;
  }
  if (!exclude && !mod_retry && (klass->tt == MRB_TT_MODULE)) {
    mod_retry = TRUE;
    tmp = mrb->object_class;
    goto retry;
  }
  return FALSE;
}

/*
 * Checks if a constant is defined in a module or class or its ancestors.
 *
 * This function calls `const_defined_0` with `recurse = TRUE`, meaning
 * it will search the superclass chain.
 *
 * Args:
 *   mrb: The mruby state.
 *   mod: The module or class (mrb_value) to check.
 *   id:  The symbol representing the name of the constant.
 *
 * Returns:
 *   TRUE if the constant is defined, FALSE otherwise.
 */
MRB_API mrb_bool
mrb_const_defined(mrb_state *mrb, mrb_value mod, mrb_sym id)
{
  return const_defined_0(mrb, mod, id, TRUE, TRUE);
}

/*
 * Checks if a constant is defined directly in a module or class.
 *
 * This function calls `const_defined_0` with `recurse = FALSE`, meaning
 * it will only search the given module/class, not its ancestors.
 *
 * Args:
 *   mrb: The mruby state.
 *   mod: The module or class (mrb_value) to check.
 *   id:  The symbol representing the name of the constant.
 *
 * Returns:
 *   TRUE if the constant is defined directly in the module/class, FALSE otherwise.
 */
MRB_API mrb_bool
mrb_const_defined_at(mrb_state *mrb, mrb_value mod, mrb_sym id)
{
  return const_defined_0(mrb, mod, id, TRUE, FALSE);
}

/*
 * Retrieves an attribute (instance variable) from an object.
 *
 * This function is a simple wrapper around `mrb_iv_get`.
 *
 * Args:
 *   mrb: The mruby state.
 *   obj: The object (mrb_value) from which to retrieve the attribute.
 *   id:  The symbol representing the name of the attribute (instance variable).
 *
 * Returns:
 *   The value of the attribute, or mrb_nil_value() if not defined.
 */
MRB_API mrb_value
mrb_attr_get(mrb_state *mrb, mrb_value obj, mrb_sym id)
{
  return mrb_iv_get(mrb, obj, id);
}

struct csym_arg {
  struct RClass *c;
  mrb_sym sym;
};

static int
csym_i(mrb_state *mrb, mrb_sym sym, mrb_value v, void *p)
{
  struct csym_arg *a = (struct csym_arg*)p;
  struct RClass *c = a->c;

  if (mrb_type(v) == c->tt && mrb_class_ptr(v) == c) {
    a->sym = sym;
    return 1;     /* stop iteration */
  }
  return 0;
}

static mrb_sym
find_class_sym(mrb_state *mrb, struct RClass *outer, struct RClass *c)
{
  struct csym_arg arg;

  if (!outer) return 0;
  if (outer == c) return 0;
  arg.c = c;
  arg.sym = 0;
  iv_foreach(mrb, class_iv_ptr(outer), csym_i, &arg);
  return arg.sym;
}

MRB_API struct RClass*
mrb_class_outer(mrb_state *mrb, struct RClass *c)
{
  mrb_value ov = mrb_obj_iv_get(mrb, (struct RObject*)c, MRB_SYM(__outer__));
  if (mrb_nil_p(ov)) return NULL;
  switch (mrb_type(ov)) {
  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
    return mrb_class_ptr(ov);
  default:
    break;
  }
  return NULL;
}

static mrb_bool
detect_outer_loop(mrb_state *mrb, struct RClass *c)
{
  struct RClass *t = c;         /* tortoise */
  struct RClass *h = c;         /* hare */

  for (;;) {
    if (h == NULL) return FALSE;
    h = mrb_class_outer(mrb, h);
    if (h == NULL) return FALSE;
    h = mrb_class_outer(mrb, h);
    t = mrb_class_outer(mrb, t);
    if (t == h) return TRUE;
  }
}

mrb_value
mrb_class_find_path(mrb_state *mrb, struct RClass *c)
{
  if (detect_outer_loop(mrb, c)) return mrb_nil_value();
  struct RClass *outer = mrb_class_outer(mrb, c);
  if (outer == NULL) return mrb_nil_value();

  mrb_sym name = find_class_sym(mrb, outer, c);
  if (name == 0) return mrb_nil_value();

  mrb_value path = mrb_str_new_capa(mrb, 40);
  const char *cname = mrb_class_name(mrb, outer);
  mrb_str_cat_cstr(mrb, path, cname);
  mrb_str_cat_cstr(mrb, path, "::");

  mrb_int len;
  const char *str = mrb_sym_name_len(mrb, name, &len);
  mrb_str_cat(mrb, path, str, len);
  if (RSTRING_PTR(path)[0] != '#') {
    iv_del(mrb, c->iv, MRB_SYM(__outer__), NULL);
    iv_put(mrb, c->iv, MRB_SYM(__classname__), path);
    mrb_field_write_barrier_value(mrb, (struct RBasic*)c, path);
    path = mrb_str_dup(mrb, path);
  }
  return path;
}

size_t
mrb_obj_iv_tbl_memsize(mrb_value obj)
{
  struct RObject *o = mrb_obj_ptr(obj);
  if (MRB_OBJ_SHAPED_P(o)) {
    mrb_shaped_iv *siv = (mrb_shaped_iv*)o->iv;
    if (!siv) return 0;
    return offsetof(mrb_shaped_iv, values) +
           sizeof(mrb_value) * siv->shape->count;
  }
  iv_tbl *t = o->iv;
  if (t == NULL) return 0;
  return sizeof(iv_tbl) + t->alloc*(sizeof(mrb_value)+sizeof(mrb_sym));
}

#define identchar(c) (ISALNUM(c) || (c) == '_' || !ISASCII(c))

mrb_bool
mrb_ident_p(const char *s, mrb_int len)
{
  for (mrb_int i = 0; i < len; i++) {
    if (!identchar(s[i])) return FALSE;
  }
  return TRUE;
}
