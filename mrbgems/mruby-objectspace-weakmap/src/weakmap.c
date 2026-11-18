/*
** weakmap.c - ObjectSpace::WeakMap and ObjectSpace::WeakKeyMap
**
** See Copyright Notice in mruby.h
**
** A map holds its entries in a table of its own, which the GC does not
** mark through, and links the table into the GC (mrb_gc_weak_add) so that
** once marking is done an entry whose object nothing else marked is
** dropped before the sweep frees it. WeakMap holds keys and values weakly
** and compares keys by identity. WeakKeyMap holds keys weakly, compared by
** `eql?` and `hash`, and values strongly: they are kept in an Array, at the
** position of their entry, under a hidden instance variable.
**
** The entries are kept in the order they were added, as CRuby keeps them,
** with an open-addressing index into them. An entry dropped or deleted
** stays in place, unused, until the table is rebuilt.
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/error.h>
#include <mruby/gc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

struct wm_entry {
  mrb_value key;
  mrb_value val;                /* WeakMap only */
  uint32_t hash;
  mrb_bool live;
};

struct wm {
  struct mrb_gc_weak weak;      /* first, so the GC's node is the map */
  struct wm_entry *ents;        /* in the order they were added */
  uint32_t *index;              /* 0, or an entry's position + 1 */
  uint32_t ecapa;               /* room in ents */
  uint32_t elen;                /* ents used, live or not */
  uint32_t icapa;               /* 0, or a power of 2 at least 2 * ecapa */
  uint32_t size;                /* live entries */
  uint32_t gen;                 /* bumped on each rebuild */
  uint32_t iter;                /* each running: entries keep their positions */
  mrb_bool keyed;              /* WeakKeyMap */
  struct RArray *vals;          /* WeakKeyMap's values, by position */
};

static void
wm_free(mrb_state *mrb, void *p)
{
  struct wm *m = (struct wm*)p;

  mrb_gc_weak_remove(mrb, &m->weak);
  mrb_free(mrb, m->ents);
  mrb_free(mrb, m->index);
  mrb_free(mrb, m);
}

static const struct mrb_data_type wm_type = { "ObjectSpace::WeakMap", wm_free };

static void
wm_drop(struct wm *m, uint32_t i)
{
  struct wm_entry *e = &m->ents[i];

  e->live = FALSE;
  e->key = e->val = mrb_nil_value();
  /* nil needs no write barrier */
  if (m->keyed) ARY_PTR(m->vals)[i] = mrb_nil_value();
  m->size--;
}

/* Called by the GC once marking is complete. */
static void
wm_clear_dead(mrb_state *mrb, struct mrb_gc_weak *weak)
{
  struct wm *m = (struct wm*)weak;

  for (uint32_t i = 0; i < m->elen; i++) {
    struct wm_entry *e = &m->ents[i];
    if (!e->live) continue;
    if (mrb_gc_weak_dead_p(mrb, e->key) ||
        (!m->keyed && mrb_gc_weak_dead_p(mrb, e->val))) {
      wm_drop(m, i);
    }
  }
}

static struct RArray*
wm_vals_new(mrb_state *mrb, mrb_value self, uint32_t capa)
{
  mrb_value ary = mrb_ary_new_capa(mrb, capa);
  /* the map and wm_clear_dead() index it by entry position without checking
     its length, so Ruby must not be able to find it and shrink it */
  mrb_ary_ptr(ary)->c = NULL; /* hide from ObjectSpace.each_object */
  if (capa > 0) mrb_ary_set(mrb, ary, capa-1, mrb_nil_value());
  /* a frozen map still takes entries, as in CRuby */
  mrb_obj_iv_set_force(mrb, mrb_obj_ptr(self), MRB_SYM(__values__), ary);
  return mrb_ary_ptr(ary);
}

/* The map of self, made the first time it is needed; a copy made by dup
   or clone has none of its own yet, and starts empty, as in CRuby. */
static struct wm*
wm_get(mrb_state *mrb, mrb_value self, mrb_bool create)
{
  struct wm *m = (struct wm*)mrb_data_check_get_ptr(mrb, self, &wm_type);

  if (m || !create) return m;
  struct RClass *os = mrb_module_get_id(mrb, MRB_SYM(ObjectSpace));
  mrb_bool keyed = mrb_obj_is_kind_of(mrb, self, mrb_class_get_under_id(mrb, os, MRB_SYM(WeakKeyMap)));
  /* the copied Array of the original is not this map's */
  struct RArray *vals = keyed ? wm_vals_new(mrb, self, 0) : NULL;
  m = (struct wm*)mrb_calloc(mrb, 1, sizeof(struct wm));
  m->keyed = keyed;
  m->vals = vals;
  m->weak.clear = wm_clear_dead;
  mrb_data_init(self, m, &wm_type);
  mrb_gc_weak_add(mrb, &m->weak);
  return m;
}

static uint32_t
wm_mix(uint64_t x)
{
  x ^= x >> 33;
  x *= 0xff51afd7ed558ccdULL;
  x ^= x >> 33;
  return (uint32_t)x;
}

static uint32_t
wm_hash(mrb_state *mrb, struct wm *m, mrb_value key)
{
  if (m->keyed) return mrb_obj_hash_code(mrb, key);
  return wm_mix((uint64_t)mrb_obj_id(key));
}

/* The position of `key`, or -1. WeakKeyMap's `eql?` may run Ruby code,
   which may change the table: the search starts again if it was rebuilt. */
static mrb_int
wm_find(mrb_state *mrb, struct wm *m, mrb_value key, uint32_t h)
{
 retry:
  if (m->size == 0) return -1;
  struct wm_entry *ents = m->ents;
  uint32_t gen = m->gen;
  uint32_t mask = m->icapa - 1;

  for (uint32_t s = h & mask; m->index[s]; s = (s + 1) & mask) {
    uint32_t i = m->index[s] - 1;
    struct wm_entry *e = &ents[i];
    if (!e->live || e->hash != h) continue;
    if (mrb_obj_eq(mrb, e->key, key)) return i;
    if (!m->keyed) continue;

    mrb_value k = e->key;
    int ai = mrb_gc_arena_save(mrb);
    mrb_gc_protect(mrb, k);
    /* growing the arena may collect, and drop the entry */
    if (!e->live) {
      mrb_gc_arena_restore(mrb, ai);
      continue;
    }
    mrb_bool eq = mrb_eql(mrb, k, key);
    mrb_gc_arena_restore(mrb, ai);
    if (m->gen != gen) goto retry;
    if (eq && e->live && mrb_obj_eq(mrb, e->key, k)) return i;
  }
  return -1;
}

static void
wm_index_add(struct wm *m, uint32_t i)
{
  uint32_t mask = m->icapa - 1, s = m->ents[i].hash & mask;

  while (m->index[s]) s = (s + 1) & mask;
  m->index[s] = i + 1;
}

/* Room for one more entry: when ents is full, the table is rebuilt with
   the live entries alone, in their order; while each runs, with all of
   them, in their positions. */
static void
wm_reserve(mrb_state *mrb, mrb_value self, struct wm *m)
{
  if (m->elen < m->ecapa) return;

  mrb_bool compact = m->iter == 0;
  uint32_t need = compact ? m->size : m->elen;
  uint32_t ecapa = 8;
  while (ecapa < need * 2) ecapa *= 2;
  /* each may collect, which only drops entries of the old table */
  struct wm_entry *ents = (struct wm_entry*)mrb_calloc(mrb, ecapa, sizeof(struct wm_entry));
  uint32_t *index = (uint32_t*)mrb_calloc(mrb, ecapa * 2, sizeof(uint32_t));
  struct RArray *vals = m->keyed ? wm_vals_new(mrb, self, ecapa) : NULL;
  uint32_t n = 0;

  for (uint32_t j = 0; j < m->elen; j++) {
    if (compact && !m->ents[j].live) continue;
    ents[n] = m->ents[j];
    if (vals) ARY_PTR(vals)[n] = ARY_PTR(m->vals)[j];
    n++;
  }
  if (vals) mrb_write_barrier(mrb, (struct RBasic*)vals);
  mrb_free(mrb, m->ents);
  mrb_free(mrb, m->index);
  m->ents = ents;
  m->index = index;
  m->ecapa = ecapa;
  m->icapa = ecapa * 2;
  m->elen = n;
  m->vals = vals;
  m->gen++;
  for (uint32_t i = 0; i < n; i++) {
    if (ents[i].live) wm_index_add(m, i);
  }
}

static mrb_value
wm_value_at(struct wm *m, mrb_int i)
{
  return m->keyed ? ARY_PTR(m->vals)[i] : m->ents[i].val;
}

static mrb_int
wm_lookup(mrb_state *mrb, mrb_value self, mrb_value key, struct wm **mp)
{
  struct wm *m = wm_get(mrb, self, FALSE);
  *mp = m;
  if (!m || m->size == 0) return -1;
  return wm_find(mrb, m, key, wm_hash(mrb, m, key));
}

static void
wm_set(mrb_state *mrb, mrb_value self, mrb_value key, mrb_value val)
{
  struct wm *m = wm_get(mrb, self, TRUE);
  uint32_t h = wm_hash(mrb, m, key);
  mrb_int i = wm_find(mrb, m, key, h);

  if (i < 0) {
    wm_reserve(mrb, self, m);
    i = m->elen++;
    struct wm_entry *e = &m->ents[i];
    e->key = key;
    e->val = mrb_nil_value();
    e->hash = h;
    e->live = TRUE;
    m->size++;
    wm_index_add(m, (uint32_t)i);
  }
  if (m->keyed) {
    mrb_ary_set(mrb, mrb_obj_value(m->vals), i, val);
  }
  else {
    m->ents[i].val = val;
  }
}

static mrb_value
wm_delete(mrb_state *mrb, mrb_value self)
{
  mrb_value key, blk;
  struct wm *m;

  mrb_get_args(mrb, "o&", &key, &blk);
  mrb_int i = wm_lookup(mrb, self, key, &m);
  if (i < 0) {
    if (mrb_nil_p(blk)) return mrb_nil_value();
    return mrb_yield(mrb, blk, key);
  }
  mrb_value val = wm_value_at(m, i);
  wm_drop(m, (uint32_t)i);
  return val;
}

static mrb_value
wm_aref(mrb_state *mrb, mrb_value self)
{
  mrb_value key = mrb_get_arg1(mrb);
  struct wm *m;
  mrb_int i = wm_lookup(mrb, self, key, &m);

  if (i < 0) return mrb_nil_value();
  return wm_value_at(m, i);
}

static mrb_value
wm_key_p(mrb_state *mrb, mrb_value self)
{
  mrb_value key = mrb_get_arg1(mrb);
  struct wm *m;

  return mrb_bool_value(wm_lookup(mrb, self, key, &m) >= 0);
}

/*
 * call-seq:
 *   map[key] = value  -> value
 *
 * Holds `value` under `key`, until either is collected.
 */
static mrb_value
wmap_aset(mrb_state *mrb, mrb_value self)
{
  mrb_value key, val;

  mrb_get_args(mrb, "oo", &key, &val);
  wm_set(mrb, self, key, val);
  return val;
}

static mrb_value
wmap_size(mrb_state *mrb, mrb_value self)
{
  struct wm *m = wm_get(mrb, self, FALSE);
  return mrb_int_value(mrb, m ? m->size : 0);
}

enum { WM_KEYS, WM_VALUES };

static mrb_value
wmap_list(mrb_state *mrb, mrb_value self, int which)
{
  struct wm *m = wm_get(mrb, self, FALSE);
  if (!m) return mrb_ary_new(mrb);

  /* made to size, so the loop allocates nothing, and nothing is collected */
  mrb_value ary = mrb_ary_new_capa(mrb, m->size);
  for (uint32_t i = 0; i < m->elen; i++) {
    struct wm_entry *e = &m->ents[i];
    if (!e->live) continue;
    mrb_ary_push(mrb, ary, which == WM_KEYS ? e->key : e->val);
  }
  return ary;
}

static mrb_value
wmap_keys(mrb_state *mrb, mrb_value self)
{
  return wmap_list(mrb, self, WM_KEYS);
}

static mrb_value
wmap_values(mrb_state *mrb, mrb_value self)
{
  return wmap_list(mrb, self, WM_VALUES);
}

enum { WM_EACH_PAIR, WM_EACH_KEY, WM_EACH_VALUE };

/* Protects the key and the value of a live WeakMap entry, and says whether
   it still is one: growing the arena may collect, and drop the entry, whose
   objects are then not to be touched. */
static mrb_bool
wm_protect_entry(mrb_state *mrb, struct wm_entry *e, mrb_value k, mrb_value v)
{
  mrb_gc_protect(mrb, k);
  mrb_gc_protect(mrb, v);
  return e->live;
}

struct wm_each {
  struct wm *m;
  mrb_value blk;
  int which;
};

/* The block may change the map, or collect: each entry is read anew. An
   entry added meanwhile is yielded too, as in CRuby. */
static mrb_value
wmap_each_body(mrb_state *mrb, void *p)
{
  struct wm_each *a = (struct wm_each*)p;
  struct wm *m = a->m;
  mrb_value blk = a->blk;
  int which = a->which;

  for (uint32_t i = 0; i < m->elen; i++) {
    struct wm_entry *e = &m->ents[i];
    if (!e->live) continue;
    mrb_value argv[2] = { e->key, e->val };
    int ai = mrb_gc_arena_save(mrb);
    /* the block's frame may be allocated before it holds them */
    if (!wm_protect_entry(mrb, e, argv[0], argv[1])) {
      mrb_gc_arena_restore(mrb, ai);
      continue;
    }
    switch (which) {
    case WM_EACH_PAIR: mrb_yield_argv(mrb, blk, 2, argv); break;
    case WM_EACH_KEY: mrb_yield(mrb, blk, argv[0]); break;
    default: mrb_yield(mrb, blk, argv[1]); break;
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  return mrb_nil_value();
}

/* While the block runs, a rebuild keeps each entry in its position, so
   that none is skipped. */
static mrb_value
wmap_each_by(mrb_state *mrb, mrb_value self, int which)
{
  mrb_value blk;
  mrb_bool error;

  mrb_get_args(mrb, "&", &blk);
  if (mrb_nil_p(blk)) mrb_raise(mrb, E_LOCALJUMP_ERROR, "no block given (yield)");
  struct wm *m = wm_get(mrb, self, FALSE);
  if (!m) return self;
  struct wm_each a = { m, blk, which };
  m->iter++;
  mrb_value exc = mrb_protect_error(mrb, wmap_each_body, &a, &error);
  m->iter--;
  if (error) mrb_exc_raise(mrb, exc);
  return self;
}

static mrb_value
wmap_each(mrb_state *mrb, mrb_value self)
{
  return wmap_each_by(mrb, self, WM_EACH_PAIR);
}

static mrb_value
wmap_each_key(mrb_state *mrb, mrb_value self)
{
  return wmap_each_by(mrb, self, WM_EACH_KEY);
}

static mrb_value
wmap_each_value(mrb_state *mrb, mrb_value self)
{
  return wmap_each_by(mrb, self, WM_EACH_VALUE);
}

/* "#<ObjectSpace::WeakMap:0x...", for the caller to close */
static mrb_value
wm_inspect_head(mrb_state *mrb, mrb_value self)
{
  mrb_value str = mrb_any_to_s(mrb, self);
  return mrb_str_resize(mrb, str, RSTRING_LEN(str) - 1);
}

/* as CRuby shows them: an object by its class and address */
static void
wm_inspect_cat(mrb_state *mrb, mrb_value str, mrb_value v)
{
  if (mrb_immediate_p(v)) mrb_str_cat_str(mrb, str, mrb_inspect(mrb, v));
  else mrb_str_cat_str(mrb, str, mrb_any_to_s(mrb, v));
}

static mrb_value
wmap_inspect(mrb_state *mrb, mrb_value self)
{
  mrb_value str = wm_inspect_head(mrb, self);
  struct wm *m = wm_get(mrb, self, FALSE);
  const char *sep = ": ";

  for (uint32_t i = 0; m && i < m->elen; i++) {
    struct wm_entry *e = &m->ents[i];
    if (!e->live) continue;
    mrb_value k = e->key, v = e->val;
    int ai = mrb_gc_arena_save(mrb);
    if (!wm_protect_entry(mrb, e, k, v)) {
      mrb_gc_arena_restore(mrb, ai);
      continue;
    }
    mrb_str_cat_cstr(mrb, str, sep);
    wm_inspect_cat(mrb, str, k);
    mrb_str_cat_lit(mrb, str, " => ");
    wm_inspect_cat(mrb, str, v);
    mrb_gc_arena_restore(mrb, ai);
    sep = ", ";
  }
  mrb_str_cat_lit(mrb, str, ">");
  return str;
}

/*
 * call-seq:
 *   map[key] = value  -> value
 *
 * Holds `value` under `key`, until `key` is collected. A key that is not
 * collected, such as an Integer or a Symbol, raises ArgumentError.
 */
static mrb_value
wkmap_aset(mrb_state *mrb, mrb_value self)
{
  mrb_value key, val;

  mrb_get_args(mrb, "oo", &key, &val);
  if (mrb_immediate_p(key)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "WeakKeyMap keys must be garbage collectable");
  }
  wm_set(mrb, self, key, val);
  return val;
}

/*
 * call-seq:
 *   map.getkey(key)  -> key or nil
 *
 * The key the map holds that is `eql?` to `key`.
 */
static mrb_value
wkmap_getkey(mrb_state *mrb, mrb_value self)
{
  mrb_value key = mrb_get_arg1(mrb);
  struct wm *m;
  mrb_int i = wm_lookup(mrb, self, key, &m);

  if (i < 0) return mrb_nil_value();
  return m->ents[i].key;
}

static mrb_value
wkmap_clear(mrb_state *mrb, mrb_value self)
{
  struct wm *m = wm_get(mrb, self, FALSE);

  if (m) {
    for (uint32_t i = 0; i < m->elen; i++) {
      if (m->ents[i].live) wm_drop(m, i);
    }
  }
  return self;
}

static mrb_value
wkmap_inspect(mrb_state *mrb, mrb_value self)
{
  mrb_value str = wm_inspect_head(mrb, self);
  struct wm *m = wm_get(mrb, self, FALSE);

  mrb_str_cat_str(mrb, str, mrb_format(mrb, " size=%i>", (mrb_int)(m ? m->size : 0)));
  return str;
}

void
mrb_mruby_objectspace_weakmap_gem_init(mrb_state *mrb)
{
  struct RClass *os = mrb_define_module_id(mrb, MRB_SYM(ObjectSpace));

  struct RClass *wmap = mrb_define_class_under_id(mrb, os, MRB_SYM(WeakMap), mrb->object_class);
  MRB_SET_INSTANCE_TT(wmap, MRB_TT_CDATA);
  mrb_include_module(mrb, wmap, mrb_module_get_id(mrb, MRB_SYM(Enumerable)));
  mrb_define_method_id(mrb, wmap, MRB_OPSYM(aref), wm_aref, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, wmap, MRB_OPSYM(aset), wmap_aset, MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, wmap, MRB_SYM(delete), wm_delete, MRB_ARGS_REQ(1)|MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, wmap, MRB_SYM(each), wmap_each, MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, wmap, MRB_SYM(each_key), wmap_each_key, MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, wmap, MRB_SYM(each_pair), wmap_each, MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, wmap, MRB_SYM(each_value), wmap_each_value, MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, wmap, MRB_SYM_Q(include), wm_key_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, wmap, MRB_SYM(inspect), wmap_inspect, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, wmap, MRB_SYM_Q(key), wm_key_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, wmap, MRB_SYM(keys), wmap_keys, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, wmap, MRB_SYM(length), wmap_size, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, wmap, MRB_SYM_Q(member), wm_key_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, wmap, MRB_SYM(size), wmap_size, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, wmap, MRB_SYM(values), wmap_values, MRB_ARGS_NONE());

  struct RClass *wkmap = mrb_define_class_under_id(mrb, os, MRB_SYM(WeakKeyMap), mrb->object_class);
  MRB_SET_INSTANCE_TT(wkmap, MRB_TT_CDATA);
  mrb_define_method_id(mrb, wkmap, MRB_OPSYM(aref), wm_aref, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, wkmap, MRB_OPSYM(aset), wkmap_aset, MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, wkmap, MRB_SYM(clear), wkmap_clear, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, wkmap, MRB_SYM(delete), wm_delete, MRB_ARGS_REQ(1)|MRB_ARGS_BLOCK());
  mrb_define_method_id(mrb, wkmap, MRB_SYM(getkey), wkmap_getkey, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, wkmap, MRB_SYM(inspect), wkmap_inspect, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, wkmap, MRB_SYM_Q(key), wm_key_p, MRB_ARGS_REQ(1));
}

void
mrb_mruby_objectspace_weakmap_gem_final(mrb_state *mrb)
{
}
