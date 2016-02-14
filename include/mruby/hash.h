/*
** mruby/hash.h - Hash class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_HASH_H
#define MRUBY_HASH_H

#include "common.h"
#include <mruby/khash.h>

/**
 * Hash class
 */
MRB_BEGIN_DECL

struct RHash {
  MRB_OBJECT_HEADER;
  struct iv_tbl *iv;
  struct kh_ht *ht;
};

#define mrb_hash_ptr(v)    ((struct RHash*)(mrb_ptr(v)))
#define mrb_hash_value(p)  mrb_obj_value((void*)(p))

MRB_API mrb_value mrb_hash_new_capa(mrb_state*, int);

/*
 * Initializes a new hash.
 */
MRB_API mrb_value mrb_hash_new(mrb_state *mrb);

/*
 * Sets a keys and values to hashes.
 */
MRB_API void mrb_hash_set(mrb_state *mrb, mrb_value hash, mrb_value key, mrb_value val);

/*
 * Gets a value from a key.
 */
MRB_API mrb_value mrb_hash_get(mrb_state *mrb, mrb_value hash, mrb_value key);

MRB_API mrb_value mrb_hash_fetch(mrb_state *mrb, mrb_value hash, mrb_value key, mrb_value def);

/*
 * Deletes hash key and value pair.
 */
MRB_API mrb_value mrb_hash_delete_key(mrb_state *mrb, mrb_value hash, mrb_value key);

/*
 * Gets an array of keys.
 */
MRB_API mrb_value mrb_hash_keys(mrb_state *mrb, mrb_value hash);
MRB_API mrb_value mrb_check_hash_type(mrb_state *mrb, mrb_value hash);
MRB_API mrb_value mrb_hash_empty_p(mrb_state *mrb, mrb_value self);

/*
 * Clears the hash.
 */
MRB_API mrb_value mrb_hash_clear(mrb_state *mrb, mrb_value hash);

/* declaration of struct kh_ht */
/* be careful when you touch the internal */
typedef struct {
  mrb_value v;
  mrb_int n;
} mrb_hash_value;

KHASH_DECLARE(ht, mrb_value, mrb_hash_value, TRUE)

/* RHASH_TBL allocates st_table if not available. */
#define RHASH(obj)   ((struct RHash*)(mrb_ptr(obj)))
#define RHASH_TBL(h)          (RHASH(h)->ht)
#define RHASH_IFNONE(h)       mrb_iv_get(mrb, (h), mrb_intern_lit(mrb, "ifnone"))
#define RHASH_PROCDEFAULT(h)  RHASH_IFNONE(h)
MRB_API struct kh_ht * mrb_hash_tbl(mrb_state *mrb, mrb_value hash);

#define MRB_HASH_DEFAULT      1
#define MRB_HASH_PROC_DEFAULT 2
#define MRB_RHASH_DEFAULT_P(h) (RHASH(h)->flags & MRB_HASH_DEFAULT)
#define MRB_RHASH_PROCDEFAULT_P(h) (RHASH(h)->flags & MRB_HASH_PROC_DEFAULT)

/* GC functions */
void mrb_gc_mark_hash(mrb_state*, struct RHash*);
size_t mrb_gc_mark_hash_size(mrb_state*, struct RHash*);
void mrb_gc_free_hash(mrb_state*, struct RHash*);

MRB_END_DECL

#endif  /* MRUBY_HASH_H */
