/*
** hash.c - Hash class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/hash.h>
#include <mruby/class.h>
#include <mruby/presym.h>

/*
 * call-seq:
 *   hsh.values_at(key, ...)   -> array
 *
 * Return an array containing the values associated with the given keys.
 * Also see <code>Hash.select</code>.
 *
 *   h = { "cat" => "feline", "dog" => "canine", "cow" => "bovine" }
 *   h.values_at("cow", "cat")  #=> ["bovine", "feline"]
 */

static mrb_value
hash_values_at(mrb_state *mrb, mrb_value hash)
{
  const mrb_value *argv;
  mrb_value result;
  mrb_int argc;
  int ai;

  mrb_get_args(mrb, "*", &argv, &argc);
  result = mrb_ary_new_capa(mrb, argc);
  if (argc == 0) return result;
  ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < argc; i++) {
    mrb_ary_push(mrb, result, mrb_hash_get(mrb, hash, argv[i]));
    mrb_gc_arena_restore(mrb, ai);
  }
  return result;
}

/*
 *  call-seq:
 *     hsh.slice(*keys) -> a_hash
 *
 *  Returns a hash containing only the given keys and their values.
 *
 *     h = { a: 100, b: 200, c: 300 }
 *     h.slice(:a)           #=> {:a=>100}
 *     h.slice(:b, :c, :d)   #=> {:b=>200, :c=>300}
 */
static mrb_value
hash_slice(mrb_state *mrb, mrb_value hash)
{
  const mrb_value *argv;
  mrb_value result;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  result = mrb_hash_new_capa(mrb, argc);
  if (argc == 0) return result; /* empty hash */
  for (mrb_int i = 0; i < argc; i++) {
    mrb_value key = argv[i];
    mrb_value val;

    val = mrb_hash_fetch(mrb, hash, key, mrb_undef_value());
    if (!mrb_undef_p(val)) {
      mrb_hash_set(mrb, result, key, val);
    }
  }
  return result;
}

struct slice_bang_i_arg {
  mrb_value keep_keys;
  mrb_value keys_to_remove;
};

/*
 * Iterator for `slice!`.
 *
 * Iterates over a hash, identifying keys that are not present in the `keep_keys`
 * hash. Keys identified for removal are appended to the `keys_to_remove` array.
 *
 * @param mrb   The mruby state.
 * @param key   The key of the current hash entry.
 * @param val   The value of the current hash entry (unused).
 * @param data  A pointer to a `slice_bang_i_arg` struct, which contains
 *              `keep_keys` and `keys_to_remove`.
 * @return      Always returns `0` to ensure the iteration continues over all
 *              hash entries.
 */
static int
slice_bang_i(mrb_state *mrb, mrb_value key, mrb_value val, void *data)
{
  struct slice_bang_i_arg *args = (struct slice_bang_i_arg *)data;
  if (!mrb_hash_key_p(mrb, args->keep_keys, key)) {
    mrb_ary_push(mrb, args->keys_to_remove, key);
  }
  return 0; /* Continue iteration */
}

/*
 *  call-seq:
 *     hsh.slice!(*keys) -> a_hash
 *
 *  Deletes keys from hsh that are not in +keys+.
 *  Returns a new hash containing the deleted key-value pairs.
 *
 *     h = { a: 1, b: 2, c: 3, d: 4 }
 *     h.slice!(:a, :c) #=> { b: 2, d: 4 }
 *     h                #=> { a: 1, c: 3 }
 */
static mrb_value
hash_slice_bang(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_value removed_hash;
  struct slice_bang_i_arg args;
  mrb_int i, len;

  mrb_get_args(mrb, "*", &argv, &argc);

  args.keep_keys = mrb_hash_new_capa(mrb, argc);
  for (i = 0; i < argc; i++) {
    mrb_hash_set(mrb, args.keep_keys, argv[i], mrb_true_value());
  }

  args.keys_to_remove = mrb_ary_new(mrb);
  mrb_hash_foreach(mrb, mrb_hash_ptr(self), slice_bang_i, &args);

  len = RARRAY_LEN(args.keys_to_remove);
  removed_hash = mrb_hash_new_capa(mrb, len);
  for (i = 0; i < len; i++) {
    mrb_value key = mrb_ary_ref(mrb, args.keys_to_remove, i);
    mrb_value val = mrb_hash_delete_key(mrb, self, key);
    mrb_hash_set(mrb, removed_hash, key, val);
  }

  return removed_hash;
}

/*
 *  call-seq:
 *     hsh.except(*keys) -> a_hash
 *
 *  Returns a hash excluding the given keys and their values.
 *
 *     h = { a: 100, b: 200, c: 300 }
 *     h.except(:a)          #=> {:b=>200, :c=>300}
 *     h.except(:b, :c, :d)  #=> {:a=>100}
 */
static mrb_value
hash_except(mrb_state *mrb, mrb_value hash)
{
  const mrb_value *argv;
  mrb_value result;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  result = mrb_hash_dup(mrb, hash);
  for (mrb_int i = 0; i < argc; i++) {
    mrb_hash_delete_key(mrb, result, argv[i]);
  }
  return result;
}

/*
 *  call-seq:
 *     Hash[ key, value, ... ] -> new_hash
 *     Hash[ [ [key, value], ... ] ] -> new_hash
 *     Hash[ object ] -> new_hash
 *
 *  Creates a new hash populated with the given objects.
 *
 *  Similar to the literal `{ _key_ => _value_, ... }`. In the first
 *  form, keys and values occur in pairs, so there must be an even number of
 *  arguments.
 *
 *  The second and third form take a single argument which is either an array
 *  of key-value pairs or an object convertible to a hash.
 *
 *     Hash["a", 100, "b", 200] #=> {"a"=>100, "b"=>200}
 *     Hash[ [ ["a", 100], ["b", 200] ] ] #=> {"a"=>100, "b"=>200}
 *     Hash["a" => 100, "b" => 200] #=> {"a"=>100, "b"=>200}
 */
static mrb_value
hash_s_create(mrb_state *mrb, mrb_value klass)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_value hash;

  mrb_get_args(mrb, "*", &argv, &argc);

  if (argc == 1) {
    mrb_value obj = argv[0];

    /* Case 1: Hash argument - copy constructor */
    if (mrb_hash_p(obj)) {
      hash = mrb_hash_new(mrb);
      mrb_hash_merge(mrb, hash, obj);
      /* Set the correct class if it's a subclass */
      if (mrb_class_ptr(klass) != mrb->hash_class) {
        mrb_obj_ptr(hash)->c = mrb_class_ptr(klass);
      }
      return hash;
    }

    /* Case 2: Array argument with nested arrays */
    if (mrb_array_p(obj)) {
      mrb_int ary_len = RARRAY_LEN(obj);
      hash = mrb_hash_new_capa(mrb, ary_len);

      for (mrb_int i = 0; i < ary_len; i++) {
        mrb_value elem = mrb_ary_ref(mrb, obj, i);
        mrb_value key = mrb_nil_value(), val = mrb_nil_value();

        if (!mrb_array_p(elem)) {
          mrb_raisef(mrb, E_ARGUMENT_ERROR,
                     "wrong element type %C (expected array)", mrb_obj_class(mrb, elem));
        }

        mrb_int elem_len = RARRAY_LEN(elem);

        switch (elem_len) {
        case 2:
          key = mrb_ary_ref(mrb, elem, 0);
          val = mrb_ary_ref(mrb, elem, 1);
          break;
        case 1:
          key = mrb_ary_ref(mrb, elem, 0);
          val = mrb_nil_value();
          break;
        case 0:
        default:
          mrb_raisef(mrb, E_ARGUMENT_ERROR,
                     "invalid number of elements (%i for 1..2)", elem_len);
        }

        mrb_hash_set(mrb, hash, key, val);
      }
      /* Set the correct class if it's a subclass */
      if (mrb_class_ptr(klass) != mrb->hash_class) {
        mrb_obj_ptr(hash)->c = mrb_class_ptr(klass);
      }
      return hash;
    }
  }

  /* Case 3: Multiple arguments as key-value pairs */
  if (argc % 2 != 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "odd number of arguments for Hash");
  }

  hash = mrb_hash_new_capa(mrb, argc / 2);
  for (mrb_int i = 0; i < argc; i += 2) {
    mrb_hash_set(mrb, hash, argv[i], argv[i + 1]);
  }

  /* Set the correct class if it's a subclass */
  if (mrb_class_ptr(klass) != mrb->hash_class) {
    mrb_obj_ptr(hash)->c = mrb_class_ptr(klass);
  }

  return hash;
}

/* Data structure for hash_key search */
struct key_search {
  mrb_value target;
  mrb_value result;
  mrb_bool found;
};

/*
 * Iterator for `key`.
 *
 * This function is designed to be used with `mrb_hash_foreach`. It iterates
 * over hash entries to find a key that corresponds to a specific target value.
 *
 * When a match is found, it stores the key, sets a flag, and stops the
 * iteration.
 *
 * @param mrb    The mruby state.
 * @param key    The key of the current hash entry.
 * @param val    The value of the current hash entry.
 * @param data   A pointer to a `key_search` struct, which contains the
 *               target value and holds the result.
 * @return       Returns `1` to stop the iteration once a match is found,
 *               otherwise returns `0` to continue.
 */
static int
hash_key_i(mrb_state *mrb, mrb_value key, mrb_value val, void *data)
{
  struct key_search *search = (struct key_search*)data;
  if (mrb_equal(mrb, val, search->target)) {
    search->result = key;
    search->found = TRUE;
    return 1; /* Stop iteration */
  }
  return 0; /* Continue iteration */
}

/*
 *  call-seq:
 *     hsh.key(value)    -> key
 *
 *  Returns the key of an occurrence of a given value. If the value is
 *  not found, returns <code>nil</code>.
 *
 *     h = { "a" => 100, "b" => 200, "c" => 300, "d" => 300 }
 *     h.key(200)   #=> "b"
 *     h.key(300)   #=> "c" or "d"
 *     h.key(999)   #=> nil
 */
static mrb_value
hash_key(mrb_state *mrb, mrb_value hash)
{
  mrb_value val;
  struct key_search search;

  mrb_get_args(mrb, "o", &val);

  search.target = val;
  search.result = mrb_nil_value();
  search.found = FALSE;

  mrb_hash_foreach(mrb, mrb_hash_ptr(hash), hash_key_i, &search);

  return search.found ? search.result : mrb_nil_value();
}

/*
 *  call-seq:
 *     hsh.deconstruct_keys(keys) -> hash
 *
 *  Returns a hash containing the contents of hsh for the given keys.
 *
 *  If keys is nil, returns the hash itself.
 *  If keys is an array, returns a new hash containing only the specified keys.
 *
 *     h = { a: 1, b: 2, c: 3, d: 4 }
 *     h.deconstruct_keys([:a, :c])   #=> { a: 1, c: 3 }
 *     h.deconstruct_keys(nil)        #=> { a: 1, b: 2, c: 3, d: 4 }
 *     h.deconstruct_keys([:x, :y])   #=> { x: nil, y: nil }
 */
static mrb_value
hash_deconstruct_keys(mrb_state *mrb, mrb_value hash)
{
  mrb_value keys;
  mrb_value result;
  mrb_int i, len;

  mrb_get_args(mrb, "o", &keys);

  /* If keys is nil, return the hash itself */
  if (mrb_nil_p(keys)) {
    return hash;
  }

  /* Keys must be an array */
  if (!mrb_array_p(keys)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %C (expected Array or nil)",
               mrb_obj_class(mrb, keys));
  }

  /* Create result hash */
  result = mrb_hash_new(mrb);
  len = RARRAY_LEN(keys);

  /* Extract specified keys */
  for (i = 0; i < len; i++) {
    mrb_value key = mrb_ary_ref(mrb, keys, i);
    mrb_value val = mrb_hash_get(mrb, hash, key);
    mrb_hash_set(mrb, result, key, val);
  }

  return result;
}

/*
 *  call-seq:
 *     hsh.__merge(*others) -> hsh
 *
 *  Merges multiple hashes into hsh. This is an internal method
 *  used by merge! for non-block cases.
 *
 *  Raises ArgumentError if no arguments given.
 *  Raises TypeError if any argument is not a Hash.
 *
 *     h = { a: 1, b: 2 }
 *     h.__merge({ c: 3 }, { d: 4 })  #=> { a: 1, b: 2, c: 3, d: 4 }
 */
static mrb_value
hash_merge(mrb_state *mrb, mrb_value hash)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);

  /* Validate arguments */
  if (argc == 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (given 0, expected 1+)");
  }

  /* Merge multiple hashes in C */
  for (mrb_int i = 0; i < argc; i++) {
    if (!mrb_hash_p(argv[i])) {
      mrb_raisef(mrb, E_TYPE_ERROR, "no implicit conversion of %C into Hash",
                 mrb_obj_class(mrb, argv[i]));
    }
    mrb_hash_merge(mrb, hash, argv[i]);
  }

  return hash;
}

void
mrb_mruby_hash_ext_gem_init(mrb_state *mrb)
{
  struct RClass *h;

  h = mrb->hash_class;
  mrb_define_method_id(mrb, h, MRB_SYM(values_at), hash_values_at, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, h, MRB_SYM(slice),     hash_slice, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, h, MRB_SYM_B(slice),    hash_slice_bang, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, h, MRB_SYM(except),    hash_except, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, h, MRB_SYM(key),       hash_key, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, h, MRB_SYM(deconstruct_keys), hash_deconstruct_keys, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, h, MRB_SYM(__merge),   hash_merge, MRB_ARGS_ANY());
  mrb_define_class_method_id(mrb, h, MRB_OPSYM(aref), hash_s_create, MRB_ARGS_ANY());
}

void
mrb_mruby_hash_ext_gem_final(mrb_state *mrb)
{
}
