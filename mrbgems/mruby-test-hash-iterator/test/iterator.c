#include <mruby.h>
#include <mruby/class.h>
#include <mruby/compile.h>
#include <mruby/data.h>
#include <mruby/hash.h>
#include <mruby/value.h>
#include <mruby/variable.h>

struct hash_iterator {
  mrb_value hash;
  mrb_hash_iterator it;
};

static void
hash_iterator_free(mrb_state *mrb, void *p) {
  struct hash_iterator *iterator = (struct hash_iterator *)p;
  mrb_free(mrb, iterator);
}

static mrb_data_type hash_iterator_type = {"HashIterator", hash_iterator_free};

static mrb_value
hash_iterator_initialize(mrb_state *mrb, mrb_value self)
{
  struct hash_iterator *iterator = (struct hash_iterator *)DATA_PTR(self);
  if (iterator) {
    hash_iterator_free(mrb, iterator);
    mrb_data_init(self, NULL, &hash_iterator_type);
  }

  iterator = (struct hash_iterator *)mrb_malloc(mrb, sizeof(struct hash_iterator));
  mrb_data_init(self, iterator, &hash_iterator_type);

  iterator->hash = mrb_get_arg1(mrb);
  mrb_iv_set(mrb, self, mrb_intern_cstr(mrb, "hash"), iterator->hash);

  iterator->it = mrb_hash_iterator_new(mrb_hash_ptr(iterator->hash));

  return self;
}

static mrb_value
hash_iterator_move_next(mrb_state *mrb, mrb_value self)
{
  struct hash_iterator *iterator = (struct hash_iterator *)DATA_PTR(self);
  mrb_bool has_next = mrb_hash_iterator_move_next(&iterator->it);

  return mrb_bool_value(has_next);
}

static mrb_value
hash_iterator_remaining(mrb_state *mrb, mrb_value self)
{
  struct hash_iterator *iterator = (struct hash_iterator *)DATA_PTR(self);
  uint32_t remaining = mrb_hash_iterator_remaining(&iterator->it);

  return mrb_int_value(mrb, (mrb_int)remaining);
}

static mrb_value
hash_iterator_key(mrb_state *mrb, mrb_value self)
{
  struct hash_iterator *iterator = (struct hash_iterator *)DATA_PTR(self);

  return mrb_hash_iterator_key(&iterator->it);
}

static mrb_value
hash_iterator_value(mrb_state *mrb, mrb_value self)
{
  struct hash_iterator *iterator = (struct hash_iterator *)DATA_PTR(self);

  return mrb_hash_iterator_value(&iterator->it);
}

#define CHECK(...) do { \
    mrb_bool result = (__VA_ARGS__); \
    if (!result) { \
      mrb_raise(mrb, E_RUNTIME_ERROR, (#__VA_ARGS__)); \
      return mrb_nil_value(); \
    } \
  } while (0)

#define CHECK_EQ(Expected, ...) \
  CHECK(mrb_eql(mrb, (Expected), (__VA_ARGS__)))

static mrb_value
hash_iterator_test1(mrb_state *mrb, mrb_value self) {
  int checkpoint = mrb_gc_arena_save(mrb);
  mrb_value hash = mrb_load_string(mrb, "{a: 10, b: 20, c: 30, d: 40, e: 50}");
  mrb_gc_arena_restore(mrb, checkpoint);

  mrb_value a = mrb_symbol_value(mrb_intern_cstr(mrb, "a"));
  mrb_value b = mrb_symbol_value(mrb_intern_cstr(mrb, "b"));
  mrb_value c = mrb_symbol_value(mrb_intern_cstr(mrb, "c"));
  mrb_value d = mrb_symbol_value(mrb_intern_cstr(mrb, "d"));
  mrb_value e = mrb_symbol_value(mrb_intern_cstr(mrb, "e"));

  mrb_value i10 = mrb_int_value(mrb, 10);
  mrb_value i20 = mrb_int_value(mrb, 20);
  mrb_value i30 = mrb_int_value(mrb, 30);
  mrb_value i40 = mrb_int_value(mrb, 40);
  mrb_value i50 = mrb_int_value(mrb, 50);

  mrb_hash_iterator iterator = mrb_hash_iterator_new(mrb_hash_ptr(hash));

  CHECK(mrb_hash_iterator_remaining(&iterator) == 5);
  CHECK_EQ(a, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i10, mrb_hash_iterator_value(&iterator));

  CHECK(mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 4);
  CHECK_EQ(b, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i20, mrb_hash_iterator_value(&iterator));

  CHECK(mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 3);
  CHECK_EQ(c, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i30, mrb_hash_iterator_value(&iterator));

  CHECK(mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 2);
  CHECK_EQ(d, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i40, mrb_hash_iterator_value(&iterator));

  CHECK(mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 1);
  CHECK_EQ(e, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i50, mrb_hash_iterator_value(&iterator));

  CHECK(!mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 0);

  return mrb_nil_value();
}

static mrb_value
hash_iterator_test2(mrb_state *mrb, mrb_value self) {
  int checkpoint = mrb_gc_arena_save(mrb);
  mrb_value hash = mrb_load_string(mrb,
    "x = {a: 10, b: 20, c: 30, d: 40, e: 50}\n"
    "x.delete(:c)\n"
    "x"
  );
  mrb_gc_arena_restore(mrb, checkpoint);

  mrb_value a = mrb_symbol_value(mrb_intern_cstr(mrb, "a"));
  mrb_value b = mrb_symbol_value(mrb_intern_cstr(mrb, "b"));
  mrb_value d = mrb_symbol_value(mrb_intern_cstr(mrb, "d"));
  mrb_value e = mrb_symbol_value(mrb_intern_cstr(mrb, "e"));

  mrb_value i10 = mrb_int_value(mrb, 10);
  mrb_value i20 = mrb_int_value(mrb, 20);
  mrb_value i40 = mrb_int_value(mrb, 40);
  mrb_value i50 = mrb_int_value(mrb, 50);

  mrb_hash_iterator iterator = mrb_hash_iterator_new(mrb_hash_ptr(hash));

  CHECK(mrb_hash_iterator_remaining(&iterator) == 4);
  CHECK_EQ(a, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i10, mrb_hash_iterator_value(&iterator));

  CHECK(mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 3);
  CHECK_EQ(b, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i20, mrb_hash_iterator_value(&iterator));

  CHECK(mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 2);
  CHECK_EQ(d, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i40, mrb_hash_iterator_value(&iterator));

  CHECK(mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 1);
  CHECK_EQ(e, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i50, mrb_hash_iterator_value(&iterator));

  CHECK(!mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 0);

  return mrb_nil_value();
}

static mrb_value
hash_iterator_test3(mrb_state *mrb, mrb_value self) {
  int checkpoint = mrb_gc_arena_save(mrb);
  mrb_value hash = mrb_load_string(mrb,
    "x = {a: 10, b: 20, c: 30, d: 40, e: 50}\n"
    "x.delete(:c)\n"
    "x.delete(:e)\n"
    "x.delete(:b)\n"
    "x.delete(:a)\n"
    "x"
  );
  mrb_gc_arena_restore(mrb, checkpoint);

  mrb_value d = mrb_symbol_value(mrb_intern_cstr(mrb, "d"));

  mrb_value i40 = mrb_int_value(mrb, 40);

  mrb_hash_iterator iterator = mrb_hash_iterator_new(mrb_hash_ptr(hash));

  CHECK(mrb_hash_iterator_remaining(&iterator) == 1);
  CHECK_EQ(d, mrb_hash_iterator_key(&iterator));
  CHECK_EQ(i40, mrb_hash_iterator_value(&iterator));

  CHECK(!mrb_hash_iterator_move_next(&iterator));
  CHECK(mrb_hash_iterator_remaining(&iterator) == 0);

  return mrb_nil_value();
}

void mrb_mruby_test_hash_iterator_gem_test(mrb_state *mrb)
{
  struct RClass *cls;

  cls = mrb_define_class(mrb, "HashIterator", mrb->object_class);
  MRB_SET_INSTANCE_TT(cls, MRB_TT_CDATA);
  mrb_define_method(mrb, cls, "initialize", hash_iterator_initialize, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, cls, "move_next", hash_iterator_move_next, MRB_ARGS_NONE());
  mrb_define_method(mrb, cls, "remaining", hash_iterator_remaining, MRB_ARGS_NONE());
  mrb_define_method(mrb, cls, "key", hash_iterator_key, MRB_ARGS_NONE());
  mrb_define_method(mrb, cls, "value", hash_iterator_value, MRB_ARGS_NONE());

  mrb_define_class_method(mrb, cls, "test1", hash_iterator_test1, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, cls, "test2", hash_iterator_test2, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, cls, "test3", hash_iterator_test3, MRB_ARGS_NONE());
}
