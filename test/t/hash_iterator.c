#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/hash.h>
#include <mruby/value.h>

#include <assert.h>
#include <stdlib.h>

#define CHECK(...) do { \
    mrb_bool result = (__VA_ARGS__); \
    assert(result && (#__VA_ARGS__)); \
    if (!result) { return EXIT_FAILURE; } \
  } while (0)

#define CHECK_EQ(Expected, ...) \
  CHECK(mrb_eql(mrb, (Expected), (__VA_ARGS__)))

int test1() {
  mrb_state *mrb = mrb_open();
  CHECK(mrb);

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

  return EXIT_SUCCESS;
}

int test2() {
  mrb_state *mrb = mrb_open();
  CHECK(mrb);

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

  return EXIT_SUCCESS;
}

int test3() {
  mrb_state *mrb = mrb_open();
  CHECK(mrb);

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

  return EXIT_SUCCESS;
}

int main() {
  return test1() | test2() | test3();
}
