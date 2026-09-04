/*
** ary_shared.c - the shared-array shape a Ruby program cannot ask for
**
** ary_make_shared() shrinks a heap array's buffer to the length it is
** actually carrying, and that length can be zero: an array emptied by `pop`
** keeps the capacity it grew to, where `clear` gives it back.  Nothing in
** Ruby takes such an array through, since the one caller that would,
** mrb_ary_make_shared_copy(), is reached from C.
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>

/* AryShared.copy_ptr_null?(ary) -> true or false
 *
 * Share `ary`'s buffer the way a C caller does, and answer whether the
 * pointer both arrays are left reading is NULL.  A zero-length shrink that
 * asked mrb_realloc() for no bytes at all would free the buffer and leave
 * one there, which is what this is for.
 */
static mrb_value
ary_shared_copy_ptr_null_p(mrb_state *mrb, mrb_value self)
{
  mrb_value ary;
  struct RArray *a;

  mrb_get_args(mrb, "A", &ary);
  a = mrb_ary_ptr(ary);
  mrb_ary_make_shared_copy(mrb, ary);
  if (!ARY_SHARED_P(a)) return mrb_false_value(); /* embedded: nothing shared */
  return mrb_bool_value(a->as.heap.aux.shared->ptr == NULL);
}

/* AryShared.heap_empty?(ary) -> true or false
 *
 * Whether `ary` is the shape the answer above is about: on the heap, not
 * shared yet, carrying nothing, and still holding room for something.  A
 * build whose arrays embed keeps an emptied one embedded unless it grew
 * past what embedding holds, so a test says what it is asking about rather
 * than assuming it.
 */
static mrb_value
ary_shared_heap_empty_p(mrb_state *mrb, mrb_value self)
{
  mrb_value ary;
  struct RArray *a;

  mrb_get_args(mrb, "A", &ary);
  a = mrb_ary_ptr(ary);
  if (ARY_EMBED_P(a) || ARY_SHARED_P(a)) return mrb_false_value();
  return mrb_bool_value(ARY_LEN(a) == 0 && a->as.heap.aux.capa > 0);
}

void
mrb_init_test_ary_shared(mrb_state *mrb)
{
  struct RClass *c = mrb_define_module(mrb, "AryShared");

  mrb_define_module_function(mrb, c, "copy_ptr_null?", ary_shared_copy_ptr_null_p, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, c, "heap_empty?", ary_shared_heap_empty_p, MRB_ARGS_REQ(1));
}
