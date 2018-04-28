/*
** enum.c - Enumerable module
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/proc.h>

typedef struct enumerable_hash_context {
  mrb_int hash;
  mrb_int index;
} enumerable_hash_context;

static mrb_value
enumerable_hash_each(mrb_state *mrb, mrb_value self)
{
  mrb_value item;
  mrb_value closure;

  mrb_get_args(mrb, "o", &item);
  closure = mrb_proc_cfunc_env_get(mrb, 0);

  if (mrb_cptr_p(closure)) {
    mrb_value item_hash;
    enumerable_hash_context *context = (enumerable_hash_context *)mrb_cptr(closure);

    item_hash = mrb_funcall(mrb, item, "hash", 0);
    if (mrb_fixnum_p(item_hash)) {
      context->hash ^= (mrb_fixnum(item_hash) << (context->index % 16));
      ++context->index;
      return mrb_nil_value();
    }
  }

  mrb_raise(mrb, E_TYPE_ERROR, "can't calculate hash");
}

static mrb_value
enumerable_hash(mrb_state *mrb, mrb_value self)
{
  /* redefine #hash 15.3.1.3.15 */
  struct RProc *proc;
  enumerable_hash_context context;
  mrb_value closure;
  mrb_value blk;
  int ai = mrb_gc_arena_save(mrb);

  context.hash = 12347;
  context.index = 0;
  closure = mrb_cptr_value(mrb, &context);

  proc = mrb_proc_new_cfunc_with_env(mrb, enumerable_hash_each, 1, &closure);
  blk = mrb_obj_value(proc);
  mrb_funcall_with_block(mrb, self, mrb_intern_cstr(mrb, "each"), 0, NULL, blk);

  mrb_gc_arena_restore(mrb, ai);
  return mrb_fixnum_value(context.hash);
}

void
mrb_init_enumerable(mrb_state *mrb)
{
  struct RClass *enumerable;
  enumerable = mrb_define_module(mrb, "Enumerable");  /* 15.3.2 */
  mrb_define_module_function(mrb, enumerable, "hash", enumerable_hash, MRB_ARGS_NONE());
}

