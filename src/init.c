/*
** init.c - initialize mruby core
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>

#define INIT_FUNC_FOREACH(def) \
  def(mrb_init_symtbl) \
  def(mrb_init_class) \
  def(mrb_init_object) \
  def(mrb_init_kernel) \
  def(mrb_init_enumerable) \
  def(mrb_init_symbol) \
  def(mrb_init_string) \
  def(mrb_init_exception) \
  def(mrb_init_proc) \
  def(mrb_init_array) \
  def(mrb_init_hash) \
  def(mrb_init_numeric) \
  def(mrb_init_range) \
  def(mrb_init_gc) \
  def(mrb_init_version) \
  def(mrb_init_mrblib)

#define INIT_FUNC_DECLS(func) void func(mrb_state*);
INIT_FUNC_FOREACH(INIT_FUNC_DECLS)

#define DONE mrb_gc_arena_restore(mrb, 0);
void
mrb_init_core(mrb_state *mrb)
{
#define INIT_FUNC_CALL(func) func(mrb); DONE;
  INIT_FUNC_FOREACH(INIT_FUNC_CALL)
}
#undef DONE
