#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/hash.h>
#include <mruby/proc.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

/*
 *  call-seq:
 *     nil.to_a    -> []
 *
 *  Always returns an empty array.
 */

static mrb_value
nil_to_a(mrb_state *mrb, mrb_value obj)
{
  return mrb_ary_new(mrb);
}

#ifndef MRB_NO_FLOAT
/*
 *  call-seq:
 *     nil.to_f    -> 0.0
 *
 *  Always returns zero.
 */

static mrb_value
nil_to_f(mrb_state *mrb, mrb_value obj)
{
  return mrb_float_value(mrb, 0.0);
}
#endif

/*
 *  call-seq:
 *     nil.to_h    -> {}
 *
 *  Always returns an empty hash.
 */

static mrb_value
nil_to_h(mrb_state *mrb, mrb_value obj)
{
  return mrb_hash_new(mrb);
}

/*
 *  call-seq:
 *     nil.to_i    -> 0
 *
 *  Always returns zero.
 */

static mrb_value
nil_to_i(mrb_state *mrb, mrb_value obj)
{
  return mrb_fixnum_value(0);
}

/*
 *  Document-method: Kernel#itself
 *
 *  call-seq:
 *     obj.itself -> an_object
 *
 *  Returns *obj*.
 *
 *      string = 'my string' #=> "my string"
 *      string.itself.object_id == string.object_id #=> true
 *
 */

/*
 *  call-seq:
 *     obj.instance_exec(arg...) {|var...| block }                       -> obj
 *
 *  Executes the given block within the context of the receiver
 *  (_obj_). In order to set the context, the variable `self` is set
 *  to _obj_ while the code is executing, giving the code access to
 *  _obj_'s instance variables.  Arguments are passed as block parameters.
 *
 *     class KlassWithSecret
 *       def initialize
 *         @secret = 99
 *       end
 *     end
 *     k = KlassWithSecret.new
 *     k.instance_exec(5) {|x| @secret+x }   #=> 104
 */

static mrb_value
obj_instance_exec(mrb_state *mrb, mrb_value self)
{
  return mrb_object_exec(mrb, self, mrb_singleton_class_ptr(mrb, self));
}

static const mrb_mt_entry nil_ext_rom_entries[] = {
  MRB_MT_ENTRY(nil_to_a, MRB_SYM(to_a), MRB_MT_FUNC|MRB_MT_NOARG),
  MRB_MT_ENTRY(nil_to_h, MRB_SYM(to_h), MRB_MT_FUNC|MRB_MT_NOARG),
  MRB_MT_ENTRY(nil_to_i, MRB_SYM(to_i), MRB_MT_FUNC|MRB_MT_NOARG),
};
static mrb_mt_tbl nil_ext_rom_mt = MRB_MT_ROM_TAB(nil_ext_rom_entries);

static const mrb_mt_entry kernel_ext_rom_entries[] = {
  MRB_MT_ENTRY(mrb_obj_itself, MRB_SYM(itself), MRB_MT_FUNC|MRB_MT_NOARG),
};
static mrb_mt_tbl kernel_ext_rom_mt = MRB_MT_ROM_TAB(kernel_ext_rom_entries);

static const mrb_mt_entry bob_ext_rom_entries[] = {
  MRB_MT_ENTRY(obj_instance_exec, MRB_SYM(instance_exec), MRB_MT_FUNC),
};
static mrb_mt_tbl bob_ext_rom_mt = MRB_MT_ROM_TAB(bob_ext_rom_entries);

void
mrb_mruby_object_ext_gem_init(mrb_state* mrb)
{
  struct RClass * n = mrb->nil_class;

  mrb_mt_init_rom(n, &nil_ext_rom_mt);
#ifndef MRB_NO_FLOAT
  mrb_define_method_id(mrb, n, MRB_SYM(to_f), nil_to_f,       MRB_ARGS_NONE());
#endif

  mrb_mt_init_rom(mrb->kernel_module, &kernel_ext_rom_mt);

  mrb_mt_init_rom(mrb_class_get_id(mrb, MRB_SYM(BasicObject)), &bob_ext_rom_mt);
}

void
mrb_mruby_object_ext_gem_final(mrb_state* mrb)
{
}
