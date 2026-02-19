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

#define NIL_EXT_ROM_MT_SIZE 3
static struct {
  union mt_ptr vals[NIL_EXT_ROM_MT_SIZE];
  mrb_sym keys[NIL_EXT_ROM_MT_SIZE];
} nil_ext_rom_data = {
  .vals = {
    { .func = nil_to_a },
    { .func = nil_to_h },
    { .func = nil_to_i },
  },
  .keys = {
    MT_KEY(MRB_SYM(to_a), MT_FUNC|MT_NOARG|MT_PUBLIC),
    MT_KEY(MRB_SYM(to_h), MT_FUNC|MT_NOARG|MT_PUBLIC),
    MT_KEY(MRB_SYM(to_i), MT_FUNC|MT_NOARG|MT_PUBLIC),
  }
};
static mt_tbl nil_ext_rom_mt = {
  NIL_EXT_ROM_MT_SIZE, NIL_EXT_ROM_MT_SIZE,
  (union mt_ptr*)&nil_ext_rom_data, NULL
};

#define KERNEL_EXT_ROM_MT_SIZE 1
static struct {
  union mt_ptr vals[KERNEL_EXT_ROM_MT_SIZE];
  mrb_sym keys[KERNEL_EXT_ROM_MT_SIZE];
} kernel_ext_rom_data = {
  .vals = {
    { .func = mrb_obj_itself },
  },
  .keys = {
    MT_KEY(MRB_SYM(itself), MT_FUNC|MT_NOARG|MT_PUBLIC),
  }
};
static mt_tbl kernel_ext_rom_mt = {
  KERNEL_EXT_ROM_MT_SIZE, KERNEL_EXT_ROM_MT_SIZE,
  (union mt_ptr*)&kernel_ext_rom_data, NULL
};

#define BOB_EXT_ROM_MT_SIZE 1
static struct {
  union mt_ptr vals[BOB_EXT_ROM_MT_SIZE];
  mrb_sym keys[BOB_EXT_ROM_MT_SIZE];
} bob_ext_rom_data = {
  .vals = {
    { .func = obj_instance_exec },
  },
  .keys = {
    MT_KEY(MRB_SYM(instance_exec), MT_FUNC|MT_PUBLIC),
  }
};
static mt_tbl bob_ext_rom_mt = {
  BOB_EXT_ROM_MT_SIZE, BOB_EXT_ROM_MT_SIZE,
  (union mt_ptr*)&bob_ext_rom_data, NULL
};

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
