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
 *  Returns <i>obj</i>.
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
 *  (_obj_). In order to set the context, the variable +self+ is set
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

void
mrb_mruby_object_ext_gem_init(mrb_state* mrb)
{
  struct RClass * n = mrb->nil_class;

  mrb_define_method_id(mrb, n, MRB_SYM(to_a), nil_to_a,       MRB_ARGS_NONE());
#ifndef MRB_NO_FLOAT
  mrb_define_method_id(mrb, n, MRB_SYM(to_f), nil_to_f,       MRB_ARGS_NONE());
#endif
  mrb_define_method_id(mrb, n, MRB_SYM(to_h), nil_to_h,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, n, MRB_SYM(to_i), nil_to_i,       MRB_ARGS_NONE());

  mrb_define_method_id(mrb, mrb->kernel_module, MRB_SYM(itself), mrb_obj_itself, MRB_ARGS_NONE());

  mrb_define_method_id(mrb, mrb_class_get_id(mrb, MRB_SYM(BasicObject)), MRB_SYM(instance_exec), obj_instance_exec, MRB_ARGS_ANY() | MRB_ARGS_BLOCK());
}

void
mrb_mruby_object_ext_gem_final(mrb_state* mrb)
{
}
