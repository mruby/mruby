#include <mruby.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/proc.h>
#include <mruby/variable.h>
#include <mruby/internal.h>

/*
 * The name Module#set_temporary_name gave a module, or nil. It holds only
 * until a constant gives the module a name of its own; call it after
 * mrb_class_path(), which settles that name.
 */
static mrb_value
mod_tmpname(mrb_state *mrb, mrb_value self)
{
  struct RObject *o = mrb_obj_ptr(self);
  if (mrb_obj_iv_defined(mrb, o, MRB_SYM(__classname__))) return mrb_nil_value();
  return mrb_obj_iv_get(mrb, o, MRB_SYM(__tmpname__));
}

/*
 * Get the name of a module/class.
 *
 * Returns the fully qualified name of the module/class as a frozen string.
 * If the module/class is anonymous, returns nil.
 *
 * Args:
 *   mrb:  The mruby state
 *   self: The module/class object
 *
 * Returns:
 *   String: The name of the module/class (frozen)
 *   nil:    If the module/class is anonymous
 */
static mrb_value
mod_name(mrb_state *mrb, mrb_value self)
{
  mrb_value name =  mrb_class_path(mrb, mrb_class_ptr(self));
  mrb_value tmp = mod_tmpname(mrb, self);
  if (mrb_string_p(tmp)) return tmp;
  if (mrb_string_p(name)) {
    mrb_basic_ptr(name)->frozen = 1;
  }
  return name;
}

/*
 * Module#to_s and Module#inspect: the temporary name, if any, else the
 * name the core gives.
 */
static mrb_value
mod_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_class_path(mrb, mrb_class_ptr(self));
  mrb_value tmp = mod_tmpname(mrb, self);
  if (mrb_string_p(tmp)) return mrb_str_dup(mrb, tmp);
  return mrb_mod_to_s(mrb, self);
}

/* whether `p` reads as a constant path such as "Foo" or "Foo::Bar" */
static mrb_bool
const_path_p(const char *p, mrb_int len)
{
  const char *e = p + len;

  for (;;) {
    if (p == e || !ISUPPER(*p)) return FALSE;
    p++;
    while (p < e && (ISALNUM(*p) || *p == '_' || (unsigned char)*p >= 0x80)) p++;
    if (p == e) return TRUE;
    if (e - p < 2 || p[0] != ':' || p[1] != ':') return FALSE;
    p += 2;
  }
}

/*
 * call-seq:
 *   mod.set_temporary_name(string) -> self
 *   mod.set_temporary_name(nil)    -> self
 *
 * Gives an anonymous module or class a name to show (Module#name,
 * Module#to_s, Module#inspect) until a constant gives it one of its own;
 * nil takes the temporary name away again. The name may not
 * read as a constant path, and a module with a permanent name keeps it.
 *
 *   m = Module.new
 *   m.set_temporary_name("fake_name")
 *   m.name          #=> "fake_name"
 *   M = m
 *   m.name          #=> "M"
 */
static mrb_value
mod_set_temporary_name(mrb_state *mrb, mrb_value self)
{
  struct RClass *c = mrb_class_ptr(self);
  mrb_value name;

  mrb_get_args(mrb, "S!", &name);
  /* settle a permanent name a constant already gives it */
  mrb_class_path(mrb, c);
  if (mrb_obj_iv_defined(mrb, (struct RObject*)c, MRB_SYM(__classname__))) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "can't change permanent name");
  }
  if (!mrb_nil_p(name)) {
    if (RSTRING_LEN(name) == 0) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "empty class/module name");
    }
    if (const_path_p(RSTRING_PTR(name), RSTRING_LEN(name))) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "the temporary name must not be a constant path to avoid confusion");
    }
    name = mrb_str_dup(mrb, name);
    mrb_basic_ptr(name)->frozen = 1;
  }
  mrb_obj_iv_set_force(mrb, (struct RObject*)c, MRB_SYM(__tmpname__), name);
  return self;
}

/*
 * Check if a module/class is a singleton class.
 *
 * Args:
 *   mrb:  The mruby state
 *   self: The module/class object to check
 *
 * Returns:
 *   true:  if the object is a singleton class
 *   false: if the object is not a singleton class
 */
static mrb_value
mod_singleton_class_p(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(mrb_sclass_p(self));
}

/*
 *  call-seq:
 *     module_exec(arg...) {|var...| block } -> obj
 *     class_exec(arg...) {|var...| block } -> obj
 *
 * Evaluates the given block in the context of the
 * class/module. The method defined in the block will belong
 * to the receiver. Any arguments passed to the method will be
 * passed to the block. This can be used if the block needs to
 * access instance variables.
 *
 *     class Thing
 *     end
 *     Thing.class_exec{
 *       def hello() "Hello there!" end
 *     }
 *     puts Thing.new.hello()
 */

static mrb_value
mod_module_exec(mrb_state *mrb, mrb_value self)
{
  return mrb_object_exec(mrb, self, mrb_class_ptr(self));
}

/* Helper structure for subclass enumeration */
struct subclass_args {
  struct RClass *c;   /* The parent class to find subclasses of */
  mrb_value ary;      /* Array to collect subclasses into */
};

/*
 * Callback function for mrb_objspace_each_objects to find direct subclasses.
 *
 * This function is called for each object in the object space. It checks if
 * the object is a class whose direct superclass matches the target class.
 *
 * Args:
 *   mrb:  The mruby state
 *   obj:  The current object being examined
 *   data: Pointer to subclass_args structure
 *
 * Returns:
 *   MRB_EACH_OBJ_OK: Continue iteration
 */
static int
add_subclasses(mrb_state *mrb, struct RBasic *obj, void *data)
{
  struct subclass_args *args = (struct subclass_args*)data;
  if (obj->tt == MRB_TT_CLASS) {
    struct RClass *c = (struct RClass*)obj;
    if (mrb_class_real(c->super) == args->c) {
      mrb_ary_push(mrb, args->ary, mrb_obj_value(obj));
    }
  }
  return MRB_EACH_OBJ_OK;
}

/*
 *  call-seq:
 *     subclasses -> array
 *
 *  Returns an array of classes where the receiver is the
 *  direct superclass of the class, excluding singleton classes.
 *  The order of the returned array is not defined.
 *
 *     class A; end
 *     class B < A; end
 *     class C < B; end
 *     class D < A; end
 *
 *     A.subclasses        #=> [D, B]
 *     B.subclasses        #=> [C]
 *     C.subclasses        #=> []
 */
static mrb_value
class_subclasses(mrb_state *mrb, mrb_value self)
{
  struct RClass *c = mrb_class_ptr(self);
  mrb_value ary = mrb_ary_new(mrb);

  if (c->flags & MRB_FL_CLASS_IS_INHERITED) {
    struct subclass_args arg = {c, ary};
    mrb_objspace_each_objects(mrb, add_subclasses, &arg);
  }
  return ary;
}

/*
 *  call-seq:
 *     attached_object -> object
 *
 *  Returns the object for which the receiver is the singleton class.
 *  Raises an TypeError if the class is not a singleton class.
 *
 *     class Foo; end
 *
 *     Foo.singleton_class.attached_object        #=> Foo
 *     Foo.attached_object                        #=> TypeError: not a singleton class
 *     Foo.new.singleton_class.attached_object    #=> #<Foo:0x000000010491a370>
 *     TrueClass.attached_object                  #=> TypeError: not a singleton class
 *     NilClass.attached_object                   #=> TypeError: not a singleton class
 */
static mrb_value
class_attached_object(mrb_state *mrb, mrb_value self)
{
  struct RClass *c = mrb_class_ptr(self);
  if (c->tt != MRB_TT_SCLASS) {
    mrb_raise(mrb, E_TYPE_ERROR, "not a singleton class");
  }
  return mrb_obj_iv_get(mrb, (struct RObject*)c, MRB_SYM(__attached__));
}

/*
 * Check if a class/module is an ancestor of another.
 *
 * This function traverses the inheritance chain of `klass` upwards to determine
 * if `super` appears anywhere in the hierarchy. It handles both regular classes/modules
 * and included classes (ICLASS) which represent modules included in the inheritance chain.
 *
 * Args:
 *   klass: The class/module to check (potential descendant)
 *   super: The class/module to search for (potential ancestor)
 *
 * Returns:
 *   true:  if `super` is found in `klass`'s inheritance chain
 *   false: if `super` is not an ancestor of `klass`
 */
static mrb_bool
is_ancestor(struct RClass *klass, struct RClass *super)
{
  struct RClass *c = klass;
  while (c) {
    if (c->tt == MRB_TT_ICLASS) {
      if (c->c == super) return TRUE;
    }
    else {
      if (c == super) return TRUE;
    }
    c = c->super;
  }
  return FALSE;
}

/* a class, module, included-module proxy or singleton class: everything
   mrb_class_ptr can walk the ancestry of */
static mrb_bool
mod_p(mrb_value v)
{
  return mrb_class_p(v) || mrb_module_p(v) || mrb_iclass_p(v) || mrb_sclass_p(v);
}

/*
 * Compare hierarchy relationship between two modules/classes.
 *
 * This function determines the ancestor relationship between `self` and `other`.
 * It checks if one is an ancestor of the other by traversing the inheritance chain.
 *
 * Args:
 *   mrb:   The mruby state
 *   self:  The first module/class to compare
 *   other: The second module/class to compare
 *
 * Returns:
 *   true:  if `self` is an ancestor of `other` (self > other)
 *   false: if `other` is an ancestor of `self` (self < other)
 *   nil:   if there's no inheritance relationship between them
 *
 * Raises:
 *   TypeError: if `other` is not a class, module, or included class
 */
static mrb_value
mod_compare_hierarchy(mrb_state *mrb, mrb_value self, mrb_value other)
{
  if (!mod_p(self) || !mod_p(other)) {
    mrb_raise(mrb, E_TYPE_ERROR, "compared with non class/module");
  }

  struct RClass *self_c = mrb_class_ptr(self);
  struct RClass *other_c = mrb_class_ptr(other);

  if (is_ancestor(self_c, other_c)) {
    return mrb_true_value();
  }
  if (is_ancestor(other_c, self_c)) {
    return mrb_false_value();
  }
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     mod <= other   -> true, false, or nil
 *
 *  Returns true if mod is a subclass of other or is the same as other.
 *  Returns nil if there's no relationship between the two.
 */
static mrb_value
mrb_mod_le(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  return mod_compare_hierarchy(mrb, self, other);
}

/*
 *  call-seq:
 *     mod < other   -> true, false, or nil
 *
 *  Returns true if mod is a subclass of other. Returns false if mod
 *  is the same as other. Returns nil if there's no relationship between the two.
 */
static mrb_value
mrb_mod_lt(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  if (mrb_obj_equal(mrb, self, other)) {
    return mrb_false_value();
  }
  return mod_compare_hierarchy(mrb, self, other);
}

/*
 *  call-seq:
 *     mod >= other   -> true, false, or nil
 *
 *  Returns true if mod is an ancestor of other, or the two modules are the same.
 *  Returns nil if there's no relationship between the two.
 */
static mrb_value
mrb_mod_ge(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  return mod_compare_hierarchy(mrb, other, self);
}

/*
 *  call-seq:
 *     mod > other   -> true, false, or nil
 *
 *  Returns true if mod is an ancestor of other. Returns false if mod
 *  is the same as other. Returns nil if there's no relationship between the two.
 */
static mrb_value
mrb_mod_gt(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  if (mrb_obj_equal(mrb, self, other)) {
    return mrb_false_value();
  }
  return mod_compare_hierarchy(mrb, other, self);
}

/*
 *  call-seq:
 *     module <=> other_module   -> -1, 0, +1, or nil
 *
 *  Comparison - Returns -1, 0, +1 or nil depending on whether module
 *  includes other_module, they are the same, or if module is included by
 *  other_module.
 *
 *  Returns nil if module has no relationship with other_module, if
 *  other_module is not a module, or if the two values are incomparable.
 */
static mrb_value
mrb_mod_cmp(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);

  if (mrb_obj_equal(mrb, self, other)) {
    return mrb_fixnum_value(0);
  }
  if (!mod_p(other)) {
    return mrb_nil_value();
  }

  mrb_value cmp = mod_compare_hierarchy(mrb, self, other);

  if (mrb_true_p(cmp)) {
    return mrb_fixnum_value(-1);
  }
  else if (mrb_false_p(cmp)) {
    return mrb_fixnum_value(1);
  }
  else {
    return mrb_nil_value();
  }
}

/* ---------------------------*/
static const mrb_mt_entry mod_ext_rom_entries[] = {
  MRB_MT_ENTRY(mrb_mod_lt,            MRB_OPSYM(lt), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(mrb_mod_le,            MRB_OPSYM(le), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(mrb_mod_cmp,           MRB_OPSYM(cmp), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(mrb_mod_gt,            MRB_OPSYM(gt), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(mrb_mod_ge,            MRB_OPSYM(ge), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(mod_module_exec,       MRB_SYM(class_exec), MRB_ARGS_ANY()|MRB_ARGS_BLOCK()),
  MRB_MT_ENTRY(mod_module_exec,       MRB_SYM(module_exec), MRB_ARGS_ANY()|MRB_ARGS_BLOCK()),
  MRB_MT_ENTRY(mod_name,              MRB_SYM(name),           MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mod_to_s,              MRB_SYM(to_s),           MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mod_to_s,              MRB_SYM(inspect),        MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mod_set_temporary_name, MRB_SYM(set_temporary_name), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(mod_singleton_class_p, MRB_SYM_Q(singleton_class), MRB_ARGS_NONE()),
};

static const mrb_mt_entry cls_ext_rom_entries[] = {
  MRB_MT_ENTRY(class_attached_object, MRB_SYM(attached_object), MRB_ARGS_NONE()),
  MRB_MT_ENTRY(class_subclasses,      MRB_SYM(subclasses),   MRB_ARGS_NONE()),
};

/*
 * Initialize the mruby-class-ext gem.
 *
 * This function registers all the extension methods to the Module and Class classes.
 * It's called automatically when the gem is loaded.
 *
 * Args:
 *   mrb: The mruby state
 */
void
mrb_mruby_class_ext_gem_init(mrb_state *mrb)
{
  struct RClass *mod = mrb->module_class;
  struct RClass *cls = mrb->class_class;

  MRB_MT_INIT_ROM(mrb, mod, mod_ext_rom_entries);
  MRB_MT_INIT_ROM(mrb, cls, cls_ext_rom_entries);
}

void
mrb_mruby_class_ext_gem_final(mrb_state *mrb)
{
}
