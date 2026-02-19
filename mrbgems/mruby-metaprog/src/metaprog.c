#include <mruby.h>
#include <mruby/array.h>
#include <mruby/hash.h>
#include <mruby/variable.h>
#include <mruby/proc.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/internal.h>
#include <mruby/khash.h>
#include <mruby/presym.h>

#undef MT_PUBLIC
#undef MT_PRIVATE
#define MT_PUBLIC MRB_METHOD_PUBLIC_FL
#define MT_PRIVATE MRB_METHOD_PRIVATE_FL
#define MT_PROTECTED MRB_METHOD_PROTECTED_FL
#define MT_NOPRIV (MT_PRIVATE|MT_PROTECTED)

static mrb_value
mrb_f_nil(mrb_state *mrb, mrb_value cv)
{
  return mrb_nil_value();
}

/* 15.3.1.3.20 */
/*
 *  call-seq:
 *     obj.instance_variable_defined?(symbol)    -> true or false
 *
 *  Returns `true` if the given instance variable is
 *  defined in *obj*.
 *
 *     class Fred
 *       def initialize(p1, p2)
 *         @a, @b = p1, p2
 *       end
 *     end
 *     fred = Fred.new('cat', 99)
 *     fred.instance_variable_defined?(:@a)    #=> true
 *     fred.instance_variable_defined?("@b")   #=> true
 *     fred.instance_variable_defined?("@c")   #=> false
 */
static mrb_value
mrb_obj_ivar_defined(mrb_state *mrb, mrb_value self)
{
  mrb_sym sym;

  mrb_get_args(mrb, "n", &sym);
  mrb_iv_name_sym_check(mrb, sym);
  return mrb_bool_value(mrb_iv_defined(mrb, self, sym));
}

/* 15.3.1.3.21 */
/*
 *  call-seq:
 *     obj.instance_variable_get(symbol)    -> obj
 *
 *  Returns the value of the given instance variable, or nil if the
 *  instance variable is not set. The `@` part of the
 *  variable name should be included for regular instance
 *  variables. Throws a `NameError` exception if the
 *  supplied symbol is not valid as an instance variable name.
 *
 *     class Fred
 *       def initialize(p1, p2)
 *         @a, @b = p1, p2
 *       end
 *     end
 *     fred = Fred.new('cat', 99)
 *     fred.instance_variable_get(:@a)    #=> "cat"
 *     fred.instance_variable_get("@b")   #=> 99
 */
static mrb_value
mrb_obj_ivar_get(mrb_state *mrb, mrb_value self)
{
  mrb_sym iv_name;

  mrb_get_args(mrb, "n", &iv_name);
  mrb_iv_name_sym_check(mrb, iv_name);
  return mrb_iv_get(mrb, self, iv_name);
}

/* 15.3.1.3.22 */
/*
 *  call-seq:
 *     obj.instance_variable_set(symbol, obj)    -> obj
 *
 *  Sets the instance variable names by *symbol* to
 *  *object*, thereby frustrating the efforts of the class's
 *  author to attempt to provide proper encapsulation. The variable
 *  did not have to exist prior to this call.
 *
 *     class Fred
 *       def initialize(p1, p2)
 *         @a, @b = p1, p2
 *       end
 *     end
 *     fred = Fred.new('cat', 99)
 *     fred.instance_variable_set(:@a, 'dog')   #=> "dog"
 *     fred.instance_variable_set(:@c, 'cat')   #=> "cat"
 *     fred.inspect                             #=> "#<Fred:0x401b3da8 @a=\"dog\", @b=99, @c=\"cat\">"
 */
static mrb_value
mrb_obj_ivar_set(mrb_state *mrb, mrb_value self)
{
  mrb_sym iv_name;
  mrb_value val;

  mrb_get_args(mrb, "no", &iv_name, &val);
  mrb_iv_name_sym_check(mrb, iv_name);
  mrb_iv_set(mrb, self, iv_name, val);
  return val;
}

/* 15.3.1.2.7 */
/* 15.3.1.3.28 */
/*
 *  call-seq:
 *     local_variables   -> array
 *
 *  Returns the names of local variables in the current scope.
 *
 *  [mruby limitation]
 *  If variable symbol information was stripped out from
 *  compiled binary files using `mruby-strip -l`, this
 *  method always returns an empty array.
 */
static mrb_value
mrb_local_variables(mrb_state *mrb, mrb_value self)
{
  return mrb_proc_local_variables(mrb, mrb->c->ci[-1].proc);
}

KHASH_DECLARE(st, mrb_sym, char, TRUE)
KHASH_DEFINE(st, mrb_sym, char, TRUE, kh_int_hash_func, kh_int_hash_equal)

struct mt_set {
  unsigned int visibility;
  khash_t(st) *set;
};

#define vicheck(flags, visi) (((visi)==MT_NOPRIV) ? (((flags)&0x3)!=MT_PRIVATE) : (((flags)&0x3)==(visi)))

static int
method_entry_i(mrb_state *mrb, mrb_sym mid, mrb_method_t m, void *p)
{
  struct mt_set *s = (struct mt_set*)p;

  if (vicheck(m.flags, s->visibility) && kh_get(st, mrb, s->set, mid) == kh_end(s->set)) {
    khint_t k = kh_put(st, mrb, s->set, mid);
    kh_val(st, s->set, k) = !MRB_METHOD_UNDEF_P(m);
  }
  return 0;
}

static void
method_entry_loop(mrb_state *mrb, struct RClass *klass, khash_t(st) *set, unsigned int visibility)
{
  struct mt_set s = {visibility, set};

  mrb_mt_foreach(mrb, klass, method_entry_i, (void*)&s);
}

static mrb_value
mrb_class_instance_method_list(mrb_state *mrb, mrb_bool recur, struct RClass *klass, unsigned int flags)
{
  mrb_value ary;
  khash_t(st) *set = kh_init(st, mrb);

  if (!recur) {
    if (klass->flags & MRB_FL_CLASS_IS_PREPENDED) {
      MRB_CLASS_ORIGIN(klass);
    }
    method_entry_loop(mrb, klass, set, flags);
  }
  else {
    struct RClass *oldklass = NULL;

    while (klass && (klass != oldklass)) {
      method_entry_loop(mrb, klass, set, flags);
      oldklass = klass;
      klass = klass->super;
    }
  }

  ary = mrb_ary_new_capa(mrb, kh_size(set));
  KHASH_FOREACH(st, set, k) {
    if (kh_val(st, set, k)) {
      mrb_ary_push(mrb, ary, mrb_symbol_value(kh_key(st, set, k)));
    }
  }
  kh_destroy(st, mrb, set);

  return ary;
}

static mrb_value
mrb_obj_methods(mrb_state *mrb, mrb_value obj, unsigned int flags)
{
  mrb_bool regular = TRUE;
  mrb_get_args(mrb, "|b", &regular);

  return mrb_class_instance_method_list(mrb, regular, mrb_class(mrb, obj) , flags);
}

/* 15.3.1.3.31 */
/*
 *  call-seq:
 *     obj.methods(regular=true)    -> array
 *
 *  Returns a list of the names of public and protected methods of
 *  `obj`. This will include all the methods accessible in
 *  `obj`'s ancestors.
 *  If the optional parameter is `false`, it
 *  returns an array of `obj`'s public and protected singleton methods,
 *  the array will not include methods in modules included in `obj`.
 *
 *     class Klass
 *       def kMethod()
 *       end
 *     end
 *     k = Klass.new
 *     k.methods[0..9]    #=> [:kMethod, :respond_to?, :nil?, :is_a?,
 *                        #    :class, :instance_variable_set,
 *                        #    :methods, :extend, :__send__, :instance_eval]
 *     k.methods.length   #=> 42
 */
static mrb_value
mrb_obj_methods_m(mrb_state *mrb, mrb_value self)
{
  return mrb_obj_methods(mrb, self, MT_NOPRIV);
}

/* 15.3.1.3.36 */
/*
 *  call-seq:
 *     obj.private_methods(all=true)   -> array
 *
 *  Returns the list of private methods accessible to *obj*. If
 *  the *all* parameter is set to `false`, only those methods
 *  in the receiver will be listed.
 */
static mrb_value
mrb_obj_private_methods(mrb_state *mrb, mrb_value self)
{
  return mrb_obj_methods(mrb, self, MT_PRIVATE);
}

/* 15.3.1.3.37 */
/*
 *  call-seq:
 *     obj.protected_methods(all=true)   -> array
 *
 *  Returns the list of protected methods accessible to *obj*. If
 *  the *all* parameter is set to `false`, only those methods
 *  in the receiver will be listed.
 */
static mrb_value
mrb_obj_protected_methods(mrb_state *mrb, mrb_value self)
{
  return mrb_obj_methods(mrb, self, MT_PROTECTED);
}

/* 15.3.1.3.38 */
/*
 *  call-seq:
 *     obj.public_methods(all=true)   -> array
 *
 *  Returns the list of public methods accessible to *obj*. If
 *  the *all* parameter is set to `false`, only those methods
 *  in the receiver will be listed.
 */
static mrb_value
mrb_obj_public_methods(mrb_state *mrb, mrb_value self)
{
  return mrb_obj_methods(mrb, self, MT_PUBLIC);
}

static mrb_value
mrb_obj_singleton_methods(mrb_state *mrb, mrb_bool recur, mrb_value obj)
{
  mrb_value ary;
  struct RClass *klass;
  khash_t(st) *set = kh_init(st, mrb);

  klass = mrb_class(mrb, obj);

  if (klass && (klass->tt == MRB_TT_SCLASS)) {
      method_entry_loop(mrb, klass, set, MT_PUBLIC);
      klass = klass->super;
  }
  if (recur) {
      while (klass && ((klass->tt == MRB_TT_SCLASS) || (klass->tt == MRB_TT_ICLASS))) {
        method_entry_loop(mrb, klass, set, MT_PUBLIC);
        klass = klass->super;
      }
  }

  ary = mrb_ary_new(mrb);
  KHASH_FOREACH(st, set, k) {
    mrb_ary_push(mrb, ary, mrb_symbol_value(kh_key(st, set, k)));
  }
  kh_destroy(st, mrb, set);

  return ary;
}

/* 15.3.1.3.45 */
/*
 *  call-seq:
 *     obj.singleton_methods(all=true)    -> array
 *
 *  Returns an array of the names of singleton methods for *obj*.
 *  If the optional *all* parameter is true, the list will include
 *  methods in modules included in *obj*.
 *  Only public and protected singleton methods are returned.
 *
 *     module Other
 *       def three() end
 *     end
 *
 *     class Single
 *       def Single.four() end
 *     end
 *
 *     a = Single.new
 *
 *     def a.one()
 *     end
 *
 *     class << a
 *       include Other
 *       def two()
 *       end
 *     end
 *
 *     Single.singleton_methods    #=> [:four]
 *     a.singleton_methods(false)  #=> [:two, :one]
 *     a.singleton_methods         #=> [:two, :one, :three]
 */
static mrb_value
mrb_obj_singleton_methods_m(mrb_state *mrb, mrb_value self)
{
  mrb_bool recur = TRUE;
  mrb_get_args(mrb, "|b", &recur);
  return mrb_obj_singleton_methods(mrb, recur, self);
}

mrb_value mrb_mod_define_method_m(mrb_state *mrb, struct RClass *c);

static mrb_value
mod_define_singleton_method(mrb_state *mrb, mrb_value self)
{
  return mrb_mod_define_method_m(mrb, mrb_class_ptr(mrb_singleton_class(mrb, self)));
}

static mrb_bool
cv_name_p(mrb_state *mrb, const char *name, mrb_int len)
{
  return len > 2 && name[0] == '@' && name[1] == '@' &&
         !ISDIGIT(name[2]) && mrb_ident_p(name+2, len-2);
}

static void
check_cv_name_sym(mrb_state *mrb, mrb_sym id)
{
  mrb_int len;
  const char *name = mrb_sym_name_len(mrb, id, &len);
  if (!cv_name_p(mrb, name, len)) {
    mrb_name_error(mrb, id, "'%n' is not allowed as a class variable name", id);
  }
}

/* 15.2.2.4.39 */
/*
 *  call-seq:
 *     remove_class_variable(sym)    -> obj
 *
 *  Removes the definition of the *sym*, returning that
 *  constant's value.
 *
 *     class Dummy
 *       @@var = 99
 *       puts @@var
 *       p class_variables
 *       remove_class_variable(:@@var)
 *       p class_variables
 *     end
 *
 *  <em>produces:</em>
 *
 *     99
 *     [:@@var]
 *     []
 */

static mrb_value
mrb_mod_remove_cvar(mrb_state *mrb, mrb_value mod)
{
  mrb_value val;
  mrb_sym id;

  mrb_get_args(mrb, "n", &id);
  check_cv_name_sym(mrb, id);

  val = mrb_iv_remove(mrb, mod, id);
  if (!mrb_undef_p(val)) return val;

  if (mrb_cv_defined(mrb, mod, id)) {
    mrb_name_error(mrb, id, "cannot remove %n for %v", id, mod);
  }

  mrb_name_error(mrb, id, "class variable %n not defined for %v", id, mod);

 /* not reached */
 return mrb_nil_value();
}

/* 15.2.2.4.16 */
/*
 *  call-seq:
 *     obj.class_variable_defined?(symbol)    -> true or false
 *
 *  Returns `true` if the given class variable is defined
 *  in *obj*.
 *
 *     class Fred
 *       @@foo = 99
 *     end
 *     Fred.class_variable_defined?(:@@foo)    #=> true
 *     Fred.class_variable_defined?(:@@bar)    #=> false
 */

static mrb_value
mrb_mod_cvar_defined(mrb_state *mrb, mrb_value mod)
{
  mrb_sym id;

  mrb_get_args(mrb, "n", &id);
  check_cv_name_sym(mrb, id);
  return mrb_bool_value(mrb_cv_defined(mrb, mod, id));
}

/* 15.2.2.4.17 */
/*
 *  call-seq:
 *     mod.class_variable_get(symbol)    -> obj
 *
 *  Returns the value of the given class variable (or throws a
 *  `NameError` exception). The `@@` part of the
 *  variable name should be included for regular class variables
 *
 *     class Fred
 *       @@foo = 99
 *     end
 *     Fred.class_variable_get(:@@foo)     #=> 99
 */

static mrb_value
mrb_mod_cvar_get(mrb_state *mrb, mrb_value mod)
{
  mrb_sym id;

  mrb_get_args(mrb, "n", &id);
  check_cv_name_sym(mrb, id);
  return mrb_cv_get(mrb, mod, id);
}

/* 15.2.2.4.18 */
/*
 *  call-seq:
 *     obj.class_variable_set(symbol, obj)    -> obj
 *
 *  Sets the class variable names by *symbol* to
 *  *object*.
 *
 *     class Fred
 *       @@foo = 99
 *       def foo
 *         @@foo
 *       end
 *     end
 *     Fred.class_variable_set(:@@foo, 101)     #=> 101
 *     Fred.new.foo                             #=> 101
 */

static mrb_value
mrb_mod_cvar_set(mrb_state *mrb, mrb_value mod)
{
  mrb_value value;
  mrb_sym id;

  mrb_get_args(mrb, "no", &id, &value);
  check_cv_name_sym(mrb, id);
  mrb_cv_set(mrb, mod, id, value);
  return value;
}

static mrb_value
mrb_mod_included_modules(mrb_state *mrb, mrb_value self)
{
  mrb_value result;
  struct RClass *c = mrb_class_ptr(self);
  struct RClass *origin = c;

  MRB_CLASS_ORIGIN(origin);
  result = mrb_ary_new(mrb);
  while (c) {
    if (c != origin && c->tt == MRB_TT_ICLASS) {
      if (c->c->tt == MRB_TT_MODULE) {
        mrb_ary_push(mrb, result, mrb_obj_value(c->c));
      }
    }
    c = c->super;
  }

  return result;
}

static mrb_value
mod_instance_methods(mrb_state *mrb, mrb_value mod, unsigned int visibility)
{
  struct RClass *c = mrb_class_ptr(mod);
  mrb_bool recur = TRUE;
  mrb_get_args(mrb, "|b", &recur);
  return mrb_class_instance_method_list(mrb, recur, c, visibility);
}

/* 15.2.2.4.33 */
/*
 *  call-seq:
 *     mod.instance_methods(include_super=true)   -> array
 *
 *  Returns an array containing the names of the public and protected instance
 *  methods in the receiver. For a module, these are the public and protected methods;
 *  for a class, they are the instance (not singleton) methods. With no
 *  argument, or with an argument that is `false`, the
 *  instance methods in *mod* are returned, otherwise the methods
 *  in *mod* and *mod*'s superclasses are returned.
 *
 *     module A
 *       def method1()  end
 *     end
 *     class B
 *       def method2()  end
 *     end
 *     class C < B
 *       def method3()  end
 *     end
 *
 *     A.instance_methods                #=> [:method1]
 *     B.instance_methods(false)         #=> [:method2]
 *     C.instance_methods(false)         #=> [:method3]
 *     C.instance_methods(true).length   #=> 43
 */

static mrb_value
mrb_mod_instance_methods(mrb_state *mrb, mrb_value mod)
{
  return mod_instance_methods(mrb, mod, MT_NOPRIV);
}

static mrb_value
mrb_mod_public_instance_methods(mrb_state *mrb, mrb_value mod)
{
  return mod_instance_methods(mrb, mod, MT_PUBLIC);
}

static mrb_value
mrb_mod_private_instance_methods(mrb_state *mrb, mrb_value mod)
{
  return mod_instance_methods(mrb, mod, MT_PRIVATE);
}

static mrb_value
mrb_mod_protected_instance_methods(mrb_state *mrb, mrb_value mod)
{
  return mod_instance_methods(mrb, mod, MT_PROTECTED);
}

static int
undefined_method_i(mrb_state *mrb, mrb_sym mid, mrb_method_t m, void *p)
{
  mrb_value ary = *(mrb_value*)p;

  if (MRB_METHOD_UNDEF_P(m)) {
    mrb_ary_push(mrb, ary, mrb_symbol_value(mid));
  }
  return 0;
}

/*
 *  call-seq:
 *     mod.undefined_methods()   -> array
 *
 *  Returns an array containing the names of the undefined methods of the module/class.
 */
static mrb_value
mrb_mod_undefined_methods(mrb_state *mrb, mrb_value mod)
{
  struct RClass *m = mrb_class_ptr(mod);
  mrb_get_args(mrb, "");        /* no argument */

  mrb_value ary = mrb_ary_new(mrb);

  if (m->flags & MRB_FL_CLASS_IS_PREPENDED) {
    MRB_CLASS_ORIGIN(m);
  }
  mrb_mt_foreach(mrb, m, undefined_method_i, (void*)&ary);

  return ary;
}

/* 15.2.2.4.41 */
/*
 *  call-seq:
 *     remove_method(symbol)   -> self
 *
 *  Removes the method identified by _symbol_ from the current
 *  class. For an example, see `Module.undef_method`.
 */

static mrb_value
mrb_mod_remove_method(mrb_state *mrb, mrb_value mod)
{
  mrb_int argc;
  const mrb_value *argv;
  struct RClass *c = mrb_class_ptr(mod);

  mrb_get_args(mrb, "*", &argv, &argc);
  mrb_check_frozen(mrb, c);
  int ai = mrb_gc_arena_save(mrb);
  while (argc--) {
    mrb_remove_method(mrb, c, mrb_obj_to_sym(mrb, *argv));
    mrb_gc_arena_restore(mrb, ai);
    argv++;
  }
  return mod;
}

static mrb_value
mrb_mod_s_constants(mrb_state *mrb, mrb_value mod)
{
  if (mrb_get_argc(mrb) > 0 || mrb_class_ptr(mod) != mrb->module_class) {
    return mrb_mod_constants(mrb, mod);
  }

  const struct RProc *proc = mrb->c->ci[-1].proc;
  struct RClass *c = MRB_PROC_TARGET_CLASS(proc);
  mrb_value ary = mrb_ary_new(mrb);

  if (!c) c = mrb->object_class;
  mrb_mod_const_at(mrb, c, ary);
  proc = proc->upper;
  while (proc) {
    struct RClass *c2 = MRB_PROC_TARGET_CLASS(proc);
    if (!c2) c2 = mrb->object_class;
    mrb_mod_const_at(mrb, c2, ary);
    proc = proc->upper;
  }
  while (c) {
    mrb_mod_const_at(mrb, c, ary);
    c = c->super;
    if (c == mrb->object_class) break;
  }
  return ary;
}

static mrb_value
mrb_mod_s_nesting(mrb_state *mrb, mrb_value mod)
{
  const struct RProc *proc;
  mrb_value ary;
  struct RClass *c = NULL;

  ary = mrb_ary_new(mrb);
  proc = mrb->c->ci[-1].proc;   /* callee proc */
  while (proc && !MRB_PROC_CFUNC_P(proc)) {
    if (MRB_PROC_SCOPE_P(proc)) {
      struct RClass *c2 = MRB_PROC_TARGET_CLASS(proc);

      if (c2 != c) {
        c = c2;
        mrb_ary_push(mrb, ary, mrb_obj_value(c));
      }
    }
    proc = proc->upper;
  }
  return ary;
}

/* ---------------------------*/
#define METAPROG_KRN_ROM_MT_SIZE 15
static struct {
  union mt_ptr vals[METAPROG_KRN_ROM_MT_SIZE];
  mrb_sym keys[METAPROG_KRN_ROM_MT_SIZE];
} metaprog_krn_rom_data = {
  .vals = {
    { .func = mrb_f_global_variables },
    { .func = mrb_local_variables },
    { .func = mrb_singleton_class },
    { .func = mrb_obj_ivar_defined },
    { .func = mrb_obj_ivar_get },
    { .func = mrb_obj_ivar_set },
    { .func = mrb_obj_instance_variables },
    { .func = mrb_obj_methods_m },
    { .func = mrb_obj_private_methods },
    { .func = mrb_obj_protected_methods },
    { .func = mrb_obj_public_methods },
    { .func = mrb_obj_singleton_methods_m },
    { .func = mod_define_singleton_method },
    { .func = mrb_f_send },
    { .func = mrb_f_public_send },
  },
  .keys = {
    MT_KEY(MRB_SYM(global_variables),            MT_FUNC|MT_NOARG|MT_PRIVATE),
    MT_KEY(MRB_SYM(local_variables),             MT_FUNC|MT_NOARG|MT_PRIVATE),
    MT_KEY(MRB_SYM(singleton_class),             MT_FUNC|MT_NOARG|MT_PUBLIC),
    MT_KEY(MRB_SYM_Q(instance_variable_defined), MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(instance_variable_get),       MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(instance_variable_set),       MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(instance_variables),          MT_FUNC|MT_NOARG|MT_PUBLIC),
    MT_KEY(MRB_SYM(methods),                     MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(private_methods),             MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(protected_methods),           MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(public_methods),              MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(singleton_methods),           MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(define_singleton_method),     MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(send),                        MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(public_send),                 MT_FUNC|MT_PUBLIC),
  }
};
static mt_tbl metaprog_krn_rom_mt = {
  METAPROG_KRN_ROM_MT_SIZE, METAPROG_KRN_ROM_MT_SIZE,
  (union mt_ptr*)&metaprog_krn_rom_data, NULL
};

#define METAPROG_MOD_ROM_MT_SIZE 14
static struct {
  union mt_ptr vals[METAPROG_MOD_ROM_MT_SIZE];
  mrb_sym keys[METAPROG_MOD_ROM_MT_SIZE];
} metaprog_mod_rom_data = {
  .vals = {
    { .func = mrb_mod_class_variables },
    { .func = mrb_mod_remove_cvar },
    { .func = mrb_mod_cvar_defined },
    { .func = mrb_mod_cvar_get },
    { .func = mrb_mod_cvar_set },
    { .func = mrb_mod_included_modules },
    { .func = mrb_mod_instance_methods },
    { .func = mrb_mod_public_instance_methods },
    { .func = mrb_mod_private_instance_methods },
    { .func = mrb_mod_protected_instance_methods },
    { .func = mrb_mod_undefined_methods },
    { .func = mrb_mod_remove_method },
    { .func = mrb_f_nil },
    { .func = mrb_mod_constants },
  },
  .keys = {
    MT_KEY(MRB_SYM(class_variables),             MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(remove_class_variable),       MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM_Q(class_variable_defined),    MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(class_variable_get),          MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(class_variable_set),          MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(included_modules),            MT_FUNC|MT_NOARG|MT_PUBLIC),
    MT_KEY(MRB_SYM(instance_methods),            MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(public_instance_methods),     MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(private_instance_methods),    MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(protected_instance_methods),  MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(undefined_instance_methods),  MT_FUNC|MT_NOARG|MT_PUBLIC),
    MT_KEY(MRB_SYM(remove_method),               MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(method_removed),              MT_FUNC|MT_PUBLIC),
    MT_KEY(MRB_SYM(constants),                   MT_FUNC|MT_PUBLIC),
  }
};
static mt_tbl metaprog_mod_rom_mt = {
  METAPROG_MOD_ROM_MT_SIZE, METAPROG_MOD_ROM_MT_SIZE,
  (union mt_ptr*)&metaprog_mod_rom_data, NULL
};

void
mrb_mruby_metaprog_gem_init(mrb_state* mrb)
{
  struct RClass *krn = mrb->kernel_module;
  struct RClass *mod = mrb->module_class;

  mrb_mt_init_rom(krn, &metaprog_krn_rom_mt);
  mrb_mt_init_rom(mod, &metaprog_mod_rom_mt);
  mrb_define_class_method_id(mrb, mod, MRB_SYM(constants), mrb_mod_s_constants, MRB_ARGS_ANY()); /* 15.2.2.3.1 */
  mrb_define_class_method_id(mrb, mod, MRB_SYM(nesting), mrb_mod_s_nesting, MRB_ARGS_NONE()); /* 15.2.2.3.2 */
}

void
mrb_mruby_metaprog_gem_final(mrb_state* mrb)
{
}
