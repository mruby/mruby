/*
** mruby - An embeddable Ruby implementation
**
** Copyright (c) mruby developers 2010-2016
**
** Permission is hereby granted, free of charge, to any person obtaining
** a copy of this software and associated documentation files (the
** "Software"), to deal in the Software without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Software, and to
** permit persons to whom the Software is furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**
** [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
*/

#ifndef MRUBY_H
#define MRUBY_H

#include <stdint.h>
#include <stddef.h>
#include <limits.h>

#include "mrbconf.h"
#include "mruby/common.h"
#include <mruby/value.h>
#include <mruby/gc.h>
#include <mruby/version.h>

/**
 * MRuby C API entry point
 */
MRB_BEGIN_DECL

typedef uint32_t mrb_code;

/**
 * Required arguments signature type.
 */
typedef uint32_t mrb_aspec;


struct mrb_irep;
struct mrb_state;

/**
 * Function pointer type of custom allocator used in @see mrb_open_allocf.
 *
 * The function pointing it must behave similarly as realloc except:
 * - If ptr is NULL it must allocate new space.
 * - If s is NULL, ptr must be freed.
 *
 * See @see mrb_default_allocf for the default implementation.
 */
typedef void* (*mrb_allocf) (struct mrb_state *mrb, void*, size_t, void *ud);

#ifndef MRB_FIXED_STATE_ATEXIT_STACK_SIZE
#define MRB_FIXED_STATE_ATEXIT_STACK_SIZE 5
#endif

typedef struct {
  mrb_sym mid;
  struct RProc *proc;
  mrb_value *stackent;
  int nregs;
  int ridx;
  int eidx;
  struct REnv *env;
  mrb_code *pc;                 /* return address */
  mrb_code *err;                /* error position */
  int argc;
  int acc;
  struct RClass *target_class;
} mrb_callinfo;

enum mrb_fiber_state {
  MRB_FIBER_CREATED = 0,
  MRB_FIBER_RUNNING,
  MRB_FIBER_RESUMED,
  MRB_FIBER_SUSPENDED,
  MRB_FIBER_TRANSFERRED,
  MRB_FIBER_TERMINATED,
};

struct mrb_context {
  struct mrb_context *prev;

  mrb_value *stack;                       /* stack of virtual machine */
  mrb_value *stbase, *stend;

  mrb_callinfo *ci;
  mrb_callinfo *cibase, *ciend;

  mrb_code **rescue;                      /* exception handler stack */
  int rsize;
  struct RProc **ensure;                  /* ensure handler stack */
  int esize;

  enum mrb_fiber_state status;
  mrb_bool vmexec;
  struct RFiber *fib;
};

struct mrb_jmpbuf;

typedef struct {
  const char *filename;
  int lineno;
  struct RClass *klass;
  const char *sep;
  mrb_sym method_id;
} mrb_backtrace_entry;

typedef void (*mrb_atexit_func)(struct mrb_state*);

#define MRB_STATE_NO_REGEXP 1
#define MRB_STATE_REGEXP    2

typedef struct mrb_state {
  struct mrb_jmpbuf *jmp;

  uint32_t flags;
  mrb_allocf allocf;                      /* memory allocation function */
  void *allocf_ud;                        /* auxiliary data of allocf */

  struct mrb_context *c;
  struct mrb_context *root_c;

  struct RObject *exc;                    /* exception */
  struct {
    struct RObject *exc;
    int n;
    int n_allocated;
    mrb_backtrace_entry *entries;
  } backtrace;
  struct iv_tbl *globals;                 /* global variable table */

  struct RObject *top_self;
  struct RClass *object_class;            /* Object class */
  struct RClass *class_class;
  struct RClass *module_class;
  struct RClass *proc_class;
  struct RClass *string_class;
  struct RClass *array_class;
  struct RClass *hash_class;

  struct RClass *float_class;
  struct RClass *fixnum_class;
  struct RClass *true_class;
  struct RClass *false_class;
  struct RClass *nil_class;
  struct RClass *symbol_class;
  struct RClass *kernel_module;

  struct alloca_header *mems;
  mrb_gc gc;

  mrb_sym symidx;
  struct kh_n2s *name2sym;      /* symbol hash */
  struct symbol_name *symtbl;   /* symbol table */
  size_t symcapa;

#ifdef MRB_ENABLE_DEBUG_HOOK
  void (*code_fetch_hook)(struct mrb_state* mrb, struct mrb_irep *irep, mrb_code *pc, mrb_value *regs);
  void (*debug_op_hook)(struct mrb_state* mrb, struct mrb_irep *irep, mrb_code *pc, mrb_value *regs);
#endif

  struct RClass *eException_class;
  struct RClass *eStandardError_class;
  struct RObject *nomem_err;              /* pre-allocated NoMemoryError */

  void *ud; /* auxiliary data */

#ifdef MRB_FIXED_STATE_ATEXIT_STACK
  mrb_atexit_func atexit_stack[MRB_FIXED_STATE_ATEXIT_STACK_SIZE];
#else
  mrb_atexit_func *atexit_stack;
#endif
  mrb_int atexit_stack_len;
} mrb_state;


typedef mrb_value (*mrb_func_t)(mrb_state *mrb, mrb_value);

/**
 * Defines a new class.
 *
 * If you're creating a gem it may look something like this:
 *
 *      !!!c
 *      void mrb_example_gem_init(mrb_state* mrb) {
 *          struct RClass *example_class;
 *          example_class = mrb_define_class(mrb, "Example_Class", mrb->object_class);
 *      }
 *
 *      void mrb_example_gem_final(mrb_state* mrb) {
 *          //free(TheAnimals);
 *      }
 *
 * @param [mrb_state *] mrb The current mruby state.
 * @param [const char *] name The name of the defined class.
 * @param [struct RClass *] super The new class parent.
 * @return [struct RClass *] Reference to the newly defined class.
 * @see mrb_define_class_under
 */
MRB_API struct RClass *mrb_define_class(mrb_state *mrb, const char *name, struct RClass *super);

/**
 * Defines a new module.
 * 
 * @param [mrb_state *] mrb_state* The current mruby state.
 * @param [const char *] char* The name of the module.
 * @return [struct RClass *] Reference to the newly defined module.
 */
MRB_API struct RClass *mrb_define_module(mrb_state *, const char*);
MRB_API mrb_value mrb_singleton_class(mrb_state*, mrb_value);

/**
 * Include a module in another class or module.
 * Equivalent to: 
 *
 *   module B                                                                                                         
 *     include A                                                                                                    
 *   end 
 * @param [mrb_state *] mrb_state* The current mruby state.
 * @param [struct RClass *] RClass* A reference to module or a class.
 * @param [struct RClass *] RClass* A reference to the module to be included.
 */
MRB_API void mrb_include_module(mrb_state*, struct RClass*, struct RClass*);

/**
 * Prepends a module in another class or module.
 *
 * Equivalent to:
 *  module B
 *    prepend A
 *  end
 * @param [mrb_state *] mrb_state* The current mruby state.
 * @param [struct RClass *] RClass* A reference to module or a class.
 * @param [struct RClass *] RClass* A reference to the module to be prepended.
 */ 
MRB_API void mrb_prepend_module(mrb_state*, struct RClass*, struct RClass*);

/**
 * Defines a global function in ruby.
 *
 * If you're creating a gem it may look something like this
 *
 * Example:
 *
 *     !!!c
 *     mrb_value example_method(mrb_state* mrb, mrb_value self)
 *     {
 *          puts("Executing example command!");
 *          return self;
 *     }
 *
 *     void mrb_example_gem_init(mrb_state* mrb)
 *     {
 *           mrb_define_method(mrb, mrb->kernel_module, "example_method", example_method, MRB_ARGS_NONE());
 *     }
 *
 * @param [mrb_state *] mrb The MRuby state reference.
 * @param [struct RClass *] cla The class pointer where the method will be defined.
 * @param [const char *] name The name of the method being defined.
 * @param [mrb_func_t] func The function pointer to the method definition.
 * @param [mrb_aspec] aspec The method parameters declaration.
 */
MRB_API void mrb_define_method(mrb_state *mrb, struct RClass *cla, const char *name, mrb_func_t func, mrb_aspec aspec);

/**
 * Defines a class method.
 *
 * Example:
 *
 *     # Ruby way
 *     class Foo
 *       def Foo.bar
 *       end
 *     end
 *     // C way
 *     mrb_value bar_method(mrb_state* mrb, mrb_value self){
 *       return mrb_nil_value();
 *     }
 *     void mrb_example_gem_init(mrb_state* mrb){
 *       struct RClass *foo;
 *       foo = mrb_define_class(mrb, "Foo", mrb->object_class);
 *       mrb_define_class_method(mrb, foo, "bar", bar_method, MRB_ARGS_NONE());
 *     }
 * @param [mrb_state *] mrb_state* The MRuby state reference. 
 * @param [struct RClass *] RClass* The class where the class method will be defined.
 * @param [const char *] char* The name of the class method being defined.
 * @param [mrb_func_t] mrb_func_t The function pointer to the class method definition.
 * @param [mrb_aspec] mrb_aspec The method parameters declaration.
 */
MRB_API void mrb_define_class_method(mrb_state *, struct RClass *, const char *, mrb_func_t, mrb_aspec);
MRB_API void mrb_define_singleton_method(mrb_state*, struct RObject*, const char*, mrb_func_t, mrb_aspec);

/**
 *  Defines a module fuction.
 *
 * Example:
 *
 *        # Ruby way
 *        module Foo                                                                                                  
 *          def Foo.bar
 *          end                                                                                                 
 *        end                                                                                                             
 *        // C way                                                                                                      
 *        mrb_value bar_method(mrb_state* mrb, mrb_value self){
 *          return mrb_nil_value();                                                                                                     
 *        }                                                                                                               
 *        void mrb_example_gem_init(mrb_state* mrb){                                                                     
 *          struct RClass *foo;
 *          foo = mrb_define_module(mrb, "Foo");  
 *          mrb_define_module_function(mrb, foo, "bar", bar_method, MRB_ARGS_NONE());
 *        }    
 *  @param [mrb_state *] mrb_state* The MRuby state reference.
 *  @param [struct RClass *] RClass* The module where the module function will be defined.
 *  @param [const char *] char* The name of the module function being defined.
 *  @param [mrb_func_t] mrb_func_t The function pointer to the module function definition. 
 *  @param [mrb_aspec] mrb_aspec The method parameters declaration.
 */
MRB_API void mrb_define_module_function(mrb_state*, struct RClass*, const char*, mrb_func_t, mrb_aspec);

/**
 *  Defines a constant.
 *
 * Example:
 *
 *          # Ruby way
 *          class ExampleClass
 *            AGE = 22
 *          end
 *          // C way
 *          #include <stdio.h> 
 *          #include <mruby.h>
 * 
 *          void
 *          mrb_example_gem_init(mrb_state* mrb){
 *            mrb_define_const(mrb, mrb->kernel_module, "AGE", mrb_fixnum_value(22));
 *          }
 *
 *          mrb_value
 *          mrb_example_gem_final(mrb_state* mrb){
 *          }
 *  @param [mrb_state *] mrb_state* The MRuby state reference.
 *  @param [struct RClass *] RClass* A class or module the constant is defined in.
 *  @param [const char *] name The name of the constant being defined.
 *  @param [mrb_value] mrb_value The value for the constant.
 */
MRB_API void mrb_define_const(mrb_state*, struct RClass*, const char *name, mrb_value);

/**
 * Undefines a method.
 *
 * Example:
 *
 *     # Ruby way
 *
 *     class ExampleClassA
 *       def example_method
 *         "example"
 *       end
 *     end
 *     ExampleClassA.new.example_method # => example
 *
 *     class ExampleClassB < ExampleClassA
 *       undef_method :example_method
 *     end
 *
 *     ExampleClassB.new.example_method # => undefined method 'example_method' for ExampleClassB (NoMethodError)
 *
 *     // C way
 *     #include <stdio.h>
 *     #include <mruby.h>
 * 
 *     mrb_value
 *     mrb_example_method(mrb_state *mrb){
 *       return mrb_str_new_cstr(mrb, "example");
 *     }
 *
 *     void
 *     mrb_example_gem_init(mrb_state* mrb){
 *       struct RClass *example_class_a;
 *       struct RClass *example_class_b;
 *       struct RClass *example_class_c;
 * 
 *       example_class_a = mrb_define_class(mrb, "ExampleClassA", mrb->object_class);
 *       mrb_define_method(mrb, example_class_a, "example_method", mrb_example_method, MRB_ARGS_NONE());
 *       example_class_b = mrb_define_class(mrb, "ExampleClassB", example_class_a);
 *       example_class_c = mrb_define_class(mrb, "ExampleClassC", example_class_b);
 *       mrb_undef_method(mrb, example_class_c, "example_method");
 *     }
 * 
 *     mrb_example_gem_final(mrb_state* mrb){
 *     }
 * @param [mrb_state*] mrb_state* The mruby state reference.
 * @param [struct RClass*] RClass* A class the method will be undefined from.
 * @param [const char*] constchar* The name of the method to be undefined.
 */
MRB_API void mrb_undef_method(mrb_state*, struct RClass*, const char*);

/**
 * Undefine a class method.
 * Example:
 *
 *      # Ruby way
 *      class ExampleClass
 *        def self.example_method
 *          "example"
 *        end
 *      end
 *
 *     ExampleClass.example_method
 *   
 *     // C way
 *     #include <stdio.h>
 *     #include <mruby.h> 
 *
 *     mrb_value
 *     mrb_example_method(mrb_state *mrb){
 *       return mrb_str_new_cstr(mrb, "example");     
 *     }
 * 
 *     void
 *     mrb_example_gem_init(mrb_state* mrb){
 *       struct RClass *example_class;
 *       example_class = mrb_define_class(mrb, "ExampleClass", mrb->object_class);
 *       mrb_define_class_method(mrb, example_class, "example_method", mrb_example_method, MRB_ARGS_NONE());
 *       mrb_undef_class_method(mrb, example_class, "example_method");
 *      }
 * 
 *      void
 *      mrb_example_gem_final(mrb_state* mrb){
 *      }
 * @param [mrb_state*] mrb_state* The mruby state reference.
 * @param [RClass*] RClass* A class the class method will be undefined from.
 * @param [constchar*] constchar* The name of the class method to be undefined.
 */
MRB_API void mrb_undef_class_method(mrb_state*, struct RClass*, const char*);

/**
 * Initialize a new object instace of c class.
 *
 * Example:
 *
 *     # Ruby way
 *     class ExampleClass
 *     end
 *
 *     p ExampleClass # => #<ExampleClass:0x9958588>
 *     // C way
 *     #include <stdio.h>
 *     #include <mruby.h>
 *
 *     void
 *     mrb_example_gem_init(mrb_state* mrb) {
 *       struct RClass *example_class;
 *       mrb_value obj;
 *       example_class = mrb_define_class(mrb, "ExampleClass", mrb->object_class); # => class ExampleClass; end
 *       obj = mrb_obj_new(mrb, example_class, 0, NULL); # => ExampleClass.new
 *       mrb_p(mrb, obj); // => Kernel#p
 *      }  
 * @param [mrb_state*] mrb The current mruby state.
 * @param [RClass*] c Reference to the class of the new object.
 * @param [mrb_int] argc Number of arguments in argv
 * @param [const mrb_value *] argv Array of mrb_value to initialize the object
 * @return [mrb_value] The newly initialized object
 */
MRB_API mrb_value mrb_obj_new(mrb_state *mrb, struct RClass *c, mrb_int argc, const mrb_value *argv);

/** @see mrb_obj_new */
MRB_INLINE mrb_value mrb_class_new_instance(mrb_state *mrb, mrb_int argc, const mrb_value *argv, struct RClass *c)
{
  return mrb_obj_new(mrb,c,argc,argv);
}

MRB_API mrb_value mrb_instance_new(mrb_state *mrb, mrb_value cv);

/**
 * Creates a new instance of Class, Class.
 *
 * Example:
 *
 *      void
 *      mrb_example_gem_init(mrb_state* mrb) {
 *        struct RClass *example_class;
 * 
 *        mrb_value obj;
 *        example_class = mrb_class_new(mrb, mrb->object_class);
 *        obj = mrb_obj_new(mrb, example_class, 0, NULL); // => #<#<Class:0x9a945b8>:0x9a94588>
 *        mrb_p(mrb, obj); // => Kernel#p
 *       }
 *
 * @param [mrb_state*] mrb The current mruby state.
 * @param [struct RClass *] super The super class or parent.
 * @return [struct RClass *] Reference to the new class.
 */
MRB_API struct RClass * mrb_class_new(mrb_state *mrb, struct RClass *super);

/**
 * Creates a new module, Module.
 *
 * Example:
 *      void
 *      mrb_example_gem_init(mrb_state* mrb) {
 *        struct RClass *example_module;
 * 
 *        example_module = mrb_module_new(mrb);
 *      }
 *
 * @param [mrb_state*] mrb The current mruby state.
 * @return [struct RClass *] Reference to the new module.
 */
MRB_API struct RClass * mrb_module_new(mrb_state *mrb);

/**
 * Returns an mrb_bool. True if class was defined, and false if the class was not defined.
 *
 * Example:
 *     void
 *     mrb_example_gem_init(mrb_state* mrb) {
 *       struct RClass *example_class;
 *       mrb_bool cd;
 *
 *       example_class = mrb_define_class(mrb, "ExampleClass", mrb->object_class);
 *       cd = mrb_class_defined(mrb, "ExampleClass");
 *      
 *       // If mrb_class_defined returns 1 then puts "True"
 *       // If mrb_class_defined returns 0 then puts "False"
 *       if (cd == 1){
 *         puts("True");
 *       }
 *       else {
 *         puts("False");
 *       }
 *      }
 *
 * @param [mrb_state*] mrb The current mruby state.
 * @param [const char *] name A string representing the name of the class.
 * @return [mrb_bool] A boolean value.
 */
MRB_API mrb_bool mrb_class_defined(mrb_state *mrb, const char *name);

/**
 * Gets a class.
 * @param [mrb_state*] mrb The current mruby state.
 * @param [const char *] name The name of the class.
 * @return [struct RClass *] A reference to the class.
*/
MRB_API struct RClass * mrb_class_get(mrb_state *mrb, const char *name);

/**
 * Gets a child class.
 * @param [mrb_state*] mrb The current mruby state.
 * @param [struct RClass *] outer The name of the parent class.
 * @param [const char *] name The name of the class.
 * @return [struct RClass *] A reference to the class.
*/
MRB_API struct RClass * mrb_class_get_under(mrb_state *mrb, struct RClass *outer, const char *name);

/**
 * Gets a module.
 * @param [mrb_state*] mrb The current mruby state.
 * @param [const char *] name The name of the module.
 * @return [struct RClass *] A reference to the module.
*/
MRB_API struct RClass * mrb_module_get(mrb_state *mrb, const char *name);

/**
 * Gets a module defined under another module.
 * @param [mrb_state*] mrb The current mruby state.
 * @param [struct RClass *] outer The name of the outer module.
 * @param [const char *] name The name of the module.
 * @return [struct RClass *] A reference to the module.
*/
MRB_API struct RClass * mrb_module_get_under(mrb_state *mrb, struct RClass *outer, const char *name);
MRB_API mrb_value mrb_notimplement_m(mrb_state*, mrb_value);

/**
 * Duplicate an object.
 *
 * Equivalent to:
 *   Object#dup    
 * @param [mrb_state*] mrb The current mruby state.
 * @param [mrb_value] obj Object to be duplicate.
 * @return [mrb_value] The newly duplicated object.
 */
MRB_API mrb_value mrb_obj_dup(mrb_state *mrb, mrb_value obj);
MRB_API mrb_value mrb_check_to_integer(mrb_state *mrb, mrb_value val, const char *method);

/**
 * Returns true if obj responds to the given method. If the method was defined for that
 * class it returns true, it returns false otherwise.
 *
 *      Example:
 *      # Ruby way
 *      class ExampleClass
 *        def example_method
 *        end
 *      end
 *
 *      ExampleClass.new.respond_to?(:example_method) # => true
 *
 *      // C way
 *      void
 *      mrb_example_gem_init(mrb_state* mrb) {
 *        struct RClass *example_class;
 *        mrb_sym mid;
 *        mrb_bool obj_resp;
 *
 *        example_class = mrb_define_class(mrb, "ExampleClass", mrb->object_class);
 *        mrb_define_method(mrb, example_class, "example_method", exampleMethod, MRB_ARGS_NONE());
 *        mid = mrb_intern_str(mrb, mrb_str_new_cstr(mrb, "example_method" ));
 *        obj_resp = mrb_obj_respond_to(mrb, example_class, mid); // => 1(true in Ruby world)
 *      
 *        // If mrb_obj_respond_to returns 1 then puts "True"
 *        // If mrb_obj_respond_to returns 0 then puts "False"
 *        if (obj_resp == 1) {
 *          puts("True");
 *        }
 *        else if (obj_resp == 0) {
 *          puts("False");
 *        }
 *      }
 *
 * @param [mrb_state*] mrb The current mruby state.
 * @param [struct RClass *] c A reference to a class.
 * @param [mrb_sym] mid A symbol referencing a method id.
 * @return [mrb_bool] A boolean value.
 */
MRB_API mrb_bool mrb_obj_respond_to(mrb_state *mrb, struct RClass* c, mrb_sym mid);

/**
 * Defines a new class under a given module
 *
 * @param [mrb_state*] mrb The current mruby state.
 * @param [struct RClass *] outer Reference to the module under which the new class will be defined
 * @param [const char *] name The name of the defined class
 * @param [struct RClass *] super The new class parent
 * @return [struct RClass *] Reference to the newly defined class
 * @see mrb_define_class
 */
MRB_API struct RClass * mrb_define_class_under(mrb_state *mrb, struct RClass *outer, const char *name, struct RClass *super);

MRB_API struct RClass * mrb_define_module_under(mrb_state *mrb, struct RClass *outer, const char *name);

/**
 * Function requires n arguments.
 *
 * @param n
 *      The number of required arguments.
 */
#define MRB_ARGS_REQ(n)     ((mrb_aspec)((n)&0x1f) << 18)

/**
 * Funtion takes n optional arguments
 *
 * @param n
 *      The number of optional arguments.
 */
#define MRB_ARGS_OPT(n)     ((mrb_aspec)((n)&0x1f) << 13)

/**
 * Funtion takes n1 mandatory arguments and n2 optional arguments
 *
 * @param n1
 *      The number of required arguments.
 * @param n2
 *      The number of optional arguments.
 */
#define MRB_ARGS_ARG(n1,n2)   (MRB_ARGS_REQ(n1)|MRB_ARGS_OPT(n2))

/** rest argument */
#define MRB_ARGS_REST()     ((mrb_aspec)(1 << 12))

/** required arguments after rest */
#define MRB_ARGS_POST(n)    ((mrb_aspec)((n)&0x1f) << 7)

/** keyword arguments (n of keys, kdict) */
#define MRB_ARGS_KEY(n1,n2) ((mrb_aspec)((((n1)&0x1f) << 2) | ((n2)?(1<<1):0)))

/**
 * Function takes a block argument
 */
#define MRB_ARGS_BLOCK()    ((mrb_aspec)1)

/**
 * Function accepts any number of arguments
 */
#define MRB_ARGS_ANY()      MRB_ARGS_REST()

/**
 * Function accepts no arguments
 */
#define MRB_ARGS_NONE()     ((mrb_aspec)0)

/**
 * Format specifiers for {mrb_get_args} function
 *
 * Must be a C string composed of the following format specifiers:
 *
 * | char | Ruby type      | C types           | Notes                                               |
 * |:----:|----------------|-------------------|----------------------------------------------------|
 * | `o`  | {Object}       | {mrb_value}       | Could be used to retrieve any type of argument     |
 * | `C`  | {Class}/{Module} | {mrb_value}     |                                                    |
 * | `S`  | {String}       | {mrb_value}       | when `!` follows, the value may be `nil`           |
 * | `A`  | {Array}        | {mrb_value}       | when `!` follows, the value may be `nil`           |
 * | `H`  | {Hash}         | {mrb_value}       | when `!` follows, the value may be `nil`           |
 * | `s`  | {String}       | char *, {mrb_int} |  Receive two arguments; `s!` gives (`NULL`,`0`) for `nil`       |
 * | `z`  | {String}       | char *            | `NULL` terminated string; `z!` gives `NULL` for `nil`           |
 * | `a`  | {Array}        | {mrb_value} *, {mrb_int} | Receive two arguments; `a!` gives (`NULL`,`0`) for `nil` |
 * | `f`  | {Float}        | {mrb_float}       |                                                    |
 * | `i`  | {Integer}      | {mrb_int}         |                                                    |
 * | `b`  | boolean        | {mrb_bool}        |                                                    |
 * | `n`  | {Symbol}       | {mrb_sym}         |                                                    |
 * | `&`  | block          | {mrb_value}       |                                                    |
 * | `*`  | rest arguments | {mrb_value} *, {mrb_int} | Receive the rest of arguments as an array.  |
 * | &vert; | optional     |                   | After this spec following specs would be optional. |
 * | `?`  | optional given | {mrb_bool}        | `TRUE` if preceding argument is given. Used to check optional argument is given. |
 *
 * @see mrb_get_args
 */
typedef const char *mrb_args_format;

/**
 * Retrieve arguments from mrb_state.
 *
 * When applicable, implicit conversions (such as `to_str`, `to_ary`, `to_hash`) are
 * applied to received arguments.
 * Used inside a function of mrb_func_t type.
 *
 * @param mrb The current MRuby state.
 * @param format [mrb_args_format] is a list of format specifiers
 * @param ... The passing variadic arguments must be a pointer of retrieving type.
 * @return the number of arguments retrieved.
 * @see mrb_args_format
 */
MRB_API mrb_int mrb_get_args(mrb_state *mrb, mrb_args_format format, ...);

static inline mrb_sym
mrb_get_mid(mrb_state *mrb) /* get method symbol */
{
  return mrb->c->ci->mid;
}

static inline int
mrb_get_argc(mrb_state *mrb) /* get argc */
{
  return mrb->c->ci->argc;
}

/* `strlen` for character string literals (use with caution or `strlen` instead)
    Adjacent string literals are concatenated in C/C++ in translation phase 6.
    If `lit` is not one, the compiler will report a syntax error:
     MSVC: "error C2143: syntax error : missing ')' before 'string'"
     GCC:  "error: expected ')' before string constant"
*/
#define mrb_strlen_lit(lit) (sizeof(lit "") - 1)

/**
 * Call existing ruby functions.
 *
 *      #include <stdio.h>
 *      #include <mruby.h>
 *      #include "mruby/compile.h"
 * 
 *      int
 *      main()
 *      {
 *        mrb_int i = 99;
 *        mrb_state *mrb = mrb_open();
 * 
 *        if (!mrb) { }
 *        FILE *fp = fopen("test.rb","r");
 *        mrb_value obj = mrb_load_file(mrb,fp);
 *        mrb_funcall(mrb, obj, "method_name", 1, mrb_fixnum_value(i));
 *        fclose(fp);
 *        mrb_close(mrb);
 *       }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [mrb_value] mrb_value A reference to an mruby value.
 * @param [const char*] const char* The name of the method.
 * @param [mrb_int] mrb_int The number of arguments the method has.
 * @param [...] ... Variadic values(not type safe!).
 * @return [mrb_value] mrb_value mruby function value.
 */
MRB_API mrb_value mrb_funcall(mrb_state*, mrb_value, const char*, mrb_int,...);
/**
 * Call existing ruby functions. This is basically the type safe version of mrb_funcall.
 * 
 *      #include <stdio.h>
 *      #include <mruby.h>
 *      #include "mruby/compile.h"
 *      int
 *      main()
 *      {
 *        mrb_int i = 99;
 *        mrb_state *mrb = mrb_open();
 * 
 *        if (!mrb) { }
 *        mrb_sym m_sym = mrb_intern_cstr(mrb, "method_name"); // Symbol for method.
 * 
 *        FILE *fp = fopen("test.rb","r");
 *        mrb_value obj = mrb_load_file(mrb,fp);
 *        mrb_funcall_argv(mrb, obj, m_sym, 1, &obj); // Calling ruby function from test.rb.
 *        fclose(fp);
 *        mrb_close(mrb);
 *       }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [mrb_value] mrb_value A reference to an mruby value.
 * @param [mrb_sym] mrb_sym The symbol representing the method.
 * @param [mrb_int] mrb_int The number of arguments the method has.
 * @param [const mrb_value*] mrb_value* Pointer to the object.
 * @return [mrb_value] mrb_value mruby function value.
 * @see mrb_funcall
 */
MRB_API mrb_value mrb_funcall_argv(mrb_state*, mrb_value, mrb_sym, mrb_int, const mrb_value*);
/**
 * Call existing ruby functions with a block.
 */
MRB_API mrb_value mrb_funcall_with_block(mrb_state*, mrb_value, mrb_sym, mrb_int, const mrb_value*, mrb_value);
/**
 * Creates a symbol from a C string.
 *
 *     # Ruby way:
 *     :symbol # => :symbol
 *     
 *     // C way:
 *     mrb_sym mrb_symbol = mrb_intern_cstr(mrb, "symbol"); //  => :symbol
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [const char*] const char* The name of the method.
 * @return [mrb_sym] mrb_sym A symbol.
 * @see mrb_intern
 */
MRB_API mrb_sym mrb_intern_cstr(mrb_state*,const char*);
/**
 * Creates a symbol from a C string.
 *
 *     # Ruby way
 *     :symbol
 *
 *     // C way
 *     void
 *     mrb_mruby_example_gem_init(mrb_state* mrb) {
 *       mrb_sym mrb_symbol;
 *       char c_str[6] = "symbol";
 *
 *       mrb_symbol = mrb_intern(mrb, "symbol", sizeof(c_str));
 *     }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [const char*] const char* A C string.
 * @param [size_t] size_t An integer representing the size of the string.
 * @return [mrb_sym] mrb_sym A symbol. 
 */
MRB_API mrb_sym mrb_intern(mrb_state*,const char*,size_t);
MRB_API mrb_sym mrb_intern_static(mrb_state*,const char*,size_t);
#define mrb_intern_lit(mrb, lit) mrb_intern_static(mrb, lit, mrb_strlen_lit(lit))
/**
 * Creates a symbol from a String.
 *
 *     void
 *     mrb_mruby_example_gem_init(mrb_state* mrb) {
 *       mrb_sym mrb_symbol;
 *       mrb_value mrb_string;
 *
 *       mrb_string = mrb_str_new_cstr(mrb, "symbol"); // Returns a Ruby string from C string.
 *       mrb_intern_str(mrb, mrb_string); // Returns Symbol from Ruby string.
 *     }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [mrb_value] mrb_value A Ruby value.
 * @return [mrb_sym] mrb_sym A symbol. 
 */
MRB_API mrb_sym mrb_intern_str(mrb_state*,mrb_value);
/**
 * Returns a Symbol, or nil otherwise.
 *
 *     void
 *     mrb_mruby_example_gem_init(mrb_state* mrb) {
 *       mrb_value mrb_symbol;
 *
 *       mrb_symbol = mrb_check_intern_cstr(mrb, "Symbol"); // Returns :Symbol
 *       mrb_p(mrb, mrb_symbol); // This is equivalent to p :Symbol
 *     }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [const char*] char* A C string.
 * @return [mrb_value] mrb_value A Ruby value. 
 */
MRB_API mrb_value mrb_check_intern_cstr(mrb_state*,const char*);
MRB_API mrb_value mrb_check_intern(mrb_state*,const char*,size_t);
/**
 * Returns a Symbol, or nil otherwise.
 *
 *     void
 *     mrb_mruby_example_gem_init(mrb_state* mrb) {
 *       mrb_value mrb_string;
 *       mrb_value mrb_symbol;
 *
 *       mrb_string = mrb_str_new_cstr(mrb, "Symbol"); // Returns a Ruby string from C string.
 *       mrb_symbol = mrb_check_intern_str(mrb, mrb_string); // Returns :Symbol
 *       mrb_p(mrb, mrb_symbol); // This is equivalent to p :Symbol
 *     }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [mrb_value] mrb_value A Ruby value.
 * @return [mrb_value] mrb_value A Ruby value. 
 */
MRB_API mrb_value mrb_check_intern_str(mrb_state*,mrb_value);
/**
 * Returns a C string from a Symbol
 *
 *     void
 *     mrb_mruby_example_gem_init(mrb_state* mrb) {
 *       mrb_sym mrb_symbol;
 *       char* symbol_name = "symbol";
 *
 *       mrb_symbol = mrb_intern(mrb, symbol_name, strlen(symbol_name)); // Returns a symbol
 *       symbol_name = mrb_sym2name(mrb, mrb_symbol); // Returns the name of the symbol
 *       printf("%s\n", symbol_name);
 *     }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [mrb_sym] mrb_sym A Symbol.
 * @return [const char*] char* A C string value. 
 */
MRB_API const char *mrb_sym2name(mrb_state*,mrb_sym);
/**
 * Returns the length of a Symbol as C string
 *
 *     void
 *     mrb_mruby_example_gem_init(mrb_state* mrb) {
 *       mrb_sym mrb_symbol;
 *       mrb_int mrb_integer;
 *       const char* symbol_name = "symbol";
 *
 *       mrb_symbol = mrb_intern(mrb, symbol_name, strlen(symbol_name)); // Returns Symbol.
 *       symbol_name = mrb_sym2name_len(mrb, mrb_symbol, &mrb_integer); // Returns length of symbol name.
 *       printf("%d\n", strlen(symbol_name));
 *     }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [mrb_sym] mrb_sym A Symbol.
 * @param [mrb_int] mrb_int A Ruby integer value.
 * @return [const char*] char* A C string value. 
 */
MRB_API const char *mrb_sym2name_len(mrb_state*,mrb_sym,mrb_int*);
/**
 * Returns a String from a Symbol.
 *
 *     void
 *     mrb_mruby_example_gem_init(mrb_state* mrb) {
 *       mrb_sym mrb_symbol;
 *       mrb_value mrb_string;
 *       const char* symbol_name = "symbol";
 *
 *       mrb_symbol = mrb_intern(mrb, symbol_name, strlen(symbol_name)); // Returns Symbol.
 *       mrb_string = mrb_sym2str(mrb, mrb_symbol); // Returns symbol name as a Ruby string.
 *       mrb_p(mrb, mrb_string);
 *     }
 * @param [mrb_state*] mrb_state* The current mruby state.
 * @param [mrb_sym] mrb_sym A Symbol.
 * @return [mrb_value] mrb_value A Ruby string value. 
 */
MRB_API mrb_value mrb_sym2str(mrb_state*,mrb_sym);
/**
 *  Allocates the requested memory and returns a pointer to it. It raises RuntimeError if no memory.
 *  
 *  @param [mrb_state*] mrb_state* The current mruby state.
 *  @param [size_t] size_t The size of memory block, in bytes.
 *  @return [void*] void* Pointer to requested block of memory.
 */
MRB_API void *mrb_malloc(mrb_state*, size_t);
MRB_API void *mrb_calloc(mrb_state*, size_t, size_t); /* raise RuntimeError if no mem */
/**
 *  Attempts to resize the memory block pointed to by ptr that was previously allocated with a call to mrb_malloc or mrb_calloc.
 *  It raises RuntimeError if no memory.
 *  
 *  @param [mrb_state*] mrb_state* The current mruby state.
 *  @param [void*] void* This is the pointer to a memory block previously allocated with mrb_malloc or mrb_calloc.
 *  @param [size_t] size_t The size of memory block, in bytes.
 *  @return [void*] void* Pointer to requested block of memory.
 */
MRB_API void *mrb_realloc(mrb_state*, void*, size_t);
/**
 *  Attempts to resize the memory block pointed to by ptr that was previously allocated with a call to mrb_malloc_simple.
 *  It returns NULL if there's no memory available.
 *  
 *  @param [mrb_state*] mrb_state* The current mruby state.
 *  @param [void*] void* This is the pointer to a memory block previously allocated with mrb_malloc_simple.
 *  @param [size_t] size_t The size of memory block, in bytes.
 *  @return [void*] void* Pointer to requested block of memory.
 */
MRB_API void *mrb_realloc_simple(mrb_state*, void*, size_t);
/**
 *  Allocates the requested memory and returns a pointer to it. It return NULL if no memory available.
 *  
 *  @param [mrb_state*] mrb_state* The current mruby state.
 *  @param [size_t] size_t The size of memory block, in bytes.
 *  @return [void*] void* Pointer to requested block of memory.
 */
MRB_API void *mrb_malloc_simple(mrb_state*, size_t);
MRB_API struct RBasic *mrb_obj_alloc(mrb_state*, enum mrb_vtype, struct RClass*);
/**
 *  Deallocates the memory previously allocated by a call to mrb_calloc, mrb_malloc, mrb_realloc, mrb_malloc_simple, or mrb_realloc_simple
 *  
 *  @param [mrb_state*] mrb_state* The current mruby state.
 *  @param [void*] void* The pointer to a memory block previously allocated.
 */
MRB_API void mrb_free(mrb_state*, void*);
/**
 *  Returns a String from C String.
 *
 *      void
 *      mrb_mruby_example_gem_init(mrb_state* mrb) {
 *        mrb_value mrb_string;
 *        char c_str[6] = "String";
 *
 *        mrb_string = mrb_str_new(mrb, c_str, strlen(c_str)); // Return Ruby string value.
 *        mrb_p(mrb, mrb_string);
 *      }
 *  @param [mrb_state*] mrb The current mruby state.
 *  @param [const char] *p A C string.
 *  @param [size_t] len The length of string.
 *  @return [mrb_value] mrb_value A Ruby value.
 */
MRB_API mrb_value mrb_str_new(mrb_state *mrb, const char *p, size_t len);

/**
 *  Returns a String from a C string.
 *
 *      void
 *      mrb_mruby_example_gem_init(mrb_state* mrb) {
 *        mrb_value mrb_string;
 *        char c_str[6] = "String";
 *
 *        mrb_string = mrb_str_new_cstr(mrb, c_str); // Return Ruby string value.
 *        mrb_p(mrb, mrb_string);
 *      }
 *  @param [mrb_state*] mrb The current mruby state.
 *  @param [const char] *p A C string.
 *  @return [mrb_value] mrb_value A Ruby value.
 */
MRB_API mrb_value mrb_str_new_cstr(mrb_state*, const char*);
MRB_API mrb_value mrb_str_new_static(mrb_state *mrb, const char *p, size_t len);
#define mrb_str_new_lit(mrb, lit) mrb_str_new_static(mrb, (lit), mrb_strlen_lit(lit))

#ifdef _WIN32
char* mrb_utf8_from_locale(const char *p, size_t len);
char* mrb_locale_from_utf8(const char *p, size_t len);
#define mrb_locale_free(p) free(p)
#define mrb_utf8_free(p) free(p)
#else
#define mrb_utf8_from_locale(p, l) (p)
#define mrb_locale_from_utf8(p, l) (p)
#define mrb_locale_free(p)
#define mrb_utf8_free(p)
#endif

/**
 * Creates new mrb_state.
 *
 * @return
 *      Pointer to the newly created mrb_state.
 */
MRB_API mrb_state* mrb_open(void);

/**
 * Create new mrb_state with custom allocators.
 *
 * @param f
 *      Reference to the allocation function.
 * @param ud
 *      User data will be passed to custom allocator f.
 *      If user data isn't required just pass NULL.
 * @return
 *      Pointer to the newly created mrb_state.
 */
MRB_API mrb_state* mrb_open_allocf(mrb_allocf f, void *ud);

/**
 * Create new mrb_state with just the MRuby core
 *
 * @param f
 *      Reference to the allocation function.
 *      Use mrb_default_allocf for the default
 * @param ud
 *      User data will be passed to custom allocator f.
 *      If user data isn't required just pass NULL.
 * @return
 *      Pointer to the newly created mrb_state.
 */
MRB_API mrb_state* mrb_open_core(mrb_allocf f, void *ud);

/**
 * Closes and frees a mrb_state.
 *
 * @param mrb
 *      Pointer to the mrb_state to be closed.
 */
MRB_API void mrb_close(mrb_state *mrb);

/**
 * The default allocation function.
 *
 * @see mrb_allocf
 */
MRB_API void* mrb_default_allocf(mrb_state*, void*, size_t, void*);

MRB_API mrb_value mrb_top_self(mrb_state *);
MRB_API mrb_value mrb_run(mrb_state*, struct RProc*, mrb_value);
MRB_API mrb_value mrb_top_run(mrb_state*, struct RProc*, mrb_value, unsigned int);
MRB_API mrb_value mrb_vm_run(mrb_state*, struct RProc*, mrb_value, unsigned int);
MRB_API mrb_value mrb_vm_exec(mrb_state*, struct RProc*, mrb_code*);
/* compatibility macros */
#define mrb_toplevel_run_keep(m,p,k) mrb_top_run((m),(p),mrb_top_self(m),(k))
#define mrb_toplevel_run(m,p) mrb_toplevel_run_keep((m),(p),0)
#define mrb_context_run(m,p,s,k) mrb_vm_run((m),(p),(s),(k))

/**
 *  Directly writes obj.inspect followed by a newline to the program’s standard output.
 *
 *      void
 *      mrb_mruby_example_gem_init(mrb_state* mrb) 
 *      {
 *          mrb_value mrb_string; // Declare mrb_string
 *          char c_str[6] = "String"; // Initialize c_str variable.
 *
 *          mrb_string = mrb_str_new(mrb, c_str, strlen(c_str)); // Create new String object. 
 *          mrb_p(mrb, mrb_string); // Writes an object as a string.
 *      }
 *  @param [mrb_state*] mrb The current mruby state.
 *  @param [mrb_value] obj An object.
 */
MRB_API void mrb_p(mrb_state*, mrb_value);

/**
 *  Returns an integer identifier for obj.
 *
 *      void
 *      mrb_mruby_example_gem_init(mrb_state* mrb) 
 *      {
 *          mrb_value mrb_string;         // Declare mrb_string as an mrb_value.
 *          mrb_int obj_id;               // Declare obj_id as an mrb_int.
 *          mrb_value mrb_fixnum;         // Declare mrb_fixnum as an mrb_value.
 *          char c_str[6] = "String";     // Initialize c_str variable.
 *
 *          mrb_string = mrb_str_new_cstr(mrb, c_str);      // Create new String object. 
 *          obj_id = mrb_obj_id(mrb_string);                // Returns object id.
 *          mrb_fixnum = mrb_fixnum_value(obj_id);          // Returns obj_id as a Fixnum. 
 *          mrb_p(mrb, mrb_fixnum);                         // prints object id.
 *      }
 *  @param [mrb_state*] mrb The current mruby state.
 *  @param [mrb_value] obj An object.
 *  @return [mrb_int] mrb_int A Ruby Integer value.
 */
MRB_API mrb_int mrb_obj_id(mrb_value obj);

/**
 *  Returns Symbol from an Object.
 *
 *      // Example:
 *      void
 *      mrb_mruby_example_gem_init(mrb_state* mrb) 
 *      {
 *          mrb_sym obj_sym;            // Declare obj_sym as an mrb_sym.
 *          mrb_value mrb_string;       // Declare mrb_string as an mrb_value.
 *          char c_str[6] = "String";   // Initialize c_str variable.
 *
 *          mrb_string = mrb_str_new(mrb, c_str, strlen(c_str));  // Create new String object.
 *          obj_sym = mrb_obj_to_sym(mrb, mrb_string);            // Returns a String Symbol.
 *          mrb_p(mrb, mrb_symbol_value(obj_sym));                // Print Symbol Object as String.
 *       }
 *  @param [mrb_state*] mrb The current mruby state.
 *  @param [mrb_value] obj An object.
 *  @return [mrb_sym] mrb_sym A Symbol.
 */
MRB_API mrb_sym mrb_obj_to_sym(mrb_state *mrb, mrb_value name);

/**
 * Returns True if the two passed in Objects are equal. Returns Flase otherwise.
 *
 * @see mrb_obj_equal
 */
MRB_API mrb_bool mrb_obj_eq(mrb_state*, mrb_value, mrb_value);

/**
 * Returns True if the two passed in Objects are equal. Returns Flase otherwise.
 *
 * @see mrb_equal
 */
MRB_API mrb_bool mrb_obj_equal(mrb_state*, mrb_value, mrb_value);

/**
 *  Returns True if the two passed in Objects are equal. Returns Flase otherwise.
 *
 *      void
 *      mrb_mruby_example_gem_init(mrb_state* mrb) 
 *      {
 *          mrb_int i1 = 1;   // Declare i1 as mrb_int.
 *          mrb_int i2 = 1;   // Declare i2 as mrb_int.
 *          mrb_value f1;     // Declare f1 as mrb_value.
 *          mrb_value f2;     // Declare f2 as mrb_value.
 *          mrb_bool obj_eql; // Declare obj_eql as mrb_bool.
 *
 *          f1 = mrb_fixnum_value(i1);            // Get Fixnum value from i1.
 *          f2 = mrb_fixnum_value(i2);            // Get Fixnum value from i2.
 *          obj_eql = mrb_obj_eq(mrb, f1, f2);    // Returns True f1 and f2 are equal.
 *          mrb_p(mrb, mrb_bool_value(obj_eql));  // Print Boolean Object as String.
 *      }
 *  @param [mrb_state*] mrb The current mruby state.
 *  @param [mrb_value] obj1 An object.
 *  @param [mrb_value] obj2 An object.
 *  @return [mrb_bool] mrb_bool A boolean value.
 */
MRB_API mrb_bool mrb_equal(mrb_state *mrb, mrb_value obj1, mrb_value obj2);

/**
 *  Returns an integer from Object. Depending on what number is passed to base it will
 *  return an integer of another base number system. ex. 2 will return binary numbers,
 *  or 10 will return decimal numbers, and 16 will return hexedecimal numbers.
 *
 *      // Example
 *      void
 *      mrb_mruby_example_gem_init(mrb_state* mrb) 
 *      {
 *          mrb_value mrb_string_base_2;     // Declare mrb_string_base_2 as mrb_value.
 *          mrb_value mrb_string_base_10;    // Declare mrb_string_base_10 as mrb_value.
 *          mrb_value mrb_string_base_16;    // Declare mrb_string_base_16 as mrb_value.
 *          mrb_value base_2;                // Declare base_2 as mrb_value.
 *          mrb_value base_10;               // Declare base_10 as mrb_value.
 *          mrb_value base_16;               // Declare base_16 as mrb_value.
 *          char c_str1_base_2[8] = "1100101";      // Declare c_str1_base_2 as C String.
 *          char c_str1_base_10[8] = "1100101";     // Declare c_str1_base_10 as C String.
 *          char c_str1_base_16[3] = "0a";          // Declare c_str1_base_16 as C String.
 *
 *          mrb_string_base_2 = mrb_str_new(mrb, c_str1_base_2, strlen(c_str1_base_2));       // Creates String Object.
 *          mrb_string_base_10 = mrb_str_new(mrb, c_str1_base_10, strlen(c_str1_base_10));    // Creates String Object.
 *          mrb_string_base_16 = mrb_str_new(mrb, c_str1_base_16, strlen(c_str1_base_16));    // Creates String Object.
 *          base_2 = mrb_convert_to_integer(mrb, mrb_string_base_2, 2);                       // returns "1100101"
 *          base_10 = mrb_convert_to_integer(mrb, mrb_string_base_10, 10);                    // returns "1100101"
 *          base_16 = mrb_convert_to_integer(mrb, mrb_string_base_16, 16);                    // returns "10"
 *          mrb_p(mrb, base_2);                                                               // Print Fixnum Object as String.
 *          mrb_p(mrb, base_10);                                                              // Print Fixnum Object as String.
 *          mrb_p(mrb, base_16);                                                              // Print Fixnum Object as String.
 *       }
 *  @param [mrb_state*] mrb The current mruby state.
 *  @param [mrb_value] val An object.
 *  @param [int] base An integer representing the number system you want returned.
 *  @return [mrb_value] mrb_value An Object.
 */
MRB_API mrb_value mrb_convert_to_integer(mrb_state *mrb, mrb_value val, int base);

/**
 * Returns an Integer from Object. This differs from mrb_convert_to_integer() in that
 * it sets base to 0 giving you an Integer of what ever String you pass in.
 *
 * @see mrb_convert_to_integer
 */
MRB_API mrb_value mrb_Integer(mrb_state *mrb, mrb_value val);

/**
 * Returns an Float from Object.
 *
 * @param [mrb_state*] mrb The current mruby state.
 * @param [mrb_value] obj An object.
 * @return [mrb_value] mrb_value An Object.
 */
MRB_API mrb_value mrb_Float(mrb_state *mrb, mrb_value val);

/**
 *  Creates a string representation of passed in value.
 *
 *  @param [mrb_state*] mrb The current mruby state.
 *  @param [mrb_value] obj An object.
 *  @return [mrb_value] mrb_value An Object.
 */
MRB_API mrb_value mrb_inspect(mrb_state *mrb, mrb_value obj);

/**
 * Returns True if the two passed in Objects are equal. Returns Flase otherwise.
 *
 * @see mrb_eql
 */
MRB_API mrb_bool mrb_eql(mrb_state *mrb, mrb_value obj1, mrb_value obj2);

MRB_API void mrb_garbage_collect(mrb_state*);
MRB_API void mrb_full_gc(mrb_state*);
MRB_API void mrb_incremental_gc(mrb_state *);
MRB_API int mrb_gc_arena_save(mrb_state*);
MRB_API void mrb_gc_arena_restore(mrb_state*,int);
MRB_API void mrb_gc_mark(mrb_state*,struct RBasic*);
#define mrb_gc_mark_value(mrb,val) do {\
  if (!mrb_immediate_p(val)) mrb_gc_mark((mrb), mrb_basic_ptr(val)); \
} while (0)
MRB_API void mrb_field_write_barrier(mrb_state *, struct RBasic*, struct RBasic*);
#define mrb_field_write_barrier_value(mrb, obj, val) do{\
  if (!mrb_immediate_p(val)) mrb_field_write_barrier((mrb), (obj), mrb_basic_ptr(val)); \
} while (0)
MRB_API void mrb_write_barrier(mrb_state *, struct RBasic*);

MRB_API mrb_value mrb_check_convert_type(mrb_state *mrb, mrb_value val, enum mrb_vtype type, const char *tname, const char *method);
MRB_API mrb_value mrb_any_to_s(mrb_state *mrb, mrb_value obj);
MRB_API const char * mrb_obj_classname(mrb_state *mrb, mrb_value obj);
MRB_API struct RClass* mrb_obj_class(mrb_state *mrb, mrb_value obj);
MRB_API mrb_value mrb_class_path(mrb_state *mrb, struct RClass *c);
MRB_API mrb_value mrb_convert_type(mrb_state *mrb, mrb_value val, enum mrb_vtype type, const char *tname, const char *method);
MRB_API mrb_bool mrb_obj_is_kind_of(mrb_state *mrb, mrb_value obj, struct RClass *c);
MRB_API mrb_value mrb_obj_inspect(mrb_state *mrb, mrb_value self);
MRB_API mrb_value mrb_obj_clone(mrb_state *mrb, mrb_value self);

#ifndef ISPRINT
#define ISASCII(c) ((unsigned)(c) <= 0x7f)
#define ISPRINT(c) (((unsigned)(c) - 0x20) < 0x5f)
#define ISSPACE(c) ((c) == ' ' || (unsigned)(c) - '\t' < 5)
#define ISUPPER(c) (((unsigned)(c) - 'A') < 26)
#define ISLOWER(c) (((unsigned)(c) - 'a') < 26)
#define ISALPHA(c) ((((unsigned)(c) | 0x20) - 'a') < 26)
#define ISDIGIT(c) (((unsigned)(c) - '0') < 10)
#define ISXDIGIT(c) (ISDIGIT(c) || ((unsigned)(c) | 0x20) - 'a' < 6)
#define ISALNUM(c) (ISALPHA(c) || ISDIGIT(c))
#define ISBLANK(c) ((c) == ' ' || (c) == '\t')
#define ISCNTRL(c) ((unsigned)(c) < 0x20 || (c) == 0x7f)
#define TOUPPER(c) (ISLOWER(c) ? ((c) & 0x5f) : (c))
#define TOLOWER(c) (ISUPPER(c) ? ((c) | 0x20) : (c))
#endif

MRB_API mrb_value mrb_exc_new(mrb_state *mrb, struct RClass *c, const char *ptr, size_t len);
MRB_API mrb_noreturn void mrb_exc_raise(mrb_state *mrb, mrb_value exc);

MRB_API mrb_noreturn void mrb_raise(mrb_state *mrb, struct RClass *c, const char *msg);
MRB_API mrb_noreturn void mrb_raisef(mrb_state *mrb, struct RClass *c, const char *fmt, ...);
MRB_API mrb_noreturn void mrb_name_error(mrb_state *mrb, mrb_sym id, const char *fmt, ...);
MRB_API void mrb_warn(mrb_state *mrb, const char *fmt, ...);
MRB_API mrb_noreturn void mrb_bug(mrb_state *mrb, const char *fmt, ...);
MRB_API void mrb_print_backtrace(mrb_state *mrb);
MRB_API void mrb_print_error(mrb_state *mrb);

/* macros to get typical exception objects
   note:
   + those E_* macros requires mrb_state* variable named mrb.
   + exception objects obtained from those macros are local to mrb
*/
#define E_RUNTIME_ERROR             (mrb_class_get(mrb, "RuntimeError"))
#define E_TYPE_ERROR                (mrb_class_get(mrb, "TypeError"))
#define E_ARGUMENT_ERROR            (mrb_class_get(mrb, "ArgumentError"))
#define E_INDEX_ERROR               (mrb_class_get(mrb, "IndexError"))
#define E_RANGE_ERROR               (mrb_class_get(mrb, "RangeError"))
#define E_NAME_ERROR                (mrb_class_get(mrb, "NameError"))
#define E_NOMETHOD_ERROR            (mrb_class_get(mrb, "NoMethodError"))
#define E_SCRIPT_ERROR              (mrb_class_get(mrb, "ScriptError"))
#define E_SYNTAX_ERROR              (mrb_class_get(mrb, "SyntaxError"))
#define E_LOCALJUMP_ERROR           (mrb_class_get(mrb, "LocalJumpError"))
#define E_REGEXP_ERROR              (mrb_class_get(mrb, "RegexpError"))
#define E_SYSSTACK_ERROR            (mrb_class_get(mrb, "SystemStackError"))

#define E_NOTIMP_ERROR              (mrb_class_get(mrb, "NotImplementedError"))
#define E_FLOATDOMAIN_ERROR         (mrb_class_get(mrb, "FloatDomainError"))

#define E_KEY_ERROR                 (mrb_class_get(mrb, "KeyError"))

MRB_API mrb_value mrb_yield(mrb_state *mrb, mrb_value b, mrb_value arg);
MRB_API mrb_value mrb_yield_argv(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv);
MRB_API mrb_value mrb_yield_with_class(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv, mrb_value self, struct RClass *c);

/* mrb_gc_protect() leaves the object in the arena */
MRB_API void mrb_gc_protect(mrb_state *mrb, mrb_value obj);
/* mrb_gc_register() keeps the object from GC. */
MRB_API void mrb_gc_register(mrb_state *mrb, mrb_value obj);
/* mrb_gc_unregister() removes the object from GC root. */
MRB_API void mrb_gc_unregister(mrb_state *mrb, mrb_value obj);

MRB_API mrb_value mrb_to_int(mrb_state *mrb, mrb_value val);
#define mrb_int(mrb, val) mrb_fixnum(mrb_to_int(mrb, val))
MRB_API void mrb_check_type(mrb_state *mrb, mrb_value x, enum mrb_vtype t);

typedef enum call_type {
  CALL_PUBLIC,
  CALL_FCALL,
  CALL_VCALL,
  CALL_TYPE_MAX
} call_type;

MRB_API void mrb_define_alias(mrb_state *mrb, struct RClass *klass, const char *name1, const char *name2);
MRB_API const char *mrb_class_name(mrb_state *mrb, struct RClass* klass);
MRB_API void mrb_define_global_const(mrb_state *mrb, const char *name, mrb_value val);

MRB_API mrb_value mrb_attr_get(mrb_state *mrb, mrb_value obj, mrb_sym id);

MRB_API mrb_bool mrb_respond_to(mrb_state *mrb, mrb_value obj, mrb_sym mid);
MRB_API mrb_bool mrb_obj_is_instance_of(mrb_state *mrb, mrb_value obj, struct RClass* c);


/*
 * Resume a Fiber
 *
 * @mrbgem mruby-fiber
 */
MRB_API mrb_value mrb_fiber_resume(mrb_state *mrb, mrb_value fib, mrb_int argc, const mrb_value *argv);

/*
 * Yield a Fiber
 *
 * @mrbgem mruby-fiber
 */
MRB_API mrb_value mrb_fiber_yield(mrb_state *mrb, mrb_int argc, const mrb_value *argv);

/*
 * FiberError reference
 *
 * @mrbgem mruby-fiber
 */
#define E_FIBER_ERROR (mrb_class_get(mrb, "FiberError"))

/* memory pool implementation */
typedef struct mrb_pool mrb_pool;
MRB_API struct mrb_pool* mrb_pool_open(mrb_state*);
MRB_API void mrb_pool_close(struct mrb_pool*);
MRB_API void* mrb_pool_alloc(struct mrb_pool*, size_t);
MRB_API void* mrb_pool_realloc(struct mrb_pool*, void*, size_t oldlen, size_t newlen);
MRB_API mrb_bool mrb_pool_can_realloc(struct mrb_pool*, void*, size_t);
MRB_API void* mrb_alloca(mrb_state *mrb, size_t);

MRB_API void mrb_state_atexit(mrb_state *mrb, mrb_atexit_func func);

MRB_API void mrb_show_version(mrb_state *mrb);
MRB_API void mrb_show_copyright(mrb_state *mrb);

#ifdef MRB_DEBUG
#include <assert.h>
#define mrb_assert(p) assert(p)
#define mrb_assert_int_fit(t1,n,t2,max) assert((n)>=0 && ((sizeof(n)<=sizeof(t2))||(n<=(t1)(max))))
#else
#define mrb_assert(p) ((void)0)
#define mrb_assert_int_fit(t1,n,t2,max) ((void)0)
#endif

#if __STDC_VERSION__ >= 201112L
#define mrb_static_assert(exp, str) _Static_assert(exp, str)
#else
#define mrb_static_assert(exp, str) mrb_assert(exp)
#endif

MRB_API mrb_value mrb_format(mrb_state *mrb, const char *format, ...);

MRB_END_DECL

#endif  /* MRUBY_H */
