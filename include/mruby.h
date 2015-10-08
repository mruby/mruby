/*
** mruby - An embeddable Ruby implementation
**
** Copyright (c) mruby developers 2010-2015
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
#include "mruby/value.h"
#include "mruby/version.h"

/**
 * @file mruby.h
 * @defgroup mruby MRuby C API
 * @{
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
 * Function pointer type of custom allocator used in @ref mrb_open_allocf.
 *
 * The function pointing it must behave similarly as realloc except:
 * - If ptr is NULL it must allocate new space.
 * - If s is NULL, ptr must be freed.
 *
 * See @ref mrb_default_allocf for the default implementation.
 */
typedef void* (*mrb_allocf) (struct mrb_state *mrb, void*, size_t, void *ud);

#ifndef MRB_GC_ARENA_SIZE
#define MRB_GC_ARENA_SIZE 100
#endif

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
  MRB_FIBER_RESUMING,
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
  struct RFiber *fib;
};

enum gc_state {
  GC_STATE_ROOT = 0,
  GC_STATE_MARK,
  GC_STATE_SWEEP
};

struct mrb_jmpbuf;

typedef void (*mrb_atexit_func)(struct mrb_state*);

typedef struct mrb_state {
  struct mrb_jmpbuf *jmp;

  mrb_allocf allocf;                      /* memory allocation function */
  void *allocf_ud;                        /* auxiliary data of allocf */

  struct mrb_context *c;
  struct mrb_context *root_c;

  struct RObject *exc;                    /* exception */
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

  struct heap_page *heaps;                /* heaps for GC */
  struct heap_page *sweeps;
  struct heap_page *free_heaps;
  size_t live; /* count of live objects */
#ifdef MRB_GC_FIXED_ARENA
  struct RBasic *arena[MRB_GC_ARENA_SIZE]; /* GC protection array */
#else
  struct RBasic **arena;                   /* GC protection array */
  int arena_capa;
#endif
  int arena_idx;

  enum gc_state gc_state; /* state of gc */
  int current_white_part; /* make white object by white_part */
  struct RBasic *gray_list; /* list of gray objects to be traversed incrementally */
  struct RBasic *atomic_gray_list; /* list of objects to be traversed atomically */
  size_t gc_live_after_mark;
  size_t gc_threshold;
  int gc_interval_ratio;
  int gc_step_ratio;
  mrb_bool gc_disabled:1;
  mrb_bool gc_full:1;
  mrb_bool is_generational_gc_mode:1;
  mrb_bool out_of_memory:1;
  size_t majorgc_old_threshold;
  struct alloca_header *mems;

  mrb_sym symidx;
  struct kh_n2s *name2sym;      /* symbol hash */
  struct symbol_name *symtbl;   /* symbol table */
  size_t symcapa;

#ifdef ENABLE_DEBUG
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
 *  <pre>
 *    void mrb_example_gem_init(mrb_state* mrb) {
 *      struct RClass *example_class;
 *      example_class = mrb_define_class(mrb, "Example_Class", mrb->object_class);
 *    }
 *
 *    void mrb_example_gem_final(mrb_state* mrb) {
 *      //free(TheAnimals);
 *    }
 *  </pre>
 *
 * @param name The name of the defined class
 * @param super The new class parent
 */
MRB_API struct RClass *mrb_define_class(mrb_state *mrb, const char *name, struct RClass *super);
MRB_API struct RClass *mrb_define_module(mrb_state *, const char*);
MRB_API mrb_value mrb_singleton_class(mrb_state*, mrb_value);
MRB_API void mrb_include_module(mrb_state*, struct RClass*, struct RClass*);
MRB_API void mrb_prepend_module(mrb_state*, struct RClass*, struct RClass*);

/**
 * Defines a global function in ruby.
 *
 * If you're creating a gem it may look something like this:
 *
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
 * @param mrb
 *      The MRuby state reference.
 * @param cla
 *      The class pointer where the method will be defined.
 * @param func
 *      The function pointer to the method definition.
 * @param aspec
 *      The method parameters declaration.
 *      See @ref mruby_mrb_aspec for details.
 */
MRB_API void mrb_define_method(mrb_state *mrb, struct RClass *cla, const char *name, mrb_func_t func, mrb_aspec aspec);

MRB_API void mrb_define_class_method(mrb_state *, struct RClass *, const char *, mrb_func_t, mrb_aspec);
MRB_API void mrb_define_singleton_method(mrb_state*, struct RObject*, const char*, mrb_func_t, mrb_aspec);
MRB_API void mrb_define_module_function(mrb_state*, struct RClass*, const char*, mrb_func_t, mrb_aspec);
MRB_API void mrb_define_const(mrb_state*, struct RClass*, const char *name, mrb_value);
MRB_API void mrb_undef_method(mrb_state*, struct RClass*, const char*);
MRB_API void mrb_undef_class_method(mrb_state*, struct RClass*, const char*);

/**
 * Initialize a new object instace of c class.
 *
 * @param mrb
 *      The current mruby state.
 * @param c
 *      Reference to the class of the new object.
 * @param argc
 *      Number of arguments in argv
 * @param argv
 *      Array of @ref mrb_value "mrb_values" to initialize the object
 * @returns
 *      The newly initialized object
 */
MRB_API mrb_value mrb_obj_new(mrb_state *mrb, struct RClass *c, mrb_int argc, const mrb_value *argv);

/** See @ref mrb_obj_new */
MRB_INLINE mrb_value mrb_class_new_instance(mrb_state *mrb, mrb_int argc, const mrb_value *argv, struct RClass *c)
{
  return mrb_obj_new(mrb,c,argc,argv);
}

MRB_API mrb_value mrb_instance_new(mrb_state *mrb, mrb_value cv);
MRB_API struct RClass * mrb_class_new(mrb_state *mrb, struct RClass *super);
MRB_API struct RClass * mrb_module_new(mrb_state *mrb);
MRB_API mrb_bool mrb_class_defined(mrb_state *mrb, const char *name);
MRB_API struct RClass * mrb_class_get(mrb_state *mrb, const char *name);
MRB_API struct RClass * mrb_class_get_under(mrb_state *mrb, struct RClass *outer, const char *name);
MRB_API struct RClass * mrb_module_get(mrb_state *mrb, const char *name);
MRB_API struct RClass * mrb_module_get_under(mrb_state *mrb, struct RClass *outer, const char *name);
MRB_API mrb_value mrb_notimplement_m(mrb_state*, mrb_value);

MRB_API mrb_value mrb_obj_dup(mrb_state *mrb, mrb_value obj);
MRB_API mrb_value mrb_check_to_integer(mrb_state *mrb, mrb_value val, const char *method);
MRB_API mrb_bool mrb_obj_respond_to(mrb_state *mrb, struct RClass* c, mrb_sym mid);
MRB_API struct RClass * mrb_define_class_under(mrb_state *mrb, struct RClass *outer, const char *name, struct RClass *super);
MRB_API struct RClass * mrb_define_module_under(mrb_state *mrb, struct RClass *outer, const char *name);

/**
 * @defgroup mruby_mrb_aspec Required arguments declaration helpers.
 *
 * Helper functions to declare arguments requirements on methods declared by
 * \ref mrb_define_method and the like
 *
 * @{
 */

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

/** @} */

/**
 * Format specifiers for \ref mrb_get_args function
 *
 * Must be a list of following format specifiers:
 *
 * | char | mruby type     | retrieve types      |note                                                |
 * |:----:|----------------|---------------------|----------------------------------------------------|
 * | o    | Object         | mrb_value           | Could be used to retrieve any type of argument     |
 * | C    | Class/Module   | mrb_value           |                                                    |
 * | S    | String         | mrb_value           | when ! follows, the value may be nil               |
 * | A    | Array          | mrb_value           | when ! follows, the value may be nil               |
 * | H    | Hash           | mrb_value           | when ! follows, the value may be nil               |
 * | s    | String         | char *, mrb_int      |  Receive two arguments; s! gives (NULL,0) for nil  |
 * | z    | String         | char *               | NUL terminated string; z! gives NULL for nil       |
 * | a    | Array          | mrb_value *, mrb_int | Receive two arguments; a! gives (NULL,0) for nil   |
 * | f    | Float          | mrb_float           |                                                    |
 * | i    | Integer        | mrb_int             |                                                    |
 * | b    | boolean        | mrb_bool            |                                                    |
 * | n    | Symbol         | mrb_sym             |                                                    |
 * | &    | block          | mrb_value           |                                                    |
 * | *    | rest arguments | mrb_value *, mrb_int | Receive the rest of arguments as an array.         |
 * | \|   | optional       |                     | After this spec following specs would be optional. |
 * | ?    | optional given | mrb_bool            | True if preceding argument is given. Used to check optional argument is given. |
 */
typedef const char *mrb_args_format;

/**
 * Retrieve arguments from mrb_state.
 *
 * When applicable, implicit conversions (such as to_str, to_ary, to_hash) are
 * applied to received arguments.
 * Use it inside a function pointed by mrb_func_t.
 *
 * @param mrb
 *      The current MRuby state.
 * @param format
 *      is a list of format specifiers see @ref mrb_args_format
 * @param ...
 *      The passing variadic arguments must be a pointer of retrieving type.
 * @return
 *      the number of arguments retrieved.
 */
MRB_API mrb_int mrb_get_args(mrb_state *mrb, mrb_args_format format, ...);

/* `strlen` for character string literals (use with caution or `strlen` instead)
    Adjacent string literals are concatenated in C/C++ in translation phase 6.
    If `lit` is not one, the compiler will report a syntax error:
     MSVC: "error C2143: syntax error : missing ')' before 'string'"
     GCC:  "error: expected ')' before string constant"
*/
#define mrb_strlen_lit(lit) (sizeof(lit "") - 1)

/**
 * Call existing ruby functions.
 */
MRB_API mrb_value mrb_funcall(mrb_state*, mrb_value, const char*, mrb_int,...);
MRB_API mrb_value mrb_funcall_argv(mrb_state*, mrb_value, mrb_sym, mrb_int, const mrb_value*);
MRB_API mrb_value mrb_funcall_with_block(mrb_state*, mrb_value, mrb_sym, mrb_int, const mrb_value*, mrb_value);
MRB_API mrb_sym mrb_intern_cstr(mrb_state*,const char*);
MRB_API mrb_sym mrb_intern(mrb_state*,const char*,size_t);
MRB_API mrb_sym mrb_intern_static(mrb_state*,const char*,size_t);
#define mrb_intern_lit(mrb, lit) mrb_intern_static(mrb, lit, mrb_strlen_lit(lit))
MRB_API mrb_sym mrb_intern_str(mrb_state*,mrb_value);
MRB_API mrb_value mrb_check_intern_cstr(mrb_state*,const char*);
MRB_API mrb_value mrb_check_intern(mrb_state*,const char*,size_t);
MRB_API mrb_value mrb_check_intern_str(mrb_state*,mrb_value);
MRB_API const char *mrb_sym2name(mrb_state*,mrb_sym);
MRB_API const char *mrb_sym2name_len(mrb_state*,mrb_sym,mrb_int*);
MRB_API mrb_value mrb_sym2str(mrb_state*,mrb_sym);

MRB_API void *mrb_malloc(mrb_state*, size_t);         /* raise RuntimeError if no mem */
MRB_API void *mrb_calloc(mrb_state*, size_t, size_t); /* ditto */
MRB_API void *mrb_realloc(mrb_state*, void*, size_t); /* ditto */
MRB_API void *mrb_realloc_simple(mrb_state*, void*, size_t); /* return NULL if no memory available */
MRB_API void *mrb_malloc_simple(mrb_state*, size_t);  /* return NULL if no memory available */
MRB_API struct RBasic *mrb_obj_alloc(mrb_state*, enum mrb_vtype, struct RClass*);
MRB_API void mrb_free(mrb_state*, void*);

MRB_API mrb_value mrb_str_new(mrb_state *mrb, const char *p, size_t len);

/**
 * Turns a C string into a Ruby string value.
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
 * @returns
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
 * @returns
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
 * @returns
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
 * @ref mrb_allocf
 */
MRB_API void* mrb_default_allocf(mrb_state*, void*, size_t, void*);

MRB_API mrb_value mrb_top_self(mrb_state *);
MRB_API mrb_value mrb_run(mrb_state*, struct RProc*, mrb_value);
MRB_API mrb_value mrb_toplevel_run(mrb_state*, struct RProc*);
MRB_API mrb_value mrb_context_run(mrb_state*, struct RProc*, mrb_value, unsigned int);

MRB_API void mrb_p(mrb_state*, mrb_value);
MRB_API mrb_int mrb_obj_id(mrb_value obj);
MRB_API mrb_sym mrb_obj_to_sym(mrb_state *mrb, mrb_value name);

MRB_API mrb_bool mrb_obj_eq(mrb_state*, mrb_value, mrb_value);
MRB_API mrb_bool mrb_obj_equal(mrb_state*, mrb_value, mrb_value);
MRB_API mrb_bool mrb_equal(mrb_state *mrb, mrb_value obj1, mrb_value obj2);
MRB_API mrb_value mrb_convert_to_integer(mrb_state *mrb, mrb_value val, int base);
MRB_API mrb_value mrb_Integer(mrb_state *mrb, mrb_value val);
MRB_API mrb_value mrb_Float(mrb_state *mrb, mrb_value val);
MRB_API mrb_value mrb_inspect(mrb_state *mrb, mrb_value obj);
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

/* fiber functions (you need to link mruby-fiber mrbgem to use) */
MRB_API mrb_value mrb_fiber_resume(mrb_state *mrb, mrb_value fib, mrb_int argc, const mrb_value *argv);
MRB_API mrb_value mrb_fiber_yield(mrb_state *mrb, mrb_int argc, const mrb_value *argv);
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

#ifdef ENABLE_DEBUG
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

/** @} */
MRB_END_DECL

#endif  /* MRUBY_H */
