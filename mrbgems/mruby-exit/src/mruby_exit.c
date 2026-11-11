#include <stdlib.h>
#include <mruby.h>
#include <mruby/array.h>
#include <mruby/error.h>
#include <mruby/variable.h>

#ifndef EXIT_SUCCESS
# define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
# define EXIT_FAILURE 1
#endif

static int
get_status(mrb_state *mrb)
{
  mrb_value status = mrb_true_value();

  mrb_get_args(mrb, "|o", &status);
  if (mrb_true_p(status)) return EXIT_SUCCESS;
  if (mrb_false_p(status)) return EXIT_FAILURE;
  mrb_int n = mrb_as_int(mrb, status);
#if MRB_INT_MAX > INT_MAX
  if (n < INT_MIN || n > INT_MAX) {
    mrb_raisef(mrb, E_RANGE_ERROR, "exit status out of range: %i", n);
  }
#endif
  return (int)n;
}

/*
 *  call-seq:
 *     exit(status=true)
 *
 *  Initiates the termination of the Ruby script by raising the
 *  SystemExit exception. This exception may be caught. The
 *  optional parameter is used to return a status code to the invoking
 *  environment.
 *
 *  `true` and `false` of _status_ means success and failure
 *  respectively.  The interpretation of other integer values are
 *  system dependent.
 *
 *     exit(0)
 */
static mrb_value
f_exit(mrb_state *mrb, mrb_value self)
{
  int status = get_status(mrb);
  mrb_value exc = mrb_obj_new(mrb, mrb_exc_get_id(mrb, MRB_SYM(SystemExit)), 0, NULL);
  struct RException *e = mrb_exc_ptr(exc);
  e->flags |= MRB_EXC_EXIT;
  mrb_iv_set(mrb, exc, MRB_IVSYM(status), mrb_int_value(mrb, (mrb_int)status));
  mrb_exc_raise(mrb, exc);
  /* not reached */
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     exit!(status=true)
 *
 *  Exits the process immediately. No exit handlers are run.
 *  <em>status</em> is returned to the underlying system as the
 *  exit status.
 *
 *     exit!(0)
 */
static mrb_value
f_exit_bang(mrb_state *mrb, mrb_value self)
{
  exit(get_status(mrb));
  /* not reached */
  return mrb_nil_value();
}

/*
 * Kernel#at_exit keeps its blocks in this hidden global, and runs them when
 * the state is closed, from the function it registers on its first call
 * with mrb_state_atexit(); a program that registers none pays nothing.
 */
#define AT_EXIT_LIST MRB_SYM(__at_exit__)

static mrb_value
at_exit_call(mrb_state *mrb, void *proc)
{
  return mrb_yield_argv(mrb, *(mrb_value*)proc, 0, NULL);
}

/*
 * Runs the blocks, the last registered first, those they register in turn
 * included. While they run, `$!` is the exception the program ended with
 * (`mrb->exc`, left as it is), or the last one a block raised. A block that
 * raises anything but SystemExit has its exception reported; the rest run
 * either way. If one raised, the process then ends with the status of the
 * last SystemExit, or in failure when another exception came after it,
 * since mrb_close() has no status to answer.
 */
static void
at_exit_run(mrb_state *mrb)
{
  mrb_value list = mrb_gv_get(mrb, AT_EXIT_LIST);
  if (!mrb_array_p(list)) return;

  struct RObject *exc = mrb->exc;
  mrb_sym bang = mrb_intern_lit(mrb, "$!");
  /* what `$!` is next, rooted here: a block may set `$!` itself, and
     mrb->exc does not hold the exception while the blocks run */
  mrb_value held = mrb_ary_new_capa(mrb, 1);
  mrb_ary_push(mrb, held, exc ? mrb_obj_value(exc) : mrb_nil_value());
  int ai = mrb_gc_arena_save(mrb);
  mrb_bool raised = FALSE;
  int status = EXIT_SUCCESS;

  mrb->exc = NULL;
  while (RARRAY_LEN(list) > 0) {
    mrb_value proc = mrb_ary_pop(mrb, list);
    mrb_bool error;

    /* out of the list, so rooted before anything allocates */
    mrb_gc_protect(mrb, proc);
    mrb_gv_set(mrb, bang, RARRAY_PTR(held)[0]);
    mrb_value r = mrb_protect_error(mrb, at_exit_call, &proc, &error);
    if (error && mrb_exception_p(r)) {
      struct RObject *e = mrb_obj_ptr(r);
      mrb_ary_set(mrb, held, 0, r);
      raised = TRUE;
      if (MRB_EXC_EXIT_P(mrb_exc_ptr(r))) {
        status = MRB_EXC_EXIT_STATUS(mrb, e);
      }
      else {
        mrb->exc = e;
        mrb_print_error(mrb);
        mrb->exc = NULL;
        status = EXIT_FAILURE;
      }
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  mrb->exc = exc;
  if (raised) exit(status);
}

/*
 *  call-seq:
 *     at_exit { block } -> proc
 *
 *  Registers the block to be called when the program ends, as the state is
 *  closed; blocks run in reverse order of registration. While they run,
 *  `$!` is the exception the program ended with, if any; a block that calls
 *  `exit` sets the exit status. `exit!` ends the program without running
 *  them.
 *
 *     at_exit { puts "goodbye" }
 *     at_exit { puts "see you" }
 *
 *  <em>produces:</em>
 *
 *     see you
 *     goodbye
 */
static mrb_value
f_at_exit(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;

  mrb_get_args(mrb, "&", &blk);
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "called without a block");
  }
  mrb_value list = mrb_gv_get(mrb, AT_EXIT_LIST);
  if (!mrb_array_p(list)) {
    list = mrb_ary_new(mrb);
    /* the hook before the list: a list without one would not run */
    mrb_state_atexit(mrb, at_exit_run);
    mrb_gv_set(mrb, AT_EXIT_LIST, list);
  }
  mrb_ary_push(mrb, list, blk);
  return blk;
}

void
mrb_mruby_exit_gem_init(mrb_state* mrb)
{
  mrb_define_class_id(mrb, MRB_SYM(SystemExit), E_EXCEPTION);
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(exit), f_exit, MRB_ARGS_OPT(1));
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM_B(exit), f_exit_bang, MRB_ARGS_OPT(1));
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(at_exit), f_at_exit, MRB_ARGS_BLOCK());
}

void
mrb_mruby_exit_gem_final(mrb_state* mrb)
{
}
