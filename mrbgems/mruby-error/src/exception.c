#include <mruby.h>
#include <mruby/error.h>

/* Helper structure to pass function and data to protection wrapper */
struct protect_data {
  mrb_func_t body;    /* Function to be executed under protection */
  mrb_value data;     /* Data to be passed to the function */
};

/* Helper function that wraps user function calls for exception protection.
 * Extracts function and data from protect_data structure and calls the
 * user function with proper parameters. Used internally by mrb_protect. */
static mrb_value
protect_body(mrb_state *mrb, void *p)
{
  struct protect_data *dp = (struct protect_data*)p;
  return dp->body(mrb, dp->data);
}

/*
 * Executes a function under exception protection.
 *
 * @param mrb    mruby state
 * @param body   Function to execute under protection
 * @param data   Data to pass to the function
 * @param state  Pointer to store exception state (true if exception occurred)
 * @return       Return value from body function, or exception object if error occurred
 *
 * This function provides a C API equivalent to Ruby's begin/rescue blocks.
 * If an exception occurs during execution of body, it will be caught and
 * the exception object returned, with *state set to true.
 */
MRB_API mrb_value
mrb_protect(mrb_state *mrb, mrb_func_t body, mrb_value data, mrb_bool *state)
{
  struct protect_data protect_data = { body, data };
  return mrb_protect_error(mrb, protect_body, &protect_data, state);
}

/*
 * Executes a function with guaranteed cleanup (ensure block).
 *
 * @param mrb      mruby state
 * @param body     Main function to execute
 * @param b_data   Data to pass to body function
 * @param ensure   Cleanup function that always executes
 * @param e_data   Data to pass to ensure function
 * @return         Return value from body function
 *
 * This function provides a C API equivalent to Ruby's begin/ensure blocks.
 * The ensure function is guaranteed to execute regardless of whether the
 * body function completes normally or raises an exception. If an exception
 * occurs in the body, it will be re-raised after the ensure block executes.
 */
MRB_API mrb_value
mrb_ensure(mrb_state *mrb, mrb_func_t body, mrb_value b_data, mrb_func_t ensure, mrb_value e_data)
{
  int ai = mrb_gc_arena_save(mrb);
  struct protect_data protect_data = { body, b_data };
  mrb_bool error;
  mrb_value result = mrb_protect_error(mrb, protect_body, &protect_data, &error);
  ensure(mrb, e_data);
  mrb_gc_arena_restore(mrb, ai);
  mrb_gc_protect(mrb, result);
  if (error) {
    mrb_exc_raise(mrb, result); /* rethrow caught exceptions */
  }
  return result;
}

/*
 * Executes a function with exception handling for StandardError and its subclasses.
 *
 * @param mrb      mruby state
 * @param body     Main function to execute
 * @param b_data   Data to pass to body function
 * @param rescue   Exception handler function
 * @param r_data   Data to pass to rescue function
 * @return         Return value from body function, or rescue function if StandardError occurred
 *
 * This function provides a C API equivalent to Ruby's begin/rescue blocks that
 * catch StandardError. It's a convenience wrapper around mrb_rescue_exceptions
 * that automatically handles StandardError and its subclasses.
 */
MRB_API mrb_value
mrb_rescue(mrb_state *mrb, mrb_func_t body, mrb_value b_data,
           mrb_func_t rescue, mrb_value r_data)
{
  return mrb_rescue_exceptions(mrb, body, b_data, rescue, r_data, 1, &mrb->eStandardError_class);
}

/*
 * Executes a function with exception handling for specific exception classes.
 *
 * @param mrb      mruby state
 * @param body     Main function to execute
 * @param b_data   Data to pass to body function
 * @param rescue   Exception handler function
 * @param r_data   Data to pass to rescue function
 * @param len      Number of exception classes to handle
 * @param classes  Array of exception classes to catch
 * @return         Return value from body function, or rescue function if matching exception occurred
 *
 * This function provides a C API equivalent to Ruby's begin/rescue blocks with
 * specific exception class handling. Only exceptions that are instances of the
 * specified classes will be caught; others will be re-raised.
 */
MRB_API mrb_value
mrb_rescue_exceptions(mrb_state *mrb, mrb_func_t body, mrb_value b_data, mrb_func_t rescue, mrb_value r_data,
                      mrb_int len, struct RClass **classes)
{
  int ai = mrb_gc_arena_save(mrb);
  struct protect_data protect_data = { body, b_data };
  mrb_bool error;
  mrb_value result = mrb_protect_error(mrb, protect_body, &protect_data, &error);
  if (error) {
    mrb_bool error_matched = FALSE;
    for (mrb_int i = 0; i < len; i++) {
      if (mrb_obj_is_kind_of(mrb, result, classes[i])) {
        error_matched = TRUE;
        break;
      }
    }

    if (!error_matched) { mrb_exc_raise(mrb, result); }

    mrb->exc = NULL;
    result = rescue(mrb, r_data);
    mrb_gc_arena_restore(mrb, ai);
    mrb_gc_protect(mrb, result);
  }
  return result;
}

void
mrb_mruby_error_gem_init(mrb_state *mrb)
{
}

void
mrb_mruby_error_gem_final(mrb_state *mrb)
{
}
