#include "mruby/panic.h"

#ifndef MRB_PANIC_ABORT
#include <stdlib.h>
#define MRB_PANIC_ABORT() abort()
#endif

static mrb_panic_hook panic_global_hook = NULL;

mrb_panic_hook
mrb_panic_get_global_hook(void)
{
  return panic_global_hook;
}

#define PANIC_SET(store) do {\
    if ((store) == NULL) {\
      (store) = (func);\
    }\
    else if ((store) == func) {\
      /* nothing to do. */\
    }\
    else {\
      if (mrb) {\
        mrb_raise(mrb, E_ARGUMENT_ERROR,\
                  "Panic hook is already defined. Reset before redefine.");\
      }\
    }\
  } while(0)

void
mrb_panic_set(mrb_state *mrb, mrb_panic_hook func)
{
  if (mrb) {
    /* state local */
    PANIC_SET(mrb->panic_hook);
  }
  else {
    /* system global */
    PANIC_SET(panic_global_hook);
  }
}

#define PANIC_RESET(hook) do {\
    if ((hook) == func) {\
      (hook) = NULL;\
    }\
    else {\
      if (mrb) {\
        mrb_raise(mrb, E_ARGUMENT_ERROR, "Can't reset panic hook. mrb_panic_hook mismatch.");\
      }\
    }\
  } while(0);

void
mrb_panic_reset(mrb_state *mrb, mrb_panic_hook func)
{
  if (mrb) {
    /* state local */
    PANIC_RESET(mrb->panic_hook);
  }
  else {
    /* system global */
    PANIC_RESET(panic_global_hook);
  }
}

static void
mrb_panic_default(mrb_state *mrb)
{
  MRB_PANIC_ABORT();
}


void
mrb_panic(mrb_state *mrb)
{
  mrb_panic_hook hook;

  if (mrb) {
    hook = mrb->panic_hook;
  }
  else {
    hook = panic_global_hook;
  }

  if (hook) {
    hook(mrb);
  }
  else {
    mrb_panic_default(mrb);
  }
}
