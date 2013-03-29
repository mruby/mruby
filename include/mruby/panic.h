#ifndef MRB_PANIC_H__
#define MRB_PANIC_H__

#include "mruby.h"

extern mrb_panic_hook mrb_panic_get_global_hook(void);

extern void mrb_panic_set(mrb_state *mrb, mrb_panic_hook func);
extern void mrb_panic_reset(mrb_state *mrb, mrb_panic_hook func);
extern void mrb_panic(mrb_state *mrb);

#endif
