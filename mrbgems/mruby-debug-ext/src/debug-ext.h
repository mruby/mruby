#ifndef MRUBY_DEBUG_EXT_H
#define MRUBY_DEBUG_EXT_H

#include "mruby.h"

#ifndef ENABLE_STDIO
#error cannot use mruby-debug-ext without stdio
#endif

#include <stdio.h>

FILE *mrb_debug_output(mrb_state *mrb);

#endif
