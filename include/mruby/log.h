#ifndef MRB_LOG_H__
#define MRB_LOG_H__

#include "mruby.h"

mrb_int mrb_log_set_printer(mrb_state *mrb, enum mrb_log_level level, mrb_log_printer log_printer);
mrb_int mrb_log_reset_printer(mrb_state *mrb, enum mrb_log_level level);

mrb_int mrb_log_set_default_printer(enum mrb_log_level level, mrb_log_printer log_printer);
mrb_int mrb_log_reset_default_printer(enum mrb_log_level level);

mrb_int mrb_log_print(mrb_state *mrb, enum mrb_log_level level, const char *message);

#endif /* MRB_LOG_H__ */
