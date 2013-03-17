#include "mruby/log.h"

/* Sets log printer.
 * You must call mrb_log_reset_printer() if the log printer was already set.
 * Or you will get ArgumentException.
 * return: 0 with success.
 *         -1 with fail.
 */
mrb_int
mrb_log_set_printer(mrb_state *mrb, enum mrb_log_level level, mrb_log_printer log_printer)
{
  mrb_int result;

  if (mrb->log_printer) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Already set log_printer. Reset before set.");
    result = -1;
  }
  else {
    mrb->log_printer[level] = log_printer;
    result = 0;
  }

  return result;
}

/* Resets log printer.
 * return: 0 with success.
 *         -1 with fail.
 */
mrb_int
mrb_log_reset_printer(mrb_state *mrb, enum mrb_log_level level)
{
  mrb->log_printer[level] = NULL;
  return 0;
}

/*
 * Embedded printers.
 */
#ifdef ENABLE_STDIO
# define log_fputs fputs
#else
# define stdout ((void *)0)
# define stderr ((void *)0)
inline void log_fputs(const char *fmt, void *p)
{
  /* dummy function */
}
#endif

static mrb_int
mrb_log_print_normal(const char *message)
{
  log_fputs(message, stdout);
  return 0;
}

static mrb_int
mrb_log_print_express(const char *message)
{
  log_fputs(message, stderr);
  return 0;
}



mrb_log_printer default_printer[] = {
  mrb_log_print_normal,
  mrb_log_print_express
};

/* Sets log default printer.
 * You must call mrb_log_reset_default_printer() before you call this.
 * This function doesn't set log_printer if default_printer is not resetted.
 * return: 0 with success.
 *         -1 with fail.
 */
mrb_int
mrb_log_set_default_printer(enum mrb_log_level level, mrb_log_printer log_printer)
{
  mrb_int result;

  if (!default_printer[level]) {
    default_printer[level] = log_printer;
    result = -1;
  }
  else {
    result = 0;
  }

  return result;
}

/* Resets log default printer.
 * return: 0 with success.
 *         -1 with fail.
 */
mrb_int
mrb_log_reset_default_printer(enum mrb_log_level level)
{
  default_printer[level] = NULL;

  return 0;
}

/* Prints to log priner.
 * return: 0 with success.
 *         -1 with fail.
 */
mrb_int
mrb_log_print(mrb_state *mrb, enum mrb_log_level level, const char *message)
{
  mrb_log_printer printer;
  mrb_int result;

  printer = mrb->log_printer[level];
  if (!printer) {
    printer = default_printer[level];
    if (!printer) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "Log printer not found.");
      result = -1;
    }
  }
  if (printer) {
    result = printer(message);
  }

  return result;
}

