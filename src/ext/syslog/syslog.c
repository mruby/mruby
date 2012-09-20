/*
** syslog.c - Syslog module
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/string.h"
#include <string.h>
#include <syslog.h>

#ifdef ENABLE_SYSLOG

static const char *syslog_ident = NULL;
static int syslog_options = -1, syslog_facility = -1, syslog_mask = -1;
static int syslog_opened = 0;

mrb_value
mrb_f_syslog_open(mrb_state *mrb, mrb_value self)
{
  mrb_value ident, opt, fac;

  ident = opt = fac = mrb_nil_value();

  mrb_get_args(mrb, "o|oo", &ident, &opt, &fac);
  if (mrb_nil_p(ident)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no log message supplied");
  }

  if (syslog_opened) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "syslog already open");
  }

  syslog_ident = mrb_string_value_cstr(mrb, &ident);

  if (mrb_nil_p(opt)) {
    syslog_options = LOG_PID | LOG_CONS;
  } else {
    syslog_options = mrb_fixnum(opt);
  }

  if (mrb_nil_p(fac)) {
    syslog_facility = LOG_USER;
  } else {
    syslog_facility = mrb_fixnum(fac);
  }

  openlog(syslog_ident, syslog_options, syslog_facility);

  syslog_opened = 1;

  setlogmask(syslog_mask = setlogmask(0));

  return self;
}

mrb_value
mrb_f_syslog_log(mrb_state *mrb, mrb_value self)
{
  mrb_value pri, str, *argv;
  int argc;

  mrb_get_args(mrb, "o*", &pri, &argv, &argc);
  if (argc < 1) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (%d for 2+)", argc+1);
  }

  if (!syslog_opened) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "must open syslog before write");
  }

  str = mrb_str_format(mrb, argc-1, argv+1, argv[0]);
  syslog(mrb_fixnum(pri), "%s", RSTRING_PTR(str)); 

  return self;
}

mrb_value
mrb_f_syslog_close(mrb_state *mrb, mrb_value self)
{
  if (!syslog_opened) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "syslog not opened");
  }

  closelog();

  syslog_ident = NULL;
  syslog_options = syslog_facility = syslog_mask = -1;
  syslog_opened = 0;

  return mrb_nil_value();
}

mrb_value
mrb_f_syslog_isopen(mrb_state *mrb, mrb_value self)
{
  return syslog_opened ? mrb_true_value() : mrb_false_value();
}

mrb_value
mrb_f_syslog_ident(mrb_state *mrb, mrb_value self)
{
  return syslog_opened ? mrb_str_new(mrb, syslog_ident, strlen(syslog_ident)) : mrb_nil_value();
}

mrb_value
mrb_f_syslog_options(mrb_state *mrb, mrb_value self)
{
  return syslog_opened ? mrb_fixnum_value(syslog_options) : mrb_nil_value();
}

mrb_value
mrb_f_syslog_facility(mrb_state *mrb, mrb_value self)
{
  return syslog_opened ? mrb_fixnum_value(syslog_facility) : mrb_nil_value();
}

void
mrb_init_syslog(mrb_state *mrb)
{
  struct RClass *slog;

  slog = mrb_define_module(mrb, "Syslog");

  mrb_define_module_function(mrb, slog, "open", mrb_f_syslog_open, ARGS_ANY());
  mrb_define_module_function(mrb, slog, "log", mrb_f_syslog_log, ARGS_ANY());
  mrb_define_module_function(mrb, slog, "close", mrb_f_syslog_close, ARGS_NONE());
  mrb_define_module_function(mrb, slog, "opened?", mrb_f_syslog_isopen, ARGS_NONE());

  mrb_define_module_function(mrb, slog, "ident", mrb_f_syslog_ident, ARGS_NONE());
  mrb_define_module_function(mrb, slog, "options", mrb_f_syslog_options, ARGS_NONE());
  mrb_define_module_function(mrb, slog, "facility", mrb_f_syslog_facility, ARGS_NONE());

  /* Syslog options */
#define mrb_define_syslog_option(c) \
    mrb_define_const(mrb, slog, #c, mrb_fixnum_value(c))

#ifdef LOG_PID
  mrb_define_syslog_option(LOG_PID);
#endif
#ifdef LOG_CONS
  mrb_define_syslog_option(LOG_CONS);
#endif
#ifdef LOG_ODELAY
  mrb_define_syslog_option(LOG_ODELAY); /* deprecated */
#endif
#ifdef LOG_NDELAY
  mrb_define_syslog_option(LOG_NDELAY);
#endif
#ifdef LOG_NOWAIT
  mrb_define_syslog_option(LOG_NOWAIT); /* deprecated */
#endif
#ifdef LOG_PERROR
  mrb_define_syslog_option(LOG_PERROR);
#endif

  /* Syslog facilities */
#define mrb_define_syslog_facility(c) \
  mrb_define_const(mrb, slog, #c, mrb_fixnum_value(c))

#ifdef LOG_AUTH
  mrb_define_syslog_facility(LOG_AUTH);
#endif
#ifdef LOG_AUTHPRIV
  mrb_define_syslog_facility(LOG_AUTHPRIV);
#endif
#ifdef LOG_CONSOLE
  mrb_define_syslog_facility(LOG_CONSOLE);
#endif
#ifdef LOG_CRON
  mrb_define_syslog_facility(LOG_CRON);
#endif
#ifdef LOG_DAEMON
  mrb_define_syslog_facility(LOG_DAEMON);
#endif
#ifdef LOG_FTP
  mrb_define_syslog_facility(LOG_FTP);
#endif
#ifdef LOG_KERN
  mrb_define_syslog_facility(LOG_KERN);
#endif
#ifdef LOG_LPR
  mrb_define_syslog_facility(LOG_LPR);
#endif
#ifdef LOG_MAIL
  mrb_define_syslog_facility(LOG_MAIL);
#endif
#ifdef LOG_NEWS
  mrb_define_syslog_facility(LOG_NEWS);
#endif
#ifdef LOG_NTP
   mrb_define_syslog_facility(LOG_NTP);
#endif
#ifdef LOG_SECURITY
  mrb_define_syslog_facility(LOG_SECURITY);
#endif
#ifdef LOG_SYSLOG
  mrb_define_syslog_facility(LOG_SYSLOG);
#endif
#ifdef LOG_USER
  mrb_define_syslog_facility(LOG_USER);
#endif
#ifdef LOG_UUCP
  mrb_define_syslog_facility(LOG_UUCP);
#endif
#ifdef LOG_LOCAL0
  mrb_define_syslog_facility(LOG_LOCAL0);
#endif
#ifdef LOG_LOCAL1
  mrb_define_syslog_facility(LOG_LOCAL1);
#endif
#ifdef LOG_LOCAL2
  mrb_define_syslog_facility(LOG_LOCAL2);
#endif
#ifdef LOG_LOCAL3
  mrb_define_syslog_facility(LOG_LOCAL3);
#endif
#ifdef LOG_LOCAL4
  mrb_define_syslog_facility(LOG_LOCAL4);
#endif
#ifdef LOG_LOCAL5
  mrb_define_syslog_facility(LOG_LOCAL5);
#endif
#ifdef LOG_LOCAL6
  mrb_define_syslog_facility(LOG_LOCAL6);
#endif
#ifdef LOG_LOCAL7
  mrb_define_syslog_facility(LOG_LOCAL7);
#endif

  /* Syslog levels and the shortcut methods */
#define mrb_define_syslog_level(c, m) \
  mrb_define_const(mrb, slog, #c, mrb_fixnum_value(c));

#ifdef LOG_EMERG
  mrb_define_syslog_level(LOG_EMERG, emerg);
#endif
#ifdef LOG_ALERT
  mrb_define_syslog_level(LOG_ALERT, alert);
#endif
#ifdef LOG_CRIT
  mrb_define_syslog_level(LOG_CRIT, crit);
#endif
#ifdef LOG_ERR
  mrb_define_syslog_level(LOG_ERR, err);
#endif
#ifdef LOG_WARNING
  mrb_define_syslog_level(LOG_WARNING, warning);
#endif
#ifdef LOG_NOTICE
  mrb_define_syslog_level(LOG_NOTICE, notice);
#endif
#ifdef LOG_INFO
  mrb_define_syslog_level(LOG_INFO, info);
#endif
#ifdef LOG_DEBUG
  mrb_define_syslog_level(LOG_DEBUG, debug);
#endif
}
#endif /* ENABLE_SYSLOG */
