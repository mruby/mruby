/*
** process.c - 
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/array.h"
#include "mruby/class.h"
#include "mruby/string.h"
#include "error.h"

#include <sys/types.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#ifdef ENABLE_PROCESS
mrb_value
mrb_f_kill(mrb_state *mrb, mrb_value klass)
{
  mrb_int pid;
  mrb_value *argv, sigo;
  int argc, sent, signo = 0;

  mrb_get_args(mrb, "oi*", &sigo, &pid, &argv, &argc);
  if (mrb_fixnum_p(sigo)) {
    signo = mrb_fixnum(sigo);
  } else {
    mrb_raisef(mrb, E_TYPE_ERROR, "bad signal type %s",
    	       mrb_obj_classname(mrb, sigo));
  }

  sent = 0;
  if (kill(pid, signo) == -1)
    mrb_sys_fail(mrb, "kill");
  sent++;

  while (argc-- > 0) {
    if (!mrb_fixnum_p(*argv)) {
      mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected Fixnum)",
      	         mrb_obj_classname(mrb, *argv));
    }
    if (kill(mrb_fixnum(*argv), signo) == -1)
      mrb_sys_fail(mrb, "kill");
    sent++;
    argv++;
  }
  return mrb_fixnum_value(sent);
}

mrb_value
mrb_f_sleep(mrb_state *mrb, mrb_value klass)
{
  int argc;
  mrb_value *argv;
  time_t beg, end;

  beg = time(0);
  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0) {
    sleep((32767<<16)+32767);
  } else if(argc == 1) {
    struct timeval tv;
    int n;

    if (mrb_fixnum_p(argv[0])) {
      tv.tv_sec = mrb_fixnum(argv[0]);
      tv.tv_usec = 0;
    } else {
      tv.tv_sec = mrb_float(argv[0]);
      tv.tv_usec = (mrb_float(argv[0]) - tv.tv_sec) * 1000000.0;
    }


    n = select(0, 0, 0, 0, &tv);
    if (n < 0)
      mrb_sys_fail(mrb, "mrb_f_sleep failed");
  } else {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "wrong # of arguments");
  }

  end = time(0) - beg;

  return mrb_fixnum_value(end);
}

#define RETSIGTYPE void

mrb_value
mrb_f_system(mrb_state *mrb, mrb_value klass)
{
  int ret;
  mrb_value *argv, pname;
  const char *path;
  int argc;
  RETSIGTYPE (*chfunc)(int);

  fflush(stdout);
  fflush(stderr);

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "wrong number of arguments");
  }

  pname = argv[0];
#ifdef SIGCHLD
  chfunc = signal(SIGCHLD, SIG_DFL);
#endif
  path = mrb_string_value_cstr(mrb, &pname);
  ret = system(path);

  if (WIFEXITED(ret) && WEXITSTATUS(ret) == 0) {
    return mrb_true_value();
  }

  return mrb_false_value();
}

mrb_value
mrb_f_exit(mrb_state *mrb, mrb_value klass)
{
  mrb_value status;
  int istatus;

  mrb_get_args(mrb, "|o", &status);
  if (!mrb_nil_p(status)) {
    if (mrb_type(status) == MRB_TT_TRUE)
      istatus = EXIT_SUCCESS;
    else {
      istatus = EXIT_FAILURE;
    }
  } else {
    istatus = EXIT_SUCCESS;
  }

  exit(istatus);
}

mrb_value
mrb_f_pid(mrb_state *mrb, mrb_value klass)
{
  return mrb_fixnum_value((mrb_int)getpid());
}

mrb_value
mrb_f_ppid(mrb_state *mrb, mrb_value klass)
{
  return mrb_fixnum_value((mrb_int)getppid());
}

void
mrb_init_process(mrb_state *mrb)
{
  struct RClass *p;

  mrb_define_method(mrb, mrb->kernel_module, "exit", mrb_f_exit, ARGS_OPT(1));
  mrb_define_method(mrb, mrb->kernel_module, "sleep", mrb_f_sleep, ARGS_ANY());
  mrb_define_method(mrb, mrb->kernel_module, "system", mrb_f_system, ARGS_ANY());

  p = mrb_define_module(mrb, "Process");
  mrb_define_class_method(mrb, p, "kill", mrb_f_kill, ARGS_ANY());
  mrb_define_class_method(mrb, p, "pid", mrb_f_pid, ARGS_NONE());
  mrb_define_class_method(mrb, p, "ppid", mrb_f_ppid, ARGS_NONE());
}
#endif /* ENABLE_PROCESS */
