/*
** status.c - Process::Status
**
** See Copyright Notice in mruby.h
**
** A Process::Status keeps only what the platform gave it: the pid and the
** raw wait status.  Every question about that status (exited?, termsig,
** coredump?) is answered by handing the raw value back to the HAL, so the
** Ruby side never grows its own idea of what the bits mean and a status
** built elsewhere reads exactly as one this gem reaped.
**
** That "built elsewhere" is the point of keeping `Process::Status.new(pid,
** raw_status)` a working construction path: mruby-io's `IO.popen` sets `$?`
** that way when this gem happens to be present, and it must keep working
** without either gem depending on the other.
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/variable.h>
#include <mruby/string.h>
#include "process_hal.h"
#include "process_internal.h"

/* Read one of the two integers a status is made of.  An instance that never
   reached #initialize has neither, and asking it anything is a mistake worth
   naming rather than reading past. */
static mrb_int
status_ivar(mrb_state *mrb, mrb_value self, mrb_sym name)
{
  mrb_value v = mrb_iv_get(mrb, self, name);

  if (!mrb_integer_p(v)) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Process::Status");
  }
  return mrb_integer(v);
}

static void
status_decode(mrb_state *mrb, mrb_value self, mrb_process_status *st)
{
  mrb_int pid = status_ivar(mrb, self, MRB_IVSYM(pid));
  mrb_int raw = status_ivar(mrb, self, MRB_IVSYM(status));

  mrb_hal_process_status_decode(mrb, pid, raw, st);
}

static mrb_value
status_flag(mrb_state *mrb, mrb_value self, unsigned int flag)
{
  mrb_process_status st;

  status_decode(mrb, self, &st);
  return mrb_bool_value((st.flags & flag) != 0);
}

/*
 * call-seq:
 *   Process::Status.new(pid, raw_status) -> status
 *
 * Wraps a platform wait status for the process +pid+.  +raw_status+ is the
 * value the platform reported the process with, as Process.waitpid passes
 * on and as Process::Status#to_i gives back.
 */
static mrb_value
status_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_int pid, raw_status;

  mrb_get_args(mrb, "ii", &pid, &raw_status);
  /* A port reads a raw status as the `int` the platform reported, so a value
     that does not fit one would be answered about from bits nobody wrote:
     #to_i would give back what was passed in while #exited? and the rest
     read the low half of it.  The pid needs no such check, since it is
     carried and handed back whole rather than narrowed. */
  mrb_process_int_arg(mrb, raw_status, "status");
  mrb_iv_set(mrb, self, MRB_IVSYM(pid), mrb_int_value(mrb, pid));
  mrb_iv_set(mrb, self, MRB_IVSYM(status), mrb_int_value(mrb, raw_status));
  return self;
}

/*
 * call-seq:
 *   status.pid -> integer
 *
 * The process ID this status came from.
 */
static mrb_value
status_pid(mrb_state *mrb, mrb_value self)
{
  return mrb_int_value(mrb, status_ivar(mrb, self, MRB_IVSYM(pid)));
}

/*
 * call-seq:
 *   status.to_i -> integer
 *
 * The platform status as it was reported, unread.  Its layout is the
 * platform's business, which is why nothing but the HAL takes it apart.
 */
static mrb_value
status_to_i(mrb_state *mrb, mrb_value self)
{
  return mrb_int_value(mrb, status_ivar(mrb, self, MRB_IVSYM(status)));
}

/*
 * call-seq:
 *   status.exited? -> true or false
 *
 * Whether the process ran to completion rather than being signalled.
 */
static mrb_value
status_exited_p(mrb_state *mrb, mrb_value self)
{
  return status_flag(mrb, self, MRB_PROCESS_STATUS_EXITED);
}

/*
 * call-seq:
 *   status.exitstatus -> integer or nil
 *
 * The status the process exited with, or nil if it did not exit.
 */
static mrb_value
status_exitstatus(mrb_state *mrb, mrb_value self)
{
  mrb_process_status st;

  status_decode(mrb, self, &st);
  if (!(st.flags & MRB_PROCESS_STATUS_EXITED)) return mrb_nil_value();
  return mrb_int_value(mrb, st.exitstatus);
}

/*
 * call-seq:
 *   status.signaled? -> true or false
 *
 * Whether an uncaught signal ended the process.
 */
static mrb_value
status_signaled_p(mrb_state *mrb, mrb_value self)
{
  return status_flag(mrb, self, MRB_PROCESS_STATUS_SIGNALED);
}

/*
 * call-seq:
 *   status.termsig -> integer or nil
 *
 * The signal that ended the process, or nil if none did.
 */
static mrb_value
status_termsig(mrb_state *mrb, mrb_value self)
{
  mrb_process_status st;

  status_decode(mrb, self, &st);
  if (!(st.flags & MRB_PROCESS_STATUS_SIGNALED)) return mrb_nil_value();
  return mrb_int_value(mrb, st.termsig);
}

/*
 * call-seq:
 *   status.stopped? -> true or false
 *
 * Whether the process is stopped rather than finished.  Only a wait made
 * with Process::WUNTRACED reports one.
 */
static mrb_value
status_stopped_p(mrb_state *mrb, mrb_value self)
{
  return status_flag(mrb, self, MRB_PROCESS_STATUS_STOPPED);
}

/*
 * call-seq:
 *   status.stopsig -> integer or nil
 *
 * The signal that stopped the process, or nil if it is not stopped.
 */
static mrb_value
status_stopsig(mrb_state *mrb, mrb_value self)
{
  mrb_process_status st;

  status_decode(mrb, self, &st);
  if (!(st.flags & MRB_PROCESS_STATUS_STOPPED)) return mrb_nil_value();
  return mrb_int_value(mrb, st.stopsig);
}

/*
 * call-seq:
 *   status.coredump? -> true or false
 *
 * Whether the signal that ended the process also dumped core.  Platforms
 * that cannot tell answer false.
 */
static mrb_value
status_coredump_p(mrb_state *mrb, mrb_value self)
{
  return status_flag(mrb, self, MRB_PROCESS_STATUS_COREDUMP);
}

/*
 * call-seq:
 *   status == other -> true or false
 *
 * Whether +other+ equals the raw status, which is what #to_i gives back.
 * The pid takes no part: two statuses holding the same platform value are
 * equal whichever processes they came from.
 */
static mrb_value
status_eq(mrb_state *mrb, mrb_value self)
{
  mrb_value raw = mrb_int_value(mrb, status_ivar(mrb, self, MRB_IVSYM(status)));
  mrb_value other;

  mrb_get_args(mrb, "o", &other);
  /* Ruby answers this question as `to_i == other`, and reaches a status on
     the right through Integer#== asking it back.  mrb_equal() answers false
     for anything that is not a number instead of asking, so a status is
     unwrapped here rather than left to it. */
  if (mrb_obj_class(mrb, self) == mrb_obj_class(mrb, other)) {
    other = mrb_int_value(mrb, status_ivar(mrb, other, MRB_IVSYM(status)));
  }
  return mrb_bool_value(mrb_equal(mrb, raw, other));
}

/*
 * call-seq:
 *   Process::Status._signame(signo) -> string or nil
 *
 * The bare name this platform gives signal +signo+, without the "SIG"
 * prefix, or nil where the number names no signal.  Process::Status#to_s
 * uses it to spell a signal out; it is not a signal API of its own.
 */
static mrb_value
status_s_signame(mrb_state *mrb, mrb_value self)
{
  mrb_int signo;
  const char *name;

  mrb_get_args(mrb, "i", &signo);
  name = mrb_hal_process_signal_name(mrb, signo);
  if (name == NULL) return mrb_nil_value();
  return mrb_str_new_cstr(mrb, name);
}

mrb_value
mrb_process_status_new(mrb_state *mrb, mrb_int pid, mrb_int raw_status)
{
  struct RClass *process = mrb_module_get_id(mrb, MRB_SYM(Process));
  struct RClass *status = mrb_class_get_under_id(mrb, process, MRB_SYM(Status));
  mrb_value argv[2];

  argv[0] = mrb_int_value(mrb, pid);
  argv[1] = mrb_int_value(mrb, raw_status);
  return mrb_obj_new(mrb, status, 2, argv);
}

void
mrb_process_status_init(mrb_state *mrb, struct RClass *process)
{
  struct RClass *status;

  status = mrb_define_class_under_id(mrb, process, MRB_SYM(Status), mrb->object_class);

  mrb_define_class_method_id(mrb, status, MRB_SYM(_signame), status_s_signame, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, status, MRB_SYM(initialize), status_initialize, MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, status, MRB_SYM(pid),        status_pid,        MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(to_i),       status_to_i,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM_Q(exited),   status_exited_p,   MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(exitstatus), status_exitstatus, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM_Q(signaled), status_signaled_p, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(termsig),    status_termsig,    MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM_Q(stopped),  status_stopped_p,  MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(stopsig),    status_stopsig,    MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM_Q(coredump), status_coredump_p, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_OPSYM(eq),       status_eq,         MRB_ARGS_REQ(1));
}
