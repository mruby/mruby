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
** That "built elsewhere" is mruby-io: its `IO.popen` sets `$?` with a status
** it builds when this gem happens to be present, and that has to keep
** working without either gem depending on the other.  `Process::Status.new`
** is undefined here as it is in CRuby, so the seam is not a constructor but
** the allocate-and-#initialize pair `mrb_obj_new()` performs, which is also
** how Process.waitpid builds the status it publishes.
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/variable.h>
#include <mruby/numeric.h>
#include <mruby/string.h>
#include "process_hal.h"
#include "process_internal.h"
#include "signal_hal.h"

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

/* Process::Status itself, which is not always an instance's own class: a
   subclass of it is still one, and #== has to read one as such. */
static struct RClass *
status_class(mrb_state *mrb)
{
  struct RClass *process = mrb_module_get_id(mrb, MRB_SYM(Process));

  return mrb_class_get_under_id(mrb, process, MRB_SYM(Status));
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
 * Wraps a platform wait status for the process +pid+.  +raw_status+ is the
 * value the platform reported the process with, as Process.waitpid passes
 * on and as Process::Status#to_i gives back.
 *
 * Reached by allocating an instance and initializing it rather than through
 * `new`, which this class does not have.  Private, as mruby makes every
 * #initialize.
 *
 * A Process::Status is frozen once built, as CRuby freezes the one it leaves
 * in <code>$?</code>.  What a process did is over by the time there is a
 * status for it, and every question a status answers is read back from the
 * two integers set here.  An instance of a subclass is left unfrozen, since
 * whatever else it is made of is set after this returns.
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
  /* Last, since the two above are what there is to write.  A second
     #initialize on the same object is refused from here on, which is what
     freezing a value means and not a case this gem's own paths reach.

     Only when the object is a Process::Status and nothing more.  A subclass
     is still being built when this returns: its own #initialize called super
     to have the two set and goes on to set whatever else it is made of, and
     freezing here would turn that into a FrozenError.  Every status this gem
     and mruby-io publish through `$?` is of this exact class, so the ones
     that answer for a reaped process are the ones that are frozen.  CRuby's
     Range does the same, freezing in #initialize only what is a Range. */
  if (mrb_obj_class(mrb, self) == status_class(mrb)) {
    mrb_obj_freeze(mrb, self);
  }
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
     unwrapped here rather than left to it.  Being a status is what decides
     that, not being of this exact class: a subclass of Process::Status is
     one, and CRuby's way round reaches it through whichever #== the object
     carries. */
  if (mrb_obj_is_kind_of(mrb, other, status_class(mrb))) {
    other = mrb_int_value(mrb, status_ivar(mrb, other, MRB_IVSYM(status)));
  }
  return mrb_bool_value(mrb_equal(mrb, raw, other));
}

/*
 * Append a number to the description being built.
 *
 * CRuby writes this description into one buffer as it goes, and so does this.
 * mrb_format() would be shorter to read, but it builds a String per piece and
 * concatenates it, which costs an allocation for every number written; the
 * buffer below is large enough for any mrb_int in base 10, sign included, so
 * the conversion cannot fail.
 */
static void
status_cat_int(mrb_state *mrb, mrb_value str, mrb_int n)
{
  char buf[MRB_INT_BIT / 3 + 3];

  mrb_str_cat_cstr(mrb, str, mrb_int_to_cstr(buf, sizeof(buf), n, 10));
}

/*
 * Spell a signal out the way Ruby does: " SIGKILL (signal 9)", or " signal 9"
 * where this platform gives the number no name.  `lead` is what stands
 * between the pid and the signal, which is " stopped" for a process that
 * stopped and nothing for one that was killed.
 */
static void
status_cat_signal(mrb_state *mrb, mrb_value str, const char *lead, mrb_int signo)
{
  const char *name = mrb_hal_signal_name(mrb, signo);

  mrb_str_cat_cstr(mrb, str, lead);
  if (name == NULL) {
    mrb_str_cat_lit(mrb, str, " signal ");
    status_cat_int(mrb, str, signo);
    return;
  }
  mrb_str_cat_lit(mrb, str, " SIG");
  mrb_str_cat_cstr(mrb, str, name);
  mrb_str_cat_lit(mrb, str, " (signal ");
  status_cat_int(mrb, str, signo);
  mrb_str_cat_lit(mrb, str, ")");
}

/*
 * call-seq:
 *   status.to_s -> string
 *
 * A description of how the process finished:
 *
 *   pid 1234 exit 0
 *   pid 1234 SIGKILL (signal 9)
 *   pid 1234 SIGSEGV (signal 11) (core dumped)
 *   pid 1234 stopped SIGSTOP (signal 19)
 *
 * A status the platform said nothing about is just "pid 1234".
 */
static mrb_value
status_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_process_status st;
  mrb_value str;

  status_decode(mrb, self, &st);
  str = mrb_str_new_lit(mrb, "pid ");
  status_cat_int(mrb, str, st.pid);

  /* Each part is asked about on its own, as CRuby asks, rather than in an
     if/else chain: a port that reports two of them at once is then described
     twice over instead of having all but the first dropped. */
  if (st.flags & MRB_PROCESS_STATUS_STOPPED) {
    status_cat_signal(mrb, str, " stopped", st.stopsig);
  }
  if (st.flags & MRB_PROCESS_STATUS_SIGNALED) {
    status_cat_signal(mrb, str, "", st.termsig);
  }
  if (st.flags & MRB_PROCESS_STATUS_EXITED) {
    mrb_str_cat_lit(mrb, str, " exit ");
    status_cat_int(mrb, str, st.exitstatus);
  }
  if (st.flags & MRB_PROCESS_STATUS_COREDUMP) {
    mrb_str_cat_lit(mrb, str, " (core dumped)");
  }
  return str;
}

mrb_value
mrb_process_status_new(mrb_state *mrb, mrb_int pid, mrb_int raw_status)
{
  struct RClass *status = status_class(mrb);
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

  mrb_define_method_id(mrb, status, MRB_SYM(initialize), status_initialize, MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, status, MRB_SYM(pid),        status_pid,        MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(to_i),       status_to_i,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(to_s),       status_to_s,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM_Q(exited),   status_exited_p,   MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(exitstatus), status_exitstatus, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM_Q(signaled), status_signaled_p, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(termsig),    status_termsig,    MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM_Q(stopped),  status_stopped_p,  MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM(stopsig),    status_stopsig,    MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_SYM_Q(coredump), status_coredump_p, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, status, MRB_OPSYM(eq),       status_eq,         MRB_ARGS_REQ(1));

  /* A status reports something that happened, so one written by hand reports
     nothing: CRuby undefines `new` on the class for that reason, and the call
     raises there as it now does here.  What is left is #initialize, which
     mrb_obj_new() calls without asking for `new`: the path Process.waitpid
     takes, and the one mruby-io takes to set `$?`.  MRB_UNDEF_ALLOCATOR() is
     not set beside this, the way Data and Complex set it, because
     mrb_obj_new() allocates through it and marking it undefined would close
     that path too.  CRuby leaves its allocator alone as well; what it takes
     away is the constructor. */
  mrb_undef_class_method_id(mrb, status, MRB_SYM(new));
}
