/*
** signal.c - Signal module
**
** See Copyright Notice in mruby.h
**
** The common half of mruby-signal: the two questions Ruby asks about a
** signal name, answered over the platform table in signal_hal.h.  Which
** names a host has is the port's business; that `EXIT` is one of them, and
** that a number naming no signal answers nil rather than raising, is Ruby's
** and is settled here.
*/

#include <mruby.h>
#include <mruby/hash.h>
#include <mruby/string.h>
#include "signal_hal.h"

/*
 * Signal 0, and the name Ruby gives it.
 *
 * No platform has a signal numbered 0: `kill(2)` spends it on asking whether
 * a process can be signalled at all, and Ruby spends the name on the handler
 * that runs at exit.  It is a name Ruby adds rather than one a host reports,
 * so it is added here, where every port gets it alike.
 */
#define SIGNAL_EXIT_NAME "EXIT"
#define SIGNAL_EXIT_NUMBER 0

/*
 * call-seq:
 *   Signal.signame(signo) -> string or nil
 *
 * The bare name this platform gives signal +signo+, without the "SIG"
 * prefix, or nil where the number names no signal here.
 *
 *   Signal.signame(9)   # => "KILL"
 *   Signal.signame(0)   # => "EXIT"
 *   Signal.signame(999) # => nil
 *
 * Where a host spells one signal two ways, the name Ruby lists first is the
 * one that comes back: signal 6 is "ABRT" rather than "IOT".
 */
static mrb_value
signal_s_signame(mrb_state *mrb, mrb_value self)
{
  mrb_int signo;
  const char *name;

  mrb_get_args(mrb, "i", &signo);
  if (signo == SIGNAL_EXIT_NUMBER) {
    return mrb_str_new_lit(mrb, SIGNAL_EXIT_NAME);
  }
  /* Nothing is narrowed on the way to the table, where the comparison is in
     mrb_int, so a number too large for a signal simply matches no entry and
     comes back nil, as it does in CRuby. */
  name = mrb_hal_signal_name(mrb, signo);
  if (name == NULL) return mrb_nil_value();
  return mrb_str_new_cstr(mrb, name);
}

/*
 * call-seq:
 *   Signal.list -> hash
 *
 * Every signal name this platform knows, mapped to its number.  A signal a
 * host spells two ways appears under each spelling:
 *
 *   Signal.list["ABRT"] # => 6
 *   Signal.list["IOT"]  # => 6
 *
 * The hash is built fresh each call, so changing it changes nothing.
 */
static mrb_value
signal_s_list(mrb_state *mrb, mrb_value self)
{
  mrb_int count = mrb_hal_signal_count(mrb);
  mrb_value list = mrb_hash_new_capa(mrb, count + 1);
  int ai = mrb_gc_arena_save(mrb);
  mrb_int i;

  mrb_hash_set(mrb, list, mrb_str_new_lit(mrb, SIGNAL_EXIT_NAME),
               mrb_int_value(mrb, SIGNAL_EXIT_NUMBER));
  mrb_gc_arena_restore(mrb, ai);

  for (i = 0; i < count; i++) {
    mrb_int signo;
    const char *name = mrb_hal_signal_at(mrb, i, &signo);

    if (name == NULL) break;
    mrb_hash_set(mrb, list, mrb_str_new_cstr(mrb, name), mrb_int_value(mrb, signo));
    mrb_gc_arena_restore(mrb, ai);
  }
  return list;
}

void
mrb_mruby_signal_gem_init(mrb_state *mrb)
{
  struct RClass *signal = mrb_define_module_id(mrb, MRB_SYM(Signal));

  mrb_define_module_function_id(mrb, signal, MRB_SYM(signame), signal_s_signame, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, signal, MRB_SYM(list),    signal_s_list,    MRB_ARGS_NONE());
}

void
mrb_mruby_signal_gem_final(mrb_state *mrb)
{
}
