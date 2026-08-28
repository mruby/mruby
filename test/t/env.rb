# The shape of a closed env's heap stack (mruby/internal.h).
#
# A closed env carries the special-variable slot past its locals only when
# its flag says so. `struct REnv`, `MRB_ENV_CLOSE()` and
# `MRB_ENV_SET_LEN()` are public, so out-of-tree code builds closed envs
# over a stack of exactly `MRB_ENV_LEN()` values; reading one past the
# locals of such an env runs off the allocation. The C helpers are in
# mrbgems/mruby-test/env.c: `__env_make_legacy` shrinks a real closed env
# into that shape for the tests below, and the reads and writes go through
# `mrb_vm_svar_get()` / `mrb_vm_svar_set()`, which resolve the owning scope
# the way `$~` does.

def env_capture(&blk)
  blk
end

# A block written in a method closes over that method's env, and the env is
# closed on the return, so what comes back is a proc over a closed env that
# carries the slot.
def env_closed_proc
  local = :held
  env_capture { [local, __env_svar_read] }
end

def env_closed_writer(v)
  local = :held
  env_capture { __env_svar_write(v); __env_svar_read }
end

# Two blocks over one env: what one writes, the other reads, because both
# resolve to the scope they were written in.
def env_closed_pair
  local = :held
  [env_capture { __env_svar_write("shared") }, env_capture { __env_svar_read }]
end

assert('REnv, closed env without the special-variable slot') do
  pr = env_closed_proc
  assert_true __env_svar?(pr)
  assert_equal :nil, __env_svar_slot(pr)

  assert_true __env_make_legacy(pr)
  assert_false __env_svar?(pr)
  assert_equal :none, __env_svar_slot(pr)

  # A collection over the shrunken env must not read past its locals.
  GC.start
  assert_equal [:held, nil], pr.call
end

assert('REnv, C closure env carries no slot') do
  pr = __env_cfunc_proc("cfunc")
  assert_false __env_svar?(pr)
  assert_equal :none, __env_svar_slot(pr)

  GC.start
  assert_equal "cfunc", pr.call
end

assert('REnv, reading special variables leaves a slotless env alone') do
  pr = env_closed_proc
  assert_true __env_make_legacy(pr)

  assert_equal [:held, nil], pr.call
  assert_false __env_svar?(pr)
  assert_equal :none, __env_svar_slot(pr)
end

assert('REnv, the first write grows a slotless env into the slot') do
  pr = env_closed_writer("written")
  len = __env_len(pr)
  assert_true __env_make_legacy(pr)

  assert_equal "written", pr.call
  assert_true __env_svar?(pr)
  assert_equal :svar, __env_svar_slot(pr)
  # the locals are still there, and the slot went past them
  assert_equal len, __env_len(pr)
  assert_equal "written", pr.call

  # The container is reachable through the env alone; a collection must
  # find it in the slot the write made.
  GC.start
  assert_equal "written", pr.call
  assert_equal :svar, __env_svar_slot(pr)
end

assert('REnv, a grown env is the one scope both its procs see') do
  writer, reader = env_closed_pair
  assert_true __env_make_legacy(writer)
  assert_false __env_svar?(reader)

  writer.call
  assert_true __env_svar?(reader)
  GC.start
  assert_equal "shared", reader.call
end

assert('REnv, the shutdown detach keeps the top-level container') do
  # `mrb_close()` detaches the top-level frame's env before it runs the
  # atexit callbacks, and carries the frame's special-variable container
  # into the escaped env. The frame is the container's only root until that
  # store, and the detach allocates on the way. The probe in
  # mrbgems/mruby-test/env.c runs that teardown in a state of its own and
  # answers what the escaped env's slot ended up holding; it answers nil
  # where this build puts no collection in the window.
  kept = __env_shutdown_svar
  skip 'the teardown window opens under MRB_GC_STRESS with a compiler' if kept.nil?
  assert_true kept
end

assert('REnv, MRB_ENV_SET_BIDX() evaluates its index exactly once') do
  # Unrelated to the slot above: mruby/proc.h documents the macro as
  # public, so an idx argument with a side effect must run exactly once
  # regardless of MRB_DEBUG (mrbgems/mruby-test/env.c).
  assert_equal 1, __env_set_bidx_eval_count
end
