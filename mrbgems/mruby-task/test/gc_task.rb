# Scheduler-driven GC (GC.scheduler_driven, driven from task.c idle points)

assert("GC.scheduler_driven toggles and forces generational mode off") do
  origin_gen = GC.generational_mode
  begin
    GC.generational_mode = true
    GC.scheduler_driven = true
    assert_true GC.scheduler_driven
    assert_false GC.generational_mode  # forced off (a minor cycle is one step)

    GC.scheduler_driven = false
    assert_false GC.scheduler_driven
  ensure
    GC.scheduler_driven = false
    GC.generational_mode = origin_gen
    GC.start
  end
end

assert("GC.scheduler_driven= raises while GC disabled, flags unchanged") do
  origin_gen = GC.generational_mode
  begin
    GC.generational_mode = true
    GC.disable
    assert_raise(RuntimeError) { GC.scheduler_driven = true }
    GC.enable
    assert_false GC.scheduler_driven
    assert_true GC.generational_mode

    # Raises even when generational mode is already off: the refusal is part
    # of the enable contract itself, not a side effect of the mode switch.
    GC.generational_mode = false
    GC.disable
    assert_raise(RuntimeError) { GC.scheduler_driven = true }
    GC.enable
    assert_false GC.scheduler_driven
  ensure
    GC.enable
    GC.scheduler_driven = false
    GC.generational_mode = origin_gen
    GC.start
  end
end

assert("GC.generational_mode= refuses to re-enable while scheduler-driven") do
  origin_gen = GC.generational_mode
  begin
    GC.scheduler_driven = true
    # A generational minor cycle completes inside one atomic step, which would
    # silently defeat scheduler-driven stepping -- so this must raise.
    assert_raise(RuntimeError) { GC.generational_mode = true }
    assert_false GC.generational_mode
    assert_true GC.scheduler_driven

    GC.scheduler_driven = false
    GC.generational_mode = true   # fine again once the mode is off
    assert_true GC.generational_mode
  ensure
    GC.scheduler_driven = false
    GC.generational_mode = origin_gen
    GC.start
  end
end

assert('GC.debt_limit= validates and round-trips') do
  origin = GC.debt_limit
  begin
    assert_equal 50000, (GC.debt_limit = 50000)
    assert_equal 50000, GC.debt_limit
    assert_raise(ArgumentError) { GC.debt_limit = -1 }
    assert_equal 0, (GC.debt_limit = 0)
  ensure
    GC.debt_limit = origin
  end
end

assert("GC.scheduler_driven collects during scheduler idle") do
  origin_gen = GC.generational_mode
  origin_interval = GC.interval_ratio
  begin
    GC.generational_mode = false
    GC.start
    base = GC.stat[:live]

    GC.step_limit = 256
    # Smallest collection credit (clamps to GC_STEP_SIZE), so idle stepping
    # kicks in after ~1k allocations instead of a fraction of the (large,
    # mrbtest-inflated) live set -- keeps the workload below deterministic.
    GC.interval_ratio = 100
    GC.scheduler_driven = true
    steps_before = GC.stat[:prof_step_count]  # nil unless MRB_GC_PROFILE

    # Allocate garbage, then sleep so the scheduler goes idle and drives GC.
    Task.new(name: "alloc") do
      30.times do
        400.times { "x" * 8 }
        sleep_ms 1
      end
    end
    Task.run   # returns once the alloc task is DORMANT (all queues empty)

    # Sample BEFORE any GC.start: a full GC reclaims garbage regardless of
    # scheduler_driven, so only a pre-GC.start reading can prove the idle
    # steps did the collecting. The task churned >=24000 garbage strings
    # (literal + product per iteration) with allocation-path GC off; staying
    # well below that means scheduler-driven idle steps reclaimed them.
    live_after_run = GC.stat[:live]
    assert_true live_after_run < base + 6000,
                "scheduler-driven GC did not reclaim during idle " \
                "(#{base} -> #{live_after_run})"
    if steps_before
      assert_true GC.stat[:prof_step_count] > steps_before,
                  "no scheduler-driven GC step ran"
    end
  ensure
    GC.scheduler_driven = false
    GC.step_limit = 0
    GC.interval_ratio = origin_interval
    GC.generational_mode = origin_gen
    GC.start
  end
end
