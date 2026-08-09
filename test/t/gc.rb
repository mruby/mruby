# Not ISO specified

assert('GC.enable') do
  assert_false GC.disable
  assert_true GC.enable
  assert_false GC.enable
end

assert('GC.disable') do
  begin
    assert_false GC.disable
    assert_true GC.disable
  ensure
    GC.enable
  end
end

assert('GC.interval_ratio=') do
  origin = GC.interval_ratio
  begin
    assert_equal 150, (GC.interval_ratio = 150)
  ensure
    GC.interval_ratio = origin
  end
end

assert('GC.step_ratio=') do
  origin = GC.step_ratio
  begin
    assert_equal 150, (GC.step_ratio = 150)
    assert_raise(ArgumentError) { GC.step_ratio = 0 }
    assert_raise(ArgumentError) { GC.step_ratio = -1 }
  ensure
    GC.step_ratio = origin
  end
end

assert('GC.step_limit=') do
  origin = GC.step_limit
  begin
    assert_equal 0, origin           # default: unlimited
    assert_equal 512, (GC.step_limit = 512)
    assert_equal 512, GC.step_limit
    assert_equal 0, (GC.step_limit = 0)  # back to unlimited
  ensure
    GC.step_limit = origin
  end
end

assert('GC.step_limit - GC completes with small limit') do
  origin = GC.step_limit
  begin
    GC.step_limit = 64
    # GC should still complete even with a small step limit
    GC.start
    assert_true GC.stat[:live] > 0
  ensure
    GC.step_limit = origin
  end
end

assert('GC.malloc_threshold=') do
  origin = GC.malloc_threshold
  begin
    assert_equal 0, origin           # default: disabled
    assert_equal 65536, (GC.malloc_threshold = 65536)
    assert_equal 65536, GC.malloc_threshold
    assert_equal 0, (GC.malloc_threshold = 0)  # back to disabled
  ensure
    GC.malloc_threshold = origin
  end
end

assert('GC.malloc_threshold - triggers GC on large allocations') do
  origin = GC.malloc_threshold
  begin
    GC.malloc_threshold = 4096
    GC.start  # force a full GC cycle
    # allocate large strings to exceed threshold
    100.times { "x" * 1024 }
    stat = GC.stat
    assert_true stat[:malloc_increase] >= 0
  ensure
    GC.malloc_threshold = origin
  end
end

assert('GC.malloc_threshold - does not mark through stale realloc buffers') do
  origin = GC.malloc_threshold
  begin
    GC.malloc_threshold = 1
    GC.start  # reset malloc_increase

    h = {}
    300.times { |i| h[i] = i }
    assert_equal 300, h.size
    300.times { |i| assert_equal i, h[i] }

    a = []
    1000.times { |i| a << i }
    assert_equal 1000, a.size
    1000.times { |i| assert_equal i, a[i] }
  ensure
    GC.malloc_threshold = origin
  end
end

assert('GC.generational_mode=') do
  origin = GC.generational_mode
  begin
    assert_false (GC.generational_mode = false)
    assert_true (GC.generational_mode = true)
    assert_true (GC.generational_mode = true)
  ensure
    GC.generational_mode = origin
  end
end

# The inline `[]`, `[]=` and arithmetic opcodes answer from C without a method
# call, so the arena restore that every cfunc return performs never runs for
# them.  What they allocate then stays arena-protected for the rest of the
# enclosing method.  `GC.stat` is itself a cfunc, so it reads the count before
# its own restore and still sees what the loop pinned.  The loop bodies below
# are built only from opcodes that do not restore, since a single send in the
# body would empty the arena and hide the retention.

assert('OP_GETIDX does not retain its result in the GC arena') do
  s = "hello"
  GC.start
  base = GC.stat[:live]
  i = 0
  while i < 20000
    s[1]
    i += 1
  end
  assert_operator GC.stat[:live] - base, :<, 5000
end

assert('OP_GETIDX does not retain a Hash default in the GC arena') do
  h = Hash.new { Object.new }
  GC.start
  base = GC.stat[:live]
  i = 0
  while i < 20000
    h[1]
    i += 1
  end
  assert_operator GC.stat[:live] - base, :<, 5000
end

assert('OP_GETIDX0 does not retain a String result in the GC arena') do
  s = "hello"
  GC.start
  base = GC.stat[:live]
  i = 0
  while i < 20000
    s[0]
    i += 1
  end
  assert_operator GC.stat[:live] - base, :<, 5000
end

assert('OP_GETIDX0 does not retain a Hash default in the GC arena') do
  h = Hash.new { Object.new }
  GC.start
  base = GC.stat[:live]
  i = 0
  while i < 20000
    h[0]
    i += 1
  end
  assert_operator GC.stat[:live] - base, :<, 5000
end

assert('OP_SETIDX does not retain a duplicated Hash key in the GC arena') do
  h = {}
  k = "a"
  GC.start
  base = GC.stat[:live]
  i = 0
  while i < 20000
    h[k] = 1
    i += 1
  end
  assert_operator GC.stat[:live] - base, :<, 5000
end

assert('OP_ADD does not retain an overflowed Integer in the GC arena') do
  # The overflow branch promotes to a big integer, so it only exists with
  # mruby-bigint.  The shift count is a variable because a constant shift is
  # folded at compile time, and a folded result out of mrb_int range makes the
  # build fail rather than raise.
  begin
    k = 62
    x = 1 << k
    x + x
  rescue RangeError
    skip "requires mruby-bigint"
  end
  # 1 << 30 overflows mrb_int on MRB_INT32 and 1 << 62 on MRB_INT64, so
  # whichever width this build has, one of the two takes the overflow branch.
  [30, 62].each do |shift|
    x = 1 << shift
    GC.start
    base = GC.stat[:live]
    i = 0
    while i < 20000
      x + x
      i += 1
    end
    assert_operator GC.stat[:live] - base, :<, 5000
  end
end
