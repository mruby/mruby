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
# enclosing method.  The loop bodies below are built only from opcodes that do
# not restore, since a single send in the body would empty the arena and hide
# the retention.
#
# Each assertion runs a full GC while the arena still holds what the loop left
# there.  The arena is a GC root, so exactly the pinned objects survive that
# collection and the rise in `GC.stat[:live]` counts them and nothing else.
# The `GC.start` has to be the first send after the loop: any cfunc return
# drains the arena, so reading `GC.stat` first would discard the very thing
# being measured.
#
# A retaining branch pins one object per iteration and reports the full 20000.
# The margin below covers the few objects `GC.stat` allocates for its own
# result; it is not slack for a partial leak, and a branch that retains on even
# a small fraction of the iterations is over it.

assert('OP_GETIDX does not retain its result in the GC arena') do
  s = "hello"
  GC.start
  base = GC.stat[:live]
  i = 0
  while i < 20000
    s[1]
    i += 1
  end
  GC.start
  assert_operator GC.stat[:live] - base, :<, 100
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
  GC.start
  assert_operator GC.stat[:live] - base, :<, 100
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
  GC.start
  assert_operator GC.stat[:live] - base, :<, 100
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
  GC.start
  assert_operator GC.stat[:live] - base, :<, 100
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
  GC.start
  assert_operator GC.stat[:live] - base, :<, 100
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
    GC.start
    assert_operator GC.stat[:live] - base, :<, 100
  end
end

# The assertions below cover the boxing sites inside the interpreter loop
# rather than the calls out of it.  `SET_INT_VALUE()` heap-allocates an
# RInteger for a value outside the fixnum range, and the inline opcodes that
# box one have no cfunc epilogue behind them either, so what they allocate
# stays in the arena the same way.  Where the fixnum range ends depends on the
# boxing mode, so the arithmetic loops are run at more than one width; on a
# build whose boxing macro cannot allocate at all they retain nothing and the
# assertions hold trivially.  `1 << shift` is written with a variable shift
# because a constant shift is folded at compile time, and a folded result out
# of mrb_int range makes the build fail rather than raise.

assert('OP_ADD does not retain a boxed Integer in the GC arena') do
  [30, 31, 62].each do |shift|
    begin
      x = 1 << shift
    rescue RangeError
      next  # mrb_int is narrower than this and mruby-bigint is absent
    end
    one = 1
    GC.start
    base = GC.stat[:live]
    i = 0
    while i < 20000
      x + one   # OP_ADD
      x + 1     # OP_ADDI
      i += 1
    end
    GC.start
    assert_operator GC.stat[:live] - base, :<, 100
  end
end

assert('OP_DIV does not retain a boxed Integer in the GC arena') do
  [30, 31, 62].each do |shift|
    begin
      x = 1 << shift
    rescue RangeError
      next
    end
    one = 1
    GC.start
    base = GC.stat[:live]
    i = 0
    while i < 20000
      x / one
      i += 1
    end
    GC.start
    assert_operator GC.stat[:live] - base, :<, 100
  end
end

assert('OP_LOADI32 does not retain a boxed Integer in the GC arena') do
  # `1073741824` is `2**30`, which fits in the operand of `OP_LOADI32` rather
  # than going to the pool, and is the first value outside the fixnum range of
  # a 32-bit host under word boxing.  That is the only configuration where this
  # opcode can allocate: everywhere else the value is a fixnum, the loop retains
  # nothing and the assertion holds trivially.
  GC.start
  base = GC.stat[:live]
  i = 0
  while i < 20000
    z = 1073741824
    i += 1
  end
  GC.start
  assert_operator GC.stat[:live] - base, :<, 100
  assert_equal 1073741824, z
end
