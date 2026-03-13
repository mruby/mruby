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
    GC.start  # reset malloc_increase
    # allocate large strings to exceed threshold
    100.times { "x" * 1024 }
    stat = GC.stat
    assert_true stat[:malloc_increase] >= 0
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
