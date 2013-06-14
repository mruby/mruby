# Not ISO specified

assert('GC.enable') do
  assert_equal GC.disable, false
  assert_equal GC.enable, true
  assert_equal GC.enable, false
end

assert('GC.disable') do
  begin
    assert_equal GC.disable, false
    assert_equal GC.disable, true
  ensure
    GC.enable
  end
end

assert('GC.interval_ratio=') do
  origin = GC.interval_ratio
  begin
    assert_equal (GC.interval_ratio = 150), 150
  ensure
    GC.interval_ratio = origin
  end
end

assert('GC.step_ratio=') do
  origin = GC.step_ratio
  begin
    assert_equal (GC.step_ratio = 150), 150
  ensure
    GC.step_ratio = origin
  end
end

assert('GC.generational_mode=') do
  origin = GC.generational_mode
  begin
    assert_equal (GC.generational_mode = false), false
    assert_equal (GC.generational_mode = true), true
    assert_equal (GC.generational_mode = true), true
  ensure
    GC.generational_mode = origin
  end
end
