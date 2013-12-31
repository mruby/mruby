assert('RData with marker') do
  m = RDataWithMarker.new
  assert_true m.released?

  m.value = ReleaseObj.new
  assert_false m.released?

  GC.start
  assert_false m.released?
end

assert('RData without marker') do
  m = RDataWithoutMarker.new
  assert_true m.released?

  m.value = ReleaseObj.new
  assert_false m.released?

  GC.start
  assert_true m.released?
end
