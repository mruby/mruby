##
# Range(Ext) Test

assert('Range#cover?') do
  assert_true ("a".."z").cover?("c")
  assert_true !("a".."z").cover?("5")
  assert_true ("a".."z").cover?("cc")
end

assert('Range#first') do
  assert_equal (10..20).first, 10
  assert_equal (10..20).first(3), [10, 11, 12]
end

assert('Range#last') do
  assert_equal (10..20).last, 20
  assert_equal (10...20).last, 20
  assert_equal (10..20).last(3), [18, 19, 20]
  assert_equal (10...20).last(3), [17, 18, 19]
end
