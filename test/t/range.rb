##
# Range ISO Test

assert('Range', '15.2.14') do
  assert_equal Range.class, Class
end

assert('Range superclass', '15.2.14.2') do
  assert_equal Range.superclass, Object
end

assert('Range#==', '15.2.14.4.1') do
  assert_true (1..10) == (1..10)
  assert_false (1..10) == (1..100)
  assert_true (1..10) == Range.new(1.0, 10.0)
end

assert('Range#===', '15.2.14.4.2') do
  a = (1..10)

  assert_true a === 5
  assert_false a === 20
end

assert('Range#begin', '15.2.14.4.3') do
  assert_equal (1..10).begin, 1
end

assert('Range#each', '15.2.14.4.4') do
  a = (1..3)
  b = 0
  a.each {|i| b += i}
  assert_equal b, 6
end

assert('Range#end', '15.2.14.4.5') do
  assert_equal (1..10).end, 10
end

assert('Range#exclude_end?', '15.2.14.4.6') do
  assert_true (1...10).exclude_end?
  assert_false (1..10).exclude_end?
end

assert('Range#first', '15.2.14.4.7') do
  assert_equal (1..10).first, 1
end

assert('Range#include', '15.2.14.4.8') do
  a = (1..10)

  assert_true a.include?(5)
  assert_false a.include?(20)
end

assert('Range#initialize', '15.2.14.4.9') do
  a = Range.new(1, 10, true)
  b = Range.new(1, 10, false)

  assert_equal a, (1...10)
  assert_true a.exclude_end?
  assert_equal b, (1..10)
  assert_false b.exclude_end?
end

assert('Range#last', '15.2.14.4.10') do
  assert_equal (1..10).last, 10
end

assert('Range#member?', '15.2.14.4.11') do
  a = (1..10)

  assert_true a.member?(5)
  assert_false a.member?(20)
end

assert('Range#eql?', '15.2.14.4.14') do
  assert_true (1..10).eql? (1..10)
  assert_false (1..10).eql? (1..100)
  assert_false (1..10).eql? (Range.new(1.0, 10.0))
  assert_false (1..10).eql? "1..10"
end
