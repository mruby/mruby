##
# Float ISO Test

assert('Float', '15.2.9') do
  assert_equal Float.class, Class
end

assert('Float superclass', '15.2.9.2') do
  assert_equal Float.superclass, Numeric
end

assert('Float#+', '15.2.9.3.1') do
  a = 3.123456788 + 0.000000001
  b = 3.123456789 + 1

  assert_float(a, 3.123456789)
  assert_float(b, 4.123456789)
end

assert('Float#-', '15.2.9.3.2') do
  a = 3.123456790 - 0.000000001
  b = 5.123456789 - 1

  assert_float(a, 3.123456789)
  assert_float(b, 4.123456789)
end

assert('Float#*', '15.2.9.3.3') do
  a = 3.125 * 3.125
  b = 3.125 * 1

  assert_float(a, 9.765625)
  assert_float(b, 3.125)
end

assert('Float#/', '15.2.9.3.4') do
  a = 3.123456789 / 3.123456789
  b = 3.123456789 / 1

  assert_float(a, 1.0)
  assert_float(b, 3.123456789)
end

assert('Float#%', '15.2.9.3.5') do
  a = 3.125 % 3.125
  b = 3.125 % 1

  assert_float(a, 0.0)
  assert_float(b, 0.125)
end

assert('Float#<=>', '15.2.9.3.6') do
  a = 3.125 <=> 3.123
  b = 3.125 <=> 3.125
  c = 3.125 <=> 3.126
  a2 = 3.125 <=> 3
  c2 = 3.125 <=> 4

  assert_equal a, 1
  assert_equal b, 0
  assert_equal c, -1
  assert_equal a2, 1
  assert_equal c2, -1
end

assert('Float#==', '15.2.9.3.7') do
  assert_true 3.1 == 3.1
  assert_false 3.1 == 3.2
end

assert('Float#ceil', '15.2.9.3.8') do
  a = 3.123456789.ceil
  b = 3.0.ceil
  c = -3.123456789.ceil
  d = -3.0.ceil

  assert_equal a, 4
  assert_equal b, 3
  assert_equal c, -3
  assert_equal d, -3
end

assert('Float#finite?', '15.2.9.3.9') do
  assert_true 3.123456789.finite?
  assert_false (1.0 / 0.0).finite?
end

assert('Float#floor', '15.2.9.3.10') do
  a = 3.123456789.floor
  b = 3.0.floor
  c = -3.123456789.floor
  d = -3.0.floor

  assert_equal a, 3
  assert_equal b, 3
  assert_equal c, -4
  assert_equal d, -3
end

assert('Float#infinite?', '15.2.9.3.11') do
  a = 3.123456789.infinite?
  b = (1.0 / 0.0).infinite?
  c = (-1.0 / 0.0).infinite?

  assert_nil a
  assert_equal b, 1
  assert_equal c, -1
end

assert('Float#round', '15.2.9.3.12') do
  a = 3.123456789.round
  b = 3.5.round
  c = 3.4999.round
  d = (-3.123456789).round
  e = (-3.5).round
  f = 12345.67.round(-1)
  g = 3.423456789.round(0)
  h = 3.423456789.round(1)
  i = 3.423456789.round(3)

  assert_equal a, 3
  assert_equal b, 4
  assert_equal c, 3
  assert_equal d, -3
  assert_equal e, -4
  assert_equal f, 12350
  assert_equal g, 3
  assert_float(h, 3.4)
  assert_float(i, 3.423)
end

assert('Float#to_f', '15.2.9.3.13') do
  a = 3.123456789

  assert_float(a.to_f, a)
end

assert('Float#to_i', '15.2.9.3.14') do
  assert_equal 3.123456789.to_i, 3
end

assert('Float#truncate', '15.2.9.3.15') do
  assert_equal 3.123456789.truncate, 3
  assert_equal(-3.1.truncate, -3)
end
