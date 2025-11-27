assert('Time#strftime') do
  t = Time.now
  assert_true t.respond_to?(:strftime)
end

assert('Time#strftime with basic formats') do
  t = Time.gm(2023, 12, 25, 10, 30, 45)

  assert_equal '2023', t.strftime('%Y')
  assert_equal '23', t.strftime('%y')
  assert_equal '12', t.strftime('%m')
  assert_equal '25', t.strftime('%d')
  assert_equal '10', t.strftime('%H')
  assert_equal '30', t.strftime('%M')
  assert_equal '45', t.strftime('%S')
end

assert('Time#strftime with combined formats') do
  t = Time.gm(2023, 12, 25, 10, 30, 45)

  assert_equal '2023-12-25', t.strftime('%Y-%m-%d')
  assert_equal '10:30:45', t.strftime('%H:%M:%S')
  assert_equal '2023-12-25 10:30:45', t.strftime('%Y-%m-%d %H:%M:%S')
end

assert('Time#strftime with weekday formats') do
  # 2023-12-25 is Monday
  t = Time.gm(2023, 12, 25)

  result = t.strftime('%A')
  assert_true result.include?('Mon') || result == 'Monday'

  result = t.strftime('%a')
  assert_true result.length >= 2

  assert_equal '1', t.strftime('%w')  # Monday is day 1
end

assert('Time#strftime with month formats') do
  t = Time.gm(2023, 12, 25)

  result = t.strftime('%B')
  assert_true result.include?('Dec') || result == 'December'

  result = t.strftime('%b')
  assert_true result.length >= 2
end

assert('Time#strftime with literal percent') do
  t = Time.gm(2023, 12, 25)

  assert_equal '%', t.strftime('%%')
  assert_equal '100%', t.strftime('100%%')
  assert_equal '2023%12', t.strftime('%Y%%%m')
end

assert('Time#strftime with empty format') do
  t = Time.now

  assert_equal '', t.strftime('')
end

assert('Time#strftime with no format specifiers') do
  t = Time.now

  assert_equal 'hello', t.strftime('hello')
  assert_equal 'test123', t.strftime('test123')
end

assert('Time#strftime with NUL byte') do
  t = Time.gm(2023, 12, 25)

  result = t.strftime("foo\0bar")
  assert_equal 7, result.length
  assert_equal 'f', result[0]
  assert_equal 'o', result[1]
  assert_equal 'o', result[2]
  assert_equal "\0", result[3]
  assert_equal 'b', result[4]
  assert_equal 'a', result[5]
  assert_equal 'r', result[6]
end

assert('Time#strftime with NUL and format specifiers') do
  t = Time.gm(2023, 12, 25)

  result = t.strftime("year\0%Y")
  assert_true result.length >= 9  # "year\0" (5) + "2023" (4)
  assert_true result.include?("\0")
  # Check last 4 characters are "2023"
  assert_equal '2023', result[-4, 4]
end

assert('Time#strftime with multiple NULs') do
  t = Time.now

  result = t.strftime("\0\0")
  assert_equal 2, result.length
  assert_equal "\0\0", result
end

assert('Time#strftime with NUL at beginning') do
  t = Time.gm(2023, 1, 1)

  result = t.strftime("\0%Y")
  assert_equal 5, result.length
  assert_equal "\0", result[0]
  assert_equal '2023', result[1, 4]
end

assert('Time#strftime with NUL at end') do
  t = Time.gm(2023, 1, 1)

  result = t.strftime("%Y\0")
  assert_equal 5, result.length
  assert_equal '2023', result[0, 4]
  assert_equal "\0", result[4]
end

assert('Time#strftime preserves timezone') do
  t_utc = Time.utc(2023, 1, 1, 12, 0, 0)
  t_local = Time.local(2023, 1, 1, 12, 0, 0)

  # Both should format their time correctly
  assert_equal '12', t_utc.strftime('%H')
  assert_equal '12', t_local.strftime('%H')
end

assert('Time#strftime with various time components') do
  t = Time.gm(2023, 6, 15, 14, 23, 7)

  # Use portable format specifiers that work on all platforms
  assert_equal '06', t.strftime('%m')   # Month with leading zero (portable)
  assert_equal '15', t.strftime('%d')
  assert_equal '14', t.strftime('%H')
  assert_equal '23', t.strftime('%M')
  assert_equal '07', t.strftime('%S')
end

assert('Time#strftime argument type error') do
  t = Time.now

  assert_raise(TypeError) { t.strftime(123) }
  assert_raise(TypeError) { t.strftime(nil) }
end

assert('Time#strftime argument count error') do
  t = Time.now

  assert_raise(ArgumentError) { t.strftime }
  assert_raise(ArgumentError) { t.strftime('%Y', '%m') }
end
