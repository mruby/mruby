##
# Numeric(Ext) Test

assert('Integer#chr') do
  assert_equal("A", 65.chr)
  assert_equal("B", 0x42.chr)

  if "こんにちわ世界".size == 7 then
      # UTF-8 gem is configured
      assert_raise(RangeError) { 0x110000.chr }
  else
      # multibyte encoding (not support yet)
      assert_raise(RangeError) { 256.chr }
  end
end

assert('Integer#div') do
  assert_equal 52, 365.div(7)
end

assert('Fixnum#odd?') do
  assert_true 1.odd?
  assert_false 2.odd?
end

assert('Fixnum#even?') do
  assert_true 2.even?
  assert_false 1.even?
end

assert('Float#div') do
  assert_float 52, 365.2425.div(7)
end
