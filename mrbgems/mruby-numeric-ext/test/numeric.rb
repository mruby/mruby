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
