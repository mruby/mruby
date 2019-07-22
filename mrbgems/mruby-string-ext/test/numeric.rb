assert('Integer#chr') do
  assert_equal("A", 65.chr)
  assert_equal("B", 0x42.chr)
  assert_raise(RangeError) { -1.chr }
end
