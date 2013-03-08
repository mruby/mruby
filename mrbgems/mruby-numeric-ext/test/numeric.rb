##
# Numeric(Ext) Test

assert('Integer#chr') do
  assert_equal(65.chr, "A")
  assert_equal(0x42.chr, "B")
end
