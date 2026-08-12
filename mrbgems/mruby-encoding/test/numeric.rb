assert('Integer#chr') do
  assert_equal("A", 65.chr)
  assert_equal("B", 0x42.chr)
  assert_equal("\xab", 171.chr)
  assert_raise(RangeError) { -1.chr }
  assert_raise(RangeError) { 256.chr }

  assert_equal("A", 65.chr("ASCII-8BIT"))
  assert_equal("B", 0x42.chr("BINARY"))
  assert_equal("\xab", 171.chr("ascii-8bit"))
  assert_raise(RangeError) { -1.chr("binary") }
  assert_raise(RangeError) { 256.chr("Ascii-8bit") }
  assert_raise(ArgumentError) { 65.chr("ASCII") }
  assert_raise(ArgumentError) { 65.chr("ASCII-8BIT", 2) }
  assert_raise(TypeError) { 65.chr(:BINARY) }

  if __ENCODING__ == "ASCII-8BIT"
    assert_raise(ArgumentError) { 65.chr("UTF-8") }
  else
    assert_equal("A", 65.chr("UTF-8"))
    assert_equal("B", 0x42.chr("UTF-8"))
    assert_equal("«", 171.chr("utf-8"))
    assert_equal("あ", 12354.chr("Utf-8"))
    assert_raise(RangeError) { -1.chr("utf-8") }
    assert_raise(RangeError) { 0x110000.chr.chr("UTF-8") }
  end
end

assert('Integer#chr(binary) of a byte that spells no character') do
  # A byte of 0x80 and above spells no UTF-8 character on its own, so what
  # `chr` hands back for one is read a byte at a time rather than decoded.
  s = 171.chr
  assert_equal [171], s.bytes
  assert_equal 1, s.length
  assert_equal [171], s.codepoints
  assert_equal 171, s.ord
  assert_equal [171], s.scrub.bytes
  assert_equal "\"\\xab\"", s.inspect
  assert_equal Encoding::UTF_8, s.encoding
  assert_false s.valid_encoding?

  # a lead byte with nothing behind it spells no character either
  e = 0xE3.chr
  assert_equal [0xE3], e.bytes
  assert_equal 1, e.length
  assert_equal Encoding::UTF_8, e.encoding
  assert_false e.valid_encoding?

  # ASCII spells a character of its own however the string is read
  a = 65.chr
  assert_equal [65], a.bytes
  assert_equal 1, a.length
  assert_equal [65], a.codepoints
  assert_equal Encoding::UTF_8, a.encoding
  assert_true a.valid_encoding?

  # `<<` reads an Integer as a code point and `append_as_bytes` as a byte;
  # neither takes anything from what `chr` returns but its bytes
  s = "".dup
  s << 171
  assert_equal [0xC2, 0xAB], s.bytes
  t = "".dup
  t.append_as_bytes(171)
  assert_equal [171], t.bytes
end if __ENCODING__ == "UTF-8"
