##
# String(Ext) Test

UTF8STRING = __ENCODING__ == "UTF-8"

assert('String#valid_encoding?') do
  assert_true "hello".valid_encoding?
  if UTF8STRING
    assert_true "あ".valid_encoding?
    assert_false "\xfe".valid_encoding?
    assert_false "あ\xfe".valid_encoding?
    assert_true "あ\xfe".b.valid_encoding?
  else
    assert_true "\xfe".valid_encoding?
  end
end

assert('String#encoding') do
  if UTF8STRING
    a = "あ"
    assert_equal Encoding::UTF_8, a.encoding
    assert_equal Encoding::BINARY, a.b.encoding
    assert_equal a, a.force_encoding(Encoding::BINARY)
    assert_equal a, a.force_encoding(Encoding::BINARY)
    assert_equal Encoding::BINARY, a.encoding
  else
    a = "hello"
    assert_equal Encoding::BINARY, a.encoding
  end
end

assert('String#encoding survives a copy') do
  # A copy holds the same bytes, so it is byte-indexed exactly when the string
  # it copies is. The copy used to come back UTF-8, which made `size` and every
  # offset computed from it read the bytes as characters again.
  if UTF8STRING
    a = "\u{1F600}".b   # F0 9F 98 80: four bytes, one character
    assert_equal Encoding::BINARY, a.dup.encoding
    assert_equal Encoding::BINARY, a.clone.encoding
    assert_equal Encoding::BINARY, a.freeze.dup.encoding
    b = "hello"
    b.replace(a)
    assert_equal Encoding::BINARY, b.encoding
    # and a copy of a UTF-8 string is still UTF-8: the flag is copied, not set
    c = "\u{1F600}"
    assert_equal Encoding::UTF_8, c.dup.encoding
    d = "x".b
    d.replace(c)
    assert_equal Encoding::UTF_8, d.encoding
  end
end
