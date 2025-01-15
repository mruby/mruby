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
