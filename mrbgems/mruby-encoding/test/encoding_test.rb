assert 'Encoding::UTF_8 defined' do
  assert_true Encoding.const_defined? :UTF_8
  assert_true Encoding::UTF_8.ascii_compatible?
  assert_equal "UTF-8", Encoding::UTF_8.name
end

assert 'Encoding::ASCII_8BIT defined' do
  assert_true Encoding.const_defined? :ASCII_8BIT
  assert_true Encoding::ASCII_8BIT.ascii_compatible?
  assert_equal "ASCII-8BIT", Encoding::ASCII_8BIT.name
end

assert 'String#encoding' do
  assert_equal Encoding::UTF_8, "".encoding
end

assert 'String#force_encoding' do
  assert_raise(ArgumentError) { "".force_encoding() }
  assert_raise(ArgumentError) { "".force_encoding("") }
  assert_equal Encoding::UTF_8, "".encoding
  assert_equal Encoding::ASCII_8BIT, "".force_encoding(Encoding::ASCII_8BIT).encoding
end
