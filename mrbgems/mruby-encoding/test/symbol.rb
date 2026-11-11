assert('Symbol#encoding') do
  assert_equal Encoding::US_ASCII, :abc.encoding
  assert_equal Encoding::US_ASCII, "abc".b.to_sym.encoding
  assert_equal Encoding::UTF_8, :"日本".encoding
  assert_equal Encoding::BINARY, "\xff".b.to_sym.encoding
end

assert('Encoding::ASCII') do
  assert_same Encoding::US_ASCII, Encoding::ASCII
end
