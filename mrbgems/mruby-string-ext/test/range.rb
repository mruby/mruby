assert('Range#max') do
  # string Range depends on String#upto
  assert_equal 'l', ('f'..'l').max
  assert_equal 'e', ('a'...'f').max
  assert_equal nil, ('z'..'l').max
end

assert('Range#min') do
  # string Range depends on String#upto
  assert_equal 'f', ('f'..'l').min
  assert_equal nil, ('z'..'l').min
end
