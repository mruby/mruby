assert('NilClass#to_a') do
  assert_equal nil.to_a, []
end

assert('NilClass#to_f') do
  assert_equal nil.to_f, 0.0
end

assert('NilClass#to_i') do
  assert_equal nil.to_i, 0
end
