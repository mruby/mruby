##
# Codegen tests

assert('empty condition in ternary expression parses correctly') do
  assert_equal () ? 1 : 2, 2
end
