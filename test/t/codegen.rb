##
# Codegen tests

assert('peephole optimization does not eliminate move whose result is reused') do
  assert_raise LocalJumpError do
    def method
      yield
    end
    method(&a &&= 0)
  end
end

assert('empty condition in ternary expression parses correctly') do
  assert_equal(() ? 1 : 2, 2)
end
