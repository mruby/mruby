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
