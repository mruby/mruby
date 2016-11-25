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

assert('codegen absorbs arguments to redo and retry if they are the argument of a call') do
  assert_nothing_raised do
    a=*"1", case nil
    when 1
      redo |
      1
    end
  end

  assert_nothing_raised do
    a=*"1", case nil
    when 1
      retry |
      1
    end
  end
end
