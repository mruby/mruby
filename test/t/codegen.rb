##
# Codegen tests

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
