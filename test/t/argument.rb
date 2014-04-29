assert('Argument 1') do
  def m()
    1
  end

  assert_equal(m, 1)
  assert_raise ArgumentError do m(1) end
end

assert('Argument 2') do
  def m(a)
    a
  end

  assert_equal(m(1), 1)
  assert_raise ArgumentError do m() end
  assert_raise ArgumentError do m(1, 2) end
end

assert('Argument 3') do
  def m(a, b)
    [a, b]
  end

  assert_equal(m(1, 2), [1, 2])
  assert_raise ArgumentError do m(1) end
  assert_raise ArgumentError do m(1, 2, 3) end
end

assert('Argument 4') do
  def m(a, b = 2)
    [a, b]
  end

  assert_equal(m(1), [1, 2])
  assert_equal(m(1, 3), [1, 3])
  assert_raise ArgumentError do m() end
  assert_raise ArgumentError do m(1, 2, 3) end
end

assert('Argument 5') do
  def m(a, b = 2, &block)
    [a, b, block.call]
  end

  assert_equal(m(1){4}, [1, 2, 4])
  assert_equal(m(1, 3){4}, [1, 3, 4])
  assert_raise ArgumentError do m() end
  assert_raise ArgumentError do m(1, 2, 3) end
end

assert('Argument 6') do
  def m(a = 1, &block)
    [a, block.call]
  end

  assert_equal(m(){3}, [1, 3])
  assert_equal(m(2){3}, [2, 3])
  assert_raise ArgumentError do m(1, 2) end
end

assert('Argument 7') do
  def m(*a)
    a
  end

  assert_equal(m(), [])
  assert_equal(m(1, 2, 3), [1, 2, 3])
end

assert('Argument 8') do
  def m(*a, &block)
    a << block.call
  end

  assert_equal(m(){3}, [3])
  assert_equal(m(1, 2, 3){4}, [1, 2, 3, 4])
end

assert('Argument 9') do
  def m(a, *b, c)
    [a, b, c]
  end

  assert_equal(m(1, 2), [1, [], 2])
  assert_equal(m(1, 2, 3, 4), [1, [2, 3], 4])
  assert_raise ArgumentError do m(1) end
end

assert('Argument 10') do
  def m(a, *b, c, &block)
    [a, b, c, block.call]
  end

  assert_equal(m(1, 2){3}, [1, [], 2, 3])
  assert_equal(m(1, 2, 3, 4){5}, [1, [2, 3], 4, 5])
  assert_raise ArgumentError do m(1) end
end

assert('Argument 11') do
  def m(a, b = 2, *c, d, &block)
    [a, b, c, d, block.call]
  end

  assert_equal(m(1, 3){4}, [1, 2, [], 3, 4])
  assert_equal(m(1, 3, 4){5}, [1, 3, [], 4, 5])
  assert_equal(m(1, 3, 4, 5){6}, [1, 3, [4], 5, 6])
  assert_equal(m(1, 3, 4, 5, 6){7}, [1, 3, [4, 5], 6, 7])
  assert_raise ArgumentError do m(1) end
end

assert('Argument 12') do
  def m(a, b, c, d, e)
    [a, b, c, d, e]
  end

  assert_equal(m(*[1, 2, 3, 4, 5]), [1, 2, 3, 4, 5])
end
