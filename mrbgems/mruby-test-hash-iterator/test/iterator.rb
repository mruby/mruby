##
# HashIterator Test

assert('HashIterator empty') do
  iterator = HashIterator.new({})

  assert_equal iterator.remaining, 0
end

assert('HashIterator 1 element') do
  iterator = HashIterator.new({e: 50})

  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 2 elements') do
  iterator = HashIterator.new({d: 40, e: 50})

  assert_equal 2, iterator.remaining
  assert_equal :d, iterator.key
  assert_equal 40, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 3 elements') do
  iterator = HashIterator.new({c: 30, d: 40, e: 50})

  assert_equal 3, iterator.remaining
  assert_equal :c, iterator.key
  assert_equal 30, iterator.value

  assert_true iterator.move_next
  assert_equal 2, iterator.remaining
  assert_equal :d, iterator.key
  assert_equal 40, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 4 elements') do
  iterator = HashIterator.new({b: 20, c: 30, d: 40, e: 50})

  assert_equal 4, iterator.remaining
  assert_equal :b, iterator.key
  assert_equal 20, iterator.value

  assert_true iterator.move_next
  assert_equal 3, iterator.remaining
  assert_equal :c, iterator.key
  assert_equal 30, iterator.value

  assert_true iterator.move_next
  assert_equal 2, iterator.remaining
  assert_equal :d, iterator.key
  assert_equal 40, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements') do
  iterator = HashIterator.new({a: 10, b: 20, c: 30, d: 40, e: 50})

  assert_equal 5, iterator.remaining
  assert_equal :a, iterator.key
  assert_equal 10, iterator.value

  assert_true iterator.move_next
  assert_equal 4, iterator.remaining
  assert_equal :b, iterator.key
  assert_equal 20, iterator.value

  assert_true iterator.move_next
  assert_equal 3, iterator.remaining
  assert_equal :c, iterator.key
  assert_equal 30, iterator.value

  assert_true iterator.move_next
  assert_equal 2, iterator.remaining
  assert_equal :d, iterator.key
  assert_equal 40, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete center') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:c)

  iterator = HashIterator.new(hash)

  assert_equal 4, iterator.remaining
  assert_equal :a, iterator.key
  assert_equal 10, iterator.value

  assert_true iterator.move_next
  assert_equal 3, iterator.remaining
  assert_equal :b, iterator.key
  assert_equal 20, iterator.value

  assert_true iterator.move_next
  assert_equal 2, iterator.remaining
  assert_equal :d, iterator.key
  assert_equal 40, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete middle') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:b)
  hash.delete(:c)
  hash.delete(:d)

  iterator = HashIterator.new(hash)

  assert_equal 2, iterator.remaining
  assert_equal :a, iterator.key
  assert_equal 10, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete first') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:b)
  hash.delete(:c)
  hash.delete(:d)
  hash.delete(:e)

  iterator = HashIterator.new(hash)

  assert_equal 1, iterator.remaining
  assert_equal :a, iterator.key
  assert_equal 10, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete left') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:a)
  hash.delete(:b)
  hash.delete(:c)
  hash.delete(:d)

  iterator = HashIterator.new(hash)

  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete last') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:e)

  iterator = HashIterator.new(hash)

  assert_equal 4, iterator.remaining
  assert_equal :a, iterator.key
  assert_equal 10, iterator.value

  assert_true iterator.move_next
  assert_equal 3, iterator.remaining
  assert_equal :b, iterator.key
  assert_equal 20, iterator.value

  assert_true iterator.move_next
  assert_equal 2, iterator.remaining
  assert_equal :c, iterator.key
  assert_equal 30, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :d, iterator.key
  assert_equal 40, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete right') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:b)
  hash.delete(:c)
  hash.delete(:d)
  hash.delete(:e)

  iterator = HashIterator.new(hash)

  assert_equal 1, iterator.remaining
  assert_equal :a, iterator.key
  assert_equal 10, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete even spots') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:b)
  hash.delete(:d)

  iterator = HashIterator.new(hash)

  assert_equal 3, iterator.remaining
  assert_equal :a, iterator.key
  assert_equal 10, iterator.value

  assert_true iterator.move_next
  assert_equal 2, iterator.remaining
  assert_equal :c, iterator.key
  assert_equal 30, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :e, iterator.key
  assert_equal 50, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete odd spots') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:a)
  hash.delete(:c)
  hash.delete(:e)

  iterator = HashIterator.new(hash)

  assert_equal 2, iterator.remaining
  assert_equal :b, iterator.key
  assert_equal 20, iterator.value

  assert_true iterator.move_next
  assert_equal 1, iterator.remaining
  assert_equal :d, iterator.key
  assert_equal 40, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end

assert('HashIterator 5 elements, delete random order') do
  hash = {a: 10, b: 20, c: 30, d: 40, e: 50}
  hash.delete(:c)
  hash.delete(:e)
  hash.delete(:b)
  hash.delete(:a)

  iterator = HashIterator.new(hash)

  assert_equal 1, iterator.remaining
  assert_equal :d, iterator.key
  assert_equal 40, iterator.value

  assert_false iterator.move_next
  assert_equal 0, iterator.remaining
end
