##
# Random Test

assert("Random.new") do
  r1 = Random.new(123)
  r2 = Random.new(123)
  r3 = Random.new(124)
  assert_equal(r1.rand, r2.rand)
  assert_not_equal(r1.rand, r3.rand)
end

assert("Kernel.srand") do
  srand(234)
  r1 = rand
  srand(234)
  r2 = rand
  srand(235)
  r3 = rand
  assert_equal(r1, r2)
  assert_not_equal(r1, r3)
end

assert("Random.srand") do
  Random.srand(345)
  r1 = rand
  srand(345)
  r2 = Random.rand
  Random.srand(346)
  r3 = rand
  assert_equal(r1, r2)
  assert_not_equal(r1, r3)
end

assert("return class of Kernel.rand") do
  assert_kind_of(Fixnum, rand(3))
  assert_kind_of(Fixnum, rand(1.5))
  assert_kind_of(Float, rand)
  assert_kind_of(Float, rand(0.5))
end

assert("Array#shuffle") do
  ary = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  orig = ary.dup
  shuffled = ary.shuffle
  assert_equal(orig, ary)
  assert_not_equal(ary, shuffled)
  assert_equal(ary.size, shuffled.size)
  shuffled.each do |x|
    assert_include(ary, x)
    ary.delete(x)
  end
end

assert('Array#shuffle!') do
  ary = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  orig = ary.dup
  assert_same(ary, ary.shuffle!)
  assert_not_equal(orig, ary)
  assert_equal(orig.size, ary.size)
  ary.each do |x|
    assert_include(orig, x)
    orig.delete(x)
  end
end

assert("Array#shuffle(random)") do
  assert_raise(TypeError) do
    # this will cause an exception due to the wrong argument
    [1, 2].shuffle "Not a Random instance"
  end

  # verify that the same seed causes the same results
  ary = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  shuffled1 = ary.shuffle Random.new 345
  shuffled2 = ary.shuffle Random.new 345
  shuffled3 = ary.shuffle Random.new 346
  assert_equal(shuffled1, shuffled2)
  assert_not_equal(shuffled1, shuffled3)
end

assert('Array#shuffle!(random)') do
  assert_raise(TypeError) do
    # this will cause an exception due to the wrong argument
    [1, 2].shuffle! "Not a Random instance"
  end

  # verify that the same seed causes the same results
  ary1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  ary1.shuffle! Random.new 345
  ary2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  ary2.shuffle! Random.new 345
  ary3 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  ary3.shuffle! Random.new 346
  assert_equal(ary1, ary2)
  assert_not_equal(ary1, ary3)
end
