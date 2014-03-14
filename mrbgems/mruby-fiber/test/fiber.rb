assert('Fiber.new') do
  f = Fiber.new{}
  assert_equal Fiber, f.class
end

assert('Fiber#resume') do
  f = Fiber.new{|x| x}
  assert_equal 2, f.resume(2)
end

assert('Fiber#alive?') do
  f = Fiber.new{ Fiber.yield }
  f.resume
  assert_true f.alive?
  f.resume
  assert_false f.alive?
end

assert('Fiber.yield') do
  f = Fiber.new{|x| Fiber.yield(x) }
  assert_equal 3, f.resume(3)
end

assert('Fiber iteration') do
  f1 = Fiber.new{
    [1,2,3].each{|x| Fiber.yield(x)}
  }
  f2 = Fiber.new{
    [9,8,7].each{|x| Fiber.yield(x)}
  }
  a = []
  3.times {
    a << f1.resume
    a << f2.resume
  }
  assert_equal [1,9,2,8,3,7], a
end

assert('Fiber with splat in the block argument list') do
  assert_equal [1], Fiber.new{|*x|x}.resume(1)
end

assert('Fiber raises on resume when dead') do
  assert_raise(RuntimeError) do
    f = Fiber.new{}
    f.resume
    assert_false f.alive?
    f.resume
  end
end

assert('Yield raises when called on root fiber') do
  assert_raise(ArgumentError) { Fiber.yield }
end
