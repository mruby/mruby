assert('Fiber.new') do
  f = Fiber.new{}
  assert_equal Fiber, f.class
  assert_raise(ArgumentError) { Fiber.new }
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

assert('Fiber#suspended?') do
  f = Fiber.new {
    assert_false f.suspended?
    assert_true Fiber.root.suspended?
    Fiber.yield 0
  }
  assert_false Fiber.root.suspended?
  assert_true f.suspended? # created state fiber must be suspended
  assert_equal 0, f.resume
  assert_true f.suspended?
  f.resume
  assert_false f.alive?
  assert_false f.suspended? # dead fiber must not be suspended
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

assert('Double resume of Fiber') do
  f = Fiber.new {
    assert_raise(RuntimeError) { f.resume }
    Fiber.yield 0
    }
  assert_equal 0, f.resume
  f.resume
  assert_false f.alive?
end

assert('Yield raises when called on root fiber') do
  assert_raise(ArgumentError) { Fiber.yield }
end
