assert('Fiber.new') {
  f = Fiber.new{}
  f.class == Fiber
}

assert('Fiber#resume') {
  f = Fiber.new{|x| x == 2}
  f.resume(2)
}

assert('Fiber#alive?') {
  f = Fiber.new{ Fiber.yield }
  f.resume
  r1 = f.alive?
  f.resume
  r2 = f.alive?
  r1 == true and r2 == false
}

assert('Fiber#==') do
  root = Fiber.current
  assert_equal root, root
  assert_equal root, Fiber.current
  assert_false root != Fiber.current
  f = Fiber.new {
    assert_false root == Fiber.current
  }
  f.resume
  assert_false f == root
  assert_true f != root
end

assert('Fiber.yield') {
  f = Fiber.new{|x| Fiber.yield(x == 3)}
  f.resume(3)
}

assert('Fiber iteration') {
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
  a == [1,9,2,8,3,7]
}

assert('Fiber with splat in the block argument list') {
  Fiber.new{|*x|x}.resume(1) == [1]
}

assert('Fiber raises on resume when dead') {
  r1 = true
  begin
    f = Fiber.new{}
    f.resume
    r1 = f.alive?
    f.resume
    false
  rescue => e1
    true
  end
}

assert('Yield raises when called on root fiber') {
  begin
    Fiber.yield
    false
  rescue => e1
    true
  end
}

assert('Double resume of Fiber') do
  f1 = Fiber.new {}
  f2 = Fiber.new {
    f1.resume
    assert_raise(RuntimeError) { f2.resume }
    Fiber.yield 0
  }
  assert_equal 0, f2.resume
  f2.resume
  assert_false f1.alive?
  assert_false f2.alive?
end

assert('Recursive resume of Fiber') do
  f1, f2 = nil, nil
  f1 = Fiber.new { assert_raise(RuntimeError) { f2.resume } }
  f2 = Fiber.new {
    f1.resume
    Fiber.yield 0
  }
  assert_equal 0, f2.resume
  f2.resume
  assert_false f1.alive?
  assert_false f2.alive?
end
