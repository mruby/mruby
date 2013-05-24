assert('Fiber.new') {
  f = Fiber.new{}
  f.class == Fiber
}

assert('Fiber#resume') {
  f = Fiber.new{|x| x == 2}
  f.resume(2)
}

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
