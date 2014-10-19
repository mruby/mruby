assert('Kernel.fail, Kernel#fail') do
  assert_raise(RuntimeError) { fail }
  assert_raise(RuntimeError) { Kernel.fail }
end

assert('Kernel#__method__') do
  assert_equal(:m, Class.new {def m; __method__; end}.new.m)
  assert_equal(:m, Class.new {define_method(:m) {__method__}}.new.m)
  c = Class.new do
    [:m1, :m2].each do |m|
      define_method(m) do
        __method__
      end
    end
  end
  assert_equal(:m1, c.new.m1)
  assert_equal(:m2, c.new.m2)
end

assert('Kernel#Array') do
  assert_equal([1], Kernel.Array(1))
  assert_equal([1, 2, 3, 4, 5], Kernel.Array([1, 2, 3, 4, 5]))
  assert_equal([1, 2, 3, 4, 5], Kernel.Array(1..5))
  assert_equal([[:a, 1], [:b, 2], [:c, 3]], Kernel.Array({a:1, b:2, c:3}))
end
