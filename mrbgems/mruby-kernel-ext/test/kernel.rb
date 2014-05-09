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
