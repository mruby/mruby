def assert_module_name(exp, mod)
  assert "assert_module_name" do
    act = mod.__send__(:name)
    assert_equal exp, act
    assert_predicate act, :frozen?
  end
end

assert 'Module#<' do
  a = Class.new
  b = Class.new(a)
  c = Class.new(a)
  d = Module.new
  e = Class.new { include d }
  f = Module.new { include d }

  # compare class to class
  assert_true b < a
  assert_false b < b
  assert_false a < b
  assert_nil c < b

  # compare class to module
  assert_true e < d
  assert_false d < e
  assert_nil a < d

  # compare module to module
  assert_true f < d
  assert_false f < f
  assert_false d < f

  assert_raise(TypeError) { a < Object.new }
end

assert 'Module#<=' do
  a = Class.new
  b = Class.new(a)
  c = Class.new(a)
  d = Module.new
  e = Class.new { include d }
  f = Module.new { include d }

  # compare class to class
  assert_true b <= a
  assert_true b <= b
  assert_false a <= b
  assert_nil c <= b

  # compare class to module
  assert_true e <= d
  assert_false d <= e
  assert_nil a <= d

  # compare module to module
  assert_true f <= d
  assert_true f <= f
  assert_false d <= f

  assert_raise(TypeError) { a <= Object.new }
end

assert 'Module#name' do
  module Outer
    class Inner; end
    const_set :SetInner, Class.new
  end

  assert_module_name 'Outer', Outer
  assert_module_name 'Outer::Inner', Outer::Inner
  assert_module_name 'Outer::SetInner', Outer::SetInner

  outer = Module.new do
    const_set :SetInner, Class.new
  end
  Object.const_set :SetOuter, outer

  assert_module_name 'SetOuter', SetOuter
  assert_module_name 'SetOuter::SetInner', SetOuter::SetInner

  mod = Module.new
  cls = Class.new

  assert_nil mod.name
  assert_nil cls.name
end

assert 'Module#singleton_class?' do
  mod = Module.new
  cls = Class.new
  scl = (class <<cls; self; end)

  assert_false mod.singleton_class?
  assert_false cls.singleton_class?
  assert_true scl.singleton_class?
end

assert 'Module#module_eval' do
  mod = Module.new
  mod.class_exec(1,2,3) do |a,b,c|
    assert_equal([1,2,3], [a,b,c])
    def hi
      "hi"
    end
  end
  cls = Class.new
  cls.class_exec(42) do |x|
    assert_equal(42, x)
    include mod
    def hello
      "hello"
    end
  end
  obj = cls.new
  assert_equal("hi", obj.hi)
  assert_equal("hello", obj.hello)
end
