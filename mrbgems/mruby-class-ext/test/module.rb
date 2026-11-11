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

assert 'Module#<=>' do
  a = Class.new
  b = Class.new(a)
  c = Class.new(a)
  d = Module.new
  e = Class.new { include d }
  f = Module.new { include d }

  # compare class to class
  assert_equal 0, a <=> a
  assert_equal(-1, b <=> a)
  assert_equal 1, a <=> b
  assert_nil c <=> b

  # compare class to module
  assert_equal(-1, e <=> d)
  assert_equal 1, d <=> e
  assert_nil a <=> d

  # compare module to module
  assert_equal(-1, f <=> d)
  assert_equal 1, d <=> f
  assert_nil a <=> f

  assert_nil a <=> Object.new
end

assert 'Module#name' do
  module Outer
    class Inner; end
    const_set :SetInner, Class.new
  end

  assert_equal 'Outer', Outer.name
  assert_equal 'Outer::Inner', Outer::Inner.name
  assert_equal 'Outer::SetInner', Outer::SetInner.name

  outer = Module.new do
    const_set :SetInner, Class.new
  end
  Object.const_set :SetOuter, outer

  assert_equal 'SetOuter', SetOuter.name
  assert_equal 'SetOuter::SetInner', SetOuter::SetInner.name

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

module ExecGivenClassMaker
  # Run in a method, so that the blocks' cref is this module rather than the
  # class `mrb_proc_new()` falls back to when a block at the top level of a
  # compiled test file has no cref to answer with.
  def self.on_class(c); c.class_exec { def from_block; end; [1].each { def from_nested_block; end } }; end
  def self.on_module(m); m.module_exec { def from_block; end }; end
  def self.private_on(c); c.class_exec { private; def hidden; end }; c.class_exec { def shown; end }; end
end

assert('a `def` in a block given to class_exec or module_exec adds to the receiver') do
  # `mrb_object_exec()` set the class the frame runs under without marking
  # the frame as given one, so a `def` in the block, or in a block made in
  # it, went to the cref of the block, `Object` for a script, where a
  # `class_eval` block was marked and answered the receiver. The visibility
  # of the block starts at the default and ends with the block, as it does
  # there.
  c = Class.new
  ExecGivenClassMaker.on_class(c)
  assert_true c.new.respond_to?(:from_block)
  assert_true c.new.respond_to?(:from_nested_block)
  assert_false Object.new.respond_to?(:from_block, true)
  assert_false Object.new.respond_to?(:from_nested_block, true)

  m = Module.new
  ExecGivenClassMaker.on_module(m)
  assert_true Class.new { include m }.new.respond_to?(:from_block)
  assert_false Object.new.respond_to?(:from_block, true)

  c = Class.new
  ExecGivenClassMaker.private_on(c)
  assert_false c.new.respond_to?(:hidden)
  assert_true c.new.respond_to?(:hidden, true)
  assert_true c.new.respond_to?(:shown)
end

module ExecGivenMethodMaker
  # Run in a method, for the same reason as above.
  def self.on_class(c); c.class_exec { def direct; def from_direct; end; end; def in_block; [1].each { def from_block; end }; end }; end
  def self.on_module(m); m.module_exec { def direct; def from_direct; end; end }; end
end

assert('a `def` in a method written in a block given to class_exec or module_exec adds to the receiver') do
  # The method carries the class the block was given, not the cref of the
  # block, so a `def` in its body, or in a block made there, adds to the
  # receiver as the `def` written in the block does.
  c = Class.new
  ExecGivenMethodMaker.on_class(c)
  c.new.direct
  c.new.in_block
  assert_true c.new.respond_to?(:from_direct)
  assert_true c.new.respond_to?(:from_block)
  assert_false Object.new.respond_to?(:from_direct, true)
  assert_false Object.new.respond_to?(:from_block, true)

  m = Module.new
  ExecGivenMethodMaker.on_module(m)
  Class.new { include m }.new.direct
  assert_true Class.new { include m }.new.respond_to?(:from_direct)
  assert_false Object.new.respond_to?(:from_direct, true)
end

assert('Module#< and friends with singleton classes') do
  m = Module.new
  o = Object.new
  sc = class << o; self; end
  assert_nil sc < m
  o.extend m
  assert_true sc < m
  assert_true sc <= m
  assert_false m < sc
  assert_true m > sc
  assert_equal(-1, sc <=> m)
  string_sc = class << String; self; end
  object_sc = class << Object; self; end
  assert_true string_sc < Class
  assert_true string_sc < object_sc
  assert_nil Comparable <=> string_sc
end

assert 'Module#set_temporary_name' do
  m = Module.new
  assert_same m, m.set_temporary_name("fake_name")
  assert_equal "fake_name", m.name
  assert_equal "fake_name", m.to_s
  assert_equal "fake_name", m.inspect
  assert_true m.name.frozen?

  c = Class.new
  c.set_temporary_name("again")
  assert_equal "again", c.name
  c.set_temporary_name(nil)
  assert_nil c.name
  assert_equal "#<Class:", c.to_s[0, 8]
end

assert 'Module#set_temporary_name gives way to a constant' do
  m = Module.new
  m.set_temporary_name("fake_outer")
  m::Inner = Class.new
  m::Inner.set_temporary_name("fake_inner")
  assert_equal "fake_inner", m::Inner.name

  # a constant gives the module and what is in it permanent names
  ModuleTestTemporaryName = m
  assert_equal "ModuleTestTemporaryName", m.name
  assert_equal "ModuleTestTemporaryName", m.to_s
  assert_equal "ModuleTestTemporaryName::Inner", m::Inner.name
  assert_equal "ModuleTestTemporaryName::Inner", m::Inner.inspect
  assert_raise(RuntimeError) { m.set_temporary_name("other") }
  assert_raise(RuntimeError) { m::Inner.set_temporary_name("other") }
end

assert 'Module#set_temporary_name checks the name' do
  m = Module.new
  assert_raise(ArgumentError) { m.set_temporary_name("") }
  assert_raise(ArgumentError) { m.set_temporary_name("Foo") }
  assert_raise(ArgumentError) { m.set_temporary_name("Foo::Bar") }
  assert_raise(TypeError) { m.set_temporary_name(:sym) }
  assert_equal "foo::Bar", m.set_temporary_name("foo::Bar").name
  assert_raise(RuntimeError) { String.set_temporary_name("str") }
end
