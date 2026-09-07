##
# Chapter 13.3 "Methods" ISO Test

assert('The alias statement', '13.3.6 a) 4)') do
  # check aliasing in all possible ways

  def alias_test_method_original; true; end

  alias alias_test_method_a alias_test_method_original
  alias :alias_test_method_b :alias_test_method_original

  assert_true(alias_test_method_original)
  assert_true(alias_test_method_a)
  assert_true(alias_test_method_b)

  # a quoted name with no interpolation is still a plain symbol
  alias :"alias_test_method_c" :"alias_test_method_original"
  assert_true(alias_test_method_c)
end


assert('The alias statement (overwrite original)', '13.3.6 a) 4)') do
  # check that an aliased method can be overwritten
  # without side effect

  def alias_test_method_original; true; end

  alias alias_test_method_a alias_test_method_original
  alias :alias_test_method_b :alias_test_method_original

  assert_true(alias_test_method_original)

  def alias_test_method_original; false; end

  assert_false(alias_test_method_original)
  assert_true(alias_test_method_a)
  assert_true(alias_test_method_b)
end

assert('The alias statement', '13.3.6 a) 5)') do
  # check that alias is raising NameError if
  # non-existing method should be undefined

  assert_raise(NameError) do
    alias new_name_a non_existing_method
  end

  assert_raise(NameError) do
    alias :new_name_b :non_existing_method
  end
end

assert('The undef statement', '13.3.7 a) 4)') do
  # check that undef is undefining method
  # based on the method name

  def existing_method_a; true; end
  def existing_method_b; true; end
  def existing_method_c; true; end
  def existing_method_d; true; end
  def existing_method_e; true; end
  def existing_method_f; true; end

  # check that methods are defined

  assert_true(existing_method_a, 'Method should be defined')
  assert_true(existing_method_b, 'Method should be defined')
  assert_true(existing_method_c, 'Method should be defined')
  assert_true(existing_method_d, 'Method should be defined')
  assert_true(existing_method_e, 'Method should be defined')
  assert_true(existing_method_f, 'Method should be defined')

  # undefine in all possible ways and check that method
  # is undefined

  undef existing_method_a
  assert_raise(NoMethodError) do
    existing_method_a
  end

  undef :existing_method_b
  assert_raise(NoMethodError) do
    existing_method_b
  end

  undef existing_method_c, existing_method_d
  assert_raise(NoMethodError) do
    existing_method_c
  end
  assert_raise(NoMethodError) do
    existing_method_d
  end

  undef :existing_method_e, :existing_method_f
  assert_raise(NoMethodError) do
    existing_method_e
  end
  assert_raise(NoMethodError) do
    existing_method_f
  end
end

assert('The undef statement (method undefined)', '13.3.7 a) 5)') do
  # check that undef is raising NameError if
  # non-existing method should be undefined

  assert_raise(NameError) do
    undef non_existing_method
  end

  assert_raise(NameError) do
    undef :non_existing_method
  end
end

assert('method_added hook') do
  c = Class.new do
    # method to retrieve @name
    def self.name; @name; end
    # hook method on method definition
    def self.method_added(name) @name = name; end
    # method definition
    def foo; end
  end
  assert_equal(:foo, c.name)
  c.define_method(:bar){}
  assert_equal(:bar, c.name)
end

assert('singleton_method_added hook') do
  a = Object.new
  # method to retrieve @name
  def a.name; @name; end
  # hook method on singleton method definition
  def a.singleton_method_added(name) @name = name; end
  # singleton method definition
  def a.foo; end
  assert_equal(:foo, a.name)
  class <<a
    def bar; end
  end
  assert_equal(:bar, a.name)
end

assert('The def statement in a method with a receiver', '13.3.1') do
  # a `def` in a method body adds to the class the method was written in,
  # which for `def self.name` is the class around it, not the singleton
  class DefTargetInSingleton
    def self.go; def m; :from_go; end; end
  end
  DefTargetInSingleton.go
  assert_equal(:from_go, DefTargetInSingleton.new.m)
  assert_raise(NoMethodError) { DefTargetInSingleton.m }

  # a block adds no scope of its own, so the class around it still answers
  class DefTargetInBlock
    def self.go; [1].each { def m; :from_block; end }; end
  end
  DefTargetInBlock.go
  assert_equal(:from_block, DefTargetInBlock.new.m)
  assert_raise(NoMethodError) { DefTargetInBlock.m }

  # `class << self` does open a scope, and a method written there adds to it
  class DefTargetInSclass
    class << self
      def go; def m; :from_sclass; end; end
    end
  end
  DefTargetInSclass.go
  assert_equal(:from_sclass, DefTargetInSclass.m)
  assert_raise(NoMethodError) { DefTargetInSclass.new.m }

  # the class a method was found in is not the one it was written in
  class DefTargetBase
    def go; def m; :from_base; end; end
  end
  class DefTargetSub < DefTargetBase; end
  DefTargetSub.new.go
  assert_equal(:from_base, DefTargetBase.new.m)

  # the top level is the scope around a `def` on an object written there
  receiver = Object.new
  def receiver.go; def def_target_at_top_level; :from_top_level; end; end
  receiver.go
  assert_equal(:from_top_level, def_target_at_top_level)
end

assert('The def statement in a class body written as a block', '13.3.1') do
  # `Class.new` and its kin hand the class to the frame running the block.
  # A `def self.name` written there is a scope of its own and keeps that
  # class, so a `def` in its body adds to the class and not to the singleton
  # or to the class the block was written in.
  k = Class.new do
    def self.go; def m; :from_class_new; end; end
  end
  k.go
  assert_equal(:from_class_new, k.new.m)
  assert_raise(NoMethodError) { k.m }

  mod = Module.new do
    def self.go; def m; :from_module_new; end; end
  end
  mod.go
  host = Class.new do
    include mod
    def probe; m; end
  end
  assert_equal(:from_module_new, host.new.probe)

  class DefTargetClassEval; end
  DefTargetClassEval.class_eval do
    def self.go; def m; :from_class_eval; end; end
  end
  DefTargetClassEval.go
  assert_equal(:from_class_eval, DefTargetClassEval.new.m)
  assert_raise(NoMethodError) { DefTargetClassEval.m }

  # a `def` written straight in such a block still adds to the class it
  # was handed
  assert_equal(:handed, Class.new { def m; :handed; end }.new.m)

  # so does one written in a block inside it: the inner block was made in a
  # scope that was handed the class, and carries it
  nested = Class.new do
    [1].each { def m; :from_nested_block; end }
  end
  assert_equal(:from_nested_block, nested.new.m)
  assert_raise(NoMethodError) { Object.new.m }
end

assert('The alias statement in a method with a receiver', '13.3.6') do
  class AliasTargetInSingleton
    def original; :original; end
    def self.go; alias aliased original; end
  end
  AliasTargetInSingleton.go
  assert_equal(:original, AliasTargetInSingleton.new.aliased)
end

assert('The undef statement in a method with a receiver', '13.3.7') do
  class UndefTargetInSingleton
    def original; :original; end
    def self.go; undef original; end
  end
  UndefTargetInSingleton.go
  assert_raise(NoMethodError) { UndefTargetInSingleton.new.original }
end

assert('super in a method with a receiver') do
  # the class a `def` adds to and the class `super` walks up from are two
  # answers, and `super` keeps taking the class the method was found in
  class SuperInSdefBase
    def self.go; :base; end
    def self.in_block; :base_block; end
  end
  class SuperInSdefSub < SuperInSdefBase
    def self.go; super; end
    def self.in_block; [1].map { super() }; end
  end
  assert_equal(:base, SuperInSdefSub.go)
  assert_equal([:base_block], SuperInSdefSub.in_block)
end
