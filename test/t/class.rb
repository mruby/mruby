##
# Class ISO Test

assert('Class', '15.2.3') do
  assert_equal(Class, Class.class)
end

assert('Class#initialize', '15.2.3.3.1') do
  c = Class.new do
    def test
      :test
    end
  end.new

  assert_equal(c.test, :test)
end

assert('Class#initialize_copy', '15.2.3.3.2') do
  class TestClass
    attr_accessor :n
    def initialize(n)
      @n = n
    end
    def initialize_copy(obj)
      @n = n
    end
  end

  c1 = TestClass.new('Foo')
  c2 = c1.dup
  c3 = TestClass.new('Bar')

  assert_equal(c1.n, c2.n)
  assert_not_equal(c1.n, c3.n)
end

assert('Class#new', '15.2.3.3.3') do
  assert_raise(TypeError, 'Singleton should raise TypeError') do
    (class <<"a"; self; end).new
  end

  class TestClass
    def initialize(args, &block)
      @result = if not args.nil? and block.nil?
        # only arguments
        :only_args
      elsif not args.nil? and not block.nil?
        # args and block is given
        :args_and_block
      else
        # this should never happen
        :broken
      end
    end

    def result; @result; end
  end

  assert_equal(:only_args, TestClass.new(:arg).result)
  # with block doesn't work yet
end

assert('Class#superclass', '15.2.3.3.4') do
  class SubClass < String; end
  assert_equal(String, SubClass.superclass)
end

# Not ISO specified

assert('Class 1') do
  class C1; end
  assert_equal(Class, C1.class)
end

assert('Class 2') do
  class C2; end
  assert_equal(C2, C2.new.class)
end

assert('Class 3') do
  class C3; end
  assert_equal(Class, C3.new.class.class)
end

assert('Class 4') do
  class C4_A; end
  class C4 < C4_A; end
  assert_equal(Class, C4.class)
end

assert('Class 5') do
  class C5_A; end
  class C5 < C5_A; end
  assert_equal(C5, C5.new.class)
end

assert('Class 6') do
  class C6_A; end
  class C6 < C6_A; end
  assert_equal(Class, C6.new.class.class)
end

assert('Class 7') do
  class C7_A; end
  class C7_B; end

  class C7 < C7_A; end

  assert_raise(TypeError) do
    # Different superclass.
    class C7 < C7_B; end
  end
end

assert('Class 8') do
  class C8_A; end

  class C8; end  # superclass is Object

  assert_raise(TypeError) do
    # Different superclass.
    class C8 < C8_A; end
  end
end

assert('Class 9') do
  Class9Const = "a"

  assert_raise(TypeError) do
    class Class9Const; end
  end
end

assert('Class Module 1') do
  module M; end
  assert_equal(Module, M.class)
end

assert('Class Module 2') do
  module M; end
  class C; include M; end
  assert_equal(C, C.new.class)
end

# nested class
assert('Class Nested 1') do
  class A; end
  class A::B; end
  assert_equal(A::B, A::B)
end

assert('Class Nested 2') do
  class A; end
  class A::B; end
  assert_equal(A::B, A::B.new.class)
end

assert('Class Nested 3') do
  class A; end
  class A::B; end
  assert_equal(Class, A::B.new.class.class)
end

assert('Class Nested 4') do
  class A; end
  class A::B; end
  class A::B::C; end
  assert_equal(A::B::C, A::B::C)
end

assert('Class Nested 5') do
  class A; end
  class A::B; end
  class A::B::C; end
  assert_equal(Class, A::B::C.class)
end

assert('Class Nested 6') do
  class A; end
  class A::B; end
  class A::B::C; end
  assert_equal(A::B::C, A::B::C.new.class)
end

assert('Class Nested 7') do
  class A; end
  class A::B; end
  class A::B2 < A::B; end
  assert_equal(A::B2, A::B2)
end

assert('Class Nested 8') do
  class A; end
  class A::B; end
  class A::B2 < A::B; end
  assert_equal(Class, A::B2.class)
end

assert('Class Colon 1') do
  class A; end
  A::C = 1
  assert_equal(1, A::C)
end

assert('Class Colon 2') do
  class A; class ::C; end end
  assert_equal(C, C)
end

assert('Class Colon 3') do
  class A; class ::C; end end
  assert_equal(Class, C.class)
end

assert('Class Dup 1') do
  class C; end
  assert_equal(Class, C.dup.class)
end

assert('Class Dup 2') do
  module M; end
  assert_equal(Module, M.dup.class)
end

assert('Class.new') do
  assert_equal(Class, Class.new.class)
  a = []
  klass = Class.new do |c|
    a << c
  end
  assert_equal([klass], a)
end

assert('class to return the last value') do
  m = class C; :m end
  assert_equal(m, :m)
end

assert('class to return nil if body is empty') do
  assert_nil(class C end)
  assert_nil(class << self; end)
end

assert('raise when superclass is not a class') do
  module FirstModule; end
  assert_raise(TypeError, 'should raise TypeError') do
    class FirstClass < FirstModule; end
  end

  class SecondClass; end
  assert_raise(TypeError, 'should raise TypeError') do
    class SecondClass < false; end
  end

  class ThirdClass; end
  assert_raise(TypeError, 'should raise TypeError') do
    class ThirdClass < ThirdClass; end
  end
end

assert('Class#inherited') do
  class Foo
    @@subclass_name = nil
    def self.inherited(subclass)
      @@subclass_name = subclass
    end
    def self.subclass_name
      @@subclass_name
    end
  end

  assert_equal(nil, Foo.subclass_name)

  class Bar < Foo
  end

  assert_equal(Bar, Foo.subclass_name)

  class Baz < Bar
  end

  assert_equal(Baz, Foo.subclass_name)
end

assert('singleton tests') do
  module FooMod
    def run_foo_mod
      100
    end
  end

  bar = String.new

  baz = class << bar
    extend FooMod
    def self.run_baz
      200
    end
  end

  assert_equal :run_baz, baz

  assert_raise(NoMethodError, 'should raise NoMethodError') do
    bar.run_foo_mod
  end
  assert_raise(NoMethodError, 'should raise NoMethodError') do
    bar.run_baz
  end

  baz = class << bar
    extend FooMod
    def self.run_baz
      300
    end
    self
  end

  assert_true baz.respond_to? :run_baz
  assert_true baz.respond_to? :run_foo_mod
  assert_equal 100, baz.run_foo_mod
  assert_equal 300, baz.run_baz

  assert_raise(NoMethodError, 'should raise NoMethodError') do
    bar.run_foo_mod
  end
  assert_raise(NoMethodError, 'should raise NoMethodError') do
    bar.run_baz
  end

  fv = false
  class << fv
    def self.run_false
      5
    end
  end

  nv = nil
  class << nv
    def self.run_nil
      6
    end
  end

  tv = true
  class << tv
    def self.run_nil
      7
    end
  end

  assert_raise(TypeError, 'should raise TypeError') do
    num = 1.0
    class << num
      def self.run_nil
        7
      end
    end
  end if Object.const_defined?(:Float)

  o = Object.new
  sc = class << o; self end
  o.freeze
  assert_predicate(sc, :frozen?)

  assert_predicate(class << Object.new.freeze; self end, :frozen?)
end

assert('clone Class') do
  class Foo
    def func
      true
    end
  end

  assert_true(Foo.clone.new.func)
end

assert('class definition in singleton class') do
  class AClassS
    class << self
      class BClass
      end

      def iclass
        BClass
      end
    end
  end
  assert_equal(Class, AClassS.iclass.class)
end

assert('class variable and class << self style class method') do
  class ClassVariableTest
    @@class_variable = "value"
    class << self
      def class_variable
        @@class_variable
      end
    end
  end

  assert_equal("value", ClassVariableTest.class_variable)
end

assert('class variable definition in singleton_class') do
  class ClassVariableDefinitionInSingletonTest
    class << self
      @@class_variable = "value"
    end
    def class_variable
      @@class_variable
    end
  end

  assert_equal("value", ClassVariableDefinitionInSingletonTest.new.class_variable)
end

assert('class variable in module and class << self style class method') do
  module ClassVariableInModuleTest
    @@class_variable = "value"
    class << self
      def class_variable
        @@class_variable
      end
    end
  end

  assert_equal("value", ClassVariableInModuleTest.class_variable)
end

assert('child class/module defined in singleton class get parent constant') do
  actual = module GetParentConstantTest
            EXPECT = "value"
            class << self
              class CHILD
                class << self
                    EXPECT
                end
              end
            end
          end
  assert_equal("value", actual)
end

assert('overriding class variable with a module (#3235)') do
  module ModuleWithCVar
    @@class_variable = 1
  end
  class CVarOverrideTest
    @@class_variable = 2
    include ModuleWithCVar

    assert_equal(1, @@class_variable)
  end
end

assert('class variable for frozen class/module') do
  module CVarForFrozenModule
    freeze
    assert_raise(FrozenError) { @@cv = 1 }
  end

  class CVarForFrozenClassA
    @@a = nil
    freeze
  end
  class CVarForFrozenClassB < CVarForFrozenClassA
    def a=(v)
      @@a = v
    end
  end
  b = CVarForFrozenClassB.new
  assert_raise(FrozenError) { b.a = 1 }
end

assert('class with non-class/module outer raises TypeError') do
  assert_raise(TypeError) { class 0::C1; end }
  assert_raise(TypeError) { class []::C2; end }
end

assert('module with extended callback') do
  module FooWithExtended
    @@extended = []

    def self.extended(base)
      @@extended << base
    end

    def self.extended_classes
      @@extended
    end

    def answer
      42
    end
  end

  class BarBeingExtended
    extend FooWithExtended
  end

  assert_equal [BarBeingExtended], FooWithExtended.extended_classes
  assert_true BarBeingExtended.respond_to?(:answer)
  assert_equal 42, BarBeingExtended.answer
end

assert("inherited hook runs before block body") do
  class A
    def self.values
      @values ||= []
    end

    def self.inherited(mod)
      mod.values << 1
    end
  end

  klass = Class.new(A) do
    self.values << 2
  end

  assert_equal [1, 2], klass.values
end

assert("inherited hook runs before class body") do
  class A
    def self.values
      @values ||= []
    end

    def self.inherited(mod)
      mod.values << 1
    end
  end

  class B < A
    self.values << 2
  end

  assert_equal [1, 2], B.values
end

assert('a visibility change makes the method table it needs') do
  # A class carries no method table until something is written into it, and a
  # visibility change writes a copy of the method it names, so it has to make
  # one first. Reaching the table without one crashed the VM (#7293), which a
  # singleton class inside another one is the shortest way to.
  assert_nothing_raised do
    o = Object.new
    class << o
      class << self
        protected :inspect
      end
    end
  end

  assert_nothing_raised do
    class << Object.new
      private :to_s
    end
  end

  # the change is the one that was asked for
  c = Class.new do
    def m
      :called
    end
    private :m
  end
  assert_raise(NoMethodError) { c.new.m }

  # and a frozen class refuses it, the way defining a method on one does
  assert_raise(FrozenError) do
    class TestVisibilityFrozen
      def m
        1
      end
    end
    TestVisibilityFrozen.freeze
    class TestVisibilityFrozen
      private :m
    end
  end
end

module Test4GivenDefMaker
  # Run in a method, so that the block's cref is this module rather than the
  # class `mrb_proc_new()` falls back to when a block at the top level of a
  # compiled test file has no cref to answer with.
  def self.klass
    Class.new do
      def direct; def from_direct; end; end
      def in_block; [1].each { def from_block; end }; end
      def self.on_singleton; def from_singleton_body; end; end
      def aliased; alias from_alias direct; end
      def undefs; undef gone; end
      def gone; end
      private
      def hidden; def from_hidden; end; end
    end
  end
  def self.mod; Module.new { def direct; def from_direct; end; end }; end
  def self.on_eval(c); c.class_eval { def direct; def from_direct; end; end }; end
  def self.on_instance(o); o.instance_eval { def direct; def from_direct; end; end }; end
end

assert('a `def` in a method written in a block given a class adds to that class') do
  # The method proc used to carry its cref, the scope the block was written
  # in, and a `def` in its body went there: `Object` for a script. The
  # class the block was given is what the method carries now, so a `def`
  # or an `alias` in the body, in a block made in the body, or in the body
  # of a `def self.name` written in the block, adds to that class. The
  # `def` written under a `private` in the block is private; the one in
  # its body starts at the default and is public.
  c = Test4GivenDefMaker.klass
  o = c.new
  o.direct
  o.in_block
  c.on_singleton
  assert_nothing_raised { o.aliased }
  o.__send__(:hidden)
  [:from_direct, :from_block, :from_singleton_body, :from_alias, :from_hidden].each do |name|
    assert_true c.method_defined?(name), name.to_s
    assert_false Object.new.respond_to?(name, true), name.to_s
  end
  assert_false c.method_defined?(:hidden)
  assert_true c.method_defined?(:gone)
  assert_nothing_raised { o.undefs }
  assert_false c.method_defined?(:gone)

  m = Test4GivenDefMaker.mod
  Class.new { include m }.new.direct
  assert_true m.method_defined?(:from_direct)
  assert_false Object.new.respond_to?(:from_direct, true)

  c = Class.new
  Test4GivenDefMaker.on_eval(c)
  c.new.direct
  assert_true c.method_defined?(:from_direct)
  assert_false Object.new.respond_to?(:from_direct, true)

  o = Object.new
  Test4GivenDefMaker.on_instance(o)
  o.direct
  assert_true o.respond_to?(:from_direct)
  assert_false Object.new.respond_to?(:from_direct, true)
end
