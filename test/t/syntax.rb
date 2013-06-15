assert('super', '11.3.4') do
  assert_raise NoMethodError do
    super
  end

  class SuperFoo
    def foo
      true
    end
    def bar(*a)
      a
    end
  end
  class SuperBar < SuperFoo
    def foo
      super
    end
    def bar(*a)
      super(*a)
    end
  end
  bar = SuperBar.new

  assert_true bar.foo
  assert_equal bar.bar(1,2,3), [1,2,3]
end

assert('yield', '11.3.5') do
  assert_raise LocalJumpError do
    yield
  end
end

assert('Abbreviated variable assignment', '11.4.2.3.2') do
  a ||= 1
  b &&= 1
  c = 1
  c += 2

  assert_equal a, 1
  assert_nil b
  assert_equal c, 3
end

assert('Nested const reference') do
  module Syntax4Const
    CONST1 = "hello world"
    class Const2
      def const1
        CONST1
      end
    end
  end
  assert_equal Syntax4Const::CONST1, "hello world"
  assert_equal Syntax4Const::Const2.new.const1, "hello world"
end

assert('Abbreviated variable assignment as returns') do
  module Syntax4AbbrVarAsgnAsReturns
    class A
      def b
        @c ||= 1
      end
    end
  end
  assert_equal Syntax4AbbrVarAsgnAsReturns::A.new.b, 1
end
