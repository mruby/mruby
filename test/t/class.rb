##
# Class ISO Test

assert('Class', '15.2.3') do
  assert_equal(Class.class, Class)
end

assert('Class superclass', '15.2.3.2') do
  assert_equal(Class.superclass, Module)
end

# Class#initialize '15.2.3.3.1' is tested in Class#new

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
    "a".singleton_class.new
  end

  class TestClass
    def initialize args, &block
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

  assert_equal(TestClass.new(:arg).result, :only_args)
  # with block doesn't work yet
end

assert('Class#superclass', '15.2.3.3.4') do
  class SubClass < String; end
  assert_equal(SubClass.superclass, String)
end

# Not ISO specified

assert('Class 1') do
  class C1; end
  assert_equal(C1.class, Class)
end

assert('Class 2') do
  class C2; end
  assert_equal(C2.new.class, C2)
end

assert('Class 3') do
  class C3; end
  assert_equal(C3.new.class.class, Class)
end

assert('Class 4') do
  class C4_A; end
  class C4 < C4_A; end
  assert_equal(C4.class, Class)
end

assert('Class 5') do
  class C5_A; end
  class C5 < C5_A; end
  assert_equal(C5.new.class, C5)
end

assert('Class 6') do
  class C6_A; end
  class C6 < C6_A; end
  assert_equal(C6.new.class.class, Class)
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
  assert_equal(M.class, Module)
end

assert('Class Module 2') do
  module M; end
  class C; include M; end
  assert_equal(C.new.class, C)
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
  assert_equal(A::B.new.class, A::B)
end

assert('Class Nested 3') do
  class A; end
  class A::B; end
  assert_equal(A::B.new.class.class, Class)
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
  assert_equal(A::B::C.class, Class)
end

assert('Class Nested 6') do
  class A; end
  class A::B; end
  class A::B::C; end
  assert_equal(A::B::C.new.class, A::B::C)
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
  assert_equal(A::B2.class, Class)
end

assert('Class Colon 1') do
  class A; end
  A::C = 1
  assert_equal(A::C, 1)
end

assert('Class Colon 2') do
  class A; class ::C; end end
  assert_equal(C, C)
end

assert('Class Colon 3') do
  class A; class ::C; end end
  assert_equal(C.class, Class)
end

assert('Class Dup 1') do
  class C; end
  assert_equal(C.dup.class, Class)
end

assert('Class Dup 2') do
  module M; end
  assert_equal(M.dup.class, Module)
end
