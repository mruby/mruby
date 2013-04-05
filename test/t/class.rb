##
# Class ISO Test

assert('Class', '15.2.3') do
  Class.class == Class
end

assert('Class superclass', '15.2.3.2') do
  Class.superclass == Module
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

  c1.n == c2.n and c1.n != c3.n
end

assert('Class#new', '15.2.3.3.3') do
  # at the moment no exception on singleton class
  #e1 = nil
  #begin
  #  class1 = e1.singleton_class.new
  #rescue => e1
  #  e2 = e1
  #end

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

  TestClass.new(:arg).result == :only_args
  # with block doesn't work yet
end

assert('Class#superclass', '15.2.3.3.4') do
  class SubClass < String; end
  SubClass.superclass == String
end

# Not ISO specified

assert('Class 1') do
  class C1; end
  C1.class == Class
end

assert('Class 2') do
  class C2; end
  C2.new.class == C2
end

assert('Class 3') do
  class C3; end
  C3.new.class.class == Class
end

assert('Class 4') do
  class C4_A; end
  class C4 < C4_A; end
  C4.class == Class
end

assert('Class 5') do
  class C5_A; end
  class C5 < C5_A; end
  C5.new.class == C5
end

assert('Class 6') do
  class C6_A; end
  class C6 < C6_A; end
  C6.new.class.class == Class
end

assert('Class 7') do
  class C7_A; end
  class C7_B; end

  class C7 < C7_A; end

  error = false
  begin
    # Different superclass.
    class C7 < C7_B; end
  rescue TypeError
    error = true
  end

  error
end

assert('Class 8') do
  class C8_A; end

  class C8; end  # superclass is Object

  error = false
  begin
    # Different superclass.
    class C8 < C8_A; end
  rescue TypeError
    error = true
  end

  error
end

assert('Class 9') do
  Class9Const = "a"

  error = false
  begin
    class Class9Const; end
  rescue TypeError
    error = true
  end

  error
end

assert('Class Module 1') do
  module M; end
  M.class == Module
end

assert('Class Module 2') do
  module M; end
  class C; include M; end
  C.new.class == C
end

# nested class
assert('Class Nested 1') do
  class A; end
  class A::B; end
  A::B == A::B
end

assert('Class Nested 2') do
  class A; end
  class A::B; end
  A::B.new.class == A::B
end

assert('Class Nested 3') do
  class A; end
  class A::B; end
  A::B.new.class.class == Class
end

assert('Class Nested 4') do
  class A; end
  class A::B; end
  class A::B::C; end
  A::B::C == A::B::C
end

assert('Class Nested 5') do
  class A; end
  class A::B; end
  class A::B::C; end
  A::B::C.class == Class
end

assert('Class Nested 6') do
  class A; end
  class A::B; end
  class A::B::C; end
  A::B::C.new.class == A::B::C
end

assert('Class Nested 7') do
  class A; end
  class A::B; end
  class A::B2 < A::B; end
  A::B2 == A::B2
end

assert('Class Nested 8') do
  class A; end
  class A::B; end
  class A::B2 < A::B; end
  A::B2.class == Class
end

assert('Class Colon 1') do
  class A; end; A::C = 1; A::C == 1
end

assert('Class Colon 2') do
  class A; class ::C; end end; C == C
end

assert('Class Colon 3') do
  class A; class ::C; end end; C.class == Class
end

assert('Class Dup 1') do
  class C; end;  C.dup.class == Class
end

assert('Class Dup 2') do
  module M; end;  M.dup.class == Module
end

assert('Class Alias 1') do
  class A
    def test; 1; end

    alias test2 test
    alias :test3 :test
  end

  A.new.test2 == 1 and A.new.test3 == 1
end

assert('Class Alias 2') do
  class A
    def test; 1; end

    alias test2 test

    def test; 2; end
  end

  A.new.test == 2 and A.new.test2 == 1
end

assert('Class Undef 1') do
  class A
    def test1; 1; end
    def test2; 2; end

    undef test1
    undef :test2
  end

  result1 = false
  begin
    A.new.test1
  rescue NoMethodError
    result1 = true
  end

  result2 = false
  begin
    A.new.test2
  rescue NoMethodError
    result2 = true
  end

  result1 == true and result2 == true
end

assert('Class Undef 2') do
  class A
    def test1; 1; end
    def test2; 2; end

    undef test1, test2
  end

  result1 = false
  begin
    A.new.test1
  rescue NoMethodError
    result1 = true
  end

  result2 = false
  begin
    A.new.test2
  rescue NoMethodError
    result2 = true
  end

  result1 == true and result2 == true
end

