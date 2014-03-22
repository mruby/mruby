assert('__FILE__') do
  file = __FILE__
  assert_true 'test/t/syntax.rb' == file || 'test\t\syntax.rb' == file
end

assert('__LINE__') do
  assert_equal 7, __LINE__
end

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
  assert_equal [1,2,3], bar.bar(1,2,3)
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

  assert_equal 1, a
  assert_nil b
  assert_equal 3, c
end

assert('case expression', '11.5.2.2.4') do
  # case-expression-with-expression, one when-clause
  x = 0
  case "a"
  when "a"
    x = 1
  end
  assert_equal 1, x

  # case-expression-with-expression, multiple when-clauses
  x = 0
  case "b"
  when "a"
    x = 1
  when "b"
    x = 2
  end
  assert_equal 2, x

  # no matching when-clause
  x = 0
  case "c"
  when "a"
    x = 1
  when "b"
    x = 2
  end
  assert_equal 0, x

  # case-expression-with-expression, one when-clause and one else-clause
  a = 0
  case "c"
  when "a"
    x = 1
  else
    x = 3
  end
  assert_equal 3, x

  # case-expression-without-expression, one when-clause
  x = 0
  case
  when true
    x = 1
  end
  assert_equal 1, x

  # case-expression-without-expression, multiple when-clauses
  x = 0
  case
  when 0 == 1
    x = 1
  when 1 == 1
    x = 2
  end
  assert_equal 2, x

  # case-expression-without-expression, one when-clause and one else-clause
  x = 0
  case
  when 0 == 1
    x = 1
  else
    x = 3
  end
  assert_equal 3, x

  # multiple when-arguments
  x = 0
  case 4
  when 1, 3, 5
    x = 1
  when 2, 4, 6
    x = 2
  end
  assert_equal 2, x

  # when-argument with splatting argument
  x = :integer
  odds  = [ 1, 3, 5, 7, 9 ]
  evens = [ 2, 4, 6, 8 ]
  case 5
  when *odds
    x = :odd
  when *evens
    x = :even
  end
  assert_equal :odd, x

  true
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
  assert_equal "hello world", Syntax4Const::CONST1
  assert_equal "hello world", Syntax4Const::Const2.new.const1
end

assert('Abbreviated variable assignment as returns') do
  module Syntax4AbbrVarAsgnAsReturns
    class A
      def b
        @c ||= 1
      end
    end
  end
  assert_equal 1, Syntax4AbbrVarAsgnAsReturns::A.new.b
end

assert('Splat and mass assignment') do
  *a = *[1,2,3]
  b, *c = *[7,8,9]

  assert_equal [1,2,3], a
  assert_equal 7, b
  assert_equal [8,9], c
end

assert('Return values of case statements') do
  a = [] << case 1
  when 3 then 2
  when 2 then 2
  when 1 then 2
  end

  b = [] << case 1
  when 2 then 2
  else
  end

  def fb
    n = 0
    Proc.new do
      n += 1
      case
      when n % 15 == 0
      else n
      end
    end
  end

  assert_equal [2], a
  assert_equal [nil], b
  assert_equal 1, fb.call
end

assert('splat in case statement') do
  values = [3,5,1,7,8]
  testa = [1,2,7]
  testb = [5,6]
  resulta = []
  resultb = []
  resultc = []
  values.each do |value|
    case value
    when *testa
      resulta << value
    when *testb
      resultb << value
    else
      resultc << value
    end
  end

  assert_equal [1,7], resulta
  assert_equal [5], resultb
  assert_equal [3,8], resultc
end

# this must be removed when Kernel.` is implemented
assert('Not implemented backquote') do
  assert_raise(NotImplementedError) { `test` }
end

assert('External command execution.') do
  module Kernel
    sym = '`'.to_sym
    alias_method :old_cmd, sym

    results = []
    define_method(sym) do |str|
      results.push str
      str
    end

    `test`
    `test dynamic #{sym}`
    assert_equal ['test', 'test dynamic `'], results

    t = `test`
    assert_equal 'test', t
    assert_equal ['test', 'test dynamic `', 'test'], results

    t = `test dynamic #{sym}`
    assert_equal 'test dynamic `', t
    assert_equal ['test', 'test dynamic `', 'test', 'test dynamic `'], results

    alias_method sym, :old_cmd
  end
  assert_raise(NotImplementedError) { `test` }
end
