assert('__FILE__') do
  file = __FILE__[-9, 9]
  assert_equal 'syntax.rb', file
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

assert('super forwards the caller\'s block from inside a block') do
  # `super` reads the block from the frame of the method it belongs to.  From
  # inside a block that frame is reached through an env, and the level the
  # instruction carries counts the envs above this frame's own, one fewer
  # than the scopes the compiler walked to find the method.
  base = Class.new { def m; block_given? ? yield(:b) : :noblk; end }
  sub = Class.new(base) { def m; r = nil; [1].each { r = super }; r; end }
  assert_equal [:blk, :b], sub.new.m { |v| [:blk, v] }

  deep = Class.new(base) { def m; r = nil; [1].each { [2].each { r = super } }; r; end }
  assert_equal [:blk, :b], deep.new.m { |v| [:blk, v] }
end

assert('super forwards the keyword arguments at their current values') do
  # Each keyword parameter is moved into its local by deleting it from the
  # dictionary the frame received, so by the time `super` runs the dictionary
  # holds only what no parameter claimed, and it is the object `**rest`
  # names.  A bare `super` builds the parent's dictionary afresh from the
  # keyword locals and a copy of `rest`, as CRuby does.
  base = Class.new do
    def kw(a:, b: 2); [a, b]; end
    def rest(a:, **o); [a, o]; end
    def restonly(**o); o; end
    def all(x, *r, y, a:, b: 2, **o, &blk); [x, r, y, a, b, o, blk ? blk.call : nil]; end
  end
  sub = Class.new(base) do
    def kw(a:, b: 2); super; end
    def rest(a:, **o); super; end
    def restonly(**o); [super, o]; end
    def all(x, *r, y, a:, b: 2, **o, &blk); super; end
  end
  o = sub.new
  assert_equal [1, 3], o.kw(a: 1, b: 3)
  assert_equal [1, 2], o.kw(a: 1)
  assert_equal [1, {c: 3}], o.rest(a: 1, c: 3)
  assert_equal [1, [2], 3, 4, 2, {c: 5}, :blk], o.all(1, 2, 3, a: 4, c: 5) { :blk }

  # the parent's dictionary is a copy, so what its keyword parameters delete
  # from it stays in the child's `rest`
  parent_dict, own = o.restonly(a: 1)
  assert_equal({a: 1}, parent_dict)
  assert_equal({a: 1}, own)
  assert_false parent_dict.equal?(own)
  twice = Class.new(base) do
    def kw(a:, b: 2); [super, super]; end
    def rest(a:, **o); [super, super, o]; end
  end
  assert_equal [[1, 2], [1, 2]], twice.new.kw(a: 1)
  assert_equal [[1, {c: 2}], [1, {c: 2}], {c: 2}], twice.new.rest(a: 1, c: 2)

  # the locals are read where `super` is, so a keyword reassigned before it
  # reaches the parent reassigned, a declared keyword wins over a key of its
  # name in `rest`, and the child's default is what the parent sees when the
  # caller passed nothing
  reassign = Class.new(base) do
    def kw(a:, b: 2); a *= 10; b = :changed; super; end
    def rest(a:, **o); o = {a: 5, z: 1}; super; end
  end
  assert_equal [10, :changed], reassign.new.kw(a: 1)
  assert_equal [1, {z: 1}], reassign.new.rest(a: 1)
  defaults = Class.new(Class.new { def m(a: :parent); a; end }) { def m(a: :child); super; end }
  assert_equal :child, defaults.new.m

  # from inside a block, a nested block or a lambda, up more than one level,
  # with a block of its own, and through `...` and the anonymous `**`
  blk = Class.new(base) do
    def kw(a:, b: 2); r = nil; [0].each { r = super }; r; end
    def rest(a:, **o); r = nil; [0].each { [1].each { r = super } }; r; end
    def restonly(**o); -> { super }.call; end
    def all(x, *r, y, a:, b: 2, **o, &blk); super { :from_child }; end
  end
  assert_equal [1, 2], blk.new.kw(a: 1)
  assert_equal [1, {c: 2}], blk.new.rest(a: 1, c: 2)
  assert_equal({a: 1}, blk.new.restonly(a: 1))
  assert_equal [1, [], 2, 3, 2, {}, :from_child], blk.new.all(1, 2, a: 3)
  deep = Class.new(sub) { def kw(a:, b: 2); super; end }
  assert_equal [1, 3], deep.new.kw(a: 1, b: 3)
  fwd = Class.new(base) { def rest(...); super; end }
  assert_equal [1, {c: 2}], fwd.new.rest(a: 1, c: 2)
  anon = Class.new(base) { def rest(*, **, &); super; end }
  assert_equal [1, {c: 2}], anon.new.rest(a: 1, c: 2)
end

assert('yield', '11.3.5') do
# it's syntax error now
#  assert_raise LocalJumpError do
#    yield
#  end
  assert_raise LocalJumpError do
    o = Object.new
    def o.foo
      yield
    end
    o.foo
  end
end

assert('yield with keyword arguments') do
  # keywords must reach the block, alone and mixed with positional/splat args
  def ky_kw; yield b: true; end
  assert_equal [{b: true}], ky_kw { |*a| a }

  def ky_pos_kw; yield 1, b: true; end
  assert_equal [1, {b: true}], ky_pos_kw { |*a| a }

  def ky_splat_kw; yield 1, *[2], b: true; end
  assert_equal [1, 2, {b: true}], ky_splat_kw { |*a| a }

  # a block declaring keyword parameters receives them as keywords
  def ky_decl; yield 1, x: 2, y: 3; end
  assert_equal [1, 2, 3], ky_decl { |a, x:, y:| [a, x, y] }
end

assert('break', '11.5.2.4.3') do
  n = 0
  a = []
  while true
    n += 1
    a.push(n)
    if n > 3
      break
    end
  end

  assert_equal [1,2,3,4], a

  n = 0
  a = []
  6.times do
    n += 1
    a.push(n)
    if n > 3
      break
    end
  end
  assert_equal [1,2,3,4], a

  a = []
  begin
    while true
      a.push 1
      break
      a.push "NG"
    end
  ensure
    a.push 2
  end
  assert_equal [1, 2], a

  a = []
  begin
    while true
      a.push 1
      break
    end
    a.push 2
  ensure
    a.push 3
  end
  assert_equal [1, 2, 3], a

  a = []
  begin
    while true
      begin
        a.push 1
        break
      ensure
        a.push 2
      end
      a.push "NG"
    end
  ensure
    a.push 3
  end
  assert_equal [1, 2, 3], a

  a = []
  begin
    while true
      begin
        a.push 1
        break
      ensure
        a.push 2
      end
      a.push "NG"
    end
    a.push 3
  ensure
    a.push 4
  end
  assert_equal [1, 2, 3, 4], a
end

assert('next with value from a block') do
  def next_yield; yield; end
  # next v returns v itself, not [v]
  assert_equal 234, (next_yield { next 234 })
  # multiple values become an array
  assert_equal [1, 2], (next_yield { next 1, 2 })
  # a splat argument is expanded, not double-wrapped
  assert_equal [7, 8], (next_yield { next *[7, 8] })
  # bare next yields nil
  assert_nil (next_yield { next })
  # next as loop control still discards its value
  assert_equal [1, 20, 3], [1, 2, 3].map { |x| next x * 10 if x == 2; x }
end

assert('safe navigation operator-assignment short-circuits on nil') do
  # a nil receiver yields nil without invoking the read or write method
  assert_nil (nil&.foo += 5)
  assert_nil (nil&.foo ||= 5)
  assert_nil (nil&.foo &&= 5)

  # the right-hand side is not evaluated when the receiver is nil
  evaluated = false
  nil&.foo += (evaluated = true; 1)
  assert_false evaluated

  # a non-nil receiver still performs the operation
  acc = Class.new { attr_accessor :x }
  c = acc.new
  c.x = 10
  assert_equal 15, (c&.x += 5)
  assert_equal 15, c.x

  d = acc.new
  d&.x ||= 7
  assert_equal 7, d.x
end

class SelfSafeCall
  def y(*); :called; end
  # `[nil].first` leaves nil in the first temporary register, which is
  # the one the nil check of the call below reads when the receiver is
  # not loaded; a receiver written as `self` is never nil
  def bare;  [nil].first; self&.y;     end
  def args;  [nil].first; self&.y(1);  end
  def value; [nil].first; x = self&.y; x; end
end

assert('a safe navigation call on a written self is made') do
  o = SelfSafeCall.new
  assert_equal :called, o.bare
  assert_equal :called, o.args
  assert_equal :called, o.value
end

assert('local variable or/and-assignment yields its value') do
  # gen_assignment_lvar() only moves, so the local-variable branch has to push
  # the result the way the other branches do. Without it the expression yields
  # nothing and every later register is off by one, which the VM catches as
  # `bidx < irep->nregs`.
  q = 1;   assert_equal 1, (q ||= 7); assert_equal 1, q
  r = nil; assert_equal 7, (r ||= 7); assert_equal 7, r
  s = 1;   assert_equal 7, (s &&= 7); assert_equal 7, s
  t = nil; assert_nil (t &&= 7)

  # in argument position, where the register slip used to assert
  u = 1
  assert_equal [1], [].push(u ||= 7)
  v = 1
  assert_equal [1, 8], [(v ||= 7), 8]

  # combined with a splat argument (clusterfuzz 6212427713413120)
  w = 1
  assert_equal [1, :h], [(w ||= 7), *[:h]]

  # and in a nested scope, through an upvar
  outer = nil
  [1].each { outer ||= 5 }
  assert_equal 5, outer
end

assert('attribute or/and-assignment persists the write in a value context') do
  acc = Class.new { attr_accessor :x }

  # ||= writing when the value is used (e.g. as a method argument): the
  # peephole must not hoist the RHS out of the write's argument register
  c = acc.new
  assert_equal 7, (c.x ||= 7)
  assert_equal 7, c.x

  # ||= skipping when the attribute is already truthy
  c2 = acc.new
  c2.x = 5
  assert_equal 5, (c2.x ||= 7)
  assert_equal 5, c2.x

  # &&= writing when truthy
  c3 = acc.new
  c3.x = 3
  assert_equal 9, (c3.x &&= 9)
  assert_equal 9, c3.x
end

assert('redo', '11.5.2.4.5') do
  sum = 0
  for i in 1..10
    sum += i
    i -= 1
    if i > 0
      redo
    end
  end

  assert_equal 220, sum

  n = 0
  a = []
  3.times do
    n += 1
    if n == 2
      redo
    end
    a.push(n)
  end
  assert_equal [1,3,4], a

  a = []
  limit = 3
  e = RuntimeError.new("!")
  for i in 0...3
    begin
      limit -= 1
      break unless limit > 0
      a.push i * 3 + 1
      raise e
    rescue
      a.push i * 3 + 2
      redo
    ensure
      a.push i * 3 + 3
    end
  end
  assert_equal [1, 2, 3, 1, 2, 3, 3], a

  a = []
  limit = 3
  e = RuntimeError.new("!")
  for i in 0...3
    a.push i * 4 + 1
    begin
      limit -= 1
      break unless limit > 0
      a.push i * 4 + 2
      raise e
    rescue
      a.push i * 4 + 3
      redo
    ensure
      a.push i * 4 + 4
    end
  end
  assert_equal [1, 2, 3, 4, 1, 2, 3, 4, 1, 4], a
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
  assert_raise(NameError) { Syntax4Const::Object }
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

assert('Abbreviated variable assignment of object attribute') do
  module Syntax4AbbrVarAsgnObjectAttr
    class A
      attr_accessor :c
      def b
        self.c ||= 1
      end
    end
  end
  assert_equal 1, Syntax4AbbrVarAsgnObjectAttr::A.new.b
end

assert('Splat and multiple assignment') do
  *a = *[1,2,3]
  b, *c = *[7,8,9]

  assert_equal [1,2,3], a
  assert_equal 7, b
  assert_equal [8,9], c

  (a, b), c = [1,2],3
  assert_equal [1,2,3], [a,b,c]
  (a, b), c = 1,2,3
  assert_equal [1,nil,2], [a,b,c]
end

assert('Splat and multiple assignment from variable') do
  a = [1, 2, 3]
  b, *c = a

  assert_equal 1, b
  assert_equal [2, 3], c
end

assert('Splat and multiple assignment from variables') do
  a = [1, 2, 3]
  b = [4, 5, 6, 7]
  c, d, *e, f, g = *a, *b

  assert_equal 1, c
  assert_equal 2, d
  assert_equal [3, 4, 5], e
  assert_equal 6, f
  assert_equal 7, g
end

assert('Splat and multiple assignment in for') do
  a = [1, 2, 3, 4, 5, 6, 7]
  for b, c, *d, e, f in [a] do
  end

  assert_equal 1, b
  assert_equal 2, c
  assert_equal [3, 4, 5], d
  assert_equal 6, e
  assert_equal 7, f
end

assert('Splat without assignment') do
  * = [0]
  a, * = [1, 2]
  assert_equal 1, a
end

assert('multiple assignment (rest)') do
  *a = 0
  assert_equal [0], a
end

assert('multiple assignment (index targets)') do
  a = [1, 2]
  a[0], a[1] = 9, 8
  assert_equal [9, 8], a

  a[0], a[1] = a[1], a[0]
  assert_equal [8, 9], a

  h = {}
  h[:x], h[:y] = 1, 2
  assert_equal({:x => 1, :y => 2}, h)

  b = Array.new(4, 0)
  b[0, 2], b[2, 2] = [1, 2], [3, 4]
  assert_equal [1, 2, 3, 4], b
end

assert('multiple assignment (rest+post)') do
  *a, b = 0, 1, 2
  *c, d = 3

  assert_equal [0, 1], a
  assert_equal 2, b
  assert_equal [], c
  assert_equal 3, d
end

assert('multiple assignment (nosplat array rhs)') do
  a, *b = []
  *c, d = [0]
  e, *f, g = [1, 2]

  assert_nil a
  assert_equal [], b
  assert_equal [], c
  assert_equal 0, d
  assert_equal 1, e
  assert_equal [], f
  assert_equal 2, g
end

assert('multiple assignment (empty array rhs #3236, #3239)') do
  a,b,*c = []; assert_equal [nil, nil, []], [a, b, c]
  a,b,*c = [1]; assert_equal [1, nil, []], [a, b, c]
  a,b,*c = [nil]; assert_equal [nil,nil, []], [a, b, c]
  a,b,*c = [[]]; assert_equal [[], nil, []], [a, b, c]
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

assert('Return values of if and case statements') do
  true_clause_value =
    if true
      1
    else
      case 2
      when 3
      end
      4
    end

  assert_equal 1, true_clause_value
end

assert('Return values of no expression case statement') do
  when_value =
    case
    when true
      1
    end

  assert_equal 1, when_value
end

assert('splat object in assignment') do
  o = Object.new
  def o.to_a
    nil
  end
  assert_equal [o], (a = *o)

  def o.to_a
    1
  end
  assert_raise(TypeError) { a = *o }

  def o.to_a
    [2]
  end
  assert_equal [2], (a = *o)
end

assert('one-line pattern match') do
  1 => a
  assert_equal(1, a)
end

assert('splat object in case statement') do
  o = Object.new
  def o.to_a
    nil
  end
  a = case o
  when *o
    1
  end
  assert_equal 1, a
end

assert('splat in case statement with a pattern that replaces the array') do
  # #=== runs while the splatted array is being walked, and replacing it with a
  # shorter one moves the buffer out from under the traversal.
  pat = Class.new do
    def initialize(a); @a = a; end
    def ===(o); @a.replace(Array.new(64, 0)); false; end
  end
  a = []
  400.times { a << pat.new(a) }
  r = case 1
      when *a then :matched
      else :none
      end
  assert_equal :none, r
  assert_equal 64, a.size
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

assert('External command execution.') do
  module Kernel
    sym = '`'.to_sym
    alias_method :old_cmd, sym

    results = []
    define_method(sym) do |str|
      results.push str
      str
    end

    `test` # NOVAL NODE_XSTR
    `test dynamic #{sym}` # NOVAL NODE_DXSTR
    assert_equal ['test', 'test dynamic `'], results

    t = `test` # VAL NODE_XSTR
    assert_equal 'test', t
    assert_equal ['test', 'test dynamic `', 'test'], results

    t = `test dynamic #{sym}` # VAL NODE_DXSTR
    assert_equal 'test dynamic `', t
    assert_equal ['test', 'test dynamic `', 'test', 'test dynamic `'], results

    results = []
    assert_equal 'test sym test sym test', `test #{:sym} test #{:sym} test`

    alias_method sym, :old_cmd
  end
  true
end

assert('parenthesed do-block in cmdarg') do
  class ParenDoBlockCmdArg
    def test(block)
      block.call
    end
  end
  x = ParenDoBlockCmdArg.new
  result = x.test (Proc.new do :ok; end)
  assert_equal :ok, result
end

assert('method definition in cmdarg') do
  result = class MethodDefinitionInCmdarg
    def self.bar(arg); arg end
    bar def foo; self.each do end end
  end
  assert_equal(:foo, result)
end

assert('optional argument in the rhs default expressions') do
  class OptArgInRHS
    def foo
      "method called"
    end
    def t(foo = foo)
      foo
    end
    def t2(foo = foo())
      foo
    end
  end
  o = OptArgInRHS.new
  assert_nil(o.t)
  assert_equal("method called", o.t2)
end

assert('optional block argument in the rhs default expressions') do
  assert_nil(Proc.new {|foo = foo| foo}.call)
end

assert('local variable definition in default value and subsequent arguments') do
  def m(a = b = 1, c) [a, b, c] end
  assert_equal([1, 1, :c], m(:c))
  assert_equal([:a, nil, :c], m(:a, :c))

  def m(a = b = 1, &c) [a, b, c ? true : nil] end
  assert_equal([1, 1, nil], m)
  assert_equal([1, 1, true], m{})
  assert_equal([:a, nil, nil], m(:a))
  assert_equal([:a, nil, true], m(:a){})
end

assert('destructured parameter read from a later default expression') do
  def m(a, (b, c), d) [a, b, c, d] end
  assert_equal([1, 2, 3, 4], m(1, [2, 3], 4))

  # the names are locals from the start, so a default evaluated before the
  # destructuring assignment reads nil rather than calling a method
  def m(a, (b, c), d = b) [a, b, c, d] end
  assert_equal([1, 2, 3, nil], m(1, [2, 3]))
  assert_equal([1, 2, 3, 4], m(1, [2, 3], 4))

  def m((a, (b, c)), d = c) [a, b, c, d] end
  assert_equal([1, 2, 3, nil], m([1, [2, 3]]))

  def m((a, b), c: a) [a, b, c] end
  assert_equal([1, 2, nil], m([1, 2]))
  assert_equal([1, 2, 3], m([1, 2], c: 3))

  # a keyword default may read a keyword assigned before it
  def m(a: 1, b: a) [a, b] end
  assert_equal([1, 1], m)
  assert_equal([2, 2], m(a: 2))
  assert_equal([1, 9], m(b: 9))
end

assert('multiline comments work correctly') do
=begin
this is a comment with nothing after begin and end
=end
=begin  this is a comment
this is a comment with extra after =begin
=end
=begin
this is a comment that has =end with spaces after it
=end
=begin this is a comment
this is a comment that has extra after =begin and =end with spaces after it
=end
  line = __LINE__
=begin  this is a comment
this is a comment that has extra after =begin and =end with tabs after it
=end  xxxxxxxxxxxxxxxxxxxxxxxxxx
  assert_equal(line + 4, __LINE__)
end

assert 'keyword arguments' do
  def m(a, b:1) [a, b] end
  assert_equal [1, 1], m(1)
  assert_equal [1, 2], m(1, b: 2)

  def m(a, b:) [a, b] end
  assert_equal [1, 2], m(1, b: 2)
  assert_raise(ArgumentError) { m b: 1 }
  assert_raise(ArgumentError) { m 1 }

  def m(a:) a end
  assert_equal 1, m(a: 1)
  assert_raise(ArgumentError) { m }
  assert_raise(ArgumentError) { m 'a'  => 1, a: 1 }
  h = { a: 1 }
  assert_equal 1, m(**h)

  def m(a: 1) a end
  assert_equal 1, m
  assert_equal 2, m(a: 2)
  assert_raise(ArgumentError) { m 1 }

  def m(**) end
  assert_nil m
  assert_nil m a: 1, b: 2
  assert_raise(ArgumentError) { m 2 }

  def m(a, **) a end
  assert_equal 1, m(1)
  assert_equal 1, m(1, a: 2, b: 3)
  assert_raise(ArgumentError) { m('a' => 1, b: 2) }

  def m(a, **k) [a, k] end
  assert_equal [1, {}], m(1)
  assert_equal [1, {a: 2, b: 3}], m(1, a: 2, b: 3)
  assert_raise(ArgumentError) { m('a' => 1, b: 2) }

  def m(a=1, **) a end
  assert_equal 1, m
  assert_equal 2, m(2, a: 1, b: 0)

  def m(a=1, **k) [a, k] end
  assert_equal [1, {}], m
  assert_equal [1, {a: 1}], m(a: 1)
  assert_equal [2, {a: 1, b: 2}], m(2, a: 1, b: 2)
  assert_equal [{a: 1}, {b: 2}], m({a: 1}, b: 2)
  assert_raise(ArgumentError) { m({a: 1}, {b: 2}) }

  def m1(a: {}) a end
  assert_equal({}, m1)
  assert_equal(:abc, m1(a: :abc))

  def m2(a: +1) a end
  assert_equal(1, m2)

  assert_nothing_raised do
    def m3 arg:
      123
    end
  end
  assert_equal(123, m3(arg: 456))

  def m(*, a:) a end
  assert_equal 1, m(a: 1)
  assert_equal 3, m(1, 2, a: 3)
  assert_raise(ArgumentError) { m('a' => 1, a: 2) }

  def m(*a, b:) [a, b] end
  assert_equal [[], 1], m(b: 1)
  assert_equal [[1, 2], 3], m(1, 2, b: 3)
  assert_raise(ArgumentError) { m('a' => 1, b: 2) }

  def m(*a, b: 1) [a, b] end
  assert_equal [[], 1], m
  assert_equal [[1, 2, 3], 4], m(1, 2, 3, b: 4)
  assert_raise(ArgumentError) { m('a' => 1, b: 2) }

  def m(*, **) end
  assert_nil m()
  assert_nil m(a: 1, b: 2)
  assert_nil m(1, 2, 3, a: 4, b: 5)

  def m(*a, **) a end
  assert_equal [], m()
  assert_equal [1, 2, 3], m(1, 2, 3, a: 4, b: 5)
  assert_equal [1], m(1, **{a: 2})

  def m(*, **k) k end
  assert_equal({}, m())
  assert_equal({a: 4, b: 5}, m(1, 2, 3, a: 4, b: 5))

  def m(a = nil, b = nil, **k) [a, k] end
  assert_equal [nil, {}], m()
  assert_equal([nil, {a: 1}], m(a: 1))
  assert_equal([{"a" => 1}, {a: 1}], m({ "a" => 1 }, a: 1))
  assert_equal([{a: 1}, {}], m({a: 1}, {}))
  assert_equal([{}, {}], m({}))

  def m(*a, **k) [a, k] end
  assert_equal([[], {}], m())
  assert_equal([[1], {}], m(1))
  assert_equal([[], {a: 1, b: 2}], m(a: 1, b: 2))
  assert_equal([[1, 2, 3], {a: 2}], m(1, 2, 3, a: 2))
  assert_equal([[], {a: 1}], m(a: 1))
  assert_equal([[{"a" => 1}], {a: 1}], m({ "a" => 1 }, a: 1))
  assert_equal([[{a: 1}, {}], {}], m({a: 1}, {}))

  def m(a:, b:) [a, b] end
  assert_equal([1, 2], m(a: 1, b: 2))
  assert_raise(ArgumentError) { m("a" => 1, a: 1, b: 2) }

  def m(a:, b: 1) [a, b] end
  assert_equal([1, 1], m(a: 1))
  assert_equal([1, 2], m(a: 1, b: 2))
  assert_raise(ArgumentError) { m(b: 1) }
  assert_raise(ArgumentError) { m("a" => 1, a: 1, b: 2) }

  def m(a:, **) a end
  assert_equal(1, m(a: 1))
  assert_equal(1, m(a: 1, b: 2))

  def m(a:, **k) [a, k] end
  assert_equal([1, {}], m(a: 1))
  assert_equal([1, {b: 2, c: 3}], m(a: 1, b: 2, c: 3))

  def m(a:, &b) [a, b] end
  assert_equal([1, nil], m(a: 1))
  result = m(a: 1, &(l = ->{}))
  assert_equal([1, l], result)

  def m(a: 1, b:) [a, b] end
  assert_equal([1, 0], m(b: 0))
  assert_equal([3, 2], m(b: 2, a: 3))
  assert_raise(ArgumentError) { m a: 1 }

  def m(a: def m(a: 1) a end, b:)
    [a, b]
  end
  assert_equal([2, 3], m(a: 2, b: 3))
  assert_equal([:m, 1], m(b: 1))
  # Note the default value of a: in the original method.
  assert_equal(1, m())

  def m(a: 1, b: 2) [a, b] end
  assert_equal([1, 2], m())
  assert_equal([4, 3], m(b: 3, a: 4))

  def m(a: 1, **) a end
  assert_equal(1, m())
  assert_equal(2, m(a: 2, b: 1))

  def m(a: 1, **k) [a, k] end
  assert_equal([1, {b: 2, c: 3}], m(b: 2, c: 3))

  def m(a:, **) yield end
  assert_raise(ArgumentError) { m { :blk } }
  assert_equal :blk, m(a: 1){ :blk }

  def m(a:, **k, &b) [b.call, k] end
  assert_raise(ArgumentError) { m { :blk } }
  assert_equal [:blk, {b: 2}], m(a: 1, b: 2){ :blk }

  def m(**k, &b) [k, b] end
  assert_equal([{ a: 1, b: 2}, nil], m(a: 1, b: 2))
  assert_equal :blk, m{ :blk }[1].call

=begin
  def m(a, b=1, *c, (*d, (e)), f: 2, g:, h:, **k, &l)
    [a, b, c, d, e, f, g, h, k, l]
  end
  result = m(9, 8, 7, 6, f: 5, g: 4, h: 3, &(l = ->{}))
  assert_equal([9, 8, [7], [], 6, 5, 4, 3, {}, l], result)
  def m a, b=1, *c, d, e:, f: 2, g:, **k, &l
    [a, b, c, d, e, f, g, k, l]
  end
  result = m(1, 2, e: 3, g: 4, h: 5, i: 6, &(l = ->{}))
  assert_equal([1, 1, [], 2, 3, 2, 4, { h: 5, i: 6 }, l], result)
=end

  def m(a: b = 1, c:) [a, b, c] end
  assert_equal([1, 1, :c], m(c: :c))
  assert_equal([:a, nil, :c], m(a: :a, c: :c))
end

assert('numbered parameters') do
  assert_equal(15, [1,2,3,4,5].reduce {_1+_2})
  assert_equal(45, Proc.new do _1 + _2 + _3 + _4 + _5 + _6 + _7 + _8 + _9 end.call(*[1, 2, 3, 4, 5, 6, 7, 8, 9]))
  assert_equal(5, -> { _1 }.call(5))
end

assert('numbered parameter after ordinary local (#6921)') do
  # a local assigned before the first _1 reference used to steal _1's
  # register, leaving the numbered parameter nil
  result = nil
  [1].each { tmp = _1 + 1; result = tmp }
  assert_equal(2, result)

  [[1, 2]].each { t = _2; result = [_1, t] }
  assert_equal([1, 2], result)
end

assert('_0 is not numbered parameter') do
  _0 = :l
  assert_equal(:l, ->{_0}.call)
end

assert('numbered parameters in symbol name (https://github.com/mruby/mruby/issues/5295)') do
  assert_equal([:_1], Array.new(1) {:_1})
end

assert('numbered parameters as hash key') do
  h = {_1: 3}
  assert_equal(3, h[:_1])
  assert_equal(7, -> { _1 }.call(7))
end

assert('numbered parameters as singleton') do
  o = Object.new
  lambda { def _1.a(b) = "a#{b}" }.call(o)
  assert_equal('ab', o.a('b'))
end

assert('argument forwarding') do
  c = Class.new {
    def a0(*a,&b)
      assert_equal([1,2,3], a)
      assert_not_nil(b)
    end
    def a(...)
      a0(...)
    end
    def b(a,...)
      assert_equal(a,1)
      a0(1,...)
    end
    def c ...
      a(...)
    end
    def d a,...
      assert_equal(a,1)
      b(1,...)
    end
    def e(...)
      [1].each { a0(...) }
    end
  }
  o = c.new
  o.a(1,2,3){}
  o.b(1,2,3){}
  o.c(1,2,3){}
  o.d(1,2,3){}
  o.e(1,2,3){}   # forwarding from inside a block
end

assert('endless def') do
  c = Class.new {
    def m1 = 42
    def m2() = 42
    def m3(x) = x+1
    def self.s1 = 42
    def self.s2() = 42
    def self.s3(x) = x + 1
    def cm1 = m3 42
    def cm2() = m3 42
    def cm3(x) = m3 x+1
    def self.cs1 = s3 42
    def self.cs2() = s3 42
    def self.cs3(x) = s3 x + 1
  }
  o = c.new
  assert_equal(42, o.m1)
  assert_equal(43, o.m3(o.m2))
  assert_equal(42, c.s1)
  assert_equal(43, c.s3(c.s2))
  assert_equal(43, o.cm1)
  assert_equal(45, o.cm3(o.cm2))
  assert_equal(43, c.cs1)
  assert_equal(45, c.cs3(c.cs2))
end

assert('at-least-once loop') do
  # basic at-least-once loop that executes once
  count = 0
  begin
    count += 1
  end while false
  assert_equal 1, count

  # at-least-once loop with true condition
  count = 0
  begin
    count += 1
  end while count < 3
  assert_equal 3, count

  # at-least-once loop with break
  count = 0
  begin
    count += 1
    break if count == 2
  end while count < 10
  assert_equal 2, count

  # at-least-once loop with next
  count = 0
  sum = 0
  begin
    count += 1
    if count == 2
      next
    end
    sum += count
  end while count < 4
  assert_equal 4, count
  assert_equal 8, sum  # 1 + 3 + 4 = 8 (skipped 2)

  # nested at-least-once loops
  outer_count = 0
  total = 0
  begin
    outer_count += 1
    inner_count = 0
    begin
      inner_count += 1
      total += inner_count
    end while inner_count < 2
  end while outer_count < 2
  assert_equal 2, outer_count
  assert_equal 6, total  # (1+2) + (1+2) = 6

  # at-least-once loop with exception handling
  count = 0
  begin
    count += 1
    raise "error" if count == 2
  rescue
    count += 10
  end while count < 5
  assert_equal 12, count  # 1, then 2+10=12
end

assert('pattern matching - basic case/in') do
  # literal patterns
  result = case 1
  in 1 then :one
  in 2 then :two
  end
  assert_equal :one, result

  # variable binding
  case 42
  in x
    assert_equal 42, x
  end

  # else clause
  result = case 3
  in 1 then :one
  in 2 then :two
  else :other
  end
  assert_equal :other, result
end

assert('pattern matching - string literal patterns') do
  result = case "hello"
           in "hello" then :match
           end
  assert_equal :match, result

  # double-quoted vs single-quoted should both work
  result = case 'world'
           in "world" then :double
           end
  assert_equal :double, result

  # alternation with strings
  result = case "ab"
           in "ab" | "cd" then :alt
           end
  assert_equal :alt, result

  # string interpolation in pattern
  expected = "lo"
  result = case "hello"
           in "hel#{expected}" then :interp
           end
  assert_equal :interp, result
end

assert('pattern matching - array patterns') do
  # simple array pattern
  case [1, 2, 3]
  in [a, b, c]
    assert_equal 1, a
    assert_equal 2, b
    assert_equal 3, c
  end

  # array pattern with rest
  case [1, 2, 3, 4, 5]
  in [first, *rest]
    assert_equal 1, first
    assert_equal [2, 3, 4, 5], rest
  end

  # array pattern with rest in middle
  case [1, 2, 3, 4, 5]
  in [first, *middle, last]
    assert_equal 1, first
    assert_equal [2, 3, 4], middle
    assert_equal 5, last
  end

  # nested array pattern
  case [[1, 2], [3, 4]]
  in [[a, b], [c, d]]
    assert_equal [1, 2, 3, 4], [a, b, c, d]
  end

  # array pattern with literal match
  result = case [1, 2, 3]
  in [1, 2, x]
    x
  end
  assert_equal 3, result

  # array literal with splat as case value (#6854):
  # the array-literal-length optimization must bail out for splat,
  # since the runtime length is unknown statically.
  a = [1, 2]
  result = case [*a]
           in [1, 2] then :match
           else :nomatch
           end
  assert_equal :match, result

  # same bug in one-line `in` pattern
  assert_true ([*a] in [1, 2])
  assert_false ([*a] in [1, 2, 3])
  assert_true ([1, *a, 4] in [1, 1, 2, 4])
end

assert('pattern matching - find patterns') do
  # find pattern - element at end
  case [1, 2, 3]
  in [*pre, 3, *post]
    assert_equal [1, 2], pre
    assert_equal [], post
  end

  # find pattern - element at beginning
  case [1, 2, 3]
  in [*pre, 1, *post]
    assert_equal [], pre
    assert_equal [2, 3], post
  end

  # find pattern - element in middle
  case [1, 2, 3, 4, 5]
  in [*pre, 3, *post]
    assert_equal [1, 2], pre
    assert_equal [4, 5], post
  end

  # find pattern - multiple middle elements
  case [1, 2, 3, 4, 5]
  in [*pre, 2, 3, *post]
    assert_equal [1], pre
    assert_equal [4, 5], post
  end

  # find pattern - anonymous rest
  result = case [1, 2, 3, 4, 5]
  in [*, 3, *]
    :found
  else
    :not_found
  end
  assert_equal :found, result

  # find pattern - no match
  result = case [1, 2, 4, 5]
  in [*pre, 3, *post]
    :found
  else
    :not_found
  end
  assert_equal :not_found, result

  # find pattern with literal values
  case [1, 2, 3, 4, 5]
  in [*pre, 2, x, *post]
    assert_equal [1], pre
    assert_equal 3, x
    assert_equal [4, 5], post
  end
end

assert('pattern matching - a key that moves the subject') do
  # A key's #== and #eql? run while the pattern helpers are walking the key
  # array and the subject hash, and both were held as raw storage across the
  # call. Rehashing the subject is refused by the Hash implementation's own
  # check; replacing the key array is allowed, so the walk has to keep up.
  moving = Class.new do
    attr_accessor :owner, :arr, :armed, :found
    def initialize(id); @id = id; @armed = false; @found = false; end
    def hash; @id; end
    def ==(_)
      if @armed
        @armed = false
        @arr.replace(Array.new(64) { |j| self.class.new(j + 100) }) if @arr
        2_000.times { |i| @owner[i + 10_000] = i } if @owner
      end
      @found
    end
    alias eql? ==
  end

  # `in {a: 1}` reaches __pat_values, which walks the key array. The lookup
  # keys are separate objects from the stored ones, so the hash has to ask
  # #eql? rather than settle it by identity.
  h1 = {}
  30.times { |i| h1[moving.new(i + 1)] = i }
  keys = Array.new(30) { |i| moving.new(i + 1) }
  keys.each { |k| k.arr = keys; k.found = true }
  keys[0].armed = true
  # The replacement keys are not in the hash, so the lookup fails; what
  # matters is that the walk follows the array it was given rather than the
  # buffer it started with.
  assert_false h1.__pat_values(keys)

  # `**rest` reaches __except, which walks the subject hash as well
  h2 = {a: 1}
  ks = Array.new(30) { |i| k = moving.new(i + 1); h2[k] = k; k }
  ks.each { |k| k.owner = h2 }
  ks[0].armed = true
  assert_raise(RuntimeError) { h2.__except([:a]) }
end

assert('pattern matching - hash patterns') do
  # simple hash pattern
  case {a: 1, b: 2}
  in {a: x, b: y}
    assert_equal 1, x
    assert_equal 2, y
  end

  # shorthand hash pattern
  case {a: 1, b: 2}
  in {a:, b:}
    assert_equal 1, a
    assert_equal 2, b
  end

  # hash pattern with extra keys (partial match)
  case {a: 1, b: 2, c: 3}
  in {a: x}
    assert_equal 1, x
  end

  # hash pattern with rest (captures unmatched keys)
  case {a: 1, b: 2, c: 3}
  in {a:, **rest}
    assert_equal 1, a
    assert_equal({b: 2, c: 3}, rest)
  end

  # hash value extraction
  h = {user: "Alice"}
  case h
  in {user: u}
    assert_equal "Alice", u
  end
end

assert('pattern matching - a clause that names `**rest` keeps its own value') do
  # Capturing `**rest` left the frame one register short of where the pattern
  # began, so the value the clause produced landed where the `case` read it
  # from, and what came out was the rest Hash, or whatever else that register
  # held.
  h = {a: 1, b: 2, c: 3}
  r = case h
      in {a:, **rest} then [a, rest]
      end
  assert_equal [1, {b: 2, c: 3}], r

  r = case h
      in {a:, **rest} then :body
      end
  assert_equal :body, r

  # no key of its own: the rest is a copy of the whole subject
  r = case h
      in {**rest} then [:all, rest]
      end
  assert_equal [:all, {a: 1, b: 2, c: 3}], r

  # the value of `in` is the match, not the captured Hash
  r = (h in {a:, **rest})
  assert_equal [true, {b: 2, c: 3}], [r, rest]

  # the statement after `=>` reads its locals from the shifted frame
  f = ->(x) { x => {a:, **rest}; [a, rest] }
  assert_equal [1, {b: 2, c: 3}], f.call(h)

  # a rest clause that does not match must leave the subject to the next
  # clause and to `else`
  r = case {z: 0}
      in {a:, **rest} then :no
      in {z:} then [:z, z]
      end
  assert_equal [:z, 0], r

  r = case {z: 0}
      in {a:, **rest} then :no
      else :else
      end
  assert_equal :else, r

  # each `**rest` of a nested pattern captures from its own subject
  r = case [h, h]
      in [{a:, **r1}, {b:, **r2}] then [a, r1, b, r2]
      end
  assert_equal [1, {b: 2, c: 3}, 2, {a: 1, c: 3}], r
end

assert('pattern matching - value patterns as a hash value') do
  # a value pattern is the receiver of `===`, the hash value its argument
  case {a: 1}
  in {a: Integer}
    assert_true true
  else
    flunk "Integer did not match the value 1"
  end

  case {a: "s"}
  in {a: Integer}
    flunk "Integer matched the value \"s\""
  else
    assert_true true
  end

  # a range is asymmetric the same way
  case {a: 1}
  in {a: 0..2}
    assert_true true
  else
    flunk "0..2 did not match the value 1"
  end

  # binding after a class pattern
  case {a: 1, b: {c: 2}}
  in {a: Integer => x, b: {c: Integer => y}}
    assert_equal 1, x
    assert_equal 2, y
  else
    flunk "nested class patterns did not match"
  end

  # the same pattern nested inside an array pattern
  case [{a: 1}]
  in [{a: Integer}]
    assert_true true
  else
    flunk "class pattern in a nested hash did not match"
  end

  # the value keeps its register for the patterns that follow
  case {a: 1, b: 2}
  in {a: Integer, b: Integer => y}
    assert_equal 2, y
  else
    flunk "two class patterns in one hash did not match"
  end
end

assert('pattern matching - guard clauses') do
  # if guard
  result = case 10
  in x if x > 5
    :big
  in x
    :small
  end
  assert_equal :big, result

  # unless guard
  result = case 3
  in x unless x > 5
    :small
  in x
    :big
  end
  assert_equal :small, result

  # guard with pattern
  result = case [1, 2, 3]
  in [a, b, c] if a + b + c > 5
    :sum_big
  in [a, b, c]
    :sum_small
  end
  assert_equal :sum_big, result
end

assert('pattern matching - alternative patterns') do
  # simple alternatives
  result = case 2
  in 1 | 2 | 3
    :found
  else
    :not_found
  end
  assert_equal :found, result

  # alternatives in array
  result = case [1, 2]
  in [1, 2] | [3, 4]
    :match
  else
    :no_match
  end
  assert_equal :match, result

  # an unimplemented pattern (`self`) on the left of `|` used to corrupt the
  # bytecode through the JMPNOT->JMPIF peephole and crash the VM; it must just
  # fail to match and fall through to the right alternative.
  result = case 42
  in self | _
    :matched
  else
    :no
  end
  assert_equal :matched, result
end

assert('pattern matching - pin operator') do
  x = 1
  # pin matches exact value
  result = case 1
  in ^x
    :matched
  else
    :not_matched
  end
  assert_equal :matched, result

  # pin in array pattern
  expected = 42
  case [42, 100]
  in [^expected, y]
    assert_equal 100, y
  end

  # pin prevents rebinding
  a = 1
  result = case 2
  in ^a
    :same
  else
    :different
  end
  assert_equal :different, result
end

assert('pattern matching - as pattern') do
  # bind entire match
  case [1, 2, 3]
  in [x, *rest] => whole
    assert_equal 1, x
    assert_equal [2, 3], rest
    assert_equal [1, 2, 3], whole
  end

  # as pattern with hash
  case {a: 1, b: 2}
  in {a:} => h
    assert_equal 1, a
    assert_equal({a: 1, b: 2}, h)
  end
end

assert('pattern matching - one-line in') do
  # basic true/false
  assert_true((1 in 1))
  assert_false((1 in 2))

  # with variable binding
  assert_true(([1, 2] in [x, y]))

  # in conditional
  matched = false
  if [1, 2, 3] in [a, *rest]
    matched = true
  end
  assert_true matched

  # with hash
  assert_true(({a: 1} in {a: 1}))
  assert_false(({a: 1} in {a: 2}))
end

assert('pattern matching - one-line =>') do
  # simple binding
  1 => x
  assert_equal 1, x

  # array destructuring
  [1, 2, 3] => [a, b, c]
  assert_equal [1, 2, 3], [a, b, c]

  # hash destructuring
  {name: "Bob", age: 25} => {name:, age:}
  assert_equal "Bob", name
  assert_equal 25, age

  # with rest
  [1, 2, 3, 4] => [first, *middle, last]
  assert_equal 1, first
  assert_equal [2, 3], middle
  assert_equal 4, last
end

assert('pattern matching - NoMatchingPatternError') do
  # => raises on mismatch
  assert_raise(NoMatchingPatternError) do
    1 => 2
  end

  # can be rescued
  begin
    [1, 2] => [1, 2, 3]
  rescue NoMatchingPatternError => e
    assert_true e.message.is_a?(String)
  end

  # case/in without else raises NoMatchingPatternError
  assert_raise(NoMatchingPatternError) do
    case 5
    in 1 then :one
    in 2 then :two
    end
  end
end

assert('pattern matching - complex patterns') do
  # array of hashes
  records = [{id: 1, value: "a"}, {id: 2, value: "b"}]
  case records
  in [first, second]
    assert_equal({id: 1, value: "a"}, first)
    assert_equal({id: 2, value: "b"}, second)
  end

  # hash with array value
  data = {items: [1, 2, 3]}
  case data
  in {items: [a, *rest]}
    assert_equal 1, a
    assert_equal [2, 3], rest
  end
end

assert('pattern matching - array pattern as the last expression of a frame') do
  # Nothing follows the pattern here, so the frame is exactly as wide as the
  # pattern's own code declares. The internal `deconstruct` and `size` calls
  # and the post-rest index must fit in that width.
  f = ->(x) { x => [a, b]; [a, b] }
  assert_equal [1, 2], f.call([1, 2])

  f = ->(x) { x => [a, [b, c]]; [a, b, c] }
  assert_equal [1, 2, 3], f.call([1, [2, 3]])

  f = ->(x) { x => [a, *, b]; [a, b] }
  assert_equal [1, 3], f.call([1, 2, 3])

  f = ->(x) { x in [a, b] }
  assert_true f.call([1, 2])
  assert_false f.call([1, 2, 3])

  f = ->(x) { case x; in [a, b]; end; [a, b] }
  assert_equal [1, 2], f.call([1, 2])
end

assert('pattern matching - find pattern as the last expression of a frame') do
  # The elements are variables on purpose: a literal element calls `===`,
  # and that call widens the frame past the gap this guards against.
  f = ->(x) { x => [*, a, *]; a }
  assert_equal 1, f.call([1, 2, 3])

  f = ->(x) { x => [*p, a, *q]; [p, a, q] }
  assert_equal [[], 1, [2, 3]], f.call([1, 2, 3])
end

assert('defined? on statically-decidable operands') do
  # literals and pure expressions
  assert_equal 'expression', defined?(1)
  assert_equal 'expression', defined?("s")
  assert_equal 'expression', defined?(:sym)
  assert_equal 'expression', defined?([1, 2])
  assert_equal 'expression', defined?({a: 1})
  assert_equal 'expression', defined?(1..2)
  assert_equal 'expression', defined?(defined?(x))

  # self, and the literals named rather than called expressions
  assert_equal 'self', defined?(self)
  assert_equal 'nil', defined?(nil)
  assert_equal 'true', defined?(true)
  assert_equal 'false', defined?(false)

  # a local variable in scope
  lv = 1
  assert_equal 'local-variable', defined?(lv)

  # assignments report "assignment" without being evaluated
  assert_equal 'assignment', defined?(unset = 1)
  assert_nil unset
  n = 5
  assert_equal 'assignment', defined?(n += 100)
  assert_equal 5, n
end

DEFINED_TEST_CONST = 1

assert('defined? on operands resolved at run time') do
  # constants (in the lexical scope of this method)
  assert_equal 'constant', defined?(DEFINED_TEST_CONST)
  assert_equal 'constant', defined?(Object)
  assert_nil defined?(NoSuchConstantHere)

  # methods reachable from self, including private ones
  assert_equal 'method', defined?(assert)          # available here
  assert_nil defined?(no_such_method_at_all)

  # instance variables of self
  o = Object.new
  o.instance_eval { @ivar_present = 1 }
  assert_equal 'instance-variable', o.instance_eval { defined?(@ivar_present) }
  assert_nil o.instance_eval { defined?(@ivar_absent) }

  # yield depends on whether the enclosing method got a block
  m = Object.new
  def m.with_block; defined?(yield); end
  assert_equal 'yield', m.with_block {}
  assert_nil m.with_block

  # the operand is not evaluated
  evaluated = false
  defined?(no_such_method_at_all(evaluated = true))
  assert_false evaluated
end

$defined_test_gvar = 1

assert('defined? on global/class variables and super') do
  # global variables (defined once assigned)
  assert_equal 'global-variable', defined?($defined_test_gvar)
  assert_nil defined?($no_such_global_anywhere)

  # class variables, in the lexical class scope
  cls = Class.new do
    @@cv_present = 1
    def read_present; defined?(@@cv_present); end
    def read_absent;  defined?(@@cv_absent); end
  end
  obj = cls.new
  assert_equal 'class variable', obj.read_present
  assert_nil obj.read_absent

  # super depends on whether the method has a super method
  base = Class.new { def greet; end }
  derived = Class.new(base) { def greet; defined?(super); end }
  assert_equal 'super', derived.new.greet
  standalone = Class.new { def solo; defined?(super); end }
  assert_nil standalone.new.solo
end

module DefinedPathOuter
  Inner = 1
end

class DefinedPathBase; Sub = 2; end
class DefinedPathChild < DefinedPathBase; end

assert('defined? on constant paths (A::B)') do
  assert_equal 'constant', defined?(DefinedPathOuter::Inner)
  assert_nil defined?(DefinedPathOuter::Missing)
  assert_nil defined?(NoSuchOuter::Inner)      # undefined parent, no raise

  # the ::  lookup follows the ancestor chain of the parent
  assert_equal 'constant', defined?(DefinedPathChild::Sub)

  # a builtin nested constant
  assert_equal 'constant', defined?(Float::INFINITY) if Object.const_defined?(:Float)
end

module DefinedDeepOuter
  module Mid
    Leaf = 1
  end
  NotAModule = 1

  def self.from_lexical_scope;      defined?(Mid::Leaf); end
  def self.from_lexical_scope_miss; defined?(Mid::Missing); end
end

class DefinedPathRoot
  @evaluated = 0
  def self.count; @evaluated; end
  def self.seen; @evaluated += 1; DefinedDeepOuter; end
  def self.boom; raise 'defined? let a constant path root raise'; end
end

assert('defined? on a constant path of any depth') do
  assert_equal 'constant', defined?(DefinedDeepOuter::Mid)
  assert_equal 'constant', defined?(DefinedDeepOuter::Mid::Leaf)
  assert_nil defined?(DefinedDeepOuter::Mid::Missing)
  assert_nil defined?(DefinedDeepOuter::Missing::Leaf)
  assert_nil defined?(NoSuchOuterAtAll::Mid::Leaf)

  # the walk stops where a name resolves to something that is not a module
  assert_nil defined?(DefinedDeepOuter::NotAModule::Leaf)
  assert_nil defined?(DefinedDeepOuter::Mid::Leaf::Deeper)

  # the first name resolves in the lexical scope of the code asking
  assert_equal 'constant', DefinedDeepOuter.from_lexical_scope
  assert_nil DefinedDeepOuter.from_lexical_scope_miss
end

# a module that holds itself, so a path of any length can be written out and
# its length is the only thing under test
module DefinedDeepSelf
  S = self
end

assert('defined? on a constant path at the length limit') do
  # the compiler collects at most DEFINED_PATH_MAX (32) names of one path,
  # since it holds them on the stack of a recursive codegen; a longer path
  # is answered nil, where CRuby, which has no such bound, answers "constant"
  assert_equal 'constant', defined?(DefinedDeepSelf::S::S::S::S::S::S::S::S::
                                    S::S::S::S::S::S::S::S::S::S::S::S::S::S::
                                    S::S::S::S::S::S::S::S::S)
  assert_nil defined?(DefinedDeepSelf::S::S::S::S::S::S::S::S::S::S::S::S::S::
                      S::S::S::S::S::S::S::S::S::S::S::S::S::S::S::S::S::S::S)
end

assert('defined? on a constant path rooted at Object') do
  assert_equal 'constant', defined?(::Object)
  assert_equal 'constant', defined?(::DefinedDeepOuter)
  assert_equal 'constant', defined?(::DefinedDeepOuter::Mid::Leaf)
  assert_nil defined?(::NoSuchOuterAtAll)
  assert_nil defined?(::DefinedDeepOuter::Missing)
end

module DefinedPathHooked
  def self.const_missing(name); @asked = name; end
  def self.asked; @asked; end
end

assert('defined? on a constant path resolves each name as reading it does') do
  # a constant Object holds is out of reach through any other module, as it
  # is for the read itself, and the walk past one does not raise
  assert_equal 'constant', defined?(Object::String)
  assert_nil defined?(String::String)
  assert_nil defined?(String::Object)
  assert_nil defined?(String::String::Leaf)
  assert_nil defined?(Comparable::String)
  assert_nil defined?(DefinedPathOuter::String)

  # const_missing is not asked
  assert_nil defined?(DefinedPathHooked::Missing)
  assert_nil defined?(DefinedPathHooked::Missing::Leaf)
  assert_nil DefinedPathHooked.asked
end

assert('defined? resolves a constant path from an evaluated root') do
  # a root that is not a constant is evaluated, once, and the names are
  # looked up from its value, the way a receiver is evaluated for its method
  m = DefinedDeepOuter
  assert_equal 'constant', defined?(m::Mid)
  assert_equal 'constant', defined?(m::Mid::Leaf)
  assert_nil defined?(m::Missing)
  assert_nil defined?(m::Mid::Missing)
  assert_nil defined?(m::NotAModule::Leaf)
  before = DefinedPathRoot.count
  assert_equal 'constant', defined?(DefinedPathRoot.seen::Mid::Leaf)
  assert_equal before + 1, DefinedPathRoot.count
  assert_nil defined?(DefinedPathRoot.seen::Missing)
  assert_equal before + 2, DefinedPathRoot.count

  # a root that is not a module, that is not itself defined, which leaves it
  # unevaluated, or that raises, which answers nil rather than letting it out
  n = 1
  assert_nil defined?(n::Leaf)
  assert_nil defined?(no_such_method_at_all::Leaf)
  assert_nil defined?(DefinedPathRoot.seen(no_such_method_at_all)::Mid)
  assert_equal before + 2, DefinedPathRoot.count
  assert_nil defined?(DefinedPathRoot.boom::Leaf)

  # the path is a receiver, or an argument, like any other
  assert_equal 'method', defined?(m::Mid::Leaf.to_s)
  assert_nil defined?(m::Mid::Leaf.no_such_method_at_all)
  assert_nil defined?(m::Missing.to_s)
  assert_equal 'method', defined?(assert(m::Mid::Leaf))
  assert_nil defined?(assert(m::Missing))
end

class DefinedRecv
  attr_accessor :foo
  def pub; end
  private def priv; end
  protected def prot; end
  def respond_to_missing?(name, include_private = false); name == :ghost; end
  def from_inside(other); defined?(other.prot); end
end

class DefinedRecvRaises
  @@evaluated = 0
  def self.count; @@evaluated; end
  def self.seen; @@evaluated += 1; self; end
  def self.boom; raise 'defined? let a receiver raise'; end
end

assert('defined? on a method reached through a receiver') do
  o = DefinedRecv.new
  assert_equal 'method', defined?(o.pub)
  assert_equal 'method', defined?(1 + 1)
  assert_equal 'method', defined?(String.new)
  assert_equal 'method', defined?([1, 2][0])
  assert_equal 'method', defined?(!o)
  assert_nil defined?(o.no_such_method_at_all)

  # the receiver is evaluated, so a chain answers for its last name and
  # answers nil where a link in it is missing
  assert_equal 'method', defined?(1.to_s.size)
  assert_nil defined?(1.no_such_method_at_all.size)

  # a receiver that is not defined is never evaluated, and never reached
  assert_nil defined?(no_such_receiver_at_all.size)
  assert_nil defined?(@no_such_ivar_at_all.size)

  # safe navigation asks about the method the same way, on the receiver as
  # it stands: `nil` has `to_s` and nothing else the name could reach
  assert_equal 'method', defined?(nil&.to_s)
  assert_nil defined?(nil&.no_such_method_at_all)
end

assert('defined? weighs how a method on a receiver may be called') do
  o = DefinedRecv.new
  assert_nil defined?(o.priv)
  assert_nil defined?(o.prot)       # self here is not a DefinedRecv
  assert_equal 'method', o.from_inside(DefinedRecv.new)
  assert_equal 'method', defined?(o.ghost)   # through respond_to_missing?
end

class DefinedBareCall
  private def priv; end
  def respond_to_missing?(name, include_private = false)
    @asked = include_private
    name == :ghost
  end
  def asked; @asked; end
  def bare_priv;  defined?(priv); end
  def bare_ghost; defined?(ghost); end
  def bare_none;  defined?(no_such_method_at_all); end
  def bare_args;  defined?(ghost(1)); end
end

class DefinedBareCallAnswers < DefinedBareCall
  def respond_to?(name, include_private = false)
    @asked = include_private
    name == :answered
  end
  def bare_answered; defined?(answered); end
end

class DefinedBareCallGone < TestNotImplement
  def bare_gone; defined?(gone); end
end

assert('defined? asks respond_to? about a call on self') do
  o = DefinedBareCall.new
  # a private method is reachable, and so is one `respond_to_missing?`
  # claims, which is asked with include_private true
  assert_equal 'method', o.bare_priv
  assert_equal 'method', o.bare_ghost
  assert_true o.asked
  assert_nil o.bare_none
  assert_equal 'method', o.bare_args

  # a `respond_to?` the object defines is what gets asked, on the same terms
  o = DefinedBareCallAnswers.new
  assert_equal 'method', o.bare_answered
  assert_true o.asked
  assert_nil o.bare_priv
  assert_nil o.bare_ghost

  # a call with a receiver written is answered on the terms of that call
  assert_nil defined?(o.answered)
  assert_nil defined?(o.priv)

  # a method standing for a feature the build does not have is not there
  assert_nil DefinedBareCallGone.new.bare_gone
  assert_nil defined?(TestNotImplement.gone)
end

class DefinedBareCallNoMissing
  undef_method :respond_to_missing?
  def bare_none; defined?(no_such_method_at_all); end
end

class DefinedBareCallNoRespondTo
  undef_method :respond_to?
  def bare_none; defined?(no_such_method_at_all); end
  def bare_self; defined?(bare_self); end
end

class DefinedBareCallRaises
  def respond_to_missing?(name, include_private = false)
    raise 'defined? let respond_to_missing? raise'
  end
  def bare_none; defined?(no_such_method_at_all); end
end

assert('defined? on self answers nil where respond_to_missing? or respond_to? is undefined') do
  # a name with no method behind it is nil, not an error about the hook
  # that has been undefined; the same asked with a receiver is nil as well
  o = DefinedBareCallNoMissing.new
  assert_nil o.bare_none
  assert_nil defined?(o.no_such_method_at_all)

  # an undefined `respond_to?` is not a redefinition either, so the answer
  # is the one `respond_to?` would give
  o = DefinedBareCallNoRespondTo.new
  assert_nil o.bare_none
  assert_equal 'method', o.bare_self
  assert_nil defined?(o.no_such_method_at_all)

  # what a `respond_to_missing?` the object defines raises is not caught
  # for a call on self, which is why an undefined one has to be told apart
  assert_raise(RuntimeError) { DefinedBareCallRaises.new.bare_none }
end

assert('defined? answers nil where evaluating a receiver raises') do
  assert_nil defined?(DefinedRecvRaises.boom.anything)
  assert_equal :caught, (defined?(DefinedRecvRaises.boom.x) ? :answered : :caught)
  assert_equal [nil, 'method'], [defined?(DefinedRecvRaises.boom.x), defined?(1.to_s)]

  # the receiver of a defined? that answers is evaluated, once
  before = DefinedRecvRaises.count
  assert_equal 'method', defined?(DefinedRecvRaises.seen.to_s)
  assert_equal before + 1, DefinedRecvRaises.count
end

assert('defined? evaluates each link of a receiver chain once') do
  before = DefinedRecvRaises.count
  assert_equal 'method', defined?(DefinedRecvRaises.seen.seen.to_s)
  assert_equal before + 2, DefinedRecvRaises.count
  assert_equal 'method', defined?(DefinedRecvRaises.seen.seen.seen.to_s)
  assert_equal before + 5, DefinedRecvRaises.count

  # a chain stops at the first link that answers nil, or that raises
  assert_nil defined?(DefinedRecvRaises.seen.no_such_method_at_all.to_s)
  assert_equal before + 6, DefinedRecvRaises.count
  assert_nil defined?(DefinedRecvRaises.seen.boom.to_s)
  assert_equal before + 7, DefinedRecvRaises.count

  # a link that is an attribute write is called on the same terms, and
  # answers with the value assigned
  o = DefinedRecv.new
  assert_equal 'method', defined?((o.foo = 1).to_s)
  assert_equal 1, o.foo
  xs = [0]
  assert_equal 'method', defined?((xs[0] = 2).to_s)
  assert_equal [2], xs

  # safe navigation leaves nil where its receiver is nil, and the next
  # link is asked of that nil
  assert_nil defined?(nil&.to_s.size)
  assert_nil defined?(nil&.to_s&.no_such_method_at_all)
end

assert('defined? weighs the arguments of a call') do
  lv = 1
  assert_equal 'method', defined?(assert(lv))
  assert_equal 'method', defined?(assert(1, 'two', :three))
  assert_nil defined?(assert(no_such_thing_at_all))
  assert_nil defined?(assert(1, no_such_thing_at_all))
  assert_nil defined?(assert(no_such_thing_at_all, 1))
  assert_nil defined?(assert(@no_such_ivar_at_all))
  assert_nil defined?(assert($no_such_global_at_all))
  assert_nil defined?(assert(NoSuchConstantAtAll))

  # an argument is weighed to whatever depth it has
  assert_nil defined?(assert(no_such_thing_at_all + 1))
  assert_nil defined?(assert(assert(no_such_thing_at_all)))
  assert_nil defined?(assert(no_such_thing_at_all.size))

  # a call through a receiver has its arguments weighed too, safe
  # navigation among them
  assert_equal 'method', defined?(1.to_s(lv))
  assert_nil defined?(1.to_s(no_such_thing_at_all))
  assert_nil defined?(nil&.to_s(no_such_thing_at_all))

  # nothing is evaluated for an argument that is not there, the receiver
  # of the call least of all
  before = DefinedRecvRaises.count
  assert_nil defined?(DefinedRecvRaises.seen.to_s(no_such_thing_at_all))
  assert_equal before, DefinedRecvRaises.count
end

def defined_anonymous_forwarding(*, **, &)
  [defined?(assert(*)), defined?(assert(**)), defined?(assert(&))]
end

def defined_forwarding(...)
  defined?(assert(...))
end

assert('defined? weighs the arguments of a call however they are passed') do
  lv = 1
  h = {k: 1}
  assert_equal 'method', defined?(assert(*lv))
  assert_nil defined?(assert(*no_such_thing_at_all))
  assert_equal 'method', defined?(assert(k: lv))
  assert_nil defined?(assert(k: no_such_thing_at_all))
  assert_nil defined?(assert(no_such_thing_at_all => 1))
  assert_equal 'method', defined?(assert(**h))
  assert_nil defined?(assert(**no_such_thing_at_all))
  assert_nil defined?(assert({a: no_such_thing_at_all}))
  assert_nil defined?(assert([*no_such_thing_at_all]))

  # what is forwarded by name alone holds nothing to weigh
  assert_equal ['method', 'method', 'method'], defined_anonymous_forwarding(1)
  assert_equal 'method', defined_forwarding(1)
end

assert('defined? leaves alone the parts of a call it does not weigh') do
  assert_equal 'method', defined?(assert(&no_such_thing_at_all))
  assert_equal 'method', defined?(assert(1..no_such_thing_at_all))

  # a call carrying a block is an expression whatever its arguments are
  assert_equal 'expression', defined?(assert(no_such_thing_at_all) { })
  assert_equal 'expression', defined?(assert(*no_such_thing_at_all) { })
end

assert('defined? weighs the elements of an array literal') do
  lv = 1
  assert_equal 'expression', defined?([1, lv])
  assert_equal 'expression', defined?([*lv])
  assert_nil defined?([no_such_thing_at_all])
  assert_nil defined?([1, no_such_thing_at_all])
  assert_nil defined?([[no_such_thing_at_all]])
  assert_nil defined?([*no_such_thing_at_all])
  assert_nil defined?([1, {k: no_such_thing_at_all}])
  assert_nil defined?(assert([[no_such_thing_at_all]]))
end

assert('defined? weighs the keys and values of a hash literal') do
  lv = 1
  h = {k: 1}
  assert_equal 'expression', defined?({})
  assert_equal 'expression', defined?({k: lv, lv => 1, **h, lv:})
  assert_nil defined?({k: no_such_thing_at_all})
  assert_nil defined?({no_such_thing_at_all => 1})
  assert_nil defined?({**no_such_thing_at_all})
  assert_nil defined?({no_such_thing_at_all:})
  assert_nil defined?({k: [no_such_thing_at_all]})

  # the ends of a range are not weighed, inside a literal or on their own
  assert_equal 'expression', defined?({k: 1..no_such_thing_at_all})
  assert_equal 'expression', defined?(1..no_such_thing_at_all)
end

assert('defined? on control flow, jumps and definitions') do
  lv = 1

  # logical operators
  assert_equal 'expression', defined?(lv && lv)
  assert_equal 'expression', defined?(lv || lv)
  assert_equal 'expression', defined?(lv and lv)
  assert_equal 'expression', defined?(lv or lv)

  # control flow
  assert_equal 'expression', defined?(if lv then 1 else 2 end)
  assert_equal 'expression', defined?(unless lv then 1 end)
  assert_equal 'expression', defined?(lv ? 1 : 2)
  assert_equal 'expression', defined?(case lv; when 1 then 2; end)
  assert_equal 'expression', defined?(case lv; in Integer then 1; end)
  assert_equal 'expression', defined?(lv in Integer)
  assert_equal 'expression', defined?(lv => Integer)
  assert_equal 'expression', defined?(while false do end)
  assert_equal 'expression', defined?(until true do end)
  assert_equal 'expression', defined?(for i in [1] do end)
  assert_equal 'expression', defined?(begin; 1; rescue; end)
  assert_equal 'expression', defined?(begin; 1; ensure; end)
  assert_equal 'expression', defined?(if (lv == 1)..(lv == 2) then 1 end)

  # jumps, which `defined?` reports on without needing a place to jump to
  assert_equal 'expression', defined?(return)
  assert_equal 'expression', defined?(break)
  assert_equal 'expression', defined?(next)
  assert_equal 'expression', defined?(redo)
  assert_equal 'expression', defined?(retry)

  # definitions, which stay undefined because the operand is not evaluated
  assert_equal 'expression', defined?(def defined_never_defined; end)
  assert_false respond_to?(:defined_never_defined, true)
  assert_equal 'expression', defined?(class DefinedNeverClass; end)
  assert_equal 'expression', defined?(module DefinedNeverModule; end)
  assert_false Object.const_defined?(:DefinedNeverClass)
  assert_false Object.const_defined?(:DefinedNeverModule)
  assert_equal 'expression', defined?(class << self; end)

  # a rescue modifier, a named-capture match, and the statements that only a
  # bare begin can hold in an operand
  assert_equal 'expression', defined?(begin; no_such_method_at_all rescue 1; end)
  assert_equal 'expression', defined?(/(?<defined_never_bound>.)/ =~ 'x')
  assert_nil defined_never_bound
  assert_equal 'expression', defined?(begin; alias defined_never_alias defined_never_defined; end)
  assert_false respond_to?(:defined_never_alias, true)
  assert_equal 'expression', defined?(begin; undef assert; end)
  assert_equal 'expression', defined?(begin; END { }; end)
end

assert('defined? answers with a frozen string') do
  lv = 1
  assert_true defined?(1).frozen?
  assert_true defined?(nil).frozen?
  assert_true defined?(lv).frozen?
  assert_true defined?(lv = 2).frozen?
  assert_true defined?(Object).frozen?
  assert_true defined?(assert).frozen?
  assert_true defined?(1.to_s).frozen?
  assert_true defined?(Object::String).frozen?
  assert_raise(FrozenError) { defined?(self).upcase! }
end

assert('defined? on a back reference reads it') do
  # `$~` holds nothing here, so every name that reads from it is nil
  assert_nil defined?($&)
  assert_nil defined?($`)
  assert_nil defined?($1)
  assert_nil defined?($1.to_s)
  assert_nil defined?(assert($1))
end

assert('defined? names it a local variable') do
  assert_equal ['local-variable'], [1].map { defined?(it) }
  assert_equal ['method'], [1].map { defined?(it.to_s) }
  assert_equal [nil], [1].map { defined?(it.no_such_method_at_all) }
end

assert('defined? sees through parentheses around one expression') do
  lv = 1
  @defined_paren_iv = 1

  assert_equal 'local-variable', defined?((lv))
  assert_equal 'local-variable', defined?(((lv)))
  assert_equal 'instance-variable', defined?((@defined_paren_iv))
  assert_equal 'constant', defined?((Object))
  assert_nil defined?((no_such_method_at_all))
  assert_equal 'nil', defined?((nil))

  # parentheses holding several statements are an expression of their own,
  # and empty ones the nil they evaluate to
  assert_equal 'expression', defined?((1; 2))
  assert_equal 'nil', defined?(())
  assert_equal 'nil', defined?(((())))
end

assert('defined? sees through a bare begin around one expression') do
  lv = 1

  assert_equal 'local-variable', defined?(begin; lv; end)
  assert_equal 'constant', defined?(begin; Object; end)
  assert_equal 'assignment', defined?(begin; unset = 1; end)
  assert_nil unset
  assert_nil defined?(begin; no_such_method_at_all; end)
  assert_nil defined?(begin; begin; (no_such_method_at_all); end; end)

  # the same where the begin is a receiver, or an argument
  assert_nil defined?(begin; no_such_method_at_all; end.to_s)
  assert_nil defined?(puts(begin; no_such_method_at_all; end))
  assert_equal 'method', defined?(begin; lv; end.to_s)

  # a begin holding several statements is an expression of its own, and an
  # empty one the nil it evaluates to
  assert_equal 'expression', defined?(begin; 1; 2; end)
  assert_equal 'nil', defined?(begin; end)
  assert_equal 'nil', defined?(begin; (); end)

  # a rescue, else or ensure clause makes it an expression whatever it holds
  assert_equal 'expression', defined?(begin; no_such_method_at_all; rescue; end)
  assert_equal 'expression', defined?(begin; no_such_method_at_all; ensure; end)
  assert_equal 'expression', defined?(begin; rescue; end)
end

assert('defined? on a call carrying a block') do
  # a literal block makes the whole call an expression, whether or not the
  # method is there and whatever the call is written on
  assert_equal 'expression', defined?(loop { break })
  assert_equal 'expression', defined?(no_such_method_at_all { })
  assert_equal 'expression', defined?([1, 2].map { |e| e })
  assert_equal 'expression', defined?(nil&.no_such_method_at_all { })

  # a block passed as `&arg` leaves an ordinary call behind
  assert_equal 'method', defined?(assert(&:to_s))
  assert_nil defined?(no_such_method_at_all(&:to_s))
end

# NOTE: `&nil` block-forbidding parameters live in syntax_block_forbid.rb,
# which the build excludes when compiling with mruby-compiler-prism (the
# Prism parser does not accept `&nil` yet).

assert('brace-less variable interpolation') do
  # `"#@iv"` is the short form of `"#{@iv}"`. It reaches the codegen as an
  # EmbeddedVariableNode, which used to be unimplemented.
  @iv = "IV"
  $gv = "GV"

  assert_equal "aIVb", "a#@iv" + "b"
  assert_equal "xGVy", "x#$gv" + "y"
  assert_equal "IVGV", "#@iv#$gv"
  assert_equal :sIV, :"s#@iv"

  # a class variable, which is only readable from a class body or method
  c = Class.new do
    @@cv = "CV"
    def self.t; "c#@@cv"; end
  end
  assert_equal "cCV", c.t

  # non-string values go through to_s, as with #{}
  @n = 42
  @u = nil
  assert_equal "42", "#@n"
  assert_equal "", "#@u"

  # `#` not followed by a variable sigil stays literal
  assert_equal "# x", "# x"
  assert_equal 3, "#@ ".length
end

assert('local variable operator-assignment with a non-numeric receiver') do
  # `x += 1` on a local variable compiles to OP_ADDILV, whose fast path handles
  # Integer and Float in place.  Anything else has to go through the method,
  # and that call must not be set up on the local variables: both the argument
  # register and the callee frame would start at the local being assigned.
  obj = Class.new { def +(n); [:added, n]; end }.new
  a = 10
  b = 20
  obj += 1
  assert_equal [:added, 1], obj
  assert_equal 10, a
  assert_equal 20, b

  obj2 = Class.new { def -(n); [:subtracted, n]; end }.new
  c = 30
  obj2 -= 2
  assert_equal [:subtracted, 2], obj2
  assert_equal 30, c

  # the operand is passed as an Integer, and an exception from the method
  # propagates rather than being swallowed
  obj3 = Class.new { def +(n); raise ArgumentError, n.to_s; end }.new
  assert_raise_with_message(ArgumentError, "7") { obj3 += 7 }
end

assert('pattern matching - the case value survives a failed clause') do
  # The move that puts the case value in a register of its own is not the
  # last read of the value: every `in` clause reads that register again.
  # Folding the move into the first clause's own move left the register the
  # later clauses read never written.
  f = ->(x) {
    case x
    in {zz: 1} then :zz
    in {a: 1} then :a
    else :none
    end
  }
  assert_equal :a, f.call({a: 1})
  assert_equal :zz, f.call({zz: 1})
  assert_equal :none, f.call({b: 1})

  g = ->(x) {
    case x
    in [1, 2] then :two
    in [1] then :one
    else :none
    end
  }
  assert_equal :two, g.call([1, 2])
  assert_equal :one, g.call([1])
  assert_equal :none, g.call([3])

  # a third clause reads it as well
  h = ->(x) {
    case x
    in {zz: 1} then :zz
    in {yy: 1} then :yy
    in {a: 1} then :a
    else :none
    end
  }
  assert_equal :a, h.call({a: 1})
  assert_equal :none, h.call({b: 1})

  # the one-line forms read it again for a later alternative
  i = ->(x) { x in {a: 1} | {b: 2} }
  assert_true i.call({b: 2})
  assert_true i.call({a: 1})
  assert_false i.call({c: 3})

  j = ->(x) { x => {a: 1} | {b: 2}; :ok }
  assert_equal :ok, j.call({b: 2})
  assert_raise(NoMatchingPatternError) { j.call({c: 3}) }
end

assert('pattern matching - the constant of a constant pattern is a test of its own') do
  # `Const[...]` and `Const(...)` reach the rest of the pattern only when the
  # constant answers `===` for the subject.
  assert_true(([1, 2] in Array[1, 2]))
  assert_false(([1, 2] in String[1, 2]))
  assert_true(({a: 1} in Hash(a: 1)))
  assert_false(({a: 1} in String(a: 1)))
  assert_true(([1, 2] in Array[*, 1, *]))
  assert_false(([1, 2] in String[*, 1, *]))

  assert_true(([] in Array[]))
  assert_false(([] in Hash[]))
  assert_true(([[1]] in Array[Array[1]]))
  assert_false(([[1]] in Array[Hash[1]]))

  [1, 2, 3] in Array[1, *rest]
  assert_equal [2, 3], rest

  # it is `===` that the constant is asked, not a class check of its own
  EqqAlways = Class.new do
    def self.===(_o); :truthy; end
  end
  assert_true(([1] in EqqAlways[1]))
  assert_false(([1] in Comparable[1]))

  # the next clause is reached when the constant refuses
  got = case [1, 2]
        in Hash[1, 2] then :hash
        in Array[1, 2] then :array
        else :none
        end
  assert_equal :array, got
end

assert('pattern matching - what a pin may name') do
  # A pin reads what it names the way any other expression is read, so it
  # reaches beyond a local of the pattern's own scope.
  outer = 5
  reader = ->(v) { v in [^outer] }
  assert_true reader.call([5])
  assert_false reader.call([6])

  # an expression
  assert_true(([3] in [^(1 + 2)]))
  assert_false(([4] in [^(1 + 2)]))
  assert_true(([5] in [^(1..9)]))
  assert_true(([1] in [^(Integer)]))
  assert_true((["ab"] in [^("a" + "b")]))

  # the expression is evaluated where the pin sits, once
  counter = [0]
  bump = ->{ counter[0] += 1; 3 }
  assert_true(([3] in [^(bump.call)]))
  assert_equal 1, counter[0]

  # an instance variable
  holder = Class.new do
    def initialize; @iv = 1; end
    def pinned(v); v in [^@iv]; end
  end.new
  assert_true holder.pinned([1])
  assert_false holder.pinned([2])

  # a class variable
  class PinCvarHolder
    @@cv = 7
    def pinned(v); v in [^@@cv]; end
  end
  assert_true PinCvarHolder.new.pinned([7])
  assert_false PinCvarHolder.new.pinned([8])

  # a global variable
  $syntax_pin_gvar = 3
  assert_true(([3] in [^$syntax_pin_gvar]))
  assert_false(([4] in [^$syntax_pin_gvar]))

  # every pattern shape a pin can sit in
  z = 2
  assert_true(({a: 2} in {a: ^z}))
  assert_true(([1, 2, 3] in [*, ^z, *]))
  assert_true(([[1, 2]] in [[1, ^z]]))
  assert_true(([2] in [1] | [^z]))
  assert_equal :two, (case [2]
                      in [1] then :one
                      in [^z] then :two
                      else :none
                      end)
  [2] => [^z]

  # a pin can name a variable the pattern bound to its left
  assert_equal :ok, (case [1, 1]; in [a, ^a] then :ok; else :no; end)
  assert_equal :no, (case [1, 2]; in [a, ^a] then :ok; else :no; end)
end

assert('pattern matching - the clauses of a case share one deconstruct') do
  # Each array or find clause sent `respond_to?` and `deconstruct` to the
  # subject afresh, so a subject with a costly hook paid for it once per
  # clause.  CRuby keeps the first answer for the rest of the `case`.
  counted = Class.new do
    attr_reader :sent, :asked
    def initialize(v); @v = v; @sent = 0; @asked = 0; end
    def deconstruct; @sent += 1; @v; end
    def respond_to?(m, priv = false); @asked += 1 if m == :deconstruct; super; end
  end

  d = counted.new([1, 2])
  r = case d
      in [3] then :no
      in [1, 2, 3] then :no
      in [1, *] then :yes
      end
  assert_equal [:yes, 1, 1], [r, d.sent, d.asked]

  # a guard, a capture and an alternative read the same answer
  d = counted.new([1, 2])
  r = case d
      in [3] | [4] then :no
      in [1, x] => whole if x == 9 then :no
      in [1, x] unless x == 2 then :no
      in [1, 2] then :yes
      end
  assert_equal [:yes, 1, 1], [r, d.sent, d.asked]

  # a find pattern shares with an array pattern
  d = counted.new([0, 1, 2])
  r = case d
      in [*, 9, *] then :no
      in [0, *] then :yes
      end
  assert_equal [:yes, 1], [r, d.sent]

  # a subject with no hook is asked once and falls through to else
  bare = Class.new do
    attr_reader :asked
    def initialize; @asked = 0; end
    def respond_to?(m, priv = false); @asked += 1 if m == :deconstruct; super; end
  end
  b = bare.new
  r = case b
      in [1] then :no
      in [*] then :no
      in [*, 1, *] then :no
      else :else
      end
  assert_equal [:else, 1], [r, b.asked]

  # a nested pattern deconstructs its own subject in every clause
  d = counted.new([1])
  x = [d, d]
  r = case x
      in [[3], _] then :no
      in [[1], _] then :yes
      end
  assert_equal [:yes, 2], [r, d.sent]

  # a case that is the whole body of a method returns through the register
  cls = Class.new do
    def self.pick(d)
      case d
      in [3] then :a
      in [1, *] then :b
      end
    end
  end
  assert_equal :b, cls.pick(counted.new([1, 2]))

  # the value of the case and the subject are where the clauses left them
  d = counted.new([7])
  r = case d
      in [8] then :no
      in Integer then :no
      in [q] then [q, d.sent]
      end
  assert_equal [7, 1], r
  r = case counted.new([1])
      in [2] then 1
      else 2
      end
  assert_equal 2, r
end

assert('pattern matching - a subject with no deconstruction hook does not match') do
  # The pattern asks whether the subject answers the hook before it sends one,
  # as CRuby does, so a subject that has none fails the pattern rather than
  # raising NoMethodError.
  f = ->(x) {
    case x
    in [1] then :arr
    in {a: 1} then :hash
    in [*, 9, *] then :find
    else :none
    end
  }
  assert_equal :none, f.call(3)
  assert_false((3 in [1]))
  assert_false((3 in {a: 1}))

  # a private hook is not one the pattern may call
  priv = Class.new do
    private def deconstruct; [1]; end
    private def deconstruct_keys(keys); {a: 1}; end
  end.new
  assert_false((priv in [1]))
  assert_false((priv in {a: 1}))

  pub = Class.new do
    def deconstruct; [1]; end
    def deconstruct_keys(keys); {a: 1}; end
  end.new
  assert_true((pub in [1]))
  assert_true((pub in {a: 1}))

  # one hook does not stand in for the other
  only_ary = Class.new do
    def deconstruct; [1]; end
  end.new
  assert_true((only_ary in [1]))
  assert_false((only_ary in {a: 1}))
  assert_true((only_ary in [*, 1, *]))
end

assert('pattern matching - a deconstruction hook has to answer a Hash') do
  # The key check a hash pattern makes reads what #deconstruct_keys answered
  # through a method only Hash carries, so anything else raises where it used
  # to leak the name of that method.
  bad_hash = Class.new { def deconstruct_keys(keys); :nothash; end }.new
  nil_hash = Class.new { def deconstruct_keys(keys); nil; end }.new
  assert_raise_with_message(TypeError, "deconstruct_keys must return Hash") do
    bad_hash in {a: 1}
  end
  assert_raise_with_message(TypeError, "deconstruct_keys must return Hash") do
    nil_hash in {a: 1}
  end
  # a pattern with no keys of its own reads the answer too
  assert_raise_with_message(TypeError, "deconstruct_keys must return Hash") do
    bad_hash in {}
  end
  assert_raise_with_message(TypeError, "deconstruct_keys must return Hash") do
    bad_hash in {**rest}
  end
  assert_raise_with_message(TypeError, "deconstruct_keys must return Hash") do
    bad_hash in {**nil}
  end

  # a hook that answers a Hash still matches
  good = Class.new do
    def deconstruct_keys(keys); {a: 1}; end
  end.new
  assert_true((good in {a: 1}))
end

class SelfAttrWrite
  def plain;    self.a = 1;             @a;       end
  def value;    x = (self.a = 1);       [x, @a];  end
  def opasgn;   @c = 1; self.c += 1;    @c;       end
  def orasgn;   @c = nil; self.c ||= 5; @c;       end
  def andasgn;  @c = 1; self.c &&= 6;   @c;       end
  def multi;    self.a, self.b = 1, 2;  [@a, @b]; end
  def safe;     self&.a = 3;            @a;       end
  def safe_op;  @c = 1; self&.c += 3;   @c;       end
  def index;    self[0] = 9;            @i;       end
  def index3;   self[0, 1] = 7;         @i;       end
  def aliased;  x = self; x.a = 1;      @a;       end

  private
  attr_writer :a, :b
  attr_accessor :c
  def []=(i, j = nil, v); @i = v; end
end

assert('a private setter is callable on a written self') do
  o = SelfAttrWrite.new
  # the forms CRuby exempts: an attribute write whose receiver is the
  # literal `self`, as a statement or a value, in the op-assign,
  # multiple, safe and index forms too
  assert_equal 1, o.plain
  assert_equal [1, 1], o.value
  assert_equal 2, o.opasgn
  assert_equal 5, o.orasgn
  assert_equal 6, o.andasgn
  assert_equal [1, 2], o.multi
  assert_equal 3, o.safe
  assert_equal 4, o.safe_op
  assert_equal 9, o.index
  assert_equal 7, o.index3

  # only the literal `self` is exempt: a local holding self is not, and
  # neither is a call from outside
  assert_raise_with_message_pattern(NoMethodError, "private method 'a=' called for SelfAttrWrite") do
    o.aliased
  end
  assert_raise_with_message_pattern(NoMethodError, "private method 'a=' called for SelfAttrWrite") do
    o.a = 1
  end
  assert_raise_with_message_pattern(NoMethodError, "private method 'c' called for SelfAttrWrite") do
    o.c
  end
end
