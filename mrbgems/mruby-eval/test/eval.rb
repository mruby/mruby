# Kernel.eval is not provided by mruby. '15.3.1.2.3'

assert('Kernel#eval', '15.3.1.3.12') do
  assert_equal(10) { eval '1 * 10' }
  assert_equal('aaa') { eval "'a' * 3" }
  assert_equal(10) {
    a = 10
    eval "a"
  }
  assert_equal(20) {
    a = 10
    eval "a = 20"
    a
  }
  assert_equal(15) {
    c = 5
    lambda {
      a = 10
      eval "c = a + c"
    }.call
    c
  }
  assert_equal(5) {
    c = 5
    lambda {
      eval 'lambda { c }.call'
    }.call
  }
  assert_equal(15) {
    c = 5
    lambda {
      a = 10
      eval 'lambda { c = a + c }.call'
    }.call
    c
  }
  assert_equal(2) {
    a = 10
    eval 'def f(a); b=a+1; end'
    f(1)
  }
end

assert('rest arguments of eval') do
  assert_raise(TypeError) { eval('0', 0, 'test', 0) }
  assert_equal ['test', 'test.rb', 10] do
    eval('[\'test\', __FILE__, __LINE__]', nil, 'test.rb', 10)
  end
end

assert 'eval syntax error' do
  assert_raise(SyntaxError) do
    eval 'p "test'
  end
end

assert 'eval deeply nested input does not crash the parser' do
  # The recursive-descent parser must hit its nesting cap and report an error
  # rather than overflowing the C stack on pathologically nested source.
  assert_raise(SyntaxError) do
    eval('(' * 1000 + '1' + ')' * 1000)
  end
  assert_raise(SyntaxError) do
    eval('[' * 1000)
  end
end

assert 'eval an operator assignment whose write name is only the `=`' do
  # A call written this way leaves the parser with a write name of exactly
  # "=", so the read name it copies out of it is zero bytes long.  mruby's
  # allocator answers a request for zero bytes with NULL where malloc answers
  # with a pointer, and the parser hands what it gets straight to memcpy() and
  # then keeps it in the constant pool for memcmp() to read.  Both are
  # undefined with a null pointer even at a length of zero.  Only a sanitizer
  # build says so, so what is asserted here is the parse; the value of the
  # test is that a sanitizer build parses this at all.  Twice in one parse,
  # since the pool only compares the second against what the first left.
  assert_raise(SyntaxError) { eval('[.m M:+=') }
  assert_raise(SyntaxError) { eval("[.m M:+=\n[.m N:+=\n") }
end

assert 'eval a string literal at the width of the pool length' do
  # The dump records a pool string's length in 16 bits, so 65535 bytes is the
  # longest one that survives the round trip into an mrb_irep.
  assert_equal 65535, eval('"' + 'x' * 65535 + '"').size
  assert_raise(SyntaxError) do
    eval('"' + 'x' * 65536 + '"')
  end
end

assert 'eval a symbol name at the width of the symbol length' do
  # The dump records a symbol name's length in 16 bits too, and 0xffff of them
  # is the length it writes for a null symbol, so 65534 bytes is the longest
  # name that comes back as itself.
  assert_equal 65534, eval(':"' + 'x' * 65534 + '"').to_s.size
  assert_raise(SyntaxError) do
    eval(':"' + 'x' * 65535 + '"')
  end
end

assert('String instance_eval') do
  obj = Object.new
  obj.instance_eval{ @test = 'test' }
  assert_raise(ArgumentError) { obj.instance_eval(0) { } }
  assert_raise(ArgumentError) { obj.instance_eval('0', 'test', 0, 'test') }
  assert_equal(['test.rb', 10]) { obj.instance_eval('[__FILE__, __LINE__]', 'test.rb', 10)}
  assert_equal('test') { obj.instance_eval('@test') }
  assert_equal('test') { obj.instance_eval { @test } }
  o = Object.new
  assert_equal ['', o, o], o.instance_eval("[''].each { |s| break [s, o, self] }")
end

assert('Kernel#eval(string) context') do
  class TestEvalConstScope
    EVAL_CONST_CLASS = 'class'
    def const_string
      eval 'EVAL_CONST_CLASS'
    end
  end
  obj = TestEvalConstScope.new
  assert_raise(NameError) { eval 'EVAL_CONST_CLASS' }
  assert_equal('class') { obj.const_string }
end

assert('BasicObject#instance_eval with begin-rescue-ensure execution order') do
  class HellRaiser
    def raise_hell
      order = [:enter_raise_hell]
      begin
        order.push :begin
        self.instance_eval("raise 'error'")
      rescue
        order.push :rescue
      ensure
        order.push :ensure
      end
      order
    end
  end

  hell_raiser = HellRaiser.new
  assert_equal([:enter_raise_hell, :begin, :rescue, :ensure], hell_raiser.raise_hell)
end

assert('BasicObject#instance_eval to define singleton methods Issue #3141') do
  foo_class = Class.new do
    def bar(x)
      instance_eval "def baz; #{x}; end"
    end
  end

  f1 = foo_class.new
  f2 = foo_class.new
  f1.bar 1
  f2.bar 2
  assert_equal(1){f1.baz}
  assert_equal(2){f2.baz}
end

assert('Kernel#eval(string) Issue #4021') do
  assert_equal('FOO') { (eval <<'EOS').call }
foo = "FOO"
Proc.new { foo }
EOS
  assert_equal('FOO') {
    def do_eval(code)
      eval(code)
    end
    do_eval(<<'EOS').call
foo = "FOO"
Proc.new { foo }
EOS
  }
end

assert('Calling the same method as the variable name') do
  hoge = Object.new
  def hoge.fuga
    "Hit!"
  end
  assert_equal("Hit!") { fuga = "Miss!"; eval "hoge.fuga" }
  assert_equal("Hit!") { fuga = "Miss!"; -> { eval "hoge.fuga" }.call }
  assert_equal("Hit!") { -> { fuga = "Miss!"; eval "hoge.fuga" }.call }
  assert_equal("Hit!") { fuga = "Miss!"; eval("-> { hoge.fuga }").call }
end

assert('Access numbered parameter from eval') do
  hoge = Object.new
  def hoge.fuga(a, &b)
    b.call(a)
  end
  assert_equal(6) {
    hoge.fuga(3) { _1 + eval("_1") }
  }
end

assert('Module#class_eval with string') do
  c = Class.new
  c.class_eval "def foo() 42; end"
  cc = c.new
  assert_true cc.respond_to?(:foo)
  assert_equal 42, c.new.foo

  b = c.class_eval("class A; def a; 55; end; end; class B; def b; A; end; end; B")
  assert_equal 55, b.new.b.new.a
end

assert 'method visibility with eval' do
  c = Class.new do
    eval <<~CODE
      private
      def bad!
        "BAD!"
      end
    CODE

    def good!
      "GOOD!"
    end
  end

  assert_raise NoMethodError do
    c.new.bad!
  end

  assert_equal "GOOD!" do
    c.new.good!
  end
end

assert('alias and undef reject a dynamic symbol') do
  # OP_ALIAS and OP_UNDEF carry a symbol index, so an interpolated name cannot
  # be expressed. The codegen used to read the InterpolatedSymbolNode as if it
  # were a SymbolNode, which ran off the end of the node
  # (clusterfuzz 6308929387429888, `alias p:"#{}"`).
  assert_raise(SyntaxError) { eval 'alias p :"#{}"' }
  assert_raise(SyntaxError) { eval 'alias :"#{1}" p' }
  assert_raise(SyntaxError) { eval 'undef :"#{}"' }
end

assert('symbol GC keeps the names of live global variables') do
  # The global outlives the code that set it: once that code is collected its
  # name is reachable from the global variable table alone, which is a root
  # the sweep has to walk in its own right.
  eval("$symbol_gc_eval_global = 99")
  GC.start

  6000.times { |i| "gc-filler-global-name-#{i}".to_sym }
  GC.start

  assert_true global_variables.include?("$symbol_gc_eval_global".to_sym)
end

assert('eval in a scope whose locals are named after a presym literal') do
  # An anonymous rest parameter is recorded under the local name `*`, which is
  # also one of the compiler's presym literals. Parsing the eval interns the
  # enclosing scope's local names into the constant pool first, so `*` is
  # already there when the presym literals go in and they no longer land on
  # consecutive pool ids. The compiler used to derive every presym id from one
  # offset, which gave each presym past `*` the id of its neighbour, so
  # `StandardError` resolved to `call` and `each` to `__case_eqq`.
  q = 1
  results = []
  [11].each { |*|
    results << eval("begin; raise 'x'; rescue => e; 'caught'; end")
    results << eval("s = 0; for i in [1, 2, 3]; s += i; end; s")
    results << eval("q")
  }
  assert_equal ['caught', 6, 1], results
end
