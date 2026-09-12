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

assert 'eval names the line a generator error is on' do
  # eval reads the diagnostic list, and the generator's entries in it used to
  # carry no position at all, so every one of them read as line 0.
  assert_raise_with_message(SyntaxError,
                            "file (eval) line 2: generator error, END not supported") do
    eval("p 1\nEND { }")
  end
  assert_raise_with_message(SyntaxError,
                            "file (eval) line 3: generator error, END not supported") do
    eval("p 1\np 2\nEND { }")
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

assert('a numbered parameter is not a name an eval string can use') do
  # A numbered parameter belongs to the block that spells it, and a string is
  # compiled with no block of its own, so the name is a method call there.
  hoge = Object.new
  def hoge.fuga(a, &b)
    b.call(a)
  end
  assert_equal(3) { hoge.fuga(3) { _1 } }
  assert_raise(NameError) { hoge.fuga(3) { _1 + eval("_1") } }
  assert_raise(NameError) { hoge.fuga(3) { eval("_1") } }
  assert_raise(NameError) { hoge.fuga(3) { |a| eval("_1") } }
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

assert('Module#class_eval with string opens a constant scope') do
  Test4EvalConstOrder = :top
  class Test4EvalConstOrderBase
    Test4EvalConstOrder = :base
  end
  class Test4EvalConstOrderOwner
    Test4EvalConstOrder = :owner
  end
  Test4EvalConstOrderOwner.class_eval <<~CODE
    class Test4EvalConstOrderSub < Test4EvalConstOrderBase
      @in_body = Test4EvalConstOrder
      def in_method; Test4EvalConstOrder; end
      class << self
        attr_reader :in_body
      end
    end
  CODE

  # the receiver's scope comes before the superclass, as with `class` nesting
  assert_equal :owner, Test4EvalConstOrderOwner::Test4EvalConstOrderSub.in_body
  assert_equal :owner, Test4EvalConstOrderOwner::Test4EvalConstOrderSub.new.in_method
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

assert 'a `def` in an eval string takes the visibility around the call' do
  class Test4EvalVisibility
    private
    eval("def from_string; end")
    protected
    eval("def protected_from_string; end")
    public
    eval("def public_from_string; :ok; end")
  end

  o = Test4EvalVisibility.new
  assert_raise(NoMethodError) { o.from_string }
  assert_raise(NoMethodError) { o.protected_from_string }
  assert_include Test4EvalVisibility.protected_instance_methods(false), :protected_from_string
  assert_equal :ok, o.public_from_string
end

assert 'the visibility an eval string starts at is the one its whole scope is at' do
  # The scope reaches the string through the env it shares with the caller, so
  # a block between the two, and a second eval inside the first, are steps on
  # the way to the `private` rather than scopes of their own.
  class Test4EvalVisibilityNested
    private
    [1].each { eval("def from_block; end") }
    eval("eval('def from_nested_string; end')")
  end

  o = Test4EvalVisibilityNested.new
  assert_raise(NoMethodError) { o.from_block }
  assert_raise(NoMethodError) { o.from_nested_string }
end

assert 'a `def` in an eval string follows a `module_function` around the call' do
  module Test4EvalVisibilityModuleFunction
    module_function
    eval("def from_string; :ok; end")
  end

  assert_equal :ok, Test4EvalVisibilityModuleFunction.from_string
  assert_include Test4EvalVisibilityModuleFunction.private_instance_methods(false), :from_string
end

assert 'eval with a binding takes the visibility where the binding was made' do
  class Test4EvalVisibilityPublicScope
    SCOPE = binding
  end
  class Test4EvalVisibilityPrivateScope
    private
    SCOPE = binding
  end

  # the binding names the scope the string runs in, so the visibility comes
  # from there and not from the private scope the `eval` is called in
  class Test4EvalVisibilityBindingCaller
    private
    eval("def from_public_scope; :ok; end", Test4EvalVisibilityPublicScope::SCOPE)
    eval("def from_private_scope; end", Test4EvalVisibilityPrivateScope::SCOPE)
  end

  assert_equal :ok, Test4EvalVisibilityPublicScope.new.from_public_scope
  assert_raise(NoMethodError) { Test4EvalVisibilityPrivateScope.new.from_private_scope }
end

assert 'a visibility written in a string given a binding stays with the scope' do
  # A binding names a scope where a plain eval string copies one, so what the
  # string writes reaches the scope itself and every other binding on it.
  class Test4EvalVisibilityBindingWrite
    FIRST = binding
    SECOND = binding
    eval("private", FIRST)
    def written_after; end
  end
  eval("def from_the_other_binding; end", Test4EvalVisibilityBindingWrite::SECOND)

  o = Test4EvalVisibilityBindingWrite.new
  assert_raise(NoMethodError) { o.written_after }
  assert_raise(NoMethodError) { o.from_the_other_binding }
end

assert 'a string given to class_eval starts public wherever it is called' do
  # `class_eval` gives the string the receiver for a scope rather than the
  # caller's, and a scope of one's own starts at the default.
  class Test4EvalVisibilityClassEvalCaller
    private
    TARGET = Class.new
    TARGET.class_eval("def from_string; :ok; end")
  end

  assert_equal :ok, Test4EvalVisibilityClassEvalCaller::TARGET.new.from_string
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

assert('a string class_eval leaves the caller\'s scope on its own class') do
  # The string runs with the receiver as its cref. That cref used to be
  # written into the env the string shared with the caller's frame, so the
  # caller's later `def` landed on the receiver and a block made afterwards
  # started its constant lookup there.
  c = Class.new
  c.const_set(:EVAL_CREF_PROBE, :receiver)
  Object.const_set(:EVAL_CREF_PROBE, :caller) unless Object.const_defined?(:EVAL_CREF_PROBE, false)
  c.class_eval("def from_string; end")
  def eval_cref_probe_def; end
  assert_false c.private_instance_methods(false).include?(:eval_cref_probe_def)
  assert_true Object.private_instance_methods(false).include?(:eval_cref_probe_def)
  assert_equal :caller, proc { EVAL_CREF_PROBE }.call
  assert_true c.method_defined?(:from_string)
end

assert('a string instance_eval leaves the caller\'s scope on its own class') do
  o = Object.new
  o.instance_eval("def from_string; end")
  Object.instance_eval("1")
  def eval_cref_probe_def2; end
  assert_false o.singleton_methods(false).include?(:eval_cref_probe_def2)
  assert_false Object.singleton_methods(false).include?(:eval_cref_probe_def2)
  assert_true Object.private_instance_methods(false).include?(:eval_cref_probe_def2)
  assert_true o.singleton_methods(false).include?(:from_string)
end

assert('a string class_eval still runs inside the caller\'s scope') do
  # The string now reaches the caller's frame through a binding of it, so
  # what a binding carries is what the string keeps: the locals for reading
  # and writing from any depth, the receiver's constants as its cref, and
  # the caller's method name.
  c = Class.new
  c.const_set(:EVAL_LOCAL_PROBE, :receiver)
  x = 1
  assert_equal 2, c.class_eval("x + 1")
  assert_equal :receiver, c.class_eval("x = 2; EVAL_LOCAL_PROBE")
  assert_equal 2, x
  [1].each { c.class_eval("x = 3") }
  assert_equal 3, x
  assert_equal :receiver, c.class_eval("proc { EVAL_LOCAL_PROBE }").call
  k = Class.new { def probe(c); c.class_eval("__method__"); end }
  assert_equal :probe, k.new.probe(c)
  # A C function as the caller has no frame to bind; the string still
  # defines on the receiver.
  c.method(:class_eval).call("def from_c_caller; end")
  assert_true c.method_defined?(:from_c_caller)
end

assert('a string given to eval is named for the method that called eval') do
  # The string runs on a frame of its own, pushed on top of the C frame of
  # `eval`, and that frame used to carry `eval` as its method name: a `super`
  # in the string looked for the superclass method of `eval` itself, and
  # `defined?(super)` answered `"super"` in a method that has no superclass
  # method to call.
  base = Class.new do
    def m(x); [:base, x]; end
    def has_super(x); end
  end
  sub = Class.new(base) do
    def m(x); eval("super(x + 10)"); end
    def has_super(x); eval("defined?(super)"); end
    def no_super; eval("defined?(super)"); end
    def named; eval("__method__"); end
  end
  o = sub.new

  assert_equal [:base, 11], o.m(1)
  assert_equal 'super', o.has_super(1)
  assert_nil o.no_super
  assert_equal :named, o.named

  # Outside a method there is no name to carry, and `eval`'s own must not
  # stand in for one.
  assert_nil eval("__method__")
  assert_nil binding.eval("__method__")
  assert_raise(NoMethodError) { eval("super") }
end

assert('`super` and `yield` in a string given to eval belong to the caller') do
  # The string's own scope chain holds no method scope, so the argument
  # layout that a bare `super` forwards and that `yield` finds the block by
  # comes from the method on the proc chain the compile context carries.
  base = Class.new do
    def m(x); [:base, x]; end
    def blk; block_given? ? yield(:b) : :noblk; end
    def kw(a:, b: 2); [a, b]; end
    def rest(a:, **o); [a, o]; end
  end
  sub = Class.new(base) do
    def m(x); eval("super"); end
    def blk; eval("super"); end
    def kw(a:, b: 2); eval("super"); end
    def rest(a:, **o); eval("a = 7; [0].each { return super }"); end
    def y; eval("yield 21"); end
    def y_nested; eval("[1].map { yield 2 }"); end
    def y_args(a, b = 1, *r, c, d: 4, &e); eval("yield a"); end
  end
  o = sub.new

  assert_equal [:base, 1], o.m(1)
  assert_equal [:blk, :b], o.blk { |v| [:blk, v] }
  # the keyword locals the string reads are the method's, by name, and the
  # `rest` it copies is the one the method's frame holds
  assert_equal [1, 3], o.kw(a: 1, b: 3)
  assert_equal [7, {c: 2}], o.rest(a: 1, c: 2)
  assert_equal 42, o.y { |v| v * 2 }
  assert_equal [4], o.y_nested { |v| v * 2 }
  assert_equal 35, o.y_args(7, 8) { |v| v * 5 }

  # Outside a method there is still no block to reach.
  assert_raise(SyntaxError) { eval("yield") }
end

assert('`return` in a string given to eval leaves the calling method') do
  # `OP_RETURN` returns to the string's own frame, whose caller is the C
  # function `eval`, so the value became `eval`'s and the method carried on.
  # A `return` here leaves the method the way one from a block does.
  k = Class.new do
    def ret; eval("return :from_string"); :after_eval; end
    def ret_nested; eval("eval('return :from_nested')"); :after_eval; end
    def ret_def; eval("def inner; return :inner; end"); inner; end
    def ret_lambda; eval("-> { return :lambda }.call"); end
  end
  o = k.new

  assert_equal :from_string, o.ret
  assert_equal :from_nested, o.ret_nested
  assert_equal :inner, o.ret_def
  assert_equal :lambda, o.ret_lambda
end

assert('a string given to eval in a `define_method` block sees the closure') do
  # `define_method` marks the block it installs a scope, the way `def` marks a
  # method body, but the block keeps the closure it was made with: what a
  # direct reference reaches from inside it, `eval` reaches too.
  class TestEvalDefineMethod
    x = 10
    define_method(:direct) { x }
    define_method(:read) { eval("x") }
    define_method(:own) { |a| b = 1; eval("[a, b, x]") }
    define_method(:nested) { [1].map { eval("x") } }
    define_method(:by_lambda, lambda { eval("x") })
    define_method(:write) { eval("x = 20") }

    class << self
      y = 30
      define_method(:sclass_read) { eval("y") }
    end
  end

  # A scope with no locals of its own is still a scope the block closes over,
  # and the walk that builds the parser's scope list has to end at it all the
  # same: it is the shape a binding's local-variable space takes too.
  class TestEvalEmptyScope
    1.times do
      z = 40
      define_method(:from_block) { eval("z") }
    end
  end

  k = TestEvalDefineMethod
  o = k.new

  assert_equal 10, o.direct
  assert_equal 10, o.read
  assert_equal [5, 1, 10], o.own(5)
  assert_equal [10], o.nested
  assert_equal 10, o.by_lambda
  assert_equal 30, k.sclass_read
  assert_equal 40, TestEvalEmptyScope.new.from_block

  # The store reaches the captured local itself, so the direct reference and
  # the next instance both see it.
  assert_equal 20, o.write
  assert_equal 20, o.direct
  assert_equal 20, k.new.read
end

assert('a constant defined in a string given to eval belongs to the class the method was written in') do
  # The frame of a method written `def self.name` carries the singleton
  # class the method was found in, and a constant, class or module the
  # string defines used to go there, or to `Object` through a binding.
  # CRuby adds it to the cref, the class the code was written in.
  class TestEvalConstDef
    def self.direct; eval("FROM_DIRECT = 1"); end
    def self.nested; proc { eval("FROM_NESTED = 2") }.call; end
    def self.bound; binding.eval("FROM_BOUND = 3"); end
    def self.opened; eval("class Opened; end; module OpenedMod; end"); end
    class << self
      def sclass; eval("FROM_SCLASS = 4"); end
    end
    def plain; eval("FROM_PLAIN = 5"); end
  end
  TestEvalConstDef.direct
  TestEvalConstDef.nested
  TestEvalConstDef.bound
  TestEvalConstDef.opened
  TestEvalConstDef.sclass
  TestEvalConstDef.new.plain
  assert_equal [1, 2, 3, 5], [
    TestEvalConstDef::FROM_DIRECT, TestEvalConstDef::FROM_NESTED,
    TestEvalConstDef::FROM_BOUND, TestEvalConstDef::FROM_PLAIN]
  assert_true TestEvalConstDef.const_defined?(:Opened, false)
  assert_true TestEvalConstDef.const_defined?(:OpenedMod, false)
  sclass = TestEvalConstDef.singleton_class
  assert_false sclass.const_defined?(:FROM_NESTED, false)
  assert_false Object.const_defined?(:FROM_BOUND, false)
  # A method written in `class << self` is written in the singleton class.
  assert_equal 4, sclass::FROM_SCLASS
  assert_false TestEvalConstDef.const_defined?(:FROM_SCLASS, false)
end

assert('a string given to eval in a `def` body has no scope around it') do
  # A method body carries no closure, so a local of the scope it was written
  # in is not a name it can reach: it is a method call there.
  class TestEvalDefScope
    x = 10
    def hidden; eval("x"); end
    def self.hidden_singleton; eval("x"); end

    class << self
      y = 30
      def hidden_sclass; eval("y"); end
    end
  end

  assert_raise(NameError) { TestEvalDefScope.new.hidden }
  assert_raise(NameError) { TestEvalDefScope.hidden_singleton }
  assert_raise(NameError) { TestEvalDefScope.hidden_sclass }
end

class EvalVisHidden
  private
  def make; eval("def written; :w; end"); end
end

class EvalVisShown
  public
  def make; eval("def written; end"); end
end

class EvalVisGuarded
  protected
  def make; eval("def written; end"); end
end

class EvalVisLater
  def make; eval("def written; end"); end
  private
end

class EvalVisReopened
  def make; eval("def written; end"); end
end
class EvalVisReopened
  private
end

assert('eval string in a method starts at the visibility of the scope the method was written in') do
  # A class body keeps one visibility for the whole of itself and a `def` in
  # it keeps none of its own, so a string evaluated inside such a method
  # reads the body's, as it stands rather than as it stood at the `def`.
  o = EvalVisHidden.new
  o.send(:make)
  assert_false o.respond_to?(:written)
  assert_true o.respond_to?(:written, true)
  assert_equal :w, o.send(:written)

  o = EvalVisShown.new
  o.make
  assert_true o.respond_to?(:written)

  o = EvalVisGuarded.new
  o.send(:make)
  assert_false o.respond_to?(:written)
  assert_true o.respond_to?(:written, true)

  # written after the `def` and reaching it: one body, one visibility
  o = EvalVisLater.new
  o.send(:make)
  assert_false o.respond_to?(:written)

  # a body opened again is a scope of its own, and what it says reaches
  # nothing the first one wrote
  o = EvalVisReopened.new
  o.make
  assert_true o.respond_to?(:written)
end

module EvalFrameClassMaker
  # A `Class.new` block written in a method: the methods it defines have
  # this module for their cref, the way a script's have `Object`.
  def self.subclass(base)
    Class.new(base) do
      def m(x); eval("super"); end
      def m_args(x); eval("super(x + 1)"); end
      def nested(x); [1].map { eval("super") }[0]; end
      def has_super; eval("defined?(super)"); end
      def own_super_after(x); eval("1"); super; end
      def bound(x); binding.eval("super"); end
    end
  end
end

assert('a string given to eval runs under the class the calling frame runs under') do
  # The string's frame, and the env the string leaves on the caller's frame,
  # took their class from the caller's proc, which holds the cref: for a
  # method written in a `Class.new` block that is the scope around the
  # block, and for a block given a class to run under it is the scope the
  # block was written in. A `super` in the string looked above that class,
  # and once the env was there the caller's own `super`, and a `def` written
  # after the call in a block given a class, went to it as well. The class a
  # frame runs under is the one the method was found in, or the one the
  # block was given.
  base = Class.new do
    def m(x); [:base, x]; end
    def m_args(x); [:base, x]; end
    def nested(x); [:base, x]; end
    def has_super; end
    def own_super_after(x); [:base, x]; end
    def bound(x); [:base, x]; end
  end
  o = EvalFrameClassMaker.subclass(base).new
  assert_equal [:base, 1], o.m(1)
  assert_equal [:base, 2], o.m_args(1)
  assert_equal [:base, 3], o.nested(3)
  assert_equal 'super', o.has_super
  assert_equal [:base, 4], o.own_super_after(4)
  assert_equal [:base, 5], o.bound(5)

  # a block given a class: a `def` after the call, one in a string, and one
  # in a block made after the call all go to the given class
  c = Class.new
  c.class_eval { eval("1"); def after_call; end }
  c.class_eval { eval("def in_string; end") }
  c.class_eval { eval("1"); [1].each { def in_block; end } }
  assert_equal [:after_call, :in_block, :in_string], c.instance_methods(false).sort
  assert_false Object.new.respond_to?(:after_call, true)
  assert_false Object.new.respond_to?(:in_string, true)
  assert_false Object.new.respond_to?(:in_block, true)
  o = Object.new
  o.instance_eval { eval("1"); def on_self; end }
  assert_equal [:on_self], o.singleton_methods

  # a constant the string defines still belongs to the scope the block was
  # written in, not to the class the block was given
  k = Class.new { eval("EvalFrameClassConst = 1") }
  assert_true Object.const_defined?(:EvalFrameClassConst, false)
  assert_false k.const_defined?(:EvalFrameClassConst, false)
end

module EvalGivenMethodMaker
  # Run in a method, so that the block's cref is this module rather than the
  # class `mrb_proc_new()` falls back to when a block at the top level of a
  # compiled test file has no cref to answer with.
  def self.klass
    Class.new do
      def in_string; eval("def from_string; end"); end
      def in_binding; eval("def from_binding; end", binding); end
    end
  end
end

assert('a `def` in a string evaluated in a method written in a block given a class adds to that class') do
  # The string's frame runs under the class the method was found in, and the
  # method carries the class its block was given, so the `def` lands where
  # one written in the method body does.
  c = EvalGivenMethodMaker.klass
  c.new.in_string
  c.new.in_binding
  assert_equal [:from_binding, :from_string, :in_binding, :in_string], c.instance_methods(false).sort
  assert_false Object.new.respond_to?(:from_string, true)
  assert_false Object.new.respond_to?(:from_binding, true)
end

module ConstCacheIrepReuse
  X = :outer
  class Inner
    X = :inner
  end
end

assert('constant read by an eval whose irep took a freed irep\'s address') do
  # The constant cache is keyed by the irep's address. Once the irep of one
  # eval string is collected, the next eval string can be compiled into the
  # same address, and a read of the same constant name from another scope
  # used to be answered from the stale entry.
  seen = []
  8.times do
    seen << [ConstCacheIrepReuse::Inner.class_eval("X"),
             ConstCacheIrepReuse.class_eval("X")]
    GC.start
  end
  assert_equal [[:inner, :outer]] * 8, seen
end

ConstCacheTestValue = :probe

assert('the constant cache forgets an irep when the irep is freed') do
  # Same defect as above, seen from the cache itself: after the irep of a
  # constant read is freed no entry may still be keyed by its address,
  # whatever the allocator does with that address next.
  dangles = ConstCacheTest.dangles_after_irep_free?("ConstCacheTestValue")
  skip "this build has no constant cache" if dangles.nil?
  assert_false dangles
end

assert('eval of a pattern deeper than the compiler walks') do
  # A pattern is walked by a recursion of its own, which nothing bounded: a
  # pattern nested as deep as it is written ran the compiler off the C stack,
  # and the walk that gave the tree back afterwards would have too. It goes
  # on the count the rest of the compiler keeps, and the tree comes from an
  # arena that is given back in one piece rather than walked.
  #
  # Deep enough that the walk this replaces would not have survived it on the
  # megabyte of stack Windows gives a thread.
  assert_raise(SyntaxError) { eval("SOK  =>_xec" * 60000) }
  # the compiler is still there afterwards, and an ordinary pattern still
  # compiles and matches
  assert_equal [1, 2], eval("q = [1, 2]; q => [a, b]; [a, b]")
  assert_true eval("({k: 1} in {k:})")
end

assert('eval of a nesting Prism would recurse through') do
  # Prism refuses to parse deeper than PRISM_DEPTH_MAXIMUM where it parses an
  # expression, but the walk over a pattern carries the count without ever
  # reading it, so a pattern nested as deep as it is written recursed until
  # the C stack ran out. The brackets the lexer opens are counted instead,
  # and the one past the limit is given to the parser as the end of input.
  assert_raise(SyntaxError) { eval("case 1\nin " + "[" * 100000 + "1" + "]" * 100000 + " then 1\nend") }
  assert_raise(SyntaxError) { eval("case 1\nin " + "{a: " * 100000 + "1" + "}" * 100000 + " then 1\nend") }
  # a nesting Prism accepts is parsed as before, and the count is per compile
  a = eval("[" * 250 + "1" + "]" * 250)
  250.times { a = a[0] }
  assert_equal 1, a
  assert_equal 2, eval("[1].map { |v| eval('[[[2]]]')[0][0][0] }[0]")
  assert_equal [1, 2], eval("q = [1, 2]; q => [a, b]; [a, b]")
end

assert 'eval a `super` and a `yield` at the width of the forwarding level' do
  # `OP_ARGARY` and `OP_BLKPUSH` carry the level between the asker and its
  # method scope in four bits of their operand, so fifteen nested blocks is
  # the deepest either can still name the frame it forwards from.  Deeper
  # than that the level wrapped and the pair read another frame's registers.
  zsuper = lambda do |depth|
    "class EvalLevelParent; def m(a) [a, :parent] end end\n" \
    "class EvalLevelChild < EvalLevelParent; def m(a)\n" +
    "[1].each { " * depth + "$eval_level = super" + " }" * depth +
    "\n$eval_level\nend end\nEvalLevelChild.new.m(1)"
  end
  assert_equal [1, :parent], eval(zsuper.call(15))
  assert_raise(SyntaxError) { eval(zsuper.call(16)) }

  yielder = lambda do |depth|
    "def eval_level_yielder\n" +
    "[1].each { " * depth + "$eval_level = yield" + " }" * depth +
    "\n$eval_level\nend\neval_level_yielder { :ok }"
  end
  assert_equal :ok, eval(yielder.call(15))
  assert_raise(SyntaxError) { eval(yielder.call(16)) }
end

assert 'eval a `super` and a `yield` at the width of the forwarded layout' do
  # The rest of that operand holds `ainfo`, the layout of the arguments being
  # forwarded, whose mandatory and optional parameters share a field of six
  # bits that only twelve bits of room are left for.  The compiler allows 31
  # of each, so 32 counted together is where the layout runs into the level.
  params = lambda do |ma, oa|
    ((1..ma).map { |i| "a#{i}" } + (1..oa).map { |i| "b#{i} = #{i}" }).join(', ')
  end

  zsuper = lambda do |ma, oa|
    "class EvalWideParent; def m(#{params.call(ma, oa)}) a1 end end\n" \
    "class EvalWideChild < EvalWideParent; def m(#{params.call(ma, oa)}) super end end\n" \
    "EvalWideChild.new.m(#{(1..ma).to_a.join(', ')})"
  end
  assert_equal 1, eval(zsuper.call(16, 15))
  assert_raise(SyntaxError) { eval(zsuper.call(16, 16)) }

  yielder = lambda do |ma, oa|
    "def eval_wide_yielder(#{params.call(ma, oa)}) yield a1 end\n" \
    "eval_wide_yielder(#{(1..ma).to_a.join(', ')}) { |v| v }"
  end
  assert_equal 1, eval(yielder.call(16, 15))
  assert_raise(SyntaxError) { eval(yielder.call(16, 16)) }
end

assert('eval of a multiple assignment with more post-splat targets than fit') do
  # The post count is the third operand of a `BBB` instruction, and `OP_EXT1`
  # to `OP_EXT3` widen only the first two. The pre targets stay in range by
  # rebasing the source array once their index reaches the width, which the
  # post targets cannot do: whether they are filled from the front or from the
  # back depends on how long the array turns out to be, and every rebase would
  # decide that over again for the group it splits off.
  masgn = lambda do |n|
    "*r, " + (0...n).map { |i| "@masgn_post#{i}" }.join(", ") + " = (0...#{n + 1}).to_a"
  end
  assert_equal 255, eval(masgn.call(255) + "; @masgn_post254")
  assert_raise(SyntaxError) { eval(masgn.call(256)) }
end

assert('eval of a local variable whose name is too long for a symbol') do
  # The lv table is dumped with a 16-bit name length, and every lv name is
  # interned into an mrb_sym, which refuses 0xffff bytes and up. A name that
  # long used to leave codegen as a valid irep and raise ArgumentError from the
  # glue instead, leaking the parser it raised past; now it is a codegen error
  # like a method name of the same length already was.
  name = "lv_too_long_" + "x" * 0x10000
  assert_raise(SyntaxError) { eval("#{name} = 1") }
  assert_raise(SyntaxError) { eval("#{name} = 1", binding) }
  assert_raise(SyntaxError) { eval("[1].each { |#{name}| }") }
  # One byte short of the bound is an ordinary local variable.
  short = "lv_just_fits_" + "x" * (0xfffe - "lv_just_fits_".size)
  assert_equal 1, eval("#{short} = 1; #{short}")
end

assert('eval with a filename too long for a symbol') do
  # The filename is interned too. Without a binding it was already refused
  # before anything was allocated; with one, the binding pass parsed first and
  # the parser raised from under itself, leaking its state.
  # The message tells the two apart: "symbol length too long" is the parser
  # raising from under itself, "filename too long" is eval refusing up front.
  file = "f" * 0x10000
  assert_raise_with_message(ArgumentError, "filename too long") { eval("1", nil, file) }
  assert_raise_with_message(ArgumentError, "filename too long") { eval("1", binding, file) }
  assert_equal 1, eval("1", binding, "f" * 0xfffe)
end
