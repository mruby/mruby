class Base
  def foo() :base end
end

class Derived < Base
  def foo() :derived end
end

class Interpreter
  attr_accessor :ret

  def do_a() @ret += "there, "; end
  def do_d() @ret += "Hello ";  end
  def do_e() @ret += "!\n";     end
  def do_v() @ret += "Dave";    end
  Dispatcher = {
    "a" => instance_method(:do_a),
    "d" => instance_method(:do_d),
    "e" => instance_method(:do_e),
    "v" => instance_method(:do_v)
  }
  def interpret(string)
    @ret = ""
    string.split("").each {|b| Dispatcher[b].bind(self).call }
  end
end

assert 'demo' do
  interpreter = Interpreter.new
  interpreter.interpret('dave')
  assert_equal "Hello there, Dave!\n", interpreter.ret
end

assert 'Method#arity' do
  Class.new {
    attr_accessor :done
    def initialize; @done = false; end
    def m0() end
    def m1(a) end
    def m2(a, b) end
    def mo1(a = nil, &b) end
    def mo2(a, b = nil) end
    def mo3(*a) end
    def mo4(a, *b, &c) end
    def mo5(a, *b, c) end
    def mo6(a, *b, c, &d) end
    def mo7(a, b = nil, *c, d, &e) end
    def ma1((a), &b) nil && a end

    def run
      assert_equal(0, method(:m0).arity)
      assert_equal(1, method(:m1).arity)
      assert_equal(2, method(:m2).arity)
      assert_equal(-1, method(:mo1).arity)
      assert_equal(-2, method(:mo2).arity)
      assert_equal(-1, method(:mo3).arity)
      assert_equal(-2, method(:mo4).arity)
      assert_equal(-3, method(:mo5).arity)
      assert_equal(-3, method(:mo6).arity)
      assert_equal(-3, method(:mo7).arity)
      assert_equal(1, method(:ma1).arity)

      assert_equal(-2, method(:__send__).arity)
      assert_equal(-1, method(:nothing).arity)
    end

    def respond_to_missing?(m, b)
      m == :nothing
    end
  }.new.run
end

assert 'Method and UnboundMethod should not be have a `new` method' do
  assert_raise(NoMethodError){ Method.new }
  assert_raise(NoMethodError){ UnboundMethod.new }
end

assert 'instance' do
  assert_kind_of Method, 1.method(:+)
  assert_kind_of UnboundMethod, Integer.instance_method(:+)
end

assert 'Method#call' do
  assert_equal 3, 1.method(:+).call(2)
  assert_equal "ab", "a".method(:+)["b"]
  klass = Class.new {
    def foo; 42; end
  }
  klass2 = Class.new(klass) {
    def foo; super; end
  }
  assert_equal 42, klass2.new.method(:foo).call

  i = Class.new {
    def bar
      yield 3
    end
  }.new
  assert_raise(LocalJumpError) { i.method(:bar).call }
  assert_equal 3, i.method(:bar).call { |i| i }

  assert_raise(ArgumentError) { nil.method(:__id__).call nil, 1 }
  assert_raise(ArgumentError) { nil.method(:__id__).call nil, opts: 1 }
end

assert 'Method#call for regression' do
  obj = BasicObject.new
  assert_equal String, Kernel.instance_method(:inspect).bind(obj).call().class, "https://github.com/ksss/mruby-method/issues/4"
end

assert 'Method#call with undefined method' do
  c = Class.new {
    attr_accessor :m, :argv
    def respond_to_missing?(m, b)
      m == :foo
    end

    def method_missing(m, *argv)
      @m = m
      @argv = argv
      super
    end
  }
  cc = c.new
  assert_raise(NameError) { cc.method(:nothing) }
  assert_kind_of Method, cc.method(:foo)
  assert_raise(NoMethodError) { cc.method(:foo).call(:arg1, :arg2) }
  assert_equal :foo, cc.m
  assert_equal [:arg1, :arg2], cc.argv

  cc = c.new
  m = cc.method(:foo)
  c.class_eval do
    def foo
      :ng
    end
  end
  assert_raise(NoMethodError) { m.call(:arg1, :arg2) }
end

assert 'Method#call with undefined method -- only kwargs' do
  c = Class.new {
    attr_accessor :m, :argv, :kwargs
    def respond_to_missing?(m, b)
      m == :foo
    end

    def method_missing(m, *argv, **kwargs)
      @m = m
      @argv = argv
      @kwargs = kwargs
      super
    end
  }
  cc = c.new
  assert_kind_of Method, cc.method(:foo)

  # Calling cc.method(:foo) works
  assert_raise(NoMethodError) { cc.method(:foo).call(kwarg1: :val1, kwarg2: :val2) }
  assert_equal :foo, cc.m
  assert_equal [], cc.argv
  assert_equal({ kwarg1: :val1, kwarg2: :val2 }, cc.kwargs)

  # calling cc.foo fails
  assert_raise(NoMethodError) { cc.foo(kwarg1: :val1, kwarg2: :val2) }
  assert_equal :foo, cc.m
  assert_equal [], cc.argv
  assert_equal({ kwarg1: :val1, kwarg2: :val2 }, cc.kwargs)
end

assert 'Method#source_location' do
  skip if proc{}.source_location.nil?

  filename = __FILE__
  klass = Class.new

  lineno = __LINE__ + 1
  klass.define_method(:find_me_if_you_can) {}
  assert_equal [filename, lineno], klass.new.method(:find_me_if_you_can).source_location

  lineno = __LINE__ + 1
  class <<klass; define_method(:s_find_me_if_you_can) {}; end
  assert_equal [filename, lineno], klass.method(:s_find_me_if_you_can).source_location

  klass = Class.new { def respond_to_missing?(m, b); m == :nothing; end }
  assert_nil klass.new.method(:nothing).source_location
end

assert 'UnboundMethod#source_location' do
  skip if proc{}.source_location.nil?

  filename = __FILE__
  klass = Class.new {
    def respond_to_missing?(m, b)
      m == :nothing
    end
  }

  lineno = __LINE__ + 1
  klass.define_method(:find_me_if_you_can) {}
  assert_equal [filename, lineno], klass.instance_method(:find_me_if_you_can).source_location
  assert_nil klass.new.method(:nothing).unbind.source_location
end

assert 'Method#parameters' do
  klass = Class.new {
    def foo(a, b=nil, *c) end
    def respond_to_missing?(m, b)
      m == :missing
    end
  }
  assert_equal [[:req, :a], [:opt, :b], [:rest, :c]], klass.new.method(:foo).parameters
  assert_equal [[:rest]], klass.new.method(:missing).parameters
end

assert 'UnboundMethod#parameters' do
  klass = Class.new {
    def foo(a, b=nil, *c) end
    def respond_to_missing?(m, b)
      m == :nothing
    end
  }
  assert_equal [[:req, :a], [:opt, :b], [:rest, :c]], klass.instance_method(:foo).parameters
  assert_equal [[:rest]], klass.new.method(:nothing).unbind.parameters
end

assert 'Method#to_proc' do
  m = 3.method(:+)
  assert_kind_of Proc, m.to_proc
  assert_equal 7, m.call(4)

  o = Object.new
  def o.foo(a, b=nil, *c)
    [a, b, c]
  end
  assert_equal [:bar, nil, []], o.method(:foo).to_proc.call(:bar)
#  We can fix this issue but leave until the problem
#  assert_equal o.method(:foo).arity, o.method(:foo).to_proc.arity

  def o.bar
    yield 39
  end
  assert_equal 42, o.bar(&3.method(:+))

  def o.baz(x, y, z, *w, u:, v:, **opts, &blk)
    { x:, y:, z:, w:, u:, v:, opts:, blk: }
  end
  blk = -> { }
  values = { x: 1, y: 2, z: 3, w: [4, 5, 6], u: 7, v: 8, opts: { s: 9, t: 10 }, blk: blk }
  assert_equal values, o.method(:baz).to_proc.call(1, 2, 3, 4, 5, 6, u: 7, v: 8, s: 9, t: 10, &blk)
  assert_equal values, o.method(:baz).to_proc.call(1, 2, 3, 4, 5, 6, **{ u: 7, v: 8, s: 9, t: 10 }, &blk)
  assert_equal values, o.method(:baz).to_proc.call(*[1, 2, 3, 4, 5, 6], u: 7, v: 8, s: 9, t: 10, &blk)
  assert_equal values, o.method(:baz).to_proc.call(*[1, 2, 3, 4, 5, 6], **{ u: 7, v: 8, s: 9, t: 10 }, &blk)

  assert_raise(ArgumentError) { nil.method(:__id__).to_proc.call nil, 1 }
  assert_raise(ArgumentError) { nil.method(:__id__).to_proc.call nil, opts: 1 }
end

assert 'to_s' do
  o = Object.new
  def o.foo; end
  m = o.method(:foo)
  assert_match("#<UnboundMethod: #{ class << o; self; end.inspect }#foo*>", m.unbind.inspect)

  c = Class.new
  c.class_eval { def foo; end; }
  m = c.new.method(:foo)
  assert_match("#<Method: #{ c.inspect }#foo*>", m.inspect)
  m = c.instance_method(:foo)
  assert_match("#<UnboundMethod: #{ c.inspect }#foo*>", m.inspect)
end

assert 'owner' do
  c = Class.new do
    def foo; end
    def self.bar; end
  end
  m = Module.new do
    def baz; end
  end
  c.include(m)
  c2 = Class.new(c)

  assert_equal(c, c.instance_method(:foo).owner)
  assert_equal(c, c2.instance_method(:foo).owner)

  assert_equal(c, c.new.method(:foo).owner)
  assert_equal(c, c2.new.method(:foo).owner)
  assert_equal((class <<c; self; end), c2.method(:bar).owner)
end

assert 'owner missing' do
  c = Class.new do
    def respond_to_missing?(name, bool)
      name == :foo
    end
  end
  c2 = Class.new(c)
  assert_equal(c, c.new.method(:foo).owner)
  assert_equal(c2, c2.new.method(:foo).owner)
end

assert 'receiver name owner' do
  o = Object.new
  def o.foo; end
  m = o.method(:foo)
  assert_equal(o, m.receiver)
  assert_equal(:foo, m.name)
  assert_equal(class << o; self; end, m.owner)
  assert_equal(:foo, m.unbind.name)
  assert_equal(class << o; self; end, m.unbind.owner)
end

assert 'Method#unbind' do
  assert_equal(:derived, Derived.new.foo)
  um = Derived.new.method(:foo).unbind
  assert_kind_of(UnboundMethod, um)
  Derived.class_eval do
    def foo() :changed end
  end
  assert_equal(:changed, Derived.new.foo)
  assert_equal(:changed, Derived.new.foo{})
  assert_equal(:derived, um.bind(Derived.new).call)
  assert_raise(TypeError) do
    um.bind(Base.new)
  end

  # TODO:
  #  Block passed method not handled correctly with workaround.
  #  See comment near `mrb_funcall_with_block` for detail.
  # assert_equal(:derived, um.bind(Derived.new).call{})
end

assert 'Kernel#method' do
  c1 = Class.new {
    def foo; :foo; end
  }
  o = c1.new
  assert_kind_of Method, o.method(:foo)
  assert_kind_of Method, o.method('foo')
  assert_raise(TypeError) { o.method(nil) }
  assert_raise(NameError) { o.method('bar') }
  assert_raise(NameError) { o.method(:bar) }
end

assert "Module#instance_method" do
  assert_kind_of UnboundMethod, Object.instance_method(:object_id)
  assert_raise(NameError) { Object.instance_method(:nothing) }
  c = Class.new {
    def respond_to_missing?(m, b)
      false
    end
  }
  assert_raise(NameError) { c.instance_method(:nothing) }
end

assert 'Kernel#singleton_method' do
  c1 = Class.new {
    def foo; :foo; end
  }
  o = c1.new
  def o.bar; :bar; end
  assert_kind_of Method, o.method(:foo)
  assert_raise(NameError) { o.singleton_method(:foo) }
  assert_kind_of Method, o.singleton_method(:bar)
  assert_raise(TypeError) { o.singleton_method(nil) }
  m = assert_nothing_raised(NameError) { break o.singleton_method(:bar) }
  assert_equal(:bar, m.call)
end

assert 'Method#super_method' do
  o = Derived.new
  m = o.method(:foo).super_method
  assert_equal(Base, m.owner)
  assert_true(o.equal? m.receiver)
  assert_equal(:foo, m.name)
  assert_nil(m.super_method)

  c = Class.new {
    def foo; end
  }
  o = c.new
  o.extend Module.new {
    def foo; end
  }
  assert_equal c, o.method(:foo).super_method.owner
  assert_equal :foo, o.method(:foo).super_method.name
  assert_equal o, o.method(:foo).super_method.receiver
end

assert 'Method#==' do
  o = Object.new
  class << o
    def foo; end
  end
  assert_not_equal(o.method(:foo), nil)
  m = o.method(:foo)
  def m.foo; end
  # TODO: assert_not_equal(o.method(:foo), m)
  assert_equal(o.method(:foo), o.method(:foo))
  # TODO: assert_false(o.method(:foo).eql? m)
  assert_true(o.method(:foo).eql? o.method(:foo))

  assert_false(0.method(:+) == 1.method(:+))
  assert_false(0.method(:+) == 0.method(:-))
  a = 0.method(:+)
  assert_true(a.method(:==) == a.method(:eql?))
end

assert "Method#initialize_copy" do
  c = Class.new {
    def foo
    end
  }.new
  m1 = c.method(:foo)
  m2 = m1.clone
  assert_equal(m1, m2)
end

assert "Method#<< and Method#>>" do
  obj = Object.new
  class << obj
    def mul2(n); n * 2; end
    def add3(n); n + 3; end
  end

  f = obj.method(:mul2)
  g = obj.method(:add3)

  m1 = f << g
  assert_kind_of Proc, m1
  assert_equal 16, m1.call(5)

  m2 = f >> g
  assert_kind_of Proc, m2
  assert_equal 13, m2.call(5)
end

assert 'UnboundMethod#arity' do
  c = Class.new {
    def foo(a, b)
    end

    def respond_to_missing?(m, b)
      m == :nothing
    end
  }
  assert_equal 2, c.instance_method(:foo).arity
  assert_equal(-1, c.new.method(:nothing).unbind.arity)
end

assert 'UnboundMethod#==' do
  assert_false(Integer.instance_method(:+) == Integer.instance_method(:-))
  assert_true(Integer.instance_method(:+) == Integer.instance_method(:+))
  assert_true(UnboundMethod.instance_method(:==) == UnboundMethod.instance_method(:eql?))
  skip unless Object.const_defined?(:Float)
  assert_false(Integer.instance_method(:+) == Float.instance_method(:+))
end

assert 'UnboundMethod#super_method' do
  m = Derived.instance_method(:foo)
  m = m.super_method
  assert_equal(Base.instance_method(:foo), m)
  assert_nil(m.super_method)

  m = Object.instance_method(:object_id)
  assert_nil(m.super_method)
end

assert 'UnboundMethod#bind' do
  m = Module.new{ def meth() :meth end }.instance_method(:meth)
  assert_raise(ArgumentError) { m.bind }
  assert_kind_of Method, m.bind(1)
  assert_kind_of Method, m.bind(:sym)
  assert_kind_of Method, m.bind(Object.new)
  assert_equal(:meth, m.bind(1).call)
  assert_equal(:meth, m.bind(:sym).call)
  assert_equal(:meth, m.bind(Object.new).call)
  sc = nil
  Class.new {
    sc = class << self
      def foo
      end
      self
    end
  }
  assert_raise(TypeError) { sc.instance_method(:foo).bind([]) }
  assert_raise(TypeError) { Array.instance_method(:each).bind(1) }
  assert_kind_of Method, Object.instance_method(:object_id).bind(Object.new)
end

assert 'UnboundMethod#bind_call' do
  m = Array.instance_method(:size)
  assert_equal(:size, m.name)
  assert_equal(0, m.bind_call([]))
  assert_equal(1, m.bind_call([1]))
  assert_equal(2, m.bind_call([1,2]))

  o = Object.new
  def m(x, y, z, *w, u:, v:, **opts, &blk)
    { x:, y:, z:, w:, u:, v:, opts:, blk: }
  end
  m = o.method(:m).unbind
  blk = -> { }
  values = { x: 1, y: 2, z: 3, w: [4, 5, 6], u: 7, v: 8, opts: { s: 9, t: 10 }, blk: blk }
  assert_equal values, m.bind_call(o, 1, 2, 3, 4, 5, 6, u: 7, v: 8, s: 9, t: 10, &blk)
  assert_equal values, m.bind_call(o, 1, 2, 3, 4, 5, 6, **{ u: 7, v: 8, s: 9, t: 10 }, &blk)
  assert_equal values, m.bind_call(o, *[1, 2, 3, 4, 5, 6], u: 7, v: 8, s: 9, t: 10, &blk)
  assert_equal values, m.bind_call(o, *[1, 2, 3, 4, 5, 6], **{ u: 7, v: 8, s: 9, t: 10 }, &blk)
  assert_raise(ArgumentError) { m.bind_call }

  assert_raise(ArgumentError) { BasicObject.instance_method(:__id__).bind_call nil, 1 }
  assert_raise(ArgumentError) { BasicObject.instance_method(:__id__).bind_call nil, opts: 1 }
end

assert 'Method#parameters and #arity on aliased methods' do
  # Regression: an alias proc carries the original method's name in body.mid
  # (not an irep), with `upper` pointing at the original proc. Both
  # mrb_proc_parameters (mruby-proc-ext) and mrb_proc_arity (core src/proc.c)
  # used to fall into their irep branch for alias procs and dereference body.mid
  # as an mrb_irep* -> SEGV / misaligned read. Both must resolve through `upper`.
  #
  # The literals below match CRuby for these positional/optional/rest/block
  # signatures (where mruby and CRuby agree), so this is independent ground
  # truth, not merely "alias == original".
  c = Class.new {
    def f0; end
    def f1(a); end
    def fopt(a, b = 1); end
    def frest(a, *b); end
    def fblk(a, &b); end
    def fmix(a, b = 1, *c, &d); end
    alias_method :a0,    :f0
    alias_method :a1,    :f1
    alias_method :aopt,  :fopt
    alias_method :arest, :frest
    alias_method :ablk,  :fblk
    alias_method :amix,  :fmix
  }
  cases = [
    # name,   parameters,                                            arity
    [:a0,    [],                                                       0],
    [:a1,    [[:req, :a]],                                             1],
    [:aopt,  [[:req, :a], [:opt, :b]],                                -2],
    [:arest, [[:req, :a], [:rest, :b]],                               -2],
    [:ablk,  [[:req, :a], [:block, :b]],                               1],
    [:amix,  [[:req, :a], [:opt, :b], [:rest, :c], [:block, :d]],     -2],
  ]
  cases.each do |name, params, arity|
    u = c.instance_method(name)
    assert_equal params, u.parameters
    assert_equal arity,  u.arity
    # the bound Method goes through the same proc paths and must agree
    b = c.new.method(name)
    assert_equal params, b.parameters
    assert_equal arity,  b.arity
  end
  # alias must equal the original it points at (parameters and arity)
  { a0: :f0, a1: :f1, aopt: :fopt, arest: :frest, ablk: :fblk, amix: :fmix }.each do |al, orig|
    assert_equal c.instance_method(orig).parameters, c.instance_method(al).parameters
    assert_equal c.instance_method(orig).arity,      c.instance_method(al).arity
  end

  # Alias-of-an-alias collapses to one proc at creation; must still resolve.
  chain = Class.new {
    def orig(x, y) end
    alias_method :a1, :orig
    alias_method :a2, :a1
  }
  assert_equal [[:req, :x], [:req, :y]], chain.instance_method(:a2).parameters
  assert_equal 2, chain.instance_method(:a2).arity

  # Aliasing a C method does NOT create an alias proc (it reuses the original
  # cfunc method), so it must behave exactly like the original and never crash.
  c2 = Class.new(String) { alias_method :up2, :upcase }
  assert_equal String.instance_method(:upcase).parameters,
               c2.instance_method(:up2).parameters
  assert_equal String.instance_method(:upcase).arity,
               c2.instance_method(:up2).arity
end

assert 'Method/UnboundMethod on C-defined (native) methods' do
  # C methods have no irep: arity/parameters come from the packed argument spec
  # (caspec) and source_location is always nil. This exercises the
  # MRB_PROC_CFUNC_P branches of mrb_proc_arity / mrb_proc_parameters and the
  # cfunc path of method_search_vm -- none of which the suite covered before.

  # source_location is nil for genuinely C-defined methods (not mrblib ones).
  assert_nil "x".method(:upcase).source_location
  assert_nil 1.method(:+).source_location
  assert_nil [].method(:push).source_location
  assert_nil String.instance_method(:upcase).source_location

  # arity: documented values for these core C methods.
  assert_equal 0,  "x".method(:upcase).arity   # no args
  assert_equal 1,  1.method(:+).arity          # one required
  assert_equal(-1, [].method(:push).arity)     # variadic (rest)
  assert_equal(-1, [].method(:first).arity)    # optional

  # parameters: always an Array of Arrays, never crashes; a no-arg C method
  # gives []. C-method parameter *kinds* are an approximation (names are absent,
  # and required args surface as :opt for non-strict procs), so we assert the
  # stable shape rather than pinning exact kind labels -- except :rest, which is
  # meaningful: a variadic C method must expose a rest parameter.
  assert_equal [], "x".method(:upcase).parameters
  [1.method(:+), [].method(:push), {}.method(:[]), [].method(:first)].each do |m|
    ps = m.parameters
    assert_true ps.is_a?(Array)
    ps.each { |p| assert_true p.is_a?(Array) }
  end
  assert_true [].method(:push).parameters.any? { |entry| entry[0] == :rest }

  # identity / metadata on a C method.
  m = "abc".method(:upcase)
  assert_equal String,  m.owner
  assert_equal :upcase, m.name
  assert_equal "abc",   m.receiver
  assert_equal "#<Method: String#upcase>", m.to_s
  assert_equal "#<UnboundMethod: String#upcase>", String.instance_method(:upcase).to_s

  # behaviour: the C function actually runs via call / [] / bind / bind_call /
  # unbind+rebind.
  assert_equal 5,  2.method(:+).call(3)
  assert_equal 5,  2.method(:+)[3]
  assert_equal 5,  Integer.instance_method(:+).bind_call(2, 3)
  assert_equal 5,  Integer.instance_method(:+).bind(2).call(3)
  assert_equal 11, 5.method(:+).unbind.bind(10).call(1)

  # eql?: equal only when bound to the SAME receiver object and same definition.
  s = "cat"
  assert_true  s.method(:upcase) == s.method(:upcase)
  assert_false s.method(:upcase) == "cat".method(:upcase)  # distinct receivers
  assert_false s.method(:upcase) == s.method(:downcase)

  # super_method resolves across C methods (Integer#to_s -> BasicObject#to_s).
  sm = 5.method(:to_s).super_method
  assert_false sm.nil?
  assert_equal :to_s, sm.name

  # binding a C UnboundMethod to an incompatible receiver still raises cleanly.
  assert_raise(TypeError) { String.instance_method(:upcase).bind(42) }
end
