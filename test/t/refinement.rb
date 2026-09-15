##
# Refinement test (MRB_USE_REFINEMENTS)

assert('Refinement') do
  skip "requires MRB_USE_REFINEMENTS" unless Object.const_defined?(:Refinement)
  assert_equal Module, Refinement.superclass
  assert_raise(NoMethodError) { Refinement.new }
end

if Object.const_defined?(:Refinement)

class RefTestC
  def foo; "C#foo"; end
  def bar; "C#bar"; end
  def with_super; "C"; end
  def ==(o); false; end
end

module RefTestM
  refine RefTestC do
    def foo; "M#foo"; end
    def baz; "M#baz"; end
    def with_super; "M+" + super; end
    def calls_baz; baz; end
  end
end

module RefTestM2
  refine RefTestC do
    def foo; "M2#foo"; end
    def with_super; "M2+" + super; end
  end
end

def ref_test_before_using
  RefTestC.new.foo
end

assert('Refinement: not visible before using') do
  assert_equal "C#foo", RefTestC.new.foo
  assert_raise(NoMethodError) { RefTestC.new.baz }
  assert_false RefTestC.new.respond_to?(:baz)
  assert_equal [], Module.used_modules
end

using RefTestM

assert('Refinement: visible after using') do
  c = RefTestC.new
  assert_equal "M#foo", c.foo
  assert_equal "M#baz", c.baz
  assert_equal "C#bar", c.bar
  assert_equal "M+C", c.with_super
  assert_equal "M#baz", c.calls_baz
  assert_equal [RefTestM], Module.used_modules
  assert_equal [RefTestM.refinements[0]], Module.used_refinements
end

assert('Refinement: method defined before using is unaffected') do
  assert_equal "C#foo", ref_test_before_using
end

def ref_test_after_using
  RefTestC.new.foo
end

assert('Refinement: method defined after using sees it') do
  assert_equal "M#foo", ref_test_after_using
end

assert('Refinement: blocks and lambdas in the scope') do
  assert_equal ["M#foo"], [RefTestC.new].map { |c| c.foo }
  l = -> { RefTestC.new.foo }
  assert_equal "M#foo", l.call
end

assert('Refinement: indirect calls honor the caller scope') do
  c = RefTestC.new
  assert_equal "M#foo", c.__send__(:foo)
  assert_equal "M#baz", c.__send__(:baz)
  if c.respond_to?(:send)   # mruby-metaprog
    assert_equal "M#baz", c.send(:baz)
    assert_equal "M#baz", c.public_send(:baz)
  end
  assert_true c.respond_to?(:baz)
  assert_equal "method", defined?(c.baz)
end

class RefTestProt; end

module RefTestProtM
  refine RefTestProt do
    protected def prot; "prot"; end
  end
end

class RefTestProt
  using RefTestProtM
  def call_prot(o); o.prot; end
  def defined_prot(o); defined?(o.prot); end
  def self.outside(o); [(o.prot rescue :err), defined?(o.prot)]; end
end

assert('Refinement: protected refined method is reached from the refined class') do
  a = RefTestProt.new
  assert_equal "prot", a.call_prot(RefTestProt.new)
  assert_equal "method", a.defined_prot(RefTestProt.new)
  assert_equal [:err, nil], RefTestProt.outside(a)
end

assert('Refinement: send does not reach a method the refinement undefined') do
  k = Class.new { def gone; "gone"; end }
  m = Module.new { refine(k) { undef_method :gone } }
  seen = nil
  k2 = Class.new { def gone; "gone"; end; define_method(:method_missing) { |n, *a| seen = n; "missing" } }
  m2 = Module.new { refine(k2) { undef_method :gone } }
  run = Class.new do
    using m
    using m2
    define_method(:go) { [(k.new.__send__(:gone) rescue :missing_raised), k2.new.__send__(:gone)] }
  end
  assert_equal [:missing_raised, "missing"], run.new.go
  assert_equal :gone, seen
  assert_equal "gone", k.new.__send__(:gone)
end

module RefTestLater
  def other; "other"; end
end
module RefTestLaterOps
  refine RefTestLater do
    def -(o); "later-"; end
    def [](i); "later[]"; end
  end
end
class RefTestLaterUse
  using RefTestLaterOps
  def self.run; [3 - 1, [7][0]]; end
end

assert('Refinement: a refined module prepended later to a guarded class') do
  # the module's refinement comes before the class's own operator only once
  # the module stands before the class, which is what prepend does; the
  # guarded fast paths must notice the change of ancestry
  assert_equal [2, 7], RefTestLaterUse.run
  Integer.prepend(RefTestLater)
  Array.prepend(RefTestLater)
  assert_equal ["later-", "later[]"], RefTestLaterUse.run
  assert_equal [2, 7], [3 - 1, [7][0]]
end

module RefTestModG; def a; "G" + super; end; end
module RefTestModF; include RefTestModG; def a; "F" + super; end; end
class RefTestModA; def a; "A"; end; end
class RefTestModB < RefTestModA; include RefTestModF; end
module RefTestModR
  refine RefTestModF do
    def a; "R" + super; end
    def own; "own"; end
  end
end

assert('Refinement: super into a refined module method cannot go on (Bug #22071)') do
  assert_equal "FGA", RefTestModB.new.a
  c = Class.new do
    using RefTestModR
    define_method(:go) { RefTestModB.new.a }
    define_method(:own) { RefTestModB.new.own }
  end
  assert_equal "own", c.new.own
  e = assert_raise(NoMethodError) { c.new.go }
  assert_true e.message.include?("called via super from a refinement method")
end

class RefTestSupC; def foo; "C"; end; end
module RefTestSupM1; refine(RefTestSupC) { def foo; "M1+" + super; end }; end
module RefTestSupM2
  refine(RefTestSupC) { using RefTestSupM1; def foo; "M2+" + super; end }
end
module RefTestSupM3; refine(RefTestSupC) { def foo; "M3+" + super; end }; end

assert('Refinement: super goes to the next refinement active where super is written') do
  a = Class.new { using RefTestSupM2; define_method(:go) { RefTestSupC.new.foo } }
  assert_equal "M2+M1+C", a.new.go
  # refinements active at the caller do not affect a super inside the method
  b = Class.new { using RefTestSupM2; using RefTestSupM3; define_method(:go) { RefTestSupC.new.foo } }
  assert_equal "M3+C", b.new.go
end

assert('Refinement: method_missing is not refined') do
  c = RefTestC.new
  assert_raise(NoMethodError) { c.no_such_method }
end

assert('Refinement: later using wins, super passes over the current refinement') do
  using RefTestM2
  c = RefTestC.new
  assert_equal "M2#foo", c.foo
  assert_equal "M#baz", c.baz
  assert_equal "M2+C", c.with_super
  assert_equal [RefTestM2, RefTestM], Module.used_modules
end

assert('Refinement: scope of using inside a block does not leak out') do
  # the `using` above was written in a block at top level, so it installed
  # into the file scope (documented divergence from CRuby)
  assert_equal "M2#foo", RefTestC.new.foo
end

class RefTestD < RefTestC
  def foo; "D+" + super; end
end

assert('Refinement: super from a subclass method sees the refinement') do
  assert_equal "D+M2#foo", RefTestD.new.foo
end

module RefTestPre
  def foo; "Pre+" + super; end
end

class RefTestP
  prepend RefTestPre
  def foo; "P#foo"; end
end

module RefTestPM
  refine RefTestP do
    def foo; "R+" + super; end
  end
end

assert('Refinement: refinements come before prepended modules') do
  assert_equal "Pre+P#foo", RefTestP.new.foo
  using RefTestPM
  assert_equal "R+Pre+P#foo", RefTestP.new.foo
end

module RefTestEnum
  refine Enumerable do
    def ref_sum; inject(0) { |a, b| a + b }; end
  end
end

assert('Refinement: refine a module') do
  assert_raise(NoMethodError) { [1, 2, 3].ref_sum }
  using RefTestEnum
  assert_equal 6, [1, 2, 3].ref_sum
  assert_equal 3, (1..2).ref_sum
end

module RefTestOps
  refine Integer do
    def +(o); "int+"; end
    def <(o); "int<"; end
  end
  refine String do
    def [](i); "str[]"; end
  end
  refine Array do
    def []=(i, v); "ary[]="; end
  end
  refine RefTestC do
    def ==(o); "eq"; end
  end
end

def ref_test_ops_unrefined
  a = [1]
  a[0] = 5
  [1 + 2, 1 < 2, "abc"[0], [1][0], a[0], RefTestC.new == 1]
end

class RefTestOpsScope
  using RefTestOps
  def self.run
    c = RefTestC.new
    a = [1]
    [1 + 2, 1 < 2, "abc"[0], (a[0] = 5), a.orig_aref(0), RefTestC.new == 1, c == c, 2.0 + 3.0]
  end
end

class Array
  alias orig_aref []
end

assert('Refinement: guarded operators') do
  assert_equal ["int+", "int<", "str[]", 5, 1, "eq", "eq", 5.0], RefTestOpsScope.run
  assert_equal [3, true, "a", 1, 5, false], ref_test_ops_unrefined
  assert_equal 3, 1 + 2
  assert_equal "a", "abc"[0]
end

assert('Refinement: class body scope ends with the class') do
  klass = Class.new
  m = Module.new do
    refine(klass) { def hi; "hi"; end }
  end
  c = Class.new do
    using m
    define_method(:x) { klass.new.hi }
  end
  assert_equal "hi", c.new.x
  assert_raise(NoMethodError) { klass.new.hi }
end

assert('Refinement: using in a block given a class is scoped to the block') do
  klass = Class.new
  m = Module.new do
    refine(klass) { def hi; "hi"; end }
  end
  holder = Module.new
  r = holder.module_eval do
    using m
    [klass.new.hi, [klass.new].map { |o| o.hi }, Module.used_modules]
  end
  assert_equal ["hi", ["hi"], [m]], r
  assert_raise(NoMethodError) { klass.new.hi }
  assert_false Module.used_modules.include?(m)
end

assert('Refinement: a frozen block keeps its refinements') do
  klass = Class.new
  m = Module.new do
    refine(klass) { def hi; "hi"; end }
  end
  c = Class.new do
    using m
    define_method(:blk) { -> { klass.new.hi } }
  end
  pr = c.new.blk
  assert_equal "hi", pr.call
  pr.freeze
  assert_equal "hi", pr.call
end

assert('Refinement: string instance_eval and class_eval see the refinements') do
  klass = Class.new { def foo; "C"; end }
  m = Module.new do
    refine(klass) { def foo; "R"; end }
  end
  c = Class.new do
    using m
    define_method(:run) { [klass.new.instance_eval("foo"), klass.class_eval("new.foo")] }
  end
  begin
    assert_equal ["R", "R"], c.new.run
  rescue NotImplementedError
    skip "requires mruby-eval"
  end
end

assert('Refinement: using errors') do
  assert_raise(TypeError) { using RefTestC }
  assert_raise(TypeError) { using RefTestM.refinements[0] }
  assert_raise(TypeError) { using 1 }
  assert_raise(RuntimeError) { def ref_test_using_in_method; using RefTestM; end; ref_test_using_in_method }
  m = Module.new { def self.m; using RefTestM; end }
  assert_raise(RuntimeError) { m.m }
  m2 = Class.new { define_method(:m) { self.class.__send__(:using, RefTestM) } }
  assert_raise(RuntimeError) { m2.new.m }
  o = Object.new
  assert_raise(RuntimeError) { RefTestM.__send__(:using, RefTestM2) }
  assert_raise(RuntimeError) { o.instance_eval { RefTestM.__send__(:using, RefTestM2) } }
end

assert('Refinement: refine errors') do
  assert_raise(ArgumentError) { Module.new { refine(RefTestC) } }
  assert_raise(TypeError) { Module.new { refine(1) { } } }
  assert_raise(TypeError) { Module.new { refine(RefTestM.refinements[0]) { } } }
  assert_raise(NoMethodError) { RefTestM.refinements[0].instance_eval { refine(RefTestC) { } } }
end

assert('Refinement: cannot be included, prepended or extended') do
  r = RefTestM.refinements[0]
  assert_raise(TypeError) { Module.new { include r } }
  assert_raise(TypeError) { Class.new { prepend r } }
  assert_raise(TypeError) { Object.new.extend(r) }
  assert_raise(TypeError) { r.instance_eval { include Comparable } }
  assert_raise(TypeError) { r.instance_eval { prepend Comparable } }
end

assert('Refinement: introspection') do
  r = RefTestM.refinements[0]
  assert_kind_of Refinement, r
  assert_true r.is_a?(Module)
  assert_equal RefTestC, r.target
  assert_equal RefTestC, r.refined_class
  assert_equal 1, RefTestM.refinements.size
  assert_equal "#<refinement:RefTestC@RefTestM>", r.to_s
  assert_equal "#<refinement:RefTestC@RefTestM>", r.inspect
  assert_equal [r, RefTestC, Object], r.ancestors.first(3)
  assert_equal BasicObject, r.ancestors.last
  assert_false RefTestC.new.is_a?(r)
  assert_equal [], Class.new.refinements
end

class RefTestA; end
class RefTestB; end

assert('Refinement: refine block sees a later refinement of the same owner') do
  m = Module.new do
    refine(RefTestA) { def go; RefTestB.new.hello; end }
    refine(RefTestB) { def hello; "hello"; end }
  end
  using m
  assert_equal "hello", RefTestA.new.go
end

assert('Refinement: refine is reopened, alias finds the refined class') do
  k = Class.new { def orig; "orig"; end }
  m = Module.new do
    refine(k) { def one; "one"; end }
    refine(k) { def two; "two"; end; alias_method :orig2, :orig }
  end
  assert_equal 1, m.refinements.size
  using m
  assert_equal "onetwo", k.new.one + k.new.two
  assert_equal "orig", k.new.orig2
end

assert('Refinement#import_methods') do
  k = Class.new
  helper = Module.new { def imported; "imported"; end; def uses_sibling; sibling; end }
  m = Module.new do
    refine(k) { def sibling; "sibling"; end; import_methods helper }
  end
  assert_raise(ArgumentError) { Module.new { refine(k) { import_methods Kernel } } }
  assert_raise(TypeError) { Module.new { refine(k) { import_methods k } } }
  using m
  assert_equal "imported", k.new.imported
  assert_equal "sibling", k.new.uses_sibling
end

assert('Refinement: using a module that includes a refining module') do
  k = Class.new
  inner = Module.new { refine(k) { def inner; "inner"; end } }
  outer = Module.new { include inner }
  using outer
  assert_equal "inner", k.new.inner
end

assert('Refinement: undef within a refinement hides the method') do
  k = Class.new { def gone; "gone"; end }
  m = Module.new { refine(k) { undef_method :gone } }
  assert_equal "gone", k.new.gone
  using m
  assert_raise(NoMethodError) { k.new.gone }
end

assert('Refinement: survives GC and scope slots are reused') do
  k = Class.new { def base; "base"; end }
  m = Module.new { refine(k) { def base; "refined"; end } }
  using m
  GC.start
  assert_equal "refined", k.new.base
  100.times do
    mm = Module.new { refine(k) { def tmp; 1; end } }
    Class.new { using mm }
  end
  GC.start
  assert_equal "refined", k.new.base
  # dead scopes are reclaimed even while the collector is turned off
  GC.disable
  begin
    2100.times { Module.new { refine(k) { } } }
  ensure
    GC.enable
  end
  assert_equal "refined", k.new.base
end

end
