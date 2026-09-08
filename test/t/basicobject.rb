##
# BasicObject

assert('BasicObject') do
  assert_equal(Class, BasicObject.class)
end

assert('BasicObject superclass') do
  assert_nil(BasicObject.superclass)
end

assert('BasicObject reopened at the top level is the class Object inherits from') do
  # `class BasicObject` at the top level asks `Object` whether it holds the
  # constant and, told it does not, makes a new class under `Object` by that
  # name. The constant is kept in `Object` as well as in `BasicObject`
  # itself, as CRuby keeps it, so the reopening reaches every object.
  reopened = class BasicObject
    def basic_object_reopen_probe; :reopened; end
    self
  end
  begin
    assert_same(Object.superclass, reopened)
    assert_nil(reopened.superclass)
    assert_equal(:reopened, nil.basic_object_reopen_probe)
    assert_equal(:reopened, Class.new(BasicObject).new.basic_object_reopen_probe)
  ensure
    class BasicObject
      undef_method :basic_object_reopen_probe
    end
  end
end

assert('BasicObject#== defined by a class is asked about the receiver itself') do
  # `OP_EQ` answers `obj == obj` from the identity of its operands, which it
  # may only do while the builtin `==` is what would be asked: CRuby's `opt_eq`
  # calls a `==` a class defines, for the receiver itself included.
  # `MRB_FL_CLASS_EQ_DEFINED` on a class records that a class among its
  # ancestors defines one. It is set when the method is defined and carried to
  # the subclasses, singleton classes and includers made afterwards, and to
  # those that exist already.
  plain = Object.new
  assert_true plain == plain

  own = Class.new { def ==(other); :own; end }
  o = own.new
  assert_equal :own, o == o
  assert_false o != o
  after = Class.new(own).new
  assert_equal :own, after == after

  base = Class.new
  before = Class.new(base).new
  base.class_eval { def ==(other); :base; end }
  assert_equal :base, before == before

  single = Object.new
  def single.==(other); :single; end
  assert_equal :single, single == single

  aliased = Class.new { def same?(other); :aliased; end; alias_method :==, :same? }.new
  assert_equal :aliased, aliased == aliased

  defined = Class.new { define_method(:==) {|other| :defined } }.new
  assert_equal :defined, defined == defined

  mod = Module.new { def ==(other); :mod; end }
  included = Class.new { include mod }.new
  assert_equal :mod, included == included
  prepended = Class.new { prepend mod }.new
  assert_equal :mod, prepended == prepended

  late = Module.new
  with_late = Class.new { include late }
  early = with_late.new
  late.module_eval { def ==(other); :late; end }
  assert_equal :late, early == early
  via_sub = Class.new(with_late).new
  assert_equal :late, via_sub == via_sub

  # a builtin `==` keeps the identity answer, Comparable's among them, which
  # CRuby answers for the receiver itself before it asks `<=>`
  cmp = Class.new { include Comparable; def <=>(other); nil; end }.new
  assert_true cmp == cmp
end

assert('BasicObject#== defined on NilClass, TrueClass or FalseClass') do
  # `nil`, `true` and `false` are immediate values, so `OP_EQ` has no class
  # pointer of theirs to read `MRB_FL_CLASS_EQ_DEFINED` from. The flag of the
  # three classes is mirrored into a bit of the mask the opcode tests for
  # every receiver, so a `==` they define is asked about the receiver itself,
  # as CRuby's `opt_eq` asks it, while the answer an Integer or a Symbol gives
  # itself is left to the bit of its own.
  assert_true nil == nil
  assert_true true == true
  assert_true false == false

  # every answer is taken while the redefinition stands and checked once it is
  # put back, so that no assertion runs under a `==` of this test's own
  answers = []
  [[NilClass, nil, :nil_eq], [TrueClass, true, :true_eq],
   [FalseClass, false, :false_eq]].each do |klass, value, answer|
    klass.class_eval { alias_method :__eq_before_test, :== }
    begin
      klass.class_eval { define_method(:==) {|other| answer } }
      answers << (value == value) << (value == 1)
      answers << (1 == 1) << (:a == :a) << (:a == :b)
    ensure
      klass.class_eval { alias_method :==, :__eq_before_test }
    end
    answers << (value == value)
  end
  assert_equal [:nil_eq, :nil_eq, true, true, false, true,
                :true_eq, :true_eq, true, true, false, true,
                :false_eq, :false_eq, true, true, false, true], answers
end
