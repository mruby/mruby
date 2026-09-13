##
# Symbol ISO Test

assert('Symbol') do
  assert_equal :"a", :a
  assert_equal :"a#{1}", :a1
  assert_equal :'a', :a
  assert_equal :'a#{1}', :"a\#{1}"
end

assert('Symbol', '15.2.11') do
  assert_equal Class, Symbol.class
end

assert('Symbol#===', '15.2.11.3.1') do
  assert_true :abc === :abc
  assert_false :abc === :cba
end

assert('Symbol#== redefined on Symbol itself reaches the redefinition') do
  # `OP_EQ` answers `:a == x` from the identity of its two operands whenever
  # the receiver is a Symbol, which it may only do while `Symbol#==` is still
  # the builtin it stands in for; `mrb->bop_redefined` records the moment it
  # is replaced, so a redefinition installed on `Symbol` is honored as in
  # CRuby. The results are read before the operator is put back because the
  # assertions themselves compare symbols.
  Symbol.class_eval do
    alias_method :__eq_before_test, :==
    def ==(other) [:eq, self, other] end
  end
  begin
    a = :abc
    same = a == :abc
    different = a == :xyz
    string = a == 'abc'
    index = [:abc, :xyz].index(:xyz)    # `mrb_equal()` asks the redefinition
  ensure
    Symbol.class_eval do
      alias_method :==, :__eq_before_test
      remove_method :__eq_before_test if respond_to?(:remove_method, true)
    end
  end
  assert_equal [:eq, :abc, :abc], same
  assert_equal [:eq, :abc, :xyz], different
  assert_equal [:eq, :abc, 'abc'], string
  assert_equal 0, index
  assert_true :abc == :abc
  assert_false :abc == :xyz
  assert_false :abc == 'abc'
end

assert('Symbol#to_s', '15.2.11.3.3') do
  assert_equal  'abc', :abc.to_s
end

assert('Symbol#to_sym', '15.2.11.3.4') do
  assert_equal :abc, :abc.to_sym
end

assert('Symbol#to_proc') do
  assert_equal 5, :abs.to_proc[-5]
end

assert('Symbol name round-trips across the inline-packing boundary (issue #6940)') do
  # Names up to 4 bytes are packed inline; longer names use a BER length
  # prefix in the pool. A strict-aliasing bug once dropped the last byte and
  # prepended the prefix for lengths >= 5 (e.g. "source" -> "\x06sourc").
  %w[a ab abc abcd abcde source abcdefghij abcdefghijklmnopqrs].each do |s|
    assert_equal s, s.to_sym.to_s
    assert_equal s.length, s.to_sym.to_s.length
  end

  # the report's concrete failure: attr_reader derives the ivar name as
  # "@" + symbol_name, so a corrupted name raised NameError on "@sour".
  cls = Class.new do
    attr_reader :source
    def initialize; @source = 42; end
  end
  assert_equal 42, cls.new.source
end

assert('Symbol, empty name') do
  # Compiling the same empty symbol twice makes the second one collide with
  # the first in the compiler's constant pool, which compares the colliding
  # entries byte by byte. An empty name carries no bytes to compare.
  assert_equal "", :"".to_s
  assert_equal :"", :"".to_sym
  assert_equal 0, :"".to_s.length
  assert_true :"".is_a?(Symbol)
end

assert('Symbol#to_s and Symbol#name outlive symbol GC') do
  stress
  # Symbol GC frees the name buffer of a dynamic symbol. A returned string
  # longer than the embedded limit used to share that buffer instead of
  # copying it, so every later read of the string was a use after free.
  name = "gc-target-symbol-" + "a" * 24
  str = name.to_sym.to_s
  frozen = name.to_sym.name

  # Reach the dynamic symbol limit so a sweep runs, then hand the freed
  # blocks to something else.
  6000.times { |i| "gc-filler-symbol-name-#{i}".to_sym }
  GC.start
  reuse = []
  3000.times { |i| reuse << "z" * 60 + i.to_s }

  assert_equal name, str
  assert_equal name, frozen
end
