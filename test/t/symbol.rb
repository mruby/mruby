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
