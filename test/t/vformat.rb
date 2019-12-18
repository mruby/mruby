def assert_format(exp, args)
  assert_equal(exp, TestVFormat.format(*args))
end

def assert_format_pattern(exp_pattern, args)
  assert_match(exp_pattern, TestVFormat.format(*args))
end

# Pass if ArgumentError is raised or return value is +exp+.
def assert_implementation_dependent(exp, args)
  begin
    ret = TestVFormat.format(*args)
  rescue ArgumentError
    return pass
  end
  if ret == exp
    pass
  else
    flunk "", "Expected ArgumentError is raised or #{ret.inspect} to be #{exp}."
  end
end

def sclass(v)
  class << v
    self
  end
end

assert('mrb_vformat') do
  n = TestVFormat::Native
  assert_format '', ['']
  assert_format 'No specifier!', ['No specifier!']
  assert_format '`c`: C', ['`c`: %c', n.c(?C)]
  assert_format '`d`: 123', ['`d`: %d', n.d(123)]
  assert_format '`d`: -79', ['`d`: %d', n.d(-79)]
  assert_format '`i`: 514', ['`i`: %i', n.i(514)]
  assert_format '`i`: -83', ['`i`: %i', n.i(-83)]
  assert_format '`t`: NilClass', ['`t`: %t', nil]
  assert_format '`t`: FalseClass', ['`t`: %t', false]
  assert_format '`t`: TrueClass', ['`t`: %t', true]
  assert_format '`t`: Fixnum', ['`t`: %t', 0]
  assert_format '`t`: Hash', ['`t`: %t', k: "value"]
  assert_format_pattern '#<Class:#<Class:#<Hash:0x*>>>', ['%t', sclass({})]
#  assert_format 'string and length', ['string %l length', n.s('andante'), n.l(3)]
  assert_format '`n`: sym', ['`n`: %n', n.n(:sym)]
  assert_format '%C文字列%', ['%s', n.s('%C文字列%')]
  assert_format '`C`: Kernel module', ['`C`: %C module', n.C(Kernel)]
  assert_format '`C`: NilClass', ['`C`: %C', n.C(nil.class)]
  assert_format_pattern '#<Class:#<String:0x*>>', ['%C', n.C(sclass(""))]
  assert_format '`T`: NilClass', ['`T`: %T', nil]
  assert_format '`T`: FalseClass', ['`T`: %T', false]
  assert_format '`T`: TrueClass', ['`T`: %T', true]
  assert_format '`T`: Fixnum', ['`T`: %T', 0]
  assert_format '`T`: Hash', ['`T`: %T', k: "value"]
  assert_format_pattern 'Class', ['%T', sclass({})]
  assert_format '`Y`: nil', ['`Y`: %Y', nil]
  assert_format '`Y`: false', ['`Y`: %Y', false]
  assert_format '`Y`: true', ['`Y`: %Y', true]
  assert_format '`Y`: Fixnum', ['`Y`: %Y', 0]
  assert_format '`Y`: Hash', ['`Y`: %Y', k: "value"]
  assert_format 'Class', ['%Y', sclass({})]
  assert_format_pattern '#<Class:#<String:0x*>>', ['%v', sclass("")]
  assert_format '`v`: 1...3', ['`v`: %v', 1...3]
  assert_format '`S`: {:a=>1, "b"=>"c"}', ['`S`: %S', a: 1, "b" => ?c]
  assert_format 'percent: %', ['percent: %%']
  assert_format '"I": inspect char', ['%!c: inspect char', n.c(?I)]
  assert_format '709: inspect mrb_int', ['%!d: inspect mrb_int', n.i(709)]
#  assert_format '"a\x00b\xff"', ['%!l', n.s("a\000b\xFFc\000d"), n.l(4)]
  assert_format ':"&.": inspect symbol', ['%!n: inspect symbol', n.n(:'&.')]
  assert_format 'inspect "String"', ['inspect %!v', 'String']
  assert_format 'inspect Array: [1, :x, {}]', ['inspect Array: %!v', [1,:x,{}]]
  assert_format_pattern '`!C`: #<Class:0x*>', ['`!C`: %!C', n.C(Class.new)]
  assert_format 'to_s -> to_s: ab,cd', ['to_s -> to_s: %n,%v', n.n(:ab), 'cd']
  assert_format 'to_s -> inspect: x:y', ['to_s -> inspect: %v%!v', 'x', :y]
  assert_format 'inspect -> to_s: "a"b', ['inspect -> to_s: %!v%n', 'a', n.n(:b)]
  assert_format 'Y -> to_s: nile', ['Y -> to_s: %Y%v', nil, "e"]
  assert_format '"abc":Z', ['%!s%!n', n.s('abc'), n.n('Z'.to_sym)]
  assert_format 'escape: \\%a,b,c,d', ['escape: \\\\\%a,b,\c%v', ',d']

  assert_implementation_dependent 'unknown specifier: %^',
                                  ['unknown specifier: %^']
  assert_implementation_dependent 'unknown specifier with modifier: %!^',
                                  ['unknown specifier with modifier: %!^']
  assert_implementation_dependent 'termination is \\', ['termination is \\']
  assert_implementation_dependent 'termination is %', ['termination is %']
  assert_implementation_dependent 'termination is %!', ['termination is %!']

  skip unless Object.const_defined?(:Float)
  assert_format '`f`: 0.0125', ['`f`: %f', n.f(0.0125)]
  assert_format '-Infinity', ['%f', n.f(-Float::INFINITY)]
  assert_format 'NaN: Not a Number', ['%f: Not a Number', n.f(Float::NAN)]
end
