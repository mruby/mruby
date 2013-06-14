##
# Symbol ISO Test

assert('Symbol', '15.2.11') do
  assert_equal Symbol.class, Class
end

assert('Symbol superclass', '15.2.11.2') do
  assert_equal Symbol.superclass, Object
end

assert('Symbol#===', '15.2.11.3.1') do
  assert_true :abc == :abc
  assert_false :abc == :cba
end

assert('Symbol#id2name', '15.2.11.3.2') do
  assert_equal :abc.id2name, 'abc'
end

assert('Symbol#to_s', '15.2.11.3.3') do
  assert_equal :abc.to_s, 'abc'
end

assert('Symbol#to_sym', '15.2.11.3.4') do
  assert_equal :abc.to_sym, :abc
end
