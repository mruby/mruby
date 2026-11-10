##
# NameError ISO Test

assert('NameError', '15.2.31') do
  assert_equal Class, NameError.class
end

assert('NameError#name', '15.2.31.2.1') do

  # This check is not duplicate with 15.2.31.2.2 check.
  # Because the NameError in this test is generated in
  # C API.
  class TestDummy
    alias foo bar
  rescue NameError => e
    $test_dummy_result = e.name
  end

  assert_equal :bar, $test_dummy_result
end

assert('NameError#initialize', '15.2.31.2.2') do
  e = NameError.new('a', :foo)

  assert_equal NameError, e.class
  assert_equal 'a', e.message
  assert_equal :foo, e.name
end

assert('NameError#receiver') do
  e = NameError.new('a', :foo, receiver: 1)
  assert_equal 1, e.receiver
  assert_nil NameError.new('a', :foo, receiver: nil).receiver
  assert_raise(ArgumentError) { NameError.new('a').receiver }
  assert_raise(ArgumentError) { NameError.new('a', :foo, bogus: 1) }

  # the module a missing constant was looked up in
  module NameErrorReceiverTest; end
  e = assert_raise(NameError) { NameErrorReceiverTest::Missing }
  assert_equal NameErrorReceiverTest, e.receiver
  e = assert_raise(NameError) { NameErrorReceiverTestMissing }
  assert_equal Object, e.receiver
end
