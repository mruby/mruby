##
# NameError ISO Test

assert('NameError', '15.2.31') do
  assert_equal NameError.class, Class
end

assert('NameError superclass', '15.2.31.2') do
  assert_equal NameError.superclass, StandardError
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

  assert_equal $test_dummy_result, :bar
end

assert('NameError#initialize', '15.2.31.2.2') do
   e = NameError.new('a', :foo)

   assert_equal e.class, NameError
   assert_equal e.message, 'a'
   assert_equal e.name, :foo
end
