assert('Kernel.eval') do
  assert_equal(10) { Kernel.eval '1 * 10' }
  assert_equal('aaa') { Kernel.eval "'a' * 3" }
  assert_equal(10) {
    a = 10
    Kernel.eval "a"
  }
  assert_equal(20) {
    a = 10
    Kernel.eval "a = 20"
    a
  }
  assert_equal(15) {
    c = 5
    lambda {
      a = 10
      Kernel.eval "c = a + c"
    }.call
    c
  }
end

assert('eval') do
  assert_equal(10) { eval '1 * 10' }
end

assert('rest arguments of eval') do
  assert_raise(ArgumentError) { Kernel.eval('0', 0, 'test', 0) }
  assert_equal ['test', 'test.rb', 10] do
    Kernel.eval('[\'test\', __FILE__, __LINE__]', nil, 'test.rb', 10)
  end
end
