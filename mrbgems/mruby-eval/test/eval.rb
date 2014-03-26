assert('Kernel.eval') do
  assert_equal(10) { Kernel.eval '1 * 10' }
  assert_equal('aaa') { Kernel.eval "'a' * 3" }
end

assert('eval') do
  assert_equal(10) { eval '1 * 10' }
end
