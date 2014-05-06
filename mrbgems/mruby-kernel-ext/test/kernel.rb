assert('Kernel.fail, Kernel#fail') do
  assert_raise(RuntimeError) { fail }
  assert_raise(RuntimeError) { Kernel.fail }
end
