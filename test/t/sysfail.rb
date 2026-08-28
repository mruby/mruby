##
# mrb_sys_fail with no SystemCallError

assert('mrb_sys_fail without SystemCallError') do
  # With mruby-errno the errno goes to SystemCallError and none of this
  # applies. The core test state has no gems, so the tests below hold, but
  # say what they depend on rather than assume it.
  skip 'SystemCallError is defined' if TestSysFail::SYSTEM_CALL_ERROR_DEFINED

  e = assert_raise(RuntimeError) { TestSysFail.fail(2) }
  assert_equal 'errno: 2', e.message

  e = assert_raise(RuntimeError) { TestSysFail.fail(2, 'wilma') }
  assert_equal 'errno: 2 - wilma', e.message
end
