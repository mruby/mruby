class CallSuperBase
  def func
    'test string'
  end

  def args(*a)
    a
  end
end

assert('mrb_call_super') do
  o = CallSuperDerived.new
  assert_equal 'test string', o.func
  assert_equal [0, 1, 2], o.args(0, 1, 2)
end
