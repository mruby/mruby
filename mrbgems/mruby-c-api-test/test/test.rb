class CallSuperBase
  def func
    'test string'
  end

  def args(*a)
    a
  end

  def method_missing(sym, *a)
    [sym, a]
  end
end

assert('mrb_call_super') do
  o = CallSuperDerived.new
  assert_equal 'test string', o.func
  assert_equal [0, 1, 2], o.args(0, 1, 2)
  assert_equal [:test, [3, 1, 4]], o.test(3, 1, 4)
end
