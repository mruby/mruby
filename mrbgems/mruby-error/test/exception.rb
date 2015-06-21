assert 'mrb_protect' do
  assert_equal ['test', false] do
    ExceptionTest.mrb_protect { 'test' }
  end
  assert_equal [nil, true] do
    ExceptionTest.mrb_protect { raise 'test' }
  end
end

assert 'mrb_ensure' do
  a = false
  assert_equal 'test' do
    ExceptionTest.mrb_ensure Proc.new { 'test' }, Proc.new { a = true }
  end
  assert_true a

  a = false
  assert_raise RuntimeError do
    ExceptionTest.mrb_ensure Proc.new { raise 'test' }, Proc.new { a = true }
  end
  assert_true a
end

assert 'mrb_rescue' do
  assert_equal 'test' do
    ExceptionTest.mrb_rescue Proc.new { 'test' }, Proc.new {}
  end

  class CustomExp < Exception
  end

  assert_raise CustomExp do
    ExceptionTest.mrb_rescue Proc.new { raise CustomExp.new 'test' }, Proc.new { 'rescue' }
  end

  assert_equal 'rescue' do
    ExceptionTest.mrb_rescue Proc.new { raise 'test' }, Proc.new { 'rescue' }
  end
end

assert 'mrb_rescue_exceptions' do
  assert_equal 'test' do
    ExceptionTest.mrb_rescue_exceptions Proc.new { 'test' }, Proc.new {}
  end

  assert_raise RangeError do
    ExceptionTest.mrb_rescue_exceptions Proc.new { raise RangeError.new 'test' }, Proc.new { 'rescue' }
  end

  assert_equal 'rescue' do
    ExceptionTest.mrb_rescue_exceptions Proc.new { raise TypeError.new 'test' }, Proc.new { 'rescue' }
  end
end
