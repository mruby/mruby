##
# NoMethodError ISO Test

assert('NoMethodError', '15.2.32') do
  NoMethodError.class == Class
  assert_raise NoMethodError do
    doesNotExistAsAMethodNameForVerySure("")
  end
end

assert('NoMethodError#args', '15.2.32.2.1') do
  a = NoMethodError.new('test', :test, [1, 2])
  assert_equal [1, 2], a.args

  assert_nothing_raised do
    begin
      doesNotExistAsAMethodNameForVerySure 3, 1, 4
    rescue NoMethodError => e
      assert_equal [3, 1, 4], e.args
    end
  end
end

assert('NoMethodError#receiver') do
  assert_equal 2, NoMethodError.new('test', :test, [1], receiver: 2).receiver
  assert_raise(ArgumentError) { NoMethodError.new('test').receiver }

  o = Object.new
  e = assert_raise(NoMethodError) { o.doesNotExistAsAMethodNameForVerySure }
  assert_same o, e.receiver
  e = assert_raise(NoMethodError) { nil.doesNotExistAsAMethodNameForVerySure }
  assert_nil e.receiver

  # a private method called with a receiver, and super with no superclass method
  c = Class.new do
    def pub; super; end
    private def priv; end
  end
  o = c.new
  e = assert_raise(NoMethodError) { o.priv }
  assert_same o, e.receiver
  e = assert_raise(NoMethodError) { o.pub }
  assert_same o, e.receiver
end
