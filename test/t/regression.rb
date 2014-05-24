##
# Regression Tests

assert('Regression test for 2298') do
  klass = Class.new do
    def initialize
      yield
    end
  end

  klass.new do
    break
  end
end

assert('Regression test for 2310') do
  class BasicObject
    def regression_2310
    end
  end
  assert_equal nil, Object.new.regression_2310
end
