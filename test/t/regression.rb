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
