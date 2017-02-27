##
# NoMethodError ISO Test

assert('NoMethodError', '15.2.32') do
  NoMethodError.class == Class
  assert_raise NoMethodError do
    doesNotExistAsAMethodNameForVerySure("")
  end
end

assert('NoMethodError#args', '15.2.32.2.1') do
  a = NoMethodError.new 'test', :test, [1, 2]
  assert_equal [1, 2], a.args

  assert_nothing_raised do
    begin
      doesNotExistAsAMethodNameForVerySure 3, 1, 4
    rescue NoMethodError => e
      assert_equal [3, 1, 4], e.args
    end
  end
end

assert('Can still raise when Kernel#method_missing is removed') do
  assert_raise(NoMethodError) do
    begin
      Kernel.alias_method(:old_method_missing, :method_missing)
      Kernel.remove_method(:method_missing)
      1.__send__(:foo)
    ensure
      Kernel.alias_method(:method_missing, :old_method_missing)
      Kernel.remove_method(:old_method_missing)
    end
  end
end

assert('Can still call super when Kernel#method_missing is removed') do
  assert_raise(NoMethodError) do
    class A
      def foo
        super
      end
    end
    begin
      Kernel.alias_method(:old_method_missing, :method_missing)
      Kernel.remove_method(:method_missing)
      A.new.foo
    ensure
      Kernel.alias_method(:method_missing, :old_method_missing)
      Kernel.remove_method(:old_method_missing)
    end
  end
end

assert("NoMethodError#new does not return an exception") do
  begin
    class << NoMethodError
      def new(*)
        nil
      end
    end

    assert_raise(TypeError) do
      Object.q
    end
  ensure
    class << NoMethodError
      remove_method :new
    end
  end
end

assert("NoMethodError#new does not return an exception") do
  begin
    class << NoMethodError
      def new(*)
        nil
      end
    end

    assert_raise(TypeError) do
      Object.q
    end
  ensure
    class << NoMethodError
      remove_method :new
    end
  end
end
