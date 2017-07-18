assert 'Module#name' do
  module A
    class B
    end
  end

  assert_nil A::B.singleton_class.name
  assert_equal 'Fixnum', Fixnum.name
  assert_equal 'A::B', A::B.name
end

assert 'Module#singleton_class?' do
  mod = Module.new
  cls = Class.new
  scl = cls.singleton_class

  assert_false mod.singleton_class?
  assert_false cls.singleton_class?
  assert_true scl.singleton_class?
end
