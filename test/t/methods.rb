##
# Chapter 13.3 "Methods" ISO Test

assert('The alias statement', '13.3.6 a) 4)') do
  # check aliasing in all possible ways

  def alias_test_method_original; true; end

  alias alias_test_method_a alias_test_method_original
  alias :alias_test_method_b :alias_test_method_original

  assert_true(alias_test_method_original)
  assert_true(alias_test_method_a)
  assert_true(alias_test_method_b)
end

assert('The alias statement (overwrite original)', '13.3.6 a) 4)') do
  # check that an aliased method can be overwritten
  # without side effect

  def alias_test_method_original; true; end

  alias alias_test_method_a alias_test_method_original
  alias :alias_test_method_b :alias_test_method_original

  assert_true(alias_test_method_original)

  def alias_test_method_original; false; end

  assert_false(alias_test_method_original)
  assert_true(alias_test_method_a)
  assert_true(alias_test_method_b)
end

assert('The alias statement', '13.3.6 a) 5)') do
  # check that alias is raising NameError if
  # non-existing method should be undefined

  assert_raise(NameError) do
    alias new_name_a non_existing_method
  end

  assert_raise(NameError) do
    alias :new_name_b :non_existing_method
  end
end

assert('The undef statement', '13.3.7 a) 4)') do
  # check that undef is undefining method
  # based on the method name

  def existing_method_a; true; end
  def existing_method_b; true; end
  def existing_method_c; true; end
  def existing_method_d; true; end
  def existing_method_e; true; end
  def existing_method_f; true; end

  # check that methods are defined

  assert_true(existing_method_a, 'Method should be defined')
  assert_true(existing_method_b, 'Method should be defined')
  assert_true(existing_method_c, 'Method should be defined')
  assert_true(existing_method_d, 'Method should be defined')
  assert_true(existing_method_e, 'Method should be defined')
  assert_true(existing_method_f, 'Method should be defined')

  # undefine in all possible ways and check that method
  # is undefined

  undef existing_method_a
  assert_raise(NoMethodError) do
    existing_method_a
  end

  undef :existing_method_b
  assert_raise(NoMethodError) do
    existing_method_b
  end

  undef existing_method_c, existing_method_d
  assert_raise(NoMethodError) do
    existing_method_c
  end
  assert_raise(NoMethodError) do
    existing_method_d
  end

  undef :existing_method_e, :existing_method_f
  assert_raise(NoMethodError) do
    existing_method_e
  end
  assert_raise(NoMethodError) do
    existing_method_f
  end
end

assert('The undef statement (method undefined)', '13.3.7 a) 5)') do
  # check that undef is raising NameError if
  # non-existing method should be undefined

  assert_raise(NameError) do
    undef non_existing_method
  end

  assert_raise(NameError) do
    undef :non_existing_method
  end
end

assert('Visibility') do
  class A
    private
    def private_method
      true
    end
    public
    def public_method
      true
    end
    protected
    def protected_method
      true
    end

    private
    def call_private_and_protected_from_public
      private_method && protected_method
    end
    public :call_private_and_protected_from_public
  end
  a = A.new
  assert_raise(NoMethodError, 'Call private method') do
    a.private_method
  end
  assert_raise(NoMethodError, 'Call protected method') do
    a.protected_method
  end
  assert_true(a.__send__(:private_method), 'Call private method (send)')
  assert_true(a.__send__(:protected_method), 'Call protected method (send)')
  assert_true(a.public_method, 'Call public method')
  assert_true(a.call_private_and_protected_from_public, 'Call private and protected methods from public method')

  class B < A
    def super_private_from_public
      private_method
    end
    def super_protected_from_public
      protected_method
    end
  end
  b = B.new 
  assert_true(b.super_private_from_public, 'Call private method from child class')
  assert_true(b.super_protected_from_public, 'Call protected method from child class')
  
  class C < A
    def private_method
      super
    end
    def protected_method
      super
    end
  end
  c = C.new
  assert_true(c.private_method, 'Call private super method')
  assert_true(c.protected_method, 'Call protected super method')

  class D < A
    public :private_method
  end
  d = D.new
  assert_true(d.private_method, 'Redefine visibility in child class')

  class A
    public
    def private_method
      false
    end
  end
  assert_false(d.private_method, 'Redefine implementation of private method')

  class A1
    private
    def f(a)
      a
    end
    
    def g(&b)
      b.call
    end
  end

  class A2 < A1
    public :f, :g
  end

  a2 = A2.new
  assert_true(a2.f(true), 'Redefine visibility (with parameters)')
  # assert_true(a2.g { true })
  
  module M
    private
    def private_method
      true
    end
    protected
    def protected_method
      true
    end
  end
  
  class AM
    include M
  end

  class BM < AM
    def test_private
      private_method
    end

    def test_protected
      protected_method
    end
  end

  bm = BM.new
  assert_true(bm.test_private)
  assert_true(bm.test_protected)

  def bm.singleton_m 
    test_private
  end

  assert_true(bm.singleton_m)

  class << bm
    private :singleton_m
  end

  assert_raise(NoMethodError) do
    bm.singleton_m
  end

  class CM
  end

  cm = CM.new

  class << cm
    include M
    def test_private
        private_method
    end
  end

  assert_true(cm.test_private) 

  class DM
    def f
      g
    end
  end

  dm = DM.new

  class << dm
    private
    def g
      true
    end
  end

  assert_raise(NoMethodError) do
    dm.g
  end

  assert_true(dm.f)

  class E
    private
    define_method(:f) { true }
  end

  e = E.new

  assert_raise(NoMethodError) do
    e.f
  end

  class F
    private
    -> () { F.define_method(:f) { true } }.call
  end

  f = F.new

  assert_raise(NoMethodError) do
    f.f
  end

  class G1
  end

  class G2
    private
     -> () { G1.define_method(:f) { true } }.call
  end

  g1 = G1.new
  assert_true(g1.f)

  class H
    private
    def self.f; true; end
  end

  assert_true(H.f)

  class << H
    private :f
  end

  assert_raise(NoMethodError) do
    H.f
  end

end if Mrbtest::METHOD_VISIBILITY
